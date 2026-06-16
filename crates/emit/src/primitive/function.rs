//! Function-definition, indirect-call, and LLVM-intrinsic IR generation. Added
//! for #2098 ([C] = (ii) boundary move) — the function-CREATION capability —
//! and extended in #2102 ([D] = (ii) boundary move) with the intrinsic-call
//! capability: the emission layer gains `llvm::Function::Create` (via
//! `LLVMAddFunction`), parameter access, the indirect call through a loaded
//! function-pointer value, and the overloaded-intrinsic declaration + call
//! (`LLVMGetIntrinsicDeclaration` + `LLVMIntrinsicGetType` + `LLVMBuildCall2`)
//! so the iterator-next / tostring / closure / destructor function-builders
//! and the `*_with_overflow` / `*_sat` / `floor` / `fabs` intrinsic users can
//! move their `Function::Create` + body emission and intrinsic calls across
//! the boundary.
//!
//! These primitives are generic and semantically trivial — nothing here knows
//! about iterators or overflow checks or any Ry-level concept (the (ii) non-
//! semantic op surface). The C++ path enters through the `abi::function`
//! externs, which resolve u32 ids / translate opaque handles, map the linkage
//! selector, and intern any produced value.

use std::ffi::{c_char, c_uint};

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{LLVMLinkage, LLVMTypeKind};

use crate::context::{EmitCtx, FuncTypeRef, FunctionRef, TypeRef, ValueRef};

impl EmitCtx {
    // Create a fresh function `name` of type `fn_ty` in the module and set its
    // linkage. Unconditionally adds a new function (`LLVMAddFunction`, the C-API
    // equivalent of `llvm::Function::Create`) — unlike `get_or_insert_function`,
    // which dedups by name; the caller owns name uniqueness (iterator/lambda/...
    // builders mint a counter-suffixed unique name). Returns the function handle;
    // the abi boundary converts it to RyFunctionRef directly (not interned —
    // same handle shape as `create_basic_block`'s BasicBlockRef).
    pub(crate) unsafe fn create_function(
        &mut self,
        name: *const c_char,
        fn_ty: FuncTypeRef,
        linkage: LLVMLinkage,
    ) -> FunctionRef {
        let f = LLVMAddFunction(self.module, name, fn_ty.0);
        LLVMSetLinkage(f, linkage);
        FunctionRef(f)
    }

    // Add the `nounwind` (LLVM `NoUnwind`) attribute to `func`. Mirrors the
    // C++ `llvm::Function::setDoesNotThrow` used by the GC visitor thunk
    // generator (`codegen_arc_gc.cpp`). The C ABI for this is a 3-call
    // sequence: look up the enum-attribute kind id by name, build the enum
    // attribute, and attach it at the function index (`LLVMAttributeFunctionIndex`
    // is `~0u` per the C header). Added for pilot G (#2196).
    pub(crate) unsafe fn function_set_nounwind(&mut self, func: FunctionRef) {
        const NAME: &[u8] = b"nounwind";
        let kind = LLVMGetEnumAttributeKindForName(NAME.as_ptr() as *const c_char, NAME.len());
        let attr = LLVMCreateEnumAttribute(self.context, kind, 0);
        // LLVMAttributeFunctionIndex from llvm-c/Types.h: (LLVMAttributeIndex)~0U.
        const FUNCTION_INDEX: c_uint = !0u32;
        LLVMAddAttributeAtIndex(func.0, FUNCTION_INDEX, attr);
    }

    // Read the `idx`-th parameter value of `func` (`LLVMGetParam`). Returns the
    // raw param value; the abi boundary interns it. `None` when `idx` is out of
    // range for `func`'s parameter count — `LLVMGetParam` itself does raw
    // pointer arithmetic (`arg_begin()[index]`) and is UB on OOB, and the abi
    // shell cannot do this guard itself because `check-emit-abi-no-ir.sh`
    // (#2069) forbids `llvm_sys::core` references in `abi/**` (#2141).
    pub(crate) unsafe fn get_param(&mut self, func: FunctionRef, idx: u32) -> Option<ValueRef> {
        if idx >= LLVMCountParams(func.0) {
            return None;
        }
        Some(ValueRef(LLVMGetParam(func.0, idx)))
    }

    // Emit a call through the runtime function-pointer value `callee` (a loaded
    // `ptr`, not a module-keyed name), typed by `fn_ty`, with `args`. `name` is
    // the already-NUL-defaulted SSA name; LLVM forbids naming a void-returning
    // call, so a void return type drops the name (mirrors `runtime_call`).
    // Returns the raw call value; the abi boundary interns it.
    pub(crate) unsafe fn call_indirect(
        &mut self,
        fn_ty: FuncTypeRef,
        callee: ValueRef,
        args: &[ValueRef],
        name: *const c_char,
    ) -> ValueRef {
        let mut args_v: Vec<LLVMValueRef> = args.iter().map(|a| a.0).collect();
        let call_name =
            if LLVMGetTypeKind(LLVMGetReturnType(fn_ty.0)) == LLVMTypeKind::LLVMVoidTypeKind {
                c"".as_ptr()
            } else {
                name
            };
        ValueRef(LLVMBuildCall2(
            self.builder,
            fn_ty.0,
            callee.0,
            args_v.as_mut_ptr(),
            args_v.len() as u32,
            call_name,
        ))
    }

    // Emit a call to the overloaded LLVM intrinsic identified by `intrinsic_id`
    // (`llvm::Intrinsic::ID`, passed as a u32; same numeric value across the
    // process because both `ry` and the cdylib share ONE libLLVM via
    // `force-dynamic`) parameterised by `overload_tys[..]`, with operand
    // `args[..]`. Three C-API calls: `LLVMGetIntrinsicDeclaration` gets / inserts
    // the per-overload `llvm::Function*`, `LLVMIntrinsicGetType` derives its
    // FunctionType (purpose-built — preferred over `LLVMGlobalGetValueType`),
    // and `LLVMBuildCall2` emits the call. Same void-name-drop as
    // `call_indirect` so a void-returning intrinsic (e.g. `llvm.memcpy`) stays
    // valid. Returns the raw call value; the abi boundary interns it.
    //
    // `overload_tys` is the type-parameter array LLVM uses to specialise the
    // overloaded intrinsic (e.g. `{i32}` for `sadd_with_overflow.i32`); for a
    // non-overloaded intrinsic it is empty. The slice is materialised into a
    // local Vec because `LLVMGetIntrinsicDeclaration` / `LLVMIntrinsicGetType`
    // take a `*mut LLVMTypeRef` (C API artifact; they do not write).
    pub(crate) unsafe fn build_intrinsic_call(
        &mut self,
        intrinsic_id: u32,
        overload_tys: &[TypeRef],
        args: &[ValueRef],
        name: *const c_char,
    ) -> ValueRef {
        let mut tys_v: Vec<LLVMTypeRef> = overload_tys.iter().map(|t| t.0).collect();
        let decl = LLVMGetIntrinsicDeclaration(
            self.module,
            intrinsic_id as c_uint,
            tys_v.as_mut_ptr(),
            tys_v.len(),
        );
        let fn_ty = LLVMIntrinsicGetType(
            self.context,
            intrinsic_id as c_uint,
            tys_v.as_mut_ptr(),
            tys_v.len(),
        );
        let mut args_v: Vec<LLVMValueRef> = args.iter().map(|a| a.0).collect();
        let call_name =
            if LLVMGetTypeKind(LLVMGetReturnType(fn_ty)) == LLVMTypeKind::LLVMVoidTypeKind {
                c"".as_ptr()
            } else {
                name
            };
        ValueRef(LLVMBuildCall2(
            self.builder,
            fn_ty,
            decl,
            args_v.as_mut_ptr(),
            args_v.len() as u32,
            call_name,
        ))
    }
}
