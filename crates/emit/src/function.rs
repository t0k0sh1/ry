//! Function-definition and indirect-call IR generation (core-role: an `impl
//! EmitCtx` over the core engine only, so it is abi-independent and the
//! `core⇏abi` invariant covers this module). Added for #2098 ([C] = (ii)
//! boundary move): this is the function-CREATION capability — the emission layer
//! gains `llvm::Function::Create` (via `LLVMAddFunction`), parameter access, and
//! the indirect call through a loaded function-pointer value, so the iterator-
//! next / tostring / closure / destructor function-builders can move their
//! `Function::Create` + body emission across the boundary.
//!
//! These primitives are generic and semantically trivial — nothing here knows
//! about iterators or any Ry-level concept (the (ii) non-semantic op surface).
//! The C++ path enters through the `abi::function` externs, which resolve u32
//! ids / translate opaque handles, map the linkage selector, and intern any
//! produced value.

use std::ffi::c_char;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{LLVMLinkage, LLVMTypeKind};

use crate::core::*;

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

    // Read the `idx`-th parameter value of `func` (`LLVMGetParam`). Returns the
    // raw param value; the abi boundary interns it.
    pub(crate) unsafe fn get_param(&mut self, func: FunctionRef, idx: u32) -> ValueRef {
        ValueRef(LLVMGetParam(func.0, idx))
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
}
