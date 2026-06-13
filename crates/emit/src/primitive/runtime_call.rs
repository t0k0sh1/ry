//! Runtime-function emission. The generic runtime call (`runtime_call`) and
//! the runtime-fn declaration / lookup helper (`get_runtime_fn`). The C++ path
//! enters through the `abi::runtime_call` externs, which validate inputs,
//! resolve the u32 ids / translate the type handles into the typed slices these
//! methods consume, and intern the call result (`get_runtime_fn` returns a raw,
//! un-interned handle).

use std::ffi::c_char;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::LLVMTypeKind;

use crate::context::{EmitCtx, FuncTypeRef, FunctionRef, TypeRef, ValueRef};
use crate::primitive::module::get_or_insert_function;

impl EmitCtx {
    // Declare-or-reuse `name` with signature `(arg_tys) -> ret_ty` and emit a
    // call against it with `args`. `arg_tys` / `args` are the abi-validated,
    // equal-length typed slices (no null elements). Returns the raw call value;
    // the abi boundary interns it.
    pub(crate) unsafe fn runtime_call(
        &mut self,
        name: *const c_char,
        ret_ty: TypeRef,
        arg_tys: &[TypeRef],
        args: &[ValueRef],
        name_hint: *const c_char,
    ) -> ValueRef {
        let ret_ty = ret_ty.0;
        let mut arg_tys_v: Vec<LLVMTypeRef> = arg_tys.iter().map(|t| t.0).collect();
        let fn_ty = LLVMFunctionType(ret_ty, arg_tys_v.as_mut_ptr(), arg_tys_v.len() as u32, 0);
        let callee = get_or_insert_function(self.module, name, fn_ty);
        let mut args_v: Vec<LLVMValueRef> = args.iter().map(|a| a.0).collect();
        // LLVM forbids naming a void-returning call; pass an empty name in that case.
        let call_name =
            if LLVMGetTypeKind(ret_ty) == LLVMTypeKind::LLVMVoidTypeKind || name_hint.is_null() {
                c"".as_ptr()
            } else {
                name_hint
            };
        let result = LLVMBuildCall2(
            self.builder,
            fn_ty,
            callee,
            args_v.as_mut_ptr(),
            args_v.len() as u32,
            call_name,
        );
        ValueRef(result)
    }

    // Declare-or-reuse `name` with the given function type and return the
    // function value directly (raw handle; the abi boundary does NOT intern it).
    pub(crate) unsafe fn get_runtime_fn(
        &mut self,
        name: *const c_char,
        fn_ty: FuncTypeRef,
    ) -> FunctionRef {
        FunctionRef(get_or_insert_function(self.module, name, fn_ty.0))
    }
}
