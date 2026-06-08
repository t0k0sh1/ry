//! Result IR generation (core-role: an `impl EmitCtx` over the core engine only,
//! so it is abi-independent and the `core⇏abi` invariant covers this module).
//! Builds an `Error` struct from a runtime error function. The Result ok/err
//! branch + phi (`result_branch`) deliberately stays in `abi::result`: its ok/err
//! builders are re-entrant C callbacks returning interned ids, so it cannot
//! become a `&mut self` method without aliasing the re-entrant `cx(ctx)` borrow
//! (#2060). The C++ path enters through the `abi::result` externs, which validate
//! inputs, resolve the type handle, and intern the result.

use std::ffi::c_char;

use llvm_sys::core::*;

use crate::core::*;

impl EmitCtx {
    // Build an `Error` aggregate: call the runtime error function `err_fn_name`
    // (`() -> ptr` message), insert it at field 0 (`err.msg`) and a zero code at
    // field 1 (`err.code`). Returns the raw aggregate; the abi boundary interns
    // it.
    pub(crate) unsafe fn build_error_from_runtime(
        &mut self,
        err_fn_name: *const c_char,
        error_ty: TypeRef,
    ) -> ValueRef {
        let error_ty = error_ty.0;
        let ptr_ty = ptr_type(self.context);
        let i64_ty = i64_type(self.context);
        let err_fn_ty = LLVMFunctionType(ptr_ty, std::ptr::null_mut(), 0, 0);
        let err_fn = get_or_insert_function(self.module, err_fn_name, err_fn_ty);
        let err_msg = LLVMBuildCall2(
            self.builder,
            err_fn_ty,
            err_fn,
            std::ptr::null_mut(),
            0,
            c"err_msg".as_ptr(),
        );
        let mut err_struct = LLVMGetUndef(error_ty);
        err_struct =
            LLVMBuildInsertValue(self.builder, err_struct, err_msg, 0, c"err.msg".as_ptr());
        err_struct = LLVMBuildInsertValue(
            self.builder,
            err_struct,
            LLVMConstInt(i64_ty, 0, 0),
            1,
            c"err.code".as_ptr(),
        );
        ValueRef(err_struct)
    }
}
