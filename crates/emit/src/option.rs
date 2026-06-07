//! Option IR generation (core-role: an `impl EmitCtx` over the core engine
//! only, so it is abi-independent and the `core⇏abi` invariant covers this
//! module). Builds the `{ i1 tag, payload }` Option aggregate for Some / None.
//! The C++ path enters through the `abi::option` externs.

use llvm_sys::core::*;

use crate::core::*;

impl EmitCtx {
    // Build `Some(inner)`: insert tag = 1 then the payload into an undef Option
    // aggregate. Returns the raw value; the abi boundary interns it.
    pub(crate) unsafe fn option_wrap_some(&mut self, inner: ValueRef, opt_ty: TypeRef) -> ValueRef {
        let i1_ty = i1_type(self.context);
        let mut val = LLVMGetUndef(opt_ty.0);
        val = LLVMBuildInsertValue(
            self.builder,
            val,
            LLVMConstInt(i1_ty, 1, 0),
            0,
            c"".as_ptr(),
        );
        val = LLVMBuildInsertValue(self.builder, val, inner.0, 1, c"".as_ptr());
        ValueRef(val)
    }

    // Build `None`: insert tag = 0 then an undef payload into an undef Option
    // aggregate. Returns the raw value; the abi boundary interns it.
    pub(crate) unsafe fn option_wrap_none(&mut self, opt_ty: TypeRef) -> ValueRef {
        let i1_ty = i1_type(self.context);
        let mut val = LLVMGetUndef(opt_ty.0);
        val = LLVMBuildInsertValue(
            self.builder,
            val,
            LLVMConstInt(i1_ty, 0, 0),
            0,
            c"".as_ptr(),
        );
        let payload_ty = LLVMStructGetTypeAtIndex(opt_ty.0, 1);
        val = LLVMBuildInsertValue(self.builder, val, LLVMGetUndef(payload_ty), 1, c"".as_ptr());
        ValueRef(val)
    }
}
