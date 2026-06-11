//! Control-flow IR generation (core-role: an `impl EmitCtx` over the core
//! engine only, so it is abi-independent and the `core⇏abi` invariant covers
//! this module). Basic-block creation, conditional / unconditional branches,
//! and PHI nodes. The C++ path enters through the `abi::control_flow` externs,
//! which resolve u32 ids / translate opaque handles and intern any result.

use std::ffi::c_char;

use llvm_sys::core::*;

use crate::core::*;

impl EmitCtx {
    // Append a fresh basic block to `func`. `name` is the already-NUL-defaulted
    // SSA-name pointer (the abi boundary maps a NULL name to the empty string).
    pub(crate) unsafe fn create_basic_block(
        &mut self,
        func: FunctionRef,
        name: *const c_char,
    ) -> BasicBlockRef {
        BasicBlockRef(LLVMAppendBasicBlockInContext(self.context, func.0, name))
    }

    // Conditional branch on `cond` to `true_bb` / `false_bb`.
    pub(crate) unsafe fn branch_cond(
        &mut self,
        cond: ValueRef,
        true_bb: BasicBlockRef,
        false_bb: BasicBlockRef,
    ) {
        LLVMBuildCondBr(self.builder, cond.0, true_bb.0, false_bb.0);
    }

    // Unconditional branch to `target`.
    pub(crate) unsafe fn branch_uncond(&mut self, target: BasicBlockRef) {
        LLVMBuildBr(self.builder, target.0);
    }

    // `ret val` (or `ret void` when `val` is None) at the builder's current
    // insert point. The function-builder counterpart to the branch terminators
    // (#2098).
    pub(crate) unsafe fn build_ret(&mut self, val: Option<ValueRef>) {
        match val {
            Some(v) => {
                LLVMBuildRet(self.builder, v.0);
            }
            None => {
                LLVMBuildRetVoid(self.builder);
            }
        }
    }

    // Build a PHI of `ty` with the given incoming (value, block) edges, added in
    // order. `name` is the already-NUL-defaulted SSA-name pointer. Returns the
    // raw phi value; the abi boundary interns it (intern/resolve are abi-side).
    pub(crate) unsafe fn create_phi(
        &mut self,
        ty: TypeRef,
        edges: &[(ValueRef, BasicBlockRef)],
        name: *const c_char,
    ) -> ValueRef {
        let phi = LLVMBuildPhi(self.builder, ty.0, name);
        for &(value, block) in edges {
            let mut v = [value.0];
            let mut bb = [block.0];
            LLVMAddIncoming(phi, v.as_mut_ptr(), bb.as_mut_ptr(), 1);
        }
        ValueRef(phi)
    }
}
