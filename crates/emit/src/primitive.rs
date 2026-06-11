//! Scalar / memory IR primitives (core-role: an `impl EmitCtx` over the core
//! engine only, so it is abi-independent and the `core⇏abi` invariant covers
//! this module). Each method is a 1:1 wrapper over a single `LLVMBuild*` (or
//! `LLVMConstInt`) — alloca / load / store / gep / icmp / and / or / add / sub /
//! select / const-int. Added for #2072 ([C] = (ii) boundary move): the string
//! byte-ops (toUpper / toLower / trim*) emit these primitives inline in C++ via
//! `builder_.Create*` today; under (ii) the C++ side calls the matching
//! `ry_emit_*` boundary entry (abi/primitive.rs) so all `IRBuilder<>::Create*`
//! moves into the emission layer. The boundary is fine-grained by necessity —
//! these primitives are semantically trivial (the lowering side has nothing to
//! decide), which is exactly the case §"Explicit non-inclusion" in
//! `docs/architecture/codegen-layering-plan.md` reserved for C++ carve-out under
//! the (i) hypothesis; (ii) supersedes that for the string-op pilot.

use std::ffi::c_char;

use llvm_sys::core::*;
use llvm_sys::LLVMIntPredicate;

use crate::core::*;

impl IcmpPred {
    // Translate the core predicate to the llvm-sys `LLVMIntPredicate`. Kept in
    // the engine (not the abi shell) so no llvm-sys type appears in abi/.
    #[inline]
    fn to_llvm(self) -> LLVMIntPredicate {
        match self {
            IcmpPred::Eq => LLVMIntPredicate::LLVMIntEQ,
            IcmpPred::Ne => LLVMIntPredicate::LLVMIntNE,
            IcmpPred::Slt => LLVMIntPredicate::LLVMIntSLT,
            IcmpPred::Sle => LLVMIntPredicate::LLVMIntSLE,
            IcmpPred::Sgt => LLVMIntPredicate::LLVMIntSGT,
            IcmpPred::Sge => LLVMIntPredicate::LLVMIntSGE,
            IcmpPred::Ult => LLVMIntPredicate::LLVMIntULT,
            IcmpPred::Ule => LLVMIntPredicate::LLVMIntULE,
            IcmpPred::Ugt => LLVMIntPredicate::LLVMIntUGT,
            IcmpPred::Uge => LLVMIntPredicate::LLVMIntUGE,
        }
    }
}

impl EmitCtx {
    // `alloca ty` at the builder's current insert point. `name` is the
    // already-NUL-defaulted SSA-name pointer (the abi boundary maps NULL → "").
    pub(crate) unsafe fn build_alloca(&mut self, ty: TypeRef, name: *const c_char) -> ValueRef {
        ValueRef(LLVMBuildAlloca(self.builder, ty.0, name))
    }

    // `load ty, ptr` (element-typed under opaque pointers).
    pub(crate) unsafe fn build_load(
        &mut self,
        ty: TypeRef,
        ptr: ValueRef,
        name: *const c_char,
    ) -> ValueRef {
        ValueRef(LLVMBuildLoad2(self.builder, ty.0, ptr.0, name))
    }

    // `store val, ptr` (no value produced).
    pub(crate) unsafe fn build_store(&mut self, val: ValueRef, ptr: ValueRef) {
        LLVMBuildStore(self.builder, val.0, ptr.0);
    }

    // Single-index `getelementptr base_ty, ptr, idx`.
    pub(crate) unsafe fn build_gep(
        &mut self,
        base_ty: TypeRef,
        ptr: ValueRef,
        idx: ValueRef,
        name: *const c_char,
    ) -> ValueRef {
        let mut idxs = [idx.0];
        ValueRef(LLVMBuildGEP2(
            self.builder,
            base_ty.0,
            ptr.0,
            idxs.as_mut_ptr(),
            1,
            name,
        ))
    }

    // `icmp <pred> lhs, rhs`.
    pub(crate) unsafe fn build_icmp(
        &mut self,
        pred: IcmpPred,
        lhs: ValueRef,
        rhs: ValueRef,
        name: *const c_char,
    ) -> ValueRef {
        ValueRef(LLVMBuildICmp(
            self.builder,
            pred.to_llvm(),
            lhs.0,
            rhs.0,
            name,
        ))
    }

    // `and lhs, rhs`.
    pub(crate) unsafe fn build_and(
        &mut self,
        lhs: ValueRef,
        rhs: ValueRef,
        name: *const c_char,
    ) -> ValueRef {
        ValueRef(LLVMBuildAnd(self.builder, lhs.0, rhs.0, name))
    }

    // `or lhs, rhs`.
    pub(crate) unsafe fn build_or(
        &mut self,
        lhs: ValueRef,
        rhs: ValueRef,
        name: *const c_char,
    ) -> ValueRef {
        ValueRef(LLVMBuildOr(self.builder, lhs.0, rhs.0, name))
    }

    // `add lhs, rhs`.
    pub(crate) unsafe fn build_add(
        &mut self,
        lhs: ValueRef,
        rhs: ValueRef,
        name: *const c_char,
    ) -> ValueRef {
        ValueRef(LLVMBuildAdd(self.builder, lhs.0, rhs.0, name))
    }

    // `sub lhs, rhs`.
    pub(crate) unsafe fn build_sub(
        &mut self,
        lhs: ValueRef,
        rhs: ValueRef,
        name: *const c_char,
    ) -> ValueRef {
        ValueRef(LLVMBuildSub(self.builder, lhs.0, rhs.0, name))
    }

    // `select cond, then_v, else_v`.
    pub(crate) unsafe fn build_select(
        &mut self,
        cond: ValueRef,
        then_v: ValueRef,
        else_v: ValueRef,
        name: *const c_char,
    ) -> ValueRef {
        ValueRef(LLVMBuildSelect(
            self.builder,
            cond.0,
            then_v.0,
            else_v.0,
            name,
        ))
    }

    // Materialize an integer constant `LLVMConstInt(ty, value, sign_extend)`.
    // Emits no instruction (a Constant is an inline operand) but is interned by
    // the abi shell so the C++ side can thread it as a `RyValueId` like any
    // other value handle.
    pub(crate) unsafe fn const_int(
        &mut self,
        ty: TypeRef,
        value: u64,
        sign_extend: bool,
    ) -> ValueRef {
        ValueRef(LLVMConstInt(ty.0, value, sign_extend as i32))
    }
}
