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

    // Struct-field `getelementptr struct_ty, ptr, i32 0, i32 field_idx`
    // (`LLVMBuildStructGEP2`). Distinct from `build_gep`'s single runtime i64
    // index — this is a compile-time field index into an aggregate (#2098).
    pub(crate) unsafe fn build_struct_gep(
        &mut self,
        struct_ty: TypeRef,
        ptr: ValueRef,
        field_idx: u32,
        name: *const c_char,
    ) -> ValueRef {
        ValueRef(LLVMBuildStructGEP2(
            self.builder,
            struct_ty.0,
            ptr.0,
            field_idx,
            name,
        ))
    }

    // Two-index `getelementptr array_ty, base, i64 0, idx` — indexes an `[N x T]`
    // aggregate (e.g. an enum name-array global). The leading `i64 0` is
    // synthesized from `self.context`; `idx` is the caller's runtime/constant
    // i64 index. When `base` and `idx` are constants the IRBuilder folds the
    // result to a ConstantExpr, byte-for-byte matching the C++
    // builder_.CreateGEP(ArrayType, base, {0, idx}) it replaces (#2100).
    pub(crate) unsafe fn build_array_gep(
        &mut self,
        array_ty: TypeRef,
        base: ValueRef,
        idx: ValueRef,
        name: *const c_char,
    ) -> ValueRef {
        let mut idxs = [
            LLVMConstInt(LLVMInt64TypeInContext(self.context), 0, 0),
            idx.0,
        ];
        ValueRef(LLVMBuildGEP2(
            self.builder,
            array_ty.0,
            base.0,
            idxs.as_mut_ptr(),
            2,
            name,
        ))
    }

    // `extractvalue agg, idx` — read a single aggregate field by compile-time
    // index (`LLVMBuildExtractValue`). Distinct from `build_struct_gep`, which
    // addresses a struct field through a *pointer*; this reads a field out of an
    // aggregate *value* (e.g. an `Option<T>` struct's tag / payload). Added for
    // #2099 (closure / FnTypeInfo crossing: the filter / map iterator next-fn
    // destructures the source `Option` in-register).
    pub(crate) unsafe fn build_extract_value(
        &mut self,
        agg: ValueRef,
        idx: u32,
        name: *const c_char,
    ) -> ValueRef {
        ValueRef(LLVMBuildExtractValue(self.builder, agg.0, idx, name))
    }

    // `zext val to dest_ty` — zero-extend an integer value to a wider integer
    // type (`LLVMBuildZExt`). Added for #2101 (hash-table lookup capability):
    // emitHashTableLookup conditionally widens an i1 key to the i64 hash-arg
    // type for `Set<bool>` / `Map<bool, V>`. The C++ side only routes here when
    // `keyTy != keyArgTy`, so the widening is always non-trivial.
    pub(crate) unsafe fn build_zext(
        &mut self,
        val: ValueRef,
        dest_ty: TypeRef,
        name: *const c_char,
    ) -> ValueRef {
        ValueRef(LLVMBuildZExt(self.builder, val.0, dest_ty.0, name))
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
