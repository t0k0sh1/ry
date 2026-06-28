//! Scalar / memory IR primitives. Each method is a 1:1 wrapper over a single
//! `LLVMBuild*` (or `LLVMConstInt`) — alloca / load / store / gep / icmp / and /
//! or / add / sub / select / const-int. Added for #2072 ([C] = (ii) boundary
//! move): the string byte-ops (toUpper / toLower / trim*) emit these primitives
//! inline in C++ via `builder_.Create*` today; under (ii) the C++ side calls
//! the matching `ry_emit_*` boundary entry (`abi/primitive.rs`) so all
//! `IRBuilder<>::Create*` moves into the emission layer. The boundary is
//! fine-grained by necessity — these primitives are semantically trivial (the
//! lowering side has nothing to decide), which is exactly the case
//! §"Explicit non-inclusion" in `docs/architecture/codegen-layering-plan.md`
//! reserved for C++ carve-out under the (i) hypothesis; (ii) supersedes that
//! for the string-op pilot.

use std::ffi::c_char;

use llvm_sys::core::*;
use llvm_sys::{LLVMAtomicOrdering, LLVMAtomicRMWBinOp, LLVMIntPredicate};

use crate::context::{AtomicBinOp, AtomicOrdering, EmitCtx, IcmpPred, TypeRef, ValueRef};

impl AtomicBinOp {
    // Translate the context binop to the llvm-sys `LLVMAtomicRMWBinOp`. Kept
    // engine-side so no llvm-sys type appears in abi/.
    #[inline]
    fn to_llvm(self) -> LLVMAtomicRMWBinOp {
        match self {
            AtomicBinOp::Add => LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpAdd,
            AtomicBinOp::Sub => LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpSub,
        }
    }
}

impl AtomicOrdering {
    // Translate the context ordering to the llvm-sys `LLVMAtomicOrdering`. Kept
    // engine-side so no llvm-sys type appears in abi/.
    #[inline]
    fn to_llvm(self) -> LLVMAtomicOrdering {
        match self {
            AtomicOrdering::NotAtomic => LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic,
            AtomicOrdering::Monotonic => LLVMAtomicOrdering::LLVMAtomicOrderingMonotonic,
            AtomicOrdering::Acquire => LLVMAtomicOrdering::LLVMAtomicOrderingAcquire,
            AtomicOrdering::Release => LLVMAtomicOrdering::LLVMAtomicOrderingRelease,
            AtomicOrdering::AcquireRelease => LLVMAtomicOrdering::LLVMAtomicOrderingAcquireRelease,
            AtomicOrdering::SeqCst => LLVMAtomicOrdering::LLVMAtomicOrderingSequentiallyConsistent,
        }
    }
}

impl IcmpPred {
    // Translate the context predicate to the llvm-sys `LLVMIntPredicate`. Kept
    // in the engine (not the abi shell) so no llvm-sys type appears in abi/.
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

    // `atomicrmw <binop> ptr, val <ordering>` — generic LLVM atomic
    // read-modify-write. Alignment is LLVM-auto-computed from the datalayout
    // (matches the C++ `CreateAtomicRMW(..., MaybeAlign(), ...)` baseline).
    // NotAtomic ordering is invalid for atomicrmw and falls back to Monotonic.
    // Added for #2190 (weak ARC capability migration).
    pub(crate) unsafe fn build_atomic_rmw(
        &mut self,
        binop: AtomicBinOp,
        ptr: ValueRef,
        val: ValueRef,
        ordering: AtomicOrdering,
        _name: *const c_char,
    ) -> ValueRef {
        let ord = if matches!(ordering, AtomicOrdering::NotAtomic) {
            LLVMAtomicOrdering::LLVMAtomicOrderingMonotonic
        } else {
            ordering.to_llvm()
        };
        // LLVMBuildAtomicRMW does NOT take a name parameter — the C++ baseline
        // `builder_.CreateAtomicRMW(...)` likewise has no name. The unused
        // `_name` keeps the abi signature symmetric with the other primitives
        // (and matches api.h) without producing a named SSA value here; LLVM
        // auto-numbers the result instruction, mirroring the C++ baseline.
        ValueRef(LLVMBuildAtomicRMW(
            self.builder,
            binop.to_llvm(),
            ptr.0,
            val.0,
            ord,
            0, // SingleThread = 0 (matches default cross-thread atomic)
        ))
    }

    // `load ty, ptr` then optionally `LLVMSetOrdering(ld, ordering)` /
    // `LLVMSetAlignment(ld, alignment)`. NotAtomic ordering → no SetOrdering
    // call (plain load). `alignment == 0` → no SetAlignment call (uses the
    // LLVMBuildLoad2 default, matching the C++ `CreateLoad + setAtomic`
    // baseline where alignment stays at the datalayout default). Added for
    // #2190.
    pub(crate) unsafe fn build_atomic_load(
        &mut self,
        ty: TypeRef,
        ptr: ValueRef,
        ordering: AtomicOrdering,
        alignment: u32,
        name: *const c_char,
    ) -> ValueRef {
        let ld = LLVMBuildLoad2(self.builder, ty.0, ptr.0, name);
        if !matches!(ordering, AtomicOrdering::NotAtomic) {
            LLVMSetOrdering(ld, ordering.to_llvm());
        }
        if alignment > 0 {
            LLVMSetAlignment(ld, alignment);
        }
        ValueRef(ld)
    }

    // `cmpxchg ptr, expected, desired <success_ordering> <failure_ordering>`
    // — generic LLVM compare-and-swap returning the `{T, i1}` aggregate.
    // Alignment is LLVM-auto-computed (matches C++ `CreateAtomicCmpXchg(...,
    // MaybeAlign(), ...)`). NotAtomic falls back to Monotonic for either
    // ordering (cmpxchg requires an atomic ordering). Added for #2190 (weak
    // upgrade CAS loop). The `_name` is unused — `LLVMBuildAtomicCmpXchg`
    // does not accept a name (the C++ baseline `CreateAtomicCmpXchg(...)`
    // likewise yields an auto-numbered SSA value); the abi keeps the
    // parameter symmetric with other primitives.
    pub(crate) unsafe fn build_atomic_cmpxchg(
        &mut self,
        ptr: ValueRef,
        expected: ValueRef,
        desired: ValueRef,
        success_ord: AtomicOrdering,
        failure_ord: AtomicOrdering,
        _name: *const c_char,
    ) -> ValueRef {
        let s = if matches!(success_ord, AtomicOrdering::NotAtomic) {
            LLVMAtomicOrdering::LLVMAtomicOrderingMonotonic
        } else {
            success_ord.to_llvm()
        };
        let f = if matches!(failure_ord, AtomicOrdering::NotAtomic) {
            LLVMAtomicOrdering::LLVMAtomicOrderingMonotonic
        } else {
            failure_ord.to_llvm()
        };
        ValueRef(LLVMBuildAtomicCmpXchg(
            self.builder,
            ptr.0,
            expected.0,
            desired.0,
            s,
            f,
            0, // SingleThread = 0 (matches default cross-thread atomic)
        ))
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

    // `insertvalue agg, val, idx` — write `val` into aggregate field `idx`,
    // returning a new aggregate value (`LLVMBuildInsertValue`). The symmetric
    // partner of `build_extract_value`. Added for #2186 (dense iterator sweep):
    // the Map iterator next-fn builds a `{key, val}` tuple via
    // `undef → insertvalue 0 → insertvalue 1` before returning the Option payload.
    pub(crate) unsafe fn build_insert_value(
        &mut self,
        agg: ValueRef,
        val: ValueRef,
        idx: u32,
        name: *const c_char,
    ) -> ValueRef {
        ValueRef(LLVMBuildInsertValue(self.builder, agg.0, val.0, idx, name))
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

    // `trunc val to dest_ty` — truncate an integer value to a narrower integer
    // type (`LLVMBuildTrunc`). Added for #2194 (concurrency capability
    // migration): thread join unwraps an i64 worker-return slot to i1 for bool
    // workers, AtomicInt CAS truncates its i64 status to i1, and AtomicBool
    // load truncates its i64 backing slot to i1. All three sites currently
    // emit a C++ `builder_.CreateTrunc`; this primitive is the boundary entry.
    pub(crate) unsafe fn build_trunc(
        &mut self,
        val: ValueRef,
        dest_ty: TypeRef,
        name: *const c_char,
    ) -> ValueRef {
        ValueRef(LLVMBuildTrunc(self.builder, val.0, dest_ty.0, name))
    }

    // `sext val to dest_ty` — sign-extend an integer value to a wider integer
    // type (`LLVMBuildSExt`). Symmetric partner of `build_zext` (#2101). Added
    // for #2192 (stdlib op follow-on sweep): `args()`'s argc widening from i32
    // (the `__ry_args_count` return type) to i64 emits a `builder_.CreateSExt`
    // today; this primitive is the boundary entry.
    pub(crate) unsafe fn build_sext(
        &mut self,
        val: ValueRef,
        dest_ty: TypeRef,
        name: *const c_char,
    ) -> ValueRef {
        ValueRef(LLVMBuildSExt(self.builder, val.0, dest_ty.0, name))
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

    // `xor lhs, rhs`.
    pub(crate) unsafe fn build_xor(
        &mut self,
        lhs: ValueRef,
        rhs: ValueRef,
        name: *const c_char,
    ) -> ValueRef {
        ValueRef(LLVMBuildXor(self.builder, lhs.0, rhs.0, name))
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

    // `mul lhs, rhs`. Added for #2096 (string-build op migration): str * n /
    // join sep total / repeat byte_len * count all multiply two i64 values.
    pub(crate) unsafe fn build_mul(
        &mut self,
        lhs: ValueRef,
        rhs: ValueRef,
        name: *const c_char,
    ) -> ValueRef {
        ValueRef(LLVMBuildMul(self.builder, lhs.0, rhs.0, name))
    }

    // `sdiv lhs, rhs` (signed). Added for #2096: emitStringRepeat's overflow
    // bound `INT64_MAX / strLen` needs a signed divide; the divisor is guarded
    // `strLen > 0` upstream so no division-by-zero path exists here.
    pub(crate) unsafe fn build_sdiv(
        &mut self,
        lhs: ValueRef,
        rhs: ValueRef,
        name: *const c_char,
    ) -> ValueRef {
        ValueRef(LLVMBuildSDiv(self.builder, lhs.0, rhs.0, name))
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

    // Materialize a floating-point constant `LLVMConstReal(ty, value)`. `ty`
    // selects f32 vs f64 semantics; the `f64` argument is narrowed by
    // `LLVMConstReal` to the target type's float layout (byte-identical to
    // `llvm::ConstantFP::get(fNTy, double)` on the C++ side).
    pub(crate) unsafe fn const_real(&mut self, ty: TypeRef, value: f64) -> ValueRef {
        ValueRef(LLVMConstReal(ty.0, value))
    }

    // Materialize a null constant of any type via `LLVMConstNull(ty)`. For
    // pointer types this is `ConstantPointerNull::get(ty)` — the typed null
    // pointer used in null-guard `icmp eq` comparisons. Added for pilot G
    // (#2196): the GC visitor thunk null-guard reads `fieldVal == null` and
    // currently emits `ConstantPointerNull::get` inline; this primitive is the
    // boundary entry. Like `const_int`, no instruction is emitted but the
    // result is interned for cross-boundary handle threading.
    pub(crate) unsafe fn const_null(&mut self, ty: TypeRef) -> ValueRef {
        ValueRef(LLVMConstNull(ty.0))
    }

    // Materialize an undef value of any type via `LLVMGetUndef(ty)`. Used as the
    // initial aggregate before `build_insert_value` fills each field — the
    // canonical `undef → insertvalue 0 → insertvalue 1 → …` build pattern. Added
    // for #2186 (dense iterator sweep): the Map iterator next-fn seeds its
    // `{key, val}` tuple from undef. Like `const_null`, no instruction is emitted
    // but the result is interned for cross-boundary handle threading.
    pub(crate) unsafe fn build_undef(&mut self, ty: TypeRef) -> ValueRef {
        ValueRef(LLVMGetUndef(ty.0))
    }
}
