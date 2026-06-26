//! abi::primitive — C boundary entry points for the scalar / memory IR
//! primitives (#2072, [C] = (ii) boundary move). Each extern resolves the u32
//! ids / translates the opaque type handle, calls the matching `core` engine
//! method on `EmitCtx`, and interns any produced value (intern / resolve are
//! abi-side; the engine never touches them). Pure mechanical translation — no
//! `LLVMBuild*` here (the `check-emit-abi-no-ir.sh` gate), so the icmp predicate
//! maps `c_int` → the core `IcmpPred` vocabulary rather than the llvm-sys
//! `LLVMIntPredicate` (the engine does that last hop).

use std::ffi::{c_char, c_int};

use crate::context::{AtomicBinOp, AtomicOrdering, IcmpPred};

use super::*;

// c_int → IcmpPred, preserving the `RY_ICMP_*` mapping in api.h exactly; any
// other value is rejected (the entry point returns its sentinel 0), so a
// malformed predicate never reaches LLVM. Written as an if/else-if chain (not a
// `match` on the `RY_ICMP_*` consts) to dodge the const-in-pattern footgun where
// a const that is not a literal pattern binds as a catch-all — the same shape
// `cow_kind_from` in abi/cow.rs uses.
#[inline]
fn icmp_pred_from(p: c_int) -> Option<IcmpPred> {
    if p == RY_ICMP_EQ {
        Some(IcmpPred::Eq)
    } else if p == RY_ICMP_NE {
        Some(IcmpPred::Ne)
    } else if p == RY_ICMP_SLT {
        Some(IcmpPred::Slt)
    } else if p == RY_ICMP_SLE {
        Some(IcmpPred::Sle)
    } else if p == RY_ICMP_SGT {
        Some(IcmpPred::Sgt)
    } else if p == RY_ICMP_SGE {
        Some(IcmpPred::Sge)
    } else if p == RY_ICMP_ULT {
        Some(IcmpPred::Ult)
    } else if p == RY_ICMP_ULE {
        Some(IcmpPred::Ule)
    } else if p == RY_ICMP_UGT {
        Some(IcmpPred::Ugt)
    } else if p == RY_ICMP_UGE {
        Some(IcmpPred::Uge)
    } else {
        None
    }
}

// c_int → AtomicBinOp, preserving the `RY_ATOMIC_BINOP_*` mapping in api.h
// exactly; any other value falls back to Add (the api.h comment documents this
// fallback). Written as an if/else chain to dodge the const-in-pattern
// footgun, same shape as `icmp_pred_from`.
#[inline]
fn atomic_binop_from(b: c_int) -> AtomicBinOp {
    if b == RY_ATOMIC_BINOP_ADD {
        AtomicBinOp::Add
    } else if b == RY_ATOMIC_BINOP_SUB {
        AtomicBinOp::Sub
    } else {
        AtomicBinOp::Add
    }
}

// c_int → AtomicOrdering, preserving the `RY_ATOMIC_ORDERING_*` mapping in
// api.h exactly; any other value falls back to NotAtomic (for loads this
// produces a plain load; for rmw / cmpxchg the engine substitutes Monotonic).
#[inline]
fn atomic_ordering_from(o: c_int) -> AtomicOrdering {
    if o == RY_ATOMIC_ORDERING_MONOTONIC {
        AtomicOrdering::Monotonic
    } else if o == RY_ATOMIC_ORDERING_ACQUIRE {
        AtomicOrdering::Acquire
    } else if o == RY_ATOMIC_ORDERING_RELEASE {
        AtomicOrdering::Release
    } else if o == RY_ATOMIC_ORDERING_ACQUIRE_RELEASE {
        AtomicOrdering::AcquireRelease
    } else if o == RY_ATOMIC_ORDERING_SEQ_CST {
        AtomicOrdering::SeqCst
    } else {
        AtomicOrdering::NotAtomic
    }
}

/// Emit `alloca ty` and return the interned result. NULL ctx / type → 0.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_alloca(
    ctx: *mut RyEmitCtx,
    ty: RyTypeRef,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let Some(t) = req_type(ty) else {
        return 0;
    };
    let v = c.build_alloca(t, name_or_empty(name));
    intern(c, to_ry_value(v.0))
}

/// Emit `load ty, ptr` and return the interned result. NULL ctx / type / ptr → 0.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_load(
    ctx: *mut RyEmitCtx,
    ty: RyTypeRef,
    ptr_id: RyValueId,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let Some(t) = req_type(ty) else {
        return 0;
    };
    let Some(ptr) = resolve_value(c, ptr_id) else {
        return 0;
    };
    let v = c.build_load(t, ptr, name_or_empty(name));
    intern(c, to_ry_value(v.0))
}

/// Emit `store val, ptr`. NULL ctx / val / ptr → no-op.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_store(ctx: *mut RyEmitCtx, val_id: RyValueId, ptr_id: RyValueId) {
    let Some(c) = checked_cx(ctx) else {
        return;
    };
    let (Some(val), Some(ptr)) = (resolve_value(c, val_id), resolve_value(c, ptr_id)) else {
        return;
    };
    c.build_store(val, ptr);
}

/// Emit `atomicrmw <binop> ptr, val <ordering>`. NULL ctx / ptr / val → 0.
/// NULL `name` → empty SSA name (the cmpxchg / atomicrmw IR is auto-numbered
/// regardless; the parameter exists for API symmetry).
#[no_mangle]
pub unsafe extern "C" fn ry_emit_atomic_rmw(
    ctx: *mut RyEmitCtx,
    binop: c_int,
    ptr_id: RyValueId,
    val_id: RyValueId,
    ordering: c_int,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(ptr), Some(val)) = (resolve_value(c, ptr_id), resolve_value(c, val_id)) else {
        return 0;
    };
    let v = c.build_atomic_rmw(
        atomic_binop_from(binop),
        ptr,
        val,
        atomic_ordering_from(ordering),
        name_or_empty(name),
    );
    intern(c, to_ry_value(v.0))
}

/// Emit `load ty, ptr` with the given atomic ordering and (optionally)
/// alignment. `alignment == 0` leaves the LLVMBuildLoad2 default in place
/// (matches the C++ `CreateLoad + setAtomic` baseline). NULL ctx / type / ptr
/// → 0.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_atomic_load(
    ctx: *mut RyEmitCtx,
    ty: RyTypeRef,
    ptr_id: RyValueId,
    ordering: c_int,
    alignment: u32,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let Some(t) = req_type(ty) else {
        return 0;
    };
    let Some(ptr) = resolve_value(c, ptr_id) else {
        return 0;
    };
    let v = c.build_atomic_load(
        t,
        ptr,
        atomic_ordering_from(ordering),
        alignment,
        name_or_empty(name),
    );
    intern(c, to_ry_value(v.0))
}

/// Emit `cmpxchg ptr, expected, desired <success_ord> <failure_ord>`.
/// Returns the `{T, i1}` aggregate (decompose via `ry_emit_extract_value`).
/// NULL ctx / ptr / expected / desired → 0. NULL `name` → empty SSA name.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_atomic_cmpxchg(
    ctx: *mut RyEmitCtx,
    ptr_id: RyValueId,
    expected_id: RyValueId,
    desired_id: RyValueId,
    success_ordering: c_int,
    failure_ordering: c_int,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(ptr), Some(expected), Some(desired)) = (
        resolve_value(c, ptr_id),
        resolve_value(c, expected_id),
        resolve_value(c, desired_id),
    ) else {
        return 0;
    };
    let v = c.build_atomic_cmpxchg(
        ptr,
        expected,
        desired,
        atomic_ordering_from(success_ordering),
        atomic_ordering_from(failure_ordering),
        name_or_empty(name),
    );
    intern(c, to_ry_value(v.0))
}

/// Emit a single-index `getelementptr base_ty, ptr, idx`. NULL ctx / type / ptr
/// / idx → 0.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_gep(
    ctx: *mut RyEmitCtx,
    base_ty: RyTypeRef,
    ptr_id: RyValueId,
    idx_id: RyValueId,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let Some(base) = req_type(base_ty) else {
        return 0;
    };
    let (Some(ptr), Some(idx)) = (resolve_value(c, ptr_id), resolve_value(c, idx_id)) else {
        return 0;
    };
    let v = c.build_gep(base, ptr, idx, name_or_empty(name));
    intern(c, to_ry_value(v.0))
}

/// Emit a struct-field `getelementptr struct_ty, ptr, 0, field_idx` — the
/// compile-time field-index GEP (`LLVMBuildStructGEP2`), distinct from the
/// runtime-index `ry_emit_gep`. NULL ctx / type / ptr → 0. NULL `name` → empty.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_struct_gep(
    ctx: *mut RyEmitCtx,
    struct_ty: RyTypeRef,
    ptr_id: RyValueId,
    field_idx: u32,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let Some(st) = req_type(struct_ty) else {
        return 0;
    };
    let Some(ptr) = resolve_value(c, ptr_id) else {
        return 0;
    };
    let v = c.build_struct_gep(st, ptr, field_idx, name_or_empty(name));
    intern(c, to_ry_value(v.0))
}

/// Emit a two-index `getelementptr array_ty, base, i64 0, idx` — the array-element
/// GEP (`LLVMBuildGEP2` with a synthesized leading `i64 0`), distinct from the
/// single-index `ry_emit_gep` and the field-index `ry_emit_struct_gep`. NULL ctx
/// / array_ty, or unresolvable base / idx, → 0. NULL `name` → empty. (#2100)
#[no_mangle]
pub unsafe extern "C" fn ry_emit_array_gep(
    ctx: *mut RyEmitCtx,
    array_ty: RyTypeRef,
    base_id: RyValueId,
    idx_id: RyValueId,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let Some(arr) = req_type(array_ty) else {
        return 0;
    };
    let (Some(base), Some(idx)) = (resolve_value(c, base_id), resolve_value(c, idx_id)) else {
        return 0;
    };
    let v = c.build_array_gep(arr, base, idx, name_or_empty(name));
    intern(c, to_ry_value(v.0))
}

/// Emit `extractvalue agg, idx` — read a single aggregate field by compile-time
/// index (`LLVMBuildExtractValue`), distinct from the pointer-addressing
/// `ry_emit_struct_gep`: this reads a field out of an aggregate *value*. NULL ctx
/// / unresolved agg → 0. NULL `name` → empty.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_extract_value(
    ctx: *mut RyEmitCtx,
    agg_id: RyValueId,
    idx: u32,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let Some(agg) = resolve_value(c, agg_id) else {
        return 0;
    };
    let v = c.build_extract_value(agg, idx, name_or_empty(name));
    intern(c, to_ry_value(v.0))
}

/// Emit `insertvalue agg, val, idx` — write `val` into aggregate field `idx`,
/// returning a new aggregate (`LLVMBuildInsertValue`). The symmetric partner of
/// `ry_emit_extract_value`. NULL ctx / unresolved agg / unresolved val → 0.
/// NULL `name` → empty. Added for #2186 (dense iterator sweep): Map iterator
/// next-fn builds the `{key, val}` tuple via `undef → insertvalue 0 → insertvalue 1`.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_insert_value(
    ctx: *mut RyEmitCtx,
    agg_id: RyValueId,
    val_id: RyValueId,
    idx: u32,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(agg), Some(val)) = (resolve_value(c, agg_id), resolve_value(c, val_id)) else {
        return 0;
    };
    let v = c.build_insert_value(agg, val, idx, name_or_empty(name));
    intern(c, to_ry_value(v.0))
}

/// Emit `zext val to dest_ty` — zero-extend `val` to the wider integer type
/// `dest_ty` (`LLVMBuildZExt`). NULL ctx / NULL dest_ty / unresolved val → 0.
/// NULL `name` → empty. Added for #2101 (hash-table lookup capability: the
/// conditional i1→i64 key widening in emitHashTableLookup). The NULL-type guard
/// fires before resolve_value, mirroring `ry_emit_load` / `ry_emit_gep`.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_zext(
    ctx: *mut RyEmitCtx,
    val_id: RyValueId,
    dest_ty: RyTypeRef,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let Some(dest) = req_type(dest_ty) else {
        return 0;
    };
    let Some(val) = resolve_value(c, val_id) else {
        return 0;
    };
    let v = c.build_zext(val, dest, name_or_empty(name));
    intern(c, to_ry_value(v.0))
}

/// Emit `trunc val to dest_ty` — truncate `val` to the narrower integer type
/// `dest_ty` (`LLVMBuildTrunc`). NULL ctx / NULL dest_ty / unresolved val → 0.
/// NULL `name` → empty. Added for #2194 (concurrency capability migration):
/// thread join i64→i1 bool unwrap, AtomicInt CAS status truncate, AtomicBool
/// load i64→i1. The NULL-type guard fires before resolve_value, mirroring
/// `ry_emit_zext`.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_trunc(
    ctx: *mut RyEmitCtx,
    val_id: RyValueId,
    dest_ty: RyTypeRef,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let Some(dest) = req_type(dest_ty) else {
        return 0;
    };
    let Some(val) = resolve_value(c, val_id) else {
        return 0;
    };
    let v = c.build_trunc(val, dest, name_or_empty(name));
    intern(c, to_ry_value(v.0))
}

/// Emit `sext val to dest_ty` — sign-extend `val` to the wider integer type
/// `dest_ty` (`LLVMBuildSExt`). Symmetric partner of `ry_emit_zext`. NULL ctx
/// / NULL dest_ty / unresolved val → 0. NULL `name` → empty. Added for #2192
/// (stdlib op follow-on sweep): `args()` widens the i32 result of
/// `__ry_args_count` to i64 for the list-length store. The NULL-type guard
/// fires before resolve_value, mirroring `ry_emit_zext` / `ry_emit_trunc`.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_sext(
    ctx: *mut RyEmitCtx,
    val_id: RyValueId,
    dest_ty: RyTypeRef,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let Some(dest) = req_type(dest_ty) else {
        return 0;
    };
    let Some(val) = resolve_value(c, val_id) else {
        return 0;
    };
    let v = c.build_sext(val, dest, name_or_empty(name));
    intern(c, to_ry_value(v.0))
}

/// Emit `icmp <predicate> lhs, rhs`. NULL ctx / unknown predicate / NULL operand → 0.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_icmp(
    ctx: *mut RyEmitCtx,
    predicate: c_int,
    lhs_id: RyValueId,
    rhs_id: RyValueId,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let Some(pred) = icmp_pred_from(predicate) else {
        return 0;
    };
    let (Some(lhs), Some(rhs)) = (resolve_value(c, lhs_id), resolve_value(c, rhs_id)) else {
        return 0;
    };
    let v = c.build_icmp(pred, lhs, rhs, name_or_empty(name));
    intern(c, to_ry_value(v.0))
}

/// Emit `and lhs, rhs`. NULL ctx / operand → 0.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_and(
    ctx: *mut RyEmitCtx,
    lhs_id: RyValueId,
    rhs_id: RyValueId,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(lhs), Some(rhs)) = (resolve_value(c, lhs_id), resolve_value(c, rhs_id)) else {
        return 0;
    };
    let v = c.build_and(lhs, rhs, name_or_empty(name));
    intern(c, to_ry_value(v.0))
}

/// Emit `or lhs, rhs`. NULL ctx / operand → 0.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_or(
    ctx: *mut RyEmitCtx,
    lhs_id: RyValueId,
    rhs_id: RyValueId,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(lhs), Some(rhs)) = (resolve_value(c, lhs_id), resolve_value(c, rhs_id)) else {
        return 0;
    };
    let v = c.build_or(lhs, rhs, name_or_empty(name));
    intern(c, to_ry_value(v.0))
}

/// Emit `xor lhs, rhs`. NULL ctx / operand → 0.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_xor(
    ctx: *mut RyEmitCtx,
    lhs_id: RyValueId,
    rhs_id: RyValueId,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(lhs), Some(rhs)) = (resolve_value(c, lhs_id), resolve_value(c, rhs_id)) else {
        return 0;
    };
    let v = c.build_xor(lhs, rhs, name_or_empty(name));
    intern(c, to_ry_value(v.0))
}

/// Emit `add lhs, rhs`. NULL ctx / operand → 0.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_add(
    ctx: *mut RyEmitCtx,
    lhs_id: RyValueId,
    rhs_id: RyValueId,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(lhs), Some(rhs)) = (resolve_value(c, lhs_id), resolve_value(c, rhs_id)) else {
        return 0;
    };
    let v = c.build_add(lhs, rhs, name_or_empty(name));
    intern(c, to_ry_value(v.0))
}

/// Emit `sub lhs, rhs`. NULL ctx / operand → 0.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_sub(
    ctx: *mut RyEmitCtx,
    lhs_id: RyValueId,
    rhs_id: RyValueId,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(lhs), Some(rhs)) = (resolve_value(c, lhs_id), resolve_value(c, rhs_id)) else {
        return 0;
    };
    let v = c.build_sub(lhs, rhs, name_or_empty(name));
    intern(c, to_ry_value(v.0))
}

/// Emit `mul lhs, rhs`. NULL ctx / operand → 0. Added for #2096 (string-build
/// op migration): str * n / join sep total / repeat byte_len * count multiply.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_mul(
    ctx: *mut RyEmitCtx,
    lhs_id: RyValueId,
    rhs_id: RyValueId,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(lhs), Some(rhs)) = (resolve_value(c, lhs_id), resolve_value(c, rhs_id)) else {
        return 0;
    };
    let v = c.build_mul(lhs, rhs, name_or_empty(name));
    intern(c, to_ry_value(v.0))
}

/// Emit `sdiv lhs, rhs` (signed). NULL ctx / operand → 0. Added for #2096:
/// emitStringRepeat's overflow-bound `INT64_MAX / strLen` divide. The C++ caller
/// gates `strLen > 0` upstream, so no division-by-zero path crosses the boundary.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_sdiv(
    ctx: *mut RyEmitCtx,
    lhs_id: RyValueId,
    rhs_id: RyValueId,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(lhs), Some(rhs)) = (resolve_value(c, lhs_id), resolve_value(c, rhs_id)) else {
        return 0;
    };
    let v = c.build_sdiv(lhs, rhs, name_or_empty(name));
    intern(c, to_ry_value(v.0))
}

/// Emit `select cond, then_v, else_v`. NULL ctx / operand → 0.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_select(
    ctx: *mut RyEmitCtx,
    cond_id: RyValueId,
    then_id: RyValueId,
    else_id: RyValueId,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(cond), Some(then_v), Some(else_v)) = (
        resolve_value(c, cond_id),
        resolve_value(c, then_id),
        resolve_value(c, else_id),
    ) else {
        return 0;
    };
    let v = c.build_select(cond, then_v, else_v, name_or_empty(name));
    intern(c, to_ry_value(v.0))
}

/// Materialize `LLVMConstInt(ty, value, sign_extend)` and return the interned
/// constant. NULL ctx / type → 0.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_const_int(
    ctx: *mut RyEmitCtx,
    ty: RyTypeRef,
    value: u64,
    sign_extend: c_int,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let Some(t) = req_type(ty) else {
        return 0;
    };
    let v = c.const_int(t, value, sign_extend != 0);
    intern(c, to_ry_value(v.0))
}

/// Materialize `LLVMConstNull(ty)` and return the interned constant. For
/// pointer types this is the typed null pointer used in null-guard `icmp eq`
/// comparisons (pilot G #2196). NULL ctx / type → 0.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_const_null(ctx: *mut RyEmitCtx, ty: RyTypeRef) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let Some(t) = req_type(ty) else {
        return 0;
    };
    let v = c.const_null(t);
    intern(c, to_ry_value(v.0))
}

/// Materialize `LLVMGetUndef(ty)` and return the interned undef value. Used as
/// the initial aggregate before `ry_emit_insert_value` fills each field. NULL ctx
/// / type → 0. Added for #2186 (dense iterator sweep): Map iterator next-fn
/// seeds its `{key, val}` tuple from undef.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_undef(ctx: *mut RyEmitCtx, ty: RyTypeRef) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let Some(t) = req_type(ty) else {
        return 0;
    };
    let v = c.build_undef(t);
    intern(c, to_ry_value(v.0))
}
