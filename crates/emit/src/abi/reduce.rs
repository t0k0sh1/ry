//! abi::reduce — C boundary entry points for the numeric reduce builtins
//! (`sum` / `min` / `max`, #2092). Each resolves the u32 value ids to raw
//! `ValueRef`s, wraps the element / list-header type handles, calls the matching
//! `core` `EmitCtx` method, and interns the produced value (intern / resolve are
//! abi-side; the engine methods never touch them). Each entry validates its
//! inputs at the boundary (#2080): `checked_cx` rejects a NULL ctx / context /
//! module / builder, the value ids resolve through `resolve_value`, and the type
//! handles are NULL-checked — malformed input returns the sentinel 0 instead of
//! reaching the engine's `LLVMBuild*` calls.
//!
//! Four entries, mirroring the two op shapes in `crate::reduce`: the list forms
//! (`ry_emit_reduce_sum_list`, `ry_emit_reduce_minmax_list_loop`) emit a whole
//! loop; the variadic forms (`ry_emit_reduce_sum_step`, `ry_emit_reduce_minmax_step`)
//! emit one fold step the C++ loop drives per argument. `minmax_list_loop` is the
//! seed+loop only — the empty-list guard + `emitRuntimeError` stay C++-side
//! (ARC-string-global machinery is out of scope for this ARC-free batch).

use std::ffi::c_int;

use crate::context::TypeRef;

use super::*;

/// `sum([..])`: emit the list-sum loop over the List at `list_ptr_id`; return the
/// interned `sum_result`. `elem_ty` is the element LLVM type (i64 / f64 / i8),
/// `list_header_ty` is `CodeGen::listHeaderTy_`.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_reduce_sum_list(
    ctx: *mut RyEmitCtx,
    list_ptr_id: RyValueId,
    elem_ty: RyTypeRef,
    list_header_ty: RyTypeRef,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let Some(list_ptr) = resolve_value(c, list_ptr_id) else {
        return 0;
    };
    if elem_ty.is_null() || list_header_ty.is_null() {
        return 0;
    }
    let result = c.reduce_sum_list(
        list_ptr,
        TypeRef(as_type(elem_ty)),
        TypeRef(as_type(list_header_ty)),
    );
    intern(c, to_ry_value(result.0))
}

/// `sum(a, b, ..)`: emit one fold step `acc + v` (named `sum_v`); return the
/// interned result. `elem_ty` selects FAdd (f64) vs Add (int).
#[no_mangle]
pub unsafe extern "C" fn ry_emit_reduce_sum_step(
    ctx: *mut RyEmitCtx,
    acc_id: RyValueId,
    val_id: RyValueId,
    elem_ty: RyTypeRef,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(acc), Some(v)) = (resolve_value(c, acc_id), resolve_value(c, val_id)) else {
        return 0;
    };
    if elem_ty.is_null() {
        return 0;
    }
    let result = c.reduce_sum_step(acc, v, TypeRef(as_type(elem_ty)));
    intern(c, to_ry_value(result.0))
}

/// `min/max([..])`: emit the seed + loop at the `mm.ok` block the caller is
/// positioned at (the empty-list guard + error stay C++-side). `data_id` /
/// `len_id` are the C++-loaded `mm_data` / `mm_len`; `is_max != 0` selects max.
/// Returns the interned `mm_result`.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_reduce_minmax_list_loop(
    ctx: *mut RyEmitCtx,
    data_id: RyValueId,
    len_id: RyValueId,
    elem_ty: RyTypeRef,
    is_max: c_int,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(data), Some(len)) = (resolve_value(c, data_id), resolve_value(c, len_id)) else {
        return 0;
    };
    if elem_ty.is_null() {
        return 0;
    }
    let result = c.reduce_minmax_list_loop(data, len, TypeRef(as_type(elem_ty)), is_max != 0);
    intern(c, to_ry_value(result.0))
}

/// `min/max(a, b, ..)`: emit one fold step `mm_cmp = cmp(v, best)` +
/// `mm_best = select(mm_cmp, v, best)`; return the interned result. `elem_ty`
/// selects FCmp (f64) vs ICmp (int); `is_max != 0` selects max.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_reduce_minmax_step(
    ctx: *mut RyEmitCtx,
    best_id: RyValueId,
    val_id: RyValueId,
    elem_ty: RyTypeRef,
    is_max: c_int,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(best), Some(v)) = (resolve_value(c, best_id), resolve_value(c, val_id)) else {
        return 0;
    };
    if elem_ty.is_null() {
        return 0;
    }
    let result = c.reduce_minmax_step(best, v, TypeRef(as_type(elem_ty)), is_max != 0);
    intern(c, to_ry_value(result.0))
}
