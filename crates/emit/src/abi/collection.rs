//! abi::collection — C boundary entry points for List mutation (append / insert
//! / remove_at / slice). Each resolves the u32 ids to raw values, wraps the type
//! handles, calls the matching `core` `EmitCtx` method, and interns any produced
//! value (intern / resolve are abi-side; the engine methods never touch them).
//! `ry_emit_list_slice` keeps its C out-param signature (`*mut RyValueId` ×2,
//! api.h-locked) and splits the engine's `SliceParts` aggregate into the two
//! out-params. Each entry point validates its inputs at the boundary (#2080):
//! `checked_cx` rejects a NULL ctx / context / module / builder, the required
//! ids resolve through `resolve_value`, the type handles are NULL-checked, and
//! `ry_emit_list_slice` rejects NULL out-params — malformed input becomes a
//! no-op / sentinel 0 instead of reaching the engine's `LLVMBuild*` calls.

use crate::core::TypeRef;

use super::*;

/// Append the interned `val_id` to the List at `list_ptr_id` (growing the buffer
/// in place); `list_header_ty` / `elem_ty` / `elem_size` describe the layout.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_collection_append(
    ctx: *mut RyEmitCtx,
    list_ptr_id: RyValueId,
    val_id: RyValueId,
    list_header_ty: RyTypeRef,
    elem_ty: RyTypeRef,
    elem_size: u64,
) {
    // boundary input validation: malformed callers are a no-op rather than
    // feeding NULL handles to the list-grow IR.
    let Some(c) = checked_cx(ctx) else {
        return;
    };
    let (Some(list_ptr), Some(val)) = (resolve_value(c, list_ptr_id), resolve_value(c, val_id))
    else {
        return;
    };
    if list_header_ty.is_null() || elem_ty.is_null() {
        return;
    }
    c.collection_append(
        list_ptr,
        val,
        TypeRef(as_type(list_header_ty)),
        TypeRef(as_type(elem_ty)),
        elem_size,
    );
}

/// Insert the interned `val_id` at `idx_id` into the List at `list_ptr_id`
/// (negative index wrapped, shifting the tail up).
#[no_mangle]
pub unsafe extern "C" fn ry_emit_collection_insert(
    ctx: *mut RyEmitCtx,
    list_ptr_id: RyValueId,
    idx_id: RyValueId,
    val_id: RyValueId,
    list_header_ty: RyTypeRef,
    elem_ty: RyTypeRef,
    elem_size: u64,
) {
    // boundary input validation: malformed callers are a no-op rather than
    // feeding NULL handles to the insert / tail-shift IR.
    let Some(c) = checked_cx(ctx) else {
        return;
    };
    let (Some(list_ptr), Some(orig_idx), Some(val)) = (
        resolve_value(c, list_ptr_id),
        resolve_value(c, idx_id),
        resolve_value(c, val_id),
    ) else {
        return;
    };
    if list_header_ty.is_null() || elem_ty.is_null() {
        return;
    }
    c.collection_insert(
        list_ptr,
        orig_idx,
        val,
        TypeRef(as_type(list_header_ty)),
        TypeRef(as_type(elem_ty)),
        elem_size,
    );
}

/// Remove the element at `idx_id` from the List at `list_ptr_id` and return the
/// interned removed value (negative index wrapped, shifting the tail down).
#[no_mangle]
pub unsafe extern "C" fn ry_emit_collection_remove_at(
    ctx: *mut RyEmitCtx,
    list_ptr_id: RyValueId,
    idx_id: RyValueId,
    list_header_ty: RyTypeRef,
    elem_ty: RyTypeRef,
    elem_size: u64,
) -> RyValueId {
    // boundary input validation: malformed callers get sentinel 0 rather than
    // feeding NULL handles to the remove / tail-shift IR.
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(list_ptr), Some(orig_idx)) =
        (resolve_value(c, list_ptr_id), resolve_value(c, idx_id))
    else {
        return 0;
    };
    if list_header_ty.is_null() || elem_ty.is_null() {
        return 0;
    }
    let removed = c.collection_remove_at(
        list_ptr,
        orig_idx,
        TypeRef(as_type(list_header_ty)),
        TypeRef(as_type(elem_ty)),
        elem_size,
    );
    intern(c, to_ry_value(removed.0))
}

/// Slice the List at `list_ptr_id` over `[start_id, end_excl_id)`; write the
/// interned clamped element count and the freshly-malloc'd sub-list buffer into
/// the `out_count` / `out_new_data` out-params (the engine's `SliceParts` split).
#[no_mangle]
pub unsafe extern "C" fn ry_emit_list_slice(
    ctx: *mut RyEmitCtx,
    list_ptr_id: RyValueId,
    start_id: RyValueId,
    end_excl_id: RyValueId,
    list_header_ty: RyTypeRef,
    elem_ty: RyTypeRef,
    elem_size: u64,
    out_count: *mut RyValueId,
    out_new_data: *mut RyValueId,
) {
    // boundary input validation: the two out-params are written unconditionally
    // on the happy path, so a NULL out-param (or ctx / handle) must reject before
    // the engine call rather than dereference a NULL write target.
    let Some(c) = checked_cx(ctx) else {
        return;
    };
    if out_count.is_null() || out_new_data.is_null() {
        return;
    }
    let (Some(list_ptr), Some(start_val), Some(end_excl_val)) = (
        resolve_value(c, list_ptr_id),
        resolve_value(c, start_id),
        resolve_value(c, end_excl_id),
    ) else {
        return;
    };
    if list_header_ty.is_null() || elem_ty.is_null() {
        return;
    }
    let parts = c.list_slice(
        list_ptr,
        start_val,
        end_excl_val,
        TypeRef(as_type(list_header_ty)),
        TypeRef(as_type(elem_ty)),
        elem_size,
    );
    *out_count = intern(c, to_ry_value(parts.count.0));
    *out_new_data = intern(c, to_ry_value(parts.new_data.0));
}
