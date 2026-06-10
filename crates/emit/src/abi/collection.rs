//! abi::collection — C boundary entry points for List mutation (append / insert
//! / remove_at / slice). Each resolves the u32 ids to raw values, wraps the type
//! handles, calls the matching `core` `EmitCtx` method, and interns any produced
//! value (intern / resolve are abi-side; the engine methods never touch them).
//! `ry_emit_list_slice` keeps its C out-param signature (`*mut RyValueId` ×2,
//! api.h-locked) and splits the engine's `SliceParts` aggregate into the two
//! out-params. Behaviour is preserved verbatim from the pre-#2061 externs — no
//! new NULL guards (a pure relocation; hardening is a separate follow-up).

use crate::core::{TypeRef, ValueRef};

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
    let c = cx(ctx);
    let list_ptr = ValueRef(as_value(resolve(c, list_ptr_id)));
    let val = ValueRef(as_value(resolve(c, val_id)));
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
    let c = cx(ctx);
    let list_ptr = ValueRef(as_value(resolve(c, list_ptr_id)));
    let orig_idx = ValueRef(as_value(resolve(c, idx_id)));
    let val = ValueRef(as_value(resolve(c, val_id)));
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
    let c = cx(ctx);
    let list_ptr = ValueRef(as_value(resolve(c, list_ptr_id)));
    let orig_idx = ValueRef(as_value(resolve(c, idx_id)));
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
    let c = cx(ctx);
    let list_ptr = ValueRef(as_value(resolve(c, list_ptr_id)));
    let start_val = ValueRef(as_value(resolve(c, start_id)));
    let end_excl_val = ValueRef(as_value(resolve(c, end_excl_id)));
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
