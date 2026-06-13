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

use std::ffi::c_int;

use crate::core::{ListCopyKind, TypeRef};

use super::*;

// c_int → ListCopyKind, preserving the `RY_LISTCOPY_KEYS` / `_VALUES` / `_TAKE`
// mapping; any other value is rejected (no-op / sentinel 0 at the caller).
#[inline]
fn list_copy_kind_from(kind: c_int) -> Option<ListCopyKind> {
    if kind == RY_LISTCOPY_KEYS {
        Some(ListCopyKind::Keys)
    } else if kind == RY_LISTCOPY_VALUES {
        Some(ListCopyKind::Values)
    } else if kind == RY_LISTCOPY_TAKE {
        Some(ListCopyKind::Take)
    } else {
        None
    }
}

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

/// Linear-search-and-memmove `remove(val)` on a List (#2095). `elem_is_str`
/// selects the `strcmp` comparison path (caller-side allowlist guards str-only
/// pointer elements before this call); `elem_is_double` selects FCmpOEQ;
/// otherwise plain ICmpEQ. Always returns the interned `i64 0` sentinel from
/// the engine method. Precondition: builder positioned within a function.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_list_remove(
    ctx: *mut RyEmitCtx,
    container_ptr_id: RyValueId,
    val_id: RyValueId,
    list_header_ty: RyTypeRef,
    list_elem_ty: RyTypeRef,
    elem_size: u64,
    elem_is_str: c_int,
    elem_is_double: c_int,
) -> RyValueId {
    // boundary input validation: malformed callers get sentinel 0 rather than
    // feeding NULL handles to the search-and-memmove IR.
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(container_ptr), Some(val)) =
        (resolve_value(c, container_ptr_id), resolve_value(c, val_id))
    else {
        return 0;
    };
    if list_header_ty.is_null() || list_elem_ty.is_null() {
        return 0;
    }
    let result = c.list_remove(
        container_ptr,
        val,
        TypeRef(as_type(list_header_ty)),
        TypeRef(as_type(list_elem_ty)),
        elem_size,
        elem_is_str != 0,
        elem_is_double != 0,
    );
    intern(c, to_ry_value(result.0))
}

/// O(n²) `distinct(list)` dedup (#2095). The caller pre-loads the source list
/// header (so the C++ baseline's instruction order — loadListHeader BEFORE
/// emitArcAllocCollectionHeader — is preserved byte-for-byte) and passes
/// `src_len` / `src_data` plus the pre-allocated ARC collection header
/// (`new_header_id`). This op writes data/cap/len fields + the inner-allocated
/// data buffer + dedup loop body. `elem_is_str` selects strcmp+ICmpEQ
/// (caller-side allowlist guards str-only pointer elements); `elem_is_double`
/// selects FCmpOEQ; otherwise plain ICmpEQ. The new_header lives on; this
/// returns nothing. Precondition: builder positioned within a function (BBs
/// are created inside the loops).
#[no_mangle]
pub unsafe extern "C" fn ry_emit_list_distinct(
    ctx: *mut RyEmitCtx,
    src_len_id: RyValueId,
    src_data_id: RyValueId,
    new_header_id: RyValueId,
    list_header_ty: RyTypeRef,
    elem_ty: RyTypeRef,
    elem_size: u64,
    elem_is_str: c_int,
    elem_is_double: c_int,
) {
    // boundary input validation: malformed callers no-op rather than feeding
    // NULL handles to the dedup loop IR.
    let Some(c) = checked_cx(ctx) else {
        return;
    };
    let (Some(src_len), Some(src_data), Some(new_header)) = (
        resolve_value(c, src_len_id),
        resolve_value(c, src_data_id),
        resolve_value(c, new_header_id),
    ) else {
        return;
    };
    if list_header_ty.is_null() || elem_ty.is_null() {
        return;
    }
    c.list_distinct(
        src_len,
        src_data,
        new_header,
        TypeRef(as_type(list_header_ty)),
        TypeRef(as_type(elem_ty)),
        elem_size,
        elem_is_str != 0,
        elem_is_double != 0,
    );
}

/// `enumerate(list)` → `List<(int, T)>` (#2095). Caller pre-loads the source
/// list header AND pre-allocates the ARC collection header (preserving the
/// C++ baseline's loadListHeader-then-emitArcAllocCollectionHeader order).
/// `retain_fn` is invoked per loop iteration with the loaded element value
/// id to emit `emitTupleComponentRetain`; NULL = no retain needed. The
/// two-step `with_ctx`-intern-then-callback rule (#2069/#2081) keeps the
/// callback outside any `&mut EmitCtx` borrow. Precondition: builder
/// positioned within a function.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_list_enumerate(
    ctx: *mut RyEmitCtx,
    src_len_id: RyValueId,
    src_data_id: RyValueId,
    new_header_id: RyValueId,
    list_header_ty: RyTypeRef,
    elem_ty: RyTypeRef,
    tuple_ty: RyTypeRef,
    tuple_size: u64,
    retain_fn: RyRetainFn,
    retain_user_ctx: *mut std::ffi::c_void,
) {
    // Mirror result.rs: decompose into Copy LLVM handles via with_ctx so the
    // engine function holds NO `&mut EmitCtx` borrow. The retain callback can
    // then re-enter cx without aliasing.
    if ctx.is_null() {
        return;
    }
    let context = with_ctx(ctx, |c| c.context);
    let b = with_ctx(ctx, |c| c.builder);
    let module = with_ctx(ctx, |c| c.module);
    if context.is_null() || b.is_null() || module.is_null() {
        return;
    }
    if list_header_ty.is_null() || elem_ty.is_null() || tuple_ty.is_null() {
        return;
    }
    let (Some(src_len), Some(src_data), Some(new_header)) = (
        with_ctx(ctx, |c| resolve_value(c, src_len_id)),
        with_ctx(ctx, |c| resolve_value(c, src_data_id)),
        with_ctx(ctx, |c| resolve_value(c, new_header_id)),
    ) else {
        return;
    };

    // Two-step closure: with_ctx intern, then call retain_fn OUTSIDE the
    // borrow. The intern handle is allowed to escape because callbacks may
    // re-enter cx — and the borrow has already been released by then.
    let mut do_retain = move |val: ValueRef| {
        if let Some(retain_fn) = retain_fn {
            unsafe {
                let val_id = with_ctx(ctx, |c| intern(c, to_ry_value(val.0)));
                retain_fn(val_id, retain_user_ctx);
            }
        }
    };
    let retain_arg: Option<&mut dyn FnMut(ValueRef)> =
        retain_fn.map(|_| &mut do_retain as &mut dyn FnMut(ValueRef));

    crate::collection::emit_list_enumerate(
        b,
        context,
        module,
        src_len,
        src_data,
        new_header,
        as_type(list_header_ty),
        as_type(elem_ty),
        as_type(tuple_ty),
        tuple_size,
        retain_arg,
    );
}

/// `reverse(list)` (#2095). Caller pre-loads list header, pre-allocates the
/// ARC collection header AND the data buffer (so `rev_dsize` / `rev_data`
/// land at the C++ baseline's positions); engine emits the reverse loop +
/// named StructGEPs `rev_new_len/cap/data` via LIST_FIELD_LEN/CAP/DATA. Post-
/// loop ARC retain dispatch stays C++-side (operates on ValueMetadata).
#[no_mangle]
pub unsafe extern "C" fn ry_emit_list_reverse(
    ctx: *mut RyEmitCtx,
    len_id: RyValueId,
    src_data_id: RyValueId,
    new_data_id: RyValueId,
    new_header_id: RyValueId,
    list_header_ty: RyTypeRef,
    elem_ty: RyTypeRef,
) {
    let Some(c) = checked_cx(ctx) else {
        return;
    };
    let (Some(len), Some(src_data), Some(new_data), Some(new_header)) = (
        resolve_value(c, len_id),
        resolve_value(c, src_data_id),
        resolve_value(c, new_data_id),
        resolve_value(c, new_header_id),
    ) else {
        return;
    };
    if list_header_ty.is_null() || elem_ty.is_null() {
        return;
    }
    let b = c.builder;
    let context = c.context;
    crate::collection::emit_list_reverse(
        b,
        context,
        len,
        src_data,
        new_data,
        new_header,
        as_type(list_header_ty),
        as_type(elem_ty),
    );
}

/// `items(map)` → `List<(K, V)>` (#2095). Caller pre-loads map header
/// (mf.len/keys/vals) and pre-allocates the ARC collection header. Engine
/// emits the loop + 2 retain callbacks + storeListHeaderFields.
#[allow(clippy::too_many_arguments)]
#[no_mangle]
pub unsafe extern "C" fn ry_emit_map_items(
    ctx: *mut RyEmitCtx,
    map_len_id: RyValueId,
    map_keys_id: RyValueId,
    map_vals_id: RyValueId,
    new_header_id: RyValueId,
    list_header_ty: RyTypeRef,
    key_ty: RyTypeRef,
    val_ty: RyTypeRef,
    tuple_ty: RyTypeRef,
    tuple_size: u64,
    retain_key_fn: RyRetainFn,
    retain_val_fn: RyRetainFn,
    retain_user_ctx: *mut std::ffi::c_void,
) {
    if ctx.is_null() {
        return;
    }
    let context = with_ctx(ctx, |c| c.context);
    let b = with_ctx(ctx, |c| c.builder);
    let module = with_ctx(ctx, |c| c.module);
    if context.is_null() || b.is_null() || module.is_null() {
        return;
    }
    if list_header_ty.is_null() || key_ty.is_null() || val_ty.is_null() || tuple_ty.is_null() {
        return;
    }
    let (Some(map_len), Some(map_keys), Some(map_vals), Some(new_header)) = (
        with_ctx(ctx, |c| resolve_value(c, map_len_id)),
        with_ctx(ctx, |c| resolve_value(c, map_keys_id)),
        with_ctx(ctx, |c| resolve_value(c, map_vals_id)),
        with_ctx(ctx, |c| resolve_value(c, new_header_id)),
    ) else {
        return;
    };

    let mut do_retain_key = move |val: ValueRef| {
        if let Some(retain_fn) = retain_key_fn {
            unsafe {
                let val_id = with_ctx(ctx, |c| intern(c, to_ry_value(val.0)));
                retain_fn(val_id, retain_user_ctx);
            }
        }
    };
    let mut do_retain_val = move |val: ValueRef| {
        if let Some(retain_fn) = retain_val_fn {
            unsafe {
                let val_id = with_ctx(ctx, |c| intern(c, to_ry_value(val.0)));
                retain_fn(val_id, retain_user_ctx);
            }
        }
    };
    let retain_key_arg: Option<&mut dyn FnMut(ValueRef)> =
        retain_key_fn.map(|_| &mut do_retain_key as &mut dyn FnMut(ValueRef));
    let retain_val_arg: Option<&mut dyn FnMut(ValueRef)> =
        retain_val_fn.map(|_| &mut do_retain_val as &mut dyn FnMut(ValueRef));

    crate::collection::emit_map_items(
        b,
        context,
        module,
        map_len,
        map_keys,
        map_vals,
        new_header,
        as_type(list_header_ty),
        as_type(key_ty),
        as_type(val_ty),
        as_type(tuple_ty),
        tuple_size,
        retain_key_arg,
        retain_val_arg,
    );
}

/// `zip(list1, list2)` → `List<(T1, T2)>` (#2095). Caller pre-computes
/// `min_len` (so the `zip_minlen` instruction lands BEFORE the ARC alloc per
/// the C++ baseline order) and pre-allocates the ARC collection header. The
/// engine emits the loop body + 2 retain callbacks + storeListHeaderFields.
#[allow(clippy::too_many_arguments)]
#[no_mangle]
pub unsafe extern "C" fn ry_emit_list_zip(
    ctx: *mut RyEmitCtx,
    min_len_id: RyValueId,
    data1_id: RyValueId,
    data2_id: RyValueId,
    new_header_id: RyValueId,
    list_header_ty: RyTypeRef,
    elem_ty1: RyTypeRef,
    elem_ty2: RyTypeRef,
    tuple_ty: RyTypeRef,
    tuple_size: u64,
    retain1_fn: RyRetainFn,
    retain2_fn: RyRetainFn,
    retain_user_ctx: *mut std::ffi::c_void,
) {
    if ctx.is_null() {
        return;
    }
    let context = with_ctx(ctx, |c| c.context);
    let b = with_ctx(ctx, |c| c.builder);
    let module = with_ctx(ctx, |c| c.module);
    if context.is_null() || b.is_null() || module.is_null() {
        return;
    }
    if list_header_ty.is_null() || elem_ty1.is_null() || elem_ty2.is_null() || tuple_ty.is_null() {
        return;
    }
    let (Some(min_len), Some(data1), Some(data2), Some(new_header)) = (
        with_ctx(ctx, |c| resolve_value(c, min_len_id)),
        with_ctx(ctx, |c| resolve_value(c, data1_id)),
        with_ctx(ctx, |c| resolve_value(c, data2_id)),
        with_ctx(ctx, |c| resolve_value(c, new_header_id)),
    ) else {
        return;
    };

    let mut do_retain1 = move |val: ValueRef| {
        if let Some(retain_fn) = retain1_fn {
            unsafe {
                let val_id = with_ctx(ctx, |c| intern(c, to_ry_value(val.0)));
                retain_fn(val_id, retain_user_ctx);
            }
        }
    };
    let mut do_retain2 = move |val: ValueRef| {
        if let Some(retain_fn) = retain2_fn {
            unsafe {
                let val_id = with_ctx(ctx, |c| intern(c, to_ry_value(val.0)));
                retain_fn(val_id, retain_user_ctx);
            }
        }
    };
    let retain1_arg: Option<&mut dyn FnMut(ValueRef)> =
        retain1_fn.map(|_| &mut do_retain1 as &mut dyn FnMut(ValueRef));
    let retain2_arg: Option<&mut dyn FnMut(ValueRef)> =
        retain2_fn.map(|_| &mut do_retain2 as &mut dyn FnMut(ValueRef));

    crate::collection::emit_list_zip(
        b,
        context,
        module,
        min_len,
        data1,
        data2,
        new_header,
        as_type(list_header_ty),
        as_type(elem_ty1),
        as_type(elem_ty2),
        as_type(tuple_ty),
        tuple_size,
        retain1_arg,
        retain2_arg,
    );
}

/// Two-pass `flat(list<list<T>>)` (#2095). The caller pre-loads the outer
/// list header (`outer_len_id` / `outer_data_id`) — preserving the C++
/// baseline's `loadListHeader` order — and this op runs pass 1 (sum inner
/// lengths), inline ARC alloc (mirror `emitArcAllocCollectionHeader`),
/// inner data buffer malloc + storeListHeaderFields, and pass 2 (memcpy
/// each inner). Returns the interned new collection header pointer; the
/// caller must register it in `arc_owned_values_` to match the original
/// `emitArcGetDataPtr` bookkeeping. Precondition: builder positioned
/// within a function.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_list_flatten(
    ctx: *mut RyEmitCtx,
    outer_len_id: RyValueId,
    outer_data_id: RyValueId,
    list_header_ty: RyTypeRef,
    arc_header_ty: RyTypeRef,
    inner_elem_ty: RyTypeRef,
    inner_elem_size: u64,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(outer_len), Some(outer_data)) = (
        resolve_value(c, outer_len_id),
        resolve_value(c, outer_data_id),
    ) else {
        return 0;
    };
    if list_header_ty.is_null() || arc_header_ty.is_null() || inner_elem_ty.is_null() {
        return 0;
    }
    let new_header = c.list_flatten(
        outer_len,
        outer_data,
        TypeRef(as_type(list_header_ty)),
        TypeRef(as_type(arc_header_ty)),
        TypeRef(as_type(inner_elem_ty)),
        inner_elem_size,
    );
    intern(c, to_ry_value(new_header.0))
}

/// Single-source full-buffer copy for `keys` / `values` / `take` (#2093): malloc
/// `count * elem_size` bytes and memcpy the whole `src_data` range into it,
/// returning the interned new buffer. `kind` selects the call site so the SSA
/// names stay byte-identical. Header alloc, ARC retain, and metadata propagation
/// stay on the codegen side (after this returns).
#[no_mangle]
pub unsafe extern "C" fn ry_emit_list_copy_full(
    ctx: *mut RyEmitCtx,
    src_data_id: RyValueId,
    count_id: RyValueId,
    elem_size: u64,
    kind: c_int,
) -> RyValueId {
    // boundary input validation: malformed callers get sentinel 0 rather than
    // feeding NULL handles / an unknown kind to the malloc+memcpy IR.
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(src_data), Some(count)) = (resolve_value(c, src_data_id), resolve_value(c, count_id))
    else {
        return 0;
    };
    let Some(k) = list_copy_kind_from(kind) else {
        return 0;
    };
    let new_data = c.list_copy_full(src_data, count, elem_size, k);
    intern(c, to_ry_value(new_data.0))
}

/// Non-destructive `appended` copy (#2093): malloc `new_len * elem_size` bytes
/// and memcpy the `old_len`-element source range, returning the interned new
/// buffer. The appended element is stored by the codegen side after its retain
/// loop, so it is not part of this op.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_list_appended(
    ctx: *mut RyEmitCtx,
    new_len_id: RyValueId,
    old_len_id: RyValueId,
    src_data_id: RyValueId,
    elem_size: u64,
) -> RyValueId {
    // boundary input validation: malformed callers get sentinel 0.
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(new_len), Some(old_len), Some(src_data)) = (
        resolve_value(c, new_len_id),
        resolve_value(c, old_len_id),
        resolve_value(c, src_data_id),
    ) else {
        return 0;
    };
    let new_data = c.list_appended_copy(new_len, old_len, src_data, elem_size);
    intern(c, to_ry_value(new_data.0))
}

/// Two-source List concat copy (#2093): malloc `new_len * elem_size` bytes, then
/// memcpy the lhs buffer at offset 0 and the rhs buffer at the element-typed GEP
/// offset `lhs_len`, returning the interned new buffer. `elem_ty` is required for
/// the mid-buffer GEP. ARC retain + metadata propagation stay on the codegen side.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_list_concat(
    ctx: *mut RyEmitCtx,
    lhs_len_id: RyValueId,
    lhs_data_id: RyValueId,
    rhs_len_id: RyValueId,
    rhs_data_id: RyValueId,
    new_len_id: RyValueId,
    elem_ty: RyTypeRef,
    elem_size: u64,
) -> RyValueId {
    // boundary input validation: malformed callers get sentinel 0 rather than
    // feeding NULL handles to the malloc / two-memcpy / GEP IR.
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(lhs_len), Some(lhs_data), Some(rhs_len), Some(rhs_data), Some(new_len)) = (
        resolve_value(c, lhs_len_id),
        resolve_value(c, lhs_data_id),
        resolve_value(c, rhs_len_id),
        resolve_value(c, rhs_data_id),
        resolve_value(c, new_len_id),
    ) else {
        return 0;
    };
    if elem_ty.is_null() {
        return 0;
    }
    let new_data = c.list_concat_copy(
        new_len,
        lhs_len,
        lhs_data,
        rhs_len,
        rhs_data,
        TypeRef(as_type(elem_ty)),
        elem_size,
    );
    intern(c, to_ry_value(new_data.0))
}
