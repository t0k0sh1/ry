//! abi::any — C boundary entry points for dynamic `any` boxing. Each shell
//! resolves the u32 ids to `ValueRef`, maps the `c_int` kind → the matching
//! `AnyWrapKind` / `AnyUnwrapKind` / `AnyTryUnwrapKind` (folding the legacy
//! `kind > N` reject into the `None → 0` arm), converts the flag fields and the
//! nullable branch handles into the Rust-native `AnyWrap` / `AnyUnwrap` /
//! `AnyTryUnwrap`, calls the `core` `EmitCtx` method, and interns the produced
//! value. The kind-independent guards (ctx/desc/context/module/builder null, the
//! always-present val / type handles) live here; the branch-specific guards stay
//! in the engine method, which returns `None` on failure → the shell interns 0
//! (the descriptor-passing pattern #2062, thin-shell + fallible-core split). The
//! retain guard calls `arc_retain` as an engine method directly, so — like #2062
//! — `any` no longer reaches the `ry_emit_arc_*` externs by name; `any` was the
//! last in-crate `use crate::abi` consumer, so `abi.rs` also dropped its dead
//! `pub(crate) use crate::context::cstr_bytes;` re-export (whose sole remaining
//! caller was the legacy `any.rs`).

use std::ffi::c_int;

use crate::composite::any::{AnyTryUnwrap, AnyUnwrap, AnyWrap};
use crate::context::{AnyTryUnwrapKind, AnyUnwrapKind, AnyWrapKind, TypeRef};

use super::*;

// c_int → AnyWrapKind, mirroring the legacy dispatch exactly: `kind > 2` was
// rejected (→ None → 0); `1`/`2` select RecordBox/EnumBox; every other accepted
// value (0, and — as the legacy `else` did — any negative) is NonBox.
#[inline]
fn any_wrap_kind_from(kind: c_int) -> Option<AnyWrapKind> {
    match kind {
        _ if kind > 2 => None,
        1 => Some(AnyWrapKind::RecordBox),
        2 => Some(AnyWrapKind::EnumBox),
        _ => Some(AnyWrapKind::NonBox),
    }
}

// c_int → AnyUnwrapKind (legacy `kind > 2` reject; `2` → Record, `1` →
// F64Promote, else Standard).
#[inline]
fn any_unwrap_kind_from(kind: c_int) -> Option<AnyUnwrapKind> {
    match kind {
        _ if kind > 2 => None,
        2 => Some(AnyUnwrapKind::Record),
        1 => Some(AnyUnwrapKind::F64Promote),
        _ => Some(AnyUnwrapKind::Standard),
    }
}

// c_int → AnyTryUnwrapKind (legacy `kind > 1` reject; `1` → F64Promote, else
// Standard).
#[inline]
fn any_try_unwrap_kind_from(kind: c_int) -> Option<AnyTryUnwrapKind> {
    match kind {
        _ if kind > 1 => None,
        1 => Some(AnyTryUnwrapKind::F64Promote),
        _ => Some(AnyTryUnwrapKind::Standard),
    }
}

// Nullable boundary type handle → Option<TypeRef> (None on NULL). Branch-specific
// type handles (target / box_layout / record_struct) are null for the kinds that
// do not use them; the engine arm that needs one rejects None.
#[inline]
fn opt_type(p: RyTypeRef) -> Option<TypeRef> {
    if p.is_null() {
        None
    } else {
        Some(TypeRef(as_type(p)))
    }
}

/// Wrap a value into `any` per `desc` (NonBox / RecordBox / EnumBox); return the
/// interned boxed value, or 0 on a NULL guard or invalid kind. See
/// `RyAnyWrapDesc` for the fields.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_any_wrap(
    ctx: *mut RyEmitCtx,
    desc: *const RyAnyWrapDesc,
) -> RyValueId {
    if desc.is_null() {
        return 0;
    }
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    if (*desc).any_ty.is_null() {
        return 0;
    }
    let kind = match any_wrap_kind_from((*desc).kind) {
        Some(k) => k,
        None => return 0,
    };
    let Some(val) = resolve_value(c, (*desc).val_id) else {
        return 0;
    };
    let d = AnyWrap {
        kind,
        target_tag: (*desc).target_tag,
        val,
        do_collection_retain: (*desc).do_collection_retain != 0,
        do_str_retain: (*desc).do_str_retain != 0,
        descriptor: resolve_value(c, (*desc).descriptor_id),
        box_layout_ty: opt_type((*desc).box_layout_ty),
        box_data_size: (*desc).box_data_size,
        any_ty: TypeRef(as_type((*desc).any_ty)),
    };
    match c.any_wrap(&d) {
        Some(v) => intern(c, to_ry_value(v.0)),
        None => 0,
    }
}

/// Unwrap an `any` to a concrete type per `desc` (Standard / F64Promote /
/// Record), emitting the tag / descriptor check and the mismatch trap; return
/// the interned value, or 0 on a guard failure. See `RyAnyUnwrapDesc`.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_any_unwrap(
    ctx: *mut RyEmitCtx,
    desc: *const RyAnyUnwrapDesc,
) -> RyValueId {
    if desc.is_null() {
        return 0;
    }
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    if (*desc).any_ty.is_null() {
        return 0;
    }
    let kind = match any_unwrap_kind_from((*desc).kind) {
        Some(k) => k,
        None => return 0,
    };
    let Some(any_val) = resolve_value(c, (*desc).any_val_id) else {
        return 0;
    };
    let d = AnyUnwrap {
        kind,
        any_val,
        any_ty: TypeRef(as_type((*desc).any_ty)),
        target_ty: opt_type((*desc).target_ty),
        expected_tag: (*desc).expected_tag,
        do_collection_retain: (*desc).do_collection_retain != 0,
        do_str_retain: (*desc).do_str_retain != 0,
        mismatch_msg: (*desc).mismatch_msg,
        mismatch_global_name: (*desc).mismatch_global_name,
        expected_desc: resolve_value(c, (*desc).expected_desc_id),
        box_layout_ty: opt_type((*desc).box_layout_ty),
        record_struct_ty: opt_type((*desc).record_struct_ty),
        desc_mismatch_msg: (*desc).desc_mismatch_msg,
        desc_mismatch_global_name: (*desc).desc_mismatch_global_name,
    };
    match c.any_unwrap(&d) {
        Some(v) => intern(c, to_ry_value(v.0)),
        None => 0,
    }
}

/// Fallibly unwrap an `any` per `desc` (Standard / F64Promote) into a
/// `Result<T, Error>`; return the interned Result, or 0 on a guard failure. See
/// `RyAnyTryUnwrapDesc`.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_any_try_unwrap(
    ctx: *mut RyEmitCtx,
    desc: *const RyAnyTryUnwrapDesc,
) -> RyValueId {
    if desc.is_null() {
        return 0;
    }
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    if (*desc).any_ty.is_null() || (*desc).res_ty.is_null() || (*desc).error_ty.is_null() {
        return 0;
    }
    let kind = match any_try_unwrap_kind_from((*desc).kind) {
        Some(k) => k,
        None => return 0,
    };
    let (Some(any_val), Some(err_msg_str)) = (
        resolve_value(c, (*desc).any_val_id),
        resolve_value(c, (*desc).err_msg_str_id),
    ) else {
        return 0;
    };
    let d = AnyTryUnwrap {
        kind,
        any_val,
        any_ty: TypeRef(as_type((*desc).any_ty)),
        res_ty: TypeRef(as_type((*desc).res_ty)),
        error_ty: TypeRef(as_type((*desc).error_ty)),
        target_ty: opt_type((*desc).target_ty),
        expected_tag: (*desc).expected_tag,
        do_collection_retain: (*desc).do_collection_retain != 0,
        do_str_retain: (*desc).do_str_retain != 0,
        err_msg_str,
    };
    match c.any_try_unwrap(&d) {
        Some(v) => intern(c, to_ry_value(v.0)),
        None => 0,
    }
}
