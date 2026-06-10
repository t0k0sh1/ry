//! abi::bounds — C boundary entry points for index bounds-checking. Each
//! resolves the u32 ids / maps the `c_int` kind to the Rust-native `BoundsKind`,
//! calls the `core` `EmitCtx` method, and interns any produced value (intern /
//! resolve are abi-side; the engine methods never touch them). Since #2061
//! migrated `collection` to call `negative_index_wrap` / `bounds_error` as engine
//! methods directly, these externs have no in-crate caller and the `pub use
//! bounds::*` re-export in `abi.rs` was dropped; the C++ path is now their only
//! consumer (the `#[no_mangle]` symbols stay exported).

use std::ffi::{c_char, c_int};

use crate::core::BoundsKind;

use super::*;

// c_int → BoundsKind, preserving the legacy `== RY_BOUNDS_LIST` (else array)
// semantics exactly so the selected message — and thus the emitted IR — is
// bit-identical for every value the C++ side passes.
#[inline]
fn bounds_kind_from(kind: c_int) -> BoundsKind {
    if kind == RY_BOUNDS_LIST {
        BoundsKind::List
    } else {
        BoundsKind::Array
    }
}

/// Emit a bounds-check sequence for the interned index `idx_id` against length
/// `len_id` (List or Array per `kind`); return the interned wrapped index for the
/// caller's GEP. `global_name` / `bb_prefix` name the error string and generated
/// blocks. The generated blocks' parent function is derived from the builder's
/// insert block, so the builder must be positioned within a function.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_bounds_check(
    ctx: *mut RyEmitCtx,
    idx_id: RyValueId,
    len_id: RyValueId,
    kind: c_int,
    global_name: *const c_char,
    bb_prefix: *const c_char,
) -> RyValueId {
    // boundary input validation: malformed callers get sentinel 0. The string
    // params (global_name / bb_prefix) are NULL-safe in the engine (`cstr_bytes`
    // maps NULL → empty), so only ctx + the resolved operands need guarding.
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(idx), Some(len)) = (resolve_value(c, idx_id), resolve_value(c, len_id)) else {
        return 0;
    };
    let result = c.bounds_check(idx, len, bounds_kind_from(kind), global_name, bb_prefix);
    intern(c, to_ry_value(result.0))
}

/// Emit `wrapped = (idx < 0) ? idx + wrap_base : idx` for interned `idx_id` /
/// `wrap_base_id`; return the interned wrapped index (i64). `prefix` names the
/// generated blocks.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_negative_index_wrap(
    ctx: *mut RyEmitCtx,
    idx_id: RyValueId,
    wrap_base_id: RyValueId,
    prefix: *const c_char,
) -> RyValueId {
    // boundary input validation: malformed callers get sentinel 0 (`prefix` is
    // NULL-safe in the engine via `cstr_bytes`).
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(idx), Some(wrap_base)) = (resolve_value(c, idx_id), resolve_value(c, wrap_base_id))
    else {
        return 0;
    };
    let result = c.negative_index_wrap(idx, wrap_base, prefix);
    intern(c, to_ry_value(result.0))
}

/// Emit the out-of-bounds exit (`fprintf(stderr, fmt_msg, orig_idx, len)` → exit
/// → unreachable) for interned `orig_idx_id` / `len_id`. `fmt_msg` needs two
/// `%lld`; `global_name` hints the dedup'd format-string global. The caller must
/// split BBs around this call (it terminates the current block).
#[no_mangle]
pub unsafe extern "C" fn ry_emit_bounds_error(
    ctx: *mut RyEmitCtx,
    orig_idx_id: RyValueId,
    len_id: RyValueId,
    fmt_msg: *const c_char,
    global_name: *const c_char,
) {
    // boundary input validation: malformed callers are a no-op. The string params
    // (fmt_msg / global_name) are NULL-safe in the engine (`cstr_bytes` +
    // `bounds_error`'s explicit `global_name.is_null()` check).
    let Some(c) = checked_cx(ctx) else {
        return;
    };
    let (Some(orig_idx), Some(len)) = (resolve_value(c, orig_idx_id), resolve_value(c, len_id))
    else {
        return;
    };
    c.bounds_error(orig_idx, len, fmt_msg, global_name);
}
