//! abi::bounds — C boundary entry points for index bounds-checking. Each
//! resolves the u32 ids / maps the `c_int` kind to the Rust-native `BoundsKind`,
//! calls the `core` `EmitCtx` method, and interns any produced value (intern /
//! resolve are abi-side; the engine methods never touch them). The
//! negative-index-wrap and bounds-error externs stay reachable in-crate: legacy
//! `collection.rs` still calls them through `crate::abi::*` (the `pub use
//! bounds::*` in `abi.rs`), pending its own migration in #2061.

use std::ffi::{c_char, c_int};

use crate::core::{BoundsKind, ValueRef};

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

#[no_mangle]
pub unsafe extern "C" fn ry_emit_bounds_check(
    ctx: *mut RyEmitCtx,
    idx_id: RyValueId,
    len_id: RyValueId,
    kind: c_int,
    global_name: *const c_char,
    bb_prefix: *const c_char,
) -> RyValueId {
    let c = cx(ctx);
    let idx = ValueRef(as_value(resolve(c, idx_id)));
    let len = ValueRef(as_value(resolve(c, len_id)));
    let result = c.bounds_check(idx, len, bounds_kind_from(kind), global_name, bb_prefix);
    intern(c, to_ry_value(result.0))
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_negative_index_wrap(
    ctx: *mut RyEmitCtx,
    idx_id: RyValueId,
    wrap_base_id: RyValueId,
    prefix: *const c_char,
) -> RyValueId {
    let c = cx(ctx);
    let idx = ValueRef(as_value(resolve(c, idx_id)));
    let wrap_base = ValueRef(as_value(resolve(c, wrap_base_id)));
    let result = c.negative_index_wrap(idx, wrap_base, prefix);
    intern(c, to_ry_value(result.0))
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_bounds_error(
    ctx: *mut RyEmitCtx,
    orig_idx_id: RyValueId,
    len_id: RyValueId,
    fmt_msg: *const c_char,
    global_name: *const c_char,
) {
    let c = cx(ctx);
    let orig_idx = ValueRef(as_value(resolve(c, orig_idx_id)));
    let len = ValueRef(as_value(resolve(c, len_id)));
    c.bounds_error(orig_idx, len, fmt_msg, global_name);
}
