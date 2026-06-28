//! abi::arc — the C boundary entry points for ARC retain / release (migrated
//! #2057 pilot). Each resolves the u32 header id and translates the C `c_int`
//! atomic flag / nullable callees into the Rust-native `Atomicity` /
//! `Option<ValueRef>`, then calls the `core` engine method on `EmitCtx` (the
//! convergence point shared with the Rust-direct path in `any.rs`).
//!
//! Stage 2 (#2484) added `ry_emit_build_arc_global`: the StringHeader-prefixed
//! ARC-immortal global builder reached by the lower crate's string / regex
//! literal lowering. Uncached at this layer — the lower crate manages two
//! independent caches (string + regex) to preserve the regex separation
//! invariant (#306) that `addResourceKind(rk_regex)` would otherwise violate
//! by poisoning a string global of identical bytes.

use std::ffi::c_int;

use crate::composite::arc::build_arc_global_raw;
use crate::context::{Atomicity, ValueRef};

use super::*;

// c_int → Atomicity, preserving the legacy `== RY_ARC_ATOMIC` (else =
// non-atomic) semantics exactly, so the emitted IR is bit-identical for every
// value the C++ side passes.
#[inline]
fn atomicity_from(atomic: c_int) -> Atomicity {
    if atomic == RY_ARC_ATOMIC {
        Atomicity::Atomic
    } else {
        Atomicity::NonAtomic
    }
}

// Nullable boundary handle → Option<ValueRef> (None on NULL).
#[inline]
fn opt_value_ref(p: RyValueRef) -> Option<ValueRef> {
    if p.is_null() {
        None
    } else {
        Some(ValueRef(as_value(p)))
    }
}

/// Emit an ARC retain on the header at `header_ptr_id` (`atomic` selects atomic
/// vs non-atomic); immortal objects are skipped. No-op on NULL ctx.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_arc_retain(
    ctx: *mut RyEmitCtx,
    header_ptr_id: RyValueId,
    atomic: c_int,
) {
    // boundary input validation: NULL ctx / handles → no-op. An invalid id that
    // resolves to a NULL header must not reach the retain IR (#2080).
    let Some(c) = checked_cx(ctx) else {
        return;
    };
    let Some(header) = resolve_value(c, header_ptr_id) else {
        return;
    };
    c.arc_retain(header, atomicity_from(atomic));
}

/// Emit an ARC release on the header at `header_ptr_id` (`atomic` mode),
/// invoking `destructor_callee` / `gc_visit_fn` when non-NULL on the free path.
/// No-op on NULL ctx.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_arc_release(
    ctx: *mut RyEmitCtx,
    header_ptr_id: RyValueId,
    atomic: c_int,
    destructor_callee: RyValueRef,
    gc_visit_fn: RyValueRef,
) {
    // boundary input validation: NULL ctx / handles → no-op. An invalid id that
    // resolves to a NULL header must not reach the release IR (#2080). The
    // destructor / gc-visit callees stay nullable (mapped to Option below).
    let Some(c) = checked_cx(ctx) else {
        return;
    };
    let Some(header) = resolve_value(c, header_ptr_id) else {
        return;
    };
    c.arc_release(
        header,
        atomicity_from(atomic),
        opt_value_ref(destructor_callee),
        opt_value_ref(gc_visit_fn),
    );
}

/// Emit an inline `atomicrmw add @__ry_arc_counter, delta monotonic` on the
/// process-global ARC live-count counter. No-op on NULL ctx. Added for #2190
/// (weak release free path needs the same accounting bump as the strong release
/// free path inside ry_emit_arc_release).
#[no_mangle]
pub unsafe extern "C" fn ry_emit_arc_counter_delta(ctx: *mut RyEmitCtx, delta: i64) {
    let Some(c) = checked_cx(ctx) else {
        return;
    };
    c.arc_counter_delta(delta);
}

/// Build a StringHeader-prefixed ARC-immortal global for the byte range
/// `bytes[0..len)` and return the interned `RyValueId` of the
/// in-bounds-GEP `ptr` to its first payload byte. `name_hint_bytes[0..
/// name_hint_len)` becomes the LLVM global name prefix (the helper appends
/// `".arc"`). No cache lookup at this layer — the lower crate (Stage 2
/// #2484) owns the per-cache dedup that preserves the regex separation
/// invariant (`addResourceKind(rk_regex)` must not poison a string global
/// of identical bytes, #306). Every call allocates a fresh global.
///
/// Length-passed name (not NUL-terminated) — `llvm::Twine::toStringRef`
/// does NOT guarantee a NUL terminator for multi-node Twines, so the C++
/// `cachedGlobalString` wrapper forwards the StringRef length explicitly
/// rather than relying on a NUL contract.
///
/// NULL ctx → 0. NULL `bytes` with non-zero `len` → 0. NULL
/// `name_hint_bytes` with non-zero `name_hint_len` → 0. Zero-length
/// `name_hint_len` is permitted (equivalent to an empty hint).
#[no_mangle]
pub unsafe extern "C" fn ry_emit_build_arc_global(
    ctx: *mut RyEmitCtx,
    bytes: *const u8,
    len: usize,
    name_hint_bytes: *const u8,
    name_hint_len: usize,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    // Borrow both byte ranges as slices under the C `(ptr, count)`
    // contract — empty (len=0) permits a NULL pointer, otherwise NULL
    // is the entry's invalid sentinel. Mirrors `ffi_slice` in
    // `crates/emit/src/abi.rs`, but with `usize` rather than `u32`
    // because the Stage 2 boundary uses `size_t`-typed lengths
    // (matching the C++ `std::string::size()` source).
    #[inline]
    unsafe fn opt_slice<'a>(p: *const u8, n: usize) -> Option<&'a [u8]> {
        if n == 0 {
            Some(&[])
        } else if p.is_null() {
            None
        } else {
            Some(std::slice::from_raw_parts(p, n))
        }
    }
    let (Some(slice), Some(name)) = (
        opt_slice(bytes, len),
        opt_slice(name_hint_bytes, name_hint_len),
    ) else {
        return 0;
    };
    let v = build_arc_global_raw(c, slice, name);
    intern(c, to_ry_value(v))
}
