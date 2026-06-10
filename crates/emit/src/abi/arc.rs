//! abi::arc — the C boundary entry points for ARC retain / release (migrated
//! #2057 pilot). Each resolves the u32 header id and translates the C `c_int`
//! atomic flag / nullable callees into the Rust-native `Atomicity` /
//! `Option<ValueRef>`, then calls the `core` engine method on `EmitCtx` (the
//! convergence point shared with the Rust-direct path in `any.rs`).

use std::ffi::c_int;

use crate::core::{Atomicity, ValueRef};

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
