//! abi::cow — C boundary entry point for the Copy-on-Write uniqueness check.
//! Resolves the u32 ids to `ValueRef`, maps the `c_int` kind → `CowKind` /
//! atomic → `Atomicity`, converts the flag fields (`c_int` → bool) and
//! `destructor_callee` (`RyValueRef` → `Option<ValueRef>`) into the Rust-native
//! `CowEnsureUnique`, calls the `core` `EmitCtx` method, and interns the
//! produced value (intern / resolve are abi-side; the engine method never
//! touches them). The element / key ARC retains and the old-header release are
//! performed by the engine method calling `arc_retain` / `arc_release`
//! directly, so — like #2061's collection — cow no longer calls the
//! `ry_emit_arc_*` externs by name and the `pub use arc::*` re-export in
//! `abi.rs` was dropped; the C++ path is now this extern's only consumer (the
//! `#[no_mangle]` symbol stays exported).

use std::ffi::c_int;

use crate::core::{Atomicity, CowKind, ValueRef};
use crate::cow::CowEnsureUnique;

use super::*;

// Nullable boundary handle → Option<ValueRef> (None on NULL). Mirrors the
// helper in `abi/arc.rs`; kept local per the per-module convention.
#[inline]
fn opt_value_ref(p: RyValueRef) -> Option<ValueRef> {
    if p.is_null() {
        None
    } else {
        Some(ValueRef(as_value(p)))
    }
}

// c_int → CowKind, preserving the legacy `RY_COW_LIST` / `_MAP` / `_SET`
// mapping exactly; any other value is rejected (the legacy body returned 0 on
// its `_ =>` match arm), so the emitted IR is bit-identical for every value the
// C++ side passes.
#[inline]
fn cow_kind_from(kind: c_int) -> Option<CowKind> {
    if kind == RY_COW_LIST {
        Some(CowKind::List)
    } else if kind == RY_COW_MAP {
        Some(CowKind::Map)
    } else if kind == RY_COW_SET {
        Some(CowKind::Set)
    } else {
        None
    }
}

/// Emit the Copy-on-Write uniqueness check for the slot described by `desc`: if
/// the collection is shared, deep-copy it, release the old header, and store the
/// new dataPtr; return the interned dataPtr PHI (original or copied), or 0 on a
/// NULL guard / invalid kind. See `RyCowEnsureUniqueDesc`. The new BBs' parent
/// function is derived from the builder's insert block, so the builder must be
/// positioned within a function.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_cow_ensure_unique(
    ctx: *mut RyEmitCtx,
    desc: *const RyCowEnsureUniqueDesc,
) -> RyValueId {
    if desc.is_null() {
        return 0;
    }
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let (Some(data_ptr), Some(slot_ptr)) = (
        resolve_value(c, (*desc).data_ptr_id),
        resolve_value(c, (*desc).slot_ptr_id),
    ) else {
        return 0;
    };
    let kind = match cow_kind_from((*desc).kind) {
        Some(k) => k,
        None => return 0,
    };
    let d = CowEnsureUnique {
        data_ptr,
        slot_ptr,
        kind,
        atomic: if (*desc).atomic == RY_ARC_ATOMIC {
            Atomicity::Atomic
        } else {
            Atomicity::NonAtomic
        },
        elem_size: (*desc).elem_size,
        key_size: (*desc).key_size,
        val_size: (*desc).val_size,
        do_elem_retain: (*desc).do_elem_retain != 0,
        elem_is_str: (*desc).elem_is_str != 0,
        do_key_retain: (*desc).do_key_retain != 0,
        key_is_str: (*desc).key_is_str != 0,
        destructor_callee: opt_value_ref((*desc).destructor_callee),
    };
    let result = c.cow_ensure_unique(&d);
    intern(c, to_ry_value(result.0))
}
