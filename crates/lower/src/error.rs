//! Thread-local error channel for `ry_lower_*` entries.
//!
//! Cross-stage boundary contract (established by #2483 Stage 1):
//!
//! - Every `ry_lower_*` entry that can fail must call `clear_last_error()`
//!   as its first operation. This guarantees that a returned `RyValueId = 0`
//!   always pairs with a message set *inside that same call* — never a
//!   stale message from a prior `ry_lower_*` invocation.
//! - On failure the body calls `set_last_error(msg)` and returns `0`.
//! - The C++ caller checks `id == 0` and calls `ry_lower_get_last_error()`
//!   to retrieve the message, then immediately copies it into a `std::string`
//!   and throws via `codegenError`. The pointer is invalidated by the next
//!   `ry_lower_*` call on the same thread, so the copy must happen before
//!   any further boundary call.
//!
//! Thread-local storage is correct because `EmitCtx` is documented as
//! single-threaded (`crates/emit/src/context.rs`); the boundary inherits
//! that contract.

use core::ffi::c_char;
use std::cell::RefCell;
use std::ffi::CString;

thread_local! {
    static LAST_ERROR: RefCell<Option<CString>> = const { RefCell::new(None) };
}

/// Record a diagnostic message in this thread's slot. Called by lowering
/// bodies immediately before returning `RyValueId = 0`. NUL bytes in the
/// input are replaced with a placeholder message so `CString::new` cannot
/// fail; callers don't have to think about that case.
pub(crate) fn set_last_error(msg: &str) {
    let cs = CString::new(msg)
        .unwrap_or_else(|_| CString::new("(error message contained NUL byte)").unwrap());
    LAST_ERROR.with(|e| *e.borrow_mut() = Some(cs));
}

/// Clear this thread's slot. Each `ry_lower_*` entry calls this as its first
/// operation so `RyValueId = 0` paired with `ry_lower_get_last_error()`
/// always reflects a failure inside the *current* call.
pub(crate) fn clear_last_error() {
    LAST_ERROR.with(|e| *e.borrow_mut() = None);
}

/// Return the most recently set error message for this thread as a
/// NUL-terminated C string, or NULL if none was set since the last
/// `clear_last_error()` (which happens at the start of each `ry_lower_*`
/// entry).
///
/// # Safety
///
/// The returned pointer is owned by this thread's `LAST_ERROR` cell and
/// remains valid only until the next `ry_lower_*` call on the same thread
/// (which clears or overwrites the slot). The C++ caller must copy the
/// string before issuing another boundary call.
#[no_mangle]
pub unsafe extern "C" fn ry_lower_get_last_error() -> *const c_char {
    LAST_ERROR.with(|e| {
        e.borrow()
            .as_ref()
            .map_or(core::ptr::null(), |cs| cs.as_ptr())
    })
}
