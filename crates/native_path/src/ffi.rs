//! Host-side FFI: the one allocation shim path needs, plus the inline
//! `string_byte_len` helper for reading the byte length from received
//! str handles.
//!
//! Discipline (#2282 §3, "ハンドル受領のみ"):
//! - allocation goes through host shims (`__ry_host_make_string`); Rust
//!   never fabricates a StringHeader
//! - reads (`byte_len` from a str handle) go through the layout-pinned
//!   `STRING_BYTELEN_OFFSET = 8` constant, mirroring the C++
//!   `static_assert` in `tests/test_abi_layout.cpp` (#1995)
//!
//! Path does not allocate via `__ry_host_make_string_uninit` (`core::join2`
//! returns a `Vec<u8>` that gets copied through `__ry_host_make_string`
//! once at the abi boundary), and does not touch IOList — so this is a
//! strict subset of the base64 pilot's `ffi.rs`.

use std::ffi::c_char;

extern "C" {
    pub fn __ry_host_make_string(src: *const c_char, byte_len: i64) -> *const c_char;
}

// ── StringHeader byte_len read (include/ry/ry_layout.hpp) ──────────────────
//
// A Ry `str` handle points at the `data` byte; `byte_len` lives at
// `handle - STRING_BYTELEN_OFFSET` (= -8). The C++ side pins
// `STRING_BYTELEN_OFFSET == 8` via `static_assert` in
// `tests/test_abi_layout.cpp`; this constant mirrors it.
pub const STRING_BYTELEN_OFFSET: isize = 8;

/// Read `byte_len` from a Ry string handle (NUL-safe, binary-safe).
///
/// # Safety
/// `handle` must be a non-null Ry string handle produced by `makeString` /
/// `makeStringUninit` (or an ARC_IMMORTAL literal global). Passing a raw C
/// string literal or `Vec::as_ptr()` reads garbage — same UB as the C++
/// inline `stringByteLen` documented in
/// `.claude/rules/runtime-memory-safety.md`. Callers in `abi.rs` must
/// null-check `handle` first; this helper does NOT.
#[inline]
pub unsafe fn string_byte_len(handle: *const c_char) -> i64 {
    let prefix = handle.cast::<u8>().offset(-STRING_BYTELEN_OFFSET) as *const i64;
    prefix.read_unaligned()
}
