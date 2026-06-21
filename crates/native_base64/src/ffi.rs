//! Host-side FFI: extern declarations for the allocation shims in
//! `src/runtime/core/host_shims.cpp` and `#[repr(C)]` mirrors of the host
//! header layouts the abi layer reads through received handles.
//!
//! Discipline (#2282 §3, "ハンドル受領のみ"):
//! - allocation goes through host shims (`__ry_host_make_*`); Rust never
//!   fabricates a StringHeader / IOListHeader
//! - reads (byte_len from a str handle, len/data from an IOListHeader)
//!   go through layout-asserted mirrors so a C++ field reorder breaks the
//!   build, mirroring the `tests/test_abi_layout.cpp` precedent (#1995).

use std::ffi::{c_char, c_void};

// ── Host allocation shims (src/runtime/core/host_shims.cpp) ────────────────
//
// The C++ `makeString` / `makeStringUninit` / `makeByteList` /
// `makeEmptyIOList` are header-only inline functions — they vanish from the
// cdylib's undefined-external set, so Rust cannot call them directly. The
// shims are thin `extern "C"` wrappers that call the inline definitions, so
// allocation semantics (arc_alloc + counter increment + memcpy + null
// terminator) stay single-source on the C++ side.
extern "C" {
    pub fn __ry_host_make_string(src: *const c_char, byte_len: i64) -> *const c_char;
    pub fn __ry_host_make_string_uninit(byte_len: i64) -> *mut c_char;
    pub fn __ry_host_make_byte_list(bytes: *const u8, len: i64) -> *mut c_void;
    pub fn __ry_host_make_empty_io_list() -> *mut c_void;
}

// ── IOListHeader mirror (include/ry/runtime/native/io.hpp) ─────────────────
//
// Layout pinned by paired compile-time assertions: the C++ side is in
// `tests/test_abi_layout.cpp` (added in #2282), the Rust side is the
// `const _: () = assert!(...)` block below. Either side reordering its
// fields breaks the build; same parity discipline as the RyCowEnsureUniqueDesc
// pair (#1995).
#[repr(C)]
pub struct IOListHeader {
    pub len: i64,
    pub cap: i64,
    pub data: *mut i8,
}

const _: () = assert!(std::mem::size_of::<IOListHeader>() == 24);
const _: () = assert!(std::mem::align_of::<IOListHeader>() == 8);
const _: () = assert!(std::mem::offset_of!(IOListHeader, len) == 0);
const _: () = assert!(std::mem::offset_of!(IOListHeader, cap) == 8);
const _: () = assert!(std::mem::offset_of!(IOListHeader, data) == 16);

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
/// `.claude/rules/runtime-memory-safety.md`.
#[inline]
pub unsafe fn string_byte_len(handle: *const c_char) -> i64 {
    let prefix = handle.cast::<u8>().offset(-STRING_BYTELEN_OFFSET) as *const i64;
    prefix.read_unaligned()
}
