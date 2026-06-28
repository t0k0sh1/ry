//! Host-side FFI: the allocation / error shims io needs, plus the inline
//! `string_byte_len` helper for reading the byte length from received str
//! handles, plus `#[repr(C)]` mirrors of the C++ `IOListHeader` and the
//! TU-local `IoFileHandle` resource.
//!
//! Discipline (#2282 §3, "ハンドル受領のみ"):
//! - allocation goes through host shims (`__ry_host_make_string`,
//!   `__ry_host_make_byte_list`, `__ry_host_make_empty_io_list`,
//!   `__ry_arc_alloc_counted`); Rust never fabricates a StringHeader /
//!   IOListHeader / ARC block via `Box::leak` or its own allocator.
//! - the shared error mirror goes through `__ry_set_last_error` so
//!   cross-module callers (json / json5 / future filesystem) can read
//!   io failures via their own `__ry_get_last_error` reads.
//! - reads (`byte_len` from a str handle) go through the layout-pinned
//!   `STRING_BYTELEN_OFFSET = 8` constant, mirroring the C++
//!   `static_assert` in `tests/test_abi_layout.cpp` (#1995).

use std::ffi::c_char;
use std::ffi::c_void;

extern "C" {
    /// `src/runtime/core/host_shims.cpp` — wraps
    /// `ry::makeString(src, byte_len)`. Returns a Ry str handle (points
    /// past the StringHeader). Binary-safe: NUL bytes in `src` survive.
    pub fn __ry_host_make_string(src: *const c_char, byte_len: i64) -> *const c_char;

    /// `src/runtime/core/host_shims.cpp` — wraps
    /// `ry::makeByteList(bytes, len)`. Returns an ARC-managed
    /// `IOListHeader *` (cast to `*mut c_void`) holding `len` bytes
    /// copied from `bytes`. The header's `data` buffer is owned and
    /// freed by ARC tear-down. MUST be paired with a non-empty `bytes`
    /// pointer: zero-length payloads MUST go through
    /// `__ry_host_make_empty_io_list` to avoid a dangling
    /// `Vec::as_ptr()` argument.
    pub fn __ry_host_make_byte_list(bytes: *const u8, len: i64) -> *mut c_void;

    /// `src/runtime/core/host_shims.cpp` — wraps
    /// `ry::makeEmptyIOList()`. Returns an ARC-managed `IOListHeader *`
    /// with `len = 0`, `cap = 0`, `data = nullptr`. Used for empty
    /// payloads (zero-byte reads, `to_bytes("")`).
    pub fn __ry_host_make_empty_io_list() -> *mut c_void;

    /// `src/runtime/core/arc_counter.cpp` — counted ARC primitive.
    /// Allocates `total_size` bytes (header + data), increments the
    /// live-count counter, returns the raw block pointer (NOT past the
    /// header — caller must add `ARC_HEADER_SIZE` to reach the data
    /// area, and is responsible for stamping `header[0] = 1`
    /// (strong_count) / `header[1] = 0` (weak_count) — the C++ inline
    /// `arc_alloc` (`include/ry/runtime/core/arc.hpp:21-31`) does this
    /// inline; the Rust counterpart in `core::alloc_file_handle`
    /// performs the same stamping.
    pub fn __ry_arc_alloc_counted(total_size: i64) -> *mut c_void;

    /// `src/runtime/core/error.cpp` — shared per-thread error mirror.
    /// Stores `msg` into a host-side `thread_local char[512]` via
    /// `snprintf` so cross-module callers (json::load_file /
    /// json5::dump_file in json.cpp / json5.cpp) can surface io
    /// failures through their own `__ry_get_last_error` reads. `msg`
    /// MUST be NUL-terminated; `lib::set_last_error` builds a scratch
    /// copy for this call.
    pub fn __ry_set_last_error(msg: *const c_char);
}

// ── ARC header layout (include/ry/ry_layout.hpp) ──────────────────────────
//
// `ARC_HEADER_SIZE = sizeof(int64_t) * 2` — two contiguous i64 words
// (strong_count, weak_count) prefix every ARC-managed block. Pinned in
// `tests/test_abi_layout.cpp`; mirrored here so `core::alloc_file_handle`
// can compute the total size and pointer-bump past the header.
pub const ARC_HEADER_SIZE: usize = 16;

// ── IOListHeader (include/ry/runtime/native/io.hpp:16-20) ─────────────────
//
// Mirror of the host `IOListHeader` struct that carries Ry `List<u8>`
// payloads. Layout is the same as a generic codegen `List<T>` header:
// `{i64 len, i64 cap, ptr data}`. Pinned in `tests/test_abi_layout.cpp`
// (added in the base64 pilot #2282).
//
// Only used by io's `abi::__ry_io_write_bytes` (consumes a header
// produced by codegen / `__ry_host_make_byte_list`) and
// `abi::__ry_io_bytes_to_str` (consumes a header). The producing path
// (`read_bytes` / `to_bytes`) hands `len` and a `Vec::as_ptr()` to
// `__ry_host_make_byte_list` instead of fabricating a header here.
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

// ── IoFileHandle (TU-local in C++ io.cpp:287-289) ─────────────────────────
//
// Mirror of the file-handle resource that backs the @native("io")
// `File` resource type. Only one field — a `FILE *` — but #[repr(C)]
// and explicit layout assertions guard against drift if the C++ side
// ever grows the struct (e.g. lifecycle state, mode bits). The C++
// struct intentionally lives in `io.cpp` only (NOT in `io.hpp`) to
// avoid ODR collisions with other potential `IoFileHandle` names; the
// matching `static_assert` block lives in `tests/test_abi_layout.cpp`
// behind a local re-declaration of the struct, keeping the host-header
// posture unchanged.
//
// `IoFileHandle::fp` is freed by `libc::fclose` in `__ry_io_file_close`
// / `__ry_io_file_cleanup`; the ARC block itself is freed by the ARC
// machinery (which calls `__ry_arc_free_counted` after `file_cleanup`).
#[repr(C)]
pub struct IoFileHandle {
    pub fp: *mut libc::FILE,
}

const _: () = assert!(std::mem::size_of::<IoFileHandle>() == 8);
const _: () = assert!(std::mem::align_of::<IoFileHandle>() == 8);
const _: () = assert!(std::mem::offset_of!(IoFileHandle, fp) == 0);

// ── StringHeader byte_len read (include/ry/ry_layout.hpp) ─────────────────
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
/// `makeStringUninit` (or an ARC_IMMORTAL literal global). Passing a raw
/// C string literal or `Vec::as_ptr()` reads garbage — same UB as the
/// C++ inline `stringByteLen` documented in
/// `.claude/rules/runtime-memory-safety.md`. Callers in `abi.rs` must
/// null-check `handle` first; this helper does NOT.
#[inline]
pub unsafe fn string_byte_len(handle: *const c_char) -> i64 {
    let prefix = handle.cast::<u8>().offset(-STRING_BYTELEN_OFFSET) as *const i64;
    prefix.read_unaligned()
}
