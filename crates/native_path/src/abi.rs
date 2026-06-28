//! `extern "C"` entry points for `@native("path")`. Every symbol is
//! wrapped in `catch_or_exit` so a Rust panic cannot unwind across the
//! C ABI (UB). NUL-byte validation and null-handle guards run here so
//! `core::*` can assume well-formed `&[u8]` inputs.
//!
//! Symbol contract (kept byte-identical to `src/runtime/native/path.cpp`):
//! - Result-returning entry points return a Ry str handle on success or
//!   `null` on failure after stashing the message via `set_last_error`
//! - `__ry_path_isAbsolute` is the only Direct (non-Result) entry —
//!   returns `i64` (0/1) and never touches the error buffer
//! - `__ry_path_get_last_error` returns a Ry str handle around the
//!   current per-thread error buffer (empty → empty handle, via the
//!   `EMPTY` guard in lib.rs)

use std::ffi::{c_char, CStr};

use crate::{catch_or_exit, ffi, get_last_error_handle, set_last_error};

// Error message strings — must remain byte-identical to the C++ side
// (`src/runtime/native/path.cpp` lines 41/45/95/114/143/167/171/177).
// Tests in `tests/spec/nul_safety_path.test.ry` assert
// `.toContain("embedded NUL")`, but the issue's "byte-identical" spec
// (and resilience to future test tightening) makes the full literal
// the canonical contract.
const ERR_JOIN_NUL: &str = "path.join: argument contains an embedded NUL byte";
const ERR_BASENAME_NUL: &str = "path.basename: argument contains an embedded NUL byte";
const ERR_DIRNAME_NUL: &str = "path.dirname: argument contains an embedded NUL byte";
const ERR_EXT_NUL: &str = "path.ext: argument contains an embedded NUL byte";
const ERR_RESOLVE_NUL: &str = "path.resolve: argument contains an embedded NUL byte";
const ERR_RESOLVE_EMPTY: &str = "cannot resolve path: empty path";

/// Read the byte payload of a Ry str handle. Null handle → empty slice
/// (matches the C++ `!p` short-circuit in every entry point).
///
/// # Safety
/// `h` must be either null or a Ry str handle produced by `makeString` /
/// `makeStringUninit` — passing a raw C string literal reads
/// uninitialized memory at `h - 8` (same UB as `ffi::string_byte_len`).
#[inline]
unsafe fn handle_bytes<'a>(h: *const c_char) -> &'a [u8] {
    if h.is_null() {
        return &[];
    }
    let len = ffi::string_byte_len(h) as usize;
    std::slice::from_raw_parts(h.cast::<u8>(), len)
}

/// Wrap a byte slice into a fresh Ry str handle via the host
/// allocation shim.
#[inline]
unsafe fn make_handle(bytes: &[u8]) -> *const c_char {
    ffi::__ry_host_make_string(bytes.as_ptr().cast::<c_char>(), bytes.len() as i64)
}

// ===== join (arity-suffix overloads) =====
//
// The C++ `join2_impl` checks NUL on both args FIRST, before the
// absolute-path / empty short-circuits. The Rust port preserves that
// order so a NUL in a segment that would otherwise be discarded by
// absolute-path rewind still produces an error (e.g.
// `join("\0bad", "/clean")` errors out on `a`, matching C++).

#[no_mangle]
pub unsafe extern "C" fn __ry_path_join2(a: *const c_char, b: *const c_char) -> *const c_char {
    catch_or_exit(|| {
        let a_bytes = handle_bytes(a);
        if a_bytes.contains(&0) {
            set_last_error(ERR_JOIN_NUL);
            return std::ptr::null();
        }
        let b_bytes = handle_bytes(b);
        if b_bytes.contains(&0) {
            set_last_error(ERR_JOIN_NUL);
            return std::ptr::null();
        }
        let out = crate::core::join2(a_bytes, b_bytes);
        make_handle(&out)
    })
}

#[no_mangle]
pub unsafe extern "C" fn __ry_path_join3(
    a: *const c_char,
    b: *const c_char,
    c: *const c_char,
) -> *const c_char {
    catch_or_exit(|| {
        let a_bytes = handle_bytes(a);
        if a_bytes.contains(&0) {
            set_last_error(ERR_JOIN_NUL);
            return std::ptr::null();
        }
        let b_bytes = handle_bytes(b);
        if b_bytes.contains(&0) {
            set_last_error(ERR_JOIN_NUL);
            return std::ptr::null();
        }
        let ab = crate::core::join2(a_bytes, b_bytes);
        let c_bytes = handle_bytes(c);
        if c_bytes.contains(&0) {
            set_last_error(ERR_JOIN_NUL);
            return std::ptr::null();
        }
        let abc = crate::core::join2(&ab, c_bytes);
        make_handle(&abc)
    })
}

#[no_mangle]
pub unsafe extern "C" fn __ry_path_join4(
    a: *const c_char,
    b: *const c_char,
    c: *const c_char,
    d: *const c_char,
) -> *const c_char {
    catch_or_exit(|| {
        let a_bytes = handle_bytes(a);
        if a_bytes.contains(&0) {
            set_last_error(ERR_JOIN_NUL);
            return std::ptr::null();
        }
        let b_bytes = handle_bytes(b);
        if b_bytes.contains(&0) {
            set_last_error(ERR_JOIN_NUL);
            return std::ptr::null();
        }
        let ab = crate::core::join2(a_bytes, b_bytes);
        let c_bytes = handle_bytes(c);
        if c_bytes.contains(&0) {
            set_last_error(ERR_JOIN_NUL);
            return std::ptr::null();
        }
        let abc = crate::core::join2(&ab, c_bytes);
        let d_bytes = handle_bytes(d);
        if d_bytes.contains(&0) {
            set_last_error(ERR_JOIN_NUL);
            return std::ptr::null();
        }
        let abcd = crate::core::join2(&abc, d_bytes);
        make_handle(&abcd)
    })
}

// ===== basename / dirname / ext (single-arg helpers) =====

#[no_mangle]
pub unsafe extern "C" fn __ry_path_basename(p: *const c_char) -> *const c_char {
    catch_or_exit(|| {
        let bytes = handle_bytes(p);
        if bytes.is_empty() {
            return make_handle(b"");
        }
        if bytes.contains(&0) {
            set_last_error(ERR_BASENAME_NUL);
            return std::ptr::null();
        }
        make_handle(crate::core::basename(bytes))
    })
}

#[no_mangle]
pub unsafe extern "C" fn __ry_path_dirname(p: *const c_char) -> *const c_char {
    catch_or_exit(|| {
        let bytes = handle_bytes(p);
        if bytes.is_empty() {
            return make_handle(b".");
        }
        if bytes.contains(&0) {
            set_last_error(ERR_DIRNAME_NUL);
            return std::ptr::null();
        }
        make_handle(crate::core::dirname(bytes))
    })
}

#[no_mangle]
pub unsafe extern "C" fn __ry_path_ext(p: *const c_char) -> *const c_char {
    catch_or_exit(|| {
        let bytes = handle_bytes(p);
        if bytes.is_empty() {
            return make_handle(b"");
        }
        if bytes.contains(&0) {
            set_last_error(ERR_EXT_NUL);
            return std::ptr::null();
        }
        make_handle(crate::core::ext(bytes))
    })
}

// ===== resolve (realpath FFI) =====
//
// The realpath buffer is allocated by libc (POSIX: "If resolved_path is
// NULL, realpath() returns a pointer to a buffer that should be freed
// with free(3)"). It MUST be released with `libc::free` — using
// Rust's allocator (Box / Vec) is undefined behavior and trips ASan.
//
// Error formatting uses `libc::strerror(raw_errno)` rather than
// `std::io::Error::last_os_error().to_string()` because the latter
// appends `" (os error N)"` and would diverge from the C++ message
// `"cannot resolve path '%s': %s"` (where `%s` is `strerror(errno)`).

#[no_mangle]
pub unsafe extern "C" fn __ry_path_resolve(p: *const c_char) -> *const c_char {
    catch_or_exit(|| {
        // Empty-path check (matches C++ `!p || !*p`).
        let byte_len = if p.is_null() {
            0
        } else {
            ffi::string_byte_len(p)
        };
        if byte_len == 0 {
            set_last_error(ERR_RESOLVE_EMPTY);
            return std::ptr::null();
        }
        let bytes = std::slice::from_raw_parts(p.cast::<u8>(), byte_len as usize);
        if bytes.contains(&0) {
            set_last_error(ERR_RESOLVE_NUL);
            return std::ptr::null();
        }
        // p is a Ry str handle: data bytes followed by a NUL terminator
        // (always written by makeString / makeStringUninit). We've
        // confirmed no embedded NULs, so the C string realpath sees
        // ends exactly at `byte_len`.
        let resolved = libc::realpath(p, std::ptr::null_mut());
        if resolved.is_null() {
            let errno_val = std::io::Error::last_os_error().raw_os_error().unwrap_or(0);
            let strerr = CStr::from_ptr(libc::strerror(errno_val)).to_string_lossy();
            let path_str = String::from_utf8_lossy(bytes);
            set_last_error(&format!("cannot resolve path '{}': {}", path_str, strerr));
            return std::ptr::null();
        }
        let rlen = libc::strlen(resolved);
        let handle = ffi::__ry_host_make_string(resolved, rlen as i64);
        libc::free(resolved.cast::<libc::c_void>());
        handle
    })
}

// ===== isAbsolute (Direct i64 entry) =====
//
// Does NOT call `string_byte_len` — reads only byte 0 after a null
// guard, matching the C++ `if (!p) return 0; return p[0] == '/' ? 1 : 0`
// (path.cpp:185-188). No error channel; no `set_last_error`.

#[no_mangle]
pub unsafe extern "C" fn __ry_path_isAbsolute(p: *const c_char) -> i64 {
    catch_or_exit(|| {
        if p.is_null() {
            return 0;
        }
        i64::from(p.cast::<u8>().read() == b'/')
    })
}

// ===== Error channel =====

#[no_mangle]
pub unsafe extern "C" fn __ry_path_get_last_error() -> *const c_char {
    catch_or_exit(|| get_last_error_handle())
}
