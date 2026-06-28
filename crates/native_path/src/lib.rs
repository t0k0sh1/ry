// Rust port of `src/runtime/native/path.cpp` (#2479 — third
// migration after the base64 pilot #2282 and convert #2478).
//
// Three-layer split (mirrors #2282 / #2478):
//   - abi    : 9 `extern "C"` symbols (`__ry_path_join2/3/4`,
//              `__ry_path_basename`, `__ry_path_dirname`,
//              `__ry_path_ext`, `__ry_path_resolve`,
//              `__ry_path_isAbsolute`, `__ry_path_get_last_error`)
//              consumed by `emitGenericNativeCall` driven by
//              `share/std/path/path.ry` descriptors. The `join`
//              symbol uses the arity-suffix convention (gate in
//              `src/codegen_call_native.cpp:840-863`).
//   - ffi    : single host shim extern decl (`__ry_host_make_string`)
//              for handing back Ry str handles, plus the inline
//              `string_byte_len` helper that reads `handle[-8]` as
//              `i64` (STRING_BYTELEN_OFFSET = 8 from
//              `include/ry/ry_layout.hpp`).
//   - core   : pure Rust path operations (join2 / basename / dirname
//              / ext), no `unsafe`, byte-identical to the C++
//              `join2_impl` / `basename` / `dirname` / `ext` bodies
//              in `src/runtime/native/path.cpp`.
//
// Panic discipline (#2282, .claude/rules/runtime-memory-safety.md):
//   panic across the C ABI is UB. Each `__ry_*` extern wraps its body
//   in `catch_or_exit`, which calls `std::panic::catch_unwind` and on a
//   caught panic flushes stdio and calls `libc::_exit(1)` — the same
//   trap path as the host's `ry_runtime_trap_exit()`. No
//   `std::process::exit` (would run atexit and touch the JIT-live LLVM
//   `ManagedStatic` heap, mirroring #1838).

// FFI naming: __ry_* matches the host convention (intentional reserved
// identifier — see .clang-tidy for the C++ counterpart).
// missing_safety_doc is allowed at the crate level because every
// `__ry_*` extern's safety contract is the same as the rest of the
// runtime ABI (callers must pass Ry-allocated str handles, or null);
// the crate-level comment above documents it once. Matches the
// crate-level allows in the base64 / convert pilot crates.
#![allow(non_upper_case_globals)]
#![allow(clippy::missing_safety_doc)]

pub mod abi;
pub mod core;
pub mod ffi;

use std::cell::RefCell;
use std::ffi::c_char;
use std::panic::{catch_unwind, AssertUnwindSafe};

// Module-local thread-local error buffer (mirrors the C++ side's
// `DEFINE_LAST_ERROR(path)` from `include/ry/runtime/core/error.hpp`).
// Capped at 511 bytes to mirror the C++ 512-byte `last_error_buf` (one
// byte reserved for the null terminator the host's `makeString` appends).
const LAST_ERROR_CAP: usize = 511;
thread_local! {
    static LAST_ERROR: RefCell<Vec<u8>> = const { RefCell::new(Vec::new()) };
}

/// Stash `msg` in the per-thread path error buffer. Call only from
/// the err arms of the Result-returning entry points
/// (`__ry_path_join*` / `__ry_path_basename` / `__ry_path_dirname` /
/// `__ry_path_ext` / `__ry_path_resolve`) — calling from a Direct
/// entry point (`__ry_path_isAbsolute`) would leave stale text the
/// next Result call could read. Same cap-and-copy discipline as the
/// base64 / convert pilot crates.
pub(crate) fn set_last_error(msg: &str) {
    LAST_ERROR.with(|cell| {
        let mut buf = cell.borrow_mut();
        buf.clear();
        let bytes = msg.as_bytes();
        let n = bytes.len().min(LAST_ERROR_CAP);
        buf.extend_from_slice(&bytes[..n]);
    });
}

/// Build a Ry str handle around the current per-thread error buffer.
/// Caller is the `__ry_path_get_last_error` abi entry; codegen via
/// `emitGenericNativeCall` resolves the symbol on the Err arm of any
/// Result-returning path call.
///
/// `LAST_ERROR` starts empty (`Vec::new()` is unallocated), so
/// `buf.as_ptr()` would be a dangling alignment sentinel before any
/// error is recorded — handing that across the FFI boundary is
/// technically UB even though the host shim's `memcpy` would no-op
/// with len 0. Route through a static `EMPTY` byte when the buffer
/// is empty so the pointer passed to `__ry_host_make_string` is
/// always valid (matches the C++ `last_error_buf[512]` initial state,
/// guard added in convert pilot follow-up #2485 review).
pub(crate) unsafe fn get_last_error_handle() -> *const c_char {
    static EMPTY: [u8; 1] = [0];
    LAST_ERROR.with(|cell| {
        let buf = cell.borrow();
        let src = if buf.is_empty() {
            EMPTY.as_ptr()
        } else {
            buf.as_ptr()
        };
        ffi::__ry_host_make_string(src.cast::<c_char>(), buf.len() as i64)
    })
}

/// Flush all open stdio streams (POSIX: `fflush(NULL)`) and exit the
/// process with status 1, bypassing `atexit` — mirrors the host's
/// `ry_runtime_trap_exit()` in `include/ry/runtime/core/alloc.hpp`. We
/// use `fflush(NULL)` rather than per-stream flush because the libc
/// crate does not expose `stdout` / `stderr` uniformly across macOS
/// (`__stdoutp`) and Linux (`stdout`); the POSIX null-argument
/// shortcut sidesteps that platform split.
fn trap_exit() -> ! {
    unsafe {
        libc::fflush(std::ptr::null_mut());
        libc::_exit(1);
    }
}

/// Run `f` and return its value. If `f` panics, the panic is caught at
/// the FFI boundary (panics through C are UB) and the process exits
/// via `trap_exit()`.
pub(crate) fn catch_or_exit<R, F: FnOnce() -> R>(f: F) -> R {
    match catch_unwind(AssertUnwindSafe(f)) {
        Ok(r) => r,
        Err(_) => trap_exit(),
    }
}
