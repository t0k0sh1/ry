// Rust port of `src/runtime/native/convert.cpp` (#2478 — second
// migration after the base64 pilot #2282).
//
// Three-layer split (mirrors #2282):
//   - abi    : 3 `extern "C"` symbols (`__ry_str_to_int`,
//              `__ry_str_to_float`, `__ry_convert_get_last_error`)
//              consumed by `emitBuiltinConversion` in
//              `src/codegen_call.cpp`
//   - ffi    : single host shim extern decl (`__ry_host_make_string`)
//              for wrapping the per-thread error buffer into a Ry str
//              handle; no `#[repr(C)]` mirrors needed (scalar args
//              only — no IOListHeader / byte_len reads)
//   - core   : pure Rust int / float parse logic, no `unsafe`,
//              `Result<T, String>` with messages that are byte-identical
//              to the C++ `setLastError(...)` strings.
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
// crate-level allows in the base64 pilot crate.
#![allow(non_upper_case_globals)]
#![allow(clippy::missing_safety_doc)]

pub mod abi;
pub mod core;
pub mod ffi;

use std::cell::RefCell;
use std::ffi::c_char;
use std::panic::{catch_unwind, AssertUnwindSafe};

// Module-local thread-local error buffer (mirrors the C++ side's
// `DEFINE_LAST_ERROR(convert)` from `include/ry/runtime/core/error.hpp`).
// Capped at 511 bytes to mirror the C++ 512-byte `last_error_buf` (one
// byte reserved for the null terminator the host's `makeString` appends).
const LAST_ERROR_CAP: usize = 511;
thread_local! {
    static LAST_ERROR: RefCell<Vec<u8>> = const { RefCell::new(Vec::new()) };
}

/// Stash `msg` in the per-thread convert error buffer. Call only from
/// the err arms of the Result-returning entry points
/// (`__ry_str_to_int` / `__ry_str_to_float`) — calling from a Direct
/// entry point would leave stale text the next Result call could read.
/// Same cap-and-copy discipline as the base64 pilot crate, and the
/// `.claude/rules/runtime-memory-safety.md` §"Do not call setLastError
/// in bool-return runtime functions" precedent.
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
/// Caller is the `__ry_convert_get_last_error` abi entry; codegen in
/// `src/codegen_call.cpp` (`emitBuiltinConversion`) resolves the
/// symbol when building the Err arm of `int(s)` / `float(s)`.
pub(crate) unsafe fn get_last_error_handle() -> *const c_char {
    LAST_ERROR.with(|cell| {
        let buf = cell.borrow();
        ffi::__ry_host_make_string(buf.as_ptr().cast::<c_char>(), buf.len() as i64)
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
