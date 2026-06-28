// Rust port of `src/runtime/native/io.cpp` (#2480 — fourth migration
// after base64 (#2282), convert (#2478), and path (#2479)).
//
// Three-layer split (mirrors #2282 / #2478 / #2479):
//   - abi    : 20 `extern "C"` symbols (`__ry_io_read_line`,
//              `__ry_io_input_prompt`, `__ry_io_read_all`,
//              `__ry_io_read_text`, `__ry_io_write_text`,
//              `__ry_io_append_text`, `__ry_io_exists`,
//              `__ry_io_delete_file`, `__ry_io_read_bytes`,
//              `__ry_io_write_bytes`, `__ry_io_to_bytes`,
//              `__ry_io_bytes_to_str`, `__ry_io_file_open`,
//              `__ry_io_file_read_all`, `__ry_io_file_read_line`,
//              `__ry_io_file_write_text`, `__ry_io_file_close`,
//              `__ry_io_file_cleanup`, `__ry_io_get_last_error`)
//              consumed by `emitGenericNativeCall` driven by
//              `share/std/io/io.ry` descriptors. Tri-state read_line
//              return + `IoFileHandle` resource lifecycle are first-of-
//              their-kind among the Rust ports.
//   - ffi    : host shim extern decls (`__ry_host_make_string`,
//              `__ry_host_make_byte_list`,
//              `__ry_host_make_empty_io_list`,
//              `__ry_arc_alloc_counted`, `__ry_set_last_error`) +
//              `#[repr(C)]` mirrors of `IOListHeader` and the TU-local
//              `IoFileHandle`, paired with compile-time layout
//              assertions and matching C++ `static_assert`s in
//              `tests/test_abi_layout.cpp`.
//   - core   : pure Rust file I/O — `MAX_READ_SIZE` cap, `O_NOFOLLOW`-
//              flavored `open_nofollow`, `libc::getline` wrapper with
//              tri-state outcome, growable read-all, seek-fast-path
//              + chunk-fallback file read, ARC-block IoFileHandle
//              allocation that header-stamps strong=1 / weak=0 to
//              mirror `arc_alloc` in `include/ry/runtime/core/arc.hpp`.
//
// Panic discipline (#2282, .claude/rules/runtime-memory-safety.md):
//   panic across the C ABI is UB. Each `__ry_io_*` extern wraps its
//   body in `catch_or_exit`, which calls `std::panic::catch_unwind` and
//   on a caught panic flushes stdio and calls `libc::_exit(1)` — the
//   same trap path as the host's `ry_runtime_trap_exit()`. No
//   `std::process::exit` (would run atexit and touch the JIT-live LLVM
//   `ManagedStatic` heap, mirroring #1838).
//
// Dual-write error channel (#2480 new):
//   io.cpp historically owned BOTH a module-local `io_last_error_buf`
//   AND mirrored every error into the shared `__ry_set_last_error`
//   buffer, so cross-module callers in `json::load_file` /
//   `json5::dump_file` (which delegate to `__ry_io_file_*`) could
//   surface io errors through their own shared `__ry_get_last_error`
//   reads (json.cpp:702-773, json5.cpp:1102-1190). The Rust port keeps
//   that dual-write inside `set_last_error` below — see the function
//   comment for the trade-off about NUL termination.

// FFI naming: __ry_* matches the host convention (intentional reserved
// identifier — see .clang-tidy for the C++ counterpart).
// missing_safety_doc is allowed at the crate level because every
// `__ry_io_*` extern's safety contract is the same as the rest of the
// runtime ABI (callers must pass Ry-allocated str / IOListHeader /
// IoFileHandle handles, or null); the crate-level comment above
// documents it once. Matches the base64 / convert / path pilot crates.
#![allow(non_upper_case_globals)]
#![allow(clippy::missing_safety_doc)]

pub mod abi;
pub mod core;
pub mod ffi;

use std::cell::RefCell;
use std::ffi::c_char;
use std::panic::{catch_unwind, AssertUnwindSafe};

// Module-local thread-local error buffer (mirrors C++ `io.cpp:25`
// `static thread_local char io_last_error_buf[512]`). Capped at 511
// bytes to mirror the C++ 512-byte buffer (one byte reserved for the
// null terminator the host's `makeString` appends).
//
// NOTE: this buffer holds the message bytes only (no trailing NUL).
// `get_last_error_handle` passes `buf.len()` to `__ry_host_make_string`
// as `byte_len`, matching C++ `makeString(io_last_error_buf,
// strlen(io_last_error_buf))`. The NUL terminator only matters for the
// shared `__ry_set_last_error` mirror; see `set_last_error` below.
const LAST_ERROR_CAP: usize = 511;
thread_local! {
    static LAST_ERROR: RefCell<Vec<u8>> = const { RefCell::new(Vec::new()) };
}

/// Stash `msg` in the per-thread io error buffer AND mirror it into
/// the shared host-side `__ry_set_last_error` buffer.
///
/// The dual-write matches the C++ `setLastError` helper at
/// `io.cpp:27-33` — the cross-module readers in `json::load_file` /
/// `json5::dump_file` (json.cpp:702-773, json5.cpp:1102-1190) call
/// `__ry_io_file_*` then propagate failures by reading the SHARED
/// `__ry_get_last_error` buffer, so the io port must keep that mirror
/// populated even though Rust crates (base64 / convert / path) use the
/// module-local thread_local exclusively.
///
/// NUL handling: the shared `__ry_set_last_error(const char *msg)`
/// reads a C-terminated string. We build a small `\0`-terminated
/// scratch copy (up to 510 message bytes + trailing `\0`, kept inside
/// the 511 cap) and pass it. Any embedded NUL in `msg` truncates the
/// shared mirror at the NUL — preferable to `CString::new`'s NulError
/// (which would surface as an unexpected error in the rare malformed-
/// message case). The module-local `LAST_ERROR` keeps the full
/// pre-NUL bytes so `__ry_io_get_last_error` still hands the full
/// (binary-safe) message back to the caller via `__ry_host_make_string`.
///
/// Call only from the err arms of Result-returning entry points
/// (`__ry_io_read_text` / `__ry_io_write_text` / ... — NOT from
/// `__ry_io_exists` which is bool-shaped). Calling from a Direct entry
/// point would leave stale text the next Result call could read.
pub(crate) fn set_last_error(msg: &str) {
    let bytes = msg.as_bytes();
    let n = bytes.len().min(LAST_ERROR_CAP);
    let payload = &bytes[..n];

    LAST_ERROR.with(|cell| {
        let mut buf = cell.borrow_mut();
        buf.clear();
        buf.extend_from_slice(payload);
    });

    // Build a NUL-terminated scratch for the shared mirror. Truncate
    // at the first interior NUL (if any) so the C-string-shaped
    // `__ry_set_last_error` reads only what fits. Reserve the full
    // `LAST_ERROR_CAP` message budget + one trailing NUL so the shared
    // mirror carries the same payload length as the module-local
    // buffer (511 bytes); the host's `snprintf(buf, 512, "%s", msg)`
    // in `src/runtime/core/error.cpp:19` then writes 511 chars + NUL,
    // matching C++ `last_error_buf[512]` semantics byte-for-byte.
    let mut scratch = Vec::with_capacity(LAST_ERROR_CAP + 1);
    for &b in payload {
        if b == 0 || scratch.len() >= LAST_ERROR_CAP {
            break;
        }
        scratch.push(b);
    }
    scratch.push(0);
    unsafe {
        ffi::__ry_set_last_error(scratch.as_ptr().cast::<c_char>());
    }
}

/// Build a Ry str handle around the current per-thread error buffer.
/// Caller is the `__ry_io_get_last_error` abi entry; codegen via
/// `emitGenericNativeCall` resolves the symbol on the Err arm of any
/// Result-returning io call.
///
/// `LAST_ERROR` starts empty (`Vec::new()` is unallocated), so
/// `buf.as_ptr()` would be a dangling alignment sentinel before any
/// error is recorded — route through a static `EMPTY` byte when the
/// buffer is empty so the pointer passed to `__ry_host_make_string` is
/// always valid (matches the C++ `io_last_error_buf[512]` initial
/// state, guard added in path port #2485 review).
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
