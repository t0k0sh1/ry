//! C boundary for the @native("convert") stdlib module — 3 `extern "C"`
//! symbols consumed by `emitBuiltinConversion` in
//! `src/codegen_call.cpp` (unchanged from the C++ port).
//!
//! Signatures match the C++ contract verbatim:
//!   `__ry_str_to_int(*const c_char, *mut i64)   -> i64`  (0 = ok, 1 = err)
//!   `__ry_str_to_float(*const c_char, *mut f64) -> i64`  (0 = ok, 1 = err)
//!   `__ry_convert_get_last_error()              -> *const c_char`
//!
//! Names stay bare (no `__ry_convert_` prefix) to match the legacy
//! kRuntimeSymbolLibraries entries in `src/codegen.cpp`. Input str
//! handles are decoded with `CStr::from_ptr` — the C `strtoll` /
//! `strtod` contract is NUL-terminated scanning, and numeric strings
//! never contain interior NULs, so the base64 `string_byte_len`
//! mechanism is intentionally not used.

use std::borrow::Cow;
use std::ffi::{c_char, CStr};

use crate::core::{parse_float, parse_int};
use crate::{catch_or_exit, get_last_error_handle, set_last_error};

/// Run `parser` on the str behind `str_handle` and route the result
/// into the C-ABI shape: write the value through `out` and return 0 on
/// Ok, stash the message in the per-thread error buffer and return 1
/// on Err.
///
/// Input handling matches the C++ `if (!str || *str == '\0')` guard
/// for null and empty input (routed to "int/float: empty string"). Ry
/// `str` handles can carry arbitrary byte buffers (built via
/// `makeString` / `makeStringUninit`), so a non-null but non-UTF-8
/// input is passed through `from_utf8_lossy` and routed to the parser,
/// which produces "int/float: invalid character in '...'" — same error
/// class as the C++ `strtoll`/`strtod` non-digit path, rather than
/// silently collapsing to empty.
unsafe fn parse_to_out<T>(
    str_handle: *const c_char,
    out: *mut T,
    parser: impl FnOnce(&str) -> Result<T, String>,
) -> i64 {
    let cow: Cow<str> = if str_handle.is_null() {
        Cow::Borrowed("")
    } else {
        let bytes = CStr::from_ptr(str_handle).to_bytes();
        match std::str::from_utf8(bytes) {
            Ok(s) => Cow::Borrowed(s),
            Err(_) => String::from_utf8_lossy(bytes),
        }
    };
    match parser(&cow) {
        Ok(v) => {
            *out = v;
            0
        }
        Err(msg) => {
            set_last_error(&msg);
            1
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn __ry_str_to_int(str_handle: *const c_char, out: *mut i64) -> i64 {
    catch_or_exit(|| parse_to_out(str_handle, out, parse_int))
}

#[no_mangle]
pub unsafe extern "C" fn __ry_str_to_float(str_handle: *const c_char, out: *mut f64) -> i64 {
    catch_or_exit(|| parse_to_out(str_handle, out, parse_float))
}

#[no_mangle]
pub unsafe extern "C" fn __ry_convert_get_last_error() -> *const c_char {
    catch_or_exit(|| get_last_error_handle())
}
