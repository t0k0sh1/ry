//! Host-side FFI: the one allocation shim convert needs. Used by
//! `get_last_error_handle` in lib.rs to wrap the per-thread error
//! buffer into a Ry `str` handle. Input str handles are read with
//! `CStr::from_ptr` in abi.rs, so no `byte_len` mirror is required.

use std::ffi::c_char;

extern "C" {
    pub fn __ry_host_make_string(src: *const c_char, byte_len: i64) -> *const c_char;
}
