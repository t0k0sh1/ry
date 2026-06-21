//! C boundary for the @native("base64") stdlib module — 9 `extern "C"`
//! symbols consumed by `src/codegen_call_base64.cpp` (unchanged from the
//! C++ port).

use std::ffi::{c_char, c_void};

use crate::catch_or_exit;
use crate::core::{
    decode_with, encode_with_into, encoded_len, STD_ALPHABET, STD_DECODE_TABLE, URL_ALPHABET,
    URL_DECODE_TABLE,
};
use crate::{ffi, get_last_error_handle, set_last_error};

/// Allocate an empty Ry str handle via the host shim. Cheap fallback for
/// null / empty inputs.
unsafe fn empty_str_handle() -> *const c_char {
    ffi::__ry_host_make_string(c"".as_ptr(), 0)
}

/// Encode `input` bytes into a freshly-allocated Ry str handle. The host
/// shim allocates the output buffer up-front (sized by `encoded_len`) and
/// the encoder writes alphabet bytes directly into it — no intermediate
/// `Vec` allocation, matching the old C++ `makeStringUninit` zero-copy
/// shape.
unsafe fn encode_into_str_handle(input: &[u8], alphabet: &[u8; 64], pad: bool) -> *const c_char {
    let out_len = encoded_len(input.len(), pad);
    let buf = ffi::__ry_host_make_string_uninit(out_len as i64);
    let out = std::slice::from_raw_parts_mut(buf.cast::<u8>(), out_len);
    encode_with_into(input, alphabet, pad, out);
    buf.cast_const()
}

/// Encode the str at `input` via `alphabet`/`pad`. Helper for the four
/// str-input encode entry points.
unsafe fn encode_str_with(input: *const c_char, alphabet: &[u8; 64], pad: bool) -> *const c_char {
    if input.is_null() {
        return empty_str_handle();
    }
    let byte_len = ffi::string_byte_len(input);
    if byte_len <= 0 {
        return empty_str_handle();
    }
    let slice = std::slice::from_raw_parts(input.cast::<u8>(), byte_len as usize);
    encode_into_str_handle(slice, alphabet, pad)
}

/// Decode the str at `input` via `decode_tbl`, returning a Ry str handle
/// (Ok) or null + `set_last_error` (Err). Helper for the two str→str
/// decode entry points.
unsafe fn decode_str_with(input: *const c_char, decode_tbl: &[i8; 256]) -> *const c_char {
    if input.is_null() {
        return empty_str_handle();
    }
    let byte_len = ffi::string_byte_len(input);
    if byte_len <= 0 {
        return empty_str_handle();
    }
    let slice = std::slice::from_raw_parts(input.cast::<u8>(), byte_len as usize);
    match decode_with(slice, decode_tbl) {
        Ok(decoded) => {
            ffi::__ry_host_make_string(decoded.as_ptr().cast::<c_char>(), decoded.len() as i64)
        }
        Err(msg) => {
            set_last_error(&msg);
            std::ptr::null()
        }
    }
}

/// Encode the bytes inside an `IOListHeader*` (Ry `List<u8>`) using
/// `alphabet`. Helper for the two bytes-input encode entry points.
unsafe fn encode_bytes_with(list: *mut c_void, alphabet: &[u8; 64], pad: bool) -> *const c_char {
    let header = list.cast::<ffi::IOListHeader>();
    let len = (*header).len;
    if len <= 0 {
        return empty_str_handle();
    }
    let slice = std::slice::from_raw_parts((*header).data.cast::<u8>(), len as usize);
    encode_into_str_handle(slice, alphabet, pad)
}

/// Decode a Ry str into a `List<u8>` (IOListHeader) using `decode_tbl`.
/// Helper for the two str→bytes decode entry points.
unsafe fn decode_bytes_with(input: *const c_char, decode_tbl: &[i8; 256]) -> *mut c_void {
    if input.is_null() {
        return ffi::__ry_host_make_empty_io_list();
    }
    let byte_len = ffi::string_byte_len(input);
    if byte_len <= 0 {
        return ffi::__ry_host_make_empty_io_list();
    }
    let slice = std::slice::from_raw_parts(input.cast::<u8>(), byte_len as usize);
    match decode_with(slice, decode_tbl) {
        Ok(decoded) => ffi::__ry_host_make_byte_list(decoded.as_ptr(), decoded.len() as i64),
        Err(msg) => {
            set_last_error(&msg);
            std::ptr::null_mut()
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn __ry_base64_encode(input: *const c_char) -> *const c_char {
    catch_or_exit(|| encode_str_with(input, STD_ALPHABET, true))
}

#[no_mangle]
pub unsafe extern "C" fn __ry_base64_decode(input: *const c_char) -> *const c_char {
    catch_or_exit(|| decode_str_with(input, &STD_DECODE_TABLE))
}

#[no_mangle]
pub unsafe extern "C" fn __ry_base64_encode_url_safe(input: *const c_char) -> *const c_char {
    catch_or_exit(|| encode_str_with(input, URL_ALPHABET, false))
}

#[no_mangle]
pub unsafe extern "C" fn __ry_base64_decode_url_safe(input: *const c_char) -> *const c_char {
    catch_or_exit(|| decode_str_with(input, &URL_DECODE_TABLE))
}

#[no_mangle]
pub unsafe extern "C" fn __ry_base64_encode_bytes(list: *mut c_void) -> *const c_char {
    catch_or_exit(|| encode_bytes_with(list, STD_ALPHABET, true))
}

#[no_mangle]
pub unsafe extern "C" fn __ry_base64_encode_bytes_url_safe(list: *mut c_void) -> *const c_char {
    catch_or_exit(|| encode_bytes_with(list, URL_ALPHABET, false))
}

#[no_mangle]
pub unsafe extern "C" fn __ry_base64_decode_bytes(input: *const c_char) -> *mut c_void {
    catch_or_exit(|| decode_bytes_with(input, &STD_DECODE_TABLE))
}

#[no_mangle]
pub unsafe extern "C" fn __ry_base64_decode_bytes_url_safe(input: *const c_char) -> *mut c_void {
    catch_or_exit(|| decode_bytes_with(input, &URL_DECODE_TABLE))
}

#[no_mangle]
pub unsafe extern "C" fn __ry_base64_get_last_error() -> *const c_char {
    catch_or_exit(|| get_last_error_handle())
}
