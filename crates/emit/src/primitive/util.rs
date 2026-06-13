//! primitive/util — name-building utilities. Pure `CString` helpers with no
//! Ry-layout knowledge.

use std::ffi::{c_char, CStr, CString};

// Build a NUL-terminated CString name from a prefix + suffix (for SSA names
// like "{prefix}_idx").
#[inline]
pub(crate) fn cname_pfx(prefix: &[u8], suffix: &[u8]) -> CString {
    let mut v = Vec::with_capacity(prefix.len() + suffix.len());
    v.extend_from_slice(prefix);
    v.extend_from_slice(suffix);
    CString::new(v).unwrap()
}

// Three-part CString name (e.g. "cow_" + tag + "_len_ptr") for the CoW retain
// loops.
#[inline]
pub(crate) fn cname3(a: &[u8], b: &[u8], c: &[u8]) -> CString {
    let mut v = Vec::with_capacity(a.len() + b.len() + c.len());
    v.extend_from_slice(a);
    v.extend_from_slice(b);
    v.extend_from_slice(c);
    CString::new(v).unwrap()
}

// Borrow C-string bytes without the NUL (empty slice on NULL). A pure `CStr`
// primitive (no `EmitCtx` / intern), used by the prefix-derived name builders
// in the bounds engine.
#[inline]
pub(crate) unsafe fn cstr_bytes<'a>(p: *const c_char) -> &'a [u8] {
    if p.is_null() {
        b""
    } else {
        CStr::from_ptr(p).to_bytes()
    }
}
