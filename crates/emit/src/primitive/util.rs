//! primitive/util — name-building utilities. Pure `CString` helpers with no
//! Ry-layout knowledge.

use std::ffi::{c_char, CStr, CString};

// Wrap a byte vector as a `CString`. The structural invariant — `cname_*`
// callers feed only static literals or `cstr_bytes` returns (NUL-stripped by
// construction) — means a clean caller never has an interior NUL. The debug
// assertion still signals an upstream bug if one slips in; the build path
// itself goes through `cstring_lossy`, which strips any NUL defensively so the
// boundary is panic-impossible (the previous `CString::new(v).unwrap()` would
// have unwound across the `unsafe extern "C" fn` boundary on a future
// invariant violation, which is UB).
#[inline]
fn make_cstring(v: Vec<u8>) -> CString {
    debug_assert!(!v.contains(&0), "interior NUL in cname input");
    cstring_lossy(v)
}

// Infallible CString construction: strip any interior NUL byte and wrap the
// remainder via `from_vec_unchecked`. Separated from `make_cstring` so the
// panic-impossible path is testable without the debug assertion firing first,
// satisfying #2445's "infallible path that cannot unwind across an extern \"C\"
// call" requirement.
#[inline]
fn cstring_lossy(mut v: Vec<u8>) -> CString {
    v.retain(|&b| b != 0);
    // SAFETY: the `retain` above guarantees `v` contains no interior NUL byte,
    // satisfying `CString::from_vec_unchecked`'s precondition.
    unsafe { CString::from_vec_unchecked(v) }
}

// Build a NUL-terminated CString name from a prefix + suffix (for SSA names
// like "{prefix}_idx").
#[inline]
pub(crate) fn cname_pfx(prefix: &[u8], suffix: &[u8]) -> CString {
    let mut v = Vec::with_capacity(prefix.len() + suffix.len());
    v.extend_from_slice(prefix);
    v.extend_from_slice(suffix);
    make_cstring(v)
}

// Three-part CString name (e.g. "cow_" + tag + "_len_ptr") for the CoW retain
// loops.
#[inline]
pub(crate) fn cname3(a: &[u8], b: &[u8], c: &[u8]) -> CString {
    let mut v = Vec::with_capacity(a.len() + b.len() + c.len());
    v.extend_from_slice(a);
    v.extend_from_slice(b);
    v.extend_from_slice(c);
    make_cstring(v)
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

#[cfg(test)]
mod tests {
    use super::{cname3, cname_pfx, cstring_lossy};

    // Clean-input parity: the public cname builders must round-trip prefix /
    // suffix byte slices into a NUL-terminated CString verbatim. Locks the
    // happy-path behavior the composite emitters (cow / bounds / cast /
    // header) depend on for SSA-name shaping.
    #[test]
    fn cname_pfx_concatenates_clean_inputs() {
        assert_eq!(
            cname_pfx(b"prefix", b"_suffix").to_bytes(),
            b"prefix_suffix"
        );
    }

    #[test]
    fn cname3_concatenates_three_clean_inputs() {
        assert_eq!(cname3(b"a_", b"b", b"_c").to_bytes(), b"a_b_c");
    }

    // Panic-impossible safeguard: if a future `cname_*` caller violates the
    // structural NUL-free invariant, the build path must still return a
    // well-formed `CString` — NEVER panic across the `unsafe extern "C" fn`
    // boundary (the previous `CString::new(v).unwrap()` would have unwound,
    // which is UB). Testing `cstring_lossy` directly bypasses the debug
    // assertion in `make_cstring` so the safety net is exercised in the
    // default debug profile too. Locks the AC for #2445.
    #[test]
    fn cstring_lossy_strips_interior_nul() {
        assert_eq!(cstring_lossy(b"abc\0def\0".to_vec()).to_bytes(), b"abcdef");
    }

    #[test]
    fn cstring_lossy_passes_clean_input_through_verbatim() {
        assert_eq!(cstring_lossy(b"hello".to_vec()).to_bytes(), b"hello");
    }
}
