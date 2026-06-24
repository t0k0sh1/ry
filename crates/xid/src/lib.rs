// ECMAScript IdentifierName predicates for json5 unquoted keys (#2314).
//
// The C++ caller in `src/runtime/native/json5.cpp` decodes a UTF-8
// codepoint, short-circuits on ASCII, and only invokes these for cp >=
// 0x80. Surrogate-half codepoints (U+D800..=U+DFFF) and out-of-range
// values fail `char::from_u32`, so both predicates return `false` for
// them — same outcome as the C++ side returning before the FFI hop, kept
// here as a belt-and-suspenders guard against any caller that hasn't
// already filtered.
//
// XID vs ID: the ECMAScript spec literally references ID_Start /
// ID_Continue, but XID_Start / XID_Continue are the NFKC-stable subset
// (the difference is a handful of obscure characters that other JSON5
// implementations also exclude in practice). If strict ID conformance
// becomes a requirement, swap the dependency without changing the FFI.

#[no_mangle]
pub extern "C" fn __ry_xid_start(cp: u32) -> bool {
    char::from_u32(cp).is_some_and(unicode_ident::is_xid_start)
}

#[no_mangle]
pub extern "C" fn __ry_xid_continue(cp: u32) -> bool {
    char::from_u32(cp).is_some_and(unicode_ident::is_xid_continue)
}
