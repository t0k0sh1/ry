//! Per-module string / regex literal interning caches (Stage 2, #2484).
//!
//! Two independent thread-local `HashMap<Vec<u8>, RyValueId>` caches replace
//! the C++-side `CodeGen::global_string_cache_` / `regex_global_cache_` pair.
//! Key is the literal byte content (NUL-safe — `std::string::size()` matches
//! Rust `Vec<u8>::len` exactly, including embedded NULs); value is the
//! `RyValueId` returned by `ry_emit_build_arc_global` for the corresponding
//! ARC-immortal global.
//!
//! Two caches, not one, to preserve the regex-vs-string separation invariant
//! (#306): the C++ shim stamps `addResourceKind(rk_regex)` on a regex global,
//! so a same-content string literal must reach a DIFFERENT global to keep its
//! `isStrLike()` discrimination intact. Folding both into one cache would
//! collapse the two globals and corrupt the metadata stamp.
//!
//! Lifecycle: `reset_all()` MUST be called from the C++ `CodeGen` constructor
//! (right after `emit_ctx_create`) — without it, a second `CodeGen` instance
//! on the same thread would resolve cached ids against the previous (now-dead)
//! `EmitCtx`, producing miscompiles or LLVM null-pointer crashes.
//!
//! Thread-local choice over per-`RyEmitCtx` ownership keeps the boundary
//! signature minimal (the `ctx` arg is accepted but unused — a hook for a
//! future migration to per-ctx state if parallel codegen ever ships). The
//! current compiler is single-threaded at the codegen phase
//! (`src/jit/jit_runner.cpp` instantiates `CodeGen` once per program), so
//! thread-local + constructor-reset is correct today.

use std::cell::RefCell;
use std::collections::HashMap;

use crate::handles::RyValueId;

thread_local! {
    static STRING_CACHE: RefCell<HashMap<Vec<u8>, RyValueId>> =
        RefCell::new(HashMap::new());
    static REGEX_CACHE: RefCell<HashMap<Vec<u8>, RyValueId>> =
        RefCell::new(HashMap::new());
}

pub(crate) fn lookup_string(bytes: &[u8]) -> Option<RyValueId> {
    STRING_CACHE.with(|c| c.borrow().get(bytes).copied())
}

pub(crate) fn insert_string(bytes: Vec<u8>, id: RyValueId) {
    STRING_CACHE.with(|c| {
        c.borrow_mut().insert(bytes, id);
    });
}

pub(crate) fn lookup_regex(bytes: &[u8]) -> Option<RyValueId> {
    REGEX_CACHE.with(|c| c.borrow().get(bytes).copied())
}

pub(crate) fn insert_regex(bytes: Vec<u8>, id: RyValueId) {
    REGEX_CACHE.with(|c| {
        c.borrow_mut().insert(bytes, id);
    });
}

pub(crate) fn reset_all() {
    STRING_CACHE.with(|c| c.borrow_mut().clear());
    REGEX_CACHE.with(|c| c.borrow_mut().clear());
}
