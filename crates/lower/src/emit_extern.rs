//! `extern "C"` declarations for the subset of `ry_emit_*` symbols this
//! crate calls. Resolved at runtime from the loaded `emit` cdylib via
//! the host process's symbol table.
//!
//! Mirrors the C signatures in `include/ry/llvm_emit/api.h`; if a
//! signature there changes, this file must change in lockstep.

use crate::handles::{RyEmitCtx, RyTypeRef, RyValueId};
use core::ffi::c_int;

extern "C" {
    /// `LLVMConstInt(ty, value, sign_extend)` — materialize an integer
    /// constant and return the interned handle. `sign_extend = 0` is
    /// zero-extend. NULL ctx / type → 0.
    ///
    /// See `include/ry/llvm_emit/api.h` `ry_emit_const_int`.
    pub(crate) fn ry_emit_const_int(
        ctx: *mut RyEmitCtx,
        ty: RyTypeRef,
        value: u64,
        sign_extend: c_int,
    ) -> RyValueId;

    /// `LLVMConstReal(ty, value)` — materialize a floating-point constant
    /// and return the interned handle. `ty` selects f32 vs f64; the f64
    /// argument is narrowed to the target type's float layout. NULL ctx /
    /// type → 0. Added for upper-codegen Stage 1 (#2483).
    ///
    /// See `include/ry/llvm_emit/api.h` `ry_emit_const_fp`.
    pub(crate) fn ry_emit_const_fp(ctx: *mut RyEmitCtx, ty: RyTypeRef, value: f64) -> RyValueId;

    /// Build a StringHeader-prefixed ARC-immortal global for `bytes[0..len)`
    /// and intern the in-bounds-GEP `ptr` to its first payload byte. NULL
    /// ctx → 0. Uncached at this layer — the lower crate manages two
    /// independent caches (string + regex) so the regex separation
    /// invariant (#306) holds even when both literal kinds share content.
    /// The name hint is passed by length (not NUL-terminated) because
    /// `llvm::Twine::toStringRef` does NOT guarantee a NUL on the C++
    /// side. Added for upper-codegen Stage 2 (#2484).
    ///
    /// See `include/ry/llvm_emit/api.h` `ry_emit_build_arc_global`.
    pub(crate) fn ry_emit_build_arc_global(
        ctx: *mut RyEmitCtx,
        bytes: *const u8,
        len: usize,
        name_hint_bytes: *const u8,
        name_hint_len: usize,
    ) -> RyValueId;
}
