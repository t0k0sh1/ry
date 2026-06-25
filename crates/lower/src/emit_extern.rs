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
}
