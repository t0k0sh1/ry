//! `ry_lower_*` boundary entries — the C-callable surface of the upper
//! codegen Rust layer.
//!
//! Discipline (mirrors `crates/emit/src/abi.rs`): this layer translates
//! mechanically — null-checks the handles, dispatches to a body, returns
//! the interned result. No Ry-semantic decisions live here directly; the
//! bodies are the seams that future stages factor into per-stage modules.

use crate::emit_extern::ry_emit_const_int;
use crate::error::clear_last_error;
use crate::expr::{lower_float_const, lower_int_const};
use crate::handles::{RyEmitCtx, RyTypeRef, RyValueId};

/// Materialize an `i1` constant for a Ry `BoolExpr`.
///
/// Pilot port of `CodeGen::emitExprVariant(const BoolExpr &e)` —
/// `value = 0` for `false`, non-zero for `true`. The C++ caller passes
/// its own `i1Ty_` (LLVM `llvm::Type *`) cast to `RyTypeRef`; this
/// function does not synthesize the type. Sign-extend is zero (`i1` is
/// unsigned in the original `ConstantInt::get(i1Ty_, value, false)`).
///
/// Returns the interned `RyValueId` from `ry_emit_const_int`; the C++
/// caller resolves it via `ry_emit_resolve`. NULL `ctx` / `i1_ty` → `0`
/// (delegated to `ry_emit_const_int`).
///
/// # Safety
///
/// `ctx` must be a valid `RyEmitCtx *` produced by `ry_emit_ctx_create`
/// (or NULL); `i1_ty` must be a valid LLVM `i1` type handle (or NULL).
#[no_mangle]
pub unsafe extern "C" fn ry_lower_bool_const(
    ctx: *mut RyEmitCtx,
    i1_ty: RyTypeRef,
    value: core::ffi::c_int,
) -> RyValueId {
    // C `int` arg, not `bool`, to keep the boundary in the C scalar
    // domain (matches the `int sign_extend` convention used across
    // `ry_emit_*`). Any non-zero value is `true`.
    let normalized: u64 = if value != 0 { 1 } else { 0 };
    ry_emit_const_int(ctx, i1_ty, normalized, 0)
}

/// Materialize an integer constant for a Ry `NumberExpr` (Stage 1 of the
/// upper-codegen migration, #2483).
///
/// `llvm_ty` is the LLVM type the C++ shim resolved from the suffix
/// (`resolveType(suffix)`, or `i64Ty_` when the suffix is empty).
/// `magnitude` is the parser-provided unsigned bit pattern (negative
/// literals arrive as `UnaryExpr` and never reach this entry).
/// `suffix_kind` is the `RyLowerSuffixKind` enum value (see
/// `include/ry/lower/api.h`); `signed_bit` matches the original
/// `ConstantInt::get(.., isSigned)` flag (1 = signed, 0 = unsigned).
///
/// On range overflow or bare-int negative bit-pattern, returns `0` after
/// recording a diagnostic via `set_last_error`. The C++ shim checks
/// `id == 0` and surfaces the message through `codegenError`.
///
/// Calls `clear_last_error()` at entry so `id == 0` always pairs with a
/// fresh message set inside this call (cross-stage boundary contract).
///
/// # Safety
///
/// `ctx` must be a valid `RyEmitCtx *` produced by `ry_emit_ctx_create`
/// (or NULL); `llvm_ty` must be a valid LLVM integer type handle (or
/// NULL).
#[no_mangle]
pub unsafe extern "C" fn ry_lower_int_const(
    ctx: *mut RyEmitCtx,
    llvm_ty: RyTypeRef,
    magnitude: u64,
    suffix_kind: u8,
    signed_bit: u8,
) -> RyValueId {
    clear_last_error();
    lower_int_const(ctx, llvm_ty, magnitude, suffix_kind, signed_bit)
}

/// Materialize a floating-point constant for a Ry `FloatExpr` (Stage 1 of
/// the upper-codegen migration, #2483).
///
/// `llvm_ty` is the LLVM type the C++ shim selected from the suffix
/// (`f64Ty_` for empty / `f64`, `f32Ty_` for `f32`). `value` is the
/// parser-provided double. `suffix_kind` is the `RyLowerSuffixKind` enum
/// value for the boundary protocol (None / F32 / F64 expected).
///
/// Has no error path under normal operation; calls `clear_last_error()`
/// at entry for boundary uniformity (defensive against future failure
/// modes — same contract as `ry_lower_int_const`).
///
/// # Safety
///
/// `ctx` must be a valid `RyEmitCtx *` (or NULL); `llvm_ty` must be a
/// valid LLVM float type handle (or NULL).
#[no_mangle]
pub unsafe extern "C" fn ry_lower_float_const(
    ctx: *mut RyEmitCtx,
    llvm_ty: RyTypeRef,
    value: f64,
    suffix_kind: u8,
) -> RyValueId {
    clear_last_error();
    lower_float_const(ctx, llvm_ty, value, suffix_kind)
}
