//! `ry_lower_*` boundary entries — the C-callable surface of the upper
//! codegen Rust layer.
//!
//! Discipline (mirrors `crates/emit/src/abi.rs`): this layer translates
//! mechanically — null-checks the handles, dispatches to a body, returns
//! the interned result. No Ry-semantic decisions live here directly; the
//! bodies are the seams that future stages factor into per-stage modules.

use crate::emit_extern::ry_emit_const_int;
use crate::error::clear_last_error;
use crate::expr::{lower_float_const, lower_int_const, lower_regex_const, lower_string_const};
use crate::handles::{RyEmitCtx, RyTypeRef, RyValueId};
use crate::intern;

/// Borrow `(ptr, len)` as `&[u8]` under the C `(pointer, count)` contract.
/// Returns `Some(&[])` for a zero-count slice (NULL pointer permitted) and
/// `None` for a non-zero count with a NULL pointer (the entry's invalid
/// sentinel — `slice::from_raw_parts` requires a non-NULL base even for
/// length-0). Mirrors `crates/emit/src/abi::ffi_slice` (which takes a
/// `u32` count); the lower crate keeps its own copy because it has no
/// emit-crate dependency.
#[inline]
unsafe fn opt_byte_slice<'a>(ptr: *const u8, len: usize) -> Option<&'a [u8]> {
    if len == 0 {
        Some(&[])
    } else if ptr.is_null() {
        None
    } else {
        Some(core::slice::from_raw_parts(ptr, len))
    }
}

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

/// Materialize an ARC-immortal global for a Ry `StringExpr` (Stage 2 of
/// the upper-codegen migration, #2484).
///
/// `bytes[0..len)` is the raw literal content (binary-safe — embedded
/// NULs preserved). `name_hint_bytes[0..name_hint_len)` is the LLVM
/// global name prefix (the C++ `cachedGlobalString` callers pass
/// `.str`, `.fmt`, `.err_msg`, …) — passed by length, NOT as a
/// NUL-terminated C string, because `llvm::Twine::toStringRef` does not
/// guarantee a NUL terminator for multi-node Twines.
///
/// Cache hit returns the previously-interned `RyValueId`; cache miss
/// calls `ry_emit_build_arc_global` and caches the result keyed on the
/// byte content. Two distinct caches (string vs regex) preserve the
/// regex separation invariant from #306.
///
/// Calls `clear_last_error()` at entry for boundary uniformity with the
/// other `ry_lower_*` entries, though no failure path is currently
/// exercised. Returns `0` only when `bytes` is NULL with non-zero `len`,
/// when `name_hint_bytes` is NULL with non-zero `name_hint_len`, or
/// when the underlying `ry_emit_build_arc_global` fails on a NULL ctx.
///
/// # Safety
///
/// `ctx` must be a valid `RyEmitCtx *` (or NULL); `bytes` must point to
/// at least `len` bytes (or be NULL when `len == 0`); `name_hint_bytes`
/// must point to at least `name_hint_len` bytes (or be NULL when
/// `name_hint_len == 0`).
#[no_mangle]
pub unsafe extern "C" fn ry_lower_string_const(
    ctx: *mut RyEmitCtx,
    bytes: *const u8,
    len: usize,
    name_hint_bytes: *const u8,
    name_hint_len: usize,
) -> RyValueId {
    // Clear any stale diagnostic from a prior `ry_lower_*` call on this
    // thread before we run — this entry does not itself populate
    // `set_last_error`, but the cross-stage contract requires that a
    // post-call `ry_lower_get_last_error` reflect THIS call's outcome
    // (`NULL` on success) rather than a prior failed entry's message.
    clear_last_error();
    let (Some(slice), Some(name)) = (
        opt_byte_slice(bytes, len),
        opt_byte_slice(name_hint_bytes, name_hint_len),
    ) else {
        return 0;
    };
    lower_string_const(ctx, slice, name)
}

/// Materialize an ARC-immortal global for a Ry `RegexExpr` (Stage 2 of
/// the upper-codegen migration, #2484).
///
/// Separate cache from `ry_lower_string_const` so a string `"hello"`
/// and a regex `/hello/` produce DIFFERENT globals — the C++ shim then
/// stamps `addResourceKind(rk_regex)` on the regex global without
/// poisoning the string-side counterpart (#306). The LLVM global name
/// prefix is hardcoded to `.regex`.
///
/// # Safety
///
/// `ctx` must be a valid `RyEmitCtx *` (or NULL); `bytes` must point to
/// at least `len` bytes (or be NULL when `len == 0`).
#[no_mangle]
pub unsafe extern "C" fn ry_lower_regex_const(
    ctx: *mut RyEmitCtx,
    bytes: *const u8,
    len: usize,
) -> RyValueId {
    // See `ry_lower_string_const` for the clear-stale-error rationale.
    clear_last_error();
    let Some(slice) = opt_byte_slice(bytes, len) else {
        return 0;
    };
    lower_regex_const(ctx, slice)
}

/// Clear the per-module thread-local string / regex interning caches.
/// MUST be called from the C++ `CodeGen` constructor immediately after
/// `ry_emit_ctx_create`. Sequential `CodeGen` instances on the same
/// thread would otherwise resolve cached ids against the previous (now
/// freed) `EmitCtx`, producing miscompiles or LLVM null-pointer
/// dereferences.
///
/// The `_ctx` parameter is unused today; it's accepted to signal that
/// this hook is conceptually per-`EmitCtx` (a future migration to
/// per-ctx state can change the implementation without changing the
/// ABI shape).
///
/// # Safety
///
/// `_ctx` is currently unused; it may be NULL or a valid pointer.
#[no_mangle]
pub unsafe extern "C" fn ry_lower_reset_module_state(_ctx: *mut RyEmitCtx) {
    intern::reset_all();
}
