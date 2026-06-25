//! `ry_lower_*` boundary entries — the C-callable surface of the upper
//! codegen Rust layer.
//!
//! Discipline (mirrors `crates/emit/src/abi.rs`): this layer translates
//! mechanically — null-checks the handles, dispatches to a body, returns
//! the interned result. No Ry-semantic decisions live here directly; the
//! bodies are the seams that future stages factor into per-stage modules.

use crate::emit_extern::ry_emit_const_int;
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
