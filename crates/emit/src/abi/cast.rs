//! abi::cast — C boundary entry point for `emitCheckedFPToInt` (#2097).
//! Resolves the u32 val_id, NULL-guards each input, calls the `core` engine
//! method, and interns the result. The string params (`bb_prefix` / `msg` /
//! `global_name`) are NULL-safe in the engine (`cstr_bytes` maps NULL → empty,
//! `get_or_create_msg_global` substitutes a default name when global_name is
//! NULL), so only ctx, val resolution, and width need guarding here.

use std::ffi::{c_char, c_int};

use super::*;

/// Emit the checked FP→int conversion sequence (FPExt(f32→f64) if needed →
/// `FCmpULT(lo) | FCmpUGE(hi)` → `CondBr` to a fresh failBB / okBB pair →
/// runtime-error exit in failBB → FPToSI / FPToUI on the original value in
/// okBB) and return the interned integer result. `target_width` is the
/// destination integer bit width (8/16/32/64); `is_signed` selects FPToSI vs
/// FPToUI and the `[-2^(W-1), 2^(W-1))` range (vs `[0, 2^W)` for unsigned).
/// `bb_prefix` / `msg` / `global_name` are borrowed C strings; the C++ caller
/// computes them up front (msg per-call-site, global_name with the
/// CodeGen-owned `fptoi_err_counter_++` baked in).
/// Precondition: the builder must be positioned within a function (BBs created
/// inside this call).
#[no_mangle]
pub unsafe extern "C" fn ry_emit_checked_fp_to_int(
    ctx: *mut RyEmitCtx,
    val_id: RyValueId,
    target_width: c_int,
    is_signed: c_int,
    bb_prefix: *const c_char,
    msg: *const c_char,
    global_name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let Some(val) = resolve_value(c, val_id) else {
        return 0;
    };
    // A zero / non-positive width has no valid LLVMIntType; reject before
    // touching the engine. (Negative c_int values also fall here.)
    if target_width <= 0 {
        return 0;
    }
    let result = c.checked_fp_to_int(
        val,
        target_width as u32,
        is_signed != 0,
        bb_prefix,
        msg,
        global_name,
    );
    intern(c, to_ry_value(result.0))
}
