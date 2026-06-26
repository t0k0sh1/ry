//! abi::option — C boundary entry points for Option wrapping. Resolve the inner
//! value id / translate the opaque type handle, call the `core` `EmitCtx`
//! method, and intern the produced aggregate (intern / resolve are abi-side).

use super::*;

/// Wrap the interned `inner_id` into a `Some` of Option type `opt_ty`; return the
/// interned aggregate.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_option_wrap_some(
    ctx: *mut RyEmitCtx,
    inner_id: RyValueId,
    opt_ty: RyTypeRef,
) -> RyValueId {
    // boundary input validation: malformed callers get sentinel 0 rather than
    // feeding a NULL inner value / Option type to the aggregate build.
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let Some(opt) = req_type(opt_ty) else {
        return 0;
    };
    let Some(inner) = resolve_value(c, inner_id) else {
        return 0;
    };
    let val = c.option_wrap_some(inner, opt);
    intern(c, to_ry_value(val.0))
}

/// Build a `None` of Option type `opt_ty`; return the interned aggregate.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_option_wrap_none(
    ctx: *mut RyEmitCtx,
    opt_ty: RyTypeRef,
) -> RyValueId {
    // boundary input validation: malformed callers get sentinel 0 rather than
    // feeding a NULL Option type to the aggregate build.
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let Some(opt) = req_type(opt_ty) else {
        return 0;
    };
    let val = c.option_wrap_none(opt);
    intern(c, to_ry_value(val.0))
}
