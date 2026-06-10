//! abi::option — C boundary entry points for Option wrapping. Resolve the inner
//! value id / translate the opaque type handle, call the `core` `EmitCtx`
//! method, and intern the produced aggregate (intern / resolve are abi-side).

use crate::core::{TypeRef, ValueRef};

use super::*;

/// Wrap the interned `inner_id` into a `Some` of Option type `opt_ty`; return the
/// interned aggregate.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_option_wrap_some(
    ctx: *mut RyEmitCtx,
    inner_id: RyValueId,
    opt_ty: RyTypeRef,
) -> RyValueId {
    let c = cx(ctx);
    let inner = ValueRef(as_value(resolve(c, inner_id)));
    let val = c.option_wrap_some(inner, TypeRef(as_type(opt_ty)));
    intern(c, to_ry_value(val.0))
}

/// Build a `None` of Option type `opt_ty`; return the interned aggregate.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_option_wrap_none(
    ctx: *mut RyEmitCtx,
    opt_ty: RyTypeRef,
) -> RyValueId {
    let c = cx(ctx);
    let val = c.option_wrap_none(TypeRef(as_type(opt_ty)));
    intern(c, to_ry_value(val.0))
}
