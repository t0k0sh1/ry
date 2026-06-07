//! abi::control_flow — C boundary entry points for control-flow emission. Each
//! resolves the u32 ids / translates opaque handles into the Rust-native engine
//! types, calls the `core` `EmitCtx` method, and interns any produced value
//! (intern / resolve are abi-side; the engine method never touches them).

use std::ffi::c_char;

use crate::core::{BasicBlockRef, FunctionRef, TypeRef, ValueRef};

use super::*;

#[no_mangle]
pub unsafe extern "C" fn ry_emit_create_basic_block(
    ctx: *mut RyEmitCtx,
    name: *const c_char,
    fn_handle: RyFunctionRef,
) -> RyBasicBlockRef {
    let c = cx(ctx);
    let nm = if name.is_null() { c"".as_ptr() } else { name };
    to_ry_bb(
        c.create_basic_block(FunctionRef(as_function(fn_handle)), nm)
            .0,
    )
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_branch_cond(
    ctx: *mut RyEmitCtx,
    cond: RyValueId,
    true_bb: RyBasicBlockRef,
    false_bb: RyBasicBlockRef,
) {
    let c = cx(ctx);
    let cond_val = ValueRef(as_value(resolve(c, cond)));
    c.branch_cond(
        cond_val,
        BasicBlockRef(as_bb(true_bb)),
        BasicBlockRef(as_bb(false_bb)),
    );
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_branch_uncond(ctx: *mut RyEmitCtx, target: RyBasicBlockRef) {
    let c = cx(ctx);
    c.branch_uncond(BasicBlockRef(as_bb(target)));
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_create_phi(
    ctx: *mut RyEmitCtx,
    ty: RyTypeRef,
    incoming_values: *const RyValueId,
    incoming_blocks: *const RyBasicBlockRef,
    count: u32,
    name_hint: *const c_char,
) -> RyValueId {
    let c = cx(ctx);
    let nm = if name_hint.is_null() {
        c"".as_ptr()
    } else {
        name_hint
    };
    let mut edges: Vec<(ValueRef, BasicBlockRef)> = Vec::with_capacity(count as usize);
    for i in 0..count as usize {
        edges.push((
            ValueRef(as_value(resolve(c, *incoming_values.add(i)))),
            BasicBlockRef(as_bb(*incoming_blocks.add(i))),
        ));
    }
    let phi = c.create_phi(TypeRef(as_type(ty)), &edges, nm);
    intern(c, to_ry_value(phi.0))
}
