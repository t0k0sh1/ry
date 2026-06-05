//! Control-flow emission: basic-block creation, conditional / unconditional
//! branches, and PHI nodes.

use llvm_sys::core::*;
use std::ffi::c_char;

use crate::ffi::*;
use crate::support::*;

#[no_mangle]
pub unsafe extern "C" fn ry_emit_create_basic_block(
    ctx: *mut RyEmitCtx,
    name: *const c_char,
    fn_handle: RyFunctionRef,
) -> RyBasicBlockRef {
    let c = cx(ctx);
    let nm = if name.is_null() { c"".as_ptr() } else { name };
    let bb = LLVMAppendBasicBlockInContext(c.context, as_function(fn_handle), nm);
    to_ry_bb(bb)
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_branch_cond(
    ctx: *mut RyEmitCtx,
    cond: RyValueId,
    true_bb: RyBasicBlockRef,
    false_bb: RyBasicBlockRef,
) {
    let c = cx(ctx);
    let cond_val = as_value(resolve(c, cond));
    LLVMBuildCondBr(c.builder, cond_val, as_bb(true_bb), as_bb(false_bb));
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_branch_uncond(ctx: *mut RyEmitCtx, target: RyBasicBlockRef) {
    let c = cx(ctx);
    LLVMBuildBr(c.builder, as_bb(target));
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
    let phi = LLVMBuildPhi(c.builder, as_type(ty), nm);
    for i in 0..count as usize {
        let mut v = [as_value(resolve(c, *incoming_values.add(i)))];
        let mut bb = [as_bb(*incoming_blocks.add(i))];
        LLVMAddIncoming(phi, v.as_mut_ptr(), bb.as_mut_ptr(), 1);
    }
    intern(c, to_ry_value(phi))
}
