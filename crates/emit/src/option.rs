//! Option emission: `ry_emit_option_wrap_some` / `ry_emit_option_wrap_none`.

use llvm_sys::core::*;

use crate::ffi::*;
use crate::support::*;

#[no_mangle]
pub unsafe extern "C" fn ry_emit_option_wrap_some(
    ctx: *mut RyEmitCtx,
    inner_id: RyValueId,
    opt_ty: RyTypeRef,
) -> RyValueId {
    let c = cx(ctx);
    let opt_ty = as_type(opt_ty);
    let inner = as_value(resolve(c, inner_id));
    let i1_ty = i1_type(c.context);
    let mut val = LLVMGetUndef(opt_ty);
    val = LLVMBuildInsertValue(c.builder, val, LLVMConstInt(i1_ty, 1, 0), 0, c"".as_ptr());
    val = LLVMBuildInsertValue(c.builder, val, inner, 1, c"".as_ptr());
    intern(c, to_ry_value(val))
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_option_wrap_none(
    ctx: *mut RyEmitCtx,
    opt_ty: RyTypeRef,
) -> RyValueId {
    let c = cx(ctx);
    let opt_ty = as_type(opt_ty);
    let i1_ty = i1_type(c.context);
    let mut val = LLVMGetUndef(opt_ty);
    val = LLVMBuildInsertValue(c.builder, val, LLVMConstInt(i1_ty, 0, 0), 0, c"".as_ptr());
    let payload_ty = LLVMStructGetTypeAtIndex(opt_ty, 1);
    val = LLVMBuildInsertValue(c.builder, val, LLVMGetUndef(payload_ty), 1, c"".as_ptr());
    intern(c, to_ry_value(val))
}
