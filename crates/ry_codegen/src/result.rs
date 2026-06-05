//! Result emission: build an `Error` struct from a runtime error function
//! (`ry_emit_build_error_from_runtime`) and the Result ok/err branch + phi
//! (`ry_emit_result_branch`).

use llvm_sys::core::*;
use std::ffi::{c_char, c_void};

use crate::ffi::*;
use crate::support::*;

#[no_mangle]
pub unsafe extern "C" fn ry_emit_build_error_from_runtime(
    ctx: *mut RyEmitCtx,
    err_fn_name: *const c_char,
    error_ty: RyTypeRef,
) -> RyValueId {
    // boundary input validation: malformed callers get sentinel 0 instead of
    // crashing the emitter (mirrors ry_emit_runtime_call's guards).
    if ctx.is_null() || err_fn_name.is_null() || error_ty.is_null() {
        return 0;
    }
    let c = cx(ctx);
    if c.context.is_null() || c.module.is_null() || c.builder.is_null() {
        return 0;
    }
    let error_ty = as_type(error_ty);
    let ptr_ty = ptr_type(c.context);
    let i64_ty = i64_type(c.context);
    let err_fn_ty = LLVMFunctionType(ptr_ty, std::ptr::null_mut(), 0, 0);
    let err_fn = get_or_insert_function(c.module, err_fn_name, err_fn_ty);
    let err_msg = LLVMBuildCall2(
        c.builder,
        err_fn_ty,
        err_fn,
        std::ptr::null_mut(),
        0,
        c"err_msg".as_ptr(),
    );
    let mut err_struct = LLVMGetUndef(error_ty);
    err_struct = LLVMBuildInsertValue(c.builder, err_struct, err_msg, 0, c"err.msg".as_ptr());
    err_struct = LLVMBuildInsertValue(
        c.builder,
        err_struct,
        LLVMConstInt(i64_ty, 0, 0),
        1,
        c"err.code".as_ptr(),
    );
    intern(c, to_ry_value(err_struct))
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_result_branch(
    ctx: *mut RyEmitCtx,
    is_err_id: RyValueId,
    res_ty: RyTypeRef,
    build_ok: RyBuildValueFn,
    build_err: RyBuildValueFn,
    user_ctx: *mut c_void,
) -> RyValueId {
    // boundary input validation: malformed callers get sentinel 0 instead of
    // crashing the emitter. build_ok / build_err are nullable at the FFI
    // boundary (RyBuildValueFn = Option<fn>); reject NULL here rather than
    // panicking on `unwrap` across the extern "C" boundary.
    if ctx.is_null() || res_ty.is_null() {
        return 0;
    }
    let (Some(build_ok), Some(build_err)) = (build_ok, build_err) else {
        return 0;
    };
    let context = cx(ctx).context;
    let b = cx(ctx).builder;
    if context.is_null() || b.is_null() {
        return 0;
    }
    let is_err = as_value(resolve(cx(ctx), is_err_id));
    let res_ty = as_type(res_ty);
    // Builder-derived parent function (builder-derived parent rule).
    let insert_bb = LLVMGetInsertBlock(b);
    if insert_bb.is_null() {
        return 0;
    }
    let fn_v = LLVMGetBasicBlockParent(insert_bb);
    let ok_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.ok".as_ptr());
    let err_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.err".as_ptr());
    let merge_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.merge".as_ptr());
    LLVMBuildCondBr(b, is_err, err_bb, ok_bb);

    LLVMPositionBuilderAtEnd(b, ok_bb);
    let ok_id = build_ok(user_ctx);
    let ok_val = as_value(resolve(cx(ctx), ok_id));
    LLVMBuildBr(b, merge_bb);
    // Re-capture the incoming block: the callback may have advanced the builder
    // through additional BBs (load-bearing).
    let ok_in = LLVMGetInsertBlock(b);

    LLVMPositionBuilderAtEnd(b, err_bb);
    let err_id = build_err(user_ctx);
    let err_val = as_value(resolve(cx(ctx), err_id));
    LLVMBuildBr(b, merge_bb);
    let err_in = LLVMGetInsertBlock(b);

    LLVMPositionBuilderAtEnd(b, merge_bb);
    let phi = LLVMBuildPhi(b, res_ty, c"result".as_ptr());
    let mut vals = [ok_val, err_val];
    let mut blocks = [ok_in, err_in];
    LLVMAddIncoming(phi, vals.as_mut_ptr(), blocks.as_mut_ptr(), 2);
    intern(cx(ctx), to_ry_value(phi))
}
