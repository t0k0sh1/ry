//! Bounds / index checking: `ry_emit_bounds_check`, negative-index wrap, and
//! the bounds-error emit sequence (`fprintf(stderr)` + `_Exit(1)` + `unreachable`).

use llvm_sys::core::*;
use llvm_sys::LLVMIntPredicate;
use std::ffi::{c_char, c_int, CStr};

use crate::ffi::*;
use crate::support::*;

#[no_mangle]
pub unsafe extern "C" fn ry_emit_bounds_check(
    ctx: *mut RyEmitCtx,
    idx_id: RyValueId,
    len_id: RyValueId,
    kind: c_int,
    global_name: *const c_char,
    bb_prefix: *const c_char,
) -> RyValueId {
    let context = cx(ctx).context;
    let b = cx(ctx).builder;
    let i1_ty = i1_type(context);
    let i64_ty = i64_type(context);
    let mut idx = as_value(resolve(cx(ctx), idx_id));
    let len = as_value(resolve(cx(ctx), len_id));
    if LLVMTypeOf(idx) == i1_ty {
        idx = LLVMBuildZExt(b, idx, i64_ty, c"idx_ext".as_ptr());
    }
    let orig_index = idx;

    // Negative-index wrap is a separate boundary function; call it through the
    // public entry so we never hold a &mut borrow across the call.
    let idx_in = intern(cx(ctx), to_ry_value(idx));
    let len_in = intern(cx(ctx), to_ry_value(len));
    let wrapped_id = ry_emit_negative_index_wrap(ctx, idx_in, len_in, bb_prefix);
    idx = as_value(resolve(cx(ctx), wrapped_id));

    let zero = LLVMConstInt(i64_ty, 0, 0);
    let p = cstr_bytes(bb_prefix);
    let neg_n = cname_pfx(p, b"_neg");
    let neg_check = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLT, idx, zero, neg_n.as_ptr());
    let over_n = cname_pfx(p, b"_over");
    let over_check = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSGE, idx, len, over_n.as_ptr());
    let oob_n = cname_pfx(p, b"_oob");
    let oob = LLVMBuildOr(b, neg_check, over_check, oob_n.as_ptr());

    let oob_block = cname_pfx(p, b".oob");
    let ok_block = cname_pfx(p, b".ok");
    // Derive parent function from the builder, not ctx->function (builder-derived
    // parent rule — .claude/rules/codegen-llvm-ir-conventions.md, #1996).
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));
    let oob_bb = LLVMAppendBasicBlockInContext(context, fn_v, oob_block.as_ptr());
    let ok_bb = LLVMAppendBasicBlockInContext(context, fn_v, ok_block.as_ptr());
    LLVMBuildCondBr(b, oob, oob_bb, ok_bb);
    LLVMPositionBuilderAtEnd(b, oob_bb);

    let fmt_msg: &CStr = if kind == RY_BOUNDS_LIST {
        c"runtime error: index %lld out of bounds for list of length %lld\n"
    } else {
        c"runtime error: index %lld out of bounds for array of length %lld\n"
    };
    let orig_in = intern(cx(ctx), to_ry_value(orig_index));
    let len_in2 = intern(cx(ctx), to_ry_value(len));
    ry_emit_bounds_error(ctx, orig_in, len_in2, fmt_msg.as_ptr(), global_name);

    LLVMPositionBuilderAtEnd(b, ok_bb);
    intern(cx(ctx), to_ry_value(idx))
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_negative_index_wrap(
    ctx: *mut RyEmitCtx,
    idx_id: RyValueId,
    wrap_base_id: RyValueId,
    prefix: *const c_char,
) -> RyValueId {
    let c = cx(ctx);
    let b = c.builder;
    let i64_ty = i64_type(c.context);
    let mut idx = as_value(resolve(c, idx_id));
    let mut wrap_base = as_value(resolve(c, wrap_base_id));
    let p = cstr_bytes(prefix);
    // Defensively normalize narrow operands to i64.
    if LLVMTypeOf(idx) != i64_ty {
        let n = cname_pfx(p, b"_idx_i64");
        idx = LLVMBuildIntCast2(b, idx, i64_ty, 1, n.as_ptr());
    }
    if LLVMTypeOf(wrap_base) != i64_ty {
        let n = cname_pfx(p, b"_wrap_base_i64");
        wrap_base = LLVMBuildIntCast2(b, wrap_base, i64_ty, 1, n.as_ptr());
    }
    let zero = LLVMConstInt(i64_ty, 0, 0);
    let n_neg = cname_pfx(p, b"_is_neg");
    let is_neg = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLT, idx, zero, n_neg.as_ptr());
    let n_wrap = cname_pfx(p, b"_wrapped");
    let wrapped = LLVMBuildAdd(b, idx, wrap_base, n_wrap.as_ptr());
    let n_idx = cname_pfx(p, b"_idx");
    let result = LLVMBuildSelect(b, is_neg, wrapped, idx, n_idx.as_ptr());
    intern(c, to_ry_value(result))
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_bounds_error(
    ctx: *mut RyEmitCtx,
    orig_idx_id: RyValueId,
    len_id: RyValueId,
    fmt_msg: *const c_char,
    global_name: *const c_char,
) {
    let c = cx(ctx);
    let b = c.builder;
    let orig_idx = as_value(resolve(c, orig_idx_id));
    let len = as_value(resolve(c, len_id));
    let ptr_ty = ptr_type(c.context);
    let i32_ty = i32_type(c.context);
    let void_ty = void_type(c.context);
    let (stdout_name, stderr_name) = if cfg!(target_os = "macos") {
        (c"__stdoutp".as_ptr(), c"__stderrp".as_ptr())
    } else {
        (c"stdout".as_ptr(), c"stderr".as_ptr())
    };
    let stderr_global = get_or_insert_global(c.module, stderr_name, ptr_ty);
    let stdout_global = get_or_insert_global(c.module, stdout_name, ptr_ty);
    let stderr_val = LLVMBuildLoad2(b, ptr_ty, stderr_global, c"stderr".as_ptr());
    let stdout_val = LLVMBuildLoad2(b, ptr_ty, stdout_global, c"stdout".as_ptr());

    // Dedup the format-string global within this ctx.
    let fmt_key = cstr_bytes(fmt_msg);
    let name_ptr = if global_name.is_null() {
        c".bounds_err_msg".as_ptr()
    } else {
        global_name
    };
    let err_msg = get_or_create_msg_global(c, fmt_key, name_ptr);

    let mut fprintf_params = [ptr_ty, ptr_ty];
    let fprintf_ty = LLVMFunctionType(i32_ty, fprintf_params.as_mut_ptr(), 2, 1);
    let fprintf_fn = get_or_insert_function(c.module, c"fprintf".as_ptr(), fprintf_ty);
    let mut fprintf_args = [stderr_val, err_msg, orig_idx, len];
    LLVMBuildCall2(
        b,
        fprintf_ty,
        fprintf_fn,
        fprintf_args.as_mut_ptr(),
        4,
        c"".as_ptr(),
    );

    let mut fflush_params = [ptr_ty];
    let fflush_ty = LLVMFunctionType(i32_ty, fflush_params.as_mut_ptr(), 1, 0);
    let fflush_fn = get_or_insert_function(c.module, c"fflush".as_ptr(), fflush_ty);
    let mut a_out = [stdout_val];
    LLVMBuildCall2(b, fflush_ty, fflush_fn, a_out.as_mut_ptr(), 1, c"".as_ptr());
    let mut a_err = [stderr_val];
    LLVMBuildCall2(b, fflush_ty, fflush_fn, a_err.as_mut_ptr(), 1, c"".as_ptr());

    let mut exit_params = [i32_ty];
    let exit_ty = LLVMFunctionType(void_ty, exit_params.as_mut_ptr(), 1, 0);
    let exit_fn = get_or_insert_function(c.module, c"_Exit".as_ptr(), exit_ty);
    let mut ea = [LLVMConstInt(i32_ty, 1, 0)];
    LLVMBuildCall2(b, exit_ty, exit_fn, ea.as_mut_ptr(), 1, c"".as_ptr());
    LLVMBuildUnreachable(b);
}
