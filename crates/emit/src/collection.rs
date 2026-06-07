//! List collection mutation: append / insert / remove_at / slice.

use llvm_sys::core::*;
use llvm_sys::LLVMIntPredicate;

use crate::abi::*;
use crate::bounds::*;
use crate::core::*;

#[no_mangle]
pub unsafe extern "C" fn ry_emit_collection_append(
    ctx: *mut RyEmitCtx,
    list_ptr_id: RyValueId,
    val_id: RyValueId,
    list_header_ty: RyTypeRef,
    elem_ty: RyTypeRef,
    elem_size: u64,
) {
    let c = cx(ctx);
    let b = c.builder;
    let context = c.context;
    let module = c.module;
    let list_ptr = as_value(resolve(c, list_ptr_id));
    let val = as_value(resolve(c, val_id));
    let list_header_ty = as_type(list_header_ty);
    let elem_ty = as_type(elem_ty);
    let i64_ty = i64_type(context);
    let ptr_ty = ptr_type(context);
    let void_ty = void_type(context);
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

    let h = load_list_header(b, list_header_ty, list_ptr, i64_ty, ptr_ty, b"app");

    let need_grow = LLVMBuildICmp(
        b,
        LLVMIntPredicate::LLVMIntEQ,
        h.len,
        h.cap,
        c"app_need_grow".as_ptr(),
    );
    let grow_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"app.grow".as_ptr());
    let store_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"app.store".as_ptr());
    LLVMBuildCondBr(b, need_grow, grow_bb, store_bb);

    emit_list_grow(
        b,
        module,
        &h,
        elem_size,
        i64_ty,
        ptr_ty,
        void_ty,
        b"app",
        c"cap_gt4".as_ptr(),
        grow_bb,
        store_bb,
    );

    LLVMPositionBuilderAtEnd(b, store_bb);
    let cur_data = LLVMBuildLoad2(b, ptr_ty, h.data_ptr, c"app_cur_data".as_ptr());
    let cur_len = LLVMBuildLoad2(b, i64_ty, h.len_ptr, c"app_cur_len".as_ptr());
    let mut elem_idx = [cur_len];
    let elem_ptr = LLVMBuildGEP2(
        b,
        elem_ty,
        cur_data,
        elem_idx.as_mut_ptr(),
        1,
        c"app_elem_ptr".as_ptr(),
    );
    LLVMBuildStore(b, val, elem_ptr);
    let new_len = LLVMBuildAdd(
        b,
        cur_len,
        LLVMConstInt(i64_ty, 1, 0),
        c"app_new_len".as_ptr(),
    );
    LLVMBuildStore(b, new_len, h.len_ptr);
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_collection_insert(
    ctx: *mut RyEmitCtx,
    list_ptr_id: RyValueId,
    idx_id: RyValueId,
    val_id: RyValueId,
    list_header_ty: RyTypeRef,
    elem_ty: RyTypeRef,
    elem_size: u64,
) {
    let context = cx(ctx).context;
    let b = cx(ctx).builder;
    let module = cx(ctx).module;
    let list_ptr = as_value(resolve(cx(ctx), list_ptr_id));
    let orig_idx = as_value(resolve(cx(ctx), idx_id));
    let val = as_value(resolve(cx(ctx), val_id));
    let list_header_ty = as_type(list_header_ty);
    let elem_ty = as_type(elem_ty);
    let i64_ty = i64_type(context);
    let ptr_ty = ptr_type(context);
    let void_ty = void_type(context);
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

    let h = load_list_header(b, list_header_ty, list_ptr, i64_ty, ptr_ty, b"ins");

    // insert() valid range is [0, len], so negative -k maps to len+1-k.
    let wrap_base = LLVMBuildAdd(
        b,
        h.len,
        LLVMConstInt(i64_ty, 1, 0),
        c"ins_wrap_base".as_ptr(),
    );
    let orig_in = intern(cx(ctx), to_ry_value(orig_idx));
    let wrap_in = intern(cx(ctx), to_ry_value(wrap_base));
    let wrapped_id = ry_emit_negative_index_wrap(ctx, orig_in, wrap_in, c"ins".as_ptr());
    let idx = as_value(resolve(cx(ctx), wrapped_id));

    let zero = LLVMConstInt(i64_ty, 0, 0);
    let neg_check = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLT, idx, zero, c"".as_ptr());
    let over_check = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSGT, idx, h.len, c"".as_ptr());
    let out_of_bounds = LLVMBuildOr(b, neg_check, over_check, c"ins_oob".as_ptr());
    let err_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"ins.err".as_ptr());
    let ok_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"ins.ok".as_ptr());
    LLVMBuildCondBr(b, out_of_bounds, err_bb, ok_bb);
    LLVMPositionBuilderAtEnd(b, err_bb);
    let orig_in2 = intern(cx(ctx), to_ry_value(orig_idx));
    let len_in = intern(cx(ctx), to_ry_value(h.len));
    ry_emit_bounds_error(
        ctx,
        orig_in2,
        len_in,
        c"runtime error: index %lld out of bounds for insert() on list of length %lld\n".as_ptr(),
        c".ins_oob_err".as_ptr(),
    );

    LLVMPositionBuilderAtEnd(b, ok_bb);
    let need_grow = LLVMBuildICmp(
        b,
        LLVMIntPredicate::LLVMIntEQ,
        h.len,
        h.cap,
        c"ins_need_grow".as_ptr(),
    );
    let grow_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"ins.grow".as_ptr());
    let move_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"ins.move".as_ptr());
    LLVMBuildCondBr(b, need_grow, grow_bb, move_bb);

    emit_list_grow(
        b,
        module,
        &h,
        elem_size,
        i64_ty,
        ptr_ty,
        void_ty,
        b"ins",
        c"".as_ptr(),
        grow_bb,
        move_bb,
    );

    LLVMPositionBuilderAtEnd(b, move_bb);
    let cur_data = LLVMBuildLoad2(b, ptr_ty, h.data_ptr, c"ins_cur_data".as_ptr());
    let mut src_idx = [idx];
    let src_ptr = LLVMBuildGEP2(
        b,
        elem_ty,
        cur_data,
        src_idx.as_mut_ptr(),
        1,
        c"ins_src".as_ptr(),
    );
    let idx_plus_one = LLVMBuildAdd(b, idx, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
    let mut dst_idx = [idx_plus_one];
    let dst_ptr = LLVMBuildGEP2(
        b,
        elem_ty,
        cur_data,
        dst_idx.as_mut_ptr(),
        1,
        c"ins_dst".as_ptr(),
    );
    let move_count = LLVMBuildSub(b, h.len, idx, c"ins_move_count".as_ptr());
    let move_bytes = LLVMBuildMul(
        b,
        move_count,
        LLVMConstInt(i64_ty, elem_size, 0),
        c"ins_move_bytes".as_ptr(),
    );
    emit_memmove(b, module, ptr_ty, i64_ty, dst_ptr, src_ptr, move_bytes);
    let mut ins_idx = [idx];
    let insert_ptr = LLVMBuildGEP2(
        b,
        elem_ty,
        cur_data,
        ins_idx.as_mut_ptr(),
        1,
        c"ins_ptr".as_ptr(),
    );
    LLVMBuildStore(b, val, insert_ptr);
    let new_len = LLVMBuildAdd(
        b,
        h.len,
        LLVMConstInt(i64_ty, 1, 0),
        c"ins_new_len".as_ptr(),
    );
    LLVMBuildStore(b, new_len, h.len_ptr);
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_collection_remove_at(
    ctx: *mut RyEmitCtx,
    list_ptr_id: RyValueId,
    idx_id: RyValueId,
    list_header_ty: RyTypeRef,
    elem_ty: RyTypeRef,
    elem_size: u64,
) -> RyValueId {
    let context = cx(ctx).context;
    let b = cx(ctx).builder;
    let module = cx(ctx).module;
    let list_ptr = as_value(resolve(cx(ctx), list_ptr_id));
    let orig_idx = as_value(resolve(cx(ctx), idx_id));
    let list_header_ty = as_type(list_header_ty);
    let elem_ty = as_type(elem_ty);
    let i64_ty = i64_type(context);
    let ptr_ty = ptr_type(context);
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

    let h = load_list_header(b, list_header_ty, list_ptr, i64_ty, ptr_ty, b"rmat");

    let orig_in = intern(cx(ctx), to_ry_value(orig_idx));
    let len_in = intern(cx(ctx), to_ry_value(h.len));
    let wrapped_id = ry_emit_negative_index_wrap(ctx, orig_in, len_in, c"rmat".as_ptr());
    let idx = as_value(resolve(cx(ctx), wrapped_id));

    let zero = LLVMConstInt(i64_ty, 0, 0);
    let neg_check = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLT, idx, zero, c"".as_ptr());
    let over_check = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSGE, idx, h.len, c"".as_ptr());
    let out_of_bounds = LLVMBuildOr(b, neg_check, over_check, c"rmat_oob".as_ptr());
    let err_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"rmat.err".as_ptr());
    let ok_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"rmat.ok".as_ptr());
    LLVMBuildCondBr(b, out_of_bounds, err_bb, ok_bb);
    LLVMPositionBuilderAtEnd(b, err_bb);
    let orig_in2 = intern(cx(ctx), to_ry_value(orig_idx));
    let len_in2 = intern(cx(ctx), to_ry_value(h.len));
    ry_emit_bounds_error(
        ctx,
        orig_in2,
        len_in2,
        c"runtime error: index %lld out of bounds for removeAt() on list of length %lld\n".as_ptr(),
        c".rmat_oob_err".as_ptr(),
    );

    LLVMPositionBuilderAtEnd(b, ok_bb);
    let mut elem_idx = [idx];
    let elem_ptr = LLVMBuildGEP2(
        b,
        elem_ty,
        h.data,
        elem_idx.as_mut_ptr(),
        1,
        c"rmat_elem_ptr".as_ptr(),
    );
    let removed_val = LLVMBuildLoad2(b, elem_ty, elem_ptr, c"rmat_val".as_ptr());
    let idx_plus_one = LLVMBuildAdd(b, idx, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
    let mut src_idx = [idx_plus_one];
    let src_ptr = LLVMBuildGEP2(
        b,
        elem_ty,
        h.data,
        src_idx.as_mut_ptr(),
        1,
        c"rmat_src".as_ptr(),
    );
    let len_minus_idx = LLVMBuildSub(b, h.len, idx, c"".as_ptr());
    let move_count = LLVMBuildSub(
        b,
        len_minus_idx,
        LLVMConstInt(i64_ty, 1, 0),
        c"rmat_move_count".as_ptr(),
    );
    let move_bytes = LLVMBuildMul(
        b,
        move_count,
        LLVMConstInt(i64_ty, elem_size, 0),
        c"rmat_move_bytes".as_ptr(),
    );
    emit_memmove(b, module, ptr_ty, i64_ty, elem_ptr, src_ptr, move_bytes);
    let new_len = LLVMBuildSub(
        b,
        h.len,
        LLVMConstInt(i64_ty, 1, 0),
        c"rmat_new_len".as_ptr(),
    );
    LLVMBuildStore(b, new_len, h.len_ptr);

    intern(cx(ctx), to_ry_value(removed_val))
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_list_slice(
    ctx: *mut RyEmitCtx,
    list_ptr_id: RyValueId,
    start_id: RyValueId,
    end_excl_id: RyValueId,
    list_header_ty: RyTypeRef,
    elem_ty: RyTypeRef,
    elem_size: u64,
    out_count: *mut RyValueId,
    out_new_data: *mut RyValueId,
) {
    let c = cx(ctx);
    let b = c.builder;
    let list_ptr = as_value(resolve(c, list_ptr_id));
    let start_val = as_value(resolve(c, start_id));
    let end_excl_val = as_value(resolve(c, end_excl_id));
    let list_header_ty = as_type(list_header_ty);
    let elem_ty = as_type(elem_ty);
    let i64_ty = i64_type(c.context);
    let ptr_ty = ptr_type(c.context);

    let sl_len_ptr = LLVMBuildStructGEP2(b, list_header_ty, list_ptr, 0, c"sl_len_ptr".as_ptr());
    let sl_data_ptr = LLVMBuildStructGEP2(b, list_header_ty, list_ptr, 2, c"sl_data_ptr".as_ptr());
    let sl_len = LLVMBuildLoad2(b, i64_ty, sl_len_ptr, c"sl_len".as_ptr());
    let sl_data = LLVMBuildLoad2(b, ptr_ty, sl_data_ptr, c"sl_data".as_ptr());

    let zero = LLVMConstInt(i64_ty, 0, 0);
    let start_neg = LLVMBuildICmp(
        b,
        LLVMIntPredicate::LLVMIntSLT,
        start_val,
        zero,
        c"".as_ptr(),
    );
    let mut c_start = LLVMBuildSelect(b, start_neg, zero, start_val, c"sl_cstart".as_ptr());
    let start_over = LLVMBuildICmp(
        b,
        LLVMIntPredicate::LLVMIntSGT,
        c_start,
        sl_len,
        c"".as_ptr(),
    );
    c_start = LLVMBuildSelect(b, start_over, sl_len, c_start, c"sl_cstart2".as_ptr());
    let end_neg = LLVMBuildICmp(
        b,
        LLVMIntPredicate::LLVMIntSLT,
        end_excl_val,
        zero,
        c"".as_ptr(),
    );
    let mut c_end = LLVMBuildSelect(b, end_neg, zero, end_excl_val, c"sl_cend".as_ptr());
    let end_over = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSGT, c_end, sl_len, c"".as_ptr());
    c_end = LLVMBuildSelect(b, end_over, sl_len, c_end, c"sl_cend2".as_ptr());

    let diff = LLVMBuildSub(b, c_end, c_start, c"sl_diff".as_ptr());
    let diff_gt0 = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSGT, diff, zero, c"".as_ptr());
    let count = LLVMBuildSelect(b, diff_gt0, diff, zero, c"sl_count".as_ptr());
    let data_size = LLVMBuildMul(
        b,
        count,
        LLVMConstInt(i64_ty, elem_size, 0),
        c"sl_dsize".as_ptr(),
    );

    let new_data = emit_malloc(b, c.module, i64_ty, ptr_ty, data_size, c"sl_data".as_ptr());

    let mut gep_idx = [c_start];
    let src_offset = LLVMBuildGEP2(
        b,
        elem_ty,
        sl_data,
        gep_idx.as_mut_ptr(),
        1,
        c"sl_src_off".as_ptr(),
    );
    emit_memcpy(b, c.module, ptr_ty, i64_ty, new_data, src_offset, data_size);

    *out_count = intern(c, to_ry_value(count));
    *out_new_data = intern(c, to_ry_value(new_data));
}
