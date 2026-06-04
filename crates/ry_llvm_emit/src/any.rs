//! Dynamic `any` boxing: `ry_emit_any_wrap` / `_unwrap` / `_try_unwrap`.

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::{LLVMABISizeOfType, LLVMGetModuleDataLayout};
use llvm_sys::LLVMIntPredicate;
use std::ffi::CStr;

use crate::arc::*;
use crate::ffi::*;
use crate::support::*;

#[no_mangle]
pub unsafe extern "C" fn ry_emit_any_wrap(
    ctx: *mut RyEmitCtx,
    desc: *const RyAnyWrapDesc,
) -> RyValueId {
    if ctx.is_null() || desc.is_null() {
        return 0;
    }
    let c = cx(ctx);
    if c.context.is_null() || c.module.is_null() || c.builder.is_null() || (*desc).any_ty.is_null()
    {
        return 0;
    }
    if (*desc).kind > 2 {
        return 0;
    }
    let b = c.builder;
    let context = c.context;
    let module = c.module;
    let val = as_value(resolve(c, (*desc).val_id));
    let any_ty = as_type((*desc).any_ty);
    if val.is_null() {
        return 0;
    }
    let i8_ty = i8_type(context);
    let i64_ty = i64_type(context);
    let i1_ty = i1_type(context);

    // RecordBox=1 / EnumBox=2 — heap-box layout `[ ArcHeader | desc ptr | payload ]`.
    if (*desc).kind == 1 || (*desc).kind == 2 {
        let layout_ty = as_type((*desc).box_layout_ty);
        let descriptor = as_value(resolve(c, (*desc).descriptor_id));
        if layout_ty.is_null() || descriptor.is_null() {
            return 0;
        }
        let dl = LLVMGetModuleDataLayout(module);
        let expected = LLVMABISizeOfType(dl, layout_ty);
        if (*desc).box_data_size != expected {
            return 0;
        }
        let box_data_size_c = LLVMConstInt(i64_ty, (*desc).box_data_size, 0);
        let header_ptr = emit_inline_arc_alloc(c, box_data_size_c);
        let mut dp_gep = [LLVMConstInt(i64_ty, ARC_HEADER_SIZE, 0)];
        let data_ptr = LLVMBuildGEP2(
            b,
            i8_ty,
            header_ptr,
            dp_gep.as_mut_ptr(),
            1,
            c"arc_box_data".as_ptr(),
        );

        // Labels differ between RecordBox ("any.rec.*") and EnumBox ("any.enum.*").
        let (desc_slot_lbl, payload_slot_lbl, tmp_lbl, tag_lbl, data_lbl, val_lbl): (
            &CStr,
            &CStr,
            &CStr,
            &CStr,
            &CStr,
            &CStr,
        ) = if (*desc).kind == 2 {
            (
                c"any.enum.desc.slot",
                c"any.enum.payload.slot",
                c"any.enum.tmp",
                c"any.enum.tag",
                c"any.enum.data",
                c"any.enum.val",
            )
        } else {
            (
                c"any.rec.desc.slot",
                c"any.rec.fields.slot",
                c"any.rec.tmp",
                c"any.rec.tag",
                c"any.rec.data",
                c"any.rec.val",
            )
        };

        let desc_ptr_slot = LLVMBuildStructGEP2(b, layout_ty, data_ptr, 0, desc_slot_lbl.as_ptr());
        LLVMBuildStore(b, descriptor, desc_ptr_slot);
        let payload_slot =
            LLVMBuildStructGEP2(b, layout_ty, data_ptr, 1, payload_slot_lbl.as_ptr());
        LLVMBuildStore(b, val, payload_slot);

        let tmp = LLVMBuildAlloca(b, any_ty, tmp_lbl.as_ptr());
        let tag_slot = LLVMBuildStructGEP2(b, any_ty, tmp, 0, tag_lbl.as_ptr());
        LLVMBuildStore(
            b,
            LLVMConstInt(i64_ty, (*desc).target_tag as u64, 0),
            tag_slot,
        );
        let any_data_slot = LLVMBuildStructGEP2(b, any_ty, tmp, 1, data_lbl.as_ptr());
        LLVMBuildStore(b, data_ptr, any_data_slot);
        let result = LLVMBuildLoad2(b, any_ty, tmp, val_lbl.as_ptr());
        return intern(c, to_ry_value(result));
    }

    // NonBox=0 — retain BEFORE the alloca+store (the two flags are exclusive).
    if (*desc).do_collection_retain != 0 {
        let mut gep = [LLVMConstInt(i64_ty, (-(ARC_HEADER_SIZE as i64)) as u64, 0)];
        let hdr = LLVMBuildGEP2(
            b,
            i8_ty,
            val,
            gep.as_mut_ptr(),
            1,
            c"arc_hdr_from_data".as_ptr(),
        );
        let hdr_id = intern(c, to_ry_value(hdr));
        arc_retain_impl(c, hdr_id, RY_ARC_NONATOMIC);
    } else if (*desc).do_str_retain != 0 {
        let mut gep = [LLVMConstInt(
            i64_ty,
            (-(STRING_HEADER_SIZE as i64)) as u64,
            0,
        )];
        let hdr = LLVMBuildGEP2(
            b,
            i8_ty,
            val,
            gep.as_mut_ptr(),
            1,
            c"str_hdr_from_data".as_ptr(),
        );
        let hdr_id = intern(c, to_ry_value(hdr));
        arc_retain_impl(c, hdr_id, RY_ARC_NONATOMIC);
    }

    let mut val_m = val;
    if LLVMTypeOf(val_m) == i1_ty {
        val_m = LLVMBuildZExt(b, val_m, i64_ty, c"any.bool.zext".as_ptr());
    }
    let tmp = LLVMBuildAlloca(b, any_ty, c"any.tmp".as_ptr());
    let tag_ptr = LLVMBuildStructGEP2(b, any_ty, tmp, 0, c"any.tag".as_ptr());
    LLVMBuildStore(
        b,
        LLVMConstInt(i64_ty, (*desc).target_tag as u64, 0),
        tag_ptr,
    );
    let data_ptr = LLVMBuildStructGEP2(b, any_ty, tmp, 1, c"any.data".as_ptr());
    LLVMBuildStore(b, val_m, data_ptr);
    let result = LLVMBuildLoad2(b, any_ty, tmp, c"any.val".as_ptr());
    intern(c, to_ry_value(result))
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_any_unwrap(
    ctx: *mut RyEmitCtx,
    desc: *const RyAnyUnwrapDesc,
) -> RyValueId {
    if ctx.is_null() || desc.is_null() {
        return 0;
    }
    let c = cx(ctx);
    if c.context.is_null() || c.module.is_null() || c.builder.is_null() || (*desc).any_ty.is_null()
    {
        return 0;
    }
    if (*desc).kind > 2 {
        return 0;
    }
    let b = c.builder;
    let context = c.context;
    let module = c.module;
    let any_val = as_value(resolve(c, (*desc).any_val_id));
    let any_ty = as_type((*desc).any_ty);
    if any_val.is_null() {
        return 0;
    }
    let i8_ty = i8_type(context);
    let i64_ty = i64_type(context);
    let ptr_ty = ptr_type(context);
    let f64_ty = LLVMDoubleTypeInContext(context);

    let tag = LLVMBuildExtractValue(b, any_val, 0, c"any.tag.val".as_ptr());
    // Builder-derived parent (builder-derived parent rule).
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

    let mismatch_msg = cstr_bytes((*desc).mismatch_msg);
    let mismatch_name = (*desc).mismatch_global_name;

    // Record=2 — tag check + descriptor chain walk.
    if (*desc).kind == 2 {
        let layout_ty = as_type((*desc).box_layout_ty);
        let record_struct_ty = as_type((*desc).record_struct_ty);
        let expected_desc = as_value(resolve(c, (*desc).expected_desc_id));
        if layout_ty.is_null() || record_struct_ty.is_null() || expected_desc.is_null() {
            return 0;
        }
        let desc_mismatch_msg = cstr_bytes((*desc).desc_mismatch_msg);
        let desc_mismatch_name = (*desc).desc_mismatch_global_name;

        let tag_match_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.rec.tag_ok".as_ptr());
        let tag_mismatch_bb =
            LLVMAppendBasicBlockInContext(context, fn_v, c"any.rec.tag_err".as_ptr());
        let desc_check_bb =
            LLVMAppendBasicBlockInContext(context, fn_v, c"any.rec.desc_check".as_ptr());
        let desc_mismatch_bb =
            LLVMAppendBasicBlockInContext(context, fn_v, c"any.rec.desc_err".as_ptr());

        let is_record = LLVMBuildICmp(
            b,
            LLVMIntPredicate::LLVMIntEQ,
            tag,
            LLVMConstInt(i64_ty, (*desc).expected_tag as u64, 0),
            c"any.is_record".as_ptr(),
        );
        LLVMBuildCondBr(b, is_record, tag_match_bb, tag_mismatch_bb);

        LLVMPositionBuilderAtEnd(b, tag_mismatch_bb);
        emit_inline_runtime_error(c, mismatch_msg, mismatch_name);

        LLVMPositionBuilderAtEnd(b, tag_match_bb);
        let tmp = LLVMBuildAlloca(b, any_ty, c"any.rec.tmp".as_ptr());
        LLVMBuildStore(b, any_val, tmp);
        let any_data_slot = LLVMBuildStructGEP2(b, any_ty, tmp, 1, c"any.rec.data.ptr".as_ptr());
        let data_ptr = LLVMBuildLoad2(b, ptr_ty, any_data_slot, c"any.rec.data".as_ptr());
        let desc_slot =
            LLVMBuildStructGEP2(b, layout_ty, data_ptr, 0, c"any.rec.desc.slot".as_ptr());
        let actual_desc = LLVMBuildLoad2(b, ptr_ty, desc_slot, c"any.rec.desc".as_ptr());

        let mut is_subtype_p = [ptr_ty, ptr_ty];
        let is_subtype_ty = LLVMFunctionType(i64_ty, is_subtype_p.as_mut_ptr(), 2, 0);
        let is_subtype_fn = get_or_insert_function(
            module,
            c"__ry_record_is_subtype_desc".as_ptr(),
            is_subtype_ty,
        );
        let mut is_subtype_a = [actual_desc, expected_desc];
        let chain_ok = LLVMBuildCall2(
            b,
            is_subtype_ty,
            is_subtype_fn,
            is_subtype_a.as_mut_ptr(),
            2,
            c"any.rec.chain.ok".as_ptr(),
        );
        let chain_bool = LLVMBuildICmp(
            b,
            LLVMIntPredicate::LLVMIntNE,
            chain_ok,
            LLVMConstInt(i64_ty, 0, 0),
            c"any.rec.chain.bool".as_ptr(),
        );
        LLVMBuildCondBr(b, chain_bool, desc_check_bb, desc_mismatch_bb);

        LLVMPositionBuilderAtEnd(b, desc_mismatch_bb);
        emit_inline_runtime_error(c, desc_mismatch_msg, desc_mismatch_name);

        LLVMPositionBuilderAtEnd(b, desc_check_bb);
        let fields_slot =
            LLVMBuildStructGEP2(b, layout_ty, data_ptr, 1, c"any.rec.fields.slot".as_ptr());
        let record_val = LLVMBuildLoad2(
            b,
            record_struct_ty,
            fields_slot,
            c"any.rec.unwrap.val".as_ptr(),
        );
        return intern(c, to_ry_value(record_val));
    }

    // F64Promote=1 — 5 BBs; merge PHI(f64).
    if (*desc).kind == 1 {
        let float_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.float".as_ptr());
        let check_int_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.check_int".as_ptr());
        let int_promote_bb =
            LLVMAppendBasicBlockInContext(context, fn_v, c"any.int2float".as_ptr());
        let mismatch_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.mismatch".as_ptr());
        let merge_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.merge".as_ptr());

        let tmp = LLVMBuildAlloca(b, any_ty, c"any.tmp.fp".as_ptr());
        LLVMBuildStore(b, any_val, tmp);
        let data_ptr = LLVMBuildStructGEP2(b, any_ty, tmp, 1, c"any.data.fp".as_ptr());

        let is_float = LLVMBuildICmp(
            b,
            LLVMIntPredicate::LLVMIntEQ,
            tag,
            LLVMConstInt(i64_ty, RY_ANY_TAG_FLOAT as u64, 0),
            c"is.float".as_ptr(),
        );
        LLVMBuildCondBr(b, is_float, float_bb, check_int_bb);

        LLVMPositionBuilderAtEnd(b, check_int_bb);
        let is_int = LLVMBuildICmp(
            b,
            LLVMIntPredicate::LLVMIntEQ,
            tag,
            LLVMConstInt(i64_ty, RY_ANY_TAG_INT as u64, 0),
            c"is.int".as_ptr(),
        );
        LLVMBuildCondBr(b, is_int, int_promote_bb, mismatch_bb);

        LLVMPositionBuilderAtEnd(b, mismatch_bb);
        emit_inline_runtime_error(c, mismatch_msg, mismatch_name);

        LLVMPositionBuilderAtEnd(b, float_bb);
        let float_val = LLVMBuildLoad2(b, f64_ty, data_ptr, c"any.f64".as_ptr());
        LLVMBuildBr(b, merge_bb);

        LLVMPositionBuilderAtEnd(b, int_promote_bb);
        let int_val = LLVMBuildLoad2(b, i64_ty, data_ptr, c"any.i64".as_ptr());
        let promoted = LLVMBuildSIToFP(b, int_val, f64_ty, c"any.i2f".as_ptr());
        LLVMBuildBr(b, merge_bb);

        LLVMPositionBuilderAtEnd(b, merge_bb);
        let phi = LLVMBuildPhi(b, f64_ty, c"any.unwrap.f64".as_ptr());
        let mut vals = [float_val, promoted];
        let mut blocks = [float_bb, int_promote_bb];
        LLVMAddIncoming(phi, vals.as_mut_ptr(), blocks.as_mut_ptr(), 2);
        return intern(c, to_ry_value(phi));
    }

    // Standard=0 — 2-way path.
    let target_ty = as_type((*desc).target_ty);
    if target_ty.is_null() {
        return 0;
    }
    let cmp = LLVMBuildICmp(
        b,
        LLVMIntPredicate::LLVMIntEQ,
        tag,
        LLVMConstInt(i64_ty, (*desc).expected_tag as u64, 0),
        c"any.tag.check".as_ptr(),
    );
    let match_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.match".as_ptr());
    let mismatch_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.mismatch".as_ptr());
    LLVMBuildCondBr(b, cmp, match_bb, mismatch_bb);

    LLVMPositionBuilderAtEnd(b, mismatch_bb);
    emit_inline_runtime_error(c, mismatch_msg, mismatch_name);

    LLVMPositionBuilderAtEnd(b, match_bb);
    let tmp = LLVMBuildAlloca(b, any_ty, c"any.tmp".as_ptr());
    LLVMBuildStore(b, any_val, tmp);
    let data_ptr = LLVMBuildStructGEP2(b, any_ty, tmp, 1, c"any.data.ptr".as_ptr());
    let unwrapped = LLVMBuildLoad2(b, target_ty, data_ptr, c"any.unwrap.val".as_ptr());

    if (*desc).do_collection_retain != 0 {
        let mut gep_i = [LLVMConstInt(i64_ty, (-(ARC_HEADER_SIZE as i64)) as u64, 0)];
        let hdr = LLVMBuildGEP2(
            b,
            i8_ty,
            unwrapped,
            gep_i.as_mut_ptr(),
            1,
            c"arc_hdr_from_data".as_ptr(),
        );
        let hdr_id = intern(c, to_ry_value(hdr));
        arc_retain_impl(c, hdr_id, RY_ARC_NONATOMIC);
    } else if (*desc).do_str_retain != 0 {
        let mut gep_i = [LLVMConstInt(
            i64_ty,
            (-(STRING_HEADER_SIZE as i64)) as u64,
            0,
        )];
        let hdr = LLVMBuildGEP2(
            b,
            i8_ty,
            unwrapped,
            gep_i.as_mut_ptr(),
            1,
            c"str_hdr_from_data".as_ptr(),
        );
        let hdr_id = intern(c, to_ry_value(hdr));
        arc_retain_impl(c, hdr_id, RY_ARC_NONATOMIC);
    }
    intern(c, to_ry_value(unwrapped))
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_any_try_unwrap(
    ctx: *mut RyEmitCtx,
    desc: *const RyAnyTryUnwrapDesc,
) -> RyValueId {
    if ctx.is_null() || desc.is_null() {
        return 0;
    }
    let c = cx(ctx);
    if c.context.is_null()
        || c.module.is_null()
        || c.builder.is_null()
        || (*desc).any_ty.is_null()
        || (*desc).res_ty.is_null()
        || (*desc).error_ty.is_null()
    {
        return 0;
    }
    if (*desc).kind > 1 {
        return 0;
    }
    let b = c.builder;
    let context = c.context;
    let any_val = as_value(resolve(c, (*desc).any_val_id));
    let any_ty = as_type((*desc).any_ty);
    let res_ty = as_type((*desc).res_ty);
    let error_ty = as_type((*desc).error_ty);
    let err_msg_str = as_value(resolve(c, (*desc).err_msg_str_id));
    if any_val.is_null() || err_msg_str.is_null() {
        return 0;
    }
    let i8_ty = i8_type(context);
    let i64_ty = i64_type(context);
    let i1_ty = i1_type(context);
    let f64_ty = LLVMDoubleTypeInContext(context);

    let tag = LLVMBuildExtractValue(b, any_val, 0, c"tryany.tag".as_ptr());
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

    // Inline the ok / err value builders: retaining the arc source would be a
    // no-op for freshly-extracted loads, so the explicit retain via
    // do_collection_retain / do_str_retain carries it.
    let build_ok = move |inner: LLVMValueRef| -> LLVMValueRef {
        unsafe {
            let mut v = LLVMConstNull(res_ty);
            v = LLVMBuildInsertValue(b, v, LLVMConstInt(i1_ty, 1, 0), 0, c"res.ok".as_ptr());
            v = LLVMBuildInsertValue(b, v, inner, 1, c"res.ok_val".as_ptr());
            v = LLVMBuildInsertValue(
                b,
                v,
                LLVMConstNull(LLVMStructGetTypeAtIndex(res_ty, 2)),
                2,
                c"".as_ptr(),
            );
            v
        }
    };
    let build_err = move || -> LLVMValueRef {
        unsafe {
            let mut err_val = LLVMGetUndef(error_ty);
            err_val = LLVMBuildInsertValue(b, err_val, err_msg_str, 0, c"".as_ptr());
            err_val = LLVMBuildInsertValue(b, err_val, LLVMConstInt(i64_ty, 0, 0), 1, c"".as_ptr());
            let mut v = LLVMConstNull(res_ty);
            v = LLVMBuildInsertValue(b, v, LLVMConstInt(i1_ty, 0, 0), 0, c"res.err".as_ptr());
            v = LLVMBuildInsertValue(
                b,
                v,
                LLVMConstNull(LLVMStructGetTypeAtIndex(res_ty, 1)),
                1,
                c"".as_ptr(),
            );
            v = LLVMBuildInsertValue(b, v, err_val, 2, c"res.err_val".as_ptr());
            v
        }
    };

    // F64Promote=1 — both loads share one alloca; Ok arm selects via isFloat.
    if (*desc).kind == 1 {
        let tmp = LLVMBuildAlloca(b, any_ty, c"tryany.fp.tmp".as_ptr());
        LLVMBuildStore(b, any_val, tmp);
        let data_ptr = LLVMBuildStructGEP2(b, any_ty, tmp, 1, c"tryany.fp.data".as_ptr());
        let f_val = LLVMBuildLoad2(b, f64_ty, data_ptr, c"tryany.fp.fval".as_ptr());
        let i_val = LLVMBuildLoad2(b, i64_ty, data_ptr, c"tryany.fp.ival".as_ptr());
        let promoted = LLVMBuildSIToFP(b, i_val, f64_ty, c"tryany.fp.i2f".as_ptr());
        let is_float = LLVMBuildICmp(
            b,
            LLVMIntPredicate::LLVMIntEQ,
            tag,
            LLVMConstInt(i64_ty, RY_ANY_TAG_FLOAT as u64, 0),
            c"tryany.fp.is_float".as_ptr(),
        );
        let is_int = LLVMBuildICmp(
            b,
            LLVMIntPredicate::LLVMIntEQ,
            tag,
            LLVMConstInt(i64_ty, RY_ANY_TAG_INT as u64, 0),
            c"tryany.fp.is_int".as_ptr(),
        );
        let is_accept = LLVMBuildOr(b, is_float, is_int, c"tryany.fp.is_accept".as_ptr());
        let is_err = LLVMBuildNot(b, is_accept, c"tryany.fp.is_err".as_ptr());

        let ok_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.ok".as_ptr());
        let err_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.err".as_ptr());
        let merge_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.merge".as_ptr());
        LLVMBuildCondBr(b, is_err, err_bb, ok_bb);

        LLVMPositionBuilderAtEnd(b, ok_bb);
        let chosen = LLVMBuildSelect(b, is_float, f_val, promoted, c"tryany.fp.val".as_ptr());
        let ok_val = build_ok(chosen);
        LLVMBuildBr(b, merge_bb);
        let ok_incoming = LLVMGetInsertBlock(b);

        LLVMPositionBuilderAtEnd(b, err_bb);
        let err_val = build_err();
        LLVMBuildBr(b, merge_bb);
        let err_incoming = LLVMGetInsertBlock(b);

        LLVMPositionBuilderAtEnd(b, merge_bb);
        let phi = LLVMBuildPhi(b, res_ty, c"result".as_ptr());
        let mut vals = [ok_val, err_val];
        let mut blocks = [ok_incoming, err_incoming];
        LLVMAddIncoming(phi, vals.as_mut_ptr(), blocks.as_mut_ptr(), 2);
        return intern(c, to_ry_value(phi));
    }

    // Standard=0 — tag-check primitive arm.
    let target_ty = as_type((*desc).target_ty);
    if target_ty.is_null() {
        return 0;
    }
    let cmp = LLVMBuildICmp(
        b,
        LLVMIntPredicate::LLVMIntEQ,
        tag,
        LLVMConstInt(i64_ty, (*desc).expected_tag as u64, 0),
        c"tryany.tag.eq".as_ptr(),
    );
    let is_err = LLVMBuildNot(b, cmp, c"tryany.is_err".as_ptr());

    let ok_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.ok".as_ptr());
    let err_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.err".as_ptr());
    let merge_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.merge".as_ptr());
    LLVMBuildCondBr(b, is_err, err_bb, ok_bb);

    LLVMPositionBuilderAtEnd(b, ok_bb);
    let tmp = LLVMBuildAlloca(b, any_ty, c"tryany.tmp".as_ptr());
    LLVMBuildStore(b, any_val, tmp);
    let data_ptr = LLVMBuildStructGEP2(b, any_ty, tmp, 1, c"tryany.data".as_ptr());
    let unwrapped = LLVMBuildLoad2(b, target_ty, data_ptr, c"tryany.val".as_ptr());
    if (*desc).do_collection_retain != 0 {
        let mut gep_i = [LLVMConstInt(i64_ty, (-(ARC_HEADER_SIZE as i64)) as u64, 0)];
        let hdr = LLVMBuildGEP2(
            b,
            i8_ty,
            unwrapped,
            gep_i.as_mut_ptr(),
            1,
            c"arc_hdr_from_data".as_ptr(),
        );
        let hdr_id = intern(c, to_ry_value(hdr));
        arc_retain_impl(c, hdr_id, RY_ARC_NONATOMIC);
    } else if (*desc).do_str_retain != 0 {
        let mut gep_i = [LLVMConstInt(
            i64_ty,
            (-(STRING_HEADER_SIZE as i64)) as u64,
            0,
        )];
        let hdr = LLVMBuildGEP2(
            b,
            i8_ty,
            unwrapped,
            gep_i.as_mut_ptr(),
            1,
            c"str_hdr_from_data".as_ptr(),
        );
        let hdr_id = intern(c, to_ry_value(hdr));
        arc_retain_impl(c, hdr_id, RY_ARC_NONATOMIC);
    }
    let ok_val = build_ok(unwrapped);
    LLVMBuildBr(b, merge_bb);
    let ok_incoming = LLVMGetInsertBlock(b);

    LLVMPositionBuilderAtEnd(b, err_bb);
    let err_val = build_err();
    LLVMBuildBr(b, merge_bb);
    let err_incoming = LLVMGetInsertBlock(b);

    LLVMPositionBuilderAtEnd(b, merge_bb);
    let phi = LLVMBuildPhi(b, res_ty, c"result".as_ptr());
    let mut vals = [ok_val, err_val];
    let mut blocks = [ok_incoming, err_incoming];
    LLVMAddIncoming(phi, vals.as_mut_ptr(), blocks.as_mut_ptr(), 2);
    intern(c, to_ry_value(phi))
}
