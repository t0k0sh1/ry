//! Copy-on-Write: `ry_emit_cow_ensure_unique` plus the phi-based retain loop
//! that retains each ARC element/key of a freshly cloned buffer.

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::{LLVMABISizeOfType, LLVMGetModuleDataLayout};
use llvm_sys::{LLVMAtomicOrdering, LLVMIntPredicate};
use std::ffi::{c_char, c_int};

use crate::arc::*;
use crate::ffi::*;
use crate::support::*;

// A phi-based for loop (used by ry_emit_cow_ensure_unique) that retains each
// ARC element/key of the cloned buffer.
// Takes the opaque ctx (not &mut EmitCtxImpl) so it can call the public
// ry_emit_arc_retain without holding a borrow across the call.
//
// The cached LLVM types (i64/i8/ptr) and the per-call (field_idx, is_str, tag)
// are passed individually, mirroring how the emission code threads these types
// throughout. Bundling them into a struct only here would be asymmetric and would
// touch a hot codegen path, so the lint is suppressed rather than refactored
// (see .claude/rules/build-warning-flags.md "intentional patterns" policy).
#[allow(clippy::too_many_arguments)]
unsafe fn emit_cow_retain_loop(
    ctx: *mut RyEmitCtx,
    header_ty: LLVMTypeRef,
    new_data_ptr: LLVMValueRef,
    i64_ty: LLVMTypeRef,
    i8_ty: LLVMTypeRef,
    ptr_ty: LLVMTypeRef,
    field_idx: u32,
    is_str: c_int,
    tag: &[u8],
) {
    let b = cx(ctx).builder;
    let context = cx(ctx).context;
    let len_field_ptr = LLVMBuildStructGEP2(
        b,
        header_ty,
        new_data_ptr,
        0,
        cname3(b"cow_", tag, b"_len_ptr").as_ptr(),
    );
    let count = LLVMBuildLoad2(
        b,
        i64_ty,
        len_field_ptr,
        cname3(b"cow_", tag, b"_len").as_ptr(),
    );
    let buf_field_ptr = LLVMBuildStructGEP2(
        b,
        header_ty,
        new_data_ptr,
        field_idx,
        cname3(b"cow_", tag, b"_buf_field").as_ptr(),
    );
    let buf = LLVMBuildLoad2(
        b,
        ptr_ty,
        buf_field_ptr,
        cname3(b"cow_", tag, b"_buf").as_ptr(),
    );

    let loop_fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));
    let loop_bb =
        LLVMAppendBasicBlockInContext(context, loop_fn, cname3(b"cow.", tag, b"_loop").as_ptr());
    let body_bb =
        LLVMAppendBasicBlockInContext(context, loop_fn, cname3(b"cow.", tag, b"_body").as_ptr());
    let loop_done_bb =
        LLVMAppendBasicBlockInContext(context, loop_fn, cname3(b"cow.", tag, b"_done").as_ptr());

    let pre_loop_bb = LLVMGetInsertBlock(b);
    LLVMBuildBr(b, loop_bb);
    LLVMPositionBuilderAtEnd(b, loop_bb);
    let idx = LLVMBuildPhi(b, i64_ty, cname3(b"cow_", tag, b"_idx").as_ptr());
    let mut init_vals = [LLVMConstInt(i64_ty, 0, 0)];
    let mut init_blocks = [pre_loop_bb];
    LLVMAddIncoming(idx, init_vals.as_mut_ptr(), init_blocks.as_mut_ptr(), 1);
    let cond = LLVMBuildICmp(
        b,
        LLVMIntPredicate::LLVMIntSLT,
        idx,
        count,
        cname3(b"cow_", tag, b"_cond").as_ptr(),
    );
    LLVMBuildCondBr(b, cond, body_bb, loop_done_bb);

    LLVMPositionBuilderAtEnd(b, body_bb);
    let mut elem_gep = [idx];
    let elem_ptr = LLVMBuildGEP2(
        b,
        ptr_ty,
        buf,
        elem_gep.as_mut_ptr(),
        1,
        cname3(b"cow_", tag, b"_ptr").as_ptr(),
    );
    let elem = LLVMBuildLoad2(b, ptr_ty, elem_ptr, cname3(b"cow_", tag, b"_val").as_ptr());
    let hdr_offset = if is_str != 0 {
        -(STRING_HEADER_SIZE as i64)
    } else {
        -(ARC_HEADER_SIZE as i64)
    };
    let mut hdr_gep = [LLVMConstInt(i64_ty, hdr_offset as u64, 0)];
    let elem_hdr = LLVMBuildGEP2(
        b,
        i8_ty,
        elem,
        hdr_gep.as_mut_ptr(),
        1,
        cname3(b"cow_", tag, b"_hdr").as_ptr(),
    );
    let elem_hdr_id = intern(cx(ctx), to_ry_value(elem_hdr));
    ry_emit_arc_retain(ctx, elem_hdr_id, RY_ARC_NONATOMIC);
    let next = LLVMBuildAdd(
        b,
        idx,
        LLVMConstInt(i64_ty, 1, 0),
        cname3(b"cow_", tag, b"_next").as_ptr(),
    );
    // Back-edge incoming uses the builder's current block (advanced past the
    // retain helper's BBs), not body_bb.
    let mut next_vals = [next];
    let mut next_blocks = [LLVMGetInsertBlock(b)];
    LLVMAddIncoming(idx, next_vals.as_mut_ptr(), next_blocks.as_mut_ptr(), 1);
    LLVMBuildBr(b, loop_bb);

    LLVMPositionBuilderAtEnd(b, loop_done_bb);
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_cow_ensure_unique(
    ctx: *mut RyEmitCtx,
    desc: *const RyCowEnsureUniqueDesc,
) -> RyValueId {
    if ctx.is_null() || desc.is_null() {
        return 0;
    }
    if cx(ctx).context.is_null() || cx(ctx).module.is_null() || cx(ctx).builder.is_null() {
        return 0;
    }
    let b = cx(ctx).builder;
    let context = cx(ctx).context;
    let module = cx(ctx).module;
    let data_ptr = as_value(resolve(cx(ctx), (*desc).data_ptr_id));
    let slot_ptr = as_value(resolve(cx(ctx), (*desc).slot_ptr_id));
    if data_ptr.is_null() || slot_ptr.is_null() {
        return 0;
    }
    let i64_ty = i64_type(context);
    let i8_ty = i8_type(context);
    let ptr_ty = ptr_type(context);
    let arc_header_ty = arc_header_type(context);

    // Anonymous mirrors of CodeGen's listHeaderTy_ / mapHeaderTy_ / setHeaderTy_
    // (src/codegen.cpp). Field order MUST stay in sync.
    let (header_ty, data_field_idx, key_field_idx): (LLVMTypeRef, u32, u32) = match (*desc).kind {
        RY_COW_LIST => {
            let mut e = [i64_ty, i64_ty, ptr_ty];
            (LLVMStructTypeInContext(context, e.as_mut_ptr(), 3, 0), 2, 0)
        }
        RY_COW_MAP => {
            let mut e = [i64_ty, i64_ty, ptr_ty, ptr_ty, i64_ty, ptr_ty];
            (LLVMStructTypeInContext(context, e.as_mut_ptr(), 6, 0), 3, 2)
        }
        RY_COW_SET => {
            let mut e = [i64_ty, i64_ty, ptr_ty, i64_ty, ptr_ty];
            (LLVMStructTypeInContext(context, e.as_mut_ptr(), 5, 0), 2, 0)
        }
        _ => return 0,
    };

    let dl = LLVMGetModuleDataLayout(module);
    let header_size = LLVMABISizeOfType(dl, header_ty);

    let mut hdr_gep = [LLVMConstInt(i64_ty, (-(ARC_HEADER_SIZE as i64)) as u64, 0)];
    let header_ptr = LLVMBuildGEP2(
        b,
        i8_ty,
        data_ptr,
        hdr_gep.as_mut_ptr(),
        1,
        c"cow_hdr".as_ptr(),
    );

    let atomic_mode = (*desc).atomic;
    let strong_ptr =
        LLVMBuildStructGEP2(b, arc_header_ty, header_ptr, 0, c"cow_strong_ptr".as_ptr());
    let strong = emit_atomic_i64_load(
        b,
        i64_ty,
        strong_ptr,
        if atomic_mode == RY_ARC_ATOMIC {
            LLVMAtomicOrdering::LLVMAtomicOrderingAcquire
        } else {
            LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic
        },
        c"cow_strong".as_ptr(),
    );
    let is_unique = LLVMBuildICmp(
        b,
        LLVMIntPredicate::LLVMIntEQ,
        strong,
        LLVMConstInt(i64_ty, 1, 0),
        c"cow_unique".as_ptr(),
    );
    let is_immortal = LLVMBuildICmp(
        b,
        LLVMIntPredicate::LLVMIntEQ,
        strong,
        LLVMConstInt(i64_ty, ARC_IMMORTAL as u64, 0),
        c"cow_immortal".as_ptr(),
    );
    let skip_cow = LLVMBuildOr(b, is_unique, is_immortal, c"cow_skip".as_ptr());

    // Builder-derived parent (builder-derived parent rule).
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));
    let copy_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"cow.copy".as_ptr());
    let cont_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"cow.cont".as_ptr());
    let orig_bb = LLVMGetInsertBlock(b);
    LLVMBuildCondBr(b, skip_cow, cont_bb, copy_bb);

    LLVMPositionBuilderAtEnd(b, copy_bb);

    let alloc_buf = move |byte_size: LLVMValueRef, name: *const c_char| -> LLVMValueRef {
        unsafe {
            let mut p = [i64_ty];
            let ty = LLVMFunctionType(ptr_ty, p.as_mut_ptr(), 1, 0);
            let f = get_or_insert_function(module, c"malloc".as_ptr(), ty);
            let mut a = [byte_size];
            LLVMBuildCall2(b, ty, f, a.as_mut_ptr(), 1, name)
        }
    };
    let memcpy_to = move |dst: LLVMValueRef, src: LLVMValueRef, byte_size: LLVMValueRef| unsafe {
        let mut p = [ptr_ty, ptr_ty, i64_ty];
        let ty = LLVMFunctionType(ptr_ty, p.as_mut_ptr(), 3, 0);
        let f = get_or_insert_function(module, c"memcpy".as_ptr(), ty);
        let mut a = [dst, src, byte_size];
        LLVMBuildCall2(b, ty, f, a.as_mut_ptr(), 3, c"".as_ptr());
    };

    // Allocate the ARC-backed collection header (mirror emitArcAlloc inline).
    let box_size = LLVMConstInt(i64_ty, ARC_HEADER_SIZE + header_size, 0);
    let cow_box = alloc_buf(box_size, c"cow_box".as_ptr());
    emit_arc_counter_delta(b, i64_ty, ptr_ty, 1);
    let new_strong_ptr =
        LLVMBuildStructGEP2(b, arc_header_ty, cow_box, 0, c"cow_new_strong_ptr".as_ptr());
    LLVMBuildStore(b, LLVMConstInt(i64_ty, 1, 0), new_strong_ptr);
    let new_weak_ptr =
        LLVMBuildStructGEP2(b, arc_header_ty, cow_box, 1, c"cow_new_weak_ptr".as_ptr());
    LLVMBuildStore(b, LLVMConstInt(i64_ty, 0, 0), new_weak_ptr);
    let mut nd_gep = [LLVMConstInt(i64_ty, ARC_HEADER_SIZE, 0)];
    let new_data_ptr = LLVMBuildGEP2(
        b,
        i8_ty,
        cow_box,
        nd_gep.as_mut_ptr(),
        1,
        c"cow_new_data".as_ptr(),
    );

    let old_len_ptr = LLVMBuildStructGEP2(b, header_ty, data_ptr, 0, c"cow_old_len_ptr".as_ptr());
    let old_len = LLVMBuildLoad2(b, i64_ty, old_len_ptr, c"cow_old_len".as_ptr());

    match (*desc).kind {
        RY_COW_LIST => {
            let old_data_field =
                LLVMBuildStructGEP2(b, header_ty, data_ptr, 2, c"cow_old_data_field".as_ptr());
            let old_data = LLVMBuildLoad2(b, ptr_ty, old_data_field, c"cow_old_data".as_ptr());
            let buf_size = LLVMBuildMul(
                b,
                old_len,
                LLVMConstInt(i64_ty, (*desc).elem_size, 0),
                c"cow_buf_size".as_ptr(),
            );
            let new_buf = alloc_buf(buf_size, c"cow_new_buf".as_ptr());
            memcpy_to(new_buf, old_data, buf_size);
            let new_len_ptr =
                LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 0, c"cow_new_len_ptr".as_ptr());
            LLVMBuildStore(b, old_len, new_len_ptr);
            let new_cap_ptr =
                LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 1, c"cow_new_cap_ptr".as_ptr());
            LLVMBuildStore(b, old_len, new_cap_ptr);
            let new_data_field =
                LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 2, c"cow_new_data_ptr".as_ptr());
            LLVMBuildStore(b, new_buf, new_data_field);
        }
        RY_COW_MAP => {
            let old_keys_field =
                LLVMBuildStructGEP2(b, header_ty, data_ptr, 2, c"cow_old_keys_field".as_ptr());
            let old_keys = LLVMBuildLoad2(b, ptr_ty, old_keys_field, c"cow_old_keys".as_ptr());
            let old_vals_field =
                LLVMBuildStructGEP2(b, header_ty, data_ptr, 3, c"cow_old_vals_field".as_ptr());
            let old_vals = LLVMBuildLoad2(b, ptr_ty, old_vals_field, c"cow_old_vals".as_ptr());
            let old_bc_ptr =
                LLVMBuildStructGEP2(b, header_ty, data_ptr, 4, c"cow_old_bc_ptr".as_ptr());
            let old_bc = LLVMBuildLoad2(b, i64_ty, old_bc_ptr, c"cow_old_bc".as_ptr());
            let old_bk_field =
                LLVMBuildStructGEP2(b, header_ty, data_ptr, 5, c"cow_old_bk_ptr".as_ptr());
            let old_bk = LLVMBuildLoad2(b, ptr_ty, old_bk_field, c"cow_old_bk".as_ptr());

            let keys_size = LLVMBuildMul(
                b,
                old_len,
                LLVMConstInt(i64_ty, (*desc).key_size, 0),
                c"cow_keys_size".as_ptr(),
            );
            let new_keys = alloc_buf(keys_size, c"cow_new_keys".as_ptr());
            memcpy_to(new_keys, old_keys, keys_size);
            let vals_size = LLVMBuildMul(
                b,
                old_len,
                LLVMConstInt(i64_ty, (*desc).val_size, 0),
                c"cow_vals_size".as_ptr(),
            );
            let new_vals = alloc_buf(vals_size, c"cow_new_vals".as_ptr());
            memcpy_to(new_vals, old_vals, vals_size);
            let bk_size = LLVMBuildMul(
                b,
                old_bc,
                LLVMConstInt(i64_ty, 8, 0),
                c"cow_bk_size".as_ptr(),
            );
            let new_bk = alloc_buf(bk_size, c"cow_new_bk".as_ptr());
            memcpy_to(new_bk, old_bk, bk_size);

            let new_len_ptr =
                LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 0, c"cow_m_len_ptr".as_ptr());
            LLVMBuildStore(b, old_len, new_len_ptr);
            let new_cap_ptr =
                LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 1, c"cow_m_cap_ptr".as_ptr());
            LLVMBuildStore(b, old_len, new_cap_ptr);
            let new_keys_field =
                LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 2, c"cow_m_keys_ptr".as_ptr());
            LLVMBuildStore(b, new_keys, new_keys_field);
            let new_vals_field =
                LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 3, c"cow_m_vals_ptr".as_ptr());
            LLVMBuildStore(b, new_vals, new_vals_field);
            let new_bc_ptr =
                LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 4, c"cow_m_bc_ptr".as_ptr());
            LLVMBuildStore(b, old_bc, new_bc_ptr);
            let new_bk_field =
                LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 5, c"cow_m_bk_ptr".as_ptr());
            LLVMBuildStore(b, new_bk, new_bk_field);
        }
        RY_COW_SET => {
            let old_elems_field =
                LLVMBuildStructGEP2(b, header_ty, data_ptr, 2, c"cow_old_elems_field".as_ptr());
            let old_elems = LLVMBuildLoad2(b, ptr_ty, old_elems_field, c"cow_old_elems".as_ptr());
            let old_bc_ptr =
                LLVMBuildStructGEP2(b, header_ty, data_ptr, 3, c"cow_old_bc_ptr".as_ptr());
            let old_bc = LLVMBuildLoad2(b, i64_ty, old_bc_ptr, c"cow_old_bc".as_ptr());
            let old_bk_field =
                LLVMBuildStructGEP2(b, header_ty, data_ptr, 4, c"cow_old_bk_ptr".as_ptr());
            let old_bk = LLVMBuildLoad2(b, ptr_ty, old_bk_field, c"cow_old_bk".as_ptr());

            let elems_size = LLVMBuildMul(
                b,
                old_len,
                LLVMConstInt(i64_ty, (*desc).elem_size, 0),
                c"cow_elems_size".as_ptr(),
            );
            let new_elems = alloc_buf(elems_size, c"cow_new_elems".as_ptr());
            memcpy_to(new_elems, old_elems, elems_size);
            let bk_size = LLVMBuildMul(
                b,
                old_bc,
                LLVMConstInt(i64_ty, 8, 0),
                c"cow_bk_size".as_ptr(),
            );
            let new_bk = alloc_buf(bk_size, c"cow_new_bk".as_ptr());
            memcpy_to(new_bk, old_bk, bk_size);

            let new_len_ptr =
                LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 0, c"cow_s_len_ptr".as_ptr());
            LLVMBuildStore(b, old_len, new_len_ptr);
            let new_cap_ptr =
                LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 1, c"cow_s_cap_ptr".as_ptr());
            LLVMBuildStore(b, old_len, new_cap_ptr);
            let new_elems_field =
                LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 2, c"cow_s_elems_ptr".as_ptr());
            LLVMBuildStore(b, new_elems, new_elems_field);
            let new_bc_ptr =
                LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 3, c"cow_s_bc_ptr".as_ptr());
            LLVMBuildStore(b, old_bc, new_bc_ptr);
            let new_bk_field =
                LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 4, c"cow_s_bk_ptr".as_ptr());
            LLVMBuildStore(b, new_bk, new_bk_field);
        }
        _ => return 0,
    }

    // Element / key retain loops (always RY_ARC_NONATOMIC — the clone is private).
    if (*desc).do_elem_retain != 0 {
        emit_cow_retain_loop(
            ctx,
            header_ty,
            new_data_ptr,
            i64_ty,
            i8_ty,
            ptr_ty,
            data_field_idx,
            (*desc).elem_is_str,
            b"elem",
        );
    }
    if (*desc).do_key_retain != 0 {
        emit_cow_retain_loop(
            ctx,
            header_ty,
            new_data_ptr,
            i64_ty,
            i8_ty,
            ptr_ty,
            key_field_idx,
            (*desc).key_is_str,
            b"key",
        );
    }

    // Release the old header (the helper leaves the builder on arc.done).
    let header_id = intern(cx(ctx), to_ry_value(header_ptr));
    ry_emit_arc_release(
        ctx,
        header_id,
        atomic_mode,
        (*desc).destructor_callee,
        std::ptr::null_mut(),
    );

    LLVMBuildStore(b, new_data_ptr, slot_ptr);

    let copy_end_bb = LLVMGetInsertBlock(b);
    LLVMBuildBr(b, cont_bb);

    LLVMPositionBuilderAtEnd(b, cont_bb);
    let phi = LLVMBuildPhi(b, ptr_ty, c"cow_ptr".as_ptr());
    let mut vals = [data_ptr, new_data_ptr];
    let mut blocks = [orig_bb, copy_end_bb];
    LLVMAddIncoming(phi, vals.as_mut_ptr(), blocks.as_mut_ptr(), 2);
    intern(cx(ctx), to_ry_value(phi))
}
