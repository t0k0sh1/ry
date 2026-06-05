//! ARC retain / release: the `arc_retain_impl` / `arc_release_impl` cores
//! (shared with Any / CoW) and the `ry_emit_arc_retain` / `ry_emit_arc_release` ABI.

use llvm_sys::core::*;
use llvm_sys::{LLVMAtomicOrdering, LLVMAtomicRMWBinOp, LLVMIntPredicate};
use std::ffi::c_int;

use crate::ffi::*;
use crate::support::*;

// Internal form takes &mut EmitCtxImpl so cow_ensure_unique can call it while
// already holding the borrow.
pub(crate) unsafe fn arc_retain_impl(c: &mut EmitCtxImpl, header_ptr_id: RyValueId, atomic: c_int) {
    let b = c.builder;
    let context = c.context;
    let header_ptr = as_value(resolve(c, header_ptr_id));
    let i64_ty = i64_type(context);
    let arc_header_ty = arc_header_type(context);
    let strong_ptr =
        LLVMBuildStructGEP2(b, arc_header_ty, header_ptr, 0, c"arc_retain_ptr".as_ptr());
    // Skip immortal objects; the load must still be atomic in atomic mode (#630).
    let cur = emit_atomic_i64_load(
        b,
        i64_ty,
        strong_ptr,
        if atomic == RY_ARC_ATOMIC {
            LLVMAtomicOrdering::LLVMAtomicOrderingMonotonic
        } else {
            LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic
        },
        c"arc_strong".as_ptr(),
    );
    let is_immortal = LLVMBuildICmp(
        b,
        LLVMIntPredicate::LLVMIntEQ,
        cur,
        LLVMConstInt(i64_ty, ARC_IMMORTAL as u64, 0),
        c"arc_immortal".as_ptr(),
    );
    // Builder-derived parent function (builder-derived parent rule).
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));
    let retain_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"arc.retain".as_ptr());
    let done_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"arc.retain.done".as_ptr());
    LLVMBuildCondBr(b, is_immortal, done_bb, retain_bb);

    LLVMPositionBuilderAtEnd(b, retain_bb);
    if atomic == RY_ARC_ATOMIC {
        LLVMBuildAtomicRMW(
            b,
            LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpAdd,
            strong_ptr,
            LLVMConstInt(i64_ty, 1, 0),
            LLVMAtomicOrdering::LLVMAtomicOrderingSequentiallyConsistent,
            0,
        );
    } else {
        let inc = LLVMBuildAdd(b, cur, LLVMConstInt(i64_ty, 1, 0), c"arc_inc".as_ptr());
        LLVMBuildStore(b, inc, strong_ptr);
    }
    LLVMBuildBr(b, done_bb);
    LLVMPositionBuilderAtEnd(b, done_bb);
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_arc_retain(
    ctx: *mut RyEmitCtx,
    header_ptr_id: RyValueId,
    atomic: c_int,
) {
    arc_retain_impl(cx(ctx), header_ptr_id, atomic);
}

pub(crate) unsafe fn arc_release_impl(
    c: &mut EmitCtxImpl,
    header_ptr_id: RyValueId,
    atomic: c_int,
    destructor_callee: RyValueRef,
    gc_visit_fn: RyValueRef,
) {
    let b = c.builder;
    let context = c.context;
    let module = c.module;
    let header_ptr = as_value(resolve(c, header_ptr_id));
    let i64_ty = i64_type(context);
    let i8_ty = i8_type(context);
    let void_ty = void_type(context);
    let ptr_ty = ptr_type(context);
    let arc_header_ty = arc_header_type(context);

    let strong_ptr = LLVMBuildStructGEP2(b, arc_header_ty, header_ptr, 0, c"arc_rel_ptr".as_ptr());
    let cur_check = emit_atomic_i64_load(
        b,
        i64_ty,
        strong_ptr,
        if atomic == RY_ARC_ATOMIC {
            LLVMAtomicOrdering::LLVMAtomicOrderingMonotonic
        } else {
            LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic
        },
        c"arc_strong_check".as_ptr(),
    );
    let is_immortal = LLVMBuildICmp(
        b,
        LLVMIntPredicate::LLVMIntEQ,
        cur_check,
        LLVMConstInt(i64_ty, ARC_IMMORTAL as u64, 0),
        c"arc_immortal".as_ptr(),
    );
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));
    let release_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"arc.release.body".as_ptr());
    let done_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"arc.done".as_ptr());
    LLVMBuildCondBr(b, is_immortal, done_bb, release_bb);

    LLVMPositionBuilderAtEnd(b, release_bb);
    let is_zero = if atomic == RY_ARC_ATOMIC {
        // atomicrmw returns the OLD value; object is dead when old == 1.
        let old = LLVMBuildAtomicRMW(
            b,
            LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpSub,
            strong_ptr,
            LLVMConstInt(i64_ty, 1, 0),
            LLVMAtomicOrdering::LLVMAtomicOrderingSequentiallyConsistent,
            0,
        );
        LLVMBuildICmp(
            b,
            LLVMIntPredicate::LLVMIntEQ,
            old,
            LLVMConstInt(i64_ty, 1, 0),
            c"arc_dead".as_ptr(),
        )
    } else {
        let cur = LLVMBuildLoad2(b, i64_ty, strong_ptr, c"arc_strong".as_ptr());
        let dec = LLVMBuildSub(b, cur, LLVMConstInt(i64_ty, 1, 0), c"arc_dec".as_ptr());
        LLVMBuildStore(b, dec, strong_ptr);
        LLVMBuildICmp(
            b,
            LLVMIntPredicate::LLVMIntEQ,
            dec,
            LLVMConstInt(i64_ty, 0, 0),
            c"arc_dead".as_ptr(),
        )
    };

    let free_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"arc.release".as_ptr());

    if !gc_visit_fn.is_null() {
        let track_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"arc.gc_track".as_ptr());
        LLVMBuildCondBr(b, is_zero, free_bb, track_bb);
        LLVMPositionBuilderAtEnd(b, track_bb);
        let mut gc_track_params = [ptr_ty, ptr_ty, ptr_ty];
        let gc_track_ty = LLVMFunctionType(void_ty, gc_track_params.as_mut_ptr(), 3, 0);
        let gc_track_fn = get_or_insert_function(module, c"__ry_gc_track".as_ptr(), gc_track_ty);
        let dtor_ptr = if destructor_callee.is_null() {
            LLVMConstNull(ptr_ty)
        } else {
            as_value(destructor_callee)
        };
        let gc_visit_fn_val = as_value(gc_visit_fn);
        let mut gc_track_args = [header_ptr, gc_visit_fn_val, dtor_ptr];
        LLVMBuildCall2(
            b,
            gc_track_ty,
            gc_track_fn,
            gc_track_args.as_mut_ptr(),
            3,
            c"".as_ptr(),
        );
        LLVMBuildBr(b, done_bb);
    } else {
        LLVMBuildCondBr(b, is_zero, free_bb, done_bb);
    }

    LLVMPositionBuilderAtEnd(b, free_bb);
    let mut gc_untrack_params = [ptr_ty];
    let gc_untrack_ty = LLVMFunctionType(void_ty, gc_untrack_params.as_mut_ptr(), 1, 0);
    let gc_untrack_fn = get_or_insert_function(module, c"__ry_gc_untrack".as_ptr(), gc_untrack_ty);
    let mut gc_untrack_args = [header_ptr];
    LLVMBuildCall2(
        b,
        gc_untrack_ty,
        gc_untrack_fn,
        gc_untrack_args.as_mut_ptr(),
        1,
        c"".as_ptr(),
    );
    if !destructor_callee.is_null() {
        let mut gep_idx = [LLVMConstInt(i64_ty, ARC_HEADER_SIZE, 0)];
        let data_ptr = LLVMBuildGEP2(
            b,
            i8_ty,
            header_ptr,
            gep_idx.as_mut_ptr(),
            1,
            c"arc_data".as_ptr(),
        );
        let mut dtor_params = [ptr_ty];
        let dtor_ty = LLVMFunctionType(void_ty, dtor_params.as_mut_ptr(), 1, 0);
        let mut dtor_args = [data_ptr];
        LLVMBuildCall2(
            b,
            dtor_ty,
            as_value(destructor_callee),
            dtor_args.as_mut_ptr(),
            1,
            c"".as_ptr(),
        );
    }
    // weak_count read: atomic load scaled to the caller's atomic mode (Acquire
    // when atomic) — weak_count atomic-load rule (#1968).
    let weak_ptr = LLVMBuildStructGEP2(b, arc_header_ty, header_ptr, 1, c"arc_weak_ptr".as_ptr());
    let weak_count = emit_atomic_i64_load(
        b,
        i64_ty,
        weak_ptr,
        if atomic == RY_ARC_ATOMIC {
            LLVMAtomicOrdering::LLVMAtomicOrderingAcquire
        } else {
            LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic
        },
        c"arc_weak".as_ptr(),
    );
    let no_weak = LLVMBuildICmp(
        b,
        LLVMIntPredicate::LLVMIntEQ,
        weak_count,
        LLVMConstInt(i64_ty, 0, 0),
        c"arc_no_weak".as_ptr(),
    );
    let real_free_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"arc.free".as_ptr());
    let skip_free_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"arc.skip_free".as_ptr());
    LLVMBuildCondBr(b, no_weak, real_free_bb, skip_free_bb);

    LLVMPositionBuilderAtEnd(b, real_free_bb);
    emit_arc_counter_delta(b, i64_ty, ptr_ty, -1);
    emit_free(b, module, ptr_ty, void_ty, header_ptr);
    LLVMBuildBr(b, done_bb);

    LLVMPositionBuilderAtEnd(b, skip_free_bb);
    LLVMBuildBr(b, done_bb);

    LLVMPositionBuilderAtEnd(b, done_bb);
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_arc_release(
    ctx: *mut RyEmitCtx,
    header_ptr_id: RyValueId,
    atomic: c_int,
    destructor_callee: RyValueRef,
    gc_visit_fn: RyValueRef,
) {
    arc_release_impl(
        cx(ctx),
        header_ptr_id,
        atomic,
        destructor_callee,
        gc_visit_fn,
    );
}
