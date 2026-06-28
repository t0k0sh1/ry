//! composite/arc — ARC retain / release IR generation (#2057 pilot), plus the
//! shared ARC-side helpers carried over from the former `core.rs`:
//! the `__ry_arc_counter_address` extern, the StringHeader-prefixed
//! `get_or_create_arc_msg_global` (#2097 — the `cachedGlobalString` shape
//! distinct from `primitive::global::get_or_create_msg_global`), the
//! `emit_arc_counter_delta` live-count bump, the mode-scaled
//! `emit_atomic_i64_load`, and the `emit_inline_arc_alloc` helper used by
//! `composite::any` / `composite::collection` to create a fresh ARC-headed box.
//!
//! `impl EmitCtx` methods take the Rust-native `ValueRef` / `Atomicity` and are
//! the convergence point shared by the C++ path (via the `abi` `ry_emit_arc_*`
//! externs) and the Rust-direct path (`composite::any`'s retain-guard, which
//! passes the value in without the id bridge).

use std::ffi::c_char;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{
    LLVMAtomicOrdering, LLVMAtomicRMWBinOp, LLVMIntPredicate, LLVMLinkage, LLVMUnnamedAddr,
};

use crate::composite::header::arc_header_type;
use crate::context::{Atomicity, EmitCtx, ValueRef, ARC_HEADER_SIZE, ARC_IMMORTAL};
use crate::primitive::libc::{emit_free, emit_malloc};
use crate::primitive::module::get_or_insert_function;
use crate::primitive::types::{i32_type, i64_type, i8_type, ptr_type, void_type};
use crate::primitive::util::{cname_pfx, cstr_bytes};

// ARC live-count counter address (mirrors the extern in src/codegen_arc.cpp).
extern "C" {
    fn __ry_arc_counter_address() -> *mut i64;
}

// `cachedGlobalString`-shaped global (`src/codegen.cpp::buildArcGlobal`): a
// StringHeader-prefixed `{ i64 ARC_IMMORTAL, i64 0, i64 byte_len, [N+1 x i8]
// data }` struct whose data slot holds the message bytes (NUL-terminated). The
// returned ConstantExpr is the in-bounds GEP `wrapTy, gv, [0, 3, 0]` — a `ptr`
// to the first byte of the payload, the shape the C++ side passes straight to
// `fprintf`. Used by `emitRuntimeError`-style exits whose fmt strings cross
// runtime symbols and must match the str-handle ABI (#2097 — checked FP→int).
// Distinct from `primitive::global::get_or_create_msg_global`, which builds a
// plain `[N+1 x i8]` global used by `ry_emit_bounds_error`.
//
// No dedup at this layer — callers that need byte-keyed interning use the
// `get_or_create_arc_msg_global` wrapper (runtime error msg path, #2097). The
// Stage 2 (#2484) `ry_emit_build_arc_global` boundary calls this raw function
// directly so the lower crate can own the string/regex separation invariant
// with its own per-cache dedup. `name_bytes` is the caller-supplied global-
// name hint as a byte slice (no NUL terminator required); this helper
// appends `.arc` to it (matching `buildArcGlobal`'s `name + ".arc"`).
pub(crate) unsafe fn build_arc_global_raw(
    c: &mut EmitCtx,
    msg: &[u8],
    name_bytes: &[u8],
) -> LLVMValueRef {
    let context = c.context;
    let i64_ty = i64_type(context);
    let i32_ty = i32_type(context);
    // ConstantDataArray::getString equivalent — appends a trailing NUL.
    let str_data = LLVMConstStringInContext2(context, msg.as_ptr() as *const c_char, msg.len(), 0);
    let str_data_ty = LLVMTypeOf(str_data);
    let mut field_tys = [i64_ty, i64_ty, i64_ty, str_data_ty];
    let wrap_ty = LLVMStructTypeInContext(context, field_tys.as_mut_ptr(), 4, 0);
    // ARC_IMMORTAL = i64::MAX — retain / release are no-ops on this sentinel.
    let arc_immortal = LLVMConstInt(i64_ty, i64::MAX as u64, 0);
    let zero_i64 = LLVMConstInt(i64_ty, 0, 0);
    let byte_len = LLVMConstInt(i64_ty, msg.len() as u64, 0);
    let mut elements = [arc_immortal, zero_i64, byte_len, str_data];
    let init_val = LLVMConstNamedStruct(wrap_ty, elements.as_mut_ptr(), 4);
    // Append ".arc" to the global name (matches buildArcGlobal's `name +
    // ".arc"`). `cname_pfx` is byte-slice-based — works with non-NUL-
    // terminated `name_bytes`.
    let arc_name = cname_pfx(name_bytes, b".arc");
    let gv = LLVMAddGlobal(c.module, wrap_ty, arc_name.as_ptr());
    LLVMSetLinkage(gv, LLVMLinkage::LLVMPrivateLinkage);
    LLVMSetInitializer(gv, init_val);
    LLVMSetGlobalConstant(gv, 1);
    LLVMSetUnnamedAddress(gv, LLVMUnnamedAddr::LLVMGlobalUnnamedAddr);
    LLVMSetAlignment(gv, 8);
    // Constant in-bounds GEP `wrapTy, gv, [i64 0, i32 3, i64 0]` — the i8
    // payload pointer.
    let mut indices = [
        LLVMConstInt(i64_ty, 0, 0),
        LLVMConstInt(i32_ty, 3, 0),
        LLVMConstInt(i64_ty, 0, 0),
    ];
    LLVMConstInBoundsGEP2(wrap_ty, gv, indices.as_mut_ptr(), 3)
}

// Cached variant — dedups by message bytes via `EmitCtx::arc_msg_cache`, the
// runtime-error-message path used by cast-helpers (#2097). Repeated calls
// with the same content share one global, mirroring the C++ side's
// `global_string_cache_`. Distinct from the Stage 2 `ry_emit_build_arc_global`
// boundary which is intentionally uncached at this layer (the lower crate
// manages two independent caches to preserve the regex separation invariant).
pub(crate) unsafe fn get_or_create_arc_msg_global(
    c: &mut EmitCtx,
    msg: &[u8],
    name: *const c_char,
) -> LLVMValueRef {
    if let Some(&gv) = c.arc_msg_cache.get(msg) {
        return gv;
    }
    let name_bytes = cstr_bytes(name);
    let gs = build_arc_global_raw(c, msg, name_bytes);
    c.arc_msg_cache.insert(msg.to_vec(), gs);
    gs
}

// Inttoptr the process-global ARC live-count address (captured at JIT compile
// time) and atomicrmw add `delta`.
pub(crate) unsafe fn emit_arc_counter_delta(
    builder: LLVMBuilderRef,
    i64_ty: LLVMTypeRef,
    ptr_ty: LLVMTypeRef,
    delta: i64,
) {
    let addr = __ry_arc_counter_address() as usize as u64;
    let ctr_addr_const = LLVMConstInt(i64_ty, addr, 0);
    let ctr_ptr = LLVMBuildIntToPtr(builder, ctr_addr_const, ptr_ty, c"arc_ctr".as_ptr());
    LLVMBuildAtomicRMW(
        builder,
        LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpAdd,
        ctr_ptr,
        LLVMConstInt(i64_ty, delta as u64, 0),
        LLVMAtomicOrdering::LLVMAtomicOrderingMonotonic,
        0,
    );
}

impl EmitCtx {
    // ARC live-count counter delta — Ry-internal accounting bump shared with
    // the inline ARC paths (`emit_inline_arc_alloc` for +1, `arc_release` free
    // path for −1, `arc_release` weak path emits its own −1 inline). Exposed
    // to the C++ boundary via `ry_emit_arc_counter_delta` so the weak release
    // free path can call into the same `__ry_arc_counter` global without
    // re-routing through `ry_emit_arc_release`. Added for #2190.
    pub(crate) unsafe fn arc_counter_delta(&mut self, delta: i64) {
        let i64_ty = i64_type(self.context);
        let ptr_ty = ptr_type(self.context);
        emit_arc_counter_delta(self.builder, i64_ty, ptr_ty, delta);
    }
}

// NotAtomic ordering uses a plain load (data-layout default alignment) to match
// the non-atomic codepath byte-for-byte; otherwise an 8-byte-aligned atomic
// load with the given ordering.
pub(crate) unsafe fn emit_atomic_i64_load(
    builder: LLVMBuilderRef,
    i64_ty: LLVMTypeRef,
    ptr: LLVMValueRef,
    ordering: LLVMAtomicOrdering,
    name: *const c_char,
) -> LLVMValueRef {
    let ld = LLVMBuildLoad2(builder, i64_ty, ptr, name);
    if ordering != LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic {
        LLVMSetAlignment(ld, 8);
        LLVMSetOrdering(ld, ordering);
    }
    ld
}

// malloc ARC_HEADER_SIZE + boxDataSize, bump the live-count (+1), init
// strong=1/weak=0, return the headerPtr (caller GEPs +ARC_HEADER_SIZE for the
// data pointer).
pub(crate) unsafe fn emit_inline_arc_alloc(
    c: &mut EmitCtx,
    box_data_size: LLVMValueRef,
) -> LLVMValueRef {
    let b = c.builder;
    let context = c.context;
    let module = c.module;
    let i64_ty = i64_type(context);
    let ptr_ty = ptr_type(context);
    let arc_header_ty = arc_header_type(context);
    let header_size_c = LLVMConstInt(i64_ty, ARC_HEADER_SIZE, 0);
    let total_size = LLVMBuildAdd(b, header_size_c, box_data_size, c"arc_alloc_size".as_ptr());
    let header_ptr = emit_malloc(b, module, i64_ty, ptr_ty, total_size, c"arc_box".as_ptr());
    emit_arc_counter_delta(b, i64_ty, ptr_ty, 1);
    let strong_ptr = LLVMBuildStructGEP2(
        b,
        arc_header_ty,
        header_ptr,
        0,
        c"arc_box_strong_ptr".as_ptr(),
    );
    LLVMBuildStore(b, LLVMConstInt(i64_ty, 1, 0), strong_ptr);
    let weak_ptr = LLVMBuildStructGEP2(
        b,
        arc_header_ty,
        header_ptr,
        1,
        c"arc_box_weak_ptr".as_ptr(),
    );
    LLVMBuildStore(b, LLVMConstInt(i64_ty, 0, 0), weak_ptr);
    header_ptr
}

impl EmitCtx {
    // ARC retain. `header` is the already-resolved ARC/String header pointer
    // (the C++ path resolves the u32 id at the abi boundary; the Rust-direct
    // path passes the value straight in).
    pub(crate) unsafe fn arc_retain(&mut self, header: ValueRef, atomic: Atomicity) {
        let b = self.builder;
        let context = self.context;
        let header_ptr = header.0;
        let i64_ty = i64_type(context);
        let arc_header_ty = arc_header_type(context);
        let strong_ptr =
            LLVMBuildStructGEP2(b, arc_header_ty, header_ptr, 0, c"arc_retain_ptr".as_ptr());
        // Skip immortal objects; the load must still be atomic in atomic mode (#630).
        let cur = emit_atomic_i64_load(
            b,
            i64_ty,
            strong_ptr,
            if atomic == Atomicity::Atomic {
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
        if atomic == Atomicity::Atomic {
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

    pub(crate) unsafe fn arc_release(
        &mut self,
        header: ValueRef,
        atomic: Atomicity,
        destructor_callee: Option<ValueRef>,
        gc_visit_fn: Option<ValueRef>,
    ) {
        let b = self.builder;
        let context = self.context;
        let module = self.module;
        let header_ptr = header.0;
        let i64_ty = i64_type(context);
        let i8_ty = i8_type(context);
        let void_ty = void_type(context);
        let ptr_ty = ptr_type(context);
        let arc_header_ty = arc_header_type(context);

        let strong_ptr =
            LLVMBuildStructGEP2(b, arc_header_ty, header_ptr, 0, c"arc_rel_ptr".as_ptr());
        let cur_check = emit_atomic_i64_load(
            b,
            i64_ty,
            strong_ptr,
            if atomic == Atomicity::Atomic {
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
        let is_zero = if atomic == Atomicity::Atomic {
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

        if let Some(gc_visit_fn) = gc_visit_fn {
            let track_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"arc.gc_track".as_ptr());
            LLVMBuildCondBr(b, is_zero, free_bb, track_bb);
            LLVMPositionBuilderAtEnd(b, track_bb);
            let mut gc_track_params = [ptr_ty, ptr_ty, ptr_ty];
            let gc_track_ty = LLVMFunctionType(void_ty, gc_track_params.as_mut_ptr(), 3, 0);
            let gc_track_fn =
                get_or_insert_function(module, c"__ry_gc_track".as_ptr(), gc_track_ty);
            let dtor_ptr = match destructor_callee {
                Some(d) => d.0,
                None => LLVMConstNull(ptr_ty),
            };
            let gc_visit_fn_val = gc_visit_fn.0;
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
        let gc_untrack_fn =
            get_or_insert_function(module, c"__ry_gc_untrack".as_ptr(), gc_untrack_ty);
        let mut gc_untrack_args = [header_ptr];
        LLVMBuildCall2(
            b,
            gc_untrack_ty,
            gc_untrack_fn,
            gc_untrack_args.as_mut_ptr(),
            1,
            c"".as_ptr(),
        );
        if let Some(destructor_callee) = destructor_callee {
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
                destructor_callee.0,
                dtor_args.as_mut_ptr(),
                1,
                c"".as_ptr(),
            );
        }
        // weak_count read: atomic load scaled to the caller's atomic mode (Acquire
        // when atomic) — weak_count atomic-load rule (#1968).
        let weak_ptr =
            LLVMBuildStructGEP2(b, arc_header_ty, header_ptr, 1, c"arc_weak_ptr".as_ptr());
        let weak_count = emit_atomic_i64_load(
            b,
            i64_ty,
            weak_ptr,
            if atomic == Atomicity::Atomic {
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
}
