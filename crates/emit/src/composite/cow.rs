//! Copy-on-Write: `EmitCtx::cow_ensure_unique` plus the phi-based retain loop
//! that retains each ARC element/key of a freshly cloned buffer. (core-role: an
//! `impl EmitCtx` using only the core engine, so it is abi-independent and the
//! `core⇏abi` invariant covers this module too. Migrated #2062.) The C boundary
//! entry point `ry_emit_cow_ensure_unique` lives in `abi/cow.rs`, which resolves
//! the u32 ids, maps the C `RyCowEnsureUniqueDesc` into the Rust-native
//! `CowEnsureUnique`, and interns the result. The retain / release of ARC
//! elements is done by calling the `arc` engine methods (`arc_retain` /
//! `arc_release`) directly — the Rust-direct convergence point shared with
//! `any.rs`, with no id-bridge round-trip.

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::{LLVMABISizeOfType, LLVMGetModuleDataLayout};
use llvm_sys::{LLVMAtomicOrdering, LLVMIntPredicate};
use std::ffi::c_char;

use crate::composite::arc::{emit_arc_counter_delta, emit_atomic_i64_load};
use crate::composite::header::{arc_header_type, build_header_struct};
use crate::context::*;
use crate::primitive::libc::{emit_malloc, emit_memcpy};
use crate::primitive::types::{i64_type, i8_type, ptr_type};
use crate::primitive::util::cname3;

// Rust-native descriptor for cow_ensure_unique. The abi shell (`abi/cow.rs`)
// resolves the u32 handle ids to `ValueRef`, maps the `c_int` kind / atomic and
// the flag fields into core vocabulary (`CowKind` / `Atomicity` / `bool` /
// `Option<ValueRef>`), and builds this before calling the engine method. The C
// `RyCowEnsureUniqueDesc` stays the locked boundary surface in `abi.rs` (see
// there for the per-field semantics); this is its already-converted core-side
// counterpart, consumed only here, so the docs below note just the post-shell
// form.
pub(crate) struct CowEnsureUnique {
    /// Resolved `data_ptr_id`.
    pub(crate) data_ptr: ValueRef,
    /// Resolved `slot_ptr_id`.
    pub(crate) slot_ptr: ValueRef,
    /// `kind` validated into `CowKind`.
    pub(crate) kind: CowKind,
    /// `atomic` mapped to `Atomicity`.
    pub(crate) atomic: Atomicity,
    /// Element byte size (List / Set).
    pub(crate) elem_size: u64,
    /// Key byte size (Map).
    pub(crate) key_size: u64,
    /// Value byte size (Map).
    pub(crate) val_size: u64,
    /// `do_elem_retain` as `bool`.
    pub(crate) do_elem_retain: bool,
    /// `elem_is_str` as `bool` (retain header-offset selector).
    pub(crate) elem_is_str: bool,
    /// `do_key_retain` as `bool`.
    pub(crate) do_key_retain: bool,
    /// `key_is_str` as `bool` (retain header-offset selector).
    pub(crate) key_is_str: bool,
    /// Resolved `destructor_callee` (`None` when null).
    pub(crate) destructor_callee: Option<ValueRef>,
}

impl EmitCtx {
    // A phi-based for loop (used by cow_ensure_unique) that retains each ARC
    // element/key of the cloned buffer by calling the `arc_retain` engine method
    // directly (no id-bridge round-trip; sequential `&mut self` so no borrow is
    // held across the call).
    //
    // The cached LLVM types (i64/i8/ptr) and the per-call (field_idx, is_str, tag)
    // are passed individually, mirroring how the emission code threads these types
    // throughout. Bundling them into a struct only here would be asymmetric and would
    // touch a hot codegen path, so the lint is suppressed rather than refactored
    // (see .claude/rules/build-warning-flags.md "intentional patterns" policy).
    #[allow(clippy::too_many_arguments)]
    unsafe fn cow_retain_loop(
        &mut self,
        header_ty: LLVMTypeRef,
        new_data_ptr: LLVMValueRef,
        i64_ty: LLVMTypeRef,
        i8_ty: LLVMTypeRef,
        ptr_ty: LLVMTypeRef,
        field_idx: u32,
        is_str: bool,
        tag: &[u8],
    ) {
        let b = self.builder;
        let context = self.context;
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
        let loop_bb = LLVMAppendBasicBlockInContext(
            context,
            loop_fn,
            cname3(b"cow.", tag, b"_loop").as_ptr(),
        );
        let body_bb = LLVMAppendBasicBlockInContext(
            context,
            loop_fn,
            cname3(b"cow.", tag, b"_body").as_ptr(),
        );
        let loop_done_bb = LLVMAppendBasicBlockInContext(
            context,
            loop_fn,
            cname3(b"cow.", tag, b"_done").as_ptr(),
        );

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
        let hdr_offset = if is_str {
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
        self.arc_retain(ValueRef(elem_hdr), Atomicity::NonAtomic);
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

    pub(crate) unsafe fn cow_ensure_unique(&mut self, d: &CowEnsureUnique) -> ValueRef {
        let b = self.builder;
        let context = self.context;
        let module = self.module;
        let data_ptr = d.data_ptr.0;
        let slot_ptr = d.slot_ptr.0;
        let i64_ty = i64_type(context);
        let i8_ty = i8_type(context);
        let ptr_ty = ptr_type(context);
        let arc_header_ty = arc_header_type(context);

        // The header struct shape is single-sourced in `core::header_fields`
        // (mirrors CodeGen's listHeaderTy_/mapHeaderTy_/setHeaderTy_ in
        // src/codegen.cpp); the C++<->Rust sync is mechanically guarded by the
        // parity test (tests/test_header_layout.cpp via ry_emit_test_header_layout)
        // and the same-type-swap behavioral coverage in tests/spec/cow.test.ry
        // (#2071). The field *indices* used for GEP below stay local to cow
        // (data = elems/vals, key = keys) — only the struct shape is shared.
        let (header_ty, data_field_idx, key_field_idx): (LLVMTypeRef, u32, u32) = match d.kind {
            CowKind::List => (build_header_struct(context, HeaderKind::List), 2, 0),
            CowKind::Map => (build_header_struct(context, HeaderKind::Map), 3, 2),
            CowKind::Set => (build_header_struct(context, HeaderKind::Set), 2, 0),
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

        let atomic_mode = d.atomic;
        let strong_ptr =
            LLVMBuildStructGEP2(b, arc_header_ty, header_ptr, 0, c"cow_strong_ptr".as_ptr());
        let strong = emit_atomic_i64_load(
            b,
            i64_ty,
            strong_ptr,
            if atomic_mode == Atomicity::Atomic {
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

        // `alloc_buf` stays in the dispatcher for the ARC-box allocation below;
        // the per-kind copy helpers call `emit_malloc` / `emit_memcpy` directly.
        let alloc_buf = move |byte_size: LLVMValueRef, name: *const c_char| -> LLVMValueRef {
            unsafe { emit_malloc(b, module, i64_ty, ptr_ty, byte_size, name) }
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

        let old_len_ptr =
            LLVMBuildStructGEP2(b, header_ty, data_ptr, 0, c"cow_old_len_ptr".as_ptr());
        let old_len = LLVMBuildLoad2(b, i64_ty, old_len_ptr, c"cow_old_len".as_ptr());

        // Per-kind buffer copy (List data / Map keys+vals+buckets / Set
        // elems+buckets). Each helper emits its own allocations + memcpys + new
        // header field stores; the shared scaffold above and the retain loops /
        // release / merge PHI below stay in the dispatcher.
        match d.kind {
            CowKind::List => {
                cow_copy_list(
                    b,
                    module,
                    header_ty,
                    data_ptr,
                    new_data_ptr,
                    old_len,
                    i64_ty,
                    ptr_ty,
                    d.elem_size,
                );
            }
            CowKind::Map => {
                cow_copy_map(
                    b,
                    module,
                    header_ty,
                    data_ptr,
                    new_data_ptr,
                    old_len,
                    i64_ty,
                    ptr_ty,
                    d.key_size,
                    d.val_size,
                );
            }
            CowKind::Set => {
                cow_copy_set(
                    b,
                    module,
                    header_ty,
                    data_ptr,
                    new_data_ptr,
                    old_len,
                    i64_ty,
                    ptr_ty,
                    d.elem_size,
                );
            }
        }

        // Element / key retain loops (always Atomicity::NonAtomic — the clone is private).
        if d.do_elem_retain {
            self.cow_retain_loop(
                header_ty,
                new_data_ptr,
                i64_ty,
                i8_ty,
                ptr_ty,
                data_field_idx,
                d.elem_is_str,
                b"elem",
            );
        }
        if d.do_key_retain {
            self.cow_retain_loop(
                header_ty,
                new_data_ptr,
                i64_ty,
                i8_ty,
                ptr_ty,
                key_field_idx,
                d.key_is_str,
                b"key",
            );
        }

        // Release the old header (the helper leaves the builder on arc.done).
        self.arc_release(ValueRef(header_ptr), atomic_mode, d.destructor_callee, None);

        LLVMBuildStore(b, new_data_ptr, slot_ptr);

        let copy_end_bb = LLVMGetInsertBlock(b);
        LLVMBuildBr(b, cont_bb);

        LLVMPositionBuilderAtEnd(b, cont_bb);
        let phi = LLVMBuildPhi(b, ptr_ty, c"cow_ptr".as_ptr());
        let mut vals = [data_ptr, new_data_ptr];
        let mut blocks = [orig_bb, copy_end_bb];
        LLVMAddIncoming(phi, vals.as_mut_ptr(), blocks.as_mut_ptr(), 2);
        ValueRef(phi)
    }
}

// Per-kind CoW buffer copy helpers, factored out of `cow_ensure_unique`. Each is
// entered after the shared scaffold has allocated `new_data_ptr` and loaded
// `old_len`, runs on the `cow.copy` block (no new blocks created), and writes the
// cloned buffers + new-header fields. They allocate via `emit_malloc` / copy via
// `emit_memcpy` directly (the dispatcher keeps the `alloc_buf` closure only for
// the ARC-box allocation). The cached LLVM types are threaded individually,
// mirroring `cow_retain_loop`; the resulting arity trips
// `clippy::too_many_arguments`, suppressed per the same "intentional pattern"
// policy (see .claude/rules/build-warning-flags.md) rather than bundled into a
// struct that would touch this hot codegen path.

#[allow(clippy::too_many_arguments)]
unsafe fn cow_copy_list(
    b: LLVMBuilderRef,
    module: LLVMModuleRef,
    header_ty: LLVMTypeRef,
    data_ptr: LLVMValueRef,
    new_data_ptr: LLVMValueRef,
    old_len: LLVMValueRef,
    i64_ty: LLVMTypeRef,
    ptr_ty: LLVMTypeRef,
    elem_size: u64,
) {
    let old_data_field =
        LLVMBuildStructGEP2(b, header_ty, data_ptr, 2, c"cow_old_data_field".as_ptr());
    let old_data = LLVMBuildLoad2(b, ptr_ty, old_data_field, c"cow_old_data".as_ptr());
    let buf_size = LLVMBuildMul(
        b,
        old_len,
        LLVMConstInt(i64_ty, elem_size, 0),
        c"cow_buf_size".as_ptr(),
    );
    let new_buf = emit_malloc(b, module, i64_ty, ptr_ty, buf_size, c"cow_new_buf".as_ptr());
    emit_memcpy(b, module, ptr_ty, i64_ty, new_buf, old_data, buf_size);
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

#[allow(clippy::too_many_arguments)]
unsafe fn cow_copy_map(
    b: LLVMBuilderRef,
    module: LLVMModuleRef,
    header_ty: LLVMTypeRef,
    data_ptr: LLVMValueRef,
    new_data_ptr: LLVMValueRef,
    old_len: LLVMValueRef,
    i64_ty: LLVMTypeRef,
    ptr_ty: LLVMTypeRef,
    key_size: u64,
    val_size: u64,
) {
    let old_keys_field =
        LLVMBuildStructGEP2(b, header_ty, data_ptr, 2, c"cow_old_keys_field".as_ptr());
    let old_keys = LLVMBuildLoad2(b, ptr_ty, old_keys_field, c"cow_old_keys".as_ptr());
    let old_vals_field =
        LLVMBuildStructGEP2(b, header_ty, data_ptr, 3, c"cow_old_vals_field".as_ptr());
    let old_vals = LLVMBuildLoad2(b, ptr_ty, old_vals_field, c"cow_old_vals".as_ptr());
    let old_bc_ptr = LLVMBuildStructGEP2(b, header_ty, data_ptr, 4, c"cow_old_bc_ptr".as_ptr());
    let old_bc = LLVMBuildLoad2(b, i64_ty, old_bc_ptr, c"cow_old_bc".as_ptr());
    let old_bk_field = LLVMBuildStructGEP2(b, header_ty, data_ptr, 5, c"cow_old_bk_ptr".as_ptr());
    let old_bk = LLVMBuildLoad2(b, ptr_ty, old_bk_field, c"cow_old_bk".as_ptr());

    let keys_size = LLVMBuildMul(
        b,
        old_len,
        LLVMConstInt(i64_ty, key_size, 0),
        c"cow_keys_size".as_ptr(),
    );
    let new_keys = emit_malloc(
        b,
        module,
        i64_ty,
        ptr_ty,
        keys_size,
        c"cow_new_keys".as_ptr(),
    );
    emit_memcpy(b, module, ptr_ty, i64_ty, new_keys, old_keys, keys_size);
    let vals_size = LLVMBuildMul(
        b,
        old_len,
        LLVMConstInt(i64_ty, val_size, 0),
        c"cow_vals_size".as_ptr(),
    );
    let new_vals = emit_malloc(
        b,
        module,
        i64_ty,
        ptr_ty,
        vals_size,
        c"cow_new_vals".as_ptr(),
    );
    emit_memcpy(b, module, ptr_ty, i64_ty, new_vals, old_vals, vals_size);
    let bk_size = LLVMBuildMul(
        b,
        old_bc,
        LLVMConstInt(i64_ty, 8, 0),
        c"cow_bk_size".as_ptr(),
    );
    let new_bk = emit_malloc(b, module, i64_ty, ptr_ty, bk_size, c"cow_new_bk".as_ptr());
    emit_memcpy(b, module, ptr_ty, i64_ty, new_bk, old_bk, bk_size);

    let new_len_ptr = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 0, c"cow_m_len_ptr".as_ptr());
    LLVMBuildStore(b, old_len, new_len_ptr);
    let new_cap_ptr = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 1, c"cow_m_cap_ptr".as_ptr());
    LLVMBuildStore(b, old_len, new_cap_ptr);
    let new_keys_field =
        LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 2, c"cow_m_keys_ptr".as_ptr());
    LLVMBuildStore(b, new_keys, new_keys_field);
    let new_vals_field =
        LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 3, c"cow_m_vals_ptr".as_ptr());
    LLVMBuildStore(b, new_vals, new_vals_field);
    let new_bc_ptr = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 4, c"cow_m_bc_ptr".as_ptr());
    LLVMBuildStore(b, old_bc, new_bc_ptr);
    let new_bk_field = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 5, c"cow_m_bk_ptr".as_ptr());
    LLVMBuildStore(b, new_bk, new_bk_field);
}

#[allow(clippy::too_many_arguments)]
unsafe fn cow_copy_set(
    b: LLVMBuilderRef,
    module: LLVMModuleRef,
    header_ty: LLVMTypeRef,
    data_ptr: LLVMValueRef,
    new_data_ptr: LLVMValueRef,
    old_len: LLVMValueRef,
    i64_ty: LLVMTypeRef,
    ptr_ty: LLVMTypeRef,
    elem_size: u64,
) {
    let old_elems_field =
        LLVMBuildStructGEP2(b, header_ty, data_ptr, 2, c"cow_old_elems_field".as_ptr());
    let old_elems = LLVMBuildLoad2(b, ptr_ty, old_elems_field, c"cow_old_elems".as_ptr());
    let old_bc_ptr = LLVMBuildStructGEP2(b, header_ty, data_ptr, 3, c"cow_old_bc_ptr".as_ptr());
    let old_bc = LLVMBuildLoad2(b, i64_ty, old_bc_ptr, c"cow_old_bc".as_ptr());
    let old_bk_field = LLVMBuildStructGEP2(b, header_ty, data_ptr, 4, c"cow_old_bk_ptr".as_ptr());
    let old_bk = LLVMBuildLoad2(b, ptr_ty, old_bk_field, c"cow_old_bk".as_ptr());

    let elems_size = LLVMBuildMul(
        b,
        old_len,
        LLVMConstInt(i64_ty, elem_size, 0),
        c"cow_elems_size".as_ptr(),
    );
    let new_elems = emit_malloc(
        b,
        module,
        i64_ty,
        ptr_ty,
        elems_size,
        c"cow_new_elems".as_ptr(),
    );
    emit_memcpy(b, module, ptr_ty, i64_ty, new_elems, old_elems, elems_size);
    let bk_size = LLVMBuildMul(
        b,
        old_bc,
        LLVMConstInt(i64_ty, 8, 0),
        c"cow_bk_size".as_ptr(),
    );
    let new_bk = emit_malloc(b, module, i64_ty, ptr_ty, bk_size, c"cow_new_bk".as_ptr());
    emit_memcpy(b, module, ptr_ty, i64_ty, new_bk, old_bk, bk_size);

    let new_len_ptr = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 0, c"cow_s_len_ptr".as_ptr());
    LLVMBuildStore(b, old_len, new_len_ptr);
    let new_cap_ptr = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 1, c"cow_s_cap_ptr".as_ptr());
    LLVMBuildStore(b, old_len, new_cap_ptr);
    let new_elems_field =
        LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 2, c"cow_s_elems_ptr".as_ptr());
    LLVMBuildStore(b, new_elems, new_elems_field);
    let new_bc_ptr = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 3, c"cow_s_bc_ptr".as_ptr());
    LLVMBuildStore(b, old_bc, new_bc_ptr);
    let new_bk_field = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 4, c"cow_s_bk_ptr".as_ptr());
    LLVMBuildStore(b, new_bk, new_bk_field);
}
