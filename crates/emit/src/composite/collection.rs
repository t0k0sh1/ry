//! List collection mutation IR generation (core-role: an `impl EmitCtx` over the
//! core engine only, so it is abi-independent and the `core⇏abi` invariant
//! covers this module). `append` / `insert` / `remove_at` / `slice` build the
//! malloc / memcpy / memmove + header GEP / load / store sequences; the shared
//! list-grow and header-load helpers live in `core`. `insert` / `remove_at` call
//! `self.negative_index_wrap` / `self.bounds_error` directly (no id bridge) — the
//! former `intern` → extern → `resolve` round-trip is gone (IR-neutral; ids never
//! appear in the emitted IR). `slice` returns a `SliceParts` aggregate. The C++
//! path enters through the `abi::collection` externs, which resolve u32 ids, wrap
//! the type handles, and intern any result. The builder-derived parent rule
//! (#1996) holds: `append` / `insert` / `remove_at` create BBs, `slice` none.

use llvm_sys::core::*;
use llvm_sys::prelude::{LLVMBuilderRef, LLVMContextRef, LLVMModuleRef, LLVMTypeRef};
use llvm_sys::target::{LLVMABISizeOfType, LLVMGetModuleDataLayout};
use llvm_sys::{LLVMIntPredicate, LLVMRealPredicate};

use crate::composite::arc::emit_arc_counter_delta;
use crate::composite::header::{emit_list_grow, load_list_header};
use crate::context::*;
use crate::primitive::libc::{emit_malloc, emit_memcpy, emit_memmove};
use crate::primitive::module::get_or_insert_function;
use crate::primitive::types::{i32_type, i64_type, ptr_type, void_type};

impl EmitCtx {
    pub(crate) unsafe fn collection_append(
        &mut self,
        list_ptr: ValueRef,
        val: ValueRef,
        list_header_ty: TypeRef,
        elem_ty: TypeRef,
        elem_size: u64,
    ) {
        let b = self.builder;
        let context = self.context;
        let module = self.module;
        let list_ptr = list_ptr.0;
        let val = val.0;
        let list_header_ty = list_header_ty.0;
        let elem_ty = elem_ty.0;
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

    pub(crate) unsafe fn collection_insert(
        &mut self,
        list_ptr: ValueRef,
        orig_idx: ValueRef,
        val: ValueRef,
        list_header_ty: TypeRef,
        elem_ty: TypeRef,
        elem_size: u64,
    ) {
        let context = self.context;
        let b = self.builder;
        let module = self.module;
        let list_ptr = list_ptr.0;
        let orig_idx = orig_idx.0;
        let val = val.0;
        let list_header_ty = list_header_ty.0;
        let elem_ty = elem_ty.0;
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
        let idx = self
            .negative_index_wrap(ValueRef(orig_idx), ValueRef(wrap_base), c"ins".as_ptr())
            .0;

        let zero = LLVMConstInt(i64_ty, 0, 0);
        let neg_check = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLT, idx, zero, c"".as_ptr());
        let over_check = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSGT, idx, h.len, c"".as_ptr());
        let out_of_bounds = LLVMBuildOr(b, neg_check, over_check, c"ins_oob".as_ptr());
        let err_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"ins.err".as_ptr());
        let ok_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"ins.ok".as_ptr());
        LLVMBuildCondBr(b, out_of_bounds, err_bb, ok_bb);
        LLVMPositionBuilderAtEnd(b, err_bb);
        self.bounds_error(
            ValueRef(orig_idx),
            ValueRef(h.len),
            c"runtime error: index %lld out of bounds for insert() on list of length %lld\n"
                .as_ptr(),
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

    pub(crate) unsafe fn collection_remove_at(
        &mut self,
        list_ptr: ValueRef,
        orig_idx: ValueRef,
        list_header_ty: TypeRef,
        elem_ty: TypeRef,
        elem_size: u64,
    ) -> ValueRef {
        let context = self.context;
        let b = self.builder;
        let module = self.module;
        let list_ptr = list_ptr.0;
        let orig_idx = orig_idx.0;
        let list_header_ty = list_header_ty.0;
        let elem_ty = elem_ty.0;
        let i64_ty = i64_type(context);
        let ptr_ty = ptr_type(context);
        let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

        let h = load_list_header(b, list_header_ty, list_ptr, i64_ty, ptr_ty, b"rmat");

        let idx = self
            .negative_index_wrap(ValueRef(orig_idx), ValueRef(h.len), c"rmat".as_ptr())
            .0;

        let zero = LLVMConstInt(i64_ty, 0, 0);
        let neg_check = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLT, idx, zero, c"".as_ptr());
        let over_check = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSGE, idx, h.len, c"".as_ptr());
        let out_of_bounds = LLVMBuildOr(b, neg_check, over_check, c"rmat_oob".as_ptr());
        let err_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"rmat.err".as_ptr());
        let ok_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"rmat.ok".as_ptr());
        LLVMBuildCondBr(b, out_of_bounds, err_bb, ok_bb);
        LLVMPositionBuilderAtEnd(b, err_bb);
        self.bounds_error(
            ValueRef(orig_idx),
            ValueRef(h.len),
            c"runtime error: index %lld out of bounds for removeAt() on list of length %lld\n"
                .as_ptr(),
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

        ValueRef(removed_val)
    }

    /// Linear-search-and-memmove `remove(val)` on a List (#2095). Mirrors the
    /// C++ `emitListRemove` (`src/codegen_call_collection.cpp`) byte-for-byte.
    /// Always returns i64 0 (unit-like). The ptr-element allowlist guard
    /// (str-only) stays C++-side because it reads `ValueMetadata`.
    #[allow(clippy::too_many_arguments)]
    pub(crate) unsafe fn list_remove(
        &mut self,
        container_ptr: ValueRef,
        val: ValueRef,
        list_header_ty: TypeRef,
        list_elem_ty: TypeRef,
        elem_size: u64,
        elem_is_str: bool,
        elem_is_double: bool,
    ) -> ValueRef {
        let context = self.context;
        let b = self.builder;
        let module = self.module;
        let container_ptr = container_ptr.0;
        let val = val.0;
        let list_header_ty = list_header_ty.0;
        let list_elem_ty = list_elem_ty.0;
        let i64_ty = i64_type(context);
        let i32_ty = i32_type(context);
        let ptr_ty = ptr_type(context);
        let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

        // List header load — INTERLEAVED gep/load to match C++ CodeGen::loadListHeader
        // (src/codegen_call.cpp); core::load_list_header groups the geps then the
        // loads, which would reorder the IR. `lrem_cap` is loaded-but-unused, exactly
        // as the C++ helper does, so the IR stays byte-identical.
        let len_ptr = LLVMBuildStructGEP2(
            b,
            list_header_ty,
            container_ptr,
            LIST_FIELD_LEN,
            c"lrem_len_ptr".as_ptr(),
        );
        let len = LLVMBuildLoad2(b, i64_ty, len_ptr, c"lrem_len".as_ptr());
        let cap_ptr = LLVMBuildStructGEP2(
            b,
            list_header_ty,
            container_ptr,
            LIST_FIELD_CAP,
            c"lrem_cap_ptr".as_ptr(),
        );
        let _cap = LLVMBuildLoad2(b, i64_ty, cap_ptr, c"lrem_cap".as_ptr());
        let data_ptr = LLVMBuildStructGEP2(
            b,
            list_header_ty,
            container_ptr,
            LIST_FIELD_DATA,
            c"lrem_data_ptr".as_ptr(),
        );
        let data = LLVMBuildLoad2(b, ptr_ty, data_ptr, c"lrem_data".as_ptr());

        // foundIdx = -1 (sentinel for "not found"); iVar = 0
        let found_idx = LLVMBuildAlloca(b, i64_ty, c"lrem_found_idx".as_ptr());
        LLVMBuildStore(b, LLVMConstInt(i64_ty, u64::MAX, 0), found_idx);
        let i_var = LLVMBuildAlloca(b, i64_ty, c"lrem_i".as_ptr());
        LLVMBuildStore(b, LLVMConstInt(i64_ty, 0, 0), i_var);

        let cond_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"lrem.cond".as_ptr());
        let body_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"lrem.body".as_ptr());
        let end_search_bb =
            LLVMAppendBasicBlockInContext(context, fn_v, c"lrem.end_search".as_ptr());

        LLVMBuildBr(b, cond_bb);
        LLVMPositionBuilderAtEnd(b, cond_bb);
        let i_val = LLVMBuildLoad2(b, i64_ty, i_var, c"lrem_iv".as_ptr());
        let fi_val = LLVMBuildLoad2(b, i64_ty, found_idx, c"lrem_fi".as_ptr());
        let not_yet_found = LLVMBuildICmp(
            b,
            LLVMIntPredicate::LLVMIntSLT,
            fi_val,
            LLVMConstInt(i64_ty, 0, 0),
            c"lrem_not_found".as_ptr(),
        );
        let in_bounds = LLVMBuildICmp(
            b,
            LLVMIntPredicate::LLVMIntSLT,
            i_val,
            len,
            c"lrem_in_bounds".as_ptr(),
        );
        let cont = LLVMBuildAnd(b, not_yet_found, in_bounds, c"lrem_cont".as_ptr());
        LLVMBuildCondBr(b, cont, body_bb, end_search_bb);

        LLVMPositionBuilderAtEnd(b, body_bb);
        let i_cur = LLVMBuildLoad2(b, i64_ty, i_var, c"lrem_ic".as_ptr());
        let mut elem_idx = [i_cur];
        let elem_ptr = LLVMBuildGEP2(
            b,
            list_elem_ty,
            data,
            elem_idx.as_mut_ptr(),
            1,
            c"lrem_elem_ptr".as_ptr(),
        );
        let list_elem = LLVMBuildLoad2(b, list_elem_ty, elem_ptr, c"lrem_elem".as_ptr());

        let matched = if elem_is_str {
            // strcmp(val, list_elem) == 0
            let mut strcmp_p = [ptr_ty, ptr_ty];
            let strcmp_ty = LLVMFunctionType(i32_ty, strcmp_p.as_mut_ptr(), 2, 0);
            let strcmp_fn = get_or_insert_function(module, c"strcmp".as_ptr(), strcmp_ty);
            let mut strcmp_a = [val, list_elem];
            let cmp = LLVMBuildCall2(
                b,
                strcmp_ty,
                strcmp_fn,
                strcmp_a.as_mut_ptr(),
                2,
                c"lrem_strcmp".as_ptr(),
            );
            LLVMBuildICmp(
                b,
                LLVMIntPredicate::LLVMIntEQ,
                cmp,
                LLVMConstInt(i32_ty, 0, 0),
                c"lrem_match".as_ptr(),
            )
        } else if elem_is_double {
            LLVMBuildFCmp(
                b,
                LLVMRealPredicate::LLVMRealOEQ,
                val,
                list_elem,
                c"lrem_match".as_ptr(),
            )
        } else {
            LLVMBuildICmp(
                b,
                LLVMIntPredicate::LLVMIntEQ,
                val,
                list_elem,
                c"lrem_match".as_ptr(),
            )
        };

        let found_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"lrem.found".as_ptr());
        let next_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"lrem.next".as_ptr());
        LLVMBuildCondBr(b, matched, found_bb, next_bb);

        LLVMPositionBuilderAtEnd(b, found_bb);
        LLVMBuildStore(b, i_cur, found_idx);
        LLVMBuildBr(b, cond_bb);

        LLVMPositionBuilderAtEnd(b, next_bb);
        let i_next = LLVMBuildAdd(b, i_cur, LLVMConstInt(i64_ty, 1, 0), c"lrem_inext".as_ptr());
        LLVMBuildStore(b, i_next, i_var);
        LLVMBuildBr(b, cond_bb);

        LLVMPositionBuilderAtEnd(b, end_search_bb);
        let idx = LLVMBuildLoad2(b, i64_ty, found_idx, c"lrem_idx".as_ptr());
        let was_found = LLVMBuildICmp(
            b,
            LLVMIntPredicate::LLVMIntSGE,
            idx,
            LLVMConstInt(i64_ty, 0, 0),
            c"lrem_was_found".as_ptr(),
        );

        let remove_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"lrem.remove".as_ptr());
        let done_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"lrem.done".as_ptr());
        LLVMBuildCondBr(b, was_found, remove_bb, done_bb);

        LLVMPositionBuilderAtEnd(b, remove_bb);
        let mut dst_idx = [idx];
        let dst_ptr = LLVMBuildGEP2(
            b,
            list_elem_ty,
            data,
            dst_idx.as_mut_ptr(),
            1,
            c"lrem_dst".as_ptr(),
        );
        let idx_plus_one = LLVMBuildAdd(b, idx, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
        let mut src_idx_a = [idx_plus_one];
        let src_ptr = LLVMBuildGEP2(
            b,
            list_elem_ty,
            data,
            src_idx_a.as_mut_ptr(),
            1,
            c"lrem_src".as_ptr(),
        );
        let len_minus_idx = LLVMBuildSub(b, len, idx, c"".as_ptr());
        let move_count = LLVMBuildSub(
            b,
            len_minus_idx,
            LLVMConstInt(i64_ty, 1, 0),
            c"lrem_move_count".as_ptr(),
        );
        let move_bytes = LLVMBuildMul(
            b,
            move_count,
            LLVMConstInt(i64_ty, elem_size, 0),
            c"lrem_move_bytes".as_ptr(),
        );
        emit_memmove(b, module, ptr_ty, i64_ty, dst_ptr, src_ptr, move_bytes);
        let new_len = LLVMBuildSub(b, len, LLVMConstInt(i64_ty, 1, 0), c"lrem_new_len".as_ptr());
        LLVMBuildStore(b, new_len, len_ptr);
        LLVMBuildBr(b, done_bb);

        LLVMPositionBuilderAtEnd(b, done_bb);
        ValueRef(LLVMConstInt(i64_ty, 0, 0))
    }

    pub(crate) unsafe fn list_slice(
        &mut self,
        list_ptr: ValueRef,
        start_val: ValueRef,
        end_excl_val: ValueRef,
        list_header_ty: TypeRef,
        elem_ty: TypeRef,
        elem_size: u64,
    ) -> SliceParts {
        let b = self.builder;
        let list_ptr = list_ptr.0;
        let start_val = start_val.0;
        let end_excl_val = end_excl_val.0;
        let list_header_ty = list_header_ty.0;
        let elem_ty = elem_ty.0;
        let i64_ty = i64_type(self.context);
        let ptr_ty = ptr_type(self.context);

        let sl_len_ptr = LLVMBuildStructGEP2(
            b,
            list_header_ty,
            list_ptr,
            LIST_FIELD_LEN,
            c"sl_len_ptr".as_ptr(),
        );
        let sl_data_ptr = LLVMBuildStructGEP2(
            b,
            list_header_ty,
            list_ptr,
            LIST_FIELD_DATA,
            c"sl_data_ptr".as_ptr(),
        );
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

        let new_data = emit_malloc(
            b,
            self.module,
            i64_ty,
            ptr_ty,
            data_size,
            c"sl_data".as_ptr(),
        );

        let mut gep_idx = [c_start];
        let src_offset = LLVMBuildGEP2(
            b,
            elem_ty,
            sl_data,
            gep_idx.as_mut_ptr(),
            1,
            c"sl_src_off".as_ptr(),
        );
        emit_memcpy(
            b,
            self.module,
            ptr_ty,
            i64_ty,
            new_data,
            src_offset,
            data_size,
        );

        SliceParts {
            count: ValueRef(count),
            new_data: ValueRef(new_data),
        }
    }

    /// Single-source full-buffer copy shared by `keys` / `values` / `take`
    /// (#2093): `<ds> = count * elem_size`, `<nd> = malloc(<ds>)`,
    /// `memcpy(<nd>, src_data, <ds>)`, return `<nd>`. Alloc count == copy count,
    /// so a single size-mul drives both. The caller (C++) has already loaded the
    /// source buffer and the count (`mapLen` / clamped `n`) and emitted the new
    /// ARC header; this op is the malloc+memcpy chain only. SSA names are picked
    /// by `kind` to stay byte-identical to the pre-migration inline emission.
    /// Creates no BBs.
    pub(crate) unsafe fn list_copy_full(
        &mut self,
        src_data: ValueRef,
        count: ValueRef,
        elem_size: u64,
        kind: ListCopyKind,
    ) -> ValueRef {
        let b = self.builder;
        let i64_ty = i64_type(self.context);
        let ptr_ty = ptr_type(self.context);
        let (ds_name, nd_name) = match kind {
            ListCopyKind::Keys => (c"keys_ds".as_ptr(), c"keys_nd".as_ptr()),
            ListCopyKind::Values => (c"vals_ds".as_ptr(), c"vals_nd".as_ptr()),
            ListCopyKind::Take => (c"tk_dsize".as_ptr(), c"tk_data".as_ptr()),
        };
        let ds = LLVMBuildMul(b, count.0, LLVMConstInt(i64_ty, elem_size, 0), ds_name);
        let nd = emit_malloc(b, self.module, i64_ty, ptr_ty, ds, nd_name);
        emit_memcpy(b, self.module, ptr_ty, i64_ty, nd, src_data.0, ds);
        ValueRef(nd)
    }

    /// Non-destructive `appended` copy (#2093): alloc `new_len` elements but copy
    /// only the `old_len`-element source range (the appended element is stored by
    /// the C++ caller after its retain loop, so it is NOT part of this op).
    /// `apd_ds = new_len * elem_size`, `apd_nd = malloc(apd_ds)`,
    /// `apd_ods = old_len * elem_size`, `memcpy(apd_nd, src_data, apd_ods)`,
    /// return `apd_nd`. `new_len` (apd_new_len, `old_len + 1`) is passed in rather
    /// than recomputed so the caller's storeListHeaderFields reuses the same SSA
    /// value. Creates no BBs.
    pub(crate) unsafe fn list_appended_copy(
        &mut self,
        new_len: ValueRef,
        old_len: ValueRef,
        src_data: ValueRef,
        elem_size: u64,
    ) -> ValueRef {
        let b = self.builder;
        let i64_ty = i64_type(self.context);
        let ptr_ty = ptr_type(self.context);
        let esz = LLVMConstInt(i64_ty, elem_size, 0);
        let ds = LLVMBuildMul(b, new_len.0, esz, c"apd_ds".as_ptr());
        let nd = emit_malloc(b, self.module, i64_ty, ptr_ty, ds, c"apd_nd".as_ptr());
        let ods = LLVMBuildMul(b, old_len.0, esz, c"apd_ods".as_ptr());
        emit_memcpy(b, self.module, ptr_ty, i64_ty, nd, src_data.0, ods);
        ValueRef(nd)
    }

    /// Two-source List concat copy (#2093): alloc `new_len` (= lhs+rhs) elements,
    /// `memcpy` the lhs buffer at offset 0, then `memcpy` the rhs buffer at the
    /// element-typed GEP offset `lhs_len`. `cat_ds = new_len * elem_size`,
    /// `cat_data = malloc(cat_ds)`, `cat_ls = lhs_len * elem_size`,
    /// `memcpy(cat_data, lhs_data, cat_ls)`,
    /// `cat_rhs_dst = gep elem_ty, cat_data, lhs_len`,
    /// `cat_rs = rhs_len * elem_size`, `memcpy(cat_rhs_dst, rhs_data, cat_rs)`,
    /// return `cat_data`. `new_len` (cat_len) is passed in so the caller reuses
    /// the same SSA value for storeListHeaderFields / the retain loop. This is the
    /// only copy op that needs `elem_ty` (for the mid-buffer GEP). Creates no BBs.
    #[allow(clippy::too_many_arguments)]
    pub(crate) unsafe fn list_concat_copy(
        &mut self,
        new_len: ValueRef,
        lhs_len: ValueRef,
        lhs_data: ValueRef,
        rhs_len: ValueRef,
        rhs_data: ValueRef,
        elem_ty: TypeRef,
        elem_size: u64,
    ) -> ValueRef {
        let b = self.builder;
        let i64_ty = i64_type(self.context);
        let ptr_ty = ptr_type(self.context);
        let esz = LLVMConstInt(i64_ty, elem_size, 0);
        let ds = LLVMBuildMul(b, new_len.0, esz, c"cat_ds".as_ptr());
        let nd = emit_malloc(b, self.module, i64_ty, ptr_ty, ds, c"cat_data".as_ptr());
        let ls = LLVMBuildMul(b, lhs_len.0, esz, c"cat_ls".as_ptr());
        emit_memcpy(b, self.module, ptr_ty, i64_ty, nd, lhs_data.0, ls);
        let mut rhs_idx = [lhs_len.0];
        let rhs_dst = LLVMBuildGEP2(
            b,
            elem_ty.0,
            nd,
            rhs_idx.as_mut_ptr(),
            1,
            c"cat_rhs_dst".as_ptr(),
        );
        let rs = LLVMBuildMul(b, rhs_len.0, esz, c"cat_rs".as_ptr());
        emit_memcpy(b, self.module, ptr_ty, i64_ty, rhs_dst, rhs_data.0, rs);
        ValueRef(nd)
    }

    /// O(n²) `distinct(list)` dedup (#2095). The caller (C++) emits
    /// `loadListHeader` BEFORE `emitArcAllocCollectionHeader` to match the C++
    /// baseline's instruction order, then passes the loaded `src_len` /
    /// `src_data` + the new ARC header to this engine method. After return,
    /// the caller runs `setTypeMeta` / `propagateMeta` (those need
    /// ValueMetadata which doesn't cross the boundary). This engine method:
    /// 1. malloc(src_len * elem_size) and store into new_header.data, src_len
    ///    into new_header.cap.
    /// 2. Outer/inner loop dedup; per-element compare via strcmp+ICmpEQ
    ///    (`elem_is_str`) / FCmpOEQ (`elem_is_double`) / ICmpEQ otherwise.
    ///    Caller-side allowlist guards str-only pointer elements.
    /// 3. After loops: store the dedup count into new_header.len.
    ///
    /// All GEP indices go through `LIST_FIELD_*` constants so a future layout
    /// change is a single-point edit (`tests/test_header_layout.cpp`).
    #[allow(clippy::too_many_arguments)]
    pub(crate) unsafe fn list_distinct(
        &mut self,
        src_len: ValueRef,
        src_data: ValueRef,
        new_header: ValueRef,
        list_header_ty: TypeRef,
        elem_ty: TypeRef,
        elem_size: u64,
        elem_is_str: bool,
        elem_is_double: bool,
    ) {
        let context = self.context;
        let b = self.builder;
        let module = self.module;
        let src_len = src_len.0;
        let src_data = src_data.0;
        let new_header = new_header.0;
        let list_header_ty = list_header_ty.0;
        let elem_ty = elem_ty.0;
        let i64_ty = i64_type(context);
        let i32_ty = i32_type(context);
        let i1_ty = LLVMInt1TypeInContext(context);
        let ptr_ty = ptr_type(context);
        let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

        // Allocate the new data buffer (capacity = src_len) and write into the
        // pre-allocated ARC collection header.
        let data_size = LLVMBuildMul(
            b,
            src_len,
            LLVMConstInt(i64_ty, elem_size, 0),
            c"dist_data_size".as_ptr(),
        );
        let new_data = emit_malloc(b, module, i64_ty, ptr_ty, data_size, c"dist_data".as_ptr());

        let dist_data_field = LLVMBuildStructGEP2(
            b,
            list_header_ty,
            new_header,
            LIST_FIELD_DATA,
            c"dist_data_field".as_ptr(),
        );
        LLVMBuildStore(b, new_data, dist_data_field);
        let dist_cap_ptr = LLVMBuildStructGEP2(
            b,
            list_header_ty,
            new_header,
            LIST_FIELD_CAP,
            c"dist_cap_ptr".as_ptr(),
        );
        LLVMBuildStore(b, src_len, dist_cap_ptr);

        // Output length counter
        let out_len = LLVMBuildAlloca(b, i64_ty, c"dist_out_len".as_ptr());
        LLVMBuildStore(b, LLVMConstInt(i64_ty, 0, 0), out_len);

        // Outer loop: for each source element
        let i_var = LLVMBuildAlloca(b, i64_ty, c"dist_i".as_ptr());
        LLVMBuildStore(b, LLVMConstInt(i64_ty, 0, 0), i_var);

        let outer_cond_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"dist.ocond".as_ptr());
        let outer_body_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"dist.obody".as_ptr());
        let outer_end_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"dist.oend".as_ptr());

        LLVMBuildBr(b, outer_cond_bb);
        LLVMPositionBuilderAtEnd(b, outer_cond_bb);
        let i_val = LLVMBuildLoad2(b, i64_ty, i_var, c"dist_iv".as_ptr());
        let i_in_bounds = LLVMBuildICmp(
            b,
            LLVMIntPredicate::LLVMIntSLT,
            i_val,
            src_len,
            c"".as_ptr(),
        );
        LLVMBuildCondBr(b, i_in_bounds, outer_body_bb, outer_end_bb);

        LLVMPositionBuilderAtEnd(b, outer_body_bb);
        let i_cur = LLVMBuildLoad2(b, i64_ty, i_var, c"dist_ic".as_ptr());
        let mut src_idx = [i_cur];
        let src_elem_ptr = LLVMBuildGEP2(
            b,
            elem_ty,
            src_data,
            src_idx.as_mut_ptr(),
            1,
            c"dist_src_ep".as_ptr(),
        );
        let src_elem = LLVMBuildLoad2(b, elem_ty, src_elem_ptr, c"dist_src_elem".as_ptr());

        // Inner loop: check if src_elem already exists in output
        let dup_found = LLVMBuildAlloca(b, i1_ty, c"dist_dup".as_ptr());
        LLVMBuildStore(b, LLVMConstInt(i1_ty, 0, 0), dup_found);
        let j_var = LLVMBuildAlloca(b, i64_ty, c"dist_j".as_ptr());
        LLVMBuildStore(b, LLVMConstInt(i64_ty, 0, 0), j_var);

        let inner_cond_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"dist.icond".as_ptr());
        let inner_body_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"dist.ibody".as_ptr());
        let inner_end_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"dist.iend".as_ptr());

        let cur_out_len = LLVMBuildLoad2(b, i64_ty, out_len, c"dist_cur_out".as_ptr());
        LLVMBuildBr(b, inner_cond_bb);

        LLVMPositionBuilderAtEnd(b, inner_cond_bb);
        let j_val = LLVMBuildLoad2(b, i64_ty, j_var, c"dist_jv".as_ptr());
        let dup_loaded = LLVMBuildLoad2(b, i1_ty, dup_found, c"".as_ptr());
        let not_dup = LLVMBuildICmp(
            b,
            LLVMIntPredicate::LLVMIntEQ,
            dup_loaded,
            LLVMConstInt(i1_ty, 0, 0),
            c"dist_not_dup".as_ptr(),
        );
        let j_in_bounds = LLVMBuildICmp(
            b,
            LLVMIntPredicate::LLVMIntSLT,
            j_val,
            cur_out_len,
            c"dist_j_inb".as_ptr(),
        );
        let inner_cont = LLVMBuildAnd(b, not_dup, j_in_bounds, c"dist_icont".as_ptr());
        LLVMBuildCondBr(b, inner_cont, inner_body_bb, inner_end_bb);

        LLVMPositionBuilderAtEnd(b, inner_body_bb);
        let j_cur = LLVMBuildLoad2(b, i64_ty, j_var, c"dist_jc".as_ptr());
        let mut out_idx = [j_cur];
        let out_elem_ptr = LLVMBuildGEP2(
            b,
            elem_ty,
            new_data,
            out_idx.as_mut_ptr(),
            1,
            c"dist_out_ep".as_ptr(),
        );
        let out_elem = LLVMBuildLoad2(b, elem_ty, out_elem_ptr, c"dist_out_elem".as_ptr());

        let matched = if elem_is_str {
            let mut strcmp_p = [ptr_ty, ptr_ty];
            let strcmp_ty = LLVMFunctionType(i32_ty, strcmp_p.as_mut_ptr(), 2, 0);
            let strcmp_fn = get_or_insert_function(module, c"strcmp".as_ptr(), strcmp_ty);
            let mut strcmp_a = [src_elem, out_elem];
            let cmp = LLVMBuildCall2(
                b,
                strcmp_ty,
                strcmp_fn,
                strcmp_a.as_mut_ptr(),
                2,
                c"dist_strcmp".as_ptr(),
            );
            LLVMBuildICmp(
                b,
                LLVMIntPredicate::LLVMIntEQ,
                cmp,
                LLVMConstInt(i32_ty, 0, 0),
                c"dist_match".as_ptr(),
            )
        } else if elem_is_double {
            LLVMBuildFCmp(
                b,
                LLVMRealPredicate::LLVMRealOEQ,
                src_elem,
                out_elem,
                c"dist_match".as_ptr(),
            )
        } else {
            LLVMBuildICmp(
                b,
                LLVMIntPredicate::LLVMIntEQ,
                src_elem,
                out_elem,
                c"dist_match".as_ptr(),
            )
        };

        let dup_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"dist.dup".as_ptr());
        let inner_next_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"dist.inext".as_ptr());
        LLVMBuildCondBr(b, matched, dup_bb, inner_next_bb);

        LLVMPositionBuilderAtEnd(b, dup_bb);
        LLVMBuildStore(b, LLVMConstInt(i1_ty, 1, 0), dup_found);
        LLVMBuildBr(b, inner_cond_bb);

        LLVMPositionBuilderAtEnd(b, inner_next_bb);
        let j_next = LLVMBuildAdd(b, j_cur, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
        LLVMBuildStore(b, j_next, j_var);
        LLVMBuildBr(b, inner_cond_bb);

        // After inner loop: if not duplicate, add to output
        LLVMPositionBuilderAtEnd(b, inner_end_bb);
        let is_dup = LLVMBuildLoad2(b, i1_ty, dup_found, c"dist_is_dup".as_ptr());

        let add_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"dist.add".as_ptr());
        let outer_next_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"dist.onext".as_ptr());
        LLVMBuildCondBr(b, is_dup, outer_next_bb, add_bb);

        LLVMPositionBuilderAtEnd(b, add_bb);
        let out_idx_val = LLVMBuildLoad2(b, i64_ty, out_len, c"dist_out_idx".as_ptr());
        let mut dst_idx = [out_idx_val];
        let dst_ptr = LLVMBuildGEP2(
            b,
            elem_ty,
            new_data,
            dst_idx.as_mut_ptr(),
            1,
            c"dist_dst".as_ptr(),
        );
        LLVMBuildStore(b, src_elem, dst_ptr);
        let out_idx_next = LLVMBuildAdd(b, out_idx_val, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
        LLVMBuildStore(b, out_idx_next, out_len);
        LLVMBuildBr(b, outer_next_bb);

        LLVMPositionBuilderAtEnd(b, outer_next_bb);
        let i_next = LLVMBuildAdd(b, i_cur, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
        LLVMBuildStore(b, i_next, i_var);
        LLVMBuildBr(b, outer_cond_bb);

        // End: set final length on the new header.
        LLVMPositionBuilderAtEnd(b, outer_end_bb);
        let final_len = LLVMBuildLoad2(b, i64_ty, out_len, c"dist_final_len".as_ptr());
        let dist_len_ptr = LLVMBuildStructGEP2(
            b,
            list_header_ty,
            new_header,
            LIST_FIELD_LEN,
            c"dist_len_ptr".as_ptr(),
        );
        LLVMBuildStore(b, final_len, dist_len_ptr);
    }

    /// Two-pass `flat(list<list<T>>)` (#2095). The caller (C++) pre-loads the
    /// outer list header (`outer_len` / `outer_data`) so the C++ baseline's
    /// `loadListHeader` order is preserved byte-for-byte. This engine method:
    /// 1. Pass 1: alloca `flat_total`; loop GEPing inner.len (LIST_FIELD_LEN)
    ///    and summing into `flat_total`.
    /// 2. ARC alloc: emit the C++ `emitArcAllocCollectionHeader` shape inline
    ///    — `arc_hdr` malloc, counter +1, strong=1, weak=0, GEP +ARC_HEADER_SIZE
    ///    named `arc_data` to get the new collection header pointer.
    /// 3. malloc(total * inner_elem_size) named `flat_data`; storeListHeaderFields
    ///    writes total / total / new_data into the new header at LIST_FIELD_*.
    /// 4. Pass 2: loop GEPing inner.data (LIST_FIELD_DATA) and memcpy each
    ///    inner buffer into new_data at the running offset.
    ///
    /// Returns the new collection header pointer; caller registers it in
    /// `arc_owned_values_` to match the original `emitArcGetDataPtr` bookkeeping.
    pub(crate) unsafe fn list_flatten(
        &mut self,
        outer_len: ValueRef,
        outer_data: ValueRef,
        list_header_ty: TypeRef,
        arc_header_ty: TypeRef,
        inner_elem_ty: TypeRef,
        inner_elem_size: u64,
    ) -> ValueRef {
        let context = self.context;
        let b = self.builder;
        let module = self.module;
        let outer_len = outer_len.0;
        let outer_data = outer_data.0;
        let list_header_ty = list_header_ty.0;
        // arc_header_ty is the C++-side named %ArcHeader struct (vs the
        // anonymous {i64,i64} from core::arc_header_type); passed across the
        // boundary so the migrated GEPs reference the same named type as the
        // C++ baseline byte-for-byte.
        let arc_header_ty = arc_header_ty.0;
        let inner_elem_ty = inner_elem_ty.0;
        let i64_ty = i64_type(context);
        let i8_ty = LLVMInt8TypeInContext(context);
        let ptr_ty = ptr_type(context);
        let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

        // ===== Pass 1: sum inner lengths =====
        let total_len = LLVMBuildAlloca(b, i64_ty, c"flat_total".as_ptr());
        LLVMBuildStore(b, LLVMConstInt(i64_ty, 0, 0), total_len);
        {
            let i_var = LLVMBuildAlloca(b, i64_ty, c"flat_s_i".as_ptr());
            LLVMBuildStore(b, LLVMConstInt(i64_ty, 0, 0), i_var);
            let cond_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"flat.s.cond".as_ptr());
            let body_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"flat.s.body".as_ptr());
            let end_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"flat.s.end".as_ptr());
            LLVMBuildBr(b, cond_bb);
            LLVMPositionBuilderAtEnd(b, cond_bb);
            let i = LLVMBuildLoad2(b, i64_ty, i_var, c"flat_si".as_ptr());
            let in_bounds =
                LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLT, i, outer_len, c"".as_ptr());
            LLVMBuildCondBr(b, in_bounds, body_bb, end_bb);
            LLVMPositionBuilderAtEnd(b, body_bb);
            let mut idx_a = [i];
            let inner_ptr = LLVMBuildGEP2(
                b,
                ptr_ty,
                outer_data,
                idx_a.as_mut_ptr(),
                1,
                c"flat_inner_ptr".as_ptr(),
            );
            let inner_list = LLVMBuildLoad2(b, ptr_ty, inner_ptr, c"flat_inner".as_ptr());
            let inner_len_ptr = LLVMBuildStructGEP2(
                b,
                list_header_ty,
                inner_list,
                LIST_FIELD_LEN,
                c"flat_ilen_ptr".as_ptr(),
            );
            let inner_len = LLVMBuildLoad2(b, i64_ty, inner_len_ptr, c"flat_ilen".as_ptr());
            let cur_total = LLVMBuildLoad2(b, i64_ty, total_len, c"flat_cur_total".as_ptr());
            let added = LLVMBuildAdd(b, cur_total, inner_len, c"".as_ptr());
            LLVMBuildStore(b, added, total_len);
            let i_next = LLVMBuildAdd(b, i, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
            LLVMBuildStore(b, i_next, i_var);
            LLVMBuildBr(b, cond_bb);
            LLVMPositionBuilderAtEnd(b, end_bb);
        }

        // ===== ARC alloc (mirror C++ emitArcAllocCollectionHeader byte-for-byte) =====
        let total = LLVMBuildLoad2(b, i64_ty, total_len, c"flat_total_len".as_ptr());
        // header_size = sizeOf(listHeaderTy_) = 24 bytes (i64 + i64 + ptr); the
        // C++ helper hardcodes it via DataLayout, and the CreateAdd of two
        // constants folds — only the `arc_hdr` malloc(40) remains in the IR.
        let header_size_bytes = {
            let dl = LLVMGetModuleDataLayout(module);
            LLVMABISizeOfType(dl, list_header_ty)
        };
        let alloc_size = LLVMConstInt(i64_ty, ARC_HEADER_SIZE + header_size_bytes, 0);
        let arc_hdr = emit_malloc(b, module, i64_ty, ptr_ty, alloc_size, c"arc_hdr".as_ptr());
        emit_arc_counter_delta(b, i64_ty, ptr_ty, 1);
        let strong_ptr =
            LLVMBuildStructGEP2(b, arc_header_ty, arc_hdr, 0, c"arc_strong_ptr".as_ptr());
        LLVMBuildStore(b, LLVMConstInt(i64_ty, 1, 0), strong_ptr);
        let weak_ptr = LLVMBuildStructGEP2(b, arc_header_ty, arc_hdr, 1, c"arc_weak_ptr".as_ptr());
        LLVMBuildStore(b, LLVMConstInt(i64_ty, 0, 0), weak_ptr);
        let mut arc_data_gep = [LLVMConstInt(i64_ty, ARC_HEADER_SIZE, 0)];
        let new_header = LLVMBuildGEP2(
            b,
            i8_ty,
            arc_hdr,
            arc_data_gep.as_mut_ptr(),
            1,
            c"arc_data".as_ptr(),
        );

        // ===== Allocate the new data buffer; storeListHeaderFields =====
        let data_size = LLVMBuildMul(
            b,
            total,
            LLVMConstInt(i64_ty, inner_elem_size, 0),
            c"flat_ds".as_ptr(),
        );
        let new_data = emit_malloc(b, module, i64_ty, ptr_ty, data_size, c"flat_data".as_ptr());

        // Mirror storeListHeaderFields (src/codegen_call.cpp): three unnamed
        // StructGEPs + stores at LIST_FIELD_LEN / CAP / DATA. Names are empty
        // to match the C++ helper.
        let new_len_p =
            LLVMBuildStructGEP2(b, list_header_ty, new_header, LIST_FIELD_LEN, c"".as_ptr());
        LLVMBuildStore(b, total, new_len_p);
        let new_cap_p =
            LLVMBuildStructGEP2(b, list_header_ty, new_header, LIST_FIELD_CAP, c"".as_ptr());
        LLVMBuildStore(b, total, new_cap_p);
        let new_data_p =
            LLVMBuildStructGEP2(b, list_header_ty, new_header, LIST_FIELD_DATA, c"".as_ptr());
        LLVMBuildStore(b, new_data, new_data_p);

        // ===== Pass 2: memcpy each inner list's data =====
        let offset = LLVMBuildAlloca(b, i64_ty, c"flat_off".as_ptr());
        LLVMBuildStore(b, LLVMConstInt(i64_ty, 0, 0), offset);
        {
            let i_var = LLVMBuildAlloca(b, i64_ty, c"flat_c_i".as_ptr());
            LLVMBuildStore(b, LLVMConstInt(i64_ty, 0, 0), i_var);
            let cond_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"flat.c.cond".as_ptr());
            let body_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"flat.c.body".as_ptr());
            let end_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"flat.c.end".as_ptr());
            LLVMBuildBr(b, cond_bb);
            LLVMPositionBuilderAtEnd(b, cond_bb);
            let i = LLVMBuildLoad2(b, i64_ty, i_var, c"flat_ci".as_ptr());
            let in_bounds =
                LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLT, i, outer_len, c"".as_ptr());
            LLVMBuildCondBr(b, in_bounds, body_bb, end_bb);
            LLVMPositionBuilderAtEnd(b, body_bb);
            let mut idx_a = [i];
            let inner_ptr = LLVMBuildGEP2(
                b,
                ptr_ty,
                outer_data,
                idx_a.as_mut_ptr(),
                1,
                c"flat_c_inner_ptr".as_ptr(),
            );
            let inner_list = LLVMBuildLoad2(b, ptr_ty, inner_ptr, c"flat_c_inner".as_ptr());
            let inner_len_ptr = LLVMBuildStructGEP2(
                b,
                list_header_ty,
                inner_list,
                LIST_FIELD_LEN,
                c"flat_c_ilen_ptr".as_ptr(),
            );
            let inner_len = LLVMBuildLoad2(b, i64_ty, inner_len_ptr, c"flat_c_ilen".as_ptr());
            let inner_data_field = LLVMBuildStructGEP2(
                b,
                list_header_ty,
                inner_list,
                LIST_FIELD_DATA,
                c"flat_c_idata_field".as_ptr(),
            );
            let inner_data = LLVMBuildLoad2(b, ptr_ty, inner_data_field, c"flat_c_idata".as_ptr());
            let cur_off = LLVMBuildLoad2(b, i64_ty, offset, c"flat_cur_off".as_ptr());
            let mut dst_idx = [cur_off];
            let dst_ptr = LLVMBuildGEP2(
                b,
                inner_elem_ty,
                new_data,
                dst_idx.as_mut_ptr(),
                1,
                c"flat_dst".as_ptr(),
            );
            let copy_bytes = LLVMBuildMul(
                b,
                inner_len,
                LLVMConstInt(i64_ty, inner_elem_size, 0),
                c"flat_cb".as_ptr(),
            );
            emit_memcpy(b, module, ptr_ty, i64_ty, dst_ptr, inner_data, copy_bytes);
            let off_next = LLVMBuildAdd(b, cur_off, inner_len, c"".as_ptr());
            LLVMBuildStore(b, off_next, offset);
            let i_next = LLVMBuildAdd(b, i, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
            LLVMBuildStore(b, i_next, i_var);
            LLVMBuildBr(b, cond_bb);
            LLVMPositionBuilderAtEnd(b, end_bb);
        }

        ValueRef(new_header)
    }
}

/// `enumerate(list)` → `List<(int, T)>` (#2095). Re-entrant: the optional
/// `retain` closure is invoked inside the loop body once per element after the
/// element load and before tuple construction, to emit `emitTupleComponentRetain`
/// IR via the C++ emitter. Because of that re-entrancy this lives as a `core`
/// free function (not `&mut self`) per the #2069 rule: a re-entrant callback
/// can re-enter `cx(ctx)` to form a fresh `&mut EmitCtx`, which would alias the
/// receiver borrow of an `&mut self` method.
///
/// The caller (C++) pre-loads the source list header (loadListHeader order
/// preserved) and pre-allocates the ARC collection header. This function:
///   1. malloc(src_len * tuple_size) named `enum_nd`, dataSize `enum_ds`.
///   2. Loop (enum.cond/body/end): GEP elem (LIST_FIELD-style positional),
///      load elem, optional retain, build tuple via UndefValue + InsertValue
///      (field 0 = index, field 1 = elem), GEP dst, store.
///   3. storeListHeaderFields(new_header, src_len, src_len, new_data) using
///      LIST_FIELD_LEN / CAP / DATA.
#[allow(clippy::too_many_arguments)]
pub(crate) unsafe fn emit_list_enumerate(
    b: LLVMBuilderRef,
    context: LLVMContextRef,
    module: LLVMModuleRef,
    src_len: ValueRef,
    src_data: ValueRef,
    new_header: ValueRef,
    list_header_ty: LLVMTypeRef,
    elem_ty: LLVMTypeRef,
    tuple_ty: LLVMTypeRef,
    tuple_size: u64,
    retain: Option<&mut dyn FnMut(ValueRef)>,
) {
    let src_len = src_len.0;
    let src_data = src_data.0;
    let new_header = new_header.0;
    let i64_ty = i64_type(context);
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

    let data_size = LLVMBuildMul(
        b,
        src_len,
        LLVMConstInt(i64_ty, tuple_size, 0),
        c"enum_ds".as_ptr(),
    );
    let new_data = emit_malloc(
        b,
        module,
        i64_ty,
        ptr_type(context),
        data_size,
        c"enum_nd".as_ptr(),
    );

    let i_var = LLVMBuildAlloca(b, i64_ty, c"enum_i".as_ptr());
    LLVMBuildStore(b, LLVMConstInt(i64_ty, 0, 0), i_var);
    let cond_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"enum.cond".as_ptr());
    let body_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"enum.body".as_ptr());
    let end_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"enum.end".as_ptr());
    LLVMBuildBr(b, cond_bb);
    LLVMPositionBuilderAtEnd(b, cond_bb);
    let i = LLVMBuildLoad2(b, i64_ty, i_var, c"ei".as_ptr());
    let in_bounds = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLT, i, src_len, c"".as_ptr());
    LLVMBuildCondBr(b, in_bounds, body_bb, end_bb);
    LLVMPositionBuilderAtEnd(b, body_bb);
    let mut idx_a = [i];
    let elem_ptr = LLVMBuildGEP2(
        b,
        elem_ty,
        src_data,
        idx_a.as_mut_ptr(),
        1,
        c"enum_ep".as_ptr(),
    );
    let elem = LLVMBuildLoad2(b, elem_ty, elem_ptr, c"enum_elem".as_ptr());

    // Re-entrant retain callback — runs OUTSIDE any `&mut EmitCtx` borrow
    // because this free function holds none (#2069 / #2081). The closure body
    // is responsible for the with_ctx-intern-then-callback two-step.
    if let Some(retain) = retain {
        retain(ValueRef(elem));
    }

    let undef_tuple = LLVMGetUndef(tuple_ty);
    let with_idx = LLVMBuildInsertValue(b, undef_tuple, i, 0, c"".as_ptr());
    let tuple_val = LLVMBuildInsertValue(b, with_idx, elem, 1, c"".as_ptr());
    let mut dst_idx_a = [i];
    let dst_ptr = LLVMBuildGEP2(
        b,
        tuple_ty,
        new_data,
        dst_idx_a.as_mut_ptr(),
        1,
        c"enum_dp".as_ptr(),
    );
    LLVMBuildStore(b, tuple_val, dst_ptr);
    let i_next = LLVMBuildAdd(b, i, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
    LLVMBuildStore(b, i_next, i_var);
    LLVMBuildBr(b, cond_bb);
    LLVMPositionBuilderAtEnd(b, end_bb);

    // storeListHeaderFields: 3 unnamed StructGEPs + stores at LIST_FIELD_LEN /
    // CAP / DATA (matches src/codegen_call.cpp `storeListHeaderFields`).
    let new_len_p =
        LLVMBuildStructGEP2(b, list_header_ty, new_header, LIST_FIELD_LEN, c"".as_ptr());
    LLVMBuildStore(b, src_len, new_len_p);
    let new_cap_p =
        LLVMBuildStructGEP2(b, list_header_ty, new_header, LIST_FIELD_CAP, c"".as_ptr());
    LLVMBuildStore(b, src_len, new_cap_p);
    let new_data_p =
        LLVMBuildStructGEP2(b, list_header_ty, new_header, LIST_FIELD_DATA, c"".as_ptr());
    LLVMBuildStore(b, new_data, new_data_p);
}

/// `reverse(list)` → new reversed `List<T>` (#2095). Caller (C++) pre-loads
/// the source list header (loadListHeader order), pre-allocates the ARC
/// collection header, computes `rev_dsize` + `rev_data` (the new buffer), and
/// passes len + src_data + new_data + new_header to the engine. The engine
/// emits the reverse loop body + named StructGEP stores `rev_new_len` /
/// `rev_new_cap` / `rev_new_data` at LIST_FIELD_LEN/CAP/DATA. The post-loop
/// ARC retain dispatch (`emitTupleElemRetainLoop` / `emitCowRetainArcElements`)
/// stays C++-side because it operates on ValueMetadata and the
/// `arc_owned_values_` / `arc_str_owned_values_` sets the boundary cannot reach.
#[allow(clippy::too_many_arguments)]
pub(crate) unsafe fn emit_list_reverse(
    b: LLVMBuilderRef,
    context: LLVMContextRef,
    len: ValueRef,
    src_data: ValueRef,
    new_data: ValueRef,
    new_header: ValueRef,
    list_header_ty: LLVMTypeRef,
    elem_ty: LLVMTypeRef,
) {
    let len = len.0;
    let src_data = src_data.0;
    let new_data = new_data.0;
    let new_header = new_header.0;
    let i64_ty = i64_type(context);
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

    let i_var = LLVMBuildAlloca(b, i64_ty, c"rev_i".as_ptr());
    LLVMBuildStore(b, LLVMConstInt(i64_ty, 0, 0), i_var);
    let cond_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"lrev.cond".as_ptr());
    let body_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"lrev.body".as_ptr());
    let end_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"lrev.end".as_ptr());
    LLVMBuildBr(b, cond_bb);
    LLVMPositionBuilderAtEnd(b, cond_bb);
    let i = LLVMBuildLoad2(b, i64_ty, i_var, c"rev_idx".as_ptr());
    let in_bounds = LLVMBuildICmp(
        b,
        LLVMIntPredicate::LLVMIntSLT,
        i,
        len,
        c"rev_cond".as_ptr(),
    );
    LLVMBuildCondBr(b, in_bounds, body_bb, end_bb);
    LLVMPositionBuilderAtEnd(b, body_bb);
    let i_cur = LLVMBuildLoad2(b, i64_ty, i_var, c"rev_i_cur".as_ptr());
    let len_minus_1 = LLVMBuildSub(b, len, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
    let src_idx = LLVMBuildSub(b, len_minus_1, i_cur, c"rev_src_idx".as_ptr());
    let mut src_idx_a = [src_idx];
    let src_ptr = LLVMBuildGEP2(
        b,
        elem_ty,
        src_data,
        src_idx_a.as_mut_ptr(),
        1,
        c"rev_src".as_ptr(),
    );
    let val = LLVMBuildLoad2(b, elem_ty, src_ptr, c"rev_val".as_ptr());
    let mut dst_idx_a = [i_cur];
    let dst_ptr = LLVMBuildGEP2(
        b,
        elem_ty,
        new_data,
        dst_idx_a.as_mut_ptr(),
        1,
        c"rev_dst".as_ptr(),
    );
    LLVMBuildStore(b, val, dst_ptr);
    let i_next = LLVMBuildAdd(b, i_cur, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
    LLVMBuildStore(b, i_next, i_var);
    LLVMBuildBr(b, cond_bb);
    LLVMPositionBuilderAtEnd(b, end_bb);

    // Named StructGEPs (rev_new_len/cap/data) via LIST_FIELD_* constants.
    let new_len_ptr = LLVMBuildStructGEP2(
        b,
        list_header_ty,
        new_header,
        LIST_FIELD_LEN,
        c"rev_new_len".as_ptr(),
    );
    LLVMBuildStore(b, len, new_len_ptr);
    let new_cap_ptr = LLVMBuildStructGEP2(
        b,
        list_header_ty,
        new_header,
        LIST_FIELD_CAP,
        c"rev_new_cap".as_ptr(),
    );
    LLVMBuildStore(b, len, new_cap_ptr);
    let new_data_field = LLVMBuildStructGEP2(
        b,
        list_header_ty,
        new_header,
        LIST_FIELD_DATA,
        c"rev_new_data".as_ptr(),
    );
    LLVMBuildStore(b, new_data, new_data_field);
}

/// `items(map)` → `List<(K, V)>` (#2095). Caller pre-loads the map header
/// (`mf.len`, `mf.keys`, `mf.vals`) and pre-allocates the ARC collection
/// header. The engine emits the per-iteration tuple build (key then val) with
/// 2 retain callbacks, plus storeListHeaderFields. The `items_data` malloc
/// uses the name `items_data` (NOT `items_nd`).
#[allow(clippy::too_many_arguments)]
pub(crate) unsafe fn emit_map_items(
    b: LLVMBuilderRef,
    context: LLVMContextRef,
    module: LLVMModuleRef,
    map_len: ValueRef,
    map_keys: ValueRef,
    map_vals: ValueRef,
    new_header: ValueRef,
    list_header_ty: LLVMTypeRef,
    key_ty: LLVMTypeRef,
    val_ty: LLVMTypeRef,
    tuple_ty: LLVMTypeRef,
    tuple_size: u64,
    retain_key: Option<&mut dyn FnMut(ValueRef)>,
    retain_val: Option<&mut dyn FnMut(ValueRef)>,
) {
    let map_len = map_len.0;
    let map_keys = map_keys.0;
    let map_vals = map_vals.0;
    let new_header = new_header.0;
    let i64_ty = i64_type(context);
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

    let data_size = LLVMBuildMul(
        b,
        map_len,
        LLVMConstInt(i64_ty, tuple_size, 0),
        c"items_ds".as_ptr(),
    );
    let new_data = emit_malloc(
        b,
        module,
        i64_ty,
        ptr_type(context),
        data_size,
        c"items_data".as_ptr(),
    );

    let i_var = LLVMBuildAlloca(b, i64_ty, c"items_i".as_ptr());
    LLVMBuildStore(b, LLVMConstInt(i64_ty, 0, 0), i_var);
    let cond_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"items.cond".as_ptr());
    let body_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"items.body".as_ptr());
    let end_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"items.end".as_ptr());
    LLVMBuildBr(b, cond_bb);
    LLVMPositionBuilderAtEnd(b, cond_bb);
    let i = LLVMBuildLoad2(b, i64_ty, i_var, c"items_ci".as_ptr());
    let in_bounds = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLT, i, map_len, c"".as_ptr());
    LLVMBuildCondBr(b, in_bounds, body_bb, end_bb);
    LLVMPositionBuilderAtEnd(b, body_bb);
    let mut k_idx = [i];
    let kp = LLVMBuildGEP2(
        b,
        key_ty,
        map_keys,
        k_idx.as_mut_ptr(),
        1,
        c"items_kp".as_ptr(),
    );
    let mut v_idx = [i];
    let vp = LLVMBuildGEP2(
        b,
        val_ty,
        map_vals,
        v_idx.as_mut_ptr(),
        1,
        c"items_vp".as_ptr(),
    );
    let k = LLVMBuildLoad2(b, key_ty, kp, c"items_k".as_ptr());
    let v = LLVMBuildLoad2(b, val_ty, vp, c"items_v".as_ptr());

    if let Some(retain_key) = retain_key {
        retain_key(ValueRef(k));
    }
    if let Some(retain_val) = retain_val {
        retain_val(ValueRef(v));
    }

    let undef_tuple = LLVMGetUndef(tuple_ty);
    let with_k = LLVMBuildInsertValue(b, undef_tuple, k, 0, c"".as_ptr());
    let tuple_val = LLVMBuildInsertValue(b, with_k, v, 1, c"".as_ptr());
    let mut dst_idx = [i];
    let dst_ptr = LLVMBuildGEP2(
        b,
        tuple_ty,
        new_data,
        dst_idx.as_mut_ptr(),
        1,
        c"items_dp".as_ptr(),
    );
    LLVMBuildStore(b, tuple_val, dst_ptr);
    let i_next = LLVMBuildAdd(b, i, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
    LLVMBuildStore(b, i_next, i_var);
    LLVMBuildBr(b, cond_bb);
    LLVMPositionBuilderAtEnd(b, end_bb);

    let new_len_p =
        LLVMBuildStructGEP2(b, list_header_ty, new_header, LIST_FIELD_LEN, c"".as_ptr());
    LLVMBuildStore(b, map_len, new_len_p);
    let new_cap_p =
        LLVMBuildStructGEP2(b, list_header_ty, new_header, LIST_FIELD_CAP, c"".as_ptr());
    LLVMBuildStore(b, map_len, new_cap_p);
    let new_data_p =
        LLVMBuildStructGEP2(b, list_header_ty, new_header, LIST_FIELD_DATA, c"".as_ptr());
    LLVMBuildStore(b, new_data, new_data_p);
}

/// `zip(list1, list2)` → `List<(T1, T2)>` (#2095). Same structural shape as
/// `emit_list_enumerate` but with two parallel inputs, a `min(len1, len2)`
/// clamp via Select, and two retain callbacks for the two tuple components.
/// Lives as a `core` free function for the same re-entrancy reason (#2069).
#[allow(clippy::too_many_arguments)]
pub(crate) unsafe fn emit_list_zip(
    b: LLVMBuilderRef,
    context: LLVMContextRef,
    module: LLVMModuleRef,
    min_len: ValueRef,
    data1: ValueRef,
    data2: ValueRef,
    new_header: ValueRef,
    list_header_ty: LLVMTypeRef,
    elem_ty1: LLVMTypeRef,
    elem_ty2: LLVMTypeRef,
    tuple_ty: LLVMTypeRef,
    tuple_size: u64,
    retain1: Option<&mut dyn FnMut(ValueRef)>,
    retain2: Option<&mut dyn FnMut(ValueRef)>,
) {
    // min_len is pre-computed on the C++ side BEFORE the ARC alloc so the
    // baseline's "zip_minlen → ARC alloc" order survives the migration.
    let min_len = min_len.0;
    let data1 = data1.0;
    let data2 = data2.0;
    let new_header = new_header.0;
    let i64_ty = i64_type(context);
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

    let data_size = LLVMBuildMul(
        b,
        min_len,
        LLVMConstInt(i64_ty, tuple_size, 0),
        c"zip_ds".as_ptr(),
    );
    let new_data = emit_malloc(
        b,
        module,
        i64_ty,
        ptr_type(context),
        data_size,
        c"zip_nd".as_ptr(),
    );

    let i_var = LLVMBuildAlloca(b, i64_ty, c"zip_i".as_ptr());
    LLVMBuildStore(b, LLVMConstInt(i64_ty, 0, 0), i_var);
    let cond_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"zip.cond".as_ptr());
    let body_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"zip.body".as_ptr());
    let end_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"zip.end".as_ptr());
    LLVMBuildBr(b, cond_bb);
    LLVMPositionBuilderAtEnd(b, cond_bb);
    let i = LLVMBuildLoad2(b, i64_ty, i_var, c"zi".as_ptr());
    let in_bounds = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLT, i, min_len, c"".as_ptr());
    LLVMBuildCondBr(b, in_bounds, body_bb, end_bb);
    LLVMPositionBuilderAtEnd(b, body_bb);
    let mut idx_a = [i];
    let ep1 = LLVMBuildGEP2(
        b,
        elem_ty1,
        data1,
        idx_a.as_mut_ptr(),
        1,
        c"zip_ep1".as_ptr(),
    );
    let mut idx_a2 = [i];
    let ep2 = LLVMBuildGEP2(
        b,
        elem_ty2,
        data2,
        idx_a2.as_mut_ptr(),
        1,
        c"zip_ep2".as_ptr(),
    );
    let e1 = LLVMBuildLoad2(b, elem_ty1, ep1, c"zip_e1".as_ptr());
    let e2 = LLVMBuildLoad2(b, elem_ty2, ep2, c"zip_e2".as_ptr());

    if let Some(retain1) = retain1 {
        retain1(ValueRef(e1));
    }
    if let Some(retain2) = retain2 {
        retain2(ValueRef(e2));
    }

    let undef_tuple = LLVMGetUndef(tuple_ty);
    let with_e1 = LLVMBuildInsertValue(b, undef_tuple, e1, 0, c"".as_ptr());
    let tuple_val = LLVMBuildInsertValue(b, with_e1, e2, 1, c"".as_ptr());
    let mut dst_idx = [i];
    let dst_ptr = LLVMBuildGEP2(
        b,
        tuple_ty,
        new_data,
        dst_idx.as_mut_ptr(),
        1,
        c"zip_dp".as_ptr(),
    );
    LLVMBuildStore(b, tuple_val, dst_ptr);
    let i_next = LLVMBuildAdd(b, i, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
    LLVMBuildStore(b, i_next, i_var);
    LLVMBuildBr(b, cond_bb);
    LLVMPositionBuilderAtEnd(b, end_bb);

    // storeListHeaderFields.
    let new_len_p =
        LLVMBuildStructGEP2(b, list_header_ty, new_header, LIST_FIELD_LEN, c"".as_ptr());
    LLVMBuildStore(b, min_len, new_len_p);
    let new_cap_p =
        LLVMBuildStructGEP2(b, list_header_ty, new_header, LIST_FIELD_CAP, c"".as_ptr());
    LLVMBuildStore(b, min_len, new_cap_p);
    let new_data_p =
        LLVMBuildStructGEP2(b, list_header_ty, new_header, LIST_FIELD_DATA, c"".as_ptr());
    LLVMBuildStore(b, new_data, new_data_p);
}
