//! Numeric reduce IR generation (core-role: an `impl EmitCtx` over the core
//! engine only, so it is abi-independent and the `core⇏abi` invariant covers
//! this module). Implements the `sum` / `min` / `max` builtins (#2092) — the
//! ARC-independent "safest batch": pure numeric primitives, no `Function::Create`
//! / `FnScope`, no ARC helper, no string global.
//!
//! Two op shapes, dictated by byte-exact IR reproduction:
//!   - **list forms** (`sum([..])`, `min/max([..])`): the whole loop lives in one
//!     coarse boundary op. `reduce_sum_list` loads the list header inline (in the
//!     INTERLEAVED gep/load order of C++ `CodeGen::loadListHeader`, NOT the
//!     grouped `core::load_list_header`) and runs the accumulate loop.
//!     `reduce_minmax_list_loop` is deliberately partial: the C++ side keeps
//!     `loadListHeader` + the empty-list check + `emitRuntimeError` (which builds
//!     an ARC string global — out of scope for this ARC-free batch), positions
//!     the builder at `mm.ok`, and this method emits only the seed + loop.
//!   - **variadic forms** (`sum(a,b,..)`, `min/max(a,b,..)`): per-step ops the
//!     C++ fold loop calls once per argument. A coarse "evaluate all args then
//!     fold" op would bunch the operand loads ahead of the arithmetic and break
//!     byte-exactness; per-step calls keep each load interleaved with its add /
//!     compare-select exactly as the inline C++ emitted it.
//!
//! Type matrix: `sum` accepts i64 / f64 / i8 (the element LLVM type carries the
//! width; only float-vs-int selects FAdd vs Add). `min`/`max` accept i64 / f64
//! only. The builder-derived parent rule (#1996/#2083) holds: the list methods
//! create BBs, the per-step methods none.

use llvm_sys::core::*;
use llvm_sys::{LLVMIntPredicate, LLVMRealPredicate};

use crate::core::*;

impl EmitCtx {
    /// `sum([..])` — accumulate a numeric list. Emits the interleaved list-header
    /// load (prefix `sum`), a zero-seeded accumulator alloca, and the
    /// `sum.cond`/`sum.body`/`sum.end` loop; returns the loaded `sum_result`.
    pub(crate) unsafe fn reduce_sum_list(
        &mut self,
        list_ptr: ValueRef,
        elem_ty: TypeRef,
        list_header_ty: TypeRef,
    ) -> ValueRef {
        let b = self.builder;
        let context = self.context;
        let list_ptr = list_ptr.0;
        let elem_ty = elem_ty.0;
        let list_header_ty = list_header_ty.0;
        let i64_ty = i64_type(context);
        let ptr_ty = ptr_type(context);
        let f64_ty = LLVMDoubleTypeInContext(context);
        let is_float = elem_ty == f64_ty;
        let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

        // List header load — INTERLEAVED gep/load to match C++ CodeGen::loadListHeader
        // (src/codegen_call.cpp); core::load_list_header groups the geps then the
        // loads, which would reorder the IR. `sum_cap` is loaded-but-unused, exactly
        // as the C++ helper does, so the IR stays byte-identical.
        let len_ptr = LLVMBuildStructGEP2(b, list_header_ty, list_ptr, 0, c"sum_len_ptr".as_ptr());
        let src_len = LLVMBuildLoad2(b, i64_ty, len_ptr, c"sum_len".as_ptr());
        let cap_ptr = LLVMBuildStructGEP2(b, list_header_ty, list_ptr, 1, c"sum_cap_ptr".as_ptr());
        let _src_cap = LLVMBuildLoad2(b, i64_ty, cap_ptr, c"sum_cap".as_ptr());
        let data_ptr =
            LLVMBuildStructGEP2(b, list_header_ty, list_ptr, 2, c"sum_data_ptr".as_ptr());
        let src_data = LLVMBuildLoad2(b, ptr_ty, data_ptr, c"sum_data".as_ptr());

        let acc_var = LLVMBuildAlloca(b, elem_ty, c"sum_acc".as_ptr());
        let seed = if is_float {
            LLVMConstReal(f64_ty, 0.0)
        } else {
            LLVMConstInt(elem_ty, 0, 0)
        };
        LLVMBuildStore(b, seed, acc_var);
        let i_var = LLVMBuildAlloca(b, i64_ty, c"sum_i".as_ptr());
        LLVMBuildStore(b, LLVMConstInt(i64_ty, 0, 0), i_var);

        let cond_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"sum.cond".as_ptr());
        let body_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"sum.body".as_ptr());
        let end_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"sum.end".as_ptr());
        LLVMBuildBr(b, cond_bb);

        LLVMPositionBuilderAtEnd(b, cond_bb);
        let i = LLVMBuildLoad2(b, i64_ty, i_var, c"si".as_ptr());
        let cond = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLT, i, src_len, c"".as_ptr());
        LLVMBuildCondBr(b, cond, body_bb, end_bb);

        LLVMPositionBuilderAtEnd(b, body_bb);
        let mut idx = [i];
        let elem_ptr = LLVMBuildGEP2(
            b,
            elem_ty,
            src_data,
            idx.as_mut_ptr(),
            1,
            c"sum_ep".as_ptr(),
        );
        let elem = LLVMBuildLoad2(b, elem_ty, elem_ptr, c"sum_elem".as_ptr());
        let acc = LLVMBuildLoad2(b, elem_ty, acc_var, c"sum_acc_val".as_ptr());
        let new_acc = if is_float {
            LLVMBuildFAdd(b, acc, elem, c"sum_add".as_ptr())
        } else {
            LLVMBuildAdd(b, acc, elem, c"sum_add".as_ptr())
        };
        LLVMBuildStore(b, new_acc, acc_var);
        let i_next = LLVMBuildAdd(b, i, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
        LLVMBuildStore(b, i_next, i_var);
        LLVMBuildBr(b, cond_bb);

        LLVMPositionBuilderAtEnd(b, end_bb);
        ValueRef(LLVMBuildLoad2(b, elem_ty, acc_var, c"sum_result".as_ptr()))
    }

    /// One straight-line `sum` fold step: `acc + v` named `sum_v` (FAdd for f64,
    /// Add for int). The C++ variadic loop calls this once per argument after the
    /// seed, so operand loads stay interleaved with the adds.
    pub(crate) unsafe fn reduce_sum_step(
        &mut self,
        acc: ValueRef,
        v: ValueRef,
        elem_ty: TypeRef,
    ) -> ValueRef {
        let b = self.builder;
        let is_float = elem_ty.0 == LLVMDoubleTypeInContext(self.context);
        let r = if is_float {
            LLVMBuildFAdd(b, acc.0, v.0, c"sum_v".as_ptr())
        } else {
            LLVMBuildAdd(b, acc.0, v.0, c"sum_v".as_ptr())
        };
        ValueRef(r)
    }

    /// `min/max([..])` loop body, emitted at the `mm.ok` block the C++ side leaves
    /// the builder positioned at (after its `loadListHeader` + empty-list guard +
    /// `emitRuntimeError`). `data` / `len` are the C++-loaded `mm_data` / `mm_len`.
    /// Emits the `mm_first` seed, `mm_best`/`mm_i` allocas, and the
    /// `mm.cond`/`mm.body`/`mm.update`/`mm.next`/`mm.end` loop; returns `mm_result`.
    pub(crate) unsafe fn reduce_minmax_list_loop(
        &mut self,
        data: ValueRef,
        len: ValueRef,
        elem_ty: TypeRef,
        is_max: bool,
    ) -> ValueRef {
        let b = self.builder;
        let context = self.context;
        let src_data = data.0;
        let src_len = len.0;
        let elem_ty = elem_ty.0;
        let i64_ty = i64_type(context);
        let is_float = elem_ty == LLVMDoubleTypeInContext(context);
        let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

        let first = LLVMBuildLoad2(b, elem_ty, src_data, c"mm_first".as_ptr());
        let best_var = LLVMBuildAlloca(b, elem_ty, c"mm_best".as_ptr());
        LLVMBuildStore(b, first, best_var);
        let i_var = LLVMBuildAlloca(b, i64_ty, c"mm_i".as_ptr());
        LLVMBuildStore(b, LLVMConstInt(i64_ty, 1, 0), i_var);

        let cond_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"mm.cond".as_ptr());
        let body_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"mm.body".as_ptr());
        let update_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"mm.update".as_ptr());
        let next_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"mm.next".as_ptr());
        let end_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"mm.end".as_ptr());
        LLVMBuildBr(b, cond_bb);

        LLVMPositionBuilderAtEnd(b, cond_bb);
        let i = LLVMBuildLoad2(b, i64_ty, i_var, c"mi".as_ptr());
        let cond = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLT, i, src_len, c"".as_ptr());
        LLVMBuildCondBr(b, cond, body_bb, end_bb);

        LLVMPositionBuilderAtEnd(b, body_bb);
        let mut idx = [i];
        let elem_ptr = LLVMBuildGEP2(b, elem_ty, src_data, idx.as_mut_ptr(), 1, c"mm_ep".as_ptr());
        let elem = LLVMBuildLoad2(b, elem_ty, elem_ptr, c"mm_elem".as_ptr());
        let best = LLVMBuildLoad2(b, elem_ty, best_var, c"mm_best_val".as_ptr());
        let cmp = if is_float {
            let pred = if is_max {
                LLVMRealPredicate::LLVMRealOGT
            } else {
                LLVMRealPredicate::LLVMRealOLT
            };
            LLVMBuildFCmp(b, pred, elem, best, c"mm_cmp".as_ptr())
        } else {
            let pred = if is_max {
                LLVMIntPredicate::LLVMIntSGT
            } else {
                LLVMIntPredicate::LLVMIntSLT
            };
            LLVMBuildICmp(b, pred, elem, best, c"mm_cmp".as_ptr())
        };
        LLVMBuildCondBr(b, cmp, update_bb, next_bb);

        LLVMPositionBuilderAtEnd(b, update_bb);
        LLVMBuildStore(b, elem, best_var);
        LLVMBuildBr(b, next_bb);

        LLVMPositionBuilderAtEnd(b, next_bb);
        let i_next = LLVMBuildAdd(b, i, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
        LLVMBuildStore(b, i_next, i_var);
        LLVMBuildBr(b, cond_bb);

        LLVMPositionBuilderAtEnd(b, end_bb);
        ValueRef(LLVMBuildLoad2(b, elem_ty, best_var, c"mm_result".as_ptr()))
    }

    /// One straight-line `min`/`max` fold step: `mm_cmp = cmp(v, best)` then
    /// `mm_best = select(mm_cmp, v, best)` (FCmp OGT/OLT for f64, ICmp SGT/SLT for
    /// int). The C++ variadic loop calls this once per argument after the seed.
    pub(crate) unsafe fn reduce_minmax_step(
        &mut self,
        best: ValueRef,
        v: ValueRef,
        elem_ty: TypeRef,
        is_max: bool,
    ) -> ValueRef {
        let b = self.builder;
        let is_float = elem_ty.0 == LLVMDoubleTypeInContext(self.context);
        let cmp = if is_float {
            let pred = if is_max {
                LLVMRealPredicate::LLVMRealOGT
            } else {
                LLVMRealPredicate::LLVMRealOLT
            };
            LLVMBuildFCmp(b, pred, v.0, best.0, c"mm_cmp".as_ptr())
        } else {
            let pred = if is_max {
                LLVMIntPredicate::LLVMIntSGT
            } else {
                LLVMIntPredicate::LLVMIntSLT
            };
            LLVMBuildICmp(b, pred, v.0, best.0, c"mm_cmp".as_ptr())
        };
        ValueRef(LLVMBuildSelect(b, cmp, v.0, best.0, c"mm_best".as_ptr()))
    }
}
