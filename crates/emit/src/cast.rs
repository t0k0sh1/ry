//! Cast — checked FP→int range-check + runtime-error exit + happy-path
//! FPToUI/FPToSI (the C++ `emitCheckedFPToInt` counterpart). The shared helper
//! that `src/codegen_expr_cast.cpp`'s 9 int-target cast cases and
//! `src/codegen_call_user.cpp::coerceToLowLevelType`'s int branch all delegate
//! to (10 callsites); migrating it pulls every callsite onto the cdylib path
//! implicitly (#2097).
//!
//! Range: signed → [-2^(W-1), 2^(W-1)), unsigned → [0, 2^W) (half-open). For
//! W ≤ 64 every bound is exactly representable in f64. The compares use
//! UNORDERED predicates (`ULT` / `UGE`), so NaN / ±inf are also rejected (NaN
//! makes both true; the OR matches every invalid input with one check).
//!
//! Mirrors C++ `emitRuntimeError`'s instruction order EXACTLY: stderr load →
//! fprintf → stdout load → fflush stdout → fflush stderr → _Exit → unreachable.
//! Distinct from `bounds_error`'s (stderr+stdout adjacent loads before
//! fprintf); a stray bounds_error reuse would reorder stdout's load and break
//! bit-exact parity (#2092 `load_list_header` instruction-order lesson).
//!
//! Variadic `fprintf` carve-out (#2100): the call uses `isVarArg=1` and is
//! built inline with `LLVMBuildCall2`, NOT routed through
//! `ry_emit_runtime_call` (whose `FunctionType` is non-variadic). Same shape
//! as `bounds_error`.

use std::ffi::c_char;

use llvm_sys::core::*;
use llvm_sys::{LLVMRealPredicate, LLVMTypeKind};

use crate::core::*;

impl EmitCtx {
    /// Emit the checked FP→int conversion sequence: FPExt(f32→f64) if needed →
    /// `FCmpULT(lo) | FCmpUGE(hi)` → `CondBr` to fresh failBB / okBB pair → in
    /// failBB the runtime-error exit → in okBB FPToSI / FPToUI on the ORIGINAL
    /// value (sub-64-bit targets still see the correct rounding direction
    /// because the range check ran in f64). Returns the converted integer
    /// value; the abi boundary interns it.
    ///
    /// `target_width` is the destination integer bit width (8/16/32/64).
    /// `is_signed` selects FPToSI vs FPToUI and the `[-2^(W-1), 2^(W-1))`
    /// range. `bb_prefix`, `msg`, `global_name` are borrowed C strings; the
    /// engine guards each against NULL via `cstr_bytes` (empty slice on NULL).
    pub(crate) unsafe fn checked_fp_to_int(
        &mut self,
        val: ValueRef,
        target_width: u32,
        is_signed: bool,
        bb_prefix: *const c_char,
        msg: *const c_char,
        global_name: *const c_char,
    ) -> ValueRef {
        let context = self.context;
        let b = self.builder;
        let f64_ty = LLVMDoubleTypeInContext(context);
        let target_ty = LLVMIntTypeInContext(context, target_width);
        let val_v = val.0;
        let p = cstr_bytes(bb_prefix);

        // FPExt(f32→f64) if needed; named "<prefix>_f64ext". An already-f64
        // value is used as-is — matches the C++ `val->getType() == f64Ty_`
        // skip.
        let val_f64 = if LLVMGetTypeKind(LLVMTypeOf(val_v)) == LLVMTypeKind::LLVMFloatTypeKind {
            let n = cname_pfx(p, b"_f64ext");
            LLVMBuildFPExt(b, val_v, f64_ty, n.as_ptr())
        } else {
            val_v
        };

        // Accept-range bounds (half-open):
        //   signed:   [-2^(W-1), 2^(W-1))   — INT_MIN exact, INT_MAX+1 rejects
        //   unsigned: [0, 2^W)
        // f64::powi exactly matches `std::ldexp(1.0, n)` for `n ≤ 64` because
        // every value involved is a power of two within f64's normal range.
        let lo = if is_signed {
            -(2.0_f64).powi(target_width as i32 - 1)
        } else {
            0.0
        };
        let hi = if is_signed {
            (2.0_f64).powi(target_width as i32 - 1)
        } else {
            (2.0_f64).powi(target_width as i32)
        };
        let lo_c = LLVMConstReal(f64_ty, lo);
        let hi_c = LLVMConstReal(f64_ty, hi);

        // Unordered compares fold the NaN check into the range check.
        let too_low_n = cname_pfx(p, b"_lo");
        let too_low = LLVMBuildFCmp(
            b,
            LLVMRealPredicate::LLVMRealULT,
            val_f64,
            lo_c,
            too_low_n.as_ptr(),
        );
        let too_high_n = cname_pfx(p, b"_hi");
        let too_high = LLVMBuildFCmp(
            b,
            LLVMRealPredicate::LLVMRealUGE,
            val_f64,
            hi_c,
            too_high_n.as_ptr(),
        );
        let invalid_n = cname_pfx(p, b"_invalid");
        let invalid = LLVMBuildOr(b, too_low, too_high, invalid_n.as_ptr());

        // BBs: <prefix>.fail / <prefix>.ok. Parent fn from the builder
        // (#1996 builder-derived parent rule; the cached EmitCtx::function
        // field was removed in #2083).
        let fail_block_n = cname_pfx(p, b".fail");
        let ok_block_n = cname_pfx(p, b".ok");
        let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));
        let fail_bb = LLVMAppendBasicBlockInContext(context, fn_v, fail_block_n.as_ptr());
        let ok_bb = LLVMAppendBasicBlockInContext(context, fn_v, ok_block_n.as_ptr());
        LLVMBuildCondBr(b, invalid, fail_bb, ok_bb);

        LLVMPositionBuilderAtEnd(b, fail_bb);
        self.runtime_error_with_value_arg(msg, global_name, ValueRef(val_f64));

        LLVMPositionBuilderAtEnd(b, ok_bb);
        // Cast the ORIGINAL value (not the FPExt'd f64) so sub-64-bit targets
        // see the correct rounding direction. Named with bb_prefix (no
        // suffix) to match the C++ baseline.
        let cast_result = if is_signed {
            LLVMBuildFPToSI(b, val_v, target_ty, bb_prefix)
        } else {
            LLVMBuildFPToUI(b, val_v, target_ty, bb_prefix)
        };
        ValueRef(cast_result)
    }

    // Mirror C++ `emitRuntimeError`'s instruction order EXACTLY: stderr load
    // (named "stderr") → cachedGlobalString (deduped on msg bytes) →
    // fprintf(stderr, fmt, val) → stdout load (named "stdout") → fflush
    // stdout → fflush stderr → _Exit(1) → unreachable. Distinct from
    // `bounds_error`'s (stderr / stdout adjacent loads before fprintf); see
    // the module doc.
    unsafe fn runtime_error_with_value_arg(
        &mut self,
        msg: *const c_char,
        global_name: *const c_char,
        extra_arg: ValueRef,
    ) {
        let b = self.builder;
        let context = self.context;
        let ptr_ty = ptr_type(context);
        let i32_ty = i32_type(context);
        let void_ty = void_type(context);
        let (stdout_name, stderr_name) = if cfg!(target_os = "macos") {
            (c"__stdoutp".as_ptr(), c"__stderrp".as_ptr())
        } else {
            (c"stdout".as_ptr(), c"stderr".as_ptr())
        };

        // 1. stderr load.
        let stderr_global = get_or_insert_global(self.module, stderr_name, ptr_ty);
        let stderr_val = LLVMBuildLoad2(b, ptr_ty, stderr_global, c"stderr".as_ptr());

        // 2. cachedGlobalString equivalent — a StringHeader-prefixed global
        // with the `.arc` name suffix the C++ side uses, so the global type
        // matches `{ i64, i64, i64, [N x i8] }` and fprintf receives an
        // in-bounds ConstantExpr GEP into the data payload. (Plain
        // `get_or_create_msg_global` would produce a bare `[N x i8]` global
        // and break bit-exact parity with the C++ baseline.) Dedup is keyed on
        // msg bytes; global_name is a name hint only.
        let msg_key = cstr_bytes(msg);
        let name_ptr = if global_name.is_null() {
            c".fptoi_err_msg".as_ptr()
        } else {
            global_name
        };
        let err_msg = get_or_create_arc_msg_global(self, msg_key, name_ptr);

        // 3. fprintf(stderr, errMsg, extra_arg). Variadic FunctionType
        // (isVarArg=1) built inline — NOT routed through ry_emit_runtime_call
        // (#2100 variadic-runtime carve-out).
        let mut fprintf_params = [ptr_ty, ptr_ty];
        let fprintf_ty = LLVMFunctionType(i32_ty, fprintf_params.as_mut_ptr(), 2, 1);
        let fprintf_fn = get_or_insert_function(self.module, c"fprintf".as_ptr(), fprintf_ty);
        let mut fprintf_args = [stderr_val, err_msg, extra_arg.0];
        LLVMBuildCall2(
            b,
            fprintf_ty,
            fprintf_fn,
            fprintf_args.as_mut_ptr(),
            3,
            c"".as_ptr(),
        );

        // 4. stdout load AFTER fprintf — matches the C++ order. (bounds_error
        // loads stdout BEFORE fprintf; that interleaving difference is the
        // exact reason a shared helper does NOT fit here.)
        let stdout_global = get_or_insert_global(self.module, stdout_name, ptr_ty);
        let stdout_val = LLVMBuildLoad2(b, ptr_ty, stdout_global, c"stdout".as_ptr());

        // 5/6. fflush stdout, fflush stderr.
        let mut fflush_params = [ptr_ty];
        let fflush_ty = LLVMFunctionType(i32_ty, fflush_params.as_mut_ptr(), 1, 0);
        let fflush_fn = get_or_insert_function(self.module, c"fflush".as_ptr(), fflush_ty);
        let mut a_out = [stdout_val];
        LLVMBuildCall2(b, fflush_ty, fflush_fn, a_out.as_mut_ptr(), 1, c"".as_ptr());
        let mut a_err = [stderr_val];
        LLVMBuildCall2(b, fflush_ty, fflush_fn, a_err.as_mut_ptr(), 1, c"".as_ptr());

        // 7. _Exit(1).
        let mut exit_params = [i32_ty];
        let exit_ty = LLVMFunctionType(void_ty, exit_params.as_mut_ptr(), 1, 0);
        let exit_fn = get_or_insert_function(self.module, c"_Exit".as_ptr(), exit_ty);
        let mut ea = [LLVMConstInt(i32_ty, 1, 0)];
        LLVMBuildCall2(b, exit_ty, exit_fn, ea.as_mut_ptr(), 1, c"".as_ptr());

        // 8. unreachable.
        LLVMBuildUnreachable(b);
    }
}
