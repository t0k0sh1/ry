//! Bounds / index-checking IR generation (core-role: an `impl EmitCtx` over the
//! core engine only, so it is abi-independent and the `core⇏abi` invariant
//! covers this module). The index bounds-check, the negative-index wrap, and the
//! bounds-error emit sequence (`fprintf(stderr)` + `_Exit(1)` + `unreachable`).
//! The C++ path enters through the `abi::bounds` externs, which resolve u32 ids,
//! map the `c_int` kind to `BoundsKind`, and intern any result. `bounds_check`
//! calls the wrap / error methods directly (no id bridge); since #2061 the
//! migrated `collection` methods reach them the same way (the `crate::abi::*`
//! re-export they used to go through was dropped).

use std::ffi::{c_char, CStr};

use llvm_sys::core::*;
use llvm_sys::LLVMIntPredicate;

use crate::core::*;

impl EmitCtx {
    // Emit a runtime index bounds-check: normalize i1 → i64, apply the
    // negative-index wrap, then branch to an OOB-error block (index < 0 or
    // index >= len) or the OK block. Returns the wrapped, in-range index value;
    // the abi boundary interns it.
    pub(crate) unsafe fn bounds_check(
        &mut self,
        idx: ValueRef,
        len: ValueRef,
        kind: BoundsKind,
        global_name: *const c_char,
        bb_prefix: *const c_char,
    ) -> ValueRef {
        let context = self.context;
        let b = self.builder;
        let i1_ty = i1_type(context);
        let i64_ty = i64_type(context);
        let mut idx = idx.0;
        let len = len.0;
        if LLVMTypeOf(idx) == i1_ty {
            idx = LLVMBuildZExt(b, idx, i64_ty, c"idx_ext".as_ptr());
        }
        let orig_index = idx;

        // Negative-index wrap is a separate engine method (its abi extern is the
        // C++ entry); call it directly so we never round-trip through the id
        // bridge.
        idx = self
            .negative_index_wrap(ValueRef(idx), ValueRef(len), bb_prefix)
            .0;

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
        // Derive parent function from the builder, not a cached function field
        // (builder-derived parent rule — .claude/rules/codegen-llvm-ir-conventions.md,
        // #1996; the cached EmitCtx::function field was removed in #2083).
        let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));
        let oob_bb = LLVMAppendBasicBlockInContext(context, fn_v, oob_block.as_ptr());
        let ok_bb = LLVMAppendBasicBlockInContext(context, fn_v, ok_block.as_ptr());
        LLVMBuildCondBr(b, oob, oob_bb, ok_bb);
        LLVMPositionBuilderAtEnd(b, oob_bb);

        let fmt_msg: &CStr = if kind == BoundsKind::List {
            c"runtime error: index %lld out of bounds for list of length %lld\n"
        } else {
            c"runtime error: index %lld out of bounds for array of length %lld\n"
        };
        self.bounds_error(
            ValueRef(orig_index),
            ValueRef(len),
            fmt_msg.as_ptr(),
            global_name,
        );

        LLVMPositionBuilderAtEnd(b, ok_bb);
        ValueRef(idx)
    }

    // Map a possibly-negative index to a non-negative one: `idx < 0 ? idx +
    // wrap_base : idx`, normalizing narrow operands to i64 first. Returns the
    // selected index; the abi boundary interns it.
    pub(crate) unsafe fn negative_index_wrap(
        &mut self,
        idx: ValueRef,
        wrap_base: ValueRef,
        prefix: *const c_char,
    ) -> ValueRef {
        let b = self.builder;
        let i64_ty = i64_type(self.context);
        let mut idx = idx.0;
        let mut wrap_base = wrap_base.0;
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
        ValueRef(result)
    }

    // Emit the OOB-exit sequence: load stdout/stderr, fprintf the (deduped)
    // format-string global with the original index + length, fflush both, then
    // `_Exit(1)` + `unreachable` (terminates the block). The caller must have
    // positioned the builder in the OOB block first.
    pub(crate) unsafe fn bounds_error(
        &mut self,
        orig_idx: ValueRef,
        len: ValueRef,
        fmt_msg: *const c_char,
        global_name: *const c_char,
    ) {
        let b = self.builder;
        let orig_idx = orig_idx.0;
        let len = len.0;
        let ptr_ty = ptr_type(self.context);
        let i32_ty = i32_type(self.context);
        let void_ty = void_type(self.context);
        let (stdout_name, stderr_name) = if cfg!(target_os = "macos") {
            (c"__stdoutp".as_ptr(), c"__stderrp".as_ptr())
        } else {
            (c"stdout".as_ptr(), c"stderr".as_ptr())
        };
        let stderr_global = get_or_insert_global(self.module, stderr_name, ptr_ty);
        let stdout_global = get_or_insert_global(self.module, stdout_name, ptr_ty);
        let stderr_val = LLVMBuildLoad2(b, ptr_ty, stderr_global, c"stderr".as_ptr());
        let stdout_val = LLVMBuildLoad2(b, ptr_ty, stdout_global, c"stdout".as_ptr());

        // Dedup the format-string global within this ctx.
        let fmt_key = cstr_bytes(fmt_msg);
        let name_ptr = if global_name.is_null() {
            c".bounds_err_msg".as_ptr()
        } else {
            global_name
        };
        let err_msg = get_or_create_msg_global(self, fmt_key, name_ptr);

        let mut fprintf_params = [ptr_ty, ptr_ty];
        let fprintf_ty = LLVMFunctionType(i32_ty, fprintf_params.as_mut_ptr(), 2, 1);
        let fprintf_fn = get_or_insert_function(self.module, c"fprintf".as_ptr(), fprintf_ty);
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
        let fflush_fn = get_or_insert_function(self.module, c"fflush".as_ptr(), fflush_ty);
        let mut a_out = [stdout_val];
        LLVMBuildCall2(b, fflush_ty, fflush_fn, a_out.as_mut_ptr(), 1, c"".as_ptr());
        let mut a_err = [stderr_val];
        LLVMBuildCall2(b, fflush_ty, fflush_fn, a_err.as_mut_ptr(), 1, c"".as_ptr());

        let mut exit_params = [i32_ty];
        let exit_ty = LLVMFunctionType(void_ty, exit_params.as_mut_ptr(), 1, 0);
        let exit_fn = get_or_insert_function(self.module, c"_Exit".as_ptr(), exit_ty);
        let mut ea = [LLVMConstInt(i32_ty, 1, 0)];
        LLVMBuildCall2(b, exit_ty, exit_fn, ea.as_mut_ptr(), 1, c"".as_ptr());
        LLVMBuildUnreachable(b);
    }
}
