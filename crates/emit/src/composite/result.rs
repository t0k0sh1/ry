//! Result IR generation (core-role: abi-independent IR emission, so the
//! `core⇏abi` invariant covers this module). `build_error_from_runtime` is an
//! `impl EmitCtx` method building an `Error` aggregate from a runtime error
//! function. The Result ok/err branch + phi (`emit_result_branch`) is a free
//! function — NOT a `&mut self` method — taking the ok/err value builders as
//! `&mut dyn FnMut() -> ValueRef` closures plus the decomposed `builder` /
//! `context` Copy handles. Holding no `&mut EmitCtx` borrow (the closures, not
//! core, own all `EmitCtx` access), it invokes the re-entrant builders without
//! aliasing the `cx(ctx)` borrow that those builders form on re-entry: #2069
//! resolves the #2060 split, where a `&mut self` method would have aliased that
//! borrow across the extern call (UB under Stacked/Tree Borrows). The C++ path
//! enters through the `abi::result` externs, which validate inputs, resolve the
//! type handle, build the two closures (callback + intern/resolve), and intern
//! the result.

use std::ffi::c_char;

use llvm_sys::core::*;
use llvm_sys::prelude::*;

use crate::context::*;
use crate::primitive::module::get_or_insert_function;
use crate::primitive::types::{i64_type, ptr_type};

impl EmitCtx {
    // Build an `Error` aggregate: call the runtime error function `err_fn_name`
    // (`() -> ptr` message), insert it at field 0 (`err.msg`) and a zero code at
    // field 1 (`err.code`). Returns the raw aggregate; the abi boundary interns
    // it.
    pub(crate) unsafe fn build_error_from_runtime(
        &mut self,
        err_fn_name: *const c_char,
        error_ty: TypeRef,
    ) -> ValueRef {
        let error_ty = error_ty.0;
        let ptr_ty = ptr_type(self.context);
        let i64_ty = i64_type(self.context);
        let err_fn_ty = LLVMFunctionType(ptr_ty, std::ptr::null_mut(), 0, 0);
        let err_fn = get_or_insert_function(self.module, err_fn_name, err_fn_ty);
        let err_msg = LLVMBuildCall2(
            self.builder,
            err_fn_ty,
            err_fn,
            std::ptr::null_mut(),
            0,
            c"err_msg".as_ptr(),
        );
        let mut err_struct = LLVMGetUndef(error_ty);
        err_struct =
            LLVMBuildInsertValue(self.builder, err_struct, err_msg, 0, c"err.msg".as_ptr());
        err_struct = LLVMBuildInsertValue(
            self.builder,
            err_struct,
            LLVMConstInt(i64_ty, 0, 0),
            1,
            c"err.code".as_ptr(),
        );
        ValueRef(err_struct)
    }
}

// Emit the Result ok/err branch + merge phi. `is_err` selects err_bb vs ok_bb;
// `build_ok` / `build_err` are re-entrant builders that emit each arm's value
// into the positioned block and return it. Free function (not `&mut self`): it
// holds only the Copy `builder` / `context` handles — no `&mut EmitCtx` borrow —
// so the abi closures may re-enter the emitter via `cx(ctx)` while a builder
// runs without aliasing a live receiver borrow across the extern call (#2069 /
// #2060). The parent function for the new BBs is derived from the builder, not a
// cached function field, per the builder-derived parent rule (#1968): a NULL
// insert block yields a NULL `ValueRef` (the abi shell interns it to 0).
pub(crate) unsafe fn emit_result_branch(
    builder: LLVMBuilderRef,
    context: LLVMContextRef,
    is_err: ValueRef,
    res_ty: TypeRef,
    build_ok: &mut dyn FnMut() -> ValueRef,
    build_err: &mut dyn FnMut() -> ValueRef,
) -> ValueRef {
    let b = builder;
    let is_err = is_err.0;
    let res_ty = res_ty.0;
    // Builder-derived parent function (builder-derived parent rule).
    let insert_bb = LLVMGetInsertBlock(b);
    if insert_bb.is_null() {
        return ValueRef(std::ptr::null_mut());
    }
    let fn_v = LLVMGetBasicBlockParent(insert_bb);
    let ok_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.ok".as_ptr());
    let err_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.err".as_ptr());
    let merge_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.merge".as_ptr());
    LLVMBuildCondBr(b, is_err, err_bb, ok_bb);

    LLVMPositionBuilderAtEnd(b, ok_bb);
    let ok_val = build_ok().0;
    LLVMBuildBr(b, merge_bb);
    // Re-capture the incoming block: the builder may have advanced through
    // additional BBs (load-bearing).
    let ok_in = LLVMGetInsertBlock(b);

    LLVMPositionBuilderAtEnd(b, err_bb);
    let err_val = build_err().0;
    LLVMBuildBr(b, merge_bb);
    let err_in = LLVMGetInsertBlock(b);

    LLVMPositionBuilderAtEnd(b, merge_bb);
    let phi = LLVMBuildPhi(b, res_ty, c"result".as_ptr());
    let mut vals = [ok_val, err_val];
    let mut blocks = [ok_in, err_in];
    LLVMAddIncoming(phi, vals.as_mut_ptr(), blocks.as_mut_ptr(), 2);
    ValueRef(phi)
}
