//! primitive/error — `fprintf(stderr, msg) + fflush + _Exit(1) + unreachable`.
//! A generic terminate-block sequence with no Ry-layout knowledge. Called by
//! composite ops (`bounds`, `any`, `cast`) that pre-construct a plain
//! `[N+1 x i8]` message global via `primitive::global::get_or_create_msg_global`
//! and want to exit. The caller must pre-split into err / ok BBs first (the
//! `emitRuntimeError`-terminates rule).

use std::ffi::c_char;

use llvm_sys::core::*;

use crate::context::EmitCtx;
use crate::primitive::global::get_or_create_msg_global;
use crate::primitive::module::{get_or_insert_function, get_or_insert_global};
use crate::primitive::types::{i32_type, ptr_type, void_type};

pub(crate) unsafe fn emit_inline_runtime_error(
    c: &mut EmitCtx,
    msg: &[u8],
    name_hint: *const c_char,
) {
    let b = c.builder;
    let context = c.context;
    let module = c.module;
    let ptr_ty = ptr_type(context);
    let i32_ty = i32_type(context);
    let void_ty = void_type(context);
    let (stdout_name, stderr_name) = if cfg!(target_os = "macos") {
        (c"__stdoutp".as_ptr(), c"__stderrp".as_ptr())
    } else {
        (c"stdout".as_ptr(), c"stderr".as_ptr())
    };
    let stderr_global = get_or_insert_global(module, stderr_name, ptr_ty);
    let stdout_global = get_or_insert_global(module, stdout_name, ptr_ty);
    let stderr_val = LLVMBuildLoad2(b, ptr_ty, stderr_global, c"stderr".as_ptr());
    let stdout_val = LLVMBuildLoad2(b, ptr_ty, stdout_global, c"stdout".as_ptr());
    let name = if name_hint.is_null() {
        c".any_err_msg".as_ptr()
    } else {
        name_hint
    };
    let err_msg = get_or_create_msg_global(c, msg, name);
    let mut fprintf_p = [ptr_ty, ptr_ty];
    let fprintf_ty = LLVMFunctionType(i32_ty, fprintf_p.as_mut_ptr(), 2, 1);
    let fprintf_fn = get_or_insert_function(module, c"fprintf".as_ptr(), fprintf_ty);
    let mut fprintf_a = [stderr_val, err_msg];
    LLVMBuildCall2(
        b,
        fprintf_ty,
        fprintf_fn,
        fprintf_a.as_mut_ptr(),
        2,
        c"".as_ptr(),
    );
    let mut fflush_p = [ptr_ty];
    let fflush_ty = LLVMFunctionType(i32_ty, fflush_p.as_mut_ptr(), 1, 0);
    let fflush_fn = get_or_insert_function(module, c"fflush".as_ptr(), fflush_ty);
    let mut a_out = [stdout_val];
    LLVMBuildCall2(b, fflush_ty, fflush_fn, a_out.as_mut_ptr(), 1, c"".as_ptr());
    let mut a_err = [stderr_val];
    LLVMBuildCall2(b, fflush_ty, fflush_fn, a_err.as_mut_ptr(), 1, c"".as_ptr());
    let mut exit_p = [i32_ty];
    let exit_ty = LLVMFunctionType(void_ty, exit_p.as_mut_ptr(), 1, 0);
    let exit_fn = get_or_insert_function(module, c"_Exit".as_ptr(), exit_ty);
    let mut exit_a = [LLVMConstInt(i32_ty, 1, 0)];
    LLVMBuildCall2(b, exit_ty, exit_fn, exit_a.as_mut_ptr(), 1, c"".as_ptr());
    LLVMBuildUnreachable(b);
}
