//! Runtime-function emission: the generic `ry_emit_runtime_call` and the
//! runtime-fn declaration / lookup helper `ry_emit_get_runtime_fn`.

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::LLVMTypeKind;
use std::ffi::c_char;

use crate::ffi::*;
use crate::support::*;

#[no_mangle]
pub unsafe extern "C" fn ry_emit_runtime_call(
    ctx: *mut RyEmitCtx,
    name: *const c_char,
    ret_ty: RyTypeRef,
    arg_tys: *const RyTypeRef,
    arg_ty_count: u32,
    arg_ids: *const RyValueId,
    arg_count: u32,
    name_hint: *const c_char,
) -> RyValueId {
    // ABI input validation: malformed callers get sentinel 0
    // instead of crashing the emitter. `name_hint` is optional and may be NULL.
    if ctx.is_null() || name.is_null() || ret_ty.is_null() {
        return 0;
    }
    let c = cx(ctx);
    if c.module.is_null() || c.builder.is_null() {
        return 0;
    }
    if (arg_ty_count > 0 && arg_tys.is_null()) || (arg_count > 0 && arg_ids.is_null()) {
        return 0;
    }
    if arg_ty_count != arg_count {
        return 0;
    }
    let ret_ty_t = as_type(ret_ty);
    let mut arg_tys_v: Vec<LLVMTypeRef> = Vec::with_capacity(arg_ty_count as usize);
    for i in 0..arg_ty_count as usize {
        let t = *arg_tys.add(i);
        if t.is_null() {
            return 0;
        }
        arg_tys_v.push(as_type(t));
    }
    let fn_ty = LLVMFunctionType(ret_ty_t, arg_tys_v.as_mut_ptr(), arg_ty_count, 0);
    let callee = get_or_insert_function(c.module, name, fn_ty);
    let mut args: Vec<LLVMValueRef> = Vec::with_capacity(arg_count as usize);
    for i in 0..arg_count as usize {
        let a = as_value(resolve(c, *arg_ids.add(i)));
        if a.is_null() {
            return 0;
        }
        args.push(a);
    }
    // LLVM forbids naming a void-returning call; pass an empty name in that case.
    let call_name =
        if LLVMGetTypeKind(ret_ty_t) == LLVMTypeKind::LLVMVoidTypeKind || name_hint.is_null() {
            c"".as_ptr()
        } else {
            name_hint
        };
    let result = LLVMBuildCall2(
        c.builder,
        fn_ty,
        callee,
        args.as_mut_ptr(),
        arg_count,
        call_name,
    );
    intern(c, to_ry_value(result))
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_get_runtime_fn(
    ctx: *mut RyEmitCtx,
    name: *const c_char,
    fn_ty: RyFuncTypeRef,
) -> RyValueRef {
    // ABI input validation: malformed callers get a NULL handle instead of
    // crashing the emitter (mirrors ry_emit_runtime_call's guards).
    if ctx.is_null() || name.is_null() || fn_ty.is_null() {
        return std::ptr::null_mut();
    }
    let c = cx(ctx);
    if c.module.is_null() {
        return std::ptr::null_mut();
    }
    let fn_ty = as_functype(fn_ty);
    to_ry_value(get_or_insert_function(c.module, name, fn_ty))
}
