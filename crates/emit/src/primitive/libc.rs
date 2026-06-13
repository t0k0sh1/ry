//! primitive/libc — declare-and-call wrappers for `malloc`, `memcpy`, `memmove`,
//! `free`. Generic C-runtime emitters with no Ry-layout knowledge. malloc takes
//! a result `name` (call sites vary: app_new_data / ins_new_data / sl_data /
//! arc_box / cow_*); memcpy / memmove / free are unnamed at every site, so the
//! empty name is fixed here.

use std::ffi::c_char;

use llvm_sys::core::*;
use llvm_sys::prelude::*;

use crate::primitive::module::get_or_insert_function;

pub(crate) unsafe fn emit_malloc(
    b: LLVMBuilderRef,
    module: LLVMModuleRef,
    i64_ty: LLVMTypeRef,
    ptr_ty: LLVMTypeRef,
    size: LLVMValueRef,
    name: *const c_char,
) -> LLVMValueRef {
    let mut p = [i64_ty];
    let ty = LLVMFunctionType(ptr_ty, p.as_mut_ptr(), 1, 0);
    let f = get_or_insert_function(module, c"malloc".as_ptr(), ty);
    let mut a = [size];
    LLVMBuildCall2(b, ty, f, a.as_mut_ptr(), 1, name)
}

pub(crate) unsafe fn emit_memcpy(
    b: LLVMBuilderRef,
    module: LLVMModuleRef,
    ptr_ty: LLVMTypeRef,
    i64_ty: LLVMTypeRef,
    dst: LLVMValueRef,
    src: LLVMValueRef,
    n: LLVMValueRef,
) {
    let mut p = [ptr_ty, ptr_ty, i64_ty];
    let ty = LLVMFunctionType(ptr_ty, p.as_mut_ptr(), 3, 0);
    let f = get_or_insert_function(module, c"memcpy".as_ptr(), ty);
    let mut a = [dst, src, n];
    LLVMBuildCall2(b, ty, f, a.as_mut_ptr(), 3, c"".as_ptr());
}

pub(crate) unsafe fn emit_memmove(
    b: LLVMBuilderRef,
    module: LLVMModuleRef,
    ptr_ty: LLVMTypeRef,
    i64_ty: LLVMTypeRef,
    dst: LLVMValueRef,
    src: LLVMValueRef,
    n: LLVMValueRef,
) {
    let mut p = [ptr_ty, ptr_ty, i64_ty];
    let ty = LLVMFunctionType(ptr_ty, p.as_mut_ptr(), 3, 0);
    let f = get_or_insert_function(module, c"memmove".as_ptr(), ty);
    let mut a = [dst, src, n];
    LLVMBuildCall2(b, ty, f, a.as_mut_ptr(), 3, c"".as_ptr());
}

pub(crate) unsafe fn emit_free(
    b: LLVMBuilderRef,
    module: LLVMModuleRef,
    ptr_ty: LLVMTypeRef,
    void_ty: LLVMTypeRef,
    ptr: LLVMValueRef,
) {
    let mut p = [ptr_ty];
    let ty = LLVMFunctionType(void_ty, p.as_mut_ptr(), 1, 0);
    let f = get_or_insert_function(module, c"free".as_ptr(), ty);
    let mut a = [ptr];
    LLVMBuildCall2(b, ty, f, a.as_mut_ptr(), 1, c"".as_ptr());
}
