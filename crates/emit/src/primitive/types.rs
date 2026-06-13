//! primitive/types — LLVM 1:1 type constructors. Pure thin wrappers over
//! `LLVMInt{1,8,32,64}TypeInContext` / `LLVMPointerTypeInContext` /
//! `LLVMVoidTypeInContext`, with no Ry-layout knowledge.

use llvm_sys::core::*;
use llvm_sys::prelude::*;

#[inline]
pub(crate) unsafe fn i1_type(c: LLVMContextRef) -> LLVMTypeRef {
    LLVMInt1TypeInContext(c)
}
#[inline]
pub(crate) unsafe fn i8_type(c: LLVMContextRef) -> LLVMTypeRef {
    LLVMInt8TypeInContext(c)
}
#[inline]
pub(crate) unsafe fn i32_type(c: LLVMContextRef) -> LLVMTypeRef {
    LLVMInt32TypeInContext(c)
}
#[inline]
pub(crate) unsafe fn i64_type(c: LLVMContextRef) -> LLVMTypeRef {
    LLVMInt64TypeInContext(c)
}
#[inline]
pub(crate) unsafe fn ptr_type(c: LLVMContextRef) -> LLVMTypeRef {
    LLVMPointerTypeInContext(c, 0)
}
#[inline]
pub(crate) unsafe fn void_type(c: LLVMContextRef) -> LLVMTypeRef {
    LLVMVoidTypeInContext(c)
}
