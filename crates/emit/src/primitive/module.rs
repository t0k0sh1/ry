//! primitive/module — `Module::getOrInsertFunction` / `::getOrInsertGlobal`
//! equivalents. Generic name-keyed symbol lookup with no Ry-layout knowledge.

use std::ffi::c_char;

use llvm_sys::core::*;
use llvm_sys::prelude::*;

// Module::getOrInsertFunction equivalent: reuse the existing same-named
// declaration if present (LLVM uniques functions by name; under opaque
// pointers the call type is supplied to LLVMBuildCall2, so a name hit is
// reused without a duplicate `declare`), else add a new declaration.
#[inline]
pub(crate) unsafe fn get_or_insert_function(
    module: LLVMModuleRef,
    name: *const c_char,
    fn_ty: LLVMTypeRef,
) -> LLVMValueRef {
    let existing = LLVMGetNamedFunction(module, name);
    if existing.is_null() {
        LLVMAddFunction(module, name, fn_ty)
    } else {
        existing
    }
}

// Module::getOrInsertGlobal equivalent.
#[inline]
pub(crate) unsafe fn get_or_insert_global(
    module: LLVMModuleRef,
    name: *const c_char,
    ty: LLVMTypeRef,
) -> LLVMValueRef {
    let existing = LLVMGetNamedGlobal(module, name);
    if existing.is_null() {
        LLVMAddGlobal(module, ty, name)
    } else {
        existing
    }
}
