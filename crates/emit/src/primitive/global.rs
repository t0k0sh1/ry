//! primitive/global — plain string-global emission with deduplication. Emits a
//! private, unnamed_addr, align-1 constant `[N+1 x i8]` global keyed on the
//! message bytes (the dedup cache lives on `EmitCtx::bounds_msg_cache` in
//! `context.rs`). No Ry ABI layout — the StringHeader-prefixed `.arc`-suffixed
//! variant used by `emitRuntimeError`-style exits lives in `composite::arc`.

use std::ffi::c_char;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{LLVMLinkage, LLVMUnnamedAddr};

use crate::context::EmitCtx;

// A private, unnamed_addr, align-1 constant string global, deduped by message
// bytes within this ctx. `name` is the already-defaulted global name.
pub(crate) unsafe fn get_or_create_msg_global(
    c: &mut EmitCtx,
    msg: &[u8],
    name: *const c_char,
) -> LLVMValueRef {
    if let Some(&gv) = c.bounds_msg_cache.get(msg) {
        return gv;
    }
    // ConstantDataArray::getString adds a trailing NUL (dont_null_terminate=0).
    let str_data =
        LLVMConstStringInContext2(c.context, msg.as_ptr() as *const c_char, msg.len(), 0);
    let gv = LLVMAddGlobal(c.module, LLVMTypeOf(str_data), name);
    LLVMSetLinkage(gv, LLVMLinkage::LLVMPrivateLinkage);
    LLVMSetInitializer(gv, str_data);
    LLVMSetGlobalConstant(gv, 1);
    LLVMSetUnnamedAddress(gv, LLVMUnnamedAddr::LLVMGlobalUnnamedAddr);
    LLVMSetAlignment(gv, 1);
    c.bounds_msg_cache.insert(msg.to_vec(), gv);
    gv
}
