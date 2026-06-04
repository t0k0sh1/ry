//! Emission-context lifecycle ABI: create / destroy / set_function and the
//! intern / resolve value-handle entry points.

use std::collections::HashMap;

use llvm_sys::prelude::*;

use crate::ffi::*;
use crate::support::*;

#[no_mangle]
pub unsafe extern "C" fn ry_emit_ctx_create(
    module: RyModuleHandle,
    builder: RyBuilderHandle,
    context: RyContextHandle,
    function: RyFunctionHandle,
) -> *mut RyEmitCtx {
    let boxed = Box::new(EmitCtxImpl {
        module: module as LLVMModuleRef,
        builder: builder as LLVMBuilderRef,
        context: context as LLVMContextRef,
        function: function as LLVMValueRef,
        // Reserve handle 0 as the "invalid" sentinel; resolve(_, 0) -> NULL.
        values: vec![std::ptr::null_mut()],
        bounds_msg_cache: HashMap::new(),
    });
    Box::into_raw(boxed) as *mut RyEmitCtx
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_ctx_destroy(ctx: *mut RyEmitCtx) {
    if !ctx.is_null() {
        drop(Box::from_raw(ctx as *mut EmitCtxImpl));
    }
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_ctx_set_function(ctx: *mut RyEmitCtx, function: RyFunctionHandle) {
    cx(ctx).function = function as LLVMValueRef;
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_intern(ctx: *mut RyEmitCtx, value: RyValueRef) -> RyValueId {
    intern(cx(ctx), value)
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_resolve(ctx: *mut RyEmitCtx, id: RyValueId) -> RyValueRef {
    resolve(cx(ctx), id)
}
