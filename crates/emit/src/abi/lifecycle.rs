//! abi::lifecycle — C boundary entry points for the emission-context lifecycle:
//! create / destroy / set_function and the intern / resolve value-handle entry
//! points. `ctx_create` boxes a `core::EmitCtx::new` and hands back the opaque
//! handle; the rest are boundary plumbing over that handle (intern / resolve are
//! abi-side; `set_function` is retained pending the deprecation noted in #1968).

use llvm_sys::prelude::*;

use crate::core::EmitCtx;

use super::*;

/// Construct an `EmitCtx` from the LLVM module / builder / context / function
/// handles and return the owning opaque handle (boxed; freed by
/// `ry_emit_ctx_destroy`).
#[no_mangle]
pub unsafe extern "C" fn ry_emit_ctx_create(
    module: RyModuleRef,
    builder: RyBuilderRef,
    context: RyContextRef,
    function: RyFunctionRef,
) -> *mut RyEmitCtx {
    let boxed = Box::new(EmitCtx::new(
        module as LLVMModuleRef,
        builder as LLVMBuilderRef,
        context as LLVMContextRef,
        function as LLVMValueRef,
    ));
    Box::into_raw(boxed) as *mut RyEmitCtx
}

/// Free the `EmitCtx` created by `ry_emit_ctx_create`. NULL is a no-op.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_ctx_destroy(ctx: *mut RyEmitCtx) {
    if !ctx.is_null() {
        drop(Box::from_raw(ctx as *mut EmitCtx));
    }
}

/// Update the cached current function. Retained pending deprecation (#1968):
/// BB-creating ops derive the parent from the builder, not this field.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_ctx_set_function(ctx: *mut RyEmitCtx, function: RyFunctionRef) {
    // boundary input validation: NULL ctx → no-op. This entry touches no LLVM
    // module/builder/context, so only the ctx pointer itself needs guarding
    // (the `checked_cx` three-field check would be over-strict here). The
    // preceding NULL guard satisfies `with_ctx`'s non-NULL precondition.
    if ctx.is_null() {
        return;
    }
    with_ctx(ctx, |c| c.function = function as LLVMValueRef);
}

/// Intern an opaque `value` handle into a `RyValueId` (sentinel 0 = invalid).
#[no_mangle]
pub unsafe extern "C" fn ry_emit_intern(ctx: *mut RyEmitCtx, value: RyValueRef) -> RyValueId {
    // boundary input validation: NULL ctx → sentinel 0 (no LLVM handle touched).
    if ctx.is_null() {
        return 0;
    }
    // The explicit `unsafe` block covers the `with_ctx` closure body (a nested
    // closure is not reached by the `unsafe fn`'s implicit body-unsafe), so the
    // closure needs no inner `unsafe`.
    unsafe { with_ctx(ctx, |c| intern(c, value)) }
}

/// Resolve a `RyValueId` back to its opaque value handle (inverse of
/// `ry_emit_intern`).
#[no_mangle]
pub unsafe extern "C" fn ry_emit_resolve(ctx: *mut RyEmitCtx, id: RyValueId) -> RyValueRef {
    // boundary input validation: NULL ctx → NULL (no LLVM handle touched).
    if ctx.is_null() {
        return std::ptr::null_mut();
    }
    unsafe { with_ctx(ctx, |c| resolve(c, id)) }
}
