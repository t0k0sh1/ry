//! abi::function — C boundary entry points for the function-creation capability
//! (#2098, [C] = (ii) boundary move): create a fresh function definition, read
//! its parameters, and emit an indirect call through a loaded function-pointer
//! value. Each extern resolves the u32 ids / translates the opaque handles, maps
//! the linkage selector `c_int` → the core `LLVMLinkage` vocabulary, calls the
//! matching `core` engine method on `EmitCtx`, and interns any produced value.
//! Pure mechanical translation — no `LLVMBuild*` / `LLVMAddFunction` here (the
//! `check-emit-abi-no-ir.sh` gate); the engine owns every IR call.

use std::ffi::{c_char, c_int};

use llvm_sys::LLVMLinkage;

use crate::core::{FuncTypeRef, FunctionRef, ValueRef};

use super::*;

// c_int → LLVMLinkage, preserving the `RY_LINKAGE_*` mapping in api.h exactly;
// any other value is rejected (the entry point returns its NULL sentinel), so a
// malformed linkage never reaches LLVM. if/else-if chain (not a `match` on the
// `RY_LINKAGE_*` consts) to dodge the const-in-pattern footgun — the same shape
// `icmp_pred_from` in abi/primitive.rs uses.
#[inline]
fn linkage_from(l: c_int) -> Option<LLVMLinkage> {
    if l == RY_LINKAGE_EXTERNAL {
        Some(LLVMLinkage::LLVMExternalLinkage)
    } else if l == RY_LINKAGE_INTERNAL {
        Some(LLVMLinkage::LLVMInternalLinkage)
    } else if l == RY_LINKAGE_PRIVATE {
        Some(LLVMLinkage::LLVMPrivateLinkage)
    } else {
        None
    }
}

/// Create a fresh function `name` of type `fn_ty` with `linkage` (a RyLinkage
/// value) and return its opaque handle. NULL ctx / fn_ty / name, or an unknown
/// linkage, → NULL. Creates no basic blocks.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_create_function(
    ctx: *mut RyEmitCtx,
    name: *const c_char,
    fn_ty: RyFuncTypeRef,
    linkage: c_int,
) -> RyFunctionRef {
    let Some(c) = checked_cx(ctx) else {
        return std::ptr::null_mut();
    };
    if fn_ty.is_null() || name.is_null() {
        return std::ptr::null_mut();
    }
    let Some(lnk) = linkage_from(linkage) else {
        return std::ptr::null_mut();
    };
    to_ry_function(
        c.create_function(name, FuncTypeRef(as_functype(fn_ty)), lnk)
            .0,
    )
}

/// Read the `idx`-th parameter value of `fn_handle` and return the interned
/// result. NULL ctx / fn_handle → 0.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_get_param(
    ctx: *mut RyEmitCtx,
    fn_handle: RyFunctionRef,
    idx: u32,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    if fn_handle.is_null() {
        return 0;
    }
    let v = c.get_param(FunctionRef(as_function(fn_handle)), idx);
    intern(c, to_ry_value(v.0))
}

/// Emit a call through the runtime function-pointer value `callee_id`, typed by
/// `fn_ty`, with `arg_ids[0..arg_count]`; return the interned result. NULL ctx /
/// fn_ty / callee, a NULL arg array for `arg_count > 0`, or any arg resolving to
/// NULL → 0. NULL `name` → empty SSA name.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_call_indirect(
    ctx: *mut RyEmitCtx,
    fn_ty: RyFuncTypeRef,
    callee_id: RyValueId,
    arg_ids: *const RyValueId,
    arg_count: u32,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    if fn_ty.is_null() {
        return 0;
    }
    let Some(callee) = resolve_value(c, callee_id) else {
        return 0;
    };
    let Some(arg_id_slice) = ffi_slice(arg_ids, arg_count) else {
        return 0;
    };
    let mut args: Vec<ValueRef> = Vec::with_capacity(arg_count as usize);
    for &id in arg_id_slice {
        let Some(v) = resolve_value(c, id) else {
            return 0;
        };
        args.push(v);
    }
    let nm = if name.is_null() { c"".as_ptr() } else { name };
    let v = c.call_indirect(FuncTypeRef(as_functype(fn_ty)), callee, &args, nm);
    intern(c, to_ry_value(v.0))
}
