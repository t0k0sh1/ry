//! abi::function — C boundary entry points for the function-creation capability
//! (#2098, [C] = (ii) boundary move) and the LLVM-intrinsic call capability
//! (#2102, [D] = (ii) boundary move): create a fresh function definition, read
//! its parameters, emit an indirect call through a loaded function-pointer
//! value, and emit an overloaded-intrinsic declaration + call as a single op
//! (`LLVMGetIntrinsicDeclaration` + `LLVMBuildCall2`, with `LLVMIntrinsicGetType`
//! deriving the FunctionType engine-side so no `llvm::Function` manipulation
//! leaks back to C++). Each extern resolves the u32 ids / translates the opaque
//! handles, maps the linkage selector `c_int` → the core `LLVMLinkage`
//! vocabulary, calls the matching `core` engine method on `EmitCtx`, and interns
//! any produced value. Pure mechanical translation — no `LLVMBuild*` /
//! `LLVMAddFunction` / `LLVMGetIntrinsicDeclaration` here (the
//! `check-emit-abi-no-ir.sh` gate); the engine owns every IR call.

use std::ffi::{c_char, c_int};

use llvm_sys::LLVMLinkage;

use crate::context::{FuncTypeRef, FunctionRef, TypeRef, ValueRef};

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

/// Add the `nounwind` attribute (LLVM `NoUnwind`) to `fn_handle`. Mirrors C++
/// `llvm::Function::setDoesNotThrow()`. NULL ctx / fn_handle → no-op. Added
/// for pilot G (#2196): the GC visitor thunk generator marks the per-type
/// `__ry_gc_visit_<TypeName>` functions as nothrow.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_function_set_nounwind(
    ctx: *mut RyEmitCtx,
    fn_handle: RyFunctionRef,
) {
    let Some(c) = checked_cx(ctx) else {
        return;
    };
    if fn_handle.is_null() {
        return;
    }
    c.function_set_nounwind(FunctionRef(as_function(fn_handle)));
}

/// Read the `idx`-th parameter value of `fn_handle` and return the interned
/// result. NULL ctx / fn_handle, or `idx` out of `LLVMCountParams(fn_handle)`
/// range, → 0. The range guard lives in `get_param` (core layer) because
/// `check-emit-abi-no-ir.sh` (#2069) forbids `llvm_sys::core` here; this shell
/// only propagates `get_param`'s `None` as the sentinel 0 (#2141).
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
    let Some(v) = c.get_param(FunctionRef(as_function(fn_handle)), idx) else {
        return 0;
    };
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

/// Emit a call to the overloaded LLVM intrinsic identified by `intrinsic_id`
/// (`llvm::Intrinsic::ID` as u32), parameterised by `overload_tys[0..overload_count]`,
/// with operand `arg_ids[0..arg_count]`; return the interned result. The engine
/// handles declaration acquisition, function-type derivation, and the call —
/// nothing about the resulting `llvm::Function*` leaks back to C++.
///
/// NULL ctx, a NULL overload-type array with `overload_count > 0`, any NULL
/// overload-type element, a NULL arg array with `arg_count > 0`, or any arg
/// resolving to NULL → 0. NULL `name` → empty SSA name. `overload_count == 0`
/// is valid (non-overloaded intrinsics) and yields an empty type slice.
/// `intrinsic_id == 0` is forwarded verbatim — the LLVM C API treats it as
/// `Intrinsic::not_intrinsic` and that is the caller's responsibility.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_intrinsic_call(
    ctx: *mut RyEmitCtx,
    intrinsic_id: u32,
    overload_tys: *const RyTypeRef,
    overload_count: u32,
    arg_ids: *const RyValueId,
    arg_count: u32,
    name: *const c_char,
) -> RyValueId {
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let Some(overload_slice) = ffi_slice(overload_tys, overload_count) else {
        return 0;
    };
    let mut overload_refs: Vec<TypeRef> = Vec::with_capacity(overload_count as usize);
    for &ty in overload_slice {
        if ty.is_null() {
            return 0;
        }
        overload_refs.push(TypeRef(as_type(ty)));
    }
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
    let v = c.build_intrinsic_call(intrinsic_id, &overload_refs, &args, nm);
    intern(c, to_ry_value(v.0))
}
