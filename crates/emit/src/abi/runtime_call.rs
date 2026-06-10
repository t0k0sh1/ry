//! abi::runtime_call — C boundary for runtime-function emission. The shells own
//! the boundary-input validation (NULL handles / count mismatch / per-element
//! NULL → sentinel), the `from_raw_parts` borrow of the parallel FFI arrays, and
//! the id-resolve / type-translate into the typed slices the `core` engine
//! methods consume. `ry_emit_runtime_call` interns its call result;
//! `ry_emit_get_runtime_fn` returns a raw, un-interned handle.

use std::ffi::c_char;

use crate::core::{FuncTypeRef, TypeRef, ValueRef};

use super::*;

/// Emit a call to runtime function `name` (return type `ret_ty`) with the
/// `arg_count` interned `arg_ids` typed by the parallel `arg_tys` (`arg_ty_count`
/// must equal `arg_count`); return the interned call result. Returns 0 on NULL /
/// count-mismatch / per-arg NULL. `name_hint` is optional.
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
    // boundary input validation: malformed callers get sentinel 0
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
    // The guards above only reject (count > 0 && null); a zero count paired with a
    // null pointer still reaches here, and slice::from_raw_parts requires a
    // non-null pointer even for zero length — so map an empty arg list to an empty
    // slice explicitly before borrowing the FFI buffers.
    let arg_tys_s: &[RyTypeRef] = if arg_ty_count == 0 {
        &[]
    } else {
        std::slice::from_raw_parts(arg_tys, arg_ty_count as usize)
    };
    let mut arg_tys_v: Vec<TypeRef> = Vec::with_capacity(arg_ty_count as usize);
    for &t in arg_tys_s {
        if t.is_null() {
            return 0;
        }
        arg_tys_v.push(TypeRef(as_type(t)));
    }
    let arg_ids_s: &[RyValueId] = if arg_count == 0 {
        &[]
    } else {
        std::slice::from_raw_parts(arg_ids, arg_count as usize)
    };
    let mut args_v: Vec<ValueRef> = Vec::with_capacity(arg_count as usize);
    for &id in arg_ids_s {
        let a = as_value(resolve(c, id));
        if a.is_null() {
            return 0;
        }
        args_v.push(ValueRef(a));
    }
    let result = c.runtime_call(
        name,
        TypeRef(as_type(ret_ty)),
        &arg_tys_v,
        &args_v,
        name_hint,
    );
    intern(c, to_ry_value(result.0))
}

/// Look up or declare runtime function `name` of type `fn_ty` and return its raw
/// (un-interned) callee handle. Returns NULL on a guard failure.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_get_runtime_fn(
    ctx: *mut RyEmitCtx,
    name: *const c_char,
    fn_ty: RyFuncTypeRef,
) -> RyValueRef {
    // boundary input validation: malformed callers get a NULL handle instead of
    // crashing the emitter (mirrors ry_emit_runtime_call's guards).
    if ctx.is_null() || name.is_null() || fn_ty.is_null() {
        return std::ptr::null_mut();
    }
    let c = cx(ctx);
    if c.module.is_null() {
        return std::ptr::null_mut();
    }
    let f = c.get_runtime_fn(name, FuncTypeRef(as_functype(fn_ty)));
    to_ry_value(f.0)
}
