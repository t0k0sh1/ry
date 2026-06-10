//! abi::result — C boundary for Result emission. Both entry points are thin
//! shells over `core`: `ry_emit_build_error_from_runtime` resolves the type
//! handle and interns the aggregate; `ry_emit_result_branch` validates inputs,
//! resolves `is_err`, wraps each C builder callback (`RyBuildValueFn`) into a
//! `FnMut() -> ValueRef` closure that re-enters the emitter and resolves the
//! returned id, then delegates the IR emission to `core::emit_result_branch` and
//! interns the phi. The closure invocation + intern/resolve juggling is the only
//! abi-side work; the LLVM IR (BB scaffold + cond-br + phi) lives in `core`,
//! which holds no `&mut EmitCtx` borrow so the re-entrant `cx(ctx)` in each
//! closure does not alias a live receiver across the extern call (#2069 resolves
//! the #2060 split). The builder-derived parent rule applies in `core`
//! (#1968 / #1969).

use std::ffi::{c_char, c_void};

use crate::core::{TypeRef, ValueRef};
use crate::result::emit_result_branch;

use super::*;

/// Build an `Error` aggregate by calling runtime error function `err_fn_name`,
/// with `error_ty` the opaque Error StructType; return the interned value.
/// Returns 0 (sentinel) on NULL ctx / name / type or an uninitialized emitter.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_build_error_from_runtime(
    ctx: *mut RyEmitCtx,
    err_fn_name: *const c_char,
    error_ty: RyTypeRef,
) -> RyValueId {
    // boundary input validation: malformed callers get sentinel 0 instead of
    // crashing the emitter (mirrors ry_emit_runtime_call's guards).
    if ctx.is_null() || err_fn_name.is_null() || error_ty.is_null() {
        return 0;
    }
    let c = cx(ctx);
    if c.context.is_null() || c.module.is_null() || c.builder.is_null() {
        return 0;
    }
    let result = c.build_error_from_runtime(err_fn_name, TypeRef(as_type(error_ty)));
    intern(c, to_ry_value(result.0))
}

/// Emit a `Result` branch: three BBs (res.ok / res.err / res.merge) joined by a
/// PHI, with `build_ok` / `build_err` (forwarded `user_ctx`) emitting each arm;
/// return the interned PHI. Returns 0 on NULL inputs. Precondition:
/// `ry_emit_ctx_set_function` must be set (three BBs are created).
#[no_mangle]
pub unsafe extern "C" fn ry_emit_result_branch(
    ctx: *mut RyEmitCtx,
    is_err_id: RyValueId,
    res_ty: RyTypeRef,
    build_ok: RyBuildValueFn,
    build_err: RyBuildValueFn,
    user_ctx: *mut c_void,
) -> RyValueId {
    // boundary input validation: malformed callers get sentinel 0 instead of
    // crashing the emitter. build_ok / build_err are nullable at the FFI
    // boundary (RyBuildValueFn = Option<fn>); reject NULL here rather than
    // panicking on `unwrap` across the extern "C" boundary.
    if ctx.is_null() || res_ty.is_null() {
        return 0;
    }
    let (Some(build_ok), Some(build_err)) = (build_ok, build_err) else {
        return 0;
    };
    let context = cx(ctx).context;
    let b = cx(ctx).builder;
    if context.is_null() || b.is_null() {
        return 0;
    }
    let is_err = ValueRef(as_value(resolve(cx(ctx), is_err_id)));
    // Wrap each re-entrant C builder into a closure: call the callback (which
    // re-enters the emitter and emits the arm's value IR), THEN resolve the
    // returned id to a value. The two-step body is load-bearing — inlining
    // `resolve(cx(ctx), build_ok(user_ctx))` would hold the `cx(ctx)` borrow
    // across the callback (args evaluate left-to-right), aliasing the re-entrant
    // borrow. `core::emit_result_branch` holds no EmitCtx borrow, so each
    // closure's transient `cx(ctx)` is the only live receiver borrow.
    let mut do_ok = move || -> ValueRef {
        unsafe {
            let ok_id = build_ok(user_ctx);
            ValueRef(as_value(resolve(cx(ctx), ok_id)))
        }
    };
    let mut do_err = move || -> ValueRef {
        unsafe {
            let err_id = build_err(user_ctx);
            ValueRef(as_value(resolve(cx(ctx), err_id)))
        }
    };
    let phi = emit_result_branch(
        b,
        context,
        is_err,
        TypeRef(as_type(res_ty)),
        &mut do_ok,
        &mut do_err,
    );
    intern(cx(ctx), to_ry_value(phi.0))
}
