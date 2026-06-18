//! abi::result — C boundary for Result emission. Both entry points are thin
//! shells over `core`: `ry_emit_build_error_from_runtime` resolves the type
//! handle and interns the aggregate; `ry_emit_result_branch` validates inputs,
//! resolves `is_err` via `resolve_value` (returning sentinel 0 on a NULL
//! resolution before any IR is built), wraps each C builder callback
//! (`RyBuildValueFn`) into a `FnMut() -> Option<ValueRef>` closure that re-enters
//! the emitter and resolves the returned id via `resolve_value` (propagating
//! `None` up through `composite::result::emit_result_branch` to the abi shell's
//! `intern(NULL) → 0`), then delegates the IR emission to that core free function
//! and interns the phi. Every resolve site uses `resolve_value` to mirror the
//! defensive guard pattern shared by `bounds.rs:48`, `arc.rs:48`, `any.rs:93`,
//! `cow.rs:67`, `reduce.rs:37`, and the rest of the abi (#2251). The closure
//! invocation + intern/resolve juggling is the only abi-side work; the LLVM IR
//! (BB scaffold + cond-br + phi) lives in `core`, which holds no `&mut EmitCtx`
//! borrow so the re-entrant `with_ctx(ctx, …)` borrow in each closure does not
//! alias a live receiver across the extern call (#2069 resolves the #2060 split;
//! #2081 confined the raw reify behind `with_ctx`, making the no-borrow-across-
//! callback contract structural rather than dependent on argument evaluation
//! order). The builder-derived parent rule applies in `core` (#1968 / #1969).

use std::ffi::{c_char, c_void};

use crate::composite::result::emit_result_branch;
use crate::context::{TypeRef, ValueRef};

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
    if err_fn_name.is_null() || error_ty.is_null() {
        return 0;
    }
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    let result = c.build_error_from_runtime(err_fn_name, TypeRef(as_type(error_ty)));
    intern(c, to_ry_value(result.0))
}

/// Emit a `Result` branch: three BBs (res.ok / res.err / res.merge) joined by a
/// PHI, with `build_ok` / `build_err` (forwarded `user_ctx`) emitting each arm;
/// return the interned PHI. Returns 0 on NULL inputs. The three BBs' parent
/// function is derived from the builder's insert block, so the builder must be
/// positioned within a function.
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
    // Each transient context access goes through `with_ctx` (the raw reify `cx`
    // is now private to `abi::ctx_access`, #2081); these three mirror the former
    // separate `cx(ctx)` borrows 1:1, preserving the guard-before-resolve order.
    let context = with_ctx(ctx, |c| c.context);
    let b = with_ctx(ctx, |c| c.builder);
    if context.is_null() || b.is_null() {
        return 0;
    }
    // The explicit `unsafe` block covers the `with_ctx` closure body too (an
    // `unsafe fn`'s implicit body-unsafe does NOT reach into nested closures, but
    // a lexical `unsafe {}` block does), so the closure needs no inner `unsafe` —
    // matching the do_ok / do_err blocks below. `resolve_value → None` (sentinel
    // 0 / out-of-range / interned NULL) maps to the entry point's sentinel 0
    // before any IR is built, mirroring the abi-wide defensive guard pattern
    // (#2251).
    let Some(is_err) = (unsafe { with_ctx(ctx, |c| resolve_value(c, is_err_id)) }) else {
        return 0;
    };
    // Wrap each re-entrant C builder into a closure: call the callback (which
    // re-enters the emitter and emits the arm's value IR), THEN resolve the
    // returned id in a fresh `with_ctx`. The two-step is now STRUCTURAL — the C
    // builder runs entirely OUTSIDE any `with_ctx`, so no `&mut EmitCtx` borrow
    // can span the re-entrant call. (The previous code achieved the same
    // ordering by relying on left-to-right argument evaluation: a single
    // `resolve(cx(ctx), build_ok(user_ctx))` would have held the `cx(ctx)` borrow
    // across the callback. `with_ctx` makes the ordering a borrow-scope guarantee
    // instead of an evaluation-order subtlety.) `composite::result::emit_result_branch` itself
    // holds no EmitCtx borrow, so each closure's transient `with_ctx` is the only
    // live receiver borrow at callback time (#2081 / #2069). Each closure
    // returns `Option<ValueRef>` so a callback-returned id that resolves to NULL
    // propagates up to `composite::result::emit_result_branch`, which returns
    // `ValueRef(null_mut())` for the abi shell's `intern(NULL) → 0` (#2251).
    let mut do_ok = move || -> Option<ValueRef> {
        unsafe {
            let ok_id = build_ok(user_ctx);
            with_ctx(ctx, |c| resolve_value(c, ok_id))
        }
    };
    let mut do_err = move || -> Option<ValueRef> {
        unsafe {
            let err_id = build_err(user_ctx);
            with_ctx(ctx, |c| resolve_value(c, err_id))
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
    unsafe { with_ctx(ctx, |c| intern(c, to_ry_value(phi.0))) }
}
