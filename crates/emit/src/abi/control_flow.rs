//! abi::control_flow — C boundary entry points for control-flow emission. Each
//! resolves the u32 ids / translates opaque handles into the Rust-native engine
//! types, calls the `core` `EmitCtx` method, and interns any produced value
//! (intern / resolve are abi-side; the engine method never touches them).

use std::ffi::{c_char, c_uint};

use crate::context::{BasicBlockRef, FunctionRef, TypeRef, ValueRef};

use super::*;

/// Create a fresh basic block named `name` inside `fn_handle` and return its
/// opaque handle. NULL `name` is treated as empty.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_create_basic_block(
    ctx: *mut RyEmitCtx,
    name: *const c_char,
    fn_handle: RyFunctionRef,
) -> RyBasicBlockRef {
    // boundary input validation: malformed callers get a NULL block instead of
    // dereferencing a bad ctx or handing a NULL parent function to LLVM. `name`
    // is optional (NULL → empty).
    let Some(c) = checked_cx(ctx) else {
        return std::ptr::null_mut();
    };
    if fn_handle.is_null() {
        return std::ptr::null_mut();
    }
    let nm = if name.is_null() { c"".as_ptr() } else { name };
    to_ry_bb(
        c.create_basic_block(FunctionRef(as_function(fn_handle)), nm)
            .0,
    )
}

/// Emit a conditional branch on the interned i1 `cond` to `true_bb` / `false_bb`
/// at the builder's current insert point (insert point unchanged).
#[no_mangle]
pub unsafe extern "C" fn ry_emit_branch_cond(
    ctx: *mut RyEmitCtx,
    cond: RyValueId,
    true_bb: RyBasicBlockRef,
    false_bb: RyBasicBlockRef,
) {
    // boundary input validation: malformed callers are a no-op rather than
    // feeding a NULL cond / target to LLVMBuildCondBr.
    let Some(c) = checked_cx(ctx) else {
        return;
    };
    let Some(cond_val) = resolve_value(c, cond) else {
        return;
    };
    if true_bb.is_null() || false_bb.is_null() {
        return;
    }
    c.branch_cond(
        cond_val,
        BasicBlockRef(as_bb(true_bb)),
        BasicBlockRef(as_bb(false_bb)),
    );
}

/// Emit an unconditional branch to `target` at the builder's current insert
/// point (insert point unchanged).
#[no_mangle]
pub unsafe extern "C" fn ry_emit_branch_uncond(ctx: *mut RyEmitCtx, target: RyBasicBlockRef) {
    // boundary input validation: NULL ctx / target → no-op.
    let Some(c) = checked_cx(ctx) else {
        return;
    };
    if target.is_null() {
        return;
    }
    c.branch_uncond(BasicBlockRef(as_bb(target)));
}

/// Emit `ret val_id` at the builder's current insert point. `val_id == 0` (the
/// explicit sentinel) emits `ret void`; a non-zero id that fails to resolve is a
/// no-op — NOT a silent `ret void`, which would be invalid IR for a non-void
/// function. NULL ctx → no-op. The function-builder return terminator (#2098).
#[no_mangle]
pub unsafe extern "C" fn ry_emit_ret(ctx: *mut RyEmitCtx, val_id: RyValueId) {
    let Some(c) = checked_cx(ctx) else {
        return;
    };
    // `ret void` is gated strictly on the 0 sentinel; an unresolvable non-zero id
    // is malformed input → no-op (do not coerce it to `ret void`).
    let val = if val_id == 0 {
        None
    } else {
        let Some(v) = resolve_value(c, val_id) else {
            return;
        };
        Some(v)
    };
    c.build_ret(val);
}

/// Emit a PHI of type `ty` from `count` parallel (value, block) incoming pairs
/// (`incoming_values` / `incoming_blocks`) at the builder's current insert point;
/// return the interned PHI handle. NULL `name_hint` is treated as empty.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_create_phi(
    ctx: *mut RyEmitCtx,
    ty: RyTypeRef,
    incoming_values: *const RyValueId,
    incoming_blocks: *const RyBasicBlockRef,
    count: u32,
    name_hint: *const c_char,
) -> RyValueId {
    // boundary input validation: malformed callers get sentinel 0. The parallel
    // incoming arrays are borrowed through `ffi_slice` (rejecting a NULL base for
    // count > 0 before the per-edge index), and each edge's value must resolve
    // non-NULL with a non-NULL block. `name_hint` is optional (NULL → empty).
    let Some(c) = checked_cx(ctx) else {
        return 0;
    };
    if ty.is_null() {
        return 0;
    }
    let (Some(values), Some(blocks)) = (
        ffi_slice(incoming_values, count),
        ffi_slice(incoming_blocks, count),
    ) else {
        return 0;
    };
    let nm = if name_hint.is_null() {
        c"".as_ptr()
    } else {
        name_hint
    };
    let mut edges: Vec<(ValueRef, BasicBlockRef)> = Vec::with_capacity(count as usize);
    for (&val_id, &block) in values.iter().zip(blocks.iter()) {
        let Some(v) = resolve_value(c, val_id) else {
            return 0;
        };
        let bb = as_bb(block);
        if bb.is_null() {
            return 0;
        }
        edges.push((v, BasicBlockRef(bb)));
    }
    let phi = c.create_phi(TypeRef(as_type(ty)), &edges, nm);
    intern(c, to_ry_value(phi.0))
}

/// Build `switch val, default_bb [num_cases]` at the builder's current insert
/// point and return the opaque RySwitchRef. The handle is NOT interned (it mirrors
/// ry_emit_create_basic_block's RyBasicBlockRef shape); each case is added via
/// ry_emit_switch_add_case. NULL ctx / default_bb, or an unresolvable val, → NULL.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_create_switch(
    ctx: *mut RyEmitCtx,
    val: RyValueId,
    default_bb: RyBasicBlockRef,
    num_cases: c_uint,
) -> RySwitchRef {
    // boundary input validation: malformed callers get a NULL switch instead of
    // dereferencing a bad ctx or handing a NULL default block to LLVM.
    let Some(c) = checked_cx(ctx) else {
        return std::ptr::null_mut();
    };
    let Some(val_v) = resolve_value(c, val) else {
        return std::ptr::null_mut();
    };
    if default_bb.is_null() {
        return std::ptr::null_mut();
    }
    to_ry_switch(
        c.build_switch(val_v, BasicBlockRef(as_bb(default_bb)), num_cases)
            .0,
    )
}

/// Append a case `on_val -> dest_bb` to `sw` (`LLVMAddCase`). NULL ctx / sw /
/// dest_bb, or an unresolvable on_val, → no-op.
#[no_mangle]
pub unsafe extern "C" fn ry_emit_switch_add_case(
    ctx: *mut RyEmitCtx,
    sw: RySwitchRef,
    on_val: RyValueId,
    dest_bb: RyBasicBlockRef,
) {
    // boundary input validation: malformed callers are a no-op rather than
    // feeding a NULL switch / case value / target to LLVMAddCase.
    let Some(c) = checked_cx(ctx) else {
        return;
    };
    if sw.is_null() || dest_bb.is_null() {
        return;
    }
    let Some(on_val_v) = resolve_value(c, on_val) else {
        return;
    };
    c.switch_add_case(
        ValueRef(as_switch(sw)),
        on_val_v,
        BasicBlockRef(as_bb(dest_bb)),
    );
}
