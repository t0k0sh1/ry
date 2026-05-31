// Rust implementation of the LLVM IR emission ABI defined in
// `include/ry/llvm_emit/api.h` (issue #1950, follow-up to #1949).
//
// Every type and function here mirrors a declaration in api.h
// byte-for-byte. The C ABI is locked by #1949 — do NOT widen or alter
// the public signatures here without updating api.h and the
// reinterpret_cast wrappers in include/ry/llvm_emit/cast_helpers.hpp.
//
// All functions are stubs (`unimplemented!()`) until Task 4 of the
// #1950 implementation plan ports the bodies from src/llvm_emit/impl.cpp.
// Building the cdylib at this point verifies that:
//   - the 27 ABI symbols are exported (`nm -D` / `otool -L`),
//   - the FFI types compile against api.h (sizeof / repr alignment),
//   - corrosion-rs is wired correctly into the CMake build,
//   - `-undefined dynamic_lookup` link arg flows through on macOS,
//   - llvm-sys = "211" finds LLVM 21 via LLVM_SYS_211_PREFIX.
// Tests of course fail with `unimplemented!()` panics — that is
// expected during Task 3.

#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(clippy::missing_safety_doc)]

use std::ffi::{c_char, c_int, c_void};

// =============================================================
// Opaque handle types (mirrors api.h Stage 2-C / #1973 typedefs).
// =============================================================

#[repr(C)]
pub struct RyEmitCtx {
    _private: [u8; 0],
}

#[repr(C)]
pub struct RyModuleOpaque {
    _private: [u8; 0],
}
pub type RyModuleHandle = *mut RyModuleOpaque;

#[repr(C)]
pub struct RyBuilderOpaque {
    _private: [u8; 0],
}
pub type RyBuilderHandle = *mut RyBuilderOpaque;

#[repr(C)]
pub struct RyContextOpaque {
    _private: [u8; 0],
}
pub type RyContextHandle = *mut RyContextOpaque;

#[repr(C)]
pub struct RyFunctionOpaque {
    _private: [u8; 0],
}
pub type RyFunctionHandle = *mut RyFunctionOpaque;

#[repr(C)]
pub struct RyTypeOpaque {
    _private: [u8; 0],
}
pub type RyTypeRef = *mut RyTypeOpaque;

#[repr(C)]
pub struct RyFuncTypeOpaque {
    _private: [u8; 0],
}
pub type RyFuncTypeRef = *mut RyFuncTypeOpaque;

#[repr(C)]
pub struct RyValueOpaque {
    _private: [u8; 0],
}
pub type RyValueRef = *mut RyValueOpaque;

#[repr(C)]
pub struct RyBasicBlockOpaque {
    _private: [u8; 0],
}
pub type RyBasicBlockRef = *mut RyBasicBlockOpaque;

// =============================================================
// Scalar typedefs and enum constants.
// =============================================================

pub type RyValueId = u32;
pub type RyBasicBlockId = u32;

pub const RY_BOUNDS_LIST: c_int = 0;
pub const RY_BOUNDS_ARRAY: c_int = 1;

pub const RY_ARC_NONATOMIC: c_int = 0;
pub const RY_ARC_ATOMIC: c_int = 1;

pub const RY_COW_LIST: c_int = 0;
pub const RY_COW_MAP: c_int = 1;
pub const RY_COW_SET: c_int = 2;

// Callback for ok/err value builders consumed by ry_emit_result_branch.
// Wrapped in Option<...> so the FFI representation is a nullable
// function pointer matching the C typedef.
pub type RyBuildValueFn = Option<unsafe extern "C" fn(user_ctx: *mut c_void) -> RyValueId>;

// =============================================================
// Descriptor structs (mirror api.h verbatim — field order, types,
// and layout MUST match the C declarations).
// =============================================================

#[repr(C)]
pub struct RyCowEnsureUniqueDesc {
    pub data_ptr_id: RyValueId,
    pub slot_ptr_id: RyValueId,
    pub kind: c_int,
    pub atomic: c_int,
    pub elem_size: u64,
    pub key_size: u64,
    pub val_size: u64,
    pub do_elem_retain: c_int,
    pub elem_is_str: c_int,
    pub do_key_retain: c_int,
    pub key_is_str: c_int,
    pub destructor_callee: RyValueRef,
}

#[repr(C)]
pub struct RyAnyWrapDesc {
    pub kind: c_int,
    pub target_tag: i64,
    pub val_id: RyValueId,
    pub do_collection_retain: c_int,
    pub do_str_retain: c_int,
    pub descriptor_id: RyValueId,
    pub box_layout_ty: RyTypeRef,
    pub box_data_size: u64,
    pub any_ty: RyTypeRef,
}

#[repr(C)]
pub struct RyAnyUnwrapDesc {
    pub kind: c_int,
    pub any_val_id: RyValueId,
    pub any_ty: RyTypeRef,
    pub target_ty: RyTypeRef,
    pub expected_tag: i64,
    pub do_collection_retain: c_int,
    pub do_str_retain: c_int,
    pub mismatch_msg: *const c_char,
    pub mismatch_global_name: *const c_char,
    pub expected_desc_id: RyValueId,
    pub box_layout_ty: RyTypeRef,
    pub record_struct_ty: RyTypeRef,
    pub desc_mismatch_msg: *const c_char,
    pub desc_mismatch_global_name: *const c_char,
}

#[repr(C)]
pub struct RyAnyTryUnwrapDesc {
    pub kind: c_int,
    pub any_val_id: RyValueId,
    pub any_ty: RyTypeRef,
    pub res_ty: RyTypeRef,
    pub error_ty: RyTypeRef,
    pub target_ty: RyTypeRef,
    pub expected_tag: i64,
    pub do_collection_retain: c_int,
    pub do_str_retain: c_int,
    pub err_msg_str_id: RyValueId,
}

// =============================================================
// ABI function stubs (27 total). All bodies are `unimplemented!()`
// until Task 4 of #1950 implementation plan.
//
// Grouped to match the migration phases documented in api.h:
//   - Lifecycle / intern / resolve (5)
//   - Stage 2-B helpers           (6)
//   - Stage 2-C Option            (2)
//   - Stage 2-C ARC               (2)
//   - Stage 2-C RuntimeCall       (1)
//   - Stage 2-C Collection        (4)
//   - Stage 2-C CoW               (1)
//   - Stage 2-C Any               (3)
//   - Stage 2-C ControlFlow       (4) — total 28 entries, ControlFlow
//                                      contributes 4; the count of
//                                      distinct ABI symbols is 27 once
//                                      duplicate basicblock entries are
//                                      removed (RyBasicBlockId is a type,
//                                      not a function).
// =============================================================

// ----- Lifecycle / intern / resolve --------------------------

#[no_mangle]
pub unsafe extern "C" fn ry_emit_ctx_create(
    _module: RyModuleHandle,
    _builder: RyBuilderHandle,
    _context: RyContextHandle,
    _function: RyFunctionHandle,
) -> *mut RyEmitCtx {
    unimplemented!("ry_emit_ctx_create — pending Task 4 (#1950)")
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_ctx_destroy(_ctx: *mut RyEmitCtx) {
    unimplemented!("ry_emit_ctx_destroy — pending Task 4 (#1950)")
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_ctx_set_function(
    _ctx: *mut RyEmitCtx,
    _function: RyFunctionHandle,
) {
    unimplemented!("ry_emit_ctx_set_function — pending Task 4 (#1950)")
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_intern(_ctx: *mut RyEmitCtx, _value: RyValueRef) -> RyValueId {
    unimplemented!("ry_emit_intern — pending Task 4 (#1950)")
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_resolve(_ctx: *mut RyEmitCtx, _id: RyValueId) -> RyValueRef {
    unimplemented!("ry_emit_resolve — pending Task 4 (#1950)")
}

// ----- Stage 2-B helpers -------------------------------------

#[no_mangle]
pub unsafe extern "C" fn ry_emit_build_error_from_runtime(
    _ctx: *mut RyEmitCtx,
    _err_fn_name: *const c_char,
    _error_ty: RyTypeRef,
) -> RyValueId {
    unimplemented!("ry_emit_build_error_from_runtime — pending Task 4 (#1950)")
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_get_runtime_fn(
    _ctx: *mut RyEmitCtx,
    _name: *const c_char,
    _fn_ty: RyFuncTypeRef,
) -> RyValueRef {
    unimplemented!("ry_emit_get_runtime_fn — pending Task 4 (#1950)")
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_bounds_check(
    _ctx: *mut RyEmitCtx,
    _idx_id: RyValueId,
    _len_id: RyValueId,
    _kind: c_int,
    _global_name: *const c_char,
    _bb_prefix: *const c_char,
) -> RyValueId {
    unimplemented!("ry_emit_bounds_check — pending Task 4 (#1950)")
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_result_branch(
    _ctx: *mut RyEmitCtx,
    _is_err_id: RyValueId,
    _res_ty: RyTypeRef,
    _build_ok: RyBuildValueFn,
    _build_err: RyBuildValueFn,
    _user_ctx: *mut c_void,
) -> RyValueId {
    unimplemented!("ry_emit_result_branch — pending Task 4 (#1950)")
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_negative_index_wrap(
    _ctx: *mut RyEmitCtx,
    _idx_id: RyValueId,
    _wrap_base_id: RyValueId,
    _prefix: *const c_char,
) -> RyValueId {
    unimplemented!("ry_emit_negative_index_wrap — pending Task 4 (#1950)")
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_bounds_error(
    _ctx: *mut RyEmitCtx,
    _orig_idx_id: RyValueId,
    _len_id: RyValueId,
    _fmt_msg: *const c_char,
    _global_name: *const c_char,
) {
    unimplemented!("ry_emit_bounds_error — pending Task 4 (#1950)")
}

// ----- Stage 2-C Option --------------------------------------

#[no_mangle]
pub unsafe extern "C" fn ry_emit_option_wrap_some(
    _ctx: *mut RyEmitCtx,
    _inner_id: RyValueId,
    _opt_ty: RyTypeRef,
) -> RyValueId {
    unimplemented!("ry_emit_option_wrap_some — pending Task 4 (#1950)")
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_option_wrap_none(
    _ctx: *mut RyEmitCtx,
    _opt_ty: RyTypeRef,
) -> RyValueId {
    unimplemented!("ry_emit_option_wrap_none — pending Task 4 (#1950)")
}

// ----- Stage 2-C ARC -----------------------------------------

#[no_mangle]
pub unsafe extern "C" fn ry_emit_arc_retain(
    _ctx: *mut RyEmitCtx,
    _header_ptr_id: RyValueId,
    _atomic: c_int,
) {
    unimplemented!("ry_emit_arc_retain — pending Task 4 (#1950)")
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_arc_release(
    _ctx: *mut RyEmitCtx,
    _header_ptr_id: RyValueId,
    _atomic: c_int,
    _destructor_callee: RyValueRef,
    _gc_visit_fn: RyValueRef,
) {
    unimplemented!("ry_emit_arc_release — pending Task 4 (#1950)")
}

// ----- Stage 2-C RuntimeCall ---------------------------------

#[no_mangle]
pub unsafe extern "C" fn ry_emit_runtime_call(
    _ctx: *mut RyEmitCtx,
    _name: *const c_char,
    _ret_ty: RyTypeRef,
    _arg_tys: *const RyTypeRef,
    _arg_ty_count: u32,
    _arg_ids: *const RyValueId,
    _arg_count: u32,
    _name_hint: *const c_char,
) -> RyValueId {
    unimplemented!("ry_emit_runtime_call — pending Task 4 (#1950)")
}

// ----- Stage 2-C Collection ----------------------------------

#[no_mangle]
pub unsafe extern "C" fn ry_emit_collection_append(
    _ctx: *mut RyEmitCtx,
    _list_ptr_id: RyValueId,
    _val_id: RyValueId,
    _list_header_ty: RyTypeRef,
    _elem_ty: RyTypeRef,
    _elem_size: u64,
) {
    unimplemented!("ry_emit_collection_append — pending Task 4 (#1950)")
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_collection_insert(
    _ctx: *mut RyEmitCtx,
    _list_ptr_id: RyValueId,
    _idx_id: RyValueId,
    _val_id: RyValueId,
    _list_header_ty: RyTypeRef,
    _elem_ty: RyTypeRef,
    _elem_size: u64,
) {
    unimplemented!("ry_emit_collection_insert — pending Task 4 (#1950)")
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_collection_remove_at(
    _ctx: *mut RyEmitCtx,
    _list_ptr_id: RyValueId,
    _idx_id: RyValueId,
    _list_header_ty: RyTypeRef,
    _elem_ty: RyTypeRef,
    _elem_size: u64,
) -> RyValueId {
    unimplemented!("ry_emit_collection_remove_at — pending Task 4 (#1950)")
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_list_slice(
    _ctx: *mut RyEmitCtx,
    _list_ptr_id: RyValueId,
    _start_id: RyValueId,
    _end_excl_id: RyValueId,
    _list_header_ty: RyTypeRef,
    _elem_ty: RyTypeRef,
    _elem_size: u64,
    _out_count: *mut RyValueId,
    _out_new_data: *mut RyValueId,
) {
    unimplemented!("ry_emit_list_slice — pending Task 4 (#1950)")
}

// ----- Stage 2-C CoW -----------------------------------------

#[no_mangle]
pub unsafe extern "C" fn ry_emit_cow_ensure_unique(
    _ctx: *mut RyEmitCtx,
    _desc: *const RyCowEnsureUniqueDesc,
) -> RyValueId {
    unimplemented!("ry_emit_cow_ensure_unique — pending Task 4 (#1950)")
}

// ----- Stage 2-C Any -----------------------------------------

#[no_mangle]
pub unsafe extern "C" fn ry_emit_any_wrap(
    _ctx: *mut RyEmitCtx,
    _desc: *const RyAnyWrapDesc,
) -> RyValueId {
    unimplemented!("ry_emit_any_wrap — pending Task 4 (#1950)")
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_any_unwrap(
    _ctx: *mut RyEmitCtx,
    _desc: *const RyAnyUnwrapDesc,
) -> RyValueId {
    unimplemented!("ry_emit_any_unwrap — pending Task 4 (#1950)")
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_any_try_unwrap(
    _ctx: *mut RyEmitCtx,
    _desc: *const RyAnyTryUnwrapDesc,
) -> RyValueId {
    unimplemented!("ry_emit_any_try_unwrap — pending Task 4 (#1950)")
}

// ----- Stage 2-C ControlFlow ---------------------------------

#[no_mangle]
pub unsafe extern "C" fn ry_emit_create_basic_block(
    _ctx: *mut RyEmitCtx,
    _name: *const c_char,
    _fn_handle: RyFunctionHandle,
) -> RyBasicBlockRef {
    unimplemented!("ry_emit_create_basic_block — pending Task 4 (#1950)")
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_branch_cond(
    _ctx: *mut RyEmitCtx,
    _cond: RyValueId,
    _true_bb: RyBasicBlockRef,
    _false_bb: RyBasicBlockRef,
) {
    unimplemented!("ry_emit_branch_cond — pending Task 4 (#1950)")
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_branch_uncond(_ctx: *mut RyEmitCtx, _target: RyBasicBlockRef) {
    unimplemented!("ry_emit_branch_uncond — pending Task 4 (#1950)")
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_create_phi(
    _ctx: *mut RyEmitCtx,
    _ty: RyTypeRef,
    _incoming_values: *const RyValueId,
    _incoming_blocks: *const RyBasicBlockRef,
    _count: u32,
    _name_hint: *const c_char,
) -> RyValueId {
    unimplemented!("ry_emit_create_phi — pending Task 4 (#1950)")
}
