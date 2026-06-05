//! FFI surface — the locked C boundary type contract: opaque handle types,
//! scalar/enum typedefs, descriptor structs, and the compile-time layout
//! assertions (#1995). Every item here mirrors `include/ry/llvm_emit/api.h`
//! byte-for-byte — the C boundary is locked (#1949); do NOT alter without updating
//! api.h and cast_helpers.hpp.

use std::ffi::{c_char, c_int, c_void};

// =============================================================
// Opaque handle types (mirror api.h).
// =============================================================

#[repr(C)]
pub struct RyEmitCtx {
    _private: [u8; 0],
}

#[repr(C)]
pub struct RyModuleOpaque {
    _private: [u8; 0],
}
pub type RyModuleRef = *mut RyModuleOpaque;

#[repr(C)]
pub struct RyBuilderOpaque {
    _private: [u8; 0],
}
pub type RyBuilderRef = *mut RyBuilderOpaque;

#[repr(C)]
pub struct RyContextOpaque {
    _private: [u8; 0],
}
pub type RyContextRef = *mut RyContextOpaque;

#[repr(C)]
pub struct RyFunctionOpaque {
    _private: [u8; 0],
}
pub type RyFunctionRef = *mut RyFunctionOpaque;

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
// boundary struct-layout assertions (#1995).
//
// Compile-time checks that lock the Rust side of the boundary to the same
// sizeof / alignof / field-offset constants asserted on the C++ side in
// tests/test_abi_layout.cpp. Both sides assert against the SAME numbers,
// so any incidental drift (field reorder, padding, type-width change) on
// either side breaks the build. These `const _` items are evaluated even
// under `cargo check`, so CI's lint step exercises them without a full
// cdylib build. `core::mem::offset_of!` is const-evaluable since Rust
// 1.77. See docs/architecture/llvm-ir-emission-boundary.md for the
// verification model and the canonical value table.
// =============================================================

// --- Descriptor structs: sizeof + alignof + every field offset ---

// RyCowEnsureUniqueDesc (12 fields).
const _: () = assert!(core::mem::size_of::<RyCowEnsureUniqueDesc>() == 64);
const _: () = assert!(core::mem::align_of::<RyCowEnsureUniqueDesc>() == 8);
const _: () = assert!(core::mem::offset_of!(RyCowEnsureUniqueDesc, data_ptr_id) == 0);
const _: () = assert!(core::mem::offset_of!(RyCowEnsureUniqueDesc, slot_ptr_id) == 4);
const _: () = assert!(core::mem::offset_of!(RyCowEnsureUniqueDesc, kind) == 8);
const _: () = assert!(core::mem::offset_of!(RyCowEnsureUniqueDesc, atomic) == 12);
const _: () = assert!(core::mem::offset_of!(RyCowEnsureUniqueDesc, elem_size) == 16);
const _: () = assert!(core::mem::offset_of!(RyCowEnsureUniqueDesc, key_size) == 24);
const _: () = assert!(core::mem::offset_of!(RyCowEnsureUniqueDesc, val_size) == 32);
const _: () = assert!(core::mem::offset_of!(RyCowEnsureUniqueDesc, do_elem_retain) == 40);
const _: () = assert!(core::mem::offset_of!(RyCowEnsureUniqueDesc, elem_is_str) == 44);
const _: () = assert!(core::mem::offset_of!(RyCowEnsureUniqueDesc, do_key_retain) == 48);
const _: () = assert!(core::mem::offset_of!(RyCowEnsureUniqueDesc, key_is_str) == 52);
const _: () = assert!(core::mem::offset_of!(RyCowEnsureUniqueDesc, destructor_callee) == 56);

// RyAnyWrapDesc (9 fields).
const _: () = assert!(core::mem::size_of::<RyAnyWrapDesc>() == 56);
const _: () = assert!(core::mem::align_of::<RyAnyWrapDesc>() == 8);
const _: () = assert!(core::mem::offset_of!(RyAnyWrapDesc, kind) == 0);
const _: () = assert!(core::mem::offset_of!(RyAnyWrapDesc, target_tag) == 8);
const _: () = assert!(core::mem::offset_of!(RyAnyWrapDesc, val_id) == 16);
const _: () = assert!(core::mem::offset_of!(RyAnyWrapDesc, do_collection_retain) == 20);
const _: () = assert!(core::mem::offset_of!(RyAnyWrapDesc, do_str_retain) == 24);
const _: () = assert!(core::mem::offset_of!(RyAnyWrapDesc, descriptor_id) == 28);
const _: () = assert!(core::mem::offset_of!(RyAnyWrapDesc, box_layout_ty) == 32);
const _: () = assert!(core::mem::offset_of!(RyAnyWrapDesc, box_data_size) == 40);
const _: () = assert!(core::mem::offset_of!(RyAnyWrapDesc, any_ty) == 48);

// RyAnyUnwrapDesc (14 fields).
const _: () = assert!(core::mem::size_of::<RyAnyUnwrapDesc>() == 96);
const _: () = assert!(core::mem::align_of::<RyAnyUnwrapDesc>() == 8);
const _: () = assert!(core::mem::offset_of!(RyAnyUnwrapDesc, kind) == 0);
const _: () = assert!(core::mem::offset_of!(RyAnyUnwrapDesc, any_val_id) == 4);
const _: () = assert!(core::mem::offset_of!(RyAnyUnwrapDesc, any_ty) == 8);
const _: () = assert!(core::mem::offset_of!(RyAnyUnwrapDesc, target_ty) == 16);
const _: () = assert!(core::mem::offset_of!(RyAnyUnwrapDesc, expected_tag) == 24);
const _: () = assert!(core::mem::offset_of!(RyAnyUnwrapDesc, do_collection_retain) == 32);
const _: () = assert!(core::mem::offset_of!(RyAnyUnwrapDesc, do_str_retain) == 36);
const _: () = assert!(core::mem::offset_of!(RyAnyUnwrapDesc, mismatch_msg) == 40);
const _: () = assert!(core::mem::offset_of!(RyAnyUnwrapDesc, mismatch_global_name) == 48);
const _: () = assert!(core::mem::offset_of!(RyAnyUnwrapDesc, expected_desc_id) == 56);
const _: () = assert!(core::mem::offset_of!(RyAnyUnwrapDesc, box_layout_ty) == 64);
const _: () = assert!(core::mem::offset_of!(RyAnyUnwrapDesc, record_struct_ty) == 72);
const _: () = assert!(core::mem::offset_of!(RyAnyUnwrapDesc, desc_mismatch_msg) == 80);
const _: () = assert!(core::mem::offset_of!(RyAnyUnwrapDesc, desc_mismatch_global_name) == 88);

// RyAnyTryUnwrapDesc (10 fields).
const _: () = assert!(core::mem::size_of::<RyAnyTryUnwrapDesc>() == 64);
const _: () = assert!(core::mem::align_of::<RyAnyTryUnwrapDesc>() == 8);
const _: () = assert!(core::mem::offset_of!(RyAnyTryUnwrapDesc, kind) == 0);
const _: () = assert!(core::mem::offset_of!(RyAnyTryUnwrapDesc, any_val_id) == 4);
const _: () = assert!(core::mem::offset_of!(RyAnyTryUnwrapDesc, any_ty) == 8);
const _: () = assert!(core::mem::offset_of!(RyAnyTryUnwrapDesc, res_ty) == 16);
const _: () = assert!(core::mem::offset_of!(RyAnyTryUnwrapDesc, error_ty) == 24);
const _: () = assert!(core::mem::offset_of!(RyAnyTryUnwrapDesc, target_ty) == 32);
const _: () = assert!(core::mem::offset_of!(RyAnyTryUnwrapDesc, expected_tag) == 40);
const _: () = assert!(core::mem::offset_of!(RyAnyTryUnwrapDesc, do_collection_retain) == 48);
const _: () = assert!(core::mem::offset_of!(RyAnyTryUnwrapDesc, do_str_retain) == 52);
const _: () = assert!(core::mem::offset_of!(RyAnyTryUnwrapDesc, err_msg_str_id) == 56);

// --- Opaque handle typedefs: sizeof + alignof only (no fields) ---
// All are `*mut <Opaque>` -> 8 bytes / 8-byte aligned on 64-bit platforms.
const _: () =
    assert!(core::mem::size_of::<RyModuleRef>() == 8 && core::mem::align_of::<RyModuleRef>() == 8);
const _: () = assert!(
    core::mem::size_of::<RyBuilderRef>() == 8 && core::mem::align_of::<RyBuilderRef>() == 8
);
const _: () = assert!(
    core::mem::size_of::<RyContextRef>() == 8 && core::mem::align_of::<RyContextRef>() == 8
);
const _: () = assert!(
    core::mem::size_of::<RyFunctionRef>() == 8 && core::mem::align_of::<RyFunctionRef>() == 8
);
const _: () =
    assert!(core::mem::size_of::<RyTypeRef>() == 8 && core::mem::align_of::<RyTypeRef>() == 8);
const _: () = assert!(
    core::mem::size_of::<RyFuncTypeRef>() == 8 && core::mem::align_of::<RyFuncTypeRef>() == 8
);
const _: () =
    assert!(core::mem::size_of::<RyValueRef>() == 8 && core::mem::align_of::<RyValueRef>() == 8);
const _: () = assert!(
    core::mem::size_of::<RyBasicBlockRef>() == 8 && core::mem::align_of::<RyBasicBlockRef>() == 8
);

// --- Scalar intern-handle typedefs (u32) ---
const _: () =
    assert!(core::mem::size_of::<RyValueId>() == 4 && core::mem::align_of::<RyValueId>() == 4);
