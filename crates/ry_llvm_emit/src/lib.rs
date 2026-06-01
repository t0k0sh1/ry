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

use std::collections::HashMap;
use std::ffi::{c_char, c_int, c_void, CStr, CString};

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::{LLVMABISizeOfType, LLVMGetModuleDataLayout};
use llvm_sys::{
    LLVMAtomicOrdering, LLVMAtomicRMWBinOp, LLVMIntPredicate, LLVMLinkage, LLVMTypeKind,
    LLVMUnnamedAddr,
};

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
// ABI struct-layout assertions (#1995).
//
// Compile-time checks that lock the Rust side of the ABI to the same
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
// All are `*mut <Opaque>` -> 8 bytes / 8-byte aligned on the 64-bit ABI.
const _: () = assert!(core::mem::size_of::<RyModuleHandle>() == 8 && core::mem::align_of::<RyModuleHandle>() == 8);
const _: () = assert!(core::mem::size_of::<RyBuilderHandle>() == 8 && core::mem::align_of::<RyBuilderHandle>() == 8);
const _: () = assert!(core::mem::size_of::<RyContextHandle>() == 8 && core::mem::align_of::<RyContextHandle>() == 8);
const _: () = assert!(core::mem::size_of::<RyFunctionHandle>() == 8 && core::mem::align_of::<RyFunctionHandle>() == 8);
const _: () = assert!(core::mem::size_of::<RyTypeRef>() == 8 && core::mem::align_of::<RyTypeRef>() == 8);
const _: () = assert!(core::mem::size_of::<RyFuncTypeRef>() == 8 && core::mem::align_of::<RyFuncTypeRef>() == 8);
const _: () = assert!(core::mem::size_of::<RyValueRef>() == 8 && core::mem::align_of::<RyValueRef>() == 8);
const _: () = assert!(core::mem::size_of::<RyBasicBlockRef>() == 8 && core::mem::align_of::<RyBasicBlockRef>() == 8);

// --- Scalar intern-handle typedefs (u32) ---
const _: () = assert!(core::mem::size_of::<RyValueId>() == 4 && core::mem::align_of::<RyValueId>() == 4);
const _: () = assert!(core::mem::size_of::<RyBasicBlockId>() == 4 && core::mem::align_of::<RyBasicBlockId>() == 4);

// =============================================================
// Internal helpers (issue #1997). The 28 ABI bodies below are ported
// from src/llvm_emit/impl.cpp to the LLVM C API (llvm-sys), producing
// byte-for-byte equivalent IR. These mirror impl.cpp's anonymous-namespace
// helpers and cast wrappers.
// =============================================================

// Concrete object behind the opaque `*mut RyEmitCtx` handle (mirrors the
// C++ RyEmitCtx struct in impl.cpp). values[0] is the null sentinel.
struct EmitCtxImpl {
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    context: LLVMContextRef,
    function: LLVMValueRef,
    values: Vec<LLVMValueRef>,
    // Dedup cache for ry_emit_bounds_error / any-error fmt-string globals
    // (keyed by message bytes; mirrors impl.cpp bounds_msg_cache).
    bounds_msg_cache: HashMap<Vec<u8>, LLVMValueRef>,
}

#[inline]
unsafe fn cx<'a>(p: *mut RyEmitCtx) -> &'a mut EmitCtxImpl {
    &mut *(p as *mut EmitCtxImpl)
}

// Opaque ABI handle → llvm-sys C API ref (pointer cast, this crate only).
#[inline]
fn as_type(p: RyTypeRef) -> LLVMTypeRef {
    p as LLVMTypeRef
}
#[inline]
fn as_functype(p: RyFuncTypeRef) -> LLVMTypeRef {
    p as LLVMTypeRef
}
#[inline]
fn as_value(p: RyValueRef) -> LLVMValueRef {
    p as LLVMValueRef
}
#[inline]
fn as_function(p: RyFunctionHandle) -> LLVMValueRef {
    p as LLVMValueRef
}
#[inline]
fn as_bb(p: RyBasicBlockRef) -> LLVMBasicBlockRef {
    p as LLVMBasicBlockRef
}
#[inline]
fn to_ry_value(v: LLVMValueRef) -> RyValueRef {
    v as RyValueRef
}
#[inline]
fn to_ry_bb(b: LLVMBasicBlockRef) -> RyBasicBlockRef {
    b as RyBasicBlockRef
}

// LLVM type accessors.
#[inline]
unsafe fn i1_type(c: LLVMContextRef) -> LLVMTypeRef {
    LLVMInt1TypeInContext(c)
}
#[inline]
unsafe fn i32_type(c: LLVMContextRef) -> LLVMTypeRef {
    LLVMInt32TypeInContext(c)
}
#[inline]
unsafe fn i64_type(c: LLVMContextRef) -> LLVMTypeRef {
    LLVMInt64TypeInContext(c)
}
#[inline]
unsafe fn ptr_type(c: LLVMContextRef) -> LLVMTypeRef {
    LLVMPointerTypeInContext(c, 0)
}
#[inline]
unsafe fn void_type(c: LLVMContextRef) -> LLVMTypeRef {
    LLVMVoidTypeInContext(c)
}

// Bridge llvm::Value* ↔ RyValueId handle space (mirror impl.cpp intern/resolve).
// Internal forms take &mut/&EmitCtxImpl so callers already holding the borrow
// do not re-alias through the public ABI entry points.
#[inline]
unsafe fn intern(c: &mut EmitCtxImpl, value: RyValueRef) -> RyValueId {
    if value.is_null() {
        return 0;
    }
    let id = c.values.len() as RyValueId;
    c.values.push(as_value(value));
    id
}

#[inline]
unsafe fn resolve(c: &EmitCtxImpl, id: RyValueId) -> RyValueRef {
    if id == 0 || id as usize >= c.values.len() {
        return std::ptr::null_mut();
    }
    to_ry_value(c.values[id as usize])
}

// Borrow C-string bytes without the NUL (empty slice on NULL).
#[inline]
unsafe fn cstr_bytes<'a>(p: *const c_char) -> &'a [u8] {
    if p.is_null() {
        b""
    } else {
        CStr::from_ptr(p).to_bytes()
    }
}

// Build a NUL-terminated CString name from a prefix + suffix (for SSA names
// like "{prefix}_idx"); mirrors impl.cpp's `p + "_idx"` Twine concatenation.
#[inline]
fn cname_pfx(prefix: &[u8], suffix: &[u8]) -> CString {
    let mut v = Vec::with_capacity(prefix.len() + suffix.len());
    v.extend_from_slice(prefix);
    v.extend_from_slice(suffix);
    CString::new(v).unwrap()
}

// Three-part CString name (e.g. "cow_" + tag + "_len_ptr"); mirrors impl.cpp
// `std::string("cow_") + tag + "_len_ptr"` in the CoW retain loops.
#[inline]
fn cname3(a: &[u8], b: &[u8], c: &[u8]) -> CString {
    let mut v = Vec::with_capacity(a.len() + b.len() + c.len());
    v.extend_from_slice(a);
    v.extend_from_slice(b);
    v.extend_from_slice(c);
    CString::new(v).unwrap()
}

// Module::getOrInsertFunction equivalent: reuse the existing same-named
// declaration if present (LLVM uniques functions by name; under opaque
// pointers the call type is supplied to LLVMBuildCall2, so a name hit is
// reused without a duplicate `declare`), else add a new declaration.
#[inline]
unsafe fn get_or_insert_function(
    module: LLVMModuleRef,
    name: *const c_char,
    fn_ty: LLVMTypeRef,
) -> LLVMValueRef {
    let existing = LLVMGetNamedFunction(module, name);
    if existing.is_null() {
        LLVMAddFunction(module, name, fn_ty)
    } else {
        existing
    }
}

// Module::getOrInsertGlobal equivalent.
#[inline]
unsafe fn get_or_insert_global(
    module: LLVMModuleRef,
    name: *const c_char,
    ty: LLVMTypeRef,
) -> LLVMValueRef {
    let existing = LLVMGetNamedGlobal(module, name);
    if existing.is_null() {
        LLVMAddGlobal(module, ty, name)
    } else {
        existing
    }
}

// Mirror of impl.cpp getOrCreateMsgGlobal / ry_emit_bounds_error inline dedup:
// a private, unnamed_addr, align-1 constant string global, deduped by message
// bytes within this ctx. `name` is the already-defaulted global name.
unsafe fn get_or_create_msg_global(
    c: &mut EmitCtxImpl,
    msg: &[u8],
    name: *const c_char,
) -> LLVMValueRef {
    if let Some(&gv) = c.bounds_msg_cache.get(msg) {
        return gv;
    }
    // ConstantDataArray::getString adds a trailing NUL (dont_null_terminate=0).
    let str_data =
        LLVMConstStringInContext2(c.context, msg.as_ptr() as *const c_char, msg.len(), 0);
    let gv = LLVMAddGlobal(c.module, LLVMTypeOf(str_data), name);
    LLVMSetLinkage(gv, LLVMLinkage::LLVMPrivateLinkage);
    LLVMSetInitializer(gv, str_data);
    LLVMSetGlobalConstant(gv, 1);
    LLVMSetUnnamedAddress(gv, LLVMUnnamedAddr::LLVMGlobalUnnamedAddr);
    LLVMSetAlignment(gv, 1);
    c.bounds_msg_cache.insert(msg.to_vec(), gv);
    gv
}

// ARC live-count counter address (mirrors the extern in src/codegen_arc.cpp).
extern "C" {
    fn __ry_arc_counter_address() -> *mut i64;
}

// Layout constants (mirror include/ry/ry_layout.hpp).
const ARC_HEADER_SIZE: u64 = 16;
const ARC_IMMORTAL: i64 = i64::MAX;

#[inline]
unsafe fn i8_type(c: LLVMContextRef) -> LLVMTypeRef {
    LLVMInt8TypeInContext(c)
}

// Anonymous {i64, i64} ARC header struct (matches impl.cpp StructType::get).
#[inline]
unsafe fn arc_header_type(c: LLVMContextRef) -> LLVMTypeRef {
    let mut elems = [i64_type(c), i64_type(c)];
    LLVMStructTypeInContext(c, elems.as_mut_ptr(), 2, 0)
}

// Mirror of CodeGen::emitArcCounterDeltaIR: inttoptr the process-global ARC
// live-count address (captured at JIT compile time) and atomicrmw add `delta`.
unsafe fn emit_arc_counter_delta(
    builder: LLVMBuilderRef,
    i64_ty: LLVMTypeRef,
    ptr_ty: LLVMTypeRef,
    delta: i64,
) {
    let addr = __ry_arc_counter_address() as usize as u64;
    let ctr_addr_const = LLVMConstInt(i64_ty, addr, 0);
    let ctr_ptr = LLVMBuildIntToPtr(builder, ctr_addr_const, ptr_ty, c"arc_ctr".as_ptr());
    LLVMBuildAtomicRMW(
        builder,
        LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpAdd,
        ctr_ptr,
        LLVMConstInt(i64_ty, delta as u64, 0),
        LLVMAtomicOrdering::LLVMAtomicOrderingMonotonic,
        0,
    );
}

// Mirror of CodeGen::emitAtomicI64Load: NotAtomic ordering uses a plain load
// (ABI-default alignment) to match the non-atomic codepath byte-for-byte;
// otherwise an 8-byte-aligned atomic load with the given ordering.
unsafe fn emit_atomic_i64_load(
    builder: LLVMBuilderRef,
    i64_ty: LLVMTypeRef,
    ptr: LLVMValueRef,
    ordering: LLVMAtomicOrdering,
    name: *const c_char,
) -> LLVMValueRef {
    let ld = LLVMBuildLoad2(builder, i64_ty, ptr, name);
    if ordering != LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic {
        LLVMSetAlignment(ld, 8);
        LLVMSetOrdering(ld, ordering);
    }
    ld
}

// Mirror of CodeGen::loadListHeader: the same {len, cap, data} GEPs + loads as
// impl.cpp loadListHeaderImpl. `prefix` concatenates with _len_ptr / _cap_ptr /
// _data_ptr / _len / _cap / _data.
struct ListHeaderLoad {
    len_ptr: LLVMValueRef,
    cap_ptr: LLVMValueRef,
    data_ptr: LLVMValueRef,
    len: LLVMValueRef,
    cap: LLVMValueRef,
    data: LLVMValueRef,
}

unsafe fn load_list_header(
    b: LLVMBuilderRef,
    list_header_ty: LLVMTypeRef,
    list_ptr: LLVMValueRef,
    i64_ty: LLVMTypeRef,
    ptr_ty: LLVMTypeRef,
    prefix: &[u8],
) -> ListHeaderLoad {
    let len_ptr =
        LLVMBuildStructGEP2(b, list_header_ty, list_ptr, 0, cname_pfx(prefix, b"_len_ptr").as_ptr());
    let cap_ptr =
        LLVMBuildStructGEP2(b, list_header_ty, list_ptr, 1, cname_pfx(prefix, b"_cap_ptr").as_ptr());
    let data_ptr =
        LLVMBuildStructGEP2(b, list_header_ty, list_ptr, 2, cname_pfx(prefix, b"_data_ptr").as_ptr());
    let len = LLVMBuildLoad2(b, i64_ty, len_ptr, cname_pfx(prefix, b"_len").as_ptr());
    let cap = LLVMBuildLoad2(b, i64_ty, cap_ptr, cname_pfx(prefix, b"_cap").as_ptr());
    let data = LLVMBuildLoad2(b, ptr_ty, data_ptr, cname_pfx(prefix, b"_data").as_ptr());
    ListHeaderLoad {
        len_ptr,
        cap_ptr,
        data_ptr,
        len,
        cap,
        data,
    }
}

// String header size (mirror include/ry/ry_layout.hpp STRING_HEADER_SIZE).
const STRING_HEADER_SIZE: u64 = 24;
// RyAnyTag discriminants (mirror include/ry/ry_layout.hpp RyAnyTag).
const RY_ANY_TAG_INT: i64 = 0;
const RY_ANY_TAG_FLOAT: i64 = 1;

// Mirror of CodeGen::emitRuntimeError (impl.cpp emitInlineRuntimeError): emits
// fprintf(stderr, msg) + fflush + _Exit(1) + unreachable, terminating the block.
// Caller must pre-split into err / ok BBs (emitRuntimeError-terminates rule).
unsafe fn emit_inline_runtime_error(c: &mut EmitCtxImpl, msg: &[u8], name_hint: *const c_char) {
    let b = c.builder;
    let context = c.context;
    let module = c.module;
    let ptr_ty = ptr_type(context);
    let i32_ty = i32_type(context);
    let void_ty = void_type(context);
    let (stdout_name, stderr_name) = if cfg!(target_os = "macos") {
        (c"__stdoutp".as_ptr(), c"__stderrp".as_ptr())
    } else {
        (c"stdout".as_ptr(), c"stderr".as_ptr())
    };
    let stderr_global = get_or_insert_global(module, stderr_name, ptr_ty);
    let stdout_global = get_or_insert_global(module, stdout_name, ptr_ty);
    let stderr_val = LLVMBuildLoad2(b, ptr_ty, stderr_global, c"stderr".as_ptr());
    let stdout_val = LLVMBuildLoad2(b, ptr_ty, stdout_global, c"stdout".as_ptr());
    let name = if name_hint.is_null() {
        c".any_err_msg".as_ptr()
    } else {
        name_hint
    };
    let err_msg = get_or_create_msg_global(c, msg, name);
    let mut fprintf_p = [ptr_ty, ptr_ty];
    let fprintf_ty = LLVMFunctionType(i32_ty, fprintf_p.as_mut_ptr(), 2, 1);
    let fprintf_fn = get_or_insert_function(module, c"fprintf".as_ptr(), fprintf_ty);
    let mut fprintf_a = [stderr_val, err_msg];
    LLVMBuildCall2(b, fprintf_ty, fprintf_fn, fprintf_a.as_mut_ptr(), 2, c"".as_ptr());
    let mut fflush_p = [ptr_ty];
    let fflush_ty = LLVMFunctionType(i32_ty, fflush_p.as_mut_ptr(), 1, 0);
    let fflush_fn = get_or_insert_function(module, c"fflush".as_ptr(), fflush_ty);
    let mut a_out = [stdout_val];
    LLVMBuildCall2(b, fflush_ty, fflush_fn, a_out.as_mut_ptr(), 1, c"".as_ptr());
    let mut a_err = [stderr_val];
    LLVMBuildCall2(b, fflush_ty, fflush_fn, a_err.as_mut_ptr(), 1, c"".as_ptr());
    let mut exit_p = [i32_ty];
    let exit_ty = LLVMFunctionType(void_ty, exit_p.as_mut_ptr(), 1, 0);
    let exit_fn = get_or_insert_function(module, c"_Exit".as_ptr(), exit_ty);
    let mut exit_a = [LLVMConstInt(i32_ty, 1, 0)];
    LLVMBuildCall2(b, exit_ty, exit_fn, exit_a.as_mut_ptr(), 1, c"".as_ptr());
    LLVMBuildUnreachable(b);
}

// Mirror of CodeGen::emitArcAlloc (impl.cpp emitInlineArcAlloc): malloc
// ARC_HEADER_SIZE + boxDataSize, bump the live-count (+1), init strong=1/weak=0,
// return the headerPtr (caller GEPs +ARC_HEADER_SIZE for the data pointer).
unsafe fn emit_inline_arc_alloc(c: &mut EmitCtxImpl, box_data_size: LLVMValueRef) -> LLVMValueRef {
    let b = c.builder;
    let context = c.context;
    let module = c.module;
    let i64_ty = i64_type(context);
    let ptr_ty = ptr_type(context);
    let arc_header_ty = arc_header_type(context);
    let header_size_c = LLVMConstInt(i64_ty, ARC_HEADER_SIZE, 0);
    let total_size = LLVMBuildAdd(b, header_size_c, box_data_size, c"arc_alloc_size".as_ptr());
    let mut malloc_p = [i64_ty];
    let malloc_ty = LLVMFunctionType(ptr_ty, malloc_p.as_mut_ptr(), 1, 0);
    let malloc_fn = get_or_insert_function(module, c"malloc".as_ptr(), malloc_ty);
    let mut malloc_a = [total_size];
    let header_ptr = LLVMBuildCall2(b, malloc_ty, malloc_fn, malloc_a.as_mut_ptr(), 1, c"arc_box".as_ptr());
    emit_arc_counter_delta(b, i64_ty, ptr_ty, 1);
    let strong_ptr = LLVMBuildStructGEP2(b, arc_header_ty, header_ptr, 0, c"arc_box_strong_ptr".as_ptr());
    LLVMBuildStore(b, LLVMConstInt(i64_ty, 1, 0), strong_ptr);
    let weak_ptr = LLVMBuildStructGEP2(b, arc_header_ty, header_ptr, 1, c"arc_box_weak_ptr".as_ptr());
    LLVMBuildStore(b, LLVMConstInt(i64_ty, 0, 0), weak_ptr);
    header_ptr
}

// Mirror of the emitRetainLoop lambda in impl.cpp ry_emit_cow_ensure_unique:
// a phi-based for loop that retains each ARC element/key of the cloned buffer.
// Takes the opaque ctx (not &mut EmitCtxImpl) so it can call the public
// ry_emit_arc_retain without holding a borrow across the call.
unsafe fn emit_cow_retain_loop(
    ctx: *mut RyEmitCtx,
    header_ty: LLVMTypeRef,
    new_data_ptr: LLVMValueRef,
    i64_ty: LLVMTypeRef,
    i8_ty: LLVMTypeRef,
    ptr_ty: LLVMTypeRef,
    field_idx: u32,
    is_str: c_int,
    tag: &[u8],
) {
    let b = cx(ctx).builder;
    let context = cx(ctx).context;
    let len_field_ptr = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 0, cname3(b"cow_", tag, b"_len_ptr").as_ptr());
    let count = LLVMBuildLoad2(b, i64_ty, len_field_ptr, cname3(b"cow_", tag, b"_len").as_ptr());
    let buf_field_ptr = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, field_idx, cname3(b"cow_", tag, b"_buf_field").as_ptr());
    let buf = LLVMBuildLoad2(b, ptr_ty, buf_field_ptr, cname3(b"cow_", tag, b"_buf").as_ptr());

    let loop_fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));
    let loop_bb = LLVMAppendBasicBlockInContext(context, loop_fn, cname3(b"cow.", tag, b"_loop").as_ptr());
    let body_bb = LLVMAppendBasicBlockInContext(context, loop_fn, cname3(b"cow.", tag, b"_body").as_ptr());
    let loop_done_bb = LLVMAppendBasicBlockInContext(context, loop_fn, cname3(b"cow.", tag, b"_done").as_ptr());

    let pre_loop_bb = LLVMGetInsertBlock(b);
    LLVMBuildBr(b, loop_bb);
    LLVMPositionBuilderAtEnd(b, loop_bb);
    let idx = LLVMBuildPhi(b, i64_ty, cname3(b"cow_", tag, b"_idx").as_ptr());
    let mut init_vals = [LLVMConstInt(i64_ty, 0, 0)];
    let mut init_blocks = [pre_loop_bb];
    LLVMAddIncoming(idx, init_vals.as_mut_ptr(), init_blocks.as_mut_ptr(), 1);
    let cond = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLT, idx, count, cname3(b"cow_", tag, b"_cond").as_ptr());
    LLVMBuildCondBr(b, cond, body_bb, loop_done_bb);

    LLVMPositionBuilderAtEnd(b, body_bb);
    let mut elem_gep = [idx];
    let elem_ptr = LLVMBuildGEP2(b, ptr_ty, buf, elem_gep.as_mut_ptr(), 1, cname3(b"cow_", tag, b"_ptr").as_ptr());
    let elem = LLVMBuildLoad2(b, ptr_ty, elem_ptr, cname3(b"cow_", tag, b"_val").as_ptr());
    let hdr_offset = if is_str != 0 {
        -(STRING_HEADER_SIZE as i64)
    } else {
        -(ARC_HEADER_SIZE as i64)
    };
    let mut hdr_gep = [LLVMConstInt(i64_ty, hdr_offset as u64, 0)];
    let elem_hdr = LLVMBuildGEP2(b, i8_ty, elem, hdr_gep.as_mut_ptr(), 1, cname3(b"cow_", tag, b"_hdr").as_ptr());
    let elem_hdr_id = intern(cx(ctx), to_ry_value(elem_hdr));
    ry_emit_arc_retain(ctx, elem_hdr_id, RY_ARC_NONATOMIC);
    let next = LLVMBuildAdd(b, idx, LLVMConstInt(i64_ty, 1, 0), cname3(b"cow_", tag, b"_next").as_ptr());
    // Back-edge incoming uses the builder's current block (advanced past the
    // retain helper's BBs), not body_bb.
    let mut next_vals = [next];
    let mut next_blocks = [LLVMGetInsertBlock(b)];
    LLVMAddIncoming(idx, next_vals.as_mut_ptr(), next_blocks.as_mut_ptr(), 1);
    LLVMBuildBr(b, loop_bb);

    LLVMPositionBuilderAtEnd(b, loop_done_bb);
}

// =============================================================
// ABI function implementations (28 total), ported from
// src/llvm_emit/impl.cpp to the LLVM C API (issue #1997).
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
//   - Stage 2-C ControlFlow       (4)
// =============================================================

// ----- Lifecycle / intern / resolve --------------------------

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

// ----- Stage 2-B helpers -------------------------------------

#[no_mangle]
pub unsafe extern "C" fn ry_emit_build_error_from_runtime(
    ctx: *mut RyEmitCtx,
    err_fn_name: *const c_char,
    error_ty: RyTypeRef,
) -> RyValueId {
    let c = cx(ctx);
    let error_ty = as_type(error_ty);
    let ptr_ty = ptr_type(c.context);
    let i64_ty = i64_type(c.context);
    let err_fn_ty = LLVMFunctionType(ptr_ty, std::ptr::null_mut(), 0, 0);
    let err_fn = get_or_insert_function(c.module, err_fn_name, err_fn_ty);
    let err_msg = LLVMBuildCall2(
        c.builder,
        err_fn_ty,
        err_fn,
        std::ptr::null_mut(),
        0,
        c"err_msg".as_ptr(),
    );
    let mut err_struct = LLVMGetUndef(error_ty);
    err_struct = LLVMBuildInsertValue(c.builder, err_struct, err_msg, 0, c"err.msg".as_ptr());
    err_struct = LLVMBuildInsertValue(
        c.builder,
        err_struct,
        LLVMConstInt(i64_ty, 0, 0),
        1,
        c"err.code".as_ptr(),
    );
    intern(c, to_ry_value(err_struct))
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_get_runtime_fn(
    ctx: *mut RyEmitCtx,
    name: *const c_char,
    fn_ty: RyFuncTypeRef,
) -> RyValueRef {
    let c = cx(ctx);
    let fn_ty = as_functype(fn_ty);
    to_ry_value(get_or_insert_function(c.module, name, fn_ty))
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_bounds_check(
    ctx: *mut RyEmitCtx,
    idx_id: RyValueId,
    len_id: RyValueId,
    kind: c_int,
    global_name: *const c_char,
    bb_prefix: *const c_char,
) -> RyValueId {
    let context = cx(ctx).context;
    let b = cx(ctx).builder;
    let i1_ty = i1_type(context);
    let i64_ty = i64_type(context);
    let mut idx = as_value(resolve(cx(ctx), idx_id));
    let len = as_value(resolve(cx(ctx), len_id));
    if LLVMTypeOf(idx) == i1_ty {
        idx = LLVMBuildZExt(b, idx, i64_ty, c"idx_ext".as_ptr());
    }
    let orig_index = idx;

    // Negative-index wrap is a proper ABI function (Stage 2-B); call it through
    // the public entry so we never hold a &mut borrow across the call.
    let idx_in = intern(cx(ctx), to_ry_value(idx));
    let len_in = intern(cx(ctx), to_ry_value(len));
    let wrapped_id = ry_emit_negative_index_wrap(ctx, idx_in, len_in, bb_prefix);
    idx = as_value(resolve(cx(ctx), wrapped_id));

    let zero = LLVMConstInt(i64_ty, 0, 0);
    let p = cstr_bytes(bb_prefix);
    let neg_n = cname_pfx(p, b"_neg");
    let neg_check = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLT, idx, zero, neg_n.as_ptr());
    let over_n = cname_pfx(p, b"_over");
    let over_check = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSGE, idx, len, over_n.as_ptr());
    let oob_n = cname_pfx(p, b"_oob");
    let oob = LLVMBuildOr(b, neg_check, over_check, oob_n.as_ptr());

    let oob_block = cname_pfx(p, b".oob");
    let ok_block = cname_pfx(p, b".ok");
    // Derive parent function from the builder, not ctx->function (builder-derived
    // parent rule — .claude/rules/codegen-llvm-ir-conventions.md, #1996).
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));
    let oob_bb = LLVMAppendBasicBlockInContext(context, fn_v, oob_block.as_ptr());
    let ok_bb = LLVMAppendBasicBlockInContext(context, fn_v, ok_block.as_ptr());
    LLVMBuildCondBr(b, oob, oob_bb, ok_bb);
    LLVMPositionBuilderAtEnd(b, oob_bb);

    let fmt_msg: &CStr = if kind == RY_BOUNDS_LIST {
        c"runtime error: index %lld out of bounds for list of length %lld\n"
    } else {
        c"runtime error: index %lld out of bounds for array of length %lld\n"
    };
    let orig_in = intern(cx(ctx), to_ry_value(orig_index));
    let len_in2 = intern(cx(ctx), to_ry_value(len));
    ry_emit_bounds_error(ctx, orig_in, len_in2, fmt_msg.as_ptr(), global_name);

    LLVMPositionBuilderAtEnd(b, ok_bb);
    intern(cx(ctx), to_ry_value(idx))
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_result_branch(
    ctx: *mut RyEmitCtx,
    is_err_id: RyValueId,
    res_ty: RyTypeRef,
    build_ok: RyBuildValueFn,
    build_err: RyBuildValueFn,
    user_ctx: *mut c_void,
) -> RyValueId {
    let context = cx(ctx).context;
    let b = cx(ctx).builder;
    let is_err = as_value(resolve(cx(ctx), is_err_id));
    let res_ty = as_type(res_ty);
    // Builder-derived parent function (builder-derived parent rule).
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));
    let ok_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.ok".as_ptr());
    let err_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.err".as_ptr());
    let merge_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.merge".as_ptr());
    LLVMBuildCondBr(b, is_err, err_bb, ok_bb);

    LLVMPositionBuilderAtEnd(b, ok_bb);
    let ok_id = (build_ok.unwrap())(user_ctx);
    let ok_val = as_value(resolve(cx(ctx), ok_id));
    LLVMBuildBr(b, merge_bb);
    // Re-capture the incoming block: the callback may have advanced the builder
    // through additional BBs (load-bearing — mirror impl.cpp).
    let ok_in = LLVMGetInsertBlock(b);

    LLVMPositionBuilderAtEnd(b, err_bb);
    let err_id = (build_err.unwrap())(user_ctx);
    let err_val = as_value(resolve(cx(ctx), err_id));
    LLVMBuildBr(b, merge_bb);
    let err_in = LLVMGetInsertBlock(b);

    LLVMPositionBuilderAtEnd(b, merge_bb);
    let phi = LLVMBuildPhi(b, res_ty, c"result".as_ptr());
    let mut vals = [ok_val, err_val];
    let mut blocks = [ok_in, err_in];
    LLVMAddIncoming(phi, vals.as_mut_ptr(), blocks.as_mut_ptr(), 2);
    intern(cx(ctx), to_ry_value(phi))
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_negative_index_wrap(
    ctx: *mut RyEmitCtx,
    idx_id: RyValueId,
    wrap_base_id: RyValueId,
    prefix: *const c_char,
) -> RyValueId {
    let c = cx(ctx);
    let b = c.builder;
    let i64_ty = i64_type(c.context);
    let mut idx = as_value(resolve(c, idx_id));
    let mut wrap_base = as_value(resolve(c, wrap_base_id));
    let p = cstr_bytes(prefix);
    // Defensively normalize narrow operands to i64 (mirror impl.cpp).
    if LLVMTypeOf(idx) != i64_ty {
        let n = cname_pfx(p, b"_idx_i64");
        idx = LLVMBuildIntCast2(b, idx, i64_ty, 1, n.as_ptr());
    }
    if LLVMTypeOf(wrap_base) != i64_ty {
        let n = cname_pfx(p, b"_wrap_base_i64");
        wrap_base = LLVMBuildIntCast2(b, wrap_base, i64_ty, 1, n.as_ptr());
    }
    let zero = LLVMConstInt(i64_ty, 0, 0);
    let n_neg = cname_pfx(p, b"_is_neg");
    let is_neg = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLT, idx, zero, n_neg.as_ptr());
    let n_wrap = cname_pfx(p, b"_wrapped");
    let wrapped = LLVMBuildAdd(b, idx, wrap_base, n_wrap.as_ptr());
    let n_idx = cname_pfx(p, b"_idx");
    let result = LLVMBuildSelect(b, is_neg, wrapped, idx, n_idx.as_ptr());
    intern(c, to_ry_value(result))
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_bounds_error(
    ctx: *mut RyEmitCtx,
    orig_idx_id: RyValueId,
    len_id: RyValueId,
    fmt_msg: *const c_char,
    global_name: *const c_char,
) {
    let c = cx(ctx);
    let b = c.builder;
    let orig_idx = as_value(resolve(c, orig_idx_id));
    let len = as_value(resolve(c, len_id));
    let ptr_ty = ptr_type(c.context);
    let i32_ty = i32_type(c.context);
    let void_ty = void_type(c.context);
    let (stdout_name, stderr_name) = if cfg!(target_os = "macos") {
        (c"__stdoutp".as_ptr(), c"__stderrp".as_ptr())
    } else {
        (c"stdout".as_ptr(), c"stderr".as_ptr())
    };
    let stderr_global = get_or_insert_global(c.module, stderr_name, ptr_ty);
    let stdout_global = get_or_insert_global(c.module, stdout_name, ptr_ty);
    let stderr_val = LLVMBuildLoad2(b, ptr_ty, stderr_global, c"stderr".as_ptr());
    let stdout_val = LLVMBuildLoad2(b, ptr_ty, stdout_global, c"stdout".as_ptr());

    // Dedup the format-string global within this ctx (default name matches impl.cpp).
    let fmt_key = cstr_bytes(fmt_msg);
    let name_ptr = if global_name.is_null() {
        c".bounds_err_msg".as_ptr()
    } else {
        global_name
    };
    let err_msg = get_or_create_msg_global(c, fmt_key, name_ptr);

    let mut fprintf_params = [ptr_ty, ptr_ty];
    let fprintf_ty = LLVMFunctionType(i32_ty, fprintf_params.as_mut_ptr(), 2, 1);
    let fprintf_fn = get_or_insert_function(c.module, c"fprintf".as_ptr(), fprintf_ty);
    let mut fprintf_args = [stderr_val, err_msg, orig_idx, len];
    LLVMBuildCall2(b, fprintf_ty, fprintf_fn, fprintf_args.as_mut_ptr(), 4, c"".as_ptr());

    let mut fflush_params = [ptr_ty];
    let fflush_ty = LLVMFunctionType(i32_ty, fflush_params.as_mut_ptr(), 1, 0);
    let fflush_fn = get_or_insert_function(c.module, c"fflush".as_ptr(), fflush_ty);
    let mut a_out = [stdout_val];
    LLVMBuildCall2(b, fflush_ty, fflush_fn, a_out.as_mut_ptr(), 1, c"".as_ptr());
    let mut a_err = [stderr_val];
    LLVMBuildCall2(b, fflush_ty, fflush_fn, a_err.as_mut_ptr(), 1, c"".as_ptr());

    let mut exit_params = [i32_ty];
    let exit_ty = LLVMFunctionType(void_ty, exit_params.as_mut_ptr(), 1, 0);
    let exit_fn = get_or_insert_function(c.module, c"_Exit".as_ptr(), exit_ty);
    let mut ea = [LLVMConstInt(i32_ty, 1, 0)];
    LLVMBuildCall2(b, exit_ty, exit_fn, ea.as_mut_ptr(), 1, c"".as_ptr());
    LLVMBuildUnreachable(b);
}

// ----- Stage 2-C Option --------------------------------------

#[no_mangle]
pub unsafe extern "C" fn ry_emit_option_wrap_some(
    ctx: *mut RyEmitCtx,
    inner_id: RyValueId,
    opt_ty: RyTypeRef,
) -> RyValueId {
    let c = cx(ctx);
    let opt_ty = as_type(opt_ty);
    let inner = as_value(resolve(c, inner_id));
    let i1_ty = i1_type(c.context);
    let mut val = LLVMGetUndef(opt_ty);
    val = LLVMBuildInsertValue(c.builder, val, LLVMConstInt(i1_ty, 1, 0), 0, c"".as_ptr());
    val = LLVMBuildInsertValue(c.builder, val, inner, 1, c"".as_ptr());
    intern(c, to_ry_value(val))
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_option_wrap_none(
    ctx: *mut RyEmitCtx,
    opt_ty: RyTypeRef,
) -> RyValueId {
    let c = cx(ctx);
    let opt_ty = as_type(opt_ty);
    let i1_ty = i1_type(c.context);
    let mut val = LLVMGetUndef(opt_ty);
    val = LLVMBuildInsertValue(c.builder, val, LLVMConstInt(i1_ty, 0, 0), 0, c"".as_ptr());
    let payload_ty = LLVMStructGetTypeAtIndex(opt_ty, 1);
    val = LLVMBuildInsertValue(c.builder, val, LLVMGetUndef(payload_ty), 1, c"".as_ptr());
    intern(c, to_ry_value(val))
}

// ----- Stage 2-C ARC -----------------------------------------

// Internal form takes &mut EmitCtxImpl so cow_ensure_unique can call it while
// already holding the borrow.
unsafe fn arc_retain_impl(c: &mut EmitCtxImpl, header_ptr_id: RyValueId, atomic: c_int) {
    let b = c.builder;
    let context = c.context;
    let header_ptr = as_value(resolve(c, header_ptr_id));
    let i64_ty = i64_type(context);
    let arc_header_ty = arc_header_type(context);
    let strong_ptr = LLVMBuildStructGEP2(b, arc_header_ty, header_ptr, 0, c"arc_retain_ptr".as_ptr());
    // Skip immortal objects; the load must still be atomic in atomic mode (#630).
    let cur = emit_atomic_i64_load(
        b,
        i64_ty,
        strong_ptr,
        if atomic == RY_ARC_ATOMIC {
            LLVMAtomicOrdering::LLVMAtomicOrderingMonotonic
        } else {
            LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic
        },
        c"arc_strong".as_ptr(),
    );
    let is_immortal = LLVMBuildICmp(
        b,
        LLVMIntPredicate::LLVMIntEQ,
        cur,
        LLVMConstInt(i64_ty, ARC_IMMORTAL as u64, 0),
        c"arc_immortal".as_ptr(),
    );
    // Builder-derived parent function (builder-derived parent rule).
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));
    let retain_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"arc.retain".as_ptr());
    let done_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"arc.retain.done".as_ptr());
    LLVMBuildCondBr(b, is_immortal, done_bb, retain_bb);

    LLVMPositionBuilderAtEnd(b, retain_bb);
    if atomic == RY_ARC_ATOMIC {
        LLVMBuildAtomicRMW(
            b,
            LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpAdd,
            strong_ptr,
            LLVMConstInt(i64_ty, 1, 0),
            LLVMAtomicOrdering::LLVMAtomicOrderingSequentiallyConsistent,
            0,
        );
    } else {
        let inc = LLVMBuildAdd(b, cur, LLVMConstInt(i64_ty, 1, 0), c"arc_inc".as_ptr());
        LLVMBuildStore(b, inc, strong_ptr);
    }
    LLVMBuildBr(b, done_bb);
    LLVMPositionBuilderAtEnd(b, done_bb);
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_arc_retain(
    ctx: *mut RyEmitCtx,
    header_ptr_id: RyValueId,
    atomic: c_int,
) {
    arc_retain_impl(cx(ctx), header_ptr_id, atomic);
}

unsafe fn arc_release_impl(
    c: &mut EmitCtxImpl,
    header_ptr_id: RyValueId,
    atomic: c_int,
    destructor_callee: RyValueRef,
    gc_visit_fn: RyValueRef,
) {
    let b = c.builder;
    let context = c.context;
    let module = c.module;
    let header_ptr = as_value(resolve(c, header_ptr_id));
    let i64_ty = i64_type(context);
    let i8_ty = i8_type(context);
    let void_ty = void_type(context);
    let ptr_ty = ptr_type(context);
    let arc_header_ty = arc_header_type(context);

    let strong_ptr = LLVMBuildStructGEP2(b, arc_header_ty, header_ptr, 0, c"arc_rel_ptr".as_ptr());
    let cur_check = emit_atomic_i64_load(
        b,
        i64_ty,
        strong_ptr,
        if atomic == RY_ARC_ATOMIC {
            LLVMAtomicOrdering::LLVMAtomicOrderingMonotonic
        } else {
            LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic
        },
        c"arc_strong_check".as_ptr(),
    );
    let is_immortal = LLVMBuildICmp(
        b,
        LLVMIntPredicate::LLVMIntEQ,
        cur_check,
        LLVMConstInt(i64_ty, ARC_IMMORTAL as u64, 0),
        c"arc_immortal".as_ptr(),
    );
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));
    let release_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"arc.release.body".as_ptr());
    let done_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"arc.done".as_ptr());
    LLVMBuildCondBr(b, is_immortal, done_bb, release_bb);

    LLVMPositionBuilderAtEnd(b, release_bb);
    let is_zero;
    if atomic == RY_ARC_ATOMIC {
        // atomicrmw returns the OLD value; object is dead when old == 1.
        let old = LLVMBuildAtomicRMW(
            b,
            LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpSub,
            strong_ptr,
            LLVMConstInt(i64_ty, 1, 0),
            LLVMAtomicOrdering::LLVMAtomicOrderingSequentiallyConsistent,
            0,
        );
        is_zero = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntEQ, old, LLVMConstInt(i64_ty, 1, 0), c"arc_dead".as_ptr());
    } else {
        let cur = LLVMBuildLoad2(b, i64_ty, strong_ptr, c"arc_strong".as_ptr());
        let dec = LLVMBuildSub(b, cur, LLVMConstInt(i64_ty, 1, 0), c"arc_dec".as_ptr());
        LLVMBuildStore(b, dec, strong_ptr);
        is_zero = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntEQ, dec, LLVMConstInt(i64_ty, 0, 0), c"arc_dead".as_ptr());
    }

    let free_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"arc.release".as_ptr());

    if !gc_visit_fn.is_null() {
        let track_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"arc.gc_track".as_ptr());
        LLVMBuildCondBr(b, is_zero, free_bb, track_bb);
        LLVMPositionBuilderAtEnd(b, track_bb);
        let mut gc_track_params = [ptr_ty, ptr_ty, ptr_ty];
        let gc_track_ty = LLVMFunctionType(void_ty, gc_track_params.as_mut_ptr(), 3, 0);
        let gc_track_fn = get_or_insert_function(module, c"__ry_gc_track".as_ptr(), gc_track_ty);
        let dtor_ptr = if destructor_callee.is_null() {
            LLVMConstNull(ptr_ty)
        } else {
            as_value(destructor_callee)
        };
        let gc_visit_fn_val = as_value(gc_visit_fn);
        let mut gc_track_args = [header_ptr, gc_visit_fn_val, dtor_ptr];
        LLVMBuildCall2(b, gc_track_ty, gc_track_fn, gc_track_args.as_mut_ptr(), 3, c"".as_ptr());
        LLVMBuildBr(b, done_bb);
    } else {
        LLVMBuildCondBr(b, is_zero, free_bb, done_bb);
    }

    LLVMPositionBuilderAtEnd(b, free_bb);
    let mut gc_untrack_params = [ptr_ty];
    let gc_untrack_ty = LLVMFunctionType(void_ty, gc_untrack_params.as_mut_ptr(), 1, 0);
    let gc_untrack_fn = get_or_insert_function(module, c"__ry_gc_untrack".as_ptr(), gc_untrack_ty);
    let mut gc_untrack_args = [header_ptr];
    LLVMBuildCall2(b, gc_untrack_ty, gc_untrack_fn, gc_untrack_args.as_mut_ptr(), 1, c"".as_ptr());
    if !destructor_callee.is_null() {
        let mut gep_idx = [LLVMConstInt(i64_ty, ARC_HEADER_SIZE, 0)];
        let data_ptr = LLVMBuildGEP2(b, i8_ty, header_ptr, gep_idx.as_mut_ptr(), 1, c"arc_data".as_ptr());
        let mut dtor_params = [ptr_ty];
        let dtor_ty = LLVMFunctionType(void_ty, dtor_params.as_mut_ptr(), 1, 0);
        let mut dtor_args = [data_ptr];
        LLVMBuildCall2(b, dtor_ty, as_value(destructor_callee), dtor_args.as_mut_ptr(), 1, c"".as_ptr());
    }
    // weak_count read: atomic load scaled to the caller's atomic mode (Acquire
    // when atomic) — weak_count atomic-load rule (#1968).
    let weak_ptr = LLVMBuildStructGEP2(b, arc_header_ty, header_ptr, 1, c"arc_weak_ptr".as_ptr());
    let weak_count = emit_atomic_i64_load(
        b,
        i64_ty,
        weak_ptr,
        if atomic == RY_ARC_ATOMIC {
            LLVMAtomicOrdering::LLVMAtomicOrderingAcquire
        } else {
            LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic
        },
        c"arc_weak".as_ptr(),
    );
    let no_weak = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntEQ, weak_count, LLVMConstInt(i64_ty, 0, 0), c"arc_no_weak".as_ptr());
    let real_free_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"arc.free".as_ptr());
    let skip_free_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"arc.skip_free".as_ptr());
    LLVMBuildCondBr(b, no_weak, real_free_bb, skip_free_bb);

    LLVMPositionBuilderAtEnd(b, real_free_bb);
    emit_arc_counter_delta(b, i64_ty, ptr_ty, -1);
    let mut free_params = [ptr_ty];
    let free_ty = LLVMFunctionType(void_ty, free_params.as_mut_ptr(), 1, 0);
    let free_fn = get_or_insert_function(module, c"free".as_ptr(), free_ty);
    let mut free_args = [header_ptr];
    LLVMBuildCall2(b, free_ty, free_fn, free_args.as_mut_ptr(), 1, c"".as_ptr());
    LLVMBuildBr(b, done_bb);

    LLVMPositionBuilderAtEnd(b, skip_free_bb);
    LLVMBuildBr(b, done_bb);

    LLVMPositionBuilderAtEnd(b, done_bb);
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_arc_release(
    ctx: *mut RyEmitCtx,
    header_ptr_id: RyValueId,
    atomic: c_int,
    destructor_callee: RyValueRef,
    gc_visit_fn: RyValueRef,
) {
    arc_release_impl(cx(ctx), header_ptr_id, atomic, destructor_callee, gc_visit_fn);
}

// ----- Stage 2-C RuntimeCall ---------------------------------

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
    // ABI input validation (mirror impl.cpp): malformed callers get sentinel 0
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
    let ret_ty_t = as_type(ret_ty);
    let mut arg_tys_v: Vec<LLVMTypeRef> = Vec::with_capacity(arg_ty_count as usize);
    for i in 0..arg_ty_count as usize {
        let t = *arg_tys.add(i);
        if t.is_null() {
            return 0;
        }
        arg_tys_v.push(as_type(t));
    }
    let fn_ty = LLVMFunctionType(ret_ty_t, arg_tys_v.as_mut_ptr(), arg_ty_count, 0);
    let callee = get_or_insert_function(c.module, name, fn_ty);
    let mut args: Vec<LLVMValueRef> = Vec::with_capacity(arg_count as usize);
    for i in 0..arg_count as usize {
        let a = as_value(resolve(c, *arg_ids.add(i)));
        if a.is_null() {
            return 0;
        }
        args.push(a);
    }
    // LLVM forbids naming a void-returning call; pass an empty name in that case.
    let call_name = if LLVMGetTypeKind(ret_ty_t) == LLVMTypeKind::LLVMVoidTypeKind || name_hint.is_null()
    {
        c"".as_ptr()
    } else {
        name_hint
    };
    let result = LLVMBuildCall2(c.builder, fn_ty, callee, args.as_mut_ptr(), arg_count, call_name);
    intern(c, to_ry_value(result))
}

// ----- Stage 2-C Collection ----------------------------------

#[no_mangle]
pub unsafe extern "C" fn ry_emit_collection_append(
    ctx: *mut RyEmitCtx,
    list_ptr_id: RyValueId,
    val_id: RyValueId,
    list_header_ty: RyTypeRef,
    elem_ty: RyTypeRef,
    elem_size: u64,
) {
    let c = cx(ctx);
    let b = c.builder;
    let context = c.context;
    let module = c.module;
    let list_ptr = as_value(resolve(c, list_ptr_id));
    let val = as_value(resolve(c, val_id));
    let list_header_ty = as_type(list_header_ty);
    let elem_ty = as_type(elem_ty);
    let i64_ty = i64_type(context);
    let ptr_ty = ptr_type(context);
    let void_ty = void_type(context);
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

    let h = load_list_header(b, list_header_ty, list_ptr, i64_ty, ptr_ty, b"app");

    let need_grow = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntEQ, h.len, h.cap, c"app_need_grow".as_ptr());
    let grow_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"app.grow".as_ptr());
    let store_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"app.store".as_ptr());
    LLVMBuildCondBr(b, need_grow, grow_bb, store_bb);

    LLVMPositionBuilderAtEnd(b, grow_bb);
    let four = LLVMConstInt(i64_ty, 4, 0);
    let doubled = LLVMBuildMul(b, h.cap, LLVMConstInt(i64_ty, 2, 0), c"app_doubled".as_ptr());
    let gt4 = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSGT, doubled, four, c"cap_gt4".as_ptr());
    let new_cap = LLVMBuildSelect(b, gt4, doubled, four, c"app_new_cap".as_ptr());
    let new_size = LLVMBuildMul(b, new_cap, LLVMConstInt(i64_ty, elem_size, 0), c"app_new_size".as_ptr());
    let mut malloc_p = [i64_ty];
    let malloc_ty = LLVMFunctionType(ptr_ty, malloc_p.as_mut_ptr(), 1, 0);
    let malloc_fn = get_or_insert_function(module, c"malloc".as_ptr(), malloc_ty);
    let mut malloc_a = [new_size];
    let new_data = LLVMBuildCall2(b, malloc_ty, malloc_fn, malloc_a.as_mut_ptr(), 1, c"app_new_data".as_ptr());
    let old_size = LLVMBuildMul(b, h.len, LLVMConstInt(i64_ty, elem_size, 0), c"app_old_size".as_ptr());
    let mut memcpy_p = [ptr_ty, ptr_ty, i64_ty];
    let memcpy_ty = LLVMFunctionType(ptr_ty, memcpy_p.as_mut_ptr(), 3, 0);
    let memcpy_fn = get_or_insert_function(module, c"memcpy".as_ptr(), memcpy_ty);
    let mut memcpy_a = [new_data, h.data, old_size];
    LLVMBuildCall2(b, memcpy_ty, memcpy_fn, memcpy_a.as_mut_ptr(), 3, c"".as_ptr());
    let mut free_p = [ptr_ty];
    let free_ty = LLVMFunctionType(void_ty, free_p.as_mut_ptr(), 1, 0);
    let free_fn = get_or_insert_function(module, c"free".as_ptr(), free_ty);
    let mut free_a = [h.data];
    LLVMBuildCall2(b, free_ty, free_fn, free_a.as_mut_ptr(), 1, c"".as_ptr());
    LLVMBuildStore(b, new_data, h.data_ptr);
    LLVMBuildStore(b, new_cap, h.cap_ptr);
    LLVMBuildBr(b, store_bb);

    LLVMPositionBuilderAtEnd(b, store_bb);
    let cur_data = LLVMBuildLoad2(b, ptr_ty, h.data_ptr, c"app_cur_data".as_ptr());
    let cur_len = LLVMBuildLoad2(b, i64_ty, h.len_ptr, c"app_cur_len".as_ptr());
    let mut elem_idx = [cur_len];
    let elem_ptr = LLVMBuildGEP2(b, elem_ty, cur_data, elem_idx.as_mut_ptr(), 1, c"app_elem_ptr".as_ptr());
    LLVMBuildStore(b, val, elem_ptr);
    let new_len = LLVMBuildAdd(b, cur_len, LLVMConstInt(i64_ty, 1, 0), c"app_new_len".as_ptr());
    LLVMBuildStore(b, new_len, h.len_ptr);
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_collection_insert(
    ctx: *mut RyEmitCtx,
    list_ptr_id: RyValueId,
    idx_id: RyValueId,
    val_id: RyValueId,
    list_header_ty: RyTypeRef,
    elem_ty: RyTypeRef,
    elem_size: u64,
) {
    let context = cx(ctx).context;
    let b = cx(ctx).builder;
    let module = cx(ctx).module;
    let list_ptr = as_value(resolve(cx(ctx), list_ptr_id));
    let orig_idx = as_value(resolve(cx(ctx), idx_id));
    let val = as_value(resolve(cx(ctx), val_id));
    let list_header_ty = as_type(list_header_ty);
    let elem_ty = as_type(elem_ty);
    let i64_ty = i64_type(context);
    let ptr_ty = ptr_type(context);
    let void_ty = void_type(context);
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

    let h = load_list_header(b, list_header_ty, list_ptr, i64_ty, ptr_ty, b"ins");

    // insert() valid range is [0, len], so negative -k maps to len+1-k.
    let wrap_base = LLVMBuildAdd(b, h.len, LLVMConstInt(i64_ty, 1, 0), c"ins_wrap_base".as_ptr());
    let orig_in = intern(cx(ctx), to_ry_value(orig_idx));
    let wrap_in = intern(cx(ctx), to_ry_value(wrap_base));
    let wrapped_id = ry_emit_negative_index_wrap(ctx, orig_in, wrap_in, c"ins".as_ptr());
    let idx = as_value(resolve(cx(ctx), wrapped_id));

    let zero = LLVMConstInt(i64_ty, 0, 0);
    let neg_check = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLT, idx, zero, c"".as_ptr());
    let over_check = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSGT, idx, h.len, c"".as_ptr());
    let out_of_bounds = LLVMBuildOr(b, neg_check, over_check, c"ins_oob".as_ptr());
    let err_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"ins.err".as_ptr());
    let ok_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"ins.ok".as_ptr());
    LLVMBuildCondBr(b, out_of_bounds, err_bb, ok_bb);
    LLVMPositionBuilderAtEnd(b, err_bb);
    let orig_in2 = intern(cx(ctx), to_ry_value(orig_idx));
    let len_in = intern(cx(ctx), to_ry_value(h.len));
    ry_emit_bounds_error(
        ctx,
        orig_in2,
        len_in,
        c"runtime error: index %lld out of bounds for insert() on list of length %lld\n".as_ptr(),
        c".ins_oob_err".as_ptr(),
    );

    LLVMPositionBuilderAtEnd(b, ok_bb);
    let need_grow = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntEQ, h.len, h.cap, c"ins_need_grow".as_ptr());
    let grow_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"ins.grow".as_ptr());
    let move_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"ins.move".as_ptr());
    LLVMBuildCondBr(b, need_grow, grow_bb, move_bb);

    LLVMPositionBuilderAtEnd(b, grow_bb);
    let four = LLVMConstInt(i64_ty, 4, 0);
    let doubled = LLVMBuildMul(b, h.cap, LLVMConstInt(i64_ty, 2, 0), c"ins_doubled".as_ptr());
    let gt4 = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSGT, doubled, four, c"".as_ptr());
    let new_cap = LLVMBuildSelect(b, gt4, doubled, four, c"ins_new_cap".as_ptr());
    let new_size = LLVMBuildMul(b, new_cap, LLVMConstInt(i64_ty, elem_size, 0), c"ins_new_size".as_ptr());
    let mut malloc_p = [i64_ty];
    let malloc_ty = LLVMFunctionType(ptr_ty, malloc_p.as_mut_ptr(), 1, 0);
    let malloc_fn = get_or_insert_function(module, c"malloc".as_ptr(), malloc_ty);
    let mut malloc_a = [new_size];
    let new_data = LLVMBuildCall2(b, malloc_ty, malloc_fn, malloc_a.as_mut_ptr(), 1, c"ins_new_data".as_ptr());
    let old_size = LLVMBuildMul(b, h.len, LLVMConstInt(i64_ty, elem_size, 0), c"ins_old_size".as_ptr());
    let mut memcpy_p = [ptr_ty, ptr_ty, i64_ty];
    let memcpy_ty = LLVMFunctionType(ptr_ty, memcpy_p.as_mut_ptr(), 3, 0);
    let memcpy_fn = get_or_insert_function(module, c"memcpy".as_ptr(), memcpy_ty);
    let mut memcpy_a = [new_data, h.data, old_size];
    LLVMBuildCall2(b, memcpy_ty, memcpy_fn, memcpy_a.as_mut_ptr(), 3, c"".as_ptr());
    let mut free_p = [ptr_ty];
    let free_ty = LLVMFunctionType(void_ty, free_p.as_mut_ptr(), 1, 0);
    let free_fn = get_or_insert_function(module, c"free".as_ptr(), free_ty);
    let mut free_a = [h.data];
    LLVMBuildCall2(b, free_ty, free_fn, free_a.as_mut_ptr(), 1, c"".as_ptr());
    LLVMBuildStore(b, new_data, h.data_ptr);
    LLVMBuildStore(b, new_cap, h.cap_ptr);
    LLVMBuildBr(b, move_bb);

    LLVMPositionBuilderAtEnd(b, move_bb);
    let cur_data = LLVMBuildLoad2(b, ptr_ty, h.data_ptr, c"ins_cur_data".as_ptr());
    let mut src_idx = [idx];
    let src_ptr = LLVMBuildGEP2(b, elem_ty, cur_data, src_idx.as_mut_ptr(), 1, c"ins_src".as_ptr());
    let idx_plus_one = LLVMBuildAdd(b, idx, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
    let mut dst_idx = [idx_plus_one];
    let dst_ptr = LLVMBuildGEP2(b, elem_ty, cur_data, dst_idx.as_mut_ptr(), 1, c"ins_dst".as_ptr());
    let move_count = LLVMBuildSub(b, h.len, idx, c"ins_move_count".as_ptr());
    let move_bytes = LLVMBuildMul(b, move_count, LLVMConstInt(i64_ty, elem_size, 0), c"ins_move_bytes".as_ptr());
    let mut memmove_p = [ptr_ty, ptr_ty, i64_ty];
    let memmove_ty = LLVMFunctionType(ptr_ty, memmove_p.as_mut_ptr(), 3, 0);
    let memmove_fn = get_or_insert_function(module, c"memmove".as_ptr(), memmove_ty);
    let mut memmove_a = [dst_ptr, src_ptr, move_bytes];
    LLVMBuildCall2(b, memmove_ty, memmove_fn, memmove_a.as_mut_ptr(), 3, c"".as_ptr());
    let mut ins_idx = [idx];
    let insert_ptr = LLVMBuildGEP2(b, elem_ty, cur_data, ins_idx.as_mut_ptr(), 1, c"ins_ptr".as_ptr());
    LLVMBuildStore(b, val, insert_ptr);
    let new_len = LLVMBuildAdd(b, h.len, LLVMConstInt(i64_ty, 1, 0), c"ins_new_len".as_ptr());
    LLVMBuildStore(b, new_len, h.len_ptr);
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_collection_remove_at(
    ctx: *mut RyEmitCtx,
    list_ptr_id: RyValueId,
    idx_id: RyValueId,
    list_header_ty: RyTypeRef,
    elem_ty: RyTypeRef,
    elem_size: u64,
) -> RyValueId {
    let context = cx(ctx).context;
    let b = cx(ctx).builder;
    let module = cx(ctx).module;
    let list_ptr = as_value(resolve(cx(ctx), list_ptr_id));
    let orig_idx = as_value(resolve(cx(ctx), idx_id));
    let list_header_ty = as_type(list_header_ty);
    let elem_ty = as_type(elem_ty);
    let i64_ty = i64_type(context);
    let ptr_ty = ptr_type(context);
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

    let h = load_list_header(b, list_header_ty, list_ptr, i64_ty, ptr_ty, b"rmat");

    let orig_in = intern(cx(ctx), to_ry_value(orig_idx));
    let len_in = intern(cx(ctx), to_ry_value(h.len));
    let wrapped_id = ry_emit_negative_index_wrap(ctx, orig_in, len_in, c"rmat".as_ptr());
    let idx = as_value(resolve(cx(ctx), wrapped_id));

    let zero = LLVMConstInt(i64_ty, 0, 0);
    let neg_check = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLT, idx, zero, c"".as_ptr());
    let over_check = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSGE, idx, h.len, c"".as_ptr());
    let out_of_bounds = LLVMBuildOr(b, neg_check, over_check, c"rmat_oob".as_ptr());
    let err_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"rmat.err".as_ptr());
    let ok_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"rmat.ok".as_ptr());
    LLVMBuildCondBr(b, out_of_bounds, err_bb, ok_bb);
    LLVMPositionBuilderAtEnd(b, err_bb);
    let orig_in2 = intern(cx(ctx), to_ry_value(orig_idx));
    let len_in2 = intern(cx(ctx), to_ry_value(h.len));
    ry_emit_bounds_error(
        ctx,
        orig_in2,
        len_in2,
        c"runtime error: index %lld out of bounds for removeAt() on list of length %lld\n".as_ptr(),
        c".rmat_oob_err".as_ptr(),
    );

    LLVMPositionBuilderAtEnd(b, ok_bb);
    let mut elem_idx = [idx];
    let elem_ptr = LLVMBuildGEP2(b, elem_ty, h.data, elem_idx.as_mut_ptr(), 1, c"rmat_elem_ptr".as_ptr());
    let removed_val = LLVMBuildLoad2(b, elem_ty, elem_ptr, c"rmat_val".as_ptr());
    let idx_plus_one = LLVMBuildAdd(b, idx, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
    let mut src_idx = [idx_plus_one];
    let src_ptr = LLVMBuildGEP2(b, elem_ty, h.data, src_idx.as_mut_ptr(), 1, c"rmat_src".as_ptr());
    let len_minus_idx = LLVMBuildSub(b, h.len, idx, c"".as_ptr());
    let move_count = LLVMBuildSub(b, len_minus_idx, LLVMConstInt(i64_ty, 1, 0), c"rmat_move_count".as_ptr());
    let move_bytes = LLVMBuildMul(b, move_count, LLVMConstInt(i64_ty, elem_size, 0), c"rmat_move_bytes".as_ptr());
    let mut memmove_p = [ptr_ty, ptr_ty, i64_ty];
    let memmove_ty = LLVMFunctionType(ptr_ty, memmove_p.as_mut_ptr(), 3, 0);
    let memmove_fn = get_or_insert_function(module, c"memmove".as_ptr(), memmove_ty);
    let mut memmove_a = [elem_ptr, src_ptr, move_bytes];
    LLVMBuildCall2(b, memmove_ty, memmove_fn, memmove_a.as_mut_ptr(), 3, c"".as_ptr());
    let new_len = LLVMBuildSub(b, h.len, LLVMConstInt(i64_ty, 1, 0), c"rmat_new_len".as_ptr());
    LLVMBuildStore(b, new_len, h.len_ptr);

    intern(cx(ctx), to_ry_value(removed_val))
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_list_slice(
    ctx: *mut RyEmitCtx,
    list_ptr_id: RyValueId,
    start_id: RyValueId,
    end_excl_id: RyValueId,
    list_header_ty: RyTypeRef,
    elem_ty: RyTypeRef,
    elem_size: u64,
    out_count: *mut RyValueId,
    out_new_data: *mut RyValueId,
) {
    let c = cx(ctx);
    let b = c.builder;
    let list_ptr = as_value(resolve(c, list_ptr_id));
    let start_val = as_value(resolve(c, start_id));
    let end_excl_val = as_value(resolve(c, end_excl_id));
    let list_header_ty = as_type(list_header_ty);
    let elem_ty = as_type(elem_ty);
    let i64_ty = i64_type(c.context);
    let ptr_ty = ptr_type(c.context);

    let sl_len_ptr = LLVMBuildStructGEP2(b, list_header_ty, list_ptr, 0, c"sl_len_ptr".as_ptr());
    let sl_data_ptr = LLVMBuildStructGEP2(b, list_header_ty, list_ptr, 2, c"sl_data_ptr".as_ptr());
    let sl_len = LLVMBuildLoad2(b, i64_ty, sl_len_ptr, c"sl_len".as_ptr());
    let sl_data = LLVMBuildLoad2(b, ptr_ty, sl_data_ptr, c"sl_data".as_ptr());

    let zero = LLVMConstInt(i64_ty, 0, 0);
    let start_neg = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLT, start_val, zero, c"".as_ptr());
    let mut c_start = LLVMBuildSelect(b, start_neg, zero, start_val, c"sl_cstart".as_ptr());
    let start_over = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSGT, c_start, sl_len, c"".as_ptr());
    c_start = LLVMBuildSelect(b, start_over, sl_len, c_start, c"sl_cstart2".as_ptr());
    let end_neg = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSLT, end_excl_val, zero, c"".as_ptr());
    let mut c_end = LLVMBuildSelect(b, end_neg, zero, end_excl_val, c"sl_cend".as_ptr());
    let end_over = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSGT, c_end, sl_len, c"".as_ptr());
    c_end = LLVMBuildSelect(b, end_over, sl_len, c_end, c"sl_cend2".as_ptr());

    let diff = LLVMBuildSub(b, c_end, c_start, c"sl_diff".as_ptr());
    let diff_gt0 = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSGT, diff, zero, c"".as_ptr());
    let count = LLVMBuildSelect(b, diff_gt0, diff, zero, c"sl_count".as_ptr());
    let data_size = LLVMBuildMul(b, count, LLVMConstInt(i64_ty, elem_size, 0), c"sl_dsize".as_ptr());

    let mut malloc_params = [i64_ty];
    let malloc_ty = LLVMFunctionType(ptr_ty, malloc_params.as_mut_ptr(), 1, 0);
    let malloc_fn = get_or_insert_function(c.module, c"malloc".as_ptr(), malloc_ty);
    let mut malloc_args = [data_size];
    let new_data = LLVMBuildCall2(b, malloc_ty, malloc_fn, malloc_args.as_mut_ptr(), 1, c"sl_data".as_ptr());

    let mut gep_idx = [c_start];
    let src_offset = LLVMBuildGEP2(b, elem_ty, sl_data, gep_idx.as_mut_ptr(), 1, c"sl_src_off".as_ptr());
    let mut memcpy_params = [ptr_ty, ptr_ty, i64_ty];
    let memcpy_ty = LLVMFunctionType(ptr_ty, memcpy_params.as_mut_ptr(), 3, 0);
    let memcpy_fn = get_or_insert_function(c.module, c"memcpy".as_ptr(), memcpy_ty);
    let mut memcpy_args = [new_data, src_offset, data_size];
    LLVMBuildCall2(b, memcpy_ty, memcpy_fn, memcpy_args.as_mut_ptr(), 3, c"".as_ptr());

    *out_count = intern(c, to_ry_value(count));
    *out_new_data = intern(c, to_ry_value(new_data));
}

// ----- Stage 2-C CoW -----------------------------------------

#[no_mangle]
pub unsafe extern "C" fn ry_emit_cow_ensure_unique(
    ctx: *mut RyEmitCtx,
    desc: *const RyCowEnsureUniqueDesc,
) -> RyValueId {
    if ctx.is_null() || desc.is_null() {
        return 0;
    }
    if cx(ctx).context.is_null() || cx(ctx).module.is_null() || cx(ctx).builder.is_null() {
        return 0;
    }
    let b = cx(ctx).builder;
    let context = cx(ctx).context;
    let module = cx(ctx).module;
    let data_ptr = as_value(resolve(cx(ctx), (*desc).data_ptr_id));
    let slot_ptr = as_value(resolve(cx(ctx), (*desc).slot_ptr_id));
    if data_ptr.is_null() || slot_ptr.is_null() {
        return 0;
    }
    let i64_ty = i64_type(context);
    let i8_ty = i8_type(context);
    let ptr_ty = ptr_type(context);
    let arc_header_ty = arc_header_type(context);

    // Anonymous mirrors of CodeGen's listHeaderTy_ / mapHeaderTy_ / setHeaderTy_
    // (src/codegen.cpp). Field order MUST stay in sync.
    let (header_ty, data_field_idx, key_field_idx): (LLVMTypeRef, u32, u32) = match (*desc).kind {
        RY_COW_LIST => {
            let mut e = [i64_ty, i64_ty, ptr_ty];
            (LLVMStructTypeInContext(context, e.as_mut_ptr(), 3, 0), 2, 0)
        }
        RY_COW_MAP => {
            let mut e = [i64_ty, i64_ty, ptr_ty, ptr_ty, i64_ty, ptr_ty];
            (LLVMStructTypeInContext(context, e.as_mut_ptr(), 6, 0), 3, 2)
        }
        RY_COW_SET => {
            let mut e = [i64_ty, i64_ty, ptr_ty, i64_ty, ptr_ty];
            (LLVMStructTypeInContext(context, e.as_mut_ptr(), 5, 0), 2, 0)
        }
        _ => return 0,
    };

    let dl = LLVMGetModuleDataLayout(module);
    let header_size = LLVMABISizeOfType(dl, header_ty);

    let mut hdr_gep = [LLVMConstInt(i64_ty, (-(ARC_HEADER_SIZE as i64)) as u64, 0)];
    let header_ptr = LLVMBuildGEP2(b, i8_ty, data_ptr, hdr_gep.as_mut_ptr(), 1, c"cow_hdr".as_ptr());

    let atomic_mode = (*desc).atomic;
    let strong_ptr = LLVMBuildStructGEP2(b, arc_header_ty, header_ptr, 0, c"cow_strong_ptr".as_ptr());
    let strong = emit_atomic_i64_load(
        b,
        i64_ty,
        strong_ptr,
        if atomic_mode == RY_ARC_ATOMIC {
            LLVMAtomicOrdering::LLVMAtomicOrderingAcquire
        } else {
            LLVMAtomicOrdering::LLVMAtomicOrderingNotAtomic
        },
        c"cow_strong".as_ptr(),
    );
    let is_unique = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntEQ, strong, LLVMConstInt(i64_ty, 1, 0), c"cow_unique".as_ptr());
    let is_immortal = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntEQ, strong, LLVMConstInt(i64_ty, ARC_IMMORTAL as u64, 0), c"cow_immortal".as_ptr());
    let skip_cow = LLVMBuildOr(b, is_unique, is_immortal, c"cow_skip".as_ptr());

    // Builder-derived parent (builder-derived parent rule).
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));
    let copy_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"cow.copy".as_ptr());
    let cont_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"cow.cont".as_ptr());
    let orig_bb = LLVMGetInsertBlock(b);
    LLVMBuildCondBr(b, skip_cow, cont_bb, copy_bb);

    LLVMPositionBuilderAtEnd(b, copy_bb);

    let alloc_buf = move |byte_size: LLVMValueRef, name: *const c_char| -> LLVMValueRef {
        unsafe {
            let mut p = [i64_ty];
            let ty = LLVMFunctionType(ptr_ty, p.as_mut_ptr(), 1, 0);
            let f = get_or_insert_function(module, c"malloc".as_ptr(), ty);
            let mut a = [byte_size];
            LLVMBuildCall2(b, ty, f, a.as_mut_ptr(), 1, name)
        }
    };
    let memcpy_to = move |dst: LLVMValueRef, src: LLVMValueRef, byte_size: LLVMValueRef| {
        unsafe {
            let mut p = [ptr_ty, ptr_ty, i64_ty];
            let ty = LLVMFunctionType(ptr_ty, p.as_mut_ptr(), 3, 0);
            let f = get_or_insert_function(module, c"memcpy".as_ptr(), ty);
            let mut a = [dst, src, byte_size];
            LLVMBuildCall2(b, ty, f, a.as_mut_ptr(), 3, c"".as_ptr());
        }
    };

    // Allocate the ARC-backed collection header (mirror emitArcAlloc inline).
    let box_size = LLVMConstInt(i64_ty, ARC_HEADER_SIZE + header_size, 0);
    let cow_box = alloc_buf(box_size, c"cow_box".as_ptr());
    emit_arc_counter_delta(b, i64_ty, ptr_ty, 1);
    let new_strong_ptr = LLVMBuildStructGEP2(b, arc_header_ty, cow_box, 0, c"cow_new_strong_ptr".as_ptr());
    LLVMBuildStore(b, LLVMConstInt(i64_ty, 1, 0), new_strong_ptr);
    let new_weak_ptr = LLVMBuildStructGEP2(b, arc_header_ty, cow_box, 1, c"cow_new_weak_ptr".as_ptr());
    LLVMBuildStore(b, LLVMConstInt(i64_ty, 0, 0), new_weak_ptr);
    let mut nd_gep = [LLVMConstInt(i64_ty, ARC_HEADER_SIZE, 0)];
    let new_data_ptr = LLVMBuildGEP2(b, i8_ty, cow_box, nd_gep.as_mut_ptr(), 1, c"cow_new_data".as_ptr());

    let old_len_ptr = LLVMBuildStructGEP2(b, header_ty, data_ptr, 0, c"cow_old_len_ptr".as_ptr());
    let old_len = LLVMBuildLoad2(b, i64_ty, old_len_ptr, c"cow_old_len".as_ptr());

    match (*desc).kind {
        RY_COW_LIST => {
            let old_data_field = LLVMBuildStructGEP2(b, header_ty, data_ptr, 2, c"cow_old_data_field".as_ptr());
            let old_data = LLVMBuildLoad2(b, ptr_ty, old_data_field, c"cow_old_data".as_ptr());
            let buf_size = LLVMBuildMul(b, old_len, LLVMConstInt(i64_ty, (*desc).elem_size, 0), c"cow_buf_size".as_ptr());
            let new_buf = alloc_buf(buf_size, c"cow_new_buf".as_ptr());
            memcpy_to(new_buf, old_data, buf_size);
            let new_len_ptr = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 0, c"cow_new_len_ptr".as_ptr());
            LLVMBuildStore(b, old_len, new_len_ptr);
            let new_cap_ptr = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 1, c"cow_new_cap_ptr".as_ptr());
            LLVMBuildStore(b, old_len, new_cap_ptr);
            let new_data_field = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 2, c"cow_new_data_ptr".as_ptr());
            LLVMBuildStore(b, new_buf, new_data_field);
        }
        RY_COW_MAP => {
            let old_keys_field = LLVMBuildStructGEP2(b, header_ty, data_ptr, 2, c"cow_old_keys_field".as_ptr());
            let old_keys = LLVMBuildLoad2(b, ptr_ty, old_keys_field, c"cow_old_keys".as_ptr());
            let old_vals_field = LLVMBuildStructGEP2(b, header_ty, data_ptr, 3, c"cow_old_vals_field".as_ptr());
            let old_vals = LLVMBuildLoad2(b, ptr_ty, old_vals_field, c"cow_old_vals".as_ptr());
            let old_bc_ptr = LLVMBuildStructGEP2(b, header_ty, data_ptr, 4, c"cow_old_bc_ptr".as_ptr());
            let old_bc = LLVMBuildLoad2(b, i64_ty, old_bc_ptr, c"cow_old_bc".as_ptr());
            let old_bk_field = LLVMBuildStructGEP2(b, header_ty, data_ptr, 5, c"cow_old_bk_ptr".as_ptr());
            let old_bk = LLVMBuildLoad2(b, ptr_ty, old_bk_field, c"cow_old_bk".as_ptr());

            let keys_size = LLVMBuildMul(b, old_len, LLVMConstInt(i64_ty, (*desc).key_size, 0), c"cow_keys_size".as_ptr());
            let new_keys = alloc_buf(keys_size, c"cow_new_keys".as_ptr());
            memcpy_to(new_keys, old_keys, keys_size);
            let vals_size = LLVMBuildMul(b, old_len, LLVMConstInt(i64_ty, (*desc).val_size, 0), c"cow_vals_size".as_ptr());
            let new_vals = alloc_buf(vals_size, c"cow_new_vals".as_ptr());
            memcpy_to(new_vals, old_vals, vals_size);
            let bk_size = LLVMBuildMul(b, old_bc, LLVMConstInt(i64_ty, 8, 0), c"cow_bk_size".as_ptr());
            let new_bk = alloc_buf(bk_size, c"cow_new_bk".as_ptr());
            memcpy_to(new_bk, old_bk, bk_size);

            let new_len_ptr = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 0, c"cow_m_len_ptr".as_ptr());
            LLVMBuildStore(b, old_len, new_len_ptr);
            let new_cap_ptr = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 1, c"cow_m_cap_ptr".as_ptr());
            LLVMBuildStore(b, old_len, new_cap_ptr);
            let new_keys_field = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 2, c"cow_m_keys_ptr".as_ptr());
            LLVMBuildStore(b, new_keys, new_keys_field);
            let new_vals_field = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 3, c"cow_m_vals_ptr".as_ptr());
            LLVMBuildStore(b, new_vals, new_vals_field);
            let new_bc_ptr = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 4, c"cow_m_bc_ptr".as_ptr());
            LLVMBuildStore(b, old_bc, new_bc_ptr);
            let new_bk_field = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 5, c"cow_m_bk_ptr".as_ptr());
            LLVMBuildStore(b, new_bk, new_bk_field);
        }
        RY_COW_SET => {
            let old_elems_field = LLVMBuildStructGEP2(b, header_ty, data_ptr, 2, c"cow_old_elems_field".as_ptr());
            let old_elems = LLVMBuildLoad2(b, ptr_ty, old_elems_field, c"cow_old_elems".as_ptr());
            let old_bc_ptr = LLVMBuildStructGEP2(b, header_ty, data_ptr, 3, c"cow_old_bc_ptr".as_ptr());
            let old_bc = LLVMBuildLoad2(b, i64_ty, old_bc_ptr, c"cow_old_bc".as_ptr());
            let old_bk_field = LLVMBuildStructGEP2(b, header_ty, data_ptr, 4, c"cow_old_bk_ptr".as_ptr());
            let old_bk = LLVMBuildLoad2(b, ptr_ty, old_bk_field, c"cow_old_bk".as_ptr());

            let elems_size = LLVMBuildMul(b, old_len, LLVMConstInt(i64_ty, (*desc).elem_size, 0), c"cow_elems_size".as_ptr());
            let new_elems = alloc_buf(elems_size, c"cow_new_elems".as_ptr());
            memcpy_to(new_elems, old_elems, elems_size);
            let bk_size = LLVMBuildMul(b, old_bc, LLVMConstInt(i64_ty, 8, 0), c"cow_bk_size".as_ptr());
            let new_bk = alloc_buf(bk_size, c"cow_new_bk".as_ptr());
            memcpy_to(new_bk, old_bk, bk_size);

            let new_len_ptr = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 0, c"cow_s_len_ptr".as_ptr());
            LLVMBuildStore(b, old_len, new_len_ptr);
            let new_cap_ptr = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 1, c"cow_s_cap_ptr".as_ptr());
            LLVMBuildStore(b, old_len, new_cap_ptr);
            let new_elems_field = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 2, c"cow_s_elems_ptr".as_ptr());
            LLVMBuildStore(b, new_elems, new_elems_field);
            let new_bc_ptr = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 3, c"cow_s_bc_ptr".as_ptr());
            LLVMBuildStore(b, old_bc, new_bc_ptr);
            let new_bk_field = LLVMBuildStructGEP2(b, header_ty, new_data_ptr, 4, c"cow_s_bk_ptr".as_ptr());
            LLVMBuildStore(b, new_bk, new_bk_field);
        }
        _ => return 0,
    }

    // Element / key retain loops (always RY_ARC_NONATOMIC — the clone is private).
    if (*desc).do_elem_retain != 0 {
        emit_cow_retain_loop(ctx, header_ty, new_data_ptr, i64_ty, i8_ty, ptr_ty, data_field_idx, (*desc).elem_is_str, b"elem");
    }
    if (*desc).do_key_retain != 0 {
        emit_cow_retain_loop(ctx, header_ty, new_data_ptr, i64_ty, i8_ty, ptr_ty, key_field_idx, (*desc).key_is_str, b"key");
    }

    // Release the old header (the helper leaves the builder on arc.done).
    let header_id = intern(cx(ctx), to_ry_value(header_ptr));
    ry_emit_arc_release(ctx, header_id, atomic_mode, (*desc).destructor_callee, std::ptr::null_mut());

    LLVMBuildStore(b, new_data_ptr, slot_ptr);

    let copy_end_bb = LLVMGetInsertBlock(b);
    LLVMBuildBr(b, cont_bb);

    LLVMPositionBuilderAtEnd(b, cont_bb);
    let phi = LLVMBuildPhi(b, ptr_ty, c"cow_ptr".as_ptr());
    let mut vals = [data_ptr, new_data_ptr];
    let mut blocks = [orig_bb, copy_end_bb];
    LLVMAddIncoming(phi, vals.as_mut_ptr(), blocks.as_mut_ptr(), 2);
    intern(cx(ctx), to_ry_value(phi))
}

// ----- Stage 2-C Any -----------------------------------------

#[no_mangle]
pub unsafe extern "C" fn ry_emit_any_wrap(
    ctx: *mut RyEmitCtx,
    desc: *const RyAnyWrapDesc,
) -> RyValueId {
    if ctx.is_null() || desc.is_null() {
        return 0;
    }
    let c = cx(ctx);
    if c.context.is_null() || c.module.is_null() || c.builder.is_null() || (*desc).any_ty.is_null()
    {
        return 0;
    }
    if (*desc).kind > 2 {
        return 0;
    }
    let b = c.builder;
    let context = c.context;
    let module = c.module;
    let val = as_value(resolve(c, (*desc).val_id));
    let any_ty = as_type((*desc).any_ty);
    if val.is_null() {
        return 0;
    }
    let i8_ty = i8_type(context);
    let i64_ty = i64_type(context);
    let i1_ty = i1_type(context);

    // RecordBox=1 / EnumBox=2 — heap-box layout `[ ArcHeader | desc ptr | payload ]`.
    if (*desc).kind == 1 || (*desc).kind == 2 {
        let layout_ty = as_type((*desc).box_layout_ty);
        let descriptor = as_value(resolve(c, (*desc).descriptor_id));
        if layout_ty.is_null() || descriptor.is_null() {
            return 0;
        }
        let dl = LLVMGetModuleDataLayout(module);
        let expected = LLVMABISizeOfType(dl, layout_ty);
        if (*desc).box_data_size != expected {
            return 0;
        }
        let box_data_size_c = LLVMConstInt(i64_ty, (*desc).box_data_size, 0);
        let header_ptr = emit_inline_arc_alloc(c, box_data_size_c);
        let mut dp_gep = [LLVMConstInt(i64_ty, ARC_HEADER_SIZE, 0)];
        let data_ptr = LLVMBuildGEP2(b, i8_ty, header_ptr, dp_gep.as_mut_ptr(), 1, c"arc_box_data".as_ptr());

        // Labels differ between RecordBox ("any.rec.*") and EnumBox ("any.enum.*").
        let (desc_slot_lbl, payload_slot_lbl, tmp_lbl, tag_lbl, data_lbl, val_lbl): (
            &CStr,
            &CStr,
            &CStr,
            &CStr,
            &CStr,
            &CStr,
        ) = if (*desc).kind == 2 {
            (
                c"any.enum.desc.slot",
                c"any.enum.payload.slot",
                c"any.enum.tmp",
                c"any.enum.tag",
                c"any.enum.data",
                c"any.enum.val",
            )
        } else {
            (
                c"any.rec.desc.slot",
                c"any.rec.fields.slot",
                c"any.rec.tmp",
                c"any.rec.tag",
                c"any.rec.data",
                c"any.rec.val",
            )
        };

        let desc_ptr_slot = LLVMBuildStructGEP2(b, layout_ty, data_ptr, 0, desc_slot_lbl.as_ptr());
        LLVMBuildStore(b, descriptor, desc_ptr_slot);
        let payload_slot = LLVMBuildStructGEP2(b, layout_ty, data_ptr, 1, payload_slot_lbl.as_ptr());
        LLVMBuildStore(b, val, payload_slot);

        let tmp = LLVMBuildAlloca(b, any_ty, tmp_lbl.as_ptr());
        let tag_slot = LLVMBuildStructGEP2(b, any_ty, tmp, 0, tag_lbl.as_ptr());
        LLVMBuildStore(b, LLVMConstInt(i64_ty, (*desc).target_tag as u64, 0), tag_slot);
        let any_data_slot = LLVMBuildStructGEP2(b, any_ty, tmp, 1, data_lbl.as_ptr());
        LLVMBuildStore(b, data_ptr, any_data_slot);
        let result = LLVMBuildLoad2(b, any_ty, tmp, val_lbl.as_ptr());
        return intern(c, to_ry_value(result));
    }

    // NonBox=0 — retain BEFORE the alloca+store (the two flags are exclusive).
    if (*desc).do_collection_retain != 0 {
        let mut gep = [LLVMConstInt(i64_ty, (-(ARC_HEADER_SIZE as i64)) as u64, 0)];
        let hdr = LLVMBuildGEP2(b, i8_ty, val, gep.as_mut_ptr(), 1, c"arc_hdr_from_data".as_ptr());
        let hdr_id = intern(c, to_ry_value(hdr));
        arc_retain_impl(c, hdr_id, RY_ARC_NONATOMIC);
    } else if (*desc).do_str_retain != 0 {
        let mut gep = [LLVMConstInt(i64_ty, (-(STRING_HEADER_SIZE as i64)) as u64, 0)];
        let hdr = LLVMBuildGEP2(b, i8_ty, val, gep.as_mut_ptr(), 1, c"str_hdr_from_data".as_ptr());
        let hdr_id = intern(c, to_ry_value(hdr));
        arc_retain_impl(c, hdr_id, RY_ARC_NONATOMIC);
    }

    let mut val_m = val;
    if LLVMTypeOf(val_m) == i1_ty {
        val_m = LLVMBuildZExt(b, val_m, i64_ty, c"any.bool.zext".as_ptr());
    }
    let tmp = LLVMBuildAlloca(b, any_ty, c"any.tmp".as_ptr());
    let tag_ptr = LLVMBuildStructGEP2(b, any_ty, tmp, 0, c"any.tag".as_ptr());
    LLVMBuildStore(b, LLVMConstInt(i64_ty, (*desc).target_tag as u64, 0), tag_ptr);
    let data_ptr = LLVMBuildStructGEP2(b, any_ty, tmp, 1, c"any.data".as_ptr());
    LLVMBuildStore(b, val_m, data_ptr);
    let result = LLVMBuildLoad2(b, any_ty, tmp, c"any.val".as_ptr());
    intern(c, to_ry_value(result))
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_any_unwrap(
    ctx: *mut RyEmitCtx,
    desc: *const RyAnyUnwrapDesc,
) -> RyValueId {
    if ctx.is_null() || desc.is_null() {
        return 0;
    }
    let c = cx(ctx);
    if c.context.is_null() || c.module.is_null() || c.builder.is_null() || (*desc).any_ty.is_null()
    {
        return 0;
    }
    if (*desc).kind > 2 {
        return 0;
    }
    let b = c.builder;
    let context = c.context;
    let module = c.module;
    let any_val = as_value(resolve(c, (*desc).any_val_id));
    let any_ty = as_type((*desc).any_ty);
    if any_val.is_null() {
        return 0;
    }
    let i8_ty = i8_type(context);
    let i64_ty = i64_type(context);
    let ptr_ty = ptr_type(context);
    let f64_ty = LLVMDoubleTypeInContext(context);

    let tag = LLVMBuildExtractValue(b, any_val, 0, c"any.tag.val".as_ptr());
    // Builder-derived parent (builder-derived parent rule).
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

    let mismatch_msg = cstr_bytes((*desc).mismatch_msg);
    let mismatch_name = (*desc).mismatch_global_name;

    // Record=2 — tag check + descriptor chain walk.
    if (*desc).kind == 2 {
        let layout_ty = as_type((*desc).box_layout_ty);
        let record_struct_ty = as_type((*desc).record_struct_ty);
        let expected_desc = as_value(resolve(c, (*desc).expected_desc_id));
        if layout_ty.is_null() || record_struct_ty.is_null() || expected_desc.is_null() {
            return 0;
        }
        let desc_mismatch_msg = cstr_bytes((*desc).desc_mismatch_msg);
        let desc_mismatch_name = (*desc).desc_mismatch_global_name;

        let tag_match_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.rec.tag_ok".as_ptr());
        let tag_mismatch_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.rec.tag_err".as_ptr());
        let desc_check_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.rec.desc_check".as_ptr());
        let desc_mismatch_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.rec.desc_err".as_ptr());

        let is_record = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntEQ, tag, LLVMConstInt(i64_ty, (*desc).expected_tag as u64, 0), c"any.is_record".as_ptr());
        LLVMBuildCondBr(b, is_record, tag_match_bb, tag_mismatch_bb);

        LLVMPositionBuilderAtEnd(b, tag_mismatch_bb);
        emit_inline_runtime_error(c, mismatch_msg, mismatch_name);

        LLVMPositionBuilderAtEnd(b, tag_match_bb);
        let tmp = LLVMBuildAlloca(b, any_ty, c"any.rec.tmp".as_ptr());
        LLVMBuildStore(b, any_val, tmp);
        let any_data_slot = LLVMBuildStructGEP2(b, any_ty, tmp, 1, c"any.rec.data.ptr".as_ptr());
        let data_ptr = LLVMBuildLoad2(b, ptr_ty, any_data_slot, c"any.rec.data".as_ptr());
        let desc_slot = LLVMBuildStructGEP2(b, layout_ty, data_ptr, 0, c"any.rec.desc.slot".as_ptr());
        let actual_desc = LLVMBuildLoad2(b, ptr_ty, desc_slot, c"any.rec.desc".as_ptr());

        let mut is_subtype_p = [ptr_ty, ptr_ty];
        let is_subtype_ty = LLVMFunctionType(i64_ty, is_subtype_p.as_mut_ptr(), 2, 0);
        let is_subtype_fn = get_or_insert_function(module, c"__ry_record_is_subtype_desc".as_ptr(), is_subtype_ty);
        let mut is_subtype_a = [actual_desc, expected_desc];
        let chain_ok = LLVMBuildCall2(b, is_subtype_ty, is_subtype_fn, is_subtype_a.as_mut_ptr(), 2, c"any.rec.chain.ok".as_ptr());
        let chain_bool = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntNE, chain_ok, LLVMConstInt(i64_ty, 0, 0), c"any.rec.chain.bool".as_ptr());
        LLVMBuildCondBr(b, chain_bool, desc_check_bb, desc_mismatch_bb);

        LLVMPositionBuilderAtEnd(b, desc_mismatch_bb);
        emit_inline_runtime_error(c, desc_mismatch_msg, desc_mismatch_name);

        LLVMPositionBuilderAtEnd(b, desc_check_bb);
        let fields_slot = LLVMBuildStructGEP2(b, layout_ty, data_ptr, 1, c"any.rec.fields.slot".as_ptr());
        let record_val = LLVMBuildLoad2(b, record_struct_ty, fields_slot, c"any.rec.unwrap.val".as_ptr());
        return intern(c, to_ry_value(record_val));
    }

    // F64Promote=1 — 5 BBs; merge PHI(f64).
    if (*desc).kind == 1 {
        let float_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.float".as_ptr());
        let check_int_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.check_int".as_ptr());
        let int_promote_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.int2float".as_ptr());
        let mismatch_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.mismatch".as_ptr());
        let merge_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.merge".as_ptr());

        let tmp = LLVMBuildAlloca(b, any_ty, c"any.tmp.fp".as_ptr());
        LLVMBuildStore(b, any_val, tmp);
        let data_ptr = LLVMBuildStructGEP2(b, any_ty, tmp, 1, c"any.data.fp".as_ptr());

        let is_float = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntEQ, tag, LLVMConstInt(i64_ty, RY_ANY_TAG_FLOAT as u64, 0), c"is.float".as_ptr());
        LLVMBuildCondBr(b, is_float, float_bb, check_int_bb);

        LLVMPositionBuilderAtEnd(b, check_int_bb);
        let is_int = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntEQ, tag, LLVMConstInt(i64_ty, RY_ANY_TAG_INT as u64, 0), c"is.int".as_ptr());
        LLVMBuildCondBr(b, is_int, int_promote_bb, mismatch_bb);

        LLVMPositionBuilderAtEnd(b, mismatch_bb);
        emit_inline_runtime_error(c, mismatch_msg, mismatch_name);

        LLVMPositionBuilderAtEnd(b, float_bb);
        let float_val = LLVMBuildLoad2(b, f64_ty, data_ptr, c"any.f64".as_ptr());
        LLVMBuildBr(b, merge_bb);

        LLVMPositionBuilderAtEnd(b, int_promote_bb);
        let int_val = LLVMBuildLoad2(b, i64_ty, data_ptr, c"any.i64".as_ptr());
        let promoted = LLVMBuildSIToFP(b, int_val, f64_ty, c"any.i2f".as_ptr());
        LLVMBuildBr(b, merge_bb);

        LLVMPositionBuilderAtEnd(b, merge_bb);
        let phi = LLVMBuildPhi(b, f64_ty, c"any.unwrap.f64".as_ptr());
        let mut vals = [float_val, promoted];
        let mut blocks = [float_bb, int_promote_bb];
        LLVMAddIncoming(phi, vals.as_mut_ptr(), blocks.as_mut_ptr(), 2);
        return intern(c, to_ry_value(phi));
    }

    // Standard=0 — 2-way path.
    let target_ty = as_type((*desc).target_ty);
    if target_ty.is_null() {
        return 0;
    }
    let cmp = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntEQ, tag, LLVMConstInt(i64_ty, (*desc).expected_tag as u64, 0), c"any.tag.check".as_ptr());
    let match_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.match".as_ptr());
    let mismatch_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"any.mismatch".as_ptr());
    LLVMBuildCondBr(b, cmp, match_bb, mismatch_bb);

    LLVMPositionBuilderAtEnd(b, mismatch_bb);
    emit_inline_runtime_error(c, mismatch_msg, mismatch_name);

    LLVMPositionBuilderAtEnd(b, match_bb);
    let tmp = LLVMBuildAlloca(b, any_ty, c"any.tmp".as_ptr());
    LLVMBuildStore(b, any_val, tmp);
    let data_ptr = LLVMBuildStructGEP2(b, any_ty, tmp, 1, c"any.data.ptr".as_ptr());
    let unwrapped = LLVMBuildLoad2(b, target_ty, data_ptr, c"any.unwrap.val".as_ptr());

    if (*desc).do_collection_retain != 0 {
        let mut gep_i = [LLVMConstInt(i64_ty, (-(ARC_HEADER_SIZE as i64)) as u64, 0)];
        let hdr = LLVMBuildGEP2(b, i8_ty, unwrapped, gep_i.as_mut_ptr(), 1, c"arc_hdr_from_data".as_ptr());
        let hdr_id = intern(c, to_ry_value(hdr));
        arc_retain_impl(c, hdr_id, RY_ARC_NONATOMIC);
    } else if (*desc).do_str_retain != 0 {
        let mut gep_i = [LLVMConstInt(i64_ty, (-(STRING_HEADER_SIZE as i64)) as u64, 0)];
        let hdr = LLVMBuildGEP2(b, i8_ty, unwrapped, gep_i.as_mut_ptr(), 1, c"str_hdr_from_data".as_ptr());
        let hdr_id = intern(c, to_ry_value(hdr));
        arc_retain_impl(c, hdr_id, RY_ARC_NONATOMIC);
    }
    intern(c, to_ry_value(unwrapped))
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_any_try_unwrap(
    ctx: *mut RyEmitCtx,
    desc: *const RyAnyTryUnwrapDesc,
) -> RyValueId {
    if ctx.is_null() || desc.is_null() {
        return 0;
    }
    let c = cx(ctx);
    if c.context.is_null()
        || c.module.is_null()
        || c.builder.is_null()
        || (*desc).any_ty.is_null()
        || (*desc).res_ty.is_null()
        || (*desc).error_ty.is_null()
    {
        return 0;
    }
    if (*desc).kind > 1 {
        return 0;
    }
    let b = c.builder;
    let context = c.context;
    let any_val = as_value(resolve(c, (*desc).any_val_id));
    let any_ty = as_type((*desc).any_ty);
    let res_ty = as_type((*desc).res_ty);
    let error_ty = as_type((*desc).error_ty);
    let err_msg_str = as_value(resolve(c, (*desc).err_msg_str_id));
    if any_val.is_null() || err_msg_str.is_null() {
        return 0;
    }
    let i8_ty = i8_type(context);
    let i64_ty = i64_type(context);
    let i1_ty = i1_type(context);
    let f64_ty = LLVMDoubleTypeInContext(context);

    let tag = LLVMBuildExtractValue(b, any_val, 0, c"tryany.tag".as_ptr());
    let fn_v = LLVMGetBasicBlockParent(LLVMGetInsertBlock(b));

    // Inline buildOkValue / buildErrValue (mirror impl.cpp lambdas): the
    // tryRetainArcSource call there is a no-op for freshly-extracted loads, so
    // the explicit retain via do_collection_retain / do_str_retain carries it.
    let build_ok = move |inner: LLVMValueRef| -> LLVMValueRef {
        unsafe {
            let mut v = LLVMConstNull(res_ty);
            v = LLVMBuildInsertValue(b, v, LLVMConstInt(i1_ty, 1, 0), 0, c"res.ok".as_ptr());
            v = LLVMBuildInsertValue(b, v, inner, 1, c"res.ok_val".as_ptr());
            v = LLVMBuildInsertValue(b, v, LLVMConstNull(LLVMStructGetTypeAtIndex(res_ty, 2)), 2, c"".as_ptr());
            v
        }
    };
    let build_err = move || -> LLVMValueRef {
        unsafe {
            let mut err_val = LLVMGetUndef(error_ty);
            err_val = LLVMBuildInsertValue(b, err_val, err_msg_str, 0, c"".as_ptr());
            err_val = LLVMBuildInsertValue(b, err_val, LLVMConstInt(i64_ty, 0, 0), 1, c"".as_ptr());
            let mut v = LLVMConstNull(res_ty);
            v = LLVMBuildInsertValue(b, v, LLVMConstInt(i1_ty, 0, 0), 0, c"res.err".as_ptr());
            v = LLVMBuildInsertValue(b, v, LLVMConstNull(LLVMStructGetTypeAtIndex(res_ty, 1)), 1, c"".as_ptr());
            v = LLVMBuildInsertValue(b, v, err_val, 2, c"res.err_val".as_ptr());
            v
        }
    };

    // F64Promote=1 — both loads share one alloca; Ok arm selects via isFloat.
    if (*desc).kind == 1 {
        let tmp = LLVMBuildAlloca(b, any_ty, c"tryany.fp.tmp".as_ptr());
        LLVMBuildStore(b, any_val, tmp);
        let data_ptr = LLVMBuildStructGEP2(b, any_ty, tmp, 1, c"tryany.fp.data".as_ptr());
        let f_val = LLVMBuildLoad2(b, f64_ty, data_ptr, c"tryany.fp.fval".as_ptr());
        let i_val = LLVMBuildLoad2(b, i64_ty, data_ptr, c"tryany.fp.ival".as_ptr());
        let promoted = LLVMBuildSIToFP(b, i_val, f64_ty, c"tryany.fp.i2f".as_ptr());
        let is_float = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntEQ, tag, LLVMConstInt(i64_ty, RY_ANY_TAG_FLOAT as u64, 0), c"tryany.fp.is_float".as_ptr());
        let is_int = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntEQ, tag, LLVMConstInt(i64_ty, RY_ANY_TAG_INT as u64, 0), c"tryany.fp.is_int".as_ptr());
        let is_accept = LLVMBuildOr(b, is_float, is_int, c"tryany.fp.is_accept".as_ptr());
        let is_err = LLVMBuildNot(b, is_accept, c"tryany.fp.is_err".as_ptr());

        let ok_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.ok".as_ptr());
        let err_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.err".as_ptr());
        let merge_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.merge".as_ptr());
        LLVMBuildCondBr(b, is_err, err_bb, ok_bb);

        LLVMPositionBuilderAtEnd(b, ok_bb);
        let chosen = LLVMBuildSelect(b, is_float, f_val, promoted, c"tryany.fp.val".as_ptr());
        let ok_val = build_ok(chosen);
        LLVMBuildBr(b, merge_bb);
        let ok_incoming = LLVMGetInsertBlock(b);

        LLVMPositionBuilderAtEnd(b, err_bb);
        let err_val = build_err();
        LLVMBuildBr(b, merge_bb);
        let err_incoming = LLVMGetInsertBlock(b);

        LLVMPositionBuilderAtEnd(b, merge_bb);
        let phi = LLVMBuildPhi(b, res_ty, c"result".as_ptr());
        let mut vals = [ok_val, err_val];
        let mut blocks = [ok_incoming, err_incoming];
        LLVMAddIncoming(phi, vals.as_mut_ptr(), blocks.as_mut_ptr(), 2);
        return intern(c, to_ry_value(phi));
    }

    // Standard=0 — tag-check primitive arm.
    let target_ty = as_type((*desc).target_ty);
    if target_ty.is_null() {
        return 0;
    }
    let cmp = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntEQ, tag, LLVMConstInt(i64_ty, (*desc).expected_tag as u64, 0), c"tryany.tag.eq".as_ptr());
    let is_err = LLVMBuildNot(b, cmp, c"tryany.is_err".as_ptr());

    let ok_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.ok".as_ptr());
    let err_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.err".as_ptr());
    let merge_bb = LLVMAppendBasicBlockInContext(context, fn_v, c"res.merge".as_ptr());
    LLVMBuildCondBr(b, is_err, err_bb, ok_bb);

    LLVMPositionBuilderAtEnd(b, ok_bb);
    let tmp = LLVMBuildAlloca(b, any_ty, c"tryany.tmp".as_ptr());
    LLVMBuildStore(b, any_val, tmp);
    let data_ptr = LLVMBuildStructGEP2(b, any_ty, tmp, 1, c"tryany.data".as_ptr());
    let unwrapped = LLVMBuildLoad2(b, target_ty, data_ptr, c"tryany.val".as_ptr());
    if (*desc).do_collection_retain != 0 {
        let mut gep_i = [LLVMConstInt(i64_ty, (-(ARC_HEADER_SIZE as i64)) as u64, 0)];
        let hdr = LLVMBuildGEP2(b, i8_ty, unwrapped, gep_i.as_mut_ptr(), 1, c"arc_hdr_from_data".as_ptr());
        let hdr_id = intern(c, to_ry_value(hdr));
        arc_retain_impl(c, hdr_id, RY_ARC_NONATOMIC);
    } else if (*desc).do_str_retain != 0 {
        let mut gep_i = [LLVMConstInt(i64_ty, (-(STRING_HEADER_SIZE as i64)) as u64, 0)];
        let hdr = LLVMBuildGEP2(b, i8_ty, unwrapped, gep_i.as_mut_ptr(), 1, c"str_hdr_from_data".as_ptr());
        let hdr_id = intern(c, to_ry_value(hdr));
        arc_retain_impl(c, hdr_id, RY_ARC_NONATOMIC);
    }
    let ok_val = build_ok(unwrapped);
    LLVMBuildBr(b, merge_bb);
    let ok_incoming = LLVMGetInsertBlock(b);

    LLVMPositionBuilderAtEnd(b, err_bb);
    let err_val = build_err();
    LLVMBuildBr(b, merge_bb);
    let err_incoming = LLVMGetInsertBlock(b);

    LLVMPositionBuilderAtEnd(b, merge_bb);
    let phi = LLVMBuildPhi(b, res_ty, c"result".as_ptr());
    let mut vals = [ok_val, err_val];
    let mut blocks = [ok_incoming, err_incoming];
    LLVMAddIncoming(phi, vals.as_mut_ptr(), blocks.as_mut_ptr(), 2);
    intern(c, to_ry_value(phi))
}

// ----- Stage 2-C ControlFlow ---------------------------------

#[no_mangle]
pub unsafe extern "C" fn ry_emit_create_basic_block(
    ctx: *mut RyEmitCtx,
    name: *const c_char,
    fn_handle: RyFunctionHandle,
) -> RyBasicBlockRef {
    let c = cx(ctx);
    let nm = if name.is_null() { c"".as_ptr() } else { name };
    let bb = LLVMAppendBasicBlockInContext(c.context, as_function(fn_handle), nm);
    to_ry_bb(bb)
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_branch_cond(
    ctx: *mut RyEmitCtx,
    cond: RyValueId,
    true_bb: RyBasicBlockRef,
    false_bb: RyBasicBlockRef,
) {
    let c = cx(ctx);
    let cond_val = as_value(resolve(c, cond));
    LLVMBuildCondBr(c.builder, cond_val, as_bb(true_bb), as_bb(false_bb));
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_branch_uncond(ctx: *mut RyEmitCtx, target: RyBasicBlockRef) {
    let c = cx(ctx);
    LLVMBuildBr(c.builder, as_bb(target));
}

#[no_mangle]
pub unsafe extern "C" fn ry_emit_create_phi(
    ctx: *mut RyEmitCtx,
    ty: RyTypeRef,
    incoming_values: *const RyValueId,
    incoming_blocks: *const RyBasicBlockRef,
    count: u32,
    name_hint: *const c_char,
) -> RyValueId {
    let c = cx(ctx);
    let nm = if name_hint.is_null() { c"".as_ptr() } else { name_hint };
    let phi = LLVMBuildPhi(c.builder, as_type(ty), nm);
    for i in 0..count as usize {
        let mut v = [as_value(resolve(c, *incoming_values.add(i)))];
        let mut bb = [as_bb(*incoming_blocks.add(i))];
        LLVMAddIncoming(phi, v.as_mut_ptr(), bb.as_mut_ptr(), 1);
    }
    intern(c, to_ry_value(phi))
}
