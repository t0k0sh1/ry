//! Shared internal emission helpers used across the ABI op modules:
//! the concrete `EmitCtxImpl` behind the opaque ctx handle, intern/resolve,
//! opaque-handle casts, LLVM type constructors, name builders, module-global
//! lookup, layout constants, and the inline runtime-error / ARC-alloc helpers.

use std::collections::HashMap;
use std::ffi::{c_char, CStr, CString};

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{LLVMAtomicOrdering, LLVMAtomicRMWBinOp, LLVMLinkage, LLVMUnnamedAddr};

use crate::ffi::*;

// Concrete object behind the opaque `*mut RyEmitCtx` handle. values[0] is the
// null sentinel.
pub(crate) struct EmitCtxImpl {
    pub(crate) module: LLVMModuleRef,
    pub(crate) builder: LLVMBuilderRef,
    pub(crate) context: LLVMContextRef,
    pub(crate) function: LLVMValueRef,
    pub(crate) values: Vec<LLVMValueRef>,
    // Dedup cache for ry_emit_bounds_error / any-error fmt-string globals
    // (keyed by message bytes).
    pub(crate) bounds_msg_cache: HashMap<Vec<u8>, LLVMValueRef>,
}

#[inline]
pub(crate) unsafe fn cx<'a>(p: *mut RyEmitCtx) -> &'a mut EmitCtxImpl {
    &mut *(p as *mut EmitCtxImpl)
}

// Opaque ABI handle → llvm-sys C API ref (pointer cast, this crate only).
#[inline]
pub(crate) fn as_type(p: RyTypeRef) -> LLVMTypeRef {
    p as LLVMTypeRef
}
#[inline]
pub(crate) fn as_functype(p: RyFuncTypeRef) -> LLVMTypeRef {
    p as LLVMTypeRef
}
#[inline]
pub(crate) fn as_value(p: RyValueRef) -> LLVMValueRef {
    p as LLVMValueRef
}
#[inline]
pub(crate) fn as_function(p: RyFunctionHandle) -> LLVMValueRef {
    p as LLVMValueRef
}
#[inline]
pub(crate) fn as_bb(p: RyBasicBlockRef) -> LLVMBasicBlockRef {
    p as LLVMBasicBlockRef
}
#[inline]
pub(crate) fn to_ry_value(v: LLVMValueRef) -> RyValueRef {
    v as RyValueRef
}
#[inline]
pub(crate) fn to_ry_bb(b: LLVMBasicBlockRef) -> RyBasicBlockRef {
    b as RyBasicBlockRef
}

// LLVM type accessors.
#[inline]
pub(crate) unsafe fn i1_type(c: LLVMContextRef) -> LLVMTypeRef {
    LLVMInt1TypeInContext(c)
}
#[inline]
pub(crate) unsafe fn i32_type(c: LLVMContextRef) -> LLVMTypeRef {
    LLVMInt32TypeInContext(c)
}
#[inline]
pub(crate) unsafe fn i64_type(c: LLVMContextRef) -> LLVMTypeRef {
    LLVMInt64TypeInContext(c)
}
#[inline]
pub(crate) unsafe fn ptr_type(c: LLVMContextRef) -> LLVMTypeRef {
    LLVMPointerTypeInContext(c, 0)
}
#[inline]
pub(crate) unsafe fn void_type(c: LLVMContextRef) -> LLVMTypeRef {
    LLVMVoidTypeInContext(c)
}

// Bridge llvm::Value* ↔ RyValueId handle space. The internal forms take
// &mut/&EmitCtxImpl so callers already holding the borrow do not re-alias
// through the public ABI entry points.
#[inline]
pub(crate) unsafe fn intern(c: &mut EmitCtxImpl, value: RyValueRef) -> RyValueId {
    if value.is_null() {
        return 0;
    }
    let id = c.values.len() as RyValueId;
    c.values.push(as_value(value));
    id
}

#[inline]
pub(crate) unsafe fn resolve(c: &EmitCtxImpl, id: RyValueId) -> RyValueRef {
    if id == 0 || id as usize >= c.values.len() {
        return std::ptr::null_mut();
    }
    to_ry_value(c.values[id as usize])
}

// Borrow C-string bytes without the NUL (empty slice on NULL).
#[inline]
pub(crate) unsafe fn cstr_bytes<'a>(p: *const c_char) -> &'a [u8] {
    if p.is_null() {
        b""
    } else {
        CStr::from_ptr(p).to_bytes()
    }
}

// Build a NUL-terminated CString name from a prefix + suffix (for SSA names
// like "{prefix}_idx").
#[inline]
pub(crate) fn cname_pfx(prefix: &[u8], suffix: &[u8]) -> CString {
    let mut v = Vec::with_capacity(prefix.len() + suffix.len());
    v.extend_from_slice(prefix);
    v.extend_from_slice(suffix);
    CString::new(v).unwrap()
}

// Three-part CString name (e.g. "cow_" + tag + "_len_ptr") for the CoW retain
// loops.
#[inline]
pub(crate) fn cname3(a: &[u8], b: &[u8], c: &[u8]) -> CString {
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
pub(crate) unsafe fn get_or_insert_function(
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
pub(crate) unsafe fn get_or_insert_global(
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

// A private, unnamed_addr, align-1 constant string global, deduped by message
// bytes within this ctx. `name` is the already-defaulted global name.
pub(crate) unsafe fn get_or_create_msg_global(
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
pub(crate) const ARC_HEADER_SIZE: u64 = 16;
pub(crate) const ARC_IMMORTAL: i64 = i64::MAX;

#[inline]
pub(crate) unsafe fn i8_type(c: LLVMContextRef) -> LLVMTypeRef {
    LLVMInt8TypeInContext(c)
}

// Anonymous {i64, i64} ARC header struct.
#[inline]
pub(crate) unsafe fn arc_header_type(c: LLVMContextRef) -> LLVMTypeRef {
    let mut elems = [i64_type(c), i64_type(c)];
    LLVMStructTypeInContext(c, elems.as_mut_ptr(), 2, 0)
}

// Inttoptr the process-global ARC live-count address (captured at JIT compile
// time) and atomicrmw add `delta`.
pub(crate) unsafe fn emit_arc_counter_delta(
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

// NotAtomic ordering uses a plain load (ABI-default alignment) to match the
// non-atomic codepath byte-for-byte; otherwise an 8-byte-aligned atomic load
// with the given ordering.
pub(crate) unsafe fn emit_atomic_i64_load(
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

// Load a list header's {len, cap, data} via StructGEP + load. `prefix`
// concatenates with _len_ptr / _cap_ptr / _data_ptr / _len / _cap / _data.
pub(crate) struct ListHeaderLoad {
    pub(crate) len_ptr: LLVMValueRef,
    pub(crate) cap_ptr: LLVMValueRef,
    pub(crate) data_ptr: LLVMValueRef,
    pub(crate) len: LLVMValueRef,
    pub(crate) cap: LLVMValueRef,
    pub(crate) data: LLVMValueRef,
}

pub(crate) unsafe fn load_list_header(
    b: LLVMBuilderRef,
    list_header_ty: LLVMTypeRef,
    list_ptr: LLVMValueRef,
    i64_ty: LLVMTypeRef,
    ptr_ty: LLVMTypeRef,
    prefix: &[u8],
) -> ListHeaderLoad {
    let len_ptr = LLVMBuildStructGEP2(
        b,
        list_header_ty,
        list_ptr,
        0,
        cname_pfx(prefix, b"_len_ptr").as_ptr(),
    );
    let cap_ptr = LLVMBuildStructGEP2(
        b,
        list_header_ty,
        list_ptr,
        1,
        cname_pfx(prefix, b"_cap_ptr").as_ptr(),
    );
    let data_ptr = LLVMBuildStructGEP2(
        b,
        list_header_ty,
        list_ptr,
        2,
        cname_pfx(prefix, b"_data_ptr").as_ptr(),
    );
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
pub(crate) const STRING_HEADER_SIZE: u64 = 24;
// RyAnyTag discriminants (mirror include/ry/ry_layout.hpp RyAnyTag).
pub(crate) const RY_ANY_TAG_INT: i64 = 0;
pub(crate) const RY_ANY_TAG_FLOAT: i64 = 1;

// Emit the inline runtime-error sequence: fprintf(stderr, msg) + fflush +
// _Exit(1) + unreachable, which terminates the block. The caller must pre-split
// into err / ok BBs first (the emitRuntimeError-terminates rule).
pub(crate) unsafe fn emit_inline_runtime_error(
    c: &mut EmitCtxImpl,
    msg: &[u8],
    name_hint: *const c_char,
) {
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
    LLVMBuildCall2(
        b,
        fprintf_ty,
        fprintf_fn,
        fprintf_a.as_mut_ptr(),
        2,
        c"".as_ptr(),
    );
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

// malloc ARC_HEADER_SIZE + boxDataSize, bump the live-count (+1), init
// strong=1/weak=0, return the headerPtr (caller GEPs +ARC_HEADER_SIZE for the
// data pointer).
pub(crate) unsafe fn emit_inline_arc_alloc(
    c: &mut EmitCtxImpl,
    box_data_size: LLVMValueRef,
) -> LLVMValueRef {
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
    let header_ptr = LLVMBuildCall2(
        b,
        malloc_ty,
        malloc_fn,
        malloc_a.as_mut_ptr(),
        1,
        c"arc_box".as_ptr(),
    );
    emit_arc_counter_delta(b, i64_ty, ptr_ty, 1);
    let strong_ptr = LLVMBuildStructGEP2(
        b,
        arc_header_ty,
        header_ptr,
        0,
        c"arc_box_strong_ptr".as_ptr(),
    );
    LLVMBuildStore(b, LLVMConstInt(i64_ty, 1, 0), strong_ptr);
    let weak_ptr = LLVMBuildStructGEP2(
        b,
        arc_header_ty,
        header_ptr,
        1,
        c"arc_box_weak_ptr".as_ptr(),
    );
    LLVMBuildStore(b, LLVMConstInt(i64_ty, 0, 0), weak_ptr);
    header_ptr
}
