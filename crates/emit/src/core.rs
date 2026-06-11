//! core — the LLVM IR generation engine (abi-independent). Owns the concrete
//! `EmitCtx` (LLVM emission state + intern table), the Rust-native `ValueRef` /
//! `Atomicity` handle types, the basic-IR layer (LLVM 1:1 type constructors),
//! and the combination layer (name builders, module-global lookup, layout
//! constants, the libc / runtime-error / ARC-alloc emitters). Must NOT reference
//! `crate::abi` — the `core⇏abi` invariant (#2057): `core` stays compilable
//! without the removable `abi` C shell (the #2023 Rust-native end-state), and
//! `core`'s rlib does not depend on the `abi` cdylib once the crate split lands.
//! The only dependency is `llvm_sys` + `std`. See `lib.rs` for the full layering.

use std::collections::HashMap;
use std::ffi::{c_char, CStr, CString};

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{
    LLVMAtomicOrdering, LLVMAtomicRMWBinOp, LLVMIntPredicate, LLVMLinkage, LLVMUnnamedAddr,
};

// Concrete object behind the opaque `*mut RyEmitCtx` handle. values[0] is the
// null sentinel.
pub(crate) struct EmitCtx {
    pub(crate) module: LLVMModuleRef,
    pub(crate) builder: LLVMBuilderRef,
    pub(crate) context: LLVMContextRef,
    pub(crate) values: Vec<LLVMValueRef>,
    // Dedup cache for ry_emit_bounds_error / any-error fmt-string globals
    // (keyed by message bytes).
    pub(crate) bounds_msg_cache: HashMap<Vec<u8>, LLVMValueRef>,
}

impl EmitCtx {
    // Construct a fresh emission context over the raw LLVM handles received
    // across the abi boundary (stored without being dereferenced). Reserves
    // handle 0 as the "invalid" sentinel so resolve(_, 0) -> NULL.
    pub(crate) fn new(
        module: LLVMModuleRef,
        builder: LLVMBuilderRef,
        context: LLVMContextRef,
    ) -> EmitCtx {
        EmitCtx {
            module,
            builder,
            context,
            values: vec![std::ptr::null_mut()],
            bounds_msg_cache: HashMap::new(),
        }
    }
}

/// Rust-native typed handle to an LLVM value. Non-null by convention; nullable
/// boundary inputs (destructor / gc-visit callees) are modelled as
/// `Option<ValueRef>`. Wraps the raw `LLVMValueRef` so the engine API does not
/// traffic in untyped pointers.
#[derive(Clone, Copy)]
pub(crate) struct ValueRef(pub(crate) LLVMValueRef);

/// The two values produced by `list_slice`: the clamped element `count` and the
/// freshly `malloc`'d sub-list `new_data` buffer. Named fields — rather than a
/// bare `(ValueRef, ValueRef)` — so the two same-typed handles cannot be
/// silently transposed where the abi shell interns each into its `*mut
/// RyValueId` out-param. The crate's first multi-value engine return.
pub(crate) struct SliceParts {
    pub(crate) count: ValueRef,
    pub(crate) new_data: ValueRef,
}

/// Rust-native typed handle to an LLVM basic block. Wraps the raw
/// `LLVMBasicBlockRef` so the engine API does not traffic in untyped pointers.
#[derive(Clone, Copy)]
pub(crate) struct BasicBlockRef(pub(crate) LLVMBasicBlockRef);

/// Rust-native typed handle to an LLVM function value.
#[derive(Clone, Copy)]
pub(crate) struct FunctionRef(pub(crate) LLVMValueRef);

/// Rust-native typed handle to an LLVM type.
#[derive(Clone, Copy)]
pub(crate) struct TypeRef(pub(crate) LLVMTypeRef);

/// Rust-native typed handle to an LLVM function type (mirrors the boundary
/// `RyFuncTypeRef`; distinct from `TypeRef` at the engine API, though both wrap
/// an `LLVMTypeRef`).
#[derive(Clone, Copy)]
pub(crate) struct FuncTypeRef(pub(crate) LLVMTypeRef);

/// ARC atomic mode for retain / release.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Atomicity {
    NonAtomic,
    Atomic,
}

/// List vs array selector for the bounds-error message (mirrors the boundary
/// `RY_BOUNDS_LIST` / `RY_BOUNDS_ARRAY` constants; the abi layer maps the
/// `c_int` kind to this before calling the engine).
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum BoundsKind {
    List,
    Array,
}

/// Collection kind selector for the Copy-on-Write clone (mirrors the boundary
/// `RY_COW_LIST` / `RY_COW_MAP` / `RY_COW_SET` constants; the abi layer maps the
/// `c_int` kind to this before calling the engine).
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum CowKind {
    List,
    Map,
    Set,
}

/// Integer-comparison predicate for `build_icmp` (mirrors the boundary
/// `RY_ICMP_*` constants in api.h; the abi layer maps the `c_int` predicate to
/// this before calling the engine, which translates it to the llvm-sys
/// `LLVMIntPredicate`). Added for the #2072 scalar-primitive vocabulary
/// ([C] = (ii) boundary move).
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum IcmpPred {
    Eq,
    Ne,
    Slt,
    Sle,
    Sgt,
    Sge,
    Ult,
    Ule,
    Ugt,
    Uge,
}

/// IR-shape selector for `any_wrap` (mirrors the C++ `lowered::AnyWrapKind`; the
/// abi layer maps the `c_int` kind — `0`/`1`/`2` — to this before calling the
/// engine, rejecting any other value as the legacy `kind > 2` guard did).
// The shared `Box` postfix mirrors the C++ `lowered::AnyWrapKind` variant names
// 1:1 (NonBox / RecordBox / EnumBox); keep the parallel naming over clippy's
// suffix-stripping suggestion.
#[allow(clippy::enum_variant_names)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum AnyWrapKind {
    /// Primitive / str / collection pointer stored directly in `any.data[8]`.
    NonBox,
    /// Heap-box `[ ArcHeader | desc ptr | record struct ]`.
    RecordBox,
    /// Structurally identical heap box carrying the per-enum descriptor.
    EnumBox,
}

/// IR-shape selector for `any_unwrap` (mirrors the C++ `lowered::AnyUnwrapKind`).
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum AnyUnwrapKind {
    /// 2-way tag-check → match (alloca/store/GEP/load) or trap.
    Standard,
    /// 5-BB float / int-promote merge PHI(f64).
    F64Promote,
    /// Tag check + descriptor chain walk, then payload GEP/load.
    Record,
}

/// IR-shape selector for `any_try_unwrap` (mirrors the C++
/// `lowered::AnyTryUnwrapKind`; only two shapes reach this op — the typed-arm
/// dispatch stays CodeGen-private — so the abi mapping rejects `kind > 1`).
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum AnyTryUnwrapKind {
    /// Tag-check primitive arm wrapped in an ok/err branch + PHI.
    Standard,
    /// f64 target with int auto-promote (both loads share one alloca).
    F64Promote,
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

// Borrow C-string bytes without the NUL (empty slice on NULL). A pure `CStr`
// primitive (no `EmitCtx` / intern), used by the prefix-derived name builders
// in the bounds engine. The abi layer re-exports it for legacy in-crate callers
// that still reach it via `crate::abi::*`.
#[inline]
pub(crate) unsafe fn cstr_bytes<'a>(p: *const c_char) -> &'a [u8] {
    if p.is_null() {
        b""
    } else {
        CStr::from_ptr(p).to_bytes()
    }
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
    c: &mut EmitCtx,
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

// === Internal collection / ARC header layout — single source of truth ===
//
// The list/map/set/arc header structs are built here from one field-kind table
// so that BOTH the emitted LLVM struct (cow.rs `cow_ensure_unique`, arc.rs, and
// this module) and the cross-language layout guard (the
// `ry_emit_test_header_layout` test-extern in abi.rs, asserted against C++'s
// canonical listHeaderTy_/mapHeaderTy_/setHeaderTy_/arcHeaderTy_ in
// tests/test_header_layout.cpp) are driven by the *same* definition. A drift vs.
// the C++ named structs (src/codegen.cpp:57-61) was previously guarded only by a
// "Field order MUST stay in sync" comment; it is now caught mechanically by that
// parity test plus the same-type-swap behavioral coverage in
// tests/spec/cow.test.ry (#2071).
//
// Keep `build_header_struct` byte-identical to the previous inline
// `LLVMStructTypeInContext` calls — it feeds a hot codegen path whose IR is
// golden-pinned (codegen-llvm-ir-conventions.md "IR-byte-identical").

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum HdrField {
    I64,
    Ptr,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum HeaderKind {
    List,
    Map,
    Set,
    Arc,
}

// The field kinds of each header, mirroring src/codegen.cpp:57-61.
//   List {len:i64, cap:i64, data:ptr}
//   Map  {len:i64, cap:i64, keys:ptr, vals:ptr, bucket_count:i64, buckets:ptr}
//   Set  {len:i64, cap:i64, elems:ptr, bucket_count:i64, buckets:ptr}
//   Arc  {strong:i64, weak:i64}
pub(crate) fn header_fields(kind: HeaderKind) -> &'static [HdrField] {
    use HdrField::{Ptr, I64};
    match kind {
        HeaderKind::List => &[I64, I64, Ptr],
        HeaderKind::Map => &[I64, I64, Ptr, Ptr, I64, Ptr],
        HeaderKind::Set => &[I64, I64, Ptr, I64, Ptr],
        HeaderKind::Arc => &[I64, I64],
    }
}

// Build the LLVM literal struct for a header from the single-sourced field list.
pub(crate) unsafe fn build_header_struct(c: LLVMContextRef, kind: HeaderKind) -> LLVMTypeRef {
    let fields = header_fields(kind);
    let mut elems: Vec<LLVMTypeRef> = Vec::with_capacity(fields.len());
    for f in fields {
        elems.push(match f {
            HdrField::I64 => i64_type(c),
            HdrField::Ptr => ptr_type(c),
        });
    }
    LLVMStructTypeInContext(c, elems.as_mut_ptr(), elems.len() as u32, 0)
}

// {i64, i64} ARC header struct (single-sourced via `header_fields`).
#[inline]
pub(crate) unsafe fn arc_header_type(c: LLVMContextRef) -> LLVMTypeRef {
    build_header_struct(c, HeaderKind::Arc)
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

// NotAtomic ordering uses a plain load (data-layout default alignment) to match the
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
pub(crate) const ANY_TAG_INT: i64 = 0;
pub(crate) const ANY_TAG_FLOAT: i64 = 1;

// Emit the inline runtime-error sequence: fprintf(stderr, msg) + fflush +
// _Exit(1) + unreachable, which terminates the block. The caller must pre-split
// into err / ok BBs first (the emitRuntimeError-terminates rule).
pub(crate) unsafe fn emit_inline_runtime_error(
    c: &mut EmitCtx,
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

// libc call helpers. Each builds the `(declare|reuse) + call` boilerplate for a
// single C runtime function. malloc takes a result `name` (call sites vary:
// app_new_data / ins_new_data / sl_data / arc_box / cow_*); memcpy / memmove /
// free are unnamed at every site, so the empty name is fixed here.
pub(crate) unsafe fn emit_malloc(
    b: LLVMBuilderRef,
    module: LLVMModuleRef,
    i64_ty: LLVMTypeRef,
    ptr_ty: LLVMTypeRef,
    size: LLVMValueRef,
    name: *const c_char,
) -> LLVMValueRef {
    let mut p = [i64_ty];
    let ty = LLVMFunctionType(ptr_ty, p.as_mut_ptr(), 1, 0);
    let f = get_or_insert_function(module, c"malloc".as_ptr(), ty);
    let mut a = [size];
    LLVMBuildCall2(b, ty, f, a.as_mut_ptr(), 1, name)
}

pub(crate) unsafe fn emit_memcpy(
    b: LLVMBuilderRef,
    module: LLVMModuleRef,
    ptr_ty: LLVMTypeRef,
    i64_ty: LLVMTypeRef,
    dst: LLVMValueRef,
    src: LLVMValueRef,
    n: LLVMValueRef,
) {
    let mut p = [ptr_ty, ptr_ty, i64_ty];
    let ty = LLVMFunctionType(ptr_ty, p.as_mut_ptr(), 3, 0);
    let f = get_or_insert_function(module, c"memcpy".as_ptr(), ty);
    let mut a = [dst, src, n];
    LLVMBuildCall2(b, ty, f, a.as_mut_ptr(), 3, c"".as_ptr());
}

pub(crate) unsafe fn emit_memmove(
    b: LLVMBuilderRef,
    module: LLVMModuleRef,
    ptr_ty: LLVMTypeRef,
    i64_ty: LLVMTypeRef,
    dst: LLVMValueRef,
    src: LLVMValueRef,
    n: LLVMValueRef,
) {
    let mut p = [ptr_ty, ptr_ty, i64_ty];
    let ty = LLVMFunctionType(ptr_ty, p.as_mut_ptr(), 3, 0);
    let f = get_or_insert_function(module, c"memmove".as_ptr(), ty);
    let mut a = [dst, src, n];
    LLVMBuildCall2(b, ty, f, a.as_mut_ptr(), 3, c"".as_ptr());
}

pub(crate) unsafe fn emit_free(
    b: LLVMBuilderRef,
    module: LLVMModuleRef,
    ptr_ty: LLVMTypeRef,
    void_ty: LLVMTypeRef,
    ptr: LLVMValueRef,
) {
    let mut p = [ptr_ty];
    let ty = LLVMFunctionType(void_ty, p.as_mut_ptr(), 1, 0);
    let f = get_or_insert_function(module, c"free".as_ptr(), ty);
    let mut a = [ptr];
    LLVMBuildCall2(b, ty, f, a.as_mut_ptr(), 1, c"".as_ptr());
}

// malloc ARC_HEADER_SIZE + boxDataSize, bump the live-count (+1), init
// strong=1/weak=0, return the headerPtr (caller GEPs +ARC_HEADER_SIZE for the
// data pointer).
pub(crate) unsafe fn emit_inline_arc_alloc(
    c: &mut EmitCtx,
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
    let header_ptr = emit_malloc(b, module, i64_ty, ptr_ty, total_size, c"arc_box".as_ptr());
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

// Emit the list capacity-grow block: new_cap = max(cap*2, 4), malloc the new
// buffer, memcpy the old elements, free the old buffer, store new data/cap, then
// branch to next_bb. The builder is positioned at grow_bb here. SSA names are
// `prefix`-derived (app_* / ins_*), except the cap>4 compare whose name is
// passed explicitly (append uses `cap_gt4`, insert leaves it unnamed) to keep
// the emitted IR byte-identical to the pre-extraction inline blocks.
#[allow(clippy::too_many_arguments)]
pub(crate) unsafe fn emit_list_grow(
    b: LLVMBuilderRef,
    module: LLVMModuleRef,
    h: &ListHeaderLoad,
    elem_size: u64,
    i64_ty: LLVMTypeRef,
    ptr_ty: LLVMTypeRef,
    void_ty: LLVMTypeRef,
    prefix: &[u8],
    gt4_name: *const c_char,
    grow_bb: LLVMBasicBlockRef,
    next_bb: LLVMBasicBlockRef,
) {
    LLVMPositionBuilderAtEnd(b, grow_bb);
    let four = LLVMConstInt(i64_ty, 4, 0);
    let doubled = LLVMBuildMul(
        b,
        h.cap,
        LLVMConstInt(i64_ty, 2, 0),
        cname_pfx(prefix, b"_doubled").as_ptr(),
    );
    let gt4 = LLVMBuildICmp(b, LLVMIntPredicate::LLVMIntSGT, doubled, four, gt4_name);
    let new_cap = LLVMBuildSelect(
        b,
        gt4,
        doubled,
        four,
        cname_pfx(prefix, b"_new_cap").as_ptr(),
    );
    let new_size = LLVMBuildMul(
        b,
        new_cap,
        LLVMConstInt(i64_ty, elem_size, 0),
        cname_pfx(prefix, b"_new_size").as_ptr(),
    );
    let new_data = emit_malloc(
        b,
        module,
        i64_ty,
        ptr_ty,
        new_size,
        cname_pfx(prefix, b"_new_data").as_ptr(),
    );
    let old_size = LLVMBuildMul(
        b,
        h.len,
        LLVMConstInt(i64_ty, elem_size, 0),
        cname_pfx(prefix, b"_old_size").as_ptr(),
    );
    emit_memcpy(b, module, ptr_ty, i64_ty, new_data, h.data, old_size);
    emit_free(b, module, ptr_ty, void_ty, h.data);
    LLVMBuildStore(b, new_data, h.data_ptr);
    LLVMBuildStore(b, new_cap, h.cap_ptr);
    LLVMBuildBr(b, next_bb);
}
