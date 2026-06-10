//! abi — the C/C++ emission boundary (the actual foreign-function interface;
//! note the `ffi` module name is the Rust-native surface and is provisional,
//! pending #2022). Owns the locked C type contract — opaque handle types,
//! scalar/enum typedefs, descriptor structs, the compile-time layout assertions
//! (#1995) — plus the boundary plumbing that exists *because C++ calls in*: the
//! confined `cx` reify (opaque handle → `EmitCtx`, walled into the `ctx_access`
//! submodule and reached only via `with_ctx` / `checked_cx`, #2081), `intern` /
//! `resolve` (the u32-id ↔ value bridge), the opaque-handle casts, and
//! `cstr_bytes`. Every C item mirrors
//! `include/ry/llvm_emit/api.h` byte-for-byte — the C boundary is locked
//! (#1949); do NOT alter without updating api.h and cast_helpers.hpp. May depend
//! on `core` (abi → core); the reverse (`core → abi`) is forbidden (#2057).

use std::ffi::{c_char, c_int, c_void};

use llvm_sys::prelude::*;

use crate::core::{EmitCtx, ValueRef};

// Per-op C boundary entry points live in child modules (one per migrated op),
// each calling the matching `core` engine method on `EmitCtx`. The cdylib
// exports the `#[no_mangle]` symbols regardless of module path. No `pub use` is
// needed: since #2062 migrated `cow` to call `arc_retain` / `arc_release` as
// engine methods directly, no in-crate module calls another op's externs by
// name. The last such re-export — `pub use arc::*` kept solely for `cow` — was
// dropped here, the same cleanup #2061 did for `pub use bounds::*`.
mod any;
mod arc;
mod bounds;
mod collection;
mod control_flow;
mod cow;
mod lifecycle;
mod option;
mod result;
mod runtime_call;
// Test-only: exposes core::header_fields for the cross-language header-layout
// parity test (#2071). IR-free (reads pure data only) — see the module doc.
mod test_introspect;

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
// and layout MUST match the C declarations). The per-field docs below
// summarize each field for the Rust reader and call out the handle kind
// (`RyValueId` u32 intern id the shell `resolve`s, vs. opaque
// `RyValueRef` / `RyTypeRef` pointers passed straight through) and the
// core vocabulary the abi shell maps it into. api.h holds the canonical
// long-form (kind tables, BB-level emission sequences) — keep these
// short and defer there rather than restating it.
// =============================================================

#[repr(C)]
pub struct RyCowEnsureUniqueDesc {
    /// Current ARC-backed dataPtr (`RyValueId`). The op derives headerPtr as
    /// `data_ptr - ARC_HEADER_SIZE`.
    pub data_ptr_id: RyValueId,
    /// Slot (alloca or GEP) that receives the new dataPtr on the copy path;
    /// left untouched on the unique / immortal path (`RyValueId`).
    pub slot_ptr_id: RyValueId,
    /// RyCowKind selector, mapped to `CowKind`: 0 = List, 1 = Map, 2 = Set.
    pub kind: c_int,
    /// RyArcAtomic, mapped to `Atomicity`; gates the strong_count load
    /// ordering (Acquire vs NotAtomic) and the internal arc_release mode.
    pub atomic: c_int,
    /// List / Set: element bytes. Map: unused (use `val_size`).
    pub elem_size: u64,
    /// Map: key bytes. 0 for List / Set.
    pub key_size: u64,
    /// Map: value bytes. 0 for List / Set.
    pub val_size: u64,
    /// 1 = retain each cloned element after the memcpy (always NON-atomic);
    /// mapped to `bool`.
    pub do_elem_retain: c_int,
    /// Element-retain header offset: 1 = StringHeader (-24), 0 = ArcHeader
    /// (-16). Consulted only when `do_elem_retain`.
    pub elem_is_str: c_int,
    /// Map only: 1 = retain each cloned Map key (always NON-atomic); mapped to
    /// `bool`. Independent of `do_elem_retain`.
    pub do_key_retain: c_int,
    /// Key-retain header offset: 1 = StringHeader (-24), 0 = ArcHeader (-16).
    /// Consulted only when `do_key_retain`.
    pub key_is_str: c_int,
    /// Per-kind collection destructor — opaque `RyValueRef` passed straight to
    /// the internal arc_release (mapped to `Option<ValueRef>`). NULL when the
    /// kind has no element-side cleanup.
    pub destructor_callee: RyValueRef,
}

#[repr(C)]
pub struct RyAnyWrapDesc {
    /// `kind` selector, mapped to `AnyWrapKind`: 0 = NonBox, 1 = RecordBox,
    /// 2 = EnumBox.
    pub kind: c_int,
    /// RyAnyTag to store in any.tag, passed as a raw i64 (matches the C++
    /// `ry::RyAnyTag` enum numerically, Int=0..Enum=9) so the boundary need
    /// not share the enum identifier.
    pub target_tag: i64,
    /// Value to wrap (`RyValueId`). NonBox: primitive / bool / str /
    /// collection pointer. Box arms: the record/enum payload.
    pub val_id: RyValueId,
    /// NonBox only: 1 = retain the collection element header (-16) before the
    /// alloca/store; mapped to `bool`.
    pub do_collection_retain: c_int,
    /// NonBox only: 1 = retain the StringHeader (-24) before the alloca/store.
    /// Mutually exclusive with `do_collection_retain`.
    pub do_str_retain: c_int,
    /// Box arms only: per-type descriptor LLVM Constant (`RyValueId`; the
    /// nullable resolve maps it to `Option<ValueRef>`).
    pub descriptor_id: RyValueId,
    /// Box arms only: `{ ptr desc, payload }` StructType — opaque `RyTypeRef`
    /// passed straight through (mapped to `Option<TypeRef>`).
    pub box_layout_ty: RyTypeRef,
    /// Box arms only: DataLayout-derived allocation size of `box_layout_ty` in
    /// bytes, pre-computed so the boundary need not consult DataLayout.
    pub box_data_size: u64,
    /// Common: the `any` StructType handle (anyTy_) — opaque `RyTypeRef`.
    pub any_ty: RyTypeRef,
}

#[repr(C)]
pub struct RyAnyUnwrapDesc {
    /// `kind` selector: 0 = Standard (2-way tag check), 1 = F64Promote
    /// (int→float auto-promote, 5 BBs), 2 = Record (descriptor-chain walk).
    pub kind: c_int,
    /// Source any value (`RyValueId`).
    pub any_val_id: RyValueId,
    /// `any` StructType handle (anyTy_) — opaque `RyTypeRef`.
    pub any_ty: RyTypeRef,
    /// Standard: target LLVM type (i64 / i1 / ptr / ...). Record: ignored
    /// (`record_struct_ty` is used). F64Promote: ignored. Opaque `RyTypeRef`.
    pub target_ty: RyTypeRef,
    /// Standard / Record: expected RyAnyTag value (raw i64).
    pub expected_tag: i64,
    /// Standard only: 1 = retain the collection element header (-16) after the
    /// load; mapped to `bool`.
    pub do_collection_retain: c_int,
    /// Standard only: 1 = retain the StringHeader (-24) after the load.
    /// Mutually exclusive with `do_collection_retain`.
    pub do_str_retain: c_int,
    /// Tag-mismatch error message — borrowed `*const c_char` (valid only for
    /// the call) for the cachedGlobalString'd format string.
    pub mismatch_msg: *const c_char,
    /// Global-name hint for `mismatch_msg`'s interned string (borrowed
    /// `*const c_char`).
    pub mismatch_global_name: *const c_char,
    /// Record only: expected per-type descriptor LLVM Constant (`RyValueId`).
    pub expected_desc_id: RyValueId,
    /// Record only: `{ ptr desc, record struct }` StructType — opaque
    /// `RyTypeRef`.
    pub box_layout_ty: RyTypeRef,
    /// Record only: the record's struct type for the payload GEP/load — opaque
    /// `RyTypeRef`.
    pub record_struct_ty: RyTypeRef,
    /// Record only: descriptor-mismatch error message (borrowed `*const
    /// c_char`).
    pub desc_mismatch_msg: *const c_char,
    /// Record only: global-name hint for `desc_mismatch_msg` (borrowed
    /// `*const c_char`).
    pub desc_mismatch_global_name: *const c_char,
}

#[repr(C)]
pub struct RyAnyTryUnwrapDesc {
    /// `kind` selector: 0 = Standard (tag-check primitive arm via
    /// result_branch), 1 = F64Promote (f64 target with int auto-promote).
    pub kind: c_int,
    /// Source any value (`RyValueId`).
    pub any_val_id: RyValueId,
    /// `any` StructType handle (anyTy_) — opaque `RyTypeRef`.
    pub any_ty: RyTypeRef,
    /// `Result<targetTy, Error>` StructType handle — opaque `RyTypeRef`.
    pub res_ty: RyTypeRef,
    /// `Error` StructType handle (errorTy_) — opaque `RyTypeRef`.
    pub error_ty: RyTypeRef,
    /// Standard: target LLVM type (i64 / i1 / ptr / ...). F64Promote: ignored
    /// (f64 hard-coded). Opaque `RyTypeRef`.
    pub target_ty: RyTypeRef,
    /// Standard only: expected RyAnyTag value (raw i64).
    pub expected_tag: i64,
    /// Standard only: 1 = retain the collection element header (-16); same
    /// semantics as `RyAnyUnwrapDesc`. Mapped to `bool`.
    pub do_collection_retain: c_int,
    /// Standard only: 1 = retain the StringHeader (-24). Mutually exclusive
    /// with `do_collection_retain`.
    pub do_str_retain: c_int,
    /// Both kinds: error message string (`RyValueId`) stored into Error slot 0.
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
// cdylib build. `std::mem::offset_of!` is const-evaluable since Rust
// 1.77. See docs/architecture/llvm-ir-emission-boundary.md for the
// verification model and the canonical value table.
// =============================================================

// --- Descriptor structs: sizeof + alignof + every field offset ---

// RyCowEnsureUniqueDesc (12 fields).
const _: () = assert!(std::mem::size_of::<RyCowEnsureUniqueDesc>() == 64);
const _: () = assert!(std::mem::align_of::<RyCowEnsureUniqueDesc>() == 8);
const _: () = assert!(std::mem::offset_of!(RyCowEnsureUniqueDesc, data_ptr_id) == 0);
const _: () = assert!(std::mem::offset_of!(RyCowEnsureUniqueDesc, slot_ptr_id) == 4);
const _: () = assert!(std::mem::offset_of!(RyCowEnsureUniqueDesc, kind) == 8);
const _: () = assert!(std::mem::offset_of!(RyCowEnsureUniqueDesc, atomic) == 12);
const _: () = assert!(std::mem::offset_of!(RyCowEnsureUniqueDesc, elem_size) == 16);
const _: () = assert!(std::mem::offset_of!(RyCowEnsureUniqueDesc, key_size) == 24);
const _: () = assert!(std::mem::offset_of!(RyCowEnsureUniqueDesc, val_size) == 32);
const _: () = assert!(std::mem::offset_of!(RyCowEnsureUniqueDesc, do_elem_retain) == 40);
const _: () = assert!(std::mem::offset_of!(RyCowEnsureUniqueDesc, elem_is_str) == 44);
const _: () = assert!(std::mem::offset_of!(RyCowEnsureUniqueDesc, do_key_retain) == 48);
const _: () = assert!(std::mem::offset_of!(RyCowEnsureUniqueDesc, key_is_str) == 52);
const _: () = assert!(std::mem::offset_of!(RyCowEnsureUniqueDesc, destructor_callee) == 56);

// RyAnyWrapDesc (9 fields).
const _: () = assert!(std::mem::size_of::<RyAnyWrapDesc>() == 56);
const _: () = assert!(std::mem::align_of::<RyAnyWrapDesc>() == 8);
const _: () = assert!(std::mem::offset_of!(RyAnyWrapDesc, kind) == 0);
const _: () = assert!(std::mem::offset_of!(RyAnyWrapDesc, target_tag) == 8);
const _: () = assert!(std::mem::offset_of!(RyAnyWrapDesc, val_id) == 16);
const _: () = assert!(std::mem::offset_of!(RyAnyWrapDesc, do_collection_retain) == 20);
const _: () = assert!(std::mem::offset_of!(RyAnyWrapDesc, do_str_retain) == 24);
const _: () = assert!(std::mem::offset_of!(RyAnyWrapDesc, descriptor_id) == 28);
const _: () = assert!(std::mem::offset_of!(RyAnyWrapDesc, box_layout_ty) == 32);
const _: () = assert!(std::mem::offset_of!(RyAnyWrapDesc, box_data_size) == 40);
const _: () = assert!(std::mem::offset_of!(RyAnyWrapDesc, any_ty) == 48);

// RyAnyUnwrapDesc (14 fields).
const _: () = assert!(std::mem::size_of::<RyAnyUnwrapDesc>() == 96);
const _: () = assert!(std::mem::align_of::<RyAnyUnwrapDesc>() == 8);
const _: () = assert!(std::mem::offset_of!(RyAnyUnwrapDesc, kind) == 0);
const _: () = assert!(std::mem::offset_of!(RyAnyUnwrapDesc, any_val_id) == 4);
const _: () = assert!(std::mem::offset_of!(RyAnyUnwrapDesc, any_ty) == 8);
const _: () = assert!(std::mem::offset_of!(RyAnyUnwrapDesc, target_ty) == 16);
const _: () = assert!(std::mem::offset_of!(RyAnyUnwrapDesc, expected_tag) == 24);
const _: () = assert!(std::mem::offset_of!(RyAnyUnwrapDesc, do_collection_retain) == 32);
const _: () = assert!(std::mem::offset_of!(RyAnyUnwrapDesc, do_str_retain) == 36);
const _: () = assert!(std::mem::offset_of!(RyAnyUnwrapDesc, mismatch_msg) == 40);
const _: () = assert!(std::mem::offset_of!(RyAnyUnwrapDesc, mismatch_global_name) == 48);
const _: () = assert!(std::mem::offset_of!(RyAnyUnwrapDesc, expected_desc_id) == 56);
const _: () = assert!(std::mem::offset_of!(RyAnyUnwrapDesc, box_layout_ty) == 64);
const _: () = assert!(std::mem::offset_of!(RyAnyUnwrapDesc, record_struct_ty) == 72);
const _: () = assert!(std::mem::offset_of!(RyAnyUnwrapDesc, desc_mismatch_msg) == 80);
const _: () = assert!(std::mem::offset_of!(RyAnyUnwrapDesc, desc_mismatch_global_name) == 88);

// RyAnyTryUnwrapDesc (10 fields).
const _: () = assert!(std::mem::size_of::<RyAnyTryUnwrapDesc>() == 64);
const _: () = assert!(std::mem::align_of::<RyAnyTryUnwrapDesc>() == 8);
const _: () = assert!(std::mem::offset_of!(RyAnyTryUnwrapDesc, kind) == 0);
const _: () = assert!(std::mem::offset_of!(RyAnyTryUnwrapDesc, any_val_id) == 4);
const _: () = assert!(std::mem::offset_of!(RyAnyTryUnwrapDesc, any_ty) == 8);
const _: () = assert!(std::mem::offset_of!(RyAnyTryUnwrapDesc, res_ty) == 16);
const _: () = assert!(std::mem::offset_of!(RyAnyTryUnwrapDesc, error_ty) == 24);
const _: () = assert!(std::mem::offset_of!(RyAnyTryUnwrapDesc, target_ty) == 32);
const _: () = assert!(std::mem::offset_of!(RyAnyTryUnwrapDesc, expected_tag) == 40);
const _: () = assert!(std::mem::offset_of!(RyAnyTryUnwrapDesc, do_collection_retain) == 48);
const _: () = assert!(std::mem::offset_of!(RyAnyTryUnwrapDesc, do_str_retain) == 52);
const _: () = assert!(std::mem::offset_of!(RyAnyTryUnwrapDesc, err_msg_str_id) == 56);

// --- Opaque handle typedefs: sizeof + alignof only (no fields) ---
// All are `*mut <Opaque>` -> 8 bytes / 8-byte aligned on 64-bit platforms.
const _: () =
    assert!(std::mem::size_of::<RyModuleRef>() == 8 && std::mem::align_of::<RyModuleRef>() == 8);
const _: () =
    assert!(std::mem::size_of::<RyBuilderRef>() == 8 && std::mem::align_of::<RyBuilderRef>() == 8);
const _: () =
    assert!(std::mem::size_of::<RyContextRef>() == 8 && std::mem::align_of::<RyContextRef>() == 8);
const _: () = assert!(
    std::mem::size_of::<RyFunctionRef>() == 8 && std::mem::align_of::<RyFunctionRef>() == 8
);
const _: () =
    assert!(std::mem::size_of::<RyTypeRef>() == 8 && std::mem::align_of::<RyTypeRef>() == 8);
const _: () = assert!(
    std::mem::size_of::<RyFuncTypeRef>() == 8 && std::mem::align_of::<RyFuncTypeRef>() == 8
);
const _: () =
    assert!(std::mem::size_of::<RyValueRef>() == 8 && std::mem::align_of::<RyValueRef>() == 8);
const _: () = assert!(
    std::mem::size_of::<RyBasicBlockRef>() == 8 && std::mem::align_of::<RyBasicBlockRef>() == 8
);

// --- Scalar intern-handle typedefs (u32) ---
const _: () =
    assert!(std::mem::size_of::<RyValueId>() == 4 && std::mem::align_of::<RyValueId>() == 4);

// =============================================================
// Boundary plumbing — exists because C++ calls in. The opaque `*mut RyEmitCtx`
// is reified into the concrete `EmitCtx` (owned by `core`) ONLY inside the
// `ctx_access` submodule below, which confines the single arbitrary-lifetime
// cast to one audited site and exports just the two scoped / validated
// accessors (#2081). `intern` / `resolve` bridge the u32-id handle space the C
// ABI traffics in to/from raw values; the `as_*` / `to_ry_*` casts reinterpret
// opaque boundary handles as llvm-sys refs. All of this is abi → core (the
// reverse is forbidden, #2057).
// =============================================================

// EmitCtx reification, confined (#2081). The arbitrary-lifetime `*mut RyEmitCtx`
// → `&mut EmitCtx` cast (`cx`) is fully private to this module; `abi::result` /
// `abi::lifecycle` / each `abi::<op>` are SIBLINGS of `ctx_access`, not
// descendants, so they cannot name `cx` — the only way to touch an `EmitCtx` is
// through `with_ctx` (scoped) or `checked_cx` (validated), both exported below.
// This shrinks the alias-audit surface to a single reify site. It does NOT make
// aliasing impossible: `checked_cx` still returns an escaping `&mut`, and either
// accessor can be nested on the same handle — the layer is "confined and locally
// auditable", not "alias-proof". New `abi/<op>.rs` files MUST reach the context
// through these accessors, never a raw reify.
mod ctx_access {
    use super::RyEmitCtx;
    use crate::core::EmitCtx;

    // The sole arbitrary-lifetime reifier. Fully private — NO `pub`, not even
    // `pub(super)` (which would re-expose it to the whole `abi` subtree, letting
    // the sibling op modules name it again and defeating the confinement). The
    // unbound `'a` is unconnected to the source pointer, so two `cx(p)` calls
    // would mint aliasing `&mut`; keeping it private forces every access through
    // the two accessors below, each of which documents how it bounds (with_ctx)
    // or consciously does not (checked_cx) that risk.
    #[inline]
    unsafe fn cx<'a>(p: *mut RyEmitCtx) -> &'a mut EmitCtx {
        &mut *(p as *mut EmitCtx)
    }

    // Scoped accessor: binds the `&mut EmitCtx` to `f`'s invocation so the borrow
    // cannot escape — each access is a lexically-bounded transient. The
    // re-entrant `result_branch` path uses this to keep every borrow OUTSIDE the
    // C builder callbacks (it resolves each arm's id in a fresh `with_ctx` AFTER
    // the callback returns), replacing the former reliance on left-to-right
    // argument-evaluation order with a structural borrow-scope guarantee
    // (#2081 / #2069); `lifecycle` uses it for its NULL-guarded plumbing.
    //
    // Precondition: `p` MUST be non-NULL — `with_ctx` does NOT NULL-check (it has
    // no sentinel to return), so callers guard first (`if p.is_null() { return
    // <sentinel> }`). It does NOT make aliasing impossible either: nesting two
    // `with_ctx` on the same handle, or raw-casting the `&mut` out of `f`, still
    // aliases. The guarantee is "the borrow scope is visible and bounded", not
    // "the borrow is unique".
    #[inline]
    pub(crate) unsafe fn with_ctx<R>(p: *mut RyEmitCtx, f: impl FnOnce(&mut EmitCtx) -> R) -> R {
        f(cx(p))
    }

    // Validated whole-body accessor: NULL ctx / context / module / builder →
    // `None` (→ the caller returns its sentinel). Checking all three fields is a
    // safe superset for ops that touch only a subset (`create_basic_block` uses
    // only `context`, `get_runtime_fn` only `module`): `ry_emit_ctx_create`
    // always stores the caller's live module/builder/context together, so a
    // partially-NULL ctx never arises from a supported call. The pure-plumbing
    // entries that emit no IR (`intern` / `resolve` / `set_function`) guard
    // `ctx.is_null()` directly and reify via `with_ctx` — they touch none of the
    // three fields.
    //
    // Returns an escaping arbitrary-lifetime `&mut` — safe for the non-re-entrant
    // op modules that hold it for one entry-point body and never re-enter. A
    // double `checked_cx(ctx)` on the same handle WOULD alias and the borrow
    // checker cannot see it: a conscious residual of the "confined, not
    // alias-proof" stance (#2081).
    #[inline]
    pub(crate) unsafe fn checked_cx<'a>(p: *mut RyEmitCtx) -> Option<&'a mut EmitCtx> {
        if p.is_null() {
            return None;
        }
        let c = cx(p);
        if c.context.is_null() || c.module.is_null() || c.builder.is_null() {
            return None;
        }
        Some(c)
    }
}
pub(crate) use ctx_access::{checked_cx, with_ctx};

// Opaque boundary handle → llvm-sys C API ref (pointer cast, this crate only).
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
pub(crate) fn as_function(p: RyFunctionRef) -> LLVMValueRef {
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

// Bridge llvm::Value* ↔ RyValueId handle space. The internal forms take
// &mut/&EmitCtx so callers already holding the borrow do not re-alias
// through the public boundary entry points.
#[inline]
pub(crate) unsafe fn intern(c: &mut EmitCtx, value: RyValueRef) -> RyValueId {
    if value.is_null() {
        return 0;
    }
    let id = c.values.len() as RyValueId;
    c.values.push(as_value(value));
    id
}

#[inline]
pub(crate) unsafe fn resolve(c: &EmitCtx, id: RyValueId) -> RyValueRef {
    if id == 0 || id as usize >= c.values.len() {
        return std::ptr::null_mut();
    }
    to_ry_value(c.values[id as usize])
}

// =============================================================
// Shared boundary input-validation helpers (#2080). Every IR-emitting entry
// point converts malformed input to its sentinel rather than passing an unvetted
// handle to the `core` engine (which forwards it straight to the LLVM C API). The
// ctx + handle validation lives in `ctx_access::checked_cx` above (co-located
// with the confined reify it builds on, #2081); the two helpers below fold the
// remaining per-op inline guard sequences that runtime_call / result / any / cow
// each spelled out — required-id resolution and FFI-array borrow — into one
// shared contract so the guard policy is uniform across the boundary.
// =============================================================

// Resolve a value id to `Some(ValueRef)`, or `None` when it resolves to NULL
// (sentinel id 0 / out-of-range / an interned NULL). The non-NULL intent lives at
// the call site: a *required* operand maps `None` to the entry point's sentinel
// (`let Some(v) = resolve_value(..) else { return 0 }`), while an *optional* one
// (e.g. an absent `any` descriptor) stores the `Option` directly. Subsumes the
// former `any.rs::opt_value_id`.
#[inline]
pub(crate) unsafe fn resolve_value(c: &EmitCtx, id: RyValueId) -> Option<ValueRef> {
    let v = as_value(resolve(c, id));
    if v.is_null() {
        None
    } else {
        Some(ValueRef(v))
    }
}

// Borrow a parallel FFI array as a slice under the C `(ptr, count)` contract: an
// empty count yields an empty slice WITHOUT touching the pointer
// (`slice::from_raw_parts` requires a non-NULL base even for length 0), a
// positive count with a NULL pointer yields `None` (→ caller's sentinel), and
// otherwise the borrowed slice. Generalizes the inline guard runtime_call spelled
// out for its `arg_tys` / `arg_ids` buffers so create_phi's two incoming arrays
// reuse it.
#[inline]
pub(crate) unsafe fn ffi_slice<'a, T>(ptr: *const T, count: u32) -> Option<&'a [T]> {
    if count == 0 {
        Some(&[])
    } else if ptr.is_null() {
        None
    } else {
        Some(std::slice::from_raw_parts(ptr, count as usize))
    }
}

// `cstr_bytes` lives in `core` (#2060) — a pure `CStr` primitive used by the
// bounds and any engines, both of which are now core-role modules reaching it
// via `crate::core` directly. The transitional `pub(crate) use crate::core::
// cstr_bytes` re-export here (kept for the legacy `any.rs` while it still did
// `use crate::abi::*`) was dropped once #2063 migrated `any` — its last consumer
// — mirroring #2062 dropping `pub use arc::*`.
