//! context — the layer-neutral state, handles, enum selectors, and pure-data
//! header layout table shared by `abi`, `primitive`, and `composite`. Owns the
//! concrete `EmitCtx` (LLVM emission state + intern table + msg dedup caches),
//! the Rust-native `ValueRef` / `BasicBlockRef` / `FunctionRef` / `TypeRef` /
//! `FuncTypeRef` handle wrappers, the boundary-mirrored enum selectors
//! (`Atomicity`, `BoundsKind`, `CowKind`, `ListCopyKind`, `IcmpPred`,
//! `AnyWrap/Unwrap/TryUnwrapKind`), the layout constants (`ARC_HEADER_SIZE`,
//! `STRING_HEADER_SIZE`, `ANY_TAG_*`, `LIST_FIELD_*`, `MAP_FIELD_*`), and the
//! pure-data `HdrField` / `HeaderKind` / `header_fields` table that drives both
//! `composite::header::build_header_struct` and the cross-language parity guard
//! (`abi::test_introspect::ry_emit_test_header_layout`).
//!
//! Must NOT reference `crate::abi`, `crate::primitive`, or `crate::composite` —
//! `context` sits below every other layer and only depends on `llvm_sys::prelude`
//! for the raw handle types (no `llvm_sys::core` IR-gen here, so the
//! `check-emit-abi-no-ir.sh` carve-out for `llvm_sys::prelude` applies to
//! `context` too). The `core⇏abi` invariant (#2057) extends to
//! `context⇏{abi,primitive,composite}`; the import-direction gate
//! (`scripts/check-emit-composite-no-primitive.sh`) enforces this.

use std::collections::HashMap;

use llvm_sys::prelude::*;

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
    // Dedup cache for `cachedGlobalString`-shaped (StringHeader-prefixed)
    // fmt-string globals used by `emitRuntimeError`-style exits (#2097: the
    // checked FP→int sequence). Keyed by message bytes, mirroring the C++
    // `global_string_cache_`. The cached value is the ConstantExpr GEP into
    // the data payload (a `ptr` to the first byte of the `[N+1 x i8]` slot),
    // ready to be passed straight to a fprintf call.
    pub(crate) arc_msg_cache: HashMap<Vec<u8>, LLVMValueRef>,
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
            arc_msg_cache: HashMap::new(),
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

/// Call-site selector for `list_copy_full` (the single-source full-buffer copy
/// shared by `keys` / `values` / `take`, #2093). The three call sites emit the
/// identical `mul + malloc + memcpy` shape and diverge ONLY in their SSA names
/// (`keys_ds`/`keys_nd`, `vals_ds`/`vals_nd`, `tk_dsize`/`tk_data`); this kind
/// picks the name pair so the migrated IR stays byte-identical per call site
/// (mirrors the boundary `RY_LISTCOPY_KEYS` / `_VALUES` / `_TAKE` constants).
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum ListCopyKind {
    Keys,
    Values,
    Take,
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

// Layout constants (mirror include/ry/ry_layout.hpp).
pub(crate) const ARC_HEADER_SIZE: u64 = 16;
pub(crate) const ARC_IMMORTAL: i64 = i64::MAX;
// String header size (mirror include/ry/ry_layout.hpp STRING_HEADER_SIZE).
pub(crate) const STRING_HEADER_SIZE: u64 = 24;
// RyAnyTag discriminants (mirror include/ry/ry_layout.hpp RyAnyTag).
pub(crate) const ANY_TAG_INT: i64 = 0;
pub(crate) const ANY_TAG_FLOAT: i64 = 1;

// === Internal collection / ARC header layout — single source of truth ===
//
// The list/map/set/arc header structs are built from one field-kind table so
// that BOTH the emitted LLVM struct (`composite::header::build_header_struct`,
// driven by `composite::cow::cow_ensure_unique` and `composite::arc`) and the
// cross-language layout guard (the `ry_emit_test_header_layout` test-extern in
// `abi/test_introspect.rs`, asserted against C++'s canonical
// `listHeaderTy_`/`mapHeaderTy_`/`setHeaderTy_`/`arcHeaderTy_` in
// `tests/test_header_layout.cpp`) are driven by the *same* definition. A drift
// vs. the C++ named structs was previously guarded only by a "Field order MUST
// stay in sync" comment; it is now caught mechanically by that parity test plus
// the same-type-swap behavioral coverage in `tests/spec/cow.test.ry` (#2071).

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

// Named GEP-field index constants — the single source of truth for List/Map
// header layouts mirrored from the C++ canonical structs. Migration sites use
// these (LIST_FIELD_DATA, MAP_FIELD_KEYS, …) instead of bare numeric indices so
// a future layout change is a single-point edit. The `header_fields` slice
// below stores the field *type* at each named position; both are pinned against
// C++'s canonical struct by `tests/test_header_layout.cpp` (#2071 extended in
// #2095).
pub(crate) const LIST_FIELD_LEN: u32 = 0;
pub(crate) const LIST_FIELD_CAP: u32 = 1;
pub(crate) const LIST_FIELD_DATA: u32 = 2;

pub(crate) const MAP_FIELD_LEN: u32 = 0;
pub(crate) const MAP_FIELD_CAP: u32 = 1;
pub(crate) const MAP_FIELD_KEYS: u32 = 2;
pub(crate) const MAP_FIELD_VALS: u32 = 3;
pub(crate) const MAP_FIELD_BUCKET_COUNT: u32 = 4;
pub(crate) const MAP_FIELD_BUCKETS: u32 = 5;

// The field kinds of each header, mirroring the C++ canonical struct layouts.
//   List {len:i64, cap:i64, data:ptr}                           (indices 0..3)
//   Map  {len:i64, cap:i64, keys:ptr, vals:ptr, bcnt:i64, bks:ptr}  (0..6)
//   Set  {len:i64, cap:i64, elems:ptr, bucket_count:i64, buckets:ptr}
//   Arc  {strong:i64, weak:i64}
// The slice positions agree with the LIST_FIELD_* / MAP_FIELD_* constants above
// — drift is caught by `tests/test_header_layout.cpp` (per-index type + ABI
// size + named-index field-type assertions).
pub(crate) fn header_fields(kind: HeaderKind) -> &'static [HdrField] {
    use HdrField::{Ptr, I64};
    match kind {
        HeaderKind::List => &[I64, I64, Ptr],
        HeaderKind::Map => &[I64, I64, Ptr, Ptr, I64, Ptr],
        HeaderKind::Set => &[I64, I64, Ptr, I64, Ptr],
        HeaderKind::Arc => &[I64, I64],
    }
}
