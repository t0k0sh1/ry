// Rust implementation of the LLVM IR emission boundary defined in
// `include/ry/llvm_emit/api.h`.
//
// Every type and function exposed here mirrors a declaration in api.h
// byte-for-byte. The C boundary is locked by #1949 — do NOT widen or alter
// the public signatures without updating api.h and the reinterpret_cast
// wrappers in include/ry/llvm_emit/cast_helpers.hpp.
//
// Build-time invariants (verified whenever the cdylib compiles):
//   - every api.h boundary symbol is exported, plus the test-only
//     `ry_emit_test_header_layout` introspection symbol (#2071; #2072 added the
//     11 scalar/memory primitives — alloca/load/store/gep/icmp/and/or/add/sub/
//     select/const_int — and #2097 added `checked_fp_to_int`, so 40 + 1 today),
//   - the FFI types compile against api.h (sizeof / repr alignment),
//   - corrosion-rs is wired correctly into the CMake build,
//   - `-undefined dynamic_lookup` link arg flows through on macOS,
//   - llvm-sys = "211" finds LLVM 21 via LLVM_SYS_211_PREFIX.
//
// Module layout (#2057 — `abi → core` layering with arc as the pilot; the
// #2025 single-file split; #2059..#2063 migrated all op extern bodies into
// `abi/`; #2069 made `result_branch` re-entrant-safe via a `core` free fn so
// every `abi/**` file became IR-free; #2109 split the former `core` umbrella
// into `context` + `primitive/` + `composite/` for layered responsibility):
//
//   - `abi`        : the C/C++ emission boundary — opaque handle types, scalar
//                    / enum typedefs, descriptor structs, layout assertions,
//                    and the plumbing that exists because C++ calls in (the
//                    `ctx_access`-confined `cx` reify reached only via
//                    `with_ctx` / `checked_cx` (#2081), `intern` / `resolve`,
//                    handle casts). Each migrated op's `#[no_mangle]` externs
//                    live in a child module `abi::<op>` (`abi/any.rs`,
//                    `abi/arc.rs`, `abi/bounds.rs`, `abi/cast.rs`,
//                    `abi/collection.rs`, `abi/control_flow.rs`, `abi/cow.rs`,
//                    `abi/function.rs`, `abi/lifecycle.rs`, `abi/option.rs`,
//                    `abi/primitive.rs`, `abi/reduce.rs`, `abi/result.rs`,
//                    `abi/runtime_call.rs`, `abi/test_introspect.rs`) that
//                    resolves / translates and calls the matching engine
//                    method or free function. The CI gate
//                    `scripts/check-emit-abi-no-ir.sh` keeps the abi layer
//                    IR-free (#2069).
//
//   - `context`    : the layer-neutral state, handles, enums, layout
//                    constants, and the pure-data `HdrField` / `HeaderKind` /
//                    `header_fields` table. Holds the concrete `EmitCtx` (LLVM
//                    handles + intern table + msg dedup caches) but no IR
//                    generation; `llvm_sys::prelude` is permitted for the raw
//                    handle types, `llvm_sys::core` is not. Imported by every
//                    other layer; imports nothing from sibling layers.
//
//   - `primitive`  : LLVM-near 1:1 emission with no Ry semantics — type
//                    constructors, name builders, module-symbol lookup, plain
//                    string globals, libc emitters, inline runtime-error,
//                    scalar / memory ops, function creation / indirect call /
//                    intrinsic call, control flow, generic `__ry_*` runtime
//                    calls. `primitive` is the LLVM C API concentration target.
//                    `primitive ⇏ composite` is enforced by
//                    `scripts/check-emit-composite-no-primitive.sh`.
//
//   - `composite`  : Ry-semantic emission — ARC retain/release, bounds check,
//                    checked FP→int, Option/Result construction, Any wrap/
//                    unwrap, collection mutations, CoW ensure-unique, reduce,
//                    and the shared header layout helpers
//                    (`build_header_struct` / `load_list_header` / etc.).
//                    `composite` may call `primitive` (the layering goal) and
//                    other `composite` siblings (sharing layout helpers).
//                    Some `composite/**.rs` still call `llvm_sys::core` /
//                    `LLVMBuild*` directly as a transitional carve-out
//                    (verbatim move preserved IR byte-exactness); a future
//                    issue migrates those to primitive method calls. The CI
//                    gate `scripts/check-emit-llvm-ir-gen-concentration.sh`
//                    freezes the carve-out allowlist and blocks new
//                    `llvm_sys::core` callers (#2109 AC #7).
//
//   - `ffi`        : the Rust-native public surface — re-exports `EmitCtx` /
//                    `ValueRef` / `Atomicity` from `context`. The convergence
//                    point where the C++ path (via `abi`) and the Rust-direct
//                    path meet. Provisional name (a holistic rename is
//                    deferred to #2022).
//
// Dependency direction is `abi → composite → primitive → context`, with
// `abi → primitive` and `abi → context` permitted as transitional shortcuts
// (the abi shell sometimes calls primitive entries directly). `context` sits
// below every other layer and imports nothing from siblings. The future
// physical crate split (#2023) drops `extern "C"` and turns the import-path
// gates into Cargo dependency constraints.

#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(clippy::missing_safety_doc)]

mod abi;
mod composite;
mod context;
mod ffi;
mod primitive;

// Surface the boundary types/constants at the crate root. Some of them
// (e.g. `RY_BOUNDS_ARRAY`) mirror api.h for boundary
// completeness without an internal Rust referent; re-exporting keeps them
// reachable — matching the pre-split crate-root visibility — instead of
// being flagged as dead code now that they live in a private module.
pub use abi::*;
