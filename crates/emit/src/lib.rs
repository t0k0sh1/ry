// Rust implementation of the LLVM IR emission boundary defined in
// `include/ry/llvm_emit/api.h`.
//
// Every type and function exposed here mirrors a declaration in api.h
// byte-for-byte. The C boundary is locked by #1949 — do NOT widen or alter
// the public signatures without updating api.h and the reinterpret_cast
// wrappers in include/ry/llvm_emit/cast_helpers.hpp.
//
// Build-time invariants (verified whenever the cdylib compiles):
//   - the 28 boundary symbols are exported (`nm -D` / `otool -L`),
//   - the FFI types compile against api.h (sizeof / repr alignment),
//   - corrosion-rs is wired correctly into the CMake build,
//   - `-undefined dynamic_lookup` link arg flows through on macOS,
//   - llvm-sys = "211" finds LLVM 21 via LLVM_SYS_211_PREFIX.
//
// Module layout (#2057 — abi/ffi/core layering with arc as the pilot; builds on
// the #2025 single-file split):
//   - `abi`   : the C/C++ emission boundary — opaque handle types, scalar / enum
//               typedefs, descriptor structs, layout assertions, and the
//               plumbing that exists because C++ calls in (`cx`, `intern` /
//               `resolve`, handle casts, `cstr_bytes`).
//   - `core`  : the abi-independent IR generation engine — `EmitCtx`, the
//               Rust-native `ValueRef` / `Atomicity` handles, basic-IR type
//               constructors, name builders, module-global lookup, layout
//               constants, libc / runtime-error / ARC-alloc emitters. Must NOT
//               reference `abi` (the `core⇏abi` invariant).
//   - `ffi`   : the Rust-native public surface — re-exports `EmitCtx` /
//               `ValueRef` / `Atomicity` from `core`. The convergence point
//               where the C++ path (via `abi`) and the Rust-direct path meet.
//               Provisional name (a holistic rename is deferred to #2022).
//   - one module per lowered-op category, each owning its `ry_emit_*` boundary
//     bodies: `lifecycle`, `bounds`, `result`, `option`, `arc`,
//     `runtime_call`, `collection`, `cow`, `any`, `control_flow`. `arc` is the
//     pilot migrated onto the layering (core method + abi extern); the rest are
//     import-re-pointed only.
//
// Dependency direction is one-way `abi → core`, with `ffi` re-exporting the
// `core` surface. The `#[no_mangle] extern "C"` boundary functions are exported
// from the cdylib regardless of which (private) module they live in.

#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(clippy::missing_safety_doc)]

mod abi;
mod any;
mod arc;
mod bounds;
mod collection;
mod control_flow;
mod core;
mod cow;
mod ffi;
mod lifecycle;
mod option;
mod result;
mod runtime_call;

// Surface the boundary types/constants at the crate root. Some of them
// (e.g. `RY_BOUNDS_ARRAY`) mirror api.h for boundary
// completeness without an internal Rust referent; re-exporting keeps them
// reachable — matching the pre-split crate-root visibility — instead of
// being flagged as dead code now that they live in a private module.
pub use abi::*;
