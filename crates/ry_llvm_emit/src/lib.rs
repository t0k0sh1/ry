// Rust implementation of the LLVM IR emission ABI defined in
// `include/ry/llvm_emit/api.h`.
//
// Every type and function exposed here mirrors a declaration in api.h
// byte-for-byte. The C ABI is locked by #1949 — do NOT widen or alter
// the public signatures without updating api.h and the reinterpret_cast
// wrappers in include/ry/llvm_emit/cast_helpers.hpp.
//
// Build-time invariants (verified whenever the cdylib compiles):
//   - the 28 ABI symbols are exported (`nm -D` / `otool -L`),
//   - the FFI types compile against api.h (sizeof / repr alignment),
//   - corrosion-rs is wired correctly into the CMake build,
//   - `-undefined dynamic_lookup` link arg flows through on macOS,
//   - llvm-sys = "211" finds LLVM 21 via LLVM_SYS_211_PREFIX.
//
// Module layout (#2025 — split of the former single-file lib.rs):
//   - `ffi`          : the locked C ABI surface (opaque handle types, scalar /
//                      enum typedefs, descriptor structs, layout assertions).
//   - `support`      : shared internal helpers (EmitCtxImpl, intern/resolve,
//                      handle casts, type constructors, name builders,
//                      module-global lookup, layout constants, inline
//                      runtime-error / ARC-alloc helpers).
//   - one module per lowered-op category, each owning its `ry_emit_*` ABI
//     bodies: `lifecycle`, `bounds`, `result`, `option`, `arc`,
//     `runtime_call`, `collection`, `cow`, `any`, `control_flow`.
//
// The `#[no_mangle] extern "C"` ABI functions are exported from the cdylib
// regardless of which (private) module they live in.

#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(clippy::missing_safety_doc)]

mod any;
mod arc;
mod bounds;
mod collection;
mod control_flow;
mod cow;
mod ffi;
mod lifecycle;
mod option;
mod result;
mod runtime_call;
mod support;

// Surface the ABI types/constants at the crate root. Some of them
// (e.g. `RyBasicBlockId`, `RY_BOUNDS_ARRAY`) mirror api.h for ABI
// completeness without an internal Rust referent; re-exporting keeps them
// reachable — matching the pre-split crate-root visibility — instead of
// being flagged as dead code now that they live in a private module.
pub use ffi::*;
