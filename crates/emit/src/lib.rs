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
//     `ry_emit_test_header_layout` introspection symbol (#2071, 28 + 1 today),
//   - the FFI types compile against api.h (sizeof / repr alignment),
//   - corrosion-rs is wired correctly into the CMake build,
//   - `-undefined dynamic_lookup` link arg flows through on macOS,
//   - llvm-sys = "211" finds LLVM 21 via LLVM_SYS_211_PREFIX.
//
// Module layout (#2057 — abi/ffi/core layering with arc as the pilot; builds on
// the #2025 single-file split. #2059 extended the migration to control_flow /
// option / lifecycle and moved each migrated op's externs into the `abi/`
// directory; #2060 migrated bounds / result / runtime_call; #2061 migrated
// collection; #2062 migrated cow; #2063 migrated any — the last legacy module,
// completing the series so no module outside the `abi/` children references
// `crate::abi`):
//   - `abi`   : the C/C++ emission boundary — opaque handle types, scalar / enum
//               typedefs, descriptor structs, layout assertions, and the
//               plumbing that exists because C++ calls in (`cx`, `intern` /
//               `resolve`, handle casts). `cstr_bytes` lives in `core` (a pure
//               `CStr` primitive used by the bounds / any engines, which reach
//               it via `crate::core` directly). Each migrated op's `#[no_mangle]`
//               externs live in a child module `abi::<op>` (`abi/any.rs`,
//               `abi/arc.rs`, `abi/bounds.rs`, `abi/collection.rs`,
//               `abi/control_flow.rs`, `abi/cow.rs`, `abi/lifecycle.rs`,
//               `abi/option.rs`, `abi/result.rs`, `abi/runtime_call.rs`) that
//               resolves / translates and calls the matching core method.
//   - `core`  : the abi-independent IR generation engine — `EmitCtx` (+ its
//               `new` constructor), the Rust-native `ValueRef` / `Atomicity` /
//               `BasicBlockRef` / `FunctionRef` / `TypeRef` / `FuncTypeRef`
//               handles + the `BoundsKind` enum, basic-IR type constructors,
//               name builders (`cstr_bytes` / `cname_pfx`), module-global
//               lookup, layout constants, libc / runtime-error / ARC-alloc
//               emitters. Must NOT reference `abi` (the `core⇏abi` invariant).
//   - `ffi`   : the Rust-native public surface — re-exports `EmitCtx` /
//               `ValueRef` / `Atomicity` from `core`. The convergence point
//               where the C++ path (via `abi`) and the Rust-direct path meet.
//               Provisional name (a holistic rename is deferred to #2022).
//   - migrated core-role op modules, each an `impl EmitCtx` over the core engine
//     only (no `use crate::abi`): `any`, `arc`, `bounds`, `collection`,
//     `control_flow`, `cow`, `option`, `runtime_call`, and `result`. `any`
//     (#2063) holds the three `any_{wrap,unwrap,try_unwrap}` methods returning
//     `Option<ValueRef>` — the abi shell does the kind mapping + top-level guards
//     and interns `None → 0`, while the branch-specific guards stay in the method
//     (the descriptor-passing pattern from #2062). `result` splits
//     asymmetrically — only `build_error_from_runtime` is a core method here;
//     `result_branch` keeps its orchestration in `abi/result.rs` because its
//     ok/err builders are re-entrant C callbacks that cannot cross into a
//     `&mut self` method (#2060). `lifecycle` emits no IR, so its only core part
//     (`EmitCtx::new`) lives in `core` and it has no core-role module. With `any`
//     migrated, no legacy module remains.
//
// Dependency direction is one-way `abi → core`, with `ffi` re-exporting the
// `core` surface. Why one-way: `abi` is the *removable* C-boundary shell — the
// #2023 end-state drops `extern "C"` for a Rust-native library — so `core` must
// stay compilable without it; once the physical crate split lands `abi` becomes
// a separate cdylib that `core`'s rlib does not depend on, and Rust-direct
// callers bypass `abi` to call `core` methods (the convergence point `ffi`
// names). The `#[no_mangle] extern "C"` boundary functions are exported from the
// cdylib regardless of which (private) module they live in.

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
mod option;
mod result;
mod runtime_call;

// Surface the boundary types/constants at the crate root. Some of them
// (e.g. `RY_BOUNDS_ARRAY`) mirror api.h for boundary
// completeness without an internal Rust referent; re-exporting keeps them
// reachable — matching the pre-split crate-root visibility — instead of
// being flagged as dead code now that they live in a private module.
pub use abi::*;
