//! ffi — the Rust-native public surface: the convergence point where the C++
//! path (via `abi`) and the Rust-direct path meet, expressed in the engine's
//! Rust-native vocabulary. For this pilot it is a thin re-export of `core`'s
//! `EmitCtx` / `ValueRef` / `Atomicity`; the layer earns its keep once an
//! external Rust consumer and the physical crate split (cdylib `abi` + rlib
//! `ffi` + `core`) arrive. NOTE: the name `ffi` is provisional and arguably
//! inverted (the real foreign-function interface is `abi`); a holistic naming
//! pass is deferred to #2022.

// Intentionally retained convergence surface: until #2022 lands a Rust-direct
// consumer (and the physical crate split), nothing inside the crate imports
// these *through* `ffi` — the core-role modules reach `core` directly and `abi`
// imports `EmitCtx` from `core`. The legacy `any.rs` was the last incidental
// `crate::ffi` consumer; #2063 migrated it to `crate::core`, so this re-export
// now has no in-crate caller. Keep it (it documents the Rust-native surface) and
// allow the lint rather than deleting the layer.
#[allow(unused_imports)]
pub(crate) use crate::core::{Atomicity, EmitCtx, ValueRef};
