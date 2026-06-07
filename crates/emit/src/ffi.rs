//! ffi — the Rust-native public surface: the convergence point where the C++
//! path (via `abi`) and the Rust-direct path meet, expressed in the engine's
//! Rust-native vocabulary. For this pilot it is a thin re-export of `core`'s
//! `EmitCtx` / `ValueRef` / `Atomicity`; the layer earns its keep once an
//! external Rust consumer and the physical crate split (cdylib `abi` + rlib
//! `ffi` + `core`) arrive. NOTE: the name `ffi` is provisional and arguably
//! inverted (the real foreign-function interface is `abi`); a holistic naming
//! pass is deferred to #2022.

pub(crate) use crate::core::{Atomicity, EmitCtx, ValueRef};
