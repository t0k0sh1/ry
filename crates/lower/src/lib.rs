//! Upper-codegen (Ry semantic lowering) — kickoff pilot for #2397.
//!
//! Single op today: `ry_lower_bool_const` — the Rust port of
//! `CodeGen::emitExprVariant(BoolExpr)`. The op is the minimum-surface
//! pilot for the new `ry_lower_*` boundary; see
//! `docs/architecture/upper-codegen-migration.md` §"Pilot module".
//!
//! Architectural layering (mirrors the `emit` crate's #2057 split, kept
//! flat while the surface is tiny):
//!
//! - The `extern "C"` `ry_lower_*` entries live in `abi.rs`.
//! - Ry-semantic decisions (currently just "wrap a bool in an i1") live
//!   inline here. As more ops migrate, the body splits per stage
//!   (`expr.rs`, `stmt.rs`, …) per the migration order in the design doc.
//!
//! The crate has NO `llvm-sys` dependency. Calls into the loaded `emit`
//! cdylib are declared `extern "C"` in `emit_extern.rs`.

#![allow(non_camel_case_types)]

mod abi;
mod emit_extern;
mod handles;
