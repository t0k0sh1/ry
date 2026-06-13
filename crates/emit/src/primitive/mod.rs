//! primitive — LLVM-near IR emission with no Ry semantics. Each module here
//! owns a single primitive capability that is generic over Ry layouts and ABI
//! decisions: type constructors, name builders, module-symbol lookup, plain
//! string globals, libc emitters, inline runtime-error, scalar / memory ops,
//! function creation / indirect call / intrinsic call, control flow, and
//! generic `__ry_*` runtime calls.
//!
//! Layering invariants (enforced by `scripts/check-emit-composite-no-primitive.sh`):
//!  - `primitive` may import `crate::context` (handles, enums, layout constants),
//!    `llvm_sys::core`, `llvm_sys::prelude`, and other `crate::primitive` siblings.
//!  - `primitive` MUST NOT import `crate::composite` (the `primitive ⇏ composite`
//!    rule — this layer must not reach upward for Ry-semantic helpers).
//!  - `primitive` MUST NOT import `crate::abi` (`abi → primitive` is allowed,
//!    not the reverse; abi is the removable C shell).
//!
//! This module group is the LLVM C API concentration target — `llvm_sys::core`
//! IR-gen is permitted here. The `composite` layer is encouraged to call into
//! these primitives instead of touching `LLVMBuild*` directly, but a
//! transitional carve-out (allowlisted by
//! `scripts/check-emit-llvm-ir-gen-concentration.sh`) keeps the existing
//! verbatim-move composite implementations bit-exact (#2109).

pub(crate) mod control_flow;
pub(crate) mod error;
pub(crate) mod function;
pub(crate) mod global;
pub(crate) mod libc;
pub(crate) mod module;
pub(crate) mod ops;
pub(crate) mod runtime_call;
pub(crate) mod types;
pub(crate) mod util;
