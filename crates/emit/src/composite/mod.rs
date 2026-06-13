//! composite — Ry-semantic emission. Each module owns a Ry composite operation
//! whose lowering decisions (ARC ordering, header layout, runtime symbol
//! choice, msg-global shape) have already been settled and that expands to a
//! fixed LLVM instruction sequence. `composite` modules may call into
//! `crate::primitive` (the goal: composite expresses its op as a sequence of
//! primitive method calls) and into sibling `composite::*` modules
//! (`composite::header` is shared layout knowledge; `composite::arc` exposes
//! the StringHeader-prefixed `.arc` global helper).
//!
//! Layering invariants:
//!  - `composite` may import `crate::context`, `crate::primitive::*`, and
//!    sibling `crate::composite::*` modules.
//!  - `composite` MUST NOT import `crate::abi` (`abi → composite` is allowed,
//!    not the reverse).
//!  - `primitive ⇏ composite` is enforced by
//!    `scripts/check-emit-composite-no-primitive.sh`.
//!  - Some composite modules still call `llvm_sys::core` / `LLVMBuild*`
//!    directly — a transitional carve-out (see #2109 AC #5/#7) frozen by
//!    `scripts/check-emit-llvm-ir-gen-concentration.sh` so no new direct
//!    callers can be added.

pub(crate) mod any;
pub(crate) mod arc;
pub(crate) mod bounds;
pub(crate) mod cast;
pub(crate) mod collection;
pub(crate) mod cow;
pub(crate) mod header;
pub(crate) mod option;
pub(crate) mod reduce;
pub(crate) mod result;
