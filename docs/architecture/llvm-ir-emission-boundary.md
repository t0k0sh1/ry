# LLVM IR Emission Boundary

This document records the current contract for the LLVM IR emission layer. It is contributor-facing: language semantics live in `docs/reference/`, while codegen vocabulary lives in [Codegen Terminology](codegen-terminology.md).

## Purpose

The emission layer owns LLVM IR construction. Caller-side codegen decides Ry semantics, prepares operation inputs, and calls the `ry_emit_*` boundary. The Rust `emit` crate turns those inputs into LLVM IR.

Current implementation:

- C boundary: `include/ry/llvm_emit/api.h`
- C++ cast helpers: `include/ry/llvm_emit/cast_helpers.hpp`
- Rust implementation: `crates/emit/src/`
- Build artifact: Rust `cdylib`, linked by the compiler process

## Boundary Rule

Public `ry_emit_*` signatures must not expose LLVM-owned C++ types:

- no `llvm::Value*`, `llvm::Type*`, `llvm::Function*`, `llvm::BasicBlock*`
- no `llvm::Module&`, `llvm::LLVMContext&`, `llvm::IRBuilder<>`
- no LLVM C++ template types or references

The boundary uses C-compatible values:

- opaque pointers for shared LLVM objects (`RyTypeRef`, `RyValueRef`, `RyFunctionRef`, etc.)
- interned integer ids for values created and owned by the emission context (`RyValueId`)
- `#[repr(C)]` / POD descriptors for composite operations
- plain scalar enums and flags

`void *user_ctx` is allowed only for callback trampolines such as `RyBuildValueFn`; it is caller-owned and never interpreted by the emission layer.

## Layer Shape

The Rust crate is organized into four roles:

| Role | Owns |
|---|---|
| `abi` | `extern "C"` entry points, descriptor structs, opaque handle types, layout assertions, handle/id plumbing |
| `composite` | Ry-layout-aware emission such as ARC, bounds checks, Option/Result, Any, collections, CoW, reduce |
| `primitive` | LLVM-near operations such as type constructors, loads/stores, GEPs, branches, function creation, runtime calls |
| `context` | `EmitCtx`, intern tables, handle wrappers, shared constants and caches |

Dependency direction is one-way:

```text
abi -> composite -> primitive -> context
abi -> primitive -> context
```

`primitive` and `context` must not depend on Ry composite semantics. `context` must stay free of boundary plumbing.

## Operation Discipline

The lowering side decides Ry semantics. The emission side should receive enough plain data to emit a fixed IR shape without re-performing semantic dispatch.

Use a composite `ry_emit_*` entry when the operation encodes Ry layout or runtime invariants, for example:

- ARC retain/release and ARC counter bookkeeping
- bounds check and negative-index handling
- Option/Result construction
- Any wrap/unwrap
- collection mutation and CoW

Use primitive entries when the operation is generic LLVM construction, for example:

- load/store/GEP/alloca
- arithmetic and comparison primitives
- branch/PHI/basic block construction
- function creation, indirect calls, intrinsic calls
- fixed-arity runtime symbol calls

Prefer reusable generic primitives over coarse descriptors when the descriptor would leak Ry semantics into emission without reducing caller-side decisions.

## Invariants

- `api.h` is the boundary source of truth; Rust mirrors it byte-for-byte for `repr(C)` layout.
- Descriptor layout is guarded on both C++ and Rust sides by compile-time assertions.
- Boundary entry points must validate null handles / invalid ids defensively and return sentinel values instead of unwinding across `extern "C"`.
- Codegen-side semantic shims may remain in C++ when they own metadata, ARC retain decisions, type-name resolution, or diagnostic behavior.
- Emission must use the builder's current insert block to derive the active parent function where needed; cached function state is not authoritative across nested function emission.
- `ry` and the `emit` cdylib must share one `libLLVM` instance. Rust builds use `llvm-sys` with dynamic LLVM; static-only LLVM prefixes are not valid for the Rust emission path.

## Verification

Applicable checks:

- `scripts/check-llvm-emit-abi-header.sh` validates that the public header stays LLVM-C++-free.
- `scripts/check-emit-abi-no-ir.sh` keeps `abi` free of direct IR generation.
- `scripts/check-emit-composite-no-primitive.sh` and `scripts/check-emit-llvm-ir-gen-concentration.sh` enforce the current module-direction and IR-generation concentration rules.
- FileCheck and `--emit-llvm-ir` parity checks are used for emission behavior changes. Migration PRs should coverage-gate the path first, then compare ASLR-normalized IR before and after the change.

## Historical Milestones

Detailed issue-by-issue migration notes are intentionally not kept here. The current contract supersedes the old C++ shim layer and the staged #1949 / #1950 / #1993 migration narrative.

Key milestones:

| Issue | Result |
|---|---|
| #1949 | Introduced the shared-library boundary scaffold |
| #1950 / #1993 | Reimplemented and cut over the emission implementation to Rust |
| #2025 / #2057 / #2059 / #2109 | Split the Rust crate into responsibility layers |
| #2229 | Removed the C++ lowering/emission shim files; caller-side C++ now calls `ry_emit_*` directly |

## Related Documents

- [Codegen Terminology](codegen-terminology.md)
- [Codegen Layering Plan](codegen-layering-plan.md)
- [Compiler Layers](compiler-layers.md)
- [Runtime Boundary](runtime-abi-boundary.md)
