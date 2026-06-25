# Codegen Terminology

This page is the canonical vocabulary for the compiler codegen stack. User-facing language terms such as module, package, visibility, and stdlib live in [Glossary](../reference/glossary.md).

## Layers

| Term | Meaning | Current home |
|---|---|---|
| codegen | The whole AST/sema-to-LLVM-IR stack | `src/codegen_*.cpp`, `include/ry/codegen.hpp`, Rust `emit` crate |
| lowering | Ry-semantic decision code that prepares what should be emitted | caller-side C++ in `src/codegen_*.cpp` |
| lowered IR / lowered op | Plain operation data naming what should happen | local values passed to `ry_emit_*`; no standalone C++ shim namespace remains |
| emission | LLVM IR construction behind the boundary | `crates/emit/src/` |
| composite emission | Ry-layout-aware emission | `crates/emit/src/composite/**.rs` |
| primitive emission | LLVM-near reusable operations with no Ry semantic dispatch | `crates/emit/src/primitive/**.rs` |

Use **lowered IR** for the data model and **lowered op** for one operation. Avoid phrases that bake in an operation count; the surface changes over time.

## Boundaries

Ry has two C-only boundaries:

| Boundary | Symbol family | Crossed when | Implemented by |
|---|---|---|---|
| runtime boundary | `__ry_*` | generated program runs | runtime libraries; see [Runtime Boundary](runtime-abi-boundary.md) |
| emission boundary | `ry_emit_*` | compiler constructs LLVM IR | Rust `emit` crate; see [LLVM IR Emission Boundary](llvm-ir-emission-boundary.md) |

The lowering side calls `ry_emit_*` at compile time. The IR it produces may call `__ry_*` at run time.

## Handle Names

Emission boundary types use two suffixes:

| Suffix | Representation | Meaning | Examples |
|---|---|---|---|
| `Id` | interned `uint32_t`; `0` is invalid | value created and owned by the emission context | `RyValueId` |
| `Ref` | opaque pointer | LLVM object shared across the boundary by pointer cast | `RyTypeRef`, `RyValueRef`, `RyFunctionRef`, `RyBasicBlockRef` |

The old `Handle` suffix was folded into `Ref` for pointer-shaped handles. `ry_emit_*` function names and `Ry*` type names stay project-prefixed to avoid collisions with runtime and libc symbols.

## Historical Note

After the Rust cutover, the C++ side briefly had `codegen_emission_*`, `codegen_lowering_*`, and `lowered_*` shim files under `ry::codegen::{emission,lowering,lowered}`. #2229 removed that layer. Current caller-side C++ constructs per-op inputs inline and calls `ry_emit_*` directly.

## Related Documents

- [Compiler Layers](compiler-layers.md)
- [Codegen Layering Plan](codegen-layering-plan.md)
- [LLVM IR Emission Boundary](llvm-ir-emission-boundary.md)
- [Runtime Boundary](runtime-abi-boundary.md)
