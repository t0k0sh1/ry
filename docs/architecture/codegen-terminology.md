# Codegen Terminology

This page is the canonical vocabulary for the **codegen** stack of the Ry compiler — the layers between the AST/sema front end and executable machine code. It was established by #2022 to remove the multiply-defined terms (notably the overloaded word "ABI") that were a source of confusion when planning the LLVM IR emission work (theme C; see [Codegen Layering Plan](codegen-layering-plan.md) / [LLVM IR Emission Boundary](llvm-ir-emission-boundary.md)). Other architecture pages assume these definitions; if any reads ambiguously, this page wins.

> User-facing *language* terminology (module / package / visibility / stdlib) lives in [`docs/reference/glossary.md`](../reference/glossary.md). This page is contributor-internal and covers only the codegen stack.

## Layers

The codegen stack is the `ry::codegen::*` namespace in C++ plus the Rust crate `emit` that implements the emission boundary (renamed from `ry_codegen` in #2040; itself renamed from `ry_llvm_emit` in #2027①). It is organized into layers:

| Term | Definition | Where it lives |
| --- | --- | --- |
| **codegen** | The whole code-generation stack: AST + sema → LLVM IR. | C++ `ry::codegen::*`, Rust crate `emit` |
| **lowering** | Translates Ry semantics into **lowered IR** ops. Owns the per-op semantic decisions (ARC retain/release, metadata, element sizes). | `src/codegen_lowering_*.cpp`, `ry::codegen::lowering` |
| **lowered IR** | The op vocabulary — plain-data structs naming *what* should happen, not *how* to express it in LLVM. | `include/ry/codegen/lowered_*.hpp`, `ry::codegen::lowered::XxxOp` |
| **emission** | Turns lowered ops into LLVM IR. Owns the basic-block / PHI / `Create*` plumbing. | `src/codegen_emission_*.cpp`, `ry::codegen::emission`, Rust crate `emit` |
| **composite emission** | Emission code that encodes a Ry ABI / layout decision — ARC retain/release, bounds check, checked FP→int, Option / Result construction, Any wrap/unwrap, collection mutations, CoW ensure-unique, reduce, and shared header struct construction. Expressed as a fixed LLVM-instruction sequence keyed on the lowered op (#2109). | `crates/emit/src/composite/**.rs` |
| **primitive emission** | Emission code that is LLVM 1:1 with no Ry-layout knowledge — type constructors, name builders, module-symbol lookup, plain string globals, libc emitters, the inline runtime-error exit, scalar / memory ops, function creation / indirect call / intrinsic call, control flow, and generic `__ry_*` runtime calls (#2109). | `crates/emit/src/primitive/**.rs` |

The full op list lives in [Codegen Layering Plan](codegen-layering-plan.md) §"Lowered IR vocabulary"; the composite / primitive split inside the `emit` crate is documented in §"Composite and primitive emission sub-layers (#2109)". Use the noun **lowered IR** (the data) and **lowered op** (one struct); retire the floating phrase "N-op vocabulary" — the op count changes over time.

## The two boundaries

Ry has two C-only `extern "C"` boundaries, **named after their layer**:

| Boundary | Symbols | Crossed | Implemented by |
| --- | --- | --- | --- |
| **runtime boundary** | `__ry_*` ("runtime calls") | at **program run time**, by JIT-executed code | the runtime — see [Runtime Boundary](runtime-abi-boundary.md) |
| **emission boundary** | `ry_emit_*` ("emission entry points") | at **compile time**, to construct LLVM IR | the emission layer (the `emit` crate) — see [LLVM IR Emission Boundary](llvm-ir-emission-boundary.md) |

The two are orthogonal: lowering drives IR construction through the *emission entry points*; the IR it builds resolves, at run time, to *runtime calls*.

## Handle naming (emission boundary types)

The emission boundary passes LLVM objects across as opaque handles, in **two** categories:

| Suffix | Representation | Meaning | Examples |
| --- | --- | --- | --- |
| **`Id`** | interned `uint32_t` (sentinel `0` = invalid) | a value the emission layer **creates and owns**, round-tripped via the per-context intern table | `RyValueId` |
| **`Ref`** | opaque pointer (`struct X *`) | an LLVM object **passed across by pointer** (types, the per-compile singleton objects, source-side values) — `reinterpret_cast` on both sides, no interning | `RyTypeRef`, `RyFuncTypeRef`, `RyValueRef`, `RyBasicBlockRef`; and, after #2027, `RyModuleRef` / `RyBuilderRef` / `RyContextRef` / `RyFunctionRef` |

The earlier third suffix `Handle` (for the per-compile singletons `RyModuleHandle` / `RyBuilderHandle` / `RyContextHandle` / `RyFunctionHandle`) folds into `Ref`: it was representationally identical (an opaque LLVM pointer), and the singleton nature is already obvious from the type name. The `ry_emit_*` function prefix and the `Ry*` type prefix are kept — an `extern "C"` symbol needs a project prefix to avoid collisions with `__ry_*` / libc / the LLVM C API.

## Migration (#2027)

This page is the naming **decision** (#2022, docs-only). The physical rename is executed atomically across `api.h` ↔ C++ ↔ Rust by #2027 (the boundary cannot change on one side alone):

- **Crate** `ry_llvm_emit` → `ry_codegen` (#2027①; the bare name `codegen` is a reserved CMake target name, CMP0171).
- **Crate** `ry_codegen` → `emit` (#2040; the `ry_` prefix was redundant. Unlike `codegen`, the bare name `emit` is **not** a reserved CMake target name, so it needs no prefix and trips no CMP0171 warning — a clean behavior-preserving rename).
- **Keep** the 28 `ry_emit_*` functions and the `Ry*` type prefix unchanged.
- **`Handle` → `Ref`** (4 typedefs): `RyModuleHandle`→`RyModuleRef`, `RyBuilderHandle`→`RyBuilderRef`, `RyContextHandle`→`RyContextRef`, `RyFunctionHandle`→`RyFunctionRef`, plus the matching `cast_helpers.hpp` accessors.
- **Remove** `RyBasicBlockId` — declared and layout-asserted but used in no public signature (the ControlFlow entries use `RyBasicBlockRef`).
- **Purge the "ABI" label** from code comments, `.claude/`, and the `api.h` header comment — using the vocabulary above. (Superseded for the emission crate by the #2057 `abi` / `ffi` / `core` module split — see [LLVM IR Emission Boundary](llvm-ir-emission-boundary.md) §"Layer structure (#2057)".)
- **Keep** `std::ffi`.
- **Do not touch**: `CHANGELOG.md` (frozen), and artifact names that contain "abi" — `runtime-abi-boundary.md`, `scripts/check-llvm-emit-abi-header.sh`, `tests/test_abi_layout.cpp`, `tests/test_emit_abi_guards.cpp`.

## Related documents

- [Compiler Layers](compiler-layers.md) — layer ordering and dependency direction.
- [Codegen Layering Plan](codegen-layering-plan.md) — the lowering / emission split and the lowered IR vocabulary.
- [LLVM IR Emission Boundary](llvm-ir-emission-boundary.md) — the emission boundary (`ry_emit_*`) inside the codegen layer.
- [Runtime Boundary](runtime-abi-boundary.md) — the orthogonal `__ry_*` boundary on the runtime side.
