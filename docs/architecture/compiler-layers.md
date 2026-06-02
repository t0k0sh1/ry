# Compiler Layers and Dependency Direction

This document records the intended dependency direction between the compiler/runtime layers of the Ry implementation. It is the reference for issue #1820 (v0.0.26 boundary-tightening) and the foundation for the Rust migration of the LLVM IR emission layer (shared-library extraction in #1949; reimplemented in Rust, landed via #1950 / #1993).

## Layers

Source code is organized into the following layers, ordered from input (left) to output (right):

```text
lexer → parser → AST → module loader → sema → codegen → runtime ABI
```

| Layer | Primary header | Responsibility |
| --- | --- | --- |
| lexer | `include/ry/lexer/lexer.hpp` | Tokenize source text into a `Token` stream. Owns indentation/dedentation state and source-position tracking. |
| parser | `include/ry/parser/parser.hpp` | Consume tokens and emit `ast::*` nodes. Owns precedence/associativity tables and recovery decisions. |
| AST | `include/ry/ast/ast.hpp` | Plain-data node definitions shared by every later layer. The AST is the contract between parsing and downstream consumers. |
| module loader | `include/ry/module/module_loader.hpp` | Resolve `import` graphs, load `.ry` source files lazily, and provide AST roots for each module. |
| sema | `include/ry/sema/*.hpp` | Static analysis that runs alongside codegen — return-path coverage (`sema_return.hpp`), pattern-match exhaustiveness, etc. |
| codegen | `include/ry/codegen.hpp` | Lower AST + sema results into LLVM IR. Owns the `CodeGen` monolith (LLVM context, ARC bookkeeping, type/metadata registries, stdlib dispatch). A 2-layer split into Ry semantic lowering vs LLVM IR emission is the v0.0.26 working hypothesis ([Codegen Layering Plan](codegen-layering-plan.md)) and was the critical path to the shared-library extraction in #1949 and the Rust reimplementation in #1950 / #1993 (both landed). |
| runtime ABI | `include/ry/runtime/{core,native}/*.hpp` | C++ runtime entry points exposed via `extern "C"` symbols (`__ry_*`). See [Runtime ABI Boundary](runtime-abi-boundary.md) for the categorization. |

`codegen_native_dispatch.hpp` / `directive_meta.hpp` / `ry_layout.hpp` / `codegen_guards.hpp` are shared declarations co-owned by codegen and the runtime ABI; they sit at the codegen/runtime interface and intentionally span the two layers.

## Dependency direction rule

**Each layer may only `#include` headers that belong to layers strictly to its left.** Adding a back-edge (e.g. `parser/` including `codegen.hpp`) violates the layering and indicates that a shared concern should be lifted into one of the layers on the left, or split into a new layer-independent helper under `include/ry/util/`.

The observed adjacency list as of v0.0.26 is:

| Header | Inbound `ry/` includes |
| --- | --- |
| `lexer/lexer.hpp` | (leaf — no `ry/` includes) |
| `parser/parser.hpp` | `lexer/lexer.hpp`, `ast/ast.hpp`, `source_manager/source_manager.hpp` |
| `module/module_loader.hpp` | `ast/ast.hpp`, `source_manager/source_manager.hpp` |
| `sema/sema_return.hpp` | `ast/ast.hpp` |
| `sema/sema.hpp` | (no `ry/` includes — implementation pulls deps via `.cpp`) |
| `codegen.hpp` | `ast/ast.hpp`, `sema/sema_return.hpp`, `source_manager/*`, `trace/trace.hpp`, `ry_layout.hpp`, `directive_meta.hpp`, `codegen_native_dispatch.hpp`, `codegen_guards.hpp` |

`source_manager/` and `trace/` are layer-independent utilities; they may be referenced by any layer.

## Invariants

- **`codegen` does not depend on `parser` or `module_loader`.** Codegen receives an already-parsed AST plus a `SourceManager` reference; it does not re-enter the parser. Maintain this invariant when adding new codegen entry points.
- **`runtime ABI` does not depend on `codegen`.** Runtime `.cpp` files in `src/runtime/{core,native}/` link against LLVM-free headers (`include/ry/runtime/{core,native}/*.hpp`) only. Codegen calls into the runtime ABI by emitting LLVM IR that resolves to `extern "C"` symbols; the runtime side never sees `llvm::Value*` or `IRBuilder<>`. This separation is what made the #1949 shared-library extraction and the Rust reimplementation in #1950 / #1993 feasible.
- **Layer-independent helpers live under `include/ry/util/`.** Pure utilities that operate on strings, type names, or other plain data (no LLVM, no parser, no codegen state) belong under `util/`. Issue #1820 establishes this directory with `include/ry/util/type_name.hpp` for type-name parsing helpers extracted from `CodeGen`.

## Related documents

- [Layer Graduation Workflow](layer-graduation-workflow.md) — graduation criteria, the per-layer graduation document template, and the "write the contract after the refactor" rule that governs how this layer hypothesis evolves into per-layer contracts.
- [Codegen Layering Plan](codegen-layering-plan.md) — codegen-specific working hypothesis for the Ry semantic lowering vs LLVM IR emission split, the lowered IR vocabulary, and the pilot extraction target.
- [LLVM IR Emission Boundary](llvm-ir-emission-boundary.md) — identifies the candidate shared-library boundary inside the codegen layer.
- [Runtime ABI Boundary](runtime-abi-boundary.md) — categorizes the `__ry_*` `extern "C"` surface for Rust migration planning.
