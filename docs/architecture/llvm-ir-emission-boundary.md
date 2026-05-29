# LLVM IR Emission Boundary

This document identifies the candidate shared-library boundary inside the codegen layer. It is the design reference for issue #1949 (extract the LLVM IR emission layer as a separate shared library) and #1950 (reimplement that shared library in Rust). Issue #1820 (v0.0.26 stage 2) prepares the surface by extracting layer-independent helpers and documenting the access patterns described here.

## What this boundary is

The "LLVM IR emission layer" is the subset of `CodeGen` that owns LLVM types and the `IRBuilder<>`. Today this is intertwined with type registries, ARC bookkeeping, stdlib dispatch, and module-namespace state inside a single monolithic class (`include/ry/codegen.hpp`). The goal of #1949 is to extract the LLVM-touching surface into its own `.so`/`.dylib` exposing a stable `extern "C"` ABI, so that #1950 can reimplement it in Rust without rewriting the rest of the compiler.

## Constraint: LLVM types must not cross the boundary

In the final design, the following LLVM-owned types **must not appear** in the shared-library ABI:

- `llvm::Value*` / `llvm::Constant*` / any `llvm::*` derivative
- `llvm::Module&` and `llvm::LLVMContext&`
- `llvm::IRBuilder<>` and its template instantiations
- `llvm::Function*` / `llvm::BasicBlock*` / `llvm::Type*`

These types are LLVM-version-coupled and cannot be reasonably exposed through a stable ABI. The boundary instead exchanges opaque handles (or value IDs) that the LLVM side resolves internally. Concretely: anywhere the current C++ code passes an `llvm::Value*` between a custom stdlib emitter and a `CodeGen` helper, that exchange must be redesigned to use a plain integer handle, a small POD struct, or a callback that lives entirely on the LLVM side.

## Access categories observed in custom stdlib emitters

The 16 `src/codegen_call_*.cpp` dispatcher files are the primary consumers of `CodeGen` internals. Their access patterns fall into five categories:

1. **LLVM context handles**: `builder_`, `mod_`, `ctx_`, `fn_`. These are the raw LLVM state — they cannot cross the boundary as-is. Custom emitters that take a `CodeGen&` and read these directly must be reshaped to receive an `EmitterContext` wrapper instead.
2. **Primitive type accessors**: `i64Ty_`, `ptrTy_`, `anyTy_`, `errorTy_`, `i8Ty_`, `f64Ty_`, etc. These are `llvm::Type*` and share the same constraint as category 1 — wrap behind opaque type-id handles in the boundary.
3. **Runtime wrapper helpers**: `getRuntimeFn`, `wrapPtrAsResult`, `wrapStatusAsResult`, `emitResultBranch`, `buildErrorFromRuntime`. **This category is the most narrowed existing boundary candidate** — all five helpers are already centralized in `src/codegen_call_dispatch.cpp`, and they encapsulate the canonical "call a runtime symbol, wrap the result as `Result<T, Error>`" pattern. They are the natural starting point for the shared-library extraction in #1949: a thin wrapper layer that exchanges opaque value handles and dispatches to the LLVM-owning side.
4. **Metadata API**: `propagateTypeMeta`, `setTypeMeta`, `emitExpr`. These mutate or query the `ValueMetadata` keyed by `llvm::Value*`. Crossing the boundary requires the metadata store to live on the LLVM side and be addressed through the same opaque handles as category 1.
5. **Predicate and error helpers**: `isStringValue`, `isFile`, `codegenError`, `requireArgs`, `propagateTypeMeta`. These mix value inspection (LLVM-coupled) with diagnostic reporting (layer-independent). Diagnostics can move to the caller side; value-inspection predicates must stay LLVM-side.

## Narrowing direction (not implemented in #1820)

The concrete narrowing — replacing direct `CodeGen&` parameters in custom emitters with a smaller `EmitterContext` interface — is **not** in scope for issue #1820. The acceptance criterion is satisfied here by documentation: emitter access is currently broad, the narrowing plan is to introduce a context struct exposing only the surface above, and the implementation is left to a follow-up issue gated on #1949's ABI design.

The recommended sequence is:

1. (this issue, #1820) Document the surface; extract layer-independent string/type-name helpers out of `CodeGen` so they no longer cross the boundary.
2. (#1949) Define the shared-library ABI starting from category 3 (the most narrowed surface today) and progressively wrap the other categories. Extract the LLVM-owning side into `libry_codegen.so`/`.dylib`.
3. (#1950) Reimplement `libry_codegen` in Rust behind the same ABI.

## Related documents

- [Compiler Layers](compiler-layers.md) — layer ordering and dependency direction.
- [Runtime ABI Boundary](runtime-abi-boundary.md) — the orthogonal `__ry_*` boundary on the runtime side.
