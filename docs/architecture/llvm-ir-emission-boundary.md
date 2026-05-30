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

## Stage 2-A landed (#1949 scaffolding)

The first #1949 PR is intentionally scaffolding-only — it stands up the shared-library boundary without migrating every category-3 helper:

- **Shared library target**: `ry_llvm_emit` is built as `add_library(SHARED ...)` in the root `CMakeLists.txt`. It produces `lib/libry_llvm_emit.{dylib,so}`; LLVM is not linked into it (its symbols resolve from the main process via `-undefined dynamic_lookup` on macOS and `-rdynamic` on Linux, mirroring the `add_ry_native_lib()` pattern). Unlike the JIT-loaded stdlib native libs the `ry` / `ry_tests` executables and the `add_ry_fuzz_target` harnesses link it explicitly because `ry_lib`'s shim layer references its symbols at compile time.
- **ABI header**: `include/ry/llvm_emit/api.h`. Defines `RyValueId` / `RyFuncId` / `RyTypeId` as `uint32_t` opaque handles, `RyEmitCtx` as an opaque struct, and a `RyEmitCallbacks` slot for the helpers that have not crossed the ABI yet. The header is the LLVM-type-exclusion contract: no `llvm::Value*` / `llvm::Module&` / `llvm::IRBuilder<>` / `llvm::Function*` / `llvm::BasicBlock*` / `llvm::Type*` appears in any public signature. Module / IRBuilder / context / function pointers are passed as transitional `void*` until categories 1 and 2 cross the ABI in a successor PR.
- **Helpers crossing the ABI in 2-A**: three entry points — two of the five category-3 helpers plus the BoundsCheck pilot.
  - `ry_emit_get_runtime_fn` — backs `CodeGen::getRuntimeFn` (category 3).
  - `ry_emit_build_error_from_runtime` — backs `CodeGen::buildErrorFromRuntime` (category 3). Takes `errorTy_` as a transitional `void*` parameter to preserve the named-`StructType` pointer identity that `isResultType` and friends rely on.
  - `ry_emit_bounds_check` — backs the BoundsCheck pilot from #1961. Not a category-3 helper; it is the first lowered-op surface that crosses the ABI. Calls into `CodeGen::emitNegativeIndexWrap` and `CodeGen::emitBoundsError` through the `RyEmitCallbacks` slot.
- **Category-3 helpers deferred** (still `CodeGen` methods, untouched call sites): `wrapPtrAsResult`, `wrapStatusAsResult`, and `emitResultBranch` — the remaining three of five. Their bodies pull in `getResultType` (a pointer-identity-sensitive `result_types_` cache) and `buildOkValue` / `buildErrValue` (which call `propagateMeta` + `tryRetainArcSource`). Migrating them via callbacks would widen the ABI surface beyond the scaffolding scope; they migrate together with the `ResultWrap` / `ResultUnwrap` lowered op in the same successor PR.
- **BoundsCheck shape**: the lowered op type in `include/ry/codegen/lowered_bounds_check.hpp` still holds `llvm::Value*` members, per the two-stage boundary policy — the lowering↔emission split is an internal C++ boundary where LLVM types are allowed; only the ABI surface is opaque. `src/codegen_emission_bounds_check.cpp` interns the op's `idx` / `len` into handles immediately before each ABI call.
- **EmitCtx lifecycle**: one ctx is created in `CodeGen::CodeGen` and destroyed in `CodeGen::~CodeGen`. `ry_emit_ctx_set_function` is called from `emitBoundsCheck` to sync the current `llvm::Function*` before the ABI creates new basic blocks. The handle table is a `std::vector<llvm::Value*>` indexed by `RyValueId`; handle 0 is the sentinel for "invalid / null".

Subsequent PRs (still under issue #1949 unless re-scoped) will: (1) migrate `wrapPtrAsResult` / `wrapStatusAsResult` / `emitResultBranch` together with `ResultWrap` op support, (2) replace the `emitNegativeIndexWrap` / `emitBoundsError` callbacks with proper ABI functions, (3) start crossing category 1 / 2 (LLVM context handles and primitive type accessors) so the transitional `void*` parameters can become typed opaque handles. The codegen-layering graduation document remains unwritten until those steps complete and the boundary is observable end-to-end.

## Related documents

- [Compiler Layers](compiler-layers.md) — layer ordering and dependency direction.
- [Runtime ABI Boundary](runtime-abi-boundary.md) — the orthogonal `__ry_*` boundary on the runtime side.
