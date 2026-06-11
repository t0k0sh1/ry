# Codegen Layering Plan

This document is the working hypothesis for splitting the codegen layer into two conceptual sub-layers — **Ry semantic lowering** and **LLVM IR emission**. It is the stage-3 design deliverable of v0.0.26 (issue #1824) and the design reference that #1949 (LLVM IR emission shared-library extraction) and #1950 (Rust reimplementation) will implement.

This is a **working hypothesis, not a graduation document**. The codegen layer is explicitly **not graduated** by this issue. The final responsibility / I/O / contract for each sub-layer is written only after the refactor in #1949 lands, per [Layer Graduation Workflow](layer-graduation-workflow.md) §"When to write the graduation document".

## Why split codegen at all

The `CodeGen` class (`include/ry/codegen.hpp`, ~2,400 lines) currently mixes two distinct responsibilities:

- **Ry semantic** state and decisions: type registries (`record_types_`, `enum_types_`, `type_aliases_`), ARC bookkeeping, ownership, stdlib dispatch, module-namespace state, type inference hints.
- **LLVM IR construction**: `IRBuilder<>`, `llvm::Module&`, primitive type accessors (`i64Ty_`, `ptrTy_`, …), block layout, PHI construction, intrinsic emission.

These are coupled through a single class. Replacing the LLVM-touching side with a different implementation language (#1950's Rust target) requires the surface that crosses the boundary to be free of LLVM-owned types, which the current single-class shape prevents. The split is the critical path to #1949 / #1950, which is the stated v0.0.26 milestone goal.

## Two conceptual sub-layers

### Ry semantic lowering

- **Owns**: Ry-level type understanding (Ry types, ownership/ARC intent, stdlib operation semantics, type inference state, qualified-import resolution).
- **Reads**: AST, sema results, module loader output, source manager.
- **Produces**: a **lowered IR** — an operation plan that names what should happen, not how to express it in LLVM. The vocabulary is defined below.
- **Does not call**: `IRBuilder<>::Create*`, `llvm::Module::getOrInsertFunction`, `llvm::Function::Create`. The lowering layer should not need to construct any `llvm::Value` directly.

### LLVM IR emission

- **Owns**: every `IRBuilder<>::Create*` call, basic-block construction, PHI nodes, intrinsic emission, runtime-symbol declaration.
- **Reads**: lowered IR ops from the semantic lowering layer.
- **Produces**: LLVM IR (the `llvm::Module` that the JIT or AOT path consumes).
- **Does not know**: Ry type aliases, ARC ownership rules, stdlib dispatch semantics, qualified-import state. If it needs to make a Ry-semantic decision, the lowering side did not produce the right op.

The boundary the two sides share is the lowered IR vocabulary in §"Lowered IR vocabulary" plus the `extern "C"` boundary in [LLVM IR Emission Boundary](llvm-ir-emission-boundary.md) (which already documents the five access categories the extraction will narrow through).

## Lowered IR vocabulary

The vocabulary is kept small so the LLVM IR emission layer can map each op to a near-1:1 sequence of LLVM API calls — required by issue #1824 AC. "Near-1:1" here means each op expands via a fixed playbook keyed on op kind plus its parameters — not one LLVM call per op. The emission side must not need to introspect Ry-level semantics to decide what playbook to run; if it does, the lowering side did not produce the right op. The working set is 10 ops; the final shape may shrink during the pilot.

| Op | Lowering input (Ry intent) | Emission output (LLVM API sequence) | Current implementation site |
|---|---|---|---|
| `RuntimeCall` | A Ry-level call resolved to a `__ry_*` runtime symbol with a structured signature and return-wrapping policy. | `mod_->getOrInsertFunction("__ry_…", funcType)` + `CreateCall`. | Scattered across `codegen_call_*.cpp` (no central helper today; see `codegen_any.cpp:199,362,912`, `codegen_call_user.cpp:392-767`). |
| `BoundsCheck` | An index expression, a length expression, and a structured error spec (error kind + source position). | `CreateICmpSLT` + `CreateICmpSGE` + `CreateCondBr` + `emitBoundsError` (exit IR). | Extracted in #1961: `lowering::lowerBoundsCheck` in `codegen_lowering_bounds_check.cpp` (constant fold + classification), `emission::emitBoundsCheck` in `codegen_emission_bounds_check.cpp` (LLVM IR). Op type in `include/ry/codegen/lowered_bounds_check.hpp`. |
| `ResultWrap` / `ResultUnwrap` | Wrap a value or runtime-error query as `Result<T, Error>`, or unwrap on a known path. | `CreateInsertValue` for the `Result` struct, or BB split + PHI for unwrap. | Already centralized: `emitResultBranch`, `wrapPtrAsResult`, `wrapStatusAsResult`, `buildErrorFromRuntime` at `codegen_call_dispatch.cpp:430-481`. |
| `OptionWrap` | Wrap a value as `Some` / `None`. | `CreateInsertValue` for the `Option` struct. | Extracted in #1967: `lowering::lowerOptionWrap` in `codegen_lowering_option_wrap.cpp` (trivial pack), `emission::emitOptionWrap` in `codegen_emission_option_wrap.cpp` (LLVM IR via `ry_emit_option_wrap_some` / `ry_emit_option_wrap_none`). Op type in `include/ry/codegen/lowered_option_wrap.hpp`. `CodeGen::buildSomeValue` / `buildNoneValue` survive as thin shims that retain `propagateMeta` + `tryRetainArcSource` side effects. |
| `AnyWrap` / `AnyUnwrap` | Decide the runtime type tag from value metadata and wrap; or unwrap to a concrete type with a tag check. | `CreateInsertValue` / `CreateExtractValue` for the `{i64 tag, [8 x i8] data}` struct + runtime helper calls. | Extracted in #1972: `lowering::lowerAnyWrap` / `lowerAnyUnwrap` / `lowerAnyTryUnwrap` in `codegen_lowering_any.cpp` (passthrough), `emission::emitAnyWrap` / `emitAnyUnwrap` / `emitAnyTryUnwrap` in `codegen_emission_any.cpp` (LLVM IR via `ry_emit_any_wrap` / `ry_emit_any_unwrap` / `ry_emit_any_try_unwrap` emission entries). Op types in `include/ry/codegen/lowered_any.hpp`. `wrapInAny` / `unwrapFromAny` / `tryUnwrapFromAny` in `codegen_any.cpp` survive as thin shims that retain type-name resolution / descriptor lookup / layout-type construction / tag computation / typed-collection registration / field-wise ARC retain / generic-substitution rejection / sub-helper dispatch (`unwrapEnumFromAny` / `tryUnwrapRecordFromAny` / `tryUnwrapListFromAny` / `tryUnwrapMapFromAny` / `tryUnwrapOptionFromAny`) / post-unwrap field-wise retain. |
| `ArcRetain` / `ArcRelease` | Increment / decrement the ARC count of an owned value. | `CreateAtomicRMW` + load/store of the header word. | `emitArcRetain` / `emitArcRelease` in `codegen_arc.cpp`. |
| `CowEnsureUnique` | Ensure a shared collection slot is private before mutation. | Strong-count atomic load + `CreateCondBr` to a clone path that does malloc + memcpy. | Extracted in #1970: `lowering::lowerCowEnsureUnique` in `codegen_lowering_cow.cpp` (DataLayout-driven element-size resolution + ARC metadata collection), `emission::emitCowEnsureUnique` in `codegen_emission_cow.cpp` (LLVM IR via `ry_emit_cow_ensure_unique`). Op type in `include/ry/codegen/lowered_cow.hpp`. `CodeGen::emitCowCheckSlot` survives as a thin shim collecting per-kind sizes and ARC flags; `emitCowCheck` retains only the `arc_backed_vars_` guard. |
| `CollectionMutate` | A list/map/set mutation request (`append`, `insert`, `removeAt`, `slice`, …) with element-type metadata. | malloc/memcpy chains, header GEP/load/store, ARC retain calls for owned elements. | Extracted in #1971 (list append / insert / removeAt / slice): `lowering::lowerCollection{Append,Insert,RemoveAt}` / `lowerListSlice` in `codegen_lowering_collection_mutate.cpp` (passthrough), `emission::emitCollection{Append,Insert,RemoveAt}` / `emitListSlice` in `codegen_emission_collection_mutate.cpp` (LLVM IR via `ry_emit_collection_append` / `_insert` / `_remove_at` / `_list_slice` emission entries). Op types in `include/ry/codegen/lowered_collection_mutate.hpp`. `emitCollOp_append` / `emitCollOp_insert` / `emitCollOp_remove_at` / `emitListSlice` in `codegen_call_collection.cpp` survive as thin shims that retain `emitCowCheck` / coercion / ARC-retain-decision / `emitArcAllocCollectionHeader` / `setTypeMeta` / `propagateMeta` side effects. Map / Set mutations and `take_back!` / `take_front!` remain pending. |
| `ControlFlow` | A structured branch / loop / match-arm decision. | Basic block creation, `CreateCondBr` / `CreateBr`, PHI assembly. | Extracted in #1973: `ry_emit_create_basic_block` / `ry_emit_branch_cond` / `ry_emit_branch_uncond` / `ry_emit_create_phi` emission entries + CodeGen-side `createBB` / `createBBInFn` / `emitBranchCond` / `emitBranchUncond` / `createPhi` wrappers. No `lowered_control_flow.hpp` struct layer is introduced (primitive CF is "passthrough" per §"Explicit non-inclusion" — the lowering side has no semantic decision). Sweep covers every `src/codegen_*.cpp` TU, not just the originally enumerated 4. |

**Explicit non-inclusion** (the surface is intentionally not extended to keep emission near-1:1):

- **Low-level primitive arithmetic** (`i32 + i32` → `CreateAdd`, `f64 * f64` → `CreateFMul`, integer comparisons, bit ops). These are **passthrough**: the lowering layer forwards them as a thin "primitive op" without inventing a vocabulary entry. Adding a `PrimitiveOp` op would inflate the vocabulary without giving the lowering layer anything to decide.
- **Lexical scope and SSA bookkeeping**. These belong inside the emission layer; the lowering layer treats variable bindings as Ry-level names and lets emission map them to allocas / `Value*`.
- **Module-level symbol declarations** (`@native` symbol registration, global variable emission). The lowering layer states "this call resolves to symbol X with signature S"; the emission layer is responsible for the `getOrInsertFunction` / `GlobalVariable` plumbing.

If a candidate op cannot be expressed as a 1:1 sequence of LLVM API calls on the emission side, it belongs in the lowering layer, not the vocabulary. The vocabulary's purpose is to give emission a stable, narrow surface; if it grows past ~10 ops or any op expands to a deep branching IR construction, the split is leaking.

**Decision (#2072, 2026-06-11) — [C] = (ii) boundary move supersedes the primitive carve-out above for the migration *direction*.** This document is a "working hypothesis, not a graduation document" (see the top), and issue #2072 was chartered to record the until-then-unrecorded direction. The (i) rationale above — keep primitive arithmetic / integer comparison / bit ops / Alloca-Load-Store as a C++ carve-out, vocabulary ≤ ~10 ops, to keep emission near-1:1 — is the correct reasoning for a **semantic** boundary and is **retained** as the record of why the hypothesis existed. #2072 chooses the alternative **physical** boundary: the emission layer owns *every* `IRBuilder<>::Create*`, including the semantically-trivial primitives this section reserved for C++. The motivation is complete C++/LLVM separation (the #2023 Rust-native end-state) over a near-1:1 vocabulary; the accepted cost is a larger, non-semantic op surface (`ry_emit_alloca` / `load` / `store` / `gep` / `icmp` / `and` / `or` / `add` / `sub` / `select` / `const_int`, added to `include/ry/llvm_emit/api.h`) plus a per-instruction FFI + intern/resolve hop. The "~10 ops or the split is leaking" line is a *semantic*-boundary metric and does not bound the (ii) primitive surface.

Scope of the decision: #2072 is **installment #1** — it establishes the primitive vocabulary and the bit-exact migration discipline on the five string byte-ops (`toUpper` / `toLower` / `trim` / `trimStart` / `trimEnd`) only. It does **not** achieve complete separation: those ops still thread `llvm::Value*` through CodeGen-side wrappers, orchestrate their own loops, call `builder_.SetInsertPoint` (a builder-state op, not a `Create*`), and obtain `len` from the shared `emitStringByteLen` (which still emits `Create*`). Complete separation arrives only once the whole codebase is swept and the intermediate `Value*` handles become `RyValueId` throughout; the full sweep and the migration of shared helpers (`emitStringByteLen`, etc.) are future work. Verification discipline established by the pilot: capture baseline IR on unmodified `main`, grep each op's markers to confirm the path actually emits, migrate, then require an ASLR-normalized before/after `--emit-llvm-ir` diff to be empty (byte-for-byte SSA-name preservation, per the `#2026` rule in `.claude/rules/codegen-llvm-ir-conventions.md`).

## Relationship to `llvm-ir-emission-boundary.md`

The two documents are **complementary**, not overlapping:

- `llvm-ir-emission-boundary.md` defines the **`extern "C"` boundary** that the shared library (#1949) and the Rust reimplementation (#1950) will expose. It classifies the existing custom-emitter access patterns into five categories and identifies category 3 (runtime wrapper helpers, already centralized in `codegen_call_dispatch.cpp:430-481`) as the most-narrowed candidate for the first extraction step.
- This document (`codegen-layering-plan.md`) defines the **conceptual responsibility split** (lowering vs emission) and the **lowered IR vocabulary** that the split is built around.

The emission-boundary document is the implementation surface; this document is the conceptual split that gives the boundary its shape. The two are co-maintained: changes to the vocabulary here imply changes to which categories cross the boundary there.

The AC item "LLVM IR emission layer's `extern "C"` boundary is sketched and shown to be free of LLVM-owned types" is satisfied jointly by these two documents — the emission boundary and the LLVM-type-exclusion constraint live in `llvm-ir-emission-boundary.md`, and this document specifies what travels across it.

## Pilot area: bounds-check intent

The pilot for the lowering / emission split is the **`BoundsCheck`** op.

### Why bounds-check is the pilot

- **Smallest demonstrator**. `emitBoundsCheck` is ~35 lines with 6 call sites (`codegen_stmt_misc.cpp:428,1082,1320`, `codegen_expr_literal.cpp:927,1145`, `codegen_arc_cow.cpp:433`). The full extraction can be reviewed in a single small PR.
- **Clear lowering / emission boundary**. The lowering side computes `BoundsCheck { idx, len, error_spec }` from the index expression, the collection length, and a position-tagged error specification. The emission side is fixed shape: `CreateICmpSLT` + `CreateICmpSGE` + `CreateCondBr` + an exit-IR call to `emitBoundsError`. Nothing on the emission side requires Ry semantic knowledge.
- **Lowest risk**. Behavior is bit-exact preserved (bounds-check IR is well-tested by `tests/spec/`), the runtime error wiring is already centralized via `emitRuntimeError` / exit IR, and the change is observable in a narrow test surface.
- **Demonstrator value**. Validates that the lowered-IR ↔ LLVM-emission shape works without committing to the wider extraction.

### What the pilot validates

- The lowered IR can carry a structured error spec without leaking `Diagnostic` types into the emission side.
- The emission side accepts the op via an `EmitterContext`-shaped interface (the narrowing direction documented in `llvm-ir-emission-boundary.md` §"Narrowing direction").
- The call-site rewrite at the 6 callers is mechanical — i.e. the split does not require redesigning callers.

### Post-extraction op shape (recorded after #1961)

The pilot landed as #1961. The actual `BoundsCheck` op shape, in `include/ry/codegen/lowered_bounds_check.hpp`:

```cpp
namespace ry::codegen::lowered {

enum class BoundsKind { List, Array };

struct BoundsCheckErrorSpec {
    BoundsKind kind;          // the only two error-message variants
                              // observed across the 6 call sites
    std::string global_name;  // cachedGlobalString dedup key
                              // (e.g. ".idx_assign_err")
    // SourceLocation loc;    // reserved; emitBoundsError does not yet
                              // consume position metadata
};

struct BoundsCheckOp {
    llvm::Value *idx;
    llvm::Value *len;
    BoundsCheckErrorSpec error_spec;
};

} // namespace ry::codegen::lowered
```

Notes on what was kept vs. cut against the original vocabulary intent (§"Lowered IR vocabulary"):

- **Error format string → enum**: the 6 call sites use exactly two `fprintf` format strings (list vs. array). Carrying a free-form format string in the op was rejected; `BoundsKind` is sufficient. The emission helper reconstructs the format string from `kind`.
- **`SourceLocation` reserved as a comment**: the doc text named "source position" as part of the error spec, but the current `emitBoundsError` → `emitRuntimeError` chain emits `fprintf(stderr, ...) + exit(1)` without consuming a position. The field is left as a comment placeholder so a future PR that threads position into the runtime error channel does not have to renegotiate the op shape.
- **`bb_prefix` stays outside the op**: the LLVM block-label hint (e.g. `"idx_assign"`, `"pcow_list"`) is an emission concern only and was not promoted into the op. The emission helper takes it as a separate parameter.
- **Constant fold stays in lowering**: the compile-time `codegenError` path uses a Ry-semantic diagnostic and so cannot live on the emission side. Lowering returns `std::nullopt` for the constant-fold path; the helper writes the folded constant back into the caller's `Value *&idx` in place. The runtime path returns a `BoundsCheckOp` carrying the un-wrapped index; emission performs `emitNegativeIndexWrap` itself so that lowering contains no `IRBuilder<>::Create*` call.
- **`emitNegativeIndexWrap` and `emitBoundsError` remain `CodeGen` methods**: the former is shared with `slice` / substring / range-index; the latter is a one-line shim over `emitRuntimeError`. Both are reachable from the emission helper as `cg.emitNegativeIndexWrap(...)` / `cg.emitBoundsError(...)`.

A separate graduation document for the lowering / emission sub-layers is **not** written at this step; that comes only after step 2 per §"When the codegen layers earn their graduation document". Writing per-layer graduation docs at the pilot stage would be exactly the aspirational anti-pattern the workflow exists to prevent.

### Alternatives considered (not selected)

- **`Result` / `Option` construction**. Helpers (`emitResultBranch`, `wrapPtrAsResult`, …) are already consolidated in `codegen_call_dispatch.cpp:430-481`, making the **extraction** the lowest risk. However, the lowering side has very little to compute (the wrap intent is already determined upstream by the time these helpers are called), so the demonstrator value of the lowering / emission split is weaker than bounds-check.
- **`any` wrap/unwrap**. `codegen_any.cpp` is ~2,116 lines with deep coupling to type metadata. The split has good semantic clarity (tag determination is lowering; struct construction is emission), but the size is too large for a pilot. This is a strong candidate for a wider extraction once the bounds-check pilot validates the shape.
- **Runtime call specs**. `mod_->getOrInsertFunction("__ry_…")` is scattered across `codegen_call_*.cpp` with no central helper. A `RuntimeCall` op consolidation would be high-value but high-scope; not suitable as a first pilot.
- **`typename` utilities**. Already addressed by #1821 (pure-utility extraction); not a codegen-split pilot.

## Staged migration plan

The codegen split proceeds in stages. Each stage has a stop condition; the next stage does not start until the current stage's stop condition is met.

| Step | Scope | Stop condition | Tracking |
|---|---|---|---|
| **0** | Workflow + plan + pilot area selected. | This document and `layer-graduation-workflow.md` merged. | This issue (#1824). |
| **1** | Pilot extraction of `BoundsCheck` into lowering + emission split. Minimal `LoweredOp::BoundsCheck` plumbing; the 6 callers updated to lower-then-emit. No other ops touched. | The 6 call-site updates land; existing tests pass; the lowering / emission separation is observable in the file layout (header + cpp boundary). | Follow-up issue (TBD; see issue #1824 §"Proposed Deliverables" follow-up implementation issues). |
| **2-A** | Scaffolding under #1949: stand up `emit` as a `SHARED` CMake target, sketch the `extern "C"` boundary in `include/ry/llvm_emit/api.h` (opaque `RyValueId` handles, `RyEmitCtx`, `RyEmitCallbacks` for the unmigrated helpers), and migrate three category-3 helpers (`getRuntimeFn`, `buildErrorFromRuntime`, `bounds_check`). Existing call sites unchanged — the helpers are shimmed by routing their bodies through the boundary. | The shared library builds and links; three category-3 helpers cross the boundary; BoundsCheck pilot's emission side dispatches through the boundary; existing C++ + Ry tests pass. | #1949 (landed). |
| **2-B** | Migrate the remaining category-3 helpers (`wrapPtrAsResult`, `wrapStatusAsResult`, `emitResultBranch`) together with the `ResultBranch` lowered op. Replace `emitNegativeIndexWrap` / `emitBoundsError` callbacks with proper boundary functions; `RyEmitCallbacks` is removed entirely. `CodeGen::emitResultBranch` survives only as a thin shim that bridges `llvm::function_ref<>`-style call sites to the C-fnptr boundary via a trampoline. `getResultType`'s `StructType` cache stays on the CodeGen side (its reverse map is consumed by ARC and Any wrapping); `resTy` crosses as `void*`, mirroring `errorTy_`. | All five category-3 helpers cross the boundary; BoundsCheck has no remaining `CodeGen` callbacks; `RyEmitCallbacks` is gone. | #1964 (landed). |
| **2-C** | Successively migrate the remaining lowered IR ops (`RuntimeCall`, `AnyWrap` / `Unwrap`, `CollectionMutate`, `CowEnsureUnique`, `ControlFlow`). Cross category 1 (LLVM context handles) and category 2 (primitive type accessors) so the transitional `void*` parameters become typed opaque handles. **Landed**: `OptionWrap` via #1967; `ArcRetain` / `ArcRelease` via #1968; `RuntimeCall` via #1969; `CowEnsureUnique` via #1970; `CollectionMutate` via #1971; `AnyWrap` / `AnyUnwrap` / `AnyTryUnwrap` via #1972; `ControlFlow` + category 1/2 opaque handles + 22-entry sweep via #1973 (final Stage 2-C PR; api.h now exposes only opaque pointer typedefs `RyValueRef` / `RyTypeRef` / `RyModuleHandle` / `RyBuilderHandle` / `RyContextHandle` / `RyFunctionHandle` / `RyFuncTypeRef` / `RyBasicBlockRef`, no `llvm::*` or transitional `void *` survives except the documented `void *user_ctx` carve-out in `RyBuildValueFn` / `ry_emit_result_branch`; CodeGen-side cast helpers in `include/ry/llvm_emit/cast_helpers.hpp` bridge per-site without scattering `reinterpret_cast`; header-level lint at `scripts/check-llvm-emit-abi-header.sh` is wired into the CI `lint` job to enforce the AC). | The codegen call sites no longer call `IRBuilder<>::Create{CondBr,Br,PHI}` / `BasicBlock::Create` directly within the ControlFlow / 10-op vocabulary. `IRBuilder<>::Create*` calls remain for the primitive arithmetic / Alloca/Store/Load / module-level symbol carve-outs per §"Explicit non-inclusion". | #1965 (Stage 2-C umbrella, closed by #1973); #1967 (OptionWrap); #1968 (ARC); #1969 (RuntimeCall); #1970 (CowEnsureUnique); #1971 (CollectionMutate); #1972 (AnyWrap/Unwrap); #1973 (ControlFlow + category 1/2 + header lint). |
| **3** | Reimplement the shared library in Rust behind the same boundary. | All existing tests pass with the Rust implementation; the C++ implementation is removed (or feature-flagged for the brief transition). | #1950 (scaffolding); #1993 (cutover, landed). |

Each stage produces evidence that the next stage can start. A graduation document for the codegen sub-layers (lowering / emission) is **not** written at step 1 — the lowering and emission sides have not stabilized yet. The graduation document is written at the end of step 2, after the shared-library shape settles.

## SRP and file-size goals

The pilot (step 1) and the wider extraction (step 2) follow the [Layer Graduation Workflow](layer-graduation-workflow.md) §"SRP and file-size policy":

- Newly created files in step 1 stay at or under 500 lines.
- Once the lowering and emission sides graduate (end of step 2), target 200–300 lines per file where it improves navigability.
- No line-count-only splits. Splitting the current `codegen_call_*.cpp` files by filename prefix without a responsibility split is explicitly forbidden by #1819's milestone policy and by the workflow doc.

## When the codegen layers earn their graduation document

The two sub-layers (`codegen-semantic-lowering-graduation.md` and `codegen-llvm-emission-graduation.md`) are written after **all** of the following hold:

1. Step 2 (#1949) has landed — the shared library is built and the codegen call sites dispatch through the boundary. Step 3 (#1950 scaffolding, #1993 cutover) has also landed — the shared library is now implemented in Rust (`crates/emit/`) and the C++ implementation is removed.
2. The lowered IR vocabulary has converged (the working list above may shrink during the pilot; the final list is recorded in the graduation doc, not predicted here).
3. Existing tests pass against the post-extraction shape — behavior is preserved end-to-end.
4. The emission layer's `extern "C"` boundary carries no LLVM-owned types (enforced by a header-level lint of `api.h`).

Until then, the lowering / emission sides have a working hypothesis (this document) but are explicitly **not graduated**.

## Related documents

- [Layer Graduation Workflow](layer-graduation-workflow.md) — the workflow this plan operates inside; defines graduation criteria, the document template, and the "write the doc after the refactor" rule.
- [Compiler Layers](compiler-layers.md) — the lightweight layer-ordering hypothesis the codegen row of which this plan refines.
- [Codegen Terminology](codegen-terminology.md) — canonical vocabulary (codegen / lowering / lowered IR / emission; the runtime / emission boundaries; handle naming).
- [LLVM IR Emission Boundary](llvm-ir-emission-boundary.md) — the `extern "C"` boundary that the wider extraction (step 2 / #1949) will implement.
- [Runtime Boundary](runtime-abi-boundary.md) — the orthogonal `__ry_*` boundary; codegen's `RuntimeCall` op routes through it, so the lowered IR vocabulary and the runtime boundary categorization stay aligned.
