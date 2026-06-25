# Upper Codegen Rust Migration

This document is the working hypothesis for migrating the **upper codegen layer** (Ry semantic lowering, currently C++ `src/codegen_*.cpp`) to Rust. It is the kickoff deliverable of issue #2397; the wider migration unfolds across subsequent issues.

This is a **working hypothesis, not a graduation document**. The upper codegen sub-layer is explicitly **not graduated** here. Per [Layer Graduation Workflow](layer-graduation-workflow.md) §"When to write the graduation document", the graduation document is written only after the Rust side stabilizes and the C++ shim is removed.

## What this migration is (and is not)

| Term | Today | Migration target |
|---|---|---|
| **emission** (lower codegen) | Rust `crates/emit/` behind `ry_emit_*` (since #1993; C++ shim removed in #2229). | unchanged |
| **lowering** (upper codegen) | C++ `src/codegen_*.cpp` — Ry semantic decisions (type registries, ARC bookkeeping, ownership, stdlib dispatch, metadata) — calls `ry_emit_*` to construct IR. | Rust crate(s) under `crates/lower*/` behind a new `ry_lower_*` boundary, called from a slimmer C++ caller. |

The two boundaries are orthogonal: lowering decides *what* should happen (the Ry semantic op); emission builds the LLVM IR for it. Migrating lowering does not change emission. See [Codegen Terminology](codegen-terminology.md) for the canonical vocabulary.

**Out of scope** for this migration as a whole:

- Rewriting emission. Emission is already Rust.
- Changing the lowered IR vocabulary in [Codegen Layering Plan](codegen-layering-plan.md) §"Lowered IR vocabulary".
- Touching the runtime ABI (`__ry_*`, [Runtime Boundary](runtime-abi-boundary.md)) or native call boundary ([Native Call Boundary](native-call-boundary.md)).
- Deleting the C++ implementation. Each lowering op migrates with a C++ shim that calls into Rust; the shim survives until the wider migration completes.

## What "upper codegen" includes

The lowering layer owns every decision in `src/codegen_*.cpp` that is *not* an `IRBuilder<>::Create*` call. The 39 `codegen_*.cpp` files split into the following concern groups:

| Group | Files (representative) | Lowering responsibilities |
|---|---|---|
| **Primitive expression literal & arithmetic** | `codegen_expr.cpp` (`emitExprVariant({Number,Float,Bool,String,Regex}Expr)`), `codegen_arith.cpp`, `codegen_expr_cast.cpp` | constant selection, suffix → type, sign decision, range validation, cast policy, overflow strategy |
| **Identifier & variable** | `codegen_expr.cpp` (`emitExprVariant(VariableExpr)`), parts of `codegen_stmt.cpp` | name → alloca / global lookup, ARC retain decision, type metadata propagation |
| **Type & metadata** | `codegen_type.cpp`, `codegen_metadata.cpp` | type name resolution, `ValueMetadata` propagation, `TypeMeta` tagging |
| **Statement & control flow** | `codegen_stmt.cpp`, `codegen_stmt_loop.cpp`, `codegen_stmt_misc.cpp`, `codegen_match.cpp` | block layout, loop scaffolding, match arm dispatch |
| **Call dispatch** | `codegen_call.cpp`, `codegen_call_*.cpp`, `codegen_native_call_descriptor.cpp` | native vs user vs builtin selection, descriptor lookup, signature checking, arg coercion |
| **Function & lambda** | `codegen_fn.cpp`, `codegen_fn_generic.cpp`, `codegen_lambda.cpp` | signature lowering, generic instantiation, closure capture analysis |
| **ARC, GC, CoW** | `codegen_arc.cpp`, `codegen_arc_gc.cpp`, `codegen_arc_cow.cpp` | retain/release placement, GC visitor synthesis, CoW slot bookkeeping |
| **Composite (Any, Result, Option, collections)** | `codegen_any.cpp`, `codegen_call_result.cpp`, `codegen_call_option.cpp`, `codegen_call_collection.cpp`, `codegen_call_set_ops.cpp`, `codegen_call_iterator.cpp` | tag selection, type-driven dispatch, element-type metadata |
| **Test / coverage / trace plumbing** | `codegen_test.cpp`, `codegen_metadata.cpp`, parts of `codegen.cpp` | test-mode injection, coverage instrumentation, trace symbol emission |

The lowering decisions inside each group call `ry_emit_*` (sometimes many times per op) to construct the IR. The migration replaces the *decision* code with Rust; the IR construction was already Rust.

## Migration order

The order favors **input surface** over LOC. Earlier rows touch less CodeGen state (type registries, `value_metadata_`, ARC caches), so they marshal across the new `ry_lower_*` boundary with the smallest opaque-handle surface. Later rows have to grow the boundary first.

| Stage | Scope | Why this order | Boundary additions |
|---|---|---|---|
| **0 — kickoff (this PR)** | `emitExprVariant(BoolExpr)`. | Smallest possible Ry-semantic decision: `bool → i1 constant`. No CodeGen state read; no metadata side-effects; no error path. Calls only the existing `ry_emit_const_int`. Validates the new `ry_lower_*` boundary shape and dual-cdylib link / symbol-resolution wiring. | One entry: `ry_lower_bool_const`. |
| **1 — primitive literals** | `emitExprVariant(NumberExpr)`, `FloatExpr`. | Adds suffix→type selection and range validation (the `validateIntRange` body). Requires a way to surface lowering errors back to C++ (`codegenError`-equivalent return channel). | `ry_lower_int_const`, `ry_lower_float_const`; one error-return helper. |
| **2 — string / regex literal & global string** | `emitExprVariant({String,Regex}Expr)`, the `cachedGlobalString` cache. | Introduces global-string interning state — first stateful cache to live on the Rust side. | `ry_lower_string_const`; the interning cache moves under Rust ownership. |
| **3 — identifier & variable read** | `emitExprVariant(VariableExpr)`. | First read of CodeGen state (`named_values_`, ARC source map, low-level type names). The name→storage table starts crossing the boundary as a lookup callback. | `ry_lower_var_read`; named-values lookup callback. |
| **4 — primitive arithmetic** | `codegen_arith.cpp` (`emitCheckedArithmetic`, `emitSaturatingArithmetic`), the constant-fold paths around them. | Combines decision (signed/unsigned × add/sub/mul × panic/Result/saturating) with the already-migrated `ry_emit_intrinsic_call`. Reuses the pilot D (#2102) overflow-intrinsic surface. | `ry_lower_arith_op` family; no new emission entries. |
| **5 — type & metadata** | `codegen_type.cpp` (`resolveType`, `isUnsignedLowLevelName`), `codegen_metadata.cpp`. | Pure functions over Ry type-name strings — no IR construction. Migrates as a self-contained Rust module callable from any later stage. | `ry_lower_resolve_type`, `ry_lower_propagate_meta`; the type registry moves to Rust. |
| **6 — control flow & match** | `codegen_stmt_loop.cpp`, `codegen_match.cpp`, `codegen_stmt_misc.cpp` (the `if` / `while` / `for` / `match` lowering). | Touches block layout and PHI scaffolding but the emission primitives are all in place (#1973 ControlFlow, #2098 function definition). | `ry_lower_if`, `ry_lower_loop`, `ry_lower_match_arm`. |
| **7 — composite (Any / Result / Option / collection)** | `codegen_any.cpp`, `codegen_call_{result,option,collection,iterator,set_ops}.cpp`. | Largest semantic-decision surface (tag selection, type-driven dispatch). Lands after the type-registry move (stage 5) so the descriptor-lookup table sits on the Rust side. | Composite-op lowering entries. |
| **8 — call dispatch** | `codegen_call*.cpp` (native call descriptor consumption), `codegen_fn.cpp` / `codegen_fn_generic.cpp`. | Consumes the descriptor table from [Native Call Boundary](native-call-boundary.md). Lands after composite (stage 7) so the wrapping helpers it needs are already Rust. | `ry_lower_call_*`. |
| **9 — ARC / GC / CoW** | `codegen_arc.cpp`, `codegen_arc_gc.cpp`, `codegen_arc_cow.cpp`. | The most CodeGen-state-heavy area (`arc_backed_vars_`, ownership tracking, GC visitor thunk synthesis). Lands last because its bookkeeping interacts with every prior stage. | `ry_lower_arc_*`. |

The order is a working hypothesis. Each stage is its own future issue, opened independently when it is taken on; this document does not pre-allocate issue numbers. If a stage's boundary surface explodes during implementation, the stage's scope is renegotiated in its own issue rather than expanded ad hoc.

## Pilot module: `BoolExpr`

The kickoff pilot is **`emitExprVariant(BoolExpr)`** (`src/codegen_expr.cpp:122-124`).

### Why `BoolExpr`

- **One-statement body**. `return llvm::ConstantInt::get(i1Ty_, e.value ? 1 : 0, false);`. The migrated path is observably small and the diff fits one PR alongside the boundary scaffolding.
- **No CodeGen state read**. Only inputs: the boolean `e.value` and the `i1Ty_` LLVM type (already a `RyTypeRef` candidate). No `value_metadata_`, no ARC caches, no `named_values_`, no error path.
- **No type registry crossing**. The `i1` type is a primitive; the lowering decision does not consult `record_types_` / `enum_types_` / `type_aliases_`. Stage 5's type-registry move is not a prerequisite.
- **Verifiable**. The emitted IR is a single LLVM `ConstantInt` (`i1 0` / `i1 1`) that is grep-confirmable in `--emit-llvm-ir` output before diffing.
- **Vacuous-diff trap avoidance**. A bare `let x = true; if x { … }` sema-folds the conditional, bypassing emission of the constant entirely. The pilot uses a probe (see §"Verification") that stores `e.value` into an `alloca` so the constant survives sema and is observable as `store i1 true` / `store i1 false`.

### Why not the alternatives the issue named

- **Numeric literal (`NumberExpr` / `FloatExpr`)**: introduces suffix → type selection, range validation, and the `codegenError` error path. Each is solvable but inflates the kickoff PR beyond a single op. Belongs at stage 1.
- **Identifier resolution (`VariableExpr`)**: reads `named_values_` (CodeGen state). The name→storage lookup is a callback shape that has to be designed; doing this for the pilot lets the design solidify on a less-clean surface. Belongs at stage 3.
- **String literal**: needs the `cachedGlobalString` interning cache to either cross or migrate. Stateful. Belongs at stage 2.

## Boundary design

The pilot adds **one** new `extern "C"` entry to a new header `include/ry/lower/api.h`:

```c
RyValueId ry_lower_bool_const(RyEmitCtx *ctx, RyTypeRef i1_ty, int value);
```

- Inputs: the emit context (existing handle), the `i1` LLVM type (existing `RyTypeRef`), the literal value (0 / 1).
- Output: an interned `RyValueId` (existing handle scheme).
- No new opaque types. No new error channel — `BoolExpr` cannot fail.
- The Rust implementation in `crates/lower/` calls the existing `ry_emit_const_int(ctx, i1_ty, value as u64, 0)` and returns the interned result.

The C++ shim `CodeGen::emitExprVariant(BoolExpr)` becomes:

```cpp
return cast_helpers::asValue(ry_emit_resolve(
    emit_ctx_.get(),
    ry_lower_bool_const(emit_ctx_.get(),
                        cast_helpers::toRyTypeRef(i1Ty_),
                        e.value ? 1 : 0)));
```

The shim survives the pilot — its only job is to feed `i1Ty_` (a CodeGen-owned `llvm::Type *`) across the boundary as `RyTypeRef`. When later stages migrate the primitive-type registry, the C++ side stops holding `i1Ty_` and the shim's last responsibility disappears.

## Lower crate shape

`crates/lower/` is a Rust `cdylib`, parallel to `crates/emit/` but **without** an `llvm-sys` dependency. The lower crate calls `ry_emit_*` declared as `extern "C"` and resolved at runtime from the loaded `emit` cdylib (`-undefined dynamic_lookup` on macOS; `-rdynamic` on the host `ry` / `ry_tests` executables on Linux — the same pattern `crates/native_base64` uses for `__ry_*`).

The crate's only opaque-type declarations duplicate the api.h handle types (`RyEmitCtx`, `RyTypeRef`, `RyValueId`) as zero-sized `repr(C)` structs / pointer aliases. They are pointer-compatible with the C++ side; no layout assertion is needed because no field is read from Rust.

The boundary discipline mirrors emit's `abi → composite → primitive → context` layering ([Codegen Layering Plan](codegen-layering-plan.md) §"Composite and primitive emission sub-layers"):

- `crates/lower/src/abi.rs` — the `ry_lower_*` `extern "C"` shell. Pure mechanical translation.
- `crates/lower/src/lib.rs` — the (currently single-file) Ry-semantic decision body.

Future stages add `crates/lower/src/{expr,stmt,call,arc,…}.rs` as the surface grows.

## Verification

### Discipline

The pilot follows the #2026 ASLR-normalized `--emit-llvm-ir` discipline ([Codegen Layering Plan](codegen-layering-plan.md), referenced from each #2072 onward installment):

1. **Baseline.** Capture `--emit-llvm-ir` output for a probe program on the pre-migration tree.
2. **Marker grep.** Confirm `store i1 1` and `store i1 0` appear in the baseline (so a vacuous diff cannot pass — see "Vacuous-diff trap" below).
3. **Migrate.** Apply the pilot diff.
4. **Compare.** Re-capture `--emit-llvm-ir` and diff against the baseline. The diff must be empty after ASLR / SSA-name normalization.

### Vacuous-diff trap

A `bool` literal that flows only into an `if` condition is sema-folded and never reaches `emitExprVariant(BoolExpr)`. The probe stores `true` and `false` into named locals so the constants outlive sema and appear in IR:

```ry
fn boolPilotProbe() -> int {
    let t = true
    let f = false
    if t { return 1 } else if f { return 2 } else { return 3 }
}
```

The `let t = true` and `let f = false` initializations emit `store i1 true, ptr %t` / `store i1 false, ptr %f` — these are the markers grep-confirmed in step 2.

### Coverage in both build presets

Both `default` (Linux / CI, `LLVM_DIR=/usr/local/llvm`) and `rust-emit` (macOS, `LLVM_DIR=/opt/homebrew/opt/llvm@21`) presets exercise the same Rust `emit` crate (since #1993; both link the same cdylib via corrosion). The pilot adds the same `lower` cdylib to both presets via the same CMake `corrosion_import_crate` mechanism, so cross-preset IR consistency is structural, not a separately enforced invariant. The byte-exact regression is verified on **each preset independently**:

- **macOS `rust-emit` (local)**: the one-shot ASLR-normalized `--emit-llvm-ir` diff in step 4 proves the migration is byte-equivalent at the cutover. This is the migration-path proof — it certifies the C++ and Rust paths agree at the moment they switch.
- **Linux `default` (CI)**: the FileCheck golden `tests/filecheck/bool_const_pilot_2397.ry` runs on every PR via the `filecheck` job and asserts the post-migration IR shape continuously. Any platform-specific loader / LLVM-output drift that would break the migrated path on `default` breaks the golden first.

Together the two preset paths cover the cross-preset proof scope item 3 calls for; neither alone is sufficient (one-shot diff proves equivalence at one moment; FileCheck proves shape continuously).

## CI

The pilot adds the new `crates/lower/` directory to the Cargo workspace (`Cargo.toml` workspace members). The existing CI `lint` job (`.github/workflows/ci.yml`) already runs over every workspace member:

- `cargo check --workspace --all-targets` — covers the new crate.
- `cargo fmt --all -- --check` — `--all` walks every workspace member.
- `cargo clippy --workspace --all-targets -- -D warnings` — same scope as the check.

No new lint step is needed; the new crate inherits the workspace lint policy in the root `Cargo.toml`.

For the byte-exact regression the pilot adds `tests/filecheck/bool_const_pilot_2397.ry` — a FileCheck golden that asserts `store i1 true` / `store i1 false` is emitted from the migrated path. CMake auto-discovers it via the existing `file(GLOB FILECHECK_TEST_FILES ...)` in `CMakeLists.txt`, and the CI `filecheck` job (`.github/workflows/ci.yml`) runs it on the Linux `default` preset, which uses the same Rust `lower` cdylib as the macOS `rust-emit` preset (the only difference is `LLVM_DIR`). The golden locks IR shape continuously — any regression in the migrated path (or in `ry_emit_const_int` it bottoms out in) breaks it. The CI filecheck job is currently warn-only across the repo per its existing comment ("LLVM-version sensitivity until goldens are confirmed stable"), so the pilot inherits that policy; promotion to required is a separate cross-cutting decision, not pilot-specific.

The one-shot ASLR-normalized before/after `--emit-llvm-ir` diff (§"Verification") remains the migration-PR proof — it certifies that the C++ and Rust paths produce byte-identical IR at the moment of the cutover, which the FileCheck golden alone cannot prove (it would pass on either path independently).

## Related documents

- [Codegen Terminology](codegen-terminology.md) — canonical vocabulary (lowering / emission / lowered IR / boundaries).
- [Codegen Layering Plan](codegen-layering-plan.md) — the lowering / emission split and the lowered IR vocabulary.
- [LLVM IR Emission Boundary](llvm-ir-emission-boundary.md) — the `ry_emit_*` boundary the lower crate calls into.
- [Layer Graduation Workflow](layer-graduation-workflow.md) — the workflow this migration operates inside; defines when a graduation document is earned.
- [Native Call Boundary](native-call-boundary.md) — the orthogonal lowering-side dispatch surface; stages 7-8 will consume it.
- [Runtime Boundary](runtime-abi-boundary.md) — orthogonal `__ry_*` boundary; unchanged by this migration.
