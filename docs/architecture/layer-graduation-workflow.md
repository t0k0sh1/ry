# Layer Graduation Workflow

This document defines how a compiler/runtime layer becomes "done enough" to earn a written contract and be considered a Rust-migration candidate. It is the stage-3 deliverable of v0.0.26 (issue #1824) and the workflow reference for follow-up codegen split work (#1949 → #1950) and for any future per-layer organization.

## Purpose

Writing detailed responsibility / I/O / contract documentation **before** the implementation matches that shape produces aspirational documentation that drifts from reality and provides false confidence. This is the same anti-pattern observed in `docs/reference/*.md` drift incidents (e.g. #889, #1118, #1474), where a written spec lagged or led the code and silently disagreed with it.

The alternative sequence this document prescribes:

1. Keep an initial design hypothesis lightweight (one paragraph in `compiler-layers.md`).
2. Refactor the code until a layer actually owns a single responsibility and a narrow dependency surface.
3. Write the layer's graduation document **after** the refactor stabilizes.
4. Use that graduation document as evidence that the layer is reorganized enough to be a Rust-migration candidate (see `runtime-abi-boundary.md` for the readiness criteria already applied to the runtime side).

A layer that has only the lightweight hypothesis is **not graduated**. A layer that has a written contract but a code shape that does not match the contract is also **not graduated** — that is documentation drift, not graduation.

## Graduation criteria

A layer (or component) is graduate-ready when **all** of the following hold:

1. **Single responsibility**. The layer's responsibility is statable in one sentence without "and" / "also". If the sentence needs a list, the layer has not yet been split far enough.
2. **Inputs and outputs are typed concretely**. "Takes an AST" / "uses CodeGen context" is not concrete. The contract names the actual types (or opaque handles) crossing the boundary.
3. **Invariants are enumerated and tested**. Each invariant has at least one test that breaks if it is violated. Untested invariants are aspirational.
4. **Allowed and forbidden dependencies are header-observable**. The `#include` graph reflects the dependency direction — adding a forbidden include must fail (compile error, lint, or CI grep). The rule in `compiler-layers.md` is the project-wide default; per-layer graduation can narrow it further.
5. **The boundary is observable in code**. A separate header, namespace, or `extern "C"` boundary — not just a comment block. A graduated layer can be located by `grep` for its boundary token (header name, namespace, or symbol prefix).
6. **Errors owned by the layer are listed with their channel**. Codegen errors surface through `codegenError`; runtime errors surface through `__ry_set_last_error`; lexer errors surface through `Diagnostic`. A graduated layer specifies which channel it uses and which error kinds it owns end-to-end.
7. **Rust migration readiness is assessed against the runtime-side criteria**. The four conditions in [Runtime Boundary](runtime-abi-boundary.md) "Rust migration readiness criteria" are the project-wide baseline:
   - Scalar / opaque-pointer / `#[repr(C)]` POD types only at the boundary.
   - No internal LLVM dependency at the boundary.
   - No transitive C++ template crossing the boundary.
   - Uniform error channel (e.g. `__ry_set_last_error` for runtime, structured `Result<T, Error>` wrapping for codegen-emitted calls).

   A layer that fails one of the four criteria is still allowed to graduate, but the graduation document records the blocker so it is visible during Rust-migration planning.

## Graduation document template

Each graduated layer earns a Markdown file under `docs/architecture/<layer>-graduation.md` (or `docs/architecture/<component>-graduation.md` for finer-grained components) with the following sections. The template is intentionally short — long graduation documents tend to repeat themselves and are harder to keep current.

```markdown
# <Layer name> Graduation

Layer: <layer name as it appears in compiler-layers.md>
Implementation: <primary header(s) / source directory>
Test coverage: <pointer to the tests that lock the contract>
Status: graduated (<date>, <PR or issue link>)

## Responsibility

One sentence. No "and".

## Inputs

The concrete types (or opaque handles) accepted at the boundary.

## Outputs

The concrete types (or opaque handles) produced at the boundary.

## Invariants

Bulleted list. Each invariant cites the test that enforces it.

## Errors

Bulleted list. Each entry: the error kind, the channel it surfaces through, and which side (layer / caller) owns recovery.

## Allowed dependencies

Other layers / utility modules this layer may #include.

## Forbidden dependencies

Other layers this layer must NOT #include. Include a `grep`-able assertion of how the forbid is observable in code.

## Rust migration readiness

For each of the four criteria in runtime-abi-boundary.md, one line: "met" / "blocked by <reason>".

## Remaining blockers

Bulleted list. Only items that block Rust migration or further responsibility narrowing — not nice-to-haves.
```

The file is checked in at the same time as the PR that completes the layer's refactor, not earlier and not in a separate follow-up. A graduation document landed in a different PR than the corresponding code refactor is treated as evidence of either (a) the refactor wasn't actually done in the cited PR, or (b) the doc is aspirational.

## SRP and file-size policy

The project applies SRP / file-size goals **after** dependency reduction, not as a primary driver:

- **First target**: a new or newly split implementation file stays at or under 500 lines (including comments).
- **Later target**: once a layer's responsibilities have stabilized, target 200–300 lines per file where it improves navigability.
- **Line-count-only splits are forbidden**. A split must correspond to a responsibility boundary — splitting a 1,000-line file into two 500-line files that still share state and dependencies is a no-op for graduation purposes (it changes file layout, not the contract).
- A layer is **not graduated** because its files are small. Small files without a clear responsibility split are just smaller monoliths.

## Anti-patterns to avoid

1. **Aspirational design ahead of code**. Writing a layer's responsibility / I/O / contract before the code has been split. The contract will not match the code, and readers will use the contract as a spec while the code does something else. This is the same drift class as the `docs/reference/*.md` incidents (#889, #1118, #1474) and is explicitly warned about in issue #1824.
2. **Graduating all layers at the same time**. Each layer's graduation is a separate exercise; trying to write graduation docs for the entire stack at once produces shallow contracts and hides the layers that actually need work.
3. **Filename-prefix-only "layering"**. Renaming `codegen_call_*.cpp` to `lowering_call_*.cpp` does not graduate the codegen layer. The responsibility split is what graduates, not the file naming.
4. **Line-count-only splits**. See SRP policy above.
5. **Writing the graduation doc in a follow-up PR**. If the doc and the refactor are not in the same PR, the doc is either drift in advance (PR ships before doc) or aspirational (doc ships before PR). Both are anti-patterns.
6. **Treating utility extraction as graduation**. Extracting `ry::util/type_name.hpp` (#1820) is a precondition for graduating the codegen layer, not graduation itself. Pure-utility extraction is necessary but not sufficient.

## When to write the graduation document

After **all** of the following hold:

1. The refactor PR that reorganizes the layer is ready (not yet merged is acceptable; speculative refactor work in a feature branch counts).
2. The layer's existing tests pass against the refactored shape — i.e. behavior is preserved, only structure changed.
3. The dependency direction is observable in code (`grep` the forbidden includes; they must not be present).
4. The error channels owned by the layer are uniform — no mixed-channel paths remain inside the layer.

If any of these fail, the graduation document is premature; finish the refactor first.

## Related documents

- [Compiler Layers](compiler-layers.md) — layer ordering, dependency direction, and the lightweight hypothesis the workflow starts from.
- [LLVM IR Emission Boundary](llvm-ir-emission-boundary.md) — the `extern "C"` boundary design for the codegen-internal split.
- [Runtime Boundary](runtime-abi-boundary.md) — the existing `__ry_*` boundary classification; its "Rust migration readiness criteria" are the format precedent for graduation criteria.
- [Codegen Layering Plan](codegen-layering-plan.md) — the codegen-specific working hypothesis (Ry semantic lowering vs LLVM IR emission) and pilot extraction target.
