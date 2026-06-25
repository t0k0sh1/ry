# Codegen Layering Plan

This document records the current codegen responsibility split. It is a compact orientation page for contributors; canonical vocabulary lives in [Codegen Terminology](codegen-terminology.md), and the C boundary contract lives in [LLVM IR Emission Boundary](llvm-ir-emission-boundary.md).

## Split

The codegen stack has two conceptual sub-layers:

| Layer | Owns | Must not own |
|---|---|---|
| Ry semantic lowering | Type and ownership decisions, stdlib/native dispatch selection, ARC intent, metadata, diagnostics, source-aware behavior | LLVM instruction construction |
| LLVM IR emission | Basic blocks, PHIs, LLVM instructions, runtime-symbol declaration, low-level LLVM type construction | Ry type aliases, overload policy, visibility, import resolution, user diagnostics |

The lowering side prepares operation inputs and calls `ry_emit_*`. The emission side maps those inputs to LLVM IR.

## Lowered Operation Surface

The stable mental model is "lowering decides what; emission decides how to build the IR." Important lowered operation families include:

| Family | Lowering decision | Emission responsibility |
|---|---|---|
| Runtime calls | selected `__ry_*` symbol, signature, wrapping policy | declare/call the symbol and return the LLVM value |
| Bounds checks | index, length, error kind | negative-index handling, range branch, runtime error IR |
| Option / Result | active variant and payload | aggregate construction and branch/PHI shape |
| Any | tag, descriptor, target type, retain policy | box/unbox/check IR |
| ARC / CoW | atomicity, destructor, element retain policy | counter updates, clone/copy paths, release calls |
| Collections | operation kind, element/key metadata, sizes | allocation, copy, header updates, mutation IR |
| Control flow | semantic branch condition and merge intent | blocks, branches, PHIs |

Primitive LLVM operations such as load/store/GEP/arithmetic/function creation also cross the boundary as generic `ry_emit_*` primitives. They are not Ry semantic ops; they exist to keep LLVM construction concentrated in the emission crate.

## Current Layout

The old C++ shim layer (`codegen_lowering_*`, `codegen_emission_*`, `lowered_*` headers) has been removed. Current code is organized as:

- caller-side C++ in `src/codegen_*.cpp` prepares inputs inline
- `CodeGen::*` helpers survive where they carry semantic side effects such as metadata propagation, ARC retain decisions, or type-name resolution
- Rust `crates/emit/src/{abi,composite,primitive,context}` owns IR construction behind `ry_emit_*`

## Design Rules

- Do not add LLVM C++ types to the public emission boundary.
- Keep diagnostics and source-aware semantic checks on the lowering side.
- Keep Ry layout knowledge in composite emission; keep generic LLVM calls in primitive emission.
- Prefer reusable primitive capabilities over coarse descriptors when the descriptor would encode caller-side semantics.
- Add a composite op only when it names a real Ry layout/runtime invariant and avoids duplicated IR shape.
- Avoid line-count-only splits; split by responsibility.

## Migration Discipline

For behavior-preserving codegen migrations:

1. Build a probe that actually reaches the path being migrated.
2. Capture baseline `--emit-llvm-ir` output before changing the path.
3. Confirm path markers in the baseline so the diff is not vacuous.
4. Migrate the implementation.
5. Require an ASLR-normalized before/after IR diff to be empty, unless the PR explicitly changes IR shape.
6. Add or update FileCheck coverage for the post-migration invariant when appropriate.

## Graduation

Layer graduation documents should be written only when a layer has a stable responsibility contract, inputs/outputs, invariants, error behavior, dependency direction, and verification story. Do not write graduation docs for speculative or pilot-only shapes.

## Related Documents

- [Codegen Terminology](codegen-terminology.md)
- [LLVM IR Emission Boundary](llvm-ir-emission-boundary.md)
- [Layer Graduation Workflow](layer-graduation-workflow.md)
- [Native Call Boundary](native-call-boundary.md)
- [Runtime Boundary](runtime-abi-boundary.md)
