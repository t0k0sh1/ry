---
paths:
  - "src/jit/test_runner.cpp"
  - "include/ry/jit/test_runner.hpp"
  - "src/jit/jit_runner.cpp"
  - "src/app/main.cpp"
---

# Test Runner Isolation

### Per-file subprocess is the only legitimate `ry test` orchestration

**Source**: #2238 (formalising the #2232 design verdict on top of
the #2234 subprocess fan-out unification)
**Tags**: testing, jit, llvm, orc, isolation, design-invariant,
runtime, parent-process

**Rule**: `ry test` must dispatch every discovered `*.test.ry`
through the `runTestFiles` → `runTestFilesSubprocessFanOut` →
`runTestFileSubprocess` chain in `src/jit/test_runner.cpp`. The
parent process must not JIT. Specifically banned:

- Reviving any `runTestFilesSequential` (or moral equivalent) that
  walks `runRySource` / `runRyFile` for multiple files inside the
  parent process. The function was removed in #2234; do not
  reintroduce it under a new name.
- Adding any new code path under the `test` subcommand in
  `src/app/main.cpp` that calls `runRySource` / `runRyFile` more
  than once per parent process.
- Sharing an `LLJIT` instance across files within one process.

**Why**: The six-step ORC teardown suppression (`.claude/rules/jit-teardown-suppression.md`)
deliberately leaks `LLJIT`, `CodeGen`, and the parsed `Program` AST; process exit is the *only* path that
reclaims this memory. Walking multiple files in one process accumulates the leak — the legacy in-process
sequential path hit `bad_alloc` at ~42 of 181 spec files (~7 GB RSS) on Linux CI before #2234.
Per-file subprocess isolation is structurally required until the six-step suppression is removed.

**How to apply**: Orchestration changes belong in `runTestFilesSubprocessFanOut`. `-p N` controls worker
count only; it is not the dimension that bounds memory. New verbs that need to "run several files" must
spawn one child per file (see the `--outline` pattern in #2236). The CI counterpart
(`.claude/rules/ci-workflows.md` "Linux Ry self-test: pass no `-p`") encodes the worker-count default
this rule depends on — keep the two in sync.

### Sub-file (`@it` / `@describe`) granularity parallelism is forbidden

**Source**: #2238 (#2232 design verdict)
**Tags**: testing, jit, parallelism, lifecycle, isolation,
design-invariant

**Rule**: Do not introduce a runner, runtime API, or CLI flag that parallelises
*within* a single `.test.ry` file at `@it` or `@describe` granularity.
Parallelism stays at the one-file-per-process boundary that
`runTestFilesSubprocessFanOut` already provides.

**Why**: File-level `@beforeAll` / `@afterAll` lifecycle hooks bracket one source file's worth of work and
own setup/teardown state in the JIT'd module. Parallelising `@it` blocks would either run hooks multiple
times against the same in-process state (corrupting ordering semantics) or serialise around them
(defeating parallelism). The semantics are incompatible by design.

**How to apply**: Treat any "make `@it` blocks run concurrently" proposal as a request to break the hook
contract — reject and link to this rule plus #2232. Per-file wall-clock pain → split into smaller
`.test.ry` files so the existing subprocess fan-out gains more parallelism.

### When this rule can be revisited

**Source**: #2238
**Tags**: testing, jit, llvm, orc, isolation, future-work,
rust-migration

**Rule**: The two rules above are absolute as long as the six-step ORC teardown
suppression is required. They become revisitable — not automatically relaxed —
when *either* of the following lands:

- A root-cause fix for the LLVM ORC / JITLink teardown heap corruption tracked
  at #742, allowing the C++ teardown chain to run normally without the
  leak-and-`_exit` workaround.
- The JIT-runner Rust migration (#1949 / #1950 / #1993) takes over ownership of
  `LLJIT` / `CodeGen` / module lifetimes with explicit `Drop` order replacing
  the C++ teardown ordering bug.

**Why**: Both rules are workarounds derived from a specific teardown defect, not architectural preferences.
If the defect goes away, the structural requirement for per-file process boundaries also goes away.

**How to apply**: Until one of the conditions above lands, treat any proposal to relax these rules as out
of scope and link the proposer here. Once a candidate condition lands, re-read this rule and
`.claude/rules/jit-teardown-suppression.md` together before any in-process orchestration is reintroduced —
`_Exit` of the JIT process, leak-on-purpose of `LLJIT` / `CodeGen` / `Program`, and the
`finalizeAfterPossibleJit` bypass in `src/app/main.cpp` are interlocked and were proven required only as
a set.
