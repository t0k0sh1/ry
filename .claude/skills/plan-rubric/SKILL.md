---
name: plan-rubric
description: 4-axis pass/fail rubric to review a plan immediately before ExitPlanMode — abstraction (WHAT/HOW separation), scope (incl. target-shrinking split forbidden in Plan mode), testability, and dependency citation. Covers how to invoke /test-design-techniques inside a plan and how to use devils-advocate as a plan critic. Invoke on explicit user request: "計画レビュー", "計画抽象度", "WHAT/HOW", "HOW 漏れ", "計画の粒度", "プラン批評", "対象 issue 分割", "target shrinking".
allowed-tools: Read, Grep, Glob
---

# Plan Rubric

A 4-axis pass/fail rubric to evaluate a plan immediately before `ExitPlanMode`. Operationalizes the WHAT/HOW separation principle in AGENTS.md §"Plan モードのルール".

> **Read-only.** Emits pass/fail only; never edits any file.

---

## Why this skill exists

A plan that dips into HOW (function signatures, line numbers, implementation steps) has two failure modes:

1. **Over-specification** — forecloses architectural alternatives; the plan becomes a cheat sheet.
2. **Bloat** — HOW prose outgrows WHAT, straining AGENTS.md's 170-line cap (#1498) and plan readability.

This skill detects both mechanically:

| Axis | What it checks |
|---|---|
| Axis 1: Abstraction (WHAT/HOW) | Each task states a success condition only, not implementation means |
| Axis 2: Scope | Every task maps to the issue's acceptance criteria |
| Axis 3: Testability | Each task carries a completion signal (test command, expected output) |
| Axis 4: Dependency citation | Rules/skills for the edited paths are cited in the plan body |

---

## When to invoke

- **Just before `ExitPlanMode`**, after the plan is complete and before user approval
- When "plan review" / "WHAT/HOW" / "plan abstraction" comes up in conversation
- After a PR review flagged "too detailed a plan" / "no implementation latitude" — invoke proactively next time

---

## The Four Axes

### Axis 1: Abstraction — WHAT/HOW separation (Pass / Fail)

**Pass**: each task states an observable success condition.
**Fail**: the task names specific functions, line numbers, arguments, or implementation decisions.

Litmus test: can an implementer satisfy the task via an *alternative* approach? If not (the path is pinned), it is HOW.

#### OK / NG examples (ry-specific)

**Pair 1: adding a stdlib module (crypto sha256)**

NG (HOW leak):
```
Task: add an ARC-safe `RcStr` overload to `__ry_crypto_sha256(const char*)` and
      insert `add_ry_native_lib(crypto ...)` at CMakeLists.txt L42
```
Why NG: signature, argument type, and CMakeLists line are pinned, foreclosing alternative ARC designs; `/stdlib-module-add`'s 5 steps surface during implementation, not the plan body.

OK (WHAT only):
```
Task: add a sha256 API to the `crypto` stdlib module via the 5 steps of
      `/stdlib-module-add`. Self-verify by accepting valid input / empty string /
      multibyte UTF-8 and by directly triggering reject branches in tests
```
Why OK: states the API surface and test perspective only; signatures and C symbols come at implementation.

---

**Pair 2: improving codegen error reporting (accept List + List)**

NG (HOW leak):
```
Task: insert a `List + List` branch right before the str-vs-non-str reject at
      `emitArithmeticOp` L138, calling `emitListConcat`
```
Why NG: function name, line number, and callee API are pinned at plan time. `codegen-llvm-ir-conventions.md` auto-loads when editing `src/codegen_*.cpp`, so dispatch-order is covered during implementation.

OK (WHAT only):
```
Task: accept `List + List` without a type error. Comply with the dispatch-order
      rule in `codegen-llvm-ir-conventions` and self-verify that existing IR
      golden tests pass
```
Why OK: states acceptance as the success condition and the rule to comply with; insertion site is decided during implementation.

---

### Axis 2: Scope (Pass / Fail)

**Pass**: every task maps to the issue's acceptance criteria, and the plan body records (a) a `/triage-side-finding` Q1-Q4 verdict for any side finding, and (b) a `/scope-decomposition` REQ-4 (re-sweep) result — either "no symmetry gap", or an entry under "out of scope". **Plus**: no task proposes splitting the target issue itself (target-shrinking split forbidden in Plan mode).
**Fail**: any of the above is missing, or the plan contains a task that proposes splitting the target issue.

Conditional rules for side findings:

- **Q1 (hard-to-reproduce CI detection) / Q2 (explicit user instruction)** — "immediate design fixes" handled in the same PR; not "no incidental fixes" violations.
- **Q4(a)** — fold into plan tasks.
- **Q4(b)** — record as a "file separate issue" task, and apply `/scope-decomposition` REQ-1 (4-axis symmetry) / REQ-2 (3 reasons to split) / REQ-3 (chain-depth guard) before drafting the issue. **Allowed even in Plan mode** because Q4(b) creates an orthogonal new issue without touching the target issue's scope.
- **Target-shrinking split is forbidden in Plan mode** — the plan must not propose splitting the target issue itself (e.g. "対象 issue が大きすぎるので 2 つに分けよう" / "この部分は別 issue にして scope を絞ろう"). This is the `/scope-decomposition` REQ-4 MUST rule (AGENTS.md §"Plan モードのルール" のユーザー方針). 判別: 「対象 issue の作業内容を狭めるか?」(YES → 禁止) / 「対象 issue とは直交する独立発見か?」(YES → Q4(b) で別 issue 起票可). If oversize is detected only at Plan mode, record it under "out of scope" instead — splitting decisions must happen before Plan mode (`/scope-decomposition` REQ-5 at issue creation / split judgment).
- Plans must not contain incidental-fix items without a Q1-Q4 verdict.

---

### Axis 3: Testability (Pass / Fail)

**Pass**: each task carries a concrete completion signal (test command, grep pattern, expected output).
**Fail**: tasks end at "fix" / "implement" without stating what marks them done.

---

### Axis 4: Dependency citation (Pass / Fail)

**Pass**: for each path a task touches, the matching `.claude/rules/` or `.claude/skills/` entry is explicitly cited in the plan body.
**Fail**: editing `src/parser*.cpp` without citing `parser-conventions.md`, or adding a stdlib module without citing `/stdlib-module-add`.

> **Vs. AGENTS.md L86**: AGENTS.md states the *obligation* (did you consult rules/skills?). Axis 4 is a *mechanical check-presence* — does the citation string actually appear? Path-scoped rules auto-load during implementation, but citing them at plan time records the design rationale.

---

## Invoking /test-design-techniques within a plan

`/test-design-techniques` lists "Plan mode: while designing test plans for a feature, to estimate coverage breadth" in its When-to-invoke. Consume it as follows:

1. For each TDD task, record **one line** naming the technique to fill **Testability (Axis 3)**.
   Example: `BVA + equivalence partitioning: type-cross boundaries for parser numeric literals`.
2. Naming the technique is WHAT; enumerating cases is HOW, deferred to the Red step where `/test-design-techniques` is invoked again.
3. Do not list "7 test cases" in the plan. Correct WHAT form: "cover boundaries with BVA, then verify ry-specific patterns via `/test-checklist` P1–P8".

---

## Invoking devils-advocate for plan review

devils-advocate (`.claude/agents/devils-advocate.md`) is a critic agent structured around Phase 1-4 (Reconstruction → Multi-Angle Attack → Prioritization → Constructive Synthesis). Invoke it to critique the plan in an independent context.

### When to invoke

- **Just before `ExitPlanMode`**, after the plan is complete and before user approval
- The change spans multiple components or includes architectural decisions
- The change includes a new API / new skill / AGENTS.md / `.claude/agents/` modification

### When to skip

- A single-file bug fix (self-evident scope, no design decision)
- A task fully covered by an existing `/tdd-cycle` Red-Green-Refactor
- Wording / typo fixes

Record the skip rationale in one line (e.g. `devils-advocate skip: single-cpp-file bug fix`).

### How to invoke

Use `Agent` with `subagent_type: devils-advocate` (no slash command — it's an agent, not a skill).

```
Agent tool example:
  subagent_type: devils-advocate
  prompt: |
    Critique the following plan. Focus on WHAT/HOW separation (Axis 1) and
    handling of side findings (Axis 2). Use Phase 1-4.

    [paste plan body]
```

### Consuming the output

| devils-advocate output | Action |
|---|---|
| Critical objections | Revise the plan before `ExitPlanMode` |
| Significant objections only | Leave as reviewer comments; decide during implementation |
| Minor objections only | Ignore; proceed to `ExitPlanMode` |
| No objections (steel-man holds) | Proceed to `ExitPlanMode` |

---

## Cross-reference

- **`AGENTS.md` §"Plan モードのルール"** — the "Plan abstraction (WHAT/HOW separation)" bullet routes here
- **`/test-design-techniques`** — one-line technique name at plan; cases expand at the Red step
- **`/test-checklist`** — inductive complement to `/test-design-techniques`; run during implementation
- **`/tdd-cycle`** — TDD tasks stay as one bundled task in plans (Red-Green-Refactor not split)
- **`.claude/agents/devils-advocate.md`** — plan-critique mode (Phase 1-4)
- **`/triage-side-finding`** — Q1-Q4 verdict (Q1 hard-to-reproduce CI / Q2 explicit user / Q3 `bug-forensics-analyst` / Q4 phase-aware autonomous: Phase A 2 分岐, Phase B 同 PR 吸収 default + 1000 行閾値) and Issue Creation Steps for Axis 2 side findings
- **`/scope-decomposition`** — REQ-4 (Plan モード re-sweep + target-shrinking split forbidden) / REQ-5 (oversize n-piece single-preview split, applied before Plan mode)
- **`/pre-commit-checklist`** — post-implementation completion check (not a plan-stage target)

---

## Notes

- Read-only: emits pass/fail; does not edit any file.
- All four axes are pass/fail. No scoring, no template — a rubric prescribing HOW would be self-contradicting.
- Plan files live at `/Users/t0k0sh1/.claude/plans/<issue#>-<random>.md` and are readable during Plan mode.
