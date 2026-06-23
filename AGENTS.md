# ry Development Guidelines

## Terminology

- Glossary: `docs/reference/glossary.md`.

## Build And Test

| Host | Configure | Build directory | CLI |
|---|---|---|---|
| Linux / CI | `cmake --preset default` | `build/` | `./build/ry` |
| macOS | `cmake --preset rust-emit` | `build-rust/` | `./build-rust/ry` |

```bash
cmake --build <build-dir>
./<build-dir>/ry_tests
./<build-dir>/ry test -p
./<build-dir>/ry test tests/spec/<file>.test.ry
```

- Requirements: Rust 1.83+ and shared libLLVM. If corrosion cannot locate Rust, pass `-DRust_COMPILER=$(rustup which rustc)` at configure.
- Repo-built `ry` prefers `share/std/`; use `RY_ENV=internal` for extra isolation.
- Full verification: `/pre-commit-checklist`.

## Instruction Routing

| Location | Purpose |
|---|---|
| `.claude/rules/*.md` | Path-scoped implementation constraints (hazards & invariants only) |
| `.claude/skills/*/SKILL.md` | Task procedures and decision flows |
| `.claude/agents/*.md` | Independent analysis and verification roles |

- Tag-search before implementing: `rg '\*\*Tags\*\*:.*<keyword>' .claude`.
- New path-specific lesson → update the matching rule (auto-loaded via `paths:`); create a new rule if none fits.
- New procedure / cross-cutting lesson → update the matching skill.
- Prompt-file edits must pass `.claude/skills/pre-commit-checklist/run-prompt-refs-lint.sh`.

## Workflow

- When asked to start an identified `#<n>` issue, run `scripts/claim-issue.sh '#<n>'` before any investigation / design / implementation. Claim success is not a stop point; continue in the same turn. If the script reports the issue is already `wip`, stop to avoid duplicate work.
- Issue work: claim if required → inspect issue → TDD → `/pre-commit-checklist`.
- Plan mode is optional. Use it only when the user requests a plan or complexity warrants one.
- TDD: existing code → add a detector test first; drop old-spec tests only after the new spec passes. New feature → Red-Green-Refactor per test case.
- When using a plan:
  - Cover implementation through self-verification only (never include `/git-push`, `/git-create-pr`, or `/git-finalize-pr`).
  - State outcomes, not implementation details. Include matching rules / skills, documentation decision, and verification.
  - Keep each Red-Green-Refactor cycle in one task. Use `/horizontal-sweep` for terminology / identifier changes.
  - Do not shrink or split the target issue after implementation scope is committed.
- Side findings → `/triage-side-finding`. Internal behavior investigation → `/ry-trace`.
- Release flow: start with `/preparing-for-release <X.Y.Z>`; releases are published by `.github/workflows/release.yml` after a validated `vX.Y.Z` tag push.

## Execution Constraints

- No background / detached processes. Builds and tests run in the foreground.
- Timeouts: build 300 s; tests up to 600 s. Parallelise verification only with foreground subagents.
- No temp files in the worktree. Ad-hoc Ry checks → `/ry-playground`. `/tmp` is allowed only when a tool requires a path (do not delete).
- No ad-hoc `rm` / `unlink` / cleanup traps / manual build-tree deletion. For deletion: tracked file → `git rm`; build tree → the owning script's `--clean`; otherwise hand the exact command to the user and wait. `rm` inside committed scripts is fine.

## Failure Handling

- Fix every ASan / UBSan / race / fuzzer finding before completion.
- Failure reports state (1) observed symptom, (2) reproduction condition, (3) evidence and next action, (4) relationship to the current change when relevant. Do not dismiss with a generic nondeterminism label. When the reproduction condition is unknown, write: `Occurrence condition not yet identified. Investigation of the reproduction condition is incomplete.`
- Sanitizer incident notes / LLVM ORC teardown suppression: `.claude/rules/jit-teardown-suppression.md`.

## Review Communication

- No responsibility disclaimers at the start of failure reports or review replies.
- Do not reject feedback solely because it differs from a neighbouring API. A rejection must cite an invariant, user-facing behavior, type safety, performance requirement, or explicit scope decision.
- Reply to review threads; do not pre-emptively resolve them.
- Recurring review lesson: path-specific → matching rule; cross-cutting → a new rule or an existing skill.

## Git And GitHub

- No direct commits to `main`. Feature branch names must not contain `main` after lowercasing and removing non-letters.
- Bring `main` into a feature branch with `git rebase origin/main` — never merge. After rebase, push with `--force-with-lease` (no extra fetch beforehand).
- Push without PR: `/git-push`. PR creation in one pass: `/git-create-pr`. Conflict resolution: `/git-resolve-conflicts`. Already-pushed PR finalisation (review, CI, merge, `wip` cleanup): `/git-finalize-pr`.
- `/git-push`, `/git-create-pr`, and `/git-finalize-pr` run only when the user directly invokes that exact slash command. Do not invoke autonomously, from another skill, propose as an option, or list as a next step.
- Before merge, stop and report any uncommitted / untracked files. Commit `.serena/` changes alongside the related work.

## User Permission Gates

- External review requests, `git add` / commit / push / PR creation, and GitHub issue creation all need explicit user direction.
- Issue creation: `/triage-side-finding` to decide → `/git-create-issue` for one concrete proposal → wait for approval → `gh issue create`. Never present "create a separate issue" as a menu option. `/preparing-for-release <X.Y.Z>` pre-authorises its own release-tracking issues.

## Path-Specific Entry Points

- Runtime memory safety: `.claude/rules/runtime-memory-safety.md`.
- Compiler warnings / static analysis: `.claude/rules/build-warning-flags.md`, `docker/README.md`.
- LLVM IR golden conventions: `.claude/rules/codegen-llvm-ir-conventions.md`.
- Stdlib module work: `/stdlib-module-add`.
- tree-sitter edits: `editor/tree-sitter/README.md` + `.claude/rules/tree-sitter-grammar-editing.md`; rebuild `ry.so`.
- CI image changes: `/ci-image-workflow`. Fuzzer harness changes: `/libfuzzer-harness`.
- Test classification (contract / internal / regression): `docs/reference/test-taxonomy.md`.

## Completion

- Run `/pre-commit-checklist` before declaring work complete. No label changes during self-verification (post-merge cleanup only via user-invoked `/git-finalize-pr`).
