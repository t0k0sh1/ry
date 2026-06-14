# ry Development Guidelines

## Terminology

- Use `docs/reference/glossary.md`.

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

- Requirements: Rust 1.83+ and shared libLLVM.
- If corrosion cannot find Rust, configure with `-DRust_COMPILER=$(rustup which rustc)`.
- Repo-built `ry` prefers `share/std/`; use `RY_ENV=internal` only for extra isolation.
- Full verification: `/pre-commit-checklist`.

## Instruction Routing

| Location | Purpose |
|---|---|
| `.claude/rules/*.md` | Path-scoped implementation constraints |
| `.claude/skills/*/SKILL.md` | Task procedures and decision flows |
| `.claude/agents/*.md` | Independent analysis and verification roles |
| `KNOWLEDGE.md` | Unclassified findings and sanitizer incident notes |

- Search tags before implementing: `rg '\*\*Tags\*\*:.*<keyword>' .claude KNOWLEDGE.md`.
- New path-specific lesson: update the matching rule.
- New procedure or cross-cutting lesson: update the matching skill.
- No matching destination: use `/knowledge-md-management`.
- Prompt-file edits must pass `.claude/skills/pre-commit-checklist/run-prompt-refs-lint.sh`.

## Workflow

- When a user asks to start work on an identified `#<n>` issue, run `scripts/claim-issue.sh '#<n>'` before investigation, design, or implementation.
  - Claim success is not a stopping point; continue the requested work in the same turn.
  - If the script reports the issue is already `wip`, stop to avoid duplicate work.
- Issue work: claim when required → inspect issue → TDD → `/pre-commit-checklist`.
- Plan mode is optional. Use it when the user requests a plan or when complexity makes an explicit plan useful; do not require it before implementation.
- TDD:
  - Existing code: add a detector test first; remove old-spec tests only after the new specification passes.
  - New features: run Red-Green-Refactor for each test case.
- When using a plan:
  - Cover implementation through self-verification only.
  - Never include `/git-push`, `/git-create-pr`, or `/git-finalize-pr`.
  - State outcomes, not implementation details.
  - Include matching rules / skills, documentation decision, and verification.
  - Keep each Red-Green-Refactor cycle in one task.
  - Use `/horizontal-sweep` for terminology or identifier changes.
  - Do not shrink or split the target issue after implementation scope is committed.
- Side findings: follow `/triage-side-finding`.
- Internal behavior investigation: use `/ry-trace`.
- Release flow: start with `/preparing-for-release <X.Y.Z>`; releases are published by `.github/workflows/release.yml` after a validated `vX.Y.Z` tag push.

## Execution Constraints

- Never start background or detached processes.
- Run builds and tests in the foreground.
- Set command timeouts:
  - Build: 300 seconds.
  - Tests: at most 600 seconds.
- Parallelize verification only with foreground subagents.
- Do not create temporary files in the worktree.
- For ad-hoc Ry checks, use `/ry-playground`.
- `/tmp` files are allowed only when a tool requires a path; do not delete them.
- Do not run ad-hoc `rm`, `unlink`, cleanup traps, or manual build-tree deletion.
- Required deletion:
  - Tracked file: `git rm`.
  - Build tree: owning script's `--clean`.
  - Otherwise: give the exact command to the user and wait.
- `rm` inside committed scripts is allowed.

## Failure Handling

- Fix every ASan, UBSan, race, and fuzzer finding before completion.
- Failure reports must state:
  1. Observed symptom.
  2. Reproduction condition.
  3. Evidence and next action.
  4. Relationship to the current change, when relevant.
- Do not dismiss a failure with a generic nondeterminism label.
- When the occurrence condition is unknown, say:
  - `Occurrence condition not yet identified. Investigation of the reproduction condition is incomplete.`
- Sanitizer incident notes: `KNOWLEDGE.md` section `## サニタイザー既知問題`.

## Review Communication

- Do not begin failure reports or review replies with responsibility disclaimers.
- Do not reject feedback solely because it differs from neighboring APIs.
- A rejection must cite an invariant, user-facing behavior, type safety, performance requirement, or explicit scope decision.
- Reply to review threads; do not resolve them preemptively.
- Recurring review lesson:
  - Path-specific: matching rule.
  - Cross-cutting or uncategorized: `/knowledge-md-management`.

## Git And GitHub

- Do not commit directly to `main`.
- Feature branches must not contain `main` after lowercasing and removing non-letters.
- Bring `main` into a feature branch with `git rebase origin/main`, never merge.
- After rebase, push with `--force-with-lease`; do not fetch again before that push.
- Feature branch creation, commit, rebase, and push: `/git-push`.
- PR creation: `/git-create-pr`.
- `/git-push`, `/git-create-pr`, and `/git-finalize-pr` run only when the user directly invokes that exact slash command.
- Never invoke these skills autonomously or from another skill.
- Never propose these skills, present them as options, include them in a plan, or list them as next steps.
- Conflict resolution: `/git-resolve-conflicts`.
- Final review, CI, verification, merge, and `wip` cleanup for an already-pushed branch: `/git-finalize-pr`.
- Before merge, stop and report any uncommitted or untracked files.
- Commit `.serena/` changes with the related work.

## User Permission Gates

Require explicit user direction for:

- External review requests.
- `git add`, commit, push, and PR creation.
- Creating a GitHub issue.

Issue creation:

- Decide whether filing is appropriate via `/triage-side-finding`.
- Present one concrete issue proposal via `/git-create-issue`.
- Wait for approval before `gh issue create`.
- Do not present “create a separate issue” as a menu option.
- `/preparing-for-release <X.Y.Z>` is pre-authorization for its release-tracking issues.

## Path-Specific Entry Points

- Runtime memory safety: `.claude/rules/runtime-memory-safety.md`.
- Compiler warnings and static analysis: `.claude/rules/build-warning-flags.md`, `docker/README.md`.
- LLVM IR golden conventions: `.claude/rules/codegen-llvm-ir-conventions.md`.
- Stdlib module work: `/stdlib-module-add`.
- tree-sitter edits: `editor/tree-sitter/README.md` and `.claude/rules/tree-sitter-grammar-editing.md`; rebuild `ry.so`.
- CI image changes: `/ci-image-workflow`.
- Fuzzer harness changes: `/libfuzzer-harness`.

## Completion

- Run `/pre-commit-checklist` before declaring work complete.
- Do not change labels during self-verification; post-merge cleanup occurs only during a user-invoked `/git-finalize-pr`.
