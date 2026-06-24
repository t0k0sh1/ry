# ry Development Guidelines

## Quick References

- Glossary: `docs/reference/glossary.md`.
- Verification: choose applicable checks from `/pre-commit-checklist`.
- Prompt edits: `.claude/skills/pre-commit-checklist/run-prompt-refs-lint.sh`.

## Build And Test

| Host | Configure | Build dir | CLI |
|---|---|---|---|
| Linux / CI | `cmake --preset default` | `build/` | `./build/ry` |
| macOS | `cmake --preset rust-emit` | `build-rust/` | `./build-rust/ry` |

```bash
cmake --build <build-dir>
./<build-dir>/ry_tests
./<build-dir>/ry test -p
./<build-dir>/ry test tests/spec/<file>.test.ry
bash scripts/check-examples.sh
```

- Requires Rust 1.83+ and shared libLLVM. If corrosion misses Rust, configure with `-DRust_COMPILER=$(rustup which rustc)`.
- Repo-built `ry` prefers `share/std/`; use `RY_ENV=internal` for extra isolation.

## Instruction Routing

- `.claude/rules/*.md`: path-scoped hazards and invariants.
- `.claude/skills/*/SKILL.md`: slash-command procedures and decision flows.
- `.claude/agents/*.md`: independent analysis / verification roles.
- Before implementation, tag-search relevant lessons: `rg '\*\*Tags\*\*:.*<keyword>' .claude`.
- Add new reusable lessons to the matching rule or skill; create a new path-scoped rule only when no destination fits.

## Workflow

- For an identified `#<n>` issue, run `scripts/claim-issue.sh '#<n>'` before investigation. If already `wip`, stop.
- Issue flow: claim if required -> inspect -> TDD -> applicable verification.
- Plan only when requested or when complexity warrants it.
- TDD: add a detector test first for existing behavior changes; use Red-Green-Refactor for new features.
- Side findings go through `/triage-side-finding`; internal behavior investigation through `/ry-trace`; releases start with `/preparing-for-release <X.Y.Z>`.

## Execution Notes

- Timeouts: build 300 s; tests up to 600 s. Parallelize verification only with foreground subagents.
- No ad-hoc `rm` / `unlink` / cleanup traps / manual build-tree deletion. Use `git rm` for tracked files and owning scripts' `--clean` for build trees; otherwise ask with the exact command.

## Failure And Review

- Fix every ASan / UBSan / race / fuzzer finding before completion.
- Failure reports must include symptom, reproduction condition, evidence / next action, and relationship to the current change. If unknown, write: `Occurrence condition not yet identified. Investigation of the reproduction condition is incomplete.`
- Review replies must address threads without pre-emptively resolving them. Reject feedback only by citing an invariant, user-facing behavior, type safety, performance, or explicit scope.

## Git And GitHub

- No direct commits to `main`; feature branch names must not contain `main` after lowercasing and removing non-letters.
- Bring `main` into feature branches with `git rebase origin/main`; after rebase, push with `--force-with-lease` and no extra fetch.
- `/git-push`, `/git-create-pr`, and `/git-finalize-pr` run only when the user directly invokes that exact command.
- `/git-resolve-conflicts` handles PR conflicts.
- Before merge, stop and report uncommitted / untracked files.
- External reviews, `git add` / commit / push / PR creation, and GitHub issue creation require explicit user direction.
- Issue creation flow: `/triage-side-finding` -> `/git-create-issue` -> user approval -> `gh issue create`.
- Use `gh issue edit --remove-label wip` for label cleanup. Never use `gh issue edit --label wip` as a cleanup operation.

## CI And Release Constraints

- Release Linux uses `ry-ci-glibc-old` and pins immutable `:llvm-<MAJOR>-rev<N>` tags; do not switch it to normal `ry-ci` or mutable tags.

## Path-Specific Entry Points

- Compiler warnings / static analysis: `.claude/rules/build-warning-flags.md`, `docker/README.md`.
- Stdlib module work: `/stdlib-module-add`.
- tree-sitter: `editor/tree-sitter/README.md` + `.claude/rules/tree-sitter-grammar-editing.md`; rebuild `ry.so`.
- CI images: `/ci-image-workflow`.
- Test classification: `docs/reference/test-taxonomy.md`.
- Examples classification: `docs/reference/examples-taxonomy.md`.

## Completion

- Run applicable checks before declaring work complete.
- No label changes during self-verification; post-merge cleanup only via user-invoked `/git-finalize-pr`.
