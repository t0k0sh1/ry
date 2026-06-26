# ry Development Guidelines

## Quick References

- Glossary: `docs/reference/glossary.md`.
- Test classification: `docs/reference/test-taxonomy.md`.
- Examples classification: `docs/reference/examples-taxonomy.md`.

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

## Specification Decisions

When the user asks to decide a design, says "grill me", or the requested behavior is ambiguous:

- Do not implement yet.
- Ask one decision question at a time.
- Include the recommended answer and reason with each question.
- If the answer can be discovered from the codebase, inspect the code instead of asking.
- Resolve dependent decisions in order.
- Finish with a concise decision log covering scope, non-goals, behavior, tests, and risks.

## Workflow

- For an identified `#<n>` issue, run `scripts/claim-issue.sh '#<n>'` before investigation. If already `wip`, stop.
- Plan only when requested or when complexity warrants it.
- Use TDD for behavior changes and bug fixes.
- Side findings: fix now when the reproduction window may close, severity is high, or the fix is small; otherwise ask before expanding scope.

## TDD

Use vertical-slice TDD:

- Start with one detector test for one observable behavior.
- Prefer public or layer-level interfaces over implementation details.
- Confirm the test fails for the intended reason when practical.
- Implement the smallest change that makes the test pass.
- Repeat one behavior at a time; do not write all tests first.
- Do not anticipate future tests with speculative code.
- Never refactor while RED.
- After GREEN, refactor only with tests passing after each step.

Internal tests are allowed when the invariant is not cleanly observable through a public interface, especially for parser, codegen, runtime, ownership, metadata, ABI, and layout behavior. Classify tests with `docs/reference/test-taxonomy.md` when adding or moving tests.

Mock only at system boundaries: external processes, network, time, randomness, filesystem, or similarly uncontrollable dependencies. Do not mock internal collaborators merely to assert call shape.

## Execution Notes

- Timeouts: build 300 s; tests up to 600 s.
- No ad-hoc `rm` / `unlink` / cleanup traps / manual build-tree deletion. Use `git rm` for tracked files and owning scripts' `--clean` for build trees; otherwise ask with the exact command.

## Failure And Review

- Fix every ASan / UBSan / race / fuzzer finding before completion.
- Failure reports must include symptom, reproduction condition, evidence / next action, and relationship to the current change. If unknown, write: `Occurrence condition not yet identified. Investigation of the reproduction condition is incomplete.`
- Review replies must address threads without pre-emptively resolving them. Reject feedback only by citing an invariant, user-facing behavior, type safety, performance, or explicit scope.

## Git And GitHub

- No direct commits to `main`; feature branch names must not contain `main` after lowercasing and removing non-letters.
- Bring `main` into feature branches with `git rebase origin/main`; after rebase, push with `--force-with-lease` and no extra fetch.
- Before merge, stop and report uncommitted / untracked files.
- External reviews, `git add` / commit / push / PR creation, and GitHub issue creation require explicit user direction.
- Use `gh issue edit --remove-label wip` for label cleanup. Never use `gh issue edit --label wip` as a cleanup operation.

## CI And Release Constraints

- Release Linux uses `ry-ci-glibc-old` and pins immutable `:llvm-<MAJOR>-rev<N>` tags; do not switch it to normal `ry-ci` or mutable tags.

## Path-Specific Notes

- Compiler warnings / static analysis: see `docker/README.md`.
- tree-sitter: see `editor/tree-sitter/README.md`; rebuild `ry.so` after grammar changes.
- Test classification: see `docs/reference/test-taxonomy.md`.
- Examples classification: see `docs/reference/examples-taxonomy.md`.

## Completion

- Run applicable checks before declaring work complete.
- For prompt-only changes, a diff review is usually sufficient unless a referenced validation script applies.
- Do not change GitHub labels during self-verification.
