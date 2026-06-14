---
name: pre-commit-checklist
description: Required self-verification before declaring work complete or publishing changes.
allowed-tools: Bash(./.claude/skills/pre-commit-checklist/*.sh:*), Bash(git diff:*), Bash(git fetch:*)
---

# Pre-commit Checklist

## Select Required Checks

Use `git diff --name-only origin/main`. Multiple categories take the strictest result.

| Change | Docs | Changelog | Tests | Sanitizers | Fuzz |
|---|---|---|---|---|---|
| `.claude/` only | skip | skip | skip | skip | skip |
| Docs / top-level Markdown only | done by edit | skip | skip | skip | skip |
| Tests only | review | review | run | run | parser-family only |
| Parser / lexer / json / utf8 / string / io | run | run | run | run | run |
| Other code | run | run | run | run | skip |

Always evaluate:

- Rules / skills update.
- Static analysis.
- Rust lint when `crates/` changed.
- Prompt-reference lint when `.claude/`, `AGENTS.md`, or `CLAUDE.md` changed.
- tree-sitter check when grammar, scanner, query, or EBNF changed.
- Background-execution prohibition.
- Label cleanup policy.

Record skipped checks and reasons in the PR description.

## Documentation And Changelog

- Update English user documentation for user-visible behavior.
- Otherwise record why no documentation update is needed.
- User-visible changes use `changelog.d/<issue>-<slug>.md`.
- Do not edit `CHANGELOG.md` directly.

## Knowledge Update

Update a rule or skill when work reveals:

- A reusable implementation constraint.
- A new rejection-test requirement.
- A rejected design alternative worth preserving.
- A non-obvious command recovery.
- A recurring review pattern.

Use `/knowledge-md-management` when no destination exists.

## Commands

```bash
./.claude/skills/pre-commit-checklist/run-tests.sh
./.claude/skills/pre-commit-checklist/run-asan.sh
./.claude/skills/pre-commit-checklist/run-tsan.sh
./.claude/skills/pre-commit-checklist/run-static-analysis-all.sh
./.claude/skills/pre-commit-checklist/run-rust-lint.sh
./.claude/skills/pre-commit-checklist/run-prompt-refs-lint.sh
./.claude/skills/pre-commit-checklist/run-fuzz.sh
./.claude/skills/pre-commit-checklist/run-tree-sitter.sh
```

- Use script `--clean` options for build-tree cleanup.
- Sanitizer wrappers supply required runtime options; use them instead of hand-written ASan/UBSan invocations.
- Fix test, sanitizer, race, clang-tidy, cppcheck, Rust lint, and prompt-reference failures.
- `scan-build` and Ry self-tests under TSan are advisory; investigate every new finding.
- Fuzzer crashes follow `/triage-side-finding`; preserve crash inputs in regression and corpus directories.

## Completion

- Confirm no background or detached process was started.
- Do not change labels during self-verification.
- Post-merge `wip` cleanup occurs only during a user-invoked `/git-finalize-pr`.
