---
name: pre-commit-checklist
description: Select applicable self-verification checks before declaring work complete or publishing changes.
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

Also evaluate when relevant:

- Rules / skills update.
- Rust lint when `crates/` changed.
- Prompt-reference lint when `.claude/`, `AGENTS.md`, or `CLAUDE.md` changed.
- Export-run-logs JSONL schema test when `scripts/export-run-logs.sh`, `tests/scripts/`, or the `__ry_test_summary` output format in `src/test_runtime.cpp` changed.
- Examples check when `examples/` or `scripts/check-examples.sh` changed.
- tree-sitter check when grammar, scanner, query, or EBNF changed.
- Label cleanup policy.

Record skipped checks and reasons in the PR description.

## Documentation And Changelog

- Update English user documentation for user-visible behavior.
- Otherwise record why no documentation update is needed.
- User-visible changes use `changelog.d/<issue>-<slug>.md`.
- Do not edit `CHANGELOG.md` directly.

## Knowledge Update

Update a rule or skill when work reveals a reusable constraint, a new rejection-test requirement, a rejected design alternative, a non-obvious command recovery, or a recurring review pattern. Create a new path-scoped rule (`.claude/rules/<topic>.md` with a `paths:` frontmatter) when no existing destination fits.

## Commands

```bash
./.claude/skills/pre-commit-checklist/run-tests.sh
./.claude/skills/pre-commit-checklist/run-asan.sh
./.claude/skills/pre-commit-checklist/run-static-analysis-all.sh
./.claude/skills/pre-commit-checklist/run-rust-lint.sh
./.claude/skills/pre-commit-checklist/run-prompt-refs-lint.sh
./.claude/skills/pre-commit-checklist/run-export-run-logs-tests.sh
./.claude/skills/pre-commit-checklist/run-examples-check.sh
./.claude/skills/pre-commit-checklist/run-fuzz.sh
./.claude/skills/pre-commit-checklist/run-tree-sitter.sh
```

- Use script `--clean` options for build-tree cleanup.
- Sanitizer wrappers supply required runtime options; use them instead of hand-written ASan/UBSan invocations.
- Fix test, sanitizer, race, cppcheck, Rust lint, and prompt-reference failures.
- Fuzzer crashes follow `/triage-side-finding`; preserve crash inputs in regression and corpus directories.

## Completion

- Do not change labels during self-verification.
- Post-merge `wip` cleanup occurs only during a user-invoked `/git-finalize-pr`.
