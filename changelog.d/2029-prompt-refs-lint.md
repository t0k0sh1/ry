### Added

- `scripts/check-prompt-refs.sh` — a reference-integrity lint for the prompt /
  instruction definition files (`.claude/**/*.md`, `AGENTS.md`, `CLAUDE.md`),
  wired into the CI `lint` job. It fails on (a) inline-code paths that do not
  exist on disk, (b) `/<name>` slash-command references with no matching
  `.claude/skills/<name>/SKILL.md`, and (c) `KNOWLEDGE.md` section references
  whose heading is not present verbatim in `KNOWLEDGE.md`. Detection is
  inline-code-span only — fenced blocks, plain prose, and `<...>` placeholders
  are the escape hatch. A local mirror
  (`.claude/skills/pre-commit-checklist/run-prompt-refs-lint.sh`,
  `/pre-commit-checklist` §3.5.7) and a path-scoped rule
  (`.claude/rules/prompt-reference-integrity.md`) accompany it. (#2029)

### Fixed

- Swept the #1827 parser/lexer path drift through the prompt definitions:
  ~20 stale inline-code paths (the old flat `src/parser*.cpp` /
  `include/ry/parser.hpp` forms updated to their current subdirectory
  locations under `src/parser/`, `src/lexer/`, `include/ry/parser/`,
  `include/ry/diagnostic/`, and `editor/tree-sitter/src/`), a dead
  slash-command link in `.claude/agents/sanitizer-runner.md`, and the
  English/Japanese `KNOWLEDGE.md` "sanitizer issues" section-name mismatch in
  `AGENTS.md` that made a grep for the cited section silently miss. Also
  corrected three references to now-closed issues (two of which were closed
  not-planned, not fixed) and a drifted `CHANGELOG.md` line-number citation in
  `AGENTS.md`. (#2029)
