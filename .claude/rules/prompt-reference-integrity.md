# Prompt / Instruction Reference Integrity

- Ensure each skill's `allowed-tools` covers every command prescribed by its body.
- After editing agent frontmatter, validate the YAML; prompt-reference lint does not validate YAML syntax.

### Inline-code references in prompt files are CI-linted — keep paths and `/<name>` skill links resolvable

**Tags**: tooling, ci, lint, references, drift, prompt-definitions

#1827 moved the parser / lexer sources into subdirectories but the rename was never swept through `.claude/` prompt definitions — leaving ~20 stale inline-code paths and a dead slash-command link. `scripts/check-prompt-refs.sh` (wired into the CI `lint` job by #2029) now mechanically blocks that drift class.

**Rule**: across `.claude/**/*.md`, `AGENTS.md`, and `CLAUDE.md`, every
**inline-code** (single-backtick) token is held to:

- a repo path (a token beginning with a tracked top-level dir such as
  `src/`, `include/`, `tests/`, `share/`, `crates/`, `editor/`, `docker/`,
  `scripts/`, `.github/`, `.claude/`, `docs/`; the authoritative prefix
  list lives in `scripts/check-prompt-refs.sh`) must exist on disk;
- a `/<name>` slash-command must have a matching
  `.claude/skills/<name>/SKILL.md` (or be a known built-in — extend
  `BUILTIN_CMDS` in the script; filesystem-path tokens go in
  `NON_SKILL_SLASH`).

**Escape hatch (intentional, documented)**: the lint inspects inline-code
spans ONLY — triple-backtick fenced blocks and plain prose are NOT
scanned. To legitimately reference something that does not (yet) exist,
put it in a fenced block or plain prose (no inline backticks), or use a
`<...>` placeholder / a glob (`*`). A `:line` / `::symbol` suffix is
stripped before the existence check.

**How to apply**:

- Run locally before pushing prompt-file changes:
  `.claude/skills/pre-commit-checklist/run-prompt-refs-lint.sh`
  (`/pre-commit-checklist`).
- When code moves, sweep the prompt files in the same PR
  (`/horizontal-sweep`); the lint is the completeness oracle — re-run it
  to zero.
- Bare filenames with no directory prefix (e.g. `scanner.c` shorthand in
  the tree-sitter docs) are intentionally NOT linted: they name a file,
  not a location. Prefer the full path for new references.

**#1827 path map** (old → current; written in a fenced block so the lint
does not flag the old forms — this is the escape hatch in action):

```text
src/parser.cpp            ->  src/parser/parser.cpp
src/parser_decl.cpp       ->  src/parser/parser_decl.cpp
src/parser_expr.cpp       ->  src/parser/parser_expr.cpp
src/lexer.cpp             ->  src/lexer/lexer.cpp
include/ry/parser.hpp     ->  include/ry/parser/parser.hpp
include/ry/diagnostic.hpp ->  include/ry/diagnostic/diagnostic.hpp
src/scanner.c             ->  editor/tree-sitter/src/scanner.c
```

See also `.claude/rules/docs-reference-conventions.md` (the `docs/` /
README analogue, including the `<...>` placeholder convention) and
`.claude/skills/horizontal-sweep/SKILL.md`.
