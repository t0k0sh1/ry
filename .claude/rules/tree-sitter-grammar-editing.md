---
paths:
  - "editor/tree-sitter/grammar.js"
  - "editor/tree-sitter/src/scanner.c"
  - "editor/tree-sitter/queries/*.scm"
---

# tree-sitter Grammar Editing

- Read `editor/tree-sitter/README.md` for the canonical spec relationship, build, install, corpus tests, smoke check, live-editing tolerance, brace-newline tolerance, and contributor workflow.
- Scanner:
  - Keep `scanner.c` `enum TokenType` and `grammar.js` `externals` in identical ordinal order; update the scanner header comment when tokens change.
  - After consuming a non-zero-width external token, call `mark_end()`.
  - Early-return guards must use the requested `valid_symbols` set, not initial lookahead.
  - Implement brace-newline tolerance at grammar level, not by tracking bracket depth in the scanner.
- Queries:
  - `(node_name)` matches a named node; `"literal"` matches an anonymous source literal.
  - Use unquoted field labels in `field_name: (node)`; quote only literal source tokens.
- Run applicable tree-sitter checks.

### Prefer internal `token(seq(...))` over an external scanner for multi-line delimited literals

**Source**: #1843 (2026-06-16, implementation — advisor reconciliation)
**Tags**: tree-sitter, external-scanner, internal-token, multiline-literal, case-arm, glr, blind-spot

**Context**: #1843 added triple-quoted block strings `"""..."""`. The first attempt declared an external token (`_block_string_literal`) and routed `"` lookahead through `scanner.c` because the existing `string_literal` rule was `[^"\\\n]`-restricted and "atomic regex can't span newlines" felt like an axiom. Wiring the matching named rule into `_literal` (or `_primary_expression`) then broke `case "x":` arms — the parser rejected the `:` after the scrutinee — and reverting the wiring dropped block-string parsing entirely. The real culprit was the external scanner: when called at the scrutinee position with `_block_string_literal` speculatively in `valid_symbols`, scan_block_string would `advance()` one `"`, fail the second-char check, and return false; the GLR state then mis-correlated the rollback against `case_match_statement`'s scrutinee/`:` boundary even though the post-rollback internal lexer matched `"x"` correctly. An isolation experiment (`(void)block_string_possible` to inert the scanner while leaving the rule in `_literal`) parsed cleanly — confirming the scanner, not the rule, regressed the smoke run.

Switching to an internal `token(seq('"""', repeat(choice(/[^"\\]/, /\\./, /"[^"]/, /""[^"]/)), '"""'))` resolved both problems: longest-match makes `"""hello"""` beat `string_literal`'s `""` prefix, while `"hello"` and `"x"` fail the `"""` start instantly and fall through to `string_literal` without any rollback game. No external scanner branch, no GLR perturbation. `./editor/tree-sitter/check.sh` rose from `pass=158` to `pass=159` (block string spec now fully parsed; not in `expected-fail.txt`).

**Rule**: when adding a multi-line delimited literal with no interpolation (block strings, raw strings, here-docs, etc.), reach for `token(seq(...))` BEFORE the external scanner. The `[^"\\\n]` restriction in tree-sitter's existing single-line strings is a *choice*, not a tree-sitter constraint — `token()` regexes span newlines whenever the body alternative permits them. Only escalate to an external scanner when the literal needs context that regex cannot express (f-string interpolation boundaries, indentation tracking, etc.).

**How to apply**:

- Single body shape, no interpolation: internal `token()`. Pattern: `seq(open_delim, repeat(choice(/[^delim_first_char\\]/, /\\./, ...escape_for_partial_delim)), close_delim)`. Use `prec` on the token if it conflicts with a sibling literal.
- After wiring into `_literal` / `_primary_expression`, run `./editor/tree-sitter/check.sh` and confirm the `pass=` count went UP by the new feature's spec count (or held steady if no new spec covers the feature). A drop is a real regression — investigate the scanner / state-machine interaction before considering `expected-fail.txt`.
- Reference site: `editor/tree-sitter/grammar.js::block_string_literal` (#1843).
