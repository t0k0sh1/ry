---
paths:
  - "editor/tree-sitter/grammar.js"
  - "editor/tree-sitter/src/scanner.c"
  - "editor/tree-sitter/queries/*.scm"
---

# tree-sitter Grammar Editing

This rule auto-loads when editing the in-tree tree-sitter grammar.
Full guidance lives in `.claude/skills/tree-sitter-grammar-editing/SKILL.md`
(or `/tree-sitter-grammar-editing`) — see it for the externals enum-order
invariant, `mark_end` and `valid_symbols` semantics, highlights.scm
named-vs-anonymous pitfalls, and verification recipes.

### Externals are matched by ordinal index, not name

**Source**: `editor/tree-sitter/src/scanner.c:35-43`,
`editor/tree-sitter/grammar.js:62-70`
**Tags**: tree-sitter, externals, scanner, ordinal-index, invariant

**Rule**: tree-sitter pairs `enum TokenType` entries in `scanner.c:35-43`
with the `externals: $ => [...]` array in `grammar.js:62-70` by **position
in the list**, not by name. Adding, removing, or reordering an entry on
one side requires the matching change on the other side in the same
commit, plus an update to the documenting header comment at
`scanner.c:1-26`.

A drift here is silent — the parser still builds, but every external
token is reinterpreted as a different token kind. Reproducer and full
verification steps in `/tree-sitter-grammar-editing` §"Externals enum
order in scanner.c MUST match the externals array in grammar.js".

### Live-editing tolerance for body fields

**Source**: issue #1623, `editor/tree-sitter/README.md` §"Live-editing tolerance",
`editor/tree-sitter/grammar.js` (`function_body`, `if_statement`,
`while_statement`, `for_statement`, `case_match_statement`,
`case_cond_statement`)
**Tags**: tree-sitter, grammar, live-editing, optional-body, indents.scm

**Rule**: Block-introducing statement bodies are wrapped in `optional(...)`
so partial input (`fn foo():` typed but body not yet started) still
produces a complete named node. This is what allows `indents.scm`
`@indent.begin` captures to fire during incremental editing and is the
canonical (intentional) divergence from `docs/grammar.ebnf`. Each relaxed
rule carries a `// Live-editing tolerance (#1623):` comment in
`grammar.js`. Do **not** apply the same relaxation to `case_arm` /
`case_cond_arm` — it introduces a parser conflict; the outer
`case_*_statement` relaxation is sufficient. Detailed pattern, recipes,
and ambiguity caveat live in `/tree-sitter-grammar-editing` §"Live-editing
tolerance".

### Canonical edit-propagation order

`docs/grammar.ebnf` (canonical EBNF) → `editor/tree-sitter/grammar.js`
→ `editor/tree-sitter/src/scanner.c` (only if external tokens change)
→ `editor/tree-sitter/queries/highlights.scm` (only for new named
nodes). See the skill's first entry for the rationale and the contract
that `docs/grammar.ebnf` is the single source of truth that
implementations under `editor/<tool>/` mirror.

### After editing, rebuild and inspect

Build/install workflow and verification recipes
(`tree-sitter parse -d`, `tree-sitter query`, smoke inputs covering
block tokens + f-string + regex) live in the skill and in
`editor/tree-sitter/README.md` §Contributor workflow. Pre-commit gating
is handled by `/pre-commit-checklist` §3.6.5.
