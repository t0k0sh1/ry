### Fixed

- `editor/tree-sitter/grammar.js` now produces complete named nodes for
  partially-typed block-introducing statements, so `indents.scm`
  `@indent.begin` captures fire during live editing in Neovim. Before this
  change, typing `fn foo():` and pressing `<CR>` left the cursor at column
  0 because the parser wrapped the incomplete statement in `(ERROR)`,
  dropping the `function_body` field that the indent capture matches.
  After this change, the body of `function_body`, `if_statement`,
  `while_statement`, `for_statement`, `case_match_statement`, and
  `case_cond_statement` is wrapped in `optional(...)` so the prefix
  `fn foo():` / `if cond:` / `while x:` / `for x in xs:` / `case c:` /
  `case:` is a valid full sentence of the grammar — the parser commits to
  the surrounding statement node as soon as it sees the `:` and the
  capture's field predicate is satisfied. The trailing `else` clause of
  `if_statement` additionally allows its `:` to be missing, so a bare
  `else` typed on its own line is absorbed into the surrounding
  `if_statement` and the existing
  `(if_statement "else" @indent.branch)` capture dedents to the parent
  `if`'s column. The relaxation introduces a precedence ambiguity with
  `else if` (continue the chain vs. end the statement and start a new
  top-level `if`), resolved by wrapping `if_statement` in
  `prec.right(...)`. The Ry compiler continues to enforce non-empty
  bodies at compile time; the relaxation is editor-side only and
  intentionally diverges from `docs/grammar.ebnf` (canonical EBNF spec).
  `case_arm` / `case_cond_arm` were not relaxed because the next-arm
  condition can begin with `(`, which would create a parser ambiguity
  with an inline body; the outer `case_*_statement` relaxation is
  sufficient for the primary live-editing scenario. See
  `editor/tree-sitter/README.md` §"Live-editing tolerance" for the full
  table. (#1623)
