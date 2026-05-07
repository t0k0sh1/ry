---
name: tree-sitter-grammar-editing
description: Pitfalls and verification recipes for editing the in-tree tree-sitter grammar (`docs/grammar.ebnf`, `editor/tree-sitter/grammar.js`, `editor/tree-sitter/src/scanner.c`, `editor/tree-sitter/queries/*.scm`). Covers the externals enum-order invariant, `mark_end` semantics for non-zero-width tokens, the `valid_symbols` early-return pattern (not lookahead), highlights.scm named-node vs anonymous-literal pitfalls, and verification with `tree-sitter parse` / `tree-sitter parse -d normal` / `tree-sitter query`. Use when modifying the grammar, the external scanner, or the highlight queries — トリガー: tree-sitter, scanner.c, mark_end, valid_symbols, externals, highlights.scm, グラマー編集, 外部スキャナ, ツリーシッター.
allowed-tools: Bash
---

# tree-sitter Grammar Editing

Reference for the recurring pitfalls when editing the in-tree tree-sitter
grammar at `editor/tree-sitter/`. Cross-reference: build/install workflow
in `editor/tree-sitter/README.md` §Contributor workflow, and pre-commit
verification in `/pre-commit-checklist` §3.6.5.

---

### `docs/grammar.ebnf` is the canonical spec — propagate edits in order

**Source**: PR #1619 (in-tree import), `editor/tree-sitter/README.md:6-10`
**Tags**: tree-sitter, ebnf, grammar, propagation, single-source-of-truth

**Rule**: `docs/grammar.ebnf` is the single source of truth for Ry syntax.
`editor/tree-sitter/grammar.js` is one *implementation* of that spec; future
LSP servers / alternative parsers under `editor/<tool>/` will be others.
When you change the language surface, propagate edits in this order:

1. `docs/grammar.ebnf` — update the canonical EBNF first.
2. `editor/tree-sitter/grammar.js` — mirror the change in the tree-sitter
   grammar.
3. `editor/tree-sitter/src/scanner.c` — only if the change touches an
   external token (block tokens, f-string segments, regex literal).
4. `editor/tree-sitter/queries/highlights.scm` (and `indents.scm`) — add
   queries for any new named nodes you introduced.

**How to apply**: If a PR touches `grammar.js` or `scanner.c` without a
matching `docs/grammar.ebnf` change, treat it as suspicious — either the
EBNF needs updating, or the grammar.js change is decoration-only. Do
not invert the order; editing grammar.js first and "back-porting" to the
EBNF leads to the EBNF silently lagging implementation.

---

### Externals enum order in scanner.c MUST match the externals array in grammar.js

**Source**: `editor/tree-sitter/src/scanner.c:1-26, 35-43`,
`editor/tree-sitter/grammar.js:62-70`
**Tags**: tree-sitter, externals, scanner, ordinal-index, invariant

**Rule**: tree-sitter matches external tokens by **ordinal index**, not by
name. `enum TokenType` in `scanner.c:35-43` and the `externals: $ => [...]`
array in `grammar.js:62-70` must have entries in identical order. Adding,
removing, or reordering an entry in one file requires the matching change
in the other in the same commit.

Current canonical order (v0.0.17):

| Index | scanner.c enum    | grammar.js externals |
|-------|-------------------|----------------------|
| 0     | `INDENT`          | `$._indent`          |
| 1     | `DEDENT`          | `$._dedent`          |
| 2     | `NEWLINE`         | `$._newline`         |
| 3     | `FSTRING_START`   | `$._fstring_start`   |
| 4     | `FSTRING_MID`     | `$._fstring_mid`     |
| 5     | `FSTRING_END`     | `$._fstring_end`     |
| 6     | `REGEX_LITERAL`   | `$._regex_literal`   |

The header comment at `scanner.c:1-26` documents this index-by-index and
must be updated when entries change.

**How to apply**: After editing either side, regenerate and run a smoke
parse to surface any swap silently:

```bash
cd editor/tree-sitter
./build.sh
printf 'x = f"hi"\n' > /tmp/_ts_externals.ry
tree-sitter parse -d normal /tmp/_ts_externals.ry 2>&1 | grep -E '_(indent|dedent|newline|fstring_(start|mid|end)|regex_literal)'
```

If the trace shows a token kind that doesn't match the source position
(e.g. `_regex_literal` where you expected `_fstring_start`), the enum
and externals array are out of sync.

**Reproducer**: temporarily swap two adjacent entries (e.g. `INDENT` and
`DEDENT` in `scanner.c`) without touching `grammar.js`, rebuild, and
observe `tree-sitter parse -d normal <any.ry>` reporting INDENT where DEDENT was
expected and vice versa — every block boundary parses backwards.

---

### Non-zero-width external tokens need a second `mark_end()` after consuming content

**Source**: `editor/tree-sitter/src/scanner.c:330-343, 410-428`
**Tags**: tree-sitter, scanner, mark_end, fstring, regex, zero-width

**Rule**: tree-sitter's external scanner API uses `lexer->mark_end(lexer)`
to mark the **end** of the current token. The scanner in `scanner.c` calls
`mark_end` at line 343 to position the boundary at the first non-blank
content character — this is correct for the zero-width block tokens
(INDENT/DEDENT/NEWLINE) and for the regex case at line 335. However any
external token that **does** consume content (the f-string opener `f"...`
in Case E, `scanner.c:417-428`) must call `mark_end` *again* after
consuming, otherwise the token retains the earlier zero-width mark and the
parser re-reads the consumed characters as if the scanner had not run.

For the f-string opener specifically, omitting the second `mark_end` (line
426) makes FSTRING_START zero-width; tree-sitter's lexer then re-reads
`f` and recognises it as an identifier, the literal `"hi"` as a string,
and the f-string is silently downgraded to `f` followed by `"hi"`.

**How to apply**: When adding a new external token that consumes content
(any case under the post-`mark_end` block in `scanner.c:344+`), end the
case with `lexer->mark_end(lexer); return true;`. Do not assume the
earlier `mark_end` at line 343 is still valid — it isn't, because
`advance(lexer)` calls between then and now have moved the cursor without
re-marking.

**Reproducer**: comment out the `mark_end` at `scanner.c:426`, rebuild, and run:

```bash
printf 'x = f"hello"\n' > /tmp/_ts_fstring.ry
cd editor/tree-sitter && tree-sitter parse /tmp/_ts_fstring.ry
```

Without the second `mark_end`, the f-string is parsed as an identifier
`f` followed by a string literal `"hello"` (visible as separate
`identifier` and `string_literal` nodes in the CST instead of an
`f_string` node).

---

### `valid_symbols` early-return guards on requested-token set, NOT on lookahead char

**Source**: `editor/tree-sitter/src/scanner.c:287-302`
**Tags**: tree-sitter, scanner, valid_symbols, lookahead, whitespace

**Rule**: The scanner's early-return guard at `scanner.c:287-302` is keyed
on `valid_symbols[...]` — the set of token kinds the parser is requesting
at this position. It is **not** keyed on `lexer->lookahead` (the next
source character). Do not add a "fast path" that returns false based on
the current lookahead character (e.g. "if not `f` and not `/`, return
false") because the parser may call the scanner at a horizontal-whitespace
position **before** the trigger character — it is the scanner's
whitespace-skip loop at `scanner.c:309-328` that finally exposes the `f`
or `/`.

Concretely: in `x = f"hi"` the parser arrives at the space after `=` and
calls the scanner with `valid_symbols[FSTRING_START] == true`. If you
guard on `lookahead == 'f'` you return false at the space, the parser
falls back to its internal lexer, the internal lexer skips the space and
reads `f` as the start of an identifier — and the scanner is never re-
entered with FSTRING_START still requested. The same trap applies to
regex literals: `x = /pat/` enters at the space, lookahead is space, and
a lookahead-based guard would prevent the scanner from ever reaching `/`.

**How to apply**: Keep the guard exactly as it is — `valid_symbols[X]`
checks only. When adding a new external token, extend the early-return
condition with `valid_symbols[NEW_TOKEN]` (mirror the
`fstring_start_possible` / `regex_possible` pattern), never with a
lookahead-character check.

**Reproducer**: replace the guard at `scanner.c:301` with a lookahead-
based version such as:

```c
if (lexer->lookahead != 'f' && lexer->lookahead != '/' &&
    lexer->lookahead != ' ' && lexer->lookahead != '\t' &&
    lexer->lookahead != '\n' && lexer->lookahead != '\r' &&
    lexer->lookahead != '#')
  return false;
```

Then `x = f"hi"` and `x = /pat/` both fail to parse: the scanner is
called at the space, the lookahead-only guard fails to recognise the
trigger lies further along, and the f-string / regex is never lexed.

---

### highlights.scm: named-node patterns vs anonymous string literals

**Source**: `editor/tree-sitter/grammar.js:416-417`,
`editor/tree-sitter/queries/highlights.scm:48-49`
**Tags**: tree-sitter, highlights, named-node, anonymous-literal, scm

**Rule**: In tree-sitter query syntax, `(node_name)` matches a **named
node** (a rule defined as `node_name: $ => ...` in grammar.js), while
`"keyword"` matches an **anonymous string literal** that appears
verbatim in some grammar production. The two are not interchangeable:

- `break_statement` in `grammar.js:416` is a named rule whose entire body
  is the literal `'break'`. It is also an anonymous-literal site.
- `(break_statement) @keyword.repeat` matches the named node — only
  `break` tokens that the parser has wrapped into a `break_statement`
  context get the highlight.
- `"break" @keyword.repeat` matches the raw token regardless of context
  — including a `break` that might appear as a substring of an anonymous
  inline rule, or an unrelated occurrence the grammar may add later.

The same trap applies to other rules whose body is a single literal: in
this grammar that includes `break_statement`, `continue_statement`,
`boolean_literal` (`true`/`false`), `none_literal` (`None`).

**How to apply**: For these single-literal rules, prefer the named-node
form `(rule_name) @capture` (see `highlights.scm:14, 15, 48, 49`). Reserve
`"literal" @capture` for keywords that only appear inline in larger
rules (e.g. `"if"`, `"else"`, `"return"` — see `highlights.scm:36-46`)
where there is no named wrapper to target.

**How to verify**: Run a query sanity check after editing:

```bash
printf 'while true:\n  break\n' > /tmp/_ts_break.ry
cd editor/tree-sitter
tree-sitter query queries/highlights.scm /tmp/_ts_break.ry 2>&1 | grep -E 'keyword|break'
```

The output should show `break` captured as `keyword.repeat` exactly once
through the named-node match.

---

### highlights.scm: `field:` is a field-label, `"field"` is a literal-string match

**Source**: `editor/tree-sitter/queries/highlights.scm:78-91`
**Tags**: tree-sitter, highlights, field, scm, query-syntax

**Rule**: In tree-sitter query syntax, `name: (identifier) @function` uses
`name:` as a **field label** — it constrains the match to identifiers
appearing under the field named `name` of the parent node (declared via
`field('name', ...)` in grammar.js). Wrapping the same word in quotes —
`"name"` — turns it into a literal-string match against source text:
tree-sitter then matches the four-character source token `name` rather
than nodes appearing in a `name:` field.

The two are easy to confuse because both look superficially like "find
something called name", but they target completely different things —
the first is a parse-tree relationship, the second is raw source text.

**How to apply**: In query rules, do not quote the field label. The
unquoted form `field_name: (node_kind) @capture` is correct (see
`highlights.scm:78-91` for `name:` usages, `highlights.scm:88-90` for
`field:`). Quoted strings only appear when targeting an anonymous keyword
or punctuation literal (see `highlights.scm:135-160`).

---

### Verification recipes for grammar / scanner / highlights edits

**Source**: `editor/tree-sitter/README.md:89-122`,
`/pre-commit-checklist` §3.6.5
**Tags**: tree-sitter, verification, build, parse, query, recipes

**Rule**: After editing any of `grammar.js`, `src/scanner.c`, or
`queries/*.scm`, run the relevant subset of these checks before opening
a PR. All commands assume `cwd = editor/tree-sitter/`.

| Step | Command | When to run |
|------|---------|-------------|
| Build | `./build.sh` | After any grammar.js or scanner.c edit |
| Build (scanner only) | `./build.sh --no-gen` | After scanner.c-only edit (skips `tree-sitter generate`) |
| Install to Neovim | `./install.sh --no-build` | After successful build, to dogfood in Neovim |
| External-scanner trace | `tree-sitter parse -d normal <file.ry>` | When a scanner change might affect block tokens, f-string segments, or regex literals |
| CST inspection | `tree-sitter parse <file.ry>` | When a grammar.js change should produce a different parse tree |
| Highlight rule check | `tree-sitter query queries/highlights.scm <file.ry>` | When highlights.scm rules change — verifies captures fire on the expected nodes |

A minimal smoke input that exercises the scanner's f-string and regex
paths together with block tokens:

```ry
fn check():
    x = f"hi"
    if x:
        y = /pattern/
        return y
```

```bash
cd editor/tree-sitter
printf 'fn check():\n    x = f"hi"\n    if x:\n        y = /pattern/\n        return y\n' > /tmp/_ts_smoke.ry
./build.sh
tree-sitter parse -d normal /tmp/_ts_smoke.ry 2>&1 | grep -E '_(indent|dedent|newline|fstring_(start|mid|end)|regex_literal)' | head -20
```

The trace must show `_indent` / `_dedent` / `_newline` events at the
expected positions, plus `_fstring_start` / `_fstring_end` for the
f-string and `_regex_literal` for `/pattern/`. ERROR nodes against
`tests/spec/*.test.ry` are expected today — the in-tree grammar does
not yet cover the full Ry surface (tracked in #1633). What this recipe
checks is the scanner-level token stream, not full-corpus coverage.

**How to apply**: Don't commit a grammar/scanner/highlights change
without running at least the build step and a CST or query inspection
on a small input that exercises the changed rule. The pre-commit
checklist (`/pre-commit-checklist` §3.6.5) gates on
`./build.sh && ./install.sh --no-build` succeeding when any of
`docs/grammar.ebnf`, `editor/tree-sitter/grammar.js`,
`editor/tree-sitter/src/` files are in the diff.
