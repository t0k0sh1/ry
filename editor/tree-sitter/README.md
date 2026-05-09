# tree-sitter-ry

Tree-sitter grammar for the Ry language. Lives in-tree under `editor/` so that
grammar changes can ride alongside the language changes that motivate them.

The canonical grammar specification is [`docs/grammar.ebnf`](../../docs/grammar.ebnf)
in this repository (single source of truth). `grammar.js` here is the
tree-sitter implementation of that spec; other consumers (LSP servers,
alternative parsers, additional editor plugins) may be added under sibling
`editor/<tool>/` directories in the future.

## Layout

```text
editor/tree-sitter/
├── grammar.js           # tree-sitter grammar definition (tracked)
├── src/
│   └── scanner.c        # external scanner (INDENT/DEDENT/NEWLINE,
│                        #   f-string segments, regex literals)
├── queries/
│   ├── highlights.scm   # tree-sitter highlight queries (tracked)
│   └── indents.scm      # tree-sitter indent queries (tracked)
├── tree-sitter.json     # tree-sitter CLI configuration
├── build.sh             # tree-sitter generate + build -> ry.so
├── install.sh           # copy ry.so + queries to Neovim parser dirs
├── check.sh             # smoke-check ry.so against tests/spec/**/*.test.ry
├── expected-fail.txt    # files the grammar does not yet parse (allowlist)
└── README.md            # this file
```

The following are reproducible outputs of `tree-sitter generate` / `tree-sitter build`
and are **not tracked**:

- `src/parser.c`, `src/grammar.json`, `src/node-types.json`
- `src/tree_sitter/*.h` (tree-sitter runtime headers)
- `bindings/`, `node_modules/`
- `ry.so`, `*.dylib`, `*.a`, `*.o`

The first three groups are excluded by `.gitignore` in this directory; the
binary suffixes are excluded by the repository-root `.gitignore`. If you need
any of these locally, run `./build.sh`.

## Prerequisites

- **tree-sitter CLI** ≥ 0.22 (`tree-sitter.json` config format).
  Install with `cargo install tree-sitter-cli` or via your package manager.
- **GNU gcc**. On macOS the system `cc` / `gcc` are Apple Clang shims;
  `build.sh` looks for Homebrew `gcc-15` … `gcc-12` in `PATH` and falls back
  with an error otherwise. Install with `brew install gcc`. On Linux the
  default `cc` is normally GNU gcc and no extra setup is needed.
- **Node.js** is *not* required for `tree-sitter generate` / `tree-sitter build`
  (the standalone CLI handles both). It is only needed if you want to use the
  optional Node bindings, which this in-tree copy intentionally does not ship.

## Build

```bash
./build.sh                # tree-sitter generate + build -> ry.so
./build.sh --no-gen       # skip generate (when only scanner.c changed)
./build.sh --debug        # build with -O0 -g
```

`build.sh` regenerates `src/parser.c` from `grammar.js`, then compiles
`parser.c` + `scanner.c` into `ry.so` via `tree-sitter build`. The script
honours an existing `CC` env var; otherwise it auto-detects a Homebrew GNU
gcc on macOS.

## Install (Neovim)

```bash
./install.sh              # builds ry.so first, then installs
./install.sh --no-build   # use an existing ry.so as-is
```

Installs to:

- `${XDG_CONFIG_HOME:-$HOME/.config}/nvim/parser/ry.so`
- `${XDG_CONFIG_HOME:-$HOME/.config}/nvim/queries/ry/highlights.scm`
- `${XDG_CONFIG_HOME:-$HOME/.config}/nvim/queries/ry/indents.scm`

`indents.scm` uses the nvim-treesitter rewrite (main-branch) capture
vocabulary (`@indent.begin` / `@indent.branch`), which requires
**Neovim 0.12+** with the rewrite branch of
[nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter)
installed. Enable tree-sitter-driven indentation per `.ry` buffer with:

```lua
vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
```

## Smoke-check (corpus regression test)

```bash
./check.sh                # auto-builds ry.so if missing, then checks 176 spec files
./check.sh --no-build     # use the existing ry.so as-is
./check.sh --verbose      # also list expected-fail entries that still fail (SKIP)
```

`check.sh` runs `tree-sitter parse` against every `tests/spec/**/*.test.ry`
and reports any file that surfaces an `ERROR` or `MISSING` node. Files
listed in `expected-fail.txt` are tolerated — they document grammar gaps
that have not yet been closed and are organised into named buckets
(tuple member access, generic bounds, lambda-block bodies, numeric
literal forms, …). Anything outside that list is treated as a grammar
regression: the script prints `FAIL: <path>` and exits non-zero.

If a previously failing file now parses cleanly (e.g. after a grammar
improvement), `check.sh` prints `WARN: <path> now passes; remove from
expected-fail.txt` but still exits 0 — the developer is expected to drop
the entry in the same PR that closes the gap.

`expected-fail.txt` is the **single** place where tolerated divergence is
recorded; `check.sh` has no hard-coded skip list. Inline `# reason`
comments after each path are stripped at read time and are advisory only.

## Live-editing tolerance

The tree-sitter grammar (`grammar.js`) is intentionally **more permissive**
than the canonical EBNF spec ([`docs/grammar.ebnf`](../../docs/grammar.ebnf))
for the specific block-introducing rules below. Their body content is
wrapped in `optional(...)` so that **partial input** typed during
incremental editing still produces a complete named node, allowing
`indents.scm` `@indent.begin` captures to fire.

| Grammar rule              | EBNF requirement                            | grammar.js tolerance                          |
|---------------------------|---------------------------------------------|-----------------------------------------------|
| `function_body`           | `:` then INDENT contracts? statement+ DEDENT | `:` followed by optional INDENT body          |
| `if_statement`            | `:` then mandatory `block`                  | `:` followed by optional `block` (consequence / alt_consequence / alternative); trailing `else` allows `:` itself to be missing so that bare `else` typed on its own line is absorbed into the surrounding `if_statement` |
| `while_statement`         | `:` then mandatory `block`                  | `:` followed by optional `block`              |
| `for_statement`           | `:` then mandatory `block`                  | `:` followed by optional `block`              |
| `case_match_statement`    | `:` then mandatory INDENT case_arm+ DEDENT  | `:` followed by optional INDENT arms          |
| `case_cond_statement`     | `:` then mandatory INDENT case_cond_arm+ DEDENT | `:` followed by optional INDENT arms      |

Each relaxed rule carries a `// Live-editing tolerance (#1623):` comment
in `grammar.js` pointing back to this section. The Ry compiler enforces
non-empty bodies at compile time — the tolerance is editor-level only.

The `if_statement` rule is wrapped in `prec.right(...)` because making
the trailing `else`'s `:` optional would otherwise create an ambiguity
when the parser sees `else if`: it cannot decide whether to continue
the current `if_statement`'s else-if chain or to end the `if_statement`
(with bare `else`) and start a new top-level `if`. Right-precedence
biases toward continuing the chain, which matches the expected
interpretation.

The `case_arm` and `case_cond_arm` rules **do not** receive the same
relaxation: making their bodies optional introduces a parser ambiguity
(the next-arm condition could begin with `(`, which is also a valid
expression statement that an inline body would absorb). The outer
`case_match_statement` / `case_cond_statement` relaxation is sufficient
for the most common live-editing scenario (`case x:` typed and `<CR>`
pressed); arm-level partial typing falls back to the existing block-level
`@indent.begin` capture on the surrounding case statement.

## Contributor workflow

Whenever a PR touches one of these paths (paths are relative to the
repo root), the local `ry.so` is no longer in sync with the grammar and
must be rebuilt + reinstalled before the editor experience matches the
language change:

- `docs/grammar.ebnf` — canonical EBNF spec
- `editor/tree-sitter/grammar.js` — tree-sitter grammar definition
- `editor/tree-sitter/src/` — external scanner (`scanner.c`)

The loop reuses the scripts documented in `## Build` and `## Install
(Neovim)` above (run from `editor/tree-sitter/`):

```bash
./build.sh                                 # regenerate parser.c + build ry.so
./install.sh --no-build                    # copy ry.so + queries to Neovim parser dir
```

If `tree-sitter generate` fails inside `build.sh`, the grammar
definition has a syntax error — fix `grammar.js` and rerun. If the
subsequent `tree-sitter build` step fails instead, inspect `scanner.c`.
After `install.sh` succeeds, optionally open a `.ry` file in Neovim to
eyeball the syntax highlighting and confirm no regressions.

> Running `tree-sitter parse <file>.ry` against existing
> `tests/spec/*.test.ry` will surface ERROR nodes today: the in-tree
> grammar does not yet cover the full Ry surface. The known-gap files
> are listed in `expected-fail.txt` and the Phase 1 corpus smoke-check
> (`./check.sh`, see above) treats them as silent SKIPs. Phase 2 — the
> hand-curated `tree-sitter test` corpus with S-expression assertions
> — is tracked separately in [#1633](https://github.com/t0k0sh1/ry/issues/1633).

The pre-commit version of this loop, with the same trigger paths, lives
in [`/pre-commit-checklist`](../../.claude/skills/pre-commit-checklist/SKILL.md)
§3.6.5.

Recurring pitfalls in grammar / scanner / highlights editing — externals
enum-order invariant, `mark_end` semantics for non-zero-width tokens, the
`valid_symbols` early-return pattern, and highlights.scm named-node vs
anonymous-literal traps — together with verification recipes
(`tree-sitter parse -d`, `tree-sitter query`) live in
[`/tree-sitter-grammar-editing`](../../.claude/skills/tree-sitter-grammar-editing/SKILL.md).
The same paths listed above auto-load that skill via
[`.claude/rules/tree-sitter-grammar-editing.md`](../../.claude/rules/tree-sitter-grammar-editing.md).

Other editors are not yet supported in-tree; integrations may be added under
`editor/<tool>/` as they appear.
