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

Other editors are not yet supported in-tree; integrations may be added under
`editor/<tool>/` as they appear.
