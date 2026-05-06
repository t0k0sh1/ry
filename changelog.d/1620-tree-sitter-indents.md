### Added

- Added `editor/tree-sitter/queries/indents.scm` with the
  nvim-treesitter rewrite capture vocabulary (`@indent.begin` /
  `@indent.branch`) so Neovim 0.12+ users get tree-sitter driven
  auto-indent / auto-dedent for `.ry` files: `<CR>` after `fn foo():`
  / `if cond:` bumps +1 indent, `else` / `else if` on its own line
  dedents to the parent `if`, and `]` / `}` / `)` on its own line
  returns to the opener's column. Multi-element tuples, list / map /
  set literals, and call / index argument lists are also handled.
  Known limitation: a parenthesized single expression spanning multiple
  lines (e.g. `s = (\n  1\n  + 2\n)`) is parsed via the hidden
  `_parenthesized` grammar rule, which tree-sitter inlines into the
  parent and cannot be matched by capture queries — contents are not
  bumped and the closing `)` does not auto-dedent.
  `editor/tree-sitter/install.sh` now also deploys `indents.scm` to
  `$XDG_CONFIG_HOME/nvim/queries/ry/indents.scm`. Enable per-buffer with
  `vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"`.
  (#1620)
