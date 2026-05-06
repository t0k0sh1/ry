### Added

- Imported the tree-sitter grammar from the standalone `tree-sitter-ry`
  repository into this repository under a new editor-agnostic layout:
  `docs/grammar.ebnf` is now the canonical grammar specification (single
  source of truth) and `editor/tree-sitter/` holds the tree-sitter
  implementation (`grammar.js`, `src/scanner.c`, `queries/highlights.scm`,
  `tree-sitter.json`, `build.sh`, `install.sh`). Generated artifacts
  (`parser.c`, `grammar.json`, `node-types.json`, runtime headers,
  `bindings/`) are reproducible via `editor/tree-sitter/build.sh` and are
  excluded from version control. (#1614)
