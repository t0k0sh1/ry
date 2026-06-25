### Fixed

- tree-sitter grammar (`editor/tree-sitter/grammar.js`) で以下 2 件の不整合を解消し、対応する corpus entry を追加して editor (Neovim 等) の syntax highlight / indent 体験を修正:
  - `directive_def_declaration` ルールが body 必須に書かれていたため、`share/std/core/directive.ry` 等の body-less な `@directive(target=[...])\nfn name(...)` 形が ERROR ノードを発生させていた問題を修正。`@directive(...)` 末尾の NEWLINE を rule に明示し、後続 `function_declaration` の body-less 形 (既存 `choice(function_body, _newline)`) を再利用する。`test/corpus/decorators.txt` に 3 ケース (single target / multi-target / `@public` 前置) を追加して shape を lock。
  - `qualified_import_statement` ルールの `module` field が単発 `IDENT` 想定だったため、`import ry.math` / `import ry.math as m` の dotted module path 形が ERROR ノードを発生させていた問題を修正。`field('module', $.module_path)` に変更し dotted form を許容。既存 corpus 3 entry および `queries/highlights.scm` の `@module` キャプチャを追従更新。
- あわせて `editor/tree-sitter/expected-fail.txt` の housekeeping:
  - `#1618` 由来 3 entry (`arc_set_map_tuple_2226.test.ry` / `tuple_nested_generic_2264.test.ry` / `nested_fn_loop_capture.test.ry`) の triage を完了し、各 bucket の既存 grammar gap (tuple member access `.0`/`.1` / top-level `@const NAME: T = value`) と一致する旨を comment で明示。
  - 既に clean parse する `tests/spec/implicit_widening.test.ry` を expected-fail から削除。
- `./editor/tree-sitter/check.sh --verbose` で smoke `pass=169 skip=49 warn=0 fail=0` / `tree-sitter test` で corpus 119/119 pass を確認。 (#2382)
