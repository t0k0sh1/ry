### Added

- Tree-sitter grammar (`editor/tree-sitter/grammar.js`) now parses the
  `[T]` generic call syntax: `load[int]("42")`,
  `mapHas[str, int](m, "a")`, `load[Map<str, any>]("{}")`, and
  arbitrarily nested type arguments such as
  `load[Map<str, List<Foo>>](text)`. The `call_expression` rule gains
  an optional `type_arguments` field that consumes the existing `_type`
  rule, so the `function` / `arguments` field names and the highlights
  query bindings are preserved. Corpus regressions covering the four
  shape categories are added to
  `editor/tree-sitter/test/corpus/expressions.txt`, and
  `tests/spec/json.test.ry` and
  `tests/spec/collection_element_metadata.test.ry` are dropped from
  `editor/tree-sitter/expected-fail.txt` now that they parse cleanly.
  This closes the editor-tooling parity gap introduced when the C++
  parser added the syntax in #1887; runtime and typecheck behavior is
  unchanged. (#1906)
