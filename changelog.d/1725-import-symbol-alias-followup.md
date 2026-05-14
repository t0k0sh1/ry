### Added

- Extended `from m import foo as bar` symbol alias to `fn`, `record`,
  `enum`, and `type alias` kinds. The module loader now generates an
  `ImportAliasStmt` AST node per non-`@const` alias, and codegen
  registers the alias under the existing function / record / enum /
  type-alias tables so every call site, type annotation, constructor,
  enum variant access, and ADT pattern match resolves transparently
  through the alias name (#1725).

- Generic-fn and generic-enum aliases are explicitly rejected at codegen
  with a "not yet supported" diagnostic; aliases for non-`@const`
  mutable globals and `@directive` definitions remain rejected by the
  module loader (#1725).
