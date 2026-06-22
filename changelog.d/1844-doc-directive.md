### Added

- `@doc("...")` built-in directive for attaching Markdown documentation strings to declarations. Accepts a single string argument — single-line (`"..."`) or triple-quoted block string (`"""..."""`) — and applies to `fn` / `async fn`, `record`, record fields, `enum`, `type` aliases, and `@directive fn` declarations. The compiler does not parse Markdown; `@doc` preserves the body as metadata for future tooling. Empty strings (`@doc("")`) are accepted; `@doc` is rejected on `for` loops, function-call statements, and enum variants. Tree-sitter highlights `@doc` payloads with the `@string.documentation` capture (falling back to `@string` when the editor lacks the predicate). (#1844)

### Changed

- Stacking the same directive twice on one declaration is now rejected — e.g. `@public @public fn ...` or `@deprecated @deprecated fn ...` raise `duplicate directive '@<name>' on the same declaration`. The rule applies uniformly to every builtin and user-defined directive. (#1844)

### Fixed

- `ry fmt` no longer mistakes `#`-prefixed lines inside triple-quoted block strings (e.g. Markdown headings in a `@doc` body) for source comments. The comment extractor now tracks `"""..."""` state across line boundaries so block-string content survives round-trip formatting unchanged. (#1844)
