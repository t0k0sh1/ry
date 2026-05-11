### Added

- Added symbol alias support in selective import (`from m import foo as bar`).
  The parser now accepts an optional `as <ident>` after each imported name,
  the formatter emits it round-trip, and self-alias (`foo as foo`) is
  normalized away (#1721).

  In this release only `@const` aliases are functional end-to-end; alias
  requests for `fn` / `record` / `enum` / `type alias` parse and reach the
  module loader but are rejected with a clear diagnostic pointing at
  follow-up #1725, which will extend codegen-side name resolution to make
  the remaining kinds work. (#1721)
