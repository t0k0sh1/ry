### Fixed

- Parser: `Ident<...>(args)` in expression position (e.g.
  `loadAs<int>("1")`) is now rejected with a dedicated diagnostic that
  directs users to the canonical `[T]` generic-call syntax
  (`f[int](x)`), instead of silently misparsing the form as a chain of
  comparison operators and surfacing a misleading
  `undefined variable: <name>` error. `Foo<T>::Variant` enum
  constructors and plain comparison chains (`a < b > c`) are
  unaffected. The runtime `loadAs[T]: ...` error-message prefix and
  the `tests/spec/json.test.ry` describe/it names are unified with the
  `[T]` user-facing notation. (#1885)
