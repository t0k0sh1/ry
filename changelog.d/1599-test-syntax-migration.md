### Changed

- Migrated 167 of 170 `tests/spec/*.test.ry` files from the deprecated
  lambda form `describe("...", ():` / `it("...", ():` to the canonical
  `@describe("...")` / `@it("...")` named-function form. Without this
  migration, `./build/ry test -p` emitted 340 deprecation warnings
  (newly visible after the warning-flush fix in #1424). Three files —
  `numeric_literal_suffix.test.ry`, `numeric_underscore_separator.test.ry`,
  and `operator_overload.test.ry` — could not be migrated because the
  tests they contain expose a separate parser/codegen bug: `f64` literal
  suffix and locally-declared `record` types fail to resolve inside any
  named-function body (including module-level `fn`), while resolving
  correctly inside a lambda body. These three files remain in the
  lambda form and are tracked for migration once the underlying bug is
  fixed (#1601). The lambda parser and codegen paths themselves are
  scheduled for removal in #1602 once #1601 lands. (#1599)
