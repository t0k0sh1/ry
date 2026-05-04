### Added

- `@native` stdlib functions imported with `from <module> import <name>` can now be
  used as **first-class function values**: bound to variables (`let f = toInt`),
  passed to higher-order functions (`xs.map(toInt)`), and forwarded through
  user-defined `fn(...) -> R`-typed parameters. Internally, the codegen
  materializes a single internal LLVM thunk per name (cached) that forwards
  through the existing native dispatch chain, so both bare `@native` and
  `@native("libname")` declarations work identically. Materialization rules:
  (a) names with **multiple overloads** (e.g. `toStr` over `int`/`float`/`bool`,
  most `math` custom-emitter natives like `abs`/`pow`/`round`/`log`) are
  rejected with `ambiguous reference to @native function 'X': multiple overloads
  exist; wrap in a lambda to select one`; (b) names with **default arguments**
  (e.g. `startsWith(haystack, needle, ignoreCase=false)`) materialize at
  full arity — the resulting binding requires every parameter; the
  default-omission shortcut is only available on the original direct call.
  User-defined `fn` declarations continue to take precedence on name conflict
  (the new path activates only when the user-fn lookup misses). (#1569)
