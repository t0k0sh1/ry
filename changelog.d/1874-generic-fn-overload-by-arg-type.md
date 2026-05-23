### Added

- Generic user-defined functions can now be overloaded by argument
  type. Multiple `fn name<T>(...)` templates with the same name are
  allowed as long as their parameter signatures differ in arity or in
  concrete argument types, and the compiler picks the matching template
  at each call site via a two-pass resolution mirroring `@native`
  dispatch: Pass 1 requires exact type match, Pass 2 (only when Pass 1
  yields zero matches) accepts the widening conversions `u8 → int`,
  `u8 → float`, and `int → float` at top-level parameter positions.
  Nested element positions (`List<T>` / `Map<K, V>` / `Set<T>` / tuples
  / function types) stay exact regardless of pass. Ambiguous matches in
  either pass and no-match across the overload set produce dedicated
  diagnostics naming the function. Templates whose parameter signatures
  normalize identically after rewriting type variables to positional
  `__T0`, `__T1`, ... are rejected at declaration time as duplicates,
  catching alpha-equivalent redeclarations such as `fn id<T>(x: T)` /
  `fn id<U>(x: U)` that previously caused silent shadowing. Single-
  declaration code is unchanged. (#1874)
