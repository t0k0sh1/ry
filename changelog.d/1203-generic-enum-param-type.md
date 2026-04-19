### Fixed

- Generic enums can now be used as function parameter types, return
  types, and let-binding type annotations. Both fully-qualified forms
  (`MyOpt<int>`) and type-parameter-referencing forms (`MyOpt<T>` inside
  a generic function `fn<T>`) resolve correctly (#1203).

### Changed

- Self-referential enum fields such as
  `enum Tree: Leaf(int), Node(int, Tree, Tree)` and their generic
  counterparts `enum LList<T>: Cons(T, LList<T>)` now emit a helpful
  diagnostic pointing to wrapper types (`List<...>`, `Map<K, ...>`,
  `Set<...>`) at declaration time instead of the cryptic
  `unknown type: Tree` / `unknown type: T`. Compiling a generic enum
  name without type arguments in a signature (e.g. `opt: MyOpt`)
  likewise produces a clear error asking for `MyOpt<T>` (#1203).
