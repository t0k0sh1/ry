### Fixed

- Generic function type inference now succeeds when the type parameter
  appears inside a container type in the declared parameter. `List<T>`,
  `Map<K, V>`, `Set<T>`, tuples `(T, T)`, and function types
  `function(T) -> T` now infer their type arguments from the call site,
  including nested combinations and cross-parameter unification. Previously
  calls such as `first_of([1, 2, 3])` for
  `function first_of<T>(xs: List<T>)` failed with
  "could not infer type parameter 'T'" even though the shape was
  unambiguous. The existing `name[T](args)` explicit syntax continues to
  work for cases where inference cannot determine the type (e.g., empty
  containers) (#823).
