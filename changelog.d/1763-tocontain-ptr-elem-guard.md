### Fixed

- Fixed undefined behavior in `expect(x).toContain(y)` and
  `expect(x).toNotContain(y)` where pointer-typed list/set elements
  were unconditionally compared with `strcmp`. Under opaque pointers
  `elemTy == ptrTy_` matches not only `List<str>` / `Set<str>` but
  also `List<List<T>>` / `List<Map<K, V>>` / `List<Set<T>>` /
  `List<fn>` / `Set<List<T>>` / `Set<Map<K, V>>` / `Set<fn>`, so the
  previous code read the bytes of a collection / closure header as a
  C string — UB that could silently report two distinct length-N
  lists as "equal" because their headers begin with the same length
  prefix. These shapes are now rejected at compile time with a clear
  diagnostic (`list element type must be int, float, str, or bool` /
  `set element type must be int, float, str, or bool`), mirroring the
  positive-allowlist guard previously applied to `toBeOneOf`
  (#1689) and `emitListRemove`. (#1763)
