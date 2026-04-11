### Fixed

- `list[i] = v`, `m[k] = v`, and their compound forms now release the
  previously-held value before storing the new one when the element type
  is itself an ARC-managed collection (`List<List<T>>`, `List<Map<K,V>>`,
  `Map<K, List<V>>`, `List<Set<T>>`, and nested combinations). Previously
  every overwrite leaked the prior inner collection's heap allocation.
  The fix is safe under self-assignment (`xs[i] = xs[i]`) and cross-slot
  copy (`xs[i] = ys[j]`) by retaining the new value before releasing the
  old one. (#855)
