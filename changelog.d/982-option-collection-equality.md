### Fixed

- `Option<List<T>>`, `Option<Map<K, V>>`, and `Option<Set<T>>` equality no
  longer returns a false-positive `true` when inner collections share a byte
  prefix; inner values are now compared element-wise. (#982)
