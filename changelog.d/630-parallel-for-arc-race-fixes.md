### Fixed

- `@parallel for` no longer corrupts captured `List` / `Map` / `Set` / `str`
  values. Worker-local ARC retain/release on captured collections now uses
  atomic operations, captured allocas are re-marked as ARC-managed inside the
  thunk, and every ARC-managed capture is retained at worker entry so the
  copy-on-write `strong_count > 1` invariant holds — preventing workers from
  mutating the shared buffer in place (which previously caused heap corruption
  under contention). (#630)
- `emitCowCheck` now uses an Acquire atomic load for `strong_count` in an
  atomic context, pairing with the `atomicrmw` retain/release and closing a
  TOCTOU race window that TSan flagged when multiple workers CoW-copied the
  same captured collection. (#630)
- `runtime_gc.cpp::collect_locked()` now reads and writes `strong_count` via
  `__atomic_load_n(ACQUIRE)` / `__atomic_store_n(RELEASE)` so garbage
  collection no longer races with concurrent ARC retain/release performed by
  `@parallel for` workers. (#630)
