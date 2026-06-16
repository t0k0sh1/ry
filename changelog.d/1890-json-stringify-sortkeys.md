### Changed

- `json.stringify` and `json.stringifySafe` now accept a `sortKeys: bool` named argument (default `false`). When `sortKeys=true`, `Map<str, any>` entries — including nested ones — are emitted in byte-lexicographic key order, equivalent to the removed `stringifySorted` / `stringifySortedSafe` functions. The named argument composes with the existing optional `indent` positional, e.g. `stringify(m, sortKeys=true)`, `stringify(m, 2, sortKeys=true)`, `stringifySafe(m, sortKeys=true)`. (#1890)

### Removed

- **Breaking change**: `json.stringifySorted` and `json.stringifySortedSafe` are removed. Migrate `stringifySorted(v)` → `stringify(v, sortKeys=true)` and `stringifySortedSafe(v)` → `stringifySafe(v, sortKeys=true)`; the optional `indent` positional argument still precedes `sortKeys` (`stringifySorted(v, 2)` → `stringify(v, 2, sortKeys=true)`). (#1890)
