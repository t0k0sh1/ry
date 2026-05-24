### Added

- Added three additive companion functions to `json.stringify`:
  - `stringifySafe(value: any) -> Result<str, Error>` and
    `stringifySafe(value: any, indent: int) -> Result<str, Error>` —
    same encoding as `stringify`, but inputs that would otherwise
    panic (non-finite floats, typed collections wrapped as `any`,
    `Set` / record / enum tags) return `Err(Error{message})` so
    callers can recover.
  - `stringifySorted(value: any) -> str` and
    `stringifySorted(value: any, indent: int) -> str` — emits
    `Map<str, any>` keys (including nested ones) in
    byte-lexicographic order so output is reproducible across runs
    that build the same logical map via different insertion
    sequences. Panic semantics match `stringify`.
  - `stringifySortedSafe(value: any) -> Result<str, Error>` and
    `stringifySortedSafe(value: any, indent: int) -> Result<str, Error>` —
    sorted-key output combined with the `Err`-on-unsupported-input
    behavior of `stringifySafe`.

  The existing `stringify` API is unchanged: signature, insertion-order
  iteration, and panic-on-unsupported-input behavior are all preserved
  for backwards compatibility. (#1853)
