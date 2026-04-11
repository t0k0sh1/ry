### Fixed

- `+` applied to `Map` or `Set` operands now produces a clear error that names the actual collection type instead of the misleading `"operator '+' not supported between str and non-str types"` message. Mixed cases such as `List<int> + Map<str, int>` also name both operand types. (#863)
