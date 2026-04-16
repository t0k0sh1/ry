### Added

- `runtime_internal.arc_live_count() -> int` — test-only introspection function that returns the running balance of ARC header allocations minus frees. Enables delta-based leak assertions in Ry spec tests without relying on LSan (#859)
