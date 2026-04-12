### Fixed

- `install.sh` now fails with a clear error when the release archive does not contain a standard library at `share/std` or `lib/std`, instead of silently installing a broken `ry` that crashes at runtime (PR #901 review)
- `arc_alloc` now guards `ARC_HEADER_SIZE + data_size` against integer overflow via `__builtin_add_overflow`, preventing an undersized heap allocation followed by out-of-bounds writes if `data_size` is near `SIZE_MAX` (PR #901 review)
