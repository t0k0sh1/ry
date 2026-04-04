### Changed

- `print()` now delegates to `to_str()` for all type formatting, ensuring consistent output between `print()`, `to_str()`, and f-string interpolation (#616)

### Fixed

- `to_str()` on ADT enums with associated data now correctly formats all field types (previously only supported int, float, str, bool) (#616)
