### Changed

- String elements inside collections (`List`, `Set`, `Map`, `Array`, `Tuple`, record) are now wrapped in double quotes when displayed via `print()` or `to_str()`, following Rust's debug display convention. Empty strings are now visible: `[""]` instead of `[]` (#756)
