### Changed

- `json.keys()` now returns `Result<List<str>, Error>` instead of `List<str>`, with proper null-pointer handling for OOM and non-object inputs (#599)
