### Fixed

- `Result<Collection, E>` and `Result<_, Collection>` equality now performs element-wise comparison of the inner collection instead of raw `strcmp` on collection header bytes (#985).
