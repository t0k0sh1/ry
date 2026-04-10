### Fixed

- Nested type aliases over union types are now fully flattened. Previously, given `type A = int | str; type B = A | bool`, declaring `x: B = 42` failed with *"type is not in union"* because the alias `A` inside the union was not expanded. `B` is now equivalent to `int | str | bool`, and overlapping members are deduplicated — so `type C = A | int` collapses to `int | str`, and `type D = B | A` (where `B` already transitively includes `A`) flattens to `bool | int | str` (#835)
