### Added

- The strict-any rule framework that produces `[strict-any/<rule>]` diagnostics for unsafe `any` patterns, and the first rule `any-arithmetic`: direct binary `+`/`-`/`*`/`/`/`%`/`//`/`**` and unary `-` on an `any`-typed operand is rejected with a hint to annotate the operand type or use `asType[T](...)` to recover a concrete value first. The opt-in entry points (`--strict-any` CLI flag, `RY_STRICT_ANY` env var) were removed in the same release once #2322 promoted the rule set to the default. (#2319)
