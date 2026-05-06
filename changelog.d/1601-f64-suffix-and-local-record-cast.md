### Fixed

- `f64` literal suffix now resolves correctly inside any named-function
  body. Previously, `x = 3.14f64` or `1.5_0f64` inside a `fn`/`@it`/`@describe`
  body (including module-level `fn`) raised `unknown type: f64`, even
  though the same code worked at module top level and inside lambda
  bodies. The root cause was a missing `f64` entry in `resolveType`'s
  primitive type table; the `FloatExpr` lambda return-type inference
  pre-pass then fell through to the `unknown type` error before any
  body statement could emit. (#1601)
- Locally-declared `record` types are now resolvable as `as`-cast
  targets inside named-function bodies. Previously, declaring a
  `record` inside a `fn` body and then using `value as <RecordName>`
  raised `unknown type: <RecordName>` because the lambda return-type
  inference pre-pass ran before the body emit loop registered the
  record into `record_types_`. The pre-pass now uses a permissive
  type lookup with a safe fallback; the strict fatal lookup at body
  emit time is unchanged, so genuinely unknown cast targets are still
  diagnosed. (#1601)
- Migrated the three deferred test files from #1599
  (`numeric_literal_suffix.test.ry`, `numeric_underscore_separator.test.ry`,
  `operator_overload.test.ry`) from the deprecated lambda form
  `describe("...", ():` / `it("...", ():` to the canonical
  `@describe("...")` / `@it("...")` named-function form. (#1601)
