### Fixed

- Compiler now rejects a `type` alias whose name collides with an existing `record`, `enum`, generic `enum`, or previously-defined `type` alias, in either declaration order. This extends the cross-category duplicate check added in #815 to type aliases (including named unions such as `type Foo = int | str`). Duplicate error messages also now point at the offending declaration instead of a stale location. (#850)
