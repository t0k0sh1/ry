### Fixed

- Top-level `let` bindings and `@const` declarations are now visible from any top-level function defined after them in the same source file. This includes reads and field access for all types — primitives, strings, lists, maps, sets, records, enums, and option/result values. Previously any such reference produced `undefined variable` at codegen (#817)

### Changed

- Assigning to a top-level mutable `let` from inside a function now writes through to the top-level binding instead of silently shadowing it with a new local. Code that relied on the old shadowing behavior must rename the inner variable explicitly (#817)
