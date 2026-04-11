### Fixed

- Compiler now rejects defining a `record` and an `enum` with the same name in the same compilation unit. This also covers generic enum templates: `record Foo` and `enum Foo<T>` can no longer coexist, and duplicate generic enum declarations are rejected. Previously both declarations were accepted, leading to inconsistent type lookup. (#815)
