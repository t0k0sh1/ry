### Fixed

- A single-expression lambda with an explicit `-> Unit` return type whose body is a Unit-returning call (e.g. `(p: int) -> Unit => store(p)`) no longer crashes the compiler. The single-expression codegen path returned the body's value unconditionally, but a Unit-returning call produces no value, so codegen dereferenced a null value and segfaulted; it now emits `ret void` for a Unit-typed body and rejects a non-Unit single-expression body as a type error (matching `return <expr>` in a Unit function). (#2421)
