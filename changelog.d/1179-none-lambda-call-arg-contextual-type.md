### Fixed

- `None()` and bare `none` in lambda variable call arguments now adopt the callee parameter's `Option<T>` inner type, so `g(None())` compiles where `g: (o: Option<str>) -> Option<str>`. Previously required a typed-variable workaround. (#1179)
