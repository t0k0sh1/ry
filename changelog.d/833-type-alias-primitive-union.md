### Fixed

- Type aliases targeting union types (e.g., `type Simple = int | str | bool`) now work correctly in variable annotations, function parameters, and function return types. Previously the compiler reported `annotation 'Simple' does not match expression type` because the union check examined the unresolved alias name instead of its target (#833)
