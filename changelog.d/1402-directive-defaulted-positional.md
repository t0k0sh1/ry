### Changed

- Defaulted parameters of user-defined `@directive` declarations may now be passed positionally in declaration order, in addition to the existing named-argument and omitted (default-value) forms. For example, given `fn logged(label: str = "info")`, all of `@logged("warn")`, `@logged(label="warn")`, and `@logged()` are now accepted. Previously the positional form was rejected with "accepts at most 0 positional argument(s)". Built-in directives (`@native`, etc.) are unaffected. (#1402)
