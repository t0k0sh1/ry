### Changed

- Directive invocation syntax is now generalized: all directives use a unified argument model supporting positional arguments, named arguments, and mixed forms (e.g. `@it("description")`, `@describe("group")`, `@property(count=100)`)
- Built-in directive signatures are now defined in a registry (`DirectiveSignature`) with allowed argument shapes and target kinds, enabling consistent validation and future user-defined directives (#663)
