### Fixed

- `docs/reference/directives.md` testing-related "Syntax:" templates
  (`@each`, `@property`, `@it`, `@describe`) no longer use placeholder
  bodies (`# test body`, `# assertions`), placeholder type names
  (`param1: type`), placeholder argument tuples (`(arg1, arg2, ...)`),
  or undefined function references (`makeInputs()`) that the parser
  rejected. All five affected blocks are now runnable examples with
  concrete types, values, and `expect(...)` bodies, matching the
  convention already established in `docs/reference/testing.md`.
  Each updated block also adds `expect` to its `from testing import`
  line. The `@each` w/ function-call block now defines a small
  `fn makeInputs() -> List<(int, int)>` helper inline so the
  function-call-as-argument lesson stands on its own. Companion to
  #717, which addressed the codegen-import side of the same drift.
  (#1629)
