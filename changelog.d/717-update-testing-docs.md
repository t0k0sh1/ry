### Changed

- `docs/reference/directives.md` testing-related code examples now
  declare an explicit `from testing import ...` line at the top of
  each `@each` / `@property` / `@it` / `@describe` block, matching
  the codegen enforcement introduced in #715 (`expect` / `mock` /
  `verify` / `fail`) and #716 (`@it` / `@describe`). Previously the
  prose stated the imports were required but the example bodies
  omitted them, so the concrete examples (Basic / Composed / Shared
  setup / Nested) would have been rejected by codegen for missing
  imports. Each block lists only the names it actually uses
  (per-block tailored, including non-codegen-enforced names like
  `each` / `property` for pedagogical consistency), matching the
  convention already in `docs/reference/testing.md`. The "Syntax:"
  templates use placeholder bodies (`# test body`, `# assertions`)
  that do not parse on their own; converting those templates to
  runnable examples is tracked in #1629. (#717)
