### Added

- Shared setup in `@describe`: variables declared in a describe function body are automatically captured by inner `@it` functions (#635)
- Nested `@describe` output indentation: test output is now indented proportionally to nesting depth (#635)

### Fixed

- `ExpectStmt` was not scanned during free-variable analysis, preventing closure capture of variables referenced in `expect(x).to_eq(...)` assertions inside nested `@it` functions (#635)

### Changed

- `describe()` and `it()` lambda call syntax is deprecated; use `@describe("name")` and `@it("name")` directives on named functions instead (#635)
