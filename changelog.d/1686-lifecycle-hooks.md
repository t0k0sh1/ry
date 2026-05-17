### Added

- Added `@beforeEach` / `@afterEach` / `@beforeAll` / `@afterAll`
  lifecycle hook directives for the testing framework. Each hook is
  declared on a parameterless, return-typeless function inside a
  `@describe` block and runs at the corresponding point in the
  describe's lifecycle: `@beforeAll` once before the first `@it`,
  `@beforeEach` before every `@it`, `@afterEach` after every `@it`
  that completes normally, and `@afterAll` once after the last `@it`.
  Hook bodies are inlined into the describe scope rather than emitted
  as standalone functions, so they may freely read and reassign
  describe-scope variables (`@it` bodies, by contrast, capture those
  variables read-only). `@describe` bodies execute once, so
  `@beforeEach` mutations accumulate across tests — write an explicit
  reset if per-test isolation is required. Constraints: at most one
  hook of each kind per describe; lifecycle hooks cannot coexist with
  `@it` / `@describe` / `@timeout` / `@skip` / `@only` / `@todo` /
  `@each` / `@property` on the same function; hooks declared outside
  a `@describe` are rejected; and hook bodies cannot introduce new
  named variables (the body is re-emitted per `@it` and the second
  emission would clash). Known limitation: a test fired by `@timeout`
  unwinds via `siglongjmp` past the inlined `@afterEach` body, so
  cleanup runs only on normal completion. (#1686)
