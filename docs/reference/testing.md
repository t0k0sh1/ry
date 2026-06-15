# Testing

Ry has a built-in RSpec-style test syntax. Test files are executed using the `ry test` subcommand.

---

## Running Tests

```bash
ry test              # Auto-discover and run all *.test.ry files in the project
ry test tests/spec   # Run all *.test.ry files under a directory (recursive)
ry test test_file.ry # Run a specific test file
ry test -p           # Run all tests in parallel (-p or --parallel)
ry test -p tests/    # Run tests in a directory in parallel
ry test -w           # Watch mode: re-run tests on file change (-w or --watch)
ry test -w -p        # Watch mode with parallel execution
ry test -w tests/    # Watch a specific directory
ry test --coverage   # Run all tests with line coverage summary
ry test --cov        # Short alias for --coverage
ry test --outline    # Print describe/it structure without running tests
```

The exit code is 0 if all tests passed, 1 if any test failed.

### Auto-Discovery Mode

When `ry test` is run without arguments, it:

1. Searches for `package.toml` to find the project root
2. Recursively discovers all `*.test.ry` files under the project root (`.git`, `build`, `node_modules` are skipped)
3. Runs each file and aggregates results

---

## Syntax

Test files use directives (`@it`, `@describe`) and the helpers `expect`, `mock`, `spy`, `verify`, `verifyCalledWith`, `fail` from the `testing` module. Import them at the top using either `from testing` (wildcard) or `from testing import ...` (named). Several enforcement paths produce different error messages:

- `@it` / `@describe` are declared in `share/std/testing/testing.ry` as `@directive` declarations. Without the import, codegen rejects them via the general directive-resolution mechanism with `unknown directive '@it'` or `unknown directive '@describe'`.
- `expect`, `mock`, `spy`, `fail`, `verifyCalledWith` are compiler intrinsics tracked separately and rejected with `'<name>' requires 'from testing import <name>'`.
- `verify` is an ordinary `@public fn verify(name: str) -> int` declared in `share/std/testing/testing.ry`. Without the import, codegen rejects the call with the standard `undefined function: verify` diagnostic. (`verifyCalledWith` is an intrinsic — not a `@public fn` — because it must inspect the mocked function's signature at compile time to validate argument types, which a regular Ry function cannot do.)

```ry
from testing import it, describe, expect

@it("test case name")
fn testAdd():
    expect(1 + 2).toEq(3)
```

Group related tests using `@describe`:

```ry
from testing import it, describe, expect

@describe("Arithmetic")
fn arithmeticTests():
    @it("should add integers")
    fn testAdd():
        expect(1 + 2).toEq(3)

    @it("should subtract integers")
    fn testSub():
        expect(5 - 3).toEq(2)
```

- The function name is used for code navigation and symbol identity
- The description string (in the directive) is used for test output and reporting
- `@it` functions must have no parameters unless combined with `@each` or `@property`
- `@it` and `@describe` functions must not have a return type annotation
- Both `@it` and `@describe` are only available with `ry test`

#### Shared Setup

Variables declared in the `@describe` function body are automatically captured by inner `@it` functions:

```ry
from testing import it, describe, expect

@describe("User validation")
fn userValidationTests():
    minLength = 8
    maxLength = 64

    @it("should reject short passwords")
    fn testShort():
        expect(minLength).toBeGreaterThan(0)

    @it("should accept passwords within length limits")
    fn testRange():
        expect(maxLength).toBeGreaterThan(minLength)
```

#### Nested `@describe`

`@describe` functions can be nested to create multi-level groupings. Output is indented to reflect the nesting depth:

```ry
from testing import it, describe, expect

@describe("API")
fn apiTests():
    @describe("GET /users")
    fn getUsersTests():
        @it("should return 200 OK")
        fn testOk():
            expect(true).toBeTrue()
```

Output:

```text
API
  GET /users
    + should return 200 OK
```

### Trailing Block Syntax

Any function call (except `mock`) can use trailing block syntax. A colon after `()` causes the indented block to be passed as a no-argument lambda in the last argument position:

```
# These are equivalent:
foo("arg"):
    bar()

foo("arg", ():
    bar()
)
```

### expect / Matchers

| Matcher | Description | Supported Types |
|---|---|---|
| `toEq(expected)` | Equality comparison | int, float, bool, str, List, Set, Map, Option, Result, record, tuple, union |
| `toNotEq(expected)` | Asserts not equal | int, float, bool, str, List, Set, Map, Option, Result, record, tuple, union |
| `toBeTrue()` | Asserts `true` | bool |
| `toBeFalse()` | Asserts `false` | bool |
| `toBeNone()` | Asserts `None` | Option |
| `toBeSome()` | Asserts Option is `Some` | Option |
| `toBeOk()` | Asserts Result is `Ok` | Result |
| `toBeErr()` | Asserts Result is `Err` | Result |
| `toContain(val)` | Asserts container includes value (for List/Set, element type must be `int`, `float`, `str`, or `bool`; for Map, checks keys) | List, Set, Map, str |
| `toNotContain(val)` | Asserts container does not include value (for List/Set, element type must be `int`, `float`, `str`, or `bool`; for Map, checks keys) | List, Set, Map, str |
| `toBeGreaterThan(v)` | Asserts `actual > v` | int, float |
| `toBeLessThan(v)` | Asserts `actual < v` | int, float |
| `toBeGreaterThanOrEq(v)` | Asserts `actual >= v` | int, float |
| `toBeLessThanOrEq(v)` | Asserts `actual <= v` | int, float |
| `toHaveLen(n)` | Asserts length equals `n` (for `str`, counts UTF-8 codepoints, not bytes) | List, Set, Map, str |
| `toBeEmpty()` | Asserts length is 0 | List, Set, Map, str |
| `toStartWith(prefix)` | Asserts string starts with prefix | str |
| `toEndWith(suffix)` | Asserts string ends with suffix | str |
| `toMatch(pattern)` | Asserts string matches the given regex pattern (unanchored; use `^` / `$` to anchor) | str |
| `toBeCloseTo(value)` / `toBeCloseTo(value, decimals)` | Asserts approximate equality: `\|actual - value\| < 0.5 * 10^-decimals`. `decimals` defaults to `2` and must be a non-negative integer literal in `[0, 15]` | int, float |
| `toBeBetween(min, max)` | Asserts `min <= actual <= max` (inclusive on both ends). Both bounds are required | int, float |
| `toBeOneOf(list)` | Asserts `actual` equals at least one element in `list`. Equivalent to `toContain` with arguments reversed | int, float, str, bool (as List elements) |
| `toBeNaN()` | Asserts value is NaN (IEEE 754); `NaN == NaN` is false, so use this matcher instead of `toEq` | float |
| `toBeInfinity()` | Asserts value is positive or negative infinity (`+∞` or `-∞`) | float |
| `toBeFinite()` | Asserts value is finite (not NaN and not `±∞`) | float |

### fail

Immediately marks the current test as failed.

```ry
from testing import it, fail

@it("should not reach here")
fn shouldNotReachHere():
    fail("unexpected error")
```

- `fail()` — marks the test as failed with a generic message
- `fail(msg)` — marks the test as failed with a custom message
- Execution continues after `fail()` (does not abort the test)
- Only available in `ry test` mode
- Requires `from testing import fail`

### Test selection: `@skip`, `@only`, `@todo`

Three directives allow individual test selection within a file:

```ry
from testing import it, expect, skip, only, todo

@skip
@it("temporarily disabled while bug #123 is open")
fn skipped():
    expect(1).toEq(2)   # never runs, counted as `skipped`

@only
@it("the one failing case I am currently debugging")
fn focused():
    expect(1 + 1).toEq(2)

@todo
@it("upcoming feature, body not yet written")
fn todo():
    notYetDefined()     # body is never compiled
```

- `@skip @it("...")` — the test is not executed, but the body **is still compiled** (Jest `xit` / `it.skip` semantics). Type errors, undefined identifiers, and other body-level codegen failures are still surfaced, so typos in skipped tests do not lurk until the test is un-skipped. Reported as `~ <name> (skipped)` (gray) and counted as `skipped`. Use this for tests that are temporarily disabled (e.g. while a bug is investigated); use `@todo` when the test has not been written yet.
- `@only @it("...")` — when at least one `@only` appears on an `@it` function in a file, every `@it` in that file *without* `@only` is implicitly skipped. Useful for focused TDD on a single failing case. The implicit skip is reported the same way as `@skip` and the implicitly-skipped bodies are likewise still compiled. `@only` on a non-`@it` function has no effect on file-wide selection — only `@only` paired with `@it` triggers the focus filter.
- `@todo @it("...")` — a placeholder. The function body is **never emitted**, so it may reference undefined identifiers, omit a `return`, or otherwise fail compilation; only the directive itself is validated. Reported as `? <name> (todo)` (cyan) and counted as `todo`. This is the only directive that suppresses body codegen entirely — `@skip` does not, by design.

Composition rules:

- The three directives compose with `@each` and `@property`: `@skip @each @it(...)` skips the entire loop, `@only @property @it(...)` runs every iteration only when `@only` is the file's focus mode, `@todo @each @it(...)` emits nothing and counts as a single `todo`.
- Mutual combinations (`@skip @only`, `@skip @todo`, `@only @todo`) are rejected at compile time.
- The MVP supports `@it` only. Attaching `@skip`, `@only`, or `@todo` to `@describe` is a compile error.
- `skipped` and `todo` do **not** influence the exit code. Only `failed` does.

In outline mode (`ry test --outline`), the directive is appended as a suffix:

```text
+ it temporarily disabled while bug #123 is open (@skip)
+ it the one failing case I am currently debugging (@only)
+ it upcoming feature, body not yet written (@todo)
```

When combined with `@each` or `@property`, the suffix shows both, e.g. `(@only @each)`. Outline mode shows the full structure of all tests regardless of `@only`, so the focus filter does not apply there.

---

## Output Format

```
Calculator
  + should add numbers
  + should subtract
  - should fail
    line 10: expected 3, got 2
  ~ should be skipped (skipped)
  ? should be todo (todo)

2 passed, 1 failed, 1 skipped, 1 todo
```

- `+` indicates pass (green), `-` indicates failure (red), `~` indicates skip (gray), `?` indicates todo (cyan)
- On failure, the line number and expected/actual values are displayed
- The summary always prints the 4-item form; `skipped` and `todo` do not contribute to the exit code (only `failed` does)

---

## Example

```ry
from testing import it, describe, expect

@describe("Arithmetic")
fn arithmeticTests():
    @it("should add integers")
    fn shouldAddIntegers():
        expect(1 + 2).toEq(3)

    @it("should compare strings")
    fn shouldCompareStrings():
        expect("hello").toEq("hello")

    @it("should check booleans")
    fn shouldCheckBooleans():
        expect(3 > 1).toBeTrue()

@describe("Booleans")
fn booleansTests():
    @it("should return false")
    fn shouldReturnFalse():
        expect(1 > 2).toBeFalse()
```

---

## Lifecycle Hooks

Four directives — `@beforeEach`, `@afterEach`, `@beforeAll`, `@afterAll` — let you factor common setup/teardown out of every `@it` body. They are declared on parameterless, return-typeless functions either **inside a `@describe` block** (the body is inlined into the describe at codegen time and runs in the describe's variable scope) or **at file top level** (the body is inlined around every `@it` in the file and runs in the file's top-level scope). File-level and describe-level hooks cascade — see [Execution order](#execution-order) for the full ordering.

```ry
from testing import describe, it, beforeEach, afterEach, beforeAll, afterAll, expect

@describe("counter")
fn counterTests():
    counter = 0
    log = ""

    @beforeAll
    fn setupAll():
        log = log + "BA;"

    @beforeEach
    fn setupEach():
        counter = counter + 1
        log = log + "BE;"

    @afterEach
    fn teardownEach():
        log = log + "AE;"

    @afterAll
    fn teardownAll():
        log = log + "AA;"

    @it("should see counter == 1 on first call")
    fn first():
        expect(counter).toEq(1)

    @it("should see counter == 2 on second call (state accumulates)")
    fn second():
        expect(counter).toEq(2)
```

### Execution order

For a describe with `N` tests, hooks run in this order:

```text
@beforeAll
(@beforeEach → @it → @afterEach) × N
@afterAll
```

#### File top-level hooks (cascade with describe-level hooks)

`@beforeAll` / `@beforeEach` / `@afterEach` / `@afterAll` may also be declared **outside any `@describe`** — at file top level. File-level hooks wrap **every** test in the file (top-level `@it` and `@it` nested inside `@describe`) and cascade with describe-level hooks: file `@beforeEach` fires **before** describe `@beforeEach`, and file `@afterEach` fires **after** describe `@afterEach`. For an `@it` inside a `@describe`:

```text
file @beforeAll                                            (once per file)
  describe @beforeAll                                      (once per describe)
    file @beforeEach → describe @beforeEach
      @it
    describe @afterEach → file @afterEach                  (per @it)
  describe @afterAll
file @afterAll                                             (once per file)
```

Top-level `@it` (no enclosing `@describe`) skips every describe-level layer: only file `@beforeAll` / `@beforeEach` / `@afterEach` / `@afterAll` run.

```ry
from testing import it, describe, beforeAll, beforeEach, afterEach, afterAll, expect

log = ""

@beforeAll
fn fileSetupAll():
    log = log + "FBA;"

@beforeEach
fn fileSetupEach():
    log = log + "FBE;"

@afterEach
fn fileTeardownEach():
    log = log + "FAE;"

@it("top-level @it sees file hooks only")
fn t1():
    expect(log).toEq("FBA;FBE;")

@describe("inner")
fn inner():
    @beforeEach
    fn descBefore():
        log = log + "DBE;"

    @it("file BE fires before describe BE")
    fn t2():
        # After t1: "FBA;FBE;FAE;". Then file FBE; describe DBE; body.
        expect(log).toEq("FBA;FBE;FAE;FBE;DBE;")
```

File-level hooks are declared at most once per kind per file (mirroring the per-describe limit). All other rules in [Constraints](#constraints) apply unchanged.

### Accumulation semantics (differs from Jest)

The `@describe` body executes **once**, not per-test. Variables declared inside the describe are allocated once and live across all tests in the block; `@beforeEach` mutations accumulate across `@it` invocations (the `counter == 1` then `counter == 2` example above). This is intentional and follows from Ry's scope model.

If you need per-test reset semantics, write the reset explicitly:

```ry
@beforeEach
fn reset():
    counter = 0           # explicit reset, not implicit
```

### Mutability rules

- **Hook bodies** are inlined into the describe scope, so they may freely read and reassign describe-scope variables (`counter = counter + 1` above).
- **`@it` bodies** are compiled as separate functions that capture describe-scope variables. Captures are read-only — attempting `counter = counter + 1` inside an `@it` body raises `cannot modify captured variable`.
- A hook body cannot **introduce a new variable** (e.g. `items: List<int> = []`) because hook bodies are re-emitted before every `@it`; the second emission would re-declare the same name and fail with `type annotation not allowed on reassignment`. Declare such variables in the describe scope and have the hook only reassign them.

### Constraints

- A hook function must have no parameters and no declared return type
- At most one of each hook kind per describe block, and at most one of each hook kind per file at top level
- A function cannot carry two lifecycle directives (e.g. `@beforeEach @afterEach`)
- A lifecycle hook directive cannot coexist with `@it`, `@describe`, `@timeout`, `@skip`, `@only`, `@todo`, `@each`, or `@property` on the same function

### Interaction with `@timeout`

`@afterEach` runs even when `@timeout(N)` fires for the body (#1781). The codegen wraps `@afterEach` in its own `sigsetjmp` landing pad with a fresh `N`-ms `setitimer` budget, so the body's `siglongjmp` lands in the `@afterEach` phase and cleanup proceeds. If `@afterEach` itself does not complete within `N` ms, a secondary failure line `@afterEach (timeout after Nms)` is printed and the test runner moves on — a hung cleanup does NOT block subsequent tests. Worst-case wall-clock per timed test is therefore `2N` ms. Because the body / `@beforeEach` may have only partially run when `@afterEach` is invoked in the timeout path, `@afterEach` must tolerate partial setup (e.g. nil-guard each handle before closing it). See the [Directives reference](directives.md#timeout) for the underlying mechanism.

---

## Feature interactions

Testing directives compose in specific ways. This section documents every supported (and intentionally rejected) combination so behavior need not be discovered by trial. Worked examples are taken from `tests/spec/feature_combinations.test.ry`.

### Summary matrix

| Combination | Status | Behavior / where to look |
|---|---|---|
| `@each` + `@beforeEach` | Compile error | [Lifecycle hooks with `@each` / `@property`](#lifecycle-hooks-with-each--property) |
| `@each` + `@afterEach` | Compile error | [Lifecycle hooks with `@each` / `@property`](#lifecycle-hooks-with-each--property) |
| `@property` + `@beforeEach` | Compile error | [Lifecycle hooks with `@each` / `@property`](#lifecycle-hooks-with-each--property) |
| `@property` + `@afterEach` | Compile error | [Lifecycle hooks with `@each` / `@property`](#lifecycle-hooks-with-each--property) |
| `@each` + `@timeout` | Compile error | [Mutually exclusive: `@each` / `@property` + `@timeout`](#mutually-exclusive-each--property--timeout) |
| `@property` + `@timeout` | Compile error | [Mutually exclusive: `@each` / `@property` + `@timeout`](#mutually-exclusive-each--property--timeout) |
| `mock` in `@beforeEach` | Supported | [`mock` / `spy` inside `@beforeEach`](#mock--spy-inside-beforeeach) |
| `spy` in `@beforeEach` | Supported | [`mock` / `spy` inside `@beforeEach`](#mock--spy-inside-beforeeach) |
| `mock` + `spy` in same `@it` | Supported | `mock` takes precedence; see [`spy(name)`](#spyname) |
| `@beforeAll` + `@each` | Supported | [`@beforeAll` / `@afterAll` with `@each` / `@property`](#beforeall--afterall-with-each--property) |
| `@afterAll` + `@each` | Supported | [`@beforeAll` / `@afterAll` with `@each` / `@property`](#beforeall--afterall-with-each--property) |
| Nested `@describe` lifecycle inheritance | Unsupported | [Nested `@describe` lifecycle](#nested-describe-lifecycle) |

The same matrix entries apply when `@property` is substituted for `@each` and vice versa (and for `@afterAll` paired with `@each` / `@property`); the table lists `@each` only to keep the row count manageable.

### Lifecycle hooks with `@each` / `@property`

`@beforeEach` and `@afterEach` cannot decorate an `@it` that also carries `@each` or `@property`. The codegen path (`src/codegen_test.cpp`) raises the compile error verbatim:

```text
error: @beforeEach / @afterEach are not yet supported with @each on @it '<fn>'
error: @beforeEach / @afterEach are not yet supported with @property on @it '<fn>'
```

This is a deliberate MVP limitation (#1686): inlining hooks per iteration would require additional codegen scaffolding to thread hook state through the runtime loop. Workarounds:

- Move per-iteration setup / teardown into the `@it` body itself (or call a helper).
- Hoist work that is shared across iterations into `@beforeAll`; see [`@beforeAll` / `@afterAll` with `@each` / `@property`](#beforeall--afterall-with-each--property).

### Mutually exclusive: `@each` / `@property` + `@timeout`

The compiler rejects this combination, again from `src/codegen_test.cpp`:

```text
error: @timeout cannot be combined with @each on fn '<fn>'
error: @timeout cannot be combined with @property on fn '<fn>'
```

`@timeout` measures the wall-clock duration of one invocation of the test body; loop-style runners cannot share a single timer budget across iterations without ambiguity. See [`@timeout` in the Directives reference](directives.md#timeout) and the [Troubleshooting entry](#each-or-property-combined-with-timeout-is-rejected-at-compile-time) for the user-facing diagnostic.

### `mock` / `spy` inside `@beforeEach`

Installing a `mock` or `spy` in the describe's `@beforeEach` produces a fresh installation for each `@it`: the hook body is inlined per `@it`, and the auto-restore at `@it` end clears both the implementation override and the call counter.

```ry
from testing import describe, it, beforeEach, mock, verify, expect

fn fetchValue() -> int:
    return 7

@describe("mock installed via beforeEach is fresh per it")
fn mockInBeforeEach():
    @beforeEach
    fn be():
        mock(fetchValue, () => 42)

    @it("first it sees the mocked value")
    fn firstIt():
        expect(fetchValue()).toEq(42)
        expect(verify("fetchValue")).toEq(1)

    @it("second it sees re-installed mock with fresh call count")
    fn secondIt():
        expect(verify("fetchValue")).toEq(0)
        expect(fetchValue()).toEq(42)
        expect(verify("fetchValue")).toEq(1)
```

`spy("name")` works the same way — the call count resets to 0 at the start of every `@it`. When `mock` and `spy` are both active on the same function inside one `@it`, `mock` takes precedence: see [`spy(name)`](#spyname).

### `@beforeAll` / `@afterAll` with `@each` / `@property`

`@beforeAll` fires once before the iteration loop begins, and `@afterAll` once after every iteration of every `@it` in the describe completes:

```ry
from testing import describe, it, each, beforeAll, expect

@describe("beforeAll runs once before all each iterations")
fn beforeAllRunsOncePerEach():
    baCount = 0

    @beforeAll
    fn ba():
        baCount = baCount + 1

    @each([(1,), (2,), (3,)])
    @it("iteration {0} sees baCount == 1")
    fn iter(x: int):
        expect(baCount).toEq(1)
        expect(x > 0).toEq(true)

    @it("after iterations baCount is still 1")
    fn checker():
        expect(baCount).toEq(1)
```

The same single-fire semantics apply to `@property` and to `@afterAll`. Because hooks are describe-local, `@afterAll`'s execution is not observable from any `@it` inside the same describe; `tests/spec/feature_combinations.test.ry` verifies the negative — `@afterAll` has not yet fired during any iteration or trailing `@it`.

### Nested `@describe` lifecycle

Each `@describe` owns its own lifecycle hooks. An outer describe's `@beforeEach` / `@afterEach` / `@beforeAll` / `@afterAll` **does not run** before or after the inner describe's `@it` blocks — see the third [Limitations](#limitations) bullet ("Nested-describe inheritance of lifecycle hooks is not supported").

---

## Mocking

### mock(fnName, replacement)

Replaces a function with a mock implementation for the current `it` block. The mock is automatically cleared when the `it` block ends.

```ry
from testing import it, describe, mock, expect

fn fetchData() -> str:
    return "real data"

@describe("mocking")
fn mockingTests():
    @it("should replace function")
    fn shouldReplaceFunction():
        mock(fetchData, () => "fake")
        expect(fetchData()).toEq("fake")

    @it("should auto-restore after it block")
    fn shouldAutoRestoreAfterItBlock():
        expect(fetchData()).toEq("real data")
```

- The first argument is the function name (identifier, not a string)
- The second argument is a replacement lambda
- The replacement must have the same parameter types and return type as the original function
- `require` and `ensure` contracts on the original function are still enforced when the mock is called
- Mocks are automatically restored at the end of each `it` block
- Requires `from testing import mock`

### verify(name)

Returns the number of times a mocked function was called (as `int`). The argument is the **string name** of the function — `verify` is an ordinary `@public fn` exported by the `testing` module, not a compiler intrinsic, so the bare-identifier sugar that `mock` accepts does not apply here.

```ry
from testing import it, describe, mock, verify, expect

@describe("verify")
fn verifyTests():
    @it("should count calls")
    fn shouldCountCalls():
        mock(fetchData, () => "fake")
        fetchData()
        fetchData()
        expect(verify("fetchData")).toEq(2)
```

- Requires `from testing import verify`
- Returns `0` when no call has been recorded for that name (including unknown function names) — there is no compile-time check that the string corresponds to a real function.

### verifyCalledWith(name, args...)

Returns the number of times a mocked function was called with arguments that exactly match `args...` (as `int`). Unlike `verify`, the function name must be a **string literal** because `verifyCalledWith` is a compiler intrinsic that resolves the original function's signature at compile time to validate argument types.

```ry
from testing import it, describe, mock, verifyCalledWith, expect

fn compute(x: int) -> int:
    return x * 2

fn takesIntList(xs: List<int>) -> int:
    return len(xs)

fn takesIntSet(xs: Set<int>) -> int:
    return len(xs)

fn takesStrIntMap(m: Map<str, int>) -> int:
    return len(m)

fn takesFn(f: fn(int) -> int) -> int:
    return f(0)

@describe("verifyCalledWith")
fn verifyCalledWithTests():
    @it("should count calls matching argument")
    fn shouldCountMatching():
        mock(compute, (x: int) => x * 10)
        compute(5)
        compute(7)
        compute(5)
        expect(verifyCalledWith("compute", 5)).toEq(2)
        expect(verifyCalledWith("compute", 7)).toEq(1)
        expect(verifyCalledWith("compute", 999)).toEq(0)

    @it("should count calls matching List<int> argument")
    fn shouldCountListIntMatching():
        mock(takesIntList, (xs: List<int>) => len(xs))
        takesIntList([1, 2, 3])
        takesIntList([1, 2, 3])
        takesIntList([1, 2])
        expect(verifyCalledWith("takesIntList", [1, 2, 3])).toEq(2)
        expect(verifyCalledWith("takesIntList", [1, 2])).toEq(1)
        expect(verifyCalledWith("takesIntList", [9, 9, 9])).toEq(0)

    @it("should count calls matching Set<int> argument unordered")
    fn shouldCountSetIntMatching():
        mock(takesIntSet, (xs: Set<int>) => len(xs))
        takesIntSet({1, 2, 3})
        takesIntSet({1, 2})
        expect(verifyCalledWith("takesIntSet", {3, 2, 1})).toEq(1)
        expect(verifyCalledWith("takesIntSet", {1, 2})).toEq(1)
        expect(verifyCalledWith("takesIntSet", {9, 9})).toEq(0)

    @it("should count calls matching Map<str, int> argument unordered")
    fn shouldCountMapStrIntMatching():
        mock(takesStrIntMap, (m: Map<str, int>) => len(m))
        takesStrIntMap({"a": 1, "b": 2})
        takesStrIntMap({"c": 3})
        expect(verifyCalledWith("takesStrIntMap", {"b": 2, "a": 1})).toEq(1)
        expect(verifyCalledWith("takesStrIntMap", {"c": 3})).toEq(1)
        expect(verifyCalledWith("takesStrIntMap", {"x": 9})).toEq(0)

    @it("should match fn-typed argument by pointer equality")
    fn shouldCountFnMatching():
        mock("takesFn", (f: fn(int) -> int) => f(0))
        lam = (x: int) => x + 1
        takesFn(lam)
        takesFn(lam)
        # Same closure value → match.
        expect(verifyCalledWith("takesFn", lam)).toEq(2)
        # Independently constructed structurally identical lambda
        # → no match (different thunk pointer).
        other = (x: int) => x + 1
        expect(verifyCalledWith("takesFn", other)).toEq(0)
```

- Requires `from testing import verifyCalledWith`
- The first argument must be a string literal — variables / runtime strings are rejected at compile time. This restriction lets the compiler validate the remaining argument types against the original function's signature.
- The function must already be mocked via `mock(...)` or spied via `spy(...)` before `verifyCalledWith` is called; calling on a function that has neither is a compile error.
- The number and types of `args...` must exactly match the original function's parameter list. Arity mismatch and type mismatch are compile errors.
- Supported argument types: `int`, `float`, `bool`, `str` (since v0.0.22, #1677), `List<T>` where `T ∈ {int, float, bool, str}` (since v0.0.22, #1703), `Set<T>` where `T ∈ {int, float, bool, str}` (since v0.0.22, #1704), `Map<K, V>` where `K, V ∈ {int, float, bool, str}` (since v0.0.22, #1705), record types whose fields are all in `{int, float, bool, str}` (since v0.0.22, #1706), tuple types whose elements are all in `{int, float, bool, str}` (since v0.0.22, #1706), and `fn(...) -> R` (function-typed) arguments compared by pointer equality (since v0.0.22, #1707). Other types (nested `List<List<T>>`, records or tuples containing collections) are rejected at compile time and are tracked for v0.0.x follow-up.
- `List<T>` arguments are compared by deep snapshot: the recorded call snapshot and the verify-side snapshot must agree on length and element-wise equality. Element comparison is byte-exact for `int` / `float` / `bool` and uses NUL-safe length+`memcmp` for `str`.
- `Set<T>` arguments are compared by **unordered** deep snapshot: the recorded and verify-side snapshots must have the same length and the same elements as a set, but element order is irrelevant (e.g. recording `{1, 2, 3}` matches `verifyCalledWith("f", {3, 2, 1})`). Per-element comparison uses the same byte-exact / NUL-safe rules as `List<T>`.
- `Map<K, V>` arguments are compared by **unordered** deep snapshot of the {key → value} pairs: the recorded and verify-side snapshots must have the same length and the same key set, with each key mapping to the same value across the two maps. Insertion order is irrelevant (e.g. recording `{"a": 1, "b": 2}` matches `verifyCalledWith("f", {"b": 2, "a": 1})`). Per-key and per-value comparison uses the same byte-exact / NUL-safe rules as `List<T>`.
- Record arguments are compared by declared **type name** plus field-by-field equality. Two records with structurally identical fields but different declared names (e.g. `Point(1, 2)` vs `Vec(1, 2)`) do not match and are rejected at compile time when the parameter type is fixed. Per-field comparison uses the same byte-exact / NUL-safe rules as `List<T>`.
- Tuple arguments are compared by **arity** plus element-by-element equality. Tuples with different arity do not match and are rejected at compile time. Per-element comparison uses the same byte-exact / NUL-safe rules as `List<T>`.
- `fn(...) -> R` arguments are compared by **pointer equality** on the `{thunk_ptr, env_ptr}` pair extracted from the uniform closure struct, not by structural / behavioral equivalence. Two independently constructed lambdas that happen to be structurally identical (e.g. `(x: int) => x + 1` written twice on different source lines) do not match — only the same closure value (a single named binding `f = ...` flowing into both the recorded call and the verify side, or two aliases of the same bare `@public fn`) matches. Capture closures with different captured environments (e.g. `makeAdder(5)` vs `makeAdder(6)`) are distinguished by the per-instance `env_ptr` even though they share a single cached capturing thunk. The fn signature must exactly match the recorded parameter type (since v0.0.22, #1715); mismatched parameter count, parameter types, or return type are rejected at compile time with both signatures included in the diagnostic. Within matching signatures, identity is determined by the pointer pair.
- `int` argument literals are widened to `float` automatically when the parameter type is `float` (matching ordinary call-site coercion).
- Returns `0` when no recorded call matches the supplied arguments.

### spy(name)

Records calls to a function without replacing its implementation. Unlike `mock`, the original function body still executes — `spy` only adds call-count and argument-recording instrumentation around it.

```ry
from testing import it, describe, spy, verify, verifyCalledWith, expect

fn compute(x: int) -> int:
    return x * 3

@describe("spy")
fn spyTests():
    @it("should record calls without replacing implementation")
    fn recordsWithoutReplacing():
        spy("compute")
        expect(compute(5)).toEq(15)         # real implementation runs
        expect(verify("compute")).toEq(1)   # call is recorded

    @it("should work with verifyCalledWith")
    fn worksWithVerifyCalledWith():
        spy("compute")
        compute(7)
        compute(8)
        compute(7)
        expect(verifyCalledWith("compute", 7)).toEq(2)
        expect(verifyCalledWith("compute", 8)).toEq(1)
```

- Requires `from testing import spy` (since v0.0.24, #1683)
- The argument is the **string literal** name of the function (same convention as `verifyCalledWith`)
- The function must exist (compile error otherwise). Overloaded functions are supported since v0.0.24 (#1682): the bare name registers spies for **all** overloads aggregately, and a signature form (`spy("foo(int)")`) targets a single overload — see "Mocking overloaded functions" below
- Spy registrations are automatically cleared at the end of each `it` block (same lifecycle as `mock`)
- `verify(name)` and `verifyCalledWith(name, args...)` work uniformly on spied functions (same call-recording mechanism as `mock`)
- `mockClear(name)` / `mockReset(name)` / `mockResetAll()` apply to spied functions identically — they share the same internal registry
- A function may be both mocked and spied across different `it` blocks within the same describe. When both are active in the same block (mock+spy coexistence), `mock` takes precedence: the replacement runs and the call is counted; the real implementation is bypassed
- See [Feature interactions](#feature-interactions) for installing `mock` / `spy` from `@beforeEach` (auto-restore semantics, fresh per-`it` state)

### mockReturnValueOnce(name, value)

Enqueues a value for the named function. The next call to that function dequeues and returns the head of the queue. When the queue empties, calls fall back to the function set via `mock(name, replacement)` (if any), then to the original implementation — matching Jest's fallback chain.

```ry
from testing import it, describe, mock, mockReturnValueOnce, verify, expect

fn fetchUser() -> str:
    return "real"

@describe("mockReturnValueOnce")
fn mockReturnValueOnceTests():
    @it("should return queued values in order then fall back to original")
    fn returnsQueuedThenOriginal():
        mockReturnValueOnce("fetchUser", "first")
        mockReturnValueOnce("fetchUser", "second")
        expect(fetchUser()).toEq("first")
        expect(fetchUser()).toEq("second")
        expect(fetchUser()).toEq("real")   # queue empty, falls back to original

    @it("should mix with mock() default (queue wins, then default, then original)")
    fn mixesWithDefault():
        mock(fetchUser, () => "fallback")
        mockReturnValueOnce("fetchUser", "queued")
        expect(fetchUser()).toEq("queued")     # queue
        expect(fetchUser()).toEq("fallback")   # default mock
        expect(fetchUser()).toEq("fallback")   # default mock (stays)
```

- Requires `from testing import mockReturnValueOnce` (since v0.0.24, #1681)
- The first argument is the **string literal** name of the function (same convention as `spy` / `verifyCalledWith`, unlike `mock` which takes an identifier); non-literal first arguments are rejected at compile time
- The function must exist and must not return `Unit` (both are compile errors). Overloaded functions are supported since v0.0.24 (#1682) only via signature form (`mockReturnValueOnce("foo(int)", 1)`) — the bare name is a compile error on overloaded functions because the return value alone cannot disambiguate
- The second argument's type must match the function's declared return type. Supported types: primitives (`int` / `float` / `bool`), `str`, `List` / `Map` / `Set`, records, `Result`, and `Option` (including bare `None` for `Option`-returning functions)
- Arguments to the mocked call are ignored — the queue holds values, not call expectations
- `verify(name)` counts only queue-served and default-mock-served calls. Once the queue empties and the call falls through to the original implementation, those calls are **not** counted (matching `mockReset` semantics — diverges from strict Jest behavior)
- `mockClear(name)` preserves the queue and only resets the call counter; `mockReset(name)` and `mockResetAll()` discard the queue
- Queues are automatically cleared at the end of each `it` block (same lifecycle as `mock` / `spy`)
- Capture-based closures registered via `mock(name, replacement)` coexist with queued values — the queue is consumed first, then the closure

### mockClear(name)

Resets the recorded call list (and call count) for a single mock to zero, but keeps the mock active. Subsequent calls continue to dispatch to the replacement.

```ry
from testing import it, describe, mock, verify, mockClear, expect

@describe("mockClear")
fn mockClearTests():
    @it("should reset count but preserve mock")
    fn shouldClearCount():
        mock(fetchData, () => "fake")
        fetchData()
        fetchData()
        expect(verify("fetchData")).toEq(2)
        mockClear("fetchData")
        expect(verify("fetchData")).toEq(0)
        # mock is still active — calls continue to hit the replacement
        x = fetchData()
        expect(verify("fetchData")).toEq(1)
```

- Requires `from testing import mockClear`
- The argument is the **string name** of the mocked or spied function (same convention as `verify`)
- No-op when `name` is not currently mocked or spied (no error)
- Affects `verify(name)` and `verifyCalledWith(name, args...)` identically — both observe the cleared call list
- Applies to spied functions identically — mock and spy share the same call-recording registry (#1683)
- **Preserves the `mockReturnValueOnce` queue** — only the call counter is reset; queued values remain and continue to be dispatched on subsequent calls (matches Jest semantics, #1681)

### mockReset(name)

Removes a single mock entirely, restoring the original implementation. After `mockReset`, calls dispatch to the original function and the call count is zero.

```ry
from testing import it, describe, mock, verify, mockReset, expect

fn fetchData() -> str:
    return "real"

@describe("mockReset")
fn mockResetTests():
    @it("should restore original implementation")
    fn shouldResetMock():
        mock(fetchData, () => "fake")
        expect(fetchData()).toEq("fake")
        mockReset("fetchData")
        expect(fetchData()).toEq("real")
        expect(verify("fetchData")).toEq(0)
```

- Requires `from testing import mockReset`
- The argument is the **string name** of the mocked or spied function
- No-op when `name` is not currently mocked or spied (no error)
- Releases the replacement closure environment (capturing closures' captured variables are dropped immediately, equivalent to `it`-block end auto-cleanup for this single mock)
- Applies to spied functions identically — removes the spy registration, after which `verify(name)` returns 0 (#1683)
- **Discards the `mockReturnValueOnce` queue** for the named function — any remaining queued values are released (#1681)

### mockResetAll()

Removes every mock currently active in the enclosing `it` block. Equivalent to the automatic cleanup that runs when an `it` block ends, but explicit and usable mid-block.

```ry
from testing import it, describe, mock, verify, mockResetAll, expect

fn fa() -> int:
    return 1

fn fb() -> int:
    return 2

@describe("mockResetAll")
fn mockResetAllTests():
    @it("should remove all mocks")
    fn shouldResetAll():
        mock(fa, () => 10)
        mock(fb, () => 20)
        fa()
        fb()
        mockResetAll()
        expect(fa()).toEq(1)
        expect(fb()).toEq(2)
        expect(verify("fa")).toEq(0)
        expect(verify("fb")).toEq(0)
```

- Requires `from testing import mockResetAll`
- Takes no arguments
- No-op when no mock or spy is currently registered
- Clears spied functions identically — the registry is shared between mock and spy (#1683)
- **Discards all `mockReturnValueOnce` queues** — every named function's queued values are released (#1681)

### Mocking overloaded functions

Since v0.0.24 (#1682), overloaded functions can be mocked / spied / verified per overload. The mock registry is keyed by **canonical signature** `"name(T1, T2, ...)"` rather than by bare function name, so each overload has an independent slot.

**Signature-form syntax.** Pass `"name(T1, T2)"` instead of the bare name to target a specific overload:

```ry
from testing import it, describe, mock, verify, verifyCalledWith, expect

fn add(a: int, b: int) -> int:
    return a + b

fn add(a: float, b: float) -> float:
    return a + b

@describe("overloaded mock")
fn overloadedMockTests():
    @it("should target int overload only")
    fn targetsIntOverload():
        mock("add(int, int)", (a: int, b: int) -> int => 100)
        expect(add(2, 3)).toEq(100)        # mocked
        expect(add(2.5, 3.5)).toEq(6.0)    # float overload still real
        expect(verify("add(int, int)")).toEq(1)
        expect(verify("add(float, float)")).toEq(0)  # not mocked/spied, so not counted
```

- Whitespace inside the parameter list is normalized — `"add(int, int)"` ≡ `"add( int , int )"`.
- The parameter types must match the function's declaration form exactly. Type aliases are resolved automatically; equivalent surface spellings such as `int?` vs `Option<int>` are **not** unified (the parser keeps them distinct strings, so use the form the function was declared with).
- Zero-parameter overloads use `"name()"` — this is distinct from the bare name `"name"`.
- Available for `mock` / `mockReturnValueOnce` / `spy` / `verify` / `verifyCalledWith` / `mockClear` / `mockReset`.

**Bare-name semantics on overloaded functions.** When the bare name (e.g. `"add"` rather than `"add(int, int)"`) is passed to an overloaded function, each API behaves as follows:

| API | Bare-name behavior on overloaded function |
|---|---|
| `mock(n, repl)` | Auto-dispatch when the replacement lambda's signature matches exactly one overload; otherwise compile error listing available signatures |
| `mockReturnValueOnce(n, v)` | Compile error — the return-value alone cannot disambiguate; signature form required |
| `spy(n)` | Registers spy for **all** overloads aggregately |
| `verify(n)` | Returns the **sum** of call counts across all overloads |
| `verifyCalledWith(n, ...)` | Compile error when the arity does not pinpoint a unique overload; otherwise dispatches to the arity-matching overload |
| `mockClear(n)` | Clears the call counter for **all** overloads |
| `mockReset(n)` | Removes mocks/spies for **all** overloads |

**Behavior shift on adding overloads.** If existing code calls `verify("foo")` and a second overload of `foo` is later introduced, the return value silently becomes the aggregate count across both overloads. To preserve per-overload counting through such a change, switch to the signature form (`verify("foo(int)")`) when the function gains overloads. The other aggregate APIs (`mockClear` / `mockReset` / `spy`) are not behavior-sensitive in the same way — their pre- and post-overload semantics coincide on a single-overload function.

**Native (`@native`) overloads.** `customEmitter`-based stdlib overloads — including the math overload set (`abs`, `floor`, `ceil`, `round`, `log`, `pow`, `digits`) — can be mocked / spied via signature form (`mock("digits(int)", ...)`, `spy("abs(float)")`). Argument recording for `verifyCalledWith` on these natives is **not** supported in v1 — only count-based `verify("digits(int)")` works. Other `@native fn` declarations (table-driven without `customEmitter`, hand-written `emitBuiltin*` helpers) cannot be mocked.

### Limitations

- Most `@native fn` declarations cannot be mocked. The exceptions are `customEmitter`-based overloads such as the math module's `abs` / `floor` / `ceil` / `round` / `log` / `pow` / `digits` (see "Mocking overloaded functions" above). Argument recording for `verifyCalledWith` on those natives is not supported in v1.
- Capture-based closures **are supported as mock replacements** (since v0.0.22, #1678) — the closure can read or mutate variables from the enclosing scope. The captured environment is released automatically when the `it` block ends.
- For `mockReturnValueOnce`, calls that fall through to the original implementation (after the queue empties and with no default `mock` set) are not counted by `verify(name)` — only queue-served and default-mock-served calls increment the counter. This diverges from strict Jest behavior but matches the Ry convention used by `mockReset` (#1681).

---

## Parameterized Tests (@each)

`@each` runs the same test with multiple sets of parameters.

**Syntax:**

```ry
from testing import it, expect

@each([
    (1, 2, 3),
    (0, 0, 0),
    (-1, 1, 0)
])
@it("should add {0} + {1} = {2}")
fn testAdd(a: int, b: int, expected: int):
    expect(a + b).toEq(expected)
```

- The list must contain tuples whose arity matches the parameter count
- Placeholders `{0}`, `{1}`, ... in the description are replaced with the parameter values
- Each tuple generates an independent test case
- Supported parameter types: `int`, `float`, `bool`, `str`

See [Feature interactions](#feature-interactions) for combinations with lifecycle hooks and `@timeout` (the lifecycle and `@timeout` combinations are compile errors in the current MVP).

---

## Property-Based Tests (@property)

`@property` generates random inputs and runs the test multiple times.

```ry
from testing import it, expect

@property(count=100)
@it("should verify addition is commutative")
fn testCommutative(a: int, b: int):
    expect(a + b).toEq(b + a)
```

- `count=N` specifies the number of random trials (default: 100). `count` must be a positive integer; zero or negative values are rejected at compile time.
- On failure, the counterexample (failing inputs) is printed
- The test stops at the first failure
- Supported parameter types: `int` ([-1000, 1000]), `float` ([-1000.0, 1000.0]), `bool`, `str` (random ASCII, 0-20 chars)

See [Feature interactions](#feature-interactions) for combinations with lifecycle hooks and `@timeout` (the lifecycle and `@timeout` combinations are compile errors in the current MVP).

---

## Test Coverage

Run tests with the `--coverage` (or `--cov`) flag to measure line coverage:

```bash
ry test --coverage                    # All tests with coverage summary
ry test --cov tests/spec/math.test.ry # Single file
ry test --coverage tests/spec/        # Directory
```

### Output

```
Test Coverage Summary:
  tests/spec/math.test.ry    100.0%  (74/74 lines)
  tests/spec/strings.test.ry  92.3%  (24/26 lines)
  -------------------------------------------------
  Total                        95.1%  (98/100 lines)
```

- Only user code is reported; standard library files are excluded
- `--coverage` with `--parallel` falls back to sequential execution

---

## Test Outline

Use `--outline` to display the `describe`/`it` structure of test files without executing any test bodies:

```bash
ry test --outline tests/spec/mock.test.ry
```

Output:

```
describe mock
  it should replace function
  it should auto-restore after it block
  it should mock with arguments
describe verify
  it should count calls
  it should count zero calls
```

- Works with individual files, directories, and `-p` (all test files)
- `@each` parameterized tests show the format template with an `(@each)` suffix
- `@property` tests show the label with a `(@property)` suffix

---

## Test Description Style

`it` descriptions should start with `should` so they read naturally as complete sentences in test output:

```text
it should add integers
it should reject invalid input
it should return error when file is missing
```

**Preferred:**

| Description | Notes |
|-------------|-------|
| `"should add integers"` | verb in base form |
| `"should reject short passwords"` | verb in base form |
| `"should return error for missing file"` | verb in base form |
| `"should add {0} + {1} = {2}"` | parameterized: verb in base form |
| `"should verify addition is commutative"` | property-based |

**Avoid:**

| Description | Reason |
|-------------|--------|
| `"adds integers"` | third-person verb, reads awkwardly as "it adds" |
| `"integer addition"` | noun phrase, not a sentence |
| `"handles error"` | third-person verb |

`describe` blocks use noun or topic phrases (e.g., `"Arithmetic"`, `"List"`, `"GET /users"`) — they do not need `should`.

---

## Troubleshooting

Common errors and what they usually mean.

### `unknown directive '@it'` or `'<name>' requires 'from testing import <name>'`

**Symptom:** The compiler rejects `@it`, `@describe`, `@beforeEach`, `expect`, `mock`, etc. with a message like `unknown directive '@it'` or `'expect' requires 'from testing import expect'`.

**Cause:** The `testing` module symbols you used are not in scope.

**Fix:** Add an explicit `from testing import ...` line at the top of the file naming every symbol you use:

```ry
from testing import describe, it, beforeEach, afterEach, expect, mock, verify
```

`testing` is not auto-imported even for files ending in `.test.ry`.

### `verify("X")` returns 0 when you expected a positive count

**Symptom:** `expect(verify("compute")).toEq(2)` fails because `verify` returned `0`.

**Cause:** One of:

- The function under test was never called through the mocked binding (e.g. typo in the mocked name)
- The mock was registered for one **overload** but the call took a different overload
- `mock(...)` ran **after** the function under test, so the real implementation was executed and the call was not counted
- The mock was set in a different `@it` block — mocks are auto-cleared at the end of every `@it` block

**Fix:**

- Check the spelling of the mocked function name (`mock(compute, ...)` takes the function as a bare identifier — see the [`## Mocking`](#mocking) section for the signature).
- If the function is overloaded, use the signature form (`mock("compute(int, int)", ...)`) or aggregate-verify with `verify("compute")` (bare name). See [Mocking overloaded functions](#mocking-overloaded-functions).
- Move the `mock(...)` call to the top of the `@it` block, before any call to the function under test.

### `expect(0.1 + 0.2).toEq(0.3)` fails

**Symptom:** A floating-point equality assertion fails with values that look equal (`expected: 0.3, got: 0.30000000000000004`).

**Cause:** IEEE 754 representation. `0.1 + 0.2` is not bit-identical to `0.3`; `toEq` uses bit-equality for floats.

**Fix:** Use `toBeCloseTo` for float comparisons:

```ry
expect(0.1 + 0.2).toBeCloseTo(0.3)            # default decimals=2
expect(1.00001).toBeCloseTo(1.00002, 4)       # custom decimals
```

The matcher table near the top of this file lists the full set of float-aware matchers (`toBeCloseTo`, `toBeNaN`, `toBeInfinity`, `toBeFinite`).

### `@afterEach` saw partially set-up state after a `@timeout` test fired

**Symptom:** A test with `@timeout(N)` triggered the timeout, and `@afterEach` ran but observed state that did not match what a normal-path `@beforeEach` would have produced (e.g. an uninitialized handle, a partial fixture).

**Cause:** As of #1781, `@afterEach` runs even when the body or `@beforeEach` mid-times-out, so the cleanup hook may see a snapshot where only some of the setup completed. The phase that fired (body or `@beforeEach`) is not distinguished at runtime — `@afterEach` is invoked unconditionally.

**Fix:** Make `@afterEach` defensive against partial setup — guard with nil / sentinel checks before touching each fixture (e.g. `if handle != nil: close(handle)`). If a setup step's success is meaningful to the cleanup, assign a flag in `@beforeEach` and gate the matching cleanup on it. See [Directives reference](directives.md#afterEach).

### `@afterEach` itself timed out (`@afterEach (timeout after Nms)` printed)

**Symptom:** stdout shows a secondary failure line `@afterEach (timeout after Nms)` below a test's normal outcome.

**Cause:** `@afterEach` blew through its own `N`-ms `setitimer` budget (the AE phase is independent of the body phase since #1781). This protects subsequent tests from being blocked by a hung cleanup, at the cost of a secondary failure line for the offending `@it`.

**Fix:** Trim long-running operations out of `@afterEach`, or raise the test's `@timeout(N)` so both body and cleanup fit within the budget. Worst-case wall-clock per timed test is `2N` (body + AE), so set `N` accordingly.

### `@each` or `@property` combined with `@timeout` is rejected at compile time

**Symptom:** A test that carries both `@each(...)` and `@timeout(N)` (or `@property(...)` and `@timeout(N)`) fails to compile.

**Cause:** `@timeout` is mutually exclusive with `@each` and `@property` in the current MVP — the timer applies to one test invocation, and loop-style runners cannot share a single timer budget across iterations.

**Fix:** Drop `@timeout` from the parameterized test, or extract a single representative case into a separate `@it` that carries `@timeout`. See the [Directives reference](directives.md#timeout).

---

## Recipes

Worked patterns for situations that compose multiple `testing` features. Each example is taken from `tests/spec/*.test.ry` so you can run it with `./build/ry test <path>` (macOS: `./build-rust/ry test <path>`) to see it pass.

### Queueing return values with `mockReturnValueOnce`

Use when order-dependent logic must observe a specific sequence of return values (e.g. retry-then-succeed, paginated fetch).

```ry
from testing import it, describe, expect, mockReturnValueOnce

fn fetchOnceStr() -> str:
  return "orig"

@describe("mockReturnValueOnce")
fn mockReturnValueOnceTests():
  @it("should return queued str values in order then fall back to original")
  fn shouldQueueStr():
    mockReturnValueOnce("fetchOnceStr", "first")
    mockReturnValueOnce("fetchOnceStr", "second")
    expect(fetchOnceStr()).toEq("first")
    expect(fetchOnceStr()).toEq("second")
    expect(fetchOnceStr()).toEq("orig")
```

After the queue is drained the next call falls back to the default `mock(...)` lambda if one is set, otherwise to the original implementation. The `mockReturnValueOnce(name, value)` subsection under `## Mocking` documents the full precedence rules.

### Observing real implementation calls with `spy`

Use when the real function must run (e.g. it has side effects you care about) and you only want to inspect arguments after the fact.

```ry
from testing import it, describe, expect, spy, verify, verifyCalledWith

fn computeReal(x: int) -> int:
  return x * 3

@describe("spy + verifyCalledWith")
fn spyVerifyCalledWithTests():
  @it("matches int argument")
  fn matchesIntArg():
    spy("computeReal")
    computeReal(7)
    computeReal(8)
    computeReal(7)
    expect(verifyCalledWith("computeReal", 7)).toEq(2)
    expect(verifyCalledWith("computeReal", 8)).toEq(1)
    expect(verifyCalledWith("computeReal", 999)).toEq(0)
```

`spy` differs from `mock` in that the real implementation still runs. If `mock` and `spy` target the same name in one `@it` block, the `mock` replacement wins — see the `spy(name)` subsection for the precedence rule.

### Float-precision comparisons with `toBeCloseTo`

Use whenever the value under test comes from float arithmetic — the bit-exact value rarely matches the decimal literal you would write by hand.

```ry
from testing import it, describe, expect

@describe("Float approximate matchers")
fn floatApproxMatchers():
  @it("should pass toBeCloseTo for 0.1 + 0.2 vs 0.3 (default decimals=2)")
  fn shouldPassToBeCloseToFloatSum():
    expect(0.1 + 0.2).toBeCloseTo(0.3)
  @it("should pass toBeCloseTo with custom decimals=4")
  fn shouldPassToBeCloseToCustomDecimals4():
    expect(1.00001).toBeCloseTo(1.00002, 4)
```

`toBeCloseTo(expected, decimals=2)` passes when `|actual - expected| < 0.5 × 10^-decimals`. Use a larger `decimals` argument when you need tighter precision.

### Property-based invariants with `@property`

Use when a property holds for **any** input (e.g. algebraic laws, monotonicity, idempotence).

```ry
from testing import it, describe, expect, property

@describe("@property tests")
fn propertyTests():
  @property(count=100)
  @it("should verify addition is commutative")
  fn shouldVerifyAdditionIsCommutative(a: int, b: int):
    expect(a + b).toEq(b + a)

  @property(count=50)
  @it("should verify double is always even")
  fn shouldVerifyDoubleIsAlwaysEven(n: int):
    expect(n * 2 % 2).toEq(0)
```

`count` controls how many randomized inputs are generated. `@property` cannot coexist with `@timeout` (compile error) — see the [Directives reference](directives.md#timeout).

### Mocking overloaded functions with the signature form

Use when the same function name has multiple overloads and you need to mock one without disturbing the others.

```ry
from testing import it, describe, expect, mock, mockResetAll

fn addNum(a: int, b: int) -> int:
  return a + b

fn addNum(a: float, b: float) -> float:
  return a + b

@describe("mock overloaded fns - signature form")
fn mockOverloadSigTests():
  @it("mock with explicit signature dispatches by sig key")
  fn mockSigBasic():
    mock("addNum(int, int)", (x: int, y: int) => 999)
    expect(addNum(1, 2)).toEq(999)
    expect(addNum(1.0, 2.0)).toEq(3.0)  # float overload untouched
    mockResetAll()
```

The signature key is the parameter type list inside parentheses, exactly as it appears in the function declaration. `verify("addNum")` (bare name) aggregates the call counts across all overloads; `verify("addNum(int, int)")` returns the count for that overload only. See [Mocking overloaded functions](#mocking-overloaded-functions).

### Per-test mock setup with `@beforeEach`

Use when several `@it` blocks in the same describe need a freshly-installed mock with a zeroed call counter. Hoisting `mock(...)` into `@beforeEach` reuses every `@it`'s auto-restore boundary, so each test sees a clean slate without manual `mockReset` / `mockClear`.

```ry
from testing import describe, it, beforeEach, mock, verify, expect

fn fetchValue() -> int:
    return 7

@describe("mock installed via beforeEach is fresh per it")
fn mockInBeforeEach():
    @beforeEach
    fn be():
        mock(fetchValue, () => 42)

    @it("first it sees the mocked value")
    fn firstIt():
        expect(fetchValue()).toEq(42)
        expect(verify("fetchValue")).toEq(1)

    @it("second it sees re-installed mock with fresh call count")
    fn secondIt():
        expect(verify("fetchValue")).toEq(0)
        expect(fetchValue()).toEq(42)
        expect(verify("fetchValue")).toEq(1)
```

The Feature interactions section's [`mock` / `spy` inside `@beforeEach`](#mock--spy-inside-beforeeach) entry shows the same fixture as the canonical evidence that this combination is supported and documents the auto-restore mechanics. Compare with [Scope `mock` as tightly as possible](#scope-mock-as-tightly-as-possible) below — describe-wide setup is only the right choice when every `@it` really does need the same mocked baseline.

### Setup patterns for `@each` parameterized tests

Use when an `@each` parameterized `@it` needs per-iteration or once-per-describe setup. `@each` cannot coexist with `@beforeEach` (compile error — see [Lifecycle hooks with `@each` / `@property`](#lifecycle-hooks-with-each--property)), so the workarounds below are the canonical alternatives.

```ry
from testing import describe, it, each, beforeAll, expect

fn freshCounter() -> int:
    return 0


# Pattern A: per-iteration setup invoked at the top of the @it body
@describe("per-iteration setup invoked at the top of the @it body")
fn perIterationInBody():
    @each([(2,), (3,), (5,)])
    @it("iteration {0} starts with a freshly-zeroed counter")
    fn iter(seed: int):
        counter = freshCounter()
        counter = counter + seed
        expect(counter).toEq(seed)


# Pattern B: shared setup hoisted into @beforeAll
@describe("shared setup hoisted into @beforeAll, reused across @each iterations")
fn sharedSetupInBeforeAll():
    factor = 0

    @beforeAll
    fn ba():
        factor = 10

    @each([(1,), (2,), (3,)])
    @it("iteration {0} multiplies the hoisted factor by the parameter")
    fn iter(x: int):
        expect(factor).toEq(10)
        expect(x * factor).toEq(x * 10)
```

Pattern A places setup that must run fresh each iteration (call counters, allocations, mutable scratch state) at the top of the `@it` body — or in a helper, as shown with `freshCounter()`. Pattern B hoists work that does not vary per iteration (loading a reference value, opening a shared connection) into `@beforeAll`, which fires once before the `@each` loop. Both patterns are exercised by `tests/spec/parameterized_lifecycle.test.ry`.

---

## Best Practices

Conventions that prevent footguns. None are enforced at compile time — they catch the failures that would otherwise reach CI or code review.

### Do not commit code carrying `@only`

`@only` makes the test runner execute only the marked tests, skipping every other one — exactly what you want during local debugging, and exactly what you do not want in CI. A stray `@only` in `main` silently suppresses real coverage.

A simple pre-commit / CI guard:

```bash
git grep -nE '^[[:space:]]*@only\b' tests/ && exit 1
```

Treat any `@only` hit in a commit as a blocker.

### Scope `mock` as tightly as possible

By default `mock(...)` is `@it`-local: it is cleared at the end of the `@it` block. Use that default unless the fixture is genuinely shared across every test in the describe. Hoisting `mock(...)` into a `@beforeEach` (or into the describe scope) makes test failures harder to localize because the same mock state flows into multiple tests.

If you do need shared mock state, reset it explicitly in `@afterEach` to avoid accumulated state — recall that the `@describe` body runs **once**, not per-test (see [Accumulation semantics (differs from Jest)](#accumulation-semantics-differs-from-jest)).

### Pair `verify(...)` with at least one behavioral assertion

`verify("compute")` answers "was it called N times?" but not "did it produce the right effect?" If the system under test calls `compute` exactly N times but with the wrong arguments — or produces the wrong observable output — a verify-count-only test passes. Always combine call-count checks with either:

- `verifyCalledWith(...)` for specific argument shapes, or
- an `expect(...)` on the system's externally observable output (return value, mutated state, log output).

### Keep `@beforeAll` cheap

`@beforeAll` runs once per describe; `@beforeEach` runs once per test. Put heavyweight setup that does **not** depend on per-test state (loading a fixture file, building a parser) in `@beforeAll`; put per-test state (resetting a counter, allocating a fresh buffer) in `@beforeEach`. Hook bodies cannot declare new variables — they may only reassign describe-scope variables. See [Mutability rules](#mutability-rules).

### Name `it` descriptions in `should ...` form

`it` descriptions should read as full sentences (`it should add integers`, not `it adds integers`). The full convention with preferred/avoided patterns is in [Test Description Style](#test-description-style).

---

## Limitations

- Nested-describe inheritance of lifecycle hooks is not supported (each `@describe` owns its own hooks) — see [Nested @describe lifecycle](#nested-describe-lifecycle) under Feature interactions
- A test with `@timeout(N)` may consume up to `2N` ms of wall-clock — the body phase and the `@afterEach` phase each get their own fresh `N`-ms `setitimer` budget (#1781). A hung `@afterEach` surfaces as a secondary failure line and does NOT block subsequent tests; see [Directives reference](directives.md#timeout) for the full composition rules.

---

## IR Golden Tests (FileCheck)

Ry exposes an `--emit-llvm-ir` flag that runs the full compiler pipeline (parse → type-check → codegen) and prints the **unoptimized LLVM IR** to stdout without executing the program. Combined with [LLVM FileCheck](https://llvm.org/docs/CommandGuide/FileCheck.html), this enables declarative structural assertions on the generated IR.

### When to use

Use FileCheck goldens when you need to assert IR structure directly:

- Opaque pointer convention (`ptr` instead of typed `i8*`)
- Arithmetic overflow patterns (`llvm.sadd.with.overflow`)
- ARC retain/release order in CoW or lambda capture scenarios
- Result/Error type layout (`%Result = type { i1, i64, ptr }`)

For behavioral correctness ("does this produce the right answer?"), use `ry test` instead.

### `ry --emit-llvm-ir` contract

```bash
ry --emit-llvm-ir run <file.ry>   # Emit unoptimized IR for a .ry file
ry --emit-llvm-ir -c '<source>'   # Emit IR for inline source
```

- Runs parse → type-check → codegen only; **does not JIT-run the program**
- Prints unoptimized IR to stdout (codegen output before LLVM O2 passes)
- On success: exits with code 0
- On parse/codegen error: prints diagnostics to stderr, exits non-zero, stdout is empty

### File location

Golden files live in `tests/filecheck/*.ry`. Each `.ry` file is both a valid Ry source file and a FileCheck script — `# CHECK:` lines are Ry comments that FileCheck reads as directives.

### Writing a golden

Create a `.ry` file in `tests/filecheck/`. Place `# CHECK:` directives at the top as comments:

```ry
# FileCheck golden: brief description of what is verified.
#
# CHECK-LABEL: define i64 @myFunc(i64 %x)
# CHECK:         alloca i64
# CHECK:         ret i64

fn myFunc(x: int) -> int:
  return x
```

**Authoring guidelines:**

- Use `# CHECK:` (Ry `#` comment syntax; `//` is not a Ry comment and causes a parse error)
- Write patterns against **unoptimized IR** — optimization passes are not applied, so `alloca`/`store`/`load` sequences for function arguments are always present
- All pointer types are `ptr` (LLVM 17+ opaque pointer convention); never write `i64*`, `i8*`, etc.
- Use `CHECK-LABEL:` to anchor patterns to a specific function definition (`define ... @funcName(...)`)
- Use `CHECK-NEXT:` for consecutive-line assertions; use `CHECK-DAG:` for order-independent assertions
- Use `CHECK-NOT:` to assert that an instruction does not appear
- When a function requires `Ok`/`Err`/`Result` (stdlib types), the file is run from the project root so `package.toml` resolves stdlib automatically; no special flag is needed

### Running locally

```bash
# Run all FileCheck goldens via CTest
ctest --test-dir build -L filecheck --output-on-failure

# Run a single golden manually
./build/ry --emit-llvm-ir run tests/filecheck/function_call.ry \
  | /opt/homebrew/opt/llvm@21/bin/FileCheck tests/filecheck/function_call.ry

# Install FileCheck (macOS)
brew install llvm@21   # → /opt/homebrew/opt/llvm@21/bin/FileCheck

# Install FileCheck (Linux)
sudo apt-get install llvm-21-tools   # → /usr/lib/llvm-21/bin/FileCheck
```

CMake auto-detects FileCheck at configure time. If not found, `cmake --preset default` prints a status message and skips the `filecheck` CTest label — other tests are unaffected.

> **macOS**: the commands above are the Linux/CI form. On macOS substitute `build-rust/` for `build/` (so `ctest --test-dir build-rust`, `./build-rust/ry`) and `cmake --preset rust-emit` for `cmake --preset default` — post-Rust-cutover preset split (`AGENTS.md` § "Build & Test").

### CI gate

The `filecheck` CI job runs on pull requests and pushes to `main`. It builds only the `ry` binary (not `ry_tests`), so it completes quickly. It uses `continue-on-error: true` (warn-only) during the initial rollout. FileCheck ships with the source-built LLVM 21 baked into the GHCR CI image (`ghcr.io/<owner>/ry-ci:llvm-21`) at `/usr/local/llvm/bin/FileCheck`, so no separate install step is needed.
