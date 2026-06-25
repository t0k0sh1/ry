# Testing

Ry test files use RSpec-style directives and helpers from `ry.testing`. Tests run with the `ry test` subcommand.

## Running Tests

```bash
ry test              # discover all *.test.ry files in the project
ry test tests/spec   # run a directory recursively
ry test file.test.ry # run one file
ry test -p           # parallel workers = CPU count - 1, minimum 1
ry test -p 8         # explicit worker count
ry test -w           # watch mode
ry test --coverage   # line coverage for one test file
ry test --outline    # print test structure without running bodies
```

Exit code is `0` when all executed tests pass and `1` when any test fails. Skipped and todo tests do not affect the exit code.

Each discovered file runs in its own child process. `-p` controls worker count only; subprocess isolation is always used.

## Basic Syntax

```ry
from ry.testing import describe, it, expect

@describe("Arithmetic")
fn arithmeticTests():
    @it("should add integers")
    fn shouldAddIntegers():
        expect(1 + 2).toEq(3)
```

Rules:

- `@it` / `@describe` functions must not declare a return type.
- `@it` functions have no parameters unless paired with `@each` or `@property`.
- `@describe` bodies execute once. Variables declared there are captured by nested tests.
- Captures inside `@it` bodies are read-only; hooks may mutate describe-scope variables.
- `@it` / `@describe` are available only under `ry test`.

Import behavior:

- `@it` / `@describe` are directive declarations from `share/std/testing/testing.ry`; missing imports produce `unknown directive`.
- `expect`, `mock`, `spy`, `fail`, and call-verification helpers are compiler intrinsics with testing-import checks.
- `verify` is an ordinary public function from `ry.testing`.

## Matchers

| Matcher | Supported actual values |
|---|---|
| `toEq(expected)`, `toNotEq(expected)` | scalar values, collections, Option, Result, records, tuples, unions |
| `toBeTrue()`, `toBeFalse()` | `bool` |
| `toBeNone()`, `toBeSome()` | `Option<T>` |
| `toBeOk()`, `toBeErr()` | `Result<T, E>` |
| `toContain(v)`, `toNotContain(v)` | `str`, `List<T>`, `Set<T>`, `Map<K, V>` keys |
| `toBeGreaterThan(v)`, `toBeLessThan(v)`, `toBeGreaterThanOrEq(v)`, `toBeLessThanOrEq(v)` | `int`, `float` |
| `toHaveLen(n)`, `toBeEmpty()` | `str`, List, Set, Map |
| `toStartWith(prefix)`, `toEndWith(suffix)`, `toMatch(pattern)` | `str` |
| `toBeCloseTo(value[, decimals])` | `int`, `float`; `decimals` is an integer literal in `0..15` |
| `toBeBetween(min, max)` | `int`, `float` |
| `toBeOneOf(list)` | scalar list membership |
| `toBeNaN()`, `toBeInfinity()`, `toBeFinite()` | `float` |

`fail()` marks the current test as failed and continues execution. `fail(msg)` uses a custom message.

## Selection Directives

| Directive | Behavior |
|---|---|
| `@skip @it(...)` | test is not executed, but its body is still compiled |
| `@only @it(...)` | focuses the current file; non-`@only` tests in that file are implicitly skipped |
| `@todo @it(...)` | placeholder; body is not emitted and may reference unfinished code |

Mutual combinations such as `@skip @only` are rejected. These directives apply to `@it`, including `@each` / `@property` tests; they do not apply to `@describe`.

Output markers:

| Marker | Meaning |
|---|---|
| `+` | passed |
| `-` | failed |
| `~` | skipped |
| `?` | todo |

## Lifecycle Hooks

Hooks factor setup and teardown around tests:

| Hook | Runs |
|---|---|
| `@beforeAll` | once before tests in the file/describe scope |
| `@beforeEach` | before each test in the scope |
| `@afterEach` | after each test in the scope |
| `@afterAll` | once after tests in the scope |

For a describe with `N` tests:

```text
@beforeAll
(@beforeEach -> @it -> @afterEach) * N
@afterAll
```

File-level hooks wrap every test in the file. Describe-level hooks wrap only tests in that describe. File `@beforeEach` runs before describe `@beforeEach`; describe `@afterEach` runs before file `@afterEach`.

Constraints:

- hook functions have no parameters and no declared return type
- at most one hook of each kind per scope
- a function cannot carry multiple lifecycle directives
- hooks cannot be combined with `@it`, `@describe`, `@timeout`, `@skip`, `@only`, `@todo`, `@each`, or `@property`
- `@beforeEach` / `@afterEach` are not supported with `@each` or `@property`

`@describe` state is shared across tests. Reset state explicitly in `@beforeEach` when each test needs a fresh value.

`@afterEach` still runs when a timed test body hits `@timeout(N)`. Cleanup must tolerate partial setup.

## Parameterized And Property Tests

`@each` runs one test body for each tuple of parameters:

```ry
from ry.testing import it, each, expect

@each([(1, 2), (3, 6)])
@it("should double {0}")
fn doubles(input: int, expected: int):
    expect(input * 2).toEq(expected)
```

`@property` runs generated values through an invariant. It is intended for pure properties with deterministic assertions.

Rules:

- `@each` / `@property` provide the parameters for the `@it` function.
- `@each` and `@property` cannot be combined with `@timeout`.
- `@beforeAll` / `@afterAll` can surround the whole iteration set.
- `@beforeEach` / `@afterEach` per iteration are not supported.

## Mocking And Spies

| Helper | Purpose |
|---|---|
| `mock(fnName, replacement)` | replaces a function for the current `@it` block |
| `spy("name")` | records calls while still running the real implementation |
| `verify("name")` | returns total recorded calls as `int`; unknown names return `0` |
| `verifyCalledWith("name", args...)` | returns matching call count |
| `calledWith("name", args...)` | returns whether any recorded call matches |
| `calledTimes("name", n)` | returns whether total calls equal `n` |
| `lastCalledWith("name", args...)` | returns whether the latest call matches |
| `mockReturnValueOnce("name", value)` | queues one return value for a mock |
| `mockClear("name")` | clears recorded calls only |
| `mockReset("name")` | clears calls and queued return values / implementation override for one name |
| `mockResetAll()` | resets all mocks/spies in the current test |

Rules:

- `mock` takes an identifier or signature string plus a replacement lambda.
- `spy` and verification helpers use string names.
- Signature strings disambiguate overloaded functions.
- Mock replacements must match the original function signature.
- Mocks and spies are automatically restored at the end of each `@it`.
- A mock installed in `@beforeEach` is installed fresh for each test.
- If both mock and spy are active for the same function, mock behavior takes precedence while call recording remains available.

`verifyCalledWith`, `calledWith`, and `lastCalledWith` validate the function and argument types at compile time. Supported argument snapshots include scalar values, `str`, lists/sets/maps of scalar values, records/tuples with scalar fields, and function-typed arguments by pointer identity.

## Coverage And Outline

`ry test --coverage` prints executed line coverage for a single test file. `ry test --outline` prints discovered `describe` / `it` structure without running test bodies; it still reports selection suffixes such as `(@skip)`, `(@only)`, `(@todo)`, `(@each)`, and `(@property)`.

## FileCheck IR Goldens

Use FileCheck tests when the contract is LLVM IR shape rather than Ry-level behavior.

Guidelines:

- Put goldens under `tests/filecheck/`.
- Prefer Ry spec tests for user-visible behavior.
- Use `ry --emit-llvm-ir` probes that force the path under test.
- Avoid vacuous goldens: include markers that prove the intended path emitted.
- Normalize ASLR-dependent values when comparing full IR before and after a migration.

## Troubleshooting

| Symptom | Likely cause |
|---|---|
| `unknown directive '@it'` | missing `from ry.testing import it` / `describe` |
| `'<name>' requires 'from testing import <name>'` | missing import for a testing intrinsic |
| `verify("X")` returns `0` | no mock/spy was installed for that name, the name is misspelled, or the overload signature differs |
| `0.1 + 0.2` equality fails | use `toBeCloseTo` for floating-point comparisons |
| `@each` / `@property` with `@timeout` fails | the combination is intentionally rejected |

## Related

- [Directives](directives.md)
- [Project Management](project.md#ry-test---run-tests)
