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

### Syntax

Test files use directives (`@it`, `@describe`) and the helpers `expect`, `mock`, `verify`, `verifyCalledWith`, `fail` from the `testing` module. Import them at the top using either `from testing` (wildcard) or `from testing import ...` (named). Several enforcement paths produce different error messages:

- `@it` / `@describe` are declared in `share/std/testing/testing.ry` as `@directive` declarations. Without the import, codegen rejects them via the general directive-resolution mechanism with `unknown directive '@it'` or `unknown directive '@describe'`.
- `expect`, `mock`, `fail`, `verifyCalledWith` are compiler intrinsics tracked separately and rejected with `'<name>' requires 'from testing import <name>'`.
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
| `toContain(val)` | Asserts container includes value | List, Set, Map, str |
| `toNotContain(val)` | Asserts container does not include value | List, Set, Map, str |
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

---

## Output Format

```
Calculator
  + should add numbers
  + should subtract
  - should fail
    line 10: expected 3, got 2

2 passed, 1 failed
```

- `+` indicates pass (green), `-` indicates failure (red)
- On failure, the line number and expected/actual values are displayed

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
- The function must already be mocked via `mock(...)` before `verifyCalledWith` is called; calling on a non-mocked function is a compile error.
- The number and types of `args...` must exactly match the original function's parameter list. Arity mismatch and type mismatch are compile errors.
- Supported argument types: `int`, `float`, `bool`, `str` (since v0.0.22, #1677), `List<T>` where `T ∈ {int, float, bool, str}` (since v0.0.22, #1703), `Set<T>` where `T ∈ {int, float, bool, str}` (since v0.0.22, #1704), `Map<K, V>` where `K, V ∈ {int, float, bool, str}` (since v0.0.22, #1705), record types whose fields are all in `{int, float, bool, str}` (since v0.0.22, #1706), tuple types whose elements are all in `{int, float, bool, str}` (since v0.0.22, #1706), and `fn(...) -> R` (function-typed) arguments compared by pointer equality (since v0.0.22, #1707). Other types (nested `List<List<T>>`, records or tuples containing collections) are rejected at compile time and are tracked for v0.0.x follow-up.
- `List<T>` arguments are compared by deep snapshot: the recorded call snapshot and the verify-side snapshot must agree on length and element-wise equality. Element comparison is byte-exact for `int` / `float` / `bool` and uses NUL-safe length+`memcmp` for `str`.
- `Set<T>` arguments are compared by **unordered** deep snapshot: the recorded and verify-side snapshots must have the same length and the same elements as a set, but element order is irrelevant (e.g. recording `{1, 2, 3}` matches `verifyCalledWith("f", {3, 2, 1})`). Per-element comparison uses the same byte-exact / NUL-safe rules as `List<T>`.
- `Map<K, V>` arguments are compared by **unordered** deep snapshot of the {key → value} pairs: the recorded and verify-side snapshots must have the same length and the same key set, with each key mapping to the same value across the two maps. Insertion order is irrelevant (e.g. recording `{"a": 1, "b": 2}` matches `verifyCalledWith("f", {"b": 2, "a": 1})`). Per-key and per-value comparison uses the same byte-exact / NUL-safe rules as `List<T>`.
- Record arguments are compared by declared **type name** plus field-by-field equality. Two records with structurally identical fields but different declared names (e.g. `Point(1, 2)` vs `Vec(1, 2)`) do not match and are rejected at compile time when the parameter type is fixed. Per-field comparison uses the same byte-exact / NUL-safe rules as `List<T>`.
- Tuple arguments are compared by **arity** plus element-by-element equality. Tuples with different arity do not match and are rejected at compile time. Per-element comparison uses the same byte-exact / NUL-safe rules as `List<T>`.
- `fn(...) -> R` arguments are compared by **pointer equality** on the `{thunk_ptr, env_ptr}` pair extracted from the uniform closure struct, not by structural / behavioral equivalence. Two independently constructed lambdas that happen to be structurally identical (e.g. `(x: int) => x + 1` written twice on different source lines) do not match — only the same closure value (a single named `let f = ...` flowing into both the recorded call and the verify side, or two `let` aliases of the same bare `@public fn`) matches. Capture closures with different captured environments (e.g. `makeAdder(5)` vs `makeAdder(6)`) are distinguished by the per-instance `env_ptr` even though they share a single cached capturing thunk. The fn signature itself is opaque to `verifyCalledWith` — only the pointer pair matters.
- `int` argument literals are widened to `float` automatically when the parameter type is `float` (matching ordinary call-site coercion).
- Returns `0` when no recorded call matches the supplied arguments.

### Limitations

- Overloaded functions cannot be mocked.
- `@native fn` declarations cannot be mocked.
- Capture-based closures **are supported as mock replacements** (since v0.0.22, #1678) — the closure can read or mutate variables from the enclosing scope. The captured environment is released automatically when the `it` block ends.

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

## Limitations

- `before_each` / `after_each` are not supported

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
ry --emit-llvm-ir <file.ry>       # Emit unoptimized IR for a .ry file
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
./build/ry --emit-llvm-ir tests/filecheck/function_call.ry \
  | /opt/homebrew/opt/llvm@21/bin/FileCheck tests/filecheck/function_call.ry

# Install FileCheck (macOS)
brew install llvm@21   # → /opt/homebrew/opt/llvm@21/bin/FileCheck

# Install FileCheck (Linux)
sudo apt-get install llvm-21-tools   # → /usr/lib/llvm-21/bin/FileCheck
```

CMake auto-detects FileCheck at configure time. If not found, `cmake --preset default` prints a status message and skips the `filecheck` CTest label — other tests are unaffected.

### CI gate

The `filecheck` CI job runs on pull requests and pushes to `main`. It builds only the `ry` binary (not `ry_tests`), so it completes quickly. It uses `continue-on-error: true` (warn-only) during the initial rollout. FileCheck ships with the source-built LLVM 21 baked into the GHCR CI image (`ghcr.io/<owner>/ry-ci:llvm-21`) at `/usr/local/llvm/bin/FileCheck`, so no separate install step is needed.
