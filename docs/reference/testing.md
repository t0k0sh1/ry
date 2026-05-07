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

Test files use directives (`@it`, `@describe`) and intrinsics (`expect`, `mock`, `verify`, `fail`) from the `testing` module. Every intrinsic a test file uses — `expect`, `mock`, `verify`, or `fail` — must appear in an explicit `from testing import ...` line at the top:

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

### verify(fnName)

Returns the number of times a mocked function was called (as `int`).

```ry
from testing import it, describe, mock, verify, expect

@describe("verify")
fn verifyTests():
    @it("should count calls")
    fn shouldCountCalls():
        mock(fetchData, () => "fake")
        fetchData()
        fetchData()
        expect(verify(fetchData)).toEq(2)
```

- Requires `from testing import verify`

### Limitations

- Overloaded functions cannot be mocked
- Capture-based closures cannot be used as replacements (use plain lambdas)
- `@native fn` declarations cannot be mocked

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
