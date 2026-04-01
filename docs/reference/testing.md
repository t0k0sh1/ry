[English](testing.md) | [日本語](../ja/reference/testing.md) | [繁體中文](../zh/reference/testing.md)

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

### describe / it

```
describe("description", ():
    it("test case name", ():
        # test body
        expect(actual_value).to_eq(expected_value)
    )
)
```

- `describe` and `it` take a description string and a **lambda argument** `():` as the second parameter
- `it` blocks and other statements (e.g., variable declarations) can be written inside a `describe` block
- Each `it` block is an independent test case
- `describe` / `expect` are only available with `ry test` (compile error with normal `ry` execution)

### Trailing Block Syntax

Any function call (except `describe`/`it`/`mock`) can use trailing block syntax. A colon after `()` causes the indented block to be passed as a no-argument lambda in the last argument position:

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
| `to_eq(expected)` | Equality comparison | int, float, bool, str |
| `to_not_eq(expected)` | Asserts not equal | int, float, bool, str |
| `to_be_true()` | Asserts `true` | bool |
| `to_be_false()` | Asserts `false` | bool |
| `to_be_none()` | Asserts `None` | Option |
| `to_be_some()` | Asserts Option is `Some` | Option |
| `to_be_ok()` | Asserts Result is `Ok` | Result |
| `to_be_err()` | Asserts Result is `Err` | Result |
| `to_contain(val)` | Asserts container includes value | List, Set, Map, str |
| `to_not_contain(val)` | Asserts container does not include value | List, Set, Map, str |
| `to_be_greater_than(v)` | Asserts `actual > v` | int, float |
| `to_be_less_than(v)` | Asserts `actual < v` | int, float |
| `to_be_greater_than_or_eq(v)` | Asserts `actual >= v` | int, float |
| `to_be_less_than_or_eq(v)` | Asserts `actual <= v` | int, float |
| `to_have_length(n)` | Asserts length equals `n` | List, Set, Map, str |
| `to_be_empty()` | Asserts length is 0 | List, Set, Map, str |
| `to_start_with(prefix)` | Asserts string starts with prefix | str |
| `to_end_with(suffix)` | Asserts string ends with suffix | str |

### fail

Immediately marks the current test as failed.

```
it("should not reach here", ():
    fail("unexpected error")
)
```

- `fail()` — marks the test as failed with a generic message
- `fail(msg)` — marks the test as failed with a custom message
- Execution continues after `fail()` (does not abort the test)
- Only available in `ry test` mode

---

## Output Format

```
Calculator
  + adds numbers
  + subtracts
  - fails test (red)
    line 10: expected 3, got 2

2 passed, 1 failed
```

- `+` indicates pass (green), `-` indicates failure (red)
- On failure, the line number and expected/actual values are displayed

---

## Example

```
describe("Arithmetic", ():
    it("adds integers", ():
        expect(1 + 2).to_eq(3)

    )
    it("compares strings", ():
        expect("hello").to_eq("hello")

    )
    it("checks booleans", ():
        expect(3 > 1).to_be_true()

    )
)
describe("Booleans", ():
    it("false check", ():
        expect(1 > 2).to_be_false()
    )
)
```

---

## Mocking

### mock(fn_name, replacement)

Replaces a function with a mock implementation for the current `it` block. The mock is automatically cleared when the `it` block ends.

```
function fetch_data() -> str:
    return "real data"

describe("mocking", ():
    it("replaces function", ():
        mock(fetch_data, () => "fake")
        expect(fetch_data()).to_eq("fake")

    )
    it("auto-restores", ():
        expect(fetch_data()).to_eq("real data")
    )
)
```

- The first argument is the function name (identifier, not a string)
- The second argument is a replacement lambda
- The replacement must have the same parameter types and return type as the original function
- `require` and `ensure` contracts on the original function are still enforced when the mock is called
- Mocks are automatically restored at the end of each `it` block

### verify(fn_name)

Returns the number of times a mocked function was called (as `int`).

```
describe("verify", ():
    it("counts calls", ():
        mock(fetch_data, () => "fake")
        fetch_data()
        fetch_data()
        expect(verify(fetch_data)).to_eq(2)
    )
)
```

### Limitations

- Overloaded functions cannot be mocked
- Capture-based closures cannot be used as replacements (use plain lambdas)
- `@native function` functions cannot be mocked

---

## Parameterized Tests (@each)

`@each` runs the same test with multiple sets of parameters. Attach it to an `it` block with a list of tuples:

```
@each([
    (1, 2, 3),
    (0, 0, 0),
    (-1, 1, 0)
])
it("adds {0} + {1} = {2}", (a: int, b: int, expected: int):
    expect(a + b).to_eq(expected)
)
```

- The list must contain tuples whose arity matches the lambda parameter count
- Placeholders `{0}`, `{1}`, ... in the description are replaced with the parameter values
- Each tuple generates an independent test case
- Supported parameter types: `int`, `float`, `bool`, `str`

---

## Property-Based Tests (@property)

`@property` generates random inputs and runs the test multiple times:

```
@property(count=100)
it("addition is commutative", (a: int, b: int):
    expect(a + b).to_eq(b + a)
)
```

- `count=N` specifies the number of random trials (default: 100)
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
  it replaces function
  it auto-restores after it block
  it with arguments
describe verify
  it counts calls
  it zero calls
```

- Works with individual files, directories, and `-p` (all test files)
- `@each` parameterized tests show the format template with an `(@each)` suffix
- `@property` tests show the label with a `(@property)` suffix

---

## Limitations

- Nesting of `describe` is not supported
- `before_each` / `after_each` are not supported
