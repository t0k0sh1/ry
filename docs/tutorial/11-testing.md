[English](11-testing.md) | [日本語](../ja/tutorial/11-testing.md) | [繁體中文](../zh/tutorial/11-testing.md)

# Testing

[<- Prev: Concurrency](10-concurrency.md) | [Next: Building a Project ->](12-building-a-project.md)

Ry has a built-in RSpec-style test syntax using `describe`, `it`, and `expect`. For the full specification, see [Testing Reference](../reference/testing.md).

---

## Running Tests

```bash
ry test                       # Auto-discover and run all *.test.ry files
ry test tests/spec            # Run all *.test.ry files under a directory (recursive)
ry test tests/my_test.test.ry # Run a specific test file
ry test -p                    # Run all tests in parallel (-p or --parallel)
```

The exit code is `0` if all tests pass, `1` if any test fails.

When run without arguments, `ry test` searches for `package.toml` to find the project root, then recursively discovers all `*.test.ry` files.

---

## Writing Tests

Use `describe` to group related tests and `it` to define individual test cases.

```python
describe("Calculator", ():
    it("adds integers", ():
        expect(1 + 2).to_eq(3)

    )
    it("subtracts integers", ():
        expect(5 - 3).to_eq(2)

    )
    it("checks booleans", ():
        expect(3 > 1).to_be_true()
    )
)
```

- `describe` and `it` take a description string and a **lambda argument** `():` as the second parameter
- `describe`, `it`, `expect`, `mock`, and `verify` are only available with `ry test` (compile error with normal `ry` execution)

---

## Matchers

| Matcher | Description | Supported Types |
|---------|-------------|-----------------|
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

`fail()` immediately marks the current test as failed.

```python
it("should handle error", ():
    match result:
        case Ok(v):
            fail("expected error")
        case Err(e):
            expect(e.message).to_eq("not found")
)
```

- `fail()` — marks the test as failed with a generic message
- `fail(message)` — marks the test as failed with a custom message
- Only available in `ry test` mode

---

## Output Format

```
Calculator
  + adds integers
  + subtracts integers
  - checks booleans
    line 10: expected true, got false

2 passed, 1 failed
```

`+` indicates pass (green), `-` indicates failure (red).

---

## Mocking

### `mock(fn_name, replacement)`

Replaces a function with a mock implementation for the current `it` block. The mock is automatically restored when the `it` block ends.
The original function's `require` and `ensure` contracts still run for mocked calls.

```python
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

### `verify(fn_name)`

Returns the number of times a mocked function was called.

```python
describe("verify", ():
    it("counts calls", ():
        mock(fetch_data, () => "fake")
        fetch_data()
        fetch_data()
        expect(verify(fetch_data)).to_eq(2)
    )
)
```

---

## Parameterized Tests

Use `@each` to run the same test with multiple inputs:

```python
@each([(1, 2, 3), (0, 0, 0), (-1, 1, 0)])
it("adds {0} + {1} = {2}", (a: int, b: int, expected: int):
    expect(a + b).to_eq(expected)
)
```

Each tuple becomes a separate test case. `{0}`, `{1}`, etc. in the description are replaced with actual values.

---

## Property-Based Tests

Use `@property` to test with randomly generated inputs:

```python
@property(count=100)
it("addition is commutative", (a: int, b: int):
    expect(a + b).to_eq(b + a)
)
```

The test runs `count` times with random values. On failure, the counterexample is printed.

---

## Testing with Contracts

Contracts (from [Error Handling](08-error-handling.md)) work together with mocking: the original function's `require` and `ensure` contracts **still run** for mocked calls. This means contracts act as implicit test assertions.

```python
function deposit(amount: int, balance: int) -> int:
    require:
        amount > 0
    ensure v:
        v > balance
    return balance + amount

describe("deposit", ():
    it("mocked version still checks contracts", ():
        mock(deposit, (amount: int, balance: int) => balance + amount)
        expect(deposit(10, 100)).to_eq(110)
        # deposit(-1, 100) would terminate with "require failed"
    )
)
```

> **Why this matters**: You can mock implementation details while keeping the contract safety net. If a mock violates a postcondition, the test catches it immediately.

---

## Limitations

- Nesting of `describe` is not supported
- `before_each` / `after_each` are not supported
- Overloaded and `@native` functions cannot be mocked

---

## Exercises

1. **Basic testing**: Write a `describe` block with tests for a `max(a: int, b: int) -> int` function, covering equal values, positive numbers, and negative numbers.

2. **Mocking**: Write a function `fetch_temperature() -> int` that returns a value. Mock it in a test to return a fixed value and use `verify` to check it was called exactly once.

3. **Parameterized tests**: Use `@each` to test a `is_even(n: int) -> bool` function with inputs `[(2, true), (3, false), (0, true), (-4, true)]`.

---

[<- Prev: Concurrency](10-concurrency.md) | [Next: Building a Project ->](12-building-a-project.md)
