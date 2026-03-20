[English](11-testing.md) | [日本語](../ja/tutorial/11-testing.md) | [繁體中文](../zh/tutorial/11-testing.md)

# Testing

[<- Prev: Design by Contract](10-contracts.md)

Ry has a built-in RSpec-style test syntax using `describe`, `it`, and `expect`. For the full specification, see [Testing Reference](../reference/testing.md).

---

## Running Tests

```bash
ry test                       # Auto-discover and run all *.test.ry files
ry test tests/my_test.test.ry # Run a specific test file
```

The exit code is `0` if all tests pass, `1` if any test fails.

When run without arguments, `ry test` searches for `ry.toml` to find the project root, then recursively discovers all `*.test.ry` files.

---

## Writing Tests

Use `describe` to group related tests and `it` to define individual test cases.

```python
describe("Calculator"):
    it("adds integers"):
        expect(1 + 2).to_eq(3)

    it("subtracts integers"):
        expect(5 - 3).to_eq(2)

    it("checks booleans"):
        expect(3 > 1).to_be_true()
```

- `describe` and `it` use **trailing block syntax**: a function call followed by `:` turns the indented block into a lambda
- `describe` / `expect` are only available with `ry test` (compile error with normal `ry` execution)

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
| `to_contain(val)` | Asserts container includes value | List, Set, str |

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

```python
fn fetch_data() -> str:
    return "real data"

describe("mocking"):
    it("replaces function"):
        mock(fetch_data, fn(): "fake")
        expect(fetch_data()).to_eq("fake")

    it("auto-restores"):
        expect(fetch_data()).to_eq("real data")
```

### `verify(fn_name)`

Returns the number of times a mocked function was called.

```python
describe("verify"):
    it("counts calls"):
        mock(fetch_data, fn(): "fake")
        fetch_data()
        fetch_data()
        expect(verify(fetch_data)).to_eq(2)
```

---

## Limitations

- Nesting of `describe` is not supported
- `before_each` / `after_each` are not supported
- Overloaded and `@native` functions cannot be mocked

---

[<- Prev: Design by Contract](10-contracts.md)
