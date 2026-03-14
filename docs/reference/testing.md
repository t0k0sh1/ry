[English](testing.md) | [日本語](../ja/reference/testing.md) | [繁體中文](../zh/reference/testing.md)

# Testing

Ry has a built-in RSpec-style test syntax. Test files are executed using the `ry test` subcommand.

---

## Running Tests

```bash
ry test test_file.ry
```

The exit code is the number of failed tests (0 = all passed).

---

## Syntax

### describe / it

```
describe "description":
    it "test case name":
        # test body
        expect(actual_value).to_eq(expected_value)
```

- Only `it` blocks can be written inside a `describe` block
- Each `it` block is an independent test case
- `describe` / `expect` are only available with `ry test` (compile error with normal `ry` execution)

### expect / Matchers

| Matcher | Description | Supported Types |
|---|---|---|
| `to_eq(expected)` | Equality comparison | int, float, bool, str |
| `to_be_true()` | Asserts `true` | bool |
| `to_be_false()` | Asserts `false` | bool |
| `to_be_none()` | Asserts `None` | Option |

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
describe "Arithmetic":
    it "adds integers":
        expect(1 + 2).to_eq(3)

    it "compares strings":
        expect("hello").to_eq("hello")

    it "checks booleans":
        expect(3 > 1).to_be_true()

describe "Booleans":
    it "false check":
        expect(1 > 2).to_be_false()
```

---

## Limitations

- Nesting of `describe` is not supported
- `before_each` / `after_each` are not supported
- Glob execution of test files (`ry test tests/`) is not supported
