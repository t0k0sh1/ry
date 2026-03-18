[English](testing.md) | [日本語](../ja/reference/testing.md) | [繁體中文](../zh/reference/testing.md)

# Testing

Ry has a built-in RSpec-style test syntax. Test files are executed using the `ry test` subcommand.

---

## Running Tests

```bash
ry test              # Auto-discover and run all *.test.ry files in the project
ry test test_file.ry # Run a specific test file
```

The exit code is 0 if all tests passed, 1 if any test failed.

### Auto-Discovery Mode

When `ry test` is run without arguments, it:

1. Searches for `ry.toml` to find the project root
2. Recursively discovers all `*.test.ry` files under the project root (`.git`, `build`, `node_modules` are skipped)
3. Runs each file and aggregates results

---

## Syntax

### describe / it

```
describe("description"):
    it("test case name"):
        # test body
        expect(actual_value).to_eq(expected_value)
```

- `describe` and `it` use **trailing block syntax**: a function call followed by `:` turns the indented block into a lambda passed as the last argument
- `it` blocks and other statements (e.g., variable declarations) can be written inside a `describe` block
- Each `it` block is an independent test case
- `describe` / `expect` are only available with `ry test` (compile error with normal `ry` execution)

### Trailing Block Syntax

Any function call can use trailing block syntax. A colon after `()` causes the indented block to be passed as a no-argument lambda in the last argument position:

```
# These are equivalent:
foo("arg"):
    bar()

foo("arg", fn():
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
| `to_contain(val)` | Asserts container includes value | List, Set, str |

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
describe("Arithmetic"):
    it("adds integers"):
        expect(1 + 2).to_eq(3)

    it("compares strings"):
        expect("hello").to_eq("hello")

    it("checks booleans"):
        expect(3 > 1).to_be_true()

describe("Booleans"):
    it("false check"):
        expect(1 > 2).to_be_false()
```

---

## Limitations

- Nesting of `describe` is not supported
- `before_each` / `after_each` are not supported
