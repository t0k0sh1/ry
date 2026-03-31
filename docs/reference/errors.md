[English](errors.md) | [日本語](../ja/reference/errors.md) | [繁體中文](../zh/reference/errors.md)

# Error Reference

## Error Format

ry displays compile errors in a Rust-inspired rich format that shows the exact location of the error with source context:

```
error: cannot reassign @const variable: x
  --> main.ry:5:1
  |
5 | x = 10
  | ^ cannot reassign @const variable: x
```

Each error message includes:
- **Error level and message** (`error: ...`)
- **File location** (`--> file:line:col`)
- **Source line** with the relevant code
- **Caret indicator** (`^`) pointing to the exact column

## Compile Errors

| Error | Cause | Example |
|-----------|------|-----|
| Use of undeclared variable | Referenced a variable that has not been declared | `print(x)` (`x` is undeclared) |
| Reassignment to @const variable | Reassigned to a variable declared with `@const` | `@const x = 1` -> `x = 2` |
| Redeclaration of same-named variable | Redeclared a variable with the same name in the same scope | `x = 1` -> another declaration of `x` |
| Type-changing reassignment | Assigned a value of a different type to a variable | `x = 1` -> `x = 3.14` |
| Type annotation mismatch | Declared type and assigned value type differ | `x: int = 3.14` |
| Overload return type conflict | Defined overloads with the same parameter types but different return types only | Two functions with parameters `(int, int)` returning `int` and `float` |
| Overload resolution failure | No overload matches the argument types | `function add(a: int, b: int)` called with `add(1.0, 2.0)` |
| Float in bitwise operation | Passed `float` type to `&`, `\|`, `^`, `~`, `<<`, `>>` | `3.14 & 1` |
| Empty list | Cannot infer type from empty list literal `[]` | `xs = []` |
| Empty map | Cannot infer type from empty map literal `{}` | `m = {}` |
| Tuple out-of-range index | Accessed a non-existent index on a tuple | `t = (1, 2)` -> `t.2` |
| break/continue outside loop | Used `break` or `continue` outside a `for`/`while` loop | `break` at function top level |
| Module import inside block | Used `from` statement inside a function or conditional block | `from math` inside a function |
| Circular import | Modules import each other | `a.ry` imports `b.ry` and `b.ry` imports `a.ry` |
| Duplicate field name | Defined the same field name twice in a struct | Defining `x` twice in `type T: x: int` |
| Non-exhaustive when | `when` does not cover all patterns | Some enum variants uncovered, missing `None` for Option, missing `Ok`/`Err` for Result, no `_` for literals |
| `?` on non-Result type | Applied `?` to an expression that is not a `Result` type | `x = 42` -> `x?` |
| `?` in non-Result function | Used `?` in a function that does not return `Result` | `function foo() -> int:` -> `bar()?` |

### Compile Error Examples

```python
# Reassignment to @const variable
@const
x = 10
x = 20   # Error

# Type-changing reassignment
n = 1
n = "hello"   # Error: assigning str to int variable

# Empty list
xs = []   # Error: type cannot be inferred

# break outside loop
break   # Error: outside loop

# Import inside block
function foo():
    from math   # Error: top level only

# Duplicate field name
record Bad:
    x: int
    x: float   # Error: x is duplicated
```

---

## Runtime Errors

| Error | Cause | Example |
|-----------|------|-----|
| List out-of-range access | List index exceeds bounds | `xs = [1, 2, 3]` -> `xs[5]` |
| Map non-existent key access | Referenced a key that does not exist in the map | `m = {"a": 1}` -> `m["b"]` |
| Contract violation | A `require`, `ensure`, or `invariant` condition evaluated to false | See [Design by Contract](contracts.md) |

All runtime errors terminate the process with `exit(1)`.

### Runtime Error Examples

```python
# List out-of-range access
xs = [1, 2, 3]
print(xs[10])   # Runtime error: exit(1)

# Map non-existent key access
m = {"a": 1}
print(m["z"])   # Runtime error: exit(1)
```
