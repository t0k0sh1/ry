[English](errors.md) | [日本語](../ja/reference/errors.md) | [繁體中文](../zh/reference/errors.md)

# Error Reference

## Compile Errors

| Error | Cause | Example |
|-----------|------|-----|
| Assignment to undeclared variable | Assigned to a variable that has not been declared | `x = 1` (`x` is undeclared) |
| Reassignment to let variable | Reassigned to a variable declared with `let` | `let x = 1` -> `x = 2` |
| Redeclaration of same-named variable | Redeclared a variable with the same name in the same scope | `let x = 1` -> `let x = 2` |
| Type-changing reassignment | Assigned a value of a different type to a variable | `let x = 1` -> `x = 3.14` |
| Type annotation mismatch | Declared type and assigned value type differ | `let x: int = 3.14` |
| Overload return type conflict | Defined overloads with the same parameter types but different return types only | Two functions with parameters `(int, int)` returning `int` and `float` |
| Overload resolution failure | No overload matches the argument types | `fn add(a: int, b: int)` called with `add(1.0, 2.0)` |
| Float in bitwise operation | Passed `float` type to `&`, `\|`, `^`, `~`, `<<`, `>>` | `3.14 & 1` |
| Empty list | Cannot infer type from empty list literal `[]` | `let xs = []` |
| Empty map | Cannot infer type from empty map literal `{}` | `let m = {}` |
| Tuple out-of-range index | Accessed a non-existent index on a tuple | `let t = (1, 2)` -> `t.2` |
| break/continue outside loop | Used `break` or `continue` outside a `for`/`while` loop | `break` at function top level |
| Module import inside block | Used `from` statement inside a function or conditional block | `from math` inside a function |
| Circular import | Modules import each other | `a.ry` imports `b.ry` and `b.ry` imports `a.ry` |
| Duplicate field name | Defined the same field name twice in a struct | Defining `x` twice in `type T: x: int` |
| Non-exhaustive match | match does not cover all patterns | Some enum variants uncovered, missing `None` for Option, no `_` for literals |
| `!!` return type mismatch | `!!` used in function not returning `(T, Error?)` | Using `!!` in a function returning `int` |
| `!!` operand type mismatch | `!!` applied to non `(T, Error?)` value | Applying `!!` to a plain `int` |

### Compile Error Examples

```python
# Reassignment to let variable
let x = 10
x = 20   # Error

# Type-changing reassignment
let n = 1
n = "hello"   # Error: assigning str to int variable

# Empty list
let xs = []   # Error: type cannot be inferred

# break outside loop
break   # Error: outside loop

# Import inside block
fn foo():
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
| List out-of-range access | List index exceeds bounds | `let xs = [1, 2, 3]` -> `xs[5]` |
| Map non-existent key access | Referenced a key that does not exist in the map | `let m = {"a": 1}` -> `m["b"]` |
| Contract violation | A `require`, `ensure`, or `invariant` condition evaluated to false | See [Design by Contract](contracts.md) |

All runtime errors terminate the process with `exit(1)`.

### Runtime Error Examples

```python
# List out-of-range access
let xs = [1, 2, 3]
print(xs[10])   # Runtime error: exit(1)

# Map non-existent key access
let m = {"a": 1}
print(m["z"])   # Runtime error: exit(1)
```
