[English](02-variables-and-types.md) | [日本語](../ja/tutorial/02-variables-and-types.md) | [繁體中文](../zh/tutorial/02-variables-and-types.md)

# 02 - Variables and Types

<- [01 - Getting Started](01-getting-started.md) / Next -> [03 - Operators](03-operators.md)

---

## Constant Declaration with let

The `let` keyword declares an immutable variable (constant). The type is automatically inferred from the value on the right-hand side. The value cannot be changed after declaration.

```python
let x = 42        # Inferred as int
let y = 3.14      # Inferred as float
let flag = true   # Inferred as bool
let name = "Ry"   # Inferred as str
```

---

## Variable Declaration with var

The `var` keyword declares a mutable variable. You can reassign a value of the same type after declaration.

```python
var count = 0
count = count + 1   # OK: reassignment to the same type
```

---

## Type Annotations

You can explicitly specify the type of a variable.

```python
let x: int = 42
let rate: float = 0.5
let ok: bool = false
let msg: str = "hello"
```

A compile error occurs if the type annotation does not match the actual type of the value.

---

## Basic Types

| Type | Description | Literal Examples |
|------|-------------|-----------------|
| `int` | 64-bit integer | `0`, `42`, `-10` |
| `byte` | Unsigned 8-bit integer (0-255) | `let b: byte = 42` |
| `float` | 64-bit floating-point number | `0.0`, `3.14`, `-1.5` |
| `bool` | Boolean | `true`, `false` |
| `str` | String | `"hello"`, `""` |

---

## String Operations

Various operations are available for strings.

```python
let a = "Hello"
let b = "World"

# Concatenation
let c = a + ", " + b   # "Hello, World"

# Comparison (lexicographic order)
print(a == b)   # false
print(a != b)   # true
print(a < b)    # true ("H" < "W")

# Length
print(len(a))   # 5

# Substring checks
let s = "Hello, World!"
print(contains(s, "World"))      # true
print(starts_with(s, "Hello"))   # true
print(ends_with(s, "!"))         # true
```

---

## Escape Sequences

The following escape sequences can be used within strings.

| Sequence | Meaning |
|----------|---------|
| `\n` | Newline |
| `\t` | Tab |
| `\\` | Backslash |
| `\"` | Double quote |
| `\0` | Null character |

```python
print("Hello\nWorld")   # Outputs on two lines
print("A\tB")           # Tab-separated
print("say \"hi\"")     # String containing double quotes
```

---

## Reassignment Rules

Variables declared with `var` can be reassigned. However, the following restrictions apply:

```python
var x = 10
x = 20        # OK: reassignment to the same type
# x = "text" # Error: reassignment with a different type is not allowed
```

`let` variables cannot be reassigned.

```python
let N = 5
# N = 6  # Error: reassignment to a let variable is not allowed
```

Redeclaring a variable with the same name is also not allowed.

```python
let x = 1
# let x = 2  # Error: redeclaring the same name is not allowed
```

---

## Tuple Destructuring

You can unpack a tuple into multiple variables in a single declaration using `let` or `var`.

```python
let a, b = (10, 20)
print(a)   # 10
print(b)   # 20
```

### Wildcard

Use `_` to ignore a position.

```python
let x, _ = (1, 2)   # only x is bound; 2 is discarded
print(x)             # 1
```

### Mutable Destructuring

Use `var` to declare mutable variables.

```python
var a, b = (10, 20)
a = 99
print(a)   # 99
```

### Rules

- The number of variables on the left must match the number of elements in the tuple.
- Each variable follows the same `let`/`var` rules as a regular declaration.
- Nested tuple destructuring is not supported.

---

<- [01 - Getting Started](01-getting-started.md) / Next -> [03 - Operators](03-operators.md)
