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

## String Interpolation (f-string)

F-strings allow you to embed expressions directly inside strings using `{}`.

```python
let name = "World"
let n = 42
print(f"Hello {name}!")        # Hello World!
print(f"n = {n}")              # n = 42
print(f"sum = {10 + 20}")      # sum = 30
```

Use `{{` and `}}` to include literal braces.

```python
print(f"{{braces}}")   # {braces}
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

<- [01 - Getting Started](01-getting-started.md) / Next -> [03 - Operators](03-operators.md)
