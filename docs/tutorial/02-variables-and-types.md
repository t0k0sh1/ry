[English](02-variables-and-types.md) | [日本語](../ja/tutorial/02-variables-and-types.md) | [繁體中文](../zh/tutorial/02-variables-and-types.md)

# 02 - Variables and Types

<- [01 - Getting Started](01-getting-started.md) / Next -> [03 - Operators](03-operators.md)

---

## Variable Declaration

In Ry, variables are declared using simple assignment syntax. By default, variables are mutable.

```python
x = 42        # Inferred as int
y = 3.14      # Inferred as float
flag = true   # Inferred as bool
name = "Ry"   # Inferred as str
```

---

## Immutable Variables with @const

The `@const` directive marks a variable as immutable (constant). The value cannot be changed after declaration.

```python
@const
x = 42        # Inferred as int

@const
y = 3.14      # Inferred as float

@const
flag = true   # Inferred as bool

@const
name = "Ry"   # Inferred as str
```

---

## Type Annotations

You can explicitly specify the type of a variable.

```python
x: int = 42

rate: float = 0.5

ok: bool = false

msg: str = "hello"
```

A compile error occurs if the type annotation does not match the actual type of the value.

---

## Basic Types

| Type | Description | Literal Examples |
|------|-------------|-----------------|
| `int` | 64-bit integer | `0`, `42`, `-10` |
| `byte` | Unsigned 8-bit integer (0-255) | `b: byte = 42` |
| `float` | 64-bit floating-point number | `0.0`, `3.14`, `-1.5` |
| `bool` | Boolean | `true`, `false` |
| `str` | String | `"hello"`, `""` |

### Low-Level Numeric Types

Ry also provides low-level numeric types for precise control over memory layout. These types have **no implicit conversions** — you must use `as` for explicit casts.

| Type | Description | Example |
|------|-------------|---------|
| `i16` | 16-bit signed integer | `x: i16 = 100` |
| `i32` | 32-bit signed integer | `x: i32 = 42` |
| `f32` | 32-bit floating-point | `x: f32 = 3.14` |

```python
a: i32 = 10
b: i32 = 20
c = a + b          # OK: i32 + i32 → i32

d = 42
# e = a + d        # Error: cannot mix i32 and int

y = a as int       # Explicit cast to int
z = d as i32       # Explicit cast to i32
```

> **Note**: `/` on low-level integers performs integer division (like Rust), not float division.
>
> **Note**: Arithmetic on low-level integers wraps on overflow (two's complement). Use `int` if overflow is a concern.

---

## String Operations

Various operations are available for strings.

```python
a = "Hello"
b = "World"

# Concatenation
c = a + ", " + b   # "Hello, World"

# Comparison (lexicographic order)
print(a == b)   # false
print(a != b)   # true
print(a < b)    # true ("H" < "W")

# Length
print(length(a))   # 5

# Substring checks
s = "Hello, World!"
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
| `\r` | Carriage return |
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

Variables declared without `@const` can be reassigned. However, the following restrictions apply:

```python
x = 10
x = 20        # OK: reassignment to the same type
# x = "text" # Error: reassignment with a different type is not allowed
```

`@const` variables cannot be reassigned.

```python
@const
N = 5
# N = 6  # Error: reassignment to a @const variable is not allowed
```

Redeclaring a variable with the same name is also not allowed.

```python
x = 1
# x = 2 with another declaration in the same scope is not allowed
```

---

## Tuple Destructuring

You can unpack a tuple into multiple variables in a single declaration.

```python
@const
a, b = (10, 20)
print(a)   # 10
print(b)   # 20
```

### Wildcard

Use `_` to ignore a position.

```python
@const
x, _ = (1, 2)   # only x is bound; 2 is discarded
print(x)             # 1
```

### Mutable Destructuring

Omit `@const` to declare mutable variables.

```python
a, b = (10, 20)
a = 99
print(a)   # 99
```

### Rules

- The number of variables on the left must match the number of elements in the tuple.
- Each variable follows the same `@const`/mutable rules as a regular declaration.
- Nested tuple destructuring is not supported.

---

<- [01 - Getting Started](01-getting-started.md) / Next -> [03 - Operators](03-operators.md)
