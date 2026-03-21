[English](math.md) | [日本語](../ja/reference/math.md) | [繁體中文](../zh/reference/math.md)

# Math Functions (`std.math`)

## Overview

The `std.math` package provides mathematical constants and functions. Unlike the `std` package, it is not automatically imported. Use explicit import to access the functions.

```python
from std.math import sqrt, PI, sin
```

---

## Constants

Constants are accessed as zero-argument functions.

| Function | Return Type | Description |
|----------|-------------|-------------|
| `PI()` | `float` | Pi (3.141592653589793) |
| `E()` | `float` | Euler's number (2.718281828459045) |

```python
from std.math import PI, E

let circumference = 2.0 * PI() * radius
```

---

## Special Values

| Function | Signature | Description |
|----------|-----------|-------------|
| `inf()` | `() -> float` | Positive infinity |
| `nan()` | `() -> float` | Not a Number |

---

## Absolute Value

| Function | Signature | Description |
|----------|-----------|-------------|
| `abs(x)` | `(int) -> int` | Absolute value of integer |
| `abs(x)` | `(float) -> float` | Absolute value of float |

```python
from std.math import abs

abs(-5)      # 5
abs(-3.14)   # 3.14
```

---

## Rounding

| Function | Signature | Description |
|----------|-----------|-------------|
| `floor(x)` | `(float) -> int` | Round down to nearest integer |
| `ceil(x)` | `(float) -> int` | Round up to nearest integer |
| `round(x)` | `(float) -> int` | Round to nearest integer |

```python
from std.math import floor, ceil, round

floor(3.7)    # 3
ceil(3.2)     # 4
round(3.5)    # 4
```

---

## Power and Root

| Function | Signature | Description |
|----------|-----------|-------------|
| `sqrt(x)` | `(float) -> float` | Square root |
| `pow(x, y)` | `(float, float) -> float` | x raised to the power of y |

```python
from std.math import sqrt, pow

sqrt(9.0)       # 3.0
pow(2.0, 3.0)   # 8.0
```

---

## Logarithm

| Function | Signature | Description |
|----------|-----------|-------------|
| `log(x)` | `(float) -> float` | Natural logarithm (base e) |
| `log2(x)` | `(float) -> float` | Base-2 logarithm |
| `log10(x)` | `(float) -> float` | Base-10 logarithm |

---

## Exponential

| Function | Signature | Description |
|----------|-----------|-------------|
| `exp(x)` | `(float) -> float` | e raised to the power of x |

---

## Trigonometric

| Function | Signature | Description |
|----------|-----------|-------------|
| `sin(x)` | `(float) -> float` | Sine (x in radians) |
| `cos(x)` | `(float) -> float` | Cosine (x in radians) |
| `tan(x)` | `(float) -> float` | Tangent (x in radians) |

---

## Inverse Trigonometric

| Function | Signature | Description |
|----------|-----------|-------------|
| `asin(x)` | `(float) -> float` | Arc sine (result in radians) |
| `acos(x)` | `(float) -> float` | Arc cosine (result in radians) |
| `atan(x)` | `(float) -> float` | Arc tangent (result in radians) |

---

## Two-argument Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `atan2(y, x)` | `(float, float) -> float` | Arc tangent of y/x (result in radians) |
| `hypot(x, y)` | `(float, float) -> float` | Hypotenuse: sqrt(x^2 + y^2) |

---

## Predicates

| Function | Signature | Description |
|----------|-----------|-------------|
| `is_nan(x)` | `(float) -> bool` | True if x is NaN |
| `is_inf(x)` | `(float) -> bool` | True if x is positive or negative infinity |

```python
from std.math import is_nan, is_inf, nan, inf

is_nan(nan())   # true
is_inf(inf())   # true
is_nan(1.0)     # false
```
