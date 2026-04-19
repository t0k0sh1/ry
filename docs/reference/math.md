[English](math.md) | [日本語](../ja/reference/math.md) | [繁體中文](../zh/reference/math.md)

# Math Functions (`math`)

## Overview

The `math` package provides mathematical constants and functions. Unlike the `std` package, it is not automatically imported. Use explicit import to access the functions.

```ry
from math import sqrt, PI, sin
```

Functions that take `float` parameters also accept `int` arguments via implicit `int → float` widening, matching the behaviour of [user-defined overload resolution](functions.md#resolution-priority). For example, `sqrt(4)` and `atan2(1, 1)` are valid — the integers are converted to `float` at the call site. Exact-type overloads still win: `pow(2, 3)` dispatches to the `(int, int) -> int` overload and returns `8` (int), not `8.0`. Low-level integer types (`i8`, `i16`, …) are not widened automatically and require explicit `as` casts.

---

## Constants

| Constant | Type | Description |
|----------|------|-------------|
| `PI` | `float` | Pi (3.141592653589793) |
| `E` | `float` | Euler's number (2.718281828459045) |
| `Inf` | `float` | Positive infinity |
| `NaN` | `float` | Not a Number |

```ry
from math import PI, E, Inf, NaN

circumference = 2.0 * PI * radius
```

---

## Absolute Value

| Function | Signature | Description |
|----------|-----------|-------------|
| `abs(x)` | `(int) -> int` | Absolute value of integer |
| `abs(x)` | `(float) -> float` | Absolute value of float |

```ry
from math import abs

abs(-5)      # 5
abs(-3.14)   # 3.14
```

---

## Rounding

| Function | Signature | Description |
|----------|-----------|-------------|
| `floor(x)` | `(float) -> int` | Round down to nearest integer |
| `floor(x, digits)` | `(float, int) -> float` | Round down to given decimal places |
| `ceil(x)` | `(float) -> int` | Round up to nearest integer |
| `ceil(x, digits)` | `(float, int) -> float` | Round up to given decimal places |
| `round(x)` | `(float) -> int` | Round to nearest integer (half away from zero) |
| `round(x, digits)` | `(float, int) -> float` | Round to given decimal places (half away from zero) |

```ry
from math import floor, ceil, round

floor(3.7)           # 3
ceil(3.2)            # 4
round(3.5)           # 4

round(3.14159, 2)    # 3.14
floor(3.789, 1)      # 3.7
ceil(3.123, 1)       # 3.2
```

The two-argument forms accept negative `digits` for rounding to powers of ten:

```ry
round(1234.5, -2)    # 1200.0
round(1750.0, -3)    # 2000.0
```

Rounding uses C99 half-away-from-zero semantics (via `round(x * 10^digits) / 10^digits`), matching the one-argument form. This differs from Python's banker's rounding — for example, `round(2.675, 2) == 2.68`, not `2.67`. `NaN` and `±Inf` pass through unchanged in the two-argument forms.

> **Note**: The one-argument forms (`floor(x)`, `ceil(x)`, `round(x)`) convert the result to `int`. If the argument is `Inf`, `-Inf`, or `NaN`, a runtime error occurs: `runtime error: floor()/ceil()/round() argument out of int range`.

---

## Power and Root

| Function | Signature | Description |
|----------|-----------|-------------|
| `sqrt(x)` | `(float) -> float` | Square root |
| `pow(x, y)` | `(float, float) -> float` | x raised to the power of y |
| `pow(x, y)` | `(int, int) -> int` | Integer exponentiation via fast-exponentiation |

```ry
from math import sqrt, pow

sqrt(9.0)       # 3.0
pow(2.0, 3.0)   # 8.0
pow(2, 10)      # 1024
pow(-2, 3)      # -8
```

The integer overload raises a runtime error when the exponent is negative (`pow(2, -1)` aborts with `pow() integer exponent must be non-negative`). Overflow wraps silently, matching Ry's existing integer arithmetic model.

---

## Logarithm

| Function | Signature | Description |
|----------|-----------|-------------|
| `log(x)` | `(float) -> float` | Natural logarithm (base e) |
| `log(x, base)` | `(float, float) -> float` | Logarithm with arbitrary base |
| `log2(x)` | `(float) -> float` | Base-2 logarithm |
| `log10(x)` | `(float) -> float` | Base-10 logarithm |

```ry
from math import log

log(8.0, 2.0)      # 3.0
log(100.0, 10.0)   # 2.0
```

`log(x, base)` is computed as `log(x) / log(base)`, so domain errors on either argument propagate as `NaN` or `-Inf`. Due to floating-point arithmetic, results for "clean" bases may not be exact (e.g. `log(1000000.0, 10.0)` evaluates to `5.999999999999999` rather than `6.0`); use `1e-12` tolerance when comparing.

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

```ry
from math import is_nan, is_inf, NaN, Inf

is_nan(NaN)   # true
is_inf(Inf)   # true
is_nan(1.0)   # false
```
