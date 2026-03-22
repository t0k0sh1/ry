[English](../../reference/math.md) | [日本語](math.md) | [繁體中文](../../zh/reference/math.md)

# 数学関数 (`std.math`)

## 概要

`std.math` パッケージは数学定数と関数を提供します。`std` パッケージと異なり、自動インポートされません。明示的なインポートで使用します。

```python
from std.math import sqrt, PI, sin
```

---

## 定数

| 定数 | 型 | 説明 |
|------|------|------|
| `PI` | `float` | 円周率 (3.141592653589793) |
| `E` | `float` | ネイピア数 (2.718281828459045) |
| `Inf` | `float` | 正の無限大 |
| `NaN` | `float` | 非数 (Not a Number) |

```python
from std.math import PI, E, Inf, NaN

@const
circumference = 2.0 * PI * radius
```

---

## 絶対値

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `abs(x)` | `(int) -> int` | 整数の絶対値 |
| `abs(x)` | `(float) -> float` | 浮動小数点数の絶対値 |

```python
from std.math import abs

abs(-5)      # 5
abs(-3.14)   # 3.14
```

---

## 丸め

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `floor(x)` | `(float) -> int` | 切り捨て |
| `ceil(x)` | `(float) -> int` | 切り上げ |
| `round(x)` | `(float) -> int` | 四捨五入 |

```python
from std.math import floor, ceil, round

floor(3.7)    # 3
ceil(3.2)     # 4
round(3.5)    # 4
```

---

## 冪乗・平方根

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `sqrt(x)` | `(float) -> float` | 平方根 |
| `pow(x, y)` | `(float, float) -> float` | x の y 乗 |

```python
from std.math import sqrt, pow

sqrt(9.0)       # 3.0
pow(2.0, 3.0)   # 8.0
```

---

## 対数

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `log(x)` | `(float) -> float` | 自然対数 (底 e) |
| `log2(x)` | `(float) -> float` | 底 2 の対数 |
| `log10(x)` | `(float) -> float` | 常用対数 (底 10) |

---

## 指数

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `exp(x)` | `(float) -> float` | e の x 乗 |

---

## 三角関数

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `sin(x)` | `(float) -> float` | 正弦 (x はラジアン) |
| `cos(x)` | `(float) -> float` | 余弦 (x はラジアン) |
| `tan(x)` | `(float) -> float` | 正接 (x はラジアン) |

---

## 逆三角関数

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `asin(x)` | `(float) -> float` | 逆正弦 (結果はラジアン) |
| `acos(x)` | `(float) -> float` | 逆余弦 (結果はラジアン) |
| `atan(x)` | `(float) -> float` | 逆正接 (結果はラジアン) |

---

## 2引数関数

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `atan2(y, x)` | `(float, float) -> float` | y/x の逆正接 (結果はラジアン) |
| `hypot(x, y)` | `(float, float) -> float` | 斜辺の長さ: sqrt(x^2 + y^2) |

---

## 判定関数

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `is_nan(x)` | `(float) -> bool` | x が NaN なら true |
| `is_inf(x)` | `(float) -> bool` | x が正または負の無限大なら true |

```python
from std.math import is_nan, is_inf, NaN, Inf

is_nan(NaN)   # true
is_inf(Inf)   # true
is_nan(1.0)   # false
```
