[English](../../reference/math.md) | [日本語](math.md) | [繁體中文](../../zh/reference/math.md)

# 数学関数 (`math`)

## 概要

`math` パッケージは数学定数と関数を提供します。`std` パッケージと異なり、自動インポートされません。明示的なインポートで使用します。

```python
from math import sqrt, PI, sin
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
from math import PI, E, Inf, NaN

circumference = 2.0 * PI * radius
```

---

## 絶対値

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `abs(x)` | `(int) -> int` | 整数の絶対値 |
| `abs(x)` | `(float) -> float` | 浮動小数点数の絶対値 |

```python
from math import abs

abs(-5)      # 5
abs(-3.14)   # 3.14
```

---

## 丸め

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `floor(x)` | `(float) -> int` | 切り捨て |
| `floor(x, digits)` | `(float, int) -> float` | 指定の小数桁数で切り捨て |
| `ceil(x)` | `(float) -> int` | 切り上げ |
| `ceil(x, digits)` | `(float, int) -> float` | 指定の小数桁数で切り上げ |
| `round(x)` | `(float) -> int` | 四捨五入（0 から遠い側へ） |
| `round(x, digits)` | `(float, int) -> float` | 指定の小数桁数で四捨五入（0 から遠い側へ） |

```python
from math import floor, ceil, round

floor(3.7)           # 3
ceil(3.2)            # 4
round(3.5)           # 4

round(3.14159, 2)    # 3.14
floor(3.789, 1)      # 3.7
ceil(3.123, 1)       # 3.2
```

2 引数形式は負の `digits` を受け付け、10 のべき乗で丸めることができます:

```python
round(1234.5, -2)    # 1200.0
round(1750.0, -3)    # 2000.0
```

丸めは C99 の half-away-from-zero セマンティクス（`round(x * 10^digits) / 10^digits` として実装）を使い、1 引数形式と一致します。これは Python の banker's rounding とは異なります -- 例えば `round(2.675, 2) == 2.68` で、`2.67` ではありません。`NaN` と `±Inf` はそのまま通過します。

---

## 冪乗・平方根

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `sqrt(x)` | `(float) -> float` | 平方根 |
| `pow(x, y)` | `(float, float) -> float` | x の y 乗 |
| `pow(x, y)` | `(int, int) -> int` | 高速累乗を使った整数累乗 |

```python
from math import sqrt, pow

sqrt(9.0)       # 3.0
pow(2.0, 3.0)   # 8.0
pow(2, 10)      # 1024
pow(-2, 3)      # -8
```

整数オーバーロードは指数が負の場合にランタイムエラーを発生させます（`pow(2, -1)` は `pow() integer exponent must be non-negative` で中断）。オーバーフローは暗黙的にラップされ、Ry 既存の整数演算モデルと一致します。

---

## 対数

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `log(x)` | `(float) -> float` | 自然対数 (底 e) |
| `log(x, base)` | `(float, float) -> float` | 任意の底の対数 |
| `log2(x)` | `(float) -> float` | 底 2 の対数 |
| `log10(x)` | `(float) -> float` | 常用対数 (底 10) |

```python
from math import log

log(8.0, 2.0)      # 3.0
log(100.0, 10.0)   # 2.0
```

`log(x, base)` は `log(x) / log(base)` として計算されるため、いずれかの引数の定義域エラーは `NaN` または `-Inf` として伝播します。

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
from math import is_nan, is_inf, NaN, Inf

is_nan(NaN)   # true
is_inf(Inf)   # true
is_nan(1.0)   # false
```
