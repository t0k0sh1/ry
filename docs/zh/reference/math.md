[English](../../reference/math.md) | [日本語](../../ja/reference/math.md) | [繁體中文](math.md)

# 數學函式 (`math`)

## 概述

`math` 套件提供數學常數與函式。與 `std` 套件不同，它不會自動匯入。需要使用明確匯入來存取函式。

```python
from math import sqrt, PI, sin
```

---

## 常數

| 常數 | 型別 | 說明 |
|------|------|------|
| `PI` | `float` | 圓周率 (3.141592653589793) |
| `E` | `float` | 尤拉數 (2.718281828459045) |
| `Inf` | `float` | 正無窮大 |
| `NaN` | `float` | 非數值 (Not a Number) |

```python
from math import PI, E, Inf, NaN

circumference = 2.0 * PI * radius
```

---

## 絕對值

| 函式 | 簽章 | 說明 |
|------|------|------|
| `abs(x)` | `(int) -> int` | 整數的絕對值 |
| `abs(x)` | `(float) -> float` | 浮點數的絕對值 |

```python
from math import abs

abs(-5)      # 5
abs(-3.14)   # 3.14
```

---

## 捨入

| 函式 | 簽章 | 說明 |
|------|------|------|
| `floor(x)` | `(float) -> int` | 向下取整 |
| `floor(x, digits)` | `(float, int) -> float` | 向下取整至指定的小數位數 |
| `ceil(x)` | `(float) -> int` | 向上取整 |
| `ceil(x, digits)` | `(float, int) -> float` | 向上取整至指定的小數位數 |
| `round(x)` | `(float) -> int` | 四捨五入（離零較遠的方向） |
| `round(x, digits)` | `(float, int) -> float` | 四捨五入至指定的小數位數（離零較遠的方向） |

```python
from math import floor, ceil, round

floor(3.7)           # 3
ceil(3.2)            # 4
round(3.5)           # 4

round(3.14159, 2)    # 3.14
floor(3.789, 1)      # 3.7
ceil(3.123, 1)       # 3.2
```

雙參數形式接受負數 `digits` 以四捨五入至 10 的冪：

```python
round(1234.5, -2)    # 1200.0
round(1750.0, -3)    # 2000.0
```

捨入採用 C99 的「離零較遠」語意（透過 `round(x * 10^digits) / 10^digits`），與單參數形式一致。這與 Python 的銀行家捨入不同 — 例如，`round(2.675, 2) == 2.68`，而非 `2.67`。`NaN` 和 `±Inf` 會原樣傳遞。

---

## 冪次與平方根

| 函式 | 簽章 | 說明 |
|------|------|------|
| `sqrt(x)` | `(float) -> float` | 平方根 |
| `pow(x, y)` | `(float, float) -> float` | x 的 y 次方 |
| `pow(x, y)` | `(int, int) -> int` | 透過快速冪算法的整數冪 |

```python
from math import sqrt, pow

sqrt(9.0)       # 3.0
pow(2.0, 3.0)   # 8.0
pow(2, 10)      # 1024
pow(-2, 3)      # -8
```

當指數為負時，整數重載會引發運行時錯誤（`pow(2, -1)` 會以 `pow() integer exponent must be non-negative` 中止）。溢位會靜默回繞，與 Ry 現有的整數算術模型一致。

---

## 對數

| 函式 | 簽章 | 說明 |
|------|------|------|
| `log(x)` | `(float) -> float` | 自然對數 (底數 e) |
| `log(x, base)` | `(float, float) -> float` | 任意底數的對數 |
| `log2(x)` | `(float) -> float` | 底數 2 的對數 |
| `log10(x)` | `(float) -> float` | 常用對數 (底數 10) |

```python
from math import log

log(8.0, 2.0)      # 3.0
log(100.0, 10.0)   # 2.0
```

`log(x, base)` 計算為 `log(x) / log(base)`，因此任一參數的定義域錯誤都會傳播為 `NaN` 或 `-Inf`。

---

## 指數

| 函式 | 簽章 | 說明 |
|------|------|------|
| `exp(x)` | `(float) -> float` | e 的 x 次方 |

---

## 三角函式

| 函式 | 簽章 | 說明 |
|------|------|------|
| `sin(x)` | `(float) -> float` | 正弦 (x 為弧度) |
| `cos(x)` | `(float) -> float` | 餘弦 (x 為弧度) |
| `tan(x)` | `(float) -> float` | 正切 (x 為弧度) |

---

## 反三角函式

| 函式 | 簽章 | 說明 |
|------|------|------|
| `asin(x)` | `(float) -> float` | 反正弦 (結果為弧度) |
| `acos(x)` | `(float) -> float` | 反餘弦 (結果為弧度) |
| `atan(x)` | `(float) -> float` | 反正切 (結果為弧度) |

---

## 雙參數函式

| 函式 | 簽章 | 說明 |
|------|------|------|
| `atan2(y, x)` | `(float, float) -> float` | y/x 的反正切 (結果為弧度) |
| `hypot(x, y)` | `(float, float) -> float` | 斜邊長度: sqrt(x^2 + y^2) |

---

## 判定函式

| 函式 | 簽章 | 說明 |
|------|------|------|
| `is_nan(x)` | `(float) -> bool` | 若 x 為 NaN 則回傳 true |
| `is_inf(x)` | `(float) -> bool` | 若 x 為正或負無窮大則回傳 true |

```python
from math import is_nan, is_inf, NaN, Inf

is_nan(NaN)   # true
is_inf(Inf)   # true
is_nan(1.0)   # false
```
