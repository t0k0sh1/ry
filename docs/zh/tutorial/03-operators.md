[English](../../tutorial/03-operators.md) | [日本語](../../ja/tutorial/03-operators.md) | [繁體中文](03-operators.md)

# 03 - 運算子

← [02 - 變數與型別](02-variables-and-types.md) / 下一個 → [04 - 控制流程](04-control-flow.md)

---

## 算術運算子

| 運算子 | 說明 | 範例 | 結果 |
|--------|------|----|------|
| `+` | 加法 | `3 + 2` | `5` |
| `-` | 減法 | `3 - 2` | `1` |
| `*` | 乘法 / 字串重複 | `3 * 2` | `6` |
| `/` | 除法（始終為 float） | `7 / 2` | `3.5` |
| `//` | 整數除法（始終為 int） | `7 // 2` | `3` |
| `%` | 取餘 | `7 % 3` | `1` |
| `**` | 次方（始終為 float） | `2 ** 10` | `1024.0` |

```python
let a = 10
let b = 3

print(a + b)    # 13
print(a - b)    # 7
print(a * b)    # 30
print(a / b)    # 3.3333...（float）
print(a // b)   # 3（int）
print(a % b)    # 1
print(2 ** 8)   # 256.0（float）
```

---

## 比較運算子

比較運算子皆回傳 `bool` 值。

| 運算子 | 說明 | 範例 |
|--------|------|----|
| `==` | 等於 | `a == b` |
| `!=` | 不等於 | `a != b` |
| `<` | 小於 | `a < b` |
| `<=` | 小於等於 | `a <= b` |
| `>` | 大於 | `a > b` |
| `>=` | 大於等於 | `a >= b` |

```python
let x = 5
let y = 10

print(x == y)   # false
print(x != y)   # true
print(x < y)    # true
print(x <= y)   # true
print(x > y)    # false
print(x >= y)   # false
```

字串也可使用比較運算子（字典序比較）。

```python
print("abc" == "abc")   # true
print("abc" < "abd")    # true
print("b" > "a")        # true
```

---

## 邏輯運算子

| 運算子 | 說明 | 範例 |
|--------|------|----|
| `and` | 邏輯 AND | `a and b` |
| `or` | 邏輯 OR | `a or b` |
| `not` | 邏輯 NOT | `not a` |

```python
let t = true
let f = false

print(t and f)   # false
print(t or f)    # true
print(not t)     # false
print(not f)     # true
```

---

## 位元運算子

位元運算子僅適用於 `int` 型別。

| 運算子 | 說明 | 範例 |
|--------|------|----|
| `&` | 位元 AND | `5 & 3` → `1` |
| `\|` | 位元 OR | `5 \| 3` → `7` |
| `^` | 位元 XOR | `5 ^ 3` → `6` |
| `~` | 位元 NOT（一元） | `~5` → `-6` |
| `<<` | 左移 | `1 << 3` → `8` |
| `>>` | 算術右移 | `8 >> 2` → `2` |
| `>>>` | 邏輯右移 | `-1 >>> 1` → `9223372036854775807` |

```python
let a = 0b1010   # 10
let b = 0b1100   # 12

print(a & b)    # 8  (0b1000)
print(a | b)    # 14 (0b1110)
print(a ^ b)    # 6  (0b0110)
print(~a)       # -11
print(1 << 4)   # 16
print(32 >> 2)  # 8
```

---

## 複合賦值運算子

更新變數值時可使用的簡寫語法。

| 運算子 | 說明 | 等同的表達式 |
|--------|------|---------|
| `+=` | 加法賦值 | `x = x + n` |
| `-=` | 減法賦值 | `x = x - n` |
| `*=` | 乘法賦值 | `x = x * n` |
| `/=` | 除法賦值 | `x = x / n` |
| `%=` | 取餘賦值 | `x = x % n` |

```python
let x = 10
x += 5    # x == 15
x -= 3    # x == 12
x *= 2    # x == 24
x /= 4    # x == 6.0（會變為 float）
```

---

## 型別提升規則

以下說明運算中 `int` 與 `float` 混合時的行為。

```python
# + - * 當其中一方為 float 時結果為 float
print(1 + 2)      # 3 (int)
print(1 + 2.0)    # 3.0 (float)
print(1.0 + 2)    # 3.0 (float)

# / 始終為 float
print(4 / 2)      # 2.0 (float)

# // 始終為 int
print(7 // 2)     # 3 (int)
print(7.0 // 2)   # 3 (int)

# ** 始終為 float
print(2 ** 3)     # 8.0 (float)

# % 當兩邊皆為 int 時為 int，其中一方為 float 時為 float
print(7 % 3)      # 1 (int)
print(7.5 % 2)    # 1.5 (float)

# + 當兩邊皆為 str 時為字串串接
print("foo" + "bar")   # "foobar"

# * 當一方為 str、另一方為 int 時為字串重複
print("ab" * 3)        # "ababab"
print(3 * "ab")        # "ababab"
```

---

## 歸屬運算子

| 運算子 | 說明 | 範例 |
|--------|------|----|
| `in` | 歸屬檢查 | `2 in {1, 2, 3}` → `true` |
| `not in` | 否定歸屬檢查 | `4 not in {1, 2, 3}` → `true` |

```python
let s = {1, 2, 3}
print(2 in s)        # true
print(4 not in s)    # true
```

---

← [02 - 變數與型別](02-variables-and-types.md) / 下一個 → [04 - 控制流程](04-control-flow.md)
