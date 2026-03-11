# 03 - 演算子

← [02 - 変数と型](02-variables-and-types.md) / 次 → [04 - 制御構文](04-control-flow.md)

---

## 算術演算子

| 演算子 | 説明 | 例 | 結果 |
|--------|------|----|------|
| `+` | 加算 | `3 + 2` | `5` |
| `-` | 減算 | `3 - 2` | `1` |
| `*` | 乗算 | `3 * 2` | `6` |
| `/` | 除算（常に float） | `7 / 2` | `3.5` |
| `//` | 整数除算（常に int） | `7 // 2` | `3` |
| `%` | 剰余 | `7 % 3` | `1` |
| `**` | 累乗（常に float） | `2 ** 10` | `1024.0` |

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

## 比較演算子

比較演算子はすべて `bool` 値を返します。

| 演算子 | 説明 | 例 |
|--------|------|----|
| `==` | 等しい | `a == b` |
| `!=` | 等しくない | `a != b` |
| `<` | より小さい | `a < b` |
| `<=` | 以下 | `a <= b` |
| `>` | より大きい | `a > b` |
| `>=` | 以上 | `a >= b` |

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

文字列にも比較演算子が使えます（辞書順比較）。

```python
print("abc" == "abc")   # true
print("abc" < "abd")    # true
print("b" > "a")        # true
```

---

## 論理演算子

| 演算子 | 説明 | 例 |
|--------|------|----|
| `and` | 論理AND | `a and b` |
| `or` | 論理OR | `a or b` |
| `not` | 論理NOT | `not a` |

```python
let t = true
let f = false

print(t and f)   # false
print(t or f)    # true
print(not t)     # false
print(not f)     # true
```

---

## ビット演算子

ビット演算子は `int` 型にのみ使用できます。

| 演算子 | 説明 | 例 |
|--------|------|----|
| `&` | ビットAND | `5 & 3` → `1` |
| `\|` | ビットOR | `5 \| 3` → `7` |
| `^` | ビットXOR | `5 ^ 3` → `6` |
| `~` | ビットNOT（単項） | `~5` → `-6` |
| `<<` | 左シフト | `1 << 3` → `8` |
| `>>` | 右シフト | `8 >> 2` → `2` |

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

## 複合代入演算子

変数の値を更新する際に使える省略記法です。

| 演算子 | 説明 | 同等の式 |
|--------|------|---------|
| `+=` | 加算代入 | `x = x + n` |
| `-=` | 減算代入 | `x = x - n` |
| `*=` | 乗算代入 | `x = x * n` |
| `/=` | 除算代入 | `x = x / n` |
| `%=` | 剰余代入 | `x = x % n` |

```python
let x = 10
x += 5    # x == 15
x -= 3    # x == 12
x *= 2    # x == 24
x /= 4    # x == 6.0（float になる）
```

---

## 型昇格ルール

演算において `int` と `float` が混在する場合の挙動を以下に示します。

```python
# + - * は片方が float なら結果は float
print(1 + 2)      # 3 (int)
print(1 + 2.0)    # 3.0 (float)
print(1.0 + 2)    # 3.0 (float)

# / は常に float
print(4 / 2)      # 2.0 (float)

# // は常に int
print(7 // 2)     # 3 (int)
print(7.0 // 2)   # 3 (int)

# ** は常に float
print(2 ** 3)     # 8.0 (float)

# % は両辺 int なら int、片方 float なら float
print(7 % 3)      # 1 (int)
print(7.5 % 2)    # 1.5 (float)

# + は両辺 str なら文字列結合
print("foo" + "bar")   # "foobar"
```

---

← [02 - 変数と型](02-variables-and-types.md) / 次 → [04 - 制御構文](04-control-flow.md)
