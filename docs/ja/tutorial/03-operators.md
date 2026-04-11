[English](../../tutorial/03-operators.md) | [日本語](03-operators.md) | [繁體中文](../../zh/tutorial/03-operators.md)

# 03 - 演算子

<- [02 - 変数と型](02-variables-and-types.md) / 次 -> [04 - 制御構文](04-control-flow.md)

---

## 算術演算子

| 演算子 | 説明 | 例 | 結果 |
|--------|------|----|------|
| `+` | 加算 | `3 + 2` | `5` |
| `-` | 減算 | `3 - 2` | `1` |
| `*` | 乗算 / 文字列繰り返し | `3 * 2` | `6` |
| `/` | 除算（常に float） | `7 / 2` | `3.5` |
| `//` | 整数除算（int 同士なら int、片方が float なら float） | `7 // 2` | `3` |
| `%` | 剰余 | `7 % 3` | `1` |
| `**` | 累乗（常に float） | `2 ** 10` | `1024` |

```python
a = 10
b = 3

print(a + b)    # 13
print(a - b)    # 7
print(a * b)    # 30
print(a / b)    # 3.3333...（float）
print(a // b)   # 3（int）
print(a % b)    # 1
print(2 ** 8)   # 256（float）
```

> **オーバーフロー安全性:** `int` の算術演算（`+`、`-`、`*`、単項 `-`）は結果が 64 ビット符号付き範囲をオーバーフローするとランタイムエラーを発生させます。オーバーフローする定数式はコンパイル時に検出されます。低レベル型（`i32`、`u8` など）はサイレントにラップアラウンドします -- 明示的なオーバーフロー制御には `checked_add`/`saturating_add`/`wrapping_add` を使ってください。

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
x = 5
y = 10

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
t = true
f = false

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
| `&` | ビットAND | `5 & 3` -> `1` |
| `\|` | ビットOR | `5 \| 3` -> `7` |
| `^` | ビットXOR | `5 ^ 3` -> `6` |
| `~` | ビットNOT（単項） | `~5` -> `-6` |
| `<<` | 左シフト | `1 << 3` -> `8` |
| `>>` | 算術右シフト | `8 >> 2` -> `2` |
| `>>>` | 論理右シフト | `-1 >>> 1` -> `9223372036854775807` |

```python
a = 0b1010   # 10
b = 0b1100   # 12

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
| `//=` | 整数除算代入 | `x = x // n` |
| `**=` | 累乗代入 | `x = x ** n` |
| `&=` | ビットAND代入 | `x = x & n` |
| `|=` | ビットOR代入 | `x = x \| n` |
| `^=` | ビットXOR代入 | `x = x ^ n` |
| `<<=` | 左シフト代入 | `x = x << n` |
| `>>=` | 右シフト代入 | `x = x >> n` |

```python
x = 10
x += 5    # x == 15
x -= 3    # x == 12
x *= 2    # x == 24
x /= 4    # x == 6（float になる）
```

---

## インクリメント・デクリメント演算子

変数を 1 増減させるための省略記法です。

| 演算子 | 説明 | 同等の式 |
|--------|------|---------|
| `x++` | 1 を加算 | `x = x + 1` |
| `x--` | 1 を減算 | `x = x - 1` |

```python
count = 0
count++       # count == 1
count++       # count == 2
count--       # count == 1
```

> **注意**: ステートメントとしてのみ使用可能で、式の中では使えません。

---

## 型昇格ルール

演算において `int` と `float` が混在する場合の挙動を以下に示します。

```python
# + - * は片方が float なら結果は float
print(1 + 2)      # 3 (int)
print(1 + 2.0)    # 3 (float)
print(1.0 + 2)    # 3 (float)

# / は常に float
print(4 / 2)      # 2 (float)

# // は両方 int なら int、片方でも float なら float
print(7 // 2)     # 3 (int)
print(7.0 // 2)   # 3 (float)

# ** は常に float
print(2 ** 3)     # 8 (float)

# % は両辺 int なら int、片方 float なら float
print(7 % 3)      # 1 (int)
print(7.5 % 2)    # 1.5 (float)

# + は両辺 str なら文字列結合
print("foo" + "bar")   # "foobar"

# * は片方が str、もう片方が int なら文字列繰り返し
print("ab" * 3)        # "ababab"
print(3 * "ab")        # "ababab"
```

---

## 所属演算子

| 演算子 | 説明 | 例 |
|--------|------|----|
| `in` | 所属チェック | `2 in {1, 2, 3}` -> `true` |
| `not in` | 否定の所属チェック | `4 not in {1, 2, 3}` -> `true` |

```python
s = {1, 2, 3}
print(2 in s)        # true
print(4 not in s)    # true
```

---

<- [02 - 変数と型](02-variables-and-types.md) / 次 -> [04 - 制御構文](04-control-flow.md)
