[English](../../tutorial/03-operators.md) | [日本語](../../ja/tutorial/03-operators.md) | [简体中文](03-operators.md)

# 03 - 运算符

<- [02 - 变量与类型](02-variables-and-types.md) / 下一篇 -> [04 - 控制流](04-control-flow.md)

---

## 算术运算符

| 运算符 | 说明 | 示例 | 结果 |
|--------|------|------|------|
| `+` | 加法 | `3 + 2` | `5` |
| `-` | 减法 | `3 - 2` | `1` |
| `*` | 乘法 / 字符串重复 | `3 * 2` | `6` |
| `/` | 除法（始终为 float） | `7 / 2` | `3.5` |
| `//` | 整除（int 操作数为 int，任一为 float 则为 float） | `7 // 2` | `3` |
| `%` | 取模 | `7 % 3` | `1` |
| `**` | 幂运算（始终为 float） | `2 ** 10` | `1024` |

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

> **溢出安全：** `int` 的算术运算（`+`、`-`、`*`、一元 `-`）如果结果超出 64 位有符号范围，会引发运行时错误。溢出的常量表达式在编译时即被捕获。底层类型（`i32`、`u8` 等）会静默回绕 —— 如需显式溢出控制，请使用 `checked_add`/`saturating_add`/`wrapping_add`。

---

## 比较运算符

所有比较运算符返回 `bool` 值。

| 运算符 | 说明 | 示例 |
|--------|------|------|
| `==` | 等于 | `a == b` |
| `!=` | 不等于 | `a != b` |
| `<` | 小于 | `a < b` |
| `<=` | 小于等于 | `a <= b` |
| `>` | 大于 | `a > b` |
| `>=` | 大于等于 | `a >= b` |

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

比较运算符也适用于字符串（字典序比较）。

```python
print("abc" == "abc")   # true
print("abc" < "abd")    # true
print("b" > "a")        # true
```

---

## 逻辑运算符

| 运算符 | 说明 | 示例 |
|--------|------|------|
| `and` | 逻辑 AND | `a and b` |
| `or` | 逻辑 OR | `a or b` |
| `not` | 逻辑 NOT | `not a` |

```python
t = true
f = false

print(t and f)   # false
print(t or f)    # true
print(not t)     # false
print(not f)     # true
```

---

## 位运算符

位运算符仅适用于 `int` 类型。

| 运算符 | 说明 | 示例 |
|--------|------|------|
| `&` | 位与 | `5 & 3` -> `1` |
| `\|` | 位或 | `5 \| 3` -> `7` |
| `^` | 位异或 | `5 ^ 3` -> `6` |
| `~` | 位取反（一元） | `~5` -> `-6` |
| `<<` | 左移 | `1 << 3` -> `8` |
| `>>` | 算术右移 | `8 >> 2` -> `2` |
| `>>>` | 逻辑右移 | `-1 >>> 1` -> `9223372036854775807` |

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

## 复合赋值运算符

更新变量值时可使用的简写语法。

| 运算符 | 说明 | 等价表达式 |
|--------|------|-----------|
| `+=` | 加法赋值 | `x = x + n` |
| `-=` | 减法赋值 | `x = x - n` |
| `*=` | 乘法赋值 | `x = x * n` |
| `/=` | 除法赋值 | `x = x / n` |
| `%=` | 取模赋值 | `x = x % n` |
| `//=` | 整除赋值 | `x = x // n` |
| `**=` | 幂运算赋值 | `x = x ** n` |
| `&=` | 位与赋值 | `x = x & n` |
| `|=` | 位或赋值 | `x = x \| n` |
| `^=` | 位异或赋值 | `x = x ^ n` |
| `<<=` | 左移赋值 | `x = x << n` |
| `>>=` | 右移赋值 | `x = x >> n` |

```python
x = 10
x += 5    # x == 15
x -= 3    # x == 12
x *= 2    # x == 24
x /= 4    # x == 6（变为 float）
```

---

## 自增 / 自减运算符

将变量增减 1 的简写语法。

| 运算符 | 说明 | 等价表达式 |
|--------|------|-----------|
| `x++` | 加 1 | `x = x + 1` |
| `x--` | 减 1 | `x = x - 1` |

```python
count = 0
count++       # count == 1
count++       # count == 2
count--       # count == 1
```

> **注意**：仅可作为语句使用，不能在表达式中使用。

---

## 类型提升规则

以下说明运算中 `int` 与 `float` 混合时的行为。

```python
# + - * 当其中一方为 float 时结果为 float
print(1 + 2)      # 3 (int)
print(1 + 2.0)    # 3 (float)
print(1.0 + 2)    # 3 (float)

# / 始终为 float
print(4 / 2)      # 2 (float)

# // 始终为 int
print(7 // 2)     # 3 (int)
print(7.0 // 2)   # 3 (int)

# ** 始终为 float
print(2 ** 3)     # 8 (float)

# % 当两边皆为 int 时为 int，其中一方为 float 时为 float
print(7 % 3)      # 1 (int)
print(7.5 % 2)    # 1.5 (float)

# + 当两边皆为 str 时为字符串拼接
print("foo" + "bar")   # "foobar"

# * 当一方为 str、另一方为 int 时为字符串重复
print("ab" * 3)        # "ababab"
print(3 * "ab")        # "ababab"
```

---

## 成员运算符

| 运算符 | 说明 | 示例 |
|--------|------|------|
| `in` | 成员检查 | `2 in {1, 2, 3}` -> `true` |
| `not in` | 否定成员检查 | `4 not in {1, 2, 3}` -> `true` |

```python
s = {1, 2, 3}
print(2 in s)        # true
print(4 not in s)    # true
```

---

<- [02 - 变量与类型](02-variables-and-types.md) / 下一篇 -> [04 - 控制流](04-control-flow.md)
