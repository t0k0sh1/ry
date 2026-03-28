[English](../../tutorial/02-variables-and-types.md) | [日本語](../../ja/tutorial/02-variables-and-types.md) | [简体中文](02-variables-and-types.md)

# 02 - 变量与类型

<- [01 - 入门](01-getting-started.md) / 下一篇 -> [03 - 运算符](03-operators.md)

---

## 变量声明

在 Ry 中，使用简单的赋值语法声明变量。默认情况下，变量是可变的。

```python
x = 42        # 推断为 int
y = 3.14      # 推断为 float
flag = true   # 推断为 bool
name = "Ry"   # 推断为 str
```

---

## 使用 @const 声明不可变变量

`@const` 指令将变量标记为不可变（常量）。声明后无法更改其值。

```python
@const
x = 42        # 推断为 int

@const
y = 3.14      # 推断为 float

@const
flag = true   # 推断为 bool

@const
name = "Ry"   # 推断为 str
```

---

## 类型标注

可以显式指定变量的类型。

```python
x: int = 42

rate: float = 0.5

ok: bool = false

msg: str = "hello"
```

当类型标注与实际值的类型不一致时，会产生编译错误。

---

## 基本类型

| 类型 | 说明 | 字面值示例 |
|------|------|-----------|
| `int` | 64 位整数 | `0`, `42`, `-10` |
| `u8` | 无符号 8 位整数（0-255） | `b: u8 = 42` |
| `float` | 64 位浮点数 | `0.0`, `3.14`, `-1.5` |
| `bool` | 布尔值 | `true`, `false` |
| `str` | 字符串 | `"hello"`, `""` |

### 底层数值类型

Ry 还提供底层数值类型，用于精确控制内存布局。这些类型**没有隐式转换** —— 必须使用 `as` 进行显式转换。

| 类型 | 说明 | 示例 |
|------|------|------|
| `i8` | 8 位有符号整数 | `x: i8 = 42` |
| `i16` | 16 位有符号整数 | `x: i16 = 100` |
| `i32` | 32 位有符号整数 | `x: i32 = 42` |
| `i64` | 64 位有符号整数 | `x: i64 = 100` |
| `u8` | 8 位无符号整数 | `x: u8 = 200` |
| `u16` | 16 位无符号整数 | `x: u16 = 60000` |
| `u32` | 32 位无符号整数 | `x: u32 = 3000000000` |
| `u64` | 64 位无符号整数 | `x: u64 = 100` |
| `f32` | 32 位浮点数 | `x: f32 = 3.14` |

```python
a: i32 = 10
b: i32 = 20
c = a + b          # OK: i32 + i32 → i32

d = 42
# e = a + d        # Error: cannot mix i32 and int

y = a as int       # 显式转换为 int
z = d as i32       # 显式转换为 i32

# 无符号类型使用无符号运算
x: u32 = 3000000000
y: u32 = 7
q = x / y          # 无符号除法（UDiv）
```

> **注意**: 底层整数的 `/` 执行整数除法（类似 Rust），而非浮点除法。有符号类型使用 `SDiv`，无符号类型使用 `UDiv`。
>
> **注意**: 底层整数的算术运算在溢出时会回绕。有符号类型使用二进制补码，无符号类型使用模运算。如果担心溢出，请使用高级 `int` 类型（64 位）。

### 固定长度数组

对于底层类型，Ry 提供固定长度连续数组 `T[N]`。这些数组在栈上分配，大小在编译时确定。

```python
buf: i32[4] = [1, 2, 3, 4]
print(buf[0])          # 1
buf[2] = 99
print(buf[2])          # 99
print(length(buf))     # 4

pixels: u8[3] = [255, 128, 0]
```

---

## 字符串操作

字符串支持多种操作。

```python
a = "Hello"
b = "World"

# 拼接
c = a + ", " + b   # "Hello, World"

# 比较（字典序）
print(a == b)   # false
print(a != b)   # true
print(a < b)    # true ("H" < "W")

# 长度
print(length(a))   # 5

# 子字符串检查
s = "Hello, World!"
print(contains(s, "World"))      # true
print(starts_with(s, "Hello"))   # true
print(ends_with(s, "!"))         # true
```

---

## 转义序列

字符串中可使用以下转义序列。

| 序列 | 含义 |
|------|------|
| `\n` | 换行 |
| `\r` | 回车 |
| `\t` | Tab |
| `\\` | 反斜杠 |
| `\"` | 双引号 |
| `\0` | 空字符 |

```python
print("Hello\nWorld")   # 分成两行输出
print("A\tB")           # 以 Tab 分隔
print("say \"hi\"")     # 包含双引号的字符串
```

---

## 重新赋值规则

未使用 `@const` 声明的变量可以重新赋值，但有以下限制：

```python
x = 10
x = 20        # OK：重新赋予相同类型的值
# x = "text" # 错误：禁止更改类型的重新赋值
```

`@const` 变量无法重新赋值。

```python
@const
N = 5
# N = 6  # 错误：禁止对 @const 变量重新赋值
```

也无法重新声明同名的变量。

```python
x = 1
# 同一作用域内禁止重新声明同名变量
```

---

## 元组解构

可以在单次声明中将元组拆解为多个变量。

```python
@const
a, b = (10, 20)
print(a)   # 10
print(b)   # 20
```

### 通配符

使用 `_` 忽略特定位置的值。

```python
@const
x, _ = (1, 2)   # 只绑定 x；2 被丢弃
print(x)             # 1
```

### 可变变量解构

省略 `@const` 即可声明可变变量。

```python
a, b = (10, 20)
a = 99
print(a)   # 99
```

### 规则

- 左侧的变量数量必须与元组的元素数量相符。
- 每个变量遵循与普通声明相同的 `@const`/可变规则。
- 不支持嵌套元组解构。

---

<- [01 - 入门](01-getting-started.md) / 下一篇 -> [03 - 运算符](03-operators.md)
