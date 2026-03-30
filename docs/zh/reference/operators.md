[English](../../reference/operators.md) | [日本語](../../ja/reference/operators.md) | [简体中文](operators.md)

# 运算符参考

## 优先级表

优先级数字越小越高（越先被求值）。

| 优先级 | 运算符 | 说明 | 结合性 |
|---|---|---|---|
| 0 | `?` `!!` | 错误传播（后缀） | 左 |
| 1 | `()` | 分组 | — |
| 2 | `+x` `-x` `~x` | 一元正号、负号、位 NOT | 右 |
| 3 | `**` | 幂运算 | 右 |
| 3.5 | `as` | 类型转换 | 左 |
| 4 | `*` `/` `%` `//` | 乘法、除法、取余、整数除法 | 左 |
| 5 | `+` `-` | 加法、减法 | 左 |
| 6 | `<<` `>>` `>>>` | 位移 | 左 |
| 7 | `&` | 位 AND | 左 |
| 8 | `^` | 位 XOR | 左 |
| 9 | `\|` | 位 OR | 左 |
| 10 | `==` `!=` `<` `<=` `>` `>=` `in` `not in` | 比较、成员测试 | 左 |
| 11 | `not` | 逻辑 NOT | 右 |
| 12 | `and` | 逻辑 AND | 左 |
| 13 | `or` | 逻辑 OR | 左 |
| 13.5 | `??` | 空值合并 | 左 |

## 算术运算符

| 运算符 | 说明 | 示例 |
|---|---|---|
| `+` | 加法 / 字符串拼接 | `1 + 2` -> `3`、`"a" + "b"` -> `"ab"` |
| `-` | 减法 | `5 - 3` -> `2` |
| `*` | 乘法 / 字符串重复 | `4 * 3` -> `12`、`"ab" * 3` -> `"ababab"` |
| `/` | 除法（始终为 float） | `7 / 2` -> `3.5` |
| `//` | 向下取整除法（向 -∞） | `7 // 2` -> `3`、`-7 // 2` -> `-4` |
| `%` | 取余 | `7 % 3` -> `1` |
| `**` | 幂运算（始终为 float） | `2 ** 10` -> `1024.0` |
| `-x` | 一元负号 | `-5`、`-3.14` |
| `+x` | 一元正号 | `+5`（不改变正负号） |

```python
a = 10 // 3    # 3 (int)
b = 10 / 3     # 3.3333... (float)
c = 2 ** 8     # 256.0 (float)
s = "foo" + "bar"  # "foobar"
```

## 比较运算符

全部返回 `bool`。

| 运算符 | 说明 |
|---|---|
| `==` | 等于 |
| `!=` | 不等于 |
| `<` | 小于 |
| `<=` | 小于等于 |
| `>` | 大于 |
| `>=` | 大于等于 |

- 可用于数值类型（int / float）和 bool。
- `str` 之间以字典序（字节顺序）比较。
- 记录类型支持 `==` 和 `!=`，自动生成逐字段比较（参见[结构体参考](structs.md#比较--)）。
- `in` 运算符用于集合、列表、映射的成员测试（`x in s`）。
- `not in` 运算符为 `in` 的否定（`x not in s`）。
- 对于映射，`in` 检查键是否存在。

```python
x = 3 < 5       # true
y = "abc" < "abd"  # true（字典序）
s = {1, 2, 3}
z = 2 in s      # true
w = 4 not in s  # true
xs = [1, 2, 3]
a = 2 in xs     # true（列表线性搜索）
m = {"a": 1}
b = "a" in m    # true（映射键搜索）
```

## 逻辑运算符

| 运算符 | 说明 | 类型 |
|---|---|---|
| `and` | 逻辑 AND | `bool` x `bool` -> `bool` |
| `or` | 逻辑 OR | `bool` x `bool` -> `bool` |
| `not` | 逻辑 NOT | `bool` -> `bool` |

```python
a = true and false   # false
b = true or false    # true
c = not true         # false
```

## 位运算符

仅可用于 `int` 类型。对 `float` 或 `bool` 使用会产生编译错误。

| 运算符 | 说明 | 示例 |
|---|---|---|
| `&` | 位 AND | `0b1100 & 0b1010` -> `0b1000` |
| `\|` | 位 OR | `0b1100 \| 0b1010` -> `0b1110` |
| `^` | 位 XOR | `0b1100 ^ 0b1010` -> `0b0110` |
| `~` | 位 NOT（一元） | `~0` -> `-1` |
| `<<` | 左移 | `1 << 4` -> `16` |
| `>>` | 算术右移 | `16 >> 2` -> `4` |
| `>>>` | 逻辑右移 | `-1 >>> 1` -> `9223372036854775807` |

```python
flags = 0b0001 | 0b0010   # 3
masked = flags & 0b0011   # 3
shifted = 1 << 8          # 256
```

## 错误传播运算符（`?` / `!!`）

后缀 `?` 运算符用于解包 `Result` 值。如果值为 `Ok(v)`，则求值为 `v`。如果值为 `Err(e)`，则外层函数立即返回 `Err(e)`。

`!!` 运算符是 `?` 的别名，语义完全相同。两者可以互换使用。

外层函数必须具有 `Result` 返回类型。

```python
fn safe_divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)

fn compute(a: int, b: int, c: int) -> Result<int, Error>:
    x = safe_divide(a, b)?    # 若 b == 0 则提前返回 Err
    y = safe_divide(x, c)!!
    return Ok(y + 1)
```

这等同于以下 `when` 模式，但更加简洁：

```python
fn compute(a: int, b: int, c: int) -> Result<int, Error>:
    when safe_divide(a, b):
        case Ok(x):
            when safe_divide(x, c):
                case Ok(y):
                    return Ok(y + 1)
                case Err(e):
                    return Err(e)
        case Err(e):
            return Err(e)
```

---

## `when:` 条件表达式

```python
x = when:
    condition => true_value
    else => false_value
```

自上而下求值条件，返回第一个为真的分支表达式。所有结果表达式必须具有相同的类型。`else =>` 为必需，因此该表达式总会产生一个值。

```python
x = when:
    3 > 2 => 10
    else => 20     # 10

s = when:
    false => "yes"
    else => "no"  # "no"

y = when:
    flag1 => 1
    flag2 => 2
    else => 3
```

---

## 范围运算符

`..` 运算符创建包含两端的整数范围。

```python
xs = 1 .. 5    # [1, 2, 3, 4, 5]

for i in 1 .. 3:
    print(i)       # 1 2 3
```

结果是包含从左操作数到右操作数（两端皆含）的所有整数的 `List<int>`。

---

## 空值合并运算符（`??`）

```python
x = option_val ?? default_val
```

如果 `option_val` 为 `Some(v)`，则返回 `v`。否则返回 `default_val`。右操作数必须与 Option 的内部类型相同。

```python
a: int? = Some(10)
b: int? = none

print(a ?? 0)    # 10
print(b ?? 0)    # 0
```

---

## 复合赋值运算符

更新变量的简写形式。`x op= y` 等价于 `x = x op y`。

| 运算符 | 等价表达式 |
|---|---|
| `x += y` | `x = x + y` |
| `x -= y` | `x = x - y` |
| `x *= y` | `x = x * y` |
| `x /= y` | `x = x / y` |
| `x %= y` | `x = x % y` |
| `x //= y` | `x = x // y` |
| `x **= y` | `x = x ** y` |
| `x &= y` | `x = x & y` |
| `x \|= y` | `x = x \| y` |
| `x ^= y` | `x = x ^ y` |
| `x <<= y` | `x = x << y` |
| `x >>= y` | `x = x >> y` |

```python
x = 10
x += 5    # x = 15
x -= 3    # x = 12
x *= 2    # x = 24
x //= 3  # x = 8
x &= 6   # x = 0
```

## 递增 / 递减运算符

仅后缀、仅语句级别的运算符，用于将变量增减 1。内部分别被转换为 `x = x + 1` 和 `x = x - 1`。

| 运算符 | 等价表达式 |
|---|---|
| `x++` | `x = x + 1` |
| `x--` | `x = x - 1` |

```python
count = 0
count++       # count = 1
count++       # count = 2
count--       # count = 1

f = 1.5
f++           # f = 2.5（int 1 会提升为 float）
```

> **注意**：`++` / `--` 只能作为语句使用，不能在表达式中使用。
> `@const` 变量不能使用递增/递减（不可变性会被强制执行）。

---

## 运算的类型规则

| 运算 | 左操作数类型 | 右操作数类型 | 结果类型 |
|---|---|---|---|
| `+ - *` | int | int | int |
| `+ - *` | float | int / float | float |
| `+ - *` | int | float | float |
| `/` | 任意数值 | 任意数值 | float |
| `//` | int | int | int |
| `//` | float 或 int（其中一方为 float） | -- | float |
| `**` | 任意数值 | 任意数值 | float |
| `%` | int | int | int |
| `%` | float 或 int（其中一方为 float） | -- | float |
| `+` | str | str | str |
| `== != < <= > >=` | 数值 / bool / str | 同类型 | bool |
| `*` | str | int | str |
| `in` | 任意 | Set<T> / List<T> / Map<K, V> | bool |
| `not in` | 任意 | Set<T> / List<T> / Map<K, V> | bool |
| `& \| ^ ~ << >> >>>` | int | int | int |
| `and or not` | bool | bool | bool |

## 运算符重载

可以为用户定义类型定义运算符的行为。

### 语法

```python
# 二元运算符（2 个参数）
fn operator+(a: MyType, b: MyType) -> MyType:
    ...

# 一元运算符（1 个参数）
fn operator-(a: MyType) -> MyType:
    ...
```

### 可重载的运算符一览

| 类别 | 运算符 |
|---|---|
| 算术（二元） | `+` `-` `*` `/` `%` `**` `//` |
| 比较（二元） | `==` `!=` `<` `<=` `>` `>=` |
| 位运算（二元） | `&` `\|` `^` `<<` `>>` `>>>` |
| 逻辑（二元） | `and` `or` |
| 成员测试 | `in` |
| 下标 | `[]`（读取）、`[]=`（写入） |
| 调用 | `()` |
| 转换 | `as` |
| 一元 | `-` `~` `not` |
| 复合赋值 | `+=` `-=` `*=` `/=` `%=` `//=` `**=` `&=` `\|=` `^=` `<<=` `>>=` |

### 返回类型约束

比较运算符和逻辑运算符必须返回 `bool`：

| 类别 | 运算符 | 必需返回类型 |
|---|---|---|
| 比较 | `==` `!=` `<` `<=` `>` `>=` | `bool` |
| 逻辑 | `and` `or` `not` | `bool` |
| 成员测试 | `in` | `bool` |
| 转换 | `as` | 必需（目标类型） |

```python
# OK
fn operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

# Error: comparison operator '==' must return 'bool', but returns 'int'
fn operator==(a: Vec2, b: Vec2) -> int:
    return 42
```

算术运算符和位运算符没有返回类型约束。

### 二元 / 一元的区别

依参数个数区分。

```python
# 二元 -
fn operator-(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x - b.x, a.y - b.y)

# 一元 -
fn operator-(v: Vec2) -> Vec2:
    return Vec2(-v.x, -v.y)
```

### 复合赋值运算符重载

复合赋值运算符（`+=`、`-=` 等）可以独立重载。这使得大数据结构可以进行就地优化。

```python
record Matrix:
    data: List
    rows: int
    cols: int

fn operator+=(a: Matrix, b: Matrix) -> Matrix:
    for i in range(len(a.data)):
        a.data[i] = a.data[i] + b.data[i]
    return a
```

#### 解析优先级

当 `x += y` 被求值时：

1. 如果定义了 `operator+=` → 直接调用
2. 如果未定义 `operator+=` 但定义了 `operator+` → 回退到 `x = x + y`
3. 如果都未定义（非内置类型） → 编译错误

```python
record Vec2:
    x: float
    y: float

fn operator+=(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x + b.x, a.y + b.y)

v = Vec2(1.0, 2.0)
v += Vec2(3.0, 4.0)  # 直接调用 operator+=
# v.x == 4.0, v.y == 6.0
```

复合赋值运算符要求恰好 2 个参数，没有返回类型约束。

### 下标运算符重载

`[]`（读取）和 `[]=`（写入）运算符可以为用户定义类型启用自定义下标行为。支持多索引访问（例如 `m[row, col]`）。

```python
record Grid:
    a: int
    b: int
    c: int
    d: int

# 读取：需要 2+ 个参数（对象 + 索引）
fn operator[](g: Grid, row: int, col: int) -> int:
    if row == 0 and col == 0:
        return g.a
    if row == 0 and col == 1:
        return g.b
    if row == 1 and col == 0:
        return g.c
    return g.d

# 写入：需要 3+ 个参数（对象 + 索引 + 值）
fn operator[]=(g: Grid, row: int, col: int, value: int):
    ...

g = Grid(1, 2, 3, 4)
print(g[0, 1])    # 2
g[1, 0] = 99
```

优先尝试用户定义的下标运算符；如果没有匹配，则回退到内置下标行为（用于列表、映射和数组）。

### 成员测试运算符重载

`in` 运算符可以被重载以定义自定义成员测试。必须返回 `bool`。

```python
record Range:
    lo: int
    hi: int

fn operator in(value: int, r: Range) -> bool:
    return value >= r.lo and value < r.hi

r = Range(1, 10)
print(5 in r)       # true
print(15 not in r)  # true
```

优先尝试用户定义的 `in` 运算符；如果没有匹配，则回退到内置行为（用于集合、映射和列表）。定义了 `in` 后，`not in` 自动支持。

### 调用运算符重载

`()` 运算符使记录可以作为可调用对象使用。至少需要 2 个参数（对象 + 参数）。

```python
record Adder:
    base: int

fn operator()(a: Adder, x: int) -> int:
    return a.base + x

add5 = Adder(5)
print(add5(10))    # 15
```

当持有记录值的变量被像函数一样调用时，编译器首先尝试 `operator()` 重载。如果没有匹配，其他调用解析策略（函数、构造函数、lambda）优先。

### 转换运算符重载

`as` 运算符可以被重载以定义自定义类型转换。接受恰好 1 个参数（源值），必须指定返回类型（目标类型）。分派按源类型和返回类型匹配。

```python
record Celsius:
    value: int

record Fahrenheit:
    value: int

fn operator as(c: Celsius) -> Fahrenheit:
    return Fahrenheit(c.value * 9 // 5 + 32)

c = Celsius(100)
f = c as Fahrenheit   # Fahrenheit(212)
```

优先尝试用户定义的 `as` 运算符；如果没有匹配，则回退到内置转换（int、float、bool、str 等）。
