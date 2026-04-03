[English](../../tutorial/04-control-flow.md) | [日本語](../../ja/tutorial/04-control-flow.md) | [简体中文](04-control-flow.md)

# 控制流

[<- 上一篇：运算符](03-operators.md) | [下一篇：函数 ->](05-functions.md)

---

## if / else

使用 `if` 根据条件进行分支处理。

```python
x = 10

if x > 0:
    print(x)
else:
    print(0)
```

- `else` 可以省略。
- 条件不限于 `bool` 值。对于 `int`，`0` 视为假，非 `0` 视为真。
- `if` 语句可以嵌套。

```python
a = 5
b = 3

if a > 0:
    if b > 0:
        print(a + b)   # 8
```

---

## when

使用 `when:` 进行多分支条件判断，或使用 `match value:` 进行模式匹配。

### 条件分支 `when:`

```python
x = -2

when:
    x > 0:
        print("positive")
    x < 0:
        print("negative")
    else:
        print("zero")
```

当需要链接多个分支时，这是推荐的形式。

### 模式匹配 `match value:`

```python
enum Color:
    Red
    Green
    Blue

c = Color::Green
match c:
    case Color::Red:
        print("red")
    case Color::Green:
        print("green")
    case Color::Blue:
        print("blue")
```

使用此形式安全地解构 enum、`Option`、`Result` 和字面值模式。

### `when:` 表达式

`when:` 也可以用作表达式。`else =>` 分支是必需的。

```python
label = when:
    score >= 90 => "A"
    score >= 80 => "B"
    else => "C"
```

这替代了嵌套的三元表达式，使多分支值选择更具可读性。

### `match` 表达式

`match` 也可以用作表达式，使用 `=>` 从每个分支返回值。

```python
res = match x:
    case Some(v) => v
    case None    => 0

label = match direction:
    case Direction::North => "N"
    case Direction::South => "S"
    case Direction::East  => "E"
    case Direction::West  => "W"

category = match score:
    case n if n >= 90 => "A"
    case n if n >= 80 => "B"
    case _            => "F"
```

match 表达式支持与 match 语句相同的所有模式：字面值、变量、enum、`Option`、`Result`、OR 模式（`|`）、守卫（`if`）和通配符（`_`）。匹配必须是穷尽的。

---

## while 循环

当条件为真时，重复执行块。

```python
i = 3
while i > 0:
    print(i)
    i = i - 1
# 3
# 2
# 1
```

---

## for 循环与 range

可使用列表或 `range` 进行迭代。

```python
for x in [1, 2, 3]:
    print(x)
# 1
# 2
# 3
```

`range(n)` 产生从 `0` 到 `n - 1` 的整数。

```python
for i in range(5):
    print(i)
# 0
# 1
# 2
# 3
# 4
```

`range(start, end)` 产生从 `start` 到 `end - 1` 的整数。

```python
for i in range(2, 5):
    print(i)
# 2
# 3
# 4
```

`..` 范围运算符创建包含两端的范围：`1 .. 3` 产生 `[1, 2, 3]`。

```python
for i in 1 .. 3:
    print(i)
# 1
# 2
# 3
```

使用 `for k, v in map` 可以遍历映射的键值对：

```python
m = {"x": 10, "y": 20}
for k, v in m:
    print(k)
    print(v)
```

---

## break 与 continue

`break` 立即跳出循环。`continue` 跳过当前迭代，进入下一次迭代。

```python
for i in range(10):
    if i == 5:
        break
    if i % 2 == 0:
        continue
    print(i)
# 1
# 3
```

在 `while` 循环中也可同样使用。

```python
n = 0
while true:
    n = n + 1
    if n % 2 == 0:
        continue
    if n > 7:
        break
    print(n)
# 1
# 3
# 5
# 7
```

> **注意**：在嵌套循环中，`break` / `continue` 仅作用于最内层的循环。在循环外使用会产生编译错误。

---

## 嵌套示例

`for` 和 `while` 循环可以嵌套。

```python
for i in range(1, 4):
    for j in range(1, 4):
        if j == 2:
            continue
        print(i * 10 + j)
# 11
# 13
# 21
# 23
# 31
# 33
```

---

## 作用域规则

控制流块有自己的作用域。

### 块作用域

在块内声明的变量无法从块外部引用。

```python
if true:
    inner = 42
# 在此处引用 inner 会产生编译错误
```

### 引用与重新赋值外部变量

可以从块内引用和重新赋值外部的变量。

```python
count = 0
for i in range(5):
    count = count + i
print(count)   # 10
```

### 内层作用域的重新赋值

在块内对变量赋值会修改外层的变量（Python 风格的作用域）。不会产生遮蔽 —— 内层的赋值会修改同一个变量。

```python
x = 1
if true:
    x = 99
    print(x)   # 99
print(x)       # 99
```

---

## 模式匹配

`match value:` 安全地对 enum、`Option`、`Result` 和字面值进行分支。

```python
enum Color:
    Red
    Green
    Blue

c = Color::Green
match c:
    case Color::Red:
        print("red")
    case Color::Green:
        print("green")
    case Color::Blue:
        print("blue")
# green
```

### Option 匹配

使用 `match value:` 安全地处理 `Some` 和 `None` 两种情况。

```python
x: Option<int> = Some(42)
match x:
    case Some(v):
        print(v)
    case None:
        print("nothing")
# 42
```

### 通配符与字面值

`_` 是匹配任何值的通配符模式。也可以使用字面值（数字、字符串、布尔值）进行匹配。

```python
n = 5
match n:
    case 0:
        print("zero")
    case 1:
        print("one")
    case _:
        print("other")
# other
```

### 守卫子句

可以使用 `if` 添加守卫条件。

```python
match n:
    case x if x > 0:
        print("positive")
    case x if x < 0:
        print("negative")
    case _:
        print("zero")
```

> **注意**：`match value:` 必须是穷尽的。对于 enum，必须覆盖所有变体。对于 Option，需要同时覆盖 `Some` 和 `None`。对于字面值，需要 `_` 通配符。

---

[<- 上一篇：运算符](03-operators.md) | [下一篇：函数 ->](05-functions.md)
