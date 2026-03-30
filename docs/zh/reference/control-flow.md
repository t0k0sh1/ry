[English](../../reference/control-flow.md) | [日本語](../../ja/reference/control-flow.md) | [简体中文](control-flow.md)

# 控制流参考

## if / else

### 语法

```python
if condition:
    # then 代码块
else:
    # else 代码块（可省略）
```

### 条件式的类型

| 类型 | 为 false 的值 | 为 true 的值 |
|---|---|---|
| `bool` | `false` | `true` |
| `int` | `0` | 非 0 |

`float` 和 `str` 无法直接用于条件式。

### 示例

```python
x = 10

if x > 5:
    print("big")
else:
    print("small or equal")
```

### 作用域规则

- `if` / `else` 的各个代码块分别拥有独立的块作用域。
- 在代码块内声明的变量无法从代码块外访问。

```python
if true:
    y = 42
# y 在此处无法访问
```

---

## while

### 语法

```python
while condition:
    # 循环体
```

当条件式为 `true` 时，重复执行循环体。

### 示例

```python
i = 0
while i < 5:
    print(i)
    i += 1
```

### 搭配 break / continue

```python
i = 0
while true:
    if i >= 3:
        break
    i += 1
```

---

## for

### 语法

```python
# 列表 / 集合遍历
for x in iterable_expr:
    # 各元素依次赋值给 x

# range（从 0 开始）
for i in range(n):
    # i = 0, 1, ..., n-1

# range（指定起始与结束）
for i in range(start, end):
    # i = start, start+1, ..., end-1

# range（指定步长）
for i in range(start, end, step):
    # i = start, start+step, start+2*step, ...
```

### 映射键值遍历

```python
for k, v in map_expr:
    # k 为键，v 为各条目的值
```

### 元组解构

遍历元组列表时，可以将元组解构为 N 个变量（需与元组元素数量匹配）。使用 `_` 丢弃值。

```python
xs = [10, 20, 30]

for i, x in enumerate(xs):
    print(f"{i}: {x}")    # 0: 10, 1: 20, 2: 30

for a, b in zip([1, 2], [10, 20]):
    print(a + b)          # 11, 22

for _, x in enumerate(xs):
    print(x)              # 丢弃索引

# N 元素解构（3 个以上的变量）
triples = [(1, 2, 3), (4, 5, 6)]
for a, b, c in triples:
    print(a + b + c)      # 6, 15

for a, _, c in triples:
    print(a + c)          # 4, 10（丢弃中间元素）
```

### 范围运算符（`..`）

`..` 运算符创建包含两端的整数范围。`1 .. 5` 产生 `[1, 2, 3, 4, 5]`。

```python
for i in 1 .. 5:
    print(i)     # 1 2 3 4 5
```

### 示例

```python
xs = [10, 20, 30]
for x in xs:
    print(x)

s = {1, 2, 3}
for x in s:
    print(x)

for i in range(5):
    print(i)     # 0 1 2 3 4

for i in range(2, 6):
    print(i)     # 2 3 4 5

for i in range(0, 10, 2):
    print(i)     # 0 2 4 6 8

for i in range(10, 0, -3):
    print(i)     # 10 7 4 1

# 映射遍历
m = {"a": 1, "b": 2}
for k, v in m:
    print(k)
    print(v)

# 范围运算符
for i in 1 .. 3:
    print(i)     # 1 2 3
```

---

## async / await

`async fn` 声明一个并发运行的函数。调用 `async fn` 返回 `Task<T>`。在另一个 `async fn` 内部使用 `await`，或从同步上下文中使用 `block_on()` 等待结果。

```python
async fn add(a: int, b: int) -> int:
    return a + b

# 从同步上下文中，使用 block_on()
t: Task<int> = add(20, 22)
print(block_on(t))                  # 42
print(block_on(add(1, 2)))          # 3

# 在 async fn 内部，使用 await
async fn double_add(a: int, b: int) -> int:
    result = await add(a, b)
    return result * 2
```

### 规则

- `async fn name(...) -> T:` 使用等待结果类型 `T` 声明。
- 调用 `async fn` 会立即返回 `Task<T>`。
- `await expr` 要求 `expr` 为 `Task<T>` 并产生 `T`。
- `await` 只能在 `async fn` 内部使用。从同步上下文中使用 `block_on(task)`。
- `block_on(task)` 阻塞当前线程直到任务完成并返回结果。
- 支持 `async fn ... -> Unit`；当不产生值时，`block_on(task)` 是等待的主要方式。
- 任务在运行时工作线程池上运行；不是每个任务一个操作系统线程。
- v1 不支持 `async` lambda 和 `async @native fn`。

---

## `@parallel for`

`@parallel` 只能附加到使用 `range(...)` 或整数 `..` 范围的计数 `for` 循环上。循环体在运行时工作线程池上以并行块运行。

```python
@parallel
for i in range(8):
    print(i)
```

### 约束

- 仅支持 `range(...)` 和整数 `..` 循环。
- 不支持解构迭代。
- 拒绝对外层可变绑定的赋值。
- 拒绝 `break` 和 `continue`。
- v1 中拒绝循环体内的索引赋值和字段赋值。

使用 `available_parallelism()` 查看运行时工作线程数。

---

## break

- 立即跳出最内层的循环（`while` 或 `for`）。
- 在循环外使用会产生编译错误。

```python
for i in range(10):
    if i == 5:
        break    # 在 i == 5 时跳出
    print(i)     # 0 1 2 3 4
```

### 错误示例

```python
# 在循环外使用 break 会产生编译错误
break   # Error: break outside loop
```

---

## continue

- 结束最内层循环的当前迭代，跳至下一次迭代。
- 在循环外使用会产生编译错误。

```python
for i in range(5):
    if i == 2:
        continue   # 跳过 i == 2
    print(i)       # 0 1 3 4
```

---

## `...`（Ellipsis）

- 不执行任何操作的语句（no-op）。用作空代码块的占位符。
- 可在任何代码块中使用：函数体、`if`/`else`、`while`、`for`、`when` 分支等。

```python
fn not_yet():
    ...

if true:
    ...
else:
    ...
```

---

## when

`when` 有两种语句形式：

- `when:` 用于多分支条件判断
- `when value:` 用于对某个值做模式匹配

### 条件分支 `when:`

```python
when:
    condition:
        # 主体
    condition:
        # 主体
    else:
        # 兜底分支
```

```python
x = 0

when:
    x > 0:
        print("positive")
    x < 0:
        print("negative")
    else:
        print("zero")
```

`when:` 会自上而下求值，只执行第一个条件为真的分支。表达式形式请参见[运算符参考](operators.md#when-条件表达式)。

### 模式匹配 `when value:`

```python
when expression:
    case pattern:
        # 主体
    case pattern if guard_condition:
        # 带守卫的主体
    case _:
        # 通配符（匹配任何值）
```

### 模式的种类

| 模式 | 示例 | 说明 |
|----------|-----|------|
| 通配符 | `_` | 匹配任何值 |
| 字面值 | `0`, `"hello"`, `true` | 值的相等比较 |
| 变量绑定 | `n` | 匹配任何值并绑定到变量 |
| enum 变体 | `Color::Red` | enum 标签的比较（简单 enum） |
| ADT enum 变体 | `Shape::Circle(r)` | 匹配带有关联数据的 enum 变体并绑定 |
| `Some(x)` | `Some(v)` | 当 Option 有值时，绑定其内容 |
| `None` | `None` | 当 Option 无值时 |
| `Ok(x)` | `Ok(v)` | 当 Result 为 Ok 时，绑定其内容 |
| `Err(x)` | `Err(e)` | 当 Result 为 Err 时，绑定错误值 |
| OR 模式 | `1 \| 2 \| 3` | 任一替代方案匹配时即匹配 |

### guard 子句

可以使用 `case pattern if condition:` 的形式指定守卫条件。只有当模式匹配且守卫条件为真时，该分支才会被执行。

### OR 模式

可以使用 `|` 组合多个模式，任一模式匹配时即匹配。OR 模式中不允许使用变量绑定（`n`、`Some(x)`、`Ok(v)`、`Err(e)`）。

```python
when x:
    case 1 | 2 | 3:
        print("small")
    case _:
        print("other")

# enum OR 模式
when color:
    case Color::Red | Color::Blue:
        print("warm or cool")
    case Color::Green:
        print("green")
```

### 穷举性检查

- enum 类型：必须覆盖所有变体或包含 `_`。OR 模式中的各替代方案会分别计算。
- Option 类型：必须覆盖 `Some` 和 `None` 或包含 `_`。
- bool 类型：必须覆盖 `true` 和 `false` 或包含 `_`。
- int / float / str 字面值：`_` 为必需。
- 带守卫的分支不计入穷举性。

### 示例

```python
# enum 模式匹配
enum Color:
    Red
    Green
    Blue

when color:
    case Color::Red:
        print("red")
    case Color::Green:
        print("green")
    case Color::Blue:
        print("blue")

# Option 模式匹配
x: Option<int> = Some(42)
when x:
    case Some(v):
        print(v)
    case None:
        print("nothing")

# Result 模式匹配
fn divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)

when divide(10, 2):
    case Ok(v):
        print(v)         # 5
    case Err(e):
        print(e.message)

# 字面值模式匹配
when x:
    case 0:
        print("zero")
    case 1:
        print("one")
    case _:
        print("other")

# guard 子句
when x:
    case n if n > 0:
        print("positive")
    case n if n < 0:
        print("negative")
    case _:
        print("zero")
```

### ADT enum 模式匹配

当 enum 变体携带关联数据时，使用绑定模式来提取值。

```python
enum Shape:
    Circle(float)
    Rectangle(float, float)
    Point

s = Shape::Circle(3.14)
when s:
    case Shape::Circle(r):
        print(r)        # 3.14
    case Shape::Rectangle(w, h):
        print(w)
        print(h)
    case Shape::Point:
        print("point")
```

多字段变体会按声明顺序将各字段绑定到不同的名称。

### 作用域规则

- 各 `case` 分支拥有块作用域。
- 通过变量绑定模式（`n`）、`Some(x)`、`Ok(v)` 或 `Err(e)` 绑定的变量仅在该分支内有效。

---

## 作用域规则

### 块作用域

- `if` / `else` / `while` / `for` / `when` 的各代码块拥有块作用域。
- 在代码块内声明的变量会在代码块结束时离开作用域。

```python
for i in range(3):
    tmp = i * 2
# tmp 在此处无法访问

if true:
    a = 1
# a 在此处无法访问
```

### 内层作用域的重新赋值

- 在内层作用域中对变量赋值会修改外层的变量（Python 风格的作用域）。
- 不会产生遮蔽——内层的赋值会修改同一个变量。

```python
x = 10
if true:
    x = 99   # 修改外层的 x
    print(x)     # 99
print(x)         # 99
```
