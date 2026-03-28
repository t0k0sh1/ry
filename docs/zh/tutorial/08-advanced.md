[English](../../tutorial/08-advanced.md) | [日本語](../../ja/tutorial/08-advanced.md) | [简体中文](08-advanced.md)

# 高级特性

[<- 上一篇：集合](07-collections.md) | [下一篇：包 ->](09-modules.md)

---

## Lambda 函数

Lambda 函数是将函数以表达式形式编写的语法，以 `fn(参数) => 表达式` 的形式书写。返回值类型会自动推断。

### 单一表达式 Lambda

```python
double = fn(x: int) => x * 2
print(double(5))  # 10

add = fn(a: int, b: int) => a + b
print(add(3, 4))  # 7
```

### 无参数 Lambda

```python
answer = fn() => 42
print(answer())  # 42
```

### 多行 Lambda

在 `:` 后换行并缩进，即可编写多条语句。

```python
abs = fn(x: int):
    if x < 0:
        return -x
    return x

print(abs(-5))  # 5
print(abs(3))   # 3
```

---

## 闭包

Lambda 函数可以捕获定义时作用域中的变量。

```python
offset = 10
add_offset = fn(x: int) => x + offset
print(add_offset(5))  # 15
```

---

## 高阶函数

可以定义接受函数作为参数的函数。函数类型以 `fn(参数类型) -> 返回值类型` 的形式书写。

```python
fn apply(f: fn(int) -> int, x: int) -> int:
    return f(x)

double = fn(x: int) => x * 2
print(apply(double, 3))                # 6
print(apply(fn(n: int) => n + 1, 10))    # 11
```

---

## 将函数作为值使用

具名函数也可以绑定到变量或作为参数传递。

```python
fn square(x: int) -> int:
    return x * x

fn apply(f: fn(int) -> int, x: int) -> int:
    return f(x)

# 将具名函数作为参数传递
print(apply(square, 4))  # 16

# 绑定到变量
sq = square
print(sq(5))  # 25
```

---

## UFCS（Uniform Function Call Syntax）

使用 UFCS 可以将 `f(a, b)` 的调用写成 `a.f(b)`，实现类似方法链的写法。

```python
fn add(a: int, b: int) -> int:
    return a + b

x = 1
print(x.add(2))   # add(x, 2) -> 3
```

### 链式调用

```python
fn double(n: int) -> int:
    return n * 2

print(x.add(2).double())   # double(add(x, 2)) -> 6
```

---

## 运算符重载

使用 `fn operator` 语法可为自定义类型定义运算符。

### 二元运算符

接受 2 个参数。

```python
record Vec2:
    x: int
    y: int

fn operator+(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x + b.x, a.y + b.y)

fn operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

v1 = Vec2(1, 2)
v2 = Vec2(3, 4)
v3 = v1 + v2
print(v3.x)       # 4
print(v1 == v2)   # false
```

### 一元运算符

接受 1 个参数。

```python
fn operator-(v: Vec2) -> Vec2:
    return Vec2(-v.x, -v.y)
```

### 支持的运算符

| 类别 | 运算符 |
|------|--------|
| 算术 | `+`, `-`, `*`, `/`, `%`, `**`, `//` |
| 比较 | `==`, `!=`, `<`, `<=`, `>`, `>=` |
| 位运算 | `&`, `\|`, `^`, `~`, `<<`, `>>` |
| 逻辑 | `and`, `or`, `not` |

---

## Option 类型

表示值是否存在的类型，可以是 `Some(值)` 或 `None`。

```python
x: Option<int> = Some(42)
print(x)   # Some(42)

y: Option<int> = None
print(y)   # None
```

### 取出值

使用 `match` 安全地取出内部的值，并处理 `None` 的情况。

```python
match x:
    case Some(v):
        print(v)    # 42
    case None:
        print("nothing")
```

---

## 并发基础

`Task<T>` 是并发工作的运行时句柄。使用 `async fn` 定义返回任务的函数，在另一个 `async fn` 内部使用 `await`，从同步上下文使用 `block_on(task)` 等待完成。

```python
async fn add(a: int, b: int) -> int:
    return a + b

# 从同步上下文使用 block_on()
t: Task<int> = add(20, 22)
print(block_on(t))                  # 42
print(block_on(add(1, 2)))          # 3

# 在 async fn 内部使用 await
async fn double_add(a: int, b: int) -> int:
    return (await add(a, b)) * 2
```

`@parallel` 可以应用于使用 `range(...)` 或整数 `..` 范围的计数 `for` 循环：

```python
@parallel
for i in range(8):
    print(i)
```

`@parallel for` 禁止使用 `break`、`continue` 以及对外部可变变量的写入。

---

## 网络（TCP 套接字）

Ry 通过 `net` 模块提供 TCP 套接字支持。`send`、`recv` 和 `close` 函数用于 TCP 套接字操作。所有网络操作返回 `Result` 类型。

```python
from net import bind, listen, accept, connect, listener_port
from io import str_to_bytes, bytes_to_str

async fn echo_server(server: TcpListener) -> str:
    match accept(server):
        case Ok(conn):
            match recv(conn, 4096):
                case Ok(data):
                    match send(conn, data):
                        case Ok(_):
                            ...
                        case Err(e):
                            ...
                case Err(e):
                    ...
            close(conn)
        case Err(e):
            ...
    close(server)
    return "done"

match bind("127.0.0.1", 0):
    case Ok(server):
        match listen(server, 1):
            case Ok(_):
                port = listener_port(server)
                t = echo_server(server)
                match connect("127.0.0.1", port):
                    case Ok(conn):
                        match send(conn, str_to_bytes("hello")):
                            case Ok(_):
                                ...
                            case Err(e):
                                ...
                        match recv(conn, 4096):
                            case Ok(resp):
                                match bytes_to_str(resp):
                                    case Ok(s):
                                        print(s)   # hello
                                    case Err(e):
                                        ...
                            case Err(e):
                                ...
                        close(conn)
                    case Err(e):
                        print("connect failed")
                block_on(t)
            case Err(e):
                ...
    case Err(e):
        print("bind failed")
```

完整 API 请参阅[网络参考手册](../reference/net.md)。

---

## F-String（字符串插值）

使用 `f"..."` 可以在字符串中直接嵌入表达式。表达式放在 `{}` 内。

```python
name = "Alice"
print(f"Hello {name}")   # Hello Alice

x = 3
y = 4
print(f"{x} + {y} = {x + y}")   # 3 + 4 = 7
```

使用 `{{` 和 `}}` 来包含字面大括号。

```python
print(f"{{escaped}}")   # {escaped}
```

---

## 类型转换（`as`）

使用 `as` 在类型之间进行显式转换。

```python
x = 42 as float     # 42.0
y = 3.14 as int      # 3（截断）
s = 42 as str         # "42"
b = true as int       # 1
```

---

## 带关联数据的 enum（ADT）

enum 变体可以携带关联值。这使得单一 enum 可以表示一系列不同形状的数据。可以选择性地为字段命名以提高文档清晰度。

```python
enum Shape:
    Circle(radius: float)
    Rectangle(width: float, height: float)
    Point
```

命名字段仅用于文档说明 —— 使定义具有自描述性。无名语法（`Circle(float)`）同样有效。

### 构造 ADT 变体

构造始终按位置进行，无论字段是否命名。

```python
c = Shape::Circle(3.14)
r = Shape::Rectangle(4.0, 5.0)
p = Shape::Point
```

### 匹配 ADT 变体

在 `case` 中使用绑定模式来提取关联数据。绑定使用你选择的变量名，而非字段名。

```python
fn describe(s: Shape) -> str:
    match s:
        case Shape::Circle(r):
            return f"circle with radius {r}"
        case Shape::Rectangle(w, h):
            return f"rectangle {w}x{h}"
        case Shape::Point:
            return "point"

print(describe(Shape::Circle(3.14)))         # circle with radius 3.14
print(describe(Shape::Rectangle(4.0, 5.0)))  # rectangle 4.0x5.0
```

---

## 泛型 enum

enum 可以带有类型参数，使其可在不同载荷类型间重复使用。

```python
enum MyOption<T>:
    MySome(T)
    MyNone
```

### 用法

```python
a = MyOption<int>::MySome(42)
b: MyOption<int> = MyOption<int>::MyNone

match a:
    case MyOption::MySome(v):
        print(v)      # 42
    case MyOption::MyNone:
        print("none")
```

---

## Result 类型

`Result<T, E>` 用于可能失败的函数。成功时返回 `Ok(value)`，失败时返回 `Err(error)`。

```python
fn divide(a: int, b: int) -> Result<int, str>:
    if b == 0:
        return Err("division by zero")
    return Ok(a // b)
```

使用 `match` 来处理结果。

```python
r = divide(10, 0)
match r:
    case Ok(v):
        print(v)
    case Err(e):
        print(e)   # division by zero
```

---

[<- 上一篇：集合](07-collections.md) | [下一篇：包 ->](09-modules.md)
