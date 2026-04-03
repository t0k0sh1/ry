[English](../../tutorial/06-records.md) | [日本語](../../ja/tutorial/06-records.md) | [简体中文](06-records.md)

# Record 与枚举

[<- 上一篇：函数](05-functions.md) | [下一篇：集合与迭代器 ->](07-collections.md)

---

## 定义 Record

使用 `record` 关键字定义 record。各字段以 `name: type` 格式描述。

```python
record Point:
    x: int
    y: int
```

Record 是在栈上分配的值类型。

---

## 创建实例

像调用函数一样使用 record 名称来创建实例。参数按照字段的定义顺序指定。

```python
p = Point(10, 20)
```

---

## 字段访问（点记法）

使用点记法访问字段。

```python
p = Point(10, 20)
print(p.x)   # 10
print(p.y)   # 20
```

Record 可以直接打印：

```python
p = Point(10, 20)
print(p)   # Point(x: 10, y: 20)
```

---

## 字段赋值

未使用 `@const` 声明的可变变量的字段可以重新赋值。

```python
p = Point(10, 20)
p.x = 100
print(p.x)   # 100
```

> **注意**：对 `@const` 声明的变量的字段赋值会产生编译错误。

---

## Record 作为函数参数

可以将 record 作为函数的参数传递。

```python
record Point:
    x: int
    y: int

function distance_x(a: Point, b: Point) -> int:
    return a.x - b.x

p1 = Point(10, 3)
p2 = Point(4, 7)
print(distance_x(p1, p2))   # 6
```

---

## 嵌套 Record

Record 的字段可以使用其他 record。

```python
record Point:
    x: int
    y: int

record Line:
    start: Point
    end: Point

line = Line(Point(0, 0), Point(10, 5))
print(line.start.x)   # 0
print(line.end.x)     # 10
```

通过链接点记法可访问嵌套字段。

---

## 枚举（enum）

使用 `enum` 关键字定义枚举。每个变体作为具名常量处理。

### 定义

```python
enum Color:
    Red
    Green
    Blue
```

### 使用方式

使用 `::` 访问变体。

```python
c = Color::Red
print(c)   # Red
```

### 比较

可以使用 `==` 和 `!=` 比较变体。

```python
when:
    c == Color::Red:
        print("red!")
    c == Color::Green:
        print("green!")
    else:
        print("blue!")
```

### 函数参数

可以使用 enum 名称作为函数的参数类型。

```python
function describe(c: Color) -> str:
    if c == Color::Red:
        return "warm"
    return "cool"
```

---

## 带关联数据的枚举（ADT）

枚举变体可以携带关联值。这让单个枚举可以表示一系列不同形状的数据 —— 这种模式称为**代数数据类型（ADT）**。

```python
enum Shape:
    Circle(radius: float)
    Rectangle(width: float, height: float)
    Point
```

命名字段仅用于文档目的 —— 使定义具有自描述性。无名语法（`Circle(float)`）同样有效。

### 构造 ADT 变体

构造始终是位置性的，无论字段是否命名。

```python
c = Shape::Circle(3.14)
r = Shape::Rectangle(4.0, 5.0)
p = Shape::Point
```

### 匹配 ADT 变体

使用 `match` 和 `case` 提取关联数据。绑定使用你选择的变量名，而非字段名。这直接与你在[控制流](04-control-flow.md)中学到的模式匹配相连接。

```python
function describe(s: Shape) -> str:
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

> **为什么使用 ADT？** 它们让你以类型安全的方式建模"多种形状之一"的数据。编译器在模式匹配时确保你处理了每个变体，在编译时捕获遗漏的情况。

---

## 泛型枚举

枚举可以接受类型参数，使其可在不同的载荷类型间复用。

```python
enum MyOption<T>:
    MySome(T)
    MyNone
```

### 使用方式

```python
a = MyOption<int>::MySome(42)
b: MyOption<int> = MyOption<int>::MyNone

match a:
    case MyOption::MySome(v):
        print(v)      # 42
    case MyOption::MyNone:
        print("none")
```

> **注意**：Ry 的内置 `Option<T>` 和 `Result<T, E>` 类型的工作方式与此完全相同。你将在[错误处理](08-error-handling.md)中学习它们。

---

## 运算符重载

可以使用 `function operator` 语法为自定义类型定义运算符。这让你的 record 可以自然地与 `+`、`==` 等运算符一起使用。

### 二元运算符

接受两个参数。

```python
record Vec2:
    x: int
    y: int

function operator+(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x + b.x, a.y + b.y)

function operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

v1 = Vec2(1, 2)
v2 = Vec2(3, 4)
v3 = v1 + v2
print(v3.x)       # 4
print(v1 == v2)   # false
```

### 一元运算符

接受一个参数。

```python
function operator-(v: Vec2) -> Vec2:
    return Vec2(-v.x, -v.y)
```

### 支持的运算符

| 类别 | 运算符 |
|------|--------|
| 算术 | `+`, `-`, `*`, `/`, `%`, `**`, `//` |
| 比较 | `==`, `!=`, `<`, `<=`, `>`, `>=` |
| 位运算 | `&`, `\|`, `^`, `~`, `<<`, `>>`, `>>>` |
| 逻辑 | `and`, `or`, `not` |
| 成员 | `in` |
| 索引访问 | `[]` |
| 索引赋值 | `[]=` |
| 函数调用 | `()` |
| 类型转换 | `as` |
| 复合赋值 | `+=`, `-=`, `*=`, `/=`, `%=`, `//=`, `**=`, `&=`, `\|=`, `^=`, `<<=`, `>>=` |

> **为什么使用运算符重载？** 它给予领域类型自然的语法。`Vec2 + Vec2` 比 `vec2_add(a, b)` 更易读，`==` 让你的类型与 `when` 和比较无缝配合。

---

## 练习

1. **ADT**：定义一个 `Animal` 枚举，包含变体 `Dog(name: str)`、`Cat(name: str, indoor: bool)` 和 `Fish`。编写一个 `describe(a: Animal) -> str` 函数，使用 `match` 为每个变体返回描述。

2. **运算符重载**：定义一个 `Money` record，包含 `amount: int` 和 `currency: str`。重载 `+`，使得相同货币的两个 `Money` 值相加返回金额之和的新 `Money`。

---

[<- 上一篇：函数](05-functions.md) | [下一篇：集合与迭代器 ->](07-collections.md)
