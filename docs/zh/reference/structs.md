[English](../../reference/structs.md) | [日本語](../../ja/reference/structs.md) | [简体中文](structs.md)

# 结构体参考

## 概述

结构体是栈上的值类型。使用 `record` 关键字定义。结构体可以使用 `invariant` 子句实现契约式设计。参阅 [契约式设计](contracts.md)。

> **命名约定**：结构体名称必须使用 PascalCase（如 `Point`、`Rectangle`）。字段名称必须使用 snake_case。编译器会强制执行这些约定。

---

## 定义语法

```python
record TypeName:
    field_name: type
    field_name: type
```

### 示例

```python
record Point:
    x: int
    y: int

record Rectangle:
    width: float
    height: float
```

---

## 构造函数

按照字段定义顺序传递参数。不支持命名参数。

```python
p = Point(10, 20)
r = Rectangle(3.0, 4.5)
```

---

## 字段访问

使用点号记法读取字段。

```python
p = Point(10, 20)
print(p.x)   # 10
print(p.y)   # 20
```

---

## 字段赋值

| 变量声明 | 字段赋值 |
|---------|--------------|
| 可变（无 `@const`） | 可以 |
| `@const` | 编译错误 |

```python
p = Point(10, 20)
p.x = 100    # OK：可变变量

@const
q = Point(10, 20)
q.x = 100    # 错误：@const 变量的字段不可变更
```

---

## 作为函数参数与返回值使用

```python
fn distance(p: Point) -> float:
    return (p.x * p.x + p.y * p.y) as float

fn make_point(x: int, y: int) -> Point:
    return Point(x, y)
```

---

## 嵌套结构体

可以将结构体作为另一个结构体的字段使用。

```python
record Point:
    x: int
    y: int

record Circle:
    center: Point
    radius: float

c = Circle(Point(0, 0), 1.0)
print(c.center.x)   # 0
```

---

## 比较（`==` / `!=`）

记录类型自动支持 `==` 和 `!=` 运算符。比较是逐字段进行的（结构性相等）。

```python
record Point:
    x: int
    y: int

p1 = Point(10, 20)
p2 = Point(10, 20)
p3 = Point(30, 40)

print(p1 == p2)  # true
print(p1 != p3)  # true
```

- 所有字段按顺序比较。对于 `==`，所有字段必须相等。对于 `!=`，至少一个字段必须不同。
- 嵌套记录会递归比较。
- 如果提供了用户定义的 `operator==` 或 `operator!=`，则优先于自动生成的版本。

---

## 记录子类型（继承）

记录支持使用 `<` 语法的单继承。子记录继承父记录的所有字段。

### 语法

```python
record ChildName < ParentName:
    child_field: type
```

### 示例

```python
record HttpError < Error:
    status: int
    url: str
```

### 字段继承

- 子记录在其布局的开头继承所有父字段。
- 构造函数先接受父字段，然后是子特有的字段。

```python
err = HttpError("not found", 404, 404, "/api")
print(err.message)  # "not found"（从 Error 继承）
print(err.status)   # 404（自有字段）
```

### 子类型强制转换

子值可以传递给期望父类型的地方。子值会被自动切片以提取父前缀字段（值类型切片）。

```python
fn handle(e: Error) -> str:
    return e.message

err = HttpError("fail", 500, 500, "/api")
handle(err)  # OK — HttpError 强制转换为 Error
```

### 深层继承

记录可以形成继承链。每一层继承所有祖先字段。

```python
record DetailedHttpError < HttpError:
    detail: str

# 构造函数：Error 字段 + HttpError 字段 + 自有字段
derr = DetailedHttpError("fail", 500, 500, "/x", "server crash")
handle(derr)  # OK — 强制转换为 Error（祖父类型）
```

### 规则

| 规则 | 详细 |
|------|------|
| 仅支持单继承 | `record A < B:` — 只能有一个父类 |
| 深层继承 | `record C < B:` 其中 `record B < A:` — 允许 |
| 名称冲突 | 子字段与父字段同名 → 编译错误 |
| 自动 `==` / `to_str` | 包含所有继承字段 |
| 不变量继承 | 构造或修改子记录时检查父的 `invariant:` 子句 |
| 子类型强制转换 | 适用于：函数参数、返回值、`Err()`、字段赋值、`?` 运算符 |
| 泛型边界 | `<T: RecordName>` 将类型参数约束为记录的子类型 |
| `@const` | 适用于所有字段（包括继承的字段） |

---

## 约束与错误

| 约束 | 详细 |
|------|------|
| 相同字段名重复 | 编译错误 |
| `@const` 变量的字段赋值 | 编译错误 |
| 直接将结构体传给 `print` | 编译错误（print 不支持） |

```python
# 错误示例：相同字段名重复
record Bad:
    x: int
    x: int   # 错误

# 错误示例：将结构体传给 print
p = Point(1, 2)
print(p)   # 错误
```

---

## 枚举类型（enum）

### 概述

枚举类型是具名常量的集合。默认以 i64 整数（0, 1, 2, ...）的连续编号表示。也可以指定显式的整数值。

### 定义语法

```python
enum TypeName:
    VariantName
    VariantName
    ...
```

### 示例

```python
enum Color:
    Red
    Green
    Blue
```

### 变体访问

使用 `::` 运算符访问变体。

```python
c = Color::Red
print(c)   # Red
```

### 比较

enum 值为整数，因此可以直接使用 `==` / `!=` 进行比较。

```python
print(Color::Red == Color::Red)    # true
print(Color::Red != Color::Green)  # true
```

### 在 if 语句中使用

```python
c = Color::Green
if c == Color::Red:
    print("red")
elif c == Color::Green:
    print("green")
else:
    print("blue")
```

### 函数参数

类型名称使用 enum 名称。

```python
fn is_red(c: Color) -> bool:
    return c == Color::Red

print(is_red(Color::Red))    # true
print(is_red(Color::Green))  # false
```

### print

使用 `print()` 会输出变体名称。

```python
c = Color::Blue
print(c)   # Blue
```

### 显式值指定

simple enum 的变体可以指定显式的整数值。适用于 HTTP 状态码或位掩码模式等用途。

```python
enum HttpStatus:
    Ok = 200
    NotFound = 404
    InternalError = 500

s = HttpStatus::NotFound
print(s)                              # NotFound
print(s == HttpStatus::NotFound)      # true
```

```python
enum Permission:
    Read = 1
    Write = 2
    Execute = 4
```

规则：
- 仅支持 simple enum（不含关联数据的 ADT 变体）。
- 值必须为整数字面量（允许负值）。
- 若任一变体有显式值，则所有变体都必须有（不可混用自动和手动）。
- 重复值会产生编译错误。
- `print()` 显示变体名称，而非整数值。

### ADT 变体的命名字段

ADT 变体字段可以选择性地包含名称以用于文档目的。命名字段使定义具有自描述性，而不改变构造或模式匹配的语义。

```python
enum Shape:
    Circle(radius: float)
    Rect(width: float, height: float)
    Point
```

- 构造始终是位置性的：`Shape::Circle(3.14)`，而非 `Shape::Circle(radius: 3.14)`。
- 模式匹配绑定用户选择的变量名：`case Shape::Circle(r):`。
- 字段名必须为 `snake_case`。不允许在单个变体内混用命名和未命名字段。
- 未命名语法（`Circle(float)`）仍然有效。

### 约束与错误

| 约束 | 详细 |
|------|------|
| 变体访问为 `EnumName::VariantName` | 必须使用 `::` 运算符 |
| 变体值 | 默认为自动分配（0, 1, 2, ...），可使用 `= value` 显式指定 |
| 比较为整数比较 | 可使用 `==`、`!=` |
| 命名字段名 | 必须为 `snake_case`；同一变体内不可重复；不可混用命名/未命名 |
