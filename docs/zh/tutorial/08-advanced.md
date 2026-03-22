[English](../../tutorial/08-advanced.md) | [日本語](../../ja/tutorial/08-advanced.md) | [繁體中文](08-advanced.md)

# 進階功能

[← 前一篇：集合](07-collections.md) | [下一篇：套件 →](09-modules.md)

---

## Lambda 函式

Lambda 函式是將函式以表達式形式撰寫的語法，以 `fn(參數): 表達式` 的形式書寫。回傳值型別會自動推論。

### 單一表達式 Lambda

```python
@const
double = fn(x: int): x * 2
print(double(5))  # 10

@const
add = fn(a: int, b: int): a + b
print(add(3, 4))  # 7
```

### 無參數 Lambda

```python
@const
answer = fn(): 42
print(answer())  # 42
```

### 多行 Lambda

在 `:` 後換行並縮排，即可撰寫多個陳述式。

```python
@const
abs = fn(x: int):
    if x < 0:
        return -x
    return x

print(abs(-5))  # 5
print(abs(3))   # 3
```

---

## 閉包

Lambda 函式可以捕獲定義時作用域中的變數。

```python
@const
offset = 10
@const
add_offset = fn(x: int): x + offset
print(add_offset(5))  # 15
```

---

## 高階函式

可以定義接受函式作為參數的函式。函式型別以 `fn(參數型別) -> 回傳值型別` 的形式書寫。

```python
fn apply(f: fn(int) -> int, x: int) -> int:
    return f(x)

@const
double = fn(x: int): x * 2
print(apply(double, 3))                # 6
print(apply(fn(n: int): n + 1, 10))    # 11
```

---

## 將函式作為值使用

具名函式也可以繫結到變數或作為參數傳遞。

```python
fn square(x: int) -> int:
    return x * x

fn apply(f: fn(int) -> int, x: int) -> int:
    return f(x)

# 將具名函式作為參數傳遞
print(apply(square, 4))  # 16

# 繫結到變數
@const
sq = square
print(sq(5))  # 25
```

---

## UFCS（Uniform Function Call Syntax）

使用 UFCS 可以將 `f(a, b)` 的呼叫寫成 `a.f(b)`，實現類似方法鏈的寫法。

```python
fn add(a: int, b: int) -> int:
    return a + b

@const
x = 1
print(x.add(2))   # add(x, 2) → 3
```

### 鏈式呼叫

```python
fn double(n: int) -> int:
    return n * 2

print(x.add(2).double())   # double(add(x, 2)) → 6
```

---

## 運算子多載

使用 `fn operator運算子` 語法可為自訂型別定義運算子。

### 二元運算子

接受 2 個參數。

```python
record Vec2:
    x: int
    y: int

fn operator+(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x + b.x, a.y + b.y)

fn operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

@const
v1 = Vec2(1, 2)
@const
v2 = Vec2(3, 4)
@const
v3 = v1 + v2
print(v3.x)       # 4
print(v1 == v2)   # false
```

### 一元運算子

接受 1 個參數。

```python
fn operator-(v: Vec2) -> Vec2:
    return Vec2(-v.x, -v.y)
```

### 支援的運算子一覽

| 類別 | 運算子 |
|------|--------|
| 算術 | `+`, `-`, `*`, `/`, `%`, `**`, `//` |
| 比較 | `==`, `!=`, `<`, `<=`, `>`, `>=` |
| 位元 | `&`, `\|`, `^`, `~`, `<<`, `>>` |
| 邏輯 | `and`, `or`, `not` |

---

## Option 型別

表示值是否存在的型別，可以是 `Some(值)` 或 `None`。

```python
@const
x: Option<int> = Some(42)
print(x)   # Some(42)

@const
y: Option<int> = None
print(y)   # None
```

### 取出值

使用 `match` 安全地取出內部的值，並處理 `None` 的情況。

```python
match x:
    case Some(v):
        print(v)    # 42
    case None:
        print("nothing")
```

---

## F-String（字串插值）

使用 `f"..."` 可以在字串中直接嵌入表達式。表達式放在 `{}` 內。

```python
@const
name = "Alice"
print(f"Hello {name}")   # Hello Alice

@const
x = 3
@const
y = 4
print(f"{x} + {y} = {x + y}")   # 3 + 4 = 7
```

使用 `{{` 和 `}}` 來包含字面大括號。

```python
print(f"{{escaped}}")   # {escaped}
```

---

## 型別轉換（`as`）

使用 `as` 在型別之間進行明確轉換。

```python
@const
x = 42 as float     # 42.0
@const
y = 3.14 as int      # 3（截斷）
@const
s = 42 as str         # "42"
@const
b = true as int       # 1
```

---

## 帶關聯資料的 enum（ADT）

enum 變體可以攜帶關聯值，讓單一 enum 代表一系列不同形狀的資料。

```python
enum Shape:
    Circle(float)
    Rectangle(float, float)
    Point
```

### 建構 ADT 變體

```python
@const
c = Shape::Circle(3.14)
@const
r = Shape::Rectangle(4.0, 5.0)
@const
p = Shape::Point
```

### 匹配 ADT 變體

在 `case` 中使用綁定模式來取出關聯資料。

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

enum 可以帶有型別參數，使其可在不同酬載型別間重複使用。

```python
enum MyOption<T>:
    MySome(T)
    MyNone
```

### 使用方式

```python
@const
a = MyOption<int>::MySome(42)
@const
b: MyOption<int> = MyOption<int>::MyNone

match a:
    case MyOption::MySome(v):
        print(v)      # 42
    case MyOption::MyNone:
        print("none")
```

---

## Result 型別

`Result<T, E>` 用於可能失敗的函式。成功時回傳 `Ok(value)`，失敗時回傳 `Err(error)`。

```python
fn divide(a: int, b: int) -> Result<int, str>:
    if b == 0:
        return Err("division by zero")
    return Ok(a // b)
```

使用 `match` 來處理結果。

```python
@const
r = divide(10, 0)
match r:
    case Ok(v):
        print(v)
    case Err(e):
        print(e)   # division by zero
```

---

[← 前一篇：集合](07-collections.md) | [下一篇：套件 →](09-modules.md)
