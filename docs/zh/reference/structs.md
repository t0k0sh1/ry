[English](../../reference/structs.md) | [日本語](../../ja/reference/structs.md) | [繁體中文](structs.md)

# 結構體參考

## 概述

結構體是堆疊上的值型別。使用 `record` 關鍵字定義。結構體可以使用 `invariant` 子句定義不變量。參閱 [契約式設計](contracts.md)。

> **命名慣例**：結構體名稱必須使用 PascalCase（如 `Point`、`Rectangle`）。欄位名稱必須使用 snake_case。編譯器會強制執行這些慣例。

---

## 定義語法

```python
record 型別名:
    欄位名: 型別
    欄位名: 型別
```

### 範例

```python
record Point:
    x: int
    y: int

record Rectangle:
    width: float
    height: float
```

---

## 建構子

按照欄位定義順序傳遞引數。不支援具名引數。

```python
p = Point(10, 20)
r = Rectangle(3.0, 4.5)
```

---

## 欄位存取

使用點記法讀取欄位。

```python
p = Point(10, 20)
print(p.x)   # 10
print(p.y)   # 20
```

---

## 欄位賦值

| 變數宣告 | 欄位賦值 |
|---------|--------------|
| 可變（無 `@const`） | 可以         |
| `@const`   | 編譯錯誤 |

```python
p = Point(10, 20)
p.x = 100    # OK: 可變變數

@const
q = Point(10, 20)
q.x = 100    # 錯誤: @const 變數的欄位不可變更
```

---

## 作為函式引數與回傳值使用

```python
fn distance(p: Point) -> float:
    return (p.x * p.x + p.y * p.y) as float

fn make_point(x: int, y: int) -> Point:
    return Point(x, y)
```

---

## 巢狀結構體

可以將結構體作為另一個結構體的欄位使用。

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

## 限制與錯誤

| 限制 | 詳細 |
|------|------|
| 相同欄位名重複 | 編譯錯誤 |
| `@const` 變數的欄位賦值 | 編譯錯誤 |
| 直接將結構體傳給 `print` | 編譯錯誤（print 不支援） |

```python
# 錯誤範例：相同欄位名重複
record Bad:
    x: int
    x: int   # 錯誤

# 錯誤範例：將結構體傳給 print
p = Point(1, 2)
print(p)   # 錯誤
```

---

## 列舉型別（enum）

### 概述

列舉型別是具名常數的集合。內部以 i64 整數（0, 1, 2, ...）表示。

### 定義語法

```python
enum 型別名:
    變體名
    變體名
    ...
```

### 範例

```python
enum Color:
    Red
    Green
    Blue
```

### 變體存取

使用 `::` 運算子存取變體。

```python
c = Color::Red
print(c)   # Red
```

### 比較

enum 值為整數，因此可以直接使用 `==` / `!=` 進行比較。

```python
print(Color::Red == Color::Red)    # true
print(Color::Red != Color::Green)  # true
```

### 在 if 陳述式中使用

```python
c = Color::Green
if c == Color::Red:
    print("red")
elif c == Color::Green:
    print("green")
else:
    print("blue")
```

### 函式引數

型別名稱使用 enum 名稱。

```python
fn is_red(c: Color) -> bool:
    return c == Color::Red

print(is_red(Color::Red))    # true
print(is_red(Color::Green))  # false
```

### print

使用 `print()` 會輸出變體名稱。

```python
c = Color::Blue
print(c)   # Blue
```

### 限制與錯誤

| 限制 | 詳細 |
|------|------|
| 變體存取為 `EnumName::VariantName` | 必須使用 `::` 運算子 |
| 變體值為自動分配 | 0, 1, 2, ... 的連續編號（無法手動指定） |
| 比較為整數比較 | 可使用 `==`、`!=` |
