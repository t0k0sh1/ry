[English](../../tutorial/06-structs.md) | [日本語](../../ja/tutorial/06-structs.md) | [繁體中文](06-structs.md)

# 結構體與列舉型別

[← 前一篇：函式](05-functions.md) | [下一篇：集合 →](07-collections.md)

---

## 使用 record 定義結構體

使用 `record` 關鍵字定義結構體。各欄位以 `name: type` 的格式描述。

```python
record Point:
    x: int
    y: int
```

結構體是堆疊上的值型別。

---

## 建構式的使用方式

像呼叫函式一樣使用結構體名稱來產生實例。參數按照欄位的定義順序指定。

```python
p = Point(10, 20)
```

---

## 欄位存取（點記法）

使用點記法存取欄位。

```python
p = Point(10, 20)
print(p.x)   # 10
print(p.y)   # 20
```

> **注意**：將結構體直接傳給 `print` 會產生錯誤。請個別傳遞欄位。

---

## 欄位賦值

未使用 `@const` 宣告的可變變數的欄位可以重新賦值。

```python
p = Point(10, 20)
p.x = 100
print(p.x)   # 100
```

> **注意**：對 `@const` 宣告的變數的欄位賦值會產生編譯錯誤。

---

## 結構體作為函式參數

可以將結構體作為函式的參數傳遞。

```python
record Point:
    x: int
    y: int

fn distance_x(a: Point, b: Point) -> int:
    return a.x - b.x

p1 = Point(10, 3)
p2 = Point(4, 7)
print(distance_x(p1, p2))   # 6
```

---

## 巢狀結構體

結構體的欄位可以使用其他結構體。

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

透過鏈結點記法可存取巢狀欄位。

---

## 列舉型別（enum）

使用 `enum` 關鍵字定義列舉型別。每個變體作為具名常數處理。

### 定義

```python
enum Color:
    Red
    Green
    Blue
```

### 使用方式

使用 `::` 存取變體。

```python
c = Color::Red
print(c)   # Red
```

### 比較

可以使用 `==` 和 `!=` 比較變體。

```python
if c == Color::Red:
    print("red!")
elif c == Color::Green:
    print("green!")
else:
    print("blue!")
```

### 函式參數

可以使用 enum 名稱作為函式的參數型別。

```python
fn describe(c: Color) -> str:
    if c == Color::Red:
        return "warm"
    return "cool"
```

---

[← 前一篇：函式](05-functions.md) | [下一篇：集合 →](07-collections.md)
