[English](../../tutorial/08-advanced.md) | [日本語](../../ja/tutorial/08-advanced.md) | [繁體中文](08-advanced.md)

# 進階功能

[← 前一篇：集合](07-collections.md) | [下一篇：模組 →](09-modules.md)

---

## Lambda 函式

Lambda 函式是將函式以表達式形式撰寫的語法，以 `(參數) -> 表達式` 的形式書寫。回傳值型別會自動推論。

### 單一表達式 Lambda

```python
let double = (x: int) -> x * 2
print(double(5))  # 10

let add = (a: int, b: int) -> a + b
print(add(3, 4))  # 7
```

### 無參數 Lambda

```python
let answer = () -> 42
print(answer())  # 42
```

### 多行 Lambda

在 `->` 後換行並縮排，即可撰寫多個陳述式。

```python
let abs = (x: int) ->
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
let offset = 10
let add_offset = (x: int) -> x + offset
print(add_offset(5))  # 15
```

---

## 高階函式

可以定義接受函式作為參數的函式。函式型別以 `fn(參數型別) -> 回傳值型別` 的形式書寫。

```python
fn apply(f: fn(int) -> int, x: int) -> int:
    return f(x)

let double = (x: int) -> x * 2
print(apply(double, 3))                # 6
print(apply((n: int) -> n + 1, 10))    # 11
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
let sq = square
print(sq(5))  # 25
```

---

## UFCS（Uniform Function Call Syntax）

使用 UFCS 可以將 `f(a, b)` 的呼叫寫成 `a.f(b)`，實現類似方法鏈的寫法。

```python
fn add(a: int, b: int) -> int:
    return a + b

let x = 1
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
type Vec2:
    x: int
    y: int

fn operator+(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x + b.x, a.y + b.y)

fn operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

let v1 = Vec2(1, 2)
let v2 = Vec2(3, 4)
let v3 = v1 + v2
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
let x: Option<int> = Some(42)
print(x)   # Some(42)

let y: Option<int> = None
print(y)   # None
```

### unwrap

使用 `unwrap` 取出內部的值。對 `None` 呼叫 `unwrap` 會產生執行期錯誤。

```python
let v = unwrap(x)   # 42
# unwrap(y) → 執行期錯誤
```

---

[← 前一篇：集合](07-collections.md) | [下一篇：模組 →](09-modules.md)
