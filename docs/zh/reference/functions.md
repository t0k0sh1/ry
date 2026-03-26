[English](../../reference/functions.md) | [日本語](../../ja/reference/functions.md) | [繁體中文](functions.md)

# 函式參考

## 函式定義語法

```python
fn 函式名(引數名: 型別, ...) -> 回傳型別:
    # 主體
    return 值
```

- 引數型別可省略。省略時視為 `any` 型別。
- 回傳型別可省略（省略時為 `Unit`）。
- 主體為縮排的區塊。
- 具有明確回傳型別（`Unit` 和 `any` 除外）的函式，必須在所有控制流路徑中包含 `return` 語句。若缺少則會產生編譯錯誤。
- 函式可以定義 `require`（前置條件）和 `ensure`（後置條件）。參閱 [契約式設計](contracts.md)。

> **命名慣例**：函式名稱和引數名稱必須使用 snake_case（如 `add`、`get_value`、`map_list`）。編譯器會強制執行此慣例。

```python
fn add(a: int, b: int) -> int:
    return a + b

fn greet(name: str):
    print("Hello, " + name)   # 回傳型別為 Unit
```

---

## 引數與回傳值的型別

| 項目 | 說明 |
|---|---|
| 引數型別 | 可省略。省略 `: 型別` 時預設為 `any` |
| 回傳型別 | 可省略。省略時為 `Unit`（相當於 void） |
| `Unit` | 不回傳值的函式的回傳型別 |

```python
fn no_return(x: int):      # 回傳型別 Unit（省略）
    print(x)

fn get_value() -> int:     # 回傳型別 int
    return 42

fn identity(x) -> any:    # 引數型別 any（省略）
    return x
```

---

## 遞迴

函式可以呼叫自身。

```python
fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)
```

---

## 多載

可以定義引數數量或型別不同的同名函式。

### 規則

- 引數的數量或型別不同即可定義同名函式。
- 呼叫時會根據引數的型別和數量選擇適當的函式。
- 僅回傳型別不同的多載是不允許的。

```python
fn area(side: int) -> int:
    return side * side

fn area(w: int, h: int) -> int:
    return w * h

a = area(5)       # 25
b = area(3, 4)    # 12
```

---

## Unit 型別函式

不回傳值的函式會回傳 `Unit`。回傳型別可以省略。

```python
fn log(msg: str):
    print(msg)

fn log_typed(msg: str) -> Unit:
    print(msg)
```

---

## Lambda 函式

可以就地定義匿名函式。

### 語法

```python
# 單一運算式（運算式的值作為回傳值。回傳型別自動推論）
fn(引數名: 型別, ...): 運算式

# 引數型別可省略（預設為 any）
fn(引數名, ...): 運算式

# 多行區塊
fn(引數名: 型別, ...):
    # 多個陳述式
    return 值

# 明確指定回傳型別（可省略）
fn(引數名: 型別, ...) -> 回傳型別: 運算式
```

### 範例

```python
double = fn(x: int): x * 2
result = double(5)   # 10

add = fn(a: int, b: int): a + b
sum = add(3, 4)      # 7

# 多行 lambda
abs = fn(x: int):
    if x < 0:
        return -x
    return x
```

---

## 閉包

Lambda 函式會以**值捕獲**定義時外層作用域的變數。

```python
base = 10
add_base = fn(x: int): x + base   # 以值捕獲 base

base = 99          # 不影響已捕獲的值
r = add_base(5)   # 15（使用捕獲時的 base = 10）
```

### 捕獲規則

| 項目 | 內容 |
|---|---|
| 捕獲方式 | 值捕獲（複製） |
| 捕獲時機 | Lambda 定義時 |
| 外層變數修改的影響 | 無（因為是複製） |

---

## 函式型別

用於將函式作為值處理的型別。

### 語法

```python
fn(引數型別1, 引數型別2, ...) -> 回傳型別
```

### 範例

```python
f: fn(int) -> int = fn(x: int): x * 2
g: fn(int, int) -> int = fn(a: int, b: int): a + b

fn apply(func: fn(int) -> int, x: int) -> int:
    return func(x)

result = apply(f, 5)   # 10
```

---

## 高階函式

可以接收函式作為引數，或將函式作為回傳值回傳。

```python
fn map_list(xs: List<int>, f: fn(int) -> int) -> List<int>:
    result: List<int> = []
    for x in xs:
        result += [f(x)]
    return result

doubled = map_list([1, 2, 3], fn(x: int): x * 2)
# [2, 4, 6]
```

---

## UFCS（統一函式呼叫語法）

可以使用 `a.f(b)` 的形式呼叫 `f(a, b)`。方便用於方法鏈。

### 語法

```python
# 一般呼叫
f(a, b)

# UFCS 呼叫（等價）
a.f(b)
```

### 鏈接

```python
fn double(x: int) -> int:
    return x * 2

fn add_one(x: int) -> int:
    return x + 1

result = 5.double().add_one()   # double(5) → 10, add_one(10) → 11
```

### 與欄位存取混用

欄位存取（`.field`）和 UFCS（`.method()`）使用相同的點記法，但透過是否有引數來區分。

```python
p = Point(3, 4)
length = p.x.to_float()   # 欄位存取 + UFCS
```

---

## 運算子多載

可以為使用者定義型別定義運算子的行為。

### 語法

```python
# 二元運算子（2 個引數）
fn operator<op>(a: 型別, b: 型別) -> 回傳型別:
    ...

# 一元運算子（1 個引數）
fn operator<op>(a: 型別) -> 回傳型別:
    ...
```

### 可多載的運算子

| 種類 | 運算子 |
|---|---|
| 算術（二元） | `+` `-` `*` `/` `%` `**` `//` |
| 比較（二元） | `==` `!=` `<` `<=` `>` `>=` |
| 位元（二元） | `&` `\|` `^` `<<` `>>` |
| 邏輯（二元） | `and` `or` |
| 一元 | `-` `~` `not` |

### 二元 / 一元的區別

依引數個數區分。

```python
record Vec2:
    x: float
    y: float

# 二元 +
fn operator+(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x + b.x, a.y + b.y)

# 一元 -
fn operator-(v: Vec2) -> Vec2:
    return Vec2(-v.x, -v.y)

# 比較
fn operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

v1 = Vec2(1.0, 2.0)
v2 = Vec2(3.0, 4.0)
v3 = v1 + v2    # Vec2(4.0, 6.0)
v4 = -v1        # Vec2(-1.0, -2.0)
```
