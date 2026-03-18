[English](../../reference/control-flow.md) | [日本語](../../ja/reference/control-flow.md) | [繁體中文](control-flow.md)

# 控制流程參考

## if / elif / else

### 語法

```python
if 條件式:
    # then 區塊
elif 條件式:
    # elif 區塊（可多個）
else:
    # else 區塊（可省略）
```

### 條件式的型別

| 型別 | 為 false 的值 | 為 true 的值 |
|---|---|---|
| `bool` | `false` | `true` |
| `int` | `0` | 非 0 |

`float` 和 `str` 無法直接用於條件式。

### 範例

```python
let x = 10

if x > 5:
    print("big")
elif x == 5:
    print("five")
else:
    print("small")
```

### 作用域規則

- `if` / `elif` / `else` 的各個區塊分別擁有獨立的區塊作用域。
- 在區塊內宣告的變數無法從區塊外存取。

```python
if true:
    let y = 42
# y 在此處無法存取
```

---

## while

### 語法

```python
while 條件式:
    # 迴圈主體
```

當條件式為 `true` 時，重複執行迴圈主體。

### 範例

```python
let i = 0
while i < 5:
    print(i)
    i += 1
```

### 搭配 break / continue

```python
let i = 0
while true:
    if i >= 3:
        break
    i += 1
```

---

## for

### 語法

```python
# 串列 / 集合走訪
for x in iterable_expr:
    # 各元素依序賦值給 x

# range（從 0 開始）
for i in range(n):
    # i = 0, 1, ..., n-1

# range（指定起始與結束）
for i in range(start, end):
    # i = start, start+1, ..., end-1

# range（指定步長）
for i in range(start, end, step):
    # i = start, start+step, start+2*step, ...
```

### 映射鍵值走訪

```python
for k, v in map_expr:
    # k 為鍵，v 為各條目的值
```

### 元組解構

走訪 2 元素元組的列表（例如 `enumerate()` 或 `zip()` 的回傳值）時，可以解構為兩個變數。使用 `_` 捨棄值。

```python
let xs = [10, 20, 30]

for i, x in enumerate(xs):
    print(f"{i}: {x}")    # 0: 10, 1: 20, 2: 30

for a, b in zip([1, 2], [10, 20]):
    print(a + b)          # 11, 22

for _, x in enumerate(xs):
    print(x)              # 捨棄索引
```

### 範圍運算子（`..`）

`..` 運算子建立包含兩端的整數範圍。`1 .. 5` 產生 `[1, 2, 3, 4, 5]`。

```python
for i in 1 .. 5:
    print(i)     # 1 2 3 4 5
```

### 範例

```python
let xs = [10, 20, 30]
for x in xs:
    print(x)

let s = {1, 2, 3}
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

# 映射走訪
let m = {"a": 1, "b": 2}
for k, v in m:
    print(k)
    print(v)

# 範圍運算子
for i in 1 .. 3:
    print(i)     # 1 2 3
```

---

## break

- 立即跳出最內層的迴圈（`while` 或 `for`）。
- 在迴圈外使用會產生編譯錯誤。

```python
for i in range(10):
    if i == 5:
        break    # 在 i == 5 時跳出
    print(i)     # 0 1 2 3 4
```

### 錯誤範例

```python
# 在迴圈外使用 break 會產生編譯錯誤
break   # Error: break outside loop
```

---

## continue

- 結束最內層迴圈的當前迭代，跳至下一次迭代。
- 在迴圈外使用會產生編譯錯誤。

```python
for i in range(5):
    if i == 2:
        continue   # 跳過 i == 2
    print(i)       # 0 1 3 4
```

---

## match

### 語法

```python
match 運算式:
    case 模式:
        # 主體
    case 模式 if 守衛條件:
        # 帶守衛的主體
    case _:
        # 萬用字元（匹配任何值）
```

### 模式的種類

| 模式 | 範例 | 說明 |
|----------|-----|------|
| 萬用字元 | `_` | 匹配任何值 |
| 字面值 | `0`, `"hello"`, `true` | 值的相等比較 |
| 變數綁定 | `n` | 匹配任何值並綁定到變數 |
| enum 變體 | `Color::Red` | enum 標籤的比較（簡單 enum） |
| ADT enum 變體 | `Shape::Circle(r)` | 匹配帶有關聯資料的 enum 變體並綁定 |
| `Some(x)` | `Some(v)` | 當 Option 有值時，綁定其內容 |
| `None` | `None` | 當 Option 無值時 |
| OR 模式 | `1 \| 2 \| 3` | 任一替代方案匹配時即匹配 |

### guard 子句

可以使用 `case 模式 if 條件式:` 的形式指定守衛條件。只有當模式匹配且守衛條件為真時，該分支才會被執行。

### OR 模式

可以使用 `|` 組合多個模式，任一模式匹配時即匹配。OR 模式中不允許使用變數綁定（`n`、`Some(x)`、`Ok(v)`、`Err(e)`）。

```python
match x:
    case 1 | 2 | 3:
        print("small")
    case _:
        print("other")

# enum OR 模式
match color:
    case Color::Red | Color::Blue:
        print("warm or cool")
    case Color::Green:
        print("green")
```

### 窮舉性檢查

- enum 型別：必須覆蓋所有變體或包含 `_`。OR 模式中的各替代方案會分別計算。
- Option 型別：必須覆蓋 `Some` 和 `None` 或包含 `_`。
- bool 型別：必須覆蓋 `true` 和 `false` 或包含 `_`。
- int / float / str 字面值：`_` 為必要。
- 帶守衛的分支不計入窮舉性。

### 範例

```python
# enum 匹配
enum Color:
    Red
    Green
    Blue

match color:
    case Color::Red:
        print("red")
    case Color::Green:
        print("green")
    case Color::Blue:
        print("blue")

# Option 匹配
let x: Option<int> = Some(42)
match x:
    case Some(v):
        print(v)
    case None:
        print("nothing")

# 字面值匹配
match x:
    case 0:
        print("zero")
    case 1:
        print("one")
    case _:
        print("other")

# guard 子句
match x:
    case n if n > 0:
        print("positive")
    case n if n < 0:
        print("negative")
    case _:
        print("zero")
```

### ADT enum 匹配

當 enum 變體攜帶關聯資料時，使用綁定模式來取出值。

```python
enum Shape:
    Circle(float)
    Rectangle(float, float)
    Point

let s = Shape::Circle(3.14)
match s:
    case Shape::Circle(r):
        print(r)        # 3.14
    case Shape::Rectangle(w, h):
        print(w)
        print(h)
    case Shape::Point:
        print("point")
```

多欄位變體會按宣告順序將各欄位綁定到不同的名稱。

### 作用域規則

- 各 `case` 分支擁有區塊作用域。
- 透過變數綁定模式 (`n`) 或 `Some(x)` 綁定的變數僅在該分支內有效。

---

## 作用域規則

### 區塊作用域

- `if` / `elif` / `else` / `while` / `for` / `match` 的各區塊擁有區塊作用域。
- 在區塊內宣告的變數會在區塊結束時離開作用域。

```python
for i in range(3):
    let tmp = i * 2
# tmp 在此處無法存取

if true:
    let a = 1
# a 在此處無法存取
```

### 遮蔽

- 在內層作用域中宣告與外層同名的變數時，在內層作用域內會參照內層的變數。
- 離開內層作用域後會恢復為外層的變數。

```python
let x = 10
if true:
    let x = 99   # 遮蔽外層的 x
    print(x)     # 99
print(x)         # 10
```
