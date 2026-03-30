[English](../../tutorial/04-control-flow.md) | [日本語](../../ja/tutorial/04-control-flow.md) | [繁體中文](04-control-flow.md)

# 控制流程

[← 前一篇：運算子](03-operators.md) | [下一篇：函式 →](05-functions.md)

---

## if / else

使用 `if` 根據條件進行分支處理。

```python
x = 10

if x > 0:
    print(x)
else:
    print(0)
```

- `else` 可以省略。
- 條件式不限於 `bool`，也可以指定其他型別。對於 `int`，`0` 視為假，非 `0` 視為真。
- `if` 可以巢狀使用。

```python
a = 5
b = 3

if a > 0:
    if b > 0:
        print(a + b)   # 8
```

---

## while 迴圈

當條件為真時，重複執行區塊。

```python
i = 3
while i > 0:
    print(i)
    i = i - 1
# 3
# 2
# 1
```

---

## for 迴圈與 range

可使用列表或 `range` 進行迭代。

```python
for x in [1, 2, 3]:
    print(x)
# 1
# 2
# 3
```

`range(n)` 產生從 `0` 到 `n - 1` 的整數。

```python
for i in range(5):
    print(i)
# 0
# 1
# 2
# 3
# 4
```

`range(start, end)` 產生從 `start` 到 `end - 1` 的整數。

```python
for i in range(2, 5):
    print(i)
# 2
# 3
# 4
```

`..` 範圍運算子建立包含兩端的範圍。`1 .. 3` 產生 `[1, 2, 3]`。

```python
for i in 1 .. 3:
    print(i)
# 1
# 2
# 3
```

使用 `for k, v in map` 可以走訪映射的鍵值對。

```python
m = {"x": 10, "y": 20}
for k, v in m:
    print(k)
    print(v)
```

---

## break 與 continue

`break` 立即跳出迴圈。`continue` 跳過目前的迭代，進入下一次迭代。

```python
for i in range(10):
    if i == 5:
        break
    if i % 2 == 0:
        continue
    print(i)
# 1
# 3
```

在 `while` 中也可同樣使用。

```python
n = 0
while true:
    n = n + 1
    if n % 2 == 0:
        continue
    if n > 7:
        break
    print(n)
# 1
# 3
# 5
# 7
```

> **注意**：在巢狀迴圈中，`break` / `continue` 僅作用於最內層的迴圈。在迴圈外使用會產生編譯錯誤。

---

## 巢狀範例

`for` 和 `while` 可以巢狀使用。

```python
for i in range(1, 4):
    for j in range(1, 4):
        if j == 2:
            continue
        print(i * 10 + j)
# 11
# 13
# 21
# 23
# 31
# 33
```

---

## 作用域規則

控制流程的區塊具有作用域。

### 區塊作用域

在區塊內宣告的變數無法從區塊外部參照。

```python
if true:
    inner = 42
# 在此處參照 inner 會產生編譯錯誤
```

### 參照與重新賦值外部變數

可以從區塊內參照和重新賦值外部的變數。

```python
count = 0
for i in range(5):
    count = count + i
print(count)   # 10
```

### 內層作用域的重新賦值

在區塊內對變數賦值會修改外層的變數（Python 風格的作用域）。不會產生遮蔽——內層的賦值會修改同一個變數。

```python
x = 1
if true:
    x = 99
    print(x)   # 99
print(x)       # 99
```

---

## when

`when:` 用於多分支條件判斷，`when value:` 用於 enum、Option 等值的模式匹配。

### 條件分支 `when:`

```python
x = -2

when:
    x > 0:
        print("positive")
    x < 0:
        print("negative")
    else:
        print("zero")
```

### 模式匹配 `when value:`

```python
enum Color:
    Red
    Green
    Blue

c = Color::Green
when c:
    case Color::Red:
        print("red")
    case Color::Green:
        print("green")
    case Color::Blue:
        print("blue")
# green
```

### Option 的模式匹配

使用 `when` 可以安全地處理 `None` 的情況。

```python
x: Option<int> = Some(42)
when x:
    case Some(v):
        print(v)
    case None:
        print("nothing")
# 42
```

### 萬用字元與字面值

`_` 是可匹配任何值的萬用字元模式。也可以使用字面值（數值、字串、布林值）進行匹配。

```python
n = 5
when n:
    case 0:
        print("zero")
    case 1:
        print("one")
    case _:
        print("other")
# other
```

### guard 子句

可以使用 `if` 新增守衛條件。

```python
when n:
    case x if x > 0:
        print("positive")
    case x if x < 0:
        print("negative")
    case _:
        print("zero")
```

### `when:` 表達式

```python
label = when:
    score >= 90 => "A"
    score >= 80 => "B"
    else => "C"
```

> **注意**：`when value:` 必須涵蓋所有模式。enum 必須包含所有變體，Option 必須包含 `Some` 和 `None`，字面值則需要 `_`。

---

[← 前一篇：運算子](03-operators.md) | [下一篇：函式 →](05-functions.md)
