[English](../../reference/collections.md) | [日本語](../../ja/reference/collections.md) | [繁體中文](collections.md)

# 集合參考（元組、串列、映射、集合）

## 元組

### 概述

固定長度、異質型別的值組合。以 LLVM literal StructType 實作，是堆疊上的值型別。

### 語法

```python
let t = (1, 3.14)
let t: (int, float) = (1, 3.14)
```

### 型別標註

```python
let pair: (int, str) = (42, "hello")
let triple: (int, float, bool) = (1, 2.0, true)
```

### 元素存取

使用 `.0`、`.1`、... 的數值索引存取。

```python
let t = (10, 3.14)
print(t.0)   # 10
print(t.1)   # 3.14
```

### 函式回傳值

```python
fn swap(a: int, b: int) -> (int, int):
    return (b, a)

let result = swap(1, 2)
print(result.0)   # 2
print(result.1)   # 1
```

### 限制與錯誤

| 限制 | 詳細 |
|------|------|
| 超出範圍的索引 | 編譯錯誤 |
| 直接將元組傳給 `print` | 編譯錯誤（print 不支援） |

---

## 串列

### 概述

相同型別的可變長度序列。分配於堆積上。

### 語法

```python
let xs = [1, 2, 3]
let xs: List<int> = [1, 2, 3]
```

### 支援的元素型別

`int`, `float`, `bool`, `str`

### 索引存取

```python
let xs = [1, 2, 3]
print(xs[0])   # 1
print(xs[2])   # 3
```

### 索引賦值

```python
let xs = [1, 2, 3]
xs[0] = 99
print(xs[0])   # 99
```

### len

```python
let xs = [1, 2, 3]
print(len(xs))   # 3
```

### print

```python
let xs = [1, 2, 3]
print(xs)   # [1, 2, 3]
```

### for 走訪

```python
let xs = [10, 20, 30]
for x in xs:
    print(x)
# 10
# 20
# 30
```

### append

向串列末尾新增元素。此為就地修改操作。

```python
var xs = [1, 2]
xs.append(3)
print(xs)   # [1, 2, 3]
```

### pop

移除並回傳串列的最後一個元素。對空串列呼叫會產生執行時錯誤。

```python
var xs = [1, 2, 3]
let v = xs.pop()
print(v)    # 3
print(xs)   # [1, 2]
```

### reverse

傳回元素順序反轉的新串列。原始串列不會被修改。也適用於字串。

```python
let xs = [1, 2, 3]
print(reverse(xs))   # [3, 2, 1]
print(xs)            # [1, 2, 3]（未修改）
```

### slice

傳回從 `start`（含）到 `end`（不含）的新子串列。索引會被鉗制在有效範圍內。

```python
let xs = [1, 2, 3, 4, 5]
print(slice(xs, 1, 3))     # [2, 3]
print(slice(xs, 0, 100))   # [1, 2, 3, 4, 5]（鉗制）
```

### filter

傳回僅包含滿足述詞的元素的新串列。原始串列不會被修改。

```python
let xs = [1, 2, 3, 4, 5]
let ys = xs.filter((x: int) -> x > 3)
print(ys)   # [4, 5]
```

### map

傳回將每個元素以給定函式轉換後的新串列。輸出元素型別可以與輸入不同。原始串列不會被修改。

```python
let xs = [1, 2, 3]
let ys = xs.map((x: int) -> x * 2)
print(ys)   # [2, 4, 6]
```

### sort

傳回排序後的新串列。預設為升序。可提供自訂比較函式。原始串列不會被修改。

```python
let xs = [3, 1, 2]
print(xs.sort())   # [1, 2, 3]

# 降序排序
let desc = xs.sort((a: int, b: int) -> a > b)
print(desc)   # [3, 2, 1]
```

### filter、map、sort 的鏈接

這些函式傳回新串列，因此可透過 UFCS 進行鏈接。

```python
let xs = [5, 3, 1, 4, 2]
let result = xs.filter((x: int) -> x > 1).map((x: int) -> x * 10).sort()
print(result)   # [20, 30, 40, 50]
```

### 限制與錯誤

| 限制 | 詳細 |
|------|------|
| 所有元素必須為相同型別 | 混合不同型別會產生編譯錯誤 |
| 空串列 `[]` | 無法進行型別推論，會產生編譯錯誤 |
| 超出範圍的存取 | 執行時錯誤（exit(1)） |

---

## 映射

### 概述

鍵與值的對應表。分配於堆積上。

### 語法

```python
let m = {"a": 1, "b": 2}
let m: Map<str, int> = {"a": 1, "b": 2}
```

### 鍵存取

```python
let m = {"a": 1, "b": 2}
print(m["a"])   # 1
```

### 插入與更新

```python
let m = {"a": 1}
m["b"] = 2     # 新增
m["a"] = 99    # 更新
```

### len

```python
let m = {"a": 1, "b": 2, "c": 3}
print(len(m))   # 3
```

### print

```python
let m = {"a": 1, "b": 2}
print(m)   # {a: 1, b: 2}
```

### has_key

```python
let m = {"a": 1, "b": 2}
print(m.has_key("a"))   # true
print(m.has_key("z"))   # false
```

### 限制與錯誤

| 限制 | 詳細 |
|------|------|
| 所有鍵必須為相同型別 | 混合不同型別的鍵會產生編譯錯誤 |
| 所有值必須為相同型別 | 混合不同型別的值會產生編譯錯誤 |
| 空映射 | 需要型別標註（如 `let m: Map<str, int> = {"a": 1}`） |
| 存取不存在的鍵 | 執行時錯誤（exit(1)） |
| 鍵搜尋 | 線性掃描 |
| 容量超過時 | 自動擴展為 2 倍 |

---

## 集合

### 概述

保持相同型別的元素且不重複的集合。分配於堆積上。

### 語法

```python
let s = {1, 2, 3}
let s: Set<int> = {1, 2, 3}
```

### 支援的元素型別

`int`, `float`, `bool`, `str`

### in 運算子（歸屬檢查）

```python
let s = {1, 2, 3}
print(2 in s)   # true
print(5 in s)   # false
```

### len

```python
let s = {1, 2, 3}
print(len(s))   # 3
```

### print

```python
let s = {1, 2, 3}
print(s)   # {1, 2, 3}
```

### add（新增元素）

新增重複的元素時會被忽略。

```python
let s = {1, 2, 3}
s.add(4)         # 新增
s.add(1)         # 已存在，因此忽略
print(len(s))    # 4
```

### remove（刪除元素）

```python
let s = {1, 2, 3}
s.remove(2)
print(2 in s)   # false
```

### for 走訪

```python
let s = {10, 20, 30}
for x in s:
    print(x)
```

### 空集合

空集合需要型別標註。

```python
let s: Set<int> = {}
```

### 函式引數

```python
fn has_value(s: Set<int>, v: int) -> bool:
    return v in s
```

### 限制與錯誤

| 限制 | 詳細 |
|------|------|
| 所有元素必須為相同型別 | 混合不同型別會產生編譯錯誤 |
| 空集合 `{}` | 需要型別標註 |
| 元素搜尋 | 線性掃描 |
| 容量超過時 | 自動擴展為 2 倍 |
