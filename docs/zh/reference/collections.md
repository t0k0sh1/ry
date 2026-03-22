[English](../../reference/collections.md) | [日本語](../../ja/reference/collections.md) | [繁體中文](collections.md)

# 集合參考（元組、串列、映射、集合）

## 元組

### 概述

固定長度、異質型別的值組合。以 LLVM literal StructType 實作，是堆疊上的值型別。

### 語法

```python
@const
t = (1, 3.14)
@const
t: (int, float) = (1, 3.14)
```

### 型別標註

```python
@const
pair: (int, str) = (42, "hello")
@const
triple: (int, float, bool) = (1, 2.0, true)
```

### 元素存取

使用 `.0`、`.1`、... 的數值索引存取。

```python
@const
t = (10, 3.14)
print(t.0)   # 10
print(t.1)   # 3.14
```

### 函式回傳值

```python
fn swap(a: int, b: int) -> (int, int):
    return (b, a)

@const
result = swap(1, 2)
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
@const
xs = [1, 2, 3]
@const
xs: List<int> = [1, 2, 3]
```

### 支援的元素型別

`int`, `float`, `bool`, `str`

### 索引存取

```python
@const
xs = [1, 2, 3]
print(xs[0])   # 1
print(xs[2])   # 3
```

### 索引賦值

```python
@const
xs = [1, 2, 3]
xs[0] = 99
print(xs[0])   # 99
```

### len

```python
@const
xs = [1, 2, 3]
print(len(xs))   # 3
```

### print

```python
@const
xs = [1, 2, 3]
print(xs)   # [1, 2, 3]
```

### for 走訪

```python
@const
xs = [10, 20, 30]
for x in xs:
    print(x)
# 10
# 20
# 30
```

### append

向串列末尾新增元素。此為就地修改操作。

```python
xs = [1, 2]
xs.append(3)
print(xs)   # [1, 2, 3]
```

### pop

移除並回傳串列的最後一個元素。對空串列呼叫會產生執行時錯誤。

```python
xs = [1, 2, 3]
@const
v = xs.pop()
print(v)    # 3
print(xs)   # [1, 2]
```

### reverse

傳回元素順序反轉的新串列。原始串列不會被修改。也適用於字串。

```python
@const
xs = [1, 2, 3]
print(reverse(xs))   # [3, 2, 1]
print(xs)            # [1, 2, 3]（未修改）
```

### slice

傳回從 `start`（含）到 `end`（不含）的新子串列。索引會被鉗制在有效範圍內。

```python
@const
xs = [1, 2, 3, 4, 5]
print(slice(xs, 1, 3))     # [2, 3]
print(slice(xs, 0, 100))   # [1, 2, 3, 4, 5]（鉗制）
```

### take

傳回包含前 `n` 個元素的新串列。若 `n` 超過串列長度，傳回整個串列的副本。若 `n <= 0`，傳回空串列。原始串列不會被修改。

```python
@const
xs = [1, 2, 3, 4, 5]
@const
ys = xs.take(3)
print(ys)   # [1, 2, 3]
print(xs.take(10))   # [1, 2, 3, 4, 5]（鉗制）
print(xs.take(0))    # []
```

### tap

對每個元素呼叫給定函式（忽略回傳值），然後傳回原始串列。適用於方法鏈中的除錯或插入副作用。

```python
@const
xs = [1, 2, 3]
@const
ys = xs.tap(fn(x: int): print(x)).map(fn(x: int): x * 2)
# 輸出 1, 2, 3，然後 ys = [2, 4, 6]
```

### filter

傳回僅包含滿足述詞的元素的新串列。原始串列不會被修改。

```python
@const
xs = [1, 2, 3, 4, 5]
@const
ys = xs.filter(fn(x: int): x > 3)
print(ys)   # [4, 5]
```

### map

傳回將每個元素以給定函式轉換後的新串列。輸出元素型別可以與輸入不同。原始串列不會被修改。

```python
@const
xs = [1, 2, 3]
@const
ys = xs.map(fn(x: int): x * 2)
print(ys)   # [2, 4, 6]
```

### sort

傳回排序後的新串列。預設為升序。可提供自訂比較函式。原始串列不會被修改。排序是**穩定的**（相等元素保持原始順序）。內部使用 TimSort。

```python
@const
xs = [3, 1, 2]
print(xs.sort())   # [1, 2, 3]

# 降序排序
@const
desc = xs.sort(fn(a: int, b: int): a > b)
print(desc)   # [3, 2, 1]
```

### filter、map、sort 的鏈接

這些函式傳回新串列，因此可透過 UFCS 進行鏈接。

```python
@const
xs = [5, 3, 1, 4, 2]
@const
result = xs.filter(fn(x: int): x > 1).map(fn(x: int): x * 10).sort()
print(result)   # [20, 30, 40, 50]
```

### reduce

使用累加函式將串列歸約為單一值，以第一個元素作為初始值。

```python
@const
xs = [1, 2, 3, 4, 5]
@const
total = reduce(xs, fn(a: int, b: int): a + b)
print(total)   # 15
```

### fold

使用明確的初始值和累加函式將串列折疊為單一值。

```python
@const
xs = [1, 2, 3, 4, 5]
@const
total = fold(xs, 0, fn(a: int, b: int): a + b)
print(total)   # 15
```

### any

如果至少有一個元素滿足述詞，則傳回 `true`。

```python
@const
xs = [1, 2, 3, 4, 5]
print(any(xs, fn(x: int): x > 4))   # true
print(any(xs, fn(x: int): x > 9))   # false
```

### all

如果所有元素都滿足述詞，則傳回 `true`。

```python
@const
xs = [2, 4, 6]
print(all(xs, fn(x: int): x > 0))   # true
print(all(xs, fn(x: int): x > 3))   # false
```

### sum

傳回所有元素的總和。

```python
@const
xs = [1, 2, 3, 4, 5]
print(sum(xs))   # 15
```

### min

傳回最小的元素。

```python
@const
xs = [3, 1, 4, 1, 5]
print(min(xs))   # 1
```

### max

傳回最大的元素。

```python
@const
xs = [3, 1, 4, 1, 5]
print(max(xs))   # 5
```

### first

傳回第一個元素。對空串列呼叫會產生執行時錯誤。

```python
@const
xs = [10, 20, 30]
print(first(xs))   # 10
```

### last

傳回最後一個元素。對空串列呼叫會產生執行時錯誤。

```python
@const
xs = [10, 20, 30]
print(last(xs))   # 30
```

### is_empty

如果串列沒有元素則傳回 `true`。

```python
@const
xs = [1, 2, 3]
print(is_empty(xs))   # false
```

### enumerate

傳回 `(索引, 元素)` 元組的串列。

```python
@const
xs = [10, 20, 30]
@const
pairs = enumerate(xs)
# pairs = [(0, 10), (1, 20), (2, 30)]

# for 迴圈中的元組解構
for i, x in enumerate(xs):
    print(f"{i}: {x}")    # 0: 10, 1: 20, 2: 30
```

### zip

將兩個串列合併為 `(元素1, 元素2)` 元組的串列。結果長度等於較短的串列。

```python
@const
xs = [1, 2, 3]
@const
ys = ["a", "b", "c"]
@const
pairs = zip(xs, ys)
# pairs = [(1, "a"), (2, "b"), (3, "c")]

# for 迴圈中的元組解構
for a, b in zip(xs, ys):
    print(f"{a}: {b}")    # 1: a, 2: b, 3: c
```

### insert

在指定索引處插入元素。該索引及之後的元素向右移動。

```python
xs = [1, 2, 3]
insert(xs, 1, 99)
print(xs)   # [1, 99, 2, 3]
```

### remove_at

移除並回傳指定索引處的元素。該索引之後的元素向左移動。

```python
xs = [1, 2, 3, 4]
@const
v = remove_at(xs, 1)
print(v)    # 2
print(xs)   # [1, 3, 4]
```

### remove

從串列中移除第一個符合的指定值。若找不到該值，則不做任何操作。這是一個可變操作。

```python
xs = [1, 2, 3, 2, 4]
remove(xs, 2)
print(xs)   # [1, 3, 2, 4]
```

### distinct

回傳一個移除重複元素的新串列。保持原始順序（保留第一次出現）。原始串列不會被修改。

```python
@const
xs = [1, 2, 3, 2, 1, 4]
print(distinct(xs))   # [1, 2, 3, 4]
print(xs)             # [1, 2, 3, 2, 1, 4]（未變更）
```

### flatten

將巢狀串列（串列的串列）展開一層。回傳新串列。原始串列不會被修改。

```python
@const
xs = [[1, 2], [3, 4]]
print(flatten(xs))   # [1, 2, 3, 4]
print(xs)            # [[1, 2], [3, 4]]（未變更）
```

### 操作複雜度

| 操作 | 複雜度 |
|------|--------|
| `xs[i]` 索引存取 | O(1) |
| `append` / `append!` | 均攤 O(1) |
| `pop` | O(1) |
| `first`、`last` | O(1) |
| `insert`、`remove_at` | O(n) |
| `sort` / `sort!` | O(n log n) |
| `take` | O(n) |
| `tap` | O(n) |
| `filter`、`map`、`reduce`、`fold` | O(n) |
| `reverse` / `reverse!` | O(n) |
| `distinct` | O(n) |
| `len` | O(1) |

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
@const
m = {"a": 1, "b": 2}
@const
m: Map<str, int> = {"a": 1, "b": 2}
```

### 鍵存取

```python
@const
m = {"a": 1, "b": 2}
print(m["a"])   # 1
```

### 插入與更新

```python
@const
m = {"a": 1}
m["b"] = 2     # 新增
m["a"] = 99    # 更新
```

### len

```python
@const
m = {"a": 1, "b": 2, "c": 3}
print(len(m))   # 3
```

### print

```python
@const
m = {"a": 1, "b": 2}
print(m)   # {a: 1, b: 2}
```

### has_key

```python
@const
m = {"a": 1, "b": 2}
print(m.has_key("a"))   # true
print(m.has_key("z"))   # false
```

### keys

傳回映射中所有鍵的串列。

```python
@const
m = {"a": 1, "b": 2, "c": 3}
print(keys(m))   # ["a", "b", "c"]
```

### values

傳回映射中所有值的串列。

```python
@const
m = {"a": 1, "b": 2, "c": 3}
print(values(m))   # [1, 2, 3]
```

### items

回傳映射中所有條目的 `(鍵, 值)` 元組串列。

```python
@const
m = {"a": 1, "b": 2}
@const
pairs = items(m)
# pairs = [("a", 1), ("b", 2)]
```

### remove（映射）

從映射中刪除指定鍵的條目。若鍵不存在則不做任何操作。

```python
@const
m = {"a": 1, "b": 2}
remove(m, "a")
print(m)   # {b: 2}
```

### get

回傳指定鍵的值，若鍵不存在則回傳預設值。

```python
@const
m = {"a": 1, "b": 2}
print(get(m, "a", 0))   # 1
print(get(m, "z", 0))   # 0
```

### merge

回傳一個合併兩個映射的新映射。當鍵重複時，第二個映射的值優先。原始映射不會被修改。

```python
@const
m1 = {"a": 1, "b": 2}
@const
m2 = {"b": 99, "c": 3}
@const
m3 = merge(m1, m2)
print(m3["a"])   # 1
print(m3["b"])   # 99
print(m3["c"])   # 3
```

### 限制與錯誤

| 限制 | 詳細 |
|------|------|
| 所有鍵必須為相同型別 | 混合不同型別的鍵會產生編譯錯誤 |
| 所有值必須為相同型別 | 混合不同型別的值會產生編譯錯誤 |
| 空映射 | 需要型別標註（如 `m: Map<str, int> = {"a": 1}`） |
| 存取不存在的鍵 | 執行時錯誤（exit(1)） |
| 鍵搜尋 | 雜湊表（平均 O(1)） |
| 容量超過時 | 自動擴展為 2 倍 |

---

## 集合

### 概述

保持相同型別的元素且不重複的集合。分配於堆積上。

### 語法

```python
@const
s = {1, 2, 3}
@const
s: Set<int> = {1, 2, 3}
```

### 支援的元素型別

`int`, `float`, `bool`, `str`

### in 運算子（歸屬檢查）

```python
@const
s = {1, 2, 3}
print(2 in s)   # true
print(5 in s)   # false
```

### len

```python
@const
s = {1, 2, 3}
print(len(s))   # 3
```

### print

```python
@const
s = {1, 2, 3}
print(s)   # {1, 2, 3}
```

### add（新增元素）

新增重複的元素時會被忽略。

```python
@const
s = {1, 2, 3}
s.add(4)         # 新增
s.add(1)         # 已存在，因此忽略
print(len(s))    # 4
```

### remove（刪除元素）

```python
@const
s = {1, 2, 3}
s.remove(2)
print(2 in s)   # false
```

### for 走訪

```python
@const
s = {10, 20, 30}
for x in s:
    print(x)
```

### 空集合

空集合需要型別標註。

```python
@const
s: Set<int> = {}
```

### 函式引數

```python
fn has_value(s: Set<int>, v: int) -> bool:
    return v in s
```

### union

回傳包含兩個集合所有元素的新集合。

```python
@const
a = {1, 2, 3}
@const
b = {3, 4, 5}
print(union(a, b))   # {1, 2, 3, 4, 5}
```

### intersection

回傳僅包含兩個集合中都存在的元素的新集合。

```python
@const
a = {1, 2, 3}
@const
b = {2, 3, 4}
print(intersection(a, b))   # {2, 3}
```

### difference

回傳包含在第一個集合中但不在第二個集合中的元素的新集合。

```python
@const
a = {1, 2, 3}
@const
b = {2, 3, 4}
print(difference(a, b))   # {1}
```

### symmetric_difference

回傳包含在任一集合中但不同時在兩個集合中的元素的新集合。

```python
@const
a = {1, 2, 3}
@const
b = {2, 3, 4}
print(symmetric_difference(a, b))   # {1, 4}
```

### is_subset

如果第一個集合的所有元素都包含在第二個集合中，則回傳 `true`。

```python
@const
a = {1, 2}
@const
b = {1, 2, 3}
print(is_subset(a, b))   # true
print(is_subset(b, a))   # false
```

### is_superset

如果第一個集合包含第二個集合的所有元素，則回傳 `true`。

```python
@const
a = {1, 2, 3}
@const
b = {1, 2}
print(is_superset(a, b))   # true
print(is_superset(b, a))   # false
```

### 限制與錯誤

| 限制 | 詳細 |
|------|------|
| 所有元素必須為相同型別 | 混合不同型別會產生編譯錯誤 |
| 空集合 `{}` | 需要型別標註 |
| 元素搜尋 | 雜湊表（平均 O(1)） |
| 容量超過時 | 自動擴展為 2 倍 |
