[English](../../tutorial/07-collections.md) | [日本語](../../ja/tutorial/07-collections.md) | [繁體中文](07-collections.md)

# 集合

[← 前一篇：結構體](06-structs.md) | [下一篇：進階功能 →](08-advanced.md)

Ry 有四種集合型別：**元組**、**列表**、**映射**、**集合**。

---

## 元組

元組是將多個值合為一體的不可變資料結構，可以持有不同型別的元素。

### 建立

```python
t = (1, 3.14)
```

### 型別標註

```python
t: (int, float) = (1, 3.14)
```

### 元素存取

使用 `.0`、`.1`、... 等索引來存取。

```python
t = (1, 3.14)
print(t.0)   # 1
print(t.1)   # 3.14
```

### 函式回傳值

想要回傳多個值時，元組很方便。

```python
fn swap(a: int, b: int) -> (int, int):
    return (b, a)

result = swap(1, 2)
print(result.0)  # 2
print(result.1)  # 1
```

### 限制事項

- 超出範圍的索引（例如：對只有 2 個元素的元組使用 `.2` 存取）會產生編譯錯誤。
- 將元組直接傳給 `print` 會產生錯誤。請個別傳遞各元素。

---

## 列表

列表是由相同型別的元素排列而成的可變長度資料結構。

### 建立

```python
xs = [1, 2, 3]
```

### 型別標註

```python
xs: List<int> = [1, 2, 3]
```

### 索引存取

```python
print(xs[0])   # 1

i = 1
print(xs[i])   # 2
```

### 索引賦值

```python
xs[0] = 99
```

### length

```python
print(length(xs))   # 3
```

### print

```python
print(xs)   # [1, 2, 3]
```

### for 走訪

```python
for x in xs:
    print(x)
```

### 函式參數

```python
fn first(xs: List<int>) -> int:
    return xs[0]
```

### filter、map、sort

串列支援 `filter`、`map`、`sort` 操作。這些操作會傳回新串列，不會修改原始串列。

```python
xs = [1, 2, 3, 4, 5]

# filter: 保留符合條件的元素
evens = xs.filter(fn(x: int): x > 3)
print(evens)   # [4, 5]

# map: 轉換每個元素
doubled = xs.map(fn(x: int): x * 2)
print(doubled)   # [2, 4, 6, 8, 10]

# sort: 升序排序（預設）
sorted = [3, 1, 2].sort()
print(sorted)   # [1, 2, 3]

# 鏈接
result = xs.filter(fn(x: int): x > 1).map(fn(x: int): x * 10).sort()
print(result)   # [20, 30, 40, 50]
```

### reduce、fold

`reduce` 從第一個元素開始將串列歸約為單一值。`fold` 則可提供明確的初始值。

```python
xs = [1, 2, 3, 4, 5]

# reduce: 從第一個元素開始
total = reduce(xs, fn(a: int, b: int): a + b)
print(total)   # 15

# fold: 提供明確的初始值
total2 = fold(xs, 0, fn(a: int, b: int): a + b)
print(total2)   # 15
```

### any、all

`any` 如果至少有一個元素滿足述詞則傳回 `true`。`all` 如果所有元素都滿足則傳回 `true`。

```python
xs = [1, 2, 3, 4, 5]

print(any(xs, fn(x: int): x > 4))   # true
print(any(xs, fn(x: int): x > 9))   # false

print(all(xs, fn(x: int): x > 0))   # true
print(all(xs, fn(x: int): x > 3))   # false
```

### sum、min、max

```python
xs = [3, 1, 4, 1, 5]
print(sum(xs))   # 14
print(min(xs))   # 1
print(max(xs))   # 5
```

### first、last、is_empty

```python
xs = [10, 20, 30]
print(first(xs))      # 10
print(last(xs))       # 30
print(is_empty(xs))   # false
```

### enumerate、zip

`enumerate` 為每個元素附上索引。`zip` 將兩個串列逐元素合併。

```python
xs = [10, 20, 30]
indexed = enumerate(xs)
# [(0, 10), (1, 20), (2, 30)]
for p in indexed:
    print(p.0)
    print(p.1)

ys = ["a", "b", "c"]
zipped = zip(xs, ys)
# [(10, "a"), (20, "b"), (30, "c")]
```

### 限制事項

- 所有元素必須是相同型別，不能混合不同型別。
- 空列表 `[]` 會產生錯誤。
- 超出範圍的存取會產生執行期錯誤（`exit(1)`）。
- 元素型別支援 `int`、`float`、`bool`、`str`。

---

## 映射

映射是管理鍵值對的關聯陣列。

### 建立

```python
m = {"a": 1, "b": 2}
```

### 型別標註

```python
m: Map<str, int> = {"a": 1, "b": 2}
```

### 鍵存取

```python
print(m["a"])   # 1
```

### 插入 / 更新

對新的鍵賦值即為插入，對既有的鍵賦值即為更新。

```python
m["c"] = 3    # 新增
m["a"] = 99   # 更新
```

### length

```python
print(length(m))   # 3
```

### print

```python
print(m)   # {a: 99, b: 2, c: 3}
```

### has_key

確認鍵是否存在。

```python
print(m.has_key("a"))   # true
```

### keys、values

`keys` 傳回所有鍵的串列。`values` 傳回所有值的串列。

```python
m = {"a": 1, "b": 2, "c": 3}
print(keys(m))     # ["a", "b", "c"]
print(values(m))   # [1, 2, 3]
```

### 函式參數

```python
fn get_val(m: Map<str, int>, k: str) -> int:
    return m[k]
```

### 限制事項

- 所有鍵必須是相同型別，所有值也必須是相同型別。
- 空映射需要型別標註。
- 存取不存在的鍵會產生執行期錯誤（`exit(1)`）。

---

## 集合

集合是持有相同型別元素且不重複的集合型別。

### 建立

```python
s = {1, 2, 3}
```

### 型別標註

```python
s: Set<int> = {1, 2, 3}
```

### in 運算子

使用 `in` 運算子確認元素是否包含在集合中。

```python
print(2 in s)   # true
print(5 in s)   # false
```

### add / remove

```python
s.add(4)       # 新增元素
s.remove(1)    # 移除元素
s.add(2)       # 已存在，因此忽略
```

### length / print

```python
print(length(s))  # 3
print(s)       # {2, 3, 4}
```

### for 走訪

```python
for x in s:
    print(x)
```

### 空集合

空集合需要型別標註。

```python
empty: Set<int> = {}
```

### 限制事項

- 所有元素必須是相同型別。
- 元素型別支援 `int`、`float`、`bool`、`str`。

---

[← 前一篇：結構體](06-structs.md) | [下一篇：進階功能 →](08-advanced.md)
