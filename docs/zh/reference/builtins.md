[English](../../reference/builtins.md) | [日本語](../../ja/reference/builtins.md) | [繁體中文](builtins.md)

# 內建函式參考

## 函式一覽

### 核心

| 函式 | 說明 |
|------|------|
| `print(expr)` | 將值輸出到標準輸出 |
| `len(x)` | 回傳串列、映射、集合的元素數量，或字串的長度 |
| `range(n)` / `range(start, end)` / `range(start, end, step)` | 生成整數串列 |

### Option

| 函式 | 說明 |
|------|------|
| `Some(expr)` | 建構 Option 型別的有值變體 |
| `unwrap(opt)` | 取出 Option 值 |

### 集合操作

| 函式 | 說明 |
|------|------|
| `has_key(map, key)` | 回傳映射中是否存在該鍵 |
| `add(set, value)` | 向集合新增元素（重複則忽略） |
| `remove(set, value)` | 從集合刪除元素 |
| `append(list, value)` | 向串列末尾新增元素（就地修改） |
| `pop(list)` | 移除並回傳串列的最後一個元素 |
| `reverse(list)` | 傳回反轉後的新串列（也適用於字串） |
| `slice(list, start, end)` | 傳回從 start 到 end 的新子串列 |
| `filter(list, pred)` | 傳回僅包含滿足述詞的元素的新串列 |
| `map(list, fn)` | 傳回將每個元素轉換後的新串列 |
| `sort(list)` / `sort(list, comp)` | 傳回排序後的新串列（預設升序） |
| `insert(list, i, val)` | 在索引 i 處插入元素 |
| `remove_at(list, i)` | 移除並回傳索引 i 處的元素 |
| `items(map)` | 回傳 (鍵, 值) 元組的串列 |
| `remove(map, key)` | 刪除指定鍵的條目 |
| `get(map, key, default)` | 回傳鍵的值，若不存在則回傳預設值 |
| `union(set, set)` | 回傳兩個集合的聯集 |
| `intersection(set, set)` | 回傳兩個集合的交集 |
| `difference(set, set)` | 回傳兩個集合的差集 |
| `symmetric_difference(set, set)` | 回傳兩個集合的對稱差 |
| `is_subset(set, set)` | 回傳第一個集合是否為第二個的子集 |
| `is_superset(set, set)` | 回傳第一個集合是否為第二個的超集 |

### [字串操作](builtins-string.md)

| 函式 | 說明 |
|------|------|
| `contains(s, sub)` | 是否包含子字串 |
| `starts_with(s, prefix)` | 是否以前綴開頭 |
| `ends_with(s, suffix)` | 是否以後綴結尾 |
| `find(s, sub)` | 子字串的位置（未找到為 -1） |
| `substring(s, start, end)` | 取得子字串 |
| `char_at(s, i)` | 取得指定位置的字元 |
| `replace(s, old, new)` | 全部取代子字串 |
| `to_upper(s)` / `to_lower(s)` | 大小寫轉換 |
| `trim(s)` / `trim_start(s)` / `trim_end(s)` | 去除空白 |
| `repeat(s, n)` | 將字串重複 n 次 |
| `reverse(s)` | 反轉字串 |
| `split(s, delim)` | 分割字串並回傳串列 |
| `join(list, sep)` | 以分隔符號連接串列中的字串 |
| `to_int(s)` / `to_float(s)` / `to_str(v)` | 型別轉換 |

→ 詳細請參閱 **[字串操作函式參考](builtins-string.md)**

---

## print

**簽名：** `print(expr)`

將值輸出到標準輸出。末尾會加上換行。

| 型別 | 輸出格式 |
|----|---------|
| `int` | `%ld` |
| `float` | `%g` |
| `bool` | `true` / `false` |
| `str` | `%s` |
| `Option` (Some) | `Some(值)` |
| `Option` (None) | `None` |
| `list` | `[元素1, 元素2, ...]` |
| `map` | `{鍵1: 值1, 鍵2: 值2, ...}` |
| `set` | `{元素1, 元素2, ...}` |
| `enum` | 變體名稱（例如：`Red`） |

```python
print(42)          # 42
print(3.14)        # 3.14
print(true)        # true
print("hello")     # hello
print(Some(1))     # Some(1)
print(None)        # None
print([1, 2, 3])   # [1, 2, 3]
print({"a": 1})    # {a: 1}
print({1, 2, 3})   # {1, 2, 3}
```

**錯誤條件：** 直接傳入結構體或元組會產生編譯錯誤。

---

## Some

**簽名：** `Some(expr) -> Option<T>`

建構 Option 型別的有值變體。

```python
let x: Option<int> = Some(42)
print(x)   # Some(42)
```

---

## unwrap

**簽名：** `unwrap(opt: Option<T>) -> T`

從 Option 值中取出內容。也可使用 UFCS 記法。

```python
let x = Some(42)
print(unwrap(x))    # 42
print(x.unwrap())   # 42 (UFCS)
```

**錯誤條件：** 傳入 `None` 會產生執行時錯誤（exit(1)）。

---

## len

**簽名：** `len(x: List<T> | Map<K, V> | Set<T> | str) -> int`

回傳串列、映射、集合的元素數量，或字串的位元組長度。

```python
print(len([1, 2, 3]))         # 3
print(len({"a": 1, "b": 2})) # 2
print(len({1, 2, 3}))         # 3
print(len("hello"))           # 5
```

---

## has_key

**簽名：** `has_key(m: Map<K, V>, key: K) -> bool`

回傳映射中是否存在指定的鍵。也可使用 UFCS 記法。

```python
let m = {"a": 1, "b": 2}
print(has_key(m, "a"))    # true
print(m.has_key("z"))     # false (UFCS)
```

---

## add

**簽名：** `add(s: Set<T>, value: T)`

向集合新增元素。若元素已存在則不做任何操作。也可使用 UFCS 記法。

```python
let s = {1, 2, 3}
s.add(4)          # UFCS
add(s, 5)         # 一般呼叫
s.add(1)          # 已存在，因此忽略
print(len(s))     # 5
```

---

## remove

**簽名：** `remove(s: Set<T>, value: T)`

從集合刪除元素。也可使用 UFCS 記法。

```python
let s = {1, 2, 3}
s.remove(2)       # UFCS
print(2 in s)     # false
```

---

## range

**簽名：** `range(n: int) -> List<int>` / `range(start: int, end: int) -> List<int>` / `range(start: int, end: int, step: int) -> List<int>`

生成整數串列。

| 形式 | 生成的值 |
|------|------------|
| `range(n)` | `[0, 1, ..., n-1]` |
| `range(start, end)` | `[start, start+1, ..., end-1]` |
| `range(start, end, step)` | `[start, start+step, start+2*step, ...]`（不包含 `end`） |

- `step > 0` 時，從 `start` 向 `end` 遞增生成。
- `step < 0` 時，從 `start` 向 `end` 遞減生成。
- `step == 0` 時，會產生執行時錯誤。

```python
print(range(3))           # [0, 1, 2]
print(range(2, 5))        # [2, 3, 4]
print(range(0, 10, 2))    # [0, 2, 4, 6, 8]
print(range(10, 0, -3))   # [10, 7, 4, 1]

for i in range(3):
    print(i)
# 0
# 1
# 2
```

---

## append

**簽名：** `append(list: List<T>, value: T)`

向串列末尾新增元素。此為就地修改操作——串列會被直接修改。也可使用 UFCS 記法。

```python
var xs = [1, 2]
xs.append(3)
print(xs)   # [1, 2, 3]
```

---

## pop

**簽名：** `pop(list: List<T>) -> T`

移除並回傳串列的最後一個元素。也可使用 UFCS 記法。

```python
var xs = [1, 2, 3]
let v = xs.pop()
print(v)    # 3
print(xs)   # [1, 2]
```

**錯誤條件：** 對空串列呼叫 `pop()` 會產生執行時錯誤（exit(1)）。

---

## reverse（串列）

**簽名：** `reverse(list: List<T>) -> List<T>`

傳回元素順序反轉的新串列。原始串列不會被修改。也適用於字串（請參閱[字串操作](builtins-string.md)）。也可使用 UFCS 記法。

```python
let xs = [1, 2, 3]
let ys = reverse(xs)
print(ys)   # [3, 2, 1]
print(xs)   # [1, 2, 3]（未修改）
```

---

## slice

**簽名：** `slice(list: List<T>, start: int, end: int) -> List<T>`

傳回從 `start`（含）到 `end`（不含）的新子串列。索引會被鉗制在有效範圍內（`0` 到 `len(list)`）。也可使用 UFCS 記法。

```python
let xs = [1, 2, 3, 4, 5]
print(slice(xs, 1, 3))     # [2, 3]
print(slice(xs, 0, 100))   # [1, 2, 3, 4, 5]（鉗制）
```

---

## filter

**簽名:** `filter(list: List<T>, pred: fn(T) -> bool) -> List<T>`

傳回僅包含述詞回傳 `true` 的元素的新串列。原始串列不會被修改。也可使用 UFCS 記法。

```python
let xs = [1, 2, 3, 4, 5]
let ys = xs.filter((x: int) -> x > 3)
print(ys)   # [4, 5]
print(xs)   # [1, 2, 3, 4, 5]  （未修改）
```

---

## map

**簽名:** `map(list: List<T>, fn: fn(T) -> U) -> List<U>`

傳回將每個元素以給定函式轉換後的新串列。輸出元素型別可以與輸入不同。原始串列不會被修改。也可使用 UFCS 記法。

```python
let xs = [1, 2, 3]
let ys = xs.map((x: int) -> x * 2)
print(ys)   # [2, 4, 6]
```

---

## sort

**簽名:** `sort(list: List<T>) -> List<T>` / `sort(list: List<T>, comp: fn(T, T) -> bool) -> List<T>`

傳回排序後的新串列。預設為升序。可提供自訂比較函式（第一引數應排在第二引數之前時回傳 `true`）。原始串列不會被修改。也可使用 UFCS 記法。

```python
let xs = [3, 1, 2]
print(xs.sort())   # [1, 2, 3]

# 降序排序
let desc = xs.sort((a: int, b: int) -> a > b)
print(desc)   # [3, 2, 1]
```
