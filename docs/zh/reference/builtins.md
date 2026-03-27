[English](../../reference/builtins.md) | [日本語](../../ja/reference/builtins.md) | [繁體中文](builtins.md)

# 內建函式參考

## 函式一覽

### 核心

| 函式 | 說明 |
|------|------|
| `print(expr)` | 將值輸出到標準輸出 |
| `length(value)` | 回傳串列、映射、集合的元素數量，或字串的 UTF-8 字元數 |
| `range(count)` / `range(start, end)` / `range(start, end, step)` | 生成整數串列 |
| `exit(code)` | 以指定的結束碼終止程序 |
| `args()` | 以 `List<str>` 回傳命令列引數 |
| `available_parallelism()` | 回傳此系統建議的平行度（可用執行緒數） |
| `sleep(duration_ms)` | 暫停執行指定的毫秒數 |
| `cancel(task)` | 請求取消已 spawn 的任務 |
| `is_cancelled()` | 當前任務已被取消時回傳 `true` |
| `task_group(fn)` | 執行 lambda 並自動 join 其中 spawn 的所有任務 |
| `env(key)` | 回傳環境變數為 `Option<str>` |
| `env(key, default)` | 回傳環境變數，若未設定則回傳 `default` |

### Option

| 函式 | 說明 |
|------|------|
| `Some(expr)` | 建構 Option 型別的有值變體 |

### 集合操作

| 函式 | 說明 |
|------|------|
| `has_key(map, key)` | 回傳映射中是否存在該鍵 |
| `add(set, value)` | 向集合新增元素（重複則忽略） |
| `remove(set, value)` | 從集合刪除元素 |
| `append(list, value)` / `append!(list, value)` | 向串列末尾新增元素（就地修改） |
| `appended(list, value)` | 傳回新增元素後的新串列（非破壞性） |
| `pop(list)` | 移除並回傳串列的最後一個元素（`Option<T>`） |
| `reverse(list)` | 傳回反轉後的新串列（也適用於字串） |
| `reverse!(list)` | 就地反轉串列（破壞性） |
| `slice(list, start, end)` | 傳回從 start 到 end 的新子串列 |
| `take(list, n)` | 傳回包含前 n 個元素的新串列 |
| `tap(list, fn)` | 對每個元素呼叫 fn 以執行副作用，傳回原始串列 |
| `filter(list, pred)` | 傳回僅包含滿足述詞的元素的新串列 |
| `map(list, fn)` | 傳回將每個元素轉換後的新串列 |
| `sort(list)` / `sort(list, comp)` | 傳回排序後的新串列（預設升序） |
| `sort!(list)` / `sort!(list, comp)` | 就地排序串列（破壞性） |
| `insert(list, i, value)` | 在索引 i 處插入元素 |
| `remove_at(list, i)` | 移除並回傳索引 i 處的元素 |
| `items(map)` | 回傳 (鍵, 值) 元組的串列 |
| `remove(map, key)` | 刪除指定鍵的條目 |
| `get(map, key)` | 回傳鍵的值（`Option<V>`） |
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
| `contains(string, substring)` | 是否包含子字串 |
| `starts_with(string, prefix)` | 是否以前綴開頭 |
| `ends_with(string, suffix)` | 是否以後綴結尾 |
| `find(string, substring)` | 子字串的字元位置（`Option<int>`） |
| `byte_len(string)` | 回傳字串的位元組長度 |
| `substring(string, start, end)` | 取得子字串 |
| `char_at(string, i)` | 取得指定位置的字元 |
| `replace(string, old, new)` | 全部取代子字串 |
| `to_upper(string)` / `to_lower(string)` | 大小寫轉換 |
| `trim(string)` / `trim_start(string)` / `trim_end(string)` | 去除空白 |
| `repeat(string, count)` | 將字串重複 n 次 |
| `reverse(string)` | 反轉字串 |
| `split(string, delimiter)` | 分割字串並回傳串列 |
| `join(values, sep)` | 以分隔符號連接串列中的字串 |
| `to_int(string)` / `to_float(string)` / `to_str(v)` | 型別轉換 |

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
x: Option<int> = Some(42)
print(x)   # Some(42)
```

---

## length

**簽名：** `length(value: List<T> | Map<K, V> | Set<T> | str) -> int`

回傳串列、映射、集合的元素數量，或字串的 UTF-8 字元數。如需取得位元組長度，請使用 `byte_len()`。

```python
print(length([1, 2, 3]))         # 3
print(length({"a": 1, "b": 2})) # 2
print(length({1, 2, 3}))         # 3
print(length("hello"))           # 5
print(length("あいう"))           # 3 (UTF-8 字元數)
```

---

## has_key

**簽名：** `has_key(map: Map<K, V>, key: K) -> bool`

回傳映射中是否存在指定的鍵。也可使用 UFCS 記法。

```python
m = {"a": 1, "b": 2}
print(has_key(m, "a"))    # true
print(m.has_key("z"))     # false (UFCS)
```

---

## add

**簽名：** `add(set: Set<T>, value: T)`

向集合新增元素。若元素已存在則不做任何操作。也可使用 UFCS 記法。

```python
s = {1, 2, 3}
s.add(4)          # UFCS
add(s, 5)         # 一般呼叫
s.add(1)          # 已存在，因此忽略
print(length(s))     # 5
```

---

## remove

**簽名：** `remove(set: Set<T>, value: T)`

從集合刪除元素。也可使用 UFCS 記法。

```python
s = {1, 2, 3}
s.remove(2)       # UFCS
print(2 in s)     # false
```

---

## range

**簽名：** `range(count: int) -> List<int>` / `range(start: int, end: int) -> List<int>` / `range(start: int, end: int, step: int) -> List<int>`

生成整數串列。

| 形式 | 生成的值 |
|------|------------|
| `range(count)` | `[0, 1, ..., count-1]` |
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

## exit

**簽名：** `exit(code: int)`

以指定的結束碼立即終止程序。`exit()` 之後的程式碼將不會被執行。

```python
exit(0)        # 正常終止
exit(1)        # 錯誤終止
```

---

## args

**簽名：** `args() -> List<str>`

以字串串列的形式回傳傳遞給腳本的命令列引數。不包含直譯器名稱或腳本檔案名稱——僅包含腳本路徑之後的引數。

```python
# 執行: ry script.ry hello world
a = args()
print(length(a))    # 2
print(a[0])      # hello
print(a[1])      # world

for x in args():
    print(x)
```

---

## sleep

**簽名：** `sleep(duration_ms: int) -> Unit`

讓目前執行緒暫停執行約 `duration_ms` 毫秒。若 `duration_ms` 小於或等於 0，則函式會立即返回。

```python
sleep(1000)    # 等待 1 秒
sleep(0)       # 立即返回
```

> **注意：** 在 `spawn` 的任務內呼叫 `sleep` 時，底層的工作執行緒會被阻塞，但可透過 `cancel()` 中斷。

---

## cancel

**簽名：** `cancel(task: Task<T>) -> Unit`

請求取消已 spawn 的任務。任務會在下一個取消點（通道操作、`sleep`、`select`）被中斷。對已完成的任務呼叫 `cancel` 不會產生任何效果。

---

## is_cancelled

**簽名：** `is_cancelled() -> bool`

若當前任務已被 `cancel()` 取消，則回傳 `true`。在任務外呼叫或任務未被取消時回傳 `false`。

---

## task_group

**簽名：** `task_group(body: fn() -> Unit) -> Unit`

建立結構化並行性的作用域。在 lambda 內 `spawn` 的所有任務會在 lambda 結束時自動 join。若子任務拋出錯誤，剩餘子任務會被取消，錯誤會傳播到父級。

```python
fn compute(x: int) -> int:
    return x * 10

task_group(fn():
    t1 = spawn compute(3)
    t2 = spawn compute(4)
)
# 此處保證兩個任務都已完成
```

---

## env

**簽名：** `env(key: str) -> Option<str>` / `env(key: str, default: str) -> str`

回傳環境變數的值。單引數形式回傳 `Option<str>`（若已設定則為 `Some(value)`，未設定則為 `None`）。雙引數形式在變數未設定時回傳 `default`。

若專案根目錄（包含 `ry.toml` 的目錄）中存在 `.env` 檔案，啟動時會自動載入。現有的環境變數不會被 `.env` 的值覆蓋。

> **注意：** `.env` 檔案通常會包含密碼、金鑰等敏感資訊，請妥善保護並避免將其提交到版本控制系統（例如 Git）中。

```python
# 單引數: 回傳 Option<str>
path = env("PATH")
match path:
    case Some(v):
        print(v)
    case None:
        print("PATH not set")

# 雙引數: 帶預設值
port = env("PORT", "8080")
print(port)   # 若 PORT 未設定則為 "8080"
```

### `.env` 檔案格式

```env
# 註解
DATABASE_URL=postgres://localhost/mydb
API_KEY="secret-key-123"
EMPTY_VALUE=
QUOTED='single quoted'
```

### 環境專屬 `.env` 檔案

設定 `RY_ENV` 時，Ry 會依照以下優先順序載入環境專屬的 `.env` 檔案：

- 先載入 `.env.<環境名>`（例如 `RY_ENV=dev` 時載入 `.env.dev`）
- 再載入 `.env`（已由 `.env.<環境名>` 設定的值不會被覆蓋）
- `RY_ENV=prod` 時不載入任何 `.env` 檔案（安全考量）
- `RY_ENV` 未設定時僅載入 `.env`（向下相容）

環境模式的詳細資訊請參閱 [RY_ENV](packages.md#ry_env)。

---

## append

**簽名：** `append(list: List<T>, value: T)`

向串列末尾新增元素。此為就地修改操作——串列會被直接修改。也可使用 UFCS 記法。

```python
xs = [1, 2]
xs.append(3)
print(xs)   # [1, 2, 3]
```

---

## pop

**簽名：** `pop(list: List<T>) -> Option<T>`

移除並回傳串列的最後一個元素（`Option<T>`）。串列為空時回傳 `None`。也可使用 UFCS 記法。

```python
xs = [1, 2, 3]
v = xs.pop()
print(v)    # Some(3)
print(xs)   # [1, 2]
```

---

## reverse（串列）

**簽名：** `reverse(list: List<T>) -> List<T>`

傳回元素順序反轉的新串列。原始串列不會被修改。也適用於字串（請參閱[字串操作](builtins-string.md)）。也可使用 UFCS 記法。

```python
xs = [1, 2, 3]
ys = reverse(xs)
print(ys)   # [3, 2, 1]
print(xs)   # [1, 2, 3]（未修改）
```

---

## slice

**簽名：** `slice(list: List<T>, start: int, end: int) -> List<T>`

傳回從 `start`（含）到 `end`（不含）的新子串列。索引會被鉗制在有效範圍內（`0` 到 `length(list)`）。也可使用 UFCS 記法。

```python
xs = [1, 2, 3, 4, 5]
print(slice(xs, 1, 3))     # [2, 3]
print(slice(xs, 0, 100))   # [1, 2, 3, 4, 5]（鉗制）
```

---

## take

**簽名：** `take(list: List<T>, count: int) -> List<T>`

傳回包含前 `count` 個元素的新串列。若 `count` 超過串列長度，傳回整個串列的副本。若 `count <= 0`，傳回空串列。原始串列不會被修改。也可使用 UFCS 記法。

```python
xs = [1, 2, 3, 4, 5]
ys = xs.take(3)
print(ys)   # [1, 2, 3]
print(xs.take(10))   # [1, 2, 3, 4, 5]（鉗制）
print(xs.take(0))    # []
```

---

## tap

**簽名：** `tap(list: List<T>, fn: fn(T) -> R) -> List<T>`

對每個元素呼叫給定函式（忽略回傳值），然後傳回原始串列。適用於方法鏈中的除錯或插入副作用。也可使用 UFCS 記法。

```python
xs = [1, 2, 3]
ys = xs.tap(fn(x: int) => print(x)).map(fn(x: int) => x * 2)
# 輸出 1, 2, 3，然後 ys = [2, 4, 6]
```

---

## filter

**簽名:** `filter(list: List<T>, pred: fn(T) -> bool) -> List<T>`

傳回僅包含述詞回傳 `true` 的元素的新串列。原始串列不會被修改。也可使用 UFCS 記法。

```python
xs = [1, 2, 3, 4, 5]
ys = xs.filter(fn(x: int) => x > 3)
print(ys)   # [4, 5]
print(xs)   # [1, 2, 3, 4, 5]  （未修改）
```

---

## map

**簽名:** `map(list: List<T>, fn: fn(T) -> U) -> List<U>`

傳回將每個元素以給定函式轉換後的新串列。輸出元素型別可以與輸入不同。原始串列不會被修改。也可使用 UFCS 記法。

```python
xs = [1, 2, 3]
ys = xs.map(fn(x: int) => x * 2)
print(ys)   # [2, 4, 6]
```

---

## sort

**簽名:** `sort(list: List<T>) -> List<T>` / `sort(list: List<T>, comparator: fn(T, T) -> bool) -> List<T>`

傳回排序後的新串列。預設為升序。可提供自訂比較函式（第一引數應排在第二引數之前時回傳 `true`）。原始串列不會被修改。排序是**穩定的**（相等元素保持原始順序）。也可使用 UFCS 記法。

```python
xs = [3, 1, 2]
print(xs.sort())   # [1, 2, 3]

# 降序排序
desc = xs.sort(fn(a: int, b: int) => a > b)
print(desc)   # [3, 2, 1]
```

---

## sort!

**簽名:** `sort!(list: List<T>)` / `sort!(list: List<T>, comparator: fn(T, T) -> bool)`

就地排序串列。排序演算法與 `sort()` 相同，但修改原始串列而非建立新串列。也可使用 UFCS 記法。

```python
xs = [3, 1, 2]
xs.sort!()
print(xs)   # [1, 2, 3]
```

---

## reverse!

**簽名：** `reverse!(list: List<T>)`

就地反轉串列。也可使用 UFCS 記法。

```python
xs = [1, 2, 3]
xs.reverse!()
print(xs)   # [3, 2, 1]
```

---

## appended

**簽名：** `appended(list: List<T>, value: T) -> List<T>`

傳回新增元素後的新串列。原始串列不會被修改。也可使用 UFCS 記法。

```python
xs = [1, 2]
ys = xs.appended(3)
print(xs)   # [1, 2]（未修改）
print(ys)   # [1, 2, 3]
```

---

## append!

**簽名：** `append!(list: List<T>, value: T)`

`append()` 的別名。就地向串列末尾新增元素。為配合 `!` 命名慣例而提供。

---

## first

**簽名：** `first(list: List<T>) -> Option<T>`

傳回串列的第一個元素（`Option<T>`）。串列為空時回傳 `None`。

```python
print(first([10, 20, 30]))   # Some(10)
```

---

## last

**簽名：** `last(list: List<T>) -> Option<T>`

傳回串列的最後一個元素（`Option<T>`）。串列為空時回傳 `None`。

```python
print(last([10, 20, 30]))   # Some(30)
```

---

## get（映射）

**簽名：** `get(map: Map<K, V>, key: K) -> Option<V>` / `get(map: Map<K, V>, key: K, default: V) -> V`

兩引數形式回傳鍵的值（`Option<V>`）。三引數形式回傳鍵的值，若不存在則回傳預設值。

```python
m = {"a": 1, "b": 2}
print(get(m, "a"))       # Some(1)
print(get(m, "z"))       # None
print(get(m, "z", 0))   # 0
```
