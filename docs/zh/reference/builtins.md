[English](../../reference/builtins.md) | [日本語](../../ja/reference/builtins.md) | [简体中文](builtins.md)

# 内置函数参考

## 函数一览

### 核心

| 函数 | 说明 |
|------|------|
| `print()` / `print(expr1, expr2, ...)` | 将值输出到标准输出（以空格分隔） |
| `length(value)` | 返回列表、映射、集合的元素数量，或字符串的 UTF-8 字符数 |
| `range(n)` / `range(start, end)` / `range(start, end, step)` | 生成整数列表 |
| `exit(code)` | 以指定的退出码终止进程 |
| `arguments()` | 以 `List<str>` 返回命令行参数 |
| `available_parallelism()` | 返回运行时工作线程数（`int`） |
| `sleep(duration_ms)` | 暂停执行指定的毫秒数 |
| `env(key)` | 返回环境变量为 `Option<str>` |
| `env(key, default)` | 返回环境变量，若未设置则返回 `default` |
| `send(stream, data)` | 通过 `TcpStream` 或 `TlsStream` 发送 `List<u8>`，返回 `Result<int, Error>` |
| `receive(stream, max)` | 从 `TcpStream` 或 `TlsStream` 接收最多 `max` 字节，返回 `Result<List<u8>, Error>` |
| `close(handle)` | 关闭 `TcpStream`、`TlsStream` 或 `TcpListener` |
| `block_on(task)` | 阻塞当前线程直到 `Task<T>` 完成并返回其结果 |
| `to_str(value)` | 将值转换为其字符串表示。支持 `int`、`float`（整数值会附带 `.0` 输出）、`bool`、`str`、record、enum、tuple、`List`、`Map`、`Set`（嵌套容器如 `Map<str, List<int>>` 会递归格式化）、`Result`、`Option`、union 类型（格式化为活动变体）以及 function 值（输出为 `<closure>`）。集合内的字符串元素会用双引号包裹（例如 `["hello", "world"]`） |
| `type_of(expr)` | 返回 `expr` 的类型作为 `Type` 值。请参阅 [type_of](#type_of) |
| `fail()` / `fail(message)` | 将当前测试标记为失败（仅在 `ry test` 模式下可用） |

### Option

| 函数 | 说明 |
|------|------|
| `Some(expr)` | 构造 Option 类型的有值变体 |

### Result / Error

| 函数 | 说明 |
|------|------|
| `Ok(value)` | 构造 `Result<T, Error>` 的成功变体 |
| `Err(error)` | 构造 `Result<T, Error>` 的错误变体 |
| `Error(message)` | 创建带有消息的 `Error` 值 |
| `Error(message, code)` | 创建带有消息和错误码的 `Error` 值 |
| `result.and_then(closure)` | 若为 `Ok`，调用 `closure`（返回 `Result<U, E>`）；若为 `Err`，传播错误 |
| `result.map(closure)` | 若为 `Ok`，对值应用 `closure` 并将返回值包装在 `Ok` 中；若为 `Err`，传播错误 |

### 检查算术

| 函数 | 说明 |
|------|------|
| `checked_add(a, b)` | 无溢出时返回 `Ok(a + b)`，否则返回 `Err(Error("arithmetic overflow"))` |
| `checked_sub(a, b)` | 无溢出时返回 `Ok(a - b)`，否则返回 `Err(Error("arithmetic overflow"))` |
| `checked_mul(a, b)` | 无溢出时返回 `Ok(a * b)`，否则返回 `Err(Error("arithmetic overflow"))` |
| `saturating_add(a, b)` | 返回 `a + b`，溢出时钳制到 `int` 范围 |
| `saturating_sub(a, b)` | 返回 `a - b`，溢出时钳制到 `int` 范围 |
| `saturating_mul(a, b)` | 返回 `a * b`，溢出时钳制到 `int` 范围 |
| `wrapping_add(a, b)` | 返回溢出时回绕的 `a + b` |
| `wrapping_sub(a, b)` | 返回溢出时回绕的 `a - b` |
| `wrapping_mul(a, b)` | 返回溢出时回绕的 `a * b` |

### 集合操作

| 函数 | 说明 |
|------|------|
| `has_key(map, key)` | 返回映射中是否存在该键 |
| `add(set, value)` | 向集合添加元素（重复则忽略） |
| `remove(set, value)` | 从集合删除元素 |
| `append(list, value)` / `append!(list, value)` | 向列表末尾添加元素（就地修改） |
| `appended(list, value)` | 返回添加元素后的新列表（非破坏性） |
| `pop(list)` | 移除并返回列表的最后一个元素（`Option<T>`） |
| `reverse(list)` | 返回反转后的新列表（也适用于字符串） |
| `reverse!(list)` | 就地反转列表（破坏性） |
| `slice(list, start, end)` | 返回从 start 到 end 的新子列表 |
| `take(list, count)` | 返回包含前 count 个元素的新列表 |
| `tap(list, function)` | 对每个元素调用 function 以执行副作用，返回原始列表 |
| `filter(list, pred)` | 返回仅包含满足谓词的元素的新列表 |
| `map(list, function)` | 返回将每个元素转换后的新列表 |
| `sort(list)` / `sort(list, comp)` | 返回排序后的新列表（默认升序） |
| `sort!(list)` / `sort!(list, comp)` | 就地排序列表（破坏性） |
| `insert(list, i, val)` | 在索引 i 处插入元素 |
| `remove_at(list, i)` | 移除并返回索引 i 处的元素 |
| `items(map)` | 返回 (键, 值) 元组的列表 |
| `remove(map, key)` | 删除指定键的条目 |
| `get(map, key)` | 返回键的值（`Option<V>`） |
| `get(map, key, default)` | 返回键的值，若不存在则返回默认值 |
| `union(set, set)` | 返回两个集合的并集 |
| `intersection(set, set)` | 返回两个集合的交集 |
| `difference(set, set)` | 返回两个集合的差集 |
| `symmetric_difference(set, set)` | 返回两个集合的对称差 |
| `is_subset(set, set)` | 返回第一个集合是否为第二个的子集 |
| `is_superset(set, set)` | 返回第一个集合是否为第二个的超集 |
| `first(list)` | 返回第一个元素（`Option<T>`），列表为空时返回 `None` |
| `last(list)` | 返回最后一个元素（`Option<T>`），列表为空时返回 `None` |
| `remove(list, value)` | 从列表中移除第一个匹配的值 |
| `is_empty(list / map / set / str)` | 返回集合或字符串是否为空 |
| `distinct(list)` | 返回移除重复元素后的新列表 |
| `flatten(list)` | 返回将嵌套列表展开后的新列表 |
| `reduce(list, fn)` | 使用归约函数将列表归约为单个值 |
| `fold(list, init, fn)` | 使用初始累加器值折叠列表 |
| `any(list, pred)` | 如果任一元素满足谓词则返回 `true` |
| `all(list, pred)` | 如果所有元素都满足谓词则返回 `true` |
| `sum(list)` | 返回所有元素的总和 |
| `min(list)` | 返回最小的元素 |
| `max(list)` | 返回最大的元素 |
| `enumerate(list)` | 返回 `(index, value)` 元组的列表。也接受 `str`，每个 UTF-8 码位产生 `(int, str)` |
| `zip(list1, list2)` | 返回将两个列表的元素配对的 `(a, b)` 元组列表。任一（或两个）参数可以是 `str` |
| `keys(map)` | 以 `List<K>` 返回所有键 |
| `values(map)` | 以 `List<V>` 返回所有值 |
| `merge(map1, map2)` | 返回包含两个映射所有条目的新映射 |

### 迭代器

| 函数 | 说明 |
|------|------|
| `iter(collection)` | 从 List、Set 或 Map 创建惰性迭代器 |
| `next(iter)` | 返回下一个元素（`Option<T>`），耗尽时返回 `None` |
| `to_list(iter)` | 将迭代器剩余的所有元素收集到 `List<T>` |
| `filter(iter, pred)` | 返回只产出满足谓词的元素的惰性迭代器 |
| `map(iter, function)` | 返回转换每个元素的惰性迭代器 |
| `take(iter, count)` | 返回最多产出 count 个元素的惰性迭代器 |

### [字符串操作](builtins-string.md)

| 函数 | 说明 |
|------|------|
| `contains(string, substring)` | 是否包含子字符串 |
| `starts_with(string, prefix)` | 是否以前缀开头 |
| `ends_with(string, suffix)` | 是否以后缀结尾 |
| `find(string, substring)` | 子字符串的字符位置（`Option<int>`） |
| `byte_len(string)` | 返回字符串的字节长度 |
| `substring(string, start, end)` | 提取子字符串 |
| `char_at(string, i)` | 获取指定位置的字符 |
| `replace(string, old, new)` | 替换所有出现的子字符串 |
| `to_upper(string)` / `to_lower(string)` | 大小写转换 |
| `trim(string)` / `trim_start(string)` / `trim_end(string)` | 去除空白 |
| `repeat(string, count)` | 将字符串重复 n 次 |
| `reverse(string)` | 反转字符串 |
| `split(string, delimiter)` | 分割字符串并返回列表 |
| `join(list, sep)` | 以分隔符连接列表中的字符串 |
| `to_int(s)` / `to_float(s)` / `to_str(v)` | 类型转换（`to_int` 与 `to_float` 返回 `Result<T, Error>`） |

-> 详细请参阅 **[字符串操作函数参考](builtins-string.md)**

---

## print

**签名：** `print()` / `print(expr1, expr2, ...)`

将一个或多个值输出到标准输出，以空格分隔。末尾会追加换行。不带参数调用时仅输出换行。

| 类型 | 输出格式 |
|----|---------|
| `int` | `%ld` |
| `float` | `%g`，整数值会附带 `.0`（例如 `3.0`、`0.0`） |
| `bool` | `true` / `false` |
| `str` | `%s` |
| `Result` (Ok) | `Ok(value)` |
| `Result` (Err) | `Err(value)` |
| `Option` (Some) | `Some(value)` |
| `Option` (None) | `None` |
| `list` | `[elem1, elem2, ...]` |
| `map` | `{key1: val1, key2: val2, ...}` |
| `set` | `{elem1, elem2, ...}` |
| `tuple` | `(elem1, elem2, ...)` |
| `enum` | 变体名称（例如：`Red`） |
| `record` | `RecordName(field: val, ...)` |
| function 值（closure / lambda） | `<closure>` |
| union | 格式化为活动变体的类型 |

整数值的 `float` 总是会附带 `.0` 输出，以便与 `int` 视觉上区分。嵌套集合（例如 `Map<str, List<int>>`）会使用内部元素的格式化器递归格式化。底层类型为 `List`、`Map` 或 `Set` 的 union 变体会格式化为该集合；底层类型为 function 值的变体会格式化为 `<closure>`。

```python
print(42)          # 42
print(3.14)        # 3.14
print(3.0)         # 3.0         (整数值 float 保留 .0)
print(0.0)         # 0.0
print(true)        # true
print("hello")     # hello
print(Ok(42))      # Ok(42)
print(Err(Error("fail")))  # Err(Error: fail (code: 0))
print(Some(1))     # Some(1)
print(None)        # None
print([1, 2, 3])   # [1, 2, 3]
print({"a": 1})    # {"a": 1}
print({1, 2, 3})   # {1, 2, 3}
print((1, "hello"))  # (1, "hello")

# 嵌套集合
m: Map<str, List<int>> = {"a": [1, 2, 3]}
print(m)           # {"a": [1, 2, 3]}

# 集合类型的 union 变体
x: int | List<int> = [1, 2, 3]
print(x)           # [1, 2, 3]

# Function 值
f = (x: int) => x * 2
print(f)           # <closure>

# 多个参数（以空格分隔）
print(1, 2, 3)             # 1 2 3
print("hello", "world")   # hello world
print(1, "hello", true)   # 1 hello true
print()                    # （空行）
```

---

## Some

**签名：** `Some(expr) -> Option<T>`

构造 Option 类型的有值变体。

```python
x: Option<int> = Some(42)
print(x)   # Some(42)
```

---

## length

**签名：** `length(x: List<T> | Map<K, V> | Set<T> | str) -> int`

返回列表、映射、集合的元素数量，或字符串的 UTF-8 字符数。如需获取字节长度，请使用 `byte_len()`。

```python
print(length([1, 2, 3]))         # 3
print(length({"a": 1, "b": 2})) # 2
print(length({1, 2, 3}))         # 3
print(length("hello"))           # 5
print(length("あいう"))           # 3（UTF-8 字符数）
```

---

## has_key

**签名：** `has_key(m: Map<K, V>, key: K) -> bool`

返回映射中是否存在指定的键。也可使用 UFCS 记法。

```python
m = {"a": 1, "b": 2}
print(has_key(m, "a"))    # true
print(m.has_key("z"))     # false（UFCS）
```

---

## add

**签名：** `add(s: Set<T>, value: T)`

向集合添加元素。若元素已存在则不做任何操作。也可使用 UFCS 记法。

```python
s = {1, 2, 3}
s.add(4)          # UFCS
add(s, 5)         # 普通调用
s.add(1)          # 已存在，因此忽略
print(length(s))     # 5
```

---

## remove

**签名：** `remove(s: Set<T>, value: T)`

从集合删除元素。也可使用 UFCS 记法。

```python
s = {1, 2, 3}
s.remove(2)       # UFCS
print(2 in s)     # false
```

---

## range

**签名：** `range(n: int) -> List<int>` / `range(start: int, end: int) -> List<int>` / `range(start: int, end: int, step: int) -> List<int>`

生成整数列表。

| 形式 | 生成的值 |
|------|------------|
| `range(n)` | `[0, 1, ..., n-1]` |
| `range(start, end)` | `[start, start+1, ..., end-1]` |
| `range(start, end, step)` | `[start, start+step, start+2*step, ...]`（不包含 `end`） |

- `step > 0` 时，从 `start` 向 `end` 递增生成。
- `step < 0` 时，从 `start` 向 `end` 递减生成。
- `step == 0` 时，会产生运行时错误。
- 如果范围为空（例如 `range(0, 10, -1)`），返回空列表。

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

**签名：** `exit(code: int)`

以指定的退出码立即终止进程。`exit()` 之后的语句会被编译为不可达块，LLVM 会在优化期间将其移除，因此它们永远不会运行：

```python
exit(0)        # 正常终止
exit(1)        # 错误终止

print("a")
exit(0)
print("b")     # 永远不会输出 — exit 之后不可达
```

相同的处理也适用于 `return`、`break` 和 `continue` — 任何发散控制流语句之后的代码都会被静默移除。

---

## arguments

**签名：** `arguments() -> List<str>`

以字符串列表的形式返回传递给脚本的命令行参数。不包含解释器名称或脚本文件名——仅包含脚本路径之后的参数。

```python
# 运行：ry script.ry hello world
a = arguments()
print(length(a))    # 2
print(a[0])      # hello
print(a[1])      # world

for x in arguments():
    print(x)
```

---

## sleep

**签名：** `sleep(duration_ms: int) -> Unit`

暂停当前线程的执行指定的毫秒数。若 `duration_ms` 为 0 或负数，则函数立即返回。

```python
sleep(1000)    # 等待 1 秒
sleep(0)       # 立即返回
```

---

## env

**签名：** `env(key: str) -> Option<str>` / `env(key: str, default: str) -> str`

返回环境变量的值。单参数形式返回 `Option<str>`（若已设置则为 `Some(value)`，未设置则为 `None`）。双参数形式在变量未设置时返回 `default`。

若项目根目录（包含 `package.toml` 的目录）中存在 `.env` 文件，启动时会自动载入到进程环境中。现有的环境变量不会被 `.env` 的值覆盖。

> **安全提示：** `.env` 文件通常包含密钥（API 密钥、数据库密码、令牌等）。请**不要**将 `.env` 提交到版本控制系统（添加到 `.gitignore` 或类似文件中），并将其内容视为敏感配置。

```python
# 单参数形式：返回 Option<str>
path = env("PATH")
case path:
    Some(v):
        print(v)
    None:
        print("PATH not set")

# 双参数形式：带默认值返回 str
port = env("PORT", "8080")
print(port)   # 若 PORT 未设置则为 "8080"
```

### `.env` 文件格式

```env
# 注释以 # 开头
DATABASE_URL=postgres://localhost/mydb
API_KEY="secret-key-123"
EMPTY_VALUE=
QUOTED='single quoted'
```

### 环境专属 `.env` 文件

设置 `RY_ENV` 时，Ry 会按照以下优先顺序载入环境专属的 `.env` 文件：

- 先载入 `.env.<env>`（例如 `RY_ENV=dev` 时载入 `.env.dev`）
- 再载入 `.env`（已由 `.env.<env>` 设置的值不会被覆盖）
- `RY_ENV=prod` 时不载入任何 `.env` 文件（安全考虑）
- `RY_ENV` 未设置时仅载入 `.env`（向后兼容）

环境模式的详细信息请参阅 [RY_ENV](packages.md#ry_env)。

---

## append

**签名：** `append(list: List<T>, value: T)`

向列表末尾添加元素。此为就地修改操作——列表会被直接修改。也可使用 UFCS 记法。

```python
xs = [1, 2]
xs.append(3)
print(xs)   # [1, 2, 3]
```

---

## pop

**签名：** `pop(list: List<T>) -> Option<T>`

移除并返回列表的最后一个元素（`Option<T>`）。列表为空时返回 `None`。也可使用 UFCS 记法。

```python
xs = [1, 2, 3]
v = xs.pop()
print(v)    # Some(3)
print(xs)   # [1, 2]
```

---

## reverse（列表）

**签名：** `reverse(list: List<T>) -> List<T>`

返回元素顺序反转的新列表。原始列表不会被修改。也适用于字符串（请参阅[字符串操作](builtins-string.md)）。也可使用 UFCS 记法。

```python
xs = [1, 2, 3]
ys = reverse(xs)
print(ys)   # [3, 2, 1]
print(xs)   # [1, 2, 3]（未修改）
```

---

## slice

**签名：** `slice(list: List<T>, start: int, end: int) -> List<T>`

返回从 `start`（含）到 `end`（不含）的新子列表。索引会被钳制在有效范围内（`0` 到 `length(list)`）。也可使用 UFCS 记法。

```python
xs = [1, 2, 3, 4, 5]
print(slice(xs, 1, 3))     # [2, 3]
print(slice(xs, 0, 100))   # [1, 2, 3, 4, 5]（钳制）
```

---

## take

**签名：** `take(list: List<T>, count: int) -> List<T>`

返回包含前 `count` 个元素的新列表。若 `count` 超过列表长度，返回整个列表的副本。若 `count <= 0`，返回空列表。原始列表不会被修改。也可使用 UFCS 记法。

```python
xs = [1, 2, 3, 4, 5]
ys = xs.take(3)
print(ys)   # [1, 2, 3]
print(xs.take(10))   # [1, 2, 3, 4, 5]（钳制）
print(xs.take(0))    # []
```

---

## tap

**签名：** `tap(list: List<T>, function: function(T) -> R) -> List<T>`

对每个元素调用给定函数（忽略返回值），然后返回原始列表。适用于方法链中的调试或插入副作用。也可使用 UFCS 记法。

```python
xs = [1, 2, 3]
ys = xs.tap((x: int) => print(x)).map((x: int) => x * 2)
# 输出 1, 2, 3，然后 ys = [2, 4, 6]
```

---

## filter

**签名：** `filter(list: List<T>, pred: function(T) -> bool) -> List<T>`

返回仅包含谓词返回 `true` 的元素的新列表。原始列表不会被修改。也可使用 UFCS 记法。

```python
xs = [1, 2, 3, 4, 5]
ys = xs.filter((x: int) => x > 3)
print(ys)   # [4, 5]
print(xs)   # [1, 2, 3, 4, 5]（未修改）
```

---

## map

**签名：** `map(list: List<T>, function: function(T) -> U) -> List<U>`

返回将每个元素以给定函数转换后的新列表。输出元素类型可以与输入不同。原始列表不会被修改。也可使用 UFCS 记法。

```python
xs = [1, 2, 3]
ys = xs.map((x: int) => x * 2)
print(ys)   # [2, 4, 6]
```

---

## sort

**签名：** `sort(list: List<T>) -> List<T>` / `sort(list: List<T>, comp: function(T, T) -> bool) -> List<T>`

返回排序后的新列表。默认为升序。可提供自定义比较函数（第一参数应排在第二参数之前时返回 `true`）。原始列表不会被修改。排序是**稳定的**（相等元素保持原始顺序）。也可使用 UFCS 记法。

```python
xs = [3, 1, 2]
print(xs.sort())   # [1, 2, 3]

# 降序排序
desc = xs.sort((a: int, b: int) => a > b)
print(desc)   # [3, 2, 1]
```

---

## sort!

**签名：** `sort!(list: List<T>)` / `sort!(list: List<T>, comp: function(T, T) -> bool)`

就地排序列表。排序算法与 `sort()` 相同，但修改原始列表而非创建新列表。也可使用 UFCS 记法。

```python
xs = [3, 1, 2]
xs.sort!()
print(xs)   # [1, 2, 3]
```

---

## reverse!

**签名：** `reverse!(list: List<T>)`

就地反转列表。也可使用 UFCS 记法。

```python
xs = [1, 2, 3]
xs.reverse!()
print(xs)   # [3, 2, 1]
```

---

## appended

**签名：** `appended(list: List<T>, value: T) -> List<T>`

返回添加元素后的新列表。原始列表不会被修改。也可使用 UFCS 记法。

```python
xs = [1, 2]
ys = xs.appended(3)
print(xs)   # [1, 2]（未修改）
print(ys)   # [1, 2, 3]
```

---

## append!

**签名：** `append!(list: List<T>, value: T)`

`append()` 的别名。就地向列表末尾添加元素。为配合 `!` 命名约定而提供。

---

## first

**签名：** `first(list: List<T>) -> Option<T>`

返回列表的第一个元素（`Option<T>`）。列表为空时返回 `None`。

```python
print(first([10, 20, 30]))   # Some(10)
```

---

## last

**签名：** `last(list: List<T>) -> Option<T>`

返回列表的最后一个元素（`Option<T>`）。列表为空时返回 `None`。

```python
print(last([10, 20, 30]))   # Some(30)
```

---

## get（映射）

**签名：** `get(map: Map<K, V>, key: K) -> Option<V>` / `get(map: Map<K, V>, key: K, default: V) -> V`

双参数形式返回键的值（`Option<V>`）。三参数形式返回键的值，若不存在则返回默认值。

```python
m = {"a": 1, "b": 2}
print(get(m, "a"))       # Some(1)
print(get(m, "z"))       # None
print(get(m, "z", 0))   # 0
```

---

## iter

**签名：** `iter(collection: List<T> | Set<T>) -> Iterator<T>` / `iter(collection: Map<K, V>) -> Iterator<(K, V)>`

从集合创建惰性迭代器。迭代器不复制数据；它引用原始集合。也可使用 UFCS 记法。

- 对于 `List<T>` 和 `Set<T>`，元素类型为 `T`。
- 对于 `Map<K, V>`，元素类型为元组 `(K, V)`。

```python
xs = [1, 2, 3]
it = xs.iter()           # Iterator<int>
ys = it.to_list()        # [1, 2, 3]

m = {"a": 1, "b": 2}
for k, v in m.iter():        # Iterator<(str, int)>
    print(k)
```

---

## next

**签名：** `next(iter: Iterator<T>) -> Option<T>`

返回迭代器的下一个元素（`Option<T>`）。当迭代器耗尽时返回 `None`。每次调用时迭代器会推进其内部状态。也可使用 UFCS 记法。

```python
it = [10, 20].iter()
print(it.next())   # Some(10)
print(it.next())   # Some(20)
print(it.next())   # None
```

---

## to_list

**签名：** `to_list(iter: Iterator<T>) -> List<T>`

将迭代器剩余的所有元素收集到新列表中。也可使用 UFCS 记法。

```python
xs = [1, 2, 3, 4, 5]
ys = xs.iter().filter((x: int) => x > 2).to_list()
print(ys)   # [3, 4, 5]
```

---

## type_of

**签名：** `type_of(expr: T) -> Type`

返回一个表达式的类型，作为 [`Type`](types.md#type) 值。每个不同的类型定义（基本类型、集合、record、enum、`Option`、`Result`、function 等）在编译时都会获得唯一的标识，因此可以使用 `==` 比较 `type_of` 值，以检查两个表达式是否共享同一类型。

- 参数会因副作用被求值，但只使用其静态类型。
- 通过 `print` 或 `to_str` 输出 `Type` 值会产生人类可读的名称（例如 `"int"`、`"List"`、`"Point"`）。
- 具有相同规范类型的两个表达式返回相等的 `Type` 值；不同的 record（或恰好同名的 record 与 enum）始终可区分。
- 字面 `none` 报告为 `"None"`。一个有类型的 `Option<T>` 值（无论是通过 `Some(...)` 构造还是从 `none` 赋值）报告为 `"Option"`。

```ry
record Point:
  x: int
  y: int

enum Color:
  Red
  Green
  Blue

print(to_str(type_of(42)))          # int
print(to_str(type_of(3.14)))        # float
print(to_str(type_of("hello")))     # str
print(to_str(type_of([1, 2, 3])))   # List
print(to_str(type_of({"a": 1})))    # Map
print(to_str(type_of({1, 2})))      # Set

p = Point(1, 2)
print(to_str(type_of(p)))           # Point

c = Color::Red
print(to_str(type_of(c)))           # Color

# 标识比较
print(type_of(42) == type_of(100))  # true
print(type_of(42) == type_of(3.14)) # false
print(type_of(p) != type_of(c))     # true

# 低层数值类型与 `int` 区分
x: i32 = 1
print(to_str(type_of(x)))           # i32
print(type_of(x) == type_of(42))    # false

# type_of 是反射性的：Type 值的类型是 Type
print(to_str(type_of(type_of(42)))) # Type
```

### `type_of` 返回的类型类别

| 输入 | `to_str(type_of(...))` |
|---|---|
| `42` | `int` |
| `3.14` | `float` |
| `true` / `false` | `bool` |
| `"hello"` | `str` |
| `[1, 2]` | `List` |
| `{"a": 1}` | `Map` |
| `{1, 2}` | `Set` |
| `x: i32 = 1` | `i32`（同样适用于 `u8`、`i16`、…、`f32`） |
| record 值 | record 名称（例如 `Point`） |
| enum 值 | enum 名称（例如 `Color`） |
| `none` 字面 | `None` |
| `Some(1)` | `Option` |
| `x: Option<int> = none` | `Option` |
| `Ok(1)` / `Err(e)` | `Result` |
| lambda / closure | `function` |
| `type_of(x)` | `Type` |

> 字面 `none` 报告为 `"None"` 以与有类型的 `Option` 值区分。任何 `Option<T>` 容器 — 无论是通过 `Some(...)` 构造，还是通过将 `none` 赋值给 `Option<T>` 类型的绑定 — 都报告为 `"Option"`。
