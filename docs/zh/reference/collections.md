[English](../../reference/collections.md) | [日本語](../../ja/reference/collections.md) | [简体中文](collections.md)

# 集合参考（元组、列表、映射、集合）

## 元组

### 概述

固定长度、异质类型的值组合。以 LLVM literal StructType 实现，是栈上的值类型。

### 语法

```python
t = (42,)                      # 单元素元组（需要尾随逗号）
t = (1, 3.14)
t: (int, float) = (1, 3.14)
```

### 类型注解

```python
single: (int,) = (42,)                     # 单元素需要尾随逗号
pair: (int, str) = (42, "hello")
triple: (int, float, bool) = (1, 2.0, true)
```

### 比较

元组通过逐元素比较支持 `==` 和 `!=`。

```python
print((1, 2) == (1, 2))    # true
print((1, 2) != (3, 4))    # true
print(("a", 1) == ("b", 1))  # false
```

### 元素访问

使用 `.0`、`.1`、... 的数值索引访问。

```python
t = (10, 3.14)
print(t.0)   # 10
print(t.1)   # 3.14
```

### 函数返回值

```python
function swap(a: int, b: int) -> (int, int):
    return (b, a)

result = swap(1, 2)
print(result.0)   # 2
print(result.1)   # 1
```

### 约束与错误

| 约束 | 详细 |
|------|------|
| 超出范围的索引 | 编译错误 |
| 比较运算符 | 仅支持 `==` 和 `!=`；不支持 `<`、`<=`、`>`、`>=` |

---

## 列表

### 概述

相同类型的可变长度序列。分配在堆上。

### 语法

```python
xs = [1, 2, 3]
xs: List<int> = [1, 2, 3]
```

### 空列表

空列表需要类型注解以确定元素类型：

```python
xs: List<int> = []
xs: List<str> = []
```

### 连接

列表可以使用 `+` 和 `+=` 进行连接：

```python
a = [1, 2, 3]
b = [4, 5, 6]
c = a + b       # [1, 2, 3, 4, 5, 6]
a += [7, 8]     # a 现在是 [1, 2, 3, 7, 8]
```

两个操作数必须具有相同的元素类型。

### 支持的元素类型

`int`, `float`, `bool`, `str`

### 索引访问

```python
xs = [1, 2, 3]
print(xs[0])   # 1
print(xs[2])   # 3
```

负数索引从末尾开始计算（Python 风格）：

```python
xs = [10, 20, 30]
print(xs[-1])   # 30（最后一个元素）
print(xs[-2])   # 20
print(xs[-3])   # 10
```

超出范围的访问（包括超出列表长度的负数索引）会产生运行时错误。

### 索引赋值

```python
xs = [1, 2, 3]
xs[0] = 99
print(xs[0])   # 99
xs[-1] = 42    # 赋值给最后一个元素
print(xs[2])   # 42
```

### length

```python
xs = [1, 2, 3]
print(length(xs))   # 3
```

### print

```python
xs = [1, 2, 3]
print(xs)   # [1, 2, 3]
```

### for 遍历

```python
xs = [10, 20, 30]
for x in xs:
    print(x)
# 10
# 20
# 30
```

### append

向列表末尾添加元素。此为就地修改操作。

```python
xs = [1, 2]
xs.append(3)
print(xs)   # [1, 2, 3]
```

### pop

移除并返回列表的最后一个元素。列表为空时返回 `None`。

```python
xs = [1, 2, 3]
v = xs.pop()
print(v)    # Some(3)
print(xs)   # [1, 2]
```

### reverse

返回元素顺序反转的新列表。原始列表不会被修改。也适用于字符串。

```python
xs = [1, 2, 3]
print(reverse(xs))   # [3, 2, 1]
print(xs)            # [1, 2, 3] (unchanged)
```

### slice

返回从 `start`（含）到 `end`（不含）的新子列表。索引会被钳制在有效范围内。

```python
xs = [1, 2, 3, 4, 5]
print(slice(xs, 1, 3))     # [2, 3]
print(slice(xs, 0, 100))   # [1, 2, 3, 4, 5] (clamped)
```

### take

返回包含前 `count` 个元素的新列表。若 `count` 超过列表长度，返回整个列表的副本。若 `count <= 0`，返回空列表。原始列表不会被修改。

```python
xs = [1, 2, 3, 4, 5]
ys = xs.take(3)
print(ys)   # [1, 2, 3]
print(xs.take(10))   # [1, 2, 3, 4, 5] (clamped)
print(xs.take(0))    # []
```

### tap

对每个元素调用给定函数（忽略返回值），然后返回原始列表。适用于方法链中的调试或插入副作用。

```python
xs = [1, 2, 3]
ys = xs.tap((x: int) => print(x)).map((x: int) => x * 2)
# prints 1, 2, 3, then ys = [2, 4, 6]
```

### filter

返回仅包含满足谓词的元素的新列表。原始列表不会被修改。

```python
xs = [1, 2, 3, 4, 5]
ys = xs.filter((x: int) => x > 3)
print(ys)   # [4, 5]
```

### map

返回将每个元素以给定函数转换后的新列表。输出元素类型可以与输入不同。原始列表不会被修改。

```python
xs = [1, 2, 3]
ys = xs.map((x: int) => x * 2)
print(ys)   # [2, 4, 6]
```

### sort

返回排序后的新列表。默认为升序。可提供自定义比较函数。原始列表不会被修改。排序是**稳定的**（相等元素保持原始顺序）。内部使用 TimSort。

```python
xs = [3, 1, 2]
print(xs.sort())   # [1, 2, 3]

# 使用比较函数降序排序
desc = xs.sort((a: int, b: int) => a > b)
print(desc)   # [3, 2, 1]
```

### filter、map、sort 的链式调用

这些函数返回新列表，因此可通过 UFCS 进行链式调用。

```python
xs = [5, 3, 1, 4, 2]
result = xs.filter((x: int) => x > 1).map((x: int) => x * 10).sort()
print(result)   # [20, 30, 40, 50]
```

### reduce

使用累加函数将列表归约为单个值，以第一个元素作为初始值。

```python
xs = [1, 2, 3, 4, 5]
total = reduce(xs, (a: int, b: int) => a + b)
print(total)   # 15
```

### fold

使用明确的初始值和累加函数将列表折叠为单个值。

```python
xs = [1, 2, 3, 4, 5]
total = fold(xs, 0, (a: int, b: int) => a + b)
print(total)   # 15
```

### any

如果至少有一个元素满足谓词，则返回 `true`。

```python
xs = [1, 2, 3, 4, 5]
print(any(xs, (x: int) => x > 4))   # true
print(any(xs, (x: int) => x > 9))   # false
```

### all

如果所有元素都满足谓词，则返回 `true`。

```python
xs = [2, 4, 6]
print(all(xs, (x: int) => x > 0))   # true
print(all(xs, (x: int) => x > 3))   # false
```

### sum

返回所有元素的总和。

```python
xs = [1, 2, 3, 4, 5]
print(sum(xs))   # 15
```

### min

返回最小的元素。

```python
xs = [3, 1, 4, 1, 5]
print(min(xs))   # 1
```

### max

返回最大的元素。

```python
xs = [3, 1, 4, 1, 5]
print(max(xs))   # 5
```

### first

返回第一个元素。列表为空时返回 `None`。

```python
xs = [10, 20, 30]
print(first(xs))   # Some(10)
```

### last

返回最后一个元素。列表为空时返回 `None`。

```python
xs = [10, 20, 30]
print(last(xs))   # Some(30)
```

### is_empty

如果列表没有元素则返回 `true`。

```python
xs = [1, 2, 3]
print(is_empty(xs))   # false
print(is_empty([]))   # true (requires type annotation in practice)
```

### enumerate

返回 `(index, element)` 元组的列表。

```python
xs = [10, 20, 30]
pairs = enumerate(xs)
# pairs = [(0, 10), (1, 20), (2, 30)]

# for 循环中的元组解构
for i, x in enumerate(xs):
    print(f"{i}: {x}")    # 0: 10, 1: 20, 2: 30
```

### zip

将两个列表合并为 `(elem1, elem2)` 元组的列表。结果长度等于较短的列表。

```python
xs = [1, 2, 3]
ys = ["a", "b", "c"]
pairs = zip(xs, ys)
# pairs = [(1, "a"), (2, "b"), (3, "c")]

# for 循环中的元组解构
for a, b in zip(xs, ys):
    print(f"{a}: {b}")    # 1: a, 2: b, 3: c
```

### insert

在指定索引处插入元素。该索引及之后的元素向右移动。

```python
xs = [1, 2, 3]
insert(xs, 1, 99)
print(xs)   # [1, 99, 2, 3]
```

### remove_at

移除并返回指定索引处的元素。该索引之后的元素向左移动。

```python
xs = [1, 2, 3, 4]
v = remove_at(xs, 1)
print(v)    # 2
print(xs)   # [1, 3, 4]
```

### remove

从列表中移除第一个匹配的指定值。若找不到该值，则不做任何操作。这是一个可变操作。

```python
xs = [1, 2, 3, 2, 4]
remove(xs, 2)
print(xs)   # [1, 3, 2, 4]
```

### distinct

返回一个移除重复元素的新列表。保持原始顺序（保留第一次出现）。原始列表不会被修改。

```python
xs = [1, 2, 3, 2, 1, 4]
print(distinct(xs))   # [1, 2, 3, 4]
print(xs)             # [1, 2, 3, 2, 1, 4] (unchanged)
```

### flatten

将嵌套列表（列表的列表）展开一层。返回新列表。原始列表不会被修改。

```python
xs = [[1, 2], [3, 4]]
print(flatten(xs))   # [1, 2, 3, 4]
print(xs)            # [[1, 2], [3, 4]] (unchanged)
```

### 操作复杂度

| 操作 | 复杂度 |
|------|--------|
| `xs[i]` 索引访问 | O(1) |
| `append` / `append!` | 均摊 O(1) |
| `pop` | O(1) |
| `first`, `last` | O(1) |
| `insert`, `remove_at` | O(n) |
| `sort` / `sort!` | O(n log n) |
| `take` | O(n) |
| `tap` | O(n) |
| `filter`, `map`, `reduce`, `fold` | O(n) |
| `reverse` / `reverse!` | O(n) |
| `distinct` | O(n) |
| `length` | O(1) |

### 约束与错误

| 约束 | 详细 |
|------|------|
| 所有元素必须为相同类型 | 混合不同类型会产生编译错误 |
| 空列表 `[]` | 需要类型注解（例如 `xs: List<int> = []`） |
| 超出范围的访问 | 运行时错误（exit(1)） |

---

## Copy-on-Write (CoW) 语义

所有集合类型（List、Map、Set）在 ARC 管理下使用 **Copy-on-Write** 语义。这意味着：

- **赋值共享数据**：`b = a` 不会复制集合——两个变量引用相同的数据。引用计数递增。
- **修改触发复制**：当共享的集合被修改（如 `append`、`remove`、索引赋值）时，在修改前会自动创建深拷贝。只有修改方承担复制成本。
- **唯一所有者就地修改**：当集合只有一个引用（`strong_count == 1`）时，修改操作就地执行，零复制开销。

```python
a = [1, 2, 3]       # strong_count = 1
b = a                # strong_count = 2（共享）
append(b, 4)         # strong_count > 1 → 深拷贝 b，然后修改
                     # a = [1, 2, 3]  (strong_count = 1)
                     # b = [1, 2, 3, 4]  (strong_count = 1, 新分配)

c = [10, 20]         # strong_count = 1
append(c, 30)        # strong_count == 1 → 就地修改（无复制）
```

### 触发 CoW 的操作

| 类型 | 可变操作 |
|------|-------------------|
| **List** | `append()`、`pop()`、`insert()`、`remove()`、`remove_at()`、`sort!()`、`reverse!()`、索引赋值（`xs[i] = val`） |
| **Map** | `remove()`、索引赋值（`m[key] = val`） |
| **Set** | `add()`、`remove()` |

非可变操作（`appended`、`slice`、`take`、`filter`、`map`、`sort`、`reverse`、`get`、`items` 等）创建新集合，不触发 CoW。

---

## 映射

### 概述

键与值的对应表。分配在堆上。

### 语法

```python
m = {"a": 1, "b": 2}
m: Map<str, int> = {"a": 1, "b": 2}
```

### 键访问

```python
m = {"a": 1, "b": 2}
print(m["a"])   # 1
```

### 插入与更新

```python
m = {"a": 1}
m["b"] = 2     # 插入新条目
m["a"] = 99    # 更新已有条目
```

### length

```python
m = {"a": 1, "b": 2, "c": 3}
print(length(m))   # 3
```

### print

```python
m = {"a": 1, "b": 2}
print(m)   # {a: 1, b: 2}
```

### has_key

```python
m = {"a": 1, "b": 2}
print(m.has_key("a"))   # true
print(m.has_key("z"))   # false
```

### keys

返回映射中所有键的列表。

```python
m = {"a": 1, "b": 2, "c": 3}
print(keys(m))   # ["a", "b", "c"]
```

### values

返回映射中所有值的列表。

```python
m = {"a": 1, "b": 2, "c": 3}
print(values(m))   # [1, 2, 3]
```

### items

返回映射中所有条目的 `(key, value)` 元组列表。

```python
m = {"a": 1, "b": 2}
pairs = items(m)
# pairs = [("a", 1), ("b", 2)]
```

### remove (Map)

从映射中删除指定键的条目。若键不存在则不做任何操作。

```python
m = {"a": 1, "b": 2}
remove(m, "a")
print(m)   # {b: 2}
```

### get

返回指定键的值，若键不存在则返回默认值。

```python
m = {"a": 1, "b": 2}
print(get(m, "a", 0))   # 1
print(get(m, "z", 0))   # 0
```

### merge

返回一个合并两个映射的新映射。当键重复时，第二个映射的值优先。原始映射不会被修改。

```python
m1 = {"a": 1, "b": 2}
m2 = {"b": 99, "c": 3}
m3 = merge(m1, m2)
print(m3["a"])   # 1
print(m3["b"])   # 99
print(m3["c"])   # 3
```

### 约束与错误

| 约束 | 详细 |
|------|------|
| 所有键必须为相同类型 | 混合不同类型的键会产生编译错误 |
| 所有值必须为相同类型 | 混合不同类型的值会产生编译错误 |
| 空映射 | 需要类型注解（例如 `m: Map<str, int> = {}`） |
| 访问不存在的键 | 运行时错误（exit(1)） |
| 键查找 | 哈希表（平均 O(1)） |
| 容量溢出 | 自动扩展为 2 倍 |

---

## 集合

### 概述

持有相同类型的元素且不重复的集合。分配在堆上。

### 语法

```python
s = {1, 2, 3}
s: Set<int> = {1, 2, 3}
```

### 支持的元素类型

`int`, `float`, `bool`, `str`

### in 运算符（成员检查）

```python
s = {1, 2, 3}
print(2 in s)   # true
print(5 in s)   # false
```

### length

```python
s = {1, 2, 3}
print(length(s))   # 3
```

### print

```python
s = {1, 2, 3}
print(s)   # {1, 2, 3}
```

### add（添加元素）

添加重复的元素时会被忽略。

```python
s = {1, 2, 3}
s.add(4)         # 添加
s.add(1)         # 已存在，因此忽略
print(length(s))    # 4
```

### remove（删除元素）

```python
s = {1, 2, 3}
s.remove(2)
print(2 in s)   # false
```

### for 遍历

```python
s = {10, 20, 30}
for x in s:
    print(x)
```

### 空集合

空集合需要类型注解。

```python
s: Set<int> = {}
```

### 函数参数

```python
function has_value(s: Set<int>, v: int) -> bool:
    return v in s
```

### union

返回包含两个集合所有元素的新集合。

```python
a = {1, 2, 3}
b = {3, 4, 5}
print(union(a, b))   # {1, 2, 3, 4, 5}
```

### intersection

返回仅包含两个集合中都存在的元素的新集合。

```python
a = {1, 2, 3}
b = {2, 3, 4}
print(intersection(a, b))   # {2, 3}
```

### difference

返回包含在第一个集合中但不在第二个集合中的元素的新集合。

```python
a = {1, 2, 3}
b = {2, 3, 4}
print(difference(a, b))   # {1}
```

### symmetric_difference

返回包含在任一集合中但不同时在两个集合中的元素的新集合。

```python
a = {1, 2, 3}
b = {2, 3, 4}
print(symmetric_difference(a, b))   # {1, 4}
```

### is_subset

如果第一个集合的所有元素都包含在第二个集合中，则返回 `true`。

```python
a = {1, 2}
b = {1, 2, 3}
print(is_subset(a, b))   # true
print(is_subset(b, a))   # false
```

### is_superset

如果第一个集合包含第二个集合的所有元素，则返回 `true`。

```python
a = {1, 2, 3}
b = {1, 2}
print(is_superset(a, b))   # true
print(is_superset(b, a))   # false
```

### 约束与错误

| 约束 | 详细 |
|------|------|
| 所有元素必须为相同类型 | 混合不同类型会产生编译错误 |
| 空集合 `{}` | 需要类型注解 |
| 元素查找 | 哈希表（平均 O(1)） |
| 容量溢出 | 自动扩展为 2 倍 |

---

## 迭代器

### 概述

惰性迭代器抽象，支持高效的数据转换管道。迭代器不会复制或具体化中间结果——每个元素按需处理。

### 创建迭代器

使用 `iter()` 从任何集合创建迭代器:

```python
xs = [1, 2, 3, 4, 5]
it = xs.iter()           # Iterator<int>

s = {10, 20, 30}
sit = s.iter()           # Iterator<int>

m = {"a": 1, "b": 2}
mit = m.iter()           # Iterator<(str, int)>
```

### 惰性方法链

迭代器方法返回新的迭代器，形成一个只在消费时才执行的管道:

| 方法 | 说明 |
|--------|-------------|
| `.filter(function)` | 仅产出谓词返回 `true` 的元素 |
| `.map(function)` | 使用给定函数转换每个元素 |
| `.take(count)` | 最多产出 `count` 个元素 |

```python
result = [1, 2, 3, 4, 5]
    .iter()
    .filter((x: int) => x > 2)
    .map((x: int) => x * 2)
    .take(2)
    .to_list()   # [6, 8]
```

### 消费迭代器

| 方法 | 说明 |
|--------|-------------|
| `.to_list()` | 将所有元素收集到 `List<T>` |
| `.next()` | 返回下一个元素，类型为 `Option<T>` |

```python
it = [10, 20].iter()
print(it.next())   # Some(10)
print(it.next())   # Some(20)
print(it.next())   # None
```

### For 循环支持

迭代器可以直接在 `for` 循环中使用:

```python
for x in [1, 2, 3].iter().filter((x: int) => x > 1):
    print(x)
# 2
# 3

for k, v in {"a": 1, "b": 2}.iter():
    print(k)
```
