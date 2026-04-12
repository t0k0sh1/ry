[English](../../tutorial/07-collections.md) | [日本語](../../ja/tutorial/07-collections.md) | [简体中文](07-collections.md)

# 集合与迭代器

[<- 上一篇：Record 与枚举](06-records.md) | [下一篇：错误处理 ->](08-error-handling.md)

Ry 有四种集合类型：**元组**、**列表**、**映射**、**集合**。

---

## 元组

元组是将多个值组合在一起的不可变数据结构，可以持有不同类型的元素。

### 创建

```python
t = (1, 3.14)
```

### 类型标注

```python
t: (int, float) = (1, 3.14)
```

### 元素访问

使用 `.0`、`.1` 等索引来访问元素。

```python
t = (1, 3.14)
print(t.0)   # 1
print(t.1)   # 3.14
```

### 作为函数返回值

想要返回多个值时，元组很方便。

```python
function swap(a: int, b: int) -> (int, int):
    return (b, a)

result = swap(1, 2)
print(result.0)  # 2
print(result.1)  # 1
```

### 限制

- 超出范围的索引（例如：对只有 2 个元素的元组使用 `.2` 访问）会产生编译错误。
- 将元组直接传给 `print` 会产生错误。请逐个传递各元素。

---

## 列表

列表是由相同类型的元素组成的可变长度数据结构。

### 创建

```python
xs = [1, 2, 3]
```

### 类型标注

```python
xs: List<int> = [1, 2, 3]
```

### 索引访问

```python
print(xs[0])   # 1

i = 1
print(xs[i])   # 2
```

### 索引赋值

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

### for 遍历

```python
for x in xs:
    print(x)
```

### 函数参数

```python
function first(xs: List<int>) -> int:
    return xs[0]
```

### filter、map、sort

列表支持 `filter`、`map`、`sort` 操作。这些操作返回新列表，不会修改原始列表。

```python
xs = [1, 2, 3, 4, 5]

# filter：保留符合条件的元素
greater_than_three = filter(xs, (x: int) => x > 3)
print(greater_than_three)   # [4, 5]

# map：转换每个元素
doubled = map(xs, (x: int) => x * 2)
print(doubled)   # [2, 4, 6, 8, 10]

# sort：升序排序（默认）
sorted = sort([3, 1, 2])
print(sorted)   # [1, 2, 3]

# 使用 UFCS（统一函数调用语法）链式调用
# x.f(args) 等价于 f(x, args)
result = xs.filter((x: int) => x > 1).map((x: int) => x * 10).sort()
print(result)   # [20, 30, 40, 50]
```

### reduce、fold

`reduce` 从第一个元素开始将列表归约为单一值。`fold` 则可提供明确的初始值。

```python
xs = [1, 2, 3, 4, 5]

# reduce：从第一个元素开始
total = reduce(xs, (a: int, b: int) => a + b)
print(total)   # 15

# fold：提供明确的初始值
total2 = fold(xs, 0, (a: int, b: int) => a + b)
print(total2)   # 15
```

### any、all

`any` 如果至少有一个元素满足谓词则返回 `true`。`all` 如果所有元素都满足则返回 `true`。

```python
xs = [1, 2, 3, 4, 5]

print(any(xs, (x: int) => x > 4))   # true
print(any(xs, (x: int) => x > 9))   # false

print(all(xs, (x: int) => x > 0))   # true
print(all(xs, (x: int) => x > 3))   # false
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
print(first(xs))      # Some(10)
print(last(xs))       # Some(30)
print(is_empty(xs))   # false
```

### enumerate、zip

`enumerate` 为每个元素附上索引。`zip` 将两个列表逐元素合并。

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

### 限制

- 所有元素必须是相同类型，不能混合不同类型。
- 空列表 `[]` 会产生错误。
- 超出范围的访问会产生运行时错误（`exit(1)`）。
- 元素类型支持 `int`、`float`、`bool`、`str`。

---

## 映射

映射是管理键值对的关联数组。

### 创建

```python
m = {"a": 1, "b": 2}
```

### 类型标注

```python
m: Map<str, int> = {"a": 1, "b": 2}
```

### 键访问

```python
print(m["a"])   # 1
```

### 插入 / 更新

对新的键赋值即为插入，对已有的键赋值即为更新。

```python
m["c"] = 3    # 插入新条目
m["a"] = 99   # 更新已有条目
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

检查键是否存在。

```python
print(has_key(m, "a"))   # true
```

### keys、values

`keys` 返回所有键的列表。`values` 返回所有值的列表。

```python
m = {"a": 1, "b": 2, "c": 3}
print(keys(m))     # ["a", "b", "c"]
print(values(m))   # [1, 2, 3]
```

### 函数参数

```python
function get_val(m: Map<str, int>, k: str) -> int:
    return m[k]
```

### 限制

- 所有键必须是相同类型，所有值也必须是相同类型。
- 空映射需要类型标注。
- 访问不存在的键会产生运行时错误（`exit(1)`）。

---

## 集合

集合是持有相同类型元素且不重复的集合类型。

### 创建

```python
s = {1, 2, 3}
```

### 类型标注

```python
s: Set<int> = {1, 2, 3}
```

### in 运算符

使用 `in` 运算符检查元素是否包含在集合中。

```python
print(2 in s)   # true
print(5 in s)   # false
```

### add / remove

```python
add(s, 4)       # 添加元素
remove(s, 1)    # 移除元素
add(s, 2)       # 已存在，因此忽略
```

### length / print

```python
print(length(s))  # 3
print(s)       # {2, 3, 4}
```

### for 遍历

```python
for x in s:
    print(x)
```

### 空集合

空集合需要类型标注。

```python
empty: Set<int> = {}
```

### 限制

- 所有元素必须是相同类型。
- 元素类型支持 `int`、`float`、`bool`、`str`。

---

## 迭代器

迭代器提供一种**惰性**的方式来处理集合。迭代器不会在每个步骤中创建中间列表，而是通过管道逐个处理元素。

> **为什么使用惰性迭代器？** 当你直接在列表上链式调用 `filter` 和 `map` 时，每个步骤都会创建新的中间列表。使用迭代器，元素逐个流经整个管道 —— 无需中间分配。当处理大型集合或只需要前几个结果（使用 `take`）时，这一点很重要。

### 创建和消费

对集合调用 `iter()` 获取迭代器，调用 `to_list()` 将结果物化为列表：

```python
xs = [1, 2, 3]
ys = to_list(iter(xs))   # [1, 2, 3]
```

### 链式操作

可以链式调用 `filter`、`map` 和 `take` 来构建管道。这使用了你在[函数](05-functions.md)中学到的 UFCS 链式调用风格：

```python
result = to_list(take(map(filter(iter([1, 2, 3, 4, 5]), (x: int) => x > 2), (x: int) => x * 2), 2))
print(result)   # [6, 8]

# UFCS 链式调用风格（等价）：
result = [1, 2, 3, 4, 5]
    .iter()
    .filter((x: int) => x > 2)
    .map((x: int) => x * 2)
    .take(2)
    .to_list()
print(result)   # [6, 8]
```

以下是一个更实际的例子 —— 处理成绩列表：

```python
scores = [85, 42, 93, 67, 78, 55, 91]

# 获取前 3 个及格分数（>= 60），翻倍作为奖励
top_bonus = to_list(take(map(filter(iter(scores), (s: int) => s >= 60), (s: int) => s * 2), 3))
print(top_bonus)   # [170, 186, 134]
```

### 使用 next() 手动迭代

`next()` 返回 `Option` —— 如果有下一个元素则为 `Some(value)`，迭代器耗尽时为 `None`。你将在[错误处理](08-error-handling.md)中进一步了解 `Option`。

```python
it = iter([10, 20])
print(next(it))   # Some(10)
print(next(it))   # Some(20)
print(next(it))   # None
```

### For 循环

迭代器可以直接在 `for` 循环中使用：

```python
for x in filter(iter([1, 2, 3]), (x: int) => x > 1):
    print(x)   # 2, 3
```

### 遍历映射和集合

映射产生键值元组。集合产生单独的元素：

```python
for k, v in iter({"a": 1, "b": 2}):
    print(f"{k} = {v}")

for x in iter({10, 20, 30}):
    print(x)
```

### 常见错误

- **忘记 `to_list()`**：迭代器管道本身不做任何事 —— 它是惰性的。你必须用 `to_list()`、`for` 循环或 `next()` 来消费它。
- **过早调用 `to_list()`**：在 `filter()` 之前放置 `to_list()` 会违背惰性求值的目的，因为它会先物化所有元素。

---

## 练习

1. **迭代器管道**：给定 `xs = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]`，使用迭代器管道计算偶数之和。（提示：使用 `.filter()` 然后 `.to_list()` 和 `sum()`。）

2. **手动迭代**：对 `[100, 200, 300]` 创建迭代器，使用 `case` 区块处理 `next()` 返回的 `Some` 和 `None` 情况。

---

[<- 上一篇：Record 与枚举](06-records.md) | [下一篇：错误处理 ->](08-error-handling.md)
