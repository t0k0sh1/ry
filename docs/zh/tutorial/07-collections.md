[English](../../tutorial/07-collections.md) | [日本語](../../ja/tutorial/07-collections.md) | [简体中文](07-collections.md)

# 集合

[<- 上一篇：Record](06-records.md) | [下一篇：高级特性 ->](08-advanced.md)

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
fn swap(a: int, b: int) -> (int, int):
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
fn first(xs: List<int>) -> int:
    return xs[0]
```

### filter、map、sort

列表支持 `filter`、`map`、`sort` 操作。这些操作返回新列表，不会修改原始列表。

```python
xs = [1, 2, 3, 4, 5]

# filter: 保留符合条件的元素
evens = xs.filter(fn(x: int) => x > 3)
print(evens)   # [4, 5]

# map: 转换每个元素
doubled = xs.map(fn(x: int) => x * 2)
print(doubled)   # [2, 4, 6, 8, 10]

# sort: 升序排序（默认）
sorted = [3, 1, 2].sort()
print(sorted)   # [1, 2, 3]

# 链式调用
result = xs.filter(fn(x: int) => x > 1).map(fn(x: int) => x * 10).sort()
print(result)   # [20, 30, 40, 50]
```

### reduce、fold

`reduce` 从第一个元素开始将列表归约为单一值。`fold` 则可提供明确的初始值。

```python
xs = [1, 2, 3, 4, 5]

# reduce: 从第一个元素开始
total = reduce(xs, fn(a: int, b: int) => a + b)
print(total)   # 15

# fold: 提供明确的初始值
total2 = fold(xs, 0, fn(a: int, b: int) => a + b)
print(total2)   # 15
```

### any、all

`any` 如果至少有一个元素满足谓词则返回 `true`。`all` 如果所有元素都满足则返回 `true`。

```python
xs = [1, 2, 3, 4, 5]

print(any(xs, fn(x: int) => x > 4))   # true
print(any(xs, fn(x: int) => x > 9))   # false

print(all(xs, fn(x: int) => x > 0))   # true
print(all(xs, fn(x: int) => x > 3))   # false
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
print(m.has_key("a"))   # true
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
fn get_val(m: Map<str, int>, k: str) -> int:
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
s.add(4)       # 添加元素
s.remove(1)    # 移除元素
s.add(2)       # 已存在，因此忽略
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

### 创建和消费

```python
xs = [1, 2, 3]
ys = xs.iter().to_list()   # [1, 2, 3]
```

### 链式操作

可以链式调用 `filter`、`map` 和 `take` 来构建管道：

```python
result = [1, 2, 3, 4, 5]
    .iter()
    .filter(fn(x: int) => x > 2)
    .map(fn(x: int) => x * 2)
    .take(2)
    .to_list()
print(result)   # [6, 8]
```

### 使用 next() 手动迭代

```python
it = [10, 20].iter()
print(it.next())   # Some(10)
print(it.next())   # Some(20)
print(it.next())   # None
```

### For 循环

迭代器可以直接在 `for` 循环中使用：

```python
for x in [1, 2, 3].iter().filter(fn(x: int) => x > 1):
    print(x)   # 2, 3
```

映射产生元组元素：

```python
for k, v in {"a": 1, "b": 2}.iter():
    print(k)
```

---

[<- 上一篇：Record](06-records.md) | [下一篇：高级特性 ->](08-advanced.md)
