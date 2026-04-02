[English](collections.md) | [日本語](../ja/reference/collections.md) | [繁體中文](../zh/reference/collections.md)

# Collection Reference (Tuple, List, Map, Set)

## Tuple

### Overview

A fixed-length combination of heterogeneous values. Implemented as a stack-allocated value type using LLVM literal StructType.

### Syntax

```python
t = (42,)                      # single-element tuple (trailing comma required)
t = (1, 3.14)
t: (int, float) = (1, 3.14)
```

### Type Annotation

```python
single: (int,) = (42,)                     # trailing comma required for single-element
pair: (int, str) = (42, "hello")
triple: (int, float, bool) = (1, 2.0, true)
```

### Comparison

Tuples support `==` and `!=` via element-wise comparison.

```python
print((1, 2) == (1, 2))    # true
print((1, 2) != (3, 4))    # true
print(("a", 1) == ("b", 1))  # false
```

### Element Access

Access elements using numeric indices `.0`, `.1`, etc.

```python
t = (10, 3.14)
print(t.0)   # 10
print(t.1)   # 3.14
```

### Function Return Values

```python
function swap(a: int, b: int) -> (int, int):
    return (b, a)

result = swap(1, 2)
print(result.0)   # 2
print(result.1)   # 1
```

### Constraints and Errors

| Constraint | Details |
|------|------|
| Out-of-range index | Compile error |
| Comparison operators | Only `==` and `!=` are supported; `<`, `<=`, `>`, `>=` are not |

---

## List

### Overview

A variable-length sequence of elements of the same type. Allocated on the heap.

### Syntax

```python
xs = [1, 2, 3]
xs: List<int> = [1, 2, 3]
```

### Empty List

An empty list requires a type annotation so the element type can be determined:

```python
xs: List<int> = []
xs: List<str> = []
```

### Concatenation

Lists can be concatenated with `+` and `+=`:

```python
a = [1, 2, 3]
b = [4, 5, 6]
c = a + b       # [1, 2, 3, 4, 5, 6]
a += [7, 8]     # a is now [1, 2, 3, 7, 8]
```

Both operands must have the same element type.

### Supported Element Types

`int`, `float`, `bool`, `str`

### Index Access

```python
xs = [1, 2, 3]
print(xs[0])   # 1
print(xs[2])   # 3
```

Negative indices wrap around from the end (Python-style):

```python
xs = [10, 20, 30]
print(xs[-1])   # 30 (last element)
print(xs[-2])   # 20
print(xs[-3])   # 10
```

Out-of-bounds access (including negative indices that exceed the list length) raises a runtime error.

### Index Assignment

```python
xs = [1, 2, 3]
xs[0] = 99
print(xs[0])   # 99
xs[-1] = 42    # assigns to last element
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

### for Iteration

```python
xs = [10, 20, 30]
for x in xs:
    print(x)
# 10
# 20
# 30
```

### append

Adds an element to the end of the list. This is a mutating operation.

```python
xs = [1, 2]
xs.append(3)
print(xs)   # [1, 2, 3]
```

### pop

Removes and returns the last element. Returns `None` if the list is empty.

```python
xs = [1, 2, 3]
v = xs.pop()
print(v)    # Some(3)
print(xs)   # [1, 2]
```

### reverse

Returns a new list with elements in reverse order. The original list is not modified. Also works on strings.

```python
xs = [1, 2, 3]
print(reverse(xs))   # [3, 2, 1]
print(xs)            # [1, 2, 3] (unchanged)
```

### slice

Returns a new sub-list from `start` (inclusive) to `end` (exclusive). Indices are clamped to the valid range.

```python
xs = [1, 2, 3, 4, 5]
print(slice(xs, 1, 3))     # [2, 3]
print(slice(xs, 0, 100))   # [1, 2, 3, 4, 5] (clamped)
```

### take

Returns a new list with the first `count` elements. If `count` exceeds the list length, returns a copy of the entire list. If `count <= 0`, returns an empty list. The original list is not modified.

```python
xs = [1, 2, 3, 4, 5]
ys = xs.take(3)
print(ys)   # [1, 2, 3]
print(xs.take(10))   # [1, 2, 3, 4, 5] (clamped)
print(xs.take(0))    # []
```

### tap

Calls the given function on each element (ignoring any return value), then returns the original list unchanged. Useful for debugging or inserting side effects in a method chain.

```python
xs = [1, 2, 3]
ys = xs.tap((x: int) => print(x)).map((x: int) => x * 2)
# prints 1, 2, 3, then ys = [2, 4, 6]
```

### filter

Returns a new list containing only elements that satisfy the predicate. The original list is not modified.

```python
xs = [1, 2, 3, 4, 5]
ys = xs.filter((x: int) => x > 3)
print(ys)   # [4, 5]
```

### map

Returns a new list with each element transformed by the given function. The output element type can differ from the input. The original list is not modified.

```python
xs = [1, 2, 3]
ys = xs.map((x: int) => x * 2)
print(ys)   # [2, 4, 6]
```

### sort

Returns a new sorted list. Default is ascending order. A custom comparator can be provided. The original list is not modified. The sort is **stable** (equal elements preserve their original order). Internally uses TimSort.

```python
xs = [3, 1, 2]
print(xs.sort())   # [1, 2, 3]

# Descending order with comparator
desc = xs.sort((a: int, b: int) => a > b)
print(desc)   # [3, 2, 1]
```

### Chaining filter, map, sort

These functions return new lists, so they can be chained via UFCS.

```python
xs = [5, 3, 1, 4, 2]
result = xs.filter((x: int) => x > 1).map((x: int) => x * 10).sort()
print(result)   # [20, 30, 40, 50]
```

### reduce

Reduces a list to a single value using an accumulator function, starting with the first element.

```python
xs = [1, 2, 3, 4, 5]
total = reduce(xs, (a: int, b: int) => a + b)
print(total)   # 15
```

### fold

Folds a list to a single value using an accumulator function and an explicit initial value.

```python
xs = [1, 2, 3, 4, 5]
total = fold(xs, 0, (a: int, b: int) => a + b)
print(total)   # 15
```

### any

Returns `true` if at least one element satisfies the predicate.

```python
xs = [1, 2, 3, 4, 5]
print(any(xs, (x: int) => x > 4))   # true
print(any(xs, (x: int) => x > 9))   # false
```

### all

Returns `true` if every element satisfies the predicate.

```python
xs = [2, 4, 6]
print(all(xs, (x: int) => x > 0))   # true
print(all(xs, (x: int) => x > 3))   # false
```

### sum

Returns the sum of all elements.

```python
xs = [1, 2, 3, 4, 5]
print(sum(xs))   # 15
```

### min

Returns the minimum element.

```python
xs = [3, 1, 4, 1, 5]
print(min(xs))   # 1
```

### max

Returns the maximum element.

```python
xs = [3, 1, 4, 1, 5]
print(max(xs))   # 5
```

### first

Returns the first element. Returns `None` if the list is empty.

```python
xs = [10, 20, 30]
print(first(xs))   # Some(10)
```

### last

Returns the last element. Returns `None` if the list is empty.

```python
xs = [10, 20, 30]
print(last(xs))   # Some(30)
```

### is_empty

Returns `true` if the list has no elements.

```python
xs = [1, 2, 3]
print(is_empty(xs))   # false
print(is_empty([]))   # true (requires type annotation in practice)
```

### enumerate

Returns a list of `(index, element)` tuples.

```python
xs = [10, 20, 30]
pairs = enumerate(xs)
# pairs = [(0, 10), (1, 20), (2, 30)]

# Tuple destructuring in for loop
for i, x in enumerate(xs):
    print(f"{i}: {x}")    # 0: 10, 1: 20, 2: 30
```

### zip

Combines two lists into a list of `(elem1, elem2)` tuples. The result length equals the shorter list.

```python
xs = [1, 2, 3]
ys = ["a", "b", "c"]
pairs = zip(xs, ys)
# pairs = [(1, "a"), (2, "b"), (3, "c")]

# Tuple destructuring in for loop
for a, b in zip(xs, ys):
    print(f"{a}: {b}")    # 1: a, 2: b, 3: c
```

### insert

Inserts an element at the specified index. Elements at and after the index are shifted right.

```python
xs = [1, 2, 3]
insert(xs, 1, 99)
print(xs)   # [1, 99, 2, 3]
```

### remove_at

Removes and returns the element at the specified index. Elements after the index are shifted left.

```python
xs = [1, 2, 3, 4]
v = remove_at(xs, 1)
print(v)    # 2
print(xs)   # [1, 3, 4]
```

### remove

Removes the first occurrence of the specified value from the list. Does nothing if the value is not found. This is a mutating operation.

```python
xs = [1, 2, 3, 2, 4]
remove(xs, 2)
print(xs)   # [1, 3, 2, 4]
```

### distinct

Returns a new list with duplicate elements removed. The original order is preserved (first occurrence kept). The original list is not modified.

```python
xs = [1, 2, 3, 2, 1, 4]
print(distinct(xs))   # [1, 2, 3, 4]
print(xs)             # [1, 2, 3, 2, 1, 4] (unchanged)
```

### flatten

Flattens a nested list (list of lists) by one level. Returns a new list. The original list is not modified.

```python
xs = [[1, 2], [3, 4]]
print(flatten(xs))   # [1, 2, 3, 4]
print(xs)            # [[1, 2], [3, 4]] (unchanged)
```

### Operation Complexity

| Operation | Complexity |
|------|--------|
| `xs[i]` index access | O(1) |
| `append` / `append!` | O(1) amortized |
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

### Constraints and Errors

| Constraint | Details |
|------|------|
| All elements must be the same type | Mixed types cause a compile error |
| Empty list `[]` | Requires type annotation (e.g., `xs: List<int> = []`) |
| Out-of-range access | Runtime error (exit(1)) |

---

## Copy-on-Write (CoW) Semantics

All collection types (List, Map, Set) use **Copy-on-Write** semantics when managed by ARC. This means:

- **Assignment shares data**: `b = a` does not copy the collection — both variables reference the same data. The reference count is incremented.
- **Mutation triggers a copy**: When a shared collection is mutated (e.g., `append`, `remove`, index assignment), a deep copy is automatically created before the mutation. Only the mutator pays the cost.
- **Unique owners mutate in-place**: When a collection has only one reference (`strong_count == 1`), mutations are performed in-place with zero copy overhead.

```python
a = [1, 2, 3]       # strong_count = 1
b = a                # strong_count = 2 (shared)
append(b, 4)         # strong_count > 1 → deep copy b, then mutate
                     # a = [1, 2, 3]  (strong_count = 1)
                     # b = [1, 2, 3, 4]  (strong_count = 1, new allocation)

c = [10, 20]         # strong_count = 1
append(c, 30)        # strong_count == 1 → mutate in-place (no copy)
```

### Operations that trigger CoW

| Type | Mutating operations |
|------|-------------------|
| **List** | `append()`, `pop()`, `insert()`, `remove()`, `remove_at()`, `sort!()`, `reverse!()`, index assignment (`xs[i] = val`) |
| **Map** | `remove()`, index assignment (`m[key] = val`) |
| **Set** | `add()`, `remove()` |

Non-mutating operations (`appended`, `slice`, `take`, `filter`, `map`, `sort`, `reverse`, `get`, `items`, etc.) create new collections and do not trigger CoW.

---

## Map

### Overview

A key-value mapping. Allocated on the heap.

### Syntax

```python
m = {"a": 1, "b": 2}
m: Map<str, int> = {"a": 1, "b": 2}
```

### Key Access

```python
m = {"a": 1, "b": 2}
print(m["a"])   # 1
```

### Insert and Update

```python
m = {"a": 1}
m["b"] = 2     # Insert new entry
m["a"] = 99    # Update existing entry
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

Returns a list of all keys in the map.

```python
m = {"a": 1, "b": 2, "c": 3}
print(keys(m))   # ["a", "b", "c"]
```

### values

Returns a list of all values in the map.

```python
m = {"a": 1, "b": 2, "c": 3}
print(values(m))   # [1, 2, 3]
```

### items

Returns a list of `(key, value)` tuples for all entries in the map.

```python
m = {"a": 1, "b": 2}
pairs = items(m)
# pairs = [("a", 1), ("b", 2)]
```

### remove (Map)

Removes the entry with the specified key from the map. Does nothing if the key does not exist.

```python
m = {"a": 1, "b": 2}
remove(m, "a")
print(m)   # {b: 2}
```

### get

Returns the value for the specified key, or a default value if the key does not exist.

```python
m = {"a": 1, "b": 2}
print(get(m, "a", 0))   # 1
print(get(m, "z", 0))   # 0
```

### merge

Returns a new map that combines all entries from both maps. When keys overlap, values from the second map take precedence. The original maps are not modified.

```python
m1 = {"a": 1, "b": 2}
m2 = {"b": 99, "c": 3}
m3 = merge(m1, m2)
print(m3["a"])   # 1
print(m3["b"])   # 99
print(m3["c"])   # 3
```

### Constraints and Errors

| Constraint | Details |
|------|------|
| All keys must be the same type | Mixed key types cause a compile error |
| All values must be the same type | Mixed value types cause a compile error |
| Empty map | Type annotation is required (e.g., `m: Map<str, int> = {"a": 1}`) |
| Accessing a non-existent key | Runtime error (exit(1)) |
| Key lookup | Hash table (O(1) average) |
| Capacity overflow | Automatically doubles in size |

---

## Set

### Overview

A collection that holds elements of the same type without duplicates. Allocated on the heap.

### Syntax

```python
s = {1, 2, 3}
s: Set<int> = {1, 2, 3}
```

### Supported Element Types

`int`, `float`, `bool`, `str`

### in Operator (Membership Check)

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

### add (Add Element)

Duplicate elements are ignored when added.

```python
s = {1, 2, 3}
s.add(4)         # Add
s.add(1)         # Ignored because it already exists
print(length(s))    # 4
```

### remove (Remove Element)

```python
s = {1, 2, 3}
s.remove(2)
print(2 in s)   # false
```

### for Iteration

```python
s = {10, 20, 30}
for x in s:
    print(x)
```

### Empty Set

An empty set requires a type annotation.

```python
s: Set<int> = {}
```

### Function Parameters

```python
function has_value(s: Set<int>, v: int) -> bool:
    return v in s
```

### union

Returns a new set containing all elements from both sets.

```python
a = {1, 2, 3}
b = {3, 4, 5}
print(union(a, b))   # {1, 2, 3, 4, 5}
```

### intersection

Returns a new set containing only elements present in both sets.

```python
a = {1, 2, 3}
b = {2, 3, 4}
print(intersection(a, b))   # {2, 3}
```

### difference

Returns a new set containing elements in the first set but not in the second.

```python
a = {1, 2, 3}
b = {2, 3, 4}
print(difference(a, b))   # {1}
```

### symmetric_difference

Returns a new set containing elements that are in either set but not in both.

```python
a = {1, 2, 3}
b = {2, 3, 4}
print(symmetric_difference(a, b))   # {1, 4}
```

### is_subset

Returns `true` if all elements of the first set are contained in the second set.

```python
a = {1, 2}
b = {1, 2, 3}
print(is_subset(a, b))   # true
print(is_subset(b, a))   # false
```

### is_superset

Returns `true` if the first set contains all elements of the second set.

```python
a = {1, 2, 3}
b = {1, 2}
print(is_superset(a, b))   # true
print(is_superset(b, a))   # false
```

### Constraints and Errors

| Constraint | Details |
|------|------|
| All elements must be the same type | Mixed types cause a compile error |
| Empty set `{}` | Type annotation is required |
| Element lookup | Hash table (O(1) average) |
| Capacity overflow | Automatically doubles in size |

---

## Iterator

### Overview

A lazy iterator abstraction that enables efficient data transformation pipelines. Iterators do not copy or materialize intermediate results — each element is processed on demand.

### Creating Iterators

Use `iter()` to create an iterator from any collection:

```python
xs = [1, 2, 3, 4, 5]
it = xs.iter()           # Iterator<int>

s = {10, 20, 30}
sit = s.iter()           # Iterator<int>

m = {"a": 1, "b": 2}
mit = m.iter()           # Iterator<(str, int)>
```

### Lazy Method Chaining

Iterator methods return new iterators, forming a pipeline that is only evaluated when consumed:

| Method | Description |
|--------|-------------|
| `.filter(function)` | Yields only elements where the predicate returns `true` |
| `.map(function)` | Transforms each element using the given function |
| `.take(count)` | Yields at most `count` elements |

```python
result = [1, 2, 3, 4, 5]
    .iter()
    .filter((x: int) => x > 2)
    .map((x: int) => x * 2)
    .take(2)
    .to_list()   # [6, 8]
```

### Consuming Iterators

| Method | Description |
|--------|-------------|
| `.to_list()` | Collects all elements into a `List<T>` |
| `.next()` | Returns the next element as `Option<T>` |

```python
it = [10, 20].iter()
print(it.next())   # Some(10)
print(it.next())   # Some(20)
print(it.next())   # None
```

### For Loop Support

Iterators can be used directly in `for` loops:

```python
for x in [1, 2, 3].iter().filter((x: int) => x > 1):
    print(x)
# 2
# 3

for k, v in {"a": 1, "b": 2}.iter():
    print(k)
```
