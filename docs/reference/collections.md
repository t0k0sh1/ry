[English](collections.md) | [日本語](../ja/reference/collections.md) | [繁體中文](../zh/reference/collections.md)

# Collection Reference (Tuple, List, Map, Set)

## Tuple

### Overview

A fixed-length combination of heterogeneous values. Implemented as a stack-allocated value type using LLVM literal StructType.

### Syntax

```python
let t = (1, 3.14)
let t: (int, float) = (1, 3.14)
```

### Type Annotation

```python
let pair: (int, str) = (42, "hello")
let triple: (int, float, bool) = (1, 2.0, true)
```

### Element Access

Access elements using numeric indices `.0`, `.1`, etc.

```python
let t = (10, 3.14)
print(t.0)   # 10
print(t.1)   # 3.14
```

### Function Return Values

```python
fn swap(a: int, b: int) -> (int, int):
    return (b, a)

let result = swap(1, 2)
print(result.0)   # 2
print(result.1)   # 1
```

### Constraints and Errors

| Constraint | Details |
|------|------|
| Out-of-range index | Compile error |
| Passing a tuple directly to `print` | Compile error (not supported by print) |

---

## List

### Overview

A variable-length sequence of elements of the same type. Allocated on the heap.

### Syntax

```python
let xs = [1, 2, 3]
let xs: List<int> = [1, 2, 3]
```

### Supported Element Types

`int`, `float`, `bool`, `str`

### Index Access

```python
let xs = [1, 2, 3]
print(xs[0])   # 1
print(xs[2])   # 3
```

### Index Assignment

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

### for Iteration

```python
let xs = [10, 20, 30]
for x in xs:
    print(x)
# 10
# 20
# 30
```

### append

Adds an element to the end of the list. This is a mutating operation.

```python
var xs = [1, 2]
xs.append(3)
print(xs)   # [1, 2, 3]
```

### pop

Removes and returns the last element. Causes a runtime error on an empty list.

```python
var xs = [1, 2, 3]
let v = xs.pop()
print(v)    # 3
print(xs)   # [1, 2]
```

### reverse

Returns a new list with elements in reverse order. The original list is not modified. Also works on strings.

```python
let xs = [1, 2, 3]
print(reverse(xs))   # [3, 2, 1]
print(xs)            # [1, 2, 3] (unchanged)
```

### slice

Returns a new sub-list from `start` (inclusive) to `end` (exclusive). Indices are clamped to the valid range.

```python
let xs = [1, 2, 3, 4, 5]
print(slice(xs, 1, 3))     # [2, 3]
print(slice(xs, 0, 100))   # [1, 2, 3, 4, 5] (clamped)
```

### take

Returns a new list with the first `n` elements. If `n` exceeds the list length, returns a copy of the entire list. If `n <= 0`, returns an empty list. The original list is not modified.

```python
let xs = [1, 2, 3, 4, 5]
let ys = xs.take(3)
print(ys)   # [1, 2, 3]
print(xs.take(10))   # [1, 2, 3, 4, 5] (clamped)
print(xs.take(0))    # []
```

### tap

Calls the given function on each element for side effects, then returns the original list unchanged. Useful for debugging or inserting side effects in a method chain.

```python
let xs = [1, 2, 3]
let ys = xs.tap(fn(x: int): print(x)).map(fn(x: int): x * 2)
# prints 1, 2, 3, then ys = [2, 4, 6]
```

### filter

Returns a new list containing only elements that satisfy the predicate. The original list is not modified.

```python
let xs = [1, 2, 3, 4, 5]
let ys = xs.filter(fn(x: int): x > 3)
print(ys)   # [4, 5]
```

### map

Returns a new list with each element transformed by the given function. The output element type can differ from the input. The original list is not modified.

```python
let xs = [1, 2, 3]
let ys = xs.map(fn(x: int): x * 2)
print(ys)   # [2, 4, 6]
```

### sort

Returns a new sorted list. Default is ascending order. A custom comparator can be provided. The original list is not modified.

```python
let xs = [3, 1, 2]
print(xs.sort())   # [1, 2, 3]

# Descending order with comparator
let desc = xs.sort(fn(a: int, b: int): a > b)
print(desc)   # [3, 2, 1]
```

### Chaining filter, map, sort

These functions return new lists, so they can be chained via UFCS.

```python
let xs = [5, 3, 1, 4, 2]
let result = xs.filter(fn(x: int): x > 1).map(fn(x: int): x * 10).sort()
print(result)   # [20, 30, 40, 50]
```

### reduce

Reduces a list to a single value using an accumulator function, starting with the first element.

```python
let xs = [1, 2, 3, 4, 5]
let total = reduce(xs, fn(a: int, b: int): a + b)
print(total)   # 15
```

### fold

Folds a list to a single value using an accumulator function and an explicit initial value.

```python
let xs = [1, 2, 3, 4, 5]
let total = fold(xs, 0, fn(a: int, b: int): a + b)
print(total)   # 15
```

### any

Returns `true` if at least one element satisfies the predicate.

```python
let xs = [1, 2, 3, 4, 5]
print(any(xs, fn(x: int): x > 4))   # true
print(any(xs, fn(x: int): x > 9))   # false
```

### all

Returns `true` if every element satisfies the predicate.

```python
let xs = [2, 4, 6]
print(all(xs, fn(x: int): x > 0))   # true
print(all(xs, fn(x: int): x > 3))   # false
```

### sum

Returns the sum of all elements.

```python
let xs = [1, 2, 3, 4, 5]
print(sum(xs))   # 15
```

### min

Returns the minimum element.

```python
let xs = [3, 1, 4, 1, 5]
print(min(xs))   # 1
```

### max

Returns the maximum element.

```python
let xs = [3, 1, 4, 1, 5]
print(max(xs))   # 5
```

### first

Returns the first element. Causes a runtime error on an empty list.

```python
let xs = [10, 20, 30]
print(first(xs))   # 10
```

### last

Returns the last element. Causes a runtime error on an empty list.

```python
let xs = [10, 20, 30]
print(last(xs))   # 30
```

### is_empty

Returns `true` if the list has no elements.

```python
let xs = [1, 2, 3]
print(is_empty(xs))   # false
print(is_empty([]))   # true (requires type annotation in practice)
```

### enumerate

Returns a list of `(index, element)` tuples.

```python
let xs = [10, 20, 30]
let pairs = enumerate(xs)
# pairs = [(0, 10), (1, 20), (2, 30)]

# Tuple destructuring in for loop
for i, x in enumerate(xs):
    print(f"{i}: {x}")    # 0: 10, 1: 20, 2: 30
```

### zip

Combines two lists into a list of `(elem1, elem2)` tuples. The result length equals the shorter list.

```python
let xs = [1, 2, 3]
let ys = ["a", "b", "c"]
let pairs = zip(xs, ys)
# pairs = [(1, "a"), (2, "b"), (3, "c")]

# Tuple destructuring in for loop
for a, b in zip(xs, ys):
    print(f"{a}: {b}")    # 1: a, 2: b, 3: c
```

### insert

Inserts an element at the specified index. Elements at and after the index are shifted right.

```python
var xs = [1, 2, 3]
insert(xs, 1, 99)
print(xs)   # [1, 99, 2, 3]
```

### remove_at

Removes and returns the element at the specified index. Elements after the index are shifted left.

```python
var xs = [1, 2, 3, 4]
let v = remove_at(xs, 1)
print(v)    # 2
print(xs)   # [1, 3, 4]
```

### remove

Removes the first occurrence of the specified value from the list. Does nothing if the value is not found. This is a mutating operation.

```python
var xs = [1, 2, 3, 2, 4]
remove(xs, 2)
print(xs)   # [1, 3, 2, 4]
```

### distinct

Returns a new list with duplicate elements removed. The original order is preserved (first occurrence kept). The original list is not modified.

```python
let xs = [1, 2, 3, 2, 1, 4]
print(distinct(xs))   # [1, 2, 3, 4]
print(xs)             # [1, 2, 3, 2, 1, 4] (unchanged)
```

### flatten

Flattens a nested list (list of lists) by one level. Returns a new list. The original list is not modified.

```python
let xs = [[1, 2], [3, 4]]
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
| `len` | O(1) |

### Constraints and Errors

| Constraint | Details |
|------|------|
| All elements must be the same type | Mixed types cause a compile error |
| Empty list `[]` | Compile error because type cannot be inferred |
| Out-of-range access | Runtime error (exit(1)) |

---

## Map

### Overview

A key-value mapping. Allocated on the heap.

### Syntax

```python
let m = {"a": 1, "b": 2}
let m: Map<str, int> = {"a": 1, "b": 2}
```

### Key Access

```python
let m = {"a": 1, "b": 2}
print(m["a"])   # 1
```

### Insert and Update

```python
let m = {"a": 1}
m["b"] = 2     # Insert new entry
m["a"] = 99    # Update existing entry
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

### keys

Returns a list of all keys in the map.

```python
let m = {"a": 1, "b": 2, "c": 3}
print(keys(m))   # ["a", "b", "c"]
```

### values

Returns a list of all values in the map.

```python
let m = {"a": 1, "b": 2, "c": 3}
print(values(m))   # [1, 2, 3]
```

### items

Returns a list of `(key, value)` tuples for all entries in the map.

```python
let m = {"a": 1, "b": 2}
let pairs = items(m)
# pairs = [("a", 1), ("b", 2)]
```

### remove (Map)

Removes the entry with the specified key from the map. Does nothing if the key does not exist.

```python
let m = {"a": 1, "b": 2}
remove(m, "a")
print(m)   # {b: 2}
```

### get

Returns the value for the specified key, or a default value if the key does not exist.

```python
let m = {"a": 1, "b": 2}
print(get(m, "a", 0))   # 1
print(get(m, "z", 0))   # 0
```

### merge

Returns a new map that combines all entries from both maps. When keys overlap, values from the second map take precedence. The original maps are not modified.

```python
let m1 = {"a": 1, "b": 2}
let m2 = {"b": 99, "c": 3}
let m3 = merge(m1, m2)
print(m3["a"])   # 1
print(m3["b"])   # 99
print(m3["c"])   # 3
```

### Constraints and Errors

| Constraint | Details |
|------|------|
| All keys must be the same type | Mixed key types cause a compile error |
| All values must be the same type | Mixed value types cause a compile error |
| Empty map | Type annotation is required (e.g., `let m: Map<str, int> = {"a": 1}`) |
| Accessing a non-existent key | Runtime error (exit(1)) |
| Key lookup | Hash table (O(1) average) |
| Capacity overflow | Automatically doubles in size |

---

## Set

### Overview

A collection that holds elements of the same type without duplicates. Allocated on the heap.

### Syntax

```python
let s = {1, 2, 3}
let s: Set<int> = {1, 2, 3}
```

### Supported Element Types

`int`, `float`, `bool`, `str`

### in Operator (Membership Check)

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

### add (Add Element)

Duplicate elements are ignored when added.

```python
let s = {1, 2, 3}
s.add(4)         # Add
s.add(1)         # Ignored because it already exists
print(len(s))    # 4
```

### remove (Remove Element)

```python
let s = {1, 2, 3}
s.remove(2)
print(2 in s)   # false
```

### for Iteration

```python
let s = {10, 20, 30}
for x in s:
    print(x)
```

### Empty Set

An empty set requires a type annotation.

```python
let s: Set<int> = {}
```

### Function Parameters

```python
fn has_value(s: Set<int>, v: int) -> bool:
    return v in s
```

### union

Returns a new set containing all elements from both sets.

```python
let a = {1, 2, 3}
let b = {3, 4, 5}
print(union(a, b))   # {1, 2, 3, 4, 5}
```

### intersection

Returns a new set containing only elements present in both sets.

```python
let a = {1, 2, 3}
let b = {2, 3, 4}
print(intersection(a, b))   # {2, 3}
```

### difference

Returns a new set containing elements in the first set but not in the second.

```python
let a = {1, 2, 3}
let b = {2, 3, 4}
print(difference(a, b))   # {1}
```

### symmetric_difference

Returns a new set containing elements that are in either set but not in both.

```python
let a = {1, 2, 3}
let b = {2, 3, 4}
print(symmetric_difference(a, b))   # {1, 4}
```

### is_subset

Returns `true` if all elements of the first set are contained in the second set.

```python
let a = {1, 2}
let b = {1, 2, 3}
print(is_subset(a, b))   # true
print(is_subset(b, a))   # false
```

### is_superset

Returns `true` if the first set contains all elements of the second set.

```python
let a = {1, 2, 3}
let b = {1, 2}
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
