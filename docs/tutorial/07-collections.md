[English](07-collections.md) | [日本語](../ja/tutorial/07-collections.md) | [繁體中文](../zh/tutorial/07-collections.md)

# Collections

[<- Prev: Structs](06-structs.md) | [Next: Advanced Features ->](08-advanced.md)

Ry has four collection types: **Tuples**, **Lists**, **Maps**, and **Sets**.

---

## Tuples

A tuple is an immutable data structure that groups multiple values together. It can hold elements of different types.

### Creation

```python
let t = (1, 3.14)
```

### Type Annotation

```python
let t: (int, float) = (1, 3.14)
```

### Element Access

Elements are accessed by index using `.0`, `.1`, etc.

```python
let t = (1, 3.14)
print(t.0)   # 1
print(t.1)   # 3.14
```

### As Function Return Values

Tuples are useful when you want to return multiple values.

```python
fn swap(a: int, b: int) -> (int, int):
    return (b, a)

let result = swap(1, 2)
print(result.0)  # 2
print(result.1)  # 1
```

### Limitations

- Accessing an out-of-bounds index (e.g., `.2` on a tuple with 2 elements) causes a compile error.
- Passing a tuple directly to `print` causes an error. Pass each element individually.

---

## Lists

A list is a variable-length data structure containing elements of the same type.

### Creation

```python
let xs = [1, 2, 3]
```

### Type Annotation

```python
let xs: List<int> = [1, 2, 3]
```

### Index Access

```python
print(xs[0])   # 1

let i = 1
print(xs[i])   # 2
```

### Index Assignment

```python
xs[0] = 99
```

### len

```python
print(len(xs))   # 3
```

### print

```python
print(xs)   # [1, 2, 3]
```

### Iteration with for

```python
for x in xs:
    print(x)
```

### Function Parameters

```python
fn first(xs: List<int>) -> int:
    return xs[0]
```

### filter, map, sort

Lists support `filter`, `map`, and `sort` operations. These return new lists without modifying the original.

```python
let xs = [1, 2, 3, 4, 5]

# filter: keep elements matching a condition
let evens = xs.filter(fn(x: int): x > 3)
print(evens)   # [4, 5]

# map: transform each element
let doubled = xs.map(fn(x: int): x * 2)
print(doubled)   # [2, 4, 6, 8, 10]

# sort: sort in ascending order (default)
let sorted = [3, 1, 2].sort()
print(sorted)   # [1, 2, 3]

# Chaining
let result = xs.filter(fn(x: int): x > 1).map(fn(x: int): x * 10).sort()
print(result)   # [20, 30, 40, 50]
```

### reduce, fold

`reduce` accumulates a list into a single value starting from the first element. `fold` does the same but with an explicit initial value.

```python
let xs = [1, 2, 3, 4, 5]

# reduce: start from first element
let total = reduce(xs, fn(a: int, b: int): a + b)
print(total)   # 15

# fold: provide an explicit initial value
let total2 = fold(xs, 0, fn(a: int, b: int): a + b)
print(total2)   # 15
```

### any, all

`any` returns `true` if at least one element satisfies the predicate. `all` returns `true` if every element does.

```python
let xs = [1, 2, 3, 4, 5]

print(any(xs, fn(x: int): x > 4))   # true
print(any(xs, fn(x: int): x > 9))   # false

print(all(xs, fn(x: int): x > 0))   # true
print(all(xs, fn(x: int): x > 3))   # false
```

### sum, min, max

```python
let xs = [3, 1, 4, 1, 5]
print(sum(xs))   # 14
print(min(xs))   # 1
print(max(xs))   # 5
```

### first, last, is_empty

```python
let xs = [10, 20, 30]
print(first(xs))      # 10
print(last(xs))       # 30
print(is_empty(xs))   # false
```

### enumerate, zip

`enumerate` pairs each element with its index. `zip` combines two lists element-by-element.

```python
let xs = [10, 20, 30]
let indexed = enumerate(xs)
# [(0, 10), (1, 20), (2, 30)]
for p in indexed:
    print(p.0)
    print(p.1)

let ys = ["a", "b", "c"]
let zipped = zip(xs, ys)
# [(10, "a"), (20, "b"), (30, "c")]
```

### Limitations

- All elements must be of the same type. Mixing different types is not allowed.
- An empty list `[]` causes an error.
- Out-of-bounds access results in a runtime error (`exit(1)`).
- Supported element types are `int`, `float`, `bool`, and `str`.

---

## Maps

A map is an associative array that manages key-value pairs.

### Creation

```python
let m = {"a": 1, "b": 2}
```

### Type Annotation

```python
let m: Map<str, int> = {"a": 1, "b": 2}
```

### Key Access

```python
print(m["a"])   # 1
```

### Insertion / Update

Assigning to a new key inserts it; assigning to an existing key updates it.

```python
m["c"] = 3    # Insert new entry
m["a"] = 99   # Update existing entry
```

### len

```python
print(len(m))   # 3
```

### print

```python
print(m)   # {a: 99, b: 2, c: 3}
```

### has_key

Checks whether a key exists.

```python
print(m.has_key("a"))   # true
```

### keys, values

`keys` returns a list of all keys. `values` returns a list of all values.

```python
let m = {"a": 1, "b": 2, "c": 3}
print(keys(m))     # ["a", "b", "c"]
print(values(m))   # [1, 2, 3]
```

### Function Parameters

```python
fn get_val(m: Map<str, int>, k: str) -> int:
    return m[k]
```

### Limitations

- All keys must be of the same type, and all values must be of the same type.
- An empty map requires a type annotation.
- Accessing a nonexistent key results in a runtime error (`exit(1)`).

---

## Sets

A set is a collection that holds elements of the same type without duplicates.

### Creation

```python
let s = {1, 2, 3}
```

### Type Annotation

```python
let s: Set<int> = {1, 2, 3}
```

### in Operator

Use the `in` operator to check if an element is in the set.

```python
print(2 in s)   # true
print(5 in s)   # false
```

### add / remove

```python
s.add(4)       # Add element
s.remove(1)    # Remove element
s.add(2)       # Ignored since it already exists
```

### len / print

```python
print(len(s))  # 3
print(s)       # {2, 3, 4}
```

### Iteration with for

```python
for x in s:
    print(x)
```

### Empty Set

An empty set requires a type annotation.

```python
let empty: Set<int> = {}
```

### Limitations

- All elements must be of the same type.
- Supported element types are `int`, `float`, `bool`, and `str`.

---

[<- Prev: Structs](06-structs.md) | [Next: Advanced Features ->](08-advanced.md)
