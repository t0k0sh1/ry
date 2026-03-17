[English](builtins.md) | [日本語](../ja/reference/builtins.md) | [繁體中文](../zh/reference/builtins.md)

# Built-in Function Reference

## Function List

### Core

| Function | Description |
|------|------|
| `print(expr)` | Prints a value to standard output |
| `len(x)` | Returns the number of elements in a list, map, or set, or the length of a string |
| `range(n)` / `range(start, end)` / `range(start, end, step)` | Generates a list of integers |

### Option

| Function | Description |
|------|------|
| `Some(expr)` | Constructs the value-present variant of an Option type |
| `unwrap(opt)` | Extracts the value from an Option |

### Collection Operations

| Function | Description |
|------|------|
| `has_key(map, key)` | Returns whether a key exists in the map |
| `add(set, value)` | Adds an element to a set (duplicates are ignored) |
| `remove(set, value)` | Removes an element from a set |
| `append(list, value)` | Adds an element to the end of a list (mutating) |
| `pop(list)` | Removes and returns the last element of a list |
| `reverse(list)` | Returns a new reversed list (also works on strings) |
| `slice(list, start, end)` | Returns a new sub-list from start to end |
| `filter(list, pred)` | Returns a new list with elements matching the predicate |
| `map(list, fn)` | Returns a new list with each element transformed |
| `sort(list)` / `sort(list, comp)` | Returns a new sorted list (default ascending) |

### [String Operations](builtins-string.md)

| Function | Description |
|------|------|
| `contains(s, sub)` | Whether a substring is contained |
| `starts_with(s, prefix)` | Whether it starts with a prefix |
| `ends_with(s, suffix)` | Whether it ends with a suffix |
| `find(s, sub)` | Position of a substring (-1 if not found) |
| `substring(s, start, end)` | Extract a substring |
| `char_at(s, i)` | Get the character at a specified position |
| `replace(s, old, new)` | Replace all occurrences of a substring |
| `to_upper(s)` / `to_lower(s)` | Uppercase / lowercase conversion |
| `trim(s)` / `trim_start(s)` / `trim_end(s)` | Whitespace removal |
| `repeat(s, n)` | Repeat a string n times |
| `reverse(s)` | Reverse a string |
| `split(s, delim)` | Split a string into a list |
| `join(list, sep)` | Join list elements with a separator |
| `to_int(s)` / `to_float(s)` / `to_str(v)` | Type conversion |

-> See **[String Operation Function Reference](builtins-string.md)** for details

---

## print

**Signature:** `print(expr)`

Prints a value to standard output. A newline is appended at the end.

| Type | Output Format |
|----|---------|
| `int` | `%ld` |
| `float` | `%g` |
| `bool` | `true` / `false` |
| `str` | `%s` |
| `Option` (Some) | `Some(value)` |
| `Option` (None) | `None` |
| `list` | `[elem1, elem2, ...]` |
| `map` | `{key1: val1, key2: val2, ...}` |
| `set` | `{elem1, elem2, ...}` |
| `enum` | Variant name (e.g., `Red`) |

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

**Error condition:** Passing a struct or tuple directly causes a compile error.

---

## Some

**Signature:** `Some(expr) -> Option<T>`

Constructs the value-present variant of an Option type.

```python
let x: Option<int> = Some(42)
print(x)   # Some(42)
```

---

## unwrap

**Signature:** `unwrap(opt: Option<T>) -> T`

Extracts the inner value from an Option. UFCS notation is also available.

```python
let x = Some(42)
print(unwrap(x))    # 42
print(x.unwrap())   # 42 (UFCS)
```

**Error condition:** Passing `None` causes a runtime error (exit(1)).

---

## len

**Signature:** `len(x: List<T> | Map<K, V> | Set<T> | str) -> int`

Returns the number of elements in a list, map, or set, or the byte length of a string.

```python
print(len([1, 2, 3]))         # 3
print(len({"a": 1, "b": 2})) # 2
print(len({1, 2, 3}))         # 3
print(len("hello"))           # 5
```

---

## has_key

**Signature:** `has_key(m: Map<K, V>, key: K) -> bool`

Returns whether a specified key exists in the map. UFCS notation is also available.

```python
let m = {"a": 1, "b": 2}
print(has_key(m, "a"))    # true
print(m.has_key("z"))     # false (UFCS)
```

---

## add

**Signature:** `add(s: Set<T>, value: T)`

Adds an element to a set. Does nothing if the element already exists. UFCS notation is also available.

```python
let s = {1, 2, 3}
s.add(4)          # UFCS
add(s, 5)         # Normal call
s.add(1)          # Ignored because it already exists
print(len(s))     # 5
```

---

## remove

**Signature:** `remove(s: Set<T>, value: T)`

Removes an element from a set. UFCS notation is also available.

```python
let s = {1, 2, 3}
s.remove(2)       # UFCS
print(2 in s)     # false
```

---

## range

**Signature:** `range(n: int) -> List<int>` / `range(start: int, end: int) -> List<int>` / `range(start: int, end: int, step: int) -> List<int>`

Generates a list of integers.

| Form | Generated Values |
|------|------------|
| `range(n)` | `[0, 1, ..., n-1]` |
| `range(start, end)` | `[start, start+1, ..., end-1]` |
| `range(start, end, step)` | `[start, start+step, start+2*step, ...]` (up to but not including `end`) |

- When `step > 0`, generates values from `start` ascending toward `end`.
- When `step < 0`, generates values from `start` descending toward `end`.
- When `step == 0`, a runtime error occurs.
- If the range is empty (e.g., `range(0, 10, -1)`), returns an empty list.

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

**Signature:** `append(list: List<T>, value: T)`

Adds an element to the end of a list. This is a mutating operation — the list is modified in place. UFCS notation is also available.

```python
var xs = [1, 2]
xs.append(3)
print(xs)   # [1, 2, 3]
```

---

## pop

**Signature:** `pop(list: List<T>) -> T`

Removes and returns the last element of a list. UFCS notation is also available.

```python
var xs = [1, 2, 3]
let v = xs.pop()
print(v)    # 3
print(xs)   # [1, 2]
```

**Error condition:** Calling `pop()` on an empty list causes a runtime error (exit(1)).

---

## reverse (list)

**Signature:** `reverse(list: List<T>) -> List<T>`

Returns a new list with elements in reverse order. The original list is not modified. Also works on strings (see [String Operations](builtins-string.md)). UFCS notation is also available.

```python
let xs = [1, 2, 3]
let ys = reverse(xs)
print(ys)   # [3, 2, 1]
print(xs)   # [1, 2, 3] (unchanged)
```

---

## slice

**Signature:** `slice(list: List<T>, start: int, end: int) -> List<T>`

Returns a new sub-list from `start` (inclusive) to `end` (exclusive). Indices are clamped to the valid range (`0` to `len(list)`). UFCS notation is also available.

```python
let xs = [1, 2, 3, 4, 5]
print(slice(xs, 1, 3))     # [2, 3]
print(slice(xs, 0, 100))   # [1, 2, 3, 4, 5] (clamped)
```

---

## filter

**Signature:** `filter(list: List<T>, pred: fn(T) -> bool) -> List<T>`

Returns a new list containing only elements for which the predicate returns `true`. The original list is not modified. UFCS notation is also available.

```python
let xs = [1, 2, 3, 4, 5]
let ys = xs.filter((x: int) -> x > 3)
print(ys)   # [4, 5]
print(xs)   # [1, 2, 3, 4, 5]  (unchanged)
```

---

## map

**Signature:** `map(list: List<T>, fn: fn(T) -> U) -> List<U>`

Returns a new list with each element transformed by the given function. The output element type can differ from the input type. The original list is not modified. UFCS notation is also available.

```python
let xs = [1, 2, 3]
let ys = xs.map((x: int) -> x * 2)
print(ys)   # [2, 4, 6]
```

---

## sort

**Signature:** `sort(list: List<T>) -> List<T>` / `sort(list: List<T>, comp: fn(T, T) -> bool) -> List<T>`

Returns a new sorted list. Default is ascending order. An optional comparator function can be provided that returns `true` if the first argument should come before the second. The original list is not modified. UFCS notation is also available.

```python
let xs = [3, 1, 2]
print(xs.sort())   # [1, 2, 3]

# Descending order
let desc = xs.sort((a: int, b: int) -> a > b)
print(desc)   # [3, 2, 1]
```
