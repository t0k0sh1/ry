[English](builtins.md) | [日本語](../ja/reference/builtins.md) | [繁體中文](../zh/reference/builtins.md)

# Built-in Function Reference

## Function List

### Core

| Function | Description |
|------|------|
| `print(expr)` | Prints a value to standard output |
| `len(x)` | Returns the number of elements in a list, map, or set, or the length of a string |
| `range(n)` / `range(start, end)` | Generates a list of integers |

### Option

| Function | Description |
|------|------|
| `Some(expr)` | Constructs the value-present variant of an Option type |
| `unwrap(opt)` | Extracts the value from an Option |

### Result

| Function | Description |
|------|------|
| `Ok(expr)` | Constructs the success variant of a Result type |
| `Err(expr)` | Constructs the error variant of a Result type |
| `is_ok(r)` | Returns whether the Result is Ok |
| `is_err(r)` | Returns whether the Result is Err |
| `unwrap_or(r, default)` | Returns the Ok value, or a default if Err |

### Collection Operations

| Function | Description |
|------|------|
| `has_key(map, key)` | Returns whether a key exists in the map |
| `add(set, value)` | Adds an element to a set (duplicates are ignored) |
| `remove(set, value)` | Removes an element from a set |

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

## Ok

**Signature:** `Ok(expr) -> Result<T, E>`

Constructs the success variant of a Result type.

```python
let r: Result<int, str> = Ok(42)
print(is_ok(r))   # true
```

---

## Err

**Signature:** `Err(expr) -> Result<T, E>`

Constructs the error variant of a Result type.

```python
let r: Result<int, str> = Err("not found")
print(is_err(r))   # true
```

---

## is_ok

**Signature:** `is_ok(r: Result<T, E>) -> bool`

Returns whether a Result contains an Ok value.

```python
let r: Result<int, str> = Ok(42)
print(is_ok(r))    # true
```

---

## is_err

**Signature:** `is_err(r: Result<T, E>) -> bool`

Returns whether a Result contains an Err value.

```python
let r: Result<int, str> = Err("fail")
print(is_err(r))   # true
```

---

## unwrap_or

**Signature:** `unwrap_or(r: Result<T, E>, default: T) -> T`

Returns the Ok value if the Result is Ok, otherwise returns the default value.

```python
let r: Result<int, str> = Err("fail")
print(unwrap_or(r, 0))   # 0

let s: Result<int, str> = Ok(42)
print(unwrap_or(s, 0))   # 42
```

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

**Signature:** `range(n: int) -> List<int>` / `range(start: int, end: int) -> List<int>`

Generates a list of integers.

| Form | Generated Values |
|------|------------|
| `range(n)` | `[0, 1, ..., n-1]` |
| `range(start, end)` | `[start, start+1, ..., end-1]` |

```python
print(range(3))       # [0, 1, 2]
print(range(2, 5))    # [2, 3, 4]

for i in range(3):
    print(i)
# 0
# 1
# 2
```
