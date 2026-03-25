[English](builtins.md) | [日本語](../ja/reference/builtins.md) | [繁體中文](../zh/reference/builtins.md)

# Built-in Function Reference

## Function List

### Core

| Function | Description |
|------|------|
| `print(expr)` | Prints a value to standard output |
| `length(value)` | Returns the number of elements in a list, map, or set, or the number of UTF-8 characters in a string |
| `range(n)` / `range(start, end)` / `range(start, end, step)` | Generates a list of integers |
| `exit(code)` | Terminates the process with the given exit code |
| `args()` | Returns command-line arguments as `List<str>` |
| `available_parallelism()` | Returns the runtime worker count as `int` |
| `sleep(duration_ms)` | Suspends execution for the specified number of milliseconds |
| `env(key)` | Returns the environment variable as `Option<str>` |
| `env(key, default)` | Returns the environment variable, or `default` if not set |
| `channel[T]()` | Creates an unbuffered `Channel<T>` |
| `channel[T](capacity)` | Creates a buffered `Channel<T>` |
| `send(ch, value)` | Sends a value through `Channel<T>` |
| `send(stream, data)` | Sends `List<byte>` through `TcpStream`, returns bytes sent |
| `try_send(ch, value)` | Attempts to send through `Channel<T>` without blocking |
| `recv(ch)` | Receives a value from `Channel<T>` |
| `recv(stream, max)` | Receives up to `max` bytes from `TcpStream` as `List<byte>` |
| `recv_opt(ch)` | Receives from `Channel<T>` as `Option<T>` or `bool` for `Channel<Unit>` |
| `try_recv(ch)` | Attempts to receive from `Channel<T>` as `Option<T>` or `bool` for `Channel<Unit>` |
| `close(ch)` | Closes a `Channel<T>` |
| `close(handle)` | Closes a `TcpStream` or `TcpListener` |
| `join(task)` | Waits for a `Task<T>` to complete and returns its result |

### Option

| Function | Description |
|------|------|
| `Some(expr)` | Constructs the value-present variant of an Option type |

### Collection Operations

| Function | Description |
|------|------|
| `has_key(map, key)` | Returns whether a key exists in the map |
| `add(set, value)` | Adds an element to a set (duplicates are ignored) |
| `remove(set, value)` | Removes an element from a set |
| `append(list, value)` / `append!(list, value)` | Adds an element to the end of a list (mutating) |
| `appended(list, value)` | Returns a new list with the element added (non-mutating) |
| `pop(list)` | Removes and returns the last element as `Option<T>` |
| `reverse(list)` | Returns a new reversed list (also works on strings) |
| `reverse!(list)` | Reverses a list in place (mutating) |
| `slice(list, start, end)` | Returns a new sub-list from start to end |
| `take(list, n)` | Returns a new list with the first n elements |
| `tap(list, fn)` | Calls fn on each element for side effects, returns the original list |
| `filter(list, pred)` | Returns a new list with elements matching the predicate |
| `map(list, fn)` | Returns a new list with each element transformed |
| `sort(list)` / `sort(list, comp)` | Returns a new sorted list (default ascending) |
| `sort!(list)` / `sort!(list, comp)` | Sorts a list in place (mutating) |
| `insert(list, i, val)` | Inserts an element at index i |
| `remove_at(list, i)` | Removes and returns the element at index i |
| `items(map)` | Returns a list of (key, value) tuples |
| `remove(map, key)` | Removes the entry with the specified key |
| `get(map, key)` | Returns the value for key as `Option<V>` |
| `get(map, key, default)` | Returns the value for key, or default if not found |
| `union(set, set)` | Returns the union of two sets |
| `intersection(set, set)` | Returns the intersection of two sets |
| `difference(set, set)` | Returns the difference of two sets |
| `symmetric_difference(set, set)` | Returns the symmetric difference of two sets |
| `is_subset(set, set)` | Returns whether the first set is a subset of the second |
| `is_superset(set, set)` | Returns whether the first set is a superset of the second |

### Iterator

| Function | Description |
|------|------|
| `iter(collection)` | Creates a lazy iterator from a List, Set, or Map |
| `next(iter)` | Returns the next element as `Option<T>`, or `None` if exhausted |
| `to_list(iter)` | Collects all remaining iterator elements into a `List<T>` |
| `filter(iter, pred)` | Returns a lazy iterator that yields only elements matching the predicate |
| `map(iter, fn)` | Returns a lazy iterator that transforms each element |
| `take(iter, n)` | Returns a lazy iterator that yields at most n elements |

### [String Operations](builtins-string.md)

| Function | Description |
|------|------|
| `contains(string, substring)` | Whether a substring is contained |
| `starts_with(string, prefix)` | Whether it starts with a prefix |
| `ends_with(string, suffix)` | Whether it ends with a suffix |
| `find(string, substring)` | Character position of a substring (`Option<int>`) |
| `byte_len(string)` | Returns the byte length of a string |
| `substring(string, start, end)` | Extract a substring |
| `char_at(string, i)` | Get the character at a specified position |
| `replace(string, old, new)` | Replace all occurrences of a substring |
| `to_upper(string)` / `to_lower(string)` | Uppercase / lowercase conversion |
| `trim(string)` / `trim_start(string)` / `trim_end(string)` | Whitespace removal |
| `repeat(string, count)` | Repeat a string n times |
| `reverse(string)` | Reverse a string |
| `split(string, delimiter)` | Split a string into a list |
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
x: Option<int> = Some(42)
print(x)   # Some(42)
```

---

## length

**Signature:** `length(x: List<T> | Map<K, V> | Set<T> | str) -> int`

Returns the number of elements in a list, map, or set, or the number of UTF-8 characters in a string. Use `byte_len()` for the byte length.

```python
print(length([1, 2, 3]))         # 3
print(length({"a": 1, "b": 2})) # 2
print(length({1, 2, 3}))         # 3
print(length("hello"))           # 5
print(length("あいう"))           # 3 (UTF-8 characters)
```

---

## has_key

**Signature:** `has_key(m: Map<K, V>, key: K) -> bool`

Returns whether a specified key exists in the map. UFCS notation is also available.

```python
m = {"a": 1, "b": 2}
print(has_key(m, "a"))    # true
print(m.has_key("z"))     # false (UFCS)
```

---

## add

**Signature:** `add(s: Set<T>, value: T)`

Adds an element to a set. Does nothing if the element already exists. UFCS notation is also available.

```python
s = {1, 2, 3}
s.add(4)          # UFCS
add(s, 5)         # Normal call
s.add(1)          # Ignored because it already exists
print(length(s))     # 5
```

---

## remove

**Signature:** `remove(s: Set<T>, value: T)`

Removes an element from a set. UFCS notation is also available.

```python
s = {1, 2, 3}
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

## exit

**Signature:** `exit(code: int)`

Terminates the process immediately with the given exit code. Code after `exit()` is unreachable.

```python
exit(0)        # normal termination
exit(1)        # error termination
```

---

## args

**Signature:** `args() -> List<str>`

Returns the command-line arguments passed to the script as a list of strings. Does not include the interpreter name or the script filename — only the arguments after the script path.

```python
# Run: ry script.ry hello world
a = args()
print(length(a))    # 2
print(a[0])      # hello
print(a[1])      # world

for x in args():
    print(x)
```

---

## sleep

**Signature:** `sleep(duration_ms: int) -> Unit`

Suspends execution of the current thread for the specified number of milliseconds. If `duration_ms` is 0 or negative, the function returns immediately.

```python
sleep(1000)    # wait 1 second
sleep(0)       # returns immediately
```

> **Note:** When called inside a `spawn`ed task, `sleep` blocks the underlying worker thread. If many tasks sleep concurrently, the thread pool may become exhausted and other tasks will stall until a sleep expires.

---

## env

**Signature:** `env(key: str) -> Option<str>` / `env(key: str, default: str) -> str`

Returns the value of an environment variable. The one-argument form returns `Option<str>` (`Some(value)` if set, `None` if not). The two-argument form returns the value or `default` if the variable is not set.

If a `.env` file exists in the project root (the directory containing `ry.toml`), its entries are automatically loaded into the process environment at startup. Existing environment variables are not overwritten by `.env` values.

> **Security note:** `.env` files typically contain secrets (API keys, database passwords, tokens, etc.). Do **not** commit `.env` to version control (add it to `.gitignore` or equivalent), and treat its contents as sensitive configuration.

```python
# One-argument form: returns Option<str>
path = env("PATH")
match path:
    case Some(v):
        print(v)
    case None:
        print("PATH not set")

# Two-argument form: returns str with default
port = env("PORT", "8080")
print(port)   # "8080" if PORT is not set
```

### `.env` file format

```env
# Comments start with #
DATABASE_URL=postgres://localhost/mydb
API_KEY="secret-key-123"
EMPTY_VALUE=
QUOTED='single quoted'
```

---

## append

**Signature:** `append(list: List<T>, value: T)`

Adds an element to the end of a list. This is a mutating operation — the list is modified in place. UFCS notation is also available.

```python
xs = [1, 2]
xs.append(3)
print(xs)   # [1, 2, 3]
```

---

## pop

**Signature:** `pop(list: List<T>) -> Option<T>`

Removes and returns the last element of a list as `Option<T>`. Returns `None` if the list is empty. UFCS notation is also available.

```python
xs = [1, 2, 3]
v = xs.pop()
print(v)    # Some(3)
print(xs)   # [1, 2]
```

---

## reverse (list)

**Signature:** `reverse(list: List<T>) -> List<T>`

Returns a new list with elements in reverse order. The original list is not modified. Also works on strings (see [String Operations](builtins-string.md)). UFCS notation is also available.

```python
xs = [1, 2, 3]
ys = reverse(xs)
print(ys)   # [3, 2, 1]
print(xs)   # [1, 2, 3] (unchanged)
```

---

## slice

**Signature:** `slice(list: List<T>, start: int, end: int) -> List<T>`

Returns a new sub-list from `start` (inclusive) to `end` (exclusive). Indices are clamped to the valid range (`0` to `length(list)`). UFCS notation is also available.

```python
xs = [1, 2, 3, 4, 5]
print(slice(xs, 1, 3))     # [2, 3]
print(slice(xs, 0, 100))   # [1, 2, 3, 4, 5] (clamped)
```

---

## take

**Signature:** `take(list: List<T>, n: int) -> List<T>`

Returns a new list with the first `n` elements. If `n` exceeds the list length, returns a copy of the entire list. If `n <= 0`, returns an empty list. The original list is not modified. UFCS notation is also available.

```python
xs = [1, 2, 3, 4, 5]
ys = xs.take(3)
print(ys)   # [1, 2, 3]
print(xs.take(10))   # [1, 2, 3, 4, 5] (clamped)
print(xs.take(0))    # []
```

---

## tap

**Signature:** `tap(list: List<T>, fn: fn(T) -> R) -> List<T>`

Calls the given function on each element (ignoring any return value), then returns the original list unchanged. Useful for debugging or inserting side effects in a method chain. UFCS notation is also available.

```python
xs = [1, 2, 3]
ys = xs.tap(fn(x: int): print(x)).map(fn(x: int): x * 2)
# prints 1, 2, 3, then ys = [2, 4, 6]
```

---

## filter

**Signature:** `filter(list: List<T>, pred: fn(T) -> bool) -> List<T>`

Returns a new list containing only elements for which the predicate returns `true`. The original list is not modified. UFCS notation is also available.

```python
xs = [1, 2, 3, 4, 5]
ys = xs.filter(fn(x: int): x > 3)
print(ys)   # [4, 5]
print(xs)   # [1, 2, 3, 4, 5]  (unchanged)
```

---

## map

**Signature:** `map(list: List<T>, fn: fn(T) -> U) -> List<U>`

Returns a new list with each element transformed by the given function. The output element type can differ from the input type. The original list is not modified. UFCS notation is also available.

```python
xs = [1, 2, 3]
ys = xs.map(fn(x: int): x * 2)
print(ys)   # [2, 4, 6]
```

---

## sort

**Signature:** `sort(list: List<T>) -> List<T>` / `sort(list: List<T>, comp: fn(T, T) -> bool) -> List<T>`

Returns a new sorted list. Default is ascending order. An optional comparator function can be provided that returns `true` if the first argument should come before the second. The original list is not modified. The sort is **stable** (equal elements preserve their original order). UFCS notation is also available.

```python
xs = [3, 1, 2]
print(xs.sort())   # [1, 2, 3]

# Descending order
desc = xs.sort(fn(a: int, b: int): a > b)
print(desc)   # [3, 2, 1]
```

---

## sort!

**Signature:** `sort!(list: List<T>)` / `sort!(list: List<T>, comp: fn(T, T) -> bool)`

Sorts a list in place. Same sorting algorithm as `sort()`, but modifies the original list instead of creating a new one. UFCS notation is also available.

```python
xs = [3, 1, 2]
xs.sort!()
print(xs)   # [1, 2, 3]
```

---

## reverse!

**Signature:** `reverse!(list: List<T>)`

Reverses a list in place. UFCS notation is also available.

```python
xs = [1, 2, 3]
xs.reverse!()
print(xs)   # [3, 2, 1]
```

---

## appended

**Signature:** `appended(list: List<T>, value: T) -> List<T>`

Returns a new list with the element added at the end. The original list is not modified. UFCS notation is also available.

```python
xs = [1, 2]
ys = xs.appended(3)
print(xs)   # [1, 2] (unchanged)
print(ys)   # [1, 2, 3]
```

---

## append!

**Signature:** `append!(list: List<T>, value: T)`

Alias for `append()`. Adds an element to the end of a list in place. Provided for naming consistency with the `!` convention.

---

## first

**Signature:** `first(list: List<T>) -> Option<T>`

Returns the first element of a list as `Option<T>`. Returns `None` if the list is empty.

```python
print(first([10, 20, 30]))   # Some(10)
```

---

## last

**Signature:** `last(list: List<T>) -> Option<T>`

Returns the last element of a list as `Option<T>`. Returns `None` if the list is empty.

```python
print(last([10, 20, 30]))   # Some(30)
```

---

## get (Map)

**Signature:** `get(map: Map<K, V>, key: K) -> Option<V>` / `get(map: Map<K, V>, key: K, default: V) -> V`

Two-argument form returns the value for key as `Option<V>`. Three-argument form returns the value or the default.

```python
m = {"a": 1, "b": 2}
print(get(m, "a"))       # Some(1)
print(get(m, "z"))       # None
print(get(m, "z", 0))   # 0
```

---

## iter

**Signature:** `iter(collection: List<T> | Set<T>) -> Iterator<T>` / `iter(collection: Map<K, V>) -> Iterator<(K, V)>`

Creates a lazy iterator from a collection. The iterator does not copy data; it references the original collection. UFCS notation is also available.

- For `List<T>` and `Set<T>`, the element type is `T`.
- For `Map<K, V>`, the element type is the tuple `(K, V)`.

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

**Signature:** `next(iter: Iterator<T>) -> Option<T>`

Returns the next element from the iterator as `Option<T>`. Returns `None` when the iterator is exhausted. The iterator advances its internal state on each call. UFCS notation is also available.

```python
it = [10, 20].iter()
print(it.next())   # Some(10)
print(it.next())   # Some(20)
print(it.next())   # None
```

---

## to_list

**Signature:** `to_list(iter: Iterator<T>) -> List<T>`

Collects all remaining elements from the iterator into a new list. UFCS notation is also available.

```python
xs = [1, 2, 3, 4, 5]
ys = xs.iter().filter(fn(x: int): x > 2).to_list()
print(ys)   # [3, 4, 5]
```
