[English](builtins.md) | [日本語](../ja/reference/builtins.md) | [繁體中文](../zh/reference/builtins.md)

# Built-in Function Reference

## Function List

### Core

| Function | Description |
|------|------|
| `print()` / `print(expr1, expr2, ..., sep=" ", end="\n")` | Prints values to standard output. `sep` controls the separator (default: space), `end` controls the line ending (default: newline) |
| `length(value)` | Returns the number of elements in a list, map, or set, or the number of UTF-8 characters in a string |
| `range(n)` / `range(start, end)` / `range(start, end, step)` | Generates a list of integers |
| `exit(code)` | Terminates the process with the given exit code |
| `arguments()` | Returns command-line arguments as `List<str>` |
| `available_parallelism()` | Returns the runtime worker count as `int` |
| `sleep(duration_ms)` | Suspends execution for the specified number of milliseconds |
| `env(key)` | Returns the environment variable as `Option<str>` |
| `env(key, default)` | Returns the environment variable, or `default` if not set |
| `send(stream, data)` | Sends `List<u8>` through `TcpStream` or `TlsStream`, returns `Result<int, Error>` |
| `receive(stream, max)` | Receives up to `max` bytes from `TcpStream` or `TlsStream` as `Result<List<u8>, Error>` |
| `close(handle)` | Closes a `TcpStream`, `TlsStream`, or `TcpListener` |
| `block_on(task)` | Blocks the current thread until a `Task<T>` completes and returns its result |
| `to_str(value)` | Converts a value to its string representation. Supports `int`, `float` (shortest round-trip representation; whole numbers print with trailing `.0`), `bool`, `str`, record, enum, tuple, `List`, `Map`, `Set` (nested containers like `Map<str, List<int>>` are recursively formatted), `Result`, `Option`, union types (formatted as the active variant), and function values (printed as `<closure>`). String elements inside collections are wrapped in double quotes (e.g., `["hello", "world"]`) |
| `type_of(expr)` | Returns the type of `expr` as a `Type` value. See [type_of](#type_of) |
| `fail()` / `fail(message)` | Marks the current test as failed (only available in `ry test` mode) |

### Option

| Function | Description |
|------|------|
| `Some(expr)` | Constructs the value-present variant of an Option type |

### Result / Error

| Function | Description |
|------|------|
| `Ok(value)` | Constructs the success variant of a `Result<T, Error>` |
| `Err(error)` | Constructs the error variant of a `Result<T, Error>` |
| `Error(message)` | Creates an `Error` value with a message |
| `Error(message, code)` | Creates an `Error` value with a message and error code |
| `result.and_then(closure)` | If `Ok`, calls `closure` (which returns `Result<U, E>`); if `Err`, propagates the error |
| `result.map(closure)` | If `Ok`, applies `closure` to the value and wraps the return in `Ok`; if `Err`, propagates the error |

### Checked Arithmetic

| Function | Description |
|------|------|
| `checked_add(a, b)` | Returns `Ok(a + b)` if no overflow, otherwise `Err(Error("arithmetic overflow"))` |
| `checked_sub(a, b)` | Returns `Ok(a - b)` if no overflow, otherwise `Err(Error("arithmetic overflow"))` |
| `checked_mul(a, b)` | Returns `Ok(a * b)` if no overflow, otherwise `Err(Error("arithmetic overflow"))` |
| `saturating_add(a, b)` | Returns `a + b`, clamped to `int` range on overflow |
| `saturating_sub(a, b)` | Returns `a - b`, clamped to `int` range on overflow |
| `saturating_mul(a, b)` | Returns `a * b`, clamped to `int` range on overflow |
| `wrapping_add(a, b)` | Returns `a + b` with wrapping on overflow |
| `wrapping_sub(a, b)` | Returns `a - b` with wrapping on overflow |
| `wrapping_mul(a, b)` | Returns `a * b` with wrapping on overflow |

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
| `take(list, count)` | Returns a new list with the first count elements |
| `tap(list, function)` | Calls function on each element for side effects, returns the original list |
| `filter(list, pred)` | Returns a new list with elements matching the predicate |
| `map(list, function)` | Returns a new list with each element transformed |
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
| `first(list)` | Returns the first element as `Option<T>`, or `None` if empty |
| `last(list)` | Returns the last element as `Option<T>`, or `None` if empty |
| `remove(list, value)` | Removes the first occurrence of value from a list |
| `is_empty(list / map / set / str)` | Returns whether the collection or string is empty |
| `distinct(list)` | Returns a new list with duplicates removed |
| `flatten(list)` | Returns a new list with nested lists flattened |
| `reduce(list, fn)` | Reduces a list to a single value using the reducer function |
| `fold(list, init, fn)` | Folds a list with an initial accumulator value |
| `any(list, pred)` | Returns `true` if any element matches the predicate |
| `all(list, pred)` | Returns `true` if all elements match the predicate |
| `sum(list)` | Returns the sum of all elements |
| `min(list)` | Returns the minimum element |
| `max(list)` | Returns the maximum element |
| `enumerate(list)` | Returns a list of `(index, value)` tuples. Also accepts a `str`, yielding `(int, str)` per UTF-8 code point |
| `zip(list1, list2)` | Returns a list of `(a, b)` tuples pairing elements from two lists. Either (or both) arguments may be a `str` |
| `keys(map)` | Returns all keys as a `List<K>` |
| `values(map)` | Returns all values as a `List<V>` |
| `merge(map1, map2)` | Returns a new map containing entries from both maps |

### Iterator

| Function | Description |
|------|------|
| `iter(collection)` | Creates a lazy iterator from a List, Set, or Map |
| `next(iter)` | Returns the next element as `Option<T>`, or `None` if exhausted |
| `to_list(iter)` | Collects all remaining iterator elements into a `List<T>` |
| `filter(iter, pred)` | Returns a lazy iterator that yields only elements matching the predicate |
| `map(iter, function)` | Returns a lazy iterator that transforms each element |
| `take(iter, count)` | Returns a lazy iterator that yields at most count elements |

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
| `to_int(s)` / `to_float(s)` / `to_str(v)` | Type conversion (`to_int` and `to_float` return `Result<T, Error>`) |

-> See **[String Operation Function Reference](builtins-string.md)** for details

---

## print

**Signature:** `print()` / `print(expr1, expr2, ..., sep=" ", end="\n")`

Prints one or more values to standard output, separated by `sep` (default: space). Appends `end` (default: newline) after the last value. When called with no arguments, prints only `end`.

### Named Parameters

| Parameter | Type  | Default | Description                              |
|-----------|-------|---------|------------------------------------------|
| `sep`     | `str` | `" "`   | Separator inserted between values        |
| `end`     | `str` | `"\n"`  | String appended after the last value     |

| Type | Output Format |
|----|---------|
| `int` | `%ld` |
| `float` | Shortest round-trip decimal (minimum digits to recover the exact `double`), with trailing `.0` for whole-number values (e.g. `3.0`, `0.0`) |
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
| `enum` | Variant name (e.g., `Red`) |
| `record` | `RecordName(field: val, ...)` |
| function value (closure / lambda) | `<closure>` |
| union | Formatted as the active variant's type |

`float` values are formatted using the **shortest round-trip representation**: the fewest decimal digits that reconstruct the exact `double` value when parsed back, matching the behaviour of Python 3, Rust, Go, and JavaScript. Whole-number floats additionally append `.0` (e.g. `3.0`, `0.0`) so they are visually distinguishable from `int`. As a result, imprecise arithmetic like `0.1 + 0.2` prints as `"0.30000000000000004"` rather than `"0.3"`, accurately reflecting the stored value. Nested collections (e.g. `Map<str, List<int>>`) are recursively formatted using the inner element's formatter. Union variants whose underlying type is `List`, `Map`, or `Set` format as that collection; variants whose underlying type is a function value format as `<closure>`.

```python
print(42)          # 42
print(3.14)        # 3.14
print(3.0)         # 3.0         (whole-number float keeps .0)
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

# Nested collections
m: Map<str, List<int>> = {"a": [1, 2, 3]}
print(m)           # {"a": [1, 2, 3]}

# Collection-typed union variant
x: int | List<int> = [1, 2, 3]
print(x)           # [1, 2, 3]

# Function value
f = (x: int) => x * 2
print(f)           # <closure>

# Multiple arguments (space-separated)
print(1, 2, 3)             # 1 2 3
print("hello", "world")   # hello world
print(1, "hello", true)   # 1 hello true
print()                    # (empty line)

# Named parameters: end and sep
print("hello", end="")    # hello  (no newline)
print("hello", end="!\n") # hello!
print(1, 2, 3, sep=", ")  # 1, 2, 3
print("a", "b", sep="-", end="!\n")  # a-b!
```

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

Terminates the process immediately with the given exit code. Statements
after `exit()` are compiled into an unreachable block that LLVM removes
during optimization, so they never run:

```python
exit(0)        # normal termination
exit(1)        # error termination

print("a")
exit(0)
print("b")     # never prints — unreachable after exit
```

The same treatment applies to `return`, `break`, and `continue` — code
after any diverging control-flow statement is silently elided.

---

## arguments

**Signature:** `arguments() -> List<str>`

Returns the command-line arguments passed to the script as a list of strings. Does not include the interpreter name or the script filename — only the arguments after the script path.

```python
# Run: ry script.ry hello world
a = arguments()
print(length(a))    # 2
print(a[0])      # hello
print(a[1])      # world

for x in arguments():
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

---

## env

**Signature:** `env(key: str) -> Option<str>` / `env(key: str, default: str) -> str`

Returns the value of an environment variable. The one-argument form returns `Option<str>` (`Some(value)` if set, `None` if not). The two-argument form returns the value or `default` if the variable is not set.

If a `.env` file exists in the project root (the directory containing `package.toml`), its entries are automatically loaded into the process environment at startup. Existing environment variables are not overwritten by `.env` values.

> **Security note:** `.env` files typically contain secrets (API keys, database passwords, tokens, etc.). Do **not** commit `.env` to version control (add it to `.gitignore` or equivalent), and treat its contents as sensitive configuration.

```python
# One-argument form: returns Option<str>
path = env("PATH")
case path:
    Some(v):
        print(v)
    None:
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

### Environment-specific `.env` files

When `RY_ENV` is set, Ry loads environment-specific `.env` files with the following priority:

- `.env.<env>` is loaded first (e.g., `.env.dev` when `RY_ENV=dev`)
- `.env` is loaded second (values already set by `.env.<env>` are not overwritten)
- When `RY_ENV=prod`, no `.env` files are loaded (security)
- When `RY_ENV` is not set, only `.env` is loaded (backward compatible)

See [RY_ENV](packages.md#ry_env) for details on environment modes.

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

**Signature:** `take(list: List<T>, count: int) -> List<T>`

Returns a new list with the first `count` elements. If `count` exceeds the list length, returns a copy of the entire list. If `count <= 0`, returns an empty list. The original list is not modified. UFCS notation is also available.

```python
xs = [1, 2, 3, 4, 5]
ys = xs.take(3)
print(ys)   # [1, 2, 3]
print(xs.take(10))   # [1, 2, 3, 4, 5] (clamped)
print(xs.take(0))    # []
```

---

## tap

**Signature:** `tap(list: List<T>, function: function(T) -> R) -> List<T>`

Calls the given function on each element (ignoring any return value), then returns the original list unchanged. Useful for debugging or inserting side effects in a method chain. UFCS notation is also available.

```python
xs = [1, 2, 3]
ys = xs.tap((x: int) => print(x)).map((x: int) => x * 2)
# prints 1, 2, 3, then ys = [2, 4, 6]
```

---

## filter

**Signature:** `filter(list: List<T>, pred: function(T) -> bool) -> List<T>`

Returns a new list containing only elements for which the predicate returns `true`. The original list is not modified. UFCS notation is also available.

```python
xs = [1, 2, 3, 4, 5]
ys = xs.filter((x: int) => x > 3)
print(ys)   # [4, 5]
print(xs)   # [1, 2, 3, 4, 5]  (unchanged)
```

---

## map

**Signature:** `map(list: List<T>, function: function(T) -> U) -> List<U>`

Returns a new list with each element transformed by the given function. The output element type can differ from the input type. The original list is not modified. UFCS notation is also available.

```python
xs = [1, 2, 3]
ys = xs.map((x: int) => x * 2)
print(ys)   # [2, 4, 6]
```

---

## sort

**Signature:** `sort(list: List<T>) -> List<T>` / `sort(list: List<T>, comp: function(T, T) -> bool) -> List<T>`

Returns a new sorted list. Default is ascending order. An optional comparator function can be provided that returns `true` if the first argument should come before the second. The original list is not modified. The sort is **stable** (equal elements preserve their original order). UFCS notation is also available.

```python
xs = [3, 1, 2]
print(xs.sort())   # [1, 2, 3]

# Descending order
desc = xs.sort((a: int, b: int) => a > b)
print(desc)   # [3, 2, 1]
```

---

## sort!

**Signature:** `sort!(list: List<T>)` / `sort!(list: List<T>, comp: function(T, T) -> bool)`

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
ys = xs.iter().filter((x: int) => x > 2).to_list()
print(ys)   # [3, 4, 5]
```

---

## type_of

**Signature:** `type_of(expr: T) -> Type`

Returns the type of an expression as a [`Type`](types.md#type) value. Every distinct type definition (primitive, collection, record, enum, `Option`, `Result`, function, etc.) receives a unique identity at compile time, so `type_of` values can be compared by `==` to check whether two expressions share the same type.

- The argument is evaluated for side effects but only its static type is used.
- Printing a `Type` value via `print` or `to_str` yields the human-readable name (for example, `"int"`, `"List"`, `"Point"`).
- Two expressions with the same canonical type return equal `Type` values; different records (or a record and an enum that happen to share a name) are always distinguishable.
- The bare `none` literal reports as `"None"`. A typed `Option<T>` value (whether constructed via `Some(...)` or assigned from `none`) reports as `"Option"`.

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

# identity comparison
print(type_of(42) == type_of(100))  # true
print(type_of(42) == type_of(3.14)) # false
print(type_of(p) != type_of(c))     # true

# low-level numeric types are distinguished from `int`
x: i32 = 1
print(to_str(type_of(x)))           # i32
print(type_of(x) == type_of(42))    # false

# type_of is reflective: the type of a Type value is Type
print(to_str(type_of(type_of(42)))) # Type
```

### Type categories returned by `type_of`

| Input | `to_str(type_of(...))` |
|---|---|
| `42` | `int` |
| `3.14` | `float` |
| `true` / `false` | `bool` |
| `"hello"` | `str` |
| `[1, 2]` | `List` |
| `{"a": 1}` | `Map` |
| `{1, 2}` | `Set` |
| `x: i32 = 1` | `i32` (and similarly for `u8`, `i16`, …, `f32`) |
| record value | record name (e.g. `Point`) |
| enum value | enum name (e.g. `Color`) |
| `none` literal | `None` |
| `Some(1)` | `Option` |
| `x: Option<int> = none` | `Option` |
| `Ok(1)` / `Err(e)` | `Result` |
| lambda / closure | `function` |
| `type_of(x)` | `Type` |

> The bare `none` literal is reported as `"None"` to distinguish it from a typed `Option` value. Any `Option<T>` container — whether constructed via `Some(...)` or assigned from `none` to an `Option<T>`-typed binding — reports as `"Option"`.
