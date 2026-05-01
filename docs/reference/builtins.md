# Built-in Function Reference

## Function List

### Core

| Function | Description |
|------|------|
| `print()` / `print(expr1, expr2, ..., sep=" ", end="\n")` | Prints values to standard output. `sep` controls the separator (default: space), `end` controls the line ending (default: newline) |
| `input()` / `input(prompt)` | Reads one line from standard input and returns it as `str` with the trailing newline removed. With `prompt`, writes it to standard output first (no trailing newline) and flushes. Returns `""` on EOF |
| `len(value)` | Returns the number of elements in a list, map, or set, or the number of UTF-8 characters in a string |
| `range(n)` / `range(start, end)` / `range(start, end, step)` | Generates a list of integers |
| `exit(code)` | Terminates the process with the given exit code |
| `args()` | Returns command-line arguments as `List<str>` |
| `availableParallelism()` | Returns the runtime worker count as `int` |
| `sleep(durationMs)` | Suspends execution for the specified number of milliseconds |
| `env(key)` | Returns the environment variable as `Option<str>` |
| `env(key, default)` | Returns the environment variable, or `default` if not set |
| `send(stream, data)` | Sends `List<u8>` through `TcpStream` or `TlsStream`, returns `Result<int, Error>` |
| `receive(stream, max)` | Receives up to `max` bytes from `TcpStream` or `TlsStream` as `Result<List<u8>, Error>` |
| `close(handle)` | Closes a `TcpStream`, `TlsStream`, or `TcpListener` |
| `blockOn(task)` | Blocks the current thread until a `Task<T>` completes and returns its result |
| `toStr(value)` | Converts a value to its string representation. Supports `int`, `float` (shortest round-trip representation; whole numbers print with trailing `.0`), `bool`, `str`, record, enum, tuple, `List`, `Map`, `Set` (nested containers like `Map<str, List<int>>` are recursively formatted), `Result`, `Option`, union types (formatted as the active variant), and function values (printed as `<closure>`). String elements inside collections are wrapped in double quotes (e.g., `["hello", "world"]`) |
| `typeOf(expr)` | Returns the type of `expr` as a `Type` value. See [typeOf](#typeof) |
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
| `result.andThen(closure)` | If `Ok`, calls `closure` (which returns `Result<U, E>`); if `Err`, propagates the error |
| `result.map(closure)` | If `Ok`, applies `closure` to the value and wraps the return in `Ok`; if `Err`, propagates the error |

### Checked Arithmetic

All functions accept `int` or any low-level integer type (`i8`..`i64`, `u8`..`u64`). Both arguments must be the same type.

| Function | Description |
|------|------|
| `checkedAdd(a, b)` | Returns `Ok(a + b)` if no overflow, otherwise `Err(Error("arithmetic overflow"))` |
| `checkedSub(a, b)` | Returns `Ok(a - b)` if no overflow, otherwise `Err(Error("arithmetic overflow"))` |
| `checkedMul(a, b)` | Returns `Ok(a * b)` if no overflow, otherwise `Err(Error("arithmetic overflow"))` |
| `saturatingAdd(a, b)` | Returns `a + b`, clamped to operand type's min/max on overflow |
| `saturatingSub(a, b)` | Returns `a - b`, clamped to operand type's min/max on overflow |
| `saturatingMul(a, b)` | Returns `a * b`, clamped to operand type's min/max on overflow |
| `wrappingAdd(a, b)` | Returns `a + b` with wrapping on overflow |
| `wrappingSub(a, b)` | Returns `a - b` with wrapping on overflow |
| `wrappingMul(a, b)` | Returns `a * b` with wrapping on overflow |

### Collection Operations

> **Full reference**: Mutation semantics, CoW behavior, and examples for list operations live in [Collections — List](collections.md#list).

| Function | Description |
|------|------|
| `hasKey(map, key)` | Returns whether a key exists in the map |
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
| `removeAt(list, i)` | Removes and returns the element at index i |
| `items(map)` | Returns a list of (key, value) tuples |
| `remove(map, key)` | Removes the entry with the specified key |
| `get(map, key)` | Returns the value for key as `Option<V>` |
| `get(map, key, default)` | Returns the value for key, or default if not found |
| `union(set, set)` | Returns the union of two sets |
| `intersection(set, set)` | Returns the intersection of two sets |
| `difference(set, set)` | Returns the difference of two sets |
| `symmetricDifference(set, set)` | Returns the symmetric difference of two sets |
| `isSubset(set, set)` | Returns whether the first set is a subset of the second |
| `isSuperset(set, set)` | Returns whether the first set is a superset of the second |
| `first(list)` | Returns the first element as `Option<T>`, or `None` if empty |
| `last(list)` | Returns the last element as `Option<T>`, or `None` if empty |
| `remove(list, value)` | Removes the first occurrence of value from a list |
| `isEmpty(list / map / set / str)` | Returns whether the collection or string is empty |
| `distinct(list)` | Returns a new list with duplicates removed |
| `flat(list)` | Returns a new list with nested lists flattened |
| `reduce(list, fn)` | Reduces a list to `Option<T>` using the reducer function. Returns `None` on an empty list. For an explicit initial value, use `fold` |
| `fold(list, init, fn)` | Folds a list with an initial accumulator value. Returns `init` on an empty list |
| `any(list, pred)` | Returns `true` if any element matches the predicate |
| `all(list, pred)` | Returns `true` if all elements match the predicate |
| `sum(list)` | Returns the sum of all elements |
| `min(list)` | Returns the minimum element. Empty list is a runtime error |
| `max(list)` | Returns the maximum element. Empty list is a runtime error |
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
| `toList(iter)` | Collects all remaining iterator elements into a `List<T>` |
| `filter(iter, pred)` | Returns a lazy iterator that yields only elements matching the predicate |
| `map(iter, function)` | Returns a lazy iterator that transforms each element |
| `take(iter, count)` | Returns a lazy iterator that yields at most count elements |

### [String Operations](builtins-string.md)

| Function | Description |
|------|------|
| `contains(string, substring)` | Whether a substring is contained |
| `startsWith(string, prefix)` | Whether it starts with a prefix |
| `endsWith(string, suffix)` | Whether it ends with a suffix |
| `find(string, substring)` | Character position of a substring (`Option<int>`) |
| `byteLen(string)` | Returns the byte length of a string |
| `substr(string, start, end)` | Extract a substring |
| `charAt(string, i)` | Get the character at a specified position |
| `replace(string, old, new)` | Replace all occurrences of a substring |
| `toUpper(string)` / `toLower(string)` | Uppercase / lowercase conversion |
| `trim(string)` / `trimStart(string)` / `trimEnd(string)` | Whitespace removal |
| `repeat(string, count)` | Repeat a string n times |
| `reverse(string)` | Reverse a string |
| `split(string, delimiter = " ")` | Split a string into a list |
| `join(list, sep)` | Join list elements with a separator |
| `toInt(s)` / `toFloat(s)` / `toStr(v)` | Type conversion (`toInt` and `toFloat` return `Result<T, Error>`) |

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

```ry
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

## input

**Signature:** `input() -> str` / `input(prompt: str) -> str`

Reads one line from standard input and returns it as a string with the trailing newline (`\n`) removed. Returns an empty string when EOF is reached. When `prompt` is provided, it is written to standard output (with no appended newline) and stdout is flushed before blocking on stdin — mirroring Python's `input(prompt)`.

Equivalent to `io.readLine()` but available as a bare builtin without `import`. Use `input` for short scripts and competitive-programming snippets; use `io.readLine` when explicitly scoping I/O through the `io` module.

```ry
name = input("Enter your name: ")
print(f"Hello, {name}!")

# No prompt — reads one line from stdin as-is
line = input()
```

---

## Some

**Signature:** `Some(expr) -> Option<T>`

Constructs the value-present variant of an Option type.

```ry
x: Option<int> = Some(42)
print(x)   # Some(42)
```

---

## len

**Signature:** `len(x: List<T> | Map<K, V> | Set<T> | str) -> int`

Returns the number of elements in a list, map, or set, or the number of UTF-8 characters in a string. Use `byteLen()` for the byte length.

```ry
print(len([1, 2, 3]))         # 3
print(len({"a": 1, "b": 2})) # 2
print(len({1, 2, 3}))         # 3
print(len("hello"))           # 5
print(len("あいう"))           # 3 (UTF-8 characters)
```

---

## hasKey

**Signature:** `hasKey(m: Map<K, V>, key: K) -> bool`

Returns whether a specified key exists in the map. UFCS notation is also available.

```ry
m = {"a": 1, "b": 2}
print(hasKey(m, "a"))    # true
print(m.hasKey("z"))     # false (UFCS)
```

`contains(m, key)` is equivalent to `hasKey(m, key)` for maps.

---

## add

**Signature:** `add(s: Set<T>, value: T)`

Adds an element to a set. Does nothing if the element already exists. UFCS notation is also available.

```ry
s = {1, 2, 3}
s.add(4)          # UFCS
add(s, 5)         # Normal call
s.add(1)          # Ignored because it already exists
print(len(s))     # 5
```

---

## remove

**Signature:** `remove(s: Set<T>, value: T)`

Removes an element from a set. UFCS notation is also available.

```ry
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

```ry
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

```ry
exit(0)        # normal termination
exit(1)        # error termination

print("a")
exit(0)
print("b")     # never prints — unreachable after exit
```

The same treatment applies to `return`, `break`, and `continue` — code
after any diverging control-flow statement is silently elided.

---

## args

**Signature:** `args() -> List<str>`

Returns the command-line arguments passed to the script as a list of strings. Does not include the interpreter name or the script filename — only the arguments after the script path.

```ry
# Run: ry script.ry hello world
a = args()
print(len(a))    # 2
print(a[0])      # hello
print(a[1])      # world

for x in args():
    print(x)
```

---

## sleep

**Signature:** `sleep(durationMs: int) -> Unit`

Suspends execution of the current thread for the specified number of milliseconds. If `durationMs` is 0 or negative, the function returns immediately.

```ry
sleep(1000)    # wait 1 second
sleep(0)       # returns immediately
```

---

## env

**Signature:** `env(key: str) -> Option<str>` / `env(key: str, default: str) -> str`

Returns the value of an environment variable. The one-argument form returns `Option<str>` (`Some(value)` if set, `None` if not). The two-argument form returns the value or `default` if the variable is not set.

If a `.env` file exists in the project root (the directory containing `package.toml`), its entries are automatically loaded into the process environment at startup. Existing environment variables are not overwritten by `.env` values.

> **Security note:** `.env` files typically contain secrets (API keys, database passwords, tokens, etc.). Do **not** commit `.env` to version control (add it to `.gitignore` or equivalent), and treat its contents as sensitive configuration.

```ry
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

See [RY_ENV](modules.md#ry_env) for details on environment modes.

---

## append

**Signature:** `append(list: List<T>, value: T)`

Adds an element to the end of a list. This is a mutating operation — the list is modified in place. UFCS notation is also available.

```ry
xs = [1, 2]
xs.append(3)
print(xs)   # [1, 2, 3]
```

---

## pop

**Signature:** `pop(list: List<T>) -> Option<T>`

Removes and returns the last element of a list as `Option<T>`. Returns `None` if the list is empty. UFCS notation is also available.

```ry
xs = [1, 2, 3]
v = xs.pop()
print(v)    # Some(3)
print(xs)   # [1, 2]
```

---

## reverse (list)

**Signature:** `reverse(list: List<T>) -> List<T>`

Returns a new list with elements in reverse order. The original list is not modified. Also works on strings (see [String Operations](builtins-string.md)). UFCS notation is also available.

```ry
xs = [1, 2, 3]
ys = reverse(xs)
print(ys)   # [3, 2, 1]
print(xs)   # [1, 2, 3] (unchanged)
```

---

## slice

**Signature:** `slice(list: List<T>, start: int, end: int) -> List<T>`

Returns a new sub-list covering `[start, end)` (end exclusive). Negative indices are resolved as `len(list) + idx` (Python-style, consistent with `lst[-1]` and `lst[a..b]`). The resolved range is then silently clamped to `[0, len(list)]`. UFCS notation is also available.

```ry
xs = [1, 2, 3, 4, 5]
print(slice(xs, 1, 3))     # [2, 3]
print(slice(xs, 0, 100))   # [1, 2, 3, 4, 5] (end clamped)
print(slice(xs, -2, 5))    # [4, 5]  (negative start wraps to index 3)
print(slice(xs, -4, -1))   # [2, 3, 4]  (both bounds wrap)
```

---

## take

**Signature:** `take(list: List<T>, count: int) -> List<T>`

Returns a new list with the first `count` elements. If `count` exceeds the list length, returns a copy of the entire list. If `count <= 0`, returns an empty list. The original list is not modified. UFCS notation is also available.

```ry
xs = [1, 2, 3, 4, 5]
ys = xs.take(3)
print(ys)   # [1, 2, 3]
print(xs.take(10))   # [1, 2, 3, 4, 5] (clamped)
print(xs.take(0))    # []
```

---

## tap

**Signature:** `tap(list: List<T>, function: fn(T) -> R) -> List<T>`

Calls the given function on each element (ignoring any return value), then returns the original list unchanged. Useful for debugging or inserting side effects in a method chain. UFCS notation is also available.

```ry
xs = [1, 2, 3]
ys = xs.tap((x: int) => print(x)).map((x: int) => x * 2)
# prints 1, 2, 3, then ys = [2, 4, 6]
```

---

## filter

**Signature:** `filter(list: List<T>, pred: fn(T) -> bool) -> List<T>`

> **See also**: [Collections — filter](collections.md#filter) for full semantics and examples. UFCS notation is also available.

---

## map

**Signature:** `map(list: List<T>, function: fn(T) -> U) -> List<U>`

> **See also**: [Collections — map](collections.md#map) for full semantics and examples. UFCS notation is also available.

---

## sort

**Signature:** `sort(list: List<T>) -> List<T>` / `sort(list: List<T>, comp: fn(T, T) -> bool) -> List<T>`

> **See also**: [Collections — sort](collections.md#sort) for full semantics and examples. UFCS notation is also available.

---

## sort!

**Signature:** `sort!(list: List<T>)` / `sort!(list: List<T>, comp: fn(T, T) -> bool)`

> **See also**: [Collections — In-Place Mutating Variants](collections.md#in-place-mutating-variants) for full semantics and examples. UFCS notation is also available.

---

## reverse!

**Signature:** `reverse!(list: List<T>)`

> **See also**: [Collections — In-Place Mutating Variants](collections.md#in-place-mutating-variants) for full semantics and examples. UFCS notation is also available.

---

## appended

**Signature:** `appended(list: List<T>, value: T) -> List<T>`

> **See also**: [Collections — In-Place Mutating Variants](collections.md#in-place-mutating-variants) for full semantics and examples. UFCS notation is also available.

---

## append!

**Signature:** `append!(list: List<T>, value: T)`

Alias for `append()`. Adds an element to the end of a list in place. Provided for naming consistency with the `!` convention.

> **See also**: [Collections — In-Place Mutating Variants](collections.md#in-place-mutating-variants) for full semantics and examples. UFCS notation is also available.

---

## first

**Signature:** `first(list: List<T>) -> Option<T>`

Returns the first element of a list as `Option<T>`. Returns `None` if the list is empty.

```ry
print(first([10, 20, 30]))   # Some(10)
```

---

## last

**Signature:** `last(list: List<T>) -> Option<T>`

Returns the last element of a list as `Option<T>`. Returns `None` if the list is empty.

```ry
print(last([10, 20, 30]))   # Some(30)
```

---

## get (Map)

**Signature:** `get(map: Map<K, V>, key: K) -> Option<V>` / `get(map: Map<K, V>, key: K, default: V) -> V`

Two-argument form returns the value for key as `Option<V>`. Three-argument form returns the value or the default.

```ry
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

```ry
xs = [1, 2, 3]
it = xs.iter()           # Iterator<int>
ys = it.toList()        # [1, 2, 3]

m = {"a": 1, "b": 2}
for k, v in m.iter():        # Iterator<(str, int)>
    print(k)
```

---

## next

**Signature:** `next(iter: Iterator<T>) -> Option<T>`

Returns the next element from the iterator as `Option<T>`. Returns `None` when the iterator is exhausted. The iterator advances its internal state on each call. UFCS notation is also available.

```ry
it = [10, 20].iter()
print(it.next())   # Some(10)
print(it.next())   # Some(20)
print(it.next())   # None
```

---

## toList

**Signature:** `toList(iter: Iterator<T>) -> List<T>`

Collects all remaining elements from the iterator into a new list. UFCS notation is also available.

```ry
xs = [1, 2, 3, 4, 5]
ys = xs.iter().filter((x: int) => x > 2).toList()
print(ys)   # [3, 4, 5]
```

---

## typeOf

**Signature:** `typeOf(expr: T) -> Type`

Returns the type of an expression as a [`Type`](types.md#type) value. Every distinct type definition (primitive, collection, record, enum, `Option`, `Result`, function, etc.) receives a unique identity at compile time, so `typeOf` values can be compared by `==` to check whether two expressions share the same type.

- The argument is evaluated for side effects but only its static type is used.
- Printing a `Type` value via `print` or `toStr` yields the human-readable name (for example, `"int"`, `"List"`, `"Point"`).
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

print(toStr(typeOf(42)))          # int
print(toStr(typeOf(3.14)))        # float
print(toStr(typeOf("hello")))     # str
print(toStr(typeOf([1, 2, 3])))   # List
print(toStr(typeOf({"a": 1})))    # Map
print(toStr(typeOf({1, 2})))      # Set

p = Point(1, 2)
print(toStr(typeOf(p)))           # Point

c = Color::Red
print(toStr(typeOf(c)))           # Color

# identity comparison
print(typeOf(42) == typeOf(100))  # true
print(typeOf(42) == typeOf(3.14)) # false
print(typeOf(p) != typeOf(c))     # true

# low-level numeric types are distinguished from `int`
x: i32 = 1
print(toStr(typeOf(x)))           # i32
print(typeOf(x) == typeOf(42))    # false

# typeOf is reflective: the type of a Type value is Type
print(toStr(typeOf(typeOf(42)))) # Type
```

### Type categories returned by `typeOf`

| Input | `toStr(typeOf(...))` |
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
| lambda / closure | `fn` |
| `typeOf(x)` | `Type` |

> The bare `none` literal is reported as `"None"` to distinguish it from a typed `Option` value. Any `Option<T>` container — whether constructed via `Some(...)` or assigned from `none` to an `Option<T>`-typed binding — reports as `"Option"`.
