[English](builtins-string.md) | [日本語](../ja/reference/builtins-string.md) | [繁體中文](../zh/reference/builtins-string.md)

# String Operation Function Reference

A list of operation functions for strings (`str`). All functions support UFCS notation.

> **Note:** All string operations are UTF-8 aware. `length()`, `char_at()`, `substring()`, `find()`, and `reverse()` operate on Unicode code points, not bytes. Use `byte_len()` if you need the byte length.

## Function List

### Search and Check

| Function | Signature | Description |
|------|-----------|------|
| `contains` | `(str, str, bool = false) -> bool` | Returns whether a substring is contained |
| `starts_with` | `(str, str, bool = false) -> bool` | Returns whether it starts with a prefix |
| `ends_with` | `(str, str, bool = false) -> bool` | Returns whether it ends with a suffix |
| `find` | `(str, str) -> Option<int>` | Returns the character position of a substring (`None` if not found) |

### Extraction and Transformation

| Function | Signature | Description |
|------|-----------|------|
| `substring` | `(str, int, int) -> str` | Extract a substring (character indices) |
| `char_at` | `(str, int) -> str` | Get the UTF-8 character at a specified position |
| `replace` | `(str, str, str) -> str` | Replace all occurrences of a substring |

### Case Conversion

| Function | Signature | Description |
|------|-----------|------|
| `to_upper` | `str -> str` | Convert to ASCII uppercase |
| `to_lower` | `str -> str` | Convert to ASCII lowercase |

### Whitespace Removal

| Function | Signature | Description |
|------|-----------|------|
| `trim` | `str -> str` | Remove leading and trailing whitespace |
| `trim_start` | `str -> str` | Remove leading whitespace |
| `trim_end` | `str -> str` | Remove trailing whitespace |

### Generation and Processing

| Function | Signature | Description |
|------|-----------|------|
| `repeat` | `(str, int) -> str` | Repeat a string n times |
| `reverse` | `str -> str` | Reverse a string (UTF-8 aware) |
| `byte_len` | `str -> int` | Returns the byte length of a string |

### Split and Join

| Function | Signature | Description |
|------|-----------|------|
| `split` | `(str, str) -> List<str>` | Split by delimiter |
| `join` | `(List<str>, str) -> str` | Join with separator |

### Type Conversion

| Function | Signature | Description |
|------|-----------|------|
| `to_int` | `str -> Result<int, Error>` | Convert string to integer |
| `to_float` | `str -> Result<float, Error>` | Convert string to floating-point number |
| `to_str` | `int/float/bool/str/enum/record -> str` | Convert value to string |

---

## contains

**Signature:** `contains(string: str, substring: str, ignore_case: bool = false) -> bool`

Returns whether string `string` contains the substring `substring`. When `ignore_case` is `true`, the comparison is case-insensitive (ASCII only).

```python
print(contains("hello", "ell"))              # true
print("hello".contains("xyz"))               # false (UFCS)
print(contains("Hello World", "hello", true))  # true (case-insensitive)
```

---

## starts_with

**Signature:** `starts_with(string: str, prefix: str, ignore_case: bool = false) -> bool`

Returns whether string `string` starts with `prefix`. When `ignore_case` is `true`, the comparison is case-insensitive (ASCII only).

```python
print(starts_with("hello", "hel"))              # true
print("hello".starts_with("world"))              # false (UFCS)
print(starts_with("Hello", "hello", true))  # true (case-insensitive)
```

---

## ends_with

**Signature:** `ends_with(string: str, suffix: str, ignore_case: bool = false) -> bool`

Returns whether string `string` ends with `suffix`. When `ignore_case` is `true`, the comparison is case-insensitive (ASCII only).

```python
print(ends_with("hello", "llo"))              # true
print("hello".ends_with("world"))              # false (UFCS)
print(ends_with("Hello World", "WORLD", true))  # true (case-insensitive)
```

---

## find

**Signature:** `find(string: str, substring: str) -> Option<int>`

Returns the character position of the first occurrence of substring `substring` in string `string`. Returns `None` if not found.

```python
print(find("hello world", "world"))   # Some(6)
print(find("hello", "xyz"))           # None
print("abcdef".find("cd"))            # Some(2) (UFCS)
```

---

## substring

**Signature:** `substring(string: str, start: int, end: int) -> str`

Returns the substring of `string` from `start` to `end` (exclusive). Indices are character positions (UTF-8 aware).

Out-of-range indices are clamped to `[0, length]`. If `end < start` after clamping, returns an empty string.

```python
print(substring("hello world", 0, 5))   # hello
print(substring("hello world", 6, 11))  # world
print("abcdef".substring(1, 4))         # bcd (UFCS)
print(substring("hello", -1, 100))      # hello (clamped)
```

---

## char_at

**Signature:** `char_at(string: str, i: int) -> str`

Returns the UTF-8 character at position `i` in string `string` as a string. Raises a runtime error if the index is out of bounds.

Negative indices wrap around from the end (Python-style): `-1` refers to the last character, `-2` to the second-to-last, and so on.

```python
print(char_at("hello", 0))    # h
print(char_at("hello", -1))   # o (last character)
print("abc".char_at(2))       # c (UFCS)
```

---

## replace

**Signature:** `replace(string: str, old: str, new: str) -> str`

Returns a new string with all occurrences of `old` in `string` replaced with `new`.

If `old` is an empty string, the input is returned unchanged (as a fresh copy).

```python
print(replace("hello world", "world", "ry"))   # hello ry
print(replace("aaa", "a", "bb"))                # bbbbbb
print("foo bar foo".replace("foo", "baz"))      # baz bar baz (UFCS)
print(replace("hello", "", "X"))                # hello (empty pattern is a no-op)
```

---

## to_upper

**Signature:** `to_upper(string: str) -> str`

Returns a new string with ASCII lowercase letters (a-z) converted to uppercase.

```python
print(to_upper("hello"))         # HELLO
print("Hello World".to_upper())  # HELLO WORLD (UFCS)
```

---

## to_lower

**Signature:** `to_lower(string: str) -> str`

Returns a new string with ASCII uppercase letters (A-Z) converted to lowercase.

```python
print(to_lower("HELLO"))         # hello
print("Hello World".to_lower())  # hello world (UFCS)
```

---

## trim

**Signature:** `trim(string: str) -> str`

Returns a new string with leading and trailing whitespace characters (spaces, tabs, newlines, carriage returns) removed.

```python
print(trim("  hello  "))   # hello
print("  hi  ".trim())     # hi (UFCS)
```

---

## trim_start

**Signature:** `trim_start(string: str) -> str`

Returns a new string with leading whitespace characters removed.

```python
print(trim_start("  hello  "))   # hello
print("  hi".trim_start())       # hi (UFCS)
```

---

## trim_end

**Signature:** `trim_end(string: str) -> str`

Returns a new string with trailing whitespace characters removed.

```python
print(trim_end("  hello  "))   #   hello
print("hi  ".trim_end())       # hi (UFCS)
```

---

## repeat

**Signature:** `repeat(string: str, count: int) -> str`

Returns a new string with `string` repeated `count` times.

```python
print(repeat("ab", 3))     # ababab
print("ha".repeat(3))      # hahaha (UFCS)
```

---

## reverse

**Signature:** `reverse(string: str) -> str`

Returns a new string with the characters reversed (UTF-8 aware).

```python
print(reverse("hello"))    # olleh
print("abc".reverse())     # cba (UFCS)
```

---

## byte_len

**Signature:** `byte_len(string: str) -> int`

Returns the byte length of string `string`. Unlike `length()`, which returns the number of UTF-8 characters, `byte_len()` returns the number of bytes.

```python
print(byte_len("hello"))   # 5
print(byte_len("あいう"))   # 9
print(length("あいう"))        # 3 (characters)
```

---

## split

**Signature:** `split(string: str, delimiter: str) -> List<str>`

Splits string `string` by delimiter `delimiter` and returns a `List<str>`.

When the delimiter is an empty string `""`, the string is split into individual characters (UTF-8 aware).

```python
parts = split("a,b,c", ",")
print(parts[0])   # a
print(parts[1])   # b
print(parts[2])   # c

for word in "hello world".split(" "):
    print(word)
# hello
# world

# Split into characters
chars = split("hello", "")
print(chars)   # [h, e, l, l, o]

# UTF-8 characters
chars = split("あいう", "")
print(chars)   # [あ, い, う]
```

---

## join

**Signature:** `join(values: List<str>, sep: str) -> str`

Joins the elements of a string list with separator `sep` and returns a string.

```python
parts = ["a", "b", "c"]
print(join(parts, ","))        # a,b,c
print(parts.join("-"))         # a-b-c (UFCS)
print(",".join(parts))         # a,b,c (UFCS, Python-style)
```

---

## to_int

**Signature:** `to_int(string: str) -> Result<int, Error>`

Converts a string to an integer. Leading whitespace is allowed. Returns `Err` if the string is empty, contains invalid characters, or overflows.

```python
match to_int("42"):
    case Ok(v):
        print(v)              # 42
    case Err(e):
        print(e.message)

match "123".to_int():          # UFCS
    case Ok(v):
        print(v)              # 123
    case Err(e):
        print(e.message)

# Invalid input returns Err
print(to_int("abc"))          # Err(Error("to_int: invalid character in 'abc'"))
print(to_int(""))             # Err(Error("to_int: empty string"))
```

---

## to_float

**Signature:** `to_float(string: str) -> Result<float, Error>`

Converts a string to a floating-point number. Returns `Err` if the string is empty, contains invalid characters, or is out of range for `float`.

```python
match to_float("3.14"):
    case Ok(v):
        print(v)              # 3.14
    case Err(e):
        print(e.message)

match "2.5".to_float():        # UFCS
    case Ok(v):
        print(v)              # 2.5
    case Err(e):
        print(e.message)

# Invalid input returns Err
print(to_float("abc"))         # Err(Error("to_float: invalid character in 'abc'"))
print(to_float(""))            # Err(Error("to_float: empty string"))
print(to_float("1e400"))       # Err(Error("to_float: out of range in '1e400'"))
```

---

## to_str

**Signature:** `to_str(v: int | float | bool | str | enum | record) -> str`

Converts a value to a string.

| Type | Conversion Format |
|----|---------|
| `int` | `%ld` |
| `float` | `%g`, with trailing `.0` for whole-number values (e.g. `"3.0"`, `"0.0"`) |
| `bool` | `"true"` / `"false"` |
| `str` | Returned as-is |
| enum | Variant name (e.g. `"Red"`) |
| record | `TypeName(field1: val1, field2: val2)` |
| `List` / `Map` / `Set` | Recursively formatted, nested containers (e.g. `Map<str, List<int>>`) are supported |
| union | Formatted as the active variant; `List`, `Map`, `Set`, and function variants are all supported |
| function value (closure / lambda) | `"<closure>"` |

Whole-number `float` values are formatted with a trailing `.0` (e.g. `to_str(3.0) == "3.0"`) so they are visually distinguishable from `int`. Record types automatically generate a `to_str` representation. If a user-defined `function to_str(v: MyRecord) -> str` is provided, it takes precedence over the auto-generated version. This also works with `print()` and f-string interpolation.

```python
print(to_str(42))         # 42
print(to_str(3.14))       # 3.14
print(to_str(3.0))        # 3.0          (whole-number float keeps .0)
print(to_str(true))       # true
print(99.to_str())        # 99 (UFCS)

enum Color:
    Red
    Green

print(to_str(Color::Red))   # Red

record Point:
    x: int
    y: int

p = Point(10, 20)
print(to_str(p))          # Point(x: 10, y: 20)
print(p)                  # Point(x: 10, y: 20)
print(f"pos={p}")         # pos=Point(x: 10, y: 20)
```
