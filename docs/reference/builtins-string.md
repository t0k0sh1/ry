[English](builtins-string.md) | [日本語](../ja/reference/builtins-string.md) | [繁體中文](../zh/reference/builtins-string.md)

# String Operation Function Reference

A list of operation functions for strings (`str`). All functions support UFCS notation.

> **Note:** All string operations are byte-level. They may not work correctly with multi-byte characters (e.g., CJK characters).

## Function List

### Search and Check

| Function | Signature | Description |
|------|-----------|------|
| `contains` | `(str, str) -> bool` | Returns whether a substring is contained |
| `starts_with` | `(str, str) -> bool` | Returns whether it starts with a prefix |
| `ends_with` | `(str, str) -> bool` | Returns whether it ends with a suffix |
| `find` | `(str, str) -> int` | Returns the position of a substring (-1 if not found) |

### Extraction and Transformation

| Function | Signature | Description |
|------|-----------|------|
| `substring` | `(str, int, int) -> str` | Extract a substring |
| `char_at` | `(str, int) -> str` | Get the character at a specified position |
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
| `reverse` | `str -> str` | Reverse a string |

### Split and Join

| Function | Signature | Description |
|------|-----------|------|
| `split` | `(str, str) -> List<str>` | Split by delimiter |
| `join` | `(List<str>, str) -> str` | Join with separator |

### Type Conversion

| Function | Signature | Description |
|------|-----------|------|
| `to_int` | `str -> int` | Convert string to integer |
| `to_float` | `str -> float` | Convert string to floating-point number |
| `to_str` | `int/float/bool/str -> str` | Convert value to string |

---

## contains

**Signature:** `contains(s: str, sub: str) -> bool`

Returns whether string `s` contains the substring `sub`.

```python
print(contains("hello", "ell"))   # true
print("hello".contains("xyz"))    # false (UFCS)
```

---

## starts_with

**Signature:** `starts_with(s: str, prefix: str) -> bool`

Returns whether string `s` starts with `prefix`.

```python
print(starts_with("hello", "hel"))   # true
print("hello".starts_with("world"))  # false (UFCS)
```

---

## ends_with

**Signature:** `ends_with(s: str, suffix: str) -> bool`

Returns whether string `s` ends with `suffix`.

```python
print(ends_with("hello", "llo"))   # true
print("hello".ends_with("world"))  # false (UFCS)
```

---

## find

**Signature:** `find(s: str, sub: str) -> int`

Returns the position (byte offset) of the first occurrence of substring `sub` in string `s`. Returns `-1` if not found.

```python
print(find("hello world", "world"))   # 6
print(find("hello", "xyz"))           # -1
print("abcdef".find("cd"))            # 2 (UFCS)
```

---

## substring

**Signature:** `substring(s: str, start: int, end: int) -> str`

Returns the substring of `s` from `start` to `end` (exclusive).

```python
print(substring("hello world", 0, 5))   # hello
print(substring("hello world", 6, 11))  # world
print("abcdef".substring(1, 4))         # bcd (UFCS)
```

---

## char_at

**Signature:** `char_at(s: str, i: int) -> str`

Returns the byte at position `i` in string `s` as a single-character string.

```python
print(char_at("hello", 0))   # h
print("abc".char_at(2))       # c (UFCS)
```

---

## replace

**Signature:** `replace(s: str, old: str, new: str) -> str`

Returns a new string with all occurrences of `old` in `s` replaced with `new`.

```python
print(replace("hello world", "world", "ry"))   # hello ry
print(replace("aaa", "a", "bb"))                # bbbbbb
print("foo bar foo".replace("foo", "baz"))      # baz bar baz (UFCS)
```

---

## to_upper

**Signature:** `to_upper(s: str) -> str`

Returns a new string with ASCII lowercase letters (a-z) converted to uppercase.

```python
print(to_upper("hello"))         # HELLO
print("Hello World".to_upper())  # HELLO WORLD (UFCS)
```

---

## to_lower

**Signature:** `to_lower(s: str) -> str`

Returns a new string with ASCII uppercase letters (A-Z) converted to lowercase.

```python
print(to_lower("HELLO"))         # hello
print("Hello World".to_lower())  # hello world (UFCS)
```

---

## trim

**Signature:** `trim(s: str) -> str`

Returns a new string with leading and trailing whitespace characters (spaces, tabs, newlines, carriage returns) removed.

```python
print(trim("  hello  "))   # hello
print("  hi  ".trim())     # hi (UFCS)
```

---

## trim_start

**Signature:** `trim_start(s: str) -> str`

Returns a new string with leading whitespace characters removed.

```python
print(trim_start("  hello  "))   # hello
print("  hi".trim_start())       # hi (UFCS)
```

---

## trim_end

**Signature:** `trim_end(s: str) -> str`

Returns a new string with trailing whitespace characters removed.

```python
print(trim_end("  hello  "))   #   hello
print("hi  ".trim_end())       # hi (UFCS)
```

---

## repeat

**Signature:** `repeat(s: str, n: int) -> str`

Returns a new string with `s` repeated `n` times.

```python
print(repeat("ab", 3))     # ababab
print("ha".repeat(3))      # hahaha (UFCS)
```

---

## reverse

**Signature:** `reverse(s: str) -> str`

Returns a new string with the bytes reversed.

```python
print(reverse("hello"))    # olleh
print("abc".reverse())     # cba (UFCS)
```

---

## split

**Signature:** `split(s: str, delim: str) -> List<str>`

Splits string `s` by delimiter `delim` and returns a `List<str>`.

```python
let parts = split("a,b,c", ",")
print(parts[0])   # a
print(parts[1])   # b
print(parts[2])   # c

for word in "hello world".split(" "):
    print(word)
# hello
# world
```

---

## join

**Signature:** `join(xs: List<str>, sep: str) -> str`

Joins the elements of a string list with separator `sep` and returns a string.

```python
let parts = ["a", "b", "c"]
print(join(parts, ","))        # a,b,c
print(parts.join("-"))         # a-b-c (UFCS)
```

---

## to_int

**Signature:** `to_int(s: str) -> int`

Converts a string to an integer.

```python
print(to_int("42"))       # 42
print(to_int("-7"))       # -7
print("123".to_int())     # 123 (UFCS)
```

---

## to_float

**Signature:** `to_float(s: str) -> float`

Converts a string to a floating-point number.

```python
print(to_float("3.14"))   # 3.14
print("2.5".to_float())   # 2.5 (UFCS)
```

---

## to_str

**Signature:** `to_str(v: int | float | bool | str) -> str`

Converts a value to a string.

| Type | Conversion Format |
|----|---------|
| `int` | `%ld` |
| `float` | `%g` |
| `bool` | `"true"` / `"false"` |
| `str` | Returned as-is |

```python
print(to_str(42))         # 42
print(to_str(3.14))       # 3.14
print(to_str(true))       # true
print(99.to_str())        # 99 (UFCS)
```
