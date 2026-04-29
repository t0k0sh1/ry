# String Operation Function Reference

A list of operation functions for strings (`str`). All functions support UFCS notation.

> **Note:** All string operations are UTF-8 aware. `len()`, `charAt()`, `substr()`, `find()`, and `reverse()` operate on Unicode code points, not bytes. Use `byteLen()` if you need the byte length.
>
> **NUL bytes:** `str` stores an explicit byte length and supports embedded NUL bytes (`\0`). All string operations are fully NUL-safe: `byteLen`, `len`, `==`, `!=`, `<`, `>`, `+`, `*`, hash/Map/Set key lookup (#1022), `contains`, `startsWith`, `endsWith`, `find` (#1047), `replace` (#1048), `substr`, `charAt`, `reverse`, `split("", _)`, `for c in str:`, `enumerate(str)` (#1049), `toUpper`, `toLower`, `trim`, `trimStart`, `trimEnd` (#1050), `split(str, delim)` with non-empty delimiter, `join`, `repeat`, `*` (#1051).
>
> **Index access:** `str` does not support `[]` index syntax. Use `charAt(s, i)` to get the character at position `i`.

## Function List

### Search and Check

| Function | Signature | Description |
|------|-----------|------|
| `contains` | `(str, str, bool = false) -> bool` | Returns whether a substring is contained |
| `startsWith` | `(str, str, bool = false) -> bool` | Returns whether it starts with a prefix |
| `endsWith` | `(str, str, bool = false) -> bool` | Returns whether it ends with a suffix |
| `find` | `(str, str) -> Option<int>` | Returns the character position of a substring (`None` if not found) |

### Extraction and Transformation

| Function | Signature | Description |
|------|-----------|------|
| `substr` | `(str, int, int) -> str` | Extract a substring (character indices) |
| `charAt` | `(str, int) -> str` | Get the UTF-8 character at a specified position |
| `replace` | `(str, str, str) -> str` | Replace all occurrences of a substring |

### Case Conversion

| Function | Signature | Description |
|------|-----------|------|
| `toUpper` | `str -> str` | Convert to ASCII uppercase |
| `toLower` | `str -> str` | Convert to ASCII lowercase |

### Whitespace Removal

| Function | Signature | Description |
|------|-----------|------|
| `trim` | `str -> str` | Remove leading and trailing whitespace |
| `trimStart` | `str -> str` | Remove leading whitespace |
| `trimEnd` | `str -> str` | Remove trailing whitespace |

### Generation and Processing

| Function | Signature | Description |
|------|-----------|------|
| `repeat` | `(str, int) -> str` | Repeat a string n times |
| `reverse` | `str -> str` | Reverse a string (UTF-8 aware) |
| `byteLen` | `str -> int` | Returns the byte length of a string |

### Split and Join

| Function | Signature | Description |
|------|-----------|------|
| `split` | `(str, str = " ") -> List<str>` | Split by delimiter |
| `join` | `(List<str>, str) -> str` | Join with separator |

### Type Conversion

| Function | Signature | Description |
|------|-----------|------|
| `toInt` | `str -> Result<int, Error>` | Convert string to integer |
| `toFloat` | `str -> Result<float, Error>` | Convert string to floating-point number |
| `toStr` | `any -> str` | Convert value to string |

---

## contains

**Signature:** `contains(string: str, substring: str, ignoreCase: bool = false) -> bool`

Returns whether string `string` contains the substring `substring`. When `ignoreCase` is `true`, the comparison is case-insensitive (ASCII only). Both `string` and `substring` may contain embedded NUL bytes (`\0`).

```ry
print(contains("hello", "ell"))              # true
print("hello".contains("xyz"))               # false (UFCS)
print(contains("Hello World", "hello", true))  # true (case-insensitive)
print(contains("a\0b", "\0b"))               # true (NUL-safe)
```

---

## startsWith

**Signature:** `startsWith(string: str, prefix: str, ignoreCase: bool = false) -> bool`

Returns whether string `string` starts with `prefix`. When `ignoreCase` is `true`, the comparison is case-insensitive (ASCII only). Both arguments may contain embedded NUL bytes (`\0`).

```ry
print(startsWith("hello", "hel"))              # true
print("hello".startsWith("world"))              # false (UFCS)
print(startsWith("Hello", "hello", true))  # true (case-insensitive)
print(startsWith("a\0b", "a\0"))            # true (NUL-safe)
```

---

## endsWith

**Signature:** `endsWith(string: str, suffix: str, ignoreCase: bool = false) -> bool`

Returns whether string `string` ends with `suffix`. When `ignoreCase` is `true`, the comparison is case-insensitive (ASCII only). Both arguments may contain embedded NUL bytes (`\0`).

```ry
print(endsWith("hello", "llo"))              # true
print("hello".endsWith("world"))              # false (UFCS)
print(endsWith("Hello World", "WORLD", true))  # true (case-insensitive)
print(endsWith("a\0b", "\0b"))               # true (NUL-safe)
```

---

## find

**Signature:** `find(string: str, substring: str) -> Option<int>`

Returns the character position of the first occurrence of substring `substring` in string `string`. Returns `None` if not found. Both arguments may contain embedded NUL bytes (`\0`); the returned index counts Unicode code points (NUL counts as one code point).

```ry
print(find("hello world", "world"))   # Some(6)
print(find("hello", "xyz"))           # None
print("abcdef".find("cd"))            # Some(2) (UFCS)
print(find("a\0b", "\0"))             # Some(1) (NUL-safe)
```

---

## substr

**Signature:** `substr(string: str, start: int, end: int) -> str`

Returns the substring of `string` from `start` to `end` (exclusive). Indices are character positions (UTF-8 aware).

Negative indices wrap Python-style: `-1` refers to the last character, `-2` to the second-to-last, etc. (`length + idx`). Indices are then clamped to `[0, length]`. If `end < start` after these adjustments, returns an empty string.

```ry
print(substr("hello world", 0, 5))       # hello
print(substr("hello world", 6, 11))      # world
print("abcdef".substr(1, 4))             # bcd (UFCS)
print(substr("Hello, World", -5, 12))    # World       (-5 wraps to 7)
print(substr("Hello, World", 0, -1))     # Hello, Worl (-1 wraps to 11)
print(substr("a\0b", 0, 3))              # "a\0b" (NUL byte is preserved)
```

---

## charAt

**Signature:** `charAt(string: str, i: int) -> str`

Returns the UTF-8 character at position `i` in string `string` as a string. Raises a runtime error if the index is out of bounds.

Negative indices wrap around from the end (Python-style): `-1` refers to the last character, `-2` to the second-to-last, and so on.

```ry
print(charAt("hello", 0))    # h
print(charAt("hello", -1))   # o (last character)
print("abc".charAt(2))       # c (UFCS)
print(charAt("a\0b", 1))     # "\0" (NUL byte is a valid character at index 1)
```

---

## replace

**Signature:** `replace(string: str, old: str, new: str) -> str`

Returns a new string with all occurrences of `old` in `string` replaced with `new`. All three arguments may contain embedded NUL bytes (`\0`).

If `old` is an empty string, the input is returned unchanged (as a fresh copy).

```ry
print(replace("hello world", "world", "ry"))   # hello ry
print(replace("aaa", "a", "bb"))                # bbbbbb
print("foo bar foo".replace("foo", "baz"))      # baz bar baz (UFCS)
print(replace("hello", "", "X"))                # hello (empty pattern is a no-op)
print(replace("a\0b\0a", "\0", "-"))            # a-b-a (NUL-safe)
```

---

## toUpper

**Signature:** `toUpper(string: str) -> str`

Returns a new string with ASCII lowercase letters (a-z) converted to uppercase. Embedded NUL bytes (`\0`) are preserved unchanged (#1050).

```ry
print(toUpper("hello"))         # HELLO
print("Hello World".toUpper())  # HELLO WORLD (UFCS)
print(byteLen(toUpper("a\0B"))) # 3 (NUL byte preserved)
```

---

## toLower

**Signature:** `toLower(string: str) -> str`

Returns a new string with ASCII uppercase letters (A-Z) converted to lowercase. Embedded NUL bytes (`\0`) are preserved unchanged (#1050).

```ry
print(toLower("HELLO"))         # hello
print("Hello World".toLower())  # hello world (UFCS)
print(byteLen(toLower("A\0b"))) # 3 (NUL byte preserved)
```

---

## trim

**Signature:** `trim(string: str) -> str`

Returns a new string with leading and trailing whitespace characters (spaces, tabs, newlines, carriage returns) removed. Interior NUL bytes (`\0`) are preserved (#1050).

```ry
print(trim("  hello  "))   # hello
print("  hi  ".trim())     # hi (UFCS)
print(byteLen(trim("  a\0b  "))) # 3 (interior NUL preserved)
```

---

## trimStart

**Signature:** `trimStart(string: str) -> str`

Returns a new string with leading whitespace characters removed. Interior NUL bytes (`\0`) are preserved (#1050).

```ry
print(trimStart("  hello  "))   # hello
print("  hi".trimStart())       # hi (UFCS)
```

---

## trimEnd

**Signature:** `trimEnd(string: str) -> str`

Returns a new string with trailing whitespace characters removed. Interior NUL bytes (`\0`) are preserved (#1050).

```ry
print(trimEnd("  hello  "))   #   hello
print("hi  ".trimEnd())       # hi (UFCS)
```

---

## repeat

**Signature:** `repeat(string: str, count: int) -> str`

Returns a new string with `string` repeated `count` times. Embedded NUL bytes (`\0`) are preserved (#1051).

```ry
print(repeat("ab", 3))     # ababab
print("ha".repeat(3))      # hahaha (UFCS)
print(byteLen("\0a".repeat(3))) # 6 (NUL bytes preserved)
```

---

## reverse

**Signature:** `reverse(string: str) -> str`

Returns a new string with the characters reversed (UTF-8 aware).

```ry
print(reverse("hello"))    # olleh
print("abc".reverse())     # cba (UFCS)
print(byteLen(reverse("a\0b")))   # 3 (NUL bytes are preserved when reversing)
```

---

## byteLen

**Signature:** `byteLen(string: str) -> int`

Returns the byte length of string `string`. Unlike `len()`, which returns the number of UTF-8 characters, `byteLen()` returns the number of bytes.

Embedded NUL bytes (`\0`) are counted — `byteLen("a\0b")` returns `3`.

```ry
print(byteLen("hello"))   # 5
print(byteLen("あいう"))   # 9
print(len("あいう"))        # 3 (characters)
print(byteLen("a\0b"))    # 3 (NUL byte is counted)
```

---

## split

**Signature:** `split(string: str, delimiter: str = " ") -> List<str>`

Splits string `string` by delimiter `delimiter` and returns a `List<str>`.
When `delimiter` is omitted, it defaults to a single space `" "`.

When the delimiter is an empty string `""`, the string is split into individual characters (UTF-8 aware).

Both `string` and `delimiter` may contain embedded NUL bytes (`\0`); all paths are NUL-safe (#1051).

```ry
parts = "1 2 3".split()
print(parts)   # ["1", "2", "3"]

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

# NUL bytes are treated as single code-point characters when splitting by ""
parts = split("a\0b", "")
print(len(parts))   # 3

# Non-empty delimiter: NUL bytes in string and delimiter are preserved
parts = split("a\0b,c\0d", ",")
print(len(parts))            # 2
print(byteLen(parts[0]))       # 3  ("a\0b")
```

> **Tip:** To iterate a string character by character, you can use a `for` loop directly without calling `split`: `for c in s:` yields each UTF-8 code point as a single-character `str`. See [control-flow.md](control-flow.md#string-iteration).

---

## join

**Signature:** `join(values: List<str>, sep: str) -> str`

Joins the elements of a string list with separator `sep` and returns a string. Both list elements and `sep` may contain embedded NUL bytes (`\0`) (#1051).

```ry
parts = ["a", "b", "c"]
print(join(parts, ","))        # a,b,c
print(parts.join("-"))         # a-b-c (UFCS)
print(",".join(parts))         # a,b,c (UFCS, Python-style)
```

---

## toInt

**Signature:** `toInt(string: str) -> Result<int, Error>`

Converts a string to an integer. Leading whitespace is allowed. Returns `Err` if the string is empty, contains invalid characters, or overflows.

```ry
case toInt("42"):
    Ok(v):
        print(v)              # 42
    Err(e):
        print(e.message)

case "123".toInt():                
    Ok(v):
        print(v)              # 123
    Err(e):
        print(e.message)

# Invalid input returns Err
print(toInt("abc"))          # Err(Error("toInt: invalid character in 'abc'"))
print(toInt(""))             # Err(Error("toInt: empty string"))
```

---

## toFloat

**Signature:** `toFloat(string: str) -> Result<float, Error>`

Converts a string to a floating-point number. Returns `Err` if the string is empty, contains invalid characters, or is out of range for `float`.

```ry
case toFloat("3.14"):
    Ok(v):
        print(v)              # 3.14
    Err(e):
        print(e.message)

case "2.5".toFloat():              
    Ok(v):
        print(v)              # 2.5
    Err(e):
        print(e.message)

# Invalid input returns Err
print(toFloat("abc"))         # Err(Error("toFloat: invalid character in 'abc'"))
print(toFloat(""))            # Err(Error("toFloat: empty string"))
print(toFloat("1e400"))       # Err(Error("toFloat: out of range in '1e400'"))
```

---

## toStr

**Signature:** `toStr(v: any) -> str`

Accepts any Ry value. Supported input types are listed in the table below.

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

Whole-number `float` values are formatted with a trailing `.0` (e.g. `toStr(3.0) == "3.0"`) so they are visually distinguishable from `int`. Record types automatically generate a `toStr` representation. If a user-defined `fn toStr(v: MyRecord) -> str` is provided, it takes precedence over the auto-generated version. This also works with `print()` and f-string interpolation.

```ry
print(toStr(42))         # 42
print(toStr(3.14))       # 3.14
print(toStr(3.0))        # 3.0          (whole-number float keeps .0)
print(toStr(true))       # true
print(99.toStr())        # 99 (UFCS)

enum Color:
    Red
    Green

print(toStr(Color::Red))   # Red

record Point:
    x: int
    y: int

p = Point(10, 20)
print(toStr(p))          # Point(x: 10, y: 20)
print(p)                  # Point(x: 10, y: 20)
print(f"pos={p}")         # pos=Point(x: 10, y: 20)
```
