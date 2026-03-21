[English](regex.md) | [日本語](../ja/reference/regex.md) | [繁體中文](../zh/reference/regex.md)

# Regular Expression Function Reference

A list of regular expression functions. All functions support UFCS notation. Pattern strings use standard regex syntax.

> **Note:** Phase 1 passes patterns as plain strings. A dedicated regex literal syntax may be added in the future.

## Function List

| Function | Signature | Description |
|----------|-----------|-------------|
| `regex_match` | `(str, str) -> bool` | Returns whether the entire text matches the pattern |
| `regex_search` | `(str, str) -> int` | Returns the start position of the first match (-1 if not found) |
| `regex_replace` | `(str, str, str) -> str` | Replaces all matches with a replacement string |
| `regex_split` | `(str, str) -> List<str>` | Splits text by pattern matches |
| `regex_find_all` | `(str, str) -> List<str>` | Returns all non-overlapping matches |

## Supported Pattern Syntax

| Syntax | Description | Example |
|--------|-------------|---------|
| `abc` | Literal characters | `"hello"` |
| `.` | Any character (except newline) | `"a.c"` matches `"abc"`, `"aXc"` |
| <code>&#124;</code> | Alternation | <code>"cat&#124;dog"</code> matches `"cat"` or `"dog"` |
| `*` | Zero or more | `"a*"` matches `""`, `"a"`, `"aaa"` |
| `+` | One or more | `"a+"` matches `"a"`, `"aaa"` |
| `?` | Zero or one | `"a?"` matches `""` or `"a"` |
| `{n}` | Exactly n times | `"a{3}"` matches `"aaa"` |
| `{n,m}` | Between n and m times | `"a{2,4}"` matches `"aa"` to `"aaaa"` |
| `{n,}` | At least n times | `"a{2,}"` matches `"aa"`, `"aaa"`, ... |
| `*?` | Zero or more (non-greedy) | `".*?"` matches shortest |
| `+?` | One or more (non-greedy) | `".+?"` matches shortest |
| `??` | Zero or one (non-greedy) | `"a??"` prefers zero |
| `{n,m}?` | Range (non-greedy) | `"a{2,4}?"` prefers n times |
| `(...)` | Group | `"(ab)+"` matches `"abab"` |
| `[abc]` | Character class | `"[aeiou]"` matches vowels |
| `[a-z]` | Character range | `"[a-z]+"` matches lowercase words |
| `[^...]` | Negated character class | `"[^0-9]"` matches non-digits |
| `^` | Start of string anchor | `"^hello"` |
| `$` | End of string anchor | `"world$"` |
| `\d` | Digit `[0-9]` | `"\d+"` matches numbers |
| `\D` | Non-digit `[^0-9]` | |
| `\w` | Word character `[a-zA-Z0-9_]` | `"\w+"` matches identifiers |
| `\W` | Non-word character | |
| `\s` | Whitespace | `"\s+"` matches spaces/tabs |
| `\S` | Non-whitespace | |
| `\.` | Escaped special character | `"\."` matches literal `.` |

## Usage Examples

### regex_match

```ry
print(regex_match("[a-z]+", "hello"))   # true
print(regex_match("[0-9]+", "hello"))   # false
print(regex_match("[a-zA-Z_]\\w*", "my_var"))  # true
```

### regex_search

```ry
let pos = regex_search("[0-9]+", "abc123def")
print(pos)  # 3
```

### regex_replace

```ry
let s = regex_replace("[0-9]+", "a1b2c3", "X")
print(s)  # aXbXcX
```

### regex_split

```ry
let parts = regex_split("\\s+", "hello  world  foo")
print(len(parts))  # 3
print(parts[0])    # hello
```

### regex_find_all

```ry
let matches = regex_find_all("[0-9]+", "a1b23c456")
print(len(matches))  # 3
print(matches[0])    # 1
print(matches[1])    # 23
print(matches[2])    # 456
```

### Range Quantifiers

```ry
print(regex_match("\\d{3}-\\d{4}", "123-4567"))  # true
print(regex_match("a{2,4}", "aaa"))               # true
print(regex_match("(ab){2,}", "ababab"))           # true
```

### Non-Greedy (Lazy) Match

```ry
# Greedy: matches longest
let g = regex_replace("\".*\"", "\"a\" and \"b\"", "X")
print(g)  # X

# Non-greedy: matches shortest
let l = regex_replace("\".*?\"", "\"a\" and \"b\"", "X")
print(l)  # X and X

# Find individual HTML-like tags
let tags = regex_find_all("<.*?>", "<a> <bb> <ccc>")
print(len(tags))  # 3
```

> **Note:** Non-greedy matching controls the overall match length. Without support for extracting parenthesized groups, mixed greedy/lazy patterns may behave differently from PCRE-style engines.

### UFCS Notation

```ry
# pattern.function(text, ...)
let m = "[a-z]+".regex_match("hello")
print(m)  # true
```
