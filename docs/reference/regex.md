[English](regex.md) | [日本語](../ja/reference/regex.md) | [繁體中文](../zh/reference/regex.md)

# Regular Expression Reference

## Regex Literal Syntax

Regex literals use the `/pattern/` syntax and produce a `Regex` type value:

```ry
from regex import is_match, split, replace

# Regex literals enable type-based overloading
"hello".is_match(/[a-z]+/)        # true
"a1b2c".split(/[0-9]/)         # ["a", "b", "c"]
"abc123".replace(/[0-9]+/, "X") # "abcX"
```

Regex literals can be stored in variables:

```ry
pat = /[a-z]+/
"hello".is_match(pat)  # true
```

The `/` inside a regex literal can be escaped with `\/`:

```ry
"a/b".is_match(/a\/b/)  # true
```

The `\0` escape sequence inside a regex literal produces a NUL byte in the pattern:

```ry
s = "a\0b"            # 3-byte string: a, NUL, b
s.is_match(/a\0b/)    # true — \0 in regex literal is a NUL byte
```

### Division vs Regex

The lexer uses context to distinguish regex literals from division:

- After value-producing tokens (identifiers, numbers, string literals, `)` or `]`), `/` is parsed as division
- After operators, keywords, or delimiters that expect an expression (`(`, `[`, `,`, `=`), `/` starts a regex literal

```ry
x = 10 / 2         # division: 5
y = is_match("a", /a/) # regex literal
```

## Function List

### Regex Literal Functions (text-first, UFCS-compatible)

These functions take a `Regex` type pattern and use text-first argument order for UFCS:

| Function | Signature | Description |
|----------|-----------|-------------|
| `is_match` | `(str, Regex) -> bool` | Returns whether the entire text matches the pattern |
| `search` | `(str, Regex) -> int` | Returns the start position of the first match (-1 if not found) |
| `replace` | `(str, Regex, str) -> str` | Replaces all matches with a replacement string |
| `split` | `(str, Regex) -> List<str>` | Splits text by pattern matches |
| `find_all` | `(str, Regex) -> List<Match>` | Returns all non-overlapping matches with capture groups |

```ry
from regex import is_match, search, replace, split, find_all

# Direct call
print(is_match("hello", /[a-z]+/))       # true

# UFCS (text.function(pattern))
print("abc123".search(/[0-9]+/))          # 3
print("abc123".replace(/[0-9]+/, "X"))    # abcX
parts = "hello world".split(/\s+/)
matches = "a1b2c3".find_all(/[0-9]/)
print(matches[0].full)   # "1"
```

### Legacy Functions (text-first)

The original `regex_*` functions remain available for backward compatibility. They take pattern strings (not regex literals) with text-first argument order, consistent with the regex literal API:

| Function | Signature | Description |
|----------|-----------|-------------|
| `regex_match` | `(text: str, pattern: str) -> bool` | Returns whether the entire text matches the pattern |
| `regex_search` | `(text: str, pattern: str) -> int` | Returns the start position of the first match (-1 if not found) |
| `regex_replace` | `(text: str, pattern: str, replacement: str) -> str` | Replaces all matches with a replacement string |
| `regex_split` | `(text: str, pattern: str) -> List<str>` | Splits text by pattern matches |
| `regex_find_all` | `(text: str, pattern: str) -> List<Match>` | Returns all non-overlapping matches with capture groups |

```ry
print(regex_match("hello", "[a-z]+"))   # true
pos = regex_search("abc123", "[0-9]+")  # 3
```

## Match Type

`find_all` and `regex_find_all` return `List<Match>` where each `Match` record has:

| Field | Type | Description |
|-------|------|-------------|
| `full` | `str` | The entire matched substring |
| `groups` | `List<str>` | Captured group texts, in order (empty list if no capture groups) |

```ry
from regex import find_all

# Without capture groups: groups is empty
matches = find_all("a1b2c3", /[0-9]/)
print(matches[0].full)                 # "1"
print(length(matches[0].groups))       # 0

# With capture groups
matches = find_all("2026-04-10", /(\d+)-(\d+)-(\d+)/)
print(matches[0].full)                 # "2026-04-10"
print(matches[0].groups[0])            # "2026"
print(matches[0].groups[1])            # "04"
print(matches[0].groups[2])            # "10"

# Multiple matches, each with their own capture groups
for m in find_all("a@b x@y", /(\w+)@(\w+)/):
    print(m.full)       # "a@b", "x@y"
    print(m.groups[0])  # "a",   "x"
    print(m.groups[1])  # "b",   "y"
```

Unmatched optional groups (e.g., `(a)?` when the group did not participate) expand to an empty string in `groups`.

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
| `(...)` | Capture group (see [Backreferences](#capture-group-backreferences)) | `"(ab)+"` matches `"abab"` |
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
| `\b` | Word boundary | `"\bword\b"` matches whole word |
| `\B` | Non-word boundary | `"\Bword"` matches inside a word |
| `(?i)` | Case-insensitive flag | `"(?i)hello"` matches `"HELLO"` |
| `\.` | Escaped special character | `"\."` matches literal `.` |

## Usage Examples

### Range Quantifiers

```ry
print(regex_match("123-4567", "\\d{3}-\\d{4}"))  # true
print(regex_match("aaa", "a{2,4}"))               # true
print(regex_match("ababab", "(ab){2,}"))           # true
```

### Non-Greedy (Lazy) Match

```ry
# Greedy: matches longest
g = regex_replace("\"a\" and \"b\"", "\".*\"", "X")
print(g)  # X

# Non-greedy: matches shortest
l = regex_replace("\"a\" and \"b\"", "\".*?\"", "X")
print(l)  # X and X

# Find individual HTML-like tags
tags = regex_find_all("<a> <bb> <ccc>", "<.*?>")
print(length(tags))         # 3
print(tags[0].full)         # "<a>"
```

### Word Boundary

```ry
# Match whole words only
pos = regex_search("hello world", "\\bworld\\b")
print(pos)  # 6

# Find all words
words = regex_find_all("hello world foo", "\\b\\w+\\b")
print(length(words))         # 3
print(words[0].full)         # "hello"
```

### Capture Group Backreferences

The `replace` / `regex_replace` functions support backreferences in the replacement string, allowing captured text to be inserted into the output.

| Syntax | Expands to |
|--------|-----------|
| `$0` | The entire match |
| `$1` – `$9` | Contents of capture group N |
| `${10}`, `${11}`, … | Multi-digit group (use `${N}` to avoid ambiguity) |
| `$$` | A literal `$` character |
| `$` + non-digit | A literal `$` followed by that character |

Out-of-range or unmatched groups expand to an empty string.

```ry
from regex import replace

# Swap words: $2 and $1
print(replace("hello world", /(\w+) (\w+)/, "$2, $1!"))
# world, hello!

# Reformat date: YYYY-MM-DD → DD/MM/YYYY
print(replace("2026-04-10", /(\d+)-(\d+)-(\d+)/, "$3/$2/$1"))
# 10/04/2026

# $0 is the whole match (no capture groups needed)
print(replace("hello world", /\w+/, "[$0]"))
# [hello] [world]

# Prefix a number with a literal $
print(replace("price: 100", /(\d+)/, "$$$1"))
# price: $100
```

### Case-Insensitive Matching

```ry
# (?i) at the start of pattern enables case-insensitive matching
print(regex_match("HELLO", "(?i)hello"))  # true
print(regex_match("Hello", "(?i)hello"))  # true
```

> **Note:** `(?i)` must appear at the beginning of the pattern and applies to the entire pattern. Partial case-insensitive matching (e.g., `(?i:sub)pattern`) is not supported.

## NUL Byte Safety

All regex operations — `regex_match`, `regex_search`, `regex_replace`, `regex_split`, `regex_find_all` and their UFCS variants (`is_match`, `search`, `replace`, `split`, `find_all`) — are fully NUL-safe (#1052) when called with **string arguments** or **already-constructed `Regex` values**. Embedded NUL bytes (`\0`) in the **subject**, **pattern** (string form), and **replacement** strings are all preserved correctly.

- The `.` metacharacter matches any byte, including `\0`.
- `regex_search` reports the correct character index even when NUL bytes precede the match.
- `regex_replace` preserves NUL bytes in both the surrounding text and the replacement string.
- `regex_split` returns segments whose byte lengths account for any embedded NUL bytes.
- `regex_find_all` counts every matched byte, including `\0`, and returns all non-overlapping matches.

- The `\0` escape in a regex literal (`/a\0b/`) produces a NUL byte in the pattern, matching NUL bytes in the subject string (#1076).
