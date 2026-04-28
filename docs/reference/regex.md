# Regular Expression Reference

## Regex Literal Syntax

Regex literals use the `/pattern/` syntax. They can be stored in variables or passed directly to functions that accept a `Regex` parameter:

```ry
from regex import isMatch, split, replace

# Regex literals enable type-based overloading
"hello".isMatch(/[a-z]+/)        # true
"a1b2c".split(/[0-9]/)         # ["a", "b", "c"]
"abc123".replace(/[0-9]+/, "X") # "abcX"
```

Regex literals can be stored in variables:

```ry
pat = /[a-z]+/
"hello".isMatch(pat)  # true
```

The `/` inside a regex literal can be escaped with `\/`:

```ry
"a/b".isMatch(/a\/b/)  # true
```

The `\0` escape sequence inside a regex literal produces a NUL byte in the pattern:

```ry
s = "a\0b"            # 3-byte string: a, NUL, b
s.isMatch(/a\0b/)    # true — \0 in regex literal is a NUL byte
```

### Division vs Regex

The lexer uses context to distinguish regex literals from division:

- After value-producing tokens (identifiers, numbers, string literals, `)` or `]`), `/` is parsed as division
- After operators, keywords, or delimiters that expect an expression (`(`, `[`, `,`, `=`), `/` starts a regex literal

```ry
x = 10 / 2         # division: 5
y = isMatch("a", /a/) # regex literal
```

## Function List

### Regex Literal Functions (text-first, UFCS-compatible)

These functions take a regex literal pattern and use text-first argument order for UFCS:

| Function | Signature | Description |
|----------|-----------|-------------|
| `isMatch` | `(str, Regex) -> bool` | Returns whether the pattern matches anywhere in the text (use `^...$` to require a full-string match) |
| `search` | `(str, Regex) -> int` | Returns the start position of the first match (-1 if not found) |
| `replace` | `(str, Regex, str) -> str` | Replaces all matches with a replacement string |
| `split` | `(str, Regex) -> List<str>` | Splits text by pattern matches |
| `findAll` | `(str, Regex) -> List<Match>` | Returns all non-overlapping matches with capture groups |

```ry
from regex import isMatch, search, replace, split, findAll

# Direct call
print(isMatch("hello", /[a-z]+/))       # true
print(isMatch("Hello 123 World", /[0-9]+/))  # true — partial (unanchored) match

# UFCS (text.fn(pattern))
print("abc123".search(/[0-9]+/))          # 3
print("abc123".replace(/[0-9]+/, "X"))    # abcX
parts = "hello world".split(/\s+/)
matches = "a1b2c3".findAll(/[0-9]/)
print(matches[0].full)   # "1"
```

### Legacy Functions (text-first)

The original `regex_*` functions remain available for backward compatibility. They take pattern strings (not regex literals) with text-first argument order, consistent with the regex literal API:

| Function | Signature | Description |
|----------|-----------|-------------|
| `regexMatch` | `(text: str, pattern: str) -> bool` | Returns whether the entire text matches the pattern |
| `regexSearch` | `(text: str, pattern: str) -> int` | Returns the start position of the first match (-1 if not found) |
| `regexReplace` | `(text: str, pattern: str, replacement: str) -> str` | Replaces all matches with a replacement string |
| `regexSplit` | `(text: str, pattern: str) -> List<str>` | Splits text by pattern matches |
| `regexFindAll` | `(text: str, pattern: str) -> List<Match>` | Returns all non-overlapping matches with capture groups |

```ry
print(regexMatch("hello", "[a-z]+"))   # true
pos = regexSearch("abc123", "[0-9]+")  # 3
```

## Match Type

`findAll` and `regexFindAll` return `List<Match>` where each `Match` record has:

| Field | Type | Description |
|-------|------|-------------|
| `full` | `str` | The entire matched substring |
| `groups` | `List<str>` | Captured group texts, in order (empty list if no capture groups) |

```ry
from regex import findAll

# Without capture groups: groups is empty
matches = findAll("a1b2c3", /[0-9]/)
print(matches[0].full)                 # "1"
print(len(matches[0].groups))       # 0

# With capture groups
matches = findAll("2026-04-10", /(\d+)-(\d+)-(\d+)/)
print(matches[0].full)                 # "2026-04-10"
print(matches[0].groups[0])            # "2026"
print(matches[0].groups[1])            # "04"
print(matches[0].groups[2])            # "10"

# Multiple matches, each with their own capture groups
for m in findAll("a@b x@y", /(\w+)@(\w+)/):
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
| `\B` | Non-word boundary (opposite of `\b`; matches where `\b` does not) | `"\Bword\B"` matches `"word"` embedded within another word |
| `(?i)` | Case-insensitive flag | `"(?i)hello"` matches `"HELLO"` |
| `\.` | Escaped special character | `"\."` matches literal `.` |

## Usage Examples

### Range Quantifiers

```ry
print(regexMatch("123-4567", "\\d{3}-\\d{4}"))  # true
print(regexMatch("aaa", "a{2,4}"))               # true
print(regexMatch("ababab", "(ab){2,}"))           # true
```

### Non-Greedy (Lazy) Match

```ry
# Greedy: matches longest
g = regexReplace("\"a\" and \"b\"", "\".*\"", "X")
print(g)  # X

# Non-greedy: matches shortest
l = regexReplace("\"a\" and \"b\"", "\".*?\"", "X")
print(l)  # X and X

# Find individual HTML-like tags
tags = regexFindAll("<a> <bb> <ccc>", "<.*?>")
print(len(tags))         # 3
print(tags[0].full)         # "<a>"
```

### Word Boundary

```ry
# Match whole words only
pos = regexSearch("hello world", "\\bworld\\b")
print(pos)  # 6

# Find all words
words = regexFindAll("hello world foo", "\\b\\w+\\b")
print(len(words))         # 3
print(words[0].full)         # "hello"
```

### Capture Group Backreferences

The `replace` / `regexReplace` functions support backreferences in the replacement string, allowing captured text to be inserted into the output.

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
print(regexMatch("HELLO", "(?i)hello"))  # true
print(regexMatch("Hello", "(?i)hello"))  # true
```

> **Note:** `(?i)` must appear at the beginning of the pattern and applies to the entire pattern. Partial case-insensitive matching (e.g., `(?i:sub)pattern`) is not supported.

## NUL Byte Safety

All regex operations — `regexMatch`, `regexSearch`, `regexReplace`, `regexSplit`, `regexFindAll` and their UFCS variants (`isMatch`, `search`, `replace`, `split`, `findAll`) — are fully NUL-safe (#1052) when called with **string arguments** or **already-constructed `Regex` values**. Embedded NUL bytes (`\0`) in the **subject**, **pattern** (string form), and **replacement** strings are all preserved correctly.

- The `.` metacharacter matches any byte, including `\0`.
- `regexSearch` reports the correct character index even when NUL bytes precede the match.
- `regexReplace` preserves NUL bytes in both the surrounding text and the replacement string.
- `regexSplit` returns segments whose byte lengths account for any embedded NUL bytes.
- `regexFindAll` counts every matched byte, including `\0`, and returns all non-overlapping matches.

- The `\0` escape in a regex literal (`/a\0b/`) produces a NUL byte in the pattern, matching NUL bytes in the subject string (#1076).
