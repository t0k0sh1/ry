[English](../../reference/regex.md) | [日本語](../../ja/reference/regex.md) | [繁體中文](regex.md)

# 正規表達式函式參考手冊

正規表達式函式的一覽表。所有函式皆支援 UFCS 記法。模式字串使用標準的正規表達式語法。

> **注意：** Phase 1 中模式以普通字串傳遞。專用的正規表達式字面量語法可能在未來版本中加入。

## 函式一覽

| 函式 | 簽名 | 說明 |
|------|------|------|
| `regex_match` | `(str, str) -> bool` | 文字全體是否匹配模式 |
| `regex_search` | `(str, str) -> int` | 傳回第一個匹配的起始位置（找不到時傳回 -1） |
| `regex_replace` | `(str, str, str) -> str` | 將匹配的部分替換為替換字串 |
| `regex_split` | `(str, str) -> List<str>` | 以模式匹配進行分割 |
| `regex_find_all` | `(str, str) -> List<str>` | 傳回所有不重疊的匹配結果 |

## 支援的模式語法

| 語法 | 說明 | 範例 |
|------|------|------|
| `abc` | 字面字元 | `"hello"` |
| `.` | 任意一個字元（換行除外） | `"a.c"` 匹配 `"abc"`、`"aXc"` |
| `\|` | 選擇 | `"cat\|dog"` 匹配 `"cat"` 或 `"dog"` |
| `*` | 零次或多次重複 | `"a*"` 匹配 `""`、`"a"`、`"aaa"` |
| `+` | 一次或多次重複 | `"a+"` 匹配 `"a"`、`"aaa"` |
| `?` | 零次或一次 | `"a?"` 匹配 `""` 或 `"a"` |
| `(...)` | 群組 | `"(ab)+"` 匹配 `"abab"` |
| `[abc]` | 字元類別 | `"[aeiou]"` 匹配母音 |
| `[a-z]` | 字元範圍 | `"[a-z]+"` 匹配小寫單字 |
| `[^...]` | 否定字元類別 | `"[^0-9]"` 匹配非數字 |
| `^` | 字串開頭錨點 | `"^hello"` |
| `$` | 字串結尾錨點 | `"world$"` |
| `\d` | 數字 `[0-9]` | `"\d+"` 匹配數字 |
| `\D` | 非數字 `[^0-9]` | |
| `\w` | 單字字元 `[a-zA-Z0-9_]` | `"\w+"` 匹配識別符 |
| `\W` | 非單字字元 | |
| `\s` | 空白字元 | `"\s+"` 匹配空格與 Tab |
| `\S` | 非空白字元 | |
| `\.` | 跳脫特殊字元 | `"\."` 匹配字面的 `.` |

## 使用範例

### regex_match

```ry
print(regex_match("[a-z]+", "hello"))   # true
print(regex_match("[0-9]+", "hello"))   # false
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
```

### UFCS 記法

```ry
# pattern.function(text, ...)
let m = "[a-z]+".regex_match("hello")
print(m)  # true
```
