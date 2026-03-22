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
| <code>&#124;</code> | 選擇 | <code>"cat&#124;dog"</code> 匹配 `"cat"` 或 `"dog"` |
| `*` | 零次或多次重複 | `"a*"` 匹配 `""`、`"a"`、`"aaa"` |
| `+` | 一次或多次重複 | `"a+"` 匹配 `"a"`、`"aaa"` |
| `?` | 零次或一次 | `"a?"` 匹配 `""` 或 `"a"` |
| `{n}` | 恰好 n 次 | `"a{3}"` 匹配 `"aaa"` |
| `{n,m}` | n 到 m 次 | `"a{2,4}"` 匹配 `"aa"` 到 `"aaaa"` |
| `{n,}` | 至少 n 次 | `"a{2,}"` 匹配 `"aa"`、`"aaa"`、... |
| `*?` | 零次或多次（非貪婪） | `".*?"` 匹配最短 |
| `+?` | 一次或多次（非貪婪） | `".+?"` 匹配最短 |
| `??` | 零次或一次（非貪婪） | `"a??"` 優先零次 |
| `{n,m}?` | 範圍（非貪婪） | `"a{2,4}?"` 優先 n 次 |
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
| `\b` | 單字邊界 | `"\bword\b"` 匹配完整單字 |
| `\B` | 非單字邊界 | `"\Bword"` 匹配單字內部 |
| `(?i)` | 忽略大小寫旗標 | `"(?i)hello"` 匹配 `"HELLO"` |
| `\.` | 跳脫特殊字元 | `"\."` 匹配字面的 `.` |

## 使用範例

### regex_match

```ry
print(regex_match("[a-z]+", "hello"))   # true
print(regex_match("[0-9]+", "hello"))   # false
```

### regex_search

```ry
@const
pos = regex_search("[0-9]+", "abc123def")
print(pos)  # 3
```

### regex_replace

```ry
@const
s = regex_replace("[0-9]+", "a1b2c3", "X")
print(s)  # aXbXcX
```

### regex_split

```ry
@const
parts = regex_split("\\s+", "hello  world  foo")
print(len(parts))  # 3
print(parts[0])    # hello
```

### regex_find_all

```ry
@const
matches = regex_find_all("[0-9]+", "a1b23c456")
print(len(matches))  # 3
print(matches[0])    # 1
print(matches[1])    # 23
```

### 範圍量詞

```ry
print(regex_match("\\d{3}-\\d{4}", "123-4567"))  # true
print(regex_match("a{2,4}", "aaa"))               # true
print(regex_match("(ab){2,}", "ababab"))           # true
```

### 非貪婪（最短）匹配

```ry
# 貪婪: 匹配最長
@const
g = regex_replace("\".*\"", "\"a\" and \"b\"", "X")
print(g)  # X

# 非貪婪: 匹配最短
@const
l = regex_replace("\".*?\"", "\"a\" and \"b\"", "X")
print(l)  # X and X

# 取得個別 HTML 標籤
@const
tags = regex_find_all("<.*?>", "<a> <bb> <ccc>")
print(len(tags))  # 3
```

> **注意：** 非貪婪匹配控制整體匹配長度。沒有使用括號分組時，greedy/lazy 混合模式可能與 PCRE 引擎行為不同。

### 單字邊界

```ry
# 匹配完整單字
@const
pos = regex_search("\\bworld\\b", "hello world")
print(pos)  # 6

# 取得所有單字
@const
words = regex_find_all("\\b\\w+\\b", "hello world foo")
print(len(words))  # 3

# \B 匹配非邊界（單字內部）
@const
pos2 = regex_search("\\Bworld", "helloworld")
print(pos2)  # 5
```

### 忽略大小寫匹配

```ry
# (?i) 放在模式開頭即可忽略大小寫
print(regex_match("(?i)hello", "HELLO"))  # true
print(regex_match("(?i)hello", "Hello"))  # true

# 字元類別也適用
print(regex_match("(?i)[a-z]+", "ABC"))  # true

# replace 和 find_all 也可使用
@const
s = regex_replace("(?i)hello", "Hello HELLO hello", "X")
print(s)  # X X X
```

> **注意：** `(?i)` 必須出現在模式的開頭，並適用於整個模式。不支援部分忽略大小寫（例如 `(?i:sub)pattern`）。

### UFCS 記法

```ry
# pattern.function(text, ...)
@const
m = "[a-z]+".regex_match("hello")
print(m)  # true
```
