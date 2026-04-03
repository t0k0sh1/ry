[English](../../reference/regex.md) | [日本語](../../ja/reference/regex.md) | [简体中文](regex.md)

# 正则表达式参考

## 正则表达式字面量语法

正则表达式字面量使用 `/pattern/` 语法，产生 `Regex` 类型的值：

```ry
from regex import is_match, split, replace

# 正则表达式字面量支持基于类型的重载
"hello".is_match(/[a-z]+/)        # true
"a1b2c".split(/[0-9]/)         # ["a", "b", "c"]
"abc123".replace(/[0-9]+/, "X") # "abcX"
```

正则表达式字面量可以存储在变量中：

```ry
pat = /[a-z]+/
"hello".is_match(pat)  # true
```

正则表达式字面量中的 `/` 可以用 `\/` 转义：

```ry
"a/b".is_match(/a\/b/)  # true
```

### 除法与正则表达式的区分

词法分析器使用上下文来区分正则表达式字面量和除法运算：

- 在产生值的标记（标识符、数字、字符串字面量、`)` 或 `]`）之后，`/` 被解析为除法
- 在运算符、关键字或期望表达式的定界符（`(`、`[`、`,`、`=`）之后，`/` 开始一个正则表达式字面量

```ry
x = 10 / 2         # 除法：5
y = is_match("a", /a/) # 正则表达式字面量
```

## 函数一览

### 正则表达式字面量函数（文本在前，兼容 UFCS）

这些函数接受 `Regex` 类型的模式，使用文本在前的参数顺序以支持 UFCS：

| 函数 | 签名 | 说明 |
|------|------|------|
| `is_match` | `(str, Regex) -> bool` | 返回整个文本是否匹配模式 |
| `search` | `(str, Regex) -> int` | 返回第一个匹配的起始位置（未找到时返回 -1） |
| `replace` | `(str, Regex, str) -> str` | 将所有匹配替换为替换字符串 |
| `split` | `(str, Regex) -> List<str>` | 按模式匹配进行分割 |
| `find_all` | `(str, Regex) -> List<str>` | 返回所有不重叠的匹配结果 |

```ry
from regex import is_match, search, replace, split, find_all

# 直接调用
print(is_match("hello", /[a-z]+/))       # true

# UFCS（text.function(pattern)）
print("abc123".search(/[0-9]+/))          # 3
print("abc123".replace(/[0-9]+/, "X"))    # abcX
parts = "hello world".split(/\s+/)
nums = "a1b2c3".find_all(/[0-9]/)
```

### 遗留函数（文本在前）

原始的 `regex_*` 函数仍然可用以保持向后兼容。它们接受模式字符串（非正则表达式字面量），使用文本在前的参数顺序，与正则表达式字面量 API 一致：

| 函数 | 签名 | 说明 |
|------|------|------|
| `regex_match` | `(text: str, pattern: str) -> bool` | 返回整个文本是否匹配模式 |
| `regex_search` | `(text: str, pattern: str) -> int` | 返回第一个匹配的起始位置（未找到时返回 -1） |
| `regex_replace` | `(text: str, pattern: str, replacement: str) -> str` | 将所有匹配替换为替换字符串 |
| `regex_split` | `(text: str, pattern: str) -> List<str>` | 按模式匹配进行分割 |
| `regex_find_all` | `(text: str, pattern: str) -> List<str>` | 返回所有不重叠的匹配结果 |

```ry
print(regex_match("hello", "[a-z]+"))   # true
pos = regex_search("abc123", "[0-9]+")  # 3
```

## 支持的模式语法

| 语法 | 说明 | 示例 |
|------|------|------|
| `abc` | 字面字符 | `"hello"` |
| `.` | 任意一个字符（换行除外） | `"a.c"` 匹配 `"abc"`、`"aXc"` |
| <code>&#124;</code> | 选择 | <code>"cat&#124;dog"</code> 匹配 `"cat"` 或 `"dog"` |
| `*` | 零次或多次 | `"a*"` 匹配 `""`、`"a"`、`"aaa"` |
| `+` | 一次或多次 | `"a+"` 匹配 `"a"`、`"aaa"` |
| `?` | 零次或一次 | `"a?"` 匹配 `""` 或 `"a"` |
| `{n}` | 恰好 n 次 | `"a{3}"` 匹配 `"aaa"` |
| `{n,m}` | n 到 m 次 | `"a{2,4}"` 匹配 `"aa"` 到 `"aaaa"` |
| `{n,}` | 至少 n 次 | `"a{2,}"` 匹配 `"aa"`、`"aaa"`、... |
| `*?` | 零次或多次（非贪婪） | `".*?"` 匹配最短 |
| `+?` | 一次或多次（非贪婪） | `".+?"` 匹配最短 |
| `??` | 零次或一次（非贪婪） | `"a??"` 优先零次 |
| `{n,m}?` | 范围（非贪婪） | `"a{2,4}?"` 优先 n 次 |
| `(...)` | 分组 | `"(ab)+"` 匹配 `"abab"` |
| `[abc]` | 字符类 | `"[aeiou]"` 匹配元音字母 |
| `[a-z]` | 字符范围 | `"[a-z]+"` 匹配小写单词 |
| `[^...]` | 否定字符类 | `"[^0-9]"` 匹配非数字 |
| `^` | 字符串开头锚点 | `"^hello"` |
| `$` | 字符串结尾锚点 | `"world$"` |
| `\d` | 数字 `[0-9]` | `"\d+"` 匹配数字 |
| `\D` | 非数字 `[^0-9]` | |
| `\w` | 单词字符 `[a-zA-Z0-9_]` | `"\w+"` 匹配标识符 |
| `\W` | 非单词字符 | |
| `\s` | 空白字符 | `"\s+"` 匹配空格与 Tab |
| `\S` | 非空白字符 | |
| `\b` | 单词边界 | `"\bword\b"` 匹配完整单词 |
| `\B` | 非单词边界 | `"\Bword"` 匹配单词内部 |
| `(?i)` | 忽略大小写标志 | `"(?i)hello"` 匹配 `"HELLO"` |
| `\.` | 转义特殊字符 | `"\."` 匹配字面的 `.` |

## 使用示例

### 范围量词

```ry
print(regex_match("123-4567", "\\d{3}-\\d{4}"))  # true
print(regex_match("aaa", "a{2,4}"))               # true
print(regex_match("ababab", "(ab){2,}"))           # true
```

### 非贪婪（最短）匹配

```ry
# 贪婪：匹配最长
g = regex_replace("\"a\" and \"b\"", "\".*\"", "X")
print(g)  # X

# 非贪婪：匹配最短
l = regex_replace("\"a\" and \"b\"", "\".*?\"", "X")
print(l)  # X and X

# 查找各个 HTML 标签
tags = regex_find_all("<a> <bb> <ccc>", "<.*?>")
print(length(tags))  # 3
```

> **注意：** 非贪婪匹配控制整体匹配长度。在不支持提取括号分组的情况下，greedy/lazy 混合模式可能与 PCRE 引擎的行为不同。

### 单词边界

```ry
# 匹配完整单词
pos = regex_search("hello world", "\\bworld\\b")
print(pos)  # 6

# 查找所有单词
words = regex_find_all("hello world foo", "\\b\\w+\\b")
print(length(words))  # 3
```

### 忽略大小写匹配

```ry
# (?i) 放在模式开头即可忽略大小写
print(regex_match("HELLO", "(?i)hello"))  # true
print(regex_match("Hello", "(?i)hello"))  # true
```

> **注意：** `(?i)` 必须出现在模式的开头，并适用于整个模式。不支持部分忽略大小写匹配（例如 `(?i:sub)pattern`）。
