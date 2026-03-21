[English](../../reference/regex.md) | [日本語](regex.md) | [繁體中文](../../zh/reference/regex.md)

# 正規表現関数リファレンス

正規表現関数の一覧です。すべての関数は UFCS 記法をサポートしています。パターン文字列は標準的な正規表現構文を使用します。

> **注意:** Phase 1 ではパターンは通常の文字列として渡します。専用の正規表現リテラル構文は将来追加される可能性があります。

## 関数一覧

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `regex_match` | `(str, str) -> bool` | テキスト全体がパターンにマッチするか |
| `regex_search` | `(str, str) -> int` | 最初のマッチの開始位置を返す（見つからない場合 -1） |
| `regex_replace` | `(str, str, str) -> str` | マッチした部分を置換文字列で置換 |
| `regex_split` | `(str, str) -> List<str>` | パターンマッチで分割 |
| `regex_find_all` | `(str, str) -> List<str>` | 重複しないすべてのマッチを返す |

## サポートするパターン構文

| 構文 | 説明 | 例 |
|------|------|-----|
| `abc` | リテラル文字 | `"hello"` |
| `.` | 任意の1文字（改行を除く） | `"a.c"` は `"abc"`, `"aXc"` にマッチ |
| <code>&#124;</code> | 選択 | <code>"cat&#124;dog"</code> は `"cat"` か `"dog"` にマッチ |
| `*` | 0回以上の繰り返し | `"a*"` は `""`, `"a"`, `"aaa"` にマッチ |
| `+` | 1回以上の繰り返し | `"a+"` は `"a"`, `"aaa"` にマッチ |
| `?` | 0回または1回 | `"a?"` は `""` か `"a"` にマッチ |
| `{n}` | ちょうど n 回 | `"a{3}"` は `"aaa"` にマッチ |
| `{n,m}` | n 回以上 m 回以下 | `"a{2,4}"` は `"aa"` 〜 `"aaaa"` にマッチ |
| `{n,}` | n 回以上 | `"a{2,}"` は `"aa"`, `"aaa"`, ... にマッチ |
| `*?` | 0回以上（非貪欲） | `".*?"` は最短マッチ |
| `+?` | 1回以上（非貪欲） | `".+?"` は最短マッチ |
| `??` | 0回または1回（非貪欲） | `"a??"` は0回を優先 |
| `{n,m}?` | 範囲（非貪欲） | `"a{2,4}?"` は n 回を優先 |
| `(...)` | グループ | `"(ab)+"` は `"abab"` にマッチ |
| `[abc]` | 文字クラス | `"[aeiou]"` は母音にマッチ |
| `[a-z]` | 文字範囲 | `"[a-z]+"` は小文字の単語にマッチ |
| `[^...]` | 否定文字クラス | `"[^0-9]"` は数字以外にマッチ |
| `^` | 文字列先頭アンカー | `"^hello"` |
| `$` | 文字列末尾アンカー | `"world$"` |
| `\d` | 数字 `[0-9]` | `"\d+"` は数値にマッチ |
| `\D` | 数字以外 `[^0-9]` | |
| `\w` | 単語文字 `[a-zA-Z0-9_]` | `"\w+"` は識別子にマッチ |
| `\W` | 単語文字以外 | |
| `\s` | 空白文字 | `"\s+"` はスペースやタブにマッチ |
| `\S` | 空白文字以外 | |
| `\.` | エスケープされた特殊文字 | `"\."` はリテラルの `.` にマッチ |

## 使用例

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

### 範囲量指定子

```ry
print(regex_match("\\d{3}-\\d{4}", "123-4567"))  # true
print(regex_match("a{2,4}", "aaa"))               # true
print(regex_match("(ab){2,}", "ababab"))           # true
```

### 非貪欲（最短）マッチ

```ry
# 貪欲: 最長マッチ
let g = regex_replace("\".*\"", "\"a\" and \"b\"", "X")
print(g)  # X

# 非貪欲: 最短マッチ
let l = regex_replace("\".*?\"", "\"a\" and \"b\"", "X")
print(l)  # X and X

# 個別のHTMLタグを取得
let tags = regex_find_all("<.*?>", "<a> <bb> <ccc>")
print(len(tags))  # 3
```

> **注意:** 非貪欲マッチはマッチ全体の長さを制御します。グループ（括弧で囲んだ部分式）がない場合、greedy/lazy の混在パターンは PCRE エンジンと異なる動作をする場合があります。

### UFCS 記法

```ry
# pattern.function(text, ...)
let m = "[a-z]+".regex_match("hello")
print(m)  # true
```
