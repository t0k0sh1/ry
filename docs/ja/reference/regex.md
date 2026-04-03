[English](../../reference/regex.md) | [日本語](regex.md) | [繁體中文](../../zh/reference/regex.md)

# 正規表現リファレンス

## 正規表現リテラル構文

正規表現リテラルは `/pattern/` 構文を使用し、`Regex` 型の値を生成します:

```ry
from regex import is_match, split, replace

# 正規表現リテラルにより型ベースのオーバーロードが有効になる
"hello".is_match(/[a-z]+/)        # true
"a1b2c".split(/[0-9]/)         # ["a", "b", "c"]
"abc123".replace(/[0-9]+/, "X") # "abcX"
```

正規表現リテラルは変数に格納できます:

```ry
pat = /[a-z]+/
"hello".is_match(pat)  # true
```

正規表現リテラル内の `/` は `\/` でエスケープできます:

```ry
"a/b".is_match(/a\/b/)  # true
```

### 除算と正規表現の区別

レクサーはコンテキストを使用して正規表現リテラルと除算を区別します:

- 値を生成するトークン（識別子、数値、文字列リテラル、`)` または `]`）の後では、`/` は除算として解析される
- 演算子、キーワード、または式を期待する区切り文字（`(`、`[`、`,`、`=`）の後では、`/` は正規表現リテラルの開始として扱われる

```ry
x = 10 / 2         # 除算: 5
y = is_match("a", /a/) # 正規表現リテラル
```

## 関数一覧

### 正規表現リテラル関数（テキスト優先、UFCS 互換）

これらの関数は `Regex` 型のパターンを取り、UFCS 用にテキスト優先の引数順序を使用します:

| 関数 | シグネチャ | 説明 |
|----------|-----------|------|
| `is_match` | `(str, Regex) -> bool` | テキスト全体がパターンにマッチするかを返す |
| `search` | `(str, Regex) -> int` | 最初のマッチの開始位置を返す（見つからない場合 -1） |
| `replace` | `(str, Regex, str) -> str` | マッチした部分を置換文字列で置換 |
| `split` | `(str, Regex) -> List<str>` | パターンマッチでテキストを分割 |
| `find_all` | `(str, Regex) -> List<str>` | 重複しないすべてのマッチを返す |

```ry
from regex import is_match, search, replace, split, find_all

# 直接呼び出し
print(is_match("hello", /[a-z]+/))       # true

# UFCS（text.function(pattern)）
print("abc123".search(/[0-9]+/))          # 3
print("abc123".replace(/[0-9]+/, "X"))    # abcX
parts = "hello world".split(/\s+/)
nums = "a1b2c3".find_all(/[0-9]/)
```

### レガシー関数（テキスト優先）

元の `regex_*` 関数は後方互換性のために引き続き利用可能です。正規表現リテラルではなくパターン文字列を受け取り、正規表現リテラル API と一貫したテキスト優先の引数順序を使用します:

| 関数 | シグネチャ | 説明 |
|----------|-----------|------|
| `regex_match` | `(text: str, pattern: str) -> bool` | テキスト全体がパターンにマッチするかを返す |
| `regex_search` | `(text: str, pattern: str) -> int` | 最初のマッチの開始位置を返す（見つからない場合 -1） |
| `regex_replace` | `(text: str, pattern: str, replacement: str) -> str` | マッチした部分を置換文字列で置換 |
| `regex_split` | `(text: str, pattern: str) -> List<str>` | パターンマッチでテキストを分割 |
| `regex_find_all` | `(text: str, pattern: str) -> List<str>` | 重複しないすべてのマッチを返す |

```ry
print(regex_match("hello", "[a-z]+"))   # true
pos = regex_search("abc123", "[0-9]+")  # 3
```

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
| `\b` | 単語境界 | `"\bword\b"` は単語全体にマッチ |
| `\B` | 非単語境界 | `"\Bword"` は単語の内部にマッチ |
| `(?i)` | 大文字小文字無視フラグ | `"(?i)hello"` は `"HELLO"` にマッチ |
| `\.` | エスケープされた特殊文字 | `"\."` はリテラルの `.` にマッチ |

## 使用例

### 範囲量指定子

```ry
print(regex_match("123-4567", "\\d{3}-\\d{4}"))  # true
print(regex_match("aaa", "a{2,4}"))               # true
print(regex_match("ababab", "(ab){2,}"))           # true
```

### 非貪欲（最短）マッチ

```ry
# 貪欲: 最長マッチ
g = regex_replace("\"a\" and \"b\"", "\".*\"", "X")
print(g)  # X

# 非貪欲: 最短マッチ
l = regex_replace("\"a\" and \"b\"", "\".*?\"", "X")
print(l)  # X and X

# 個別の HTML タグを取得
tags = regex_find_all("<a> <bb> <ccc>", "<.*?>")
print(length(tags))  # 3
```

> **注意:** 非貪欲マッチはマッチ全体の長さを制御します。括弧で囲んだグループの抽出がサポートされていないため、greedy/lazy の混在パターンは PCRE エンジンと異なる動作をする場合があります。

### 単語境界

```ry
# 単語全体にマッチ
pos = regex_search("hello world", "\\bworld\\b")
print(pos)  # 6

# すべての単語を取得
words = regex_find_all("hello world foo", "\\b\\w+\\b")
print(length(words))  # 3
```

### 大文字小文字を無視したマッチ

```ry
# (?i) をパターンの先頭に置くと大文字小文字を無視
print(regex_match("HELLO", "(?i)hello"))  # true
print(regex_match("Hello", "(?i)hello"))  # true
```

> **注意:** `(?i)` はパターンの先頭に記述する必要があり、パターン全体に適用されます。部分的な大文字小文字無視（例: `(?i:sub)pattern`）はサポートされていません。
