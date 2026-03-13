# 文字列操作関数リファレンス

文字列（`str`）に対する操作関数の一覧です。すべての関数で UFCS 記法が使用可能です。

> **注意:** すべての文字列操作はバイト単位です。マルチバイト文字（日本語等）では正しく動作しない場合があります。

## 関数一覧

### 検索・判定

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `contains` | `(str, str) → bool` | 部分文字列が含まれるかを返す |
| `starts_with` | `(str, str) → bool` | 接頭辞で始まるかを返す |
| `ends_with` | `(str, str) → bool` | 接尾辞で終わるかを返す |
| `find` | `(str, str) → int` | 部分文字列の位置を返す（未発見は -1） |

### 抽出・変換

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `substring` | `(str, int, int) → str` | 部分文字列を取得 |
| `char_at` | `(str, int) → str` | 指定位置の文字を取得 |
| `replace` | `(str, str, str) → str` | 部分文字列を全置換 |

### 大文字・小文字

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `to_upper` | `str → str` | ASCII 大文字に変換 |
| `to_lower` | `str → str` | ASCII 小文字に変換 |

### 空白除去

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `trim` | `str → str` | 前後の空白を除去 |
| `trim_start` | `str → str` | 先頭の空白を除去 |
| `trim_end` | `str → str` | 末尾の空白を除去 |

### 生成・加工

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `repeat` | `(str, int) → str` | 文字列を n 回繰り返す |
| `reverse` | `str → str` | 文字列を逆順にする |

### 分割・結合

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `split` | `(str, str) → List<str>` | デリミタで分割 |
| `join` | `(List<str>, str) → str` | セパレータで結合 |

### 型変換

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `to_int` | `str → int` | 文字列を整数に変換 |
| `to_float` | `str → float` | 文字列を浮動小数点数に変換 |
| `to_str` | `int/float/bool/str → str` | 値を文字列に変換 |

---

## contains

**シグネチャ:** `contains(s: str, sub: str) -> bool`

文字列 `s` に部分文字列 `sub` が含まれるかを返します。

```python
print(contains("hello", "ell"))   # true
print("hello".contains("xyz"))    # false (UFCS)
```

---

## starts_with

**シグネチャ:** `starts_with(s: str, prefix: str) -> bool`

文字列 `s` が `prefix` で始まるかを返します。

```python
print(starts_with("hello", "hel"))   # true
print("hello".starts_with("world"))  # false (UFCS)
```

---

## ends_with

**シグネチャ:** `ends_with(s: str, suffix: str) -> bool`

文字列 `s` が `suffix` で終わるかを返します。

```python
print(ends_with("hello", "llo"))   # true
print("hello".ends_with("world"))  # false (UFCS)
```

---

## find

**シグネチャ:** `find(s: str, sub: str) -> int`

文字列 `s` 中の部分文字列 `sub` の最初の出現位置（バイトオフセット）を返します。見つからない場合は `-1` を返します。

```python
print(find("hello world", "world"))   # 6
print(find("hello", "xyz"))           # -1
print("abcdef".find("cd"))            # 2 (UFCS)
```

---

## substring

**シグネチャ:** `substring(s: str, start: int, end: int) -> str`

文字列 `s` の `start` から `end`（排他）までの部分文字列を返します。

```python
print(substring("hello world", 0, 5))   # hello
print(substring("hello world", 6, 11))  # world
print("abcdef".substring(1, 4))         # bcd (UFCS)
```

---

## char_at

**シグネチャ:** `char_at(s: str, i: int) -> str`

文字列 `s` の `i` 番目のバイトを1文字の文字列として返します。

```python
print(char_at("hello", 0))   # h
print("abc".char_at(2))       # c (UFCS)
```

---

## replace

**シグネチャ:** `replace(s: str, old: str, new: str) -> str`

文字列 `s` 中の `old` をすべて `new` に置換した新しい文字列を返します。

```python
print(replace("hello world", "world", "ry"))   # hello ry
print(replace("aaa", "a", "bb"))                # bbbbbb
print("foo bar foo".replace("foo", "baz"))      # baz bar baz (UFCS)
```

---

## to_upper

**シグネチャ:** `to_upper(s: str) -> str`

ASCII 小文字（a-z）を大文字に変換した新しい文字列を返します。

```python
print(to_upper("hello"))         # HELLO
print("Hello World".to_upper())  # HELLO WORLD (UFCS)
```

---

## to_lower

**シグネチャ:** `to_lower(s: str) -> str`

ASCII 大文字（A-Z）を小文字に変換した新しい文字列を返します。

```python
print(to_lower("HELLO"))         # hello
print("Hello World".to_lower())  # hello world (UFCS)
```

---

## trim

**シグネチャ:** `trim(s: str) -> str`

文字列の前後の空白文字（スペース、タブ、改行、復帰）を除去した新しい文字列を返します。

```python
print(trim("  hello  "))   # hello
print("  hi  ".trim())     # hi (UFCS)
```

---

## trim_start

**シグネチャ:** `trim_start(s: str) -> str`

文字列の先頭の空白文字を除去した新しい文字列を返します。

```python
print(trim_start("  hello  "))   # hello
print("  hi".trim_start())       # hi (UFCS)
```

---

## trim_end

**シグネチャ:** `trim_end(s: str) -> str`

文字列の末尾の空白文字を除去した新しい文字列を返します。

```python
print(trim_end("  hello  "))   #   hello
print("hi  ".trim_end())       # hi (UFCS)
```

---

## repeat

**シグネチャ:** `repeat(s: str, n: int) -> str`

文字列 `s` を `n` 回繰り返した新しい文字列を返します。

```python
print(repeat("ab", 3))     # ababab
print("ha".repeat(3))      # hahaha (UFCS)
```

---

## reverse

**シグネチャ:** `reverse(s: str) -> str`

文字列をバイト単位で逆順にした新しい文字列を返します。

```python
print(reverse("hello"))    # olleh
print("abc".reverse())     # cba (UFCS)
```

---

## split

**シグネチャ:** `split(s: str, delim: str) -> List<str>`

文字列 `s` をデリミタ `delim` で分割し、`List<str>` を返します。

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

**シグネチャ:** `join(xs: List<str>, sep: str) -> str`

文字列リストの要素をセパレータ `sep` で結合した文字列を返します。

```python
let parts = ["a", "b", "c"]
print(join(parts, ","))        # a,b,c
print(parts.join("-"))         # a-b-c (UFCS)
```

---

## to_int

**シグネチャ:** `to_int(s: str) -> int`

文字列を整数に変換します。

```python
print(to_int("42"))       # 42
print(to_int("-7"))       # -7
print("123".to_int())     # 123 (UFCS)
```

---

## to_float

**シグネチャ:** `to_float(s: str) -> float`

文字列を浮動小数点数に変換します。

```python
print(to_float("3.14"))   # 3.14
print("2.5".to_float())   # 2.5 (UFCS)
```

---

## to_str

**シグネチャ:** `to_str(v: int | float | bool | str) -> str`

値を文字列に変換します。

| 型 | 変換形式 |
|----|---------|
| `int` | `%ld` |
| `float` | `%g` |
| `bool` | `"true"` / `"false"` |
| `str` | そのまま返す |

```python
print(to_str(42))         # 42
print(to_str(3.14))       # 3.14
print(to_str(true))       # true
print(99.to_str())        # 99 (UFCS)
```
