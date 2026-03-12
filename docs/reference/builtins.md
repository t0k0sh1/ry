# 組み込み関数リファレンス

## 関数一覧

| 関数 | 説明 |
|------|------|
| `print(expr)` | 値を標準出力に表示 |
| `Some(expr)` | Option型の値ありバリアントを構築 |
| `unwrap(opt)` | Option値を取り出す |
| `len(x)` | リスト・マップ・セットの要素数、文字列の長さを返す |
| `has_key(map, key)` | マップにキーが存在するかを返す |
| `contains(str, sub)` | 文字列に部分文字列が含まれるかを返す |
| `starts_with(str, prefix)` | 文字列が接頭辞で始まるかを返す |
| `ends_with(str, suffix)` | 文字列が接尾辞で終わるかを返す |
| `to_int(s)` | 文字列を整数に変換 |
| `to_float(s)` | 文字列を浮動小数点数に変換 |
| `to_str(v)` | 値を文字列に変換 |
| `find(s, sub)` | 部分文字列の位置を返す（見つからなければ -1） |
| `substring(s, start, end)` | 部分文字列を取得 |
| `char_at(s, i)` | 指定位置の文字を取得 |
| `replace(s, old, new)` | 部分文字列を全置換 |
| `to_upper(s)` | 大文字に変換 |
| `to_lower(s)` | 小文字に変換 |
| `trim(s)` | 前後の空白を除去 |
| `trim_start(s)` | 先頭の空白を除去 |
| `trim_end(s)` | 末尾の空白を除去 |
| `repeat(s, n)` | 文字列を n 回繰り返す |
| `reverse(s)` | 文字列を逆順にする |
| `split(s, delim)` | 文字列を分割してリストを返す |
| `join(list, sep)` | リストの文字列をセパレータで結合 |
| `add(set, value)` | セットに要素を追加（重複は無視） |
| `remove(set, value)` | セットから要素を削除 |
| `range(n)` / `range(start, end)` | 整数のリストを生成 |

---

## print

**シグネチャ:** `print(expr)`

値を標準出力に表示します。末尾に改行が付きます。

| 型 | 出力形式 |
|----|---------|
| `int` | `%ld` |
| `float` | `%g` |
| `bool` | `true` / `false` |
| `str` | `%s` |
| `Option` (Some) | `Some(値)` |
| `Option` (None) | `None` |
| `list` | `[要素1, 要素2, ...]` |
| `map` | `{キー1: 値1, キー2: 値2, ...}` |
| `set` | `{要素1, 要素2, ...}` |
| `enum` | バリアント名（例: `Red`） |

```python
print(42)          # 42
print(3.14)        # 3.14
print(true)        # true
print("hello")     # hello
print(Some(1))     # Some(1)
print(None)        # None
print([1, 2, 3])   # [1, 2, 3]
print({"a": 1})    # {a: 1}
print({1, 2, 3})   # {1, 2, 3}
```

**エラー条件:** 構造体・タプルを直接渡すとコンパイルエラー。

---

## Some

**シグネチャ:** `Some(expr) -> Option<T>`

Option型の値ありバリアントを構築します。

```python
let x: Option<int> = Some(42)
print(x)   # Some(42)
```

---

## unwrap

**シグネチャ:** `unwrap(opt: Option<T>) -> T`

Option値から中身を取り出します。UFCS記法も使用可能です。

```python
let x = Some(42)
print(unwrap(x))    # 42
print(x.unwrap())   # 42 (UFCS)
```

**エラー条件:** `None` を渡すとランタイムエラー（exit(1)）。

---

## len

**シグネチャ:** `len(x: list[T] | map[K, V] | set[T] | str) -> int`

リスト・マップ・セットの要素数、または文字列のバイト長を返します。

```python
print(len([1, 2, 3]))         # 3
print(len({"a": 1, "b": 2})) # 2
print(len({1, 2, 3}))         # 3
print(len("hello"))           # 5
```

---

## has_key

**シグネチャ:** `has_key(m: map[K, V], key: K) -> bool`

マップに指定したキーが存在するかを返します。UFCS記法も使用可能です。

```python
let m = {"a": 1, "b": 2}
print(has_key(m, "a"))    # true
print(m.has_key("z"))     # false (UFCS)
```

---

## contains

**シグネチャ:** `contains(s: str, sub: str) -> bool`

文字列 `s` に部分文字列 `sub` が含まれるかを返します。UFCS記法も使用可能です。

```python
print(contains("hello", "ell"))   # true
print("hello".contains("xyz"))    # false (UFCS)
```

---

## starts_with

**シグネチャ:** `starts_with(s: str, prefix: str) -> bool`

文字列 `s` が `prefix` で始まるかを返します。UFCS記法も使用可能です。

```python
print(starts_with("hello", "hel"))   # true
print("hello".starts_with("world"))  # false (UFCS)
```

---

## ends_with

**シグネチャ:** `ends_with(s: str, suffix: str) -> bool`

文字列 `s` が `suffix` で終わるかを返します。UFCS記法も使用可能です。

```python
print(ends_with("hello", "llo"))   # true
print("hello".ends_with("world"))  # false (UFCS)
```

---

## to_int

**シグネチャ:** `to_int(s: str) -> int`

文字列を整数に変換します。UFCS記法も使用可能です。

```python
print(to_int("42"))       # 42
print(to_int("-7"))       # -7
print("123".to_int())     # 123 (UFCS)
```

---

## to_float

**シグネチャ:** `to_float(s: str) -> float`

文字列を浮動小数点数に変換します。UFCS記法も使用可能です。

```python
print(to_float("3.14"))   # 3.14
print("2.5".to_float())   # 2.5 (UFCS)
```

---

## to_str

**シグネチャ:** `to_str(v: int | float | bool | str) -> str`

値を文字列に変換します。UFCS記法も使用可能です。

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

---

## find

**シグネチャ:** `find(s: str, sub: str) -> int`

文字列 `s` 中の部分文字列 `sub` の最初の出現位置（バイトオフセット）を返します。見つからない場合は `-1` を返します。UFCS記法も使用可能です。

```python
print(find("hello world", "world"))   # 6
print(find("hello", "xyz"))           # -1
print("abcdef".find("cd"))            # 2 (UFCS)
```

---

## substring

**シグネチャ:** `substring(s: str, start: int, end: int) -> str`

文字列 `s` の `start` から `end`（排他）までの部分文字列を返します。UFCS記法も使用可能です。

```python
print(substring("hello world", 0, 5))   # hello
print(substring("hello world", 6, 11))  # world
print("abcdef".substring(1, 4))         # bcd (UFCS)
```

---

## char_at

**シグネチャ:** `char_at(s: str, i: int) -> str`

文字列 `s` の `i` 番目のバイトを1文字の文字列として返します。UFCS記法も使用可能です。

```python
print(char_at("hello", 0))   # h
print("abc".char_at(2))       # c (UFCS)
```

---

## replace

**シグネチャ:** `replace(s: str, old: str, new: str) -> str`

文字列 `s` 中の `old` をすべて `new` に置換した新しい文字列を返します。UFCS記法も使用可能です。

```python
print(replace("hello world", "world", "ry"))   # hello ry
print(replace("aaa", "a", "bb"))                # bbbbbb
print("foo bar foo".replace("foo", "baz"))      # baz bar baz (UFCS)
```

---

## to_upper

**シグネチャ:** `to_upper(s: str) -> str`

ASCII小文字（a-z）を大文字に変換した新しい文字列を返します。UFCS記法も使用可能です。

```python
print(to_upper("hello"))         # HELLO
print("Hello World".to_upper())  # HELLO WORLD (UFCS)
```

---

## to_lower

**シグネチャ:** `to_lower(s: str) -> str`

ASCII大文字（A-Z）を小文字に変換した新しい文字列を返します。UFCS記法も使用可能です。

```python
print(to_lower("HELLO"))         # hello
print("Hello World".to_lower())  # hello world (UFCS)
```

---

## trim

**シグネチャ:** `trim(s: str) -> str`

文字列の前後の空白文字（スペース、タブ、改行、復帰）を除去した新しい文字列を返します。UFCS記法も使用可能です。

```python
print(trim("  hello  "))   # hello
print("  hi  ".trim())     # hi (UFCS)
```

---

## trim_start

**シグネチャ:** `trim_start(s: str) -> str`

文字列の先頭の空白文字を除去した新しい文字列を返します。UFCS記法も使用可能です。

```python
print(trim_start("  hello  "))   # hello
print("  hi".trim_start())       # hi (UFCS)
```

---

## trim_end

**シグネチャ:** `trim_end(s: str) -> str`

文字列の末尾の空白文字を除去した新しい文字列を返します。UFCS記法も使用可能です。

```python
print(trim_end("  hello  "))   #   hello
print("hi  ".trim_end())       # hi (UFCS)
```

---

## repeat

**シグネチャ:** `repeat(s: str, n: int) -> str`

文字列 `s` を `n` 回繰り返した新しい文字列を返します。UFCS記法も使用可能です。

```python
print(repeat("ab", 3))     # ababab
print("ha".repeat(3))      # hahaha (UFCS)
```

---

## reverse

**シグネチャ:** `reverse(s: str) -> str`

文字列をバイト単位で逆順にした新しい文字列を返します。UFCS記法も使用可能です。

```python
print(reverse("hello"))    # olleh
print("abc".reverse())     # cba (UFCS)
```

**注意:** バイト単位の逆転のため、マルチバイト文字（日本語等）では正しく動作しません。

---

## split

**シグネチャ:** `split(s: str, delim: str) -> list[str]`

文字列 `s` をデリミタ `delim` で分割し、`list[str]` を返します。UFCS記法も使用可能です。

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

**シグネチャ:** `join(xs: list[str], sep: str) -> str`

文字列リストの要素をセパレータ `sep` で結合した文字列を返します。UFCS記法も使用可能です。

```python
let parts = ["a", "b", "c"]
print(join(parts, ","))        # a,b,c
print(parts.join("-"))         # a-b-c (UFCS)
```

---

## add

**シグネチャ:** `add(s: set[T], value: T)`

セットに要素を追加します。既に存在する要素を追加した場合は何もしません。UFCS記法も使用可能です。

```python
let s = {1, 2, 3}
s.add(4)          # UFCS
add(s, 5)         # 通常の呼び出し
s.add(1)          # 既に存在するため無視
print(len(s))     # 5
```

---

## remove

**シグネチャ:** `remove(s: set[T], value: T)`

セットから要素を削除します。UFCS記法も使用可能です。

```python
let s = {1, 2, 3}
s.remove(2)       # UFCS
print(2 in s)     # false
```

---

## range

**シグネチャ:** `range(n: int) -> list[int]` / `range(start: int, end: int) -> list[int]`

整数のリストを生成します。

| 形式 | 生成される値 |
|------|------------|
| `range(n)` | `[0, 1, ..., n-1]` |
| `range(start, end)` | `[start, start+1, ..., end-1]` |

```python
print(range(3))       # [0, 1, 2]
print(range(2, 5))    # [2, 3, 4]

for i in range(3):
    print(i)
# 0
# 1
# 2
```
