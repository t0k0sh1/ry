[English](../../reference/builtins-string.md) | [日本語](builtins-string.md) | [繁體中文](../../zh/reference/builtins-string.md)

# 文字列操作関数リファレンス

文字列（`str`）に対する操作関数の一覧です。すべての関数で UFCS 記法が使用可能です。

> **注意:** すべての文字列操作は UTF-8 対応です。`length()`、`char_at()`、`substring()`、`find()`、`reverse()` は Unicode コードポイント単位で動作し、バイト単位ではありません。バイト長が必要な場合は `byte_len()` を使用してください。

## 関数一覧

### 検索・判定

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `contains` | `(str, str, bool = false) -> bool` | 部分文字列が含まれるかを返す |
| `starts_with` | `(str, str, bool = false) -> bool` | 接頭辞で始まるかを返す |
| `ends_with` | `(str, str, bool = false) -> bool` | 接尾辞で終わるかを返す |
| `find` | `(str, str) -> Option<int>` | 部分文字列の文字位置を返す（未発見は `None`） |

### 抽出・変換

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `substring` | `(str, int, int) -> str` | 部分文字列を取得（文字インデックス） |
| `char_at` | `(str, int) -> str` | 指定位置の UTF-8 文字を取得 |
| `replace` | `(str, str, str) -> str` | 部分文字列を全置換 |

### 大文字・小文字

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `to_upper` | `str -> str` | ASCII 大文字に変換 |
| `to_lower` | `str -> str` | ASCII 小文字に変換 |

### 空白除去

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `trim` | `str -> str` | 前後の空白を除去 |
| `trim_start` | `str -> str` | 先頭の空白を除去 |
| `trim_end` | `str -> str` | 末尾の空白を除去 |

### 生成・加工

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `repeat` | `(str, int) -> str` | 文字列を n 回繰り返す |
| `reverse` | `str -> str` | 文字列を逆順にする（UTF-8 対応） |
| `byte_len` | `str -> int` | 文字列のバイト長を返す |

### 分割・結合

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `split` | `(str, str) -> List<str>` | デリミタで分割 |
| `join` | `(List<str>, str) -> str` | セパレータで結合 |

### 型変換

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `to_int` | `str -> Result<int, Error>` | 文字列を整数に変換 |
| `to_float` | `str -> Result<float, Error>` | 文字列を浮動小数点数に変換 |
| `to_str` | `int/float/bool/str/enum/record -> str` | 値を文字列に変換 |

---

## contains

**シグネチャ:** `contains(string: str, substring: str, ignore_case: bool = false) -> bool`

文字列 `string` に部分文字列 `substring` が含まれるかを返します。`ignore_case` が `true` の場合、比較は大文字小文字を区別しません（ASCII のみ）。

```python
print(contains("hello", "ell"))              # true
print("hello".contains("xyz"))               # false (UFCS)
print(contains("Hello World", "hello", true))  # true（大文字小文字区別なし）
```

---

## starts_with

**シグネチャ:** `starts_with(string: str, prefix: str, ignore_case: bool = false) -> bool`

文字列 `string` が `prefix` で始まるかを返します。`ignore_case` が `true` の場合、比較は大文字小文字を区別しません（ASCII のみ）。

```python
print(starts_with("hello", "hel"))              # true
print("hello".starts_with("world"))              # false (UFCS)
print(starts_with("Hello", "hello", true))  # true（大文字小文字区別なし）
```

---

## ends_with

**シグネチャ:** `ends_with(string: str, suffix: str, ignore_case: bool = false) -> bool`

文字列 `string` が `suffix` で終わるかを返します。`ignore_case` が `true` の場合、比較は大文字小文字を区別しません（ASCII のみ）。

```python
print(ends_with("hello", "llo"))              # true
print("hello".ends_with("world"))              # false (UFCS)
print(ends_with("Hello World", "WORLD", true))  # true（大文字小文字区別なし）
```

---

## find

**シグネチャ:** `find(string: str, substring: str) -> Option<int>`

文字列 `string` 中の部分文字列 `substring` の最初の出現位置（文字位置）を返します。見つからない場合は `None` を返します。

```python
print(find("hello world", "world"))   # Some(6)
print(find("hello", "xyz"))           # None
print("abcdef".find("cd"))            # Some(2) (UFCS)
```

---

## substring

**シグネチャ:** `substring(string: str, start: int, end: int) -> str`

文字列 `string` の `start` から `end`（排他）までの部分文字列を返します。インデックスは文字位置（UTF-8 対応）です。

範囲外のインデックスは `[0, length]` にクランプされます。クランプ後に `end < start` の場合は空文字列を返します。

```python
print(substring("hello world", 0, 5))   # hello
print(substring("hello world", 6, 11))  # world
print("abcdef".substring(1, 4))         # bcd (UFCS)
print(substring("hello", -1, 100))      # hello（クランプされる）
```

---

## char_at

**シグネチャ:** `char_at(string: str, i: int) -> str`

文字列 `string` の `i` 番目の UTF-8 文字を文字列として返します。インデックスが範囲外の場合はランタイムエラーになります。

負のインデックスは末尾から数えます（Python スタイル）: `-1` は最後の文字、`-2` は最後から2番目の文字を指します。

```python
print(char_at("hello", 0))    # h
print(char_at("hello", -1))   # o（最後の文字）
print("abc".char_at(2))       # c (UFCS)
```

---

## replace

**シグネチャ:** `replace(string: str, old: str, new: str) -> str`

文字列 `string` 中の `old` をすべて `new` に置換した新しい文字列を返します。

`old` が空文字列の場合、入力はそのまま（新しいコピーとして）返されます。

```python
print(replace("hello world", "world", "ry"))   # hello ry
print(replace("aaa", "a", "bb"))                # bbbbbb
print("foo bar foo".replace("foo", "baz"))      # baz bar baz (UFCS)
print(replace("hello", "", "X"))                # hello（空のパターンは何もしない）
```

---

## to_upper

**シグネチャ:** `to_upper(string: str) -> str`

ASCII 小文字（a-z）を大文字に変換した新しい文字列を返します。

```python
print(to_upper("hello"))         # HELLO
print("Hello World".to_upper())  # HELLO WORLD (UFCS)
```

---

## to_lower

**シグネチャ:** `to_lower(string: str) -> str`

ASCII 大文字（A-Z）を小文字に変換した新しい文字列を返します。

```python
print(to_lower("HELLO"))         # hello
print("Hello World".to_lower())  # hello world (UFCS)
```

---

## trim

**シグネチャ:** `trim(string: str) -> str`

文字列の前後の空白文字（スペース、タブ、改行、復帰）を除去した新しい文字列を返します。

```python
print(trim("  hello  "))   # hello
print("  hi  ".trim())     # hi (UFCS)
```

---

## trim_start

**シグネチャ:** `trim_start(string: str) -> str`

文字列の先頭の空白文字を除去した新しい文字列を返します。

```python
print(trim_start("  hello  "))   # hello
print("  hi".trim_start())       # hi (UFCS)
```

---

## trim_end

**シグネチャ:** `trim_end(string: str) -> str`

文字列の末尾の空白文字を除去した新しい文字列を返します。

```python
print(trim_end("  hello  "))   #   hello
print("hi  ".trim_end())       # hi (UFCS)
```

---

## repeat

**シグネチャ:** `repeat(string: str, count: int) -> str`

文字列 `string` を `count` 回繰り返した新しい文字列を返します。

```python
print(repeat("ab", 3))     # ababab
print("ha".repeat(3))      # hahaha (UFCS)
```

---

## reverse

**シグネチャ:** `reverse(string: str) -> str`

文字列を逆順にした新しい文字列を返します（UTF-8 対応）。

```python
print(reverse("hello"))    # olleh
print("abc".reverse())     # cba (UFCS)
```

---

## byte_len

**シグネチャ:** `byte_len(string: str) -> int`

文字列 `string` のバイト長を返します。UTF-8 文字数を返す `length()` とは異なり、`byte_len()` はバイト数を返します。

```python
print(byte_len("hello"))   # 5
print(byte_len("あいう"))   # 9
print(length("あいう"))        # 3 (文字数)
```

---

## split

**シグネチャ:** `split(string: str, delimiter: str) -> List<str>`

文字列 `string` をデリミタ `delimiter` で分割し、`List<str>` を返します。

デリミタが空文字列 `""` の場合、文字列は個別の文字に分割されます（UTF-8 対応）。

```python
parts = split("a,b,c", ",")
print(parts[0])   # a
print(parts[1])   # b
print(parts[2])   # c

for word in "hello world".split(" "):
    print(word)
# hello
# world

# 文字ごとに分割
chars = split("hello", "")
print(chars)   # [h, e, l, l, o]

# UTF-8 文字
chars = split("あいう", "")
print(chars)   # [あ, い, う]
```

> **Tip:** 文字列を 1 文字ずつ反復するなら、`split` を呼ばずに `for` ループを直接使えます。`for c in s:` は各 UTF-8 コードポイントを 1 文字の `str` として生成します。詳細は [control-flow.md](control-flow.md#文字列の反復) を参照してください。

---

## join

**シグネチャ:** `join(values: List<str>, sep: str) -> str`

文字列リストの要素をセパレータ `sep` で結合した文字列を返します。

```python
parts = ["a", "b", "c"]
print(join(parts, ","))        # a,b,c
print(parts.join("-"))         # a-b-c (UFCS)
print(",".join(parts))         # a,b,c (UFCS, Python スタイル)
```

---

## to_int

**シグネチャ:** `to_int(string: str) -> Result<int, Error>`

文字列を整数に変換します。先頭の空白は許容されます。文字列が空の場合、無効な文字を含む場合、またはオーバーフローする場合は `Err` を返します。

```python
case to_int("42"):
    Ok(v):
        print(v)              # 42
    Err(e):
        print(e.message)

case "123".to_int():                
    Ok(v):
        print(v)              # 123
    Err(e):
        print(e.message)

# 無効な入力は Err を返す
print(to_int("abc"))          # Err(Error("to_int: invalid character in 'abc'"))
print(to_int(""))             # Err(Error("to_int: empty string"))
```

---

## to_float

**シグネチャ:** `to_float(string: str) -> Result<float, Error>`

文字列を浮動小数点数に変換します。文字列が空の場合、無効な文字を含む場合、または `float` の範囲外の場合は `Err` を返します。

```python
case to_float("3.14"):
    Ok(v):
        print(v)              # 3.14
    Err(e):
        print(e.message)

case "2.5".to_float():              
    Ok(v):
        print(v)              # 2.5
    Err(e):
        print(e.message)

# 無効な入力は Err を返す
print(to_float("abc"))         # Err(Error("to_float: invalid character in 'abc'"))
print(to_float(""))            # Err(Error("to_float: empty string"))
print(to_float("1e400"))       # Err(Error("to_float: out of range in '1e400'"))
```

---

## to_str

**シグネチャ:** `to_str(v: int | float | bool | str | enum | record) -> str`

値を文字列に変換します。

| 型 | 変換形式 |
|----|---------|
| `int` | `%ld` |
| `float` | `%g`、整数値の場合は末尾に `.0` を付加（例: `"3.0"`, `"0.0"`） |
| `bool` | `"true"` / `"false"` |
| `str` | そのまま返す |
| enum | バリアント名（例: `"Red"`） |
| record | `TypeName(field1: val1, field2: val2)` |
| `List` / `Map` / `Set` | 再帰的にフォーマットされ、ネストしたコンテナ（例: `Map<str, List<int>>`）もサポート |
| ユニオン | アクティブなバリアントとしてフォーマット。`List`, `Map`, `Set`, 関数バリアントもすべてサポート |
| 関数値（クロージャ / ラムダ） | `"<closure>"` |

整数値の `float` は末尾に `.0` が付加されるため（例: `to_str(3.0) == "3.0"`）、`int` と視覚的に区別できます。record 型は `to_str` 表現を自動生成します。ユーザー定義の `function to_str(v: MyRecord) -> str` が提供されている場合、自動生成バージョンより優先されます。これは `print()` や f-string 補間でも同様に機能します。

```python
print(to_str(42))         # 42
print(to_str(3.14))       # 3.14
print(to_str(3.0))        # 3.0          (整数値の float は .0 を保持)
print(to_str(true))       # true
print(99.to_str())        # 99 (UFCS)

enum Color:
    Red
    Green

print(to_str(Color::Red))   # Red

record Point:
    x: int
    y: int

p = Point(10, 20)
print(to_str(p))          # Point(x: 10, y: 20)
print(p)                  # Point(x: 10, y: 20)
print(f"pos={p}")         # pos=Point(x: 10, y: 20)
```
