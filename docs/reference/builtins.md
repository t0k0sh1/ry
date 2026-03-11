# 組み込み関数リファレンス

## 関数一覧

| 関数 | 説明 |
|------|------|
| `print(expr)` | 値を標準出力に表示 |
| `Some(expr)` | Option型の値ありバリアントを構築 |
| `unwrap(opt)` | Option値を取り出す |
| `len(x)` | リスト・マップの要素数、文字列の長さを返す |
| `has_key(map, key)` | マップにキーが存在するかを返す |
| `contains(str, sub)` | 文字列に部分文字列が含まれるかを返す |
| `starts_with(str, prefix)` | 文字列が接頭辞で始まるかを返す |
| `ends_with(str, suffix)` | 文字列が接尾辞で終わるかを返す |
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

```python
print(42)          # 42
print(3.14)        # 3.14
print(true)        # true
print("hello")     # hello
print(Some(1))     # Some(1)
print(None)        # None
print([1, 2, 3])   # [1, 2, 3]
print({"a": 1})    # {a: 1}
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

**シグネチャ:** `len(x: list[T] | map[K, V] | str) -> int`

リスト・マップの要素数、または文字列のバイト長を返します。

```python
print(len([1, 2, 3]))         # 3
print(len({"a": 1, "b": 2})) # 2
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
