[English](../../reference/builtins.md) | [日本語](builtins.md) | [繁體中文](../../zh/reference/builtins.md)

# 組み込み関数リファレンス

## 関数一覧

### コア

| 関数 | 説明 |
|------|------|
| `print(expr)` | 値を標準出力に表示 |
| `len(x)` | リスト・マップ・セットの要素数、文字列の長さを返す |
| `range(n)` / `range(start, end)` | 整数のリストを生成 |

### Option

| 関数 | 説明 |
|------|------|
| `Some(expr)` | Option型の値ありバリアントを構築 |
| `unwrap(opt)` | Option値を取り出す |

### コレクション操作

| 関数 | 説明 |
|------|------|
| `has_key(map, key)` | マップにキーが存在するかを返す |
| `add(set, value)` | セットに要素を追加（重複は無視） |
| `remove(set, value)` | セットから要素を削除 |

### [文字列操作](builtins-string.md)

| 関数 | 説明 |
|------|------|
| `contains(s, sub)` | 部分文字列が含まれるか |
| `starts_with(s, prefix)` | 接頭辞で始まるか |
| `ends_with(s, suffix)` | 接尾辞で終わるか |
| `find(s, sub)` | 部分文字列の位置（未発見は -1） |
| `substring(s, start, end)` | 部分文字列を取得 |
| `char_at(s, i)` | 指定位置の文字を取得 |
| `replace(s, old, new)` | 部分文字列を全置換 |
| `to_upper(s)` / `to_lower(s)` | 大文字・小文字変換 |
| `trim(s)` / `trim_start(s)` / `trim_end(s)` | 空白除去 |
| `repeat(s, n)` | 文字列を n 回繰り返す |
| `reverse(s)` | 文字列を逆順にする |
| `split(s, delim)` | 文字列を分割してリストを返す |
| `join(list, sep)` | リストの文字列をセパレータで結合 |
| `to_int(s)` / `to_float(s)` / `to_str(v)` | 型変換 |

→ 詳細は **[文字列操作関数リファレンス](builtins-string.md)** を参照

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

**シグネチャ:** `len(x: List<T> | Map<K, V> | Set<T> | str) -> int`

リスト・マップ・セットの要素数、または文字列のバイト長を返します。

```python
print(len([1, 2, 3]))         # 3
print(len({"a": 1, "b": 2})) # 2
print(len({1, 2, 3}))         # 3
print(len("hello"))           # 5
```

---

## has_key

**シグネチャ:** `has_key(m: Map<K, V>, key: K) -> bool`

マップに指定したキーが存在するかを返します。UFCS記法も使用可能です。

```python
let m = {"a": 1, "b": 2}
print(has_key(m, "a"))    # true
print(m.has_key("z"))     # false (UFCS)
```

---

## add

**シグネチャ:** `add(s: Set<T>, value: T)`

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

**シグネチャ:** `remove(s: Set<T>, value: T)`

セットから要素を削除します。UFCS記法も使用可能です。

```python
let s = {1, 2, 3}
s.remove(2)       # UFCS
print(2 in s)     # false
```

---

## range

**シグネチャ:** `range(n: int) -> List<int>` / `range(start: int, end: int) -> List<int>`

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
