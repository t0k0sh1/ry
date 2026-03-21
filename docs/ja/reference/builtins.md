[English](../../reference/builtins.md) | [日本語](builtins.md) | [繁體中文](../../zh/reference/builtins.md)

# 組み込み関数リファレンス

## 関数一覧

### コア

| 関数 | 説明 |
|------|------|
| `print(expr)` | 値を標準出力に表示 |
| `len(x)` | リスト・マップ・セットの要素数、文字列の UTF-8 文字数を返す |
| `range(n)` / `range(start, end)` / `range(start, end, step)` | 整数のリストを生成 |
| `exit(code)` | 指定した終了コードでプロセスを終了 |
| `args()` | コマンドライン引数を `List<str>` として返す |

### Option

| 関数 | 説明 |
|------|------|
| `Some(expr)` | Option型の値ありバリアントを構築 |

### コレクション操作

| 関数 | 説明 |
|------|------|
| `has_key(map, key)` | マップにキーが存在するかを返す |
| `add(set, value)` | セットに要素を追加（重複は無視） |
| `remove(set, value)` | セットから要素を削除 |
| `append(list, value)` / `append!(list, value)` | リストの末尾に要素を追加（ミューテーション操作） |
| `appended(list, value)` | 要素を末尾に追加した新しいリストを返す（非破壊） |
| `pop(list)` | リストの末尾の要素を削除して `Option<T>` として返す |
| `reverse(list)` | 逆順の新しいリストを返す（文字列にも対応） |
| `reverse!(list)` | リストをその場で逆順にする（ミューテーション操作） |
| `slice(list, start, end)` | start から end までの新しい部分リストを返す |
| `take(list, n)` | 先頭 n 要素の新しいリストを返す |
| `tap(list, fn)` | 各要素に fn を呼び出し、元のリストを返す |
| `filter(list, pred)` | 述語を満たす要素だけの新しいリストを返す |
| `map(list, fn)` | 各要素を変換した新しいリストを返す |
| `sort(list)` / `sort(list, comp)` | ソート済みの新しいリストを返す（デフォルト昇順） |
| `sort!(list)` / `sort!(list, comp)` | リストをその場でソートする（ミューテーション操作） |
| `insert(list, i, val)` | インデックス i に要素を挿入 |
| `remove_at(list, i)` | インデックス i の要素を削除して返す |
| `items(map)` | (キー, 値) タプルのリストを返す |
| `remove(map, key)` | 指定したキーのエントリを削除 |
| `get(map, key)` | キーの値を `Option<V>` として返す |
| `get(map, key, default)` | キーの値を返す（存在しない場合はデフォルト値） |
| `union(set, set)` | 2つのセットの和集合を返す |
| `intersection(set, set)` | 2つのセットの積集合を返す |
| `difference(set, set)` | 2つのセットの差集合を返す |
| `symmetric_difference(set, set)` | 2つのセットの対称差を返す |
| `is_subset(set, set)` | 最初のセットが2番目の部分集合かを返す |
| `is_superset(set, set)` | 最初のセットが2番目の上位集合かを返す |

### [文字列操作](builtins-string.md)

| 関数 | 説明 |
|------|------|
| `contains(s, sub)` | 部分文字列が含まれるか |
| `starts_with(s, prefix)` | 接頭辞で始まるか |
| `ends_with(s, suffix)` | 接尾辞で終わるか |
| `find(s, sub)` | 部分文字列の文字位置（`Option<int>`） |
| `byte_len(s)` | 文字列のバイト長を返す |
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

## len

**シグネチャ:** `len(x: List<T> | Map<K, V> | Set<T> | str) -> int`

リスト・マップ・セットの要素数、または文字列の UTF-8 文字数を返します。バイト長が必要な場合は `byte_len()` を使用してください。

```python
print(len([1, 2, 3]))         # 3
print(len({"a": 1, "b": 2})) # 2
print(len({1, 2, 3}))         # 3
print(len("hello"))           # 5
print(len("あいう"))           # 3 (UTF-8 文字数)
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

**シグネチャ:** `range(n: int) -> List<int>` / `range(start: int, end: int) -> List<int>` / `range(start: int, end: int, step: int) -> List<int>`

整数のリストを生成します。

| 形式 | 生成される値 |
|------|------------|
| `range(n)` | `[0, 1, ..., n-1]` |
| `range(start, end)` | `[start, start+1, ..., end-1]` |
| `range(start, end, step)` | `[start, start+step, start+2*step, ...]` (`end` は含まない) |

- `step > 0` の場合、`start` から昇順に `end` に向かって生成します。
- `step < 0` の場合、`start` から降順に `end` に向かって生成します。
- `step == 0` の場合、ランタイムエラーになります。

```python
print(range(3))           # [0, 1, 2]
print(range(2, 5))        # [2, 3, 4]
print(range(0, 10, 2))    # [0, 2, 4, 6, 8]
print(range(10, 0, -3))   # [10, 7, 4, 1]

for i in range(3):
    print(i)
# 0
# 1
# 2
```

---

## exit

**シグネチャ:** `exit(code: int)`

指定した終了コードでプロセスを即座に終了します。`exit()` 以降のコードは到達不能になります。

```python
exit(0)        # 正常終了
exit(1)        # エラー終了
```

---

## args

**シグネチャ:** `args() -> List<str>`

スクリプトに渡されたコマンドライン引数を文字列のリストとして返します。インタープリター名やスクリプトファイル名は含まれません — スクリプトパスの後の引数のみです。

```python
# 実行: ry script.ry hello world
let a = args()
print(len(a))    # 2
print(a[0])      # hello
print(a[1])      # world

for x in args():
    print(x)
```

---

## append

**シグネチャ:** `append(list: List<T>, value: T)`

リストの末尾に要素を追加します。これはミューテーション操作で、リストがその場で変更されます。UFCS記法も使用可能です。

```python
var xs = [1, 2]
xs.append(3)
print(xs)   # [1, 2, 3]
```

---

## pop

**シグネチャ:** `pop(list: List<T>) -> Option<T>`

リストの末尾の要素を削除して `Option<T>` として返します。リストが空の場合は `None` を返します。UFCS記法も使用可能です。

```python
var xs = [1, 2, 3]
let v = xs.pop()
print(v)    # Some(3)
print(xs)   # [1, 2]
```

---

## reverse (list)

**シグネチャ:** `reverse(list: List<T>) -> List<T>`

要素を逆順にした新しいリストを返します。元のリストは変更されません。文字列に対しても使用できます（[文字列操作関数リファレンス](builtins-string.md)を参照）。UFCS記法も使用可能です。

```python
let xs = [1, 2, 3]
let ys = reverse(xs)
print(ys)   # [3, 2, 1]
print(xs)   # [1, 2, 3]（変更なし）
```

---

## slice

**シグネチャ:** `slice(list: List<T>, start: int, end: int) -> List<T>`

`start`（含む）から `end`（含まない）までの新しい部分リストを返します。インデックスは有効範囲（`0` から `len(list)` まで）にクランプされます。UFCS記法も使用可能です。

```python
let xs = [1, 2, 3, 4, 5]
print(slice(xs, 1, 3))     # [2, 3]
print(slice(xs, 0, 100))   # [1, 2, 3, 4, 5]（クランプされる）
```

---

## take

**シグネチャ:** `take(list: List<T>, n: int) -> List<T>`

先頭 `n` 要素の新しいリストを返します。`n` がリストの長さを超える場合はリスト全体のコピーを返します。`n <= 0` の場合は空リストを返します。元のリストは変更されません。UFCS記法も使用可能です。

```python
let xs = [1, 2, 3, 4, 5]
let ys = xs.take(3)
print(ys)   # [1, 2, 3]
print(xs.take(10))   # [1, 2, 3, 4, 5]（クランプされる）
print(xs.take(0))    # []
```

---

## tap

**シグネチャ:** `tap(list: List<T>, fn: fn(T) -> R) -> List<T>`

各要素に対して関数を呼び出し（戻り値は無視）、元のリストをそのまま返します。メソッドチェーン中のデバッグや副作用の挿入に有用です。UFCS記法も使用可能です。

```python
let xs = [1, 2, 3]
let ys = xs.tap(fn(x: int): print(x)).map(fn(x: int): x * 2)
# 1, 2, 3 を出力し、ys = [2, 4, 6]
```

---

## filter

**シグネチャ:** `filter(list: List<T>, pred: fn(T) -> bool) -> List<T>`

述語が `true` を返す要素のみを含む新しいリストを返します。元のリストは変更されません。UFCS記法も使用可能です。

```python
let xs = [1, 2, 3, 4, 5]
let ys = xs.filter((x: int) -> x > 3)
print(ys)   # [4, 5]
print(xs)   # [1, 2, 3, 4, 5]  （変更なし）
```

---

## map

**シグネチャ:** `map(list: List<T>, fn: fn(T) -> U) -> List<U>`

各要素を関数で変換した新しいリストを返します。出力の要素型は入力と異なっても構いません。元のリストは変更されません。UFCS記法も使用可能です。

```python
let xs = [1, 2, 3]
let ys = xs.map((x: int) -> x * 2)
print(ys)   # [2, 4, 6]
```

---

## sort

**シグネチャ:** `sort(list: List<T>) -> List<T>` / `sort(list: List<T>, comp: fn(T, T) -> bool) -> List<T>`

ソート済みの新しいリストを返します。デフォルトは昇順です。カスタム比較関数を指定できます（第一引数が第二引数の前に来るべき場合に `true` を返す）。元のリストは変更されません。ソートは**安定**です（等しい要素の元の順序が保持されます）。UFCS記法も使用可能です。

```python
let xs = [3, 1, 2]
print(xs.sort())   # [1, 2, 3]

# 降順ソート
let desc = xs.sort((a: int, b: int) -> a > b)
print(desc)   # [3, 2, 1]
```

---

## sort!

**シグネチャ:** `sort!(list: List<T>)` / `sort!(list: List<T>, comp: fn(T, T) -> bool)`

リストをその場でソートします。ソートアルゴリズムは `sort()` と同じですが、新しいリストを作成する代わりに元のリストを変更します。UFCS記法も使用可能です。

```python
var xs = [3, 1, 2]
xs.sort!()
print(xs)   # [1, 2, 3]
```

---

## reverse!

**シグネチャ:** `reverse!(list: List<T>)`

リストをその場で逆順にします。UFCS記法も使用可能です。

```python
var xs = [1, 2, 3]
xs.reverse!()
print(xs)   # [3, 2, 1]
```

---

## appended

**シグネチャ:** `appended(list: List<T>, value: T) -> List<T>`

要素を末尾に追加した新しいリストを返します。元のリストは変更されません。UFCS記法も使用可能です。

```python
let xs = [1, 2]
let ys = xs.appended(3)
print(xs)   # [1, 2]（変更なし）
print(ys)   # [1, 2, 3]
```

---

## append!

**シグネチャ:** `append!(list: List<T>, value: T)`

`append()` のエイリアスです。リストの末尾に要素をその場で追加します。`!` 命名規約との一貫性のために提供されています。

---

## first

**シグネチャ:** `first(list: List<T>) -> Option<T>`

リストの最初の要素を `Option<T>` として返します。リストが空の場合は `None` を返します。

```python
print(first([10, 20, 30]))   # Some(10)
```

---

## last

**シグネチャ:** `last(list: List<T>) -> Option<T>`

リストの最後の要素を `Option<T>` として返します。リストが空の場合は `None` を返します。

```python
print(last([10, 20, 30]))   # Some(30)
```

---

## get (Map)

**シグネチャ:** `get(map: Map<K, V>, key: K) -> Option<V>` / `get(map: Map<K, V>, key: K, default: V) -> V`

2引数形式はキーの値を `Option<V>` として返します。3引数形式はキーの値またはデフォルト値を返します。

```python
let m = {"a": 1, "b": 2}
print(get(m, "a"))       # Some(1)
print(get(m, "z"))       # None
print(get(m, "z", 0))   # 0
```
