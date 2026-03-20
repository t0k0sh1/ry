[English](../../reference/collections.md) | [日本語](collections.md) | [繁體中文](../../zh/reference/collections.md)

# コレクションリファレンス（タプル・リスト・マップ・セット）

## タプル

### 概要

固定長・異種型の値の組み合わせ。LLVM literal StructType として実装されたスタック上の値型です。

### 構文

```python
let t = (1, 3.14)
let t: (int, float) = (1, 3.14)
```

### 型アノテーション

```python
let pair: (int, str) = (42, "hello")
let triple: (int, float, bool) = (1, 2.0, true)
```

### 要素アクセス

`.0`, `.1`, ... の数値インデックスでアクセスします。

```python
let t = (10, 3.14)
print(t.0)   # 10
print(t.1)   # 3.14
```

### 関数戻り値

```python
fn swap(a: int, b: int) -> (int, int):
    return (b, a)

let result = swap(1, 2)
print(result.0)   # 2
print(result.1)   # 1
```

### 制約とエラー

| 制約 | 詳細 |
|------|------|
| 範囲外インデックス | コンパイルエラー |
| `print` にタプルを直接渡す | コンパイルエラー（print非対応） |

---

## リスト

### 概要

同一型の可変長シーケンス。ヒープ上に確保されます。

### 構文

```python
let xs = [1, 2, 3]
let xs: List<int> = [1, 2, 3]
```

### 対応する要素型

`int`, `float`, `bool`, `str`

### インデックスアクセス

```python
let xs = [1, 2, 3]
print(xs[0])   # 1
print(xs[2])   # 3
```

### インデックス代入

```python
let xs = [1, 2, 3]
xs[0] = 99
print(xs[0])   # 99
```

### len

```python
let xs = [1, 2, 3]
print(len(xs))   # 3
```

### print

```python
let xs = [1, 2, 3]
print(xs)   # [1, 2, 3]
```

### for 走査

```python
let xs = [10, 20, 30]
for x in xs:
    print(x)
# 10
# 20
# 30
```

### append

リストの末尾に要素を追加します。これはミューテーション操作で、リストがその場で変更されます。

```python
var xs = [1, 2]
xs.append(3)
print(xs)   # [1, 2, 3]
```

### pop

リストの末尾の要素を削除して返します。空のリストに対して呼び出すとランタイムエラーになります。

```python
var xs = [1, 2, 3]
let v = xs.pop()
print(v)    # 3
print(xs)   # [1, 2]
```

### reverse

要素を逆順にした新しいリストを返します。元のリストは変更されません。文字列に対しても使用できます。

```python
let xs = [1, 2, 3]
print(reverse(xs))   # [3, 2, 1]
print(xs)            # [1, 2, 3]（変更なし）
```

### slice

`start`（含む）から `end`（含まない）までの新しい部分リストを返します。インデックスは有効範囲にクランプされます。

```python
let xs = [1, 2, 3, 4, 5]
print(slice(xs, 1, 3))     # [2, 3]
print(slice(xs, 0, 100))   # [1, 2, 3, 4, 5]（クランプされる）
```

### filter

述語を満たす要素だけを含む新しいリストを返します。元のリストは変更されません。

```python
let xs = [1, 2, 3, 4, 5]
let ys = xs.filter(fn(x: int): x > 3)
print(ys)   # [4, 5]
```

### map

各要素を関数で変換した新しいリストを返します。出力の要素型は入力と異なっても構いません。元のリストは変更されません。

```python
let xs = [1, 2, 3]
let ys = xs.map(fn(x: int): x * 2)
print(ys)   # [2, 4, 6]
```

### sort

ソート済みの新しいリストを返します。デフォルトは昇順です。カスタム比較関数を指定できます。元のリストは変更されません。

```python
let xs = [3, 1, 2]
print(xs.sort())   # [1, 2, 3]

# 降順ソート
let desc = xs.sort(fn(a: int, b: int): a > b)
print(desc)   # [3, 2, 1]
```

### filter, map, sort のチェーン

これらの関数は新しいリストを返すため、UFCS で連鎖できます。

```python
let xs = [5, 3, 1, 4, 2]
let result = xs.filter(fn(x: int): x > 1).map(fn(x: int): x * 10).sort()
print(result)   # [20, 30, 40, 50]
```

### reduce

アキュムレータ関数を使ってリストを単一の値に畳み込みます。最初の要素を初期値として使用します。

```python
let xs = [1, 2, 3, 4, 5]
let total = reduce(xs, fn(a: int, b: int): a + b)
print(total)   # 15
```

### fold

明示的な初期値とアキュムレータ関数を使ってリストを単一の値に畳み込みます。

```python
let xs = [1, 2, 3, 4, 5]
let total = fold(xs, 0, fn(a: int, b: int): a + b)
print(total)   # 15
```

### any

述語を満たす要素が1つ以上あれば `true` を返します。

```python
let xs = [1, 2, 3, 4, 5]
print(any(xs, fn(x: int): x > 4))   # true
print(any(xs, fn(x: int): x > 9))   # false
```

### all

すべての要素が述語を満たす場合に `true` を返します。

```python
let xs = [2, 4, 6]
print(all(xs, fn(x: int): x > 0))   # true
print(all(xs, fn(x: int): x > 3))   # false
```

### sum

全要素の合計を返します。

```python
let xs = [1, 2, 3, 4, 5]
print(sum(xs))   # 15
```

### min

最小の要素を返します。

```python
let xs = [3, 1, 4, 1, 5]
print(min(xs))   # 1
```

### max

最大の要素を返します。

```python
let xs = [3, 1, 4, 1, 5]
print(max(xs))   # 5
```

### first

最初の要素を返します。空のリストに対して呼び出すとランタイムエラーになります。

```python
let xs = [10, 20, 30]
print(first(xs))   # 10
```

### last

最後の要素を返します。空のリストに対して呼び出すとランタイムエラーになります。

```python
let xs = [10, 20, 30]
print(last(xs))   # 30
```

### is_empty

リストが空であれば `true` を返します。

```python
let xs = [1, 2, 3]
print(is_empty(xs))   # false
```

### enumerate

`(インデックス, 要素)` のタプルのリストを返します。

```python
let xs = [10, 20, 30]
let pairs = enumerate(xs)
# pairs = [(0, 10), (1, 20), (2, 30)]

# for ループでのタプル分解
for i, x in enumerate(xs):
    print(f"{i}: {x}")    # 0: 10, 1: 20, 2: 30
```

### zip

2つのリストを `(要素1, 要素2)` のタプルのリストに結合します。結果の長さは短い方のリストと同じになります。

```python
let xs = [1, 2, 3]
let ys = ["a", "b", "c"]
let pairs = zip(xs, ys)
# pairs = [(1, "a"), (2, "b"), (3, "c")]

# for ループでのタプル分解
for a, b in zip(xs, ys):
    print(f"{a}: {b}")    # 1: a, 2: b, 3: c
```

### insert

指定したインデックスに要素を挿入します。そのインデックス以降の要素は右にシフトされます。

```python
var xs = [1, 2, 3]
insert(xs, 1, 99)
print(xs)   # [1, 99, 2, 3]
```

### remove_at

指定したインデックスの要素を削除して返します。そのインデックス以降の要素は左にシフトされます。

```python
var xs = [1, 2, 3, 4]
let v = remove_at(xs, 1)
print(v)    # 2
print(xs)   # [1, 3, 4]
```

### remove

リストから指定した値の最初の出現を削除します。値が見つからない場合は何もしません。破壊的操作です。

```python
var xs = [1, 2, 3, 2, 4]
remove(xs, 2)
print(xs)   # [1, 3, 2, 4]
```

### distinct

重複を排除した新しいリストを返します。元の順序は保持されます（最初の出現を残します）。元のリストは変更されません。

```python
let xs = [1, 2, 3, 2, 1, 4]
print(distinct(xs))   # [1, 2, 3, 4]
print(xs)             # [1, 2, 3, 2, 1, 4]（変更なし）
```

### flatten

ネストされたリスト（リストのリスト）を1段階フラット化します。新しいリストを返します。元のリストは変更されません。

```python
let xs = [[1, 2], [3, 4]]
print(flatten(xs))   # [1, 2, 3, 4]
print(xs)            # [[1, 2], [3, 4]]（変更なし）
```

### 制約とエラー

| 制約 | 詳細 |
|------|------|
| 全要素は同一型 | 異なる型が混在するとコンパイルエラー |
| 空リスト `[]` | 型推論できないためコンパイルエラー |
| 範囲外アクセス | ランタイムエラー（exit(1)） |

---

## マップ

### 概要

キーと値の対応表。ヒープ上に確保されます。

### 構文

```python
let m = {"a": 1, "b": 2}
let m: Map<str, int> = {"a": 1, "b": 2}
```

### キーアクセス

```python
let m = {"a": 1, "b": 2}
print(m["a"])   # 1
```

### 挿入・更新

```python
let m = {"a": 1}
m["b"] = 2     # 新規追加
m["a"] = 99    # 更新
```

### len

```python
let m = {"a": 1, "b": 2, "c": 3}
print(len(m))   # 3
```

### print

```python
let m = {"a": 1, "b": 2}
print(m)   # {a: 1, b: 2}
```

### has_key

```python
let m = {"a": 1, "b": 2}
print(m.has_key("a"))   # true
print(m.has_key("z"))   # false
```

### keys

マップの全キーのリストを返します。

```python
let m = {"a": 1, "b": 2, "c": 3}
print(keys(m))   # ["a", "b", "c"]
```

### values

マップの全値のリストを返します。

```python
let m = {"a": 1, "b": 2, "c": 3}
print(values(m))   # [1, 2, 3]
```

### items

マップの全エントリの `(キー, 値)` タプルのリストを返します。

```python
let m = {"a": 1, "b": 2}
let pairs = items(m)
# pairs = [("a", 1), ("b", 2)]
```

### remove（マップ）

指定したキーのエントリをマップから削除します。キーが存在しない場合は何もしません。

```python
let m = {"a": 1, "b": 2}
remove(m, "a")
print(m)   # {b: 2}
```

### get

指定したキーの値を返します。キーが存在しない場合はデフォルト値を返します。

```python
let m = {"a": 1, "b": 2}
print(get(m, "a", 0))   # 1
print(get(m, "z", 0))   # 0
```

### merge

2つのマップを結合した新しいマップを返します。キーが重複する場合は第2マップの値が優先されます。元のマップは変更されません。

```python
let m1 = {"a": 1, "b": 2}
let m2 = {"b": 99, "c": 3}
let m3 = merge(m1, m2)
print(m3["a"])   # 1
print(m3["b"])   # 99
print(m3["c"])   # 3
```

### 制約とエラー

| 制約 | 詳細 |
|------|------|
| 全キーは同一型 | 異なる型のキーが混在するとコンパイルエラー |
| 全値は同一型 | 異なる型の値が混在するとコンパイルエラー |
| 空マップ | 型注釈が必要（`let m: Map<str, int> = {"a": 1}` など） |
| 存在しないキーアクセス | ランタイムエラー（exit(1)） |
| キー検索 | 線形スキャン |
| 容量超過時 | 自動で2倍に拡張 |

---

## セット

### 概要

同一型の要素を重複なしで保持するコレクション。ヒープ上に確保されます。

### 構文

```python
let s = {1, 2, 3}
let s: Set<int> = {1, 2, 3}
```

### 対応する要素型

`int`, `float`, `bool`, `str`

### in 演算子（所属チェック）

```python
let s = {1, 2, 3}
print(2 in s)   # true
print(5 in s)   # false
```

### len

```python
let s = {1, 2, 3}
print(len(s))   # 3
```

### print

```python
let s = {1, 2, 3}
print(s)   # {1, 2, 3}
```

### add（要素追加）

重複する要素を追加した場合は無視されます。

```python
let s = {1, 2, 3}
s.add(4)         # 追加
s.add(1)         # 既に存在するため無視
print(len(s))    # 4
```

### remove（要素削除）

```python
let s = {1, 2, 3}
s.remove(2)
print(2 in s)   # false
```

### for 走査

```python
let s = {10, 20, 30}
for x in s:
    print(x)
```

### 空セット

空セットは型注釈が必要です。

```python
let s: Set<int> = {}
```

### 関数引数

```python
fn has_value(s: Set<int>, v: int) -> bool:
    return v in s
```

### union

両方のセットの全要素を含む新しいセットを返します。

```python
let a = {1, 2, 3}
let b = {3, 4, 5}
print(union(a, b))   # {1, 2, 3, 4, 5}
```

### intersection

両方のセットに存在する要素のみを含む新しいセットを返します。

```python
let a = {1, 2, 3}
let b = {2, 3, 4}
print(intersection(a, b))   # {2, 3}
```

### difference

最初のセットにはあるが、2番目のセットにはない要素を含む新しいセットを返します。

```python
let a = {1, 2, 3}
let b = {2, 3, 4}
print(difference(a, b))   # {1}
```

### symmetric_difference

いずれかのセットにあるが、両方にはない要素を含む新しいセットを返します。

```python
let a = {1, 2, 3}
let b = {2, 3, 4}
print(symmetric_difference(a, b))   # {1, 4}
```

### is_subset

最初のセットの全要素が2番目のセットに含まれている場合に `true` を返します。

```python
let a = {1, 2}
let b = {1, 2, 3}
print(is_subset(a, b))   # true
print(is_subset(b, a))   # false
```

### is_superset

最初のセットが2番目のセットの全要素を含んでいる場合に `true` を返します。

```python
let a = {1, 2, 3}
let b = {1, 2}
print(is_superset(a, b))   # true
print(is_superset(b, a))   # false
```

### 制約とエラー

| 制約 | 詳細 |
|------|------|
| 全要素は同一型 | 異なる型が混在するとコンパイルエラー |
| 空セット `{}` | 型注釈が必要 |
| 要素検索 | 線形スキャン |
| 容量超過時 | 自動で2倍に拡張 |
