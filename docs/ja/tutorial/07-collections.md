[English](../../tutorial/07-collections.md) | [日本語](07-collections.md) | [繁體中文](../../zh/tutorial/07-collections.md)

# コレクションとイテレータ

[<- 前: Record と列挙型](06-records.md) | [次: エラーハンドリング ->](08-error-handling.md)

Ry には4種類のコレクション型があります: **タプル**、**リスト**、**マップ**、**セット**。

---

## タプル

タプルは複数の値を一つにまとめた不変のデータ構造です。異なる型の要素を保持できます。

### 生成

```python
t = (1, 3.14)
```

### 型アノテーション

```python
t: (int, float) = (1, 3.14)
```

### 要素アクセス

`.0`, `.1`, ... のようにインデックスでアクセスします。

```python
t = (1, 3.14)
print(t.0)   # 1
print(t.1)   # 3.14
```

### 関数の戻り値

複数の値を返したいときにタプルが便利です。

```python
function swap(a: int, b: int) -> (int, int):
    return (b, a)

result = swap(1, 2)
print(result.0)  # 2
print(result.1)  # 1
```

### 制限事項

- 範囲外のインデックス（例: 要素数2のタプルに `.2` でアクセス）はコンパイルエラーになります。
- `print` にタプルを直接渡すとエラーになります。各要素を個別に渡してください。

---

## リスト

リストは同じ型の要素を並べた可変長のデータ構造です。

### 生成

```python
xs = [1, 2, 3]
```

### 型アノテーション

```python
xs: List<int> = [1, 2, 3]
```

### インデックスアクセス

```python
print(xs[0])   # 1

i = 1
print(xs[i])   # 2
```

### インデックス代入

```python
xs[0] = 99
```

### length

```python
print(length(xs))   # 3
```

### print

```python
print(xs)   # [1, 2, 3]
```

### for 走査

```python
for x in xs:
    print(x)
```

### 関数引数

```python
function first(xs: List<int>) -> int:
    return xs[0]
```

### filter, map, sort

リストは `filter`、`map`、`sort` 操作をサポートしています。これらは元のリストを変更せず、新しいリストを返します。

```python
xs = [1, 2, 3, 4, 5]

# filter: 条件に一致する要素だけを残す
evens = filter(xs, (x: int) => x > 3)
print(evens)   # [4, 5]

# map: 各要素を変換する
doubled = map(xs, (x: int) => x * 2)
print(doubled)   # [2, 4, 6, 8, 10]

# sort: 昇順ソート（デフォルト）
sorted = sort([3, 1, 2])
print(sorted)   # [1, 2, 3]

# UFCS（統一関数呼び出し構文）によるチェーン
# x.f(args) は f(x, args) と等価
result = xs.filter((x: int) => x > 1).map((x: int) => x * 10).sort()
print(result)   # [20, 30, 40, 50]
```

### reduce, fold

`reduce` はリストを最初の要素から1つの値に集約します。`fold` は明示的な初期値を指定できます。

```python
xs = [1, 2, 3, 4, 5]

# reduce: 最初の要素から開始
total = reduce(xs, (a: int, b: int) => a + b)
print(total)   # 15

# fold: 明示的な初期値を指定
total2 = fold(xs, 0, (a: int, b: int) => a + b)
print(total2)   # 15
```

### any, all

`any` は述語を満たす要素が1つ以上あれば `true` を返します。`all` はすべての要素が満たす場合に `true` を返します。

```python
xs = [1, 2, 3, 4, 5]

print(any(xs, (x: int) => x > 4))   # true
print(any(xs, (x: int) => x > 9))   # false

print(all(xs, (x: int) => x > 0))   # true
print(all(xs, (x: int) => x > 3))   # false
```

### sum, min, max

```python
xs = [3, 1, 4, 1, 5]
print(sum(xs))   # 14
print(min(xs))   # 1
print(max(xs))   # 5
```

### first, last, is_empty

```python
xs = [10, 20, 30]
print(first(xs))      # Some(10)
print(last(xs))       # Some(30)
print(is_empty(xs))   # false
```

### enumerate, zip

`enumerate` は各要素にインデックスを付けます。`zip` は2つのリストを要素ごとに結合します。

```python
xs = [10, 20, 30]
indexed = enumerate(xs)
# [(0, 10), (1, 20), (2, 30)]
for p in indexed:
    print(p.0)
    print(p.1)

ys = ["a", "b", "c"]
zipped = zip(xs, ys)
# [(10, "a"), (20, "b"), (30, "c")]
```

### 制限事項

- 全要素が同じ型である必要があります。異なる型を混在させることはできません。
- 空リスト `[]` はエラーになります。
- 範囲外アクセスはランタイムエラー（`exit(1)`）になります。
- 要素の型として `int`, `float`, `bool`, `str` をサポートしています。

---

## マップ

マップはキーと値のペアを管理する連想配列です。

### 生成

```python
m = {"a": 1, "b": 2}
```

### 型アノテーション

```python
m: Map<str, int> = {"a": 1, "b": 2}
```

### キーアクセス

```python
print(m["a"])   # 1
```

### 挿入 / 更新

新規キーへの代入で挿入、既存キーへの代入で更新します。

```python
m["c"] = 3    # 新規追加
m["a"] = 99   # 更新
```

### length

```python
print(length(m))   # 3
```

### print

```python
print(m)   # {a: 99, b: 2, c: 3}
```

### has_key

キーが存在するか確認します。

```python
print(has_key(m, "a"))   # true
```

### keys, values

`keys` は全キーのリストを返します。`values` は全値のリストを返します。

```python
m = {"a": 1, "b": 2, "c": 3}
print(keys(m))     # ["a", "b", "c"]
print(values(m))   # [1, 2, 3]
```

### 関数引数

```python
function get_val(m: Map<str, int>, k: str) -> int:
    return m[k]
```

### 制限事項

- 全キーが同じ型、全値が同じ型である必要があります。
- 空マップは型注釈が必要です。
- 存在しないキーへのアクセスはランタイムエラー（`exit(1)`）になります。

---

## セット

セットは同じ型の要素を重複なしで保持するコレクションです。

### 生成

```python
s = {1, 2, 3}
```

### 型アノテーション

```python
s: Set<int> = {1, 2, 3}
```

### in 演算子

要素がセットに含まれるかを `in` 演算子で確認できます。

```python
print(2 in s)   # true
print(5 in s)   # false
```

### add / remove

```python
add(s, 4)       # 要素追加
remove(s, 1)    # 要素削除
add(s, 2)       # 既に存在するため無視
```

### length / print

```python
print(length(s))  # 3
print(s)       # {2, 3, 4}
```

### for 走査

```python
for x in s:
    print(x)
```

### 空セット

空セットは型注釈が必要です。

```python
empty: Set<int> = {}
```

### 制限事項

- 全要素が同じ型である必要があります。
- 要素の型として `int`, `float`, `bool`, `str` をサポートしています。

---

## イテレータ

イテレータはコレクションを**遅延的に**処理する方法を提供します。各ステップで中間リストを作成する代わりに、パイプラインを通じて要素を1つずつ処理します。

> **なぜ遅延イテレータなのか?** リストに対して直接 `filter` と `map` をチェーンすると、各ステップで新しい中間リストが生成されます。イテレータを使うと、要素はパイプライン全体を1つずつ通過します -- 中間的なメモリ割り当てがありません。大きなコレクションを処理する場合や、最初の数件の結果だけが必要な場合（`take` を使用）に重要です。

### 生成と消費

コレクションに対して `iter()` を呼んでイテレータを取得し、`to_list()` で結果をリストに実体化します:

```python
xs = [1, 2, 3]
ys = to_list(iter(xs))   # [1, 2, 3]
```

### 操作のチェーン

`filter`、`map`、`take` をチェーンしてパイプラインを構築できます。これは[関数](05-functions.md)で学んだ UFCS チェーンスタイルを使います:

```python
result = to_list(take(map(filter(iter([1, 2, 3, 4, 5]), (x: int) => x > 2), (x: int) => x * 2), 2))
print(result)   # [6, 8]

# UFCS チェーンスタイル（等価）:
result = [1, 2, 3, 4, 5]
    .iter()
    .filter((x: int) => x > 2)
    .map((x: int) => x * 2)
    .take(2)
    .to_list()
print(result)   # [6, 8]
```

より実践的な例 -- スコアのリストを処理:

```python
scores = [85, 42, 93, 67, 78, 55, 91]

# 合格スコア（>= 60）のうち上位3つを取得し、ボーナスとして2倍にする
top_bonus = to_list(take(map(filter(iter(scores), (s: int) => s >= 60), (s: int) => s * 2), 3))
print(top_bonus)   # [170, 186, 134]
```

### next() による手動イテレーション

`next()` は `Option` を返します -- 次の要素がある場合は `Some(value)`、イテレータが使い尽くされた場合は `None`。`Option` については[エラーハンドリング](08-error-handling.md)で詳しく学びます。

```python
it = iter([10, 20])
print(next(it))   # Some(10)
print(next(it))   # Some(20)
print(next(it))   # None
```

### for ループ

イテレータは `for` ループで直接使えます:

```python
for x in filter(iter([1, 2, 3]), (x: int) => x > 1):
    print(x)   # 2, 3
```

### マップとセットのイテレーション

マップはキーと値のタプルを生成します。セットは個々の要素を生成します:

```python
for k, v in iter({"a": 1, "b": 2}):
    print(f"{k} = {v}")

for x in iter({10, 20, 30}):
    print(x)
```

### よくあるミス

- **`to_list()` を忘れる**: イテレータパイプラインだけでは何も実行されません -- 遅延的です。`to_list()`、`for` ループ、または `next()` で消費する必要があります。
- **`to_list()` を早すぎる位置で呼ぶ**: `filter()` の前に `to_list()` を置くと、すべての要素を先に実体化してしまうため、遅延評価の目的が損なわれます。

---

## 演習

1. **イテレータパイプライン**: `xs = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]` が与えられたとき、イテレータパイプラインを使って偶数の合計を計算してください。（ヒント: `.filter()` の後に `.to_list()` と `sum()` を使います。）

2. **手動イテレーション**: `[100, 200, 300]` のイテレータを作成し、`match` で `next()` の `Some` と `None` を処理してください。

---

[<- 前: Record と列挙型](06-records.md) | [次: エラーハンドリング ->](08-error-handling.md)
