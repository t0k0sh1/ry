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
let ys = xs.filter((x: int) -> x > 3)
print(ys)   # [4, 5]
```

### map

各要素を関数で変換した新しいリストを返します。出力の要素型は入力と異なっても構いません。元のリストは変更されません。

```python
let xs = [1, 2, 3]
let ys = xs.map((x: int) -> x * 2)
print(ys)   # [2, 4, 6]
```

### sort

ソート済みの新しいリストを返します。デフォルトは昇順です。カスタム比較関数を指定できます。元のリストは変更されません。

```python
let xs = [3, 1, 2]
print(xs.sort())   # [1, 2, 3]

# 降順ソート
let desc = xs.sort((a: int, b: int) -> a > b)
print(desc)   # [3, 2, 1]
```

### filter, map, sort のチェーン

これらの関数は新しいリストを返すため、UFCS で連鎖できます。

```python
let xs = [5, 3, 1, 4, 2]
let result = xs.filter((x: int) -> x > 1).map((x: int) -> x * 10).sort()
print(result)   # [20, 30, 40, 50]
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

### 制約とエラー

| 制約 | 詳細 |
|------|------|
| 全要素は同一型 | 異なる型が混在するとコンパイルエラー |
| 空セット `{}` | 型注釈が必要 |
| 要素検索 | 線形スキャン |
| 容量超過時 | 自動で2倍に拡張 |
