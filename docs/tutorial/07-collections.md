# コレクション

[← 前: 構造体](06-structs.md) | [次: 高度な機能 →](08-advanced.md)

Ry には4種類のコレクション型があります: **タプル**、**リスト**、**マップ**、**セット**。

---

## タプル

タプルは複数の値を一つにまとめた不変のデータ構造です。異なる型の要素を保持できます。

### 生成

```python
let t = (1, 3.14)
```

### 型アノテーション

```python
let t: (int, float) = (1, 3.14)
```

### 要素アクセス

`.0`, `.1`, ... のようにインデックスでアクセスします。

```python
let t = (1, 3.14)
print(t.0)   # 1
print(t.1)   # 3.14
```

### 関数の戻り値

複数の値を返したいときにタプルが便利です。

```python
fn swap(a: int, b: int) -> (int, int):
    return (b, a)

let result = swap(1, 2)
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
let xs = [1, 2, 3]
```

### 型アノテーション

```python
let xs: list<int> = [1, 2, 3]
```

### インデックスアクセス

```python
print(xs[0])   # 1

let i = 1
print(xs[i])   # 2
```

### インデックス代入

```python
xs[0] = 99
```

### len

```python
print(len(xs))   # 3
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
fn first(xs: list<int>) -> int:
    return xs[0]
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
let m = {"a": 1, "b": 2}
```

### 型アノテーション

```python
let m: map<str, int> = {"a": 1, "b": 2}
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

### len

```python
print(len(m))   # 3
```

### print

```python
print(m)   # {a: 99, b: 2, c: 3}
```

### has_key

キーが存在するか確認します。

```python
print(m.has_key("a"))   # true
```

### 関数引数

```python
fn get_val(m: map<str, int>, k: str) -> int:
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
let s = {1, 2, 3}
```

### 型アノテーション

```python
let s: set<int> = {1, 2, 3}
```

### in 演算子

要素がセットに含まれるかを `in` 演算子で確認できます。

```python
print(2 in s)   # true
print(5 in s)   # false
```

### add / remove

```python
s.add(4)       # 要素追加
s.remove(1)    # 要素削除
s.add(2)       # 既に存在するため無視
```

### len / print

```python
print(len(s))  # 3
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
let empty: set<int> = {}
```

### 制限事項

- 全要素が同じ型である必要があります。
- 要素の型として `int`, `float`, `bool`, `str` をサポートしています。

---

[← 前: 構造体](06-structs.md) | [次: 高度な機能 →](08-advanced.md)
