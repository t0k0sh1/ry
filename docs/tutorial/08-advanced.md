# 高度な機能

[← 前: コレクション](07-collections.md) | [次: モジュール →](09-modules.md)

---

## ラムダ関数

ラムダ関数（アロー関数）は、関数を式として記述する構文です。`(引数): 戻り値型 => 式` の形で書きます。

### 単一式ラムダ

```python
let double = (x: int): int => x * 2
print(double(5))  # 10

let add = (a: int, b: int): int => a + b
print(add(3, 4))  # 7
```

### 引数なしラムダ

```python
let answer = (): int => 42
print(answer())  # 42
```

### 複数行ラムダ

`=>` の後に改行してインデントすることで、複数の文を書けます。

```python
let abs = (x: int): int =>
    if x < 0:
        return -x
    return x

print(abs(-5))  # 5
print(abs(3))   # 3
```

---

## クロージャ

ラムダ関数は定義時のスコープにある変数をキャプチャできます。

```python
let offset = 10
let add_offset = (x: int): int => x + offset
print(add_offset(5))  # 15
```

---

## 高階関数

関数を引数として受け取る関数を定義できます。関数型は `fn(引数型) -> 戻り値型` と書きます。

```python
fn apply(f: fn(int) -> int, x: int) -> int:
    return f(x)

let double = (x: int): int => x * 2
print(apply(double, 3))                   # 6
print(apply((n: int): int => n + 1, 10))  # 11
```

---

## 関数を値として扱う

名前付き関数も変数に束縛したり、引数として渡したりできます。

```python
fn square(x: int) -> int:
    return x * x

fn apply(f: fn(int) -> int, x: int) -> int:
    return f(x)

# 名前付き関数を引数として渡す
print(apply(square, 4))  # 16

# 変数に束縛する
let sq = square
print(sq(5))  # 25
```

---

## UFCS（Uniform Function Call Syntax）

UFCS を使うと、`f(a, b)` の呼び出しを `a.f(b)` と書けます。メソッドチェーンのような記述が可能になります。

```python
fn add(a: int, b: int) -> int:
    return a + b

let x = 1
print(x.add(2))   # add(x, 2) → 3
```

### チェーン呼び出し

```python
fn double(n: int) -> int:
    return n * 2

print(x.add(2).double())   # double(add(x, 2)) → 6
```

---

## 演算子オーバーロード

`fn operator演算子` 構文でカスタム型に演算子を定義できます。

### 二項演算子

パラメータを2個取ります。

```python
type Vec2:
    x: int
    y: int

fn operator+(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x + b.x, a.y + b.y)

fn operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

let v1 = Vec2(1, 2)
let v2 = Vec2(3, 4)
let v3 = v1 + v2
print(v3.x)       # 4
print(v1 == v2)   # false
```

### 単項演算子

パラメータを1個取ります。

```python
fn operator-(v: Vec2) -> Vec2:
    return Vec2(-v.x, -v.y)
```

### 対応演算子一覧

| 種別 | 演算子 |
|------|--------|
| 算術 | `+`, `-`, `*`, `/`, `%`, `**`, `//` |
| 比較 | `==`, `!=`, `<`, `<=`, `>`, `>=` |
| ビット | `&`, `\|`, `^`, `~`, `<<`, `>>` |
| 論理 | `and`, `or`, `not` |

---

## Option 型

値が存在するかどうかを表す型です。`Some(値)` または `None` をとります。

```python
let x: Option<int> = Some(42)
print(x)   # Some(42)

let y: Option<int> = None
print(y)   # None
```

### unwrap

`unwrap` で内部の値を取り出します。`None` に対して `unwrap` を呼ぶとランタイムエラーになります。

```python
let v = unwrap(x)   # 42
# unwrap(y) → ランタイムエラー
```

---

[← 前: コレクション](07-collections.md) | [次: モジュール →](09-modules.md)
