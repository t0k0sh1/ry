[English](../../tutorial/08-advanced.md) | [日本語](08-advanced.md) | [繁體中文](../../zh/tutorial/08-advanced.md)

# 高度な機能

[← 前: コレクション](07-collections.md) | [次: パッケージ →](09-modules.md)

---

## ラムダ関数

ラムダ関数は、関数を式として記述する構文です。`fn(引数) => 式` の形で書きます。戻り値型は自動推論されます。

### 単一式ラムダ

```python
double = fn(x: int) => x * 2
print(double(5))  # 10

add = fn(a: int, b: int) => a + b
print(add(3, 4))  # 7
```

### 引数なしラムダ

```python
answer = fn() => 42
print(answer())  # 42
```

### 複数行ラムダ

`:` の後に改行してインデントすることで、複数の文を書けます。

```python
abs = fn(x: int):
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
offset = 10
add_offset = fn(x: int) => x + offset
print(add_offset(5))  # 15
```

---

## 高階関数

関数を引数として受け取る関数を定義できます。関数型は `fn(引数型) -> 戻り値型` と書きます。

```python
fn apply(f: fn(int) -> int, x: int) -> int:
    return f(x)

double = fn(x: int) => x * 2
print(apply(double, 3))                # 6
print(apply(fn(n: int) => n + 1, 10))    # 11
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
sq = square
print(sq(5))  # 25
```

---

## UFCS（Uniform Function Call Syntax）

UFCS を使うと、`f(a, b)` の呼び出しを `a.f(b)` と書けます。メソッドチェーンのような記述が可能になります。

```python
fn add(a: int, b: int) -> int:
    return a + b

x = 1
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
record Vec2:
    x: int
    y: int

fn operator+(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x + b.x, a.y + b.y)

fn operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

v1 = Vec2(1, 2)
v2 = Vec2(3, 4)
v3 = v1 + v2
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
x: Option<int> = Some(42)
print(x)   # Some(42)

y: Option<int> = None
print(y)   # None
```

### 値の取り出し

`match` を使って内部の値を安全に取り出し、`None` の場合も処理できます。

```python
match x:
    case Some(v):
        print(v)    # 42
    case None:
        print("nothing")
```

---

## F-String（文字列補間）

`f"..."` を使って、文字列内に式を直接埋め込むことができます。式は `{}` 内に記述します。

```python
name = "Alice"
print(f"Hello {name}")   # Hello Alice

x = 3
y = 4
print(f"{x} + {y} = {x + y}")   # 3 + 4 = 7
```

リテラルの波括弧を出力するには `{{` と `}}` を使います。

```python
print(f"{{escaped}}")   # {escaped}
```

---

## 型キャスト（`as`）

`as` を使って型を明示的に変換できます。

```python
x = 42 as float     # 42.0
y = 3.14 as int      # 3（切り捨て）
s = 42 as str         # "42"
b = true as int       # 1
```

---

## 関連データを持つ enum（ADT）

enum バリアントに関連する値を持たせることができます。これにより、1 つの enum でさまざまな形のデータファミリーを表現できます。

```python
enum Shape:
    Circle(float)
    Rectangle(float, float)
    Point
```

### ADT バリアントの構築

```python
c = Shape::Circle(3.14)
r = Shape::Rectangle(4.0, 5.0)
p = Shape::Point
```

### ADT バリアントのマッチング

`case` にバインディングパターンを使って関連データを取り出します。

```python
fn describe(s: Shape) -> str:
    match s:
        case Shape::Circle(r):
            return f"circle with radius {r}"
        case Shape::Rectangle(w, h):
            return f"rectangle {w}x{h}"
        case Shape::Point:
            return "point"

print(describe(Shape::Circle(3.14)))         # circle with radius 3.14
print(describe(Shape::Rectangle(4.0, 5.0)))  # rectangle 4.0x5.0
```

---

## ジェネリック enum

enum は型パラメータを取ることができ、異なるペイロード型で再利用可能になります。

```python
enum MyOption<T>:
    MySome(T)
    MyNone
```

### 使用法

```python
a = MyOption<int>::MySome(42)
b: MyOption<int> = MyOption<int>::MyNone

match a:
    case MyOption::MySome(v):
        print(v)      # 42
    case MyOption::MyNone:
        print("none")
```

---

## Result 型

`Result<T, E>` は失敗する可能性のある関数に使用します。成功時は `Ok(value)`、失敗時は `Err(error)` を返します。

```python
fn divide(a: int, b: int) -> Result<int, str>:
    if b == 0:
        return Err("division by zero")
    return Ok(a // b)
```

`match` を使って結果を処理します。

```python
r = divide(10, 0)
match r:
    case Ok(v):
        print(v)
    case Err(e):
        print(e)   # division by zero
```

---

[← 前: コレクション](07-collections.md) | [次: パッケージ →](09-modules.md)
