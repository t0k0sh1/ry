[English](../../tutorial/06-records.md) | [日本語](06-records.md) | [繁體中文](../../zh/tutorial/06-records.md)

# Record と列挙型

[<- 前: 関数](05-functions.md) | [次: コレクションとイテレータ ->](07-collections.md)

---

## Record の定義

`record` キーワードで record を定義します。各フィールドは `name: type` の形式で記述します。

```python
record Point:
    x: int
    y: int
```

Record はスタック上の値型です。

---

## インスタンスの生成

record 名を関数のように呼び出してインスタンスを生成します。引数はフィールドの定義順に指定します。

```python
p = Point(10, 20)
```

---

## フィールドアクセス（ドット記法）

フィールドにはドット記法でアクセスします。

```python
p = Point(10, 20)
print(p.x)   # 10
print(p.y)   # 20
```

Record は直接 print できます:

```python
p = Point(10, 20)
print(p)   # Point(x: 10, y: 20)
```

---

## フィールドへの代入

`@const` なしで宣言した可変変数のフィールドは再代入できます。

```python
p = Point(10, 20)
p.x = 100
print(p.x)   # 100
```

> **注意**: `@const` で宣言した変数のフィールドへの代入はコンパイルエラーになります。

---

## 関数の引数としての Record

Record を関数の引数として渡せます。

```python
record Point:
    x: int
    y: int

function distance_x(a: Point, b: Point) -> int:
    return a.x - b.x

p1 = Point(10, 3)
p2 = Point(4, 7)
print(distance_x(p1, p2))   # 6
```

---

## ネストした Record

Record のフィールドに別の record を使えます。

```python
record Point:
    x: int
    y: int

record Line:
    start: Point
    end: Point

line = Line(Point(0, 0), Point(10, 5))
print(line.start.x)   # 0
print(line.end.x)     # 10
```

ドット記法をチェーンすることでネストしたフィールドにアクセスできます。

---

## 列挙型（enum）

`enum` キーワードで列挙型を定義できます。各バリアントは名前付きの定数として扱われます。

### 定義

```python
enum Color:
    Red
    Green
    Blue
```

### 使い方

バリアントには `::` でアクセスします。

```python
c = Color::Red
print(c)   # Red
```

### 比較

`==` や `!=` でバリアントを比較できます。

```python
case:
    c == Color::Red:
        print("red!")
    c == Color::Green:
        print("green!")
    _:
        print("blue!")
```

### 関数引数

関数の引数型として enum 名を使えます。

```python
function describe(c: Color) -> str:
    if c == Color::Red:
        return "warm"
    return "cool"
```

---

## 関連データ付き列挙型（ADT）

enum のバリアントは関連する値を持つことができます。これにより、単一の enum でさまざまな形状のデータを表現できます -- **代数的データ型（ADT）** として知られるパターンです。

```python
enum Shape:
    Circle(radius: float)
    Rectangle(width: float, height: float)
    Point
```

名前付きフィールドはドキュメント用です -- 定義を自己記述的にします。名前なし構文（`Circle(float)`）も有効です。

### ADT バリアントの構築

フィールドが名前付きかどうかに関わらず、構築は常に位置指定です。

```python
c = Shape::Circle(3.14)
r = Shape::Rectangle(4.0, 5.0)
p = Shape::Point
```

### ADT バリアントのマッチング

`case` を使って ADT バリアントから関連データを取り出します。バインディングにはフィールド名ではなく任意の変数名を使います。これは[制御構文](04-control-flow.md)で学んだパターンマッチングと直接つながります。

```python
function describe(s: Shape) -> str:
    case s:
        Shape::Circle(r):
            return f"circle with radius {r}"
        Shape::Rectangle(w, h):
            return f"rectangle {w}x{h}"
        Shape::Point:
            return "point"

print(describe(Shape::Circle(3.14)))         # circle with radius 3.14
print(describe(Shape::Rectangle(4.0, 5.0)))  # rectangle 4.0x5.0
```

> **なぜ ADT なのか?** 「複数の形状のうちの1つ」であるデータを型安全にモデル化できます。コンパイラがパターンマッチング時にすべてのバリアントを処理しているか保証し、漏れをコンパイル時に検出します。

---

## ジェネリック列挙型

enum は型パラメータを取ることができ、異なるペイロード型にわたって再利用可能になります。

```python
enum MyOption<T>:
    MySome(T)
    MyNone
```

### 使い方

```python
a = MyOption<int>::MySome(42)
b: MyOption<int> = MyOption<int>::MyNone

case a:
    MyOption::MySome(v):
        print(v)      # 42
    MyOption::MyNone:
        print("none")
```

> **注意**: Ry の組み込み `Option<T>` と `Result<T, E>` 型はこれとまったく同じように動作します。[エラーハンドリング](08-error-handling.md)で学びます。

---

## 演算子オーバーロード

`function operator` 構文でカスタム型に演算子を定義できます。これにより record が `+`、`==` などの演算子で自然に動作するようになります。

### 二項演算子

2つのパラメータを取ります。

```python
record Vec2:
    x: int
    y: int

function operator+(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x + b.x, a.y + b.y)

function operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

v1 = Vec2(1, 2)
v2 = Vec2(3, 4)
v3 = v1 + v2
print(v3.x)       # 4
print(v1 == v2)   # false
```

### 単項演算子

1つのパラメータを取ります。

```python
function operator-(v: Vec2) -> Vec2:
    return Vec2(-v.x, -v.y)
```

### サポートされている演算子

| カテゴリ | 演算子 |
|---------|--------|
| 算術 | `+`, `-`, `*`, `/`, `%`, `**`, `//` |
| 比較 | `==`, `!=`, `<`, `<=`, `>`, `>=` |
| ビット | `&`, `\|`, `^`, `~`, `<<`, `>>`, `>>>` |
| 論理 | `and`, `or`, `not` |
| 所属 | `in` |
| インデックスアクセス | `[]` |
| インデックス代入 | `[]=` |
| 関数呼び出し | `()` |
| 型キャスト | `as` |
| 複合代入 | `+=`, `-=`, `*=`, `/=`, `%=`, `//=`, `**=`, `&=`, `\|=`, `^=`, `<<=`, `>>=` |

> **なぜ演算子オーバーロードなのか?** ドメイン型に自然な構文を与えます。`Vec2 + Vec2` は `vec2_add(a, b)` より読みやすく、`==` があれば `case` や比較でシームレスに動作します。

---

## 演習

1. **ADT**: `Dog(name: str)`、`Cat(name: str, indoor: bool)`、`Fish` のバリアントを持つ `Animal` enum を定義してください。`case` を使って各バリアントの説明を返す `describe(a: Animal) -> str` 関数を書いてください。

2. **演算子オーバーロード**: `amount: int` と `currency: str` を持つ `Money` record を定義してください。同じ通貨の2つの `Money` 値を足すと、合計金額の新しい `Money` を返すように `+` をオーバーロードしてください。

---

[<- 前: 関数](05-functions.md) | [次: コレクションとイテレータ ->](07-collections.md)
