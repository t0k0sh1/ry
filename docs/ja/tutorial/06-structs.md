[English](../../tutorial/06-structs.md) | [日本語](06-structs.md) | [繁體中文](../../zh/tutorial/06-structs.md)

# 構造体と列挙型

[← 前: 関数](05-functions.md) | [次: コレクション →](07-collections.md)

---

## type による構造体定義

`type` キーワードで構造体を定義します。各フィールドは `name: type` の形式で記述します。

```python
type Point:
    x: int
    y: int
```

構造体はスタック上の値型です。

---

## コンストラクタの使い方

構造体名を関数のように呼び出してインスタンスを生成します。引数はフィールドの定義順に指定します。

```python
let p = Point(10, 20)
```

---

## フィールドアクセス（ドット記法）

フィールドにはドット記法でアクセスします。

```python
let p = Point(10, 20)
print(p.x)   # 10
print(p.y)   # 20
```

> **注意**: `print` に構造体を直接渡すとエラーになります。フィールドを個別に渡してください。

---

## フィールドへの代入

`var` で宣言した変数のフィールドは再代入できます。

```python
var p = Point(10, 20)
p.x = 100
print(p.x)   # 100
```

> **注意**: `let` で宣言した変数のフィールドへの代入はコンパイルエラーになります。

---

## 関数の引数としての構造体

構造体を関数の引数として渡せます。

```python
type Point:
    x: int
    y: int

fn distance_x(a: Point, b: Point) -> int:
    return a.x - b.x

let p1 = Point(10, 3)
let p2 = Point(4, 7)
print(distance_x(p1, p2))   # 6
```

---

## ネスト構造体

構造体のフィールドに別の構造体を使えます。

```python
type Point:
    x: int
    y: int

type Line:
    start: Point
    end: Point

let line = Line(Point(0, 0), Point(10, 5))
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
let c = Color::Red
print(c)   # Red
```

### 比較

`==` や `!=` でバリアントを比較できます。

```python
if c == Color::Red:
    print("red!")
elif c == Color::Green:
    print("green!")
else:
    print("blue!")
```

### 関数引数

関数の引数型として enum 名を使えます。

```python
fn describe(c: Color) -> str:
    if c == Color::Red:
        return "warm"
    return "cool"
```

---

[← 前: 関数](05-functions.md) | [次: コレクション →](07-collections.md)
