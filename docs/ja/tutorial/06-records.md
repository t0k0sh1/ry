[English](../../tutorial/06-records.md) | [日本語](06-records.md) | [繁體中文](../../zh/tutorial/06-records.md)

# Record と列挙型

[← 前: 関数](05-functions.md) | [次: コレクション →](07-collections.md)

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

> **注意**: `print` に record を直接渡すとエラーになります。フィールドを個別に渡してください。

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

fn distance_x(a: Point, b: Point) -> int:
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
