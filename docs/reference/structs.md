# 構造体リファレンス

## 概要

構造体はスタック上の値型です。`type` キーワードで定義します。

---

## 定義構文

```python
type 型名:
    フィールド名: 型
    フィールド名: 型
```

### 例

```python
type Point:
    x: int
    y: int

type Rectangle:
    width: float
    height: float
```

---

## コンストラクタ

フィールド定義順に引数を渡します。名前付き引数はサポートされていません。

```python
let p = Point(10, 20)
let r = Rectangle(3.0, 4.5)
```

---

## フィールドアクセス

ドット記法でフィールドを読み取ります。

```python
let p = Point(10, 20)
print(p.x)   # 10
print(p.y)   # 20
```

---

## フィールド代入

| 変数宣言 | フィールド代入 |
|---------|--------------|
| `let`   | 可能         |
| `const` | コンパイルエラー |

```python
let p = Point(10, 20)
p.x = 100    # OK: let変数

const q = Point(10, 20)
q.x = 100    # エラー: const変数のフィールドは変更不可
```

---

## 関数引数・戻り値としての使用

```python
fn distance(p: Point) -> float:
    return (p.x * p.x + p.y * p.y) as float

fn make_point(x: int, y: int) -> Point:
    return Point(x, y)
```

---

## ネスト構造体

構造体を別の構造体のフィールドとして使用できます。

```python
type Point:
    x: int
    y: int

type Circle:
    center: Point
    radius: float

let c = Circle(Point(0, 0), 1.0)
print(c.center.x)   # 0
```

---

## 制約とエラー

| 制約 | 詳細 |
|------|------|
| 同一フィールド名の重複 | コンパイルエラー |
| `const` 変数のフィールド代入 | コンパイルエラー |
| `print` に構造体を直接渡す | コンパイルエラー（print非対応） |

```python
# エラー例: 同一フィールド名の重複
type Bad:
    x: int
    x: int   # エラー

# エラー例: printに構造体を渡す
let p = Point(1, 2)
print(p)   # エラー
```
