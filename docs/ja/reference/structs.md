[English](../../reference/structs.md) | [日本語](structs.md) | [繁體中文](../../zh/reference/structs.md)

# 構造体リファレンス

## 概要

構造体はスタック上の値型です。`record` キーワードで定義します。構造体には `invariant` 節で契約による設計の不変条件を定義できます。[契約による設計](contracts.md) を参照。

> **命名規則**: 構造体名は PascalCase（例: `Point`、`Rectangle`）を使用する必要があります。フィールド名は snake_case を使用します。コンパイラがこれらの規則を強制します。

---

## 定義構文

```python
record TypeName:
    field_name: type
    field_name: type
```

### 例

```python
record Point:
    x: int
    y: int

record Rectangle:
    width: float
    height: float
```

---

## コンストラクタ

フィールド定義順に引数を渡します。名前付き引数はサポートされていません。

```python
p = Point(10, 20)
r = Rectangle(3.0, 4.5)
```

---

## フィールドアクセス

ドット記法でフィールドを読み取ります。

```python
p = Point(10, 20)
print(p.x)   # 10
print(p.y)   # 20
```

---

## フィールド代入

| 変数宣言 | フィールド代入 |
|---------|--------------|
| 可変（`@const` なし） | 可能         |
| `@const`   | コンパイルエラー |

```python
p = Point(10, 20)
p.x = 100    # OK: 可変変数

@const
q = Point(10, 20)
q.x = 100    # エラー: @const変数のフィールドは変更不可
```

---

## 関数引数・戻り値としての使用

```python
function distance(p: Point) -> float:
    return (p.x * p.x + p.y * p.y) as float

function make_point(x: int, y: int) -> Point:
    return Point(x, y)
```

---

## ネスト構造体

構造体を別の構造体のフィールドとして使用できます。

```python
record Point:
    x: int
    y: int

record Circle:
    center: Point
    radius: float

c = Circle(Point(0, 0), 1.0)
print(c.center.x)   # 0
```

---

## 比較 (`==` / `!=`)

レコード型は `==` と `!=` 演算子を自動的にサポートします。フィールドごとの比較（構造的等価性）が行われます。

```python
record Point:
    x: int
    y: int

p1 = Point(10, 20)
p2 = Point(10, 20)
p3 = Point(30, 40)

print(p1 == p2)  # true
print(p1 != p3)  # true
```

- すべてのフィールドが順番に比較されます。`==` ではすべてのフィールドが等しい必要があり、`!=` では少なくとも1つのフィールドが異なる必要があります。
- ネストされたレコードは再帰的に比較されます。
- ユーザー定義の `operator==` または `operator!=` がある場合、自動生成版より優先されます。

---

## レコードのサブタイプ化（継承）

レコードは `<` 構文を使った単一継承をサポートします。子レコードは親のすべてのフィールドを継承します。

### 構文

```python
record ChildName < ParentName:
    child_field: type
```

### 例

```python
record HttpError < Error:
    status: int
    url: str
```

### フィールドの継承

- 子レコードはレイアウトの先頭に親のすべてのフィールドを継承します。
- コンストラクタは親のフィールドを先に、次に子固有のフィールドを取ります。

```python
err = HttpError("not found", 404, 404, "/api")
print(err.message)  # "not found"（Error から継承）
print(err.status)   # 404（固有フィールド）
```

### サブタイプの型強制

子の値は親の型が期待される場所に渡すことができます。子は自動的にスライスされ、親のプレフィックスフィールドが抽出されます（値型スライシング）。

```python
function handle(e: Error) -> str:
    return e.message

err = HttpError("fail", 500, 500, "/api")
handle(err)  # OK — HttpError が Error に型強制される
```

### 深い継承

レコードは継承チェーンを形成できます。各レベルはすべての祖先フィールドを継承します。

```python
record DetailedHttpError < HttpError:
    detail: str

# コンストラクタ: Error フィールド + HttpError フィールド + 固有フィールド
derr = DetailedHttpError("fail", 500, 500, "/x", "server crash")
handle(derr)  # OK — Error（祖父母）に型強制される
```

### ルール

| ルール | 詳細 |
|------|------|
| 単一継承のみ | `record A < B:` — 親は1つのみ |
| 深い継承 | `record C < B:` where `record B < A:` — 許可 |
| 名前の衝突 | 子のフィールドが親のフィールドと同名 → コンパイルエラー |
| 自動 `==` / `to_str` | 継承したフィールドもすべて含む |
| 不変条件の継承 | 親の `invariant:` 節は子レコードの構築・変更時にもチェックされる |
| サブタイプの型強制 | 適用先: 関数引数、return、`Err()`、フィールド代入、`?` 演算子 |
| ジェネリックバウンド | `<T: RecordName>` で型パラメータをレコードのサブタイプに制約 |
| `@const` | 継承したフィールドも含むすべてのフィールドに適用 |

---

## 制約とエラー

| 制約 | 詳細 |
|------|------|
| 同一フィールド名の重複 | コンパイルエラー |
| `@const` 変数のフィールド代入 | コンパイルエラー |

```python
# エラー例: 同一フィールド名の重複
record Bad:
    x: int
    x: int   # エラー
```

---

## 列挙型（enum）

### 概要

列挙型は名前付き定数の集合です。デフォルトでは i64 整数（0, 1, 2, ...）の連番として表現されます。明示的な整数値を割り当てることもできます。

### 定義構文

```python
enum TypeName:
    VariantName
    VariantName
    ...
```

### 例

```python
enum Color:
    Red
    Green
    Blue
```

### バリアントアクセス

`::` 演算子でバリアントにアクセスします。

```python
c = Color::Red
print(c)   # Red
```

### 比較

enum 値は整数なので `==` / `!=` でそのまま比較できます。

```python
print(Color::Red == Color::Red)    # true
print(Color::Red != Color::Green)  # true
```

### if 文での使用

```python
c = Color::Green
case:
    c == Color::Red:
        print("red")
    c == Color::Green:
        print("green")
    _:
        print("blue")
```

### 関数引数

型名として enum 名を使用します。

```python
function is_red(c: Color) -> bool:
    return c == Color::Red

print(is_red(Color::Red))    # true
print(is_red(Color::Green))  # false
```

### print

`print()` でバリアント名が出力されます。

```python
c = Color::Blue
print(c)   # Blue
```

### 明示的な値の割り当て

simple enum のバリアントに明示的な整数値を割り当てることができます。HTTP ステータスコードやビットマスクパターンなどに有用です。

```python
enum HttpStatus:
    Ok = 200
    NotFound = 404
    InternalError = 500

s = HttpStatus::NotFound
print(s)                              # NotFound
print(s == HttpStatus::NotFound)      # true
```

```python
enum Permission:
    Read = 1
    Write = 2
    Execute = 4
```

ルール:
- simple enum（関連データを持たない ADT バリアントなし）のみ対応
- 値は整数リテラルのみ（負の値も可）
- いずれかのバリアントに明示的値がある場合、全バリアントに必要（混在不可）
- 重複値はコンパイルエラー
- `print()` はバリアント名を表示（整数値ではない）

### ADT バリアントの名前付きフィールド

ADT バリアントのフィールドにはドキュメント目的で名前をオプションで付けることができます。名前付きフィールドは定義を自己説明的にしますが、構築やパターンマッチングのセマンティクスは変わりません。

```python
enum Shape:
    Circle(radius: float)
    Rect(width: float, height: float)
    Point
```

- 構築は常に位置ベース: `Shape::Circle(3.14)` であり、`Shape::Circle(radius: 3.14)` ではない。
- パターンマッチングはユーザーが選んだ変数名で束縛: `case Shape::Circle(r):`。
- フィールド名は `snake_case` でなければならない。単一バリアント内での名前付きと名前なしの混在は不可。
- 名前なし構文（`Circle(float)`）は引き続き有効。

### 制約とエラー

| 制約 | 詳細 |
|------|------|
| バリアントアクセスは `EnumName::VariantName` | `::` 演算子が必須 |
| バリアント値 | デフォルトは自動割り当て（0, 1, 2, ...）、`= 値` で明示的に指定可能 |
| 比較は整数比較 | `==`, `!=` が使用可能 |
| 名前付きフィールド名 | `snake_case` でなければならない; バリアント内で重複不可; 名前付きと名前なしの混在不可 |
