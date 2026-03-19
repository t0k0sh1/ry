[English](../../reference/types.md) | [日本語](types.md) | [繁體中文](../../zh/reference/types.md)

# 型リファレンス

## 型一覧

| 型 | 内部表現 | リテラル例 | 説明 |
|---|---|---|---|
| `int` | i64 | `42`, `-7`, `0xFF`, `0b1010` | 64ビット符号付き整数 |
| `byte` | i8 | （専用リテラルなし） | 符号なし8ビット整数（0-255）。型アノテーション `let b: byte = 42` で使用 |
| `float` | f64 | `3.14`, `0.5` | 64ビット浮動小数点数 |
| `bool` | i1 | `true`, `false` | 真偽値 |
| `str` | ptr | `"hello"`, `""`, `"a\nb"` | 文字列（ヒープ上の不変バイト列） |
| `Unit` | void | （戻り値なし） | 戻り値型省略時の暗黙の戻り値型 |
| `Option<T>` | `{ i1, T }` | `Some(42)`, `None` | 値が存在するかもしれない型 |
| `(T1, T2, ...)` | LLVM StructType (literal) | `(1, 3.14)` | タプル型 |
| `List<T>` | ptr（ヒープ） | `[1, 2, 3]` | 動的配列 |
| `Map<K, V>` | ptr（ヒープ） | `{"a": 1}` | ハッシュマップ |
| `Set<T>` | ptr（ヒープ） | `{1, 2, 3}` | 重複なしの集合 |
| `fn(T1, T2) -> R` | ptr（関数ポインタ） | `fn(x: int): x * 2` | 関数型 |
| ユーザー定義型 | LLVM StructType (named) | `record Point: ...` | `record` キーワードで定義する構造体 |
| `enum` | i64 / タグ付きユニオン | `Color::Red`, `Shape::Circle(3.14)` | `enum` キーワードで定義する列挙型（関連データをサポート） |
| `Error` | `{ ptr, i64 }` | `Error("msg")`, `Error("msg", 404)` | 組み込みエラー型 |
| `T1 \| T2` | `{ i64, [N x i8] }` | `int \| str` | union 型（複数の型のいずれかを保持） |
| int リテラル | i64 | `42`, `0 \| 1` | int リテラル型（値の制約） |
| str リテラル | ptr | `"N" \| "S"` | str リテラル型（値の制約） |
| 範囲 | i64 | `1..12`, `-10..10` | 範囲型（整数の範囲制約） |

## 型アノテーション構文

変数宣言時に型を明示できます。型が推論可能な場合は省略可能です。

```python
let x: int = 42
let b: byte = 255
let f: float = 3.14
let s: str = "hello"
let b: bool = true
let opt: Option<int> = Some(10)
let t: (int, float) = (1, 3.14)
let xs: List<int> = [1, 2, 3]
let m: Map<str, int> = {"a": 1}
let s: Set<int> = {1, 2, 3}
let fn_val: fn(int) -> int = fn(x: int): x * 2
let u: int | str = 42
```

## 使用可能な型名一覧

| 型名 | 備考 |
|---|---|
| `int` | 組み込みスカラー型 |
| `byte` | 組み込みスカラー型（符号なし 0-255） |
| `float` | 組み込みスカラー型 |
| `bool` | 組み込みスカラー型 |
| `str` | 組み込み文字列型 |
| `Unit` | 戻り値なし関数の戻り値型 |
| `Option<T>` | ジェネリック型（T は任意の型） |
| `(T1, T2, ...)` | タプル型（要素数・型の組み合わせは任意） |
| `List<T>` | ジェネリック動的配列型 |
| `Map<K, V>` | ジェネリックハッシュマップ型 |
| `Set<T>` | ジェネリック集合型 |
| `fn(T1, ...) -> R` | 関数型 |
| `Error` | 組み込みエラー型（`message: str`、`code: int`） |
| `T1 \| T2 \| ...` | union 型（`\|` で区切った複数の型のいずれか） |
| ユーザー定義型名 | `record` または `enum` キーワードで宣言した型 |
| int リテラル型 | int リテラル値による制約（例: `42`、`0 \| 1`） |
| str リテラル型 | 文字列リテラル値による制約（例: `"N" \| "S"`） |
| 範囲型 | 整数の範囲による制約（例: `1..12`、`-10..10`） |

## 型エイリアス

`type` キーワードで既存の型に新しい名前を付けます。エイリアスは元の型と完全に互換性があります。

```python
type Meters = float
type StringList = List<str>

let d: Meters = 3.14
let names: StringList = ["Alice", "Bob"]
```

> **命名規則**: 型エイリアス名は PascalCase（例: `Meters`、`StringList`）を使用する必要があります。コンパイラがこの規則を強制します。

型エイリアスは関数型、リテラル型、範囲型にも使用できます:

```python
type Callback = fn(int, int) -> int

let add: Callback = fn(a: int, b: int): a + b
print(add(3, 4))    # 7
```

```python
type Month = 1..12
type Direction = "N" | "S" | "E" | "W"
type Digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

let m: Month = 6
let d: Direction = "N"
let n: Digit = 5
```

---

## リテラル型

リテラル型は、変数の値を特定の定数値に制限します。定数値の場合はコンパイル時に、動的な値の場合は実行時に制約がチェックされます。

### int リテラル型

```python
let x: 42 = 42           # 単一リテラル型
let y: 0 | 1 = 0         # int リテラルの union
var z: 0 | 1 = 0
z = 1                     # OK
# z = 2                   # コンパイルエラー（定数）または実行時エラー（動的値）
```

### str リテラル型

```python
let dir: "N" | "S" | "E" | "W" = "N"
# let bad: "N" | "S" = "X"    # コンパイルエラー
```

### 制約チェック

- **コンパイル時**: 代入値が定数（`ConstantInt` や文字列リテラル）の場合、コンパイル時にチェックされ違反時はコンパイルエラー
- **実行時**: 値が動的（関数の戻り値など）の場合、実行時にチェックされ違反時はプログラムがエラー終了

---

## 範囲型

範囲型は、整数変数の値を連続した範囲（両端を含む）に制限します。

```python
let month: 1..12 = 6       # OK
# let bad: 1..12 = 0       # コンパイルエラー: 範囲外
# let bad: 1..12 = 13      # コンパイルエラー: 範囲外

let t: -10..10 = -5        # 負の範囲もサポート
```

### `var` での再代入（実行時チェック）

```python
var x: 1..12 = 6
x = 12                      # OK
# x = dynamic_value()       # 実行時チェック: 範囲外ならエラー終了
```

### 関数パラメータでの使用

```python
fn set_month(m: 1..12) -> int:
    return m

set_month(6)                # OK
# set_month(13)             # コンパイルエラー（定数引数）
```

---

## `none` キーワードと Option 型の省略記法

`none` キーワードは Option 型の値が存在しないことを表し、`None` と同等です。

`T?` 構文は `Option<T>` の省略記法です。

```python
let x: int? = 42       # Option<int> と同等
let y: int? = none      # None と同等

fn find(xs: List<int>, val: int) -> int?:
    for x in xs:
        if x == val:
            return Some(x)
    return none
```

---

## F-String（文字列補間）

`f"..."` 構文による文字列補間です。`{}` 内の式が評価され、文字列に変換されます。

```python
let name = "world"
print(f"Hello {name}")     # Hello world

let a = 1
let b = 2
print(f"{a} + {b} = {a + b}")   # 1 + 2 = 3
```

### 補間で使用可能な型

`{}` 内には `int`、`float`、`bool`、`str` に評価される任意の式を使用できます。

### エスケープシーケンス

| シーケンス | 出力 |
|---|---|
| `{{` | `{`（リテラルの波括弧） |
| `}}` | `}`（リテラルの波括弧） |
| `\n` `\t` `\\` `\"` | 通常の文字列と同じ |

```python
print(f"{{braces}}")   # {braces}
```

## 型キャスト（`as`）

`as` キーワードによる明示的な型変換です。

```python
let x = 42 as float     # 42.0
let y = 3.14 as int      # 3
let z = 1 as bool        # true
let s = 42 as str         # "42"
let b = 255 as byte       # byte 値 255
```

### サポートされるキャスト

| 変換元 | 変換先 | 動作 |
|---|---|---|
| `int` | `float` | `SIToFP` |
| `float` | `int` | 切り捨て（`FPToSI`） |
| `int` | `bool` | `0` -> `false`、非0 -> `true` |
| `bool` | `int` | `false` -> `0`、`true` -> `1` |
| `int` / `float` / `bool` | `str` | 文字列表現 |
| `int` | `byte` | 切り捨て（下位8ビット） |
| `byte` | `int` | ゼロ拡張 |

サポートされないキャスト（例: `str as int`）はコンパイルエラーになります。文字列から数値への変換には `to_int()` / `to_float()` を使用してください。

## 関連データを持つ enum（ADT）

バリアント名の後ろに括弧で型を指定することで、enum バリアントに関連データを持たせることができます。括弧なしのバリアントは従来通りの単純なタグとして機能します。

```python
enum Shape:
    Circle(float)
    Rectangle(float, float)
    Point
```

### コンストラクタ

`EnumName::Variant(value)` の構文でデータ付きバリアントを構築します。

```python
let c = Shape::Circle(3.14)
let r = Shape::Rectangle(4.0, 5.0)
let p = Shape::Point
```

### バインディング付きパターンマッチング

`case EnumName::Variant(binding):` の形式で関連データを取り出せます。

```python
match c:
    case Shape::Circle(r):
        print(r)            # 3.14
    case Shape::Rectangle(w, h):
        print(w)
        print(h)
    case Shape::Point:
        print("point")
```

### 内部表現

ADT enum はタグ付きユニオンとして格納されます: `{ i64 tag, [N x i8] data }`。`N` は最大バリアントのペイロードに合わせたサイズです。

---

## ジェネリック enum

enum は `<T>` の形式で型パラメータを持てます。これにより、同じ enum 構造で異なる型のペイロードを保持できます。

```python
enum MyOption<T>:
    MySome(T)
    MyNone
```

### 使用法

コンパイラが型を推論できない場合は、具体的な型引数を指定してインスタンス化します。

```python
let a = MyOption<int>::MySome(42)
let b = MyOption<int>::MyNone

match a:
    case MyOption::MySome(v):
        print(v)      # 42
    case MyOption::MyNone:
        print("none")
```

---

## Error 型

エラーハンドリング用の組み込み型です。`Error` は `message`（str）と `code`（int）の2つのフィールドを持ちます。

```python
let e = Error("something went wrong")       # code のデフォルトは 0
let e2 = Error("not found", 404)            # 明示的な code

print(e.message)   # something went wrong
print(e2.code)     # 404
print(e2)          # Error: not found (code: 404)
```

### エラーハンドリング規約

失敗する可能性のある関数は `(T, Error?)` タプルを返します:

```python
fn divide(a: int, b: int) -> (int, Error?):
    if b == 0:
        return (0, Some(Error("division by zero")))
    return (a // b, none)

let val, err = divide(10, 2)
if err != none:
    match err:
        case Some(e):
            print(e.message)
else:
    print(val)          # 5
```

### `!!` 演算子（エラー伝播）

`!!` 後置演算子は `(T, Error?)` タプルから値を取り出します。エラーが存在する場合、そのエラーは囲む関数に伝播されます。

```python
fn read_file(path: str) -> (str, Error?):
    if path == "":
        return ("", Some(Error("empty path")))
    return ("content", none)

fn process() -> (str, Error?):
    let data = read_file("test.txt")!!   # エラーがあれば伝播
    return (data, none)
```

囲む関数も `(X, Error?)` を返す必要があります。

### 内部表現

`Error` は `{ ptr message, i64 code }` として表現されます。

## union 型

`|` を使って複数の型を持ちうる変数を宣言できます。

```python
let x: int | str = 42
x = "hello"     # 再代入可能（union のいずれかの型）
print(x)        # hello
```

### 関数引数・戻り値での使用

```python
fn show(x: int | str) -> int:
    print(x)
    return 0

fn get_val(flag: bool) -> int | str:
    if flag:
        return 42
    return "hello"
```

### 内部表現

union 型は `{ i64 tag, [N x i8] data }` として表現されます。`tag` は各コンポーネント型のインデックス（アルファベット順ソート後）を示し、`data` は最大コンポーネントサイズ分のバイト配列です。

### 制約

- union に含まれない型を代入するとコンパイルエラー
- `int | str` と `str | int` は同じ型（正規化される）
- `print()` で union 値を出力すると、実行時のタグに基づいて適切な型で表示される

## 型規則（演算時の型変換）

| 演算 | 左辺 | 右辺 | 結果型 | 備考 |
|---|---|---|---|---|
| `+` `-` `*` | int | int | int | |
| `+` `-` `*` | byte | byte または int | int | byte は演算時に int へ ZExt 昇格 |
| `+` `-` `*` | float または int | float または int（片方がfloat） | float | 暗黙のfloat昇格 |
| `/` | 任意の数値 | 任意の数値 | float | 常にfloat |
| `//` | 任意の数値 | 任意の数値 | int | float入力は切り捨て変換 |
| `**` | 任意の数値 | 任意の数値 | float | libm `pow` 使用 |
| `%` | int | int | int | |
| `%` | float または int | float または int（片方がfloat） | float | |
| `+` | str | str | str | 文字列結合 |
| `==` `!=` `<` `<=` `>` `>=` | str | str | bool | 辞書順比較 |
| `==` `!=` `<` `<=` `>` `>=` | 数値または bool | 数値または bool | bool | |
| `in` | 任意 | Set<T> | bool | 要素がセットに含まれるか |
| `&` `\|` `^` `~` `<<` `>>` | int | int | int | float にはエラー |

### エスケープシーケンス（str リテラル内）

| シーケンス | 意味 |
|---|---|
| `\n` | 改行 |
| `\t` | タブ |
| `\\` | バックスラッシュ |
| `\"` | ダブルクォート |
| `\0` | ヌル文字 |

## 型安全性の制約

- **暗黙の型変換はない** — `int` と `float` を混在させると float への昇格が発生するが、それ以外の暗黙変換は存在しない。`byte` は演算時に `int` へ自動昇格する（ZExt）。型アノテーション `let b: byte = 42` でのみ `int` リテラルから `byte` への縮小変換が許可される。
- **変数の型は宣言時に固定される** — 一度 `int` として宣言した変数に `float` を再代入することはできない。
- **ビット演算は `int` のみ** — `float` や `bool` に対してビット演算を適用するとコンパイルエラー。
- **`bool` 以外の型も条件式に使える** — `if` の条件式には `int`（0 = false、非0 = true）など `bool` 以外も使用可能。
