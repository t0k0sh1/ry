[English](../../reference/types.md) | [日本語](types.md) | [繁體中文](../../zh/reference/types.md)

# 型リファレンス

## 型一覧

| 型 | 内部表現 | リテラル例 | 説明 |
|---|---|---|---|
| `int` | i64 | `42`, `-7`, `0xFF`, `0b1010` | 64ビット符号付き整数 |
| `byte` | i8 | （専用リテラルなし） | 符号なし8ビット整数（0-255）。型アノテーション `b: byte = 42` で使用 |
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
| `any` | `{ i64, [8 x i8] }` | `x: any = 42` | 任意のプリミティブ値を保持できるタグ付きユニオン |
| `T1 \| T2` | `{ i64, [N x i8] }` | `int \| str` | union 型（複数の型のいずれかを保持） |
| int リテラル | i64 | `42`, `0 \| 1` | int リテラル型（値の制約） |
| str リテラル | ptr | `"N" \| "S"` | str リテラル型（値の制約） |
| 範囲 | i64 | `1..12`, `-10..10` | 範囲型（整数の範囲制約） |

## 型アノテーション構文

変数宣言時に型を明示できます。型が推論可能な場合は省略可能です。

```python
x: int = 42
b: byte = 255
f: float = 3.14
s: str = "hello"
b: bool = true
opt: Option<int> = Some(10)
t: (int, float) = (1, 3.14)
xs: List<int> = [1, 2, 3]
m: Map<str, int> = {"a": 1}
s: Set<int> = {1, 2, 3}
fn_val: fn(int) -> int = fn(x: int): x * 2
u: int | str = 42
a: any = 42
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
| `any` | 任意のプリミティブ値（`int`, `float`, `bool`, `str`）または `Unit` を保持できる組み込み型。具体型への代入時はランタイム型チェック付きで自動 unwrap される。`any(int)` → `float` の自動昇格に対応。詳細は [any 型](#any-型) を参照 |
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

d: Meters = 3.14
names: StringList = ["Alice", "Bob"]
```

> **命名規則**: 型エイリアス名は PascalCase（例: `Meters`、`StringList`）を使用する必要があります。コンパイラがこの規則を強制します。

型エイリアスは関数型、リテラル型、範囲型にも使用できます:

```python
type Callback = fn(int, int) -> int

add: Callback = fn(a: int, b: int): a + b
print(add(3, 4))    # 7
```

```python
type Month = 1..12
type Direction = "N" | "S" | "E" | "W"
type Digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

m: Month = 6
d: Direction = "N"
n: Digit = 5
```

---

## リテラル型

リテラル型は、変数の値を特定の定数値に制限します。定数値の場合はコンパイル時に、動的な値の場合は実行時に制約がチェックされます。

### int リテラル型

```python
x: 42 = 42           # 単一リテラル型
y: 0 | 1 = 0         # int リテラルの union
z: 0 | 1 = 0
z = 1                     # OK
# z = 2                   # コンパイルエラー（定数）または実行時エラー（動的値）
```

### str リテラル型

```python
dir: "N" | "S" | "E" | "W" = "N"
# @const bad: "N" | "S" = "X"    # コンパイルエラー
```

### 制約チェック

- **コンパイル時**: 代入値が定数（`ConstantInt` や文字列リテラル）の場合、コンパイル時にチェックされ違反時はコンパイルエラー
- **実行時**: 値が動的（関数の戻り値など）の場合、実行時にチェックされ違反時はプログラムがエラー終了

---

## 範囲型

範囲型は、整数変数の値を連続した範囲（両端を含む）に制限します。

```python
month: 1..12 = 6       # OK
# @const bad: 1..12 = 0       # コンパイルエラー: 範囲外
# @const bad: 1..12 = 13      # コンパイルエラー: 範囲外

t: -10..10 = -5        # 負の範囲もサポート
```

### 可変変数での再代入（実行時チェック）

```python
x: 1..12 = 6
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
x: int? = 42       # Option<int> と同等
y: int? = none      # None と同等

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
name = "world"
print(f"Hello {name}")     # Hello world

a = 1
b = 2
print(f"{a} + {b} = {a + b}")   # 1 + 2 = 3
```

### 補間で使用可能な型

`{}` 内には `int`、`float`、`bool`、`str` に評価される任意の式を使用できます。

### エスケープシーケンス

| シーケンス | 出力 |
|---|---|
| `{{` | `{`（リテラルの波括弧） |
| `}}` | `}`（リテラルの波括弧） |
| `\n` `\r` `\t` `\\` `\"` | 通常の文字列と同じ |

```python
print(f"{{braces}}")   # {braces}
```

## 型キャスト（`as`）

`as` キーワードによる明示的な型変換です。

```python
x = 42 as float     # 42.0
y = 3.14 as int      # 3
z = 1 as bool        # true
s = 42 as str         # "42"
b = 255 as byte       # byte 値 255
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
c = Shape::Circle(3.14)
r = Shape::Rectangle(4.0, 5.0)
p = Shape::Point
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
a = MyOption<int>::MySome(42)
b = MyOption<int>::MyNone

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
e = Error("something went wrong")       # code のデフォルトは 0
e2 = Error("not found", 404)            # 明示的な code

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

val, err = divide(10, 2)
match err:
    case Some(e):
        print(e.message)
    case None:
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
    data = read_file("test.txt")!!   # エラーがあれば伝播
    return (data, none)
```

囲む関数も `(X, Error?)` を返す必要があります。

### 内部表現

`Error` は `{ ptr message, i64 code }` として表現されます。

## union 型

`|` を使って複数の型を持ちうる変数を宣言できます。

```python
x: int | str = 42
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

## any 型

`any` 型は、任意のプリミティブ値を保持できる組み込みの動的型です。Python の柔軟な型付けアプローチに倣い、静的な型の保証が不要な場面で、ジェネリクスや union 型を使わずに複数の型を扱えるようにします。

### 保持可能な型

| 型 | タグ | 説明 |
|---|-----|------|
| `int` | 0 | 64ビット符号付き整数 |
| `float` | 1 | 64ビット浮動小数点数 |
| `bool` | 2 | 真偽値 |
| `str` | 3 | 文字列 |
| `Unit` | 4 | Unit 値（戻り値なし関数用） |

`any` にはコレクション型（`List`、`Map`、`Set`）、リソース型（`TcpListener`、`TcpStream` 等）、関数ポインタ、ユーザー定義型（`record`、`enum`）は**保持できません**。

### 内部表現

`any` はタグ付きユニオンとして実装されています:

```
{ i64 tag, [8 x i8] data }   // 合計 16 バイト
```

`tag` フィールドが格納されている型を識別し、`data` フィールドに値（最大 8 バイト）を保持します。

### ラッピングとアンラッピング

具体型の値は `any` への代入時に自動的に**ラップ**され、`any` の値は具体型への代入時に自動的に**アンラップ**されます。

```python
# ラッピング: 具体型 → any
x: any = 42          # int が any にラップされる
x = "hello"          # 異なる型への再代入が可能

# アンラッピング: any → 具体型
fn get_value() -> any:
    return 42
n: int = get_value()  # any(int) が int にアンラップされる

# アンラップ時の int → float 自動昇格
f: float = get_value()  # any(int) がアンラップされ float に昇格
```

実行時の型がターゲット型と一致しない場合（例: `any(str)` を `int` 変数にアンラップ）、**ランタイムエラー**が発生します。

### 再代入

`any` 変数は、保持可能な任意の型の値に再代入できます:

```python
x: any = 42
x = 3.14       # OK: float を保持
x = "hello"    # OK: str を保持
x = true       # OK: bool を保持
```

### 算術演算

両方のオペランドが `any` の場合、実行時の型に基づいて演算がディスパッチされます:

| 演算 | 型 | 結果 |
|------|---|------|
| `+` | int + int | int |
| `+` | float + float | float |
| `+` | int + float | float |
| `+` | str + str | str（連結） |
| `-` | 数値 | int または float |
| `*` | 数値 | int または float |
| `*` | str * int / int * str | str（繰り返し） |
| `/` | 数値 | float（常に） |
| `//` | int // int | int |
| `//` | float を含む場合 | float |
| `%` | 数値 | int または float |
| `**` | 数値 | float（常に） |
| 単項 `-` | int | int |
| 単項 `-` | float | float |

一方が `any` で他方が具体型の場合、具体型の値は演算前に自動的にラップされます。

```python
x: any = 10
y: any = x + 20    # 20 が自動ラップされる; 結果は any(int) = 30
```

互換性のない型の組み合わせ（例: `str - int`）は**ランタイムエラー**になります。

### 比較演算

| 演算 | 動作 |
|------|------|
| `==`、`!=` | 同じ型同士で動作; int/float の混合比較が可能 |
| `<`、`<=`、`>`、`>=` | 数値（int/float 混合可）と文字列（辞書順） |

```python
x: any = 3
y: any = 3.0
print(x == y)    # true（int/float 比較）
```

比較時の型不一致（例: `int < str`）は**ランタイムエラー**になります。

### 文字列変換

`any` の値は `print()` と f-string 補間をサポートします:

```python
x: any = 42
print(x)              # 42
print(f"value: {x}")  # value: 42
```

変換ルール: `int` → 10進文字列、`float` → `%g` 形式、`bool` → `"true"`/`"false"`、`str` → そのまま、`Unit` → `"Unit"`。

### 型付き関数への any の受け渡し

`any` の値を具体的な引数型を持つ関数に渡せます。値は実行時の型チェック付きで自動的にアンラップされます:

```python
fn add_one(x: int) -> int:
    return x + 1

v: any = 42
result = add_one(v)   # any(int) が int にアンラップされる; 結果は 43
```

---

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
| `\r` | 復帰 |
| `\t` | タブ |
| `\\` | バックスラッシュ |
| `\"` | ダブルクォート |
| `\0` | ヌル文字 |

## 型安全性の制約

- **暗黙の型変換はない** — `int` と `float` を混在させると float への昇格が発生するが、それ以外の暗黙変換は存在しない。`byte` は演算時に `int` へ自動昇格する（ZExt）。型アノテーション `b: byte = 42` でのみ `int` リテラルから `byte` への縮小変換が許可される。
- **変数の型は宣言時に固定される** — 一度 `int` として宣言した変数に `float` を再代入することはできない。
- **ビット演算は `int` のみ** — `float` や `bool` に対してビット演算を適用するとコンパイルエラー。
- **`bool` 以外の型も条件式に使える** — `if` の条件式には `int`（0 = false、非0 = true）など `bool` 以外も使用可能。
