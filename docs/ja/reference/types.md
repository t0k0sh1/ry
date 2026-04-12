[English](../../reference/types.md) | [日本語](types.md) | [繁體中文](../../zh/reference/types.md)

# 型リファレンス

## 型一覧

| 型 | 内部表現 | リテラル例 | 説明 |
|---|---|---|---|
| `int` | i64 | `42`, `-7`, `0xFF`, `0b1010`, `100_000` | 64ビット符号付き整数 |
| `u8` | i8 | （専用リテラルなし） | 符号なし8ビット整数（0-255）。型アノテーション `b: u8 = 42` で使用 |
| `float` | f64 | `3.14`, `0.5`, `3.14_159`, `1e10`, `1.5e-3`, `2.5E+2` | 64ビット浮動小数点数（科学的記法をサポート） |
| `bool` | i1 | `true`, `false` | 真偽値 |
| `str` | ptr | `"hello"`, `""`, `"a\nb"` | 文字列（ヒープ上の不変バイト列） |
| `Unit` | void | （戻り値なし） | 戻り値のない関数の戻り値型。`-> Unit` で明示的に指定する必要がある |
| `Option<T>` | `{ i1, T }` | `Some(42)`, `None` | 値が存在するかもしれない型 |
| `(T1, T2, ...)` | LLVM StructType (literal) | `(1, 3.14)` | タプル型 |
| `List<T>` | ptr（ヒープ） | `[1, 2, 3]` | 動的配列 |
| `Map<K, V>` | ptr（ヒープ） | `{"a": 1}` | ハッシュマップ |
| `Set<T>` | ptr（ヒープ） | `{1, 2, 3}` | 重複なしの集合 |
| `function(T1, T2) -> R` | ptr（関数ポインタ） | `(x: int) => x * 2` | 関数型 |
| ユーザー定義型 | LLVM StructType (named) | `record Point: ...` | `record` キーワードで定義する構造体 |
| `enum` | i64 / タグ付きユニオン | `Color::Red`, `Shape::Circle(3.14)` | `enum` キーワードで定義する列挙型（関連データをサポート） |
| `Error` | `{ ptr, i64 }` | `Error("msg")`, `Error("msg", 404)` | 組み込みエラー型 |
| `Type` | `{ i64, ptr }` | `type_of(42)` | `type_of` が返すコンパイル時の型 identity。[Type](#type) を参照 |
| `any` | `{ i64, [8 x i8] }` | `x: any = 42` | 任意のプリミティブ値を保持できるタグ付きユニオン |
| `T1 \| T2` | `{ i64, [N x i8] }` | `int \| str` | union 型（複数の型のいずれかを保持） |
| int リテラル | i64 | `42`, `0 \| 1` | int リテラル型（値の制約） |
| str リテラル | ptr | `"N" \| "S"` | str リテラル型（値の制約） |
| 範囲 | i64 | `1..12`, `-10..10` | 範囲型（整数の範囲制約） |
| `i8` | i8 | `x: i8 = 42`, `x = 42i8` | 8ビット符号付き整数（低レベル、暗黙の変換なし） |
| `i16` | i16 | `x: i16 = 100`, `x = 100i16` | 16ビット符号付き整数（低レベル、暗黙の変換なし） |
| `i32` | i32 | `x: i32 = 42`, `x = 42i32` | 32ビット符号付き整数（低レベル、暗黙の変換なし） |
| `i64` | i64 | `x: i64 = 100`, `x = 100i64` | 64ビット符号付き整数（低レベル、暗黙の変換なし） |
| `u8` | i8 | `x: u8 = 200`, `x = 200u8` | 8ビット符号なし整数（低レベル、暗黙の変換なし） |
| `u16` | i16 | `x: u16 = 60000`, `x = 60000u16` | 16ビット符号なし整数（低レベル、暗黙の変換なし） |
| `u32` | i32 | `x: u32 = 4294967295`, `x = 100u32` | 32ビット符号なし整数（低レベル、暗黙の変換なし） |
| `u64` | i64 | `x: u64 = 18446744073709551615`, `x = 0xFFFFFFFFFFFFFFFFu64` | 2^64 − 1 までの 64ビット符号なし整数（低レベル、暗黙の変換なし） |
| `f32` | float | `x: f32 = 3.14`, `x = 1e10f32` | 32ビット浮動小数点数（低レベル、暗黙の変換なし） |
| `weak T` | ptr (header) | `weak s` | ARC 管理された値への弱参照（解放を妨げない） |
| `Regex` | ptr | `/[a-z]+/`, `/\d{3}/` | 正規表現パターン（正規表現リテラル構文で作成） |
| `Result<T, E>` | `{ i1, T/E }` | `Ok(42)`, `Err(Error("fail"))` | 成功（`Ok`）または失敗（`Err`）を表す型 |
| `Task<T>` | ptr | （async 関数が返す） | 非同期タスクハンドル（`await` と `block_on` で使用） |
| `Iterator<T>` | ptr | （`iter()` で作成） | 逐次要素アクセス用の遅延イテレータ |
| `T[N]` | `[N x T]` | `buf: i32[8]` | 固定長配列。低レベル型 T の N 要素（スタック割り当て、連続メモリ） |

## 型アノテーション構文

変数宣言時に型を明示できます。型が推論可能な場合は省略可能です。

```python
x: int = 42
b: u8 = 255
f: float = 3.14
s: str = "hello"
b: bool = true
opt: Option<int> = Some(10)
t: (int, float) = (1, 3.14)
xs: List<int> = [1, 2, 3]
m: Map<str, int> = {"a": 1}
s: Set<int> = {1, 2, 3}
fn_val: function(int) -> int = (x: int) => x * 2
rx: Regex = /[0-9]+/
u: int | str = 42
a: any = 42
```

## 使用可能な型名一覧

| 型名 | 備考 |
|---|---|
| `int` | 組み込みスカラー型 |
| `u8` | 組み込みスカラー型（符号なし 0-255） |
| `float` | 組み込みスカラー型 |
| `bool` | 組み込みスカラー型 |
| `str` | 組み込み文字列型 |
| `Unit` | 戻り値なし関数の戻り値型 |
| `Option<T>` | ジェネリック型（T は任意の型） |
| `(T1, T2, ...)` | タプル型（要素数・型の組み合わせは任意） |
| `List<T>` | ジェネリック動的配列型 |
| `Map<K, V>` | ジェネリックハッシュマップ型 |
| `Set<T>` | ジェネリック集合型 |
| `function(T1, ...) -> R` | 関数型 |
| `Error` | 組み込みエラー型（`message: str`、`code: int`） |
| `any` | 任意のプリミティブ値（`int`, `float`, `bool`, `str`）または `Unit` を保持できる組み込み型。暗黙の変換をサポート: 具体型の値は `any` への代入時に自動的にラップされ、`any` の値は具体型への代入時にランタイム型チェック付きで自動アンラップされる。`any(int)` → `float` の自動昇格に対応。詳細は [any 型](#any-型) を参照 |
| `T1 \| T2 \| ...` | union 型（`\|` で区切った複数の型のいずれか） |
| `i8` | 低レベル 8ビット符号付き整数（暗黙の変換なし） |
| `i16` | 低レベル 16ビット符号付き整数（暗黙の変換なし） |
| `i32` | 低レベル 32ビット符号付き整数（暗黙の変換なし） |
| `i64` | 低レベル 64ビット符号付き整数（暗黙の変換なし） |
| `u8` | 低レベル 8ビット符号なし整数（暗黙の変換なし） |
| `u16` | 低レベル 16ビット符号なし整数（暗黙の変換なし） |
| `u32` | 低レベル 32ビット符号なし整数（暗黙の変換なし） |
| `u64` | 低レベル 64ビット符号なし整数（暗黙の変換なし） |
| `f32` | 低レベル 32ビット浮動小数点数（暗黙の変換なし） |
| `T[N]` | 低レベル型 `T` の `N` 要素の固定長配列。スタック割り当て、連続メモリ。インデックスの読み書きと `length()` をサポート |
| ユーザー定義型名 | `record` または `enum` キーワードで宣言した型 |

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
type Callback = function(int, int) -> int

add: Callback = function(a: int, b: int) => a + b
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

型エイリアスはユニオン型（プリミティブ型やユーザー定義型を含む）も指せます。エイリアスはインライン展開したユニオンと同一に振る舞います:

```python
type Simple = int | str | bool

x: Simple = 42
y: Simple = "hello"
z: Simple = true

function describe(v: Simple) -> str:
  return to_str(v)
```

ユニオンの構成要素がまたエイリアスであるネストされたエイリアスは透過的にフラット化され、重複メンバーは除去されます。以下の 3 つの形式は等価です:

```python
type A = int | str
type B = A | bool          # `int | str | bool` と同じ
type C = B | int           # `int | str | bool` と同じ（int は重複除去）

x: B = 42
y: B = "hello"
z: B = true
```

---

## 数値リテラル

### 整数リテラル

10 進、16 進（`0x`/`0X`）、2 進（`0b`/`0B`）形式を受け付けます。桁の間にアンダースコアを視覚的な区切り文字として使えます（`1_000_000`, `0xFFFF_FFFF`）。

受け付けられる値の大きさはターゲット型で決まります:

| ターゲット | 範囲 |
|---|---|
| 裸の `int` / `i64` | `-9_223_372_036_854_775_808 .. 9_223_372_036_854_775_807` (i64) |
| `i8` / `i16` / `i32` | 対応する符号付き範囲 |
| `u8` / `u16` / `u32` | `0 .. 2^N - 1` |
| `u64` | `0 .. 18_446_744_073_709_551_615` (2^64 − 1) |

大きな符号なしリテラルには、サフィックス（`18446744073709551615u64`）または受け取り側の変数への型アノテーション（`x: u64 = 18446744073709551615`）が必要です。負リテラルは非負の大きさに対する単項マイナスとして到達するため、`-1i8` は許容されますが `-1u8` は拒否されます。

```python
max_u64: u64 = 18446744073709551615     # 2^64 - 1
mask:    u64 = 0xFFFF_FFFF_FFFF_FFFF    # 同じ値を 16 進で
word:    u32 = 4294967295               # 2^32 - 1
```

### 浮動小数点リテラル

```text
FloatLiteral := DecDigits '.' DecDigits Exponent? FloatSuffix?
             |  DecDigits Exponent FloatSuffix?
Exponent     := ('e' | 'E') ('+' | '-')? DecDigits
FloatSuffix  := 'f32' | 'f64'
```

float が期待される場所ならどこでも科学的記法が使えます:

```python
avogadro  = 6.022e23
planck    = 6.626e-34
light_spd = 2.998E8
big       = 1e10f32
```

指数がオーバーフローする場合は `+Inf` / `-Inf` が生成されます（コンパイルエラーではありません）。注意: ランタイムの `to_float()` コンバータはより厳格で、オーバーフロー時に `+Inf` ではなく `Err(Error)` を返します。

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
function set_month(m: 1..12) -> int:
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

function find(xs: List<int>, val: int) -> int?:
    for x in xs:
        if x == val:
            return Some(x)
    return none
```

---

## 弱参照（`weak T`）

`weak` 参照は、ARC 管理された値への非所有参照です。強参照とは異なり、弱参照は強参照カウントをインクリメントしません。最後の強参照が解放されると、参照先オブジェクトは解放され、残存する弱参照は自動的に `None` になります。

弱参照は参照サイクルを解消するためのユーザー向けメカニズムです。

### 弱参照の作成

型アノテーションと式の両方で `weak` キーワードを使用します:

```python
s = "hello"
w: weak str = weak s
```

`weak T` 型は新しい型コンストラクタで、`T` は ARC 管理された型（現在は `str`、`List<T>`、`Map<K, V>`、`Set<T>`）でなければなりません。

### 弱参照のアクセス（アップグレード）

弱変数のアクセスは自動的に**アップグレード**を行います。これは強参照カウントのアトミックなチェックとインクリメントです。結果は常に `Option<T>` です:

- 参照先がまだ存在する場合（強参照カウント > 0）は `Some(value)`
- 参照先が解放済みの場合（強参照カウント == 0）は `None`

```python
s = "alive"
w: weak str = weak s
case w:
  Some(v):
    print(v)           # "alive"
  None:
    print("deallocated")
```

合体演算子（`??`）も弱参照で使用できます:

```python
w: weak str = weak s
val = w ?? "default"
```

### 再代入

弱参照は再代入可能です。古い弱参照は解放され、新しいものが保持されます:

```python
a = "first"
b = "second"
w: weak str = weak a
w = weak b
```

### スレッドセーフティ

アップグレード操作は内部的に compare-and-swap (CAS) ループを使用するため、スレッド間で安全に使用できます。これは強参照が並行して解放される可能性があるため重要です。

### スコープのクリーンアップ

弱参照はスコープから外れると自動的に解放されます。強参照カウントと弱参照カウントの両方がゼロになると、ARC ヘッダーが解放されます。

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

`{}` 内には `int`、`float`、`bool`、`str`、record 型、タプル、またはコレクション型（`List`、`Map`、`Set`）に評価される任意の式を使用できます。

```python
xs = [1, 2, 3]
print(f"items: {xs}")     # items: [1, 2, 3]

t = (1, "hello")
print(f"tuple: {t}")      # tuple: (1, hello)
```

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
b = 255 as u8         # u8 値 255
```

### サポートされるキャスト

| 変換元 | 変換先 | 動作 |
|---|---|---|
| `int` | `float` | `SIToFP` |
| `float` | `int` | 切り捨て（`FPToSI`） |
| `int` | `bool` | `0` -> `false`、非0 -> `true` |
| `bool` | `int` | `false` -> `0`、`true` -> `1` |
| `int` / `float` / `bool` | `str` | 文字列表現 |
| `int` | `u8` | 切り捨て（下位8ビット） |
| `u8` | `int` | ゼロ拡張 |

| `int` | `i8` / `i16` / `i32` / `i64` | 切り捨て（i64 の場合は恒等） |
| `i8` / `i16` / `i32` / `i64` | `int` | 符号拡張（`SExt`） |
| `int` | `u8` / `u16` / `u32` / `u64` | 切り捨て（u64 の場合は恒等） |
| `u8` / `u16` / `u32` / `u64` | `int` | ゼロ拡張（`ZExt`） |
| 符号付き | 符号付き（より広い） | 符号拡張（`SExt`） |
| 符号付き | 符号付き（より狭い） | 切り捨て |
| 符号なし | 符号なし/符号付き（より広い） | ゼロ拡張（`ZExt`） |
| 符号なし | 符号なし/符号付き（より狭い） | 切り捨て |
| 符号付き / 符号なし整数 | `float` | `SIToFP` / `UIToFP` → `f64` |
| `float` | 符号付き / 符号なし整数 | `FPToSI` / `FPToUI` |
| `float` | `f32` | `FPTrunc` |
| `f32` | `float` | `FPExt` |
| 符号付き整数 | `f32` | `SIToFP` |
| 符号なし整数 | `f32` | `UIToFP` |
| `f32` | 符号付き / 符号なし整数 | `FPToSI` / `FPToUI` |

`as` のターゲット型はジェネリック型を含む完全な型構文をサポートします:

```python
x = value as Option<int>
y = data as Map<str, int>
```

`as` キャスト（ジェネリクスを含む）は、組み込みキャストまたは対応するユーザー定義の `operator as` が必要です。それ以外はコンパイルエラーになります。文字列から数値への変換には `to_int()` / `to_float()` を使用してください。

## 関連データを持つ enum（ADT）

バリアント名の後ろに括弧で型を指定することで、enum バリアントに関連データを持たせることができます。括弧なしのバリアントは従来通りの単純なタグとして機能します。

```python
enum Shape:
    Circle(float)
    Rectangle(float, float)
    Point
```

### 名前付きフィールド

バリアントにはドキュメント明確化のために名前付きフィールドをオプションで使用できます。名前付きフィールドはバリアント定義を自己説明的にしますが、ランタイム動作は変わりません。構築とパターンマッチングは位置ベースのままです。

```python
enum Shape:
    Circle(radius: float)
    Rectangle(width: float, height: float)
    Point
```

ルール:
- フィールド名は `snake_case` でなければなりません。
- 単一バリアント内では、すべてのフィールドが名前付きか名前なしのいずれかでなければなりません（混在不可）。
- バリアント内のフィールド名の重複はコンパイルエラーです。

### コンストラクタ

`EnumName::Variant(value)` の構文でデータ付きバリアントを構築します。フィールドが名前付きであっても引数は常に位置ベースです。

```python
c = Shape::Circle(3.14)
r = Shape::Rectangle(4.0, 5.0)
p = Shape::Point
```

### バインディング付きパターンマッチング

`case EnumName::Variant(binding):` の形式で関連データを取り出せます。バインディングにはフィールド名ではなくユーザーが選択した変数名を使用します。

```python
case c:
    Shape::Circle(r):
        print(r)            # 3.14
    Shape::Rectangle(w, h):
        print(w)
        print(h)
    Shape::Point:
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

case a:
    MyOption::MySome(v):
        print(v)      # 42
    MyOption::MyNone:
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

### Result を使ったエラーハンドリング

失敗する可能性のある関数は `Result<V, E>` を返します:

```python
function divide(a: int, b: int) -> Result<int, Error>:
    if b == 0:
        return Err(Error("division by zero"))
    return Ok(a // b)

case divide(10, 2):
    Ok(v):
        print(v)            # 5
    Err(e):
        print(e.message)
```

戻り値が意味を持たない場合は `Result<Unit, Error>` を使用します:

```python
function save(path: str, data: str) -> Result<Unit, Error>:
    return Ok(0 as u8)   # Unit プレースホルダー

case save("/tmp/test.txt", "hello"):
    Ok(_):
        print("saved")
    Err(e):
        print(e.message)
```

### Result 型

`Result<V, E>` は2つのコンストラクタを持つ組み込みパラメータ化型です:

- `Ok(value)` -- 成功バリアント
- `Err(error)` -- エラーバリアント

`case` を使った網羅的なエラーハンドリングに使用します。`Ok` と `Err` の両方のケースをカバーするか、`_` ワイルドカードを使用する必要があります。

**等価性:**
`Result<T, E>` は `==` と `!=` をサポートします。バリアントが一致し（`Ok`/`Ok` または `Err`/`Err`）、内部値が等しい場合に 2 つの結果は等しくなります。

```python
function make_ok(v: int) -> Result<int, Error>: return Ok(v)
make_ok(42) == make_ok(42)   # true
make_ok(1)  == make_ok(2)    # false
make_ok(1)  != Err(Error("e"))  # true
```

**テストマッチャー:**
- `expect(x).to_be_ok()` -- 結果が `Ok` であることをアサート
- `expect(x).to_be_err()` -- 結果が `Err` であることをアサート

### 内部表現

`Error` は `{ ptr message, i64 code }` として表現されます。
`Result<V, E>` は `{ i1 isOk, V okValue, E errValue }` として表現されます。

## Type

`Type` は組み込みの [`type_of`](builtins.md#type_of) 関数が返す値です。型のコンパイル時 identity を表現し、実行時に反射的な比較ができます。

```ry
print(to_str(type_of(42)))          # int
print(to_str(type_of([1, 2, 3])))   # List

print(type_of(42) == type_of(100))  # true
print(type_of(42) == type_of(3.14)) # false
```

主な性質:

- 型定義（プリミティブ、コレクション、record、enum、`Option`、`Result`、`function`、`Type` 自身など）ごとにコンパイル時に一意な identity が与えられる。
- `Type` 値に対する `==` / `!=` は表示名ではなく identity を比較する。2 つの異なる record（または同名の record と enum）は常に区別可能。
- `print` と `to_str` は人間が読める型名を表示する（例: `"int"`, `"List"`, `"Point"`, `"i32"`）。
- 低レベルの数値型（`i8`, `i16`, …, `f32`）は `int` / `float` と区別される。
- コレクションのジェネリクスはベース名に畳まれる。`type_of([1, 2])` は `"List<int>"` ではなく `"List"` を返す。
- `Type` は反射的。`type_of(type_of(x))` は `Type` 自身を表す `Type` 値を返す。

### 内部表現

`Type` は `{ i64 id, ptr name }` として表現されます。`id` フィールドは等価性比較に、`name` フィールドは表示に使われます。両フィールドはコンパイル時に `type_of` によって格納されます。

## union 型

`|` を使って複数の型を持ちうる変数を宣言できます。

```python
x: int | str = 42
x = "hello"     # 再代入可能（union のいずれかの型）
print(x)        # hello
```

### 関数引数・戻り値での使用

```python
function show(x: int | str) -> int:
    print(x)
    return 0

function get_val(flag: bool) -> int | str:
    if flag:
        return 42
    return "hello"
```

### 内部表現

union 型は `{ i64 tag, [N x i8] data }` として表現されます。`tag` は各コンポーネント型のインデックス（アルファベット順ソート後）を示し、`data` は最大コンポーネントサイズ分のバイト配列です。

### 等価性

ユニオン型は現在、プリミティブバリアント（`int`, `float`, `str`, `bool`）について `==` と `!=` をサポートします。2 つのユニオン値は、同じバリアント（同じタグ）を持ち、内部値が等しい場合に等しくなります。

```python
x: int | str = 42
y: int | str = 42
x == y   # true

z: int | str = "42"
x == z   # false (異なるタグ: int と str)
```

### 制約

- union に含まれない型を代入するとコンパイルエラー
- `int | str` と `str | int` は同じ型（正規化される）
- `print()` で union 値を出力すると、実行時のタグに基づいて適切な型で表示される
- `==` と `!=` はプリミティブバリアント（`int`, `float`, `str`, `bool`）をサポート。クロージャバリアントは非対応

## any 型

`any` 型は、任意のプリミティブ値を保持できる組み込みの動的型です。Python の柔軟な型付けアプローチに倣い、静的な型の保証が不要な場面で、ジェネリクスや union 型を使わずに複数の型を扱えるようにします。

### 保持可能な型

`any` は以下の型を保持できます:

| 型 | タグ | 説明 |
|------|-----|------|
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
function get_value() -> any:
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
function add_one(x: int) -> int:
    return x + 1

v: any = 42
result = add_one(v)   # any(int) が int にアンラップされる; 結果は 43
```

---

## 型規則（演算時の型変換）

| 演算 | 左辺 | 右辺 | 結果型 | 備考 |
|---|---|---|---|---|
| `+` `-` `*` | int | int | int | |
| `+` `-` `*` | u8 | u8 | u8 | 低レベル型: native 幅の unsigned 演算、暗黙の昇格なし |
| `+` `-` `*` | float または int | float または int（片方がfloat） | float | 暗黙のfloat昇格 |
| `/` | 任意の数値 | 任意の数値 | float | 常にfloat |
| `//` | 任意の数値 | 任意の数値 | int または float | 切り捨て除算（-∞ 方向）; int オペランド同士なら int、片方が float なら float |
| `**` | 任意の数値 | 任意の数値 | float | libm `pow` 使用 |
| `%` | int | int | int | |
| `%` | float または int | float または int（片方がfloat） | float | |
| `+` | str | str | str | 文字列結合 |
| `==` `!=` `<` `<=` `>` `>=` | str | str | bool | 辞書順比較 |
| `==` `!=` `<` `<=` `>` `>=` | 数値または bool | 数値または bool | bool | |
| `in` | 任意 | Set<T> | bool | 要素がセットに含まれるか |
| `&` `\|` `^` `~` `<<` `>>` | int | int | int | float にはエラー |
| `+` `-` `*` | i32 | i32 | i32 | 低レベル型: 暗黙の変換なし、同じ型が必要 |
| `/` `//` | i32 | i32 | i32 | 符号付き整数除算（`SDiv`） |
| `/` `//` | u32 | u32 | u32 | 符号なし整数除算（`UDiv`） |
| `%` | i32 | i32 | i32 | 符号付き剰余（`SRem`） |
| `%` | u32 | u32 | u32 | 符号なし剰余（`URem`） |
| `+` `-` `*` `/` | f32 | f32 | f32 | |
| `==` `!=` | i32/u32 | i32/u32 | bool | 符号非依存の等値比較 |
| `<` `<=` `>` `>=` | i32 | i32 | bool | 符号付き比較（`ICMP_SLT` 等） |
| `<` `<=` `>` `>=` | u32 | u32 | bool | 符号なし比較（`ICMP_ULT` 等） |
| `>>` | i32 | i32 | i32 | 算術右シフト（符号保持） |
| `>>` | u32 | u32 | u32 | 論理右シフト（ゼロ埋め） |
| `**` | 低レベル | 任意 | エラー | 低レベル型には累乗演算子は未対応 |
| 混合 | 低レベル | 異なる型 | エラー | 低レベル型と高レベル型の混合はコンパイルエラー |

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

- **暗黙の拡大変換** -- 関数呼び出しでは安全な拡大変換がサポートされます: `u8` → `int`、`u8` → `float`、`int` → `float`。二項演算子では `int` と `float` の混合で float 昇格が発生します。`u8` は native 幅の unsigned 演算を行う低レベル型であり、二項演算子での `u8` と `int` の混合はコンパイルエラーです。縮小変換（例: `float` → `int`）は暗黙には許可されません。`int` リテラルから `u8` への縮小変換は型アノテーション `b: u8 = 42` でのみ許可されます。
- **変数の型は宣言時に固定される** -- 一度 `int` として宣言した変数に `float` を再代入することはできない。
- **ビット演算は `int` のみ** -- `float` や `bool` に対してビット演算を適用するとコンパイルエラー。
- **`bool` 以外の型も条件式に使える** -- `if` の条件式には `int`（0 = false、非0 = true）など `bool` 以外も使用可能。
- **数値リテラルセパレータ** -- アンダースコアは数値リテラルの視覚的な区切りとして使用可能: `100_000`、`0xFF_FF`、`0b1010_0101`、`3.14_159`。アンダースコアは桁の間に配置する必要があります（先頭、末尾、連続は不可）。
- **数値リテラルサフィックス** -- 低レベル型はリテラルサフィックスで指定可能: `42i32`、`255u8`、`3.14f32`、`.5f32`、`0xFFu8`、`0b1010u8`。float サフィックスを付けた整数リテラル（`42f32`）は float 値を生成します。整数サフィックスを付けた浮動小数点リテラル（`3.14i32`）はコンパイルエラーです。範囲外の値（例: `256u8`、`129i8`）もコンパイルエラーです。
- **低レベル数値型（`i8`、`i16`、`i32`、`i64`、`u8`、`u16`、`u32`、`u64`、`f32`）は暗黙の変換なし** -- 低レベル型同士または高レベル型（`int`、`float`）との混合はコンパイルエラーです。明示的な `as` キャストを使用してください。低レベル整数の `/` 演算子は（Rust と同様に）float 除算ではなく整数除算を行います。符号付き型は `SDiv`/`SRem`、符号なし型は `UDiv`/`URem` を使用します。
- **符号付き vs 符号なし** -- 符号付き型（`i8`、`i16`、`i32`、`i64`）は符号付き比較（`ICMP_SLT` 等）と算術右シフト（`AShr`）を使用します。符号なし型（`u8`、`u16`、`u32`、`u64`）は符号なし比較（`ICMP_ULT` 等）と論理右シフト（`LShr`）を使用します。`>>>` 演算子は符号に関わらず常に論理シフトを行います。
- **`int` 算術オーバーフローはランタイムエラー** -- 高レベル `int` 型の算術演算（`+`、`-`、`*`、単項 `-`）はオーバーフロー時にランタイムエラーを発生させます。Swift のデフォルト動作と同様です。これにより、2の補数ラッピングによるサイレントなデータ破損を防ぎます。オーバーフローする定数式はコンパイル時に検出されます。
- **低レベル整数のオーバーフローはラップアラウンド** -- 低レベル整数型の算術演算はオーバーフロー時に Ry 定義の2の補数ラップ（符号付き）またはモジュラー演算（符号なし）を使用します。例えば、`2147483647i32 + 1i32` は `-2147483648` にラップします。明示的なオーバーフロー制御には `checked_add/sub/mul`（`Result<T, Error>` を返す）、`saturating_add/sub/mul`（型の境界値にクランプ）、`wrapping_add/sub/mul`（自己文書化的なラッピング）を使用できます。[関数リファレンス](functions.md#checkedsaturating-arithmetic) を参照。
