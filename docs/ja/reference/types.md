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
| `fn(T1, T2) -> R` | ptr（関数ポインタ） | `(x: int) -> x * 2` | 関数型 |
| ユーザー定義型 | LLVM StructType (named) | `type Point: ...` | `type` キーワードで定義する構造体 |
| `enum` | i64 | `Color::Red` | `enum` キーワードで定義する列挙型 |
| `T1 \| T2` | `{ i64, [N x i8] }` | `int \| str` | union 型（複数の型のいずれかを保持） |

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
let fn_val: fn(int) -> int = (x: int) -> x * 2
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
| `T1 \| T2 \| ...` | union 型（`\|` で区切った複数の型のいずれか） |
| ユーザー定義型名 | `type` または `enum` キーワードで宣言した型 |

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
