# ry

LLVM JIT ベースのシンプルなプログラミング言語。ソースコードを読み込み、LLVM ORC JIT でネイティブコードにコンパイル・即時実行します。

## 特徴

- **LLVM JIT コンパイル** — ORC LLJIT による高速なネイティブ実行
- **6 つの組み込み型 + ユーザー定義型 + タプル + リスト + マップ** — `int` (i64)、`float` (f64)、`bool` (i1)、`str` (ptr)、`Unit` (void)、`Option<T>` (nullable)、`type` による構造体定義、タプル型 `(T1, T2, ...)`、リスト型 `list[T]`、マップ型 `map[K, V]`
- **豊富な演算子** — 算術・比較・論理・ビット演算をサポート（ユーザー定義型への演算子オーバーロード対応）
- **let / const** — `let x = 42`（変数）/ `const x = 42`（定数）による明示的宣言
- **型アノテーション** — `let a: int = 10` のように明示的な型宣言が可能
- **関数定義** — `fn` キーワードによるユーザー定義関数（引数・戻り値の型宣言、再帰対応、オーバーロード対応）
- **制御構文** — `if`/`elif`/`else`、`while` ループ（Python スタイルのインデントブロック）
- **モジュールインポート** — `from ... import ...` 構文で別ファイルの関数をインポート（相対パス・`RY_PATH` 検索・循環検出）
- **UFCS** — `a.f(b)` を `f(a, b)` として呼び出す Uniform Function Call Syntax
- **型安全** — 変数への型変更再代入を禁止、const 変数への再代入を禁止
- **暗黙の型変換** — int/float 混合演算時に自動昇格

## サンプルコード

```python
# 型アノテーション付き変数宣言
let a: int = 10
let b: float = 3.14
let c: bool = true

# 文字列
let greeting: str = "hello"
print(greeting)
print(len(greeting))               # 5
print("hello" + " world")          # hello world
print("hello" == "hello")          # true
print(greeting.contains("ell"))    # true
print(greeting.starts_with("hel")) # true
print(greeting.ends_with("llo"))   # true

# 型推論による変数宣言
let x = 10 + 3 * 2
print(x)

# 定数宣言
const pi = 3.14159

# 浮動小数点
let r = 5
let area = pi * r ** 2
print(area)

# 比較・論理演算
let a = 10 > 5 and 3 <= 3
print(a)

# ビット演算
let mask = 0xFF & (1 << 4)
print(mask)

# 関数定義
fn add(a: int, b: int) -> int:
    return a + b

let result = add(1, 2)
print(result)

# 再帰関数
fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print(factorial(5))

# 関数オーバーロード
fn add(a: int, b: int) -> int:
    return a + b

fn add(a: float, b: float) -> float:
    return a + b

print(add(1, 2))       # 3
print(add(1.5, 2.5))   # 4

# 再代入（let で宣言した変数のみ）
let count = 0
count = count + 1
print(count)

# 条件分岐
let x = 10
if x > 20:
    print(1)
elif x > 5:
    print(2)
else:
    print(3)

# while ループ
let i = 3
while i > 0:
    print(i)
    i = i - 1

# 構造体定義
type Point:
    x: int
    y: int

let p = Point(10, 20)
print(p.x)
print(p.y)

# 構造体を関数の引数に
fn distance_x(a: Point, b: Point) -> int:
    return a.x - b.x

let d = distance_x(Point(10, 0), Point(3, 0))
print(d)

# 戻り値型省略（Unit型）
fn greet():
    print(42)

greet()

# Option型
let x: Option<int> = Some(42)
print(x)

let y: Option<int> = None
print(y)

let v = unwrap(x)
print(v)

# 演算子オーバーロード
type Vec2:
    x: int
    y: int

fn operator+(a: Vec2, b: Vec2) -> Vec2:
    return Vec2(a.x + b.x, a.y + b.y)

fn operator==(a: Vec2, b: Vec2) -> bool:
    return a.x == b.x and a.y == b.y

let v1 = Vec2(1, 2)
let v2 = Vec2(3, 4)
let v3 = v1 + v2
print(v3.x)             # 4
print(v3.y)             # 6
print(v1 == v2)         # false

# UFCS (Uniform Function Call Syntax)
fn add(a: int, b: int) -> int:
    return a + b

let x = 1
print(x.add(2))       # add(x, 2) → 3

# チェーン呼び出し
fn double(n: int) -> int:
    return n * 2

print(x.add(2).double())  # double(add(x, 2)) → 6

# タプル型
let t = (1, 3.14)
print(t.0)
print(t.1)

# タプルを返す関数
fn swap(a: int, b: int) -> (int, int):
    return (b, a)

let result = swap(1, 2)
print(result.0)
print(result.1)

# リスト型
let xs = [1, 2, 3]
print(xs[0])       # 1
print(len(xs))     # 3
print(xs)          # [1, 2, 3]

# リストを関数の引数に
fn first(xs: list[int]) -> int:
    return xs[0]

print(first(xs))   # 1

# マップ型
let m = {"a": 1, "b": 2}
print(m["a"])       # 1
print(len(m))       # 2
print(m)            # {a: 1, b: 2}

m["c"] = 3          # キー追加
m["a"] = 99         # 値更新
print(m.has_key("a"))  # true

# モジュールインポート
from math import add, sub
print(add(1, 2))
```

## 必要環境

- LLVM 21
- CMake 3.20+
- C++17 対応コンパイラ

## ビルド

```bash
cmake -B build -DLLVM_DIR=/usr/local/llvm/lib/cmake/llvm
cmake --build build
```

## 実行

```bash
./build/ry <file.ry>
```

## テスト

GoogleTest を使用したユニットテストが含まれています。

```bash
cd build && ctest --output-on-failure
```

## 言語仕様

### コメント

`#` から行末までがコメントとして無視されます。

```python
# これはコメント
x = 10  # 行末コメント
```

### 型

| 型 | 内部表現 | リテラル例 |
|---|---|---|
| int | i64 | `42`, `-7` |
| float | f64 | `3.14`, `0.5` |
| bool | i1 | `true`, `false` |
| str | ptr | `"hello"`, `""` |
| Unit | void | 戻り値なし関数の戻り値型 |
| Option\<T\> | { i1, T } | `Some(42)`, `None` |
| (T1, T2, ...) | LLVM StructType (literal) | `(1, 3.14)`, `(a, b, c)` |
| list[T] | ptr (ヒープ確保) | `[1, 2, 3]`, `["a", "b"]` |
| map[K, V] | ptr (ヒープ確保) | `{"a": 1, "b": 2}` |
| ユーザー定義型 | LLVM StructType (named) | `type Point: ...` で定義 |

### 演算子（優先順位: 高→低）

| 優先順位 | 演算子 | 説明 |
|---|---|---|
| 1 | `()` | グループ化 |
| 2 | `+x` `-x` `~x` | 単項正負、ビットNOT |
| 3 | `**` | 累乗（右結合） |
| 4 | `*` `/` `%` `//` | 乗算・除算・剰余・整数除算 |
| 5 | `+` `-` | 加算・減算 |
| 6 | `<<` `>>` | ビットシフト |
| 7 | `&` | ビットAND |
| 8 | `^` | ビットXOR |
| 9 | `\|` | ビットOR |
| 10 | `==` `!=` `<` `<=` `>` `>=` | 比較 |
| 11 | `not` | 論理NOT |
| 12 | `and` | 論理AND |
| 13 | `or` | 論理OR |

### 演算の型規則

- `/` は常に float を返す
- `//` は常に int を返す（float 入力は切り捨て変換）
- `**` は常に float を返す（libm `pow` を使用）
- `%` は両辺が int なら int、片方でも float なら float
- `+` `-` `*` は片方が float なら float に昇格
- `+` は両辺が str の場合、文字列結合を返す
- `==` `!=` は両辺が str の場合、文字列比較を行う（`<` `<=` `>` `>=` は未対応）
- ビット演算子は int のみ（float を渡すとエラー）
- 比較演算子は bool を返す

### 組み込み関数

| 関数 | 説明 |
|---|---|
| `print(expr)` | 値を標準出力に表示（型に応じて `%ld` / `%g` / `true`/`false` / `%s` / `Some(...)` / `None` / `[...]` / `{...}`） |
| `Some(expr)` | Option 型の値ありバリアントを構築 |
| `unwrap(opt)` | Option 値を取り出す（None ならランタイムエラーで exit(1)） |
| `len(list_or_map_or_str)` | リスト・マップの要素数、または文字列の長さを返す |
| `has_key(map, key)` | マップにキーが存在するかを bool で返す（UFCS: `m.has_key(k)`） |
| `contains(str, sub)` | 文字列に部分文字列が含まれるかを bool で返す（UFCS: `s.contains("x")`） |
| `starts_with(str, prefix)` | 文字列が指定の接頭辞で始まるかを bool で返す（UFCS: `s.starts_with("x")`） |
| `ends_with(str, suffix)` | 文字列が指定の接尾辞で終わるかを bool で返す（UFCS: `s.ends_with("x")`） |

### 制御構文

#### if / elif / else

Python スタイルのインデントブロックによる条件分岐です。条件式の後にコロン `:` を書き、ブロックはスペース（4 スペース推奨）でインデントします。

```python
if x > 0:
    print(x)
elif x == 0:
    print(0)
else:
    print(-1)
```

- 条件式は `bool` 以外も受け付けます（`int`: 0 が false、非 0 が true）
- `elif` / `else` は省略可能
- ブロック内で宣言した変数はブロック外からは参照できません（ブロックスコープ）
- 外側のスコープの変数はブロック内から参照・再代入可能
- 内側のスコープで同名の変数を宣言すると外側の変数をシャドーイング
- ネスト可能

#### while

Python スタイルの `while` ループです。条件式が真の間、ブロックを繰り返し実行します。

```python
let i = 3
while i > 0:
    print(i)
    i = i - 1
```

- 条件式は `bool` 以外も受け付けます（`int`: 0 が false、非 0 が true）
- ネスト可能（while 内 while、while 内 if など）

#### 関数定義

`fn` キーワードでユーザー定義関数を宣言します。引数と戻り値には型宣言が必要です。

```python
fn add(a: int, b: int) -> int:
    return a + b

let result = add(1, 2)
print(result)
```

- 引数の型宣言は必須（`name: type` 形式）
- 戻り値の型は `->` の後に指定（省略時は `Unit` 型）
- `return` 文で値を返す（`Unit` 関数では `return` のみ、または省略可能）
- 再帰呼び出し対応
- 関数オーバーロード: 引数の数や型が異なる同名関数を複数定義可能
  - 呼び出し時に引数の型で自動的に適切なオーバーロードを選択
  - 引数型が同一で戻り値型のみ異なる定義はコンパイルエラー
  - マッチするオーバーロードがない場合はコンパイルエラー
- 関数は式レベルでも文レベルでも呼び出し可能（`let x = f(1)` / `f(1)`）
- UFCS（Uniform Function Call Syntax）: `a.f(b)` は `f(a, b)` に脱糖される
  - チェーン可能: `a.f(b).g(c)` → `g(f(a, b), c)`
  - フィールドアクセスと混在可能: `p.x.f()` → `f(p.x)`
- 演算子オーバーロード: `fn operator<op>(params) -> RetType:` 構文でユーザー定義型に演算子を定義可能
  - 二項演算子: パラメータ 2 個（`fn operator+(a: Vec2, b: Vec2) -> Vec2:`）
  - 単項演算子: パラメータ 1 個（`fn operator-(a: Vec2) -> Vec2:`）
  - 対応演算子: `+`, `-`, `*`, `/`, `%`, `**`, `//`, `==`, `!=`, `<`, `<=`, `>`, `>=`, `&`, `|`, `^`, `~`, `<<`, `>>`, `and`, `or`, `not`
  - 組み込み型（int, float, bool）の演算子はハードコードのまま維持（ユーザー定義が優先）

### 変数・定数宣言

変数は `let`、定数は `const` で宣言します。型推論と型アノテーションの両方が使えます。

```python
# let: 変数（再代入可能）
let x = 42          # int
let y = 3.14        # float
let z = true        # bool

# const: 定数（再代入不可）
const pi = 3.14159

# 型アノテーション（明示的に型を指定）
let x: int = 42
let y: float = 3.14
const z: bool = true

# 再代入（let のみ）
let count = 0
count = count + 1
```

型アノテーションを付けた場合、右辺の式の型がアノテーションと一致しなければコンパイルエラーになります。暗黙的な型変換は行いません（例: `let a: float = 10` はエラー）。

使用可能な型名: `int`, `float`, `bool`, `str`, `Unit`, `Option<T>`, `(T1, T2, ...)`, `list[T]`, `map[K, V]`, およびユーザー定義型名

#### 型定義（構造体）

`type` キーワードでユーザー定義の構造体型を宣言します。フィールドはインデントブロック内に `name: type` 形式で記述します。

```python
type Point:
    x: int
    y: int

# コンストラクタ呼び出し（関数呼び出しと同じ構文）
let p = Point(10, 20)

# フィールドアクセス（ドット記法）
print(p.x)   # 10
print(p.y)   # 20

# 構造体を関数の引数・戻り値に使用
fn make_point(x: int, y: int) -> Point:
    return Point(x, y)

# ネスト構造体
type Line:
    start: Point
    end: Point
```

- 構造体はスタック上の値型として扱われます
- コンストラクタの引数はフィールド定義順に対応
- 同一フィールド名の重複定義はエラー
- フィールドへの代入（`p.x = 10`）は未対応
- `print()` に構造体を直接渡すとエラー

#### タプル型

タプルは複数の値をまとめて扱うための型です。関数から複数の値を返す場合に便利です。

```python
# タプルリテラル
let t = (1, 3.14)

# 型アノテーション付き
let t: (int, float) = (1, 3.14)

# 要素アクセス（.0, .1, ...）
print(t.0)    # 1
print(t.1)    # 3.14

# 関数の戻り値にタプル
fn swap(a: int, b: int) -> (int, int):
    return (b, a)

let result = swap(1, 2)
print(result.0)  # 2
print(result.1)  # 1
```

- タプルは LLVM の literal StructType として実装（構造的等価）
- 要素へのアクセスは `.0`, `.1`, ... のインデックス記法
- 範囲外のインデックスはコンパイルエラー
- `print()` にタプルを直接渡すとエラー（要素ごとにアクセスして出力）

#### リスト型

リストは同じ型の要素を可変長で保持する型です。ヒープ上に確保されます。

```python
# リストリテラル
let xs = [1, 2, 3]

# 型アノテーション付き
let xs: list[int] = [1, 2, 3]

# インデックスアクセス
print(xs[0])    # 1
print(xs[2])    # 3

# 変数によるインデックス
let i = 1
print(xs[i])    # 2

# 長さ取得
print(len(xs))  # 3

# リスト全体の表示
print(xs)       # [1, 2, 3]

# 関数の引数に
fn first(xs: list[int]) -> int:
    return xs[0]

print(first(xs))  # 1
```

- 全要素が同じ型でなければコンパイルエラー（`[1, 3.14]` はエラー）
- 空リスト `[]` は型推論できないためエラー
- 範囲外アクセス（負のインデックス含む）はランタイムエラーで exit(1)
- `int`, `float`, `bool`, `str` の要素をサポート

#### マップ型

マップはキーと値のペアを保持する型です（Python の辞書に相当）。ヒープ上に確保されます。

```python
# マップリテラル
let m = {"a": 1, "b": 2}

# 型アノテーション付き
let m: map[str, int] = {"a": 1, "b": 2}

# キーアクセス
print(m["a"])    # 1

# キー代入（挿入/更新）
m["c"] = 3      # 新規キー追加
m["a"] = 99     # 既存キー更新

# 長さ取得
print(len(m))    # 3

# マップ全体の表示
print(m)         # {a: 99, b: 2, c: 3}

# キー存在チェック（UFCS）
print(m.has_key("a"))  # true
print(m.has_key("z"))  # false

# 関数の引数に
fn get_val(m: map[str, int], k: str) -> int:
    return m[k]
```

- 全キーが同じ型、全値が同じ型でなければコンパイルエラー
- 空マップ `{}` は型推論できないためエラー
- 存在しないキーへのアクセスはランタイムエラーで exit(1)
- キーの検索は線形スキャン（`str` は `strcmp`、その他は値比較）
- 容量超過時は自動的に2倍に拡張

#### モジュールインポート

`from` キーワードで別ファイルの関数をインポートします。

```python
# 全関数をインポート
from math

# 特定の関数を選択インポート
from math import add

# 複数関数を選択インポート
from math import add, sub

# サブディレクトリはドット区切り
from utils.math import add
# → utils/math.ry を検索
```

- モジュール名は拡張子なし（`from math` → `math.ry` を検索）
- ドット区切りでサブディレクトリを指定（`from utils.math` → `utils/math.ry`）
- トップレベルでのみ使用可能（ブロック内では不可）
- 二重インポートは自動的にスキップ
- 循環インポートはエラー

**検索パスの優先順位:**

1. インポート元ファイルのディレクトリからの相対パス
2. 環境変数 `RY_PATH`（コロン区切りで複数パス指定可能）

```bash
export RY_PATH=/path/to/libs:/another/path
./build/ry main.ry
```

### 制約

- 変数・定数の宣言には `let` / `const` が必要（未宣言の変数への代入はエラー）
- `const` で宣言した定数への再代入はエラー
- 同じ名前の変数を再宣言するとエラー（`let x = 1` の後に `let x = 2` はエラー）
- 変数の型変更再代入は禁止（例: `let x = 1` の後に `x = 3.14` はエラー）
- 型アノテーションは暗黙的型変換を許容しない（strict）
- ループ（for）は未実装
