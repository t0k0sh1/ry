# ry

LLVM JIT ベースのシンプルなプログラミング言語。ソースコードを読み込み、LLVM ORC JIT でネイティブコードにコンパイル・即時実行します。

## 特徴

- **LLVM JIT コンパイル** — ORC LLJIT による高速なネイティブ実行
- **4 つの型** — `int` (i64)、`float` (f64)、`bool` (i1)、`string` (ptr)
- **豊富な演算子** — 算術・比較・論理・ビット演算をサポート
- **let / const** — `let x = 42`（変数）/ `const x = 42`（定数）による明示的宣言
- **型アノテーション** — `let a: int = 10` のように明示的な型宣言が可能
- **関数定義** — `fn` キーワードによるユーザー定義関数（引数・戻り値の型宣言、再帰対応）
- **制御構文** — `if`/`elif`/`else`、`while` ループ（Python スタイルのインデントブロック）
- **モジュールインポート** — `from ... import ...` 構文で別ファイルの関数をインポート（相対パス・`RY_PATH` 検索・循環検出）
- **型安全** — 変数への型変更再代入を禁止、const 変数への再代入を禁止
- **暗黙の型変換** — int/float 混合演算時に自動昇格

## サンプルコード

```python
# 型アノテーション付き変数宣言
let a: int = 10
let b: float = 3.14
let c: bool = true

# 文字列
let greeting: string = "hello"
print(greeting)

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
| string | ptr | `"hello"`, `""` |

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
- ビット演算子は int のみ（float を渡すとエラー）
- 比較演算子は bool を返す

### 組み込み関数

| 関数 | 説明 |
|---|---|
| `print(expr)` | 値を標準出力に表示（型に応じて `%ld` / `%g` / `true`/`false` / `%s`） |

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
- 戻り値の型は `->` の後に指定
- `return` 文で値を返す
- 再帰呼び出し対応
- 関数は式レベルでも文レベルでも呼び出し可能（`let x = f(1)` / `f(1)`）

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

使用可能な型名: `int`, `float`, `bool`, `string`

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
