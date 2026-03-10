# ry

LLVM JIT ベースのシンプルなプログラミング言語。ソースコードを読み込み、LLVM ORC JIT でネイティブコードにコンパイル・即時実行します。

## 特徴

- **LLVM JIT コンパイル** — ORC LLJIT による高速なネイティブ実行
- **3 つの型** — `int` (i64)、`float` (f64)、`bool` (i1)
- **豊富な演算子** — 算術・比較・論理・ビット演算をサポート
- **let / const** — `let x = 42`（変数）/ `const x = 42`（定数）による明示的宣言
- **型アノテーション** — `let a: int = 10` のように明示的な型宣言が可能
- **制御構文** — `if`/`elif`/`else` による条件分岐（Python スタイルのインデントブロック）
- **型安全** — 変数への型変更再代入を禁止、const 変数への再代入を禁止
- **暗黙の型変換** — int/float 混合演算時に自動昇格

## サンプルコード

```python
# 型アノテーション付き変数宣言
let a: int = 10
let b: float = 3.14
let c: bool = true

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
| `print(expr)` | 値を標準出力に表示（型に応じて `%ld` / `%g` / `true`/`false`） |

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
- ブロック内で宣言した変数は外側のスコープでも参照できます（フラットスコープ）
- ネスト可能

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

使用可能な型名: `int`, `float`, `bool`

### 制約

- 変数・定数の宣言には `let` / `const` が必要（未宣言の変数への代入はエラー）
- `const` で宣言した定数への再代入はエラー
- 同じ名前の変数を再宣言するとエラー（`let x = 1` の後に `let x = 2` はエラー）
- 変数の型変更再代入は禁止（例: `let x = 1` の後に `x = 3.14` はエラー）
- 型アノテーションは暗黙的型変換を許容しない（strict）
- 関数定義・ループ（for/while）は未実装
