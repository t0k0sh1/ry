# ry

LLVM JIT ベースのシンプルなプログラミング言語。ソースコードを読み込み、LLVM ORC JIT でネイティブコードにコンパイル・即時実行します。

## 特徴

- **LLVM JIT コンパイル** — ORC LLJIT による高速なネイティブ実行
- **豊富な型システム** — `int`, `float`, `bool`, `str`, `Option<T>`, タプル, `list<T>`, `map<K,V>`, `set<T>`, `enum`, 関数型, ユーザー定義構造体
- **演算子** — 算術・比較・論理・ビット演算・複合代入・`in` 演算子（演算子オーバーロード対応）
- **関数** — `fn` 定義・再帰・オーバーロード・ラムダ（クロージャ）・高階関数・UFCS
- **制御構文** — `if`/`elif`/`else`, `while`, `for...in`, `break`/`continue`
- **モジュール** — `from ... import ...` による関数インポート
- **型安全** — 型推論・型アノテーション・型変更再代入禁止・let/var

## サンプルコード

```python
# 変数と型
let x: int = 42
let name: str = "hello"
let pi = 3.14159

# 関数定義
fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print(factorial(5))    # 120

# ラムダとクロージャ
let offset = 10
let add_offset = (x: int): int => x + offset
print(add_offset(5))   # 15

# 構造体
type Point:
    x: int
    y: int

fn operator+(a: Point, b: Point) -> Point:
    return Point(a.x + b.x, a.y + b.y)

let p = Point(1, 2) + Point(3, 4)
print(p.x)             # 4

# コレクション
let xs = [1, 2, 3]
let m = {"a": 1, "b": 2}
let s = {1, 2, 3}

for x in xs:
    print(x)

print(2 in s)          # true
print(m["a"])           # 1

# 列挙型
enum Color:
    Red
    Green
    Blue

let c = Color::Red
print(c)               # Red

# モジュールインポート
from math import add
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

```bash
cd build && ctest --output-on-failure
```

## ドキュメント

詳しい言語仕様・チュートリアルは [docs/](docs/README.md) を参照してください。
