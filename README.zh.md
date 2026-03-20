[English](README.md) | [日本語](README.ja.md) | [繁體中文](README.zh.md)

# Ry

基於 LLVM JIT 的簡潔程式語言。讀取原始碼後，透過 LLVM ORC JIT 編譯為原生程式碼並即時執行。

## 特色

- **LLVM JIT 編譯** — 使用 ORC LLJIT 實現高速原生執行
- **豐富的型別系統** — `int`、`float`、`bool`、`str`、`Option<T>`、`Error`、元組、`List<T>`、`Map<K,V>`、`Set<T>`、`enum`、函式型別、使用者自訂結構體
- **運算子** — 算術、比較、邏輯、位元（`>>>` 邏輯右移）、複合賦值、`in` / `not in`、字串重複（`"ab" * 3`）、`as` 型別轉換（支援運算子多載）
- **f-string** — 使用 `f"Hello {name}"` 進行字串內插
- **契約式設計** — `require`（前置條件）、`ensure`（後置條件）、`invariant`（結構體不變量）、`old()`、`result`
- **指令** — `@deprecated` 編譯時元資料標註
- **函式** — `fn` 定義、遞迴、多載、Lambda（閉包）、高階函式、UFCS
- **控制流程** — `if`/`elif`/`else`、`while`、`for...in`、`break`/`continue`
- **套件** — 基於目錄的套件、自動匯入的 `std` 函式庫、`from ... import ...`
- **型別安全** — 型別推論、型別標註、禁止變更型別的重新賦值、let/var

## 範例程式碼

```python
# 變數與型別
let x: int = 42
let name: str = "hello"
let pi = 3.14159

# 函式定義
fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print(factorial(5))    # 120

# Lambda 與閉包
let offset = 10
let add_offset = (x: int): int => x + offset
print(add_offset(5))   # 15

# 結構體
type Point:
    x: int
    y: int

fn operator+(a: Point, b: Point) -> Point:
    return Point(a.x + b.x, a.y + b.y)

let p = Point(1, 2) + Point(3, 4)
print(p.x)             # 4

# 集合型別
let xs = [1, 2, 3]
let m = {"a": 1, "b": 2}
let s = {1, 2, 3}

for x in xs:
    print(x)

print(2 in s)          # true
print(m["a"])           # 1

# 串流操作 (filter, map, sort)
let result = [5, 3, 1, 4, 2].filter((x: int) -> x > 1).map((x: int) -> x * 10).sort()
print(result)          # [20, 30, 40, 50]

# 列舉型別
enum Color:
    Red
    Green
    Blue

let c = Color::Red
print(c)               # Red

# 套件匯入
from math import add
print(add(1, 2))
```

## 安裝

### 一行指令（macOS Apple Silicon）

```bash
curl -fsSL https://raw.githubusercontent.com/t0k0sh1/ry/main/install.sh | sh
```

指定特定版本：

```bash
curl -fsSL https://raw.githubusercontent.com/t0k0sh1/ry/main/install.sh | sh -s v0.0.3
```

預設安裝至 `~/.local/bin`。可透過 `RY_INSTALL_DIR` 環境變數變更安裝位置。

標準函式庫安裝至 `$RY_HOME/lib/std/`（預設：`~/.ry/lib/std/`）。

### 從原始碼建置

需求環境：
- LLVM 21
- CMake 3.20+
- 支援 C++17 的編譯器

```bash
cmake -B build -DLLVM_DIR=/usr/local/llvm/lib/cmake/llvm
cmake --build build
```

## 執行

```bash
ry <file.ry>
```

## 測試

```bash
cd build && ctest --output-on-failure
```

## 文件

詳細的語言規格與教學請參閱 [docs/](docs/zh/README.md)。
