[English](../../reference/types.md) | [日本語](../../ja/reference/types.md) | [繁體中文](types.md)

# 型別參考

## 型別一覽

| 型別 | 內部表示 | 字面值範例 | 說明 |
|---|---|---|---|
| `int` | i64 | `42`, `-7`, `0xFF`, `0b1010` | 64 位元有號整數 |
| `byte` | i8 | （無專用字面值） | 無號 8 位元整數（0-255）。透過型別標註 `let b: byte = 42` 使用 |
| `float` | f64 | `3.14`, `0.5` | 64 位元浮點數 |
| `bool` | i1 | `true`, `false` | 布林值 |
| `str` | ptr | `"hello"`, `""`, `"a\nb"` | 字串（堆積上的不可變位元組序列） |
| `Unit` | void | （無回傳值） | 省略回傳值型別時的隱式回傳型別 |
| `Option<T>` | `{ i1, T }` | `Some(42)`, `None` | 可能存在值的型別 |
| `(T1, T2, ...)` | LLVM StructType (literal) | `(1, 3.14)` | 元組型別 |
| `List<T>` | ptr（堆積） | `[1, 2, 3]` | 動態陣列 |
| `Map<K, V>` | ptr（堆積） | `{"a": 1}` | 雜湊映射 |
| `Set<T>` | ptr（堆積） | `{1, 2, 3}` | 不重複的集合 |
| `fn(T1, T2) -> R` | ptr（函式指標） | `fn(x: int): x * 2` | 函式型別 |
| 使用者定義型別 | LLVM StructType (named) | `record Point: ...` | 以 `record` 關鍵字定義的結構體 |
| `enum` | i64 / 標籤聯合 | `Color::Red`, `Shape::Circle(3.14)` | 以 `enum` 關鍵字定義的列舉型別（支援關聯資料） |
| `Result<T, E>` | `{ i64, [N x i8] }` | `Ok(42)`, `Err("fail")` | 用於錯誤處理的結果型別 |
| `T1 \| T2` | `{ i64, [N x i8] }` | `int \| str` | union 型別（可持有多種型別之一） |

## 型別標註語法

宣告變數時可以明確指定型別。當型別可推論時可以省略。

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

## 可用型別名稱一覽

| 型別名稱 | 備註 |
|---|---|
| `int` | 內建純量型別 |
| `byte` | 內建純量型別（無號 0-255） |
| `float` | 內建純量型別 |
| `bool` | 內建純量型別 |
| `str` | 內建字串型別 |
| `Unit` | 無回傳值函式的回傳型別 |
| `Option<T>` | 泛型型別（T 為任意型別） |
| `(T1, T2, ...)` | 元組型別（元素數量和型別組合任意） |
| `List<T>` | 泛型動態陣列型別 |
| `Map<K, V>` | 泛型雜湊映射型別 |
| `Set<T>` | 泛型集合型別 |
| `fn(T1, ...) -> R` | 函式型別 |
| `Result<T, E>` | 結果型別（T = Ok 型別、E = Err 型別） |
| `T1 \| T2 \| ...` | union 型別（以 `\|` 分隔的多個型別之一） |
| 使用者定義型別名稱 | 以 `record` 或 `enum` 關鍵字宣告的型別 |
| int 字面量型別 | 以 int 字面量值限制（例：`42`、`0 \| 1`） |
| str 字面量型別 | 以字串字面量值限制（例：`"N" \| "S"`） |
| 範圍型別 | 以整數範圍限制（例：`1..12`、`-10..10`） |

## 型別別名

`type` 關鍵字為現有型別建立新的名稱。別名與原始型別完全互換。

```python
type Meters = float
type StringList = List<str>

let d: Meters = 3.14
let names: StringList = ["Alice", "Bob"]
```

> **命名慣例**：型別別名名稱必須使用 PascalCase（如 `Meters`、`StringList`）。編譯器會強制執行此慣例。

型別別名也可以用於函式型別、字面量型別和範圍型別：

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

## 字面量型別

字面量型別將變數的值限制為特定的常數值。對於常數值，在編譯時進行約束檢查；對於動態值，在執行時進行約束檢查。

### int 字面量型別

```python
let x: 42 = 42           # 單一字面量型別
let y: 0 | 1 = 0         # int 字面量的 union
var z: 0 | 1 = 0
z = 1                     # OK
# z = 2                   # 編譯錯誤（常數）或執行時錯誤（動態值）
```

### str 字面量型別

```python
let dir: "N" | "S" | "E" | "W" = "N"
# let bad: "N" | "S" = "X"    # 編譯錯誤
```

### 約束檢查

- **編譯時**：當賦值為常數（`ConstantInt` 或字串字面量）時，在編譯時檢查，違反時產生編譯錯誤
- **執行時**：當值為動態（如函式回傳值）時，在執行時檢查，違反時程式以錯誤退出

---

## 範圍型別

範圍型別將整數變數的值限制在連續的範圍內（包含兩端）。

```python
let month: 1..12 = 6       # OK
# let bad: 1..12 = 0       # 編譯錯誤：超出範圍
# let bad: 1..12 = 13      # 編譯錯誤：超出範圍

let t: -10..10 = -5        # 支援負數範圍
```

### 使用 `var` 重新賦值（執行時檢查）

```python
var x: 1..12 = 6
x = 12                      # OK
# x = dynamic_value()       # 執行時檢查：超出範圍則錯誤退出
```

### 在函式參數中使用

```python
fn set_month(m: 1..12) -> int:
    return m

set_month(6)                # OK
# set_month(13)             # 編譯錯誤（常數引數）
```

---

## `none` 關鍵字與 Option 型別簡寫

`none` 關鍵字表示 Option 型別的值不存在，等同於 `None`。

`T?` 語法是 `Option<T>` 的簡寫。

```python
let x: int? = 42       # 等同於 Option<int>
let y: int? = none      # 等同於 None

fn find(xs: List<int>, val: int) -> int?:
    for x in xs:
        if x == val:
            return Some(x)
    return none
```

---

## F-String（字串插值）

使用 `f"..."` 語法進行字串插值。`{}` 內的表達式會被求值並轉換為字串。

```python
let name = "world"
print(f"Hello {name}")     # Hello world

let a = 1
let b = 2
print(f"{a} + {b} = {a + b}")   # 1 + 2 = 3
```

### 插值中支援的型別

`{}` 內可使用求值結果為 `int`、`float`、`bool` 或 `str` 的任意表達式。

### 跳脫序列

| 序列 | 輸出 |
|---|---|
| `{{` | `{`（字面大括號） |
| `}}` | `}`（字面大括號） |
| `\n` `\t` `\\` `\"` | 與普通字串相同 |

```python
print(f"{{braces}}")   # {braces}
```

## 型別轉換（`as`）

使用 `as` 關鍵字進行明確的型別轉換。

```python
let x = 42 as float     # 42.0
let y = 3.14 as int      # 3
let z = 1 as bool        # true
let s = 42 as str         # "42"
let b = 255 as byte       # byte 值 255
```

### 支援的轉換

| 來源 | 目標 | 行為 |
|---|---|---|
| `int` | `float` | `SIToFP` |
| `float` | `int` | 截斷（`FPToSI`） |
| `int` | `bool` | `0` → `false`、非零 → `true` |
| `bool` | `int` | `false` → `0`、`true` → `1` |
| `int` / `float` / `bool` | `str` | 字串表示 |
| `int` | `byte` | 截斷（低 8 位元） |
| `byte` | `int` | 零擴展 |

不支援的轉換（例如 `str as int`）會產生編譯錯誤。字串轉數值請使用 `to_int()` / `to_float()`。

## 帶關聯資料的 enum（ADT）

在變體名稱後面加上括號並指定型別，enum 變體就可以攜帶關聯資料。不帶括號的變體仍然是單純的標籤。

```python
enum Shape:
    Circle(float)
    Rectangle(float, float)
    Point
```

### 建構子

使用 `EnumName::Variant(value)` 語法建立帶有資料的變體。

```python
let c = Shape::Circle(3.14)
let r = Shape::Rectangle(4.0, 5.0)
let p = Shape::Point
```

### 帶綁定的模式匹配

使用 `case EnumName::Variant(binding):` 形式取出關聯資料。

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

### 內部表示

ADT enum 以標籤聯合的形式儲存：`{ i64 tag, [N x i8] data }`，`N` 的大小足以容納最大變體的酬載。

---

## 泛型 enum

enum 可以使用角括號語法 `<T>` 帶有型別參數，使相同的 enum 結構可以持有不同型別的酬載。

```python
enum MyOption<T>:
    MySome(T)
    MyNone
```

### 使用方式

當編譯器無法推論型別時，需提供具體的型別引數來實例化。

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

## Result 型別

用於可恢復錯誤處理的型別。`Result<T, E>` 的值為 `Ok(value)`（成功）或 `Err(error)`（失敗）。

```python
fn divide(a: int, b: int) -> Result<int, str>:
    if b == 0:
        return Err("division by zero")
    return Ok(a // b)

let r = divide(10, 2)
match r:
    case Ok(v):
        print(v)      # 5
    case Err(e):
        print(e)
```

### 內部表示

`Result<T, E>` 以 `{ i64 tag, [max(sizeof(T), sizeof(E)) x i8] data }` 表示。Tag 0 = Ok、Tag 1 = Err。

### 模式匹配

使用 `match` 搭配 `Ok(binding)` 和 `Err(binding)` 模式。為確保窮舉性，兩種模式都是必需的。

## union 型別

可以使用 `|` 宣告可能持有多種型別的變數。

```python
let x: int | str = 42
x = "hello"     # 可重新賦值（union 中的任一型別）
print(x)        # hello
```

### 在函式引數與回傳值中的使用

```python
fn show(x: int | str) -> int:
    print(x)
    return 0

fn get_val(flag: bool) -> int | str:
    if flag:
        return 42
    return "hello"
```

### 內部表示

union 型別以 `{ i64 tag, [N x i8] data }` 表示。`tag` 表示各組成型別的索引（按字母順序排序後），`data` 是最大組成型別大小的位元組陣列。

### 限制

- 賦值不屬於 union 的型別會產生編譯錯誤
- `int | str` 和 `str | int` 是相同的型別（會被正規化）
- 使用 `print()` 輸出 union 值時，會根據執行時的 tag 以適當的型別顯示

## 型別規則（運算時的型別轉換）

| 運算 | 左運算元 | 右運算元 | 結果型別 | 備註 |
|---|---|---|---|---|
| `+` `-` `*` | int | int | int | |
| `+` `-` `*` | byte | byte 或 int | int | byte 在運算時以 ZExt 提升為 int |
| `+` `-` `*` | float 或 int | float 或 int（其中一方為 float） | float | 隱式 float 提升 |
| `/` | 任意數值 | 任意數值 | float | 始終為 float |
| `//` | 任意數值 | 任意數值 | int | float 輸入會截斷轉換 |
| `**` | 任意數值 | 任意數值 | float | 使用 libm `pow` |
| `%` | int | int | int | |
| `%` | float 或 int | float 或 int（其中一方為 float） | float | |
| `+` | str | str | str | 字串串接 |
| `==` `!=` `<` `<=` `>` `>=` | str | str | bool | 字典序比較 |
| `==` `!=` `<` `<=` `>` `>=` | 數值或 bool | 數值或 bool | bool | |
| `in` | 任意 | Set<T> | bool | 元素是否包含在集合中 |
| `&` `\|` `^` `~` `<<` `>>` | int | int | int | 對 float 會產生錯誤 |

### 跳脫序列（str 字面值內）

| 序列 | 意義 |
|---|---|
| `\n` | 換行 |
| `\t` | 定位字元 |
| `\\` | 反斜線 |
| `\"` | 雙引號 |
| `\0` | 空字元 |

## 型別安全性限制

- **沒有隱式型別轉換** — 混合使用 `int` 和 `float` 時會發生 float 提升，但除此之外不存在隱式轉換。`byte` 在運算時會自動提升為 `int`（ZExt）。僅在型別標註 `let b: byte = 42` 時允許從 `int` 字面值到 `byte` 的窄化轉換。
- **變數型別在宣告時固定** — 一旦以 `int` 宣告的變數，就無法重新賦值為 `float`。
- **位元運算僅限 `int`** — 對 `float` 或 `bool` 使用位元運算會產生編譯錯誤。
- **`bool` 以外的型別也可用於條件式** — `if` 的條件式可使用 `int`（0 = false、非 0 = true）等 `bool` 以外的型別。
