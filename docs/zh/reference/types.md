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
| `fn(T1, T2) -> R` | ptr（函式指標） | `(x: int) -> x * 2` | 函式型別 |
| 使用者定義型別 | LLVM StructType (named) | `type Point: ...` | 以 `type` 關鍵字定義的結構體 |
| `enum` | i64 | `Color::Red` | 以 `enum` 關鍵字定義的列舉型別 |
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
let fn_val: fn(int) -> int = (x: int) -> x * 2
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
| `T1 \| T2 \| ...` | union 型別（以 `\|` 分隔的多個型別之一） |
| 使用者定義型別名稱 | 以 `type` 或 `enum` 關鍵字宣告的型別 |

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
