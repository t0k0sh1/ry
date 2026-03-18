[English](../../reference/directives.md) | [日本語](../../ja/reference/directives.md) | [繁體中文](directives.md)

# 指令

指令是可以附加到宣告上的編譯時元資料。使用 `@name` 語法。

## 語法

```
@name
@name(key=value, ...)
```

指令放置在目標宣告之前。可以堆疊多個指令。

## 支援的目標

指令可以套用到以下宣告:

- `fn` - 函式定義
- `record` - 結構體定義
- `let` / `var` - 變數宣告
- `record` 定義內的欄位

## 內建指令

### `@deprecated`

將宣告標記為已棄用。當已棄用的實體被使用（呼叫、參照或存取）時，會發出編譯時警告。

**套用於函式:**

```
@deprecated
fn old_function() -> int:
    return 42

print(old_function())   # warning: 'old_function' is deprecated
```

**套用於型別:**

```
@deprecated
record OldPoint:
    x: int
    y: int

let p = OldPoint(1, 2)  # warning: 'OldPoint' is deprecated
```

**套用於變數:**

```
@deprecated
let old_value = 99

print(old_value)         # warning: 'old_value' is deprecated
```

**套用於欄位:**

```
record Config:
    @deprecated
    old_setting: int
    new_setting: int

let c = Config(1, 2)
print(c.old_setting)     # warning: 'Config.old_setting' is deprecated
print(c.new_setting)     # 無警告
```

### `@native`

宣告由執行環境（內建）提供實作的函式。該函式不能有函式本體。

**基本語法:**

```
@native
fn contains(s: str, sub: str) -> bool

print(contains("hello world", "world"))  # true
```

**運算子多載:**

```
@native
fn operator+(a: str, b: str) -> str

print("hello" + " world")  # hello world
```

**與 UFCS 搭配使用:**

```
@native
fn to_upper(s: str) -> str

print("hello".to_upper())  # HELLO
```

**參數數量驗證:**

當 `@native` 宣告包含型別簽名時，編譯器會在呼叫處驗證參數數量。支援重載函式（例如：1、2、3 個參數的 `range`），只要任一重載匹配即通過驗證。

**標準函式庫宣告 (`core/`):**

`core/` 目錄包含所有內建函式的 `@native` 宣告，按類別組織：

| 檔案 | 內容 |
|---|---|
| `core/builtins.ry` | `print`, `len`, `range`, `enumerate`, `zip`, `exit`, `args` |
| `core/str.ry` | `contains`, `starts_with`, `ends_with`, `find`, `substring`, `char_at`, `replace`, `to_upper`, `to_lower`, `trim`, `trim_start`, `trim_end`, `repeat`, `split`, `join` |
| `core/convert.ry` | `to_int`, `to_float`, `to_str` |
| `core/list.ry` | `append`, `pop`, `insert`, `remove_at`, `slice`, `distinct`, `flatten`, `sort`, `first`, `last`, `is_empty` |
| `core/map.ry` | `keys`, `values`, `items`, `has_key`, `get`, `merge` |
| `core/set.ry` | `add`, `remove`, `union`, `intersection`, `difference`, `symmetric_difference`, `is_subset`, `is_superset` |
| `core/higher_order.ry` | `filter`, `map`, `reduce`, `fold`, `any`, `all`, `sum`, `min`, `max` |

當 `ry` 執行檔附近存在 `core/` 目錄時，這些檔案會作為前導自動載入。前導機制使得內建函式呼叫時的參數數量驗證生效。

**限制事項:**
- `@native` 函式不能有本體（簽名後不能加 `:`）。
- 加上本體會導致解析錯誤: `@native function must not have a body`。
- 宣告的函式必須對應到現有的內建函式，否則在編譯時會發生錯誤。

**未來擴充方向:**
- `@native("libfoo.so")` — 綁定外部共享函式庫的 FFI。

### 參數（未來擴充）

指令支援可選的參數語法，為未來擴充做準備:

```
@deprecated(reason="use new_api instead")
fn old_api() -> int:
    return 0
```

目前，參數會被解析但不會被 `@deprecated` 指令使用。

## 注意事項

- 已棄用的實體仍然正常運作，僅會發出警告。
- 警告在使用點發出，不在定義點發出。
- 定義已棄用的實體但不使用它，不會產生警告。
- 未知的指令名稱會導致解析錯誤。
- 在不支援的目標（如 `if`、`while`）上使用指令會導致解析錯誤。
