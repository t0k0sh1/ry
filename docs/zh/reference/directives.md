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
- `type` - 型別定義
- `let` / `var` - 變數宣告
- `type` 定義內的欄位

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
type OldPoint:
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
type Config:
    @deprecated
    old_setting: int
    new_setting: int

let c = Config(1, 2)
print(c.old_setting)     # warning: 'Config.old_setting' is deprecated
print(c.new_setting)     # 無警告
```

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
