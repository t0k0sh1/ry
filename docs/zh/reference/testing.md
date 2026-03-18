[English](../../reference/testing.md) | [日本語](../../ja/reference/testing.md) | [繁體中文](testing.md)

# 測試功能

Ry 內建 RSpec 風格的測試語法。使用 `ry test` 子指令執行測試檔案。

---

## 執行方式

```bash
ry test              # 自動探索並執行專案內所有 *.test.ry 檔案
ry test test_file.ry # 執行指定的測試檔案
```

結束代碼為 0 表示所有測試通過，1 表示有測試失敗。

### 自動探索模式

不帶引數執行 `ry test` 時：

1. 搜尋 `ry.toml` 以找到專案根目錄
2. 在專案根目錄下遞迴探索所有 `*.test.ry` 檔案（`.git`、`build`、`node_modules` 會被跳過）
3. 逐一執行並彙總結果

---

## 語法

### describe / it

```
describe("說明文字"):
    it("測試案例名稱"):
        # 測試主體
        expect(實際值).to_eq(預期值)
```

- `describe` 和 `it` 使用**尾隨區塊語法**: 函式呼叫後加上 `:` 會將縮排區塊作為 lambda 傳入最後的參數
- `describe` 區塊內可以撰寫 `it` 區塊及其他語句（如變數宣告等）
- 各 `it` 區塊為獨立的測試案例
- `describe` / `expect` 僅能在 `ry test` 中使用（在一般的 `ry` 執行中會產生編譯錯誤）

### 尾隨區塊語法

任何函式呼叫都可以使用尾隨區塊語法。在 `()` 後加上 `:` 會將縮排區塊作為無參數 lambda 傳入最後的參數位置:

```
# 以下兩者等價:
foo("arg"):
    bar()

foo("arg", fn():
    bar()
)
```

### expect / 匹配器

| 匹配器 | 說明 | 支援型別 |
|---|---|---|
| `to_eq(expected)` | 相等比較 | int, float, bool, str |
| `to_not_eq(expected)` | 不相等 | int, float, bool, str |
| `to_be_true()` | 為 `true` | bool |
| `to_be_false()` | 為 `false` | bool |
| `to_be_none()` | 為 `None` | Option |
| `to_be_some()` | Option 為 `Some` | Option |
| `to_contain(val)` | 容器包含值 | List, Set, str |

---

## 輸出格式

```
Calculator
  + adds numbers
  + subtracts
  - fails test （紅色）
    line 10: expected 3, got 2

2 passed, 1 failed
```

- `+` 為成功（綠色），`-` 為失敗（紅色）
- 失敗時顯示行號與預期值/實際值

---

## 範例

```
describe("Arithmetic"):
    it("adds integers"):
        expect(1 + 2).to_eq(3)

    it("compares strings"):
        expect("hello").to_eq("hello")

    it("checks booleans"):
        expect(3 > 1).to_be_true()

describe("Booleans"):
    it("false check"):
        expect(1 > 2).to_be_false()
```

---

## 限制事項

- 不支援 `describe` 的巢狀
- 不支援 `before_each` / `after_each`
