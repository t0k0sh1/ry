[English](../../reference/testing.md) | [日本語](../../ja/reference/testing.md) | [繁體中文](testing.md)

# 測試功能

Ry 內建 RSpec 風格的測試語法。使用 `ry test` 子指令執行測試檔案。

---

## 執行方式

```bash
ry test test_file.ry
```

測試的結束代碼為失敗的測試數量（0 = 全部通過）。

---

## 語法

### describe / it

```
describe "說明文字":
    it "測試案例名稱":
        # 測試主體
        expect(實際值).to_eq(預期值)
```

- `describe` 區塊內只能撰寫 `it` 區塊
- 各 `it` 區塊為獨立的測試案例
- `describe` / `expect` 僅能在 `ry test` 中使用（在一般的 `ry` 執行中會產生編譯錯誤）

### expect / 匹配器

| 匹配器 | 說明 | 支援型別 |
|---|---|---|
| `to_eq(expected)` | 相等比較 | int, float, bool, str |
| `to_be_true()` | 為 `true` | bool |
| `to_be_false()` | 為 `false` | bool |
| `to_be_none()` | 為 `None` | Option |

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
describe "Arithmetic":
    it "adds integers":
        expect(1 + 2).to_eq(3)

    it "compares strings":
        expect("hello").to_eq("hello")

    it "checks booleans":
        expect(3 > 1).to_be_true()

describe "Booleans":
    it "false check":
        expect(1 > 2).to_be_false()
```

---

## 限制事項

- 不支援 `describe` 的巢狀
- 不支援 `before_each` / `after_each`
- 不支援以 glob 執行測試檔案（`ry test tests/`）
