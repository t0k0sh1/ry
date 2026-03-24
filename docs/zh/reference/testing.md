[English](../../reference/testing.md) | [日本語](../../ja/reference/testing.md) | [繁體中文](testing.md)

# 測試功能

Ry 內建 RSpec 風格的測試語法。使用 `ry test` 子指令執行測試檔案。

---

## 執行方式

```bash
ry test              # 自動探索並執行專案內所有 *.test.ry 檔案
ry test tests/spec   # 遞迴執行指定目錄下所有 *.test.ry 檔案
ry test test_file.ry # 執行指定的測試檔案
ry test -p           # 並行執行所有測試（-p 或 --parallel）
ry test -p tests/    # 並行執行指定目錄的測試
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
describe("說明文字", fn():
    it("測試案例名稱", fn():
        # 測試主體
        expect(實際值).to_eq(預期值)
    )
)
```

- `describe` 和 `it` 接受描述字串和**lambda 引數** `fn():` 作為第二個參數
- `describe` 區塊內可以撰寫 `it` 區塊及其他語句（如變數宣告等）
- 各 `it` 區塊為獨立的測試案例
- `describe` / `expect` 僅能在 `ry test` 中使用（在一般的 `ry` 執行中會產生編譯錯誤）

### 尾隨區塊語法

任何函式呼叫（`describe`/`it`/`mock` 除外）都可以使用尾隨區塊語法。在 `()` 後加上 `:` 會將縮排區塊作為無參數 lambda 傳入最後的參數位置:

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
| `to_contain(val)` | 容器包含值 | List, Set, Map, str |
| `to_not_contain(val)` | 容器不包含值 | List, Set, Map, str |
| `to_be_greater_than(v)` | `actual > v` | int, float |
| `to_be_less_than(v)` | `actual < v` | int, float |
| `to_be_greater_than_or_eq(v)` | `actual >= v` | int, float |
| `to_be_less_than_or_eq(v)` | `actual <= v` | int, float |
| `to_have_length(n)` | 長度為 `n` | List, Set, Map, str |
| `to_be_empty()` | 長度為 0 | List, Set, Map, str |
| `to_start_with(prefix)` | 字串以 prefix 開頭 | str |
| `to_end_with(suffix)` | 字串以 suffix 結尾 | str |

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
describe("Arithmetic", fn():
    it("adds integers", fn():
        expect(1 + 2).to_eq(3)

    )
    it("compares strings", fn():
        expect("hello").to_eq("hello")

    )
    it("checks booleans", fn():
        expect(3 > 1).to_be_true()

    )
)
describe("Booleans", fn():
    it("false check", fn():
        expect(1 > 2).to_be_false()
    )
)
```

---

## 模擬（Mock）

### mock(fn_name, replacement)

在當前 `it` 區塊中將函式替換為模擬實作。`it` 區塊結束時模擬會自動清除。

```
fn fetch_data() -> str:
    return "real data"

describe("mocking", fn():
    it("replaces function", fn():
        mock(fetch_data, fn(): "fake")
        expect(fetch_data()).to_eq("fake")

    )
    it("auto-restores", fn():
        expect(fetch_data()).to_eq("real data")
    )
)
```

- 第一個參數為函式名稱（識別符，非字串）
- 第二個參數為替換用 lambda
- 替換函式必須與原始函式具有相同的參數型別和回傳型別
- `it` 區塊結束時模擬會自動還原

### verify(fn_name)

回傳模擬函式被呼叫的次數（`int`）。

```
describe("verify", fn():
    it("counts calls", fn():
        mock(fetch_data, fn(): "fake")
        fetch_data()
        fetch_data()
        expect(verify(fetch_data)).to_eq(2)
    )
)
```

### 模擬的限制事項

- 不支援多載函式的模擬
- 不支援使用捕獲閉包進行模擬（僅支援純 lambda）
- 不支援 `@native fn` 的模擬

---

## 參數化測試 (@each)

`@each` 可以用多組參數執行同一個測試。將元組列表附加到 `it` 區塊:

```
@each([
    (1, 2, 3),
    (0, 0, 0),
    (-1, 1, 0)
])
it("adds {0} + {1} = {2}", fn(a: int, b: int, expected: int):
    expect(a + b).to_eq(expected)
)
```

- 列表必須包含與 lambda 參數數量相同的元組
- 描述中的 `{0}`, `{1}`, ... 會被替換為參數值
- 每個元組生成一個獨立的測試案例
- 支援的參數型別: `int`, `float`, `bool`, `str`

---

## 基於屬性的測試 (@property)

`@property` 生成隨機輸入並多次執行測試:

```
@property(count=100)
it("addition is commutative", fn(a: int, b: int):
    expect(a + b).to_eq(b + a)
)
```

- `count=N` 指定隨機試驗次數（預設: 100）
- 失敗時會顯示反例（導致失敗的輸入值）
- 在第一次失敗時停止測試
- 支援的參數型別: `int` ([-1000, 1000])、`float` ([-1000.0, 1000.0])、`bool`、`str` (隨機 ASCII、0-20 字元)

---

## 限制事項

- 不支援 `describe` 的巢狀
- 不支援 `before_each` / `after_each`
