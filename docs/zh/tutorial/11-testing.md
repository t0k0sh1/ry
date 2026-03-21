[English](../../tutorial/11-testing.md) | [日本語](../../ja/tutorial/11-testing.md) | [繁體中文](11-testing.md)

# 測試

[← 前一篇：契約式設計](10-contracts.md)

Ry 內建了使用 `describe`、`it`、`expect` 的 RSpec 風格測試語法。詳細規格請參閱[測試參考手冊](../reference/testing.md)。

---

## 執行測試

```bash
ry test                       # 自動探索並執行所有 *.test.ry 檔案
ry test tests/my_test.test.ry # 執行特定的測試檔案
```

所有測試通過時，結束碼為 `0`；若有任一測試失敗，則為 `1`。

不帶參數執行時，`ry test` 會搜尋 `ry.toml` 來找到專案根目錄，然後遞迴地探索所有 `*.test.ry` 檔案。

---

## 撰寫測試

使用 `describe` 將相關測試分組，使用 `it` 定義各個測試案例。

```python
describe("Calculator", fn():
    it("adds integers", fn():
        expect(1 + 2).to_eq(3)

    )
    it("subtracts integers", fn():
        expect(5 - 3).to_eq(2)

    )
    it("checks booleans", fn():
        expect(3 > 1).to_be_true()
    )
)
```

- `describe` 和 `it` 接受描述字串和**lambda 引數** `fn():` 作為第二個參數。
- `describe` / `it` / `expect` / `mock` / `verify` 僅可在 `ry test` 中使用（一般的 `ry` 執行會產生編譯錯誤）。

---

## 匹配器

| 匹配器 | 說明 | 支援型別 |
|--------|------|---------|
| `to_eq(expected)` | 等值比較 | int, float, bool, str |
| `to_not_eq(expected)` | 不等值斷言 | int, float, bool, str |
| `to_be_true()` | `true` 斷言 | bool |
| `to_be_false()` | `false` 斷言 | bool |
| `to_be_none()` | `None` 斷言 | Option |
| `to_be_some()` | Option 為 `Some` 的斷言 | Option |
| `to_contain(val)` | 容器包含值的斷言 | List, Set, str |

---

## 輸出格式

```
Calculator
  + adds integers
  + subtracts integers
  - checks booleans
    line 10: expected true, got false

2 passed, 1 failed
```

`+` 表示通過（綠色），`-` 表示失敗（紅色）。

---

## 模擬（Mock）

### `mock(fn_name, replacement)`

在目前的 `it` 區塊中將函式替換為模擬實作。`it` 區塊結束時會自動還原。

```python
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

### `verify(fn_name)`

傳回模擬函式被呼叫的次數。

```python
describe("verify", fn():
    it("counts calls", fn():
        mock(fetch_data, fn(): "fake")
        fetch_data()
        fetch_data()
        expect(verify(fetch_data)).to_eq(2)
    )
)
```

---

## 參數化測試

使用 `@each` 以多組輸入執行同一個測試:

```python
@each([(1, 2, 3), (0, 0, 0), (-1, 1, 0)])
it("adds {0} + {1} = {2}", fn(a: int, b: int, expected: int):
    expect(a + b).to_eq(expected)
)
```

每個元組成為一個獨立的測試案例。描述中的 `{0}`, `{1}` 會被替換為實際值。

---

## 基於屬性的測試

使用 `@property` 以隨機生成的輸入進行測試:

```python
@property(count=100)
it("addition is commutative", fn(a: int, b: int):
    expect(a + b).to_eq(b + a)
)
```

測試以隨機值執行 `count` 次。失敗時會顯示反例。

---

## 限制事項

- 不支援 `describe` 的巢狀使用
- 不支援 `before_each` / `after_each`
- 多載函式及 `@native` 函式無法模擬

---

[← 前一篇：契約式設計](10-contracts.md)
