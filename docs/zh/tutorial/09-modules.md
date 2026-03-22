[English](../../tutorial/09-modules.md) | [日本語](../../ja/tutorial/09-modules.md) | [繁體中文](09-modules.md)

# 套件

[← 前一篇：進階功能](08-advanced.md) | [下一篇：契約式設計 →](10-contracts.md)

Ry 使用套件系統來管理跨檔案和目錄的程式碼。詳細規格請參閱[套件參考手冊](../reference/packages.md)。

---

## from/import 語法

使用 `from` 語法匯入其他檔案的函式。

```python
from math import add, sub   # 選擇性匯入
from math                    # 全部匯入
```

這樣就可以使用 `math.ry` 中定義的函式。

---

## 子目錄（點分隔）

可以使用點分隔來指定子目錄中的套件。

```python
from utils.math import add   # 匯入 utils/math.ry
```

每個點對應一層目錄分隔。

---

## 目錄套件

套件可以是單一的 `.ry` 檔案，也可以是包含多個 `.ry` 檔案的目錄。當套件解析為目錄時，其中所有的 `.ry` 檔案會自動載入。

```
mypackage/
  math.ry      # fn add(), fn sub()
  string.ry    # fn concat()
```

```python
from mypackage              # 匯入 add、sub、concat
from mypackage import add   # 僅匯入 add
```

不需要特殊的入口檔案（如 `__init__.py`）。以 `_` 開頭的檔案會被排除。

---

## 標準函式庫（`std`）

`std` 套件會自動匯入到所有程式中。不需要撰寫 `from std`。

```python
# 這些函式無需匯入即可使用
print("hello")
@const
n = len("world")
@const
xs = range(5)
```

也可以從 `std` 子套件中明確匯入特定定義：

```python
from std.str import contains
```

### RY_HOME

標準函式庫安裝在 `$RY_HOME/lib/std/`。`RY_HOME` 的預設值為 `~/.ry`。

```bash
export RY_HOME="$HOME/.ry"   # 預設
```

---

## 搜尋路徑的優先順序

套件檔案按以下順序搜尋：

1. **匯入來源檔案的目錄** — 首先搜尋撰寫匯入的檔案所在的目錄。
2. **`$RY_HOME/lib`** — 標準函式庫的位置。
3. **執行檔相對的 `lib/`** — 相對於 `ry` 執行檔的目錄。
4. **`RY_PATH` 環境變數** — 找不到時，按順序搜尋 `RY_PATH` 中指定的目錄。

---

## RY_PATH 環境變數

可以使用冒號分隔指定多個目錄。

```bash
export RY_PATH=/home/user/ry-libs:/usr/local/ry-libs
```

設定後，可以從任何地方匯入指定目錄中的套件。

---

## 限制事項

- `from` 陳述式只能寫在檔案的**最上層**，不能寫在函式或區塊內部。
- 多次匯入同一套件時，會自動跳過（不會發生重複匯入）。
- **循環匯入**（A 匯入 B，B 匯入 A）會產生錯誤。

```python
# 錯誤範例：a.ry 和 b.ry 互相匯入的情況
# a.ry: from b import foo
# b.ry: from a import bar  ← 循環匯入錯誤
```

---

[← 前一篇：進階功能](08-advanced.md) | [下一篇：契約式設計 →](10-contracts.md)
