[English](../../tutorial/09-modules.md) | [日本語](../../ja/tutorial/09-modules.md) | [繁體中文](09-modules.md)

# 模組

[← 前一篇：進階功能](08-advanced.md)

使用模組可以將程式碼分割到多個檔案中進行管理。

---

## from/import 語法

使用 `from` 語法匯入其他檔案的函式。

```python
from math import add, sub
```

這樣就可以使用 `math.ry` 中定義的 `add` 和 `sub`。

---

## 選擇性匯入 vs 全部匯入

### 選擇性匯入

僅明確匯入需要的函式。

```python
from math import add, sub
```

### 全部匯入

僅指定模組名稱時，會匯入該模組的所有函式。

```python
from math
```

---

## 子目錄（點分隔）

可以使用點分隔來指定子目錄中的模組。

```python
from utils.math import add   # 匯入 utils/math.ry
```

每個點對應一層目錄分隔。

---

## 搜尋路徑的優先順序

模組檔案按以下順序搜尋：

1. **匯入來源檔案的目錄** — 首先搜尋撰寫匯入的檔案所在的目錄。
2. **`RY_PATH` 環境變數** — 找不到時，按順序搜尋 `RY_PATH` 中指定的目錄。

---

## RY_PATH 環境變數

可以使用冒號分隔指定多個目錄。

```python
# 在 shell 中設定的範例
export RY_PATH=/home/user/ry-libs:/usr/local/ry-libs
```

設定後，可以從任何地方匯入指定目錄中的模組。

---

## 限制事項

- `from` 陳述式只能寫在檔案的**最上層**，不能寫在函式或區塊內部。
- 多次匯入同一模組時，會自動跳過（不會發生重複匯入）。
- **循環匯入**（A 匯入 B，B 匯入 A）會產生錯誤。

```python
# 錯誤範例：a.ry 和 b.ry 互相匯入的情況
# a.ry: from b import foo
# b.ry: from a import bar  ← 循環匯入錯誤
```

---

[← 前一篇：進階功能](08-advanced.md)
