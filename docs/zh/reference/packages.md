[English](../../reference/packages.md) | [日本語](../../ja/reference/packages.md) | [繁體中文](packages.md)

# 套件參考

## 概述

Ry 使用套件系統來組織程式碼。**套件**可以是單一的 `.ry` 檔案，或是包含多個 `.ry` 檔案的目錄。使用 `from` 陳述式匯入套件。

`std` 套件（標準函式庫）會自動匯入到每個程式中。

---

## 匯入語法

### 匯入全部定義

```python
from math
```

匯入套件內的所有函式與型別。

### 選擇性匯入

```python
from math import add
```

僅匯入指定的定義。

### 多重選擇性匯入

```python
from math import add, sub
```

以逗號分隔選擇性匯入多個定義。

---

## 套件解析

以點號分隔的套件名稱按以下方式解析：

| 匯入陳述式 | 解析結果 |
|---|---|
| `from math` | `math/` 目錄（套件）或 `math.ry` 檔案 |
| `from utils.math` | `utils/math/` 目錄或 `utils/math.ry` 檔案 |
| `from std.str` | `std/str/` 目錄或 `std/str.ry` 檔案 |

### 解析順序

對於每個搜尋路徑：
1. **目錄** (`{path}/`) — 若存在，載入目錄內的所有 `.ry` 檔案（套件）
2. **檔案** (`{path}.ry`) — 單一檔案（向後相容）

### 目錄套件

當套件解析為目錄時：
- 目錄內的所有 `.ry` 檔案會自動載入
- 以 `_` 開頭的檔案會被排除
- 不需要特殊的入口檔案（如 `__init__.py`）
- 目錄內檔案中定義的所有函式與型別都會被匯出

```
mypackage/
  math.ry      # fn add(), fn sub()
  string.ry    # fn concat()
```

```python
from mypackage          # 匯入 add, sub, concat
from mypackage import add   # 僅匯入 add
```

---

## 標準函式庫 (`std`)

`std` 套件會自動匯入到每個程式中。提供的功能：
- 內建函式（`print`, `len`, `range` 等）
- 字串函式（`contains`, `find`, `replace` 等）
- 型別轉換函式（`to_int`, `to_float`, `to_str`）
- 集合函式（`map`, `filter`, `sort` 等）

也可以明確匯入特定的定義：

```python
from std.str import contains
```

### RY_HOME

標準函式庫安裝於 `$RY_HOME/lib/std/`。`RY_HOME` 的預設值為 `~/.ry`。

```bash
export RY_HOME="$HOME/.ry"   # 預設值
```

---

## 搜尋路徑的優先順序

1. 匯入來源檔案所在的目錄
2. `$RY_HOME/lib`（標準函式庫位置）
3. 執行檔相對的 `lib/` 目錄
4. `RY_PATH` 環境變數中包含的路徑（以冒號分隔）

---

## RY_PATH 環境變數

在 `RY_PATH` 中以冒號分隔指定目錄，即可新增至套件搜尋路徑。

```bash
export RY_PATH="/usr/local/ry/lib:/home/user/ry-packages"
```

---

## 限制

| 限制 | 詳細 |
|------|------|
| 可使用的位置 | 僅限頂層（函式或區塊內不可） |
| 重複匯入 | 自動跳過（不會產生錯誤） |
| 循環匯入 | 編譯錯誤 |

```python
# 錯誤範例：在區塊內匯入
fn main():
    from math   # 錯誤：僅能在頂層匯入

# OK：多次匯入相同套件不會產生錯誤
from math
from math   # 被跳過
```

---

## 建立套件檔案

### 單一檔案套件

```python
# math.ry
fn add(a: int, b: int) -> int:
    return a + b

fn sub(a: int, b: int) -> int:
    return a - b
```

```python
# main.ry
from math import add, sub

print(add(1, 2))   # 3
print(sub(5, 3))   # 2
```

### 目錄套件

```
mylib/
  math.ry
  string.ry
```

```python
# main.ry
from mylib import add, concat
```
