[English](../../reference/modules.md) | [日本語](../../ja/reference/modules.md) | [繁體中文](modules.md)

# 模組參考

## 概述

Ry 以檔案為單位管理模組。使用 `from` 陳述式匯入模組。

---

## 匯入語法

### 匯入全部函式

```python
from math
```

匯入模組內的所有函式。

### 選擇性匯入

```python
from math import add
```

僅匯入指定的函式。

### 多重選擇性匯入

```python
from math import add, sub
```

以逗號分隔選擇性匯入多個函式。

---

## 子目錄的模組

以點號分隔指定子目錄。

| 匯入陳述式 | 對應的檔案路徑 |
|-------------|-------------------|
| `from math` | `math.ry` |
| `from utils.math` | `utils/math.ry` |
| `from a.b.c` | `a/b/c.ry` |

```python
from utils.math import add
from net.http import get
```

模組名稱不包含副檔名（`.ry`）。

---

## 搜尋路徑的優先順序

1. 匯入來源檔案所在的目錄
2. `RY_PATH` 環境變數中包含的路徑（以冒號分隔）

---

## RY_PATH 環境變數

在 `RY_PATH` 中以冒號分隔指定目錄，即可新增至模組搜尋路徑。

```bash
export RY_PATH="/usr/local/ry/lib:/home/user/ry-modules"
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

# OK：多次匯入相同模組不會產生錯誤
from math
from math   # 被跳過
```

---

## 建立模組檔案

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
