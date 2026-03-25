[English](../../reference/json.md) | [日本語](../../ja/reference/json.md) | [繁體中文](json.md)

# JSON 函式參考

JSON 解析與序列化函式。所有函式需要從 `json` 明確匯入。

```python
from json import parse, stringify, json_type, json_get, json_at, json_str, json_int, json_float, json_bool, json_len, json_keys, json_free
```

## 概述

`json` 套件提供將 JSON 文字解析為不透明的 `JsonValue` 型別、透過存取函式讀取其內容、以及重新序列化為文字的功能。由於 JSON 值具有動態型別，本套件使用型別化的存取函式。

## 函式列表

### 解析 / 序列化

| 函式 | 簽名 | 說明 |
|------|------|------|
| `parse` | `(str) -> Result<JsonValue, Error>` | 將 JSON 字串解析為 JsonValue |
| `stringify` | `(JsonValue) -> str` | 將 JsonValue 序列化為緊湊的 JSON 文字 |
| `stringify` | `(JsonValue, int) -> str` | 以縮排格式輸出（參數為空格數） |

### 型別查詢

| 函式 | 簽名 | 說明 |
|------|------|------|
| `json_type` | `(JsonValue) -> str` | 回傳 JSON 型別: `"object"`, `"array"`, `"string"`, `"number"`, `"boolean"`, `"null"` |

### 物件 / 陣列存取

| 函式 | 簽名 | 說明 |
|------|------|------|
| `json_get` | `(JsonValue, str) -> Result<JsonValue, Error>` | 以鍵值從物件取得欄位 |
| `json_at` | `(JsonValue, int) -> Result<JsonValue, Error>` | 以索引從陣列取得元素 |

### 值提取

| 函式 | 簽名 | 說明 |
|------|------|------|
| `json_str` | `(JsonValue) -> Result<str, Error>` | 提取字串值 |
| `json_int` | `(JsonValue) -> Result<int, Error>` | 提取整數值 |
| `json_float` | `(JsonValue) -> Result<float, Error>` | 提取浮點數值 |
| `json_bool` | `(JsonValue) -> Result<bool, Error>` | 提取布林值 |

### 集合資訊

| 函式 | 簽名 | 說明 |
|------|------|------|
| `json_len` | `(JsonValue) -> int` | 回傳陣列長度或物件鍵數 |
| `json_keys` | `(JsonValue) -> List<str>` | 回傳物件的鍵列表 |

### 記憶體管理

| 函式 | 簽名 | 說明 |
|------|------|------|
| `json_free` | `(JsonValue) -> Unit` | 釋放 JsonValue 及其所有子元素 |

## 注意事項

- `json_int` 接受整數和整數值的浮點數（例如 `42.0` → `42`）
- `json_float` 接受浮點數和整數（例如 `42` → `42.0`）
- `json_get` 和 `json_at` 回傳解析樹中子元素的參照。不要對子元素呼叫 `json_free`，只對 `parse` 回傳的根值呼叫
- `json_type` 對整數和浮點數都回傳 `"number"`
