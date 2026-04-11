[English](../../reference/json.md) | [日本語](../../ja/reference/json.md) | [繁體中文](json.md)

# JSON 函数参考

JSON 解析与序列化。所有函数需要从 `json` 明确导入。

```python
from json import parse, stringify, kind, get, at, to_str, to_int, to_float, to_bool, length, keys, json_free
```

## 概述

`json` 包提供将 JSON 文本解析为不透明的 `JsonValue` 类型、通过访问函数读取其内容、以及重新序列化为文本的功能。由于 JSON 值可以是异构的（对象可以包含字符串、数字、布尔值、数组和嵌套对象），本包使用不透明指针类型和类型化的访问函数。

## 函数列表

### 解析 / 序列化

| 函数 | 签名 | 说明 |
|------|------|------|
| `parse` | `(str) -> Result<JsonValue, Error>` | 将 JSON 字符串解析为 JsonValue |
| `stringify` | `(JsonValue) -> str` | 将 JsonValue 序列化为紧凑的 JSON 文本 |
| `stringify` | `(JsonValue, int) -> str` | 以缩进格式输出（参数为空格数） |

### 类型查询

| 函数 | 签名 | 说明 |
|------|------|------|
| `kind` | `(JsonValue) -> str` | 返回 JSON 类型: `"object"`, `"array"`, `"string"`, `"number"`, `"boolean"`, `"null"` |

### 对象 / 数组访问

| 函数 | 签名 | 说明 |
|------|------|------|
| `get` | `(JsonValue, str) -> Result<JsonValue, Error>` | 以键从对象获取字段 |
| `at` | `(JsonValue, int) -> Result<JsonValue, Error>` | 以索引从数组获取元素 |

### 值提取

| 函数 | 签名 | 说明 |
|------|------|------|
| `to_str` | `(JsonValue) -> Result<str, Error>` | 提取字符串值 |
| `to_int` | `(JsonValue) -> Result<int, Error>` | 提取整数值 |
| `to_float` | `(JsonValue) -> Result<float, Error>` | 提取浮点数值 |
| `to_bool` | `(JsonValue) -> Result<bool, Error>` | 提取布尔值 |

### 集合信息

| 函数 | 签名 | 说明 |
|------|------|------|
| `length` | `(JsonValue) -> int` | 返回数组长度或对象键数 |
| `keys` | `(JsonValue) -> Result<List<str>, Error>` | 返回对象的键列表，若值不是对象则返回错误 |

### 内存管理

| 函数 | 签名 | 说明 |
|------|------|------|
| `json_free` | `(JsonValue) -> Unit` | 释放 JsonValue 及其所有子元素 |

## 解開 `Result<JsonValue, Error>`

`parse`、`get` 和 `at` 返回 `Result<JsonValue, Error>`。在將內部值傳遞給其他 json 函數之前，您必須先解開 `Result` — 直接傳遞 `Result` 在編譯期會被拒絕：

```python
case parse(text):
  Ok(doc):
    # ✗ 錯誤：kind() 需要 JsonValue 參數
    # kind(get(doc, "name"))
    # ✓ 先解開
    case get(doc, "name"):
      Ok(name_val):
        print(kind(name_val))
      Err(e):
        print("no name")
  Err(e):
    print("parse error")
```

對 `Result` 的通用字串化（`to_str(result)`、`print(result)`、f-string 內插）仍然可用，並會格式化為 `Ok(...)` / `Err(...)`，與其他任何 `Result` 值的行為一致。

## 使用示例

### 解析与访问字段

```python
from json import parse, get, to_str, to_int, json_free

case parse("{\"name\": \"Alice\", \"age\": 30}"):
  Ok(data):
    case get(data, "name"):
      Ok(val):
        case to_str(val):
          Ok(name):
            print(name)   # "Alice"
          Err(e):
            print("error")
      Err(e):
        print("error")
    json_free(data)
  Err(e):
    print("parse error: " + e.message)
```

### 处理数组

```python
from json import parse, at, to_int, length, json_free

case parse("[10, 20, 30]"):
  Ok(data):
    print(to_str(length(data)))   # 3
    case at(data, 0):
      Ok(elem):
        case to_int(elem):
          Ok(n):
            print(to_str(n))   # 10
          Err(e):
            print("error")
      Err(e):
        print("error")
    json_free(data)
  Err(e):
    print("parse error")
```

### 带缩进的序列化

```python
from json import parse, stringify, json_free

case parse("{\"key\":\"value\",\"count\":42}"):
  Ok(data):
    print(stringify(data, 2))
    # {
    #   "key": "value",
    #   "count": 42
    # }
    json_free(data)
  Err(e):
    print("error")
```

## 注意事项

- `to_int` 接受整数和整数值的浮点数（例如 `42.0` -> `42`）
- `to_float` 接受浮点数和整数（例如 `42` -> `42.0`）
- `get` 和 `at` 返回解析树中子元素的引用——不要对子元素调用 `json_free`，只对 `parse` 返回的根值调用
- `kind` 对整数和浮点数都返回 `"number"`
