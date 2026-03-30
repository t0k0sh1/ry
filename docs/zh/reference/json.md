[English](../../reference/json.md) | [日本語](../../ja/reference/json.md) | [繁體中文](json.md)

# JSON 函数参考

JSON 解析与序列化。所有函数需要从 `json` 明确导入。

```python
from json import parse, stringify, json_type, json_get, json_at, json_str, json_int, json_float, json_bool, json_len, json_keys, json_free
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
| `json_type` | `(JsonValue) -> str` | 返回 JSON 类型: `"object"`, `"array"`, `"string"`, `"number"`, `"boolean"`, `"null"` |

### 对象 / 数组访问

| 函数 | 签名 | 说明 |
|------|------|------|
| `json_get` | `(JsonValue, str) -> Result<JsonValue, Error>` | 以键从对象获取字段 |
| `json_at` | `(JsonValue, int) -> Result<JsonValue, Error>` | 以索引从数组获取元素 |

### 值提取

| 函数 | 签名 | 说明 |
|------|------|------|
| `json_str` | `(JsonValue) -> Result<str, Error>` | 提取字符串值 |
| `json_int` | `(JsonValue) -> Result<int, Error>` | 提取整数值 |
| `json_float` | `(JsonValue) -> Result<float, Error>` | 提取浮点数值 |
| `json_bool` | `(JsonValue) -> Result<bool, Error>` | 提取布尔值 |

### 集合信息

| 函数 | 签名 | 说明 |
|------|------|------|
| `json_len` | `(JsonValue) -> int` | 返回数组长度或对象键数 |
| `json_keys` | `(JsonValue) -> List<str>` | 返回对象的键列表 |

### 内存管理

| 函数 | 签名 | 说明 |
|------|------|------|
| `json_free` | `(JsonValue) -> Unit` | 释放 JsonValue 及其所有子元素 |

## 使用示例

### 解析与访问字段

```python
from json import parse, json_get, json_str, json_int, json_free

when parse("{\"name\": \"Alice\", \"age\": 30}"):
  case Ok(data):
    when json_get(data, "name"):
      case Ok(val):
        when json_str(val):
          case Ok(name):
            print(name)   # "Alice"
          case Err(e):
            print("error")
      case Err(e):
        print("error")
    json_free(data)
  case Err(e):
    print("parse error: " + e.message)
```

### 处理数组

```python
from json import parse, json_at, json_int, json_len, json_free

when parse("[10, 20, 30]"):
  case Ok(data):
    print(to_str(json_len(data)))   # 3
    when json_at(data, 0):
      case Ok(elem):
        when json_int(elem):
          case Ok(n):
            print(to_str(n))   # 10
          case Err(e):
            print("error")
      case Err(e):
        print("error")
    json_free(data)
  case Err(e):
    print("parse error")
```

### 带缩进的序列化

```python
from json import parse, stringify, json_free

when parse("{\"key\":\"value\",\"count\":42}"):
  case Ok(data):
    print(stringify(data, 2))
    # {
    #   "key": "value",
    #   "count": 42
    # }
    json_free(data)
  case Err(e):
    print("error")
```

## 注意事项

- `json_int` 接受整数和整数值的浮点数（例如 `42.0` → `42`）
- `json_float` 接受浮点数和整数（例如 `42` → `42.0`）
- `json_get` 和 `json_at` 返回解析树中子元素的引用——不要对子元素调用 `json_free`，只对 `parse` 返回的根值调用
- `json_type` 对整数和浮点数都返回 `"number"`
