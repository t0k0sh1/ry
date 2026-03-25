[English](json.md) | [日本語](../ja/reference/json.md) | [繁體中文](../zh/reference/json.md)

# JSON Function Reference

JSON parsing and serialization. All functions require explicit import from `json`.

```python
from json import parse, stringify, json_type, json_get, json_at, json_str, json_int, json_float, json_bool, json_len, json_keys, json_free
```

## Overview

The `json` package provides functions to parse JSON text into an opaque `JsonValue` type, access its contents via accessor functions, and serialize it back to text. Since JSON values can be heterogeneous (objects can contain strings, numbers, booleans, arrays, and nested objects), the package uses an opaque pointer type with typed accessor functions.

## Function List

### Parse / Stringify

| Function | Signature | Description |
|----------|-----------|-------------|
| `parse` | `(str) -> Result<JsonValue, Error>` | Parses a JSON string into a JsonValue |
| `stringify` | `(JsonValue) -> str` | Serializes a JsonValue to compact JSON text |
| `stringify` | `(JsonValue, int) -> str` | Serializes with pretty printing (indent = number of spaces) |

### Type Query

| Function | Signature | Description |
|----------|-----------|-------------|
| `json_type` | `(JsonValue) -> str` | Returns the JSON type: `"object"`, `"array"`, `"string"`, `"number"`, `"boolean"`, or `"null"` |

### Object / Array Access

| Function | Signature | Description |
|----------|-----------|-------------|
| `json_get` | `(JsonValue, str) -> Result<JsonValue, Error>` | Gets a field from an object by key |
| `json_at` | `(JsonValue, int) -> Result<JsonValue, Error>` | Gets an element from an array by index |

### Value Extraction

| Function | Signature | Description |
|----------|-----------|-------------|
| `json_str` | `(JsonValue) -> Result<str, Error>` | Extracts a string value |
| `json_int` | `(JsonValue) -> Result<int, Error>` | Extracts an integer value |
| `json_float` | `(JsonValue) -> Result<float, Error>` | Extracts a float value |
| `json_bool` | `(JsonValue) -> Result<bool, Error>` | Extracts a boolean value |

### Collection Info

| Function | Signature | Description |
|----------|-----------|-------------|
| `json_len` | `(JsonValue) -> int` | Returns the length of an array or number of keys in an object |
| `json_keys` | `(JsonValue) -> List<str>` | Returns the keys of an object |

### Memory Management

| Function | Signature | Description |
|----------|-----------|-------------|
| `json_free` | `(JsonValue) -> Unit` | Frees a JsonValue and all its children |

## Usage Examples

### Parsing and accessing fields

```python
from json import parse, json_get, json_str, json_int, json_free

match parse("{\"name\": \"Alice\", \"age\": 30}"):
  case Ok(data):
    match json_get(data, "name"):
      case Ok(val):
        match json_str(val):
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

### Working with arrays

```python
from json import parse, json_at, json_int, json_len, json_free

match parse("[10, 20, 30]"):
  case Ok(data):
    print(to_str(json_len(data)))   # 3
    match json_at(data, 0):
      case Ok(elem):
        match json_int(elem):
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

### Stringify with pretty printing

```python
from json import parse, stringify, json_free

match parse("{\"key\":\"value\",\"count\":42}"):
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

## Notes

- `json_int` accepts both JSON integers and floats that are whole numbers (e.g., `42.0` → `42`)
- `json_float` accepts both JSON floats and integers (e.g., `42` → `42.0`)
- `json_get` and `json_at` return references to child values within the parsed tree — do not call `json_free` on child values, only on the root value returned by `parse`
- The `json_type` function returns `"number"` for both integers and floats
