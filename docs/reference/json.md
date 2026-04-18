[English](json.md) | [日本語](../ja/reference/json.md) | [繁體中文](../zh/reference/json.md)

# JSON Function Reference

JSON parsing and serialization. All functions require explicit import from `json`.

```ry
from json import parse, stringify, kind, get, at, to_str, to_int, to_float, to_bool, length, keys, json_free
```

## Overview

The `json` package provides functions to parse JSON text into an opaque `JsonValue` type, access its contents via accessor functions, and serialize it back to text. Since JSON values can be heterogeneous (objects can contain strings, numbers, booleans, arrays, and nested objects), the package uses an opaque pointer type with typed accessor functions.

## Function List

### Parse / Stringify

| Function | Signature | Description |
|----------|-----------|-------------|
| `parse` | `(str) -> Result<JsonValue, Error>` | Parses a JSON string into a JsonValue |
| `stringify` | `(JsonValue) -> str` | Serializes a JsonValue to compact JSON text |
| `stringify` | `(JsonValue, int) -> str` | Serializes with pretty printing (indent = number of spaces). A negative indent falls back to compact output. |

### Type Query

| Function | Signature | Description |
|----------|-----------|-------------|
| `kind` | `(JsonValue) -> str` | Returns the JSON type: `"object"`, `"array"`, `"string"`, `"number"`, `"boolean"`, or `"null"` |

### Object / Array Access

| Function | Signature | Description |
|----------|-----------|-------------|
| `get` | `(JsonValue, str) -> Result<JsonValue, Error>` | Gets a field from an object by key |
| `at` | `(JsonValue, int) -> Result<JsonValue, Error>` | Gets an element from an array by index |

### Value Extraction

| Function | Signature | Description |
|----------|-----------|-------------|
| `to_str` | `(JsonValue) -> Result<str, Error>` | Extracts a string value |
| `to_int` | `(JsonValue) -> Result<int, Error>` | Extracts an integer value |
| `to_float` | `(JsonValue) -> Result<float, Error>` | Extracts a float value |
| `to_bool` | `(JsonValue) -> Result<bool, Error>` | Extracts a boolean value |

### Collection Info

| Function | Signature | Description |
|----------|-----------|-------------|
| `length` | `(JsonValue) -> int` | Returns the length of an array or number of keys in an object. Returns `0` for non-container values (string / number / boolean / null). |
| `keys` | `(JsonValue) -> Result<List<str>, Error>` | Returns the keys of an object, or an error if the value is not an object |

### Memory Management

| Function | Signature | Description |
|----------|-----------|-------------|
| `json_free` | `(JsonValue) -> Unit` | Frees a JsonValue and all its children |

## Unwrapping `Result<JsonValue, Error>`

`parse`, `get`, and `at` return `Result<JsonValue, Error>`. You must
unwrap the `Result` before passing the inner value to another json
function — passing the `Result` directly is rejected at compile time:

```ry
case parse(text):
  Ok(doc):
    # ✗ error: kind() requires a JsonValue argument
    # kind(get(doc, "name"))
    # ✓ unwrap first
    case get(doc, "name"):
      Ok(name_val):
        print(kind(name_val))
      Err(e):
        print("no name")
  Err(e):
    print("parse error")
```

Generic stringification (`to_str(result)`, `print(result)`, f-string
interpolation) still works on a `Result` and formats as `Ok(...)` /
`Err(...)`, matching the behavior for any other `Result` value.

## Usage Examples

### Parsing and accessing fields

```ry
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

### Working with arrays

```ry
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

### Stringify with pretty printing

```ry
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

## Notes

- `to_int` accepts both JSON integers and floats that are whole numbers (e.g., `42.0` → `42`)
- `to_float` accepts both JSON floats and integers (e.g., `42` → `42.0`)
- `get` and `at` return references to child values within the parsed tree — do not call `json_free` on child values, only on the root value returned by `parse`
- The `kind` function returns `"number"` for both integers and floats
- Embedded NUL bytes (`\u0000`) are fully supported per RFC 8259: `parse` accepts `\u0000` in string values and object keys; `stringify` emits `\u0000` for any NUL byte in a string; `to_str`, `get`, and `keys` preserve NUL bytes correctly
