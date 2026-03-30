[English](../../reference/json.md) | [日本語](json.md) | [繁體中文](../../zh/reference/json.md)

# JSON 関数リファレンス

JSON のパースとシリアライズを行う関数一覧です。すべての関数は `json` からの明示的なインポートが必要です。

```python
from json import parse, stringify, json_type, json_get, json_at, json_str, json_int, json_float, json_bool, json_len, json_keys, json_free
```

## 概要

`json` パッケージは、JSON テキストをオペーク（不透明）な `JsonValue` 型にパースし、アクセサ関数で内容にアクセスし、テキストに再シリアライズする機能を提供します。JSON の値は異種型を含みうるため（オブジェクトには文字列、数値、真偽値、配列、ネストされたオブジェクトが含まれる）、オペークポインタ型と型付きアクセサ関数を使用します。

## 関数一覧

### パース / シリアライズ

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `parse` | `(str) -> Result<JsonValue, Error>` | JSON 文字列を JsonValue にパース |
| `stringify` | `(JsonValue) -> str` | JsonValue をコンパクトな JSON テキストに変換 |
| `stringify` | `(JsonValue, int) -> str` | インデント付きで整形出力（引数はスペース数） |

### 型クエリ

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `json_type` | `(JsonValue) -> str` | JSON の型を返す: `"object"`, `"array"`, `"string"`, `"number"`, `"boolean"`, `"null"` |

### オブジェクト / 配列アクセス

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `json_get` | `(JsonValue, str) -> Result<JsonValue, Error>` | オブジェクトからキーでフィールドを取得 |
| `json_at` | `(JsonValue, int) -> Result<JsonValue, Error>` | 配列からインデックスで要素を取得 |

### 値の取り出し

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `json_str` | `(JsonValue) -> Result<str, Error>` | 文字列値を取り出す |
| `json_int` | `(JsonValue) -> Result<int, Error>` | 整数値を取り出す |
| `json_float` | `(JsonValue) -> Result<float, Error>` | 浮動小数点値を取り出す |
| `json_bool` | `(JsonValue) -> Result<bool, Error>` | 真偽値を取り出す |

### コレクション情報

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `json_len` | `(JsonValue) -> int` | 配列の長さまたはオブジェクトのキー数を返す |
| `json_keys` | `(JsonValue) -> List<str>` | オブジェクトのキー一覧を返す |

### メモリ管理

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `json_free` | `(JsonValue) -> Unit` | JsonValue とその子要素をすべて解放 |

## 使用例

### フィールドのパースとアクセス

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

### 配列の操作

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

### 整形出力付きシリアライズ

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

## 注意事項

- `json_int` は整数と整数値の浮動小数点数（例: `42.0` → `42`）の両方を受け付けます
- `json_float` は浮動小数点数と整数（例: `42` → `42.0`）の両方を受け付けます
- `json_get` と `json_at` はパースツリー内の子要素への参照を返します。子要素に対して `json_free` を呼ばず、`parse` で返されたルート値に対してのみ呼んでください
- `json_type` は整数と浮動小数点数の両方に対して `"number"` を返します
