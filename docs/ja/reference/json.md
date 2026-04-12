[English](../../reference/json.md) | [日本語](json.md) | [繁體中文](../../zh/reference/json.md)

# JSON 関数リファレンス

JSON のパースとシリアライズを行う関数一覧です。すべての関数は `json` からの明示的なインポートが必要です。

```python
from json import parse, stringify, kind, get, at, to_str, to_int, to_float, to_bool, length, keys, json_free
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
| `kind` | `(JsonValue) -> str` | JSON の型を返す: `"object"`, `"array"`, `"string"`, `"number"`, `"boolean"`, `"null"` |

### オブジェクト / 配列アクセス

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `get` | `(JsonValue, str) -> Result<JsonValue, Error>` | オブジェクトからキーでフィールドを取得 |
| `at` | `(JsonValue, int) -> Result<JsonValue, Error>` | 配列からインデックスで要素を取得 |

### 値の取り出し

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `to_str` | `(JsonValue) -> Result<str, Error>` | 文字列値を取り出す |
| `to_int` | `(JsonValue) -> Result<int, Error>` | 整数値を取り出す |
| `to_float` | `(JsonValue) -> Result<float, Error>` | 浮動小数点値を取り出す |
| `to_bool` | `(JsonValue) -> Result<bool, Error>` | 真偽値を取り出す |

### コレクション情報

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `length` | `(JsonValue) -> int` | 配列の長さまたはオブジェクトのキー数を返す |
| `keys` | `(JsonValue) -> Result<List<str>, Error>` | オブジェクトのキー一覧を返す。値がオブジェクトでない場合はエラー |

### メモリ管理

| 関数 | シグネチャ | 説明 |
|------|-----------|------|
| `json_free` | `(JsonValue) -> Unit` | JsonValue とその子要素をすべて解放 |

## `Result<JsonValue, Error>` のアンラップ

`parse`, `get`, `at` は `Result<JsonValue, Error>` を返します。内側の値を
別の json 関数に渡す前に `Result` をアンラップする必要があります。
`Result` を直接渡すとコンパイル時に拒否されます:

```python
case parse(text):
  Ok(doc):
    # ✗ エラー: kind() は JsonValue 引数を要求
    # kind(get(doc, "name"))
    # ✓ 先にアンラップする
    case get(doc, "name"):
      Ok(name_val):
        print(kind(name_val))
      Err(e):
        print("no name")
  Err(e):
    print("parse error")
```

汎用的な文字列化（`to_str(result)`、`print(result)`、f-string 補間）は
`Result` に対してもそのまま動作し、他の `Result` 値と同様に
`Ok(...)` / `Err(...)` としてフォーマットされます。

## 使用例

### フィールドのパースとアクセス

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

### 配列の操作

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

### 整形出力付きシリアライズ

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

## 注意事項

- `to_int` は整数と整数値の浮動小数点数（例: `42.0` → `42`）の両方を受け付けます
- `to_float` は浮動小数点数と整数（例: `42` → `42.0`）の両方を受け付けます
- `get` と `at` はパースツリー内の子要素への参照を返します。子要素に対して `json_free` を呼ばず、`parse` で返されたルート値に対してのみ呼んでください
- `kind` は整数と浮動小数点数の両方に対して `"number"` を返します
