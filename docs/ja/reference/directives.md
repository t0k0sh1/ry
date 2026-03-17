[English](../../reference/directives.md) | [日本語](directives.md) | [繁體中文](../../zh/reference/directives.md)

# ディレクティブ

ディレクティブは宣言に付与できるコンパイル時メタデータです。`@name` 構文を使用します。

## 構文

```
@name
@name(key=value, ...)
```

ディレクティブは対象の宣言の前に配置します。複数のディレクティブを重ねることもできます。

## 対象

ディレクティブは以下の宣言に適用できます:

- `fn` - 関数定義
- `record` - 構造体定義
- `let` / `var` - 変数宣言
- `record` 定義内のフィールド

## 組み込みディレクティブ

### `@deprecated`

宣言を非推奨としてマークします。非推奨のエンティティが使用（呼び出し、参照、アクセス）されると、コンパイル時に警告が出力されます。

**関数に対して:**

```
@deprecated
fn old_function() -> int:
    return 42

print(old_function())   # warning: 'old_function' is deprecated
```

**型に対して:**

```
@deprecated
record OldPoint:
    x: int
    y: int

let p = OldPoint(1, 2)  # warning: 'OldPoint' is deprecated
```

**変数に対して:**

```
@deprecated
let old_value = 99

print(old_value)         # warning: 'old_value' is deprecated
```

**フィールドに対して:**

```
record Config:
    @deprecated
    old_setting: int
    new_setting: int

let c = Config(1, 2)
print(c.old_setting)     # warning: 'Config.old_setting' is deprecated
print(c.new_setting)     # 警告なし
```

### パラメータ（将来拡張）

ディレクティブは将来の拡張に備え、パラメータ構文をサポートしています:

```
@deprecated(reason="use new_api instead")
fn old_api() -> int:
    return 0
```

現時点では、パラメータはパースされますが `@deprecated` ディレクティブでは使用されません。

## 注意事項

- 非推奨のエンティティは正常に動作します。警告が出力されるだけです。
- 警告は使用箇所で出力され、定義箇所では出力されません。
- 非推奨のエンティティを定義しても、使用しなければ警告は出力されません。
- 未知のディレクティブ名はパースエラーになります。
- サポートされない対象（`if`、`while` 等）にディレクティブを付与するとパースエラーになります。
