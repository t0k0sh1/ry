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
- 変数宣言（`@const` 付きまたは通常代入）
- `record` 定義内のフィールド
- `for` - `@parallel` のみ対応
- `it` - テストケース定義（`@each` / `@property` のみ）

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

@const
p = OldPoint(1, 2)  # warning: 'OldPoint' is deprecated
```

**変数に対して:**

```
@deprecated
@const
old_value = 99

print(old_value)         # warning: 'old_value' is deprecated
```

**フィールドに対して:**

```
record Config:
    @deprecated
    old_setting: int
    new_setting: int

@const
c = Config(1, 2)
print(c.old_setting)     # warning: 'Config.old_setting' is deprecated
print(c.new_setting)     # 警告なし
```

### `@const`

変数を不変としてマークします。`@const` で宣言された変数は初期化後に再代入できません。`@const` なしの場合、変数はデフォルトで可変です。

```
@const
x = 42
# x = 10   # エラー: @const 変数への再代入はできません
```

**型アノテーション付き:**

```
@const
name: str = "hello"
```

**タプル分割代入:**

```
@const
a, b = (1, 2)
```

### `@native`

ランタイム（組み込み）によって実装が提供される関数を宣言します。関数本体を持つことはできません。

**基本構文:**

```
@native
fn contains(string: str, substring: str) -> bool

print(contains("hello world", "world"))  # true
```

**演算子オーバーロード:**

```
@native
fn operator+(a: str, b: str) -> str

print("hello" + " world")  # hello world
```

**UFCS との組み合わせ:**

```
@native
fn to_upper(string: str) -> str

print("hello".to_upper())  # HELLO
```

**引数数の検証:**

`@native` 宣言に型シグネチャが含まれている場合、コンパイラは呼び出し時に引数の数を検証します。オーバーロードされた関数（例: 1, 2, 3 引数の `range`）もサポートされ、いずれかのオーバーロードにマッチすれば検証を通過します。

**標準ライブラリ宣言 (`core/`):**

`core/` ディレクトリにはすべての組み込み関数の `@native` 宣言がカテゴリ別に格納されています:

| ファイル | 内容 |
|---|---|
| `core/builtins.ry` | `print`, `length`, `range`, `enumerate`, `zip`, `exit`, `args`, `available_parallelism` |
| `core/str.ry` | `contains`, `starts_with`, `ends_with`, `find`, `substring`, `char_at`, `replace`, `to_upper`, `to_lower`, `trim`, `trim_start`, `trim_end`, `repeat`, `reverse`, `split`, `join` |
| `core/convert.ry` | `to_int`, `to_float`, `to_str` |
| `core/list.ry` | `append`, `pop`, `insert`, `remove_at`, `slice`, `distinct`, `flatten`, `sort`, `first`, `last`, `is_empty` |
| `core/map.ry` | `keys`, `values`, `items`, `has_key`, `get`, `merge` |
| `core/set.ry` | `add`, `remove`, `union`, `intersection`, `difference`, `symmetric_difference`, `is_subset`, `is_superset` |
| `core/higher_order.ry` | `filter`, `map`, `reduce`, `fold`, `any`, `all`, `sum`, `min`, `max` |

これらのファイルは `ry` 実行バイナリの近くに `core/` ディレクトリが存在する場合、プレリュードとして自動的にロードされます。プレリュードにより、組み込み関数呼び出し時の引数数検証が有効になります。

**制約事項:**
- `@native` 関数は本体を持てません（シグネチャの後に `:` を付けるとエラー）。
- 本体を付けるとパースエラー: `@native function must not have a body`。
- 宣言した関数は既存の組み込み関数に対応している必要があります。対応していない場合はコンパイル時にエラーになります。

**将来の拡張方向:**
- `@native("libfoo.so")` — 外部共有ライブラリへの FFI バインディング。

### `@parallel`

counted `for` ループを並列実行対象としてマークします。

```
@parallel
for i in range(8):
    work(i)
```

**対応対象:**

- `for` 文のみ

**制約事項:**

- `for` 文には 1 つの `@parallel` だけ指定できます。
- 反復対象は `range(...)` または整数 `..` に限られます。
- 分解代入付きの反復は未対応です。
- 外側の可変変数への代入は拒否されます。
- v1 ではループ本体内の `break`、`continue`、インデックス代入、フィールド代入は拒否されます。

### `@each`

パラメタライズドテストを有効にし、`it` ブロックを異なるパラメータで複数回実行します。

**構文:**

```
@each([(引数1, 引数2, ...), ...])
it("{0} と {1} の説明", fn(param1: 型, param2: 型):
    # テスト本体
)
```

**対応対象:** `it` 呼び出しのみ

**制約事項:**
- 引数はタプルのリストである必要がある
- タプルのアリティはラムダのパラメータ数と一致する必要がある
- 説明文の `{0}`, `{1}`, ... はパラメータ値の文字列表現で置換される

### `@property`

プロパティベーステストを有効にし、`it` ブロックにランダム入力を生成します。

**構文:**

```
@property(count=100)
it("プロパティ名", fn(a: int, b: int):
    # ランダム値によるテスト本体
)
```

**対応対象:** `it` 呼び出しのみ

**パラメータ:**

| パラメータ | 型 | デフォルト | 説明 |
|-----------|------|---------|-------------|
| `count` | int | 100 | ランダム試行回数 |

**対応するパラメータ型:**

| 型 | 範囲 |
|------|-------|
| `int` | -1000 〜 1000 |
| `float` | -1000.0 〜 1000.0 |
| `bool` | true または false |
| `str` | ランダム ASCII、0-20文字 |

失敗時は反例（失敗を引き起こしたパラメータ値）が表示されます。

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
- サポートされない対象（`if`、`while` 等）にディレクティブを付与するとパースエラーになります。`for` に使えるのは `@parallel` のみです。
