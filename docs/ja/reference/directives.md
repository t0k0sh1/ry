[English](../../reference/directives.md) | [日本語](directives.md) | [繁體中文](../../zh/reference/directives.md)

# ディレクティブ

ディレクティブは宣言に付与できるコンパイル時メタデータアノテーションです。Java のアノテーションと同様の `@name` 構文を使用します。

## 構文

```
@name
@name(key=value, ...)
```

ディレクティブは対象の宣言の前に配置します。複数のディレクティブを重ねることもできます。

## 対象

ディレクティブは以下の宣言に適用できます:

- `function` - 関数定義（`@it` / `@describe` で装飾された名前付きテスト関数を含む）
- `record` - レコード定義
- 変数宣言（`@const` 付きまたは通常代入）
- `record` 定義内のフィールド
- `for` - カウント付きループのみ（`@parallel` 用）
- `it` / `describe` 呼び出し（従来のラムダ形式） - `@each` と `@property` 用のテストケースとテストグループ

## 組み込みディレクティブ

### `@deprecated`

宣言を非推奨としてマークします。非推奨のエンティティが使用（呼び出し、参照、アクセス）されると、コンパイル時に警告が出力されます。

**関数に対して:**

```
@deprecated
function old_function() -> int:
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

**トップレベルの `@const` と関数について:** トップレベルの `@const` 宣言は、同じソースファイル内で以降に定義されるすべてのトップレベル関数から参照可能です。不変性はすべての参照に対して強制され、トップレベル `@const` の record フィールドへのミューテーションも含まれます。詳細は [functions.md](functions.md) の「トップレベル変数と関数ボディ内での `@const`」セクションを参照してください。

### `@native`

ランタイムによって実装が提供される関数を宣言します。関数本体を持つことはできません。

オプションの文字列引数で共有ライブラリのモジュール名を指定できます。`@native("libname")` 関数が呼ばれると、JIT は対応する共有ライブラリ（macOS では `libry_<libname>.dylib`、Linux では `libry_<libname>.so`）を動的にロードし、そこからランタイムシンボルを解決します:

```ry
@native              # 組み込み（プロセスに静的リンクされる）
@native("base64")    # libry_base64.dylib/.so から動的ロード
```

**基本構文:**

```
@native
function contains(string: str, substring: str) -> bool

print(contains("hello world", "world"))  # true
```

**演算子オーバーロード:**

```
@native
function operator+(a: str, b: str) -> str

print("hello" + " world")  # hello world
```

**UFCS との組み合わせ:**

```
@native
function to_upper(string: str) -> str

print("hello".to_upper())  # HELLO
```

**引数数の検証:**

`@native` 宣言に型シグネチャが含まれている場合、コンパイラは呼び出し時に引数の数を検証します。オーバーロードされた関数（例: 1, 2, 3 引数の `range`）もサポートされ、いずれかのオーバーロードにマッチすれば検証を通過します。

```
@native
function range(n: int) -> List<int>
@native
function range(start: int, end: int) -> List<int>

print(length(range(5)))       # OK: 1引数のオーバーロードにマッチ
print(length(range(1, 10)))   # OK: 2引数のオーバーロードにマッチ
print(length(range()))        # Error: expects 1 or 2 argument(s), but got 0
```

**標準ライブラリ宣言 (`core/`):**

`core/` ディレクトリにはすべての組み込み関数の `@native` 宣言がカテゴリ別に格納されています:

| ファイル | 内容 |
|---|---|
| `core/builtins.ry` | `print`, `length`, `range`, `enumerate`, `zip`, `exit`, `args`, `available_parallelism`, `sleep` |
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
- 引数なしの `@native` の場合、宣言した関数は既存の組み込み関数に対応している必要があります。対応していない場合はコンパイル時にエラーになります。`@native("libname")` の場合、関数は宣言されたシグネチャに基づいてコンパイルされ、ロードされたライブラリからシンボルを解決できなかった場合は JIT リンク時に失敗します。

**ライブラリ指定:**
- `@native("libname")` は、native 関数が `libry_<libname>.dylib`（macOS）または `libry_<libname>.so`（Linux）という共有ライブラリに存在することを指定します。JIT 起動時、必要な共有ライブラリは以下の検索パス（順序通り）からロードされます:
  1. `exe/../lib/` — インストールレイアウト
  2. `exe/lib/` — 開発 / ビルドレイアウト
  3. `$RY_HOME/lib/` — ユーザーインストール環境
- `@native`（静的）と `@native("libname")`（動的）のどちらの宣言も、引数数検証と呼び出し解決のために登録されます。違いは JIT にランタイムシンボルをどう提供するかだけです。
- ランタイム関数名は `__ry_<libname>_<fn_name>` という規約に従います（例: `@native("base64") fn encode(...)` → `__ry_base64_encode`）。これは stdlib パッケージでも、ユーザー定義の native ライブラリでも同じように動作します。

### `@parallel`

カウント付き `for` ループを並列実行対象としてマークします。

```
@parallel
for i in range(8):
    work(i)
```

**対応対象:**

- `for` 文のみ

**制約事項:**

- `for` 文には 1 つの `@parallel` だけ指定できます。
- 反復対象は `range(...)` または整数 `..` レンジに限られます。
- 分解代入付きの反復は未対応です。
- 外側の可変変数への代入は拒否されます。
- v1 ではループ本体内の `break`、`continue`、インデックス代入、フィールド代入は拒否されます。

### `@each`

パラメタライズドテストを有効にし、テストを異なるパラメータで複数回実行します。

**構文（名前付き関数に対して、推奨）:**

```ry
@each([(arg1, arg2, ...), ...])
@it("should handle {0} and {1}")
function test_handle(param1: type, param2: type):
    # テスト本体
```

**構文（従来の `it` ラムダに対して）:**

```ry
@each([(arg1, arg2, ...), ...])
it("should handle {0} and {1}", (param1: type, param2: type):
    # テスト本体
)
```

引数はタプルのリストを返す任意の式を使えます。関数呼び出しも可能です:

```ry
@each(make_inputs())
@it("should handle {0}")
function test_handle(x: int):
    # テスト本体
```

**対応対象:** `@it` で装飾された関数、または従来の `it` 呼び出し。

**制約事項:**
- 引数はタプルのリストを返す式である必要がある
- タプルのアリティは関数のパラメータ数と一致する必要がある
- 説明文の `{0}`, `{1}`, ... はパラメータ値の文字列表現で置換される

### `@property`

プロパティベーステストを有効にし、テストにランダム入力を生成します。

**構文（名前付き関数に対して、推奨）:**

```ry
@property(count=100)
@it("should verify property name")
function test_property(a: int, b: int):
    # ランダム値によるテスト本体
```

**構文（従来の `it` ラムダに対して）:**

```ry
@property(count=100)
it("should verify property name", (a: int, b: int):
    # ランダム値によるテスト本体
)
```

**対応対象:** `@it` で装飾された関数、または従来の `it` 呼び出し。

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

### `@it`

名前付き関数に装飾することでテストケースを宣言します。関数ボディがテスト本体となり、`ry test` で実行されます。完全な仕様は [テストリファレンス](testing.md) を参照してください。

**構文:**

```ry
@it("description")
function test_name():
    # アサーション
```

**基本例:**

```ry
@it("should add 1 + 2 = 3")
function test_add():
    expect(1 + 2).to_eq(3)
```

**`@each` / `@property` との組み合わせ:**

```ry
@each([(1, 2, 3), (0, 0, 0), (-1, 1, 0)])
@it("should add {0} + {1} = {2}")
function test_add_each(a: int, b: int, expected: int):
    expect(a + b).to_eq(expected)

@property(count=100)
@it("should verify addition is commutative")
function test_commutative(a: int, b: int):
    expect(a + b).to_eq(b + a)
```

**対応対象:** `function` 宣言のみ。関数は戻り値型の注釈を持ってはいけません。

**制約事項:**
- `ry test` で実行される `*.test.ry` ファイル内でのみ有効
- `@each` と組み合わせる場合、関数のパラメータリストはタプルのアリティと一致する必要がある
- `@property` と組み合わせる場合、各パラメータ型はサポートされているジェネレータ型（`int`、`float`、`bool`、`str`）のいずれかでなければならない

### `@describe`

名前付き関数に装飾することで、関連するテストをグループ化します。ボディ内で宣言された内側の `@it` 関数はそのグループに属し、ボディに直接宣言された変数はすべての内側 `@it` にキャプチャされる共有セットアップとして機能します。従来のラムダ形式と異なり、`@describe` グループは**ネストが可能**です。出力はネストの深さに比例してインデントされます。

**構文:**

```ry
@describe("group name")
function group_name():
    @it("nested test")
    function test_nested():
        # アサーション
```

**基本例:**

```ry
@describe("arithmetic")
function arithmetic_tests():
    @it("should subtract")
    function test_sub():
        expect(10 - 3).to_eq(7)

    @it("should multiply")
    function test_mul():
        expect(4 * 5).to_eq(20)
```

**共有セットアップ:**

外側の `@describe` ボディで宣言された変数は、内側のすべての `@it` 関数に自動的にキャプチャされます。

```ry
@describe("shared setup")
function shared_setup_tests():
    base = 100
    offset = 5

    @it("should use base")
    function test_base():
        expect(base).to_eq(100)

    @it("should use base and offset")
    function test_combined():
        expect(base + offset).to_eq(105)
```

**ネストしたグループ:**

```ry
@describe("outer")
function outer():
    @describe("inner")
    function inner():
        @it("should pass deeply nested test")
        function test_deep():
            expect(1 + 1).to_eq(2)
```

**対応対象:** `function` 宣言のみ。関数はパラメータも戻り値型の注釈も持ってはいけません。

### `@inline`

LLVM オプティマイザにインライン化のヒントを与えます。デフォルトでは、関数を積極的にインライン化するよう指示します。

**基本的な使い方（常にインライン化）:**

```
@inline
function add(a: int, b: int) -> int:
    return a + b
```

**mode パラメータ付き:**

```
@inline(mode="always")
function hot_path(x: int) -> int:
    return x * 2 + 1

@inline(mode="hint")
function medium_path(x: int) -> int:
    return x + 1

@inline(mode="never")
function cold_error_handler(msg: str):
    print("ERROR: " + msg)
```

**モード:**

| モード | LLVM 属性 | 説明 |
|--------|----------|------|
| `always`（デフォルト） | `AlwaysInline` | 常にインライン化する |
| `hint` | `InlineHint` | オプティマイザにインライン化を提案する |
| `never` | `NoInline` | インライン化を禁止する |

**制約:**
- `@inline` は `@native` と併用できません（native 関数にはインライン化するボディがありません）。
- 不明な mode 値はコンパイルエラーになります。

### パラメータ（将来拡張）

ディレクティブは将来の拡張に備え、パラメータ構文をサポートしています:

```
@deprecated(reason="use new_api instead")
function old_api() -> int:
    return 0
```

現時点では、パラメータはパースされますが `@deprecated` ディレクティブでは使用されません。

## 注意事項

- 非推奨のエンティティは正常に動作します。警告が出力されるだけです。
- 警告は使用箇所で出力され、定義箇所では出力されません。
- 非推奨のエンティティを定義しても、使用しなければ警告は出力されません。
- 未知のディレクティブ名はパースエラーになります。
- サポートされない対象（`if`、`while` 等）にディレクティブを付与するとパースエラーになります。`for` に使えるのは `@parallel` のみです。
