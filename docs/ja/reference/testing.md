[English](../../reference/testing.md) | [日本語](testing.md) | [繁體中文](../../zh/reference/testing.md)

# テスト機能

Ry はRSpec風のテスト構文を内蔵しています。`ry test` サブコマンドでテストファイルを実行します。

---

## 実行方法

```bash
ry test              # プロジェクト内の *.test.ry を自動検出して実行
ry test tests/spec   # 指定ディレクトリ以下の *.test.ry を再帰的に実行
ry test test_file.ry # 特定のテストファイルを実行
ry test -p           # 全テストを並列実行（-p または --parallel）
ry test -p tests/    # 指定ディレクトリのテストを並列実行
ry test -w           # ウォッチモード: ファイル変更時にテストを自動再実行（-w または --watch）
ry test -w -p        # ウォッチモード + 並列実行
ry test -w tests/    # 特定ディレクトリをウォッチ
ry test --coverage   # 全テストをラインカバレッジ付きで実行
ry test --cov        # --coverage の短縮形
ry test --outline    # テストを実行せずに describe/it 構造を表示
```

終了コードは全テスト成功時に 0、失敗がある場合は 1 です。

### 自動検出モード

`ry test` を引数なしで実行すると:

1. `package.toml` を探してプロジェクトルートを特定
2. プロジェクトルート以下の `*.test.ry` ファイルを再帰的に検出（`.git`、`build`、`node_modules` はスキップ）
3. 各ファイルを実行し、結果を集計

---

## 構文

### 関数ベース構文（推奨）

`@it` と `@describe` ディレクティブを使って、テストケースを通常の名前付き関数として定義します:

```ry
@it("test case name")
function test_add():
    expect(1 + 2).to_eq(3)
```

関連するテストは `@describe` でグループ化します:

```ry
@describe("Arithmetic")
function arithmetic_tests():
    @it("should add integers")
    function test_add():
        expect(1 + 2).to_eq(3)

    @it("should subtract integers")
    function test_sub():
        expect(5 - 3).to_eq(2)
```

- 関数名はコードナビゲーションとシンボル同一性に使われる
- 説明文字列（ディレクティブ内）はテスト出力と報告に使われる
- `@it` 関数は `@each` または `@property` と組み合わせない限り、パラメータを持ってはならない
- `@it` と `@describe` は `ry test` でのみ使用可能

#### 共有セットアップ

`@describe` 関数ボディで宣言された変数は、内側の `@it` 関数に自動的にキャプチャされます:

```ry
@describe("User validation")
function user_validation_tests():
    min_length = 8
    max_length = 64

    @it("should reject short passwords")
    function test_short():
        expect(min_length).to_be_greater_than(0)

    @it("should accept passwords within length limits")
    function test_range():
        expect(max_length).to_be_greater_than(min_length)
```

#### ネストした `@describe`

`@describe` 関数はネストさせて多層のグループ化を作成できます。出力はネストの深さを反映してインデントされます:

```ry
@describe("API")
function api_tests():
    @describe("GET /users")
    function get_users_tests():
        @it("should return 200 OK")
        function test_ok():
            expect(true).to_be_true()
```

出力:

```text
API
  GET /users
    + should return 200 OK
```

### ラムダ構文（非推奨）

> **非推奨**: `describe()` と `it()` のラムダ呼び出し構文は非推奨です。代わりに `@describe` と `@it` ディレクティブを名前付き関数に使用してください。ラムダ構文は将来のリリースで削除されます。
>
> 移行:
>
> | ラムダ構文 | ディレクティブ構文 |
> |---|---|
> | `it("name", (): ...)` | `@it("name") function name(): ...` |
> | `describe("name", (): ...)` | `@describe("name") function name(): ...` |

```
describe("説明文", ():
    it("テストケース名", ():
        # テスト本体
        expect(実際の値).to_eq(期待値)
    )
)
```

- `describe` と `it` は説明文字列と**ラムダ引数** `():` を第二引数に取る
- `describe` ブロック内には `it` ブロックやその他の文（変数宣言など）を記述可能
- 各 `it` ブロックは独立したテストケース
- `describe` / `expect` は `ry test` でのみ使用可能（通常の `ry` 実行ではコンパイルエラー）

### トレイリングブロック構文

任意の関数呼び出し（`describe`/`it`/`mock` を除く）にトレイリングブロック構文が使えます。`()` の後に `:` を付けると、インデントブロックが引数なしラムダとして最後の引数に渡されます:

```
# 以下は等価:
foo("arg"):
    bar()

foo("arg", ():
    bar()
)
```

### expect / マッチャー

| マッチャー | 説明 | 対応型 |
|---|---|---|
| `to_eq(expected)` | 等値比較 | int, float, bool, str |
| `to_not_eq(expected)` | 等しくないこと | int, float, bool, str |
| `to_be_true()` | `true` であること | bool |
| `to_be_false()` | `false` であること | bool |
| `to_be_none()` | `None` であること | Option |
| `to_be_some()` | Option が `Some` であること | Option |
| `to_be_ok()` | Result が `Ok` であること | Result |
| `to_be_err()` | Result が `Err` であること | Result |
| `to_contain(val)` | コンテナが値を含むこと | List, Set, Map, str |
| `to_not_contain(val)` | コンテナが値を含まないこと | List, Set, Map, str |
| `to_be_greater_than(v)` | `actual > v` であること | int, float |
| `to_be_less_than(v)` | `actual < v` であること | int, float |
| `to_be_greater_than_or_eq(v)` | `actual >= v` であること | int, float |
| `to_be_less_than_or_eq(v)` | `actual <= v` であること | int, float |
| `to_have_length(n)` | 長さが `n` であること | List, Set, Map, str |
| `to_be_empty()` | 長さが 0 であること | List, Set, Map, str |
| `to_start_with(prefix)` | 文字列が prefix で始まること | str |
| `to_end_with(suffix)` | 文字列が suffix で終わること | str |

### fail

現在のテストを即座に失敗としてマークします。

```
it("should not reach here", ():
    fail("unexpected error")
)
```

- `fail()` — 汎用メッセージでテストを失敗にする
- `fail(msg)` — カスタムメッセージでテストを失敗にする
- `fail()` 後も実行は継続される（テストを中断はしない）
- `ry test` モードでのみ使用可能

---

## 出力形式

```
Calculator
  + should add numbers
  + should subtract
  - should fail (赤色)
    line 10: expected 3, got 2

2 passed, 1 failed
```

- `+` は成功（緑色）、`-` は失敗（赤色）
- 失敗時は行番号と期待値/実際の値を表示

---

## 例

```
describe("Arithmetic", ():
    it("should add integers", ():
        expect(1 + 2).to_eq(3)

    )
    it("should compare strings", ():
        expect("hello").to_eq("hello")

    )
    it("should check booleans", ():
        expect(3 > 1).to_be_true()

    )
)
describe("Booleans", ():
    it("should return false", ():
        expect(1 > 2).to_be_false()
    )
)
```

---

## モック

### mock(fn_name, replacement)

現在の `it` ブロック内で関数をモック実装に差し替えます。`it` ブロック終了時にモックは自動的にクリアされます。

```
function fetch_data() -> str:
    return "real data"

describe("mocking", ():
    it("should replace function", ():
        mock(fetch_data, () => "fake")
        expect(fetch_data()).to_eq("fake")

    )
    it("should auto-restore after it block", ():
        expect(fetch_data()).to_eq("real data")
    )
)
```

- 第1引数は関数名（識別子、文字列ではない）
- 第2引数は差し替え用ラムダ
- 差し替え関数は元の関数と同じ引数型・戻り値型である必要がある
- 元の関数の `require` と `ensure` 契約はモック呼び出し時にも強制される
- `it` ブロック終了時にモックは自動復元される

### verify(fn_name)

モック済み関数の呼び出し回数を `int` で返します。

```
describe("verify", ():
    it("should count calls", ():
        mock(fetch_data, () => "fake")
        fetch_data()
        fetch_data()
        expect(verify(fetch_data)).to_eq(2)
    )
)
```

### モックの制限事項

- オーバーロードされた関数のモックは非対応
- キャプチャ付きクロージャでのモックは非対応（プレーンラムダのみ）
- `@native function` のモックは非対応

---

## パラメタライズドテスト (@each)

`@each` は同じテストを複数のパラメータセットで実行します。

**関数ベース構文（推奨）:**

```ry
@each([
    (1, 2, 3),
    (0, 0, 0),
    (-1, 1, 0)
])
@it("should add {0} + {1} = {2}")
function test_add(a: int, b: int, expected: int):
    expect(a + b).to_eq(expected)
```

**ラムダ構文:**

```ry
@each([
    (1, 2, 3),
    (0, 0, 0),
    (-1, 1, 0)
])
it("should add {0} + {1} = {2}", (a: int, b: int, expected: int):
    expect(a + b).to_eq(expected)
)
```

- リストにはパラメータ数と同じアリティのタプルを含める
- 説明文の `{0}`, `{1}`, ... はパラメータ値で置換される
- 各タプルは独立したテストケースとして実行される
- 対応するパラメータ型: `int`, `float`, `bool`, `str`

---

## プロパティベーステスト (@property)

`@property` はランダムな入力を生成し、テストを複数回実行します。

**関数ベース構文（推奨）:**

```ry
@property(count=100)
@it("should verify addition is commutative")
function test_commutative(a: int, b: int):
    expect(a + b).to_eq(b + a)
```

**ラムダ構文:**

```ry
@property(count=100)
it("should verify addition is commutative", (a: int, b: int):
    expect(a + b).to_eq(b + a)
)
```

- `count=N` でランダム試行回数を指定（デフォルト: 100）
- 失敗時は反例（失敗した入力値）が表示される
- 最初の失敗でテストを停止
- 対応するパラメータ型: `int` ([-1000, 1000])、`float` ([-1000.0, 1000.0])、`bool`、`str` (ランダム ASCII、0-20文字)

---

## テストカバレッジ

`--coverage`（または `--cov`）フラグでラインカバレッジを計測できます:

```bash
ry test --coverage                    # 全テスト + カバレッジサマリー
ry test --cov tests/spec/math.test.ry # 単一ファイル
ry test --coverage tests/spec/        # ディレクトリ
```

### 出力例

```
Test Coverage Summary:
  tests/spec/math.test.ry    100.0%  (74/74 lines)
  tests/spec/strings.test.ry  92.3%  (24/26 lines)
  -------------------------------------------------
  Total                        95.1%  (98/100 lines)
```

- 標準ライブラリのファイルは除外され、ユーザーコードのみが対象
- `--coverage` と `--parallel` を同時に指定した場合、逐次実行にフォールバック

---

## テストアウトライン

`--outline` を使用すると、テスト本体を実行せずにテストファイルの `describe`/`it` 構造を表示できます:

```bash
ry test --outline tests/spec/mock.test.ry
```

出力:

```
describe mock
  it should replace function
  it should auto-restore after it block
  it should mock with arguments
describe verify
  it should count calls
  it should count zero calls
```

- 個別ファイル、ディレクトリ、`-p`（全テストファイル）で使用可能
- `@each` パラメタライズドテストはフォーマットテンプレートに `(@each)` サフィックスを付けて表示
- `@property` テストはラベルに `(@property)` サフィックスを付けて表示

---

## テスト説明のスタイル

`it` の説明は `should` で始めることが推奨されます。テスト出力で完全な文として自然に読めます:

```text
it should add integers
it should reject invalid input
it should return error when file is missing
```

**推奨:**

| 説明 | 備考 |
|-------------|-------|
| `"should add integers"` | 動詞は原形 |
| `"should reject short passwords"` | 動詞は原形 |
| `"should return error for missing file"` | 動詞は原形 |
| `"should add {0} + {1} = {2}"` | パラメタライズド: 動詞は原形 |
| `"should verify addition is commutative"` | プロパティベース |

**避けるべき:**

| 説明 | 理由 |
|-------------|--------|
| `"adds integers"` | 三人称動詞、"it adds" として読むとぎこちない |
| `"integer addition"` | 名詞句、文ではない |
| `"handles error"` | 三人称動詞 |

`describe` ブロックは名詞またはトピック句を使います（例: `"Arithmetic"`、`"List"`、`"GET /users"`）。`should` は不要です。

---

## 制限事項

- `describe` のネスト（ラムダ構文）は未対応。ネストしたグループ化には関数ベースの `@describe` ディレクティブ構文を使用してください
- `before_each` / `after_each` は未対応
