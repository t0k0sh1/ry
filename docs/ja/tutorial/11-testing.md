[English](../../tutorial/11-testing.md) | [日本語](11-testing.md) | [繁體中文](../../zh/tutorial/11-testing.md)

# テスト

[<- 前: 並行処理](10-concurrency.md) | [次: プロジェクトのビルド ->](12-building-a-project.md)

Ry には `describe`、`it`、`expect` を使った RSpec スタイルの組み込みテスト構文があります。詳細な仕様は[テストリファレンス](../reference/testing.md)を参照してください。

---

## テストの実行

```bash
ry test                       # *.test.ry ファイルを自動検出して実行
ry test tests/spec            # 指定ディレクトリ以下の *.test.ry を再帰的に実行
ry test tests/my_test.test.ry # 特定のテストファイルを実行
ry test -p                    # 全テストを並列実行（-p または --parallel）
```

すべてのテストが成功すると終了コード `0`、1つでも失敗すると `1` が返されます。

引数なしで実行すると、`ry test` は `package.toml` を探してプロジェクトルートを特定し、すべての `*.test.ry` ファイルを再帰的に検出します。

---

## テストの書き方

`describe` で関連するテストをグループ化し、`it` で個々のテストケースを定義します。

```python
describe("Calculator", ():
    it("adds integers", ():
        expect(1 + 2).to_eq(3)

    )
    it("subtracts integers", ():
        expect(5 - 3).to_eq(2)

    )
    it("checks booleans", ():
        expect(3 > 1).to_be_true()
    )
)
```

- `describe` と `it` は説明文字列と**ラムダ引数** `():` を第二引数に取ります
- `describe`、`it`、`expect`、`mock`、`verify` は `ry test` でのみ使用できます（通常の `ry` 実行ではコンパイルエラー）

---

## マッチャー

| マッチャー | 説明 | 対応型 |
|----------|------|--------|
| `to_eq(expected)` | 等値比較 | int, float, bool, str |
| `to_not_eq(expected)` | 不等値アサーション | int, float, bool, str |
| `to_be_true()` | `true` アサーション | bool |
| `to_be_false()` | `false` アサーション | bool |
| `to_be_none()` | `None` アサーション | Option |
| `to_be_some()` | Option が `Some` であるアサーション | Option |
| `to_be_ok()` | Result が `Ok` であるアサーション | Result |
| `to_be_err()` | Result が `Err` であるアサーション | Result |
| `to_contain(val)` | コンテナに値が含まれるアサーション | List, Set, Map, str |
| `to_not_contain(val)` | コンテナに値が含まれないアサーション | List, Set, Map, str |
| `to_be_greater_than(v)` | `actual > v` アサーション | int, float |
| `to_be_less_than(v)` | `actual < v` アサーション | int, float |
| `to_be_greater_than_or_eq(v)` | `actual >= v` アサーション | int, float |
| `to_be_less_than_or_eq(v)` | `actual <= v` アサーション | int, float |
| `to_have_length(n)` | 長さが `n` に等しいアサーション | List, Set, Map, str |
| `to_be_empty()` | 長さが 0 であるアサーション | List, Set, Map, str |
| `to_start_with(prefix)` | 文字列がプレフィックスで始まるアサーション | str |
| `to_end_with(suffix)` | 文字列がサフィックスで終わるアサーション | str |

### fail

`fail()` は現在のテストを即座に失敗としてマークします。

```python
it("should handle error", ():
    case result:
        Ok(v):
            fail("expected error")
        Err(e):
            expect(e.message).to_eq("not found")
)
```

- `fail()` -- 汎用メッセージでテストを失敗にする
- `fail(message)` -- カスタムメッセージでテストを失敗にする
- `ry test` モードでのみ使用可能

---

## 出力フォーマット

```
Calculator
  + adds integers
  + subtracts integers
  - checks booleans
    line 10: expected true, got false

2 passed, 1 failed
```

`+` は成功（緑）、`-` は失敗（赤）を示します。

---

## モック

### `mock(fn_name, replacement)`

現在の `it` ブロック内で関数をモック実装に置き換えます。`it` ブロック終了時に自動的に復元されます。
元の関数の `require` と `ensure` の契約はモックされた呼び出しに対しても引き続き実行されます。

```python
function fetch_data() -> str:
    return "real data"

describe("mocking", ():
    it("replaces function", ():
        mock(fetch_data, () => "fake")
        expect(fetch_data()).to_eq("fake")

    )
    it("auto-restores", ():
        expect(fetch_data()).to_eq("real data")
    )
)
```

### `verify(fn_name)`

モックされた関数の呼び出し回数を返します。

```python
describe("verify", ():
    it("counts calls", ():
        mock(fetch_data, () => "fake")
        fetch_data()
        fetch_data()
        expect(verify(fetch_data)).to_eq(2)
    )
)
```

---

## パラメタライズドテスト

`@each` を使って同じテストを複数の入力で実行できます:

```python
@each([(1, 2, 3), (0, 0, 0), (-1, 1, 0)])
it("adds {0} + {1} = {2}", (a: int, b: int, expected: int):
    expect(a + b).to_eq(expected)
)
```

各タプルが個別のテストケースになります。説明文の `{0}`, `{1}` は実際の値で置換されます。

---

## プロパティベーステスト

`@property` を使ってランダム生成された入力でテストできます:

```python
@property(count=100)
it("addition is commutative", (a: int, b: int):
    expect(a + b).to_eq(b + a)
)
```

テストは `count` 回ランダム値で実行されます。失敗時は反例が表示されます。

---

## 契約を使ったテスト

契約（[エラーハンドリング](08-error-handling.md)参照）はモックと連携します: 元の関数の `require` と `ensure` の契約はモックされた呼び出しに対しても**引き続き実行されます**。これにより、契約は暗黙的なテストアサーションとして機能します。

```python
function deposit(amount: int, balance: int) -> int:
    require:
        amount > 0
    ensure v:
        v > balance
    return balance + amount

describe("deposit", ():
    it("mocked version still checks contracts", ():
        mock(deposit, (amount: int, balance: int) => balance + amount)
        expect(deposit(10, 100)).to_eq(110)
        # deposit(-1, 100) は "require failed" で終了する
    )
)
```

> **なぜこれが重要なのか**: 実装の詳細をモックしつつ、契約のセーフティネットを保持できます。モックが事後条件に違反した場合、テストが即座にそれを検出します。

---

## 制限事項

- `describe` のネストはサポートされていません
- `before_each` / `after_each` はサポートされていません
- オーバーロード関数および `@native` 関数はモックできません

---

## 演習

1. **基本的なテスト**: `max(a: int, b: int) -> int` 関数のテストとして、等しい値、正の数、負の数をカバーする `describe` ブロックを書いてください。

2. **モック**: 値を返す関数 `fetch_temperature() -> int` を書いてください。テスト内で固定値を返すようにモックし、`verify` で正確に1回呼ばれたことを確認してください。

3. **パラメタライズドテスト**: `@each` を使って `is_even(n: int) -> bool` 関数を入力 `[(2, true), (3, false), (0, true), (-4, true)]` でテストしてください。

---

[<- 前: 並行処理](10-concurrency.md) | [次: プロジェクトのビルド ->](12-building-a-project.md)
