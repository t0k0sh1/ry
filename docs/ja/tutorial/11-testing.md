[English](../../tutorial/11-testing.md) | [日本語](11-testing.md) | [繁體中文](../../zh/tutorial/11-testing.md)

# テスト

[← 前: 契約による設計](10-contracts.md)

Ry には `describe`、`it`、`expect` を使った RSpec スタイルの組み込みテスト構文があります。詳細な仕様は[テストリファレンス](../reference/testing.md)を参照してください。

---

## テストの実行

```bash
ry test                       # *.test.ry ファイルを自動検出して実行
ry test tests/spec            # 指定ディレクトリ以下の *.test.ry を再帰的に実行
ry test tests/my_test.test.ry # 特定のテストファイルを実行
```

すべてのテストが成功すると終了コード `0`、1つでも失敗すると `1` が返されます。

引数なしで実行すると、`ry test` は `ry.toml` を探してプロジェクトルートを特定し、すべての `*.test.ry` ファイルを再帰的に検出します。

---

## テストの書き方

`describe` で関連するテストをグループ化し、`it` で個々のテストケースを定義します。

```python
describe("Calculator", fn():
    it("adds integers", fn():
        expect(1 + 2).to_eq(3)

    )
    it("subtracts integers", fn():
        expect(5 - 3).to_eq(2)

    )
    it("checks booleans", fn():
        expect(3 > 1).to_be_true()
    )
)
```

- `describe` と `it` は説明文字列と**ラムダ引数** `fn():` を第二引数に取ります。
- `describe` / `it` / `expect` / `mock` / `verify` は `ry test` でのみ使用できます（通常の `ry` 実行ではコンパイルエラー）。

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
| `to_contain(val)` | コンテナに値が含まれるアサーション | List, Set, str |

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

```python
fn fetch_data() -> str:
    return "real data"

describe("mocking", fn():
    it("replaces function", fn():
        mock(fetch_data, fn(): "fake")
        expect(fetch_data()).to_eq("fake")

    )
    it("auto-restores", fn():
        expect(fetch_data()).to_eq("real data")
    )
)
```

### `verify(fn_name)`

モックされた関数の呼び出し回数を返します。

```python
describe("verify", fn():
    it("counts calls", fn():
        mock(fetch_data, fn(): "fake")
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
it("adds {0} + {1} = {2}", fn(a: int, b: int, expected: int):
    expect(a + b).to_eq(expected)
)
```

各タプルが個別のテストケースになります。説明文の `{0}`, `{1}` は実際の値で置換されます。

---

## プロパティベーステスト

`@property` を使ってランダム生成された入力でテストできます:

```python
@property(count=100)
it("addition is commutative", fn(a: int, b: int):
    expect(a + b).to_eq(b + a)
)
```

テストは `count` 回ランダム値で実行されます。失敗時は反例が表示されます。

---

## 制限事項

- `describe` のネストはサポートされていません
- `before_each` / `after_each` はサポートされていません
- オーバーロード関数および `@native` 関数はモックできません

---

[← 前: 契約による設計](10-contracts.md)
