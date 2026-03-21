[English](../../reference/testing.md) | [日本語](testing.md) | [繁體中文](../../zh/reference/testing.md)

# テスト機能

Ry はRSpec風のテスト構文を内蔵しています。`ry test` サブコマンドでテストファイルを実行します。

---

## 実行方法

```bash
ry test              # プロジェクト内の *.test.ry を自動検出して実行
ry test test_file.ry # 特定のテストファイルを実行
```

終了コードは全テスト成功時に 0、失敗がある場合は 1 です。

### 自動検出モード

`ry test` を引数なしで実行すると:

1. `ry.toml` を探してプロジェクトルートを特定
2. プロジェクトルート以下の `*.test.ry` ファイルを再帰的に検出（`.git`、`build`、`node_modules` はスキップ）
3. 各ファイルを実行し、結果を集計

---

## 構文

### describe / it

```
describe("説明文", fn():
    it("テストケース名", fn():
        # テスト本体
        expect(実際の値).to_eq(期待値)
    )
)
```

- `describe` と `it` は説明文字列と**ラムダ引数** `fn()` を第二引数に取る
- `describe` ブロック内には `it` ブロックやその他の文（変数宣言など）を記述可能
- 各 `it` ブロックは独立したテストケース
- `describe` / `expect` は `ry test` でのみ使用可能（通常の `ry` 実行ではコンパイルエラー）

### トレイリングブロック構文

任意の関数呼び出しにトレイリングブロック構文が使えます。`()` の後に `:` を付けると、インデントブロックが引数なしラムダとして最後の引数に渡されます:

```
# 以下は等価:
foo("arg"):
    bar()

foo("arg", fn():
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

---

## 出力形式

```
Calculator
  + adds numbers
  + subtracts
  - fails test (赤色)
    line 10: expected 3, got 2

2 passed, 1 failed
```

- `+` は成功（緑色）、`-` は失敗（赤色）
- 失敗時は行番号と期待値/実際の値を表示

---

## 例

```
describe("Arithmetic", fn():
    it("adds integers", fn():
        expect(1 + 2).to_eq(3)

    )
    it("compares strings", fn():
        expect("hello").to_eq("hello")

    )
    it("checks booleans", fn():
        expect(3 > 1).to_be_true()

    )
)
describe("Booleans", fn():
    it("false check", fn():
        expect(1 > 2).to_be_false()
    )
)
```

---

## モック

### mock(fn_name, replacement)

現在の `it` ブロック内で関数をモック実装に差し替えます。`it` ブロック終了時にモックは自動的にクリアされます。

```
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

- 第1引数は関数名（識別子、文字列ではない）
- 第2引数は差し替え用ラムダ
- 差し替え関数は元の関数と同じ引数型・戻り値型である必要がある
- `it` ブロック終了時にモックは自動復元される

### verify(fn_name)

モック済み関数の呼び出し回数を `int` で返します。

```
describe("verify", fn():
    it("counts calls", fn():
        mock(fetch_data, fn(): "fake")
        fetch_data()
        fetch_data()
        expect(verify(fetch_data)).to_eq(2)
    )
)
```

### モックの制限事項

- オーバーロードされた関数のモックは非対応
- キャプチャ付きクロージャでのモックは非対応（プレーンラムダのみ）
- `@native fn` のモックは非対応

---

## 制限事項

- `describe` のネストは未対応
- `before_each` / `after_each` は未対応
