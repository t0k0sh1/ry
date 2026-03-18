[English](../../reference/testing.md) | [日本語](testing.md) | [繁體中文](../../zh/reference/testing.md)

# テスト機能

Ry はRSpec風のテスト構文を内蔵しています。`ry test` サブコマンドでテストファイルを実行します。

---

## 実行方法

```bash
ry test test_file.ry
```

テストの終了コードは失敗したテスト数です（0 = 全パス）。

---

## 構文

### describe / it

```
describe("説明文"):
    it("テストケース名"):
        # テスト本体
        expect(実際の値).to_eq(期待値)
```

- `describe` と `it` は**トレイリングブロック構文**を使用: 関数呼び出しの後に `:` を付けるとインデントブロックがラムダとして最後の引数に渡される
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
| `to_contain(val)` | コンテナが値を含むこと | List, Set, str |

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
describe("Arithmetic"):
    it("adds integers"):
        expect(1 + 2).to_eq(3)

    it("compares strings"):
        expect("hello").to_eq("hello")

    it("checks booleans"):
        expect(3 > 1).to_be_true()

describe("Booleans"):
    it("false check"):
        expect(1 > 2).to_be_false()
```

---

## 制限事項

- `describe` のネストは未対応
- `before_each` / `after_each` は未対応
- テストファイルのglob実行（`ry test tests/`）は未対応
