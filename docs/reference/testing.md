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
describe "説明文":
    it "テストケース名":
        # テスト本体
        expect(実際の値).to_eq(期待値)
```

- `describe` ブロック内には `it` ブロックのみ記述可能
- 各 `it` ブロックは独立したテストケース
- `describe` / `expect` は `ry test` でのみ使用可能（通常の `ry` 実行ではコンパイルエラー）

### expect / マッチャー

| マッチャー | 説明 | 対応型 |
|---|---|---|
| `to_eq(expected)` | 等値比較 | int, float, bool, str |
| `to_be_true()` | `true` であること | bool |
| `to_be_false()` | `false` であること | bool |
| `to_be_none()` | `None` であること | Option |

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
describe "Arithmetic":
    it "adds integers":
        expect(1 + 2).to_eq(3)

    it "compares strings":
        expect("hello").to_eq("hello")

    it "checks booleans":
        expect(3 > 1).to_be_true()

describe "Booleans":
    it "false check":
        expect(1 > 2).to_be_false()
```

---

## 制限事項

- `describe` のネストは未対応
- `before_each` / `after_each` は未対応
- テストファイルのglob実行（`ry test tests/`）は未対応
