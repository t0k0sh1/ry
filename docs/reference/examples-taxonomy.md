# Examples Taxonomy

`examples/` 配下の `.ry` ファイルを「何のためにあるか」で 3 カテゴリに分類する。LoRA training の positive data として使うものと、含めてはならないものをルールで区別可能にすることが目的。`tests/` 側の `docs/reference/test-taxonomy.md` と同じ三段構成 (定義 → 制約 → 具体例) を踏襲する。

## カテゴリ

### canonical

LoRA training の positive data として使う「公式の Ry の書き方」を示す例。`examples/` 直下の現存ファイルは provisionally このカテゴリに置かれている。要件は次節の「品質ルール」を満たすこと。

例: `examples/fibonacci.ry`, `examples/closures_higher_order.ry`, `examples/json_record_loading.ry`。

### regression

過去のバグ・review 指摘を再現または防御する例。LoRA training には**含めない**。

- 冒頭コメントに `# regression #NNNN` を 1 行付け、ファイル単位で識別可能にする。`docs/reference/test-taxonomy.md` の regression カテゴリと同じ `#NNNN` 規約。
- 意図的に compiler error / runtime panic を発火させてよい。
- 現状ゼロ。

### scratch

review・探索・実験用の一時コード。LoRA training には**含めない**。

- 冒頭コメントに `# scratch` を 1 行付け、ファイル単位で識別可能にする。
- 動作不完全な断片や polish 前の探索コードを含んでよい。
- 現状ゼロ。

## canonical の品質ルール

canonical カテゴリのファイルは以下をすべて満たす:

1. ビルドした `<build-dir>/ry <file>` がゼロ exit code で完結する (`<build-dir>` は preset により `build/` または `build-rust/`、`AGENTS.md` の "Build And Test" 表を参照)。
2. `fn` の引数は明示型注釈を持つ。戻り値および lambda 引数の注釈省略は許容する (`docs/architecture/implicit-any-paths.md` の Path 1/2/3 の keep / deprecate 区分に従う)。LoRA training data としては戻り値の明示注釈を**推奨**する。
3. import は canonical 形式のみ — `from ry.<module> import …` または `import ry.<module>` (`changelog.d/2351-reject-legacy-stdlib-imports.md` で legacy 形式が hard error 化済み)。
4. strict-any default (v0.0.30 以降の compiler 既定) でビルドできる。`any` の使用は dynamic boundary に限定する (次節)。
5. 意図的な panic / abort / 未網羅 `case` 起因のフォールスルー失敗を含まない。
6. ライブラリ外部依存なし — `from ry.*` の stdlib のみで完結する。

ルール 1 と 6 は LoRA training 入力として安定 reproducible であるため。ルール 2 から 5 は Ry の "stronger typing direction" の品質指針を反映する。

## `any` の使用ポリシー (dynamic boundary 限定)

詳細な classification は `docs/architecture/implicit-any-paths.md`、mode semantics は `docs/reference/strict-any.md` を source of truth とする。canonical examples の範囲では以下のみ判断材料となる:

**許可される (dynamic boundary)**: JSON / JSON5 parse 結果、`@native` heterogeneous-return プレースホルダ、FFI / `@extern` 戻り値、plugin / diagnostics 境界。

**避けるべき**: `fn` 引数の implicit any (型注釈省略)、`v: any = …` から具体型への implicit unwrap、`any` 値への直接演算。同等比較 `==` / `!=` は許容。`Map<str, any>` の値を具体型として扱う場面では `case asType[T](v)` 等のチェック付き narrowing を使う。

## LoRA training data のセレクション

`canonical` カテゴリのみを training data に含める。除外条件:

- 冒頭コメントに `# regression #NNNN` または `# scratch` を含むファイル。
- `<build-dir>/ry <file>` が非ゼロ exit で完結するファイル。
- 「canonical の品質ルール」のいずれかを満たさないファイル。

自動化 (manifest 出力 / CI ガード) と全数検証は別 issue のスコープで、本ドキュメントはルールの定義のみを担う。training pipeline 本体 (別リポジトリ) との接続点は `docs/architecture/jsonl-run-logs.md` を参照。

## 関連

- `docs/reference/test-taxonomy.md` — `tests/` 側の対応分類。本文書の regression 規約はこれと整合する。
- `docs/reference/strict-any.md` — `any` ポリシーの mode semantics、診断 id、default flip 計画。
- `docs/architecture/implicit-any-paths.md` — implicit any 経路の網羅目録と keep / deprecate 区分。
- `docs/architecture/jsonl-run-logs.md` — `scripts/export-run-logs.sh` 経由の training pipeline 接続点。
