### Internal

- `docs/reference/examples-taxonomy.md` を追加。`examples/` 配下の `.ry` ファイルを **canonical** / **regression** / **scratch** の 3 カテゴリに分類するポリシー文書で、canonical の品質ルール (runnable / canonical import / 明示型注釈 / `any` を dynamic boundary 限定 / 意図的失敗禁止 / stdlib のみ依存)、`any` の使用ポリシー (`docs/reference/strict-any.md` と `docs/architecture/implicit-any-paths.md` を canonical examples に適用)、および LoRA training data セレクションルール (canonical のみ採用) を定義する。後続 issue (#2326 canonical 拡張 / #2328 realistic small programs / #2329 verification / #2330 spec-derived examples / #2324 stdlib-docs migration) が参照する policy 基盤。(#2327)
- `AGENTS.md` Path-Specific Entry Points に `Examples classification: docs/reference/examples-taxonomy.md.` を 1 行追加し、agent routing から policy を発見可能にした。(#2327)
