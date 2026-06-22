### Added

- `tests/scripts/test-export-run-logs.sh` — `scripts/export-run-logs.sh` の JSONL スキーマ契約 (`docs/architecture/jsonl-run-logs.md`) を 1 ケース fixture で検証する pure shell + `jq` ハーネス。`run_meta` / `command` の両レコードに `schema_version: "1"`、必須キー、`summary` ↔ `exit_code` 整合を assert する。あわせて `scripts/export-run-logs.sh` の `schema_version: "1"` リテラル重複を `SCHEMA_VERSION` 変数 1 箇所に抽出して 2 ブロック間の drift を機械的に排除。`/pre-commit-checklist` から `.claude/skills/pre-commit-checklist/run-export-run-logs-tests.sh` 経由で実行可能。(#2300)
