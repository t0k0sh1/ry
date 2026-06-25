### Changed

- `scripts/export-run-logs.sh` の `RUN_NONCE` 生成を microsecond ベース (`jq -nr 'now * 1000000 | floor'`) から `/dev/urandom` 由来の decimal uint64 に置き換え、生成ロジックを sourceable helper `scripts/lib/run-nonce.sh` (`gen_run_nonce`) に切り出し。これにより同一 wall-clock 秒内で複数の invocation が並走しても `run-id` (`YYYYMMDD-HHMMSS-<short-sha>-<nonce>`) が衝突しなくなる。`tests/scripts/test-export-run-logs.sh` に同一 shell プロセス内で `gen_run_nonce` を 50 回呼んで全値が distinct であることを assert する回帰チェックを追加 (PID stable な `$$` ベース実装への regression を検知)。`docs/architecture/jsonl-run-logs.md` を新しい nonce 仕様にあわせて更新。(#2402)
