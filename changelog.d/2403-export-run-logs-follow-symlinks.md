### Fixed

- `scripts/export-run-logs.sh` のディレクトリ展開で symlink された `.test.ry` が `find ... -type f` に拾われず黙って除外される問題を修正。`find -L` でリンクを辿るように変更し、リテラルファイル経路の `[[ -f ... ]]` 判定 (symlink 既追従) と挙動を一致させた。`tests/scripts/test-export-run-logs.sh` にディレクトリ配下の symlink `.test.ry` を回帰検出する sub-test を追加。(#2403)
