### Changed

- `ry test -p` / `ry test --parallel` の暗黙デフォルトワーカー数を `std::thread::hardware_concurrency()` から `hardware_concurrency() - 1`(最低 1)に変更し、1 コアをユーザ操作や他プロセス用に空ける挙動に統一した。`hardware_concurrency()` が `0`(取得失敗)あるいは `1` を返した場合はどちらも 1 ワーカーになる。明示指定の `-p N` / `--parallel N` / `--parallel=N`、およびシーケンシャル `ry test` の挙動は変えない。並列実行開始時に `Running M test files with K workers...` を stderr に表示し、終了サマリーの `(K workers)` 表示と合わせて開始・終了の両端から並列数を確認できる。`.github/workflows/ci.yml` の bare `-p` 呼び出しは新しいデフォルトをそのまま継承する。(#2216)
