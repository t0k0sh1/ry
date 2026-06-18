### Fixed

- `ry test --outline` で directory / auto-discovery 経由でも各 `.test.ry` の outline が出力されるよう、子 subprocess の argv に `--outline` を forward した。fan-out の親は outline モード時にサマリ行と progress 行を suppress するので、出力は per-file outline 内容のみ(stdout+stderr は subprocess pipe で merge されるため pre-#2234 sequential 経路の分離ストリームと byte-identical ではないが内容は等価)。#2234 が同 path で warn + disable していたものを復活させた形。`--coverage` (cross-process 集計が必要) と `--trace` (shared file の clobbering) は引き続き single-file のみで multi-file warn + disable のまま。 (#2236)
