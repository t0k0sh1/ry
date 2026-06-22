### Changed

- `share/std/manifest.json` から `version` フィールドを削除。コード上一切消費されない装飾メタデータで、`src/cli/self_update.cpp` の `install_stdlib` は読み込み直後に捨て、書き戻し時は呼出元から渡された `new_version` を上書きするのみだった。`StdlibManifest` 構造体と `write_manifest()` シグネチャを単純化し、`install_stdlib()` の `new_version` パラメータも併せて削除。`files` 配列は引き続き hand-maintained で、`/preparing-for-release` skill に追加した新 verify task が release ごとに on-disk stdlib との突合をゲートする。(#2296)
