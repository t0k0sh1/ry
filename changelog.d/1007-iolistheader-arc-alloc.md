### Fixed

- `to_bytes`, `read_bytes`, `tcp_receive`, `tls_receive`, HTTP `body_bytes` が返す `List<u8>` を変数に代入すると macOS で `malloc: *** error for object ...: pointer being freed was not allocated` がクラッシュしていた問題を修正 (#1007)
