### Fixed

- HTTP リクエストの `query_all`, `cookies_all`, `form_fields`, `form_file` が返す `Map<str, str>` を変数に代入すると macOS で `malloc: *** error for object ...: pointer being freed was not allocated` がクラッシュしていた問題を修正 (#1011)
