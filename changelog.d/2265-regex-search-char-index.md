### fix(regex): `regexSearch` / `search` return char index instead of byte offset (#2265)

`regexSearch(text, pattern)` および UFCS `text.search(/pattern/)` は Thompson NFA から得た byte offset をそのまま返していたため、multibyte UTF-8 を含む subject では返値が character index と乖離していた (例: `regexSearch("あx", "x")` は `1` を期待するところ `3` を返していた)。`include/ry/runtime/core/regex.hpp` の `__ry_regex_search` 宣言コメントは元から "char-index" と謳っており、`find()` / `len` / `charAt` / `substr` 等 repo 全体の string API ポリシーとも整合しない契約違反だった。

`__ry_regex_search` (`src/runtime/core/regex.cpp`) の戻り直前で `__ry_utf8_char_index_n` を呼び、`-1` (not found) と `kRegexSearchError` (catch sentinel) は変換せず短絡することで character index に揃えた。ASCII および NUL バイトを含む subject では byte == char で挙動不変、`__ry_regex_is_match` の `>= 0` 判定にも影響しない。docs/reference/regex.md トップテーブル 2 箇所の "start position" も "character index of the first match start" に厳密化した。
