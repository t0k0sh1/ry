### fix(codegen): tuple element splitter tracks both `<>` and `()` depth (#2264)

tuple type の要素位置に内部 `,` を持つ 2 引数 generic (`Map<K, V>` / `Result<T, E>`) を書くと `unknown type: Map<str` 等のコンパイルエラーが出ていた問題を修正した (例: `xs: List<(int, Map<str, int>)> = []`)。`src/codegen_type.cpp` の tuple 要素分割が `(` `)` のみで depth を追跡し `<` `>` を無視していたため、内側 generic の `,` を tuple 要素区切りと誤認していた。canonical な `ry::util::splitTupleTypeName` (内部で `splitTopLevelCommas` を使い `<>`/`()`/`[]` を追跡) に置き換えて修正。List / Set / bare tuple / fn return / type alias / Result-in-tuple の全パターンを spec test で検証。
