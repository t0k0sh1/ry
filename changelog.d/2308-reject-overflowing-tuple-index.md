### Fixed

- `CodeGen::emitExprVariant(FieldAccessExpr)` (`src/codegen_expr_literal.cpp`) が `unsigned long` を超える数値タプルフィールドインデックス (例: `t.999999999999999999999999999999999999999999`) を受け取った際、`std::stoul` の uncaught `std::out_of_range` でプロセス全体が abort (`libc++abi: ... stoul: out of range`, exit 134) する問題を修正。`src/parser/parser_decl.cpp:951-955` の固定長配列サイズで既に確立されている `std::strtoul` + `errno == ERANGE` + end-pointer check のパターンを移植し、parse 失敗時は既存の `tuple index <field> out of range` 診断にルーティングする (ユーザー可視診断は不変)。 (#2308)
