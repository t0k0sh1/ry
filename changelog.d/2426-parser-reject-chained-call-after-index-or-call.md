### Fixed

- `f(args)[T]("arg")` 等の chained call (callee が単一 IDENT でない `<expr>(args)`) を parser が silent split せず明示的な parse error で reject するよう変更。従来は `r = make(42)[int]("inner")` を 2 文 `r = make(42)[int]` + `("inner")` に silent 分解し、`ry fmt` 後に改行で分断されたコードが出力されて「fmt が壊した」と誤認される原因となっていた。エラーメッセージは関連 issue #809 (chained call サポートは `not_planned`) への参照と、中間変数への束縛 (`tmp = f(args)[T]` → `tmp(args)`) という workaround を含む。`Ident[T](args)` (`identity[int](42)` 等) や `f(...)?[T]` / `f(...)?.method()` は引き続き正常動作する。 (#2426)
