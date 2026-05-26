### Fixed

- Nested tuple/record field access with chained numeric indices
  (`nested.0.0`, `pair.1.0`, `((1,2),(3,4)).0.1`) failed to parse with
  `expected field name or index after '.'`. The lexer greedily absorbed
  `.0` after an integer literal as a fraction part, so `t.0.0` lexed as
  `[Ident, Dot, Float("0.0")]` instead of `[Ident, Dot, Number("0"),
  Dot, Number("0")]`. Suppress fraction absorption when the integer
  literal directly follows a `Dot` token (`src/lexer.cpp` —
  `prev_kind_ != TokenKind::Dot` check, symmetric to the existing
  leading-dot float disambiguation). Update `docs/grammar.ebnf` to
  reflect that `INTEGER` is accepted in field-access position alongside
  `IDENT`. Non-regression for `1.5`, `(1.5)`, `a + 1.5`, `.5`, and
  `5.double()` is verified by lexer unit tests
  (`DotAfterDotSuppressesFractionAbsorption`). (#1892)
