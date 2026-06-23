---
paths:
  - "src/parser/*.cpp"
  - "include/ry/parser/*.hpp"
---

# Parser — Block Opening After Header Expression

This file covers only hazards that are not visible from reading the code.

### Block openings that follow a header expression must route through `consumeBlockOpening()` / `parseBlock()` — never hand-roll the `Indent` check

**Tags**: parser, block-opening, chain_pending_dedents_, multiline-ufcs, drift, recurrence

A multiline UFCS chain (`parsePostfixContinuation` in `src/parser/parser_expr.cpp`) absorbs `Indent` tokens across `\n.<method>()` hops. When the chain ends on a non-Newline/Dedent token (e.g. `:`, `>`, `)`), absorbed Indents stay on `Parser::chain_pending_dedents_`. The next block opening must decrement this counter in place of expecting a literal `Indent` token.

`Parser::parseBlock()` was made CPD-aware in #2136, but four `case` body openings and one `case`-expression arm body in #2311 still hand-rolled the check:

```cpp
if (lex_.peek().kind != TokenKind::Indent)
    parseError("expected indented block");
lex_.next(); // consume Indent
```

This pattern silently fails after a header that can leave CPD>0. Both #2136 (if/while bodies) and #2311 (case subjects and arm guards) have hit the same defect class.

**Rule**: any block opening whose `:` is preceded by a sub-expression that can call `parsePostfixContinuation` (`if` / `while` / `for` condition, `case` subject, `case` arm guard, function `return`, etc.) **must** route through `Parser::consumeBlockOpening()` or `Parser::parseBlock()`. Hand-rolling `if (lex_.peek().kind != TokenKind::Indent)` at such sites is forbidden — it bypasses `chain_pending_dedents_` accounting and silently breaks the multiline UFCS surface.

**Exclusion criterion**: a hand-rolled Indent check is only safe at openings whose `:` is preceded by **no expression** — pure declaration headers such as `fn name(params) -> Type:`, `record Foo:`, `enum E:`, `require:` / `ensure:`, `(params) -> Type:` (lambda block). At those sites CPD is provably 0, so the helper is unnecessary. If a future grammar change puts an expression in front of any of these openings, sweep them through the helper at the same time.

**How to apply**:

- New block-opening sites: use `consumeBlockOpening()` (or `parseBlock()` when the body content is a statement list). The helper takes an optional `missing_indent_msg` for call-site-specific diagnostics.
- When reviewing a parser change that adds an expression position before a `:`, search for nearby `if (lex_.peek().kind != TokenKind::Indent)` and confirm the body opening uses the helper.
- The full list of helper users lives in `src/parser/parser.cpp` (`parseBlock`), `src/parser/parser_decl.cpp` (both `parseCaseStatement*`), and `src/parser/parser_expr.cpp` (both `parseCaseExpr*` and `parseCaseExprArmBody`).
