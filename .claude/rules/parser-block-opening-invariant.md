---
paths:
  - "src/parser/*.cpp"
  - "include/ry/parser/*.hpp"
---

# Parser Block Opening

- Block openings after header expressions must use `Parser::consumeBlockOpening()` or `Parser::parseBlock()`.
- Do not hand-roll `if (lex_.peek().kind != TokenKind::Indent)` after expressions that can call `parsePostfixContinuation`; it bypasses `chain_pending_dedents_`.
- Hand-rolled Indent checks are only safe for pure declaration headers with no preceding expression, such as `fn`, `record`, `enum`, `require`, `ensure`, and lambda block headers.
- When adding an expression before `:`, sweep nearby block-opening code for this invariant.
