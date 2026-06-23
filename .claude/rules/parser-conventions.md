---
paths:
  - "src/parser/*.cpp"
  - "src/lexer/*.cpp"
  - "include/ry/parser/parser.hpp"
  - "include/ry/lexer/lexer.hpp"
  - "src/formatter*.cpp"
---

# Parser / Lexer

This file covers only hazards that are not visible from reading the code.
Coding conventions (depth-tracking, naming, `strtoull` usage, etc.) are enforced by CI lint.

### Statement grammar rejects bare identifier-starting expressions — this is intentional

**Tags**: parser, statement-grammar, expression-statement, if-block-expr

`parseStatement` requires one of `=` / `+=` / `[` / `.` / `(` / `++` / `--` when the statement begins with an identifier. `y` or `y + 1` is rejected with "expected '=', '+=', ... after identifier". The `if`-expr block tail also goes through `parseBlock` → `parseStatement`, so identifier-starting bare expressions still require parentheses. **The asymmetry with case-expr block tails is intentional** (#1891 scope boundary): case-expr uses an expression-first/restore pattern that accepts identifier-starting forms; `if`-expr does not. Documented in `docs/reference/control-flow.md`.

### `in_if_cond_` flag suppresses bare-ident `Ident FatArrow` dispatch inside if-expression conditions

**Tags**: parser, lambda, if-expression, bare-lambda, fatarrow, lookahead, ambiguity, blind-spot

The `Ident` branch in `parsePrimary` guards bare-lambda dispatch with `lex_.peek().kind == TokenKind::FatArrow && !in_if_cond_`. The flag is set and restored only around the `parseConditional()` call inside `parseIfExpression`. Clobbering it unconditionally to `false` would break lambdas in outer `if` or then/else bodies. **This flag looks like a bug but is intentional** — do not remove `in_if_cond_` or permanently set it to false.

### `@directive` definition syntax bypasses the registry validator

**Tags**: parser, directive, directive-def, ast, formatter, registry

`@directive(target=...) fn name(params)` is intercepted by `parseStatement` before `validateDirective` runs, producing a `DirectiveDefStmt` AST node — it does not go through `builtinDirectiveRegistry()`. Do not add `@directive` to `builtinDirectiveRegistry()`: `validateDirective` would then reject it as unknown. The `allowed_targets` field on built-in directives is informational only — `validateDirectiveSignature` does not read it, so adding a target does not automatically cause rejections at non-target sites.

### Multiline postfix `.` continuation needs `chainIndents` / `chain_pending_dedents_` balanced-Dedent drain

**Tags**: parser, postfix, ufcs, multiline, indent-dedent, save-restore, block-boundary, blind-spot

When `parsePostfixContinuation` consumes multiline `.` tokens, it must track the net `Indent` count earned by the chain in the parser member `chain_pending_dedents_` and drain that count before returning. Without the drain, `parseBlock` misreads trailing `Dedent` tokens from the chain as the surrounding block's exit and exits early. The lexer emits `Indent`/`Dedent` regardless of bracket depth, so lexer tuning cannot avoid this. The four speculative parse sites that transitively reach `parsePostfixContinuation` (lambda dispatch / case-arm tail / generic call / generic-enum dispatch) save/restore `chain_pending_dedents_` only on the lexer-restore path. When adding a new speculative site, add it to this 4-site list and include save/restore.

### Speculative `try { parseX() } catch (...)` needs a commit-flag for hard validation errors

**Tags**: parser, speculative-parse, try-catch, lambda, commit-flag, diagnostic-wording

A hard validation error (invalid input, not ambiguity) thrown inside a speculative `try / catch (...)` is swallowed and produces the wrong diagnostic. Delay throwing until after the disambiguator is consumed; set a commit-flag immediately after commit is certain, then re-throw. **Placing the flag set before the disambiguator also makes the fallback path a hard error** — the ordering is an invariant.

### `verifyFormatting` does NOT catch a dispatch miss in `formatExprInner`

**Tags**: formatter, regex-literal, string-literal, escape, lexer-asymmetry, verifyFormatting, blind-spot

`verifyFormatting` tests only text idempotency (`formatSource(formatSource(x)) == formatSource(x)`). A missing branch for a new AST variant in `formatExprInner` falls through to `/* unknown expr */`, which is a fixed point — so the check passes silently. Add a direct round-trip test to `tests/test_formatter.cpp` for every new `ExprNode` variant. The escape-strategy asymmetry between `StringExpr` (decoded → `escapeString`) and `RegexExpr` (preserved → verbatim) is a real-world instance of the latent-bug class this blind spot enables.

### Postfix concatenation can silently fuse adjacent tokens into a longer operator

**Tags**: formatter, postfix, token-fusion, lexer-greedy, verifyFormatting, blind-spot

When `formatExprInner` emits `ErrorPropagateExpr` as `inner + "?"`, an operand ending in `?` produces `??`, which the lexer tokenizes greedily as the null-coalescing operator. Guard against adjacency fusion with a **text-level** `inner.back() == '?'` check, not a structural type check. The set of fusion-risk characters is determined by the lexer; before touching any postfix branch, grep `src/lexer/lexer.cpp` for multi-character rules.

### INT64_MIN magnitude negation needs unsigned subtraction to avoid UBSan trap

**Tags**: codegen, numeric-literal, unary-minus, int64-min

In the `-<NumberExpr>` fast-path of `emitExprVariant(UnaryExpr)`, when `mag == 2^63`, `static_cast<uint64_t>(-static_cast<int64_t>(mag))` is signed-integer UB (flagged by UBSan). The correct form is `const uint64_t negBits = static_cast<uint64_t>(0) - mag;` — unsigned arithmetic guarantees two's-complement.

### `std::sto*` converters in parser/codegen paths silently truncate and abort on overflow

**Tags**: parser, codegen, integer-overflow, exception-safety, strtoull, libfuzzer

`std::stoull` / `std::stoul` / `std::stoi` throw `std::out_of_range` on overflow and `std::invalid_argument` on non-numeric input, causing the frontend to abort (`fuzz_parser` reports this as a deadly signal). In addition, `std::stoull` silently truncates hex/binary/underscore tokens. There is no harness for the codegen path — reviewers are the only gate. `src/codegen_type.cpp:133` and `src/codegen_expr_literal.cpp:109` remain unfixed (#1281 closed not-planned).
