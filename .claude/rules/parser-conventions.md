---
paths:
  - "src/parser*.cpp"
  - "src/lexer*.cpp"
  - "include/ry/parser.hpp"
  - "include/ry/lexer.hpp"
  - "src/formatter*.cpp"
---

# Parser / Lexer

### Only `fn` is a function declaration keyword; `function` is a normal identifier

**Source**: #1343 (2026-04-23, implementation)
**Tags**: lexer, keyword, function-type, canonical-type-id, migration

**Rule**: The lexer maps only `fn` to `TokenKind::Fn`. The string `function` tokenizes as `Ident`, so it can be used as a variable or parameter name. Function types are spelled `fn(T) -> R` in source; `isFunctionTypeName` and `splitFunctionTypeName` accept only the `fn(` prefix. The compile-time / `to_str` category name for function-typed values is the canonical type id string `"fn"` (not `"function"`). When updating examples or C++-embedded Ry in tests, avoid leaving `function` as a reserved spelling.

### Use depth-tracking for nested delimiters, never naive find()

**Source**: #801 PR
**Tags**: parser, lexer, delimiters

**Context**: `parseFnTypeAnnotation()` originally used
`string::find(')')` to locate the closing paren, which broke on
nested function-typed parameters like
`fn((int) -> int) -> int`.

**Rule**: When scanning for a matching closing delimiter (`)`, `]`,
`}`), always use a depth counter that increments on the opening and
decrements on the closing. Never use `string::find(')')` or similar.
Reuse `CodeGen::findMatchingCloseParen()` in `src/codegen_type.cpp`
as the canonical helper.

### NumberExpr.value holds a non-negative bit pattern, not a signed magnitude

**Source**: #807 + #819 sweep (2026-04-10)
**Tags**: parser, codegen, numeric-literal, u64, bit-pattern

**Context**: The parser accepts integer literals up to `UINT64_MAX`
via `strtoull`, but `NumberExpr.value` is declared as `int64_t`. To
support `u64` max literals (`18446744073709551615` = bit pattern
`0xFFFFFFFFFFFFFFFF` = `int64_t(-1)`) without changing the AST type,
the field is documented to store the non-negative magnitude as a bit
pattern. The invariant breaks only if a parser site tries to be
clever and stores a pre-negated value (e.g., the old pattern-literal
path in `parser_decl.cpp` built `NumberExpr{-val, ""}` for `case -1:`).
Codegen's empty-suffix emit path then cannot distinguish "legitimate
negative from unary minus" from "overflow bit pattern >= 2^63".

**Rule**: `NumberExpr.value` is always the unsigned bit pattern of a
non-negative magnitude. Negation is expressed as
`UnaryExpr("-", NumberExpr{magnitude, suffix})`, the same as
`parser_expr.cpp`. Any new parser site that creates a NumberExpr
from a literal that may be negative MUST wrap the node in a UnaryExpr
instead of storing a pre-negated `int64_t`. In codegen, interpret
`static_cast<uint64_t>(value)` when the target type is unsigned, and
for the empty-suffix path treat `value < 0` as "literal exceeds
`INT64_MAX`" → emit a "integer literal out of range for int" error.

**How to apply**: Never write `NumberExpr{-val, ...}`. Always wrap in
UnaryExpr. The empty-suffix `value < 0` check in
`src/codegen_expr.cpp::emitExprVariant(const NumberExpr &)` depends
on this invariant — a regression would either silently accept
`x = 18446744073709551615` (wrong i64 emit) or reject `case -1:`
(false positive).

### Statement-level LHS should reuse parsePostfixContinuation, not re-implement postfix hops

**Source**: #812 (2026-04-11, implementation)
**Tags**: parser, lvalue, lhs, postfix, assignment

**Context**: Before #812, `Parser::parseStatement` dispatched `Ident [ ... ]`
and `Ident . field` with a hardcoded 1-hop switch, rejecting any chained
form (`list[i].field = v`, `record.a.b = v`, `list[i][j] = v`, etc.) with
"expected '=' after index expression" or "expected '=' after field name".
The fix split `parsePostfix` into `parsePrimary` + `parsePostfixContinuation(base)`
and drives the statement LHS by feeding a synthetic `VariableExpr` base
into the continuation, then inspecting the chain tail (`IndexExpr` /
`FieldAccessExpr` / `CallExpr`) to decide which `*AssignStmt` variant to
emit.

**Rule**: When a new statement form needs to accept the same expression
shapes as an lvalue, do NOT duplicate the `.`/`[` loop in the statement
parser. Extract a continuation helper from `parsePostfix` and call it from
the statement entry point. Dispatch on the chain tail node to choose the
stmt variant.

**How to apply**: Any statement that starts with `Ident` and may accept
chained postfix hops (e.g. a future `move var.a.b into ...` or similar)
should call `parsePostfixContinuation(baseExpr)` rather than re-parsing `.`
and `[` inline. UFCS call statements (`ident.method(args)`) remain a
special case detected before entering the continuation, because they
produce a `CallStmt` rather than an `ExprStmt<CallExpr>`.

### Use strtod, not std::stod, for float literal parsing

**Source**: #819 sweep (2026-04-10)
**Tags**: parser, float, scientific-notation, exception-safety

**Context**: `std::stod` throws `std::out_of_range` on overflow
(e.g., `1e400`), which crashes the frontend before diagnostics can
be produced. #819's scope check explicitly allows overflow to
surface as `+Inf`, matching the runtime `to_float` converter.

**Rule**: Use `std::strtod` + `errno` for parsing float literals in
the frontend. Accept `HUGE_VAL` / `-HUGE_VAL` as valid `Inf` results
and only treat non-zero trailing characters as errors. See
`include/ry/parser.hpp::parseFloatLiteral`.

---

### Avoid std::sto* throw-on-fail converters in parser/codegen paths

**Source**: #1259 (2026-04-21, fuzz_parser crash on array size overflow)
**Tags**: parser, codegen, integer-overflow, exception-safety, strtoull, libfuzzer

**Context**: `std::stoull` / `std::stoul` / `std::stoi` throw
`std::out_of_range` on overflow and `std::invalid_argument` on
non-numeric input. These exceptions propagate out of the frontend
uncaught and abort the process (visible to `fuzz_parser` as
`SUMMARY: libFuzzer: deadly signal`). #1259's repro: `T[99…99]`
(84 digits) in `parseTypeNameSingle` via `std::stoull(value)`.

Additionally, `std::stoull` with default base 10 **silently** parses
hex/binary/underscore-containing tokens by stopping at the first
non-digit character — so `T[0xFF]` pre-#1259 silently became `T[0]`,
and `T[1_000]` became `T[1]`. The lexer's `TokenKind::Number`
encompasses hex (`0xFF`) and binary (`0b10`) literals plus
underscore separators (`1_000`), so any `std::stoull` call against
a Number token's raw `value` string is vulnerable to both crashes
and silent miscounts.

**Rule**: Never use `std::sto*` on lexer-produced numeric strings.
Use `std::strtoull` (or `std::strtoul` for narrower targets) with:
1. `errno = 0` reset before the call
2. `char *end` output parameter
3. Explicit base (`10` for decimal-only, `0` for prefix-sensitive)
4. Reject if `errno == ERANGE || end != c_str() + size()`

Reference site: `src/parser_decl.cpp` array-size branch in
`parseTypeNameSingle` (post-#1259) and `NumberExpr.value strtoull`
entry above for integer literal parsing.

**Known hit sites** (all should be audited with this rule):
- `src/parser_decl.cpp:787` — fixed in #1259 (array size `T[N]`)
- `src/codegen_type.cpp:133` — inline-array type resolution from
  string name; tracked in #1281
- `src/codegen_expr_literal.cpp:109` — tuple numeric field access
  (`.0`, `.1`, ...); tracked in #1281

**How to apply**: When you see a new `std::sto*` call anywhere in
`src/parser*.cpp`, `src/codegen*.cpp`, or any frontend path that
handles user-provided numeric strings, replace it before merging.
A fuzz harness (current: `fuzz_parser`, `fuzz_json`, `fuzz_utf8`)
will catch parser-path regressions, but codegen paths have no
harness as of #1259 — reviewer diligence is the only gate there.

---

### Ry statements don't accept bare-identifier expression statements

**Source**: #798/#799/#800 implementation (case/if expression unification)
**Tags**: parser, statement-grammar, expression-statement, if-block-expr

**Context**: Ry's `parseStatement` (`src/parser.cpp:499-664`) handles
non-identifier tokens as expression statements (`[1,2].map(f)`, `42`,
`"str"`, etc.) via the generic `parseConditional` fallback at line
499-501. But when a statement **starts with an identifier**, the parser
commits to the ident-dispatch path and requires one of `=`, `+=`, `[`,
`.`, `(`, `++`, `--` to follow. So `y` or `y + 1` as a bare statement is
rejected with `expected '=', '+=', ... after identifier`.

**Implication for `if` expression block form** (#798): the tail
expression of `if cond: body else: body` is parsed via `parseBlock` →
`parseStatement`, so the last line cannot be a bare identifier or an
unparenthesized identifier-starting binary expression. Users must wrap
such expressions in parens: `(y + 1)`, `(x)`. This is a **real
limitation** of the block form, not a bug.

**Rule**: When introducing new block-valued expression forms, remember
that Ry's statement grammar cannot parse identifier-starting pure
expression statements. Either (a) require parenthesized tail expressions
in user documentation, (b) use non-identifier expressions (literals,
calls), or (c) write a custom block parser that calls `parseConditional`
directly for the tail line.

### Identifier-trailing `!` tokenization must exclude every multi-char operator starting with `!`

**Source**: #1211 (2026-04-20, bug fix)
**Tags**: parser, lexer, identifier, trailing-bang, bangbang, ambiguity

**Context**: The lexer greedily absorbs a trailing `!` into an
identifier to support mutating method names (`sort!`, `reverse!`,
`append!`, `clear!`). The original guard only excluded `!=`, so `r!!`
tokenized as `r!` (Ident) + `!` (Error) and parse-failed as
`expected ')'` when used in expression position like `Ok(r!!)`. The
postfix error-propagation alias `!!` was thus broken for the
identifier-direct case — the documented equivalence with `?` was only
honored when the preceding token was not an identifier (e.g. after `)`).

**Rule**: The trailing-bang absorption in the identifier branch
(`src/lexer.cpp` identifier tokenization) must exclude **every**
multi-character operator token that begins with `!`, not just `!=`.
Currently that means `!=` and `!!`; any future operator starting with
`!` (hypothetical `!~`, `!?`, etc.) must be added to the exclusion set
at the same time it is introduced in the operator lexer branch.

**How to apply**: When adding a new operator whose first character is
`!`, update both (a) the operator dispatch in `Lexer::next` and
(b) the trailing-bang exclusion in the identifier tokenizer. A unit
test asserting `tokenize("r<op>")` yields `Ident("r")` + the new
operator prevents regression — `tokenize("<op>")` standalone does not
catch it because the bug is specific to the identifier-adjacent case.

---

### Tuple pattern `parsePattern`: same ambiguity resolution as expression parser

**Source**: #834 (2026-04-16, implementation)
**Tags**: parser, pattern, tuple, grouping, ambiguity

**Rule**: The `parsePattern` `LParen` branch uses the same grouping-vs-tuple disambiguation as the expression parser (`src/parser_expr.cpp`):
- `()` → rejected ("zero-tuple pattern not supported")
- `(p)` (no comma) → grouping, returns the inner pattern unwrapped
- `(p,)` → 1-tuple `TuplePattern` with one element
- `(p1, p2, ...)` → N-tuple `TuplePattern`

**Why**: Keeping expression and pattern parsers consistent avoids user confusion (grouping `(p)` behaves the same in both contexts) and makes future record / enum-payload pattern parsers easier to cross-reference.

**How to apply**: When adding the next pattern type that starts with `(` (e.g., positional record `Point(a, b)` — note that starts with `Ident LParen`, not bare `LParen`), keep the bare-`LParen` branch as tuple-only and handle `Ident LParen` in a separate branch.

### `parseDirectives` must defer `LParen` when tuple-destructure LHS lookahead matches

**Source**: #1189 (2026-04-19, implementation)
**Tags**: parser, directive, tuple, destructure, lookahead, ambiguity

**Rule**: In `parseDirectives()`, before consuming `LParen` as the start of a directive argument list (`@name(arg, ...)`), check `!looksLikeParenthesizedTupleDestructure()`. If the lookahead matches, leave the `LParen` for `parseStatement()` to handle as the start of a tuple-destructure LHS (`@const (a, b) = expr`).

**Why**: Directive-arg syntax `@name(arg, ...)` and statement-level tuple-destructure LHS `(ident, ident, ...) =` both start with `@name LParen Ident`. The only disambiguator is the trailing `=` after `RParen`. Without this guard, `@const (a, b) = expr` is misparsed as `@const(a, b)` with positional args `a` and `b`, and the subsequent `=` trips the "directives are not supported on this statement" gate. `couldBeLambda`-style conservative lookahead (restored via `lex_.saveState()` / `restoreState()`) is the right tool — false negatives are fine because they fall through to the old behavior.

**How to apply**: When adding any new statement form that begins with `LParen` after directives (e.g., `@const (_ as Pattern) = expr`, hypothetical future destructuring variants), extend `looksLikeParenthesizedTupleDestructure` (or add a parallel predicate) and make `parseDirectives` defer to it in the same guard location. Never let `parseDirectives` unconditionally eat `LParen`.

### `@directive` definition syntax bypasses the registry validator

**Source**: #708 (2026-04-27, implementation)
**Tags**: parser, directive, directive-def, ast, formatter, registry

**Rule**: `@directive(target=...) fn name(params)` is intercepted in `parseStatement` **before** `validateDirective` runs, and produces a dedicated `DirectiveDefStmt` AST node — it does not flow through `builtinDirectiveRegistry()` like `@native` / `@inline` / `@deprecated`. The dispatch order:

1. `parseStatement` calls `parseDirectives()` to collect directives
2. If `hasDirective(directives, "directive")` and the next token is `Fn`, call `parseDirectiveDefStatement(directives)` instead of `parseFnStatement`
3. The directive arg validation (target required, named-only, value-type checks, allowed-target set) is enforced inside `parseDirectiveDefStatement` directly via `parseError`
4. `@directive` is **not registered** in `directive_meta.cpp` — `validateDirective` would reject it as unknown if it ever reached that path

**Why**: The `target` argument shapes the AST representation (extracted into `DirectiveDefStmt.targets`), so it must be validated at the same point the AST node is built. Registry-routed validators only see the directive args after parsing, which is too late to influence node construction. The early-intercept pattern also lets the parser emit precise diagnostics like `@directive must be followed by 'fn'` and `@directive cannot be combined with other directives` at the right source location.

**How to apply**: If you add another directive that produces a dedicated AST node (rather than just attaching to an existing statement), follow the same pattern — intercept in `parseStatement` before `validateDirective`, build the dedicated node inside a helper, and skip registry registration. Conversely, do **not** add `@directive` to `builtinDirectiveRegistry()` — it would create a phantom validator that never fires.

**Adjacent invariants**:
- `DirectiveDefStmt.targets` is always `std::vector<std::string>`. Bare-string sugar `target="function"` is canonicalized into `{"function"}` at parse time, so downstream consumers (codegen, formatter, future #710 signature builder) never see the bare-string form.
- `DirectiveDefStmt` codegen is intentionally a no-op (`emitStmt(DirectiveDefStmt&)` in `src/codegen_stmt_misc.cpp`) until #710 consumes it for runtime registration. A future contributor seeing the empty body should not assume logic is missing — the IR-emission gap is by design.
- Formatter always emits `target=[...]` (List form), even when the source used the bare-string sugar. `FormatterTest.DirectiveDefBareStringSugarCanonicalises` locks this in.
- After #1397: required params (no default value) are recorded in `DirectiveSignature::positional_param_names` (in declaration order) so they may be passed either positionally or by name at the use site. Optional params (with default) remain in `named_params` (named-only). The `validateDirectiveSignature` shared validator detects positional+named duplicates and missing-required errors using the `positional_param_names` list. Built-in directives leave `positional_param_names` empty for backward compatibility — their existing positional-only semantics are preserved because `min_positional`/`max_positional` still gate them.

### Formatter→parser roundtrip: `TupleDestructStmt` must not emit `: ` between pattern and `=`

**Source**: #1189 (2026-04-19, implementation)
**Tags**: formatter, parser, tuple, destructure, roundtrip, latent_bug

**Rule**: `formatTupleDestruct()` in `src/formatter_stmt.cpp` must emit only `<pattern> = <value>` (plus optional `@const` directive on a prior line). Do **not** emit a stray `: ` between the closing `)` of the pattern and the `=`. The immutability is conveyed by the `@const` directive emitted before the statement, not by a `:` suffix on the LHS.

**Why**: Until #1189 landed, the parser rejected all parenthesized tuple-destructure forms, so the formatter's output `(a, b):  = (1, 2)` never round-tripped through parse. Enabling the parenthesized parse branch exposed the latent `: ` bug — formatted output now fails `ry fmt` verification ("formatted output failed to re-parse"). Adding `FormatterTest.ParenTupleDestructRoundTrip` locks this in so future formatter edits cannot regress.

**How to apply**: When adding or modifying a formatter rule for a new statement shape, grep for a matching parser spec test and add a `verifyFormatting` / roundtrip assertion. Formatter output that fails to re-parse is a silent correctness bug during `ry fmt`; only the verification pass catches it.

### UnaryExpr fast-path covers bare int for INT64_MIN (`-9223372036854775808`)

**Source**: #1025 (2026-04-16)
**Tags**: codegen, numeric-literal, unary-minus, int64-min, ubsan

**Rule**: The `-<NumberExpr>` fast-path in `src/codegen_expr.cpp::emitExprVariant(UnaryExpr)`
accepts bare int (empty suffix) as equivalent to `i64` for the magnitude check
(`|INT64_MIN|` = 2^63). A standalone `9223372036854775808` without the unary minus must
still be rejected by the bare-`NumberExpr` path (`value < 0` check).

Magnitude > 2^63 on the bare path produces `"integer literal out of range for int: -<mag>"`
(matching the existing bare-int error wording); the signed-suffix path keeps the
`"<suffix> literal out of range: -<mag>"` wording.

**UB fix**: The original computation `static_cast<uint64_t>(-static_cast<int64_t>(mag))`
is signed-integer UB when `mag == 2^63` (negation of INT64_MIN overflows). Use
unsigned two's-complement arithmetic instead:
```cpp
const uint64_t negBits = static_cast<uint64_t>(0) - mag;  // well-defined, same bits
```
This also applies to the signed-suffix path — add `-9223372036854775808i64` to
`SignedSuffixMinAcceptedViaUnaryMinus` to keep it covered.

**How to apply**: If you touch this fast-path, keep the bare-int branch in sync with the
signed-low-level-suffix branch. Always use unsigned subtraction for `negBits`; never
negate a `uint64_t` value via `static_cast<int64_t>` when the magnitude may equal 2^(N-1).

