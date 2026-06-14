---
paths:
  - "src/parser/*.cpp"
  - "src/lexer/*.cpp"
  - "include/ry/parser/parser.hpp"
  - "include/ry/lexer/lexer.hpp"
  - "src/formatter*.cpp"
---

# Parser / Lexer

### Only `fn` is a function declaration keyword; `function` is a normal identifier

**Source**: #1343 (2026-04-23, implementation)
**Tags**: lexer, keyword, function-type, canonical-type-id, migration

**Rule**: The lexer maps only `fn` to `TokenKind::Fn`. The string `function` tokenizes as `Ident`, so it can be used as a variable or parameter name. Function types are spelled `fn(T) -> R` in source; `isFunctionTypeName` and `splitFunctionTypeName` accept only the `fn(` prefix. The compile-time / `str` category name for function-typed values is the canonical type id string `"fn"` (not `"function"`). When updating examples or C++-embedded Ry in tests, avoid leaving `function` as a reserved spelling.

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
path in `src/parser/parser_decl.cpp` built `NumberExpr{-val, ""}` for `case -1:`).
Codegen's empty-suffix emit path then cannot distinguish "legitimate
negative from unary minus" from "overflow bit pattern >= 2^63".

**Rule**: `NumberExpr.value` is always the unsigned bit pattern of a
non-negative magnitude. Negation is expressed as
`UnaryExpr("-", NumberExpr{magnitude, suffix})`, the same as
`src/parser/parser_expr.cpp`. Any new parser site that creates a NumberExpr
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
surface as `+Inf`, matching the runtime `float` converter.

**Rule**: Use `std::strtod` + `errno` for parsing float literals in
the frontend. Accept `HUGE_VAL` / `-HUGE_VAL` as valid `Inf` results
and only treat non-zero trailing characters as errors. See
`include/ry/parser/parser.hpp::parseFloatLiteral`.

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

Reference site: `src/parser/parser_decl.cpp` array-size branch in
`parseTypeNameSingle` (post-#1259) and `NumberExpr.value strtoull`
entry above for integer literal parsing.

**Known hit sites** (all should be audited with this rule):
- `src/parser/parser_decl.cpp:787` — fixed in #1259 (array size `T[N]`)
- `src/codegen_type.cpp:133` — inline-array type resolution from
  string name; still unfixed (#1281 proposed a fix, closed not-planned)
- `src/codegen_expr_literal.cpp:109` — tuple numeric field access
  (`.0`, `.1`, ...); still unfixed (#1281 proposed a fix, closed not-planned)

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

**Context**: Ry's `parseStatement` (`src/parser/parser.cpp:499-664`) handles
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

### Block-valued case-EXPRESSION arms parse the tail expression-first (option (c)); case-vs-if tail asymmetry is intentional

**Source**: #1891 (2026-06-07, implementation)
**Tags**: parser, case-expr, block-arm, tail-expression, speculative-parse, if-block-expr, asymmetry

**Context**: #1891 added indented-block arms to `case` *expressions*
(`parseCaseExprArmBody` in `src/parser/parser_expr.cpp`). The issue's
motivating example ends a block arm with `tmp * 2` — an
identifier-starting binary expression that the entry above shows Ry's
statement grammar rejects. The `if`-expression block form (#798,
`parseIfExpressionBranchBody` → `parseBlock` → `parseStatement`) takes
option (a): it requires `(tmp * 2)`. To keep the case-expr feature
ergonomic, its block arms instead take **option (c)** and do NOT reuse
`parseBlock`.

**Rule**: `parseCaseExprArmBody` parses each block line
**expression-first**: `lex_.saveState()`, try `parseConditional()`, and
treat the line as the arm's tail value only when the expression consumes
the whole line AND nothing but `DEDENT`/`EOF` follows. Otherwise
`restoreState()` and re-parse the line via `parseStatement()` — which
accepts ordinary statements (assignments, calls) and re-surfaces the
canonical diagnostic for a non-tail bare identifier-binary. A
`parseConditional()` that throws `DiagnosticError` (statement-only
construct such as `while`/`for`/`return`, or a real syntax error) is
caught and likewise routed to `parseStatement()`, so the double-rewind
never masks a genuine error. A block reaching `DEDENT` with no tail
expression is rejected: `case arm block must end with an expression`.

Two consequences worth remembering:
- A UFCS / module-qualified call as the tail (`obj.method()`) parses as a
  `CallExpr` here (expression context), not the `CallStmt` that
  `parseStatement` would yield — so it is usable as a value, sidestepping
  the CallStmt-vs-ExprStmt trap.
- **case-vs-if tail asymmetry is intentional (an #1891 scope boundary)**:
  case-expr block tails accept identifier-starting expressions; `if`-expr
  block tails still require parentheses. Bringing `if` to parity means
  giving `parseIfExpressionBranchBody` the same expression-first tail
  loop — deliberately out of scope for #1891, and documented in
  `docs/reference/control-flow.md`.

**How to apply**: any future block-valued expression form that wants an
identifier-starting tail should adopt this expression-first/restore
pattern rather than `parseBlock`. Keep the double-rewind (catch →
`restoreState`; expr-but-not-tail → `restoreState` → `parseStatement`) so
genuine statement diagnostics are never swallowed by the speculative
`parseConditional`.

### Identifier-trailing `!` tokenization must exclude every multi-char operator starting with `!`

**Source**: #1211 (2026-04-20, bug fix); updated #1568 (`!!` operator removed)
**Tags**: parser, lexer, identifier, trailing-bang, ambiguity

**Context**: The lexer greedily absorbs a trailing `!` into an
identifier to support mutating method names (`sort!`, `reverse!`,
`append!`, `clear!`). Two-character operators that begin with `!` must
be excluded from this absorption, otherwise `r<op>` mis-tokenizes as
`r!` (Ident) + the operator's tail. The currently active such operator
is `!=`. (#1211 originally added `!!` to the exclusion as well; #1568
removed the `!!` operator entirely, so the exclusion is now `!=`-only.)

**Rule**: The trailing-bang absorption in the identifier branch
(`src/lexer/lexer.cpp` identifier tokenization) must exclude **every**
multi-character operator token that begins with `!`, not just whatever
operators happen to be present today. Any future operator starting with
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

**Rule**: The `parsePattern` `LParen` branch uses the same grouping-vs-tuple disambiguation as the expression parser (`src/parser/parser_expr.cpp`):
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

### Tuple-destructure LHS names must be camelCase (with `_` placeholder allowed)

**Source**: #1450 (2026-04-30, implementation)
**Tags**: parser, tuple, destructure, casing, camelCase, identifier, isCamelCase

**Rule**: Both forms of tuple destructure assignment in `src/parser/parser.cpp` enforce camelCase on every LHS name via `isCamelCase`, with `_` accepted as a placeholder at any position:

- Parenthesized form (`(a, b) = expr`): both the first name and every name read inside the comma loop are validated immediately after consuming the `Ident` token.
- Bare form (`a, b = expr`): the first name is `first.value` (already consumed by the outer `Ident` dispatch in `parseStatement`) and must be checked there too — a position the rest-only sweep would miss. Each rest name is checked inside the comma loop.

The error wording is `"tuple-destructure name '<n>' must be camelCase"`, matching the established pattern used by `src/parser/parser_decl.cpp` (`fn name '...' must be camelCase`, `parameter name '...' must be camelCase`, `field name '...' must be camelCase`, etc.).

**Why**: Pre-#1450 the two tuple-destructure sites were the last LHS-binding parser sites that accepted snake_case identifiers, even though every other binding site (`fn` decl, `let`/assign LHS via `parseAssignTarget`, lambda params, record fields, enum variant fields, loop variables) had been migrated to camelCase by #1409 / #1443 and follow-ups. The casing rule applies to all identifiers introduced into the local scope, and tuple destructure introduces them — so consistency demanded the same enforcement here.

**How to apply**:
- Both forms have a `first` position consumed before the comma loop and a `rest` position consumed inside it. A guard at only one position passes the other-position regression test for the wrong reason; lock both with separate negative tests (see `BareTupleDestructRejectsSnakeCaseFirst` vs `BareTupleDestructRejectsSnakeCaseRest`).
- The `_` placeholder is a floor (must remain accepted), not a ceiling (allowed only in bare form). It is accepted in both forms at any position because `(_, b)` was already valid pre-#1450 via `ParenTupleDestructWildcard`.
- The tuple-destructure parse path is gated by `looksLikeParenthesizedTupleDestructure()` (paren form) or the outer `Ident` dispatch (bare form) — neither sits inside a `try / catch (...)` speculative wrapper, so `parseError` propagates as a user-visible diagnostic. The commit-flag pattern from #1449 does **not** apply here.

### Module-global typed-decl `name: Type = value` enforces camelCase, with `@native`/`@const` SCREAMING_SNAKE_CASE carve-out

**Source**: #1470 (2026-04-30, implementation)
**Tags**: parser, typed-decl, module-global, camelCase, screaming-snake-case, native, const, isCamelCase

**Rule**: The keywordless implicit-binding form `name: Type = value` (parsed in `parseStatement` at the `Ident :` branch in `src/parser/parser.cpp`) enforces `isCamelCase(name)` on the LHS identifier. SCREAMING_SNAKE_CASE is accepted only when the declaration carries a `@native` or `@const` directive, matching the established stdlib convention for built-in / module-level constants (`PI`, `E`, `INF`, `NAN`). The error wording is `"variable name '<n>' must be camelCase (or SCREAMING_SNAKE_CASE for @native or @const variable names)"`.

**Why**: Pre-#1470 this site was the last LHS-binding parser site that silently accepted `snake_case` identifiers, even though every other binding site (`fn` decl, lambda params, record fields, tuple-destructure LHS) had been migrated to camelCase by #1443 / #1449 / #1450. The site is shared between top-level and block contexts (`parseStatement` is called from both), so the fix propagates uniformly to function bodies as well, completing the v0.0.16 naming convention rollout.

**How to apply**:
- The carve-out gate is `hasDirective(directives, "native") || hasDirective(directives, "const")`. Do **not** broaden it to allow PascalCase: PascalCase is reserved for type names (records / enums / type aliases) and would create asymmetry with the `fn`-name carve-out at `src/parser/parser_decl.cpp:130` which only allows camelCase or SCREAMING_SNAKE_CASE.
- Stdlib `share/std/math/math.ry` constants must be SCREAMING_SNAKE_CASE — `Inf` and `NaN` were renamed to `INF` and `NAN` in the same PR for this reason. Future stdlib constants follow the same convention.
- Mathematical concept names (`NaN`, `±Inf`) in prose / docstrings remain unchanged — only the Ry identifier exports were renamed.

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

### User-defined directives applied outside `target=[...]` are silent no-ops

**Source**: #1425 (2026-04-28, implementation)
**Tags**: codegen, directive, target, silent-no-op, validateDirectives, allowed_targets, parser-asymmetry

**Rule**: When a user-defined directive declared via `@directive(target=[...])` is applied to a node whose kind is **not** in the declared target list, codegen silently skips the directive — no error, no warning, no effect. `validateDirectives()` (`src/codegen_fn.cpp`) takes a `DirectiveTarget current` parameter; for each user-defined directive it `continue`s when `sig.allowed_targets != 0 && !hasTarget(sig.allowed_targets, current)`, so `validateDirectiveSignature()` is never called and any future effect-firing logic guarded by the same check stays inactive too. Argument validation is therefore also skipped on a target mismatch, which is intentional and matches the v0.0.15 spec — a later minor may upgrade this to a warning.

**Why**: v0.0.15 wants `@directive(...)` to support tag-style usage where a directive declared for one site can be sprinkled on adjacent sites without breaking compilation. Erroring out would force users to declare a separate directive per target; warning would require a noise budget the project does not yet have. Skipping silently keeps the door open for future warning escalation while preserving today's "harmless" semantics.

**How to apply**:
- Every call site of `validateDirectives()` must pass an explicit `DirectiveTarget`. Today the call sites are: `FnStmt` (Function), `RecordStmt` and field directives in `emitStmt(RecordStmt)` (Record, Field), `AssignStmt` and `TupleDestructStmt` (Statement), `CallStmt` (Statement, reachable from user source after #1427), and `ForStmt` (ForLoop).
- Built-in directives go through the `validateDirectiveArgs()` registry path inside `validateDirectives()` and are unaffected by the target check — that mismatch detection is intentionally out of scope (#1425 "Out of scope").
- The `sig.allowed_targets != 0` guard is defensive: parser already requires `target=[...]` to be non-empty (`src/parser/parser_decl.cpp` rejects the empty list), but if a future change ever permits "any target", `allowed_targets == 0` will fall back to validate-everything rather than silently skip.

**Parser-side asymmetry — resolved by #1427**: Until #1427, `src/parser/parser.cpp:460-462` and `:718-720` rejected every user-defined directive on `for` statements (only `@parallel` allowed) and on function-call statements (only the special `@each` / `@property` on `it(...)` form allowed) at parse time, making the silent-no-op behavior unobservable at those sites. #1427 replaced both gates with a `builtinDirectiveRegistry()` membership check: only registry-tracked directives (today: `@native` only) are rejected, and user-defined directives pass through to codegen `validateDirectives()` where the silent-no-op rule applies. The `ForLoop` and `Statement` codegen wiring added defensively in #1425 is now reachable from user source. Tests `ForLoopAcceptsUserDirectiveAsSilentNoOp` / `CallStmtAcceptsUserDirectiveAsSilentNoOp` (formerly `…RejectsUserDirectiveAtParseTime`, flipped per the "Relaxing a rejection branch …" rule in `.claude/skills/test-checklist/SKILL.md`) lock in the new permissive behavior; `ForLoopRejectsBuiltinNativeDirective` / `CallStmtRejectsBuiltinNativeDirective` / `ForLoopRejectsMultipleParallel` lock in the restrictions that remain.

### Formatter→parser roundtrip: `TupleDestructStmt` must not emit `: ` between pattern and `=`

**Source**: #1189 (2026-04-19, implementation)
**Tags**: formatter, parser, tuple, destructure, roundtrip, latent_bug

**Rule**: `formatTupleDestruct()` in `src/formatter_stmt.cpp` must emit only `<pattern> = <value>` (plus optional `@const` directive on a prior line). Do **not** emit a stray `: ` between the closing `)` of the pattern and the `=`. The immutability is conveyed by the `@const` directive emitted before the statement, not by a `:` suffix on the LHS.

**Why**: Until #1189 landed, the parser rejected all parenthesized tuple-destructure forms, so the formatter's output `(a, b):  = (1, 2)` never round-tripped through parse. Enabling the parenthesized parse branch exposed the latent `: ` bug — formatted output now fails `ry fmt` verification ("formatted output failed to re-parse"). Adding `FormatterTest.ParenTupleDestructRoundTrip` locks this in so future formatter edits cannot regress.

**How to apply**: When adding or modifying a formatter rule for a new statement shape, grep for a matching parser spec test and add a `verifyFormatting` / roundtrip assertion. Formatter output that fails to re-parse is a silent correctness bug during `ry fmt`; only the verification pass catches it.

### Speculative `try { parseX() } catch (...) { fallback }` needs a commit-flag for hard validation errors

**Source**: #1449 (2026-04-29, implementation — advisor call-out)
**Tags**: parser, speculative-parse, try-catch, lambda, commit-flag, diagnostic-wording

**Context**: `parsePrimary`'s lambda dispatch wraps `parseParenLambdaExpr()` in `try { ... } catch (...) { lex_.restoreState(...); }` so that `(a, b)` (tuple) and `(a, b) => a + b` (lambda) can share a prefix and fall back if the lambda parse fails. When #1449 added a hard `isCamelCase` check on lambda param names, throwing inline inside the param loop got swallowed by that catch — the lexer rewound to before `(`, the statement parser then re-saw `f = (my_x, my_y) => ...` and emitted the wrong diagnostic ("expected '=', '+=', ... after identifier") instead of the intended `parameter name 'my_x' must be camelCase`.

**Rule**: When you need a hard validation error (one that is **not** an "ambiguity ⇒ try the other branch" signal) inside a function whose caller wraps it in a speculative `try / catch (...)`, introduce a member commit-flag (e.g. `lambda_committed_`) on `Parser`:

1. **Defer the validation** until you have consumed the disambiguator that proves the speculative branch was the right one (for `parseParenLambdaExpr` that is `')'` followed by one of `->` / `=>` / `:`). Collect the data you need to validate (e.g. `paramNameTokens`) up to that point but do not throw yet.
2. **Set the flag** the instant you confirm commitment, **after** the disambiguator check that decides whether to fall back. Setting it before the disambiguator turns the fallback path into a hard error too (e.g. tuple `(a, b)` with no body marker would lose its fall-through to `parseTuple`).
3. **Re-throw past the catch** with `if (committed_) { committed_ = prev; throw; } committed_ = prev; lex_.restoreState(...);`. Save and restore the previous flag value so nested speculative parses (e.g. lambda inside lambda body) compose correctly.

**Why a member flag, not an exception subclass**: Using `throw RealParseError` vs `throw SpeculativeFailure` would also work, but every existing `parseError(...)` site in the parser throws the same type, so introducing a hierarchy would require tagging every call site — the flag is a 5-line change confined to the speculative branch.

**How to apply**:
- Reference site: `src/parser/parser_expr.cpp::parseParenLambdaExpr` (commit flag set just past `)` and the `'->' / '=>' / ':'` lookahead) + `src/parser/parser_expr.cpp::parsePrimary` (save/restore + conditional re-throw in the lambda dispatch). The flag itself lives on `include/ry/parser/parser.hpp`.
- The disambiguator-then-flag ordering matters: if you set the flag before the lookahead check, valid tuples with snake_case names like `(my_a, my_b)` would be rejected even though they have no lambda body marker. Tests `LambdaParamRejectsSnakeCase` (negative) and `LambdaParamAcceptsCamelCase` (positive) lock both halves.

### Multiline postfix `.` continuation needs `chainIndents` balanced-Dedent drain to keep the surrounding block intact

**Source**: #2115 (2026-06-13, implementation — advisor call-out)
**Tags**: parser, postfix, ufcs, multiline, indent-dedent, save-restore, block-boundary, blind-spot

**Context**: `parsePostfixContinuation` (`src/parser/parser_expr.cpp`) historically exited on any token other than `Dot`/`LBracket`/`Question`, so the documented multiline UFCS idiom in `docs/reference/collections.md` "Lazy Method Chaining" — `result = xs\n    .iter()\n    .toList()` — failed with `"unexpected token '.'"`. The naive fix (`lex_.saveState()` + skip `Newline`/`Indent`/`Dedent` looking for `.`, commit on Dot / restore otherwise) handles the dispatch but leaves a hole: when the speculative pass commits an `Indent` (because the continuation line is at a deeper column), the matching `Dedent` lands AFTER the chain ends. `parseBlock` (`src/parser/parser.cpp:1059`) reads the chain's trailing `Newline`, then sees `Dedent`, and exits the surrounding block prematurely. At top level the equivalent failure is `parseProgram` seeing a stray `Dedent` it cannot route into a statement.

The lexer (`src/lexer/lexer.cpp:189-202`) emits `Indent`/`Dedent` by raw column change regardless of context — there is no bracket-depth suppression — so the asymmetry is real, not avoidable by lexer tuning.

**Rule**: When extending `parsePostfixContinuation` (or any other expression-internal speculative consumer of structural tokens) with multiline `.` continuation, track the net `Indent` count the chain consumed via a local counter (`chainIndents`) and **drain matching `Dedent`s before returning**. The drain consumes intervening `Newline`s and `Dedent`s until `chainIndents` returns to 0; on any other token, stop and leave the rest in the stream.

Algorithm:

1. `auto saved = lex_.saveState(); int savedChainIndents = chainIndents; bool sawNewline = false;`
2. Loop: consume `Newline` *only when* `!sawNewline` (set `sawNewline = true`) / `Indent` (`++chainIndents`) *only when* `sawNewline` / `Dedent` (`--chainIndents`) *only when* `sawNewline && chainIndents > 0`; break on anything else.
3. If now `sawNewline && peek == Dot` → commit (do not restore). Outer loop dispatches on the `Dot`.
4. Else → `lex_.restoreState(saved); chainIndents = savedChainIndents; break;`.
5. After the outer loop exits, drain matching trailing Dedents: while `chainIndents > 0`, consume `Newline` or `Dedent` (decrementing on Dedent); break on anything else (defensive).

The `chainIndents > 0` guard inside the speculative skip is critical — without it, a Dedent that belongs to the surrounding block (e.g. the function-body end after the chain's last call) is greedily consumed during speculation, and the restore can only put it back into the lexer state (not into the parser-side counter), so the matching cleanup later would over-drain. With the guard, Dedents the chain didn't "earn" are left alone for the surrounding context.

**Single-Newline limit (no blank-line continuation)**: the `sawNewline` flag caps the absorbed Newline count at one. A blank line between the expression and the `.` (`x = xs\n\n.toList()`) is intentionally rejected as a statement separator, matching the Swift / JS / Kotlin convention and the single-Newline form documented in `docs/reference/collections.md`. Allowing multiple Newlines would silently let blank lines change statement boundaries — surfaced by CodeRabbit during #2115 review and locked in by `ParserTest.UfcsMultilineChainRejectsBlankLineSeparator`.

**Comment-only lines must be transparent at the LEXER, not the drain loop (#2137)**: comment-only lines (`    # skip empty` between hops) produce **the same token stream** as a blank line at the parser — both materialise as two consecutive `Newline`s — so the drain loop cannot discriminate them. The fix lives in `src/lexer/lexer.cpp::Lexer::readToken()`: snapshot `entered_at_line_start = at_line_start_` at the top, and after Step 4's comment skip, if `entered_at_line_start && peek == \n` consume the trailing newline and `return readToken();` — comment-only lines emit no token at all. The buggy input `xs\n    # c\n    .iter()` then reduces to byte-identical token stream as `xs\n    .iter()`, so the drain loop's single-Newline cap, the `chain_pending_dedents_` accounting, and every downstream Newline-counting site (`parseProgram`, `parseBlock`) behave identically to the no-comment case. Critically, blank-line rejection is preserved because the suppression fires ONLY when the first non-whitespace char is `#` — blank lines still emit their `Newline`. Mid-line comments (`foo() # x`) have `entered_at_line_start = false` and fall through to the normal `Newline` emit. Locked in by `ParserTest.UfcsMultilineChainCommentBetweenHopsTransparent`, `…MultipleCommentsBetweenHopsTransparent`, plus the boundary negatives `…RejectsBlankThenCommentSeparator` / `…RejectsCommentThenBlankSeparator` — all in `tests/test_parser.cpp`.

**How to apply (future bugs in this family)**: when an alleged "drain loop should also skip X" surfaces, FIRST check whether X is token-equivalent to an existing rejected case (blank line). If yes, the discriminating info lives only in the lexer — do not relax the drain loop's Newline cap (it would silently re-accept the rejected case). Look for a lexer-side fix that removes the offending token from the stream entirely.

**Why ONLY `.` continues, not `[` / `?`**: `[` on a continuation line is ambiguous with a fresh list-literal statement; `?` rarely makes sense as a leading postfix on a new line. Both are excluded conservatively.

**Lexer-side limitation — `. INTEGER` (tuple-index) is not supported on continuation lines**: `src/lexer/lexer.cpp:452-458` tokenizes `.<digit>` as a Float literal whenever `prev_kind_` is not in the explicit exclusion list, and `Newline` / `Indent` / `Dedent` are not in that list. So `xs\n.0` reaches the parser as `xs Newline Indent Float(".0")` — the `Dot` lookahead never fires. Relaxing this requires a separate lexer change (add structural-token kinds to the exclusion list); for now the `docs/grammar.ebnf` comment splits the postfix `.` rule into `[ NEWLINE { INDENT | DEDENT } ] '.' IDENT` (multiline-tolerant) vs `'.' INTEGER` (single-line only) so the asymmetry is documented for future audits. `ParserTest.UfcsMultilineChainTupleIndexFailsAsLimitation` (`tests/test_parser.cpp`) locks this in.

**tree-sitter blind spot — `optional($._newline)` on its own is a major regression**: extending `field_access` with `optional($._newline)` in `editor/tree-sitter/grammar.js` causes tree-sitter's GLR parser to speculatively consume every statement-terminating `_newline` as a continuation prefix; baseline `check.sh` drops from 157 PASS / 0 FAIL to 11 PASS / 146 FAIL across the spec corpus. The conflict is between `_newline` as statement terminator (in `block`/`source_file`) and `_newline` as chain continuation marker (in `field_access`). Resolving it requires a more sophisticated grammar structure (dynamic precedence or a dedicated multiline-field-access variant gated by a lookahead trick); #2115 explicitly defers the tree-sitter side to a follow-up issue. To avoid degrading the editor view of existing spec files, the multiline-UFCS test cases live in a dedicated spec file `tests/spec/expr_stmt_multiline_ufcs.test.ry` (registered in `editor/tree-sitter/expected-fail.txt`) rather than appended to `expr_stmt.test.ry`.

**Stall case (#2136 — drain cannot complete when the chain ends on a non-structural token)**: When the chain ends on `>`, `:`, `)`, `,` etc., the tail Dedent drain cannot consume the matching Dedents because they have not been emitted yet (they appear after the surrounding sub-expression closes). The original #2115 implementation silently discarded `chainIndents` on stall, leaving the surrounding `parseBlock` to either see no Indent token (degenerate case: the chain's continuation column equals the body's column — `if xs\n    .count() > 0:\n    print(1)`) or to encounter a stray Dedent at statement / program boundaries (deeper-body case, binary RHS case, call-arg case).

The #2136 fix promotes `chainIndents` to a Parser member (`include/ry/parser/parser.hpp::chain_pending_dedents_`). On stall the remaining count stays on the member; on subsequent block boundaries the parser consumes against it:

1. `src/parser/parser_expr.cpp::parsePostfixContinuation`: the speculative skip and tail drain both read/write the member. The speculative skip saves/restores the member on speculative failure (alongside `lex_.saveState`/`restoreState`).
2. `src/parser/parser.cpp::parseBlock`: the Indent check has three branches — real `Indent` consumed normally, else drain stream Dedents against `chain_pending_dedents_` first (cpd≥2 case) and treat any remaining cpd as one implicit Indent, else `expected indented block`. After this, snapshot `entry_pending = chain_pending_dedents_`; the body loop's drain only consumes Dedents while `cpd > entry_pending` so the block's own exit Dedent is left for the close. The two-step (drain Dedents → implicit-Indent the residue) is required because a single decrement leaves a stray Dedent in the stream for cpd≥2 — caught by `ParserTest.UfcsMultilineChainInIfCondDoubleHop`.
3. `src/parser/parser.cpp::parseProgram`: drain Dedents while `chain_pending_dedents_ > 0 && peek == Dedent` at the top of the statement loop. Top-level has no "own" Indent so `entry_pending` is implicitly 0.
4. Speculative parse sites that transitively call `parsePostfixContinuation` (and may absorb chain Indents) must save and restore the member on the **lexer-restore path only** (committed/throw paths propagate state as part of real parse output). The four such sites are:
   - `src/parser/parser_expr.cpp` lambda dispatch in `parsePrimary` (the `(...)` could-be-lambda branch — try/catch around `parseParenLambdaExpr()`)
   - `src/parser/parser_expr.cpp` case-arm tail-vs-stmt speculative `parseConditional()` in `parseCaseExprArmBody`
   - `src/parser/parser_expr.cpp` `Ident[T](args)` generic call (try block calling `parseArgList()`)
   - `src/parser/parser_expr.cpp` `Ident<T>::Variant(args)` generic-enum dispatch (try block with committed-flag, calling `parseArgList()`)

Pure-lookahead sites (`couldBeLambda`, `looksLikeParenthesizedTupleDestructure`, `parseCastTypeName`, `couldBeGenericEnum`, the enum-variant named-field 1-token lookahead in `src/parser/parser_decl.cpp`) consume tokens directly without invoking expression parsing — they do not need the cpd save/restore. Adding a new speculative site that reaches expression parsing is a regression risk; the rule of thumb is "any `lex_.saveState()` whose try block calls `parseConditional` / `parseArgList` / `parseParenLambdaExpr` / `parsePostfixContinuation` must add the cpd save/restore in matching restore paths."

**How to apply**:
- Reference site: `src/parser/parser_expr.cpp::parsePostfixContinuation` (the `chain_pending_dedents_` counter + speculative skip + tail Dedent drain pattern). The C++ side is self-contained — no helper extraction needed because the only call site is this function.
- Any future expression-internal feature that speculatively skips structural tokens (e.g. a multiline `if`-expression body, multiline `case` scrutinee) must replicate the `chain_pending_dedents_ > 0` Dedent guard + trailing-drain + member-stall pattern, or the surrounding block will exit early on the unbalanced Dedent / fire `expected indented block` on the absorbed Indent.
- Adding a new speculative parse site that may transitively touch `parsePostfixContinuation`: extend the save/restore set above. The four-site list is the current closed set; any addition must come with both a test exercising chain-in-the-speculative-body and a docs update here.
- Other `peek != Indent` checks in declaration headers (`fn` body / record / enum / case-stmt arms — `src/parser/parser_decl.cpp` and a few sites in `src/parser/parser_expr.cpp`) carry the same latent risk for multiline UFCS in their respective header positions, but were out of scope for #2136 (no reproducer reported). When extending coverage there, follow the same `parseBlock` 3-branch shape.
- For tree-sitter follow-ups, do NOT default to `optional($._newline)` between a postfix operator and `.`; investigate dynamic-precedence (`prec.dynamic`) or a dedicated `field_access_multiline` variant gated by lookahead before committing.

### `in_if_cond_` flag suppresses bare-ident `Ident FatArrow` dispatch inside if-expression conditions

**Source**: #1572 (2026-05-04, implementation)
**Tags**: parser, lambda, if-expression, bare-lambda, fatarrow, lookahead, ambiguity, blind-spot

**Context**: #1572 added bare-paren-omitted single-param lambda dispatch (`s => expr`) in `parsePrimary`'s `Ident` branch. Without a guard, the dispatch fires inside the cond of `if cond => then else else`, consuming `=>` as a lambda body marker and breaking every existing `if flag => Some(1) else None()` site (`tests/spec/option_branch_merge_none.test.ry` alone has 14+ such sites).

**Rule**: `parsePrimary`'s `Ident` branch must guard the bare-lambda dispatch with `lex_.peek().kind == TokenKind::FatArrow && !in_if_cond_`. The flag is set by `parseIfExpression` only around the `parseConditional()` call that parses the **condition**:

```cpp
ExprPtr Parser::parseIfExpression() {
    Token ifTok = lex_.next();          // consume 'if'
    bool prev_in_if_cond = in_if_cond_;
    in_if_cond_ = true;
    ExprPtr cond = parseConditional();  // bare lambdas suppressed here
    in_if_cond_ = prev_in_if_cond;
    // ... '=>' is consumed by parseIfExpression itself as the then-arm marker.
    // The then / else arms run with the saved (typically false) state, so
    // bare lambdas inside them remain accepted.
}
```

**Why a flag, not a richer lookahead**: bare-ident `s` followed by `=>` is locally indistinguishable between "bare lambda" and "if-cond identifier followed by then-arm". Only the surrounding parser state knows which is in scope. A peek-based heuristic would have to scan past the entire then/else arms to find the matching `else` keyword (expensive and still ambiguous with nested `if`s in the cond), or commit prematurely and break existing spec.

**Why save/restore (not unconditional `false` on exit)**: nested if-expressions (`if outer => if inner => x else y else z`) and lambdas inside then/else arms (`if c => x => x*2 else y => y*2`) must compose correctly. Restoring the previous value (rather than clobbering to `false`) preserves the outer context.

**Why no commit-flag**: `Ident FatArrow` is a 2-token commit point with no fallback path. The bare-lambda branch is **not** wrapped in a speculative `try / catch (...)`, so the commit-flag pattern from the entry above does **not** apply — `parseError` for `isCamelCase` violations propagates as a hard diagnostic immediately, and there is no other parse to fall back to.

**How to apply**: Any future `Ident <suffix>` dispatch added in `parsePrimary` (hypothetical `s @ pattern`, typed-binding shortcuts, postfix builder shorthands, etc.) that conflicts with an outer-context production must replicate this pattern:

1. Add a `bool in_<context>_ = false;` member to `Parser` in `include/ry/parser/parser.hpp`.
2. Set/restore the flag at the call site that owns the conflicting production (`parseIfExpression` for if-cond, `parseCaseExpression` for case-scrutinee, etc.).
3. Guard the new `parsePrimary` dispatch with `&& !in_<context>_`.

Reference site: `src/parser/parser_expr.cpp::parsePrimary` Ident branch (bare-lambda dispatch with the `!in_if_cond_` guard) + `src/parser/parser_expr.cpp::parseIfExpression` (save/restore around `parseConditional` for the cond). The flag itself lives on `include/ry/parser/parser.hpp` next to `lambda_committed_`. Tests `BareLambdaPreservesIfExpressionWithBareIdentCond` and `BareLambdaInIfThenElse` in `tests/test_parser.cpp` lock both halves of the invariant.

### UnaryExpr fast-path covers bare int for INT64_MIN (`-9223372036854775808`)

**Source**: #1025 (2026-04-16)
**Tags**: codegen, numeric-literal, unary-minus, int64-min

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

### `formatExprInner` literal branches: escape strategy depends on whether the lexer decoded or preserved escapes — and `verifyFormatting`'s text-idempotency check does NOT catch a dispatch miss

**Source**: #2113 (2026-06-13, fix)
**Tags**: formatter, regex-literal, string-literal, escape, lexer-asymmetry, verifyFormatting, blind-spot

**Context**: `formatExprInner` in `src/formatter.cpp` dispatches `ExprNode` variants via an `if constexpr` chain ending in a `/* unknown expr */` fallthrough. `StringExpr` routes its value through `escapeString` because the lexer **decoded** `\n` / `\t` / `\\` / `\"` / `\0` into the corresponding bytes (`src/lexer/lexer.cpp` string branch). `RegexExpr` looks like a sibling literal, but the lexer **preserves** the two-byte sequence `\` + next-char verbatim — `/\d+/` produces `pattern = "\d+"` (3 bytes: `\`, `d`, `+`). The only translation the regex lexer performs is `\0` → a single NUL byte. So `RegexExpr` must emit `v.pattern` verbatim and only reverse the NUL translation; routing through `escapeString` would turn `\d` into `\\d`, changing the regex semantics silently.

Why `Formatter::verifyFormatting` (`src/formatter.cpp:778-791`) does **not** rescue a missing dispatch arm: the check is **text idempotency** (`formatSource(formatSource(x)) == formatSource(x)`), not AST equivalence. `/* unknown expr */` appears in a value-token context after a comma (`replace("…", X, "…")`), where the lexer treats the leading `/` as the start of a regex literal (the preceding `,` is not in the lexer's value-producing exclusion set). The whole sequence parses as `RegexExpr{pattern: "* unknown expr *"}`, which the formatter then re-emits via the same fallthrough into the **same** `/* unknown expr */` text. The output is a perfect fixed point of the formatter, so idempotency passes — but the runtime regex engine fails to compile `* unknown expr *`. Likewise inside string-arg position the comment-form is its own fixed point. A missing dispatch arm therefore ships silently.

**Rule**:

1. When adding a new literal kind to `formatExprInner`, look at the lexer's storage form for that kind before choosing the escape strategy:
   - lexer **decoded** the escapes (escape sequences became bytes) → route through `escapeString` (the StringExpr pattern);
   - lexer **preserved** the escapes (two-byte `\<x>` kept as-is) → emit `v.<field>` verbatim and only undo any one-off translations (for `RegexExpr` that is NUL → `\0`).
2. The discriminating test for any verbatim-emit branch is a backslash-bearing input that is *not* a single-char escape (`/\d+/` works because the lexer keeps both bytes; a string literal `"\d+"` would be a parse-time decision and not the same case). The round-trip test must use a backslash sequence that survives the lexer two-byte preserved, because that is the form `escapeString` would corrupt.
3. Do not rely on `verifyFormatting` to catch a dispatch miss. The `/* unknown expr */` fallthrough is a fixed point of the formatter in every value-token context (after `=`, `,`, `(`, `[`, etc.). Each new variant added to `ExprNode` must come with at least one direct round-trip test in `tests/test_formatter.cpp`.

**How to apply**:
- Same site whenever extending `formatExprInner`: insert the branch next to siblings, then immediately add a `tests/test_formatter.cpp` test that includes a backslash-bearing input. The test is the primary guard — `verifyFormatting` is not.
- If a future PR makes `verifyFormatting` AST-aware (current scope rejects it — fixing it would also need a roundtrip-AST equality helper that ignores doc-comment whitespace), this rule's #3 can be relaxed. Until then, treat the safety net as text-level only.
- Adjacent invariant: `src/lexer/lexer.cpp` regex branch (lines 325–360) and `RegexExpr::pattern` (`include/ry/ast/ast.hpp`) form a contract — backslash-anything is two-byte preserved, only `\0` is one-byte translated. A future addition of regex flags (`/pattern/i`) or a new translation would invalidate the "NUL is the only lossy translation" assumption and the `RegexExpr` formatter branch must be updated in the same PR.

### `formatExprInner` postfix concatenation can silently fuse adjacent tokens into a longer operator — guard on the trailing character of the inner text

**Source**: #2114 (2026-06-13, fix)
**Tags**: formatter, postfix, token-fusion, lexer-greedy, verifyFormatting, blind-spot, error-propagate, safe-index

**Context**: `formatExprInner` emits `ErrorPropagateExpr` as `formatExpr(operand) + "?"`. When the operand is itself an `IndexExpr` with `try_mode=true` (formatted as `xs[i]?`) or another `ErrorPropagateExpr` (`a?`), the bare concatenation produces `xs[i]??` / `a??`. The lexer's `?` branch (`src/lexer/lexer.cpp` line 494-500) is greedy: a `?` immediately followed by `?` (no whitespace check) collapses into a single `TokenKind::QuestionQuestion` (the null-coalescing binary operator). Re-parse then sees `xs[i] ??` and expects an RHS, failing with "unexpected token" — a hard re-parse error from `verifyFormatting`. Unlike the #2113 dispatch-miss above, this one is NOT a silent fixed point; the existing `verifyFormatting` test path does catch it once a round-trip test exists. The fix is a one-line check: if `inner.back() == '?'`, emit `inner + " ?"` instead of `inner + "?"`.

**Rule**:

1. Any postfix character whose 2× form is itself a multi-character lexer token (today the only postfix-emit case is `?` → `??`; the closest sibling is the unary-prefix `-` → `--` / `+` → `++` adjacency on lines 296-302, but that is a prefix branch, not a postfix one) must be guarded against adjacency fusion when concatenated after `formatExpr(operand)`. The text-level check `inner.back() == '<char>'` is the canonical guard — a structural type check is brittle because new AST nodes ending in the same character (or a transformed expression that happens to expose the same trailing char) silently bypass it.
2. The fuse-risk character set is determined by the lexer, not the formatter. Before touching a postfix branch in `formatExprInner`, grep `src/lexer/lexer.cpp` for the multi-char rule and confirm whether the formatter's natural concatenation can resurrect that fusion. Today the only postfix-emit branches at risk are `ErrorPropagateExpr` and `IndexExpr.try_mode` (both ending in `?`); the unary prefix branch (`-`/`~`/`++`/`--` at `src/formatter.cpp` line 296-302) has the same class for `- -x` / `--x` and is treated separately (out of scope for #2114, tracked if surfaced via `/triage-side-finding`).

**How to apply**:
- Reference fix site: `src/formatter.cpp` `ErrorPropagateExpr` branch (formerly line 444-445) — `if (!inner.empty() && inner.back() == '?') return inner + " ?";` before the default `return inner + "?";`. The space is load-bearing — the lexer skips it and emits two `Question` tokens (whitespace gates the greedy match).
- Test pattern: `tests/test_formatter.cpp` `FormatterTest.OptionPropagateAfterSafeIndexAdjacency` covers the IndexExpr case (the issue's repro); `FormatterTest.OptionPropagateAfterPropagateAdjacency` covers the nested `ErrorPropagateExpr` case that forces the general fix over an IndexExpr-only check. Both assert the formatted string contains `? ?` (space-separated) AND that `verifyFormatting` re-parses cleanly. Adding only the IndexExpr test would silently pass under a narrow structural-type fix while still shipping the nested-`?` bug.
- Adjacent invariant: `docs/reference/operators.md` line 207 ("Footgun — `??` greedy tokenization") documents the user-facing rule that `m["k"]? ?? default` requires a space. The formatter must always emit the spelling that obeys that rule on round-trip; a future lexer change that adds whitespace-insensitive `??` parsing would invalidate this guard.

