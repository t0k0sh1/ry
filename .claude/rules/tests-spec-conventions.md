---
paths:
  - "tests/spec/**/*.test.ry"
---

# Tests — Spec Conventions

### `case` arms in `.test.ry` files must have a non-empty body

**Source**: #804 (2026-04-14, implementation)
**Tags**: testing, case, option, ry-syntax

**Rule**: Every arm of a `case` expression in Ry must have at least one statement. An arm with no body (e.g. `None:` followed immediately by the closing `)`) causes a parser error pointing at the enclosing `describe` or `it` call — which can be confusing.

```ry
# ❌ Parser error — empty arm not allowed
case opt:
    Some(v): expect(v).toEq(1)
    None:           # ← triggers "unexpected token ')'"

# ✅ Use a flag variable to verify the None path
gotNone = false
case opt:
    Some(v): fail("unexpected Some")
    None: gotNone = true
expect(gotNone).toEq(true)
```

### `expect(str).toEq("literal")` is NUL-truncating — use `expect(str == "literal").toEq(true)` for NUL-containing strings

**Source**: PR #1048 and #1049 (CodeRabbit review). **Tags**: testing, NUL-safety, codegen_test

`toEq` for string values emits a `strcmp` call (`codegen_test.cpp:784` via the `isStringValue` branch).
`strcmp` stops at the first `\0`, so `expect(substr("a\0b", 0, 3)).toEq("a\0b")` passes even
when `substr` returns `"a"` — both C-strings compare equal as `""` / `"a"` depending on content.

**Why**: The `==` operator between two `str` values routes through `emitComparisonOp`
(`codegen_expr.cpp:1016`) → `__ry_str_cmp` (byte_len + memcmp), which is NUL-safe.
The `toEq` matcher is a separate code path that does not reuse that logic.

**How to apply**: When the expected value contains an embedded `\0`, write the assertion as:
```ry
expect(expr == "a\0b").toEq(true)   # NUL-safe: routes through __ry_str_cmp
# NOT:
expect(expr).toEq("a\0b")           # NUL-truncating: strcmp stops at \0
```
Assertions whose expected value has no embedded NUL are safe to leave as `toEq("literal")`.
Only `toHaveLen` and `toBeEmpty` are NUL-safe matchers besides `toEq(bool)`.

### Ry expect matchers: use `toNotContain`, not `notToContain`

**Source**: PR #1054 (fix/1054-nul-safety-c-boundaries). **Tags**: testing, ry-syntax, matchers

**Rule**: The correct negating form for string containment in Ry expect matchers is
`toNotContain("...")`, not `notToContain("...")`. Using `notToContain` compiles but calls
a non-existent method at runtime, producing an "undefined method" error.

```ry
# ❌ Wrong — runtime error
expect(e.message).notToContain("NUL")

# ✅ Correct
expect(e.message).toNotContain("NUL")
```

### Ry case arms: `()` unit expression and `if <var>:` as sole body cause parse failures

**Source**: PR #1054 (fix/1054-nul-safety-c-boundaries). **Tags**: testing, case, parser, unit, if

**Rule**: Two patterns in case arm bodies trigger parser errors that manifest as a confusing
"unexpected token ')'" at the nearest enclosing `describe(..., ():` or `it(..., ():` line:

**Pattern 1: `()` as a case arm body**
```ry
# Fails — parser sees ')' and thinks it closes the outer describe/it closure:
case isDir(d):
  Ok(v): removeAll(d)
  Err(_): ()         # <-- breaks parser

# Fix: use a real expression, or discard the conditional entirely:
removeAll(d)        # discard Result; Ok = removed, Err = didn't exist, both fine for setup
```

**Pattern 2: `if <bool>:` as the sole statement in an inline case arm body**
```ry
# Fails — single-arm if expression at end of inline case arm body breaks parser:
case isDir(d):
  Ok(v): if v: removeAll(d)   # <-- breaks parser
  Err(_): 0

# Fix: use unconditional call (discard result) or move inside a multiline body with a trailing statement:
removeAll(d)          # simplest fix for setup guards
```

**Pattern 3: `Ok(true)` / `Ok(false)` literal patterns are NOT supported**
```ry
# Fails — nested literal matching inside constructor patterns:
case isDir(d):
  Ok(true): removeAll(d)   # <-- parser error, not a type error
  _: ()

# Fix: bind to variable, then check:
case isDir(d):
  Ok(v): expect(v).toBeTrue()
  Err(e): fail("isDir failed: " + e.message)
```

**How to apply**: In test files, always use `Err(e): fail(...)` (not `Err(_): ()`) for error arms.
For setup guards ("if dir exists, remove it"), prefer unconditional `removeAll(d)` — the returned
`Result<Unit, Error>` is discarded if unused. For `Result<bool, Error>` predicates, use
`Ok(v): expect(v).toBeTrue()` / `Ok(v): expect(v).toBeFalse()` patterns.

### Naming-convention sweeps must include the implicit `name: type = value` form, not just `let`/`var`, and must cover module-global declarations

**Source**: #1466 (2026-04-30, follow-up to #1450 / #1451); #1468 (2026-04-30, module-global blind spot)
**Tags**: testing, naming, camelCase, sweep, blind-spot, module-global

**Rule**: When grep-driven renaming sweeps target Ry identifiers in `.test.ry` files, the search pattern must include the implicit-binding form (`name: type = value` with no `let`/`var` keyword), not just keyword-prefixed declarations — and it must cover **module-global** declarations (no leading whitespace) as well as bodies inside `it(...)`. The camelCase parser flip (#1443) and the `tests/spec/` sweep (#1450 / #1451) used patterns anchored on `let` / `var`, missing every implicit binding inside `it(...)` blocks; #1466's follow-up sweep then anchored on `^\s+` (one or more leading spaces), which still missed module-global implicit bindings such as `cow_global_box: CowBox = ...` at column 0 (#1468 cleanup).

**Why**: The parser accepts `noHeaders: Map<str, str> = {}` exactly like a `let` declaration, both as an indented binding inside `it(...)` and as a module-global at the top level. `grep -E '\b(let|var)\s+...'` skips both forms; `grep -E '^\s+...'` skips the module-global form. A second blind spot in #1466's recommended grep was that the identifier body was written as `[a-zA-Z0-9]+`, which excludes `_` — so multi-underscore names (`cow_global_box`) failed to match because the engine consumed only `cow_global` before the trailing `\s*[:=]` check ran into a stray `_box`. Tests still pass under either spelling, so without an exhaustive grep the rename silently leaves the old name in place.

**How to apply**: For any future Ry-identifier sweep, audit with the anchor-form-agnostic regex below — `^\s*` (zero or more leading spaces) matches both indented and module-global declarations, the body uses `[a-zA-Z0-9_]` so the engine can consume any number of underscores, and the trailing `\s*[:=]` covers `name: Type = ...` and `name = ...`. The regex deliberately omits `let` / `var` / `const` keyword forms — the parser camelCase enforcement from #1443 already rejects those at compile time, so a residual snake_case binding under a keyword cannot ship undetected; the implicit-binding form (no keyword) is the one that slips past the parser:

```bash
grep -rEn --include='*.ry' '^\s*[a-z][a-zA-Z0-9_]*_[a-zA-Z0-9_]+\s*[:=]' tests/spec/ \
  | grep -vE ':\s*#|"[^"]*[a-z_]+[^"]*"'
```

Apply the rewrite via `sed` on the matching files (`sed -i '' 's/oldName/newName/g' <files>`) — per-site `Read` + `Edit` is the pattern that produced #1466's gap in the first place.
