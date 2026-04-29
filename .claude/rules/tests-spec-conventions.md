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
got_none = false
case opt:
    Some(v): fail("unexpected Some")
    None: got_none = true
expect(got_none).toEq(true)
```

### `expect(str).toEq("literal")` is NUL-truncating — use `expect(str == "literal").toEq(true)` for NUL-containing strings

**Source**: PR #1048 and #1049 (CodeRabbit review). **Tags**: testing, NUL-safety, codegen_test

`toEq` for string values emits a `strcmp` call (`codegen_test.cpp:784` via the `isStringValue` branch).
`strcmp` stops at the first `\0`, so `expect(substring("a\0b", 0, 3)).toEq("a\0b")` passes even
when `substring` returns `"a"` — both C-strings compare equal as `""` / `"a"` depending on content.

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
case is_dir(d):
  Ok(v): remove_all(d)
  Err(_): ()         # <-- breaks parser

# Fix: use a real expression, or discard the conditional entirely:
remove_all(d)        # discard Result; Ok = removed, Err = didn't exist, both fine for setup
```

**Pattern 2: `if <bool>:` as the sole statement in an inline case arm body**
```ry
# Fails — single-arm if expression at end of inline case arm body breaks parser:
case is_dir(d):
  Ok(v): if v: remove_all(d)   # <-- breaks parser
  Err(_): 0

# Fix: use unconditional call (discard result) or move inside a multiline body with a trailing statement:
remove_all(d)          # simplest fix for setup guards
```

**Pattern 3: `Ok(true)` / `Ok(false)` literal patterns are NOT supported**
```ry
# Fails — nested literal matching inside constructor patterns:
case is_dir(d):
  Ok(true): remove_all(d)   # <-- parser error, not a type error
  _: ()

# Fix: bind to variable, then check:
case is_dir(d):
  Ok(v): expect(v).toBeTrue()
  Err(e): fail("is_dir failed: " + e.message)
```

**How to apply**: In test files, always use `Err(e): fail(...)` (not `Err(_): ()`) for error arms.
For setup guards ("if dir exists, remove it"), prefer unconditional `remove_all(d)` — the returned
`Result<Unit, Error>` is discarded if unused. For `Result<bool, Error>` predicates, use
`Ok(v): expect(v).toBeTrue()` / `Ok(v): expect(v).toBeFalse()` patterns.
