---
name: test-checklist
description: Structured checklist of test perspectives to consult BEFORE writing tests in TDD. Invoke when about to create/update a .test.ry spec, when the `/tdd-cycle` step "テスト作成" is next, or when the user says "テスト観点" / "テストチェックリスト" / "どんなテストを書くべき". Surfaces annotation variants, mutation-in-loop, embedded NUL, type-cross boundary, workaround masking, and error-message-text gaps.
allowed-tools: Read, Grep, Glob, Bash(git diff:*), Bash(git log:*)
metadata:
  short-description: Checklist of test perspectives before writing .test.ry specs
---

# Test Checklist

Produce a structured checklist of test perspectives for the target feature **before** any test is written. The goal is to surface omission classes that repeatedly caused post-release bugs in the ry project (#1020–#1027).

> **This skill does NOT write, edit, commit, or run tests.** It reports perspectives only. Read-only operations only.

---

## Why this skill exists

`/tdd-cycle` defines *when* to write tests; this skill defines *what to cover*. P1–P8 below codify recurring omission classes from past ry releases (#1020–#1027): happy-path tests pass while the targeted parse/codegen branch goes uncovered (annotation variants, in-loop mutation, embedded NUL, type-cross boundary, arithmetic-workaround masking, message-text gaps).

> **Forbidden conclusion**: "Tests pass → this boundary is covered." A test that avoids the direct code path does not cover the underlying parse/codegen branch.

---

## When to invoke

両モードのテスト作成段階 — 既存コードの変更時の Step 1（変更を検出できるテストの確認）/ 新機能追加時の Red（テスト失敗確認）— で呼ぶ。

Also invoke before writing tests during PR review response.

---

## Context

- Current branch: !`git branch --show-current`

---

## Inputs

User input: $ARGUMENTS

- If provided: treat as the target file(s) / feature area (path or description)
- If empty: derive changed files dynamically (see Step 1)

---

## Steps

### Step 1: Identify target feature

- If `$ARGUMENTS` is a file path or glob → use it as the target.
- If `$ARGUMENTS` is a feature description → identify the relevant source file(s) with `Glob` or `Grep`.
- If empty → derive changed files dynamically:
  1. Try diff against `origin/main`: `git diff --name-only origin/main..HEAD`
  2. If `origin/main` is unavailable (no remote, fresh clone, detached HEAD, etc.):
     - Try: `git diff --name-only HEAD~1..HEAD`
     - If `HEAD~1` is unavailable (initial commit or shallow clone): `git diff --name-only --root HEAD`
  - New `.ry` or `.cpp` files → **新機能追加時** mode
  - Modified existing files → **既存コードの変更時** mode

Report: `Target: <path or description>`, `Mode: <新機能追加時 | 既存コードの変更時>`

### Step 2: Classify by category

Map the target to one or more of the seven categories using the file-name hints:

| Category | File-name / code hints |
|---|---|
| Arithmetic/Operators | `*operator*`, `*arith*`, `*checked_arith*`, arithmetic ops in source |
| Strings | `runtime_string*`, `*str*`, string operations |
| Collections (List/Map/Set) | `runtime_list*`, `runtime_map*`, `runtime_set*`, `*append*`, `for` loops with collection |
| Type System / Inference | `*type_check*`, `*infer*`, `*lambda*`, `*result*` |
| Parser / Literals | `parser*`, `lexer*`, `*literal*`, numeric/string literal handling |
| Diagnostic Quality | `*error_reporter*`, `*diagnostic*`, error messages |
| ARC / Memory | `*arc*`, `runtime_internal*`, `runtime_alloc*`, reference counting |

Report: `Categories: [<list>]`

### Step 3: Run pattern scan (P1–P8)

For each selected category, consult the **Categories** section below and list the applicable patterns (P1–P8). For each pattern, decide: `COVERED`, `NOT COVERED`, or `PARTIAL`. Use `Read` and `Grep` to inspect existing test files under `tests/spec/` and `tests/test_codegen_fail.cpp`.

### Step 4: Detect existing-test anti-patterns

Run these checks against the target test files; report each match with a verdict.

| Pattern | Detection | Path | Verdict |
|---|---|---|---|
| P5 INT64_MIN literal workaround | `grep '\-9223372036854775807\s*-\s*1'` | `tests/spec/` | **FAIL** if matched (live: `int_overflow.test.ry:29`) |
| P5 named-constant workaround | `grep '(MAX\|MIN)\w*\s*[+-]\s*1'` | `tests/spec/` | **FAIL** if matched (rare; literals are primary) |
| P3 embedded NUL absent | `grep -L '"\\0"'` | `tests/spec/str*.test.ry` | files without match → likely lack coverage |
| P6 message text not verified | `Err(e):` arms must use `toEq`/`toContain`/`toMatch` on `e.message` (not bare `toBeErr()`) | `tests/spec/` | **PARTIAL** if assertion absent |
| P1 annotation variant gap | typed lambda `\(\w+:\s*\w+.*\)\s*=>` present but no untyped `\(\w+\)\s*=>` in same file | per-file | **FAIL** if typed-only |
| P7 compile-time text not verified | `expectCompileError` call has no 2nd argument | `tests/test_codegen_fail.cpp` | **PARTIAL** |

### Step 5: Emit report

Output using the **Report Template** section below, with concrete proposed code snippets for every `NOT COVERED` or `PARTIAL` item.

---

## Patterns (P1–P8)

| ID | Name | One-line rule | Bug evidence |
|----|------|---------------|-------------|
| P1 | Annotation variant coverage | Test fully-typed, param-only-typed, and fully-untyped variants; also with and without return-type annotation (6 combinations) | #1020, #1024 |
| P2 | Mutation-in-iteration | Test `append!`/`pop`/`remove`/`add`/`insert!`/`m[k]=v` called inside a `for` loop; include nested-for, tuple-destructure, and literal-iterable variants | #1021 |
| P3 | Embedded special bytes | Hardcode `"\0"` and `"a\0b"` in string tests | #1022 |
| P4 | Type-cross boundary matrix | Test the matrix: type (int/float) × value (0, 0.0, -0.0, NaN, Inf) for each arithmetic operator | #1023 |
| P5 | Workaround masking | Boundary values must be direct literals — never via arithmetic (`-INT64_MAX - 1` is **forbidden**) | #1025 |
| P6 | Runtime error message text | Verify error message text with `expect(e.message).toEq(...)`, not just that an error occurred | #1026, #1027 |
| P7 | Compile-time diagnostic text | Pass expected message text as the second argument to `expectCompileError` | #1026, #1027 |
| P8 | Rejection-branch direct trigger | Every new rejection branch needs a test that directly triggers it — happy-path tests of adjacent legal cases do NOT count | `.claude/rules/tests-rejection-tdd.md` |

---

## Categories

> Generic equivalence/boundary cases (numeric literal forms, empty/single/large collections, ASCII vs UTF-8 partitions, etc.) live in `/test-design-techniques`. The rows below focus on ry-specific patterns (P1–P8).

### Arithmetic / Operators (P1, P4, P5, P8)

| Check | Pattern |
|---|---|
| All type combinations (int×int, int×float, float×float) | P4 |
| Zero-division: `int/0`, `int/0.0`, `float/0`, `float/0.0`, `0.0/0.0` | P4 |
| INT64_MIN as direct literal `-9223372036854775808` | P5 |
| Compound assignment (`+=`, `-=`, `*=`) with the same boundary set as binary ops | P1 |
| Every rejection branch in arithmetic codegen triggered directly | P8 |

**Required shape (P5 direct-literal):**
```ry
it("should handle INT64_MIN literal directly", ():
  expect(-9223372036854775808).toEq(-9223372036854775808)
)
```

**Forbidden shape (P5 workaround — `tests/spec/int_overflow.test.ry:29` is a live instance):**
```ry
min = -9223372036854775807 - 1   -- DO NOT use arithmetic to express boundary literals
```

---

### Strings (P3, P6, P8)

| Check | Pattern |
|---|---|
| Embedded NUL byte `"\0"` and `"a\0b"` | P3 |
| UFCS equivalence: `f(s, ...)` == `s.f(...)` result | — |
| Runtime error message text verified for invalid operations (e.g. `str[i]`) | P6 |
| `len` vs `byteLen` divergence on multibyte chars | — |

**Required shape (P3 NUL byte):**
```ry
it("should preserve embedded NUL bytes", ():
  s = "a\0b"
  expect(s.byteLen()).toEq(3)
)
```

**Required shape (P6 error message — both arms, message text verified):**
```ry
it("should report correct error for str indexing", ():
  case strIndex("hello", 0):
    Ok(v):
      fail("expected Err but got Ok")
    Err(e):
      expect(e.message).toEq("str does not support index access; use charAt(s, i) instead")
)
```

---

### Collections (List / Map / Set) (P1, P2, P8)

| Check | Pattern |
|---|---|
| Lambdas to `map`/`filter`/`reduce`/`fold`: fully-typed, param-only-typed, fully-untyped | P1 |
| `append!` called inside `for` loop | P2 |
| `pop` / `remove` called inside `for` loop | P2 |
| `add` / `insert!` / `m[k]=v` called inside `for` loop | P2 |
| Nested `for` with inner collection mutation | P2 |
| `for` over literal iterable (not a variable) with mutation | P2 |
| Every rejection branch in collection codegen triggered | P8 |

**Required shape (P2 mutation-in-iteration):**
```ry
it("should handle append! inside for loop", ():
  xs = [1, 2, 3]
  for x in [4, 5]:
    xs.append!(x)
  expect(xs).toEq([1, 2, 3, 4, 5])
)
```

**Required shape (P1 fully-untyped lambda with reduce):**
```ry
it("should reduce with fully-untyped lambda", ():
  result = reduce([1, 2, 3], (a, b) => a + b)
  expect(result).toEq(6)
)
```

---

### Type System / Inference (P1, P8)

| Check | Pattern |
|---|---|
| Lambda annotation matrix: (typed params + typed return) / (typed params only) / (no annotations) | P1 |
| Result inference: Ok-arm, Err-arm, both arms, `?` propagation | P1 |
| `if`-expression inside lambda returning `Ok`/`Err` without explicit return type | P1 |
| `IfBlockExpr` (block-form `if`) returning `Ok`/`Err` | P1 |
| `andThen` / `map` chain where intermediate step returns `Err`/`None` | P1 |
| Nested types (`Option<Result<T,E>>` etc.) inferred correctly | P1 |
| Every rejection branch in type-checker triggered directly | P8 |

**Required shape (P1 untyped lambda returning Result):**
```ry
it("should infer Result type without annotation", ():
  f = (s) => tryParse(s)   -- no type annotation on f
  case f("42"):
    Ok(v):
      expect(v).toEq(42)
    Err(e):
      fail("unexpected error")
)
```

---

### Parser / Literals (P5, P7, P8)

| Check | Pattern |
|---|---|
| INT64_MIN / INT64_MAX as direct literals | P5 |
| Unsupported octal (`0o17`) diagnostic message text verified | P7 |
| Compile-time error text for unsupported syntax verified with `expectCompileError` 2nd arg | P7 |
| Every parser rejection branch triggered directly | P8 |

**P5 (direct literal vs workaround):** see Arithmetic section above for required and forbidden shapes; the same rule applies to parser-level boundary literals.

**Required shape (P7 compile-time diagnostic text — C++):**
```cpp
// tests/test_codegen_fail.cpp
expectCompileError(R"(
  x = 0o17
)", "octal literals are not supported; use 0x... for hex or 0b... for binary");
```

---

### Diagnostic Quality (P6, P7)

| Check | Pattern |
|---|---|
| Runtime error message contains correct type/variable name | P6 |
| Runtime error message does NOT leak implementation details | P6 |
| Compile-time error suggests an alternative (e.g. "use `charAt()`") | P7 |
| Both runtime (`e.message`) and compile-time (`expectCompileError` 2nd arg) paths verified | P6 + P7 |

**Required shape (P6 runtime — both arms required):**
```ry
it("should give helpful error for invalid str operation", ():
  case badStrOp("hello"):
    Ok(v):
      fail("expected Err but got Ok")
    Err(e):
      expect(e.message).toEq("str does not support index access; use charAt(s, i) instead")
)
```

**P7 (compile-time text):** see Parser/Literals section above for the canonical shape; the rule (`expectCompileError` must take a 2nd-argument message) applies to all `expectCompileError` calls regardless of category.

---

### ARC / Memory (P8 + leak-detection)

| Check | Pattern |
|---|---|
| Closure captures variable → value retained correctly (not dangling reference) | — |
| ARC object created and destroyed inside loop → no leak | — |
| Field overwrite → old value released | — |
| Leak check uses `arcLiveCount()` **delta** (not absolute value) | — |
| Every new ARC-path rejection branch triggered directly | P8 |

**Required shape (leak check using delta — see `.claude/rules/tests-arc-leak-pattern.md`):**
```ry
from runtime_internal import arcLiveCount

it("should not leak ARC objects in loop", ():
  before = arcLiveCount()
  for _ in range(0, 100):
    s = "hello"
  delta = arcLiveCount() - before
  expect(delta).toEq(0)
)
```

**Forbidden shape (absolute count — unreliable across test runs):**
```ry
expect(arcLiveCount()).toEq(0)   -- FORBIDDEN: depends on global ARC state
```

**Exception (see `.claude/rules/tests-rejection-tdd.md`):** Defensive pointer-shape guards that are unreachable from Ry source do not require regression tests. Document the exception in the same rule file instead.

---

## Report Template

```text
Test Checklist Report: <target>
Mode: <新機能追加時 | 既存コードの変更時>; Categories: <list>; Patterns: <list>

[P1] Annotation variant coverage — <verdict>
[P2] Mutation-in-iteration — <verdict>
[P3] Embedded special bytes — <verdict>
[P4] Type-cross boundary matrix — <verdict>
[P5] Workaround masking — <verdict>     (if FAIL: <file:line> — found <expr>, required <literal>)
[P6] Runtime error message text — <verdict>
[P7] Compile-time diagnostic text — <verdict>
[P8] Rejection-branch direct trigger — <verdict>

Verdict vocabulary: COVERED / NOT COVERED / PARTIAL / PASS / FAIL / UNKNOWN / N/A.
For each NOT COVERED / PARTIAL / FAIL row, append a proposed test snippet (.test.ry or C++).

Reference: `/tdd-cycle` §<mode>; `.claude/rules/tests-rejection-tdd.md`.
```

---

## Notes

- Read-only skill (`Read`, `Grep`, `Glob`, `git diff`, `git log`).
- Cross-references: `/tdd-cycle` (timing); `/test-design-techniques` (deductive complement); `.claude/rules/tests-rejection-tdd.md` (P8); `.claude/rules/tests-arc-leak-pattern.md` (ARC delta); `.claude/rules/tests-spec-conventions.md` (`case` arm conventions).

*Canonical source examples: `tests/spec/result.test.ry` (P6), `tests/spec/arc_release_on_index_overwrite.test.ry` (ARC delta), `tests/spec/int_overflow.test.ry` (P5 live workaround instance).*
