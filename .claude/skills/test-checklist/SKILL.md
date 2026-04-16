---
name: test-checklist
description: Structured checklist of test perspectives to consult BEFORE writing tests in TDD. Invoke when about to create/update a .test.ry spec, when AGENTS.md TDD step "テスト作成" is next, or when the user says "テスト観点" / "テストチェックリスト" / "どんなテストを書くべき". Surfaces annotation variants, mutation-in-loop, embedded NUL, type-cross boundary, workaround masking, and error-message-text gaps.
allowed-tools: Read, Grep, Glob, Bash(git diff:*), Bash(git log:*)
metadata:
  short-description: Checklist of test perspectives before writing .test.ry specs
---

# Test Checklist

Produce a structured checklist of test perspectives for the target feature **before** any test is written. The goal is to surface omission classes that repeatedly caused post-release bugs in the ry project (#1020–#1027).

> **This skill does NOT write, edit, commit, or run tests.** It reports perspectives only. Read-only operations only.

---

## Why this skill exists

AGENTS.md §"TDD ベースの開発プロセス" defines *when* to write tests. This skill defines *what to cover*.

The anti-pattern it breaks:

```text
Write tests for the happy path
  → boundary value is set via arithmetic workaround
  → direct literal path is never exercised
  → lexer/parser bug ships to release
```

Recurring omission classes that caused real bugs:

- Annotation variant gap (#1020, #1024) — fully-typed lambda tested; untyped lambda missing
- Mutation-in-iteration (#1021) — collection mutated inside `for` loop not tested
- Embedded special bytes (#1022) — `"\0"` inside string never tested
- Type-cross boundary (#1023) — `int/0` tested but `int/0.0` was not
- Workaround masking (#1025) — boundary value set via `-MAX - 1` instead of direct literal
- Error message text (#1026, #1027) — error *occurs* is tested but error *message text* is not

> **Forbidden conclusion**: "Tests pass → this boundary is covered." A test that avoids the direct code path does not cover the underlying parse/codegen branch.

---

## When to invoke

Invoke at the start of the "テスト作成" step in both AGENTS.md TDD modes:

| AGENTS.md mode | Invoke before |
|---|---|
| 既存コードの変更時 | Step 1 — "変更を検出できるテストが存在することを確認" |
| 新機能追加時 | Step 1 — "変更後の仕様に基づくテストを作成" |

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
  1. Find base branch: `git log --format='%D' HEAD | grep -o 'origin/v[0-9][0-9.]*' | head -1`
  2. If a base branch was found: `git diff --name-only <base-branch>..HEAD`
  3. If no base branch was found (detached HEAD, fresh branch, etc.):
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

Run the following checks on the target test file(s):

**P5 — INT64_MIN arithmetic workaround (known instance: `tests/spec/int_overflow.test.ry:29`):**

```text
Grep pattern: -9223372036854775807\s*-\s*1
Path: tests/spec/
```

If matched → report location as **P5 FAIL**.

**P5 — Generic MAX/MIN ±1 workaround (named-constant variant):**

```text
Grep pattern: (MAX|MIN)\w*\s*[+-]\s*1
Path: tests/spec/
```

> Note: This pattern catches named-constant workarounds (e.g. `max_val = ...; max_val + 1`). Ry test code typically uses numeric literals, so no match is the common case. Use the INT64_MIN literal pattern above as the primary detection mechanism.

**P3 — Embedded NUL byte coverage absent:**

```text
Grep (files-without-match) pattern: "\\0"
Path: tests/spec/str*.test.ry
```

Files without a match likely lack P3 coverage.

**P6 — Runtime error message text not verified:**

For each file containing `Err(e):`, check whether it also contains an explicit message assertion on `e.message` (e.g. `to_eq`, `to_contain`, `to_match`, or any expression that directly checks the text). If only `to_be_err()` is used with no message text check → **P6 PARTIAL**.

**P1 — Annotation variant gap:**

Two-step: (1) grep for a fully-typed lambda `\(\w+:\s*\w+.*\)\s*=>` in the test file; (2) check whether the same file also has an untyped lambda `\(\w+,\s*\w+\)\s*=>` or `\(\w+\)\s*=>`. If typed-only → **P1 FAIL**.

**P7 — Compile-time diagnostic text not verified:**

In `tests/test_codegen_fail.cpp`, check whether `expectCompileError` calls include a second argument (expected message text). Calls with only a source snippet → **P7 PARTIAL**.

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
| P6 | Runtime error message text | Verify error message text with `expect(e.message).to_eq(...)`, not just that an error occurred | #1026, #1027 |
| P7 | Compile-time diagnostic text | Pass expected message text as the second argument to `expectCompileError` | #1026, #1027 |
| P8 | Rejection-branch direct trigger | Every new rejection branch needs a test that directly triggers it — happy-path tests of adjacent legal cases do NOT count | KNOWLEDGE.md:104 |

---

## Categories

### Arithmetic / Operators

Applicable patterns: **P1, P4, P5, P8**

| Check | Pattern |
|---|---|
| All type combinations (int×int, int×float, float×float) | P4 |
| Zero-division: `int/0`, `int/0.0`, `float/0`, `float/0.0`, `0.0/0.0` | P4 |
| INT64_MIN as direct literal `-9223372036854775808` | P5 |
| INT64_MAX, INT64_MIN, 0, -1 as boundary values | P5 |
| Compound assignment (`+=`, `-=`, `*=`) with the same boundary set as binary ops | P1 |
| Every rejection branch in arithmetic codegen triggered directly | P8 |

**Required shape (P5 direct-literal):**
```ry
it("should handle INT64_MIN literal directly", ():
  expect(-9223372036854775808).to_eq(-9223372036854775808)
)
```

**Forbidden shape (P5 workaround — `tests/spec/int_overflow.test.ry:29` is a live instance):**
```ry
min = -9223372036854775807 - 1   -- DO NOT use arithmetic to express boundary literals
```

---

### Strings

Applicable patterns: **P3, P6, P8**

| Check | Pattern |
|---|---|
| Empty string `""` | P3 |
| ASCII-only | P3 |
| Multibyte UTF-8 | P3 |
| Embedded NUL byte `"\0"` and `"a\0b"` | P3 |
| UFCS equivalence: `f(s, ...)` == `s.f(...)` result | — |
| Runtime error message text verified for invalid operations (e.g. `str[i]`) | P6 |
| `length` vs `byte_len` divergence on multibyte chars | — |

**Required shape (P3 NUL byte):**
```ry
it("should preserve embedded NUL bytes", ():
  s = "a\0b"
  expect(s.byte_len()).to_eq(3)
)
```

**Required shape (P6 error message — both arms, message text verified):**
```ry
it("should report correct error for str indexing", ():
  case str_index("hello", 0):
    Ok(v):
      fail("expected Err but got Ok")
    Err(e):
      expect(e.message).to_eq("use char_at() to index a string")
)
```

---

### Collections (List / Map / Set)

Applicable patterns: **P1, P2, P8**

| Check | Pattern |
|---|---|
| Empty collection, 1-element, large (> initial capacity) | — |
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
  expect(xs).to_eq([1, 2, 3, 4, 5])
)
```

**Required shape (P1 fully-untyped lambda with reduce):**
```ry
it("should reduce with fully-untyped lambda", ():
  result = reduce([1, 2, 3], (a, b) => a + b)
  expect(result).to_eq(6)
)
```

---

### Type System / Inference

Applicable patterns: **P1, P8**

| Check | Pattern |
|---|---|
| Lambda annotation matrix: (typed params + typed return) / (typed params only) / (no annotations) | P1 |
| Result inference: Ok-arm, Err-arm, both arms, `?` propagation | P1 |
| `if`-expression inside lambda returning `Ok`/`Err` without explicit return type | P1 |
| `IfBlockExpr` (block-form `if`) returning `Ok`/`Err` | P1 |
| `and_then` / `map` chain where intermediate step returns `Err`/`None` | P1 |
| Nested types (`Option<Result<T,E>>` etc.) inferred correctly | P1 |
| Every rejection branch in type-checker triggered directly | P8 |

**Required shape (P1 untyped lambda returning Result):**
```ry
it("should infer Result type without annotation", ():
  f = (s) => try_parse(s)   -- no type annotation on f
  case f("42"):
    Ok(v):
      expect(v).to_eq(42)
    Err(e):
      fail("unexpected error")
)
```

---

### Parser / Literals

Applicable patterns: **P5, P7, P8**

| Check | Pattern |
|---|---|
| INT64_MIN / INT64_MAX as direct literals | P5 |
| Hex literals (`0xff`), binary literals (`0b1010`) | — |
| Scientific notation (`1e10`, `-2.5e-3`) | — |
| Underscore-separated digits (`1_000_000`) | — |
| Unsupported octal (`0o17`) diagnostic message text verified | P7 |
| Type-suffix literals (`42i32`, `255u8`, `3.14f32`) | — |
| Compile-time error text for unsupported syntax verified with `expectCompileError` 2nd arg | P7 |
| Every parser rejection branch triggered directly | P8 |

**Forbidden shape (P5):**
```ry
min = -9223372036854775807 - 1   -- FORBIDDEN: bypasses the literal parse path
```

**Required shape (P5 direct literal):**
```ry
it("INT64_MIN parses correctly", ():
  expect(-9223372036854775808).to_eq(-9223372036854775808)
)
```

**Required shape (P7 compile-time diagnostic text — C++):**
```cpp
// tests/test_codegen_fail.cpp
expectCompileError(R"(
  x = 0o17
)", "octal literals are not supported; use 0x... for hex or 0b... for binary");
```

---

### Diagnostic Quality

Applicable patterns: **P6, P7**

| Check | Pattern |
|---|---|
| Runtime error message contains correct type/variable name | P6 |
| Runtime error message does NOT leak implementation details | P6 |
| Compile-time error suggests an alternative (e.g. "use `char_at()`") | P7 |
| Both runtime (`e.message`) and compile-time (`expectCompileError` 2nd arg) paths verified | P6 + P7 |

**Required shape (P6 runtime — both arms required):**
```ry
it("should give helpful error for invalid str operation", ():
  case bad_str_op("hello"):
    Ok(v):
      fail("expected Err but got Ok")
    Err(e):
      expect(e.message).to_eq("use char_at() to index a string")
)
```

**Required vs forbidden (P7 compile-time):**
```cpp
// CORRECT: message text verified
expectCompileError(R"(x = 0o17)", "octal literals are not supported");

// INCORRECT: no second argument — P7 PARTIAL
expectCompileError(R"(x = 0o17)");
```

---

### ARC / Memory

Applicable patterns: **P8** (and leak-detection)

| Check | Pattern |
|---|---|
| Closure captures variable → value retained correctly (not dangling reference) | — |
| ARC object created and destroyed inside loop → no leak | — |
| Field overwrite → old value released | — |
| Leak check uses `arc_live_count()` **delta** (not absolute value) | — |
| Every new ARC-path rejection branch triggered directly | P8 |

**Required shape (leak check using delta — KNOWLEDGE.md:132):**
```ry
from runtime_internal import arc_live_count

it("should not leak ARC objects in loop", ():
  before = arc_live_count()
  for _ in range(0, 100):
    s = "hello"
  delta = arc_live_count() - before
  expect(delta).to_eq(0)
)
```

**Forbidden shape (absolute count — unreliable across test runs):**
```ry
expect(arc_live_count()).to_eq(0)   -- FORBIDDEN: depends on global ARC state
```

**Exception (KNOWLEDGE.md:1058):** Defensive pointer-shape guards that are unreachable from Ry source do not require regression tests. Document the exception in KNOWLEDGE.md instead.

---

## Report Template

```text
Test Checklist Report: <target>

Mode: <新機能追加時 | 既存コードの変更時>
Detected categories: <list>
Applicable patterns: <list>

[P1] Annotation variant coverage — <COVERED | NOT COVERED | PARTIAL | N/A>
  <details if NOT COVERED or PARTIAL>

[P2] Mutation-in-iteration — <COVERED | NOT COVERED | N/A>
  <details>

[P3] Embedded special bytes — <COVERED | NOT COVERED | N/A>
  <details>

[P4] Type-cross boundary matrix — <COVERED | PARTIAL | N/A>
  <missing cells>

[P5] Workaround masking — <PASS | FAIL | N/A>
  Location: <file:line> — Found: <workaround expression>
  Required: <direct literal>
  Proposed fix:
    <code snippet>

[P6] Runtime error message text — <PASS | PARTIAL | N/A>
  <files where e.message or to_eq is missing>

[P7] Compile-time diagnostic text — <PASS | PARTIAL | N/A>
  <expectCompileError calls missing second argument>

[P8] Rejection-branch direct trigger — <COVERED | NOT COVERED | UNKNOWN>
  <rejection branch locations from grepping the source>

---
Proposed test additions:
<concrete .test.ry or C++ snippets for each NOT COVERED / PARTIAL item>

Reference: AGENTS.md TDD §<mode>; KNOWLEDGE.md:104 (rejection branch rule)
```

---

## Notes

- This skill performs **read-only** operations only (`Read`, `Grep`, `Glob`, `git diff`, `git log`). It never writes files, edits tests, or creates commits.
- KNOWLEDGE.md:46 — `.test.ry` `case` arms must have a non-empty body: use `()` for intentional no-op, or `fail("reason")` to mark an unexpected path.
- For *when* to write tests, see AGENTS.md §"TDD ベースの開発プロセス".
- For rejection-branch test requirements, see KNOWLEDGE.md:104.
- For ARC leak-detection patterns, see KNOWLEDGE.md:132.

*Canonical source for test syntax examples: `tests/spec/result.test.ry` (P6), `tests/spec/arc_release_on_index_overwrite.test.ry` (ARC delta), `tests/spec/int_overflow.test.ry` (P5 live workaround instance).*
