---
name: test-design-techniques
description: Five deductive test design techniques (equivalence partitioning, boundary value analysis, state transition, decision table, pairwise) for systematic edge case derivation. Invoke at the テスト作成 step in /tdd-cycle to enumerate test cases from specification structure, complementing /test-checklist's ry-specific recurring-omission patterns. Trigger phrases include 同値分割 / 境界値 / 状態遷移 / 決定表 / ペアワイズ / テスト設計 / エッジケース / 演繹的 / どんなテストケース.
allowed-tools: Read, Grep, Glob
metadata:
  short-description: Five deductive test design techniques for systematic edge case enumeration
---

# Test Design Techniques

Five general-purpose, deductive test design techniques for systematically enumerating test cases during the テスト作成 step of `/tdd-cycle`. Provides the *technique catalog* that complements `/test-checklist`'s ry-specific recurring-omission patterns (P1–P8).

> **This skill does NOT write, edit, commit, or run tests.** It guides selection of techniques and case enumeration. Read-only operations only.

---

## Why this skill exists

`/test-checklist` (P1–P8) is **inductive** — it lists ry-specific omission classes derived from past bugs (#1020-#1027 and beyond). Those patterns prevent regressions of known failure modes.

This skill is **deductive** — it provides general-purpose techniques (taught in formal software testing literature) to systematically *generate* test cases from specification structure. Use both:

| Skill | Mode | Purpose |
|---|---|---|
| `/test-design-techniques` | Deductive | Enumerate cases from specification structure |
| `/test-checklist` | Inductive | Catch ry-specific recurring omissions |

The two are complementary. Use `/test-design-techniques` first to derive the case set, then `/test-checklist` to verify ry-specific classes are not missed.

---

## When to invoke

- **`/tdd-cycle` テスト作成 step**: before listing test cases, to choose techniques applicable to the spec
- **Plan モード**: while designing test plans for a feature, to estimate coverage breadth
- **PR レビュー対応**: when a reviewer asks "what about case X?" and you need to argue the case set is systematic

Use both `/test-design-techniques` (derive) and `/test-checklist` (verify) at the テスト作成 step.

---

## The Five Techniques

### 1. 同値分割 (Equivalence Partitioning)

**Definition**: Partition the input domain into classes where the system is expected to behave equivalently. Test one representative per class — testing every value within a class is redundant.

**When to use**: Input has natural categorization (e.g. type / range / format / state).

**Procedure**:
1. List input parameters and their domains
2. For each parameter, partition into valid and invalid equivalence classes
3. Pick one representative per class as a test case

**ry application examples**:

- **Parser numeric literals** (`src/lexer.cpp`):
  - Valid classes: integer (`42`), float (`3.14`), hex (`0xff`), binary (`0b1010`), scientific (`1e10`), with underscore separators (`1_000`), with type suffix (`42i32`)
  - Invalid classes: unsupported octal (`0o17`), unsupported suffix (`42u128`)
  - One test per class — `42` covers all positive integers in the same class
- **`from xxx import` module path**: hyphenated (rejected, #1483), absolute (rejected), relative-dot (`from .submodule`), bare identifier (`from base64`)
- **`Result<T,E>` matching**: Ok-arm only / Err-arm only / both arms / `?` propagation

**Anti-pattern**: writing 10 tests for `add(1,2)`, `add(2,3)`, `add(3,4)` — all in the same equivalence class (positive int + positive int).

---

### 2. 境界値分析 (Boundary Value Analysis)

**Definition**: Test the values at and immediately around the boundaries of equivalence classes. Bugs cluster at boundaries because off-by-one errors, overflow conditions, and threshold mismatches live there.

**When to use**: Input has range constraints, capacity limits, or branch conditions on numeric thresholds.

**Procedure**:

For a boundary `B`, test:
- `B - 1` (just below)
- `B` (at the boundary)
- `B + 1` (just above)

For a range `[lo, hi]`: `lo - 1`, `lo`, `lo + 1`, `hi - 1`, `hi`, `hi + 1`.

**ry application examples**:

- **`int64` overflow** (`tests/spec/int_overflow.test.ry`): `INT64_MAX`, `INT64_MAX - 1`, `INT64_MIN`, `INT64_MIN + 1`, plus `0`, `-1`, `1`. **Critical**: must use **direct literals** for `INT64_MIN` (`-9223372036854775808`), never `-INT64_MAX - 1` (P5 in `/test-checklist`)
- **`List` capacity**: empty (size 0), one element (size 1), exactly the initial-capacity threshold, capacity + 1 (forces reallocation), empty after `pop`
- **Loop iteration counts**: `range(0, 0)` (empty), `range(0, 1)` (single), `range(0, n)` where `n` matches a SIMD lane width if codegen vectorizes
- **String byte length**: `""` (empty), one byte, exactly NUL byte (`"\0"`), multibyte UTF-8 boundary (3-byte → 4-byte char), at the `byteLen` size used by an inline-buffer optimization
- **`@const` value boundaries**: `@const fn N(): 0` and `@const fn N(): -1` if a guard branches on `N >= 0`

**Cross-link**: P5 (workaround masking) in `/test-checklist` — boundary values must come from direct literals, not arithmetic.

---

### 3. 状態遷移テスト (State Transition Testing)

**Definition**: For systems with state, model the state machine and test each transition (state × event → next state). Aim to cover every state, every transition, and key paths.

**When to use**: Component has internal state that changes across operations (parser modes, type inference contexts, ARC reference counts, lock acquisition, async/await suspension).

**Procedure**:
1. Identify states (e.g. lexer modes, type inference scopes)
2. Identify events (e.g. token consumed, scope opened / closed)
3. Build a state-transition matrix; aim for **0-switch coverage** (every transition fired at least once) or **1-switch coverage** (every pair of consecutive transitions)
4. Write one test per transition

**ry application examples**:

- **Type inference scope** (`src/type_check.cpp`): enter `fn` body → infer locals → enter nested lambda → exit lambda → exit fn. Each scope-entry and scope-exit is a transition; test that variable lookup respects shadowing at each transition
- **ARC reference count** (`src/runtime_arc_counter.cpp`): `RC=1 → RC=2` (clone) → `RC=1` (drop) → `RC=0` (free). Test that the codepath through every transition produces the correct count delta. Combine with `arcLiveCount()` delta pattern (see `.claude/rules/tests-arc-leak-pattern.md`)
- **Lock state** (`share/std/thread/lock.ry`): `Released → Acquired → Released`; plus invalid transitions: `Released → Released` (double release), `Acquired → Acquired` (deadlock-prone re-entry)
- **Result chain** (`andThen` / `?` propagation): `Ok(v1) → andThen(f) → Ok(v2)`, `Ok(v1) → andThen(f) → Err(e)`, `Err(e1) → andThen(f) → Err(e1)` (Err short-circuits)
- **Lexer modes**: normal → string-literal (on `"`) → escape (on `\`) → string-literal → normal (on closing `"`). Test each transition, especially escape inside a multi-byte sequence

**Tip**: When the state space is small, draw the diagram (states × events → next state) before writing tests. A markdown table works fine.

---

### 4. 決定表テスト (Decision Table Testing)

**Definition**: When output depends on combinations of input conditions, build a decision table (rows = conditions, columns = rules / actions). Each unique combination of conditions becomes one test case.

**When to use**: Logic is "if A and B and not C, then ..." — multiple conditions interact.

**Procedure**:
1. List boolean conditions (or categorical inputs)
2. List actions / outputs
3. Build a table: each column is a rule (combination); mark T / F for each condition and the resulting action
4. **Collapse equivalent rules** (use `-` for "don't care" once a higher-priority condition determines the outcome)
5. One test per remaining rule

**ry application examples**:

**Operator overload resolution** (`src/codegen_call_dispatch.cpp`, `+` operator):

| Condition | R1 | R2 | R3 | R4 |
|---|---|---|---|---|
| LHS is int | T | T | F | F |
| RHS is int | T | F | T | F |
| **Action** | int op | int→float promote, float op | float←int promote, float op | float op |

One test per rule: R1 = `1+2`, R2 = `1+2.0`, R3 = `1.0+2`, R4 = `1.0+2.0`.

**`Result<T,E>` `?` propagation**:

| Condition | R1 | R2 | R3 |
|---|---|---|---|
| Caller `fn` returns `Result` | T | T | F |
| Operand is `Ok` | T | F | – |
| Operand is `Err` | F | T | – |
| **Action** | unwrap Ok | propagate Err to caller | parse error: `?` outside Result-returning fn |

**Module privacy** (`src/module_loader.cpp`):

| Condition | R1 | R2 | R3 |
|---|---|---|---|
| Symbol starts with `_` | T | T | F |
| Importing module is owner | T | F | – |
| **Action** | OK | reject (`cannot import private symbol`) | OK |

**When to skip**: If conditions are independent and the action doesn't combine them, equivalence partitioning is enough — decision tables shine on **interaction** between conditions.

---

### 5. ペアワイズ法 (Pairwise / All-Pairs Testing)

**Definition**: When N parameters interact and exhaustive testing is infeasible (combinatorial explosion), guarantee that every pair of parameter values is tested in at least one case. Empirically catches roughly 80 % of combinatorial bugs at a fraction of the cost of exhaustive testing.

**When to use**: 3+ parameters with multiple values each, where exhaustive testing would mean dozens of cases.

**Procedure**:
1. List parameters and their values
2. Use a covering-array generator (`pict`, `allpairs.py`) or hand-construct for small inputs to produce a minimal set covering all pairs
3. One test per row of the covering array

**ry application examples**:

- **Native function dispatch** (3 axes): module `{base64, math, filesystem}` × arity `{0, 1, 2}` × argument type `{int, str, list}`. Exhaustive 3 × 3 × 3 = 27 cases; pairwise covers every pair in 9 cases.
- **Codegen pipeline**: LLVM target `{x86_64, arm64}` × optimization level `{0, 2}` × sanitizer `{none, ASan, UBSan, TSan}`. Exhaustive 2 × 2 × 4 = 16; pairwise around 8.
- **stdlib I/O composition**: `io.{readText, readBytes, readLine}` × encoding `{ASCII, UTF-8, UTF-16}` × empty file `{yes, no}` — pairwise covers most realistic combinations.
- **ARC × annotation × loop body**: P1 (lambda annotation variants: typed / param-typed / untyped) × P2 (mutation-in-loop: yes / no) × value type (`int`, `str`, `List`). Pairwise compresses 3 × 2 × 3 = 18 to roughly 9.

**When to skip**: With ≤ 2 parameters, or ≤ 2 values per parameter, exhaustive is small enough that pairwise gives no real saving.

---

## How to combine the techniques

A typical テスト作成 step uses multiple techniques in sequence:

1. **Equivalence partitioning** to identify input classes
2. **Boundary value analysis** at each class edge
3. **Decision table** if multiple conditions interact
4. **State transition** if the component has state
5. **Pairwise** to control case count when 3+ axes are involved

After enumeration, run `/test-checklist` to verify ry-specific patterns (P1–P8) are also covered.

---

## Cross-reference

- **`/tdd-cycle`** §"Cross-reference" — invoke this skill at the start of the テスト作成 step
- **`/test-checklist`** — complementary inductive coverage (ry-specific recurring omissions, P1–P8)
- **`/pre-commit-checklist`** — verify the enumerated cases were actually written before commit
- `.claude/rules/tests-rejection-tdd.md` — every rejection branch needs a direct trigger (cross-cuts P8 + decision table)
- `.claude/rules/tests-arc-leak-pattern.md` — ARC tests use `arcLiveCount()` delta (cross-cuts state transition + boundary value)

---

## Notes

- This skill performs **read-only** operations. It guides technique choice and case enumeration; it does not write tests or run them.
- The "演繹的 vs 帰納的" framing is the canonical distinction between this skill and `/test-checklist`.
- Reference origins: equivalence partitioning and boundary value analysis are codified by Myers (1979); decision tables originate from early CCITT specifications; pairwise / all-pairs from Mandl (1985); state-transition testing from Chow (1978).
