# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

## [0.0.20] - 2026-05-05

### Added

- List destructuring assignment. `a, b = some_list` (and the parenthesized
  form `(a, b) = some_list`) now unpacks a `List<T>` whose runtime length
  matches the number of positions on the left, where each `_` wildcard
  still counts as a position (so `_, b = some_list` requires two RHS
  elements). The `_` wildcard, `@const` prefix, and function-return values
  work the same as for tuple destructuring. A length mismatch aborts with
  `runtime error: list destructuring expected N elements but got M`,
  matching Python's semantics. The motivating idiom `a, b = split(s, " ")`
  now works without an intermediate temporary. (#1567)
- `@native` stdlib functions imported with `from <module> import <name>` can now be
  used as **first-class function values**: bound to variables (`let f = toInt`),
  passed to higher-order functions (`xs.map(toInt)`), and forwarded through
  user-defined `fn(...) -> R`-typed parameters. Internally, the codegen
  materializes a single internal LLVM thunk per name (cached) that forwards
  through the existing native dispatch chain, so both bare `@native` and
  `@native("libname")` declarations work identically. Materialization rules:
  (a) names with **multiple overloads** (e.g. `toStr` over `int`/`float`/`bool`,
  most `math` custom-emitter natives like `abs`/`pow`/`round`/`log`) are
  rejected with `ambiguous reference to @native function 'X': multiple overloads
  exist; wrap in a lambda to select one`; (b) names with **default arguments**
  (e.g. `startsWith(haystack, needle, ignoreCase=false)`) materialize at
  full arity — the resulting binding requires every parameter; the
  default-omission shortcut is only available on the original direct call.
  User-defined `fn` declarations continue to take precedence on name conflict
  (the new path activates only when the user-fn lookup misses). (#1569)
- `sequence(values: List<Result<T, E>>) -> Result<List<T>, E>` and
  `sequence(values: List<Option<T>>) -> Option<List<T>>` for folding
  a list of `Result`/`Option` into a single `Result`/`Option` of list,
  short-circuiting on the first `Err`/`None`. Empty list returns
  `Ok([])` / `Some([])`. UFCS form `xs.sequence()` is also supported.
  (#1570)
- `count(string: str, substring: str, ignoreCase: bool = false) -> int` to the
  `str` module. Returns the number of non-overlapping occurrences of
  `substring` in `string`, matching Python / Go semantics
  (`"aaaa".count("aa") == 2`). Empty `substring` returns `byteLen + 1` (gap
  count); `ignoreCase` performs ASCII-only folding; arguments may contain
  embedded NUL bytes. (#1571)
- Single-parameter lambdas may now omit the parentheses when the parameter
  has no type annotation and the body is a single expression: `xs.filter(s => s == "1")`.
  Multi-arg, type-annotated, and block-bodied lambdas keep their existing paren-required
  syntax. (#1572)
- `digits(n: int) -> List<int>` and `digits(n: int, base: int) -> List<int>`
  to the `math` module. Decomposes a non-negative integer into its digits
  low-first (least-significant digit at index 0), matching Ruby's
  `Integer#digits` (`digits(1234) == [4, 3, 2, 1]`, `digits(255, 16) == [15, 15]`,
  `digits(0) == [0]`). Default base is 10. Composes with `sum` for digit-sum
  in one expression: `sum(digits(1234)) == 10`. Aborts with a runtime error
  on negative `n` or `base < 2`. (#1578)

### Removed

- `!!` operator. Use `?` instead — the two operators were identical aliases since
  introduction, and removing one eliminates a stylistic split that added review
  cost without benefit. Sources using `!!` will fail to parse; replace each `!!`
  with `?`. (#1568)

### Fixed

- ARC retain was missing on str elements when destructuring an unannotated `List<str>` produced by `split()`: `parts = split("a b", " "); a, b = parts; parts = split("c d", " ")` would release the original list (and its str elements) on the source rebind while `a` / `b` still held raw pointers into the freed strings — UAF only avoided in tests because the str-aware destructor was suppressed by a #1266 carve-out, leaving the strings leaked rather than dangling. The root cause was a counter asymmetry between `__ry_arc_alloc_counted` (which incremented `g_arc_live_count` by +1) and `makeString` / `makeStringUninit` / `freeStringSlot` (which were no-ops on the counter). With the counter symmetric for every dynamic str allocation, the `split()` emitter (`src/codegen_call_string.cpp`) now safely stamps `list_elem_type_name = "str"` on its result so `resolveCollectionDestructor` dispatches to the str-aware variant — and `tryRetainArcSource` Case 4 now emits the missing element retain on `a, b = parts` for the untyped form. The `xs: List<str> = ...` annotation branch in `src/codegen_stmt.cpp` was extended to stamp the same metadata, so typed and untyped destructuring share one path. The pre-existing `#1266` carve-out in `.claude/rules/codegen-arc-cow.md` is narrowed to historical context, since "no stamp without retain" no longer applies once allocate / free are counter-symmetric. New regression coverage: `tests/filecheck/arc_retain_list_destruct_str_elem_untyped.ry` (IR-level, asserts the `getelementptr i8, ptr %destruct_elem_*, i64 -24` retain block on each destructured element) and a new spec case in `tests/spec/arc_split_chars.test.ry` ("split() untyped — destructure + rebind does not leak"). `tests/spec/arc_split_chars.test.ry` and `tests/spec/arc_list_destructure.test.ry` (case 3) had their `arcLiveCount` deltas re-baselined and their commentary rewritten to reflect the symmetric regime; `tests/spec/str_arc.test.ry` two `ExprStmt` release tests were re-baselined for the same reason. (#1576)
- The intermediate buffer of a chained string concatenation `"x" + "y" + "z"` was never released, leaking one `StringHeader` allocation per inner concat. `emitArithmeticOp`'s str+str branch (`src/codegen_expr.cpp`) allocated a result buffer with `__ry_string_make_uninit` and inserted it into `arc_str_owned_values_`, but never released the lhs/rhs operands when those were themselves freshly-produced concat buffers. For `BinaryExpr("+", BinaryExpr("+", "x", "y"), "z")`, the inner `"xy"` buf survived past its only use as the outer concat's lhs because `emitStmt(ExprStmt)` only releases the outermost SSA value. The fix releases lhs/rhs at the concat site itself — after the second `memcpy` and before `arc_str_owned_values_.insert(buf)` — for any operand that was tracked in `arc_str_owned_values_`. Because the release is local to the concat emitter, it works in every enclosing context (bare `ExprStmt`, let-binding, return value, function arg, nested binary). Verified via `runtime_internal.arcLiveCount()` delta assertions in `tests/spec/str_arc.test.ry` (chained 3-arg / 4-arg / let-binding now net to the correct counter delta) and a new IR golden `tests/filecheck/str_concat_chain_release.ry` that asserts the inner `__ry_string_make_uninit` buf is followed by an `arc.release.body` block before the outer buf is stored. Combined with the #1576 ARC counter symmetrization, chained concat in `ExprStmt` position now nets to delta `0`. (#1583)

## [0.0.19] - 2026-05-04

### Added

- `release.yml` now runs a `codeql-gate` preflight job that confirms the `codeql.yml` workflow run for the exact `github.sha` finished with `conclusion=success` before the `release` job is allowed to publish artifacts. Previously, `release.yml` (`build` → `release`) ran independently of `codeql.yml`, so a tag push that arrived close to a main push could publish a GitHub Release before the corresponding CodeQL analysis had even completed — observed at v0.0.18 (#1539 follow-up), where the GitHub Release was published 2026-05-03T03:55:21Z while the CodeQL run for `e3431e86` was still `in_progress`. The new gate uses `gh api` to query `actions/workflows/codeql.yml/runs?head_sha=<SHA>&event=push`, deliberately filtering by `event=push` so only mainline analysis state counts (PR-triggered and `workflow_dispatch`-triggered CodeQL runs do not satisfy the gate). The polling logic is two-phase: Phase 1 waits up to `INITIAL_WAIT_SECONDS` (default 120s) for a CodeQL run to appear for the SHA — needed because main-push and tag-push arrival order is not guaranteed and the CodeQL run may not be enqueued yet when `release.yml` starts — and Phase 2 then polls up to `POLL_TIMEOUT_SECONDS` (default 30 minutes) at `POLL_INTERVAL_SECONDS` (default 30s) intervals until the run reaches `status=completed`. The gate fails closed: `failure` / `cancelled` / `timed_out` / `action_required` / Phase 1 timeout / Phase 2 timeout all exit non-zero and prevent `release` from running. To intentionally bypass the gate (CodeQL outage, broken analysis), `release.yml` now also accepts a `workflow_dispatch` boolean input `skip_codeql_gate` (default `false`); the gate job's `if:` skips itself when the input is `true`, and the `release` job's `if:` accepts either `success` or `skipped` from the gate (with `always() && needs.build.result == 'success'` so a build failure still blocks release). All three timing parameters are exposed as job-level `env:` so the polling script body can be exercised locally with shortened timeouts. `.claude/skills/preparing-for-release/SKILL.md` is updated with a new "Note: CodeQL gate is enforced by `release.yml`" section after Step 3 explaining the operational consequences (no manual pre-tag CodeQL check needed; `event=push` filter; escape hatch via `workflow_dispatch`), and the Release prep / Release issue templates each carry a short `## Note` paragraph pointing at the gate. End-to-end verification of the gate against a real tag push will only be observable at the next stable release; pre-merge checks were limited to YAML/bash syntax validation and local execution of the polling script with shortened timeouts against a known-success SHA (`e3431e86`) and a synthetic nonexistent SHA. (#1542)
- Registered `@public` as a compiler built-in directive in `src/directive_meta.cpp`'s `builtinDirectiveRegistry()` so `@public fn`, `@public name = value`, and `@public record` declarations parse and codegen cleanly without any visibility effect (the annotation is inert at this stage). The signature carries `T::Function | T::Record | T::Statement` for `allowed_targets`, takes zero positional arguments (`min_pos=0`, `max_pos=0`), and has no custom validator — matching the bootstrap pattern of `@native` (compiler builtin, not declared in `share/std/core/directive.ry`). This unblocks #1546 (stdlib `@public` annotation) by accepting the syntax at parser/registry level; the visibility effect itself (default = package-internal, `@public` = universe) is implemented in #1544, and the `docs/reference/directives.md` reference update belongs to #1547. Out of scope: enum and type-alias targets — `EnumStmt` and `TypeAliasStmt` lack a `directives` field and the parser explicitly rejects directives on them at `src/parser.cpp:446-449,547-548`; stdlib has zero such declarations today, so #1546 is not blocked. Regression tests: `DirectiveTest.{PublicDirectiveRegistered,PublicDirectiveOnFnCodegenPass,PublicDirectiveOnLetCodegenPass,PublicDirectiveOnRecordCodegenPass,PublicDirectiveRejectsArgs}` in `tests/test_codegen_directive.cpp` (the last one is the direct `max_positional=0` rejection-branch regression per `.claude/rules/tests-rejection-tdd.md`); `ParserTest.{PublicOnFn,PublicOnLet,PublicOnRecord}` in `tests/test_parser.cpp`. (#1545)

### Changed

- **Breaking:** Flipped the default symbol visibility from universe to **package-internal**. Definitions are now visible only within the same package by default; the new `@public` directive (registered as a compiler builtin in #1545 and applied to stdlib in #1546) makes a definition visible across package boundaries. A **package** here is the directory tree rooted at the nearest ancestor `package.toml`; files without any `package.toml` ancestor share a single anonymous package, so ad-hoc scripts and the REPL `-c` input continue to see each other but cannot reach non-`@public` symbols inside any rooted package (including stdlib internals). The `@public` annotations added in #1546 cover every public-facing stdlib symbol, so existing user code that imports stdlib via `from math import sqrt`, `from io import readText`, etc. continues to work; the import error message for the new failure mode is `cannot import 'X' from module 'Y': symbol is not @public` (`src/module_loader.cpp`), distinct from the existing `'X' not found in module 'Y'` so a missing-vs-private mistake is diagnosable. (#1544)
- **Breaking:** Removed the legacy underscore-prefix visibility convention. The `_` leading underscore on identifiers no longer marks a definition as private — it is now a stylistic naming convention with no compiler-enforced meaning. Wildcard imports (`from m`) no longer filter `_`-prefixed names, named imports of `_`-prefixed symbols (`from m import _x`) no longer error, and directory modules no longer skip `_*.ry` files. All visibility decisions go through `@public` and the package boundary; the `_` prefix is purely cosmetic. The pre-existing parse-time check that allowed `_camelCase` identifiers is unchanged — only the import-side filtering and file-skip logic was removed. The `isPrivateName()` helper in `src/module_loader.cpp` is gone, and `extractDefinitions()` was refactored into a `collect` + `filter` pair so cross-package access errors can distinguish "not found" from "not @public". (#1544)
- Extended `EnumStmt` and `TypeAliasStmt` AST nodes with a `directives` field so `@public` can be applied uniformly to every export-bearing definition kind (`fn`, `record`, `enum`, `type` alias, `let`, `@directive`). Previously these two node kinds had no directives slot and were always treated as public; the parser now attaches directives parsed before `enum` / `type` declarations, the formatter round-trips them, and `isPublicDefinition()` in `src/module_loader.cpp` consults the field. New cross-package visibility tests in `tests/test_codegen_stmt.cpp` and parser positive tests in `tests/test_parser.cpp` lock the behavior. This closes the AST gap that was originally tracked separately as #1559 — folded into #1544 because the visibility-default flip is incomplete without it. (#1544)
- Added `share/std/package.toml` so the standard library has its own package root. Without this file, every stdlib `.ry` file would have walked up past the install prefix, found no `package.toml`, and shared the anonymous package with the user's code — making the stdlib's package-internal symbols silently importable. The manifest is minimal (`[project] name = "std"`, no `entry`); a round-trip test in `tests/test_project_config.cpp` locks the empty-`entry` parse + serialize behavior so a future tightening of `[project]` requirements does not break stdlib silently. The file is installed into `$RY_HOME/share/std/package.toml` by the existing `share/std/` install rule (no `CMakeLists.txt` change required). (#1544)
- Annotated every public-facing symbol in `share/std/` with `@public` to prepare for the v0.0.19 default-visibility flip from universe to package-internal (#1543's case A-X). Coverage: **256** `@public` annotations across 19 stdlib `.ry` files (`base64`, `builtins`, `convert`, `filesystem`, `gc`, `higher_order`, `http`, `io`, `json`, `list`, `map`, `math`, `net`, `path`, `regex`, `set`, `str`, `testing`, `thread`). All `@native` functions, the four math-module constants (`PI`, `E`, `INF`, `NAN`), the four `testing.ry` directive declarations (`it`, `describe`, `each`, `property`), and the resource-cleanup helpers (`httpClientResponseFree`, `jsonFree`, `lockFree`, `rwlockFree`, `semaphoreFree`, `barrierFree`, `atomicIntFree`, `atomicBoolFree`) are marked `@public`. Two modules are intentionally left without `@public`, keeping their symbols package-internal: `share/std/core/directive.ry` (re-exported via `builtins.ry` within the same stdlib package) and `share/std/runtime_internal/runtime_internal.ry` (test-instrumentation only, not public API). The annotations are inert at this point — `@public` was registered as a compiler builtin in #1545 (parser/registry-level acceptance only) and the visibility effect itself lands in #1544 when the default flips. Existing tests, golden files, and ABI surface are unchanged. (#1546)
- Relaxed the `@directive can only be combined with @public` parser constraint (`src/parser.cpp`) so `@public` may be combined with `@directive(target=...)` declarations. Previously the parser rejected any directive list containing `@directive` plus a second directive (the original wording: "@directive cannot be combined with other directives"); the relaxation is a strict whitelist — only `@public` is admitted alongside `@directive`, and combinations with any other directive (e.g. `@inline @directive`, `@native @directive`, `@deprecated @directive`) continue to be rejected with the new wording "@directive can only be combined with @public". `DirectiveDefStmt` was extended with a `directives: std::vector<Directive>` field that captures auxiliary directives (today: `@public`) attached to the declaration; the `@directive` entry itself is excluded from that field because its contents are already represented by the dedicated `name` / `targets` / `params` fields on the AST node. The relaxation is required so `share/std/testing/testing.ry`'s four `@directive fn` declarations (`it`, `describe`, `each`, `property`) can also be marked `@public`; without it, `@public + @directive` would parse-error and #1546's stdlib annotation would be incomplete. The validation hook for the new field is intentionally deferred to #1544 — at this point the field is collected but not consumed by `validateDirectives`, matching the inert design of `@public` until the default flips. Regression tests: `DirectiveDefParserTest.{AcceptsPublicBeforeDirective,AcceptsPublicAfterDirective,AcceptsBareDirectiveHasEmptyDirectives,RejectsCombiningDirectiveWithInlineBefore,RejectsCombiningDirectiveWithInlineAfter}` in `tests/test_parser.cpp` (the pre-existing `RejectsCombiningDirectiveWithDeprecated{Before,After}` continue to guard the non-`@public` rejection branch) plus `FormatterTest.DirectiveDefRoundTripWithPublic` in `tests/test_formatter.cpp` for the formatter roundtrip. (#1546)
- Removed 6 dangling references to a previously existing `KNOWLEDGE.md` from `.claude/rules/` (4 files: `tests-arc-leak-pattern.md`, `codegen-pattern-and-match.md`, `codegen-stdlib-dispatcher.md`, `codegen-arc-cow.md`). All references were broken pointers — the surrounding sentences are self-contained, so only the citation fragments were removed. Brings the rules tree into alignment with `/knowledge-md-management` REQ-3 (no individual-entry references to KNOWLEDGE.md from `AGENTS.md` / `.claude/rules/` / `.claude/skills/` / `.claude/agents/`). Also updated the skill's own self-reference (`.claude/skills/knowledge-md-management/SKILL.md`) to mark the cleanup as completed. Knowledge-base only — no code, ABI, or build behavior changes. (#1550)

### Removed

- **Breaking:** Removed the `_*.ry`-file skipping convention from directory module loading. Files like `share/std/list/_helpers.ry` (none currently exist in the tree) were previously skipped during `loadModuleDir()`; now every `.ry` file in a directory module is loaded. Test files (`*.test.ry`) continue to be skipped as before — the `.test.ry` suffix is a separate, retained convention. (#1544)

### Fixed

- Stripped the lone blank line between the leading header comment block and the first declaration in `share/std/core/directive.ry` and `share/std/runtime_internal/runtime_internal.ry` so the two files match what `ry fmt` emits. Pre-existing drift carried since #1490 and surfaced during #1546's stdlib `@public` self-verification — both files were intentionally left without `@public` (kept package-internal) and so were not touched there, yet they still failed `ry fmt --check` on `main`. The other 19 stdlib `.ry` files already follow the "header comment, then declaration with no separating blank line" convention; this aligns the last two outliers with that de-facto rule. The formatter behavior itself is unchanged — fixing the source is the cheaper of the two directions sketched in #1554, and no formatter idempotency rule needed to change. No code, ABI, or build behavior changes; `ry fmt --check share/std/**/*.ry` now exits 0 across all 21 stdlib files. (#1554)
- Cross-package and same-package selective wrapper patterns (REQ-B3 from #1543) now work as documented: a `@public` facade can transitively call non-`@public` helpers in the same package, and external importers can `from foo import bas` without `bas`'s body failing with `undefined function: helper`. Previously the visibility filter in `extractDefinitions` (`src/module_loader.cpp`) doubled as a code-availability filter — non-`@public` definitions were dropped from the importer's program even when only the `@public` facade was named, so the helper could not be linked at JIT codegen. The filter is now decoupled into two layers: code availability (always copy every exportable definition into `dest` for codegen) and name visibility (the `from foo import <name>` validation still rejects non-`@public` names cross-package, satisfying REQ-A1). The four `*WildcardExcludesNonPublic` test cases for functions, `@directive` defs, enums, and type aliases were renamed to `*WildcardIncludesAllForCodegen` and flipped to assert the new semantics; seven new wrapper-pattern tests were added (cross-pkg selective in/out same file, cross-pkg wildcard, same-pkg selective, same-pkg wildcard, REQ-A1 regression, two importers of the same module). **Known limitations**: (a) two cross-package modules with the same internal helper name now collide at codegen with `duplicate function`; (b) an importer-local function colliding with an imported package-internal helper similarly collides — same-package selective imports surface this where they used to silently drop the helper. Workarounds: rename the colliding helper or restructure to use a single canonical module. `docs/guide/visibility.md` was rewritten to describe the working pattern and the collision limitations; `docs/reference/directives.md` cross-package wildcard example was updated to match. (#1560)

## [0.0.18] - 2026-05-03

### Added

- New `.claude/skills/test-design-techniques/` skill cataloguing five general-purpose deductive test design techniques (equivalence partitioning, boundary value analysis, state transition testing, decision table testing, pairwise / all-pairs) with definitions and ry-specific application examples (parser numeric literals, `int64` overflow boundaries, type-inference scope transitions, operator overload resolution decision tables, native-dispatch pairwise axes). The new skill is the **deductive** complement to the existing `/test-checklist` (which is **inductive** — ry-specific recurring omissions P1–P8); `/tdd-cycle` and `/test-checklist` both gain a one-line cross-reference pointing at `/test-design-techniques` so the テスト作成 step starts with deductive enumeration and follows up with inductive verification. Knowledge-base only — no code, ABI, or build behavior changes. (#1497)
- New `.claude/agents/devils-advocate.md` sub-agent providing structured contrarian analysis (steel-man reconstruction, multi-angle attack across assumptions / failure modes / hidden costs / alternatives / reversibility / scale / stakeholders / precedent / second-order effects / selection bias, severity-ranked objections, constructive synthesis) for stress-testing design proposals and technical plans. The agent has its own persistent memory at `.claude/agent-memory/devils-advocate/` and is invokable via the `Agent` tool with `subagent_type: "devils-advocate"`. Bundled into the same PR as the `/test-design-techniques` skill addition because it is a complementary internal-tooling change (one PR, one mainline integration). (#1497)
- Added the "計画の抽象度（WHAT/HOW 分離）" principle to `AGENTS.md` §"Plan モードのルール" as a single bullet (one line, keeping AGENTS.md under the 170-line policy from #1498) and introduced a new `.claude/skills/plan-rubric/SKILL.md` skill that evaluates plans on four pass/fail axes (抽象度 WHAT/HOW, スコープ, テスト可能性, 依存表明) before `ExitPlanMode`. The skill includes ry-specific OK/NG examples (stdlib モジュール追加 with `__ry_crypto_sha256` signature anti-pattern, codegen エラー報告 with `emitArithmeticOp` line-number anti-pattern) for the WHAT/HOW axis, defines how to invoke `/test-design-techniques` from a Plan (technique names are listed at Plan-stage as WHAT; concrete cases are expanded later in the Red step as HOW), and documents when to invoke the `devils-advocate` agent (via `Agent` tool with `subagent_type: devils-advocate`) at ExitPlanMode for architectural plans, with skip criteria for single-file bug fixes. The skill is read-only — it inspects plans rather than editing them. Knowledge-base only — no code, ABI, or build behavior changes. (#1501)

### Changed

- Reduced `AGENTS.md` from 255 lines / 20,594 bytes to 164 lines / 12,618 bytes by delegating four duplicated sections to existing rules and a new skill, and compressing additional verbose prose. Sections delegated: compiler warning flags → `.claude/rules/build-warning-flags.md` (zero-warnings policy and `-Werror` status added); LLVM IR golden-test conventions → `.claude/rules/codegen-llvm-ir-conventions.md` (`--emit-llvm-ir` pipeline, manual run command, CI warn-only note added to existing entry); runtime memory-safety table → `.claude/rules/runtime-memory-safety.md` (forbidden-function table, `oom_abort(n)`, NULL-check rule, CI lint note added as new entry); internal behavior trace usage → new `.claude/skills/ry-trace/SKILL.md` with auto-fire description (trigger keywords: `trace`, `--trace`, `--trace-out`, `JSON Lines`, `内部挙動`, `import 解決`, `JIT 実行`). Additionally compressed in-place: terminology preamble, CI container image description, knowledge-base meta section, ASan/UBSan blockquote, Plan-mode prerequisites, repo build & stdlib resolution section, PR-review learning section, workflow overview, issue-driven development section. The heading `## FileCheck IR Golden Tests` was renamed to `## IR ゴールデンテスト` to satisfy the issue's grep-based acceptance criterion (`grep -nE "checked_malloc|FileCheck|-Wall -Wextra"` returns 0 hits). (#1498)
- Simplified `.claude/skills/tdd-cycle/SKILL.md` from 39 lines to 25 lines in line with issue #1500: the "新機能追加時" 3-step enumeration is replaced with a single sentence delegating to general TDD (Red-Green-Refactor) and noting "each test case runs its own internal cycle; do not mark complete after the happy path"; the "既存コードの変更時" 6-step ry-specific flow is preserved verbatim; `## Cross-reference` is compressed from 4 bullets to 2 (テスト観点 / 完了前) while keeping `/test-design-techniques`, `/test-checklist`, and `/pre-commit-checklist` cross-references; the intro sentence, source-of-truth note, and `## Context` section are dropped. A new top-level bullet `**TDD サイクルの分割禁止**` is added to `AGENTS.md §"Plan モードのルール"` prohibiting Red/Green/Refactor split into separate Plan tasks (each case runs its cycle internally). The 2-row `/tdd-cycle` invocation table at `.claude/skills/test-checklist/SKILL.md` is collapsed into a single prose line to follow tdd-cycle's Step renumbering. Knowledge-base only — no code, ABI, or build behavior changes. (#1500)
- Added a `.claude/agents/<name>.md` bullet to the `AGENTS.md` ナレッジベース section so contributors and Claude Code learn, on first read, that subagent definitions live alongside `.claude/rules/` and `.claude/skills/` and which directory holds them. The new bullet documents that agents launch via the `Agent` tool's `subagent_type: <name>` parameter as an **独立コンテキスト**, contrasting with skills which execute in the same context, and notes that `/<name>` slash commands do not address agents (skill ではなく agent のため) — wording that mirrors the existing explanation already in `.claude/skills/plan-rubric/SKILL.md` line 148. The use case (Plan / 設計 / 実装 の批評で artifact だけを別コンテキストに渡したいケース) and the only currently-defined agent (`.claude/agents/devils-advocate.md`) are also called out so the section is accurate today rather than describing a hypothetical fleet. The section heading was extended from `## ナレッジベース (.claude/rules/ + .claude/skills/)` to `## ナレッジベース (.claude/rules/ + .claude/skills/ + .claude/agents/)` for consistency with the new bullet. The "読む" / "書く" / "いつ書く" trailing bullets are intentionally left scoped to `.claude/rules/` + `.claude/skills/` because they describe the lessons-learned workflow (entry format `### <heading>` + `**Source**:` + `**Tags**:` + `**Rule**:`) that does not apply to agent definitions; the agents directory holds subagent specs, not accumulated lessons. The parent #1496 acceptance criterion ("rename `devils-advocate.md` to `critic.md` or create a new `critic.md`") is intentionally not pursued: per the issue-author's call during the Plan-mode AskUserQuestion, the file stays named `devils-advocate.md` and `.claude/skills/plan-rubric/SKILL.md`'s nine `devils-advocate` references are kept as-is. Knowledge-base only — no code, ABI, or build behavior changes. (#1515)
- Refreshed the Cppcheck section of `.claude/rules/build-warning-flags.md` to match the post-#1505 CI environment, where the lint job uses the source-built cppcheck 2.16.0 bundled in the `ghcr.io/<owner>/ry-ci:llvm-21` image (`docker/ci.Dockerfile:163-186`) rather than the Ubuntu 24.04 apt package (cppcheck 2.13). Removed the stale "Gotcha: Cppcheck 2.13 ... does NOT support `#` comment lines in `--suppressions-list` files" paragraph, since the constraint no longer applies under 2.16 (`#` comment support was added in 2.14) — keeping it would also have contradicted the section's own Rule that suppressions must carry a rationale comment. Added a short paragraph after the Rule documenting that inline `// cppcheck-suppress <id>` comments are also supported (CI already invokes `cppcheck --inline-suppr`) and explaining when to prefer them over `.cppcheck-suppressions` entries. Added `#`-prefixed rationale comments to the two existing entries in `.cppcheck-suppressions` (`unknownMacro` global and `syntaxError:src/test_runtime.cpp`) so the file complies with the Rule. Existing inline `// cppcheck-suppress` annotations in `src/runtime_http*.cpp` and `src/runtime_sort.cpp` are left untouched: the Rule covers newly added suppressions, and re-annotating historical ones is out of scope. (#1519)

### Fixed

- Pinned `release.yml`'s Linux container reference from the mutable `ghcr.io/<owner>/ry-ci-glibc-old:llvm-21` pointer to the immutable per-build tag `:llvm-21-rev3` so that re-running the release workflow on an older `vX.Y.Z` source tag pulls the same image bits as the original release. Previously, `build-ci-image.yml`'s `manifest` job overwrote `:llvm-21` on every rebuild, which meant a re-run for an older release would silently pick up today's image and produce bit-different binaries — invalidating downstream `sha256sums.txt`, signatures, and any distro-packager checksum mirrors. The immutable `:llvm-<MAJOR>-rev<N>` tag is published in parallel by the same `manifest` job and is monotonically increasing (never reused), so static-pinning it inside `release.yml` makes the workflow file itself a record of which image rev was used at release time. Other workflows (`ci.yml`, `codeql.yml`, dev `docker/Dockerfile`) continue to use the mutable `:llvm-21` pointer because their artifacts never leave the runner and they should auto-track the latest image. The new convention is documented as a rule in `.claude/rules/ci-workflows.md` ("Release container must pin to immutable `:llvm-<MAJOR>-rev<N>` tag") and a procedure in `.claude/skills/ci-image-workflow/SKILL.md` ("How to update release.yml's pin"), and `.claude/skills/preparing-for-release/SKILL.md` Step 4 now includes a Release-prep task that verifies the pin is fresh against the latest published rev (using the public GHCR token endpoint rather than `gh api .../packages/...` which would require a `read:packages` PAT scope). Knowledge-base + workflow YAML only — no code, ABI, or test behavior changes; E2E verification is only possible at the next real release because `release.yml` runs on `tag` push only. (#1508)
- `ry self-update --nightly` now returns an explicit "no longer supported" error instead of the confusing `Error: Version v--nightly not found.` message. The `--nightly` mode was removed in v0.0.14 (#1372) when self-update was simplified to always target the latest stable release, but `--nightly` still slipped through `is_valid_tag()`'s lenient regex (`^v?[0-9A-Za-z._-]+$`) and was sent to the GitHub release-search path, yielding a misleading error. `cmd_self_update()` (`src/self_update.cpp`) now intercepts the exact string `--nightly` between the `--help` check and the `is_valid_tag()` check, prints `Error: --nightly is no longer supported. self-update always targets the latest stable release.` followed by `(removed in v0.0.14 — see #1372)` to stderr, and returns exit code 1 — directing users at the historical removal PR rather than implying the tag is missing. Match scope is exact-string only: `--nightly=value` and other variants still fall through to `is_valid_tag()` as before, so this is a pure addition of one rejection branch with no behavior change to any other input. Regression test: `HelpOption.SelfUpdateNightlyRejected` in `tests/test_help.cpp` (uses the existing `runRy` fork/exec helper to assert non-zero exit and that stderr contains both `--nightly is no longer supported` and `v0.0.14`). The internal `mode` abstraction's residual nightly-aware branches are out of scope for this CLI UX fix and tracked separately. (#1533)

## [0.0.17] - 2026-05-01

### Added

- New `docs/reference/glossary.md` defining the canonical terminology used across the Ry reference: **Module** (`xxx` in `from xxx import ...` — either a single `.ry` file or a directory of `.ry` files), **Package** (an external library managed by the `ry` command via the planned `ry add` / `ry remove` subcommands — **not yet implemented as of v0.0.17**), **`package.toml`** (the project manifest, named by analogy with Rust's `Cargo.toml` even though it describes a project rather than a package in the strict sense), and **stdlib (`std`)** (the auto-imported standard library — `math`, `io`, `path`, `filesystem`, etc.). Indexed at the top of `docs/README.md` so subsequent reference pages can defer to it. This glossary is the basis for follow-up terminology cleanups in v0.0.17. (#1480)

### Changed

- Renamed `docs/reference/packages.md` to `docs/reference/modules.md` and rewrote the page under v0.0.17's module/package terminology (introduced by #1480's glossary). The page is now titled "Module Reference" and uses "module" exclusively for `from ... import ...` units; the word "package" is reserved for the future `ry add` external-library feature. Section headers updated: "Package Resolution" → "Module Resolution", "Directory Packages" → "Directory Modules", "Sub-packages" → "Sub-modules", "Single File Package" / "Directory Package" → "Single File Module" / "Directory Module"; the `__ry_<package>_<symbol>` placeholder in the Native Function Naming Convention section was renamed to `__ry_<module>_<symbol>`; in-prose import-syntax placeholders were also updated (`from pkg` / `from .pkg` → `from mymodule` / `from .submodule`) so no `pkg` (an abbreviation derivative of "package") survives in the page. Anchor-bearing headers ("Standard Library (`std`)" and "RY_ENV") preserved verbatim so existing inbound links keep resolving. Cross-page link targets updated in `docs/README.md`, `docs/reference/glossary.md`, `docs/reference/project.md`, `docs/reference/directives.md`, `docs/reference/builtins.md`, `docs/reference/naming.md`, and a doc-link comment in `tests/test_parser.cpp:1870-1871` (also rephrased "Package-private" → "Module-private" in that comment for consistency with the new `naming.md` prose); `glossary.md`'s pre-existing-names note was simplified after `packages.md` ceased to exist (only `package.toml` remains as a stability-preserved name); `naming.md` prose changed "package-private names" to "module-private names"; `directives.md`'s `@native` paragraph changed "stdlib packages" / "legacy packages" to "stdlib modules" / "legacy modules" for cross-page consistency. The C ABI symbols themselves (`__ry_base64_encode`, `__ry_filesystem_listDir`, etc.) are unchanged — this is a documentation-terminology rename only. (#1481)
- Swept residual "package" terminology in the root `README.md` and `docs/reference/*.md` so that every stdlib unit imported via `from ... import ...` is consistently called a **module**, completing the v0.0.17 module/package terminology rollout begun by #1480 (glossary) and #1481 (`packages.md` → `modules.md`). `README.md`: feature-list bullet "Packages — Directory-based packages, auto-imported `std` library" → "Modules — Directory-based modules, auto-imported `std` library", and the sample-code comment `# Package import` → `# Module import`. `docs/reference/directives.md`: user-defined-directives section opener "Packages can declare their own compile-time directives" → "Modules can declare their own compile-time directives"; "private to the declaring package" → "private to the declaring module" in the export/import paragraph; and the in-prose import-syntax placeholders `pkg/mod.ry` / `from pkg import directiveName` / `from mypkg import logged, cached` → `mymodule/mod.ry` / `from mymodule import directiveName` / `from mymodule import logged, cached` so no `pkg` (a derivative spelling of "package") survives in the page (matching the placeholder convention #1481 established in `modules.md`). `docs/reference/{math,json,gc,thread,filesystem}.md`: package → module in the opening prose introducing each stdlib unit (e.g. "The `math` package provides..." → "The `math` module provides..."); `math.md` also corrects "Unlike the `std` package" → "Unlike the `std` module"; `json.md` corrects the second mention "the package uses an opaque pointer type" → "the module uses..."; `filesystem.md` updates both "the `filesystem` package" and "the `io` package" in the same sentence. `docs/reference/thread.md`: also updates the "thread package" cell in the async/await comparison table to "thread module". `docs/reference/builtins.md`: `input` description "explicitly scoping I/O through the `io` package" → "...the `io` module". `docs/reference/naming.md`: approved-abbreviations table column "Filesystem package" → "Filesystem module" for the `mkdir` / `mkdirAll` rows. Intentionally preserved: the manifest filename `package.toml` and every reference to it across `docs/reference/{project,testing,modules,builtins,glossary}.md` and `docs/README.md` (kept by analogy with Rust's `Cargo.toml` per the glossary's stability note); the `## Package` and ``## `package.toml` `` entries in `docs/reference/glossary.md` (canonical definitions of the v0.0.17 terminology); and the "Module, Package, `package.toml`, stdlib" enumeration in `docs/README.md`'s reference index (it advertises the four glossary entries, all of which still exist). Package management functionality (`ry add` / `ry remove`) remains unimplemented and is reserved for a future release. (#1482)
- **Breaking:** Aligned internal compiler error messages and the import-resolve trace event with the v0.0.17 module/package terminology (introduced by #1480's glossary). User-facing import errors now say "module" wherever they previously said "package": `package not found: X` → `module not found: X` (`src/module_loader.cpp:245,272`), `'name' not found in package 'X'` → `'name' not found in module 'X'` (`:108,375`), `cannot import private symbol 'X' from package 'Y'` → `... from module 'Y'` (`:81,370`), `invalid package path (path traversal): X` / `invalid package path (absolute): X` → `invalid module path (path traversal): X` / `invalid module path (absolute): X` (`:166,168`); parser-side `expected package name after 'from'` → `expected module name after 'from'` (`src/parser.cpp:393`) and `hyphens '-' are not allowed in package names; use underscores '_' instead` → `hyphens '-' are not allowed in module names; use underscores '_' instead` (`:353`). The `import.resolve.error` trace event's `detail` field value changed from `"package not found"` to `"module not found"` — this is the observable breaking change for tooling that consumes `--trace-out` JSON Lines and matches on the `detail` string. Internal API rename: the `ModuleLoader::loadPackageDir(abs_dir_path)` member function is renamed to `loadModuleDir(abs_dir_path)` (`include/ry/module_loader.hpp:56`, `src/module_loader.cpp:310,384`); only the in-process header API is affected — there is no external C ABI surface for this method. Header / implementation comments referring to "package/file", "package_path" cache key, "Resolve a package path", "from a package directory", "Directory (package)", and "Exclude test files from package loading" are likewise updated to "module" wording. Six regression tests lock in the new wording: `ParserTest.{ImportHyphenError,ImportRelativeHyphenError,ImportFromMissingModuleNameError}` in `tests/test_parser.cpp` and `ImportTest.{ModuleNotFoundErrorMentionsModule,NameNotFoundErrorMentionsModule,PrivateImportErrorMentionsModule}` in `tests/test_codegen_stmt.cpp` — each new test asserts both the presence of the new "module" wording and the absence of the old "package" wording. The `package_path` parameter name in `ModuleLoader::resolve` is intentionally preserved (the issue scoped wording-only edits to comments containing `package_path`, not the parameter itself); the pre-existing internal asymmetry between the `module_path` trace key and the `package_path` parameter it reads from is retained for this PR. The manifest filename `package.toml` is similarly preserved per #1480's stability note. (#1483)
- Refreshed the internal knowledge base (`.claude/skills/`, `.claude/rules/`, `AGENTS.md`) to v0.0.17's module/package terminology, completing the rollout begun by #1480/#1481/#1482/#1483. The `.claude/skills/stdlib-package-add/` skill is renamed to `.claude/skills/stdlib-module-add/` (with `git mv` to preserve history) and its `SKILL.md` rewrites the procedure prose to "stdlib module" (placeholders also flipped: `<pkg>` → `<mod>`, `@native("pkg")` → `@native("mod")`); the path-scoped rule `.claude/rules/stdlib-package-additions.md` is renamed to `.claude/rules/stdlib-module-additions.md` and its prose / placeholders updated identically. Cross-references inside the skill body and the rule body that pointed to the old filenames are repointed; four sibling rule files (`tests-arc-leak-pattern.md`, `tests-cpp-conventions.md`, `codegen-stdlib-dispatcher.md`, `runtime-memory-safety.md`) and `build-warning-flags.md`'s self-registration paragraph have their "stdlib package(s)" / "stdlib-package" prose flipped to the module spelling. A second-pass `[Pp]kg` derivative grep (per the `docs-reference-conventions.md` "lexical derivatives" methodology) caught additional `<pkg>` placeholders inside `tests-cpp-conventions.md` (`share/std/<pkg>/<pkg>.ry`, `__ry_<pkg>_<oldName>`, `runSource("...@native(\"<pkg>\")...")`, `from pkg import _name`) and `codegen-stdlib-dispatcher.md` (`share/std/<pkg>/<pkg>.ry`, `libry_<pkg>.dylib`, `used_native_libraries_.insert("<pkg>")`, `@native("pkg")` cross-reference) — all flipped to `<mod>` / `mod` for consistency with the renamed skill body. Two further skills picked up incidentally from the `[Pp]ackage` grep: `commands-environment-gotchas/SKILL.md` updates its quoted module-loader error from `'encode_bytes' not found in package 'base64'` to `'encodeBytes' not found in module 'base64'` so the example matches the diagnostic emitted by `src/module_loader.cpp:108,375` after #1483 (and the camelCase function spelling adopted in #1415); `git-branch-naming/SKILL.md` updates the `feat/add-crypto-stdlib` example caption from "new standard library package" to "new standard library module". `AGENTS.md` gains a top-level terminology note pointing at `docs/reference/glossary.md` so contributors land on the canonical v0.0.17 definitions on first read. Intentionally preserved: legacy C++ identifiers (`effectivePackage`, `deriveNativePackage`, `RY_REGISTER_STDLIB_PACKAGE`), ABI symbols (`__ry_<symbol>` prefix family), apt/OS package mentions in CI prose (`build-warning-flags.md:114` "Ubuntu 24.04 package", `ci-workflows.md` "LLVM packages", `static-analysis-tools/SKILL.md:66` "apt パッケージ", `llvm-mirror-workflow/SKILL.md`'s `extra-packages` action input), and the `package`/`pkg` example tokens inside `docs-reference-conventions.md`'s terminology-sweep methodology rule (those are the rule's own worked example, not stale prose). The manifest filename `package.toml` is also kept verbatim per the glossary's stability note. This is a documentation/knowledge-base change only; no code or build behavior is affected. (#1484)
- Final v0.0.17 terminology sweep: replaced remaining incorrectly-used "package" (in the sense of "module") references across stdlib `.ry` source comments, test descriptions, GoogleTest names, and C++ source/header prose comments with "module", completing the cleanup begun by #1480/#1482/#1483/#1484. Edits cover (A-1) `share/std/filesystem/filesystem.ry` and `share/std/runtime_internal/runtime_internal.ry` Ry comments; (A-2) `tests/spec/relative_import/relative_import.test.ry` `it()` description and 6 GoogleTest names in `tests/test_codegen_stmt.cpp` (`DirectoryPackage{ImportAll,SelectiveImport,SkipsUnderscoreFiles,FallbackToFile}` → `DirectoryModule*`) and `tests/test_parser.cpp` (`RelativeImportDot{Subpackage,NestedSubpackage}` → `*Submodule`); (A-3) C++ source comments in `src/codegen_call_runtime_internal.cpp`, `src/runtime_arc_counter.cpp`, and `src/codegen_fn.cpp`; (A-4) C++ header/source prose comments in `include/ry/stdlib_registry.hpp`, `include/ry/codegen.hpp`, `include/ry/codegen_native_dispatch.hpp`, `src/codegen_call_dispatch.cpp`, and `src/codegen_call_native.cpp`; (A-5) `effectivePackage` description prose in `src/codegen_fn.cpp:578` rewritten to "use library name as the effective module"; (B-3) `src/project_config.cpp:195` "in package name" → "in project name" (the `[project] name` field is what is being normalized, so the original prose was a `package` term misuse); (B-6) `src/codegen_call_native.cpp:597` placeholder comment `__ry_<package>_<fn_name>` → `__ry_<module>_<fn_name>`. Intentionally preserved (legacy ABI / source-stability identifiers per the glossary): `effectivePackage` / `deriveNativePackage` C++ variables, `RY_REGISTER_STDLIB_PACKAGE` macro, `StdlibPackageEntry` struct, `sig.package` field, the `__ry_<symbol>` C symbol prefix family, and the `package.toml` manifest filename. Test identifiers in `tests/test_builtin_stdlib_registry.cpp` and `tests/test_codegen_directive.cpp` are kept as-is because they directly assert on the legacy `StdlibPackageEntry::package_name` and `sig.package` fields. Parameter names like `package_path` (`include/ry/module_loader.hpp:53`) and `package` (`include/ry/codegen.hpp`) are also kept; renaming them is tracked separately to keep this PR's scope minimal. The historical "per-package docs were consulted" prose in `.claude/rules/docs-reference-conventions.md:264` is retained as-is because it documents a v0.0.17 pre-glossary lesson in its original wording. This is a documentation/knowledge-base / test-name change only; no code, ABI, or build behavior is affected. (#1490)

## [0.0.16] - 2026-04-30

### Added

- Reference documentation for Ry's v0.0.16 naming conventions (`docs/reference/naming.md`): camelCase for functions/variables/fields, PascalCase for records/enums/type aliases, acronym first-letter-only rule, approved abbreviations table, and verbose-by-intent rationale for `toInt`/`toStr`. (#1410)

### Changed

- **Breaking:** Renamed three built-in functions to align with the v0.0.16 naming conventions: `length(...)` → `len(...)` (all overloads — `str`, `List`, `Map`, `Set`, and the JSON value form), `arguments()` → `args()`, and `available_parallelism()` → `availableParallelism()`. The old names are removed entirely; there is no alias or deprecation period. `print`, `input`, `range`, `zip`, `exit`, `sleep`, `env`, and `enumerate` keep their existing names (`enumerate` cannot be shortened because `enum` is a reserved keyword). (#1411)
- **Breaking:** Renamed string-family stdlib functions to align with the v0.0.16 naming conventions. `share/std/str`: `starts_with` → `startsWith`, `ends_with` → `endsWith`, `byte_len` → `byteLen`, `substring` → `substr` (only this one shortens, per the approved abbreviation table), `char_at` → `charAt`, `to_upper` → `toUpper`, `to_lower` → `toLower`, `trim_start` → `trimStart`, `trim_end` → `trimEnd`. `contains`, `find`, `replace`, `trim`, `repeat`, `reverse`, `split`, `join` are already single-word and unchanged. `share/std/convert`: `to_int` → `toInt`, `to_str` → `toStr`, `to_float` → `toFloat` (the `to`-prefix is intentional, not abbreviated to `int` / `str` / `float`, to avoid colliding with the type-name spellings). `share/std/json`: `to_str` / `to_int` / `to_float` / `to_bool` → `toStr` / `toInt` / `toFloat` / `toBool`, and `json_free` → `jsonFree`. `share/std/regex`: `regex_match` / `regex_search` / `regex_replace` / `regex_split` / `regex_find_all` → `regexMatch` / `regexSearch` / `regexReplace` / `regexSplit` / `regexFindAll`, and `is_match` / `find_all` → `isMatch` / `findAll`. The old names are removed entirely; there is no alias or deprecation period. (#1412)
- **Breaking:** Renamed collection-family stdlib functions to align with the v0.0.16 naming conventions. `share/std/list`: `remove_at` → `removeAt`, `flatten` → `flat` (per the approved abbreviation table), `is_empty` → `isEmpty`. `share/std/map`: `has_key` → `hasKey`. `share/std/set`: `symmetric_difference` → `symmetricDifference`, `is_subset` → `isSubset`, `is_superset` → `isSuperset`. `share/std/higher_order` requires no renames. The old names are removed entirely; there is no alias or deprecation period. (#1413)
- **Breaking:** Renamed `math.Inf` → `math.INF` and `math.NaN` → `math.NAN` so the only mathematical constants exported from `share/std/math` use SCREAMING_SNAKE_CASE consistently with `math.PI` and `math.E`. The previous PascalCase spellings were the only stdlib constants that did not follow the camelCase-or-SCREAMING_SNAKE_CASE rule. The old names are removed entirely; there is no alias or deprecation period. (#1470)
- **Breaking:** Renamed path-family and filesystem-family stdlib functions to align with the v0.0.16 naming conventions. `share/std/path`: `extension` → `ext` (per the approved abbreviation table), `is_absolute` → `isAbsolute`. `share/std/filesystem`: `make_dir` → `mkdir` (POSIX-aligned short form), `make_dir_all` → `mkdirAll`, `glob_files` → `glob` (the suffix is redundant given the package), `list_dir` → `listDir`, `remove_all` → `removeAll`, `file_size` → `fileSize`, `is_file` → `isFile`, `is_dir` → `isDir`, `is_symlink` → `isSymlink`, `read_link` → `readLink`. `walk`, `copy`, `move`, `remove`, `chmod`, and `symlink` are already single-word and unchanged. The old names are removed entirely; there is no alias or deprecation period. (#1414)
- **Breaking:** Renamed I/O-network family stdlib functions to align with the v0.0.16 naming conventions. `share/std/io`: `read_line` → `readLine`, `read_all` → `readAll`, `read_text` → `readText`, `write_text` → `writeText`, `append_text` → `appendText`, `delete_file` → `deleteFile`, `read_bytes` → `readBytes`, `write_bytes` → `writeBytes`, `to_bytes` → `toBytes`, `bytes_to_str` → `bytesToStr`. `share/std/net`: `listener_port` → `listenerPort`, `tls_connect` → `tlsConnect`, `set_timeout` → `setTimeout`, `set_receive_timeout` → `setReceiveTimeout`, `set_send_timeout` → `setSendTimeout`. `share/std/http`: `body_bytes` → `bodyBytes`, `query_all` → `queryAll`, `form_field` → `formField`, `form_fields` → `formFields`, `form_file` → `formFile`, `http_get` → `httpGet`, `http_post` → `httpPost`, `http_request` → `httpRequest`, `http_client_response_free` → `httpClientResponseFree`. `share/std/base64`: `encode_url_safe` → `encodeUrlSafe`, `decode_url_safe` → `decodeUrlSafe`, `encode_bytes` → `encodeBytes`, `encode_bytes_url_safe` → `encodeBytesUrlSafe`, `decode_bytes` → `decodeBytes`, `decode_bytes_url_safe` → `decodeBytesUrlSafe`. The old names are removed entirely; there is no alias or deprecation period. (#1415)
- **Breaking:** Renamed thread/atomic/math/gc/runtime_internal stdlib functions and codegen-only builtins to align with the v0.0.16 naming conventions. `share/std/thread`: `thread_spawn` → `threadSpawn`, `thread_join` → `threadJoin`, `lock_new` → `lockNew`, `lock_acquire` → `lockAcquire`, `lock_release` → `lockRelease`, `lock_free` → `lockFree`, `rwlock_new` → `rwlockNew`, `rwlock_read_lock` → `rwlockReadLock`, `rwlock_write_lock` → `rwlockWriteLock`, `rwlock_unlock` → `rwlockUnlock`, `rwlock_free` → `rwlockFree`, `semaphore_new` → `semaphoreNew`, `semaphore_acquire` → `semaphoreAcquire`, `semaphore_release` → `semaphoreRelease`, `semaphore_free` → `semaphoreFree`, `barrier_new` → `barrierNew`, `barrier_wait` → `barrierWait`, `barrier_free` → `barrierFree`, `atomic_int_new` → `atomicIntNew`, `atomic_int_load` → `atomicIntLoad`, `atomic_int_store` → `atomicIntStore`, `atomic_int_add` → `atomicIntAdd`, `atomic_int_sub` → `atomicIntSub`, `atomic_int_cas` → `atomicIntCas`, `atomic_int_free` → `atomicIntFree`, `atomic_bool_new` → `atomicBoolNew`, `atomic_bool_load` → `atomicBoolLoad`, `atomic_bool_store` → `atomicBoolStore`, `atomic_bool_free` → `atomicBoolFree`. `share/std/math`: `is_nan` → `isNan`, `is_inf` → `isInf`. `share/std/gc`: `set_threshold` → `setThreshold`. `share/std/runtime_internal`: `arc_live_count` → `arcLiveCount`. Codegen-only builtins: `type_of` → `typeOf`, `block_on` → `blockOn`, `and_then` → `andThen`, `to_list` → `toList`, `checked_add`/`checked_sub`/`checked_mul` → `checkedAdd`/`checkedSub`/`checkedMul`, `saturating_add`/`saturating_sub`/`saturating_mul` → `saturatingAdd`/`saturatingSub`/`saturatingMul`, `wrapping_add`/`wrapping_sub`/`wrapping_mul` → `wrappingAdd`/`wrappingSub`/`wrappingMul`. Runtime symbols (`__ry_*`) follow the existing package-specific ABI convention: most packages mirror the Ry identifier spelling, while legacy `base64`/`string` symbols remain snake_case; `__ry_gc_set_threshold` was updated to `__ry_gc_setThreshold` under generic native dispatch. The old names are removed entirely; there is no alias or deprecation period. (#1416)
- Audited all `record` and `enum` declarations repo-wide for compliance with the v0.0.16 acronym rule (`HttpClient` not `HTTPClient`). Of 146 declarations, 6 test-only types had a multi-letter capital prefix and were corrected: `TAPoint` → `TaPoint`, `TAName` → `TaName`, `NTRecA`/`B`/`C` → `NtRecA`/`B`/`C` (`tests/spec/type_advanced.test.ry`), and `FStrExplicit` → `FstrExplicit` (`tests/spec/type_of.test.ry`). Additionally, 8 type aliases in `tests/spec/type_advanced.test.ry` were renamed for visual consistency with the renamed records: `TAVal` → `TaVal`, `TAShape` → `TaShape`, `NTAInner`/`Outer` → `NtaInner`/`Outer`, `NTRInner`/`Outer` → `NtrInner`/`Outer`, `NCycA`/`B` → `NcycA`/`B`. Inner-scope short placeholder aliases (`NA1`–`NC4` inside `it()` bodies) were left as-is — they are test-local and produce no visual mismatch with the renamed records. (#1417)
- **Breaking:** Flipped the parser's user-identifier casing enforcement from `snake_case` to `camelCase`, completing the v0.0.16 naming convention switch. User-defined `fn` and `@directive` names now require camelCase via `isMutationFnName` (which still permits the trailing `!` mutation suffix, e.g. `clear!`, `appendInPlace!`); `@native fn` declarations continue to permit `SCREAMING_SNAKE_CASE` for FFI-style names. Function parameters, record fields, enum ADT variant fields, `for`-loop variables, and `@directive` parameters are validated with `isCamelCase` (extended to accept an optional leading `_` for package-private names). PascalCase enforcement on records, enums, enum variants, and type aliases is unchanged. The migration moved 292 user-defined `fn` declarations across `tests/spec/` and the embedded Ry source inside `tests/*.cpp` to camelCase; existing parser fixtures that locked the old `EXPECT_THROW` were flipped or paired with positive `EXPECT_EQ(prog.size(), 1u)` tests so the new spec is anchored under the same input. There is no alias layer — old snake_case identifiers in user code are now hard rejects. (#1443)
- **Breaking:** Renamed all 18 `expect()` test DSL matchers from `snake_case` to `camelCase`, completing the v0.0.16 naming convention migration for the testing layer (parent #1409, parser flip #1443): `to_eq` → `toEq`, `to_not_eq` → `toNotEq`, `to_contain` → `toContain`, `to_not_contain` → `toNotContain`, `to_be_greater_than` → `toBeGreaterThan`, `to_be_less_than` → `toBeLessThan`, `to_be_greater_than_or_eq` → `toBeGreaterThanOrEq`, `to_be_less_than_or_eq` → `toBeLessThanOrEq`, `to_have_length` → `toHaveLen` (also applies the `length` → `len` rule from #1411), `to_start_with` → `toStartWith`, `to_end_with` → `toEndWith`, `to_be_true` → `toBeTrue`, `to_be_false` → `toBeFalse`, `to_be_none` → `toBeNone`, `to_be_some` → `toBeSome`, `to_be_ok` → `toBeOk`, `to_be_err` → `toBeErr`, `to_be_empty` → `toBeEmpty`. The parser whitelists in `src/parser.cpp`, the dispatch chain and runtime error wording in `src/codegen_test.cpp`, the `ExpectStmt::matcher` field comment in `include/ry/ast.hpp`, every `*.test.ry` call site under `tests/spec/`, every embedded Ry literal inside `tests/*.cpp`, and the matcher table plus examples in `docs/reference/testing.md` (and adjacent reference pages) were all migrated together. Per the #1409 "no back-compat shim" stance there are no aliases — old `snake_case` matcher names are now hard rejects from the parser. (#1448)
- **Breaking:** Lambda parameters now also require `camelCase`, completing the v0.0.16 naming-convention sweep that #1443 began. `parseParenLambdaExpr` validates each parameter name with `isCamelCase` (the same rule used at five sites in `parser_decl.cpp` for `fn` parameters, record fields, etc., which permits a leading `_` for package-private names) and rejects snake_case with the existing wording `parameter name 'X' must be camelCase`. The validation is deferred until just past the closing `)` so that the speculative `parseParenLambdaExpr → tuple` fallback in `parsePrimary` does not swallow the diagnostic; a new `lambda_committed_` flag re-throws past the speculative `try { ... } catch (...)` once `'->' / '=>' / ':'` is observed. Tests `LambdaParamRejectsSnakeCase`, `LambdaParamAcceptsCamelCase`, and `LambdaParamAcceptsUnderscorePrefix` lock the rule. No source migration was required — the existing `tests/spec`, `share/std`, and `examples` lambdas were already camelCase or single-letter. (#1449)

### Fixed

- Parser now enforces camelCase on tuple-destructure LHS identifiers in both the parenthesized form (`(a, b) = expr`) and the bare form (`a, b = expr`), aligning with the v0.0.16 naming conventions enforced elsewhere by #1443. The `_` placeholder remains accepted at any position. (#1450)
- Parser now enforces camelCase on module-global typed declarations of the form `name: Type = value` (the keywordless implicit-binding form), closing the last gap left by #1443 / #1449 / #1450. SCREAMING_SNAKE_CASE remains accepted on `@native` and `@const` declarations. (#1470)
- Migration notes in `docs/reference/naming.md` now also document the camelCase enforcement extensions added by #1449 (lambda parameters), #1450 (tuple-destructure LHS identifiers in both `(a, b) = expr` and bare `a, b = expr` forms), and #1470 (module-global typed declarations of the form `name: Type = value`). Previously the section listed only #1443's seven categories, leaving readers to infer that the follow-up gaps were not enforced. (#1474)
- Migrated remaining snake_case identifiers in `docs/reference/*.md` example code to camelCase, completing the v0.0.16 documentation switch started in #1456. Touched 14 files: `directives.md` (`old_value` → `oldValue`, `test_handle`, `make_inputs`, `test_property`, `test_name`, `test_add`, `test_add_each`, `test_commutative`, `group_name`, `test_nested`, `arithmetic_tests`, `test_sub`, `test_mul`, `shared_setup_tests`, `test_base`, `test_combined`, `test_deep`, `hot_path`, `medium_path`, `cold_error_handler`, `old_api`, `target_fn`, `target_fn2`, `slow_fn`, `fast_fn`, `other_fn`, `directive_name`, plus the `__ry_<libname>_<fn_name>` placeholder reworded to `__ry_<libname>_<symbol>` with prose clarifying that most packages mirror the Ry name verbatim while legacy `base64`/`string` packages keep snake_case C symbols); `testing.md`, `operators.md`, `http.md` (`port_holder` → `portHolder`, `file_info` → `fileInfo`), `collections.md` (`has_value` — kept ARC `strong_count` prose), `control-flow.md`, `packages.md` (`helper_fn`, `public_api`; `_helper` package-private convention preserved), `contracts.md` (`new_balance`, `min_balance`), `errors.md` (`max_int`), `functions.md` (`add_base`; `return_type` and `param_type1/2` syntax placeholders), `json.md` (`name_val`), `net.md` (`echo_server`), `thread.md` (`my_fn`), `types.md` (`light_spd`, `fn_val`, `max_u64`, `dynamic_value`). External or non-identifier strings preserved: `before_each`/`after_each` (referenced as a feature name not implemented), `strong_count`/`weak_count`/`byte_len` (C `StringHeader` field names), `content_type`/`session_id`/`filename` (HTTP form / cookie keys), filesystem paths, C++ binary names, and the `naming.md` historical prose contrasting old snake_case with new camelCase. (#1444)
- Aligned snake_case parameter / record-field names in `docs/reference/{builtins,builtins-string,directives,http}.md` with the camelCase identifiers actually declared in `share/std/`, completing a follow-up to the #1443 user-identifier camelCase migration. `builtins.md`: `duration_ms` → `durationMs`. `builtins-string.md`: `ignore_case` → `ignoreCase` (across `contains` / `startsWith` / `endsWith`). `directives.md`: the `@deprecated` record-field example switched from `old_setting` / `new_setting` to `oldSetting` / `newSetting` (the parser's camelCase enforcement on record fields previously rejected the example). `http.md`: `max_requests` → `maxRequests` and `port_callback` → `portCallback` in both signature tables and prose. (#1454)
- Renamed snake_case fn names in `docs/reference/{directives,http}.md` to camelCase, matching the parser's camelCase enforcement on `fn` declarations (split out from #1454 so the two PRs map 1:1 to scope). `directives.md`: the `@deprecated` on functions example switched from `old_function` to `oldFunction` (the prose comment `# warning: 'old_function' is deprecated` was updated in step). `http.md`: `on_port` → `onPort`, and `start_server` → `startServer` across both the "Non-blocking Server with `async fn`" and "Server with Request Limit" example blocks. Variable names like `port_holder` are parser-allowed and intentionally left as-is. Other snake_case fn declarations that remain in `docs/reference/` are tracked separately. (#1456)
- Migrated residual snake_case Ry identifiers in `.claude/rules/` and `.claude/skills/` knowledge-base examples to camelCase, closing the gap left by #1444 (`docs/reference/`) and #1451 (`tests/spec/`). Touched 10 files: `tests-spec-conventions.md` (`got_none` → `gotNone`, `is_dir` → `isDir`, `remove_all` → `removeAll`), `stdlib-package-additions.md` (`ignore_case` → `ignoreCase` parameter on the `@native fn startsWith` example, plus `find_all` → `findAll` Ry-API references), `test-checklist/SKILL.md` (`byte_len` → `byteLen`, `str_index` → `strIndex`, `try_parse` → `tryParse`, `bad_str_op` → `badStrOp`, `to_match` → `toMatch`, plus the runtime error-message expectation rewritten to match the actual diagnostic emitted by `codegen_expr_literal.cpp:638` — `"str does not support index access; use charAt(s, i) instead"`), `codegen-arc-cow.md` (`weak_ref`/`closure_arg` → `weakRef`/`closureArg` in inline-prose Ry syntax examples; the NUL-safety operations list — `byte_len` → `byteLen`, `starts_with`/`ends_with` → `startsWith`/`endsWith`, `char_at` → `charAt`, `is_empty` → `isEmpty`, `to_upper`/`to_lower`/`trim_start`/`trim_end` → `toUpper`/`toLower`/`trimStart`/`trimEnd`, the `regex_*` family and UFCS variants → `regexMatch`/`regexSearch`/`regexReplace`/`regexSplit`/`regexFindAll` and `isMatch`/`findAll`, `json.to_str` → `json.toStr`, `io.write_text`/`io.append_text`/`io.write_bytes` → `io.writeText`/`io.appendText`/`io.writeBytes`; the str-temporary example `"foo".to_upper()` → `"foo".toUpper()`; the diagnostic text quoted at line 467 also updated to the actual `charAt` form), `codegen-type-and-metadata.md` (`fn first_of` → `fn firstOf` to match `tests/spec/generic_infer_container.test.ry`; `to_str([["a","b"],["c"]])` → `toStr([...])`; the `http_get`/`http_post`/`http_request` HTTP-stdlib references → `httpGet`/`httpPost`/`httpRequest`; `mk_thread()` → `mkThread()`; `make_result_fn()` → `makeResultFn()` placeholder), `docs-reference-conventions.md` (`remove_at` → `removeAt` in stdlib declaration / test-assertion examples; the C++ symbol `emitCollOp_remove_at` was preserved as it remains the actual function name in `src/codegen_call_collection.cpp`), `parser-conventions.md` (`to_str` → `toStr`, `to_float` → `toFloat` in prose referencing the canonical category name and the runtime converter), `regex-engine.md` (`find_all` → `findAll`, `is_match` → `isMatch`, `regex_match` → `regexMatch`, plus the prefixed-form placeholder `regex_*` → `regex*` to match the modern API shape), `codegen-fn-and-generic.md` (`takes_float(4)` → `takesFloat(4)` placeholder), and `codegen-stdlib-dispatcher.md` (tag list `bytes_to_str, write_bytes` → `bytesToStr, writeBytes`). Intentionally left in snake_case: the `runtime_internal` package directory (`share/std/runtime_internal/`), `__ry_*` C runtime symbols (clarified in #1463), C++ field/parameter names (`byte_len` in StringHeader layout, `list_elem_type_name`, `record_types_`, `thread_result`, etc.), the `is_absolute` "old name" example in `tests-cpp-conventions.md` that illustrates rename blind-spots, the `<fn_name>`/`fn_val`/`add_base`/etc. "before" placeholders in the `docs-reference-conventions.md` migration-pattern entry, the `my_x`/`my_a`/etc. snake_case examples in `parser-conventions.md` that demonstrate `LambdaParamRejectsSnakeCase` rejections, and the `byte_len` prose mirroring the `__ry_str_cmp` C++ comment in `codegen_expr.cpp:1017`. (#1464)
- Migrated 7 residual snake_case variable bindings in `tests/spec/` to camelCase, closing the gap left by #1450 / #1451. The bindings used the implicit `name: type = value` form inside `it(...)` blocks (no `let`/`var` keyword), which is why the previous sweep missed them. Touched 5 files: `tests/spec/nul_safety_http_client.test.ry` (`no_headers` → `noHeaders`, ×3 sites), `tests/spec/nul_safety_http.test.ry` (`no_headers` → `noHeaders`), `tests/spec/concurrency_stress.test.ry` (`outer_log` → `outerLog`), `tests/spec/option_branch_merge_none.test.ry` (`inner_cond` → `innerCond`), and `tests/spec/list_concat_arc.test.ry` (`empty_a` → `emptyA`). The `nul_safety_http*` `describe(...)` label strings were intentionally left in snake_case to match the corresponding test file names. Applied via `sed`-based bulk substitution rather than per-site edits to make the rewrite exhaustive. (#1466)
- Replaced residual `length()` / `substring()` Ry-API mentions in prose with `len()` / `substr()` across `docs/` and `.claude/rules/` + `.claude/skills/`, closing the gap left by #1444 / #1456 / #1459 / #1464 (which only covered code blocks and code examples, not surrounding prose that cited the old names as the "current API"). Touched 7 files: `docs/README.md` (builtins description column), `docs/reference/packages.md` (two `Built-in functions` enumerations), `.claude/rules/codegen-arc-cow.md` (NUL-safety operations list — `length` → `len` and `substring` → `substr`), `.claude/rules/codegen-stdlib-dispatcher.md` (bare-builtins enumeration), `.claude/rules/codegen-type-and-metadata.md` (two `result.length()` references in lambda-return rule entries), `.claude/rules/tests-spec-conventions.md` (NUL-truncation example using `substring`), and `.claude/skills/test-checklist/SKILL.md` (multibyte-divergence checklist row). Intentionally preserved: the rename-history table and prose in `docs/reference/naming.md` (#1411 historical contrast), `CHANGELOG.md` past-version entries, English-prose uses of "length" / "substring" as nouns (`byte length`, `fixed-length array`, `substring check`, function parameter names like `find(string, substring)`), C++/C symbol references (`emitStrOp_substring`, `__ry_utf8_substring`, `**Tags**: substring`), and the historical bug citation `length(xs)` in `codegen-fn-and-generic.md:440` describing pre-#1411 dispatch-misroute behavior. (#1469)

## [0.0.15] - 2026-04-28

### Added

- User-defined `@directive` declarations imported via `from <pkg> import <name>` now register in a per-program directive table and are accepted by directive validation, alongside the built-in registry. Defining a directive whose name collides with a built-in, or registering the same directive name twice in one program, is rejected. Unknown named arguments on a user directive are also rejected. (#710)
- Reference documentation for the user-defined `@directive(target=...)` declaration syntax in `docs/reference/directives.md`, including `target` parameter values, parameter mapping rules, and the bootstrap rule for `@directive` and `@native`. Each existing built-in directive section is also labeled with its definition origin. (#1392)
- `DirectiveDefStmt` (e.g. `@directive(target="function") fn name(params)`) is now exportable from packages. Both wildcard (`from pkg`) and named (`from pkg import name`) imports include directive definitions, with the same `_`-prefix privacy rules as functions and types. (#709)

### Changed

- `@it` and `@describe` are now stdlib-package directives provided by `share/std/testing/testing.ry`. Test files that use them must add an explicit `from testing import it, describe` (or `from testing`) at the top. The directives are no longer in the C++ built-in directive registry. (#710)
- Migrated 6 built-in directives from the C++ registry to stdlib `.ry` declarations. `@inline`, `@parallel`, `@const`, and `@deprecated` are now declared in `share/std/core/directive.ry` and remain implicitly available via the `share/std/builtins.ry` re-export. `@each` and `@property` are now declared in `share/std/testing/testing.ry` and require an explicit `from testing import each, property` (or the subset used) — consistent with `@it` / `@describe`. Only `@directive` and `@native` remain as compiler built-ins (the bootstrap pair). (#1390)
- Defaulted parameters of user-defined `@directive` declarations may now be passed positionally in declaration order, in addition to the existing named-argument and omitted (default-value) forms. For example, given `fn logged(label: str = "info")`, all of `@logged("warn")`, `@logged(label="warn")`, and `@logged()` are now accepted. Previously the positional form was rejected with "accepts at most 0 positional argument(s)". Built-in directives (`@native`, etc.) are unaffected. (#1402)
- User-defined directives applied to a target outside their declared `target=[...]` list now silently no-op instead of triggering undefined behavior. The compile succeeds, no diagnostic is emitted, and the directive's argument validation is also skipped. Built-in directives are unaffected. Note that for-loop and function-call use sites still reject all user-defined directives at the parser level (tracked separately in #1427). (#1425)
- The parser now accepts user-defined directives on `for` statements and function-call statements. Previously every user-defined directive at those two sites was rejected at parse time, masking the codegen-level silent-no-op behavior introduced in #1425. The compiler built-in directive `@native` is still rejected at both sites; applying `@parallel` more than once on the same `for` loop is also still rejected. (#1427)

### Removed

- Removed the `stage` parameter from user-defined `@directive(...)` declarations. `@directive(target=[...]) fn name(...)` is now the canonical form. `@directive(target=[...], stage="compile")` is rejected as `unknown argument 'stage'` (hard error, no deprecation window). The `stage` knob conveyed no useful information today (only `"compile"` was accepted) and was reserved for a Tier 2 design (#1400) that has been declined. (#1408)

### Fixed

- User-defined `@directive` declarations now accept required parameters in named-argument form. Previously `@mydir(description="hi")` for `fn mydir(description: str)` was rejected with "unknown named argument"; now both `@mydir("hi")` and `@mydir(description="hi")` are accepted. Mixed positional+named for the same parameter is rejected as a duplicate, and missing required parameters produce a clearer error. (#1397)
- Removed dead language-switcher lines from 25 docs pages (`docs/README.md` and 24 `docs/reference/*.md` pages). Both the three-language `[English] | [日本語] | [繁體中文]` pattern (21 files) and the residual English-only self-link `[English](self.md)` (4 files) pointed to non-existent `docs/ja/` and `docs/zh/` trees. (#1398)
- Aligned directive terminology in a parser code comment (`src/parser_decl.cpp` `parseDirectiveDefStatement`), in the `@directive` definition section of `.claude/rules/parser-conventions.md`, and in the `README.md` / `docs/README.md` overview lines — now all consistently use "directive(s)" / "compile-time instructions" rather than "annotation(s)" / "decorating" / "compile-time metadata", matching the canonical definition in `docs/reference/directives.md`. (#1422)

## [0.0.14] - 2026-04-26

### Changed

- Self-referential enum diagnostic now also suggests `Task<T>` and `Channel<T>` as valid indirection wrappers, aligning the recommendation with the existing checker's acceptance. The message previously only mentioned `List`/`Map`/`Set`, even though pointer-backed `Task<T>` and `Channel<T>` are equally valid indirections. (#1351)
- `release.yml` now deletes the matching `vX.Y.Z-nightly` prerelease (and its tag) after a stable `vX.Y.Z` release is published, preventing `ry self-update` from pinning users to a stale nightly that predates the stable release. (#1365)
- Heavy CI analysis (`clang-tidy`, `scan-build`, `asan`, `tsan`) now runs on every pull request instead of only on `v*.*.*` branch pushes. CodeQL also runs per PR plus on push to `main`, replacing the previous daily cron. The redundant `ci-scheduled.yml` workflow has been removed. (#1367)
- Release workflow now triggers on tag push (`v*.*.*`) instead of `workflow_dispatch` only. Pushing a semver tag from `main` builds, tests, and publishes a GitHub Release in one shot. (#1369)

### Removed

- `VERSION` file removed. CI derives the version from `${GITHUB_REF_NAME#v}`; local builds default to `0.0.0`. (#1369)
- `ry self-update --nightly` flag and the implicit nightly default (when the running version had a prerelease suffix, `self-update` with no arguments previously targeted the latest prerelease). `self-update` now always targets the latest stable release unless an explicit version tag is given. The nightly build workflow (`dev-release.yml`) has been retired as part of this change. (#1372)

### Fixed

- Lambda return-type inference now correctly narrows `@native` overloads that differ only in ptr-backed argument types (`str` vs `List` vs `Map` vs `Set`). Previously `f = () => length(xs)` failed with "ambiguous @native call in lambda return-type inference". Captured collection variables also retain their source-level element/key/value type metadata so the body dispatches to the correct runtime overload. (#1349)
- `for a, b in setOfTuples:` no longer fails with "for loop destructuring requires a list of tuples". The multi-variable for-loop binding path now handles `Set<(T, U)>` alongside maps and lists of tuples, and source-level element type names on `Set<T>` annotations are propagated for non-primitive inner types (collections, records, enums, tuples). (#1350)
- `List<str>` and `Set<str>` literals now correctly retain locally-constructed str elements, preventing dangling pointers when source variables go out of scope. Mirrors the `Map<str, str>` literal fix from #1353. (#1354)

## [0.0.13] - 2026-04-24

### Added

- Parenthesized tuple destructuring assignment `(a, b) = expr` and
  `@const (a, b) = expr` (#1189). Mirrors the existing bare form
  `a, b = expr` and matches what the formatter has been emitting.
- `input()` / `input(prompt)` builtin — reads one line from standard input as the stdin counterpart of `print()`. Returns `""` on EOF with the trailing newline stripped. Available without `import`, mirroring Python's `input()` (#1261)
- Introduced LLVM FileCheck-based golden IR tests for codegen regressions (`tests/filecheck/`) (#897)
- Added `ry --emit-llvm-ir` flag to emit unoptimized LLVM IR to stdout without running the program (#897)

### Changed

- `x: float = 10` (int → float widening) and `x: int = 3.14` (float → int truncation toward zero) are now accepted without an explicit `as` cast. The same coercion applies to record field compound assign (`r.n **= 2`) and collection-element compound assign (`xs[0] **= 2`, `m["k"] **= 2`). Low-level numeric types (`i64`, `f32`, etc.) still require exact type match, and narrowing is still rejected at function arg / return / if-expr branch sites (#1192).
- Function return values now support implicit `int` ↔ `float` coercion, matching
  the behavior at variable declaration and reassignment sites. `-> float`
  functions accept `int` return values (widening), and `-> int` functions
  accept `float` return values (truncation toward zero). Low-level numeric
  types (`i64`, `f32`, etc.) still require explicit `as` casts. (#1195)
- `is_match(text, /pattern/)` now performs **partial (unanchored) search** — it returns `true` if the pattern matches anywhere in the text, consistent with its name and with `search()` / `regex_search()`. Previously it performed a full-string match. To require a full-string match, anchor the pattern explicitly with `^` and `$` (e.g. `/^[a-z]+$/`). The legacy string-pattern `regex_match(text, pattern)` is unchanged and still requires a full-string match (#1197).
- Self-referential enum fields such as
  `enum Tree: Leaf(int), Node(int, Tree, Tree)` and their generic
  counterparts `enum LList<T>: Cons(T, LList<T>)` now emit a helpful
  diagnostic pointing to wrapper types (`List<...>`, `Map<K, ...>`,
  `Set<...>`) at declaration time instead of the cryptic
  `unknown type: Tree` / `unknown type: T`. Compiling a generic enum
  name without type arguments in a signature (e.g. `opt: MyOpt`)
  likewise produces a clear error asking for `MyOpt<T>` (#1203).
- `reduce(list, fn)` now returns `Option<T>` (previously `T`) and returns `None`
  for an empty list instead of raising a runtime error. Unwrap with `?? default`
  or pattern match, e.g. `(reduce(xs, fn)) ?? 0`. `fold(list, init, fn)` is
  unchanged and remains the preferred function when you have a seed value.
  (#1209)
- Function types are written `fn(T1, ...) -> R` only; `function(...)` is no longer accepted as a type or declaration keyword.
- `type_of` / `to_str` category for function-typed values is reported as `"fn"` (was `"function"`).
- Trace `symbol_define` entries use kind `"fn"` for user-defined functions (was `"function"`).

### Removed

- The `function` keyword is removed; use `fn` for all function definitions and `async fn` for async definitions (#1343).

### Fixed

- Restored `HeaderFilterRegex` in `.clang-tidy` to `^include/ry/.*\.hpp$`, removing the unintentional `src/` inclusion added defensively in #950 (#1150)
- `None()` and bare `none` in `if`/`case` branch-merge positions now correctly
  adopt the sibling arm's `Option<T>` inner type instead of defaulting to
  `Option<i8>` or `Option<i64>` (#1154)
- Generic type constraint checks (`<T: RecordName>`) no longer reject
  type aliases that resolve to a record type. Both the bound and the
  concrete type argument are now resolved through the alias table
  before the subtype check, while error messages continue to report
  the user-written names. (#1155)
- `case <subject>: (a, b)` where the subject is `Option<T>` or `Result<T, E>`
  no longer silently destructures the LLVM struct layout as a tuple.
  Previously the TuplePattern arm's source-name-based guard was skipped when
  the subject had no enum annotation, allowing `{i1, T}` to pass arity
  validation and producing wrong IR or an `ICmp` type-mismatch crash.
  The pattern test now rejects these subjects structurally via
  `isTupleStructType`, independent of any source-level type name. (#1156)
- `coerceResultType` no longer silently drops the active payload when a
  function-returned `Result` is bound to a variable with a different `Result`
  annotation. Such mismatches are now rejected at compile time with an explicit
  type-error message (#1157)
- Fix f-string interpolation of enums with explicit discriminant values (`enum E { A = 5 }`) no longer misreads `byte_len` via a non-StringHeader pointer, which could truncate output or trigger UB on the unreachable default branch (#1159)
- `None()` and bare `none` in lambda variable call arguments now adopt the callee parameter's `Option<T>` inner type, so `g(None())` compiles where `g: (o: Option<str>) -> Option<str>`. Previously required a typed-variable workaround. (#1179)
- `lst[a..b]` (list range-indexing) no longer crashes at codegen with `ICmp`
  type mismatch between `ptr` and `i64`. The indexing path now detects a
  `RangeExpr` as the first index, negative-wraps each bound against the list
  length, and routes to the shared slice helper. Semantics match
  `slice(lst, a, b + 1)` (inclusive, out-of-bounds clamped, negatives wrap).
  (#1184)
- `contains(map, key)` and `m.contains(key)` now correctly perform map key lookup instead of always returning `false` (#1185)
- `None()` / `none` passed as a positional field value in a record/struct
  constructor now correctly inherits the field's `Option<T>` inner type,
  matching the behavior already available in `let` annotations, if/case
  branches, and lambda call arguments (#1186).
- Eliminate intermittent SIGABRT/SIGBUS in `ry test -p` triggered by
  `tests/spec/combinatorial/collection_element.test.ry` during JIT
  teardown by cancelling the ResourceTracker scope_exit before leaking
  the LLJIT (#1187)
- Formatter no longer emits a stray colon and space (`": "`) between the
  pattern and `=` in `TupleDestructStmt` output, which previously broke
  formatter → parser round-tripping for `@const` variants (#1189).
- `x: int = 2 ** 3`, `x: int = 10 / 2`, and `x **= n` / `x /= n` (where `x: int`) now compile successfully. `**` and `/` still return `float`, but high-level `int` and `float` variables implicitly accept cross-type values at declaration, reassignment, and compound assignment (#1192).
- `@native` stdlib functions (`math.sqrt`, `math.sin`, `math.cos`, `math.tan`, `math.asin`, `math.acos`, `math.atan`, `math.atan2`, `math.hypot`, `math.exp`, `math.log2`, `math.log10`, and other table-driven natives) now accept `int` arguments with implicit `int → float` widening, matching user-defined function overload resolution. Exact-match precedence is preserved: `pow(2, 3)` still dispatches to the `(int, int) -> int` overload (#1193)
- `slice(lst, start, end)` now resolves negative `start` / `end` as offsets from the end of the list (`length + idx`), consistent with Python-style indexing, subscript access, and the `lst[a..b]` range-index operator (#1184). Over-negative inputs are silently clamped to `0`. (#1198)
- `substring(s, start, end)` now resolves negative `start` / `end` as offsets from the end of the string (`length + idx`), consistent with Python-style indexing and matching `char_at()`, `slice()`, and `lst[-1]` subscript access. Over-negative inputs are silently clamped to `0`. (#1199)
- Generic enums can now be used as function parameter types, return
  types, and let-binding type annotations. Both fully-qualified forms
  (`MyOpt<int>`) and type-parameter-referencing forms (`MyOpt<T>` inside
  a generic function `fn<T>`) resolve correctly (#1203).
- `slice(lst, a, b)` / `lst[a..b]` now correctly retains ARC-managed
  reference-typed elements (`List<str>`, `List<List<T>>`, `List<Map<K,V>>`,
  closures), preventing use-after-free when the source list is dropped (#1204)
- Fix `lst[a..b]` and `slice(lst, a, b)` losing nested element metadata
  (`list_elem_type_name`, `list_elem_fn_type_info`, `nested_list_elem`,
  `map_value_type_name`) when slicing collections such as
  `List<List<int>>`, `List<Map<str, int>>`, or `List<function>`.
  Second-level access on the resulting slice (e.g.
  `slice(xs, 0, 1)[0][0]`) now works correctly. (#1205)
- Calling `reduce(list, init, fn)` with 3 arguments (Python/JS style) now
  reports a targeted compile error suggesting `fold(list, init, fn)` instead of
  the generic "takes exactly 2 arguments" message. (#1209)
- Reject `function operator +(...)` with whitespace between `operator` and a symbolic operator. Only the canonical `function operator+(...)` (no space) is now accepted for symbolic operators. Keyword operators (`in`, `as`, `and`, `or`, `not`) and bracket/call operators (`[]`, `[]=`, `()`) are unaffected. (#1210)
- `!!` error-propagation operator now works in expression position immediately after an identifier (e.g. `Ok(r!!)`, `Some(v!! + 1)`), matching the documented equivalence with `?`. The lexer previously consumed the trailing `!` as part of the identifier (to support mutating method names like `sort!`), so `r!!` tokenized as `r!` + `!` and failed to parse (#1211)
- `math` custom emitters (`floor` / `ceil` / `round` / `log` / `pow` mixed-type) now accept `int` arguments via implicit `int → float` widening, completing the fix started in #1193 for table-driven `@native` dispatch. Exact-match precedence is preserved: `pow(2, 3)` still returns int `8`, while `pow(2.0, 3)` and `pow(2, 3.0)` now return float `8.0` instead of erroring (#1230)
- `as int` / `as i64` / `as i32` / `as i16` / `as i8` / `as u8` /
  `as u16` / `as u32` / `as u64` casts and the implicit `float → int`
  coercions (`x: int = 1.0 / 0.0`, compound assignments such as `x /= 0`
  where `x: int`) now raise a runtime error and exit with status 1 when
  the source value is `NaN`, `±inf`, or outside the target integer's
  representable range. Previously these silently produced LLVM poison
  (undefined behavior) via `fptosi` / `fptoui`. (#1232)
- `floor()`, `ceil()`, `round()`, and `trunc()` now correctly accept
  `-9.223372036854776e+18` (exactly `INT64_MIN`) as input. The previous
  `fabs(x) >= 2^63` overflow guard incorrectly rejected this value. (#1232)
- `take(lst, n)` now ARC-retains reference-typed elements, preventing
  use-after-free when the source list is released (same defect class
  as #1204 for `emitListSlice`). (#1235)
- `List + List` concatenation now ARC-retains reference-typed elements,
  preventing use-after-free when either source list is released (same
  defect class as #1204 for `emitListSlice` and #1235 for `take()`). (#1236)
- ADT enum variant payload fields with collection (`List`/`Map`/`Set`), nested enum, `Option`, or `Result` types now format correctly via `print` / `to_str` instead of rendering as an empty string, raw tag integer, or wrongly-nested value. Self-referential ADTs such as `enum Tree: Node(int, List<Tree>)` now print faithfully (#1238).
- Fix `appended(lst, elem)` losing nested element metadata
  (`list_elem_type_name`, `list_elem_fn_type_info`, `nested_list_elem`,
  `map_value_type_name`) when appending to collections such as
  `List<List<int>>`, `List<Map<str, int>>`, or `List<function>`.
  Second-level access on the resulting list (e.g.
  `appended(xs, [5, 6])[0][0]`) now works correctly. (#1239)
- Fix `take(lst, n)` losing nested element metadata
  (`list_elem_type_name`, `list_elem_fn_type_info`, `nested_list_elem`,
  `map_value_type_name`) when taking the prefix of collections such as
  `List<List<int>>`, `List<Map<str, int>>`, or `List<function>`.
  Second-level access on the resulting list (e.g.
  `take(xs, 2)[0][0]`) now works correctly. (#1240)
- Fix `distinct(lst)` losing nested element metadata
  (`list_elem_type_name`, `list_elem_fn_type_info`, `nested_list_elem`,
  `map_value_type_name`) when deduplicating collections such as
  `List<Map<str, int>>` or `List<function>`.
  Second-level access on the resulting list (e.g.
  `distinct(xs)[0]["a"]`) now works correctly. (#1241)
- Whole-list reassignment (`xs = [...]`) now releases ARC-managed inner elements, preventing the ~3 ARC headers per iteration leak observed when rebinding `List<List<T>>`, `List<Map<K,V>>`, `List<Set<T>>`, `Map<K, List<V>>`, etc. inside a loop. Applies to List/Map/Set element types; str elements remain on the existing path. (#1242)
- `appended(list, elem)`, `insert(list, i, elem)`, and `merge(map1, map2)` now retain ARC-managed collection elements they duplicate from source containers, matching the retain-on-store discipline already used by `slice` / `take`. Without these retains, the destructor fix above would have introduced UAFs when a source container was rebound or went out of scope. (#1242)
- Parser no longer aborts on overflow or non-decimal integers in array type
  `T[N]`. `parseTypeNameSingle` now uses `strtoull` + `errno` instead of
  `std::stoull`, so inputs such as `T[99999999999999999999...]`, `T[0xFF]`,
  or `T[1_000]` are rejected with a structured diagnostic instead of crashing
  via uncaught `std::out_of_range` / `std::invalid_argument`. Discovered by
  `fuzz_parser`. (#1259)
- `distinct()` now emits a compile error for lists of non-string pointer
  elements such as `List<Map<K, V>>`, `List<function(...) -> R>`, and
  `List<Set<T>>`. Previously the guard only rejected `List<List<T>>` and
  silently fell through to a `strcmp` on non-C-string pointers, which is
  undefined behaviour. (#1262)
- ARC retain now fires for container element loads (`xs = ys[i]`,
  `v = m["k"]`, function return, call-site argument passing) for nested
  ARC containers and `List<str>` / `Map<K,str>` borrows. Previously
  missed in `AssignStmt`, `return`, caller-side argument passing, match
  binding, type coercion, and lambda capture — every caller of
  `tryRetainArcSource`. Prerequisite for the `#1242` destructor fix that
  makes nested collection headers reclaimable. (#1266)
- `remove()` on a list now emits a compile error for lists of non-string
  pointer elements such as `List<List<T>>`, `List<Map<K, V>>`, `List<Set<T>>`,
  and `List<function(...) -> R>`. Previously the guard only rejected
  `List<List<T>>` and silently fell through to a `strcmp` on non-C-string
  pointers, which is undefined behaviour. (#1268)
- The `in` / `not in` operator on a list now emits a compile error for lists
  of non-string pointer elements such as `List<List<T>>`, `List<Map<K, V>>`,
  `List<Set<T>>`, and `List<function(...) -> R>`. Previously there was no
  guard at all and the linear-search loop fell through to `strcmp` on
  non-C-string pointers (Map/Set/closure/list headers), which is undefined
  behaviour. Mirrors the `distinct()` (#1262) and `remove()` (#1268) guards.
  (#1269)
- `floor(x)`, `ceil(x)`, and `round(x)` with a single `int` argument now
  short-circuit and return the input unchanged. Previously the value was
  widened to `f64` and passed through `floor`/`ceil`/`round`, losing
  precision for magnitudes above `2^53`. The 2-argument form and the
  widening precedence rules from #1193/#1230 are unaffected (#1346).
- `for x in s:` on a `Set<T>` now reads element-type metadata from
  `set_elem_type_name` instead of `list_elem_type_name`. Previously
  iterating a `Set<str>` silently fell through to the `list_elem` path and
  misread the loaded element, producing wrong values at the use site
  (#1346).
- `m[k] = v` on an empty-then-inserted `Map<str, str>` now retains the str
  key and value at SetItem time. Previously the retain was gated on
  `mapKeyArcKind != CollectionKind::Str` / `mapValArcKind != CollectionKind::Str`
  (a stale leftover from the #1266 destructor-only carve-out), leaving
  both slots as weak references. When the local source strings went out of
  scope, the map's slots became dangling pointers and subsequent lookups
  surfaced as "map key not found". The Map/List/Set literal-construction
  variants have a different root cause and are tracked separately in
  #1347 (#1346).
- `m: Map<str, str> = {k: v}` (non-empty Map literal with str keys/values)
  now retains each str handle at literal-construction time and stamps
  `map_key_type_name = "str"` / `map_value_type_name = "str"` on the
  returned header so the destructor dispatches to the str-releasing
  variant. Previously the retain gate relied on
  `inferCollectionTypeName(val)`, which returns `""` for plain str values
  and short-circuits at `Empty`, so the map held borrowed references to
  locally-constructed strings. When the source strings went out of scope
  the map's slots became dangling pointers, reproducing the #1346
  "map key not found" symptom through the literal path. `retainArcValue`
  routes through `tryRetainArcSource` Case 2b (no-op for fresh `+1`
  `makeString` values) and Case 1 (emits retain for `LoadInst` from a
  bound variable alloca), preserving `#1266` counter symmetry. List/Set
  literal variants are deferred to v0.0.14+ because the #1266
  destructor-only carve-out for them has a different resolution path
  (#1347).
- Inline if-expression (`if cond: then-expr else: else-expr`) now accepts a
  newline between the then-branch expression and `else:`. Previously the
  parser rejected `if x > 0: x\nelse: -x` because the trailing Newline
  after the inline then-branch was treated as a statement terminator,
  causing `parseIfExpression` to fail on the missing `else` at the current
  token (#1346).

## [0.0.12] - 2026-04-18

### Added

- `in` and `not in` operators now support substring check when the right operand is a `str`.
  `"world" in "hello world"` evaluates to `true`; empty-needle `"" in s` evaluates to `true`
  to match Python and the existing `contains` semantics. (#1032)
- `base64.encode_bytes(List<u8>) -> str` and `base64.encode_bytes_url_safe(List<u8>) -> str` for encoding raw binary byte lists to base64 without going through `str` (#1130)
- `base64.decode_bytes(str) -> Result<List<u8>, Error>` and `base64.decode_bytes_url_safe(str) -> Result<List<u8>, Error>` for decoding base64 directly to raw bytes, preserving embedded NUL bytes and non-UTF-8 sequences (#1130)
- `to_eq` and `to_not_eq` test matchers now support `List`, `Set`, `Map`, `Option`, `Result`, record, tuple, and union types in addition to the previously supported `int`, `float`, `bool`, and `str` (#737)
- Tuple destructuring patterns in `case` statements and expressions (#834). Supports binding patterns `(a, b)`, literal patterns `(1, 2)`, mixed `(1, n)`, wildcard `(_, n)`, 1-tuples `(v,)`, guard clauses `(a, b) if a > b`, and nested patterns such as `(Some(v), _)`. A fully irrefutable tuple pattern (all elements are variables or `_`) is treated as exhaustive.
- `runtime_internal.arc_live_count() -> int` — test-only introspection function that returns the running balance of ARC header allocations minus frees. Enables delta-based leak assertions in Ry spec tests without relying on LSan (#859)
- `Map + Map` (merge, rhs-wins on key collision) and `Set + Set` (union) are now supported via `+` and `+=` operators, parallel to existing `List + List` concatenation (#866)
- `tests/spec/concurrency_stress.test.ry`: stress tests for `@parallel for` with Map/Set captures (CoW semantics), GC collect() during parallel execution, nested `@parallel for`, many `thread_spawn` workers sharing a str capture, and Lock high-contention (4 threads × 2000 iterations) (#872)
- `tests/test_runtime_arc_contention_stress.cpp`: C++ GoogleTest suite exercising concurrent atomic `retain`/`release` on a single ARC header (16 threads × 10,000 iterations); part of the required `build-tsan/ry_tests` gate (#872)
- `tests/test_runtime_lock_stress.cpp`: C++ GoogleTest suite for `__ry_lock_acquire`/`release` under high contention (8 threads × 10,000 iterations, sequential reacquire, and independent-lock baselines) (#872)
- Integrated Clang Static Analyzer (`scan-build`) into CI `scan-build` job (#898)
- `Set<T>` `==` and `!=` now support complex element types: records, tuples, and nested collections (`Set<Point>`, `Set<List<int>>`, `Set<Map<str, int>>`, `Set<Set<int>>`) (#958)
- `Map<K, V>` `==` and `!=` now support complex key types: records, tuples, and nested collections (`Map<Point, int>`, `Map<(int, int), str>`, `Map<List<int>, str>`, etc.). Non-primitive keys use an O(n·m) structural linear-scan lookup; primitive keys continue using the existing hash-based path unchanged (#961)
- Positional record destructuring patterns in `case` arms: `case Point(a, b):` binds record fields by declaration order (#989)
- Nested patterns are now supported inside ADT enum constructor pattern arms (#990).
  Each binding position may be a variable, a literal, a wildcard, or a tuple pattern.
  A single tuple pattern whose arity matches the variant's field count is unwrapped
  and matched field-by-field, so `Event::Click((0, 0))`, `Event::Click((x, y))`,
  `Event::Click((_, y))`, and `Wrapper::Val(42)` all work as expected. Plain variable
  bindings (`Shape::Circle(r)`) continue to work unchanged.

### Changed

- `str` now stores an explicit byte length (`StringHeader` layout: `strong_count`, `weak_count`, `byte_len` prefix before the character data). The operations `byte_len`, `length`, `==`, `!=`, `<`, `>`, `+`, `*`, and Map/Set key lookup are fully NUL-safe; strings containing embedded NUL bytes (`\0`) are no longer silently truncated. (#1022)
- Indexing a `str` value with `[]` now emits a clear diagnostic pointing to `char_at(s, i)`, instead of the misleading "cannot determine list element type" message (#1026)
- Writing an octal literal (`0o...`) now produces a targeted compile error
  explaining that octal literals are not supported and suggesting `0x...`
  (hex) or `0b...` (binary) instead. Previously it produced the generic
  `invalid character after numeric literal` diagnostic. (#1027)
- `checked_add`, `checked_sub`, `checked_mul`, `saturating_add`, `saturating_sub`, `saturating_mul`, `wrapping_add`, `wrapping_sub`, `wrapping_mul` now accept the high-level `int` type in addition to low-level integer types (`i8`..`i64`, `u8`..`u64`) (#1028)
- `bool` operands are now rejected at compile time for arithmetic operators
  (`+`, `-`, `*`, `/`, `//`, `%`, `**`, unary `-`) and bitwise operators
  (`&`, `|`, `^`, `<<`, `>>`, unary `~`). Previously, `bool` was silently promoted
  to `int`. Use `bool as int` for explicit conversion. This also aligns the bitwise
  implementation with the documentation (#1030).
- `str` values are now fully ARC-managed (#1046). Dynamic strings created by `+` concatenation, `repeat`, f-string interpolation, and runtime functions are automatically freed when their last reference goes out of scope, eliminating string leaks. `List<str>`, `Map<K, str>`, and `Set<str>` also release string payloads when the collection is freed.
- `path.join`, `path.basename`, `path.dirname`, `path.extension` now return `Result<str, Error>` instead of `str`; callers receive a typed error if any argument contains an embedded NUL byte (#1054)
- `filesystem.is_file`, `filesystem.is_dir`, `filesystem.is_symlink` now return `Result<bool, Error>` instead of `bool`; callers receive a typed error if the path contains an embedded NUL byte (#1054)
- `http.listen` handler type is now `function(HttpRequest) -> Result<HttpResponse, Error>`; the listen loop synthesises a 500 response when the handler returns `Err` (#1054)
- `http.header(req, key)`, `http.query(req, key)`, `http.cookie(req, name)`, `http.form_field(req, name)`, `http.form_file(req, name)` now return `Result<Option<…>, Error>` instead of `Option<…>`; callers receive a typed error if the key/name contains an embedded NUL byte (#1054)
- `http.response(status, headers, body)` now returns `Result<HttpResponse, Error>` instead of `HttpResponse`; callers receive a typed error if any header key or value contains an embedded NUL byte (#1054)
- `http.header(resp, key)` (client response accessor) now returns `Result<Option<str>, Error>` instead of `Option<str>` (#1054)
- Unified ARC header offset dispatch for str: added `CapturedArcKind::Str` variant and `emitArcHeaderForAlloca` helper to prevent closure capture retain/release from using the wrong header offset (−16 instead of −24) for str values (#1105).
- `@it` and `@describe` functions with a return type annotation now produce a compile error instead of silently ignoring the annotation (#1122)
- `List<T>` and `Map<K, V>` `==` / `!=` now support complex element/value types: records, tuples, and nested collections (`List<List<T>>`, `List<Map<K,V>>`, `Map<str, List<T>>`, `Map<str, Map<K,V>>`, etc.) (#736).
- Internal codegen now uses `record` terminology throughout (`RecordInfo`, `record_types_`, `emitRecordConstructor`, `emitRecordComparison`, `findRecordTypeName`, `createRecordVisitFunction`, `recordToString`, `recordHasArcFields`, `arc_field_record_vars_`) to align with the `record` keyword used at the language surface (#816)
- User-visible error messages updated from "struct type" to "record type" (e.g., "unknown record type", "field access on non-record type") (#816)
- `ConcurrencySpecSuite` (in-process `@parallel for` / async spec suite) is now enabled under ASan builds; the `DISABLED_` guard added in commit `fb010ea` was removed after #630's atomic-ARC fix resolved the root cause (non-atomic ARC ops racing with ASan shadow-memory interceptors) (#872)
- Expanded clang-tidy `HeaderFilterRegex` to include `src/` implementation headers (#950)
- `union == / !=` now supports collection (`List`, `Map`, `Set`), record, ADT enum, and nested union variants in addition to primitives. Function-typed variants remain unsupported. (#960)

### Removed

- Removed `docs/tutorial/` directory and related references from `docs/README.md`, `AGENTS.md`, and top-level `README.md` (#968)

### Fixed

- `Err([...])` and similar Err-constructor expressions can now be coerced to a
  `Result<Ok, Collection>` type annotation at variable declaration and reassignment
  sites (e.g., `a: Result<int, List<int>> = Err([1, 2, 3])`).  Previously this
  emitted a type error because the inferred struct layout differed from the
  annotation layout (#1001).
- Pattern-matching an `Err(binding)` arm now correctly propagates collection
  element-type metadata to the bound variable, enabling index access and
  collection operations on the Err payload without a "cannot determine list
  element type" error.
- `T?` shorthand return type now propagates collection metadata identically to
  `Option<T>` — `xs.length()`, index access, and equality now work correctly for
  functions declared as `-> List<T>?` / `-> Map<K,V>?` / `-> Set<T>?` (#1003)
- `to_bytes`, `read_bytes`, `tcp_receive`, `tls_receive`, HTTP `body_bytes` が返す `List<u8>` を変数に代入すると macOS で `malloc: *** error for object ...: pointer being freed was not allocated` がクラッシュしていた問題を修正 (#1007)
- `__ry_split_chars` (used by `split(str, "")`) now allocates its returned `ListHeader`
  with `arc_alloc` so that ARC retain/release in `emitVarDecl` reads a valid counter
  prefix. Previously the `checked_malloc` allocation placed malloc metadata at
  `header_ptr - 16`, which could be corrupted by retain and crash on scope-exit
  release with `pointer being freed was not allocated` on non-ASan macOS builds.
  Same bug class as #1007. (#1010)
- HTTP リクエストの `query_all`, `cookies_all`, `form_fields`, `form_file` が返す `Map<str, str>` を変数に代入すると macOS で `malloc: *** error for object ...: pointer being freed was not allocated` がクラッシュしていた問題を修正 (#1011)
- Fix refcount imbalance when pattern-matching `Some(...)` on a value declared with the `T?` shorthand (e.g., `str?`, `List<int>?`). `extractGenericTypeArg` now recognises the `T?` suffix form as equivalent to `Option<T>`, ensuring the typed ARC retain path (Path 2a) is selected instead of the heuristic fallback (#1015).
- Fix use-after-free when pattern-binding `str` or bare function pointer
  fields of tuples / records / enum variants (#1016)
- `reduce` with a lambda that omits parameter type annotations now
  returns the correct result. Previously, on `List<int>` (and other
  primitive lists) the accumulator seed was stored as a narrow value
  into a 16-byte `any` slot, leaving the payload uninitialized and
  producing garbage values like `14.0` instead of `15` (#1020).
- Fixed use-after-free when mutating a list, set, or map during `for` iteration.
  The loop now snapshots the iterable at entry via an ARC retain; mutations through
  the source alias inside the loop body trigger copy-on-write and do not affect the
  iteration — appended elements are not visited, and removed elements are still
  visited (#1021).
- `bytes_to_str` now preserves embedded NUL bytes instead of rejecting them. (#1022)
- `weak str` upgrade no longer returns `None` instead of `Some` when the strong reference is alive; codegen now uses the correct `STRING_HEADER_SIZE` (24) offset to reach `strong_count` instead of the collection `ARC_HEADER_SIZE` (16). (#1022)
- `int / 0` now follows IEEE 754 and returns `inf` (or `-inf` for negative
  dividends; `nan` for `0 / 0`), consistent with `10.0 / 0` and `10 / 0.0`
  which already returned `inf`. The `/` operator is documented as always
  returning `float`, so integer operands are promoted before division and
  IEEE 754 semantics apply. This reverts the integer-specific runtime-error
  guard added in #754; `//` (floor division) and `%` (modulo) retain
  integer semantics and still raise a runtime error on a zero divisor for
  integer operands (#1023).
- Lambda return-type inference now correctly unifies `Ok(T)` and `Err(Error)` branches in an if-expression body, so unannotated lambdas like `(x: int) => if x > 10 => Ok(x * 2) else Err(Error("too small"))` compile without a spurious "all branches must have the same type" error (#1024)
- `-9223372036854775808` (INT64_MIN) is now accepted as a bare integer
  literal. Previously it required the `i64` suffix or a workaround
  such as `-9223372036854775807 - 1`. A standalone
  `9223372036854775808` (without the unary minus) remains rejected,
  and `-9223372036854775809` is rejected at compile time (#1025).
- `Map<K, any>`, `List<any>`, and `Set<any>` now accept direct assignment of concrete values (`str`, `int`, `float`, `bool`). Previously, assignments like `m["name"] = "Alice"` or `xs.append!(42)` would fail with a type mismatch error even though the `any` type is documented to support implicit conversion. The fix applies the canonical widening pattern to six collection element-write sites: `Map` index-assign, `List` index-assign, `List.append!`, `List.appended`, `List.insert`, and `Set.add`. The symmetric unwrap direction (`any` → concrete) is also supported at all six sites, and `Set<any>` element comparison uses the `__ry_any_eq` runtime function. (#1029)
- `print` and `to_str` on `float` now use the shortest round-trip decimal representation (minimum digits to reconstruct the exact `double` value), matching Python 3, Rust, Go, and JavaScript. Imprecise arithmetic like `0.1 + 0.2` now prints as `"0.30000000000000004"` instead of `"0.3"`, accurately reflecting the stored value. Exact literals such as `3.14`, `3.0`, and `2.5` are unchanged (#1031)
- For-loop UAF guard now fires for `FieldAccessExpr` iterables
  (e.g. `for x in obj.items: append!(obj.items, ...)`), not only bare
  variable references (#1041).
- Lambda return-type inference now unifies `Some(T)` and `None()` branches in
  if-expr, matching the `Ok`/`Err` behavior added in #1024. Previously
  `(x: int) => if cond => Some(x) else None()` failed with `undefined function: None`,
  and even `(x: int) => Some(x)` alone failed with a return-type mismatch (#1043)
- `contains`, `starts_with`, `ends_with`, and `find` now honour embedded NUL bytes instead of truncating at the first `\0` (#1047).
- `replace` now honours embedded NUL bytes in the haystack, needle, and replacement instead of truncating at the first `\0` (#1048).
- `substring`, `char_at`, `reverse`, `split("", "")`, `for c in str:`, and `enumerate(str)` now honour embedded NUL bytes instead of truncating at the first `\0` (#1049).
- String operations `to_upper`, `to_lower`, `trim`, `trim_start`, `trim_end` now formally preserve embedded NUL bytes (#1050)
- `split` with non-empty delimiter now preserves embedded NUL bytes in the subject and delimiter; the inline `strstr`/`strlen` codegen path was replaced by `__ry_str_split` in `runtime_string.cpp` using `memmem` (#1051)
- `join` and `repeat` / `*` string operations formally NUL-safe (#1051)
- Regex operations `regex_match`, `regex_search`, `regex_replace`, `regex_split`, `regex_find_all` (and UFCS variants `is_match`, `search`, `replace`, `split`, `find_all`) now preserve embedded NUL bytes in subject, pattern, and replacement; the public ABI was extended to carry explicit byte lengths for all string arguments (#1052)
- `json.parse` now accepts `\u0000` in string values and object keys (previously rejected with an error) (#1053)
- `json.stringify` now emits `\u0000` for embedded NUL bytes instead of truncating the string (#1053)
- `json.to_str`, `json.get`, and `json.keys` now correctly handle strings and keys containing embedded NUL bytes (#1053)
- HTTP client body truncated at first embedded NUL byte: `Content-Length` was computed with `strlen(body)`; now uses `stringByteLen(body)` for binary-safe payloads (#1054)
- HTTP request URL silently truncated at embedded NUL: `http_get`, `http_post`, and `http_request` now reject URLs containing embedded NUL bytes with a typed `Err` (#1054)
- HTTP `http_request` method silently truncated at embedded NUL: now rejected with a typed `Err` (#1054)
- HTTP header build used `std::string::operator+=` on Ry handles, truncating values at the first NUL; replaced with byte-length-correct `append(data, byte_len)` (#1054)
- DNS hostname lookup (`net.bind`, `net.connect`, `net.tls_connect`) silently truncated hosts containing embedded NUL bytes; now rejected with a typed `Err` (#1054)
- `path.join`, `path.basename`, `path.dirname`, `path.extension`, `path.resolve` silently truncated paths at embedded NUL bytes; now rejected with a typed `Err` (#1054)
- `filesystem` functions silently truncated paths at embedded NUL bytes; now rejected with a typed `Err` (#1054)
- `bytes_to_str()` and `write_bytes()` now reject non-`u8` list arguments at compile time instead of silently producing garbage output. Plain integer list literals like `[97, 0, 98]` use 64-bit element layout incompatible with the byte-list runtime; passing them previously caused corrupted output. Use `[97u8, 0u8, 98u8]` (explicit `u8` literals) or `to_bytes("...")` instead (#1055).
- `weak <alias>` where the alias resolves to `str` now uses the correct `StringHeader`
  offset instead of the `ArcHeader` offset. Without this fix, weak upgrade of a str-alias
  weak ref could load the wrong `strong_count` and crash or return wrong results (#1060)
- `fold()` now accepts untyped lambdas (e.g. `fold(xs, 0, (a, b) => a + b)`), matching the fix already applied to `reduce()` in #1038 (#1061)
- Lambda with explicit return type annotation (e.g. `(a, b) -> int => a + b`)
  now correctly coerces `any`-typed body expressions to the declared return
  type. Previously this failed at compile time when lambda parameters were
  untyped (which default to `any`), blocking the common
  `reduce(xs, (a, b) -> int => a + b)` pattern. Fix applies to both
  expression-body and block-body lambdas, and to `return` statements in
  regular functions. (#1062)
- The `in` and `not in` operators now accept concrete values on `Set<any>`, `List<any>`, and `Map<any, V>` containers, and support testing `any`-typed values against collections with concrete element types. Previously, expressions like `"x" in s` on a `Set<any>` failed with a compile-time type-mismatch error despite the write side (`add`, `append!`, index-assign) already accepting the same widening since #1029. The three check sites in `src/codegen_expr.cpp` (Set, Map, and List membership branches) now apply the canonical 3-branch any-widening pattern. The List branch additionally gained an `isAnyType` case in its inline comparison loop that invokes `__ry_any_eq` with scratch allocas hoisted outside the loop, mirroring `emitSetElementLookup` and `emitMapKeyLookup`. The symmetric unwrap direction (`any` value tested against a concrete container) is also supported. (#1065)
- `is_empty` on strings now honours embedded NUL bytes instead of returning `true` for strings that begin with `\0`. The check now reads `byte_len` from the StringHeader (via `emitStringByteLen`) instead of comparing only the first byte (#1069).
- Regex literal `\0` escape now produces a NUL byte in the pattern, matching string literal behavior (`/a\0b/` now correctly matches `"a\0b"`) (#1076)
- `bs: List<u8> = [97, 0, 98]` now compiles correctly; the `List<u8>` annotation propagates `u8` to each integer literal element so the list has 8-bit element stride and passes the `bytes_to_str` / `write_bytes` compile-time type gate (#1079)
- Reassignment to a `List<u8>` (or other `List<T>` with low-level integer element type) variable now propagates the element suffix so `bytes_to_str`, `write_bytes`, and TLS/TCP byte-list consumers accept the list, matching the declaration-time behavior from #1079 (#1085)
- Parallel test runner (`ry test -p`) now prints the failing file path and exit code for any non-zero worker, eliminating silent failure-count increments that were unattributable to a specific file. (#1088)
- Test runtime flushes stdout at every `it` boundary and after the summary, so output is preserved even when a worker exits abnormally. (#1088)
- Fixed an intermittent `~40%` failure rate in `ry test -p` on macOS caused by a crash in `~LLJIT()` during JIT teardown. Extended the existing Linux `(void)jit.release()` workaround to also apply on macOS. (#1088, #742)
- For-loops over captured collections (`VariableExpr` / `FieldAccessExpr` iterables) inside `thread_spawn` closures no longer crash the JIT optimizer (`LowerExpectIntrinsicPass`). The thread thunk now releases ARC-managed locals before its `ret void`, matching the parallel-for thunk pattern (#1090).
- `for x in xs[i]:` now snapshots the indexed collection via ARC retain, preventing
  use-after-free when the same slot is mutated (`append!`/`add`/`xs[i][k] = v`) inside
  the loop body. Extends the guard from #1021 (`VariableExpr`) and #1041 (`FieldAccessExpr`)
  to `IndexExpr` iterables. (#1091)
- `for a, b in xs[0]:` where `xs: List<List<(int, int)>>` now correctly types the second destructured variable `b` as `int` instead of reading raw bytes (#1094)
- `for x in outer[0][0]:` where `outer: List<List<List<int>>>` now correctly iterates all elements instead of running 0 times (#1095)
- `None()` call-form is now recognised as a None literal in let-decl, local
  variable reassignment, and module-global reassignment contexts, matching the
  behaviour of bareword `None` and `none`. Previously `x: Option<int> = None()`
  and `x = None()` (on an already-declared `Option<T>` variable) produced a
  type-mismatch compile error (#1099).
- `List<u8>` / `List<i8>` compound assignment (`bs += [99]`) no longer raises "list concatenation requires matching element types"; element suffix propagation now covers compound-op branches for both local variables and module-global write-through (#1102)
- Closure construction and destructor were corrupting `StringHeader.byte_len` when a str value was captured, by retaining/releasing at the wrong ARC header offset. Fixed by dispatching through `CapturedArcKind::Str` in `codegen_lambda.cpp` and `codegen_arc_cow.cpp` (#1105).
- Bare-expression str temporaries (e.g., `"foo".to_upper()` used as a statement) were leaked because `emitStmt(ExprStmt)` only checked `arc_owned_values_` and missed `arc_str_owned_values_` (#1105).
- Fixed memory leak when overwriting a slot in `List<List<str>>`, `Map<K, List<str>>`, or a record field of a nested collection type containing `str` elements. The overwritten inner collection's `str` handles are now released correctly (#1108).
- Result-returning lambda with unannotated parameter no longer loses its `Ok` payload when flowing into a typed `Result<T, E>` binding (#1111)
- Unannotated lambda body with 3+ branches constructing `Err(Error(...))` now compiles without "all branches must have the same type" error (#1111)
- Option branch-type merge in unannotated lambda if-expressions now prefers concrete types over `anyTy_` placeholders, matching the Result merge logic. Also propagates the `anyTy_` unwrap pattern from `Ok` to `Some` so concrete-vs-any branches produce matching `Option<T>` structs (#1115).
- `Err(x)` with an unannotated lambda parameter no longer causes a branch-type mismatch when the enclosing function's Result Err slot is a primitive type (`int`, `float`, `bool`, `str`) (#1116)
- `reverse!()` on a string now produces a clear diagnostic instead of a misleading
  "requires a list" internal error (#1124)
- Rejected embedded NUL bytes in path arguments of `io.read_text`, `io.write_text`,
  `io.append_text`, `io.delete_file`, `io.read_bytes`, and `io.write_bytes`; each
  now returns `Err(Error{ message: "<fn>: argument contains an embedded NUL byte" })`
  instead of silently truncating the C string and operating on an unintended file.
  `io.exists` returns `false` for such paths (no error channel available). Brings
  `io` to parity with the existing guards in `filesystem` and `path` (#1128).
- `base64.encode`, `base64.decode`, `base64.encode_url_safe`, `base64.decode_url_safe` no longer silently truncate input at embedded NUL bytes. `encode` / `encode_url_safe` now correctly process the full binary payload (binary-safe). `decode` / `decode_url_safe` now return `Err("invalid base64 character at position N")` for inputs containing NUL (since NUL is not a valid base64 character), instead of silently succeeding on the prefix before the NUL (#1129).
- `io.write_text` and `io.append_text` silently truncated content at the first
  embedded NUL byte because they used `fputs(content, f)`. They now use
  `fwrite(content, 1, stringByteLen(content), f)` for binary-transparent writes,
  matching the already-safe `io.write_bytes` path. `fclose` return code is still
  checked so buffered-write errors surface as `Err` (#1133).
- thread: align `thread_spawn` / `thread_join` `@native` declarations with their runtime behaviour (supports `int` / `float` / `bool` workers in addition to `Unit`) by using `any` as the declaration-level placeholder (#1135)
- `List<Set<T>>` and `List<Map<K,V>>` equality no longer silently falls back to pointer comparison, which produced incorrect results (#736).
- Clearer compile-time error for `Set<T>` equality with non-primitive element types, with reference to tracking issue (#736).
- ADT enum `==` / `!=` now compares the variant payload in addition to the tag.
  Previously two values with the same tag but different payload were incorrectly treated
  as equal (e.g. `Circle(1.0) == Circle(2.0)` returned `true`). (#959)
- Nested-collection equality (`Set<List<T>>`, `Set<Map<K,V>>`, `Set<Set<T>>`) now
  returns correct results regardless of insertion order (#963)
- `Set.contains(elem)`, `elem in set`, `set.add(elem)`, and `set.remove(elem)` now
  use structural equality when the element type is a nested collection, instead of
  incorrectly treating the element pointer as a C string (#963)
- `Option<List<T>>`, `Option<Map<K, V>>`, and `Option<Set<T>>` equality no
  longer returns a false-positive `true` when inner collections share a byte
  prefix; inner values are now compared element-wise. (#982)
- `Result<Collection, E>` and `Result<_, Collection>` equality now performs element-wise comparison of the inner collection instead of raw `strcmp` on collection header bytes (#985).
- ARC retain missing for fields extracted in pattern binding arms — `Some(xs)`, `Ok(xs)`, `Err(msg)`, record, enum-constructor, tuple, and variable patterns now correctly retain ARC-managed bindings, preventing use-after-free and refcount underflow under ASan (#997)
- `ListHeader` objects returned from runtime string-list builders (`makeStringList`, `makeMatchList`) are now allocated with `arc_alloc` so that Ry's ARC retain/release machinery can safely manage their lifetime (#997)
- `IOListHeader` objects returned from IO/network runtime functions (`receive`, `read_bytes`, `str_to_bytes`, TLS receive, HTTP body bytes) are now allocated with `arc_alloc`, fixing use-after-free when Ry's ARC retain/release accesses `header_ptr - 16` on pattern-bound byte-list values (#997)
- Fix use-after-free when a function returns `Result` or `Option` wrapping a collection (List, Map, Set) — covers direct parameters (`Ok(v)`) and record/tuple field access (`Ok(rec.field)`) — the inner value is now retained before scope cleanup releases local variables (#999)
- `emitStrGetDataPtr` now registers the recovered str handle in `arc_str_owned_values_` (STRING_HEADER_SIZE=24 offset) instead of `arc_owned_values_` (ARC_HEADER_SIZE=16 offset); using the wrong set caused incorrect header arithmetic on any subsequent retain/release of a str pointer recovered from a StringHeader (PR #1148 review)
- Map CoW clone now retains str keys independently of value retention; `elementTypeIsArcManaged` only checked `map_value_type_name`, so `Map<str, V>` CoW clones dropped key refcounts to zero after releasing the old header — use-after-free (PR #1148 review)
- `emitMapKeyLookup` now correctly routes StructType keys through the linear-scan path when `map_key_type_name` metadata is absent; previously an empty key name with an LLVM StructType fell through to `emitHashTableLookup` which has no hash function for structs (PR #1148 review)
- `Set<any>.remove(elem)` now applies the same 3-way any-widening (concrete → any wrap / any → concrete unwrap) as `Set<any>.add(elem)`, eliminating the compile-time type-mismatch error for concrete-typed arguments (PR #1148 review)
- `http.listen()` handler return-type validation now rejects pointer types whose type name is not `HttpResponse`; previously any opaque pointer type passed the check in the LLVM opaque-pointer model (PR #1148 review)
- `http.listen()` now registers `"net"` and `"http"` in `used_native_libraries_` so the JIT linker resolves `__ry_bind`, `__ry_listen`, and the HTTP runtime symbols; previously the function compiled but crashed at JIT link time (PR #1148 review)
- `http.listen()` now closes the TCP listener on `__ry_listen` failure before returning the error, preventing a file-descriptor leak (PR #1148 review)
- `str * n` with `n ≤ 0` now returns a heap-allocated empty StringHeader instead of a global constant; the PHI that merges the empty and repeat branches is registered in `arc_str_owned_values_`, so the global constant was previously released on scope exit — undefined behaviour (PR #1148 review)
- Record ARC reassignment now retains the incoming value for `InsertValueInst` chains (e.g. `r2 = { r.field, new_val }`), not only for `LoadInst` and `ExtractValueInst`; the missing retain caused use-after-free when an `InsertValueInst` aggregate was stored into an ARC-field record variable (PR #1148 review)
- `int` and `float` `to_str` / `value_to_string` no longer leak the allocated StringHeader; the buffer is now registered in `arc_str_owned_values_` so it is released on scope exit (PR #1148 review)
- `base64.decode_bytes` and `base64.decode_bytes_url_safe` now guard against a null `input` pointer before calling `stringByteLen`; a null input now returns an empty `List<u8>` instead of dereferencing at a negative offset (PR #1148 review)
- Fixed `is_empty([])` example in `docs/reference/collections.md` to use a type-annotated variable declaration (`empty: List<int> = []`); bare `[]` requires type inference context that is not always available (PR #1148 review)

## [0.0.11] - 2026-04-14

### Added

- `print()` now supports `end` and `sep` named parameters to control line ending and separator (#747)
- `Option.map()` combinator: transform the inner value of an `Option` with a function, returning `Some(f(x))` for `Some(x)` and `None` for `None` (#804)
- `regex.replace` and `regex_replace` now support capture group backreferences in the replacement string: `$1`–`$9` expand to the corresponding captured groups, `$0` expands to the entire match, `$$` produces a literal `$`, and `${N}` handles multi-digit group indices (#829)
- Trailing commas are now allowed in list, map, and set literals, function call arguments, function and lambda parameters, enum variant field lists, generic type parameters, generic type arguments, function type parameters, and enum constructor patterns (#832)
- Clang-Tidy static analysis with `bugprone-*`, `performance-*`, `cert-*` checks (#893)

### Changed

- `find_all` and `regex_find_all` now return `List<Match>` instead of `List<str>`. Each `Match` record has a `full: str` field (the matched text) and a `groups: List<str>` field (captured groups, in order). Patterns without capture groups return an empty `groups` list. (#830)
- CI now uses a mirrored LLVM 21.1.8 toolchain from GitHub Releases instead of fetching from apt.llvm.org on every run (#892)
- Integrated clang-tidy provisioning into `setup-llvm` action; CI no longer installs clang-tidy via a separate apt step (#934)
- Resolved all 85 existing clang-tidy warnings across `src/` and `include/ry/`; clang-tidy is now a hard CI gate with `WarningsAsErrors: '*'` (#935)

## [0.0.10] - 2026-04-12

### Changed

- CI: ccache now only saves on `main` and `v*` branch pushes, preventing redundant cache accumulation on PR runs (#926)

## [0.0.9] - 2026-04-12

### Changed

- Documented that `share/std/math/math.ry` intentionally uses bare `@native` (no argument) because math functions have no separate shared library (#907)
- `include/ry/codegen.hpp` uniform closure comment now documents all three struct fields: `{thunk_ptr, env_ptr, env_dtor_ptr}` (#907)
- `include/ry/ry_layout.hpp` `ARC_HEADER_SIZE` derived from `sizeof(int64_t) * 2` instead of hardcoded `16` (#907)
- `AGENTS.md` wip-label timing unified to post-merge rule; constant registration guidance now cites header path (#907)
- `.cursorrules` stdlib CMake step now distinguishes `ry_lib` (codegen) from `add_ry_native_lib` (runtime) targets (#907)
- `.claude/skills/git-commit/SKILL.md` `allowed-tools` now includes `git diff` and `git log` (#907)
- `.claude/skills/git-resolve-conflicts/SKILL.md` (+ `.codex` mirror) verifies `headRefName` before merging base branch (#907)
- `.claude/skills/git-branch-naming/SKILL.md` removes `style` branch type; branch creation is now conditional (#907)
- `.claude/skills/git-fix-pr-reviews/SKILL.md` generalizes nitpick marker detection and parameterizes reviewer handle (#907)
- `.codex/skills/git-search-issues/SKILL.md` adds deterministic precedence rule for ambiguous inputs (#907)
- `.codex/skills/git-triage-issue/SKILL.md` scopes autonomous action to Cases 1-2 only (#907)
- `.claude/skills/git-merge-pr/SKILL.md` (+ `.codex` mirror) uses dynamic default branch detection instead of hardcoded `main` (#907)
- `.codex/skills/git-switch-branch/SKILL.md` handles local branches without upstream (#907)
- `.github/workflows/release.yml` native-lib glob uses `nullglob` + array check for diagnosable errors (#907)

### Fixed

- `install.sh` now fails with a clear error when the release archive does not contain a standard library at `share/std` or `lib/std`, instead of silently installing a broken `ry` that crashes at runtime (PR #901 review)
- `arc_alloc` now guards `ARC_HEADER_SIZE + data_size` against integer overflow via `__builtin_add_overflow`, preventing an undersized heap allocation followed by out-of-bounds writes if `data_size` is near `SIZE_MAX` (PR #901 review)
- ADT example in control-flow reference used `Shape::Rect` instead of `Shape::Rectangle` (#907)
- Concurrency tutorial incorrectly implied `send`/`receive`/`close` are `net` module exports; clarified they are language builtins (#907)
- `to_str` signature in builtins-string reference now matches the supported-types table (`any` instead of a restricted union) (#907)
- `docs/zh/reference/operators.md` described `else =>` as required but examples used `_ =>`; unified to `_ =>` (#907)
- `docs/tutorial/11-testing.md` `test_should_handle_error` example had uninitialized `result` variable (#907)
- `docs/reference/functions.md` mutual recursion description now correctly states forward-declaration applies to nested functions too (#907)
- Typo "overloads case a call" corrected to "overloads match a call" in `docs/reference/functions.md` (#907)
- Traditional Chinese leftovers (`字串`/`巢状`/`缩排`) normalized to Simplified Chinese in `docs/zh/tutorial/11-testing.md` (#907)
- "match statements/expressions" terminology in `docs/zh/reference/control-flow.md` updated to `case` (#907)
- Code fences for `@each`/`@property` in testing reference now include `ry` language identifier (markdownlint MD040) (#907)
- Heading "Handling Results with match" corrected to "with case" in `docs/tutorial/08-error-handling.md` (#907)

## [0.0.8] - 2026-04-12

### Added

- Systematic combinatorial test coverage in `tests/spec/combinatorial/` (#628): 113 tests across 9 files covering type×operation matrix (equality, fn argument/return, collection element, match, nested types, syntax combinations, print/display, stdlib boundary inputs)
- `@it("description")` directive on named functions: test cases can now be defined as ordinary named functions with the `@it` directive (#634)
- `@describe("group")` directive on named functions: test groups can now be defined as ordinary named functions with the `@describe` directive (#635)
- `@each` and `@property` directives compose with `@it` on named functions for parameterized and property-based tests (#634)
- Shared setup in `@describe`: variables declared in a describe function body are automatically captured by inner `@it` functions (#635)
- Nested `@describe` output indentation: test output is now indented proportionally to nesting depth (#635)
- `NativeFnSignature` registry that captures full type information (parameter names/types, return type, package) from `@native` function declarations (#646)
- Documented the `__ry_<pkg>_<name>` native function naming convention (#646)
- `@native("libname")` directive syntax for specifying shared library module names (#647)
- Dynamic library loading for `@native("libname")` declarations — the JIT now loads shared libraries at startup (#649)
- Stdlib runtime packages are built as shared libraries (`.dylib`/`.so`) in addition to the existing static linking (#649)
- Nested named functions now obey lexical scoping: they are visible only within their enclosing function and do not collide with same-named functions in sibling scopes (#660)
- Nested named functions can now capture variables from enclosing scopes, behaving as closures just like lambdas (#661)
- `==` and `!=` operators now work for `List<T>`, `Set<T>`, `Map<K,V>`, `Result<T,E>`, and union types (#725)
  - List: element-wise comparison (supports `int`, `float`, `str`, `bool` elements)
  - Set: unordered equality — `{1,2,3} == {3,2,1}` is `true`
  - Map: key/value equality — maps with the same key-value pairs are equal regardless of insertion order
  - Result: compares `is_ok` flag and the inner `Ok` or `Err` value
  - Union (`A|B`): compares tag (variant kind) first, then the inner value for matching tags
- `ry` and `ry test` can resolve a bare `*.ry` filename (e.g. `ry main.ry`) when the file is not in the current directory: the project root is tried first, then each `[paths]` directory in key order; the first match wins (#741).
- `?` operator now accepts `Option<T>` operands in addition to `Result<T, E>`. When used on a `Some(v)` it evaluates to `v`; when used on a `None` the enclosing function returns `None` early. The enclosing function must declare an `Option` return type. `!!` is an alias with identical semantics. (#795)
- `??` operator now accepts `Result<T, E>` on the left-hand side in addition to `Option<T>`. For `Ok(v)` it evaluates to `v`; for `Err(_)` it evaluates to the right-hand default (the error value is discarded). (#796)
- `?` / `!!` can now be used directly at the top level of a script. When the operand is `Err(e)` or `None`, the error message is written to stderr and the process exits with status `1`. `__ry_main__`'s existing return-type contract is unchanged. (#745)
- `for c in s:` now iterates a string character by character, yielding each UTF-8 code point as a single-character `str`. `enumerate(s)` and `zip(s, t)` also accept `str` arguments with the same semantics. (#746, #827)
- `type_of(expr)` built-in function that returns a `Type` value representing the compile-time type identity of its argument. Supports `==` / `!=` for identity-based comparison and is printable via `print` / `to_str`. Covers primitives, low-level numeric types, collections (`List`, `Map`, `Set`), records, enums, `Option`, `Result`, functions/closures, `None`, and `Type` itself (reflective) (#793)
- `Type` primitive type representing the compile-time identity of a Ry type. Each distinct type definition receives a unique identity, so different records (or a record and an enum sharing a name) are always distinguishable by `==` (#793)
- `case` statement and expression unify `when` (conditional branching) and `match` (pattern matching) into a single construct (#799). Two forms are supported: `case:` for multi-branch conditionals without a subject (replaces `when:`) and `case <expr>:` for pattern matching with a subject (replaces `match`). Both forms support a block body (`:`) and a single-expression body (`=>`). Use `_` as the wildcard/default arm instead of `else`.
- `if` expression syntax for two-branch conditional values (#798). Supports both a single-expression form (`if cond => true_value else false_value`) and a block form (`if cond: body else: body`) with tail-expression semantics. For multi-branch expressions, use `case:` instead.
- Scientific notation float literals (`1e10`, `1.5e-3`, `2.5E+2`, `1_000e3`). Overflowing exponents (`1e400`) produce `+Inf` to match the runtime `to_float` converter (#819)
- `math.round(x, digits)`, `math.floor(x, digits)`, and `math.ceil(x, digits)`
  overloads for rounding a `float` to a given number of decimal places,
  returning a `float`. Negative `digits` rounds to powers of ten
  (`round(1234.5, -2) == 1200.0`). The two-argument forms reuse C99
  half-away-from-zero semantics so the result matches the one-argument
  `round()` applied to the scaled value — note this differs from Python's
  banker's rounding (`round(2.675, 2) == 2.68`, not `2.67`). `NaN` and `±Inf`
  pass through unchanged. (#842)
- `math.log(x, base)` overload for computing a logarithm with an arbitrary
  base, defined as `log(x) / log(base)`. Domain errors on either argument
  propagate as `NaN` or `-Inf`. (#842)
- `math.pow(x, y)` overload for `(int, int) -> int` using fast-exponentiation
  (O(log y)). A negative exponent raises a runtime error
  (`pow() integer exponent must be non-negative`). Overflow wraps silently,
  matching Ry's existing integer arithmetic model. (#842)

### Changed

- Captured variables in closures are now effectively final — reassignment inside the closure body produces a compile error (#213)
- `print()` now delegates to `to_str()` for all type formatting, ensuring consistent output between `print()`, `to_str()`, and f-string interpolation (#616)
- All C runtime memory allocations now use OOM-safe wrappers (`checked_malloc`, `checked_strdup`, etc.) that abort with a clear message instead of silently returning NULL (#631)
- Integer overflow checks added to array-size calculations in hash table rehash, UTF-8 reverse, and JSON parser (#631)
- CI now enforces a lint check that blocks raw `malloc`/`realloc`/`strdup` in new code (#631)
- `describe()` and `it()` lambda call syntax is deprecated; use `@describe("name")` and `@it("name")` directives on named functions instead (#635)
- Stdlib source files moved from `lib/std/` to `share/std/` following Unix FHS conventions (#645)
- Refactored math, io, json package dispatch to use table-driven native call dispatch (#650)
- Stdlib native dispatch migrated to table-driven architecture for net, http, and thread packages (#651)
- Stdlib `.ry` declarations updated from `@native` to `@native("libname")` for dynamic library resolution (#651)
- Stdlib runtime implementations separated from the static compiler library into shared libraries (#651)
- Directive invocation syntax is now generalized: all directives use a unified argument model supporting positional arguments, named arguments, and mixed forms (e.g. `@it("description")`, `@describe("group")`, `@property(count=100)`)
- Built-in directive signatures are now defined in a registry (`DirectiveSignature`) with allowed argument shapes and target kinds, enabling consistent validation and future user-defined directives (#663)
- Migrated all test descriptions (`it()` / `@it()`) to "should-style" wording for natural "it should ..." readability in test output and `--outline` mode (#664)
- Added test description style guideline to `docs/reference/testing.md` (#664)
- Stdlib package dispatch now uses self-registering pattern instead of X-macros; adding a new stdlib package with custom codegen no longer requires modifying core compiler headers (#674)
- Resource type tracking is now dynamic via `ResourceKindRegistry` instead of a hardcoded enum; new opaque resource types can be added without modifying `codegen.hpp` (#674)
- Error messages for `?` and `??` operator misuse now mention both `Option` and `Result` in the offending context.
- String elements inside collections (`List`, `Set`, `Map`, `Array`, `Tuple`, record) are now wrapped in double quotes when displayed via `print()` or `to_str()`, following Rust's debug display convention. Empty strings are now visible: `[""]` instead of `[]` (#756)
- `to_float(str)` now returns `Result<float, Error>` instead of `float`, matching the shape of `to_int(str)`. Invalid input previously returned `0.0` silently; it now returns `Err(Error(...))`. Empty strings, non-numeric content, and out-of-range values are reported as errors. **Breaking change**: existing code must unwrap the `Result` (e.g., via `case` or `?`). (#806)
- Assigning to a top-level mutable `let` from inside a function now writes through to the top-level binding instead of silently shadowing it with a new local. Code that relied on the old shadowing behavior must rename the inner variable explicitly (#817)
- `remove_at(values: List<int>, index: int)` in `share/std/list.ry` is now declared to return `int` instead of `Unit`, matching both the runtime implementation and the existing `collections.test.ry` expectations (#889)

### Removed

- Legacy `native_fn_arg_counts_` dispatch guard replaced by `native_fn_sigs_` (#651)
- Removed dedicated codegen dispatch files for base64, filesystem, and gc packages (now handled by generic native dispatch) (#651)
- **Breaking**: The `when` and `match` keywords have been removed (#800). Legacy code using these keywords must migrate to `case`. Migration table:
  | Before | After |
  |---|---|
  | `when:` | `case:` |
  | `match value:` with `case pattern:` arms | `case value:` with bare `pattern:` arms |
  | `else:` / `else =>` inside `when` arms | `_:` / `_ =>` |

### Fixed

- `to_str()` on ADT enums with associated data now correctly formats all field types (previously only supported int, float, str, bool) (#616)
- `@parallel for` no longer corrupts captured `List` / `Map` / `Set` / `str`
  values. Worker-local ARC retain/release on captured collections now uses
  atomic operations, captured allocas are re-marked as ARC-managed inside the
  thunk, and every ARC-managed capture is retained at worker entry so the
  copy-on-write `strong_count > 1` invariant holds — preventing workers from
  mutating the shared buffer in place (which previously caused heap corruption
  under contention). (#630)
- `emitCowCheck` now uses an Acquire atomic load for `strong_count` in an
  atomic context, pairing with the `atomicrmw` retain/release and closing a
  TOCTOU race window that TSan flagged when multiple workers CoW-copied the
  same captured collection. (#630)
- `runtime_gc.cpp::collect_locked()` now reads and writes `strong_count` via
  `__atomic_load_n(ACQUIRE)` / `__atomic_store_n(RELEASE)` so garbage
  collection no longer races with concurrent ARC retain/release performed by
  `@parallel for` workers. (#630)
- `ExpectStmt` was not scanned during free-variable analysis, preventing closure capture of variables referenced in `expect(x).to_eq(...)` assertions inside nested `@it` functions (#635)
- Installed `ry` binary no longer crashes with `dyld: Library not loaded` when using native packages (#659)
- Native shared libraries are now included in release and nightly distribution tarballs (#659)
- `self-update` now installs native shared libraries alongside the binary and stdlib (#659)
- Broadened SSRF private address filter to block carrier-grade NAT (`100.64.0.0/10`), benchmarking (`198.18.0.0/15`), multicast (`224.0.0.0/4`), reserved (`240.0.0.0/4`), IPv6 unspecified (`::`), and IPv6 multicast (`ff00::/8`) (#667)
- Added error handling for `fcntl` failure when restoring blocking mode after non-blocking connect (#667)
- Passing a capturing closure as a `function(...)` argument no longer crashes (#688)
- Directive arguments now support compound expressions such as function calls and binary operators (`@each(make_inputs())`, `@foo(x + 1)`) (#694)
- Unknown or invalid directive arguments on `record`, record fields, variable assignments, and `for` loops now produce a compile-time error, consistent with how function directives are validated (#696)
- Option equality (`==` / `!=`) now correctly compares inner values when both operands are `Some`, instead of comparing only the `has_value` flag (#726)
- Element type metadata is now preserved when accessing elements of `List<Map<K,V>>`, `List<Set<T>>`, and `List<closure>` by index or in a `for` loop (#727)
  - `xs[0]["key"]` on `List<Map<str, int>>` now works correctly
  - `for m in xs: m["key"]` on `List<Map<str, int>>` now works correctly
  - `xs[0]` on `List<Set<int>>` supports the `in` operator
  - Closures stored in a list (`fns[0](arg)`) are now callable after retrieval
- `print()`, `to_str()`, and f-string interpolation now work with closure values — they produce `"<closure>"` instead of a compile-time error (#728)
- Parser no longer crashes on out-of-range integer literals such as `9223372036854775808` (INT64_MAX + 1); a clear compile error is reported instead (#729)
- Missing explicit paths to `*.ry` files (e.g. `ry src/missing.ry`) now report **no such file** instead of unknown command (#741).
- `package.toml` `[paths]` entries (other than `src`) round-trip through `serialize`/`load` (#741).
- Fixed SEGFAULT when calling a two-level nested function return with type annotation (#752)
- f-string interpolation inside closures now correctly captures outer variables (#753)
- Integer division by zero (`1 / 0`) now raises a runtime error instead of returning `inf` (#754)
- Lambda expressions returning pointer types (f-string, record `str` field, string concatenation, cast to `float`) no longer cause IR verify errors (#755)
- Return type inference now correctly handles local variables instead of falling back to `int` (#770)
- "return type mismatch" errors now show expected and actual types
- `Any`-typed string values inside collections are now displayed with double quotes, consistent with statically-typed strings (#771)
- Double quotes and backslashes inside strings are now escaped when displayed in collections (#772)
- Sprint buffer depth overflow now aborts with a clear error message instead of silently corrupting output (#773)
- Closure capture analysis now handles `CastExpr`, `WhenCondExpr`, `MatchExpr`, `RangeExpr`, `ErrorPropagateExpr`, `AwaitExpr`, `WeakExpr`, and `SetExpr`, preventing "undefined variable" errors when these expression types reference captured variables (#776)
- Match/when pattern bindings are now correctly excluded from closure capture analysis, preventing incorrect capture of outer variables with the same name (#779)
- Low-level integer types (`i32`, `u8`, etc.) now raise a runtime error on division/modulo by zero instead of causing undefined behavior (#783)
- Expression-bodied lambdas returning collection literals (List, Map, Set) now produce correct values (#788)
- Expression-bodied lambdas now correctly retain ARC references and clean up scope before returning, preventing potential use-after-free when returning captured ARC-managed values (#789)
- Propagate collection return type metadata for block-bodied lambdas with inferred return types, so `result.length()` / indexing work on the value returned by `f = (x: int):\n  return [x, x * 2]` style lambdas (#790).
- `1num = 1` now correctly produces a syntax error instead of silently succeeding (#794)
- `replace(s, "", repl)` no longer hangs with an infinite loop; an empty pattern now returns a fresh copy of the input unchanged (#802)
- `NaN != NaN` now returns `true` as required by IEEE 754; float `!=` comparisons use `fcmp une` (unordered not-equal) instead of `fcmp one` (#803)
- `is_empty()` now accepts `str` arguments in addition to lists, maps, and sets (#831)
- `Result<JsonValue, Error>` returned by `json.get` / `json.at` no longer
  sneaks past JSON type checks via metadata alone. `isJsonValue()` now
  also requires the underlying LLVM value to be a pointer, so passing a
  `Result` to `kind` / `stringify` / `get` / `at` produces the existing
  "requires a JsonValue argument" diagnostic instead of an LLVM IR verify
  error. `to_str(result)` and `print(result)` still work and format as
  `Ok(...)` / `Err(...)` via the generic `valueToString` path (#805).
- Using `List` / `str` / `Map` / `Set` (or any other ptr-backed value) as
  a boolean condition in `if` / `while` / `case` or under the unary `not`
  operator now produces a clear compile-time error suggesting
  `length(x) > 0` or `not is_empty(x)`, replacing the previous
  `icmp ne ptr, i0 0` IR verify failure (#818).
- `exit(0)` followed by more statements no longer triggers
  `Terminator found in the middle of a basic block`. `emitExit()` now
  switches to a fresh dead basic block so trailing IR lands on a valid
  (unreachable) block and LLVM DCE removes it during optimization (#821).
- `u64` maximum value (`18446744073709551615`) now parses successfully when written with a `u64` suffix or under a `u64` / unsigned type annotation. Hex and binary forms (`0xFFFFFFFFFFFFFFFFu64`, `0b11...1u64`) are accepted too; range checking for `int` / `i64` / `u8`-`u32` happens in codegen against the target type (#807)
- `print()` / `to_str()` on `Map<K, List|Map|Set<...>>` now shows the actual nested container contents instead of empty strings for the values (#811)
- `print()` / `to_str()` on union types with `List`, `Map`, or `Set` variants now works instead of failing at compile time with "cannot convert ... variant of union to string" (#836)
- Whole-number `float` values now print with a trailing `.0` (e.g. `3.0`, `0.0`) instead of being indistinguishable from `int`, matching Python behavior (#808)
- `print()` / `to_str()` on a `Map` whose value type is a function now outputs `<closure>` instead of garbage bytes (#810)
- `wrapInUnion` now disambiguates same-LLVM-type variants (e.g. `List<int> | Map<str, int>`) by the value's collection metadata instead of always picking the first pointer-typed variant, fixing runtime miscategorization for collection/function unions
- Chained assignment targets are now accepted by the parser and codegen,
  including `list[i].field = v`, `record.a.b = v`, `list[i][j] = v`, and
  compound forms such as `list[i] += v` and `record.field[i] *= v` (#812).
  Previously these raised "expected '=' after index expression" or
  "expected '=' after field name". Compound assignment to a missing map key
  (`m["absent"] += 1`) now produces a clear runtime error instead of
  silently inserting a default value.
- Compiler now rejects defining a `record` and an `enum` with the same name in the same compilation unit. This also covers generic enum templates: `record Foo` and `enum Foo<T>` can no longer coexist, and duplicate generic enum declarations are rejected. Previously both declarations were accepted, leading to inconsistent type lookup. (#815)
- Top-level `let` bindings and `@const` declarations are now visible from any top-level function defined after them in the same source file. This includes reads and field access for all types — primitives, strings, lists, maps, sets, records, enums, and option/result values. Previously any such reference produced `undefined variable` at codegen (#817)
- Enum values returned from user functions now print as variant names
  (or `Variant(payload)` for ADT enums) instead of raw integers. Simple
  enums, ADT enums, and already-instantiated generic enums are all
  handled. Enum-typed elements stored in `List<Color>` literals also
  propagate correctly. (#820)
- `for i, x in enumerate(...)`, `for a, b in zip(...)`, and
  `for k, v in Map<K, V>` now preserve collection-element metadata on
  destructured variables, so `print` / `sum` / `length` work correctly
  when the elements are themselves `List` / `Map` / `Set` / enum. (#813)
- Generic function type inference now succeeds when the type parameter
  appears inside a container type in the declared parameter. `List<T>`,
  `Map<K, V>`, `Set<T>`, tuples `(T, T)`, and function types
  `function(T) -> T` now infer their type arguments from the call site,
  including nested combinations and cross-parameter unification. Previously
  calls such as `first_of([1, 2, 3])` for
  `function first_of<T>(xs: List<T>)` failed with
  "could not infer type parameter 'T'" even though the shape was
  unambiguous. The existing `name[T](args)` explicit syntax continues to
  work for cases where inference cannot determine the type (e.g., empty
  containers) (#823).
- `thread_join(t)` now returns the worker's value wrapped in `Ok(v)`
  instead of always `Ok(0)`. Workers using an expression-bodied lambda
  may return `int`, `float`, `bool`, or `Unit`. Joining an
  already-joined thread returns `Err("thread already joined")`. ARC
  types (`str`, `List`, `Map`, `Set`, records) and sum types
  (`Option`, `Result`, enums), block-bodied lambdas with a non-`Unit`
  return value, and panic-to-`Err` propagation remain unsupported and
  are tracked as follow-up issues. (#828)
- Type aliases targeting union types (e.g., `type Simple = int | str | bool`) now work correctly in variable annotations, function parameters, and function return types. Previously the compiler reported `annotation 'Simple' does not match expression type` because the union check examined the unresolved alias name instead of its target (#833)
- Nested type aliases over union types are now fully flattened. Previously, given `type A = int | str; type B = A | bool`, declaring `x: B = 42` failed with *"type is not in union"* because the alias `A` inside the union was not expanded. `B` is now equivalent to `int | str | bool`, and overlapping members are deduplicated — so `type C = A | int` collapses to `int | str`, and `type D = B | A` (where `B` already transitively includes `A`) flattens to `bool | int | str` (#835)
- Compiler now rejects a `type` alias whose name collides with an existing `record`, `enum`, generic `enum`, or previously-defined `type` alias, in either declaration order. This extends the cross-category duplicate check added in #815 to type aliases (including named unions such as `type Foo = int | str`). Duplicate error messages also now point at the offending declaration instead of a stale location. (#850)
- Chained writes through nested collections (`a[i][j] = v`, `r.items[i] = v`, `m[k1][k2] = v`) no longer leak through aliases. Path copy-on-write walks the LHS from root to leaf and clones every level whose reference count is greater than one before the mutation (#854)
- Record-to-record assignment (`r2 = r1`) now retains ARC-managed fields (`List<T>`, `Map<K, V>`, `Set<T>`) so both aliases share ownership of the inner containers. A subsequent mutation through one alias is isolated from the other by path copy-on-write (#854)
- `list[i] = v`, `m[k] = v`, and their compound forms now release the
  previously-held value before storing the new one when the element type
  is itself an ARC-managed collection (`List<List<T>>`, `List<Map<K,V>>`,
  `Map<K, List<V>>`, `List<Set<T>>`, and nested combinations). Previously
  every overwrite leaked the prior inner collection's heap allocation.
  The fix is safe under self-assignment (`xs[i] = xs[i]`) and cross-slot
  copy (`xs[i] = ys[j]`) by retaining the new value before releasing the
  old one. (#855)
- `rec.arcField = newList` now releases the previously-stored ARC-managed
  collection (`List`/`Map`/`Set`) before the overwrite, matching the
  element-slot fix from #855. Applies to plain and compound assignment on
  `VariableExpr`, `FieldAccessExpr` (chained `outer.inner.items = ...`),
  and `IndexExpr` (`list[i].arcField = ...`) left-hand sides. Sibling
  `fieldTypeIsArcManaged` predicate added so record field types are
  classified from their declared AST type rather than container metadata.
  (#857)
- `xs[i] += v` and `m[k] += v` now dispatch correctly when the element
  type is itself an ARC-managed collection (`List<List<T>>`,
  `Map<K, List<V>>`, and nested combinations reached via chained LHS such
  as `rec.items[i] += v`). Previously the loaded slot value lost its
  type metadata, so `emitArithmeticOp`'s list-concat dispatch fell
  through to the string path and produced a misleading
  `operator '+' not supported between str and non-str types` error.
  The fix propagates the container's element type name onto the loaded
  SSA value via `propagateTypeMeta` — the same pattern the formatter
  already uses for nested element loads. As a secondary fix, the
  empty-declaration path (`xs: List<List<int>> = []`) now records
  `list_elem_type_name` symmetric to the existing `List<Map>` /
  `List<Set>` branches so compound ops work on append-grown containers
  as well. (#858)
- `rec.arcField += v` now dispatches correctly when the field type is
  itself an ARC-managed collection (`List<T>`, `List<List<T>>`, etc.).
  This covers plain record field assignment (`b.items += [3]`), nested
  record field access (`outer.inner.items += [3]`), and chained LHS
  through a list of records (`lst[0].items += [3]`). Previously the
  field extracted from the struct lost its type metadata, so
  `emitArithmeticOp`'s list-concat dispatch fell through to the string
  path and produced a misleading `operator '+' not supported between
  str and non-str types` error. The fix propagates the field's declared
  type name onto the extracted SSA value via `propagateTypeMeta` at all
  three `FieldAssignStmt` compound branches — sibling fix to #858,
  which addressed the same class of metadata-loss bug on the
  `IndexAssignStmt` compound path. (#862)
- `+` applied to `Map` or `Set` operands now produces a clear error that names the actual collection type instead of the misleading `"operator '+' not supported between str and non-str types"` message. Mixed cases such as `List<int> + Map<str, int>` also name both operand types. (#863)
- `rwlock_unlock` now dispatches between shared and exclusive release via
  a `thread_local` counter per RWLock, eliminating the two-step window in
  `rwlock_read_lock` where `std::shared_mutex::lock_shared()` was held
  but the tracking map had not yet been updated. Under the previous
  implementation an unlock that observed the transient state would have
  fallen through to exclusive `unlock()`, corrupting `std::shared_mutex`
  state. (#871, follow-up to #630 P1)
- `ThreadHandle::has_error` is now a `std::atomic<bool>`; the worker
  thread's catch blocks store it with `memory_order_release` after
  writing `error_msg`, and `thread_join` loads it with
  `memory_order_acquire`. This makes the error-field publish/subscribe
  contract explicit, TSan-friendly, and robust for any future pre-join
  error polling path. (#871, follow-up to #630 P1)
- Lambdas (expression-body and block-body) that return one of their own
  collection-typed parameters now correctly propagate the parameter's
  declared shape so that `result.length()` and indexing work on the
  returned value (#886).
- Corrected `` `match value:` `` references to `` `case value:` `` in the pattern matching tutorial — the actual keyword is `case` (#889)
- Rewrote the networking example in the concurrency tutorial so the server/client snippets match runnable `net` test code (#889)
- Replaced outdated "struct" phrasing in `README.md` and `docs/README.md` with "record" to match the Ry keyword (#889)
- Updated the install one-liner in `README.md` to the current release version (#889)
- Added the `@describe` / `@it` directive-based test style to the testing tutorial and to the directives reference, so the new preferred syntax is actually documented (#889)
- Expanded the `README.md` feature list to mention pattern matching, the built-in testing framework, union types, GC (`std.gc`), and the `?` error propagation operator (#889)
- Expanded the `README.md` directives line beyond `@deprecated` to include the other common directives (#889)
- Added an explicit "In-Place Mutating Variants" section to the collections reference covering `append!`, `sort!`, `reverse!`, and the non-mutating `appended` counterpart (#889)
- Corrected stdlib `@native` declaration return types that had silently drifted from their codegen dispatcher implementations (#890):
  - `items(map: Map<str, int>)` now declared as `-> List<(str, int)>` (was `-> List<int>`)
  - `enumerate(values: List<int>)` now declared as `-> List<(int, int)>` (was `-> List<int>`)
  - `zip(values: List<int>, other_values: List<int>)` now declared as `-> List<(int, int)>` (was `-> List<int>`)
  The dispatchers (`emitCollOp_items`, `emitBuiltinQuery` for `enumerate`/`zip`) always returned lists of tuples; only the declarations were wrong. No behavior change — this corrects the stdlib documentation to match reality.

## [0.0.7] - 2026-04-03

### Fixed

- Fix Linux (GCC 11) build failure caused by incomplete type in self-referential `FnTypeInfo` struct (#623)

## [0.0.6] - 2026-04-03

### Added

- Empty list literal `[]` is now supported with type annotation (e.g., `xs: List<int> = []`) (#545)
- List concatenation with `+` and `+=` operators (e.g., `[1, 2] + [3, 4]` → `[1, 2, 3, 4]`) (#546)
- Tuple `==` and `!=` comparison now works via element-wise comparison (e.g., `(1, 2) == (1, 2)` → `true`) (#542)
- Single-element tuple type annotation `(int,)` is now supported in variable declarations and function signatures (#561)
- `split(s, "")` now splits a string into individual characters with full UTF-8 support (e.g., `split("hello", "")` → `["h", "e", "l", "l", "o"]`) (#549)
- Match expression syntax with `=>` for single-expression arms, enabling `res = match x: case Some(v) => v case None => 0` — all pattern types (literal, variable, enum, Option, Result, OR, guard) are supported (#499)
- The `as` cast operator now accepts the full type syntax including generic types (e.g., `x as Option<int>`, `x as Map<str, int>`) (#490)
- Regex literal syntax (`/pattern/`) that produces a `Regex` type, enabling type-based overload resolution and UFCS-compatible function calls (e.g., `"hello".is_match(/[a-z]+/)`, `"a1b2".split(/[0-9]/)`) (#458)
- New text-first regex functions: `is_match`, `search`, `replace`, `split`, `find_all` — overloaded to accept `Regex` type patterns alongside existing string functions
- `print()` now accepts multiple arguments with space-separated output (e.g., `print(1, "hello", true)` → `1 hello true`), and calling `print()` with no arguments now prints only a newline
- `body_bytes()` function for `HttpRequest` and `HttpClientResponse` that returns `List<u8>`, enabling binary-safe HTTP body access without NUL-byte truncation (#284)
- Structured `--trace` / `--trace-out=PATH` CLI mode for machine-readable internal execution tracing as JSON Lines, covering parse/import/codegen/jit/runtime milestones plus function and branch events
- Restructured tutorials from 11 to 12 files: dismantled overcrowded `08-advanced.md` (14 topics) into focused chapters, added new `08-error-handling`, `10-concurrency`, and `12-building-a-project` tutorials, expanded `05-functions` with closures/default args/UFCS, `06-records` with ADT/operator overloading, `07-collections` with lazy iterators, and `02-variables-and-types` with f-strings/type casting. Each tutorial now includes "Why" explanations, exercises, and common mistakes (#444)
- Bare `ry` command runs the entry point file specified in `package.toml`, with `ry -- arg1 arg2` to pass arguments (#443)
- `--outline` option for `ry test`: prints the `describe`/`it` structure of test files without executing test bodies, useful for reviewing test organization at a glance (#442)
- Cycle collector for ARC: CPython-style trial deletion algorithm detects and reclaims circular reference chains that ARC alone cannot free. Includes `gc` stdlib package with `collect()`, `enable()`, `disable()`, `set_threshold()` API. Static analysis identifies potentially cyclic types at compile time — non-cyclic types have zero GC overhead (#417)
- ARC for closures: closures with captured variables are now ARC-managed — automatically freed when no longer referenced, with proper retain/release of captured ARC-typed variables (collections, resources, other closures) (#415)
- Copy-on-Write (CoW) semantics for collection types (List, Map, Set): shared collections are automatically deep-copied before mutation, preserving value semantics while avoiding unnecessary copies when the collection has a single owner (#414)
- ARC integration with resource types: `TcpStream`, `TcpListener`, `TlsStream`, `Lock`, `RWLock`, `Semaphore`, `Barrier`, `Thread`, `AtomicInt`, `AtomicBool`, `HttpRequest`, `HttpResponse`, `HttpClientResponse`, `JsonValue` are now automatically cleaned up when no longer referenced — deterministic RAII-style resource management via ARC destructors (#418)
- `weak` reference type for ARC: non-owning references that do not prevent deallocation, with atomic CAS-based upgrade to `Option<T>`, automatic scope cleanup, and pattern matching support (#416)
- `ignore_case` parameter for `contains()`, `starts_with()`, `ends_with()` — optional boolean (default `false`) enables ASCII case-insensitive matching
- ARC (Automatic Reference Counting) for collection types (List, Map, Set) and strings: automatic memory management via retain/release with scope-based cleanup, destructor generation for internal buffers, and immortal sentinel for global string constants (#413)
- ARC infrastructure: header layout (`{ strong_count, weak_count }`), `arc_alloc`/`arc_retain`/`arc_release` codegen primitives with Swift-style atomic switching support (#412)
- Relative imports: `from .helper import greet`, `from .utils import add`, `from . import add, sub` for importing relative to the current file's directory
- Auto-convert non-str operands to str in `+` concatenation: `"abc" + 2` produces `"abc2"`, `1 + "abc"` produces `"1abc"` (#393)
- Leading-dot float literals (e.g. `.5`, `.01`, `.5f64`) are now supported as shorthand for `0.5`, `0.01`, etc.
- Numeric underscore separators for improved readability: `100_000`, `0xFF_FF`, `0b1010_0101`, `3.14_159`
- Ed25519 signature verification for self-update artifacts to prevent supply-chain attacks (#124)
- `thread` package: native OS thread API with Thread, Lock, RWLock, Semaphore, Barrier, AtomicInt, AtomicBool (#363)
- `ry run` command to execute scripts defined in `package.toml` `[scripts]` section (#384)
- `path` standard library package with file path operations: `join`, `basename`, `dirname`, `extension`, `resolve`, `is_absolute` (#185)
- `filesystem` standard library package with file/directory manipulation: `list_dir`, `walk`, `glob_files`, `copy`, `move`, `remove`, `remove_all`, `make_dir`, `make_dir_all`, `file_size`, `is_file`, `is_dir`, `is_symlink`, `chmod`, `symlink`, `read_link` (#184)
- Runtime bounds checking for `char_at()` on strings — out-of-bounds access now raises a descriptive runtime error instead of silently returning an empty string (#395)
- Python-style negative index wrap-around for lists, arrays, and `char_at()` — e.g. `xs[-1]` accesses the last element (#395)
- Boundary clamping for `substring()` — out-of-range indices are clamped to `[0, length]` (#395)
- Descriptive runtime error messages for out-of-bounds access, including the actual index and collection length (#395)
- Mutual recursion and forward function references: functions can now call each other regardless of definition order, as long as they have explicit return type annotations (#550)
- `and_then` and `map` method chaining for `Result` type, enabling flat error handling without nested `match` (#597)
- Parser now accepts keyword tokens (e.g., `and`, `or`, `not`) as method names after `.` for UFCS calls

### Changed

- **Breaking**: Pattern matching syntax renamed from `when value:` to `match value:`; conditional `when:` (without subject) is unchanged (#482)
- **Breaking**: Anonymous function lambda form `function(...) => ...` is no longer supported; use parenthesized lambda syntax `(x: int) => x + 1` instead (#483)
- **Breaking**: Single-expression lambda syntax changed from `(params): expr` to `(params) => expr`; block lambdas `(params):\n  body` are unchanged (#498)
- **Breaking**: Self-update now requires Ed25519 signature verification by default; set `RY_SKIP_SIGNATURE=1` to opt out (#469)
- HTTP and JSON parsing hot paths now use pointer-based parsing to avoid unnecessary `substr` copies and temporary string allocations (#467)
- **Breaking:** Renamed the function declaration keyword from `fn` to `function`; legacy `fn` / `async fn` now produce migration errors with guidance
- Added concise Option A lambda syntax: `(x: int) -> int => x + 1` and `(x: int) => x + 1`
- **Breaking:** `args()` renamed to `arguments()` for command-line argument access (#111)
- **Breaking:** `recv()` renamed to `receive()`, `set_recv_timeout()` renamed to `set_receive_timeout()` for network operations (#111)
- **Breaking:** HTTP server functions simplified for UFCS: `http_method` → `method`, `http_path` → `path`, `http_header` → `header`, `http_body` → `body`, `http_query` → `query`, `http_query_all` → `query_all`, `http_cookie` → `cookie`, `http_cookies` → `cookies`, `http_form_field` → `form_field`, `http_form_file` → `form_file`, `http_form_fields` → `form_fields`, `http_response` → `response`, `http_listen` → `listen` (#208)
- **Breaking:** HTTP client accessor functions simplified: `http_client_status` → `status`, `http_client_body` → `body`, `http_client_header` → `header` (#208)
- **Breaking:** JSON functions simplified for UFCS: `json_type` → `kind`, `json_get` → `get`, `json_at` → `at`, `json_str` → `to_str`, `json_int` → `to_int`, `json_float` → `to_float`, `json_bool` → `to_bool`, `json_len` → `length`, `json_keys` → `keys` (#208)
- **Breaking:** IO functions simplified: `file_exists` → `exists`, `str_to_bytes` → `to_bytes` (#208)
- Expanded abbreviated parameter names in stdlib declarations: path (`a,b,c,d,p`), list (`n,f`), thread (`a`) (#111)
- Synced stdlib declaration files (`.ry`) with implementations: added missing `remove`, `take`, `tap` to `list.ry`, `remove` to `map.ry`, and corrected IO function return types to `Result` in `io.ry` (#454)
- **Breaking:** Stdin execution now requires explicit `-c` flag (`echo 'code' | ry -c`). Bare `ry` without arguments runs the `entry` file from `package.toml` instead of reading stdin (#443)
- Control-flow syntax now keeps `if`/`else`, removes `elif`, replaces `match` with `when value:`, and replaces ternary `?:` with `when:` expressions
- `char_at()` now uses a single-pass UTF-8 traversal for bounds checking and character extraction, eliminating a redundant full-string scan (#407)
- `ry new` / `ry init` now normalize hyphens to underscores in package names (e.g. `ry new my-app` creates `name = "my_app"` in package.toml)
- `.test.ry` files are excluded from directory package loading
- Fixed-length array type syntax changed from `[T; N]` to `T[N]` (e.g. `buf: i32[4] = [1, 2, 3, 4]`)
- `to_int(str)` now returns `Result<int, Error>` instead of bare `int`, properly detecting invalid input (#543)
- `int` arithmetic (`+`, `-`, `*`, unary `-`) now raises a runtime error on overflow instead of silently wrapping (#544)
- Constant expressions that overflow are caught at compile time (#544)
- Clarified in documentation that closures capture by value in both directions: outer variable changes do not affect the closure, and mutations inside the closure do not affect the outer scope (#552)
- All collection headers (List, Set, Map) now use ARC allocation uniformly, ensuring correct reference counting and CoW behavior (#572)
- `json.keys()` now returns `Result<List<str>, Error>` instead of `List<str>`, with proper null-pointer handling for OOM and non-object inputs (#599)

### Fixed

- Single-element tuple literal `(42,)` and trailing commas in tuple literals now parse correctly (#556)
- `print()` and `to_str()` now support tuples including nested tuples, displaying them as `(elem1, elem2)` (#541)
- `print()` on lists of tuples (e.g., `zip()` result) now correctly displays tuple elements instead of empty entries (#540)
- f-string interpolation now supports collection types (List, Map, Set) and tuples (e.g., `f"items: {xs}"`) (#547)
- Operator overloads (`operator[]`, `operator+`, etc.) now correctly propagate return type metadata for collection types, fixing "cannot determine list element type for index access" when `operator[]` returns `List<T>` (#537)
- `operator as` overload resolution now uses semantic type names instead of LLVM types, preventing false matches between pointer-backed types (`str`, `List<T>`, `Map<K,V>`, etc.) (#537)
- `to_str()` on union-typed values now returns the string representation of the actual value instead of the discriminant index (#536)
- Option/Result type-meta guard now checks all collection metadata keys (`TM_ListElem`, `TM_SetElem`, `TM_TaskResult`), not just `TM_MapKey`, preventing potential metadata overwrites when wrapping collection types (#525)
- Generic function parameters with collection types (`List<T>`, `Map<K, V>`, `Set<T>`) are now properly marked as ARC-managed during instantiation, preventing potential memory leaks (#524)
- Return type inference now correctly resolves user-defined struct types in functions and lambdas without explicit type annotations (#515)
- Return-path analysis now recognizes exhaustive `match` statements on custom enums and `bool`, removing false "does not return a value on all code paths" errors when all variants are covered (#513)
- Indexing into `List`, `Map`, or `Set` fields of a record (e.g., `record.field[idx]`) no longer fails with "cannot determine list element type" (#511)
- `join()` now works correctly with UFCS string receiver (e.g., `",".join(parts)`) (#508)
- Closures returned from functions can now be called, and function-type parameters can be captured in closures — enabling higher-order patterns like `make_adder`, `compose`, and currying (#510)
- **Breaking**: Legacy regex functions (`regex_match`, `regex_search`, `regex_replace`, `regex_split`, `regex_find_all`) now use text-first argument order `(text, pattern)` consistent with the regex literal API; previously `(pattern, text)` which caused silent incorrect results (#512)
- SEGFAULT when multiple functions use `match` on ADT enum parameters — `resolveType()` now correctly returns the ADT struct type instead of `i64` for enums with variant data (#507)
- `append!` / `appended` and other collection operations now work correctly on `List`, `Map`, and `Set` values returned from user-defined functions (#509)
- `operator as` codegen now supports generic target types (e.g., `int?`, `Result<int, Error>`), not just struct types (#501)
- Self-update tar validation now uses a whitelist approach, rejecting all archive entries that are not regular files or directories (device nodes, FIFOs, sockets, etc.) (#471)
- `print()` output inside `@parallel for` loops is no longer interleaved across threads — each `print()` call now produces atomic output via thread-local buffering (#473)
- Mocked functions now still enforce the original function's `require` and `ensure` contracts, preventing tests from bypassing contract checks (#441)
- Hardened codegen type promotion (`promoteToInt`/`promoteToFloat`) to reject struct and pointer types, preventing invalid LLVM IR from arithmetic, comparison, bitwise, and unary operators on non-numeric types (#394)
- Added null/allocation-failure guards in runtime functions (`runtime_io`, `runtime_net`, `runtime_path`, `runtime_regex`, `runtime_sort`) to prevent undefined behavior from null pointer dereference and integer overflow (#394)
- Cycle collector now generates visit functions for record (struct) types, enabling GC traversal of ARC pointer fields embedded in record types within ADT enum payloads (#432)
- Explicit resource free/close functions (`lock_free`, `close`, `json_free`, etc.) now decrement the ARC reference count instead of immediately freeing — aliased resources no longer cause use-after-free (#427)
- Closure destructors now recursively release captured resources and nested closures, preventing memory/resource leaks when closures are freed (#429)
- Variable reassignment now uses the full destructor resolver (covering resources and closures) instead of only resolving collection destructors
- Parser error message for unexpected tokens in statement position now says `unexpected token 'X'` instead of listing all valid keywords — also removes `expect` from keyword listing since it is a function, not a keyword (#404)
- Eliminated DNS rebinding TOCTOU gap in HTTP client SSRF protection — DNS is now resolved once and the same result is used for both the private-host check and the connection, preventing attackers from bypassing SSRF guards via DNS rebinding; also added IPv4-mapped IPv6 address detection (#470)
- HTTP response headers containing CR or LF characters are now silently skipped to prevent response splitting attacks (#472)
- Fixed float output examples in operator tutorial to match actual `%g` formatting (e.g. `1024` not `1024.0`)
- Added curl one-liner installer to Getting Started tutorial
- Fixed "struct" terminology to "record" across tutorial and reference docs
- Improved error message when hyphens are used in import paths (e.g. `from my-pkg import foo` now suggests using underscores)
- Binary operations between `str` and non-`str` types (e.g. `"abc" - 2`, `"abc" / 2`) now raise compile-time type errors instead of producing garbage output or LLVM IR verification errors (#396)
- `ry version` now works as an alias for `ry --version` instead of trying to execute the VERSION file on case-insensitive filesystems (#381)
- Dev Release nightly build fails due to missing dependencies (`openssl@3`, `ninja`, `googletest`) and removed schedule trigger (#380)
- Chained Map index access now works correctly for nested Maps (#538)
- `return none` now works correctly in generic functions with `T?` return type (#539)
- UFCS call on list literal (e.g., `[1, 2, 3].map(...)`) no longer fails to parse at statement level (#551)
- Operator overloads now correctly propagate function-type return metadata (#554)
- Fix undefined behavior in collection header deallocation where scope cleanup read invalid memory before plain-malloc headers (#572)
- Fix memory leak when collection operation results (appended, slice, etc.) are discarded as expression statements (#572)
- Heap corruption after Iterator tests caused by leaked iterator headers and states; iterator memory is now freed at scope exit (#577)
- Inline `case` body in `match`/`when` statements now parses correctly (e.g., `case Ok(v): expr`) (#587)
- Fixed double-free heap corruption in JSON `get()`/`at()` child values (#594)
- Arithmetic, comparison, and bitwise operations between high-level `int` and low-level `i64`/`u64` now correctly produce a compile error instead of silently succeeding (#595)
- `print()` now supports `Result` types directly, displaying `Ok(value)` or `Err(error)` (#612)
- `to_str()` now correctly converts `Result` and `Option` types to their string representation instead of returning the internal tag value (#611)

## [0.0.5] - 2026-03-28

### Added

- Low-level numeric types: i8, i16, i32, i64, u8, u16, u32, u64, f32 (#288)
- Numeric literal suffixes e.g. `42i32`, `3.14f32` (#289)
- Unsigned negation check — reject unary `-` on unsigned types (#312)
- `any` type with runtime dispatch, implicit conversion, and wrap/unwrap (#216, #219, #220, #221, #222, #223, #224, #225, #226, #227, #228)
- Return type inference for named functions when annotation is omitted
- `Result<V, E>` type for null-safe error handling (#104)
- `?` operator for Result error propagation (#176)
- `ensure` variable binding and remove `result`/`old` keywords (#105)
- Generic functions with type parameters (#210)
- Nested type parameter parsing (`>>`) (#263)
- Record auto-generated `operator==` and `operator!=` (#305)
- Record auto-generated `to_str` (#306)
- Record subtyping with `<` syntax for field inheritance and subtype coercion (#307)
- Record invariant inheritance: parent `invariant:` clauses are checked on child construction (#355)
- Auto-slice Error subtypes in `Err()` for Result return type coercion (#354)
- Subtype coercion for field assignment (#359)
- Subtype coercion for `?` error propagation operator (#360)
- Generic type constraints with record bounds (`<T: RecordName>`) (#297)
- `@inline` directive for function inlining hints (#299)
- Explicit value assignment for simple enum variants (#309)
- Named fields in ADT enum variants (#308)
- Subscript operator overloading `operator[]` / `operator[]=` with multi-index support (#202)
- Membership operator overloading `operator in` for user-defined types (#202)
- Call operator overloading `operator()` for callable records (#202)
- Cast operator overloading `operator as` for user-defined type conversions (#202)
- Tail call optimization (TCO) for self-recursive functions via LLVM `musttail` (#214)
- Compound assignment operator overloading with in-place optimization (#204)
- Enforce bool return type for comparison and logical operator overloads (#203)
- N-element tuple destructuring in for loops (#302)
- Implicit widening conversion in overload resolution (#212)
- `json` standard library package — parse/stringify with opaque JsonValue type (#179)
- `base64` standard library package (#183)
- TCP socket timeouts and TLS/SSL support (#76, #77)
- HTTP client functionality: `http_get`, `http_post`, `http_request` (#129)
- HTTP cookie parsing: `http_cookie`, `http_cookies` (#128)
- HTTP query parameter parsing: `http_query`, `http_query_all` (#127)
- HTTP chunked transfer encoding (#164)
- HTTP multipart/form-data parsing for server (#82)
- HTTP `max_requests` parameter for `http_listen` shutdown control (#165)
- Comprehensive HTTP status code reason phrases per RFC 9110 (#119, #125)
- `.env` file auto-loading and `env()` built-in function (#158)
- `RY_ENV` environment variable and `--env` CLI flag (#159)
- `.env` / `RY_ENV` integration with short aliases and environment-specific files (#171)
- `sleep(duration_ms: int)` built-in function (#146)
- `ry fmt` command for code formatting (#151)
- `ry new <project-name>` command (#149)
- `ry test --coverage` for line coverage measurement (#166)
- `ry test --watch` for auto-rerunning tests on file change (#163)
- Parallel test execution with `-p` / `--parallel` flag (#147)
- `--help` / `-h` option support for commands and subcommands (#337)
- HTTP keep-alive support for `http_listen` server (#79)
- Stdin execution via pipe and here-document (#250)
- `fail()` helper in test framework (#177)
- HTTP automatic redirect following for client requests (#148)
- Self-update artifact checksum verification (#116)
- Linux x86_64 (amd64) release build in CI (#154)
- Linux ARM64 (aarch64) release build in CI (#155)
- `block_on(task)` built-in function for synchronous Task waiting (#206)

### Changed

- Default return type changed from `Unit` to `any` when omitted (#218)
- Allow omitting parameter type annotations (defaults to `any`) (#217)
- Lambda expression syntax changed from `:` to `=>` (#301)
- Flatten stdlib imports — `from std.x` to `from x` (#178)
- Rename `ry.toml` to `package.toml` (#335)
- Restrict `await` to `async fn` context only — use `block_on()` in synchronous code (#206)

### Fixed

- Set literal now deduplicates elements at construction time — `{1, 2, 3, 2, 1}` correctly has length 3 (#376)
- Repo-built `ry` now prefers the checked-out stdlib over stale `~/.ry/lib/std`, restoring `base64`, `json`, and `net` timeout imports during language development (#367, #370)
- Floor division (`//`) now uses correct floor semantics instead of truncation (#239)
- Zero-division guards for integer `//` and `%` operators (#242)
- NaN comparison aligned with compiler's ordered semantics (#240)
- Require return on all code paths for non-Unit/any functions (#209)
- HTTP body NUL byte truncation (#281)
- Filter hop-by-hop headers in HTTP client requests (#280)
- `repeat()` type check and n<=0 guard (#272)
- ConstantInt metadata corruption from LLVM sharing (#311)
- Wrap value in `any` on reassignment to any-typed variable (#232)
- Reject non-str pointer types in `any` to prevent mistagging (#233)
- Overload ranking prefers concrete types over `any` (#252)
- OR pattern binding check — reject bindings but allow wildcards (#139)
- HTTP client response resource type tracking (#140)
- Directive move-only semantics to prevent silent expr loss (#102)
- Memory leak in `@property` test random strings (#100)
- UTF-8 `utf8_char_len_safe()` buffer overread (#99)
- TCP partial write handling (#114)
- TCP `recv` buffer freed on error (#115)
- TCP error handling unified to return Result instead of `exit(1)` (#120, #123)
- Truncated HTTP request body rejection (#117)
- `ry fmt` crash, `join()` arg mismatch, and multiple formatter bugs (#162)
- `ry fmt` duplicate blank line before section comments (#167)
- `ry fmt` round-trip verification to prevent code destruction (#168)
- `!` suffix restricted to function names only (#156)
- Nested stdlib modules copied recursively during self-update (#112)
- Self-update mandatory checksum verification and hardlink rejection (#126)
- Test timeout applied per `it`-block instead of per file (#333)

### Removed

- Concurrency primitives: channels, spawn, select, task_group, cancel (#304)
- `byte` type in favor of `u8` (#294)
- `join(task)` built-in — replaced by `block_on(task)` (#206)

## [0.0.4] - 2026-03-22

### Added

- Improved builtins — UTF-8, Option returns, mutating variants (#44)
- 9 new test matchers and extended existing ones (#46)
- `take` and `tap` list builtins (#47)
- Increment/decrement operators (`x++`, `x--`) (#48)
- Regex phase 2 — range quantifiers and non-greedy matching (#49)
- Lazy iterator abstraction (#50)
- Word boundary `\b`/`\B` and case-insensitive `(?i)` flag (#51)
- Concurrency primitives: spawn/await, channels, select, `@parallel for` (#54)
- `@each` / `@property` test directives (#57)
- `std.math` package (#58)
- `@native let` constants and `_`-prefix private symbols (#59)
- `std.io` module with file I/O, stdin, and byte operations (#60)
- TCP socket API for HTTP server foundation (#61)
- HTTP server API (#62)
- Directory path argument support in `ry test` (#64)
- Stable TimSort via C++ runtime replacing QuickSort (#52)

### Changed

- Replace `let`/`var` with Python-style assignment and `@const` directive (#75)

### Fixed

- Socket timeouts to prevent test hangs (#95)

## [0.0.3] - 2026-03-20

### Added

- `>>>`, string `*`, and `not in` operators (#10)
- `filter`, `map`, `sort` stream-like operations for lists (#11)
- Design by Contract support (#15)
- Directive support with `@deprecated` (#16)
- f-string, `as` cast, and `Result<T, E>` (#18)
- Compound assignment operators, `in`/`not in` for list/map, and `range()` step (#19)
- r-string (raw string) support (#20)
- Ternary operator, match OR pattern, list operations (#21)
- Lambda (`fn`), tuple destructuring, enum ADT, generic enum, collection ops (#22)
- `record` keyword, type alias, operators, naming enforcement, and collection ops (#23)
- `args()` and `exit(code)` built-in functions (#24)
- `@native` directive for built-in function declarations (#25)
- Collection functions: `remove`, `distinct`, `flatten`, `merge` (#26)
- Literal types and range types (#28)
- Function type aliases (#29)
- Generalized trailing block syntax, demoted `describe`/`it` to functions (#30)
- For-loop tuple destructuring and `@native` stdlib prelude (#31)
- `ry test` auto-discovery and removed `test_dir` (#32)
- Built-in Error type and `!!` operator replacing `Result<T, E>` (#33)
- Rust-style rich error messages (#35)
- Directory-based package system with std library (#36)
- NFA-based regex engine (Phase 1) (#37)
- `...` (ellipsis) no-op statement (#38)
- Mock/verify support in test framework (#39)
- Ry self-tests (#41)

### Changed

- Require type annotation for `none` and remove `unwrap()` (#34)

### Fixed

- Short-circuit eval, FnScope contract protection, lexer safety (#40)
- Three compiler bugs found during self-test development (#41)
- Self-update repo name and missing releases handling (#3)

## [0.0.2] - 2026-03-14

### Added

- `ry self-update` command (#1)

## [0.0.1] - 2026-03-14

Initial release.

[Unreleased]: https://github.com/t0k0sh1/ry/compare/v0.0.20...HEAD
[0.0.20]: https://github.com/t0k0sh1/ry/compare/v0.0.19...v0.0.20
[0.0.19]: https://github.com/t0k0sh1/ry/compare/v0.0.18...v0.0.19
[0.0.18]: https://github.com/t0k0sh1/ry/compare/v0.0.17...v0.0.18
[0.0.17]: https://github.com/t0k0sh1/ry/compare/v0.0.16...v0.0.17
[0.0.16]: https://github.com/t0k0sh1/ry/compare/v0.0.15...v0.0.16
[0.0.15]: https://github.com/t0k0sh1/ry/compare/v0.0.14...v0.0.15
[0.0.14]: https://github.com/t0k0sh1/ry/compare/v0.0.13...v0.0.14
[0.0.13]: https://github.com/t0k0sh1/ry/compare/v0.0.12...v0.0.13
[0.0.12]: https://github.com/t0k0sh1/ry/compare/v0.0.11...v0.0.12
[0.0.11]: https://github.com/t0k0sh1/ry/compare/v0.0.10...v0.0.11
[0.0.10]: https://github.com/t0k0sh1/ry/compare/v0.0.9...v0.0.10
[0.0.9]: https://github.com/t0k0sh1/ry/compare/v0.0.8...v0.0.9
[0.0.8]: https://github.com/t0k0sh1/ry/compare/v0.0.7...v0.0.8
[0.0.7]: https://github.com/t0k0sh1/ry/compare/v0.0.6...v0.0.7
[0.0.6]: https://github.com/t0k0sh1/ry/compare/v0.0.5...v0.0.6
[0.0.5]: https://github.com/t0k0sh1/ry/compare/v0.0.4...v0.0.5
[0.0.4]: https://github.com/t0k0sh1/ry/compare/v0.0.3...v0.0.4
[0.0.3]: https://github.com/t0k0sh1/ry/compare/v0.0.2...v0.0.3
[0.0.2]: https://github.com/t0k0sh1/ry/compare/v0.0.1...v0.0.2
[0.0.1]: https://github.com/t0k0sh1/ry/releases/tag/v0.0.1
