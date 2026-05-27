---
name: pre-commit-checklist
description: Pre-commit checklist — docs / CHANGELOG / rules+skills / tests / ASan+UBSan / TSan / libFuzzer / background hygiene / labels. Use before declaring complete, before PR, self-verify, running sanitizers/tests; also on Japanese triggers 作業完了前, 完了前, 実装完了, 修正完了, マージ前, PR を出す前, セルフ検証, 動作確認, サニタイザー実行, テスト実行, チェックリスト. Always fires near the end of feature work.
allowed-tools: Bash(./.claude/skills/pre-commit-checklist/*.sh:*), Bash(git diff:*), Bash(git fetch:*)
---

# Pre-commit Checklist

Mandatory checklist before declaring a task complete: documentation, CHANGELOG, knowledge-base updates, full tests, sanitizers, fuzzing, background-task hygiene, and label policy.

> **Source-of-truth note**: previously in `AGENTS.md` §"作業完了前チェックリスト"; relocated by #1384. Section §3〜§3.7 execution steps were extracted into `./*.sh` scripts by #1942 (rationale: reduce SKILL.md context size and consolidate permission prompts into a single approval per script).

## 0. Change-type × Skip Matrix

List the changed paths and consult the matrix to identify skippable sections.

```bash
git diff --name-only origin/main
```

> Includes uncommitted changes (working tree vs `origin/main`). Run `git fetch origin main` first if not fetched. On detached HEAD or a fresh clone without `origin/main`, fall back to `git diff --name-only HEAD`.

| Change type | §1 Doc | §2 CHANGELOG | §3 Tests | §3.5 Sanitizer | §3.6 libFuzzer |
|---|---|---|---|---|---|
| `.md` / `docs/` only | ✓ | skip | skip | skip | skip |
| `changelog.d/` only | skip | ✓ | skip | skip | skip |
| `.claude/` only | skip | skip | skip | skip | skip |
| `tests/` only | review | review | ✓ | ✓ | parser-family only |
| includes parser/lexer/json/utf8/string※ | ✓ | ✓ | ✓ | ✓ | ✓ |
| other code changes | ✓ | ✓ | ✓ | ✓ | skip |

**Legend**: `✓` = required / `skip` = may omit (record in PR description) / `review` = judgment call.

※ "parser/lexer/json/utf8/string family" = changes to `src/(parser|lexer|runtime_json|runtime_utf8|runtime_string)*` or `include/ry/(parser|lexer|runtime_json|runtime_string).hpp`. **`runtime_string*` is a dependency of both `fuzz_json` and `fuzz_utf8`**, so it always falls in this row.

**Notes**: multiple matching rows ⇒ take the strictest per column (`✓` > `review` > `skip`). Always required regardless of matrix: §2.5 (rules/skills, when applicable), §3.5.5 (Static Analysis), §3.6.5 (tree-sitter Grammar Regression Check, when applicable), §3.7 (background hygiene), §4 (Label Cleanup, no-op). For `.md` / `docs/`-only and `changelog.d/`-only PRs, §4 is effectively the only required action: the edited area satisfies its own `✓` (self-edit = done), and the Skip-if bash returns `skip` for everything else.

> **Logging duty**: whenever a section is skipped, record `Skipped §X — <reason>` in the PR description (or in the CHANGELOG fragment if one exists). Skip logs are required for future audit.

## 1. Documentation Update Check (English only)

> **Skip if** — changed files are only `.claude/` or `changelog.d/` (no user-visible change):
>
> ```bash
> git diff --name-only origin/main | grep -vE '^(\.claude/|changelog\.d/)' | head -1
> ```
>
> Empty output ⇒ skip. Record `Skipped §1 — no user-visible change` in the PR description.

For any **add / change / remove** of behavior, update **English documentation only**. Decision criterion: not "is it already documented?" but "should the user know about it?" Update `docs/reference/` when types / operators / control flow / functions / collections / builtins / errors change spec; update `docs/README.md` TOC for new pages; update `README.md` Features / Sample Code / Installation / Usage when affected (details delegated to `docs/`). If no update is needed, state the reason (internal refactor only, tests-only, etc.).

## 2. CHANGELOG Update Check

> **Skip if** — changed files are only `.claude/` / `docs/` / top-level `*.md` / `changelog.d/` (not a `feat:` / `fix:` / breaking change):
>
> ```bash
> git diff --name-only origin/main | grep -vE '^(\.claude/|docs/|changelog\.d/|[^/]+\.md$)' | head -1
> ```
>
> Empty output ⇒ skip. Record `Skipped §2 — no user-visible change` in the PR description.

For user-impacting changes, add a fragment under `changelog.d/{issue}-{slug}.md` (e.g. `changelog.d/545-546-list-improvements.md`). Body: only `### Added` / `### Changed` / `### Fixed` / `### Removed` sections. Combine multiple categories in one file when they coexist.

```markdown
### Added

- Empty list literal `[]` is now supported with type annotation (#545)

### Fixed

- Some bugfix description (#545)
```

> **Do not edit `CHANGELOG.md` directly.** Fragments are assembled into `CHANGELOG.md` by `scripts/assemble-changelog.sh` at release time (currently manual — see `/release-orchestrator`).

No fragment is needed for internal refactors, test-only changes, or CI-only changes.

## 2.5. `.claude/rules/` + `.claude/skills/` Update Check

If any of the following happened during the work, add an entry to `.claude/rules/` or `.claude/skills/`:

1. **Added a new reject branch / validation check** — verify a regression-test rule entry exists; otherwise add one to `.claude/rules/tests-rejection-tdd.md`:
   ```bash
   git diff origin/<base> -- 'src/**' 'include/**' \
     | grep -nE '^\+.*(codegenError|parserError|return std::nullopt)'
   ```
   For each hit, confirm a regression test that triggers the reject branch directly exists (a legal-case test does not substitute).
2. **Non-obvious pitfall during implementation** — e.g. LLVM API quirk, opaque-pointer caveat, ARC retain/release ordering. Add to the path-scoped `.claude/rules/<name>.md` (ARC → `codegen-arc-cow.md`, type/metadata → `codegen-type-and-metadata.md`, runtime memory → `runtime-memory-safety.md`).
3. **Rejected design alternative** — record why so the evaluation isn't repeated later. Add to the matching path rule.
4. **Recovered from a command / env / shell mistake** — non-obvious fix (undocumented or counter-intuitive). Categories: wrong flag combo, missing env var (`ASAN_OPTIONS`, `RY_ENV`, etc.), wrong `cmake --preset` name or path, wrong `gh` / `git` subcommand or flag, wrong heredoc / quoting / escaping. Add to `.claude/skills/commands-environment-gotchas/SKILL.md`. Simple typos don't qualify.
5. **Cross-cutting PR-review pattern** — recurring across multiple paths ⇒ add to `.claude/skills/pr-review-recurring-patterns/SKILL.md`.

If no entry is needed, state the reason (pure bug fix that won't recur, PR-local-only feedback, etc.).

## 3. Run All Tests

> **Skip if** — changed files are only `.claude/` / `docs/` / top-level `*.md` / `changelog.d/` (no source impact):
>
> ```bash
> git diff --name-only origin/main | grep -vE '^(\.claude/|docs/|changelog\.d/|[^/]+\.md$)' | head -1
> ```
>
> Empty output ⇒ skip. Record `Skipped §3 — no source code change` in the PR description.

```bash
./.claude/skills/pre-commit-checklist/run-tests.sh
```

Runs `cmake --preset default && cmake --build build && ./build/ry_tests && ./build/ry test -p`. The script auto-removes `build/` when `CMakeCache.txt` belongs to a sanitizer/fuzzer preset; pass `--clean` to force removal. Fix any failure before declaring complete.

## 3.5. Sanitizer Verification

> **Skip if** — same as §3 (no source impact). Record `Skipped §3.5 — no source code change`.

> **macOS note**: run ASan / UBSan / TSan via Docker. Avoids macOS-host-only issues such as `fuzz_json` hangs under ASan and the Darwin upstream TSan `LargeMmapAllocator` bug (#1865). `docker/run.sh` sets sanitizer env vars per preset.

**ASan + UBSan** (memory safety + undefined behavior):

```bash
./.claude/skills/pre-commit-checklist/run-asan.sh
```

Fix any finding before declaring complete; do not commit while errors remain.

**TSan** (thread safety):

```bash
./.claude/skills/pre-commit-checklist/run-tsan.sh
```

The C++ TSan run (`ry_tests`) is required and validates `ConcurrencySpecSuite` (= `tests/spec/concurrency.test.ry` stress test). The Ry self-test (`ry test -p`) is warn-only due to the TSan `LargeMmapAllocator` CHECK problem (upstream #1716, Linux-only) — a clean C++ run is sufficient for this PR. LLVM ORC teardown crashes (`~LLJIT()` / `removeResourceTracker` / `~CodeGen()` / `~OverloadEntry()`) can surface on both OSes but are suppressed by the three-stage leak in `src/jit_runner.cpp` (#1187 + #1657); file a new issue if this pattern recurs.

If a race is detected (C++ or self-test), fix it in this PR (`/triage-side-finding` Q1 — hard-to-reproduce CI detection ⇒ immediate fix). Do not park it as a known race. See `/tsan-known-issues` for `LargeMmapAllocator` and ORC teardown entries. For new race patterns not in the #630 audit, file a separate concurrency issue and add a reproducer under `tests/spec/concurrency*.test.ry`.

## 3.5.5. Static Analysis

Reproduce the CI `lint` / `clang-tidy` / `scan-build` jobs locally before pushing. Configuration, suppression rules, and false-positive handling are delegated to `/static-analysis-tools`.

> **macOS note**: use Docker to avoid clang-tidy PCH incompatibility (Apple clang ↔ Homebrew LLVM clang), missing scan-build PATH, and Homebrew LLVM dependencies (#1865).

**clang-tidy** (required):

```bash
./.claude/skills/pre-commit-checklist/run-clang-tidy.sh
```

**cppcheck** (required):

```bash
./.claude/skills/pre-commit-checklist/run-cppcheck.sh
```

**scan-build** (warn-only — strongly recommended): CI runs with `continue-on-error: true`. New null-deref / use-after-free / division-by-zero findings should be addressed in the same PR. CI switches scope per event (#1738): PRs run `--target ry`, push-to-main runs all targets — Docker matches the PR-equivalent fast scan.

```bash
./.claude/skills/pre-commit-checklist/run-scan-build.sh
```

All three at once:

```bash
./.claude/skills/pre-commit-checklist/run-static-analysis-all.sh
```

> `scan-build` and `all` use a dedicated `build-scan-docker/` (host) ↔ `build-scan/` (container) so `build-docker/` stays clean. No cleanup needed before the next `./docker/run.sh default ...`. HTML reports land in `build-scan-docker/scan-build-report/<timestamp>/index.html`.

Fix clang-tidy / cppcheck failures before declaring complete. Common patterns (e.g. `performance-inefficient-string-concatenation`) and canonical workarounds are in `.claude/rules/build-warning-flags.md`.

## 3.6. libFuzzer Fuzzing

> **Skip if** — changed files do **not** include the parser/lexer/json/utf8/string family:
>
> ```bash
> git diff --name-only origin/main | grep -E '(src/(parser|lexer|runtime_(json|utf8|string))|include/ry/(parser|lexer|runtime_(json|string)))' | head -1
> ```
>
> **Empty output ⇒ skip**. Otherwise run the matching fuzzer. Record `Skipped §3.6 — no parser/lexer/json/utf8/string change` in the PR description. **Fuzzer mapping**: parser/lexer → `fuzz_parser`; json → `fuzz_json` (+ `fuzz_utf8` if `runtime_string*` changed); utf8/string → `fuzz_utf8` (+ `fuzz_json` if `runtime_string*` changed). `tests/`-only changes targeting these areas ⇒ run the matching fuzzer only (judgment call); run all three when unsure.

**CI jobs are disabled; always run locally on the feature branch.** Harness requirements and known limits are delegated to `/libfuzzer-harness`.

> **macOS note**: `fuzz_json` hangs under native macOS ASan, so Docker is required (#1865).

```bash
./.claude/skills/pre-commit-checklist/run-fuzz.sh
```

Runs all three targets (`fuzz_parser` / `fuzz_json` / `fuzz_utf8`) for 60 s each with `-rss_limit_mb=512` and `-artifact_prefix=tests/fuzz/regressions/<name>/`. For a single target, invoke `./docker/run.sh fuzz <target> ...` directly. All three targets must exit 0 after 60 s. On crash, follow `/triage-side-finding`. **Hard-to-reproduce crashes** (no repro in 3 local attempts / no saved corpus / CI-only) ⇒ Q1 = Yes ⇒ **fix in the same PR** (don't lose the reproduction window — same principle as the TSan race rule above). If the current PR's code directly caused it, also fix in the same PR (Q4(a)). Only file a separate issue (Q4(b)) when the bug is locally reproducible, pre-existing, and would substantially expand scope. Save the crash input to both `tests/fuzz/regressions/<name>/` and `tests/fuzz/corpus/<name>/` regardless of reproducibility.

## 3.6.5. tree-sitter Grammar Regression Check

> **Skip if** — changed files do **not** include the tree-sitter grammar / EBNF spec / external scanner:
>
> ```bash
> git diff --name-only origin/main | grep -E '^(docs/grammar\.ebnf$|editor/tree-sitter/(grammar\.js$|src/))' | head -1
> ```
>
> **Empty output ⇒ skip**. Otherwise rebuild & reinstall. Record `Skipped §3.6.5 — no tree-sitter grammar change` in the PR description.

Evaluated independently of the §0 matrix: a `.md` / `docs/`-only PR still fires this step if `docs/grammar.ebnf` is touched (which is why the matrix row for `.md` / `docs/`-only omits it).

```bash
./.claude/skills/pre-commit-checklist/run-tree-sitter.sh
```

Chains `build.sh` (`tree-sitter generate` + `build` → `ry.so`; generate failure = grammar syntax error), `install.sh --no-build` (copies into Neovim parser dir), and `check.sh --no-build` (`tree-sitter parse` over every `tests/spec/**/*.test.ry`; **exit 0 required**). `expected-fail.txt` entries are SKIPped; ones now parsing emit `WARN: ... now passes` and should be removed from the list. Out-of-list ERRORs are the only regressions that count. Hand-curated Phase 2 corpus lives in `editor/tree-sitter/test/corpus/*.txt` (#1633); a visual highlight check in Neovim is recommended.

## 3.7. Background Task Residual Check

Before declaring complete, confirm no self-launched background tasks or shells remain.

```bash
./.claude/skills/pre-commit-checklist/check-background.sh
```

Exits 1 if `ps aux | grep -E 'zsh.*cat'` finds orphan shells (the classic `run_in_background=true` + heredoc zombie: `zsh` + `cat` blocked on stdin — see AGENTS.md §"Bash コマンドの実行ルール"). The script only **detects** — stop any remaining via `TaskStop` or `kill <pid>` (Claude-tool operations, not shell-invocable). Also confirm every background task finished via `BashOutput` / `TaskOutput`.

## 4. Label Cleanup

**Do not change labels at the self-verification stage.** Label transitions happen post-merge: `git-close-pr` Step 7 removes `wip` autonomously. Do not run individual commands directly.
