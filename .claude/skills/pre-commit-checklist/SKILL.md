---
name: pre-commit-checklist
description: Pre-commit checklist — docs / CHANGELOG / rules+skills / tests / ASan+UBSan / TSan / libFuzzer / background execution prohibition check / labels. Use before declaring complete, before PR, self-verify, running sanitizers/tests; also on Japanese triggers 作業完了前, 完了前, 実装完了, 修正完了, マージ前, PR を出す前, セルフ検証, 動作確認, サニタイザー実行, テスト実行, チェックリスト. Always fires near the end of feature work.
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
| includes parser/lexer/json/utf8/string/io※ | ✓ | ✓ | ✓ | ✓ | ✓ |
| other code changes | ✓ | ✓ | ✓ | ✓ | skip |

**Legend**: `✓` = required / `skip` = may omit (record in PR description) / `review` = judgment call.

※ "parser/lexer/json/utf8/string/io family" = changes to `src/parser/*`, `src/lexer/*`, `src/runtime/native/json.cpp`, `src/runtime/core/utf8.cpp`, `src/runtime/core/string.cpp` (also `src/runtime/core/regex_parser.cpp`), `src/runtime/native/io.cpp`, or `include/ry/parser/*.hpp`, `include/ry/lexer/*.hpp`, `include/ry/runtime/native/json.hpp`, `include/ry/runtime/core/string.hpp`, `include/ry/runtime/native/io.hpp`. **`runtime/core/string.cpp` is a dependency of both `fuzz_json` and `fuzz_utf8`**, so it always falls in this row.

**Notes**: multiple matching rows ⇒ take the strictest per column (`✓` > `review` > `skip`). Always required regardless of matrix: §2.5 (rules/skills, when applicable), §3.5.5 (Static Analysis), §3.5.6 (Rust Lint, when `crates/` changed), §3.5.7 (Prompt/Instruction Reference Lint, when `.claude/` / `AGENTS.md` / `CLAUDE.md` changed), §3.6.5 (tree-sitter Grammar Regression Check, when applicable), §3.7 (background hygiene), §4 (Label Cleanup, no-op). For `.md` / `docs/`-only and `changelog.d/`-only PRs, §4 is effectively the only required action: the edited area satisfies its own `✓` (self-edit = done), and the Skip-if bash returns `skip` for everything else.

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

> **Precondition**: a Rust 1.83+ toolchain (`cargo` / `rustc`) must be on `PATH` — `cmake --preset {default,asan,tsan,fuzz}` all build the `emit` Rust cdylib via corrosion, and the prefix must ship a shared `libLLVM`. See `AGENTS.md` § "Build & Test" (the `ry-ci` Docker image bakes both in).

```bash
./.claude/skills/pre-commit-checklist/run-tests.sh
```

Runs `cmake --preset default && cmake --build build && ./build/ry_tests && ./build/ry test -p` (on macOS the script auto-detects the host and substitutes the `rust-emit` preset → `build-rust/`; see `AGENTS.md` § "Build & Test"). The script auto-removes the host build dir when its `CMakeCache.txt` belongs to a sanitizer/fuzzer preset; pass `--clean` to force removal. Fix any failure before declaring complete.

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

The C++ TSan run (`ry_tests`) is required and validates `ConcurrencySpecSuite` (= `tests/spec/concurrency.test.ry` stress test). The Ry self-test (`ry test -p`) is warn-only due to the TSan `LargeMmapAllocator` CHECK problem (upstream #1716, Linux-only) — a clean C++ run is sufficient for this PR. LLVM ORC teardown crashes (`~LLJIT()` / `removeResourceTracker` / `~CodeGen()` / `~OverloadEntry()`) can surface on both OSes but are suppressed by the three-stage leak in `src/jit/jit_runner.cpp` (#1187 + #1657); file a new issue if this pattern recurs.

If a race is detected (C++ or self-test), fix it in this PR (`/triage-side-finding` Q1 — hard-to-reproduce CI detection ⇒ immediate fix). Do not park it as a known race. Upstream TSan allocator bugs / LLVM ORC teardown crashes etc. are catalogued in `KNOWLEDGE.md` の `## サニタイザー既知問題` section. For new race patterns not in the #630 audit, file a separate concurrency issue and add a reproducer under `tests/spec/concurrency*.test.ry`.

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

## 3.5.6. Rust Lint (clippy + rustfmt)

> **Skip if** — no `crates/` files changed:
>
> ```bash
> git diff --name-only origin/main | grep -E '^crates/' | head -1
> ```
>
> Empty output ⇒ skip. Record `Skipped §3.5.6 — no Rust crate change`.

Reproduce the CI `lint` job's Rust quality gate (`cargo fmt --check` +
`cargo clippy -- -D warnings` over `crates/emit`):

```bash
./.claude/skills/pre-commit-checklist/run-rust-lint.sh
```

The toolchain is pinned by `rust-toolchain.toml` to the version baked into the
CI image (`docker/ci.Dockerfile` `RUST_VERSION`), so local rustfmt / clippy
output matches what CI gates on — bump both together. `clippy` compiles
`llvm-sys`, so it needs `LLVM_SYS_211_PREFIX` + a shared libLLVM; the script
defaults it to Homebrew `llvm@21` on macOS (CI bakes it into the image ENV). Fix
any diff / warning before declaring complete; do not commit while
`cargo clippy -- -D warnings` fails. (#2015)

Lint policy lives in `[workspace.lints]` (root `Cargo.toml`); crate-specific FFI
carve-outs stay as `#![allow(...)]` in `crates/emit/src/lib.rs`.

## 3.5.7. Prompt/Instruction Reference Lint (#2029)

> **Skip if** — no `.claude/`, `AGENTS.md`, or `CLAUDE.md` files changed:
>
> ```bash
> git diff --name-only origin/main | grep -E '^\.claude/|^AGENTS\.md$|^CLAUDE\.md$' | head -1
> ```
>
> Empty output ⇒ skip. Record `Skipped §3.5.7 — no prompt-definition change`.

Reproduce the CI `lint` job's reference-integrity gate — fails on stale
inline-code paths (#1827-class drift), dead slash-command (`/<name>`) links, and `KNOWLEDGE.md`
section-name English/Japanese drift:

```bash
./.claude/skills/pre-commit-checklist/run-prompt-refs-lint.sh
```

The lint inspects **inline-code backtick spans only** across `.claude/**/*.md`
+ `AGENTS.md` + `CLAUDE.md`. Fenced blocks and plain prose are the escape hatch
for intentionally non-existent example paths; `<...>` placeholders, globs, and
`:line` / `::symbol` suffixes are skipped. `KNOWLEDGE.md` is the section-heading
source of truth, not a path-scan target. The `::error::` output names each
`file:line` and its fix — correct it before declaring complete. Convention
details (incl. the `<...>` placeholder rule): `.claude/rules/docs-reference-conventions.md`.

## 3.6. libFuzzer Fuzzing

> **Skip if** — changed files do **not** include the parser/lexer/json/utf8/string family:
>
> ```bash
> git diff --name-only origin/main | grep -E '(src/(parser|lexer)|src/runtime/native/(json|io)\.cpp|src/runtime/core/(utf8|string)\.cpp|include/ry/(parser|lexer)|include/ry/runtime/native/(json|io)\.hpp|include/ry/runtime/core/string\.hpp)' | head -1
> ```
>
> **Empty output ⇒ skip**. Otherwise run the matching fuzzer. Record `Skipped §3.6 — no parser/lexer/json/utf8/string/io change` in the PR description. **Fuzzer mapping**: parser/lexer → `fuzz_parser`; json → `fuzz_json` (+ `fuzz_utf8` if `runtime/core/string.cpp` changed); utf8/string → `fuzz_utf8` (+ `fuzz_json` if `runtime/core/string.cpp` changed); io → `fuzz_io_open`. `tests/`-only changes targeting these areas ⇒ run the matching fuzzer only (judgment call); run all four when unsure.

**CI jobs are disabled; always run locally on the feature branch.** Harness requirements and known limits are delegated to `/libfuzzer-harness`.

> **macOS note**: `fuzz_json` hangs under native macOS ASan, so Docker is required (#1865).

```bash
./.claude/skills/pre-commit-checklist/run-fuzz.sh
```

Runs all four targets (`fuzz_parser` / `fuzz_json` / `fuzz_utf8` / `fuzz_io_open`) for 60 s each with `-rss_limit_mb=2048` and `-artifact_prefix=tests/fuzz/regressions/<name>/`. The 2048 MB cap (raised from 512 MB) accommodates libFuzzer's coverage-tracking overhead (~275k inline 8-bit counters + PC table), which empirically peaks at 400–600 MB across all four harnesses (measured: `fuzz_parser` 514 MB, `fuzz_json` 597 MB, `fuzz_utf8` 429 MB, `fuzz_io_open` 536 MB). The previous 512 MB cap triggered a startup OOM in `fuzz_parser` and was borderline for `fuzz_json` / `fuzz_io_open` — this is a structural corpus + coverage overhead, not a parser-specific bug (#1976). For a single target, invoke `./docker/run.sh fuzz <target> ...` directly. All four targets must exit 0 after 60 s. On crash, follow `/triage-side-finding`. **Hard-to-reproduce crashes** (no repro in 3 local attempts / no saved corpus / CI-only) ⇒ Q1 = Yes ⇒ **fix in the same PR** (don't lose the reproduction window — same principle as the TSan race rule above). If the current PR's code directly caused it, also fix in the same PR (Q4(a)). For other fuzzer crashes (locally reproducible, pre-existing): **fuzz crashes are crash-class** (SEGV / UAF / corruption) under `/triage-side-finding` Phase B rules ⇒ Q4(a) fix in same PR regardless of size or origin (Phase B does not autonomously route to Q4(b); only `>1000` 行の非クラッシュ系 escalates to Q2 informed-consent gate). Save the crash input to both `tests/fuzz/regressions/<name>/` and `tests/fuzz/corpus/<name>/` regardless of reproducibility.

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

## 3.7. Background Execution Prohibition Check

Claude (メインエージェント) から起動する background 実行は #1947 で全面禁止 (AGENTS.md §"Bash コマンドの実行ルール" 参照)。セルフ検証では会話履歴を振り返り、本セッションで以下のいずれかが発生していないか確認する:

- `Bash(run_in_background=true)` の呼び出し
- shell の末尾 `&` による background 起動 (`cmake --build &` 等)
- `nohup` / `disown` / その他 detach 手段
- `Agent({run_in_background: true, ...})` (subagent background)

該当呼び出しがあった場合は完了宣言を停止しユーザーに報告 (foreground 同期実行に切り替えるか、並列化が必要なら subagent を foreground で複数同時起動する設計に変更)。該当なしであれば OK。

> **並列化が必要だった場合の正しいパターン**: single message に multiple `Agent` tool calls を入れて foreground 並列実行。`.claude/agents/` には sanitizer-runner / test-runner / fuzzer-runner / pr-review-responder 等の事前 subagent が用意されている。
>
> **Why detection by recall, not `ps aux`?** 旧 `check-background.sh` (#1947 で削除) は `ps aux | grep -E 'zsh.*cat'` で OS 全体を見ていたため、別 Claude Code セッションの正規プロセスを誤検出する欠陥があった (#1944)。全面禁止下では「使わなかったことを会話履歴で確認」が正しい scope。

## 4. Label Cleanup

**Do not change labels at the self-verification stage.** Label transitions happen post-merge: `git-finalize-pr` Step 7 removes `wip` autonomously. Do not run individual commands directly.
