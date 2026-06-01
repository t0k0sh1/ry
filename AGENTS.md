# ry - Development Guidelines

Situational playbooks live in `.claude/skills/`; trigger them by description or by `/<skill-name>`.

> **Terminology (v0.0.17)**: definitions are in `docs/reference/glossary.md` (#1480). `module` = the unit of `from xxx import ...`; `package` is reserved for future use; `effectivePackage` / `RY_REGISTER_STDLIB_PACKAGE` / `__ry_<symbol>` retain their legacy naming.

## Build & Test

```bash
cmake --preset default                                  # Ninja + LLVM (CMakePresets.json)
cmake --build build                                     # Ninja parallelizes automatically
./build/ry_tests                                        # C++ tests (GoogleTest)
./build/ry test -p                                      # Ry self-tests (all *.test.ry)
./build/ry test tests/spec/<file>.test.ry               # run an individual file
```

> The `./build/ry` built inside the repo prefers the project-local `share/std/` per the hidden `[paths]._dev_stdlib` setting in `package.toml`. Use `RY_ENV=internal` only when extra isolation is needed.

> **Rust `ry_llvm_emit` (`RY_LLVM_EMIT_IMPL_RUST=ON`, #1950/#1993)**: build with `cmake --preset rust-emit` (→ `build-rust/`). This path REQUIRES a **shared libLLVM** in the LLVM prefix so `ry` and the Rust cdylib share one LLVM instance — otherwise `ConstantFP::get` hangs on float constants (two-LLVM `fltSemantics` split, #1997; see `docs/architecture/llvm-ir-emission-boundary.md` §"Sub-issue 3 landed"). The static-only `/usr/local/llvm` does NOT qualify; the `rust-emit` preset points `LLVM_DIR` at Homebrew `llvm@21` (ships `libLLVM.dylib`). CI's container already builds LLVM with `LLVM_BUILD_LLVM_DYLIB=ON`. The default (OFF, C++ impl) path is unaffected and uses `--preset default` (`/usr/local/llvm` static).

## tree-sitter grammar build & install

A PR that modifies any of `docs/grammar.ebnf` / `editor/tree-sitter/grammar.js` / `editor/tree-sitter/src/scanner.c` requires rebuilding `ry.so`. Build/install commands, prerequisites, pitfalls (externals enum order / `mark_end` / `valid_symbols` semantics / highlights.scm), and the verification recipe are in `.claude/skills/tree-sitter-grammar-editing/SKILL.md` (or `/tree-sitter-grammar-editing`) and `editor/tree-sitter/README.md`; the self-verification procedure is in `/pre-commit-checklist` §3.6.5. When editing `editor/tree-sitter/grammar.js` / `src/scanner.c` / `queries/*.scm`, that skill is auto-loaded via a path-scoped rule.

## Compiler warning flags

Details on compiler warning flags are in `.claude/rules/build-warning-flags.md`.

## IR golden tests

The notation and execution procedure for LLVM IR golden tests are in `.claude/rules/codegen-llvm-ir-conventions.md`.

## CI: container image (GHCR pre-baked)

CI Linux jobs use a pre-baked container (`ghcr.io/<owner>/ry-ci:llvm-21`; the glibc-old job in release.yml is pinned to the immutable `ry-ci-glibc-old:llvm-21-rev<N>`) (#1505, #1508). Image build, version bumps, `rev<N>` tags, rollback, and the release-pin update procedure are in `.claude/skills/ci-image-workflow/SKILL.md` (or `/ci-image-workflow`). macOS continues to use Homebrew.

## Knowledge base (.claude/rules/ + .claude/skills/ + .claude/agents/ + KNOWLEDGE.md)

- **`.claude/rules/<name>.md`** — path-scoped rule. Auto-loaded when editing a file matching the frontmatter `paths:` glob.
- **`.claude/skills/<name>/SKILL.md`** — context-triggered skill. Invoked when the `description:` matches.
- **`.claude/agents/<name>.md`** — subagent definition. Launched as an **independent context** via the `Agent` tool with `subagent_type: <name>` (in contrast to skills, which run inside the same context). Cannot be invoked via the `/<name>` slash-command form (because it is an agent, not a skill). Use this for tasks like critique of plans, design, or implementation where you want the artifact evaluated in isolation from the main conversation history. **For parallelizable verification steps, launch multiple subagents foreground concurrently** (a single message with multiple `Agent` tool calls). Background execution is prohibited (see AGENTS.md §"Bash execution rules"; #1947). Current catalog:
    - `.claude/agents/devils-advocate.md` — critique agent for plan / design review
    - `.claude/agents/bug-forensics-analyst.md` — bug origin determination / git archaeology / test-gap analysis (launched via `/triage-side-finding` Q3)
    - `.claude/agents/sanitizer-runner.md` — subagent that runs and analyzes ASan+UBSan / TSan in an independent context (for parallelization)
    - `.claude/agents/test-runner.md` — subagent that runs and triages C++ ry_tests + Ry self-tests in an independent context (for parallelization)
    - `.claude/agents/fuzzer-runner.md` — subagent that runs and triages libFuzzer harnesses in an independent context (for parallelization)
    - `.claude/agents/pr-review-responder.md` — subagent that analyzes CodeRabbit / human reviewer comments and produces replies and fix proposals
- **`KNOWLEDGE.md`** (repository root) — a provisional buffer for uncategorized findings. New knowledge that has no matching entry in rules / skills accumulates here, and once stable is promoted into rules / skills. Format, grep convention, external-reference policy, "when to write" triggers, and the promotion procedure are in `/knowledge-md-management`.

## ASan + UBSan (Address + UndefinedBehavior Sanitizer)

For local development, use `cmake --preset asan` to enable ASan + UBSan together and run the tests. Build commands, runtime env (`ASAN_OPTIONS=detect_container_overflow=0` / `UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1`), and the rationale for each setting (suppressing LLVM mixed false positives / why `-fno-sanitize=vptr,function`) are in `.claude/skills/commands-environment-gotchas/SKILL.md` (or `/commands-environment-gotchas`); the self-verification procedure is in `/pre-commit-checklist` §3.5.

Any issue detected by ASan or UBSan (memory leak, buffer overflow, use-after-free, undefined behavior, etc.) MUST be resolved. Do not commit with a sanitizer error left unresolved.

Incident knowledge and known issues for ASan / UBSan (masking mechanisms, allocator differences, platform-specific manifestation paths) are in the `## Known sanitizer issues` section of `KNOWLEDGE.md`.

## TSan (ThreadSanitizer)

For verifying thread safety, use the TSan preset. The build command (`cmake --preset tsan`), exclusivity with ASan-UBSan (`build-tsan/` isolation), the required vs. warn-only job split, and known upstream bugs (LargeMmapAllocator / LLVM ORC teardown / signal-handler `siglongjmp`) are in the `## Known sanitizer issues` section of `KNOWLEDGE.md`; the self-verification procedure is in `/pre-commit-checklist` §3.5.

> If you introduce a new race, you MUST fix it within the same PR. Warn-only is only a workaround for the TSan allocator bug; it does not license the introduction of an actual race.

## libFuzzer (coverage-guided fuzzing)

**The CI job is currently disabled** — you MUST run it manually during feature-branch self-verification (see `/pre-commit-checklist` §3.6). Save crash inputs in both `tests/fuzz/regressions/<name>/` and `tests/fuzz/corpus/<name>/`. Harness requirements, build commands, and known limitations are in `.claude/skills/libfuzzer-harness/SKILL.md` (or `/libfuzzer-harness`). Incident knowledge and known issues are also in the `## Known sanitizer issues` section of `KNOWLEDGE.md`.

## Memory-safety rules (C++ runtime)

The runtime memory-safety rules (forbidden-function table / `oom_abort(n)` / NULL checks on external input / CI-lint auto-block) are in `.claude/rules/runtime-memory-safety.md`.

## Workflow overview

Issue review → consult the knowledge base (path-scoped rules also auto-load during implementation) → Plan mode (Task 1 = `/git-claim-issue` to attach `wip`) → TDD implementation → self-verification via `/pre-commit-checklist` → subsequent git operations (commit / push / PR / merge) follow "Separation of Concerns". If you want to chain PR review response → CI check → push → merge in a single command, use `/git-close-pr` (it stops on blockers).

## Issue-driven development

- **Repository**: `t0k0sh1/ry`
- **Start**: the user specifies an issue number / URL → understand the content → enter Plan mode. When instructed to "find the next issue", fetch open issues (excluding `wip`), present candidates with bugs prioritized → after selection, enter Plan mode.
- **Label handling**: labels MUST be attached/removed via skills (`git-claim-issue` / `git-close-pr` Step 7 use `--add-label` / `--remove-label`, preserving existing labels).
- **Scope verification when splitting an issue**: a decision to file or separate a derivative issue is checked via `/scope-decomposition` against symmetry (4 axes, REQ-1), split rationale (3 categories, REQ-2), derivative-chain caution (3rd-degree and beyond, REQ-3), and the single-preview-for-n-split rule when filing an oversized issue (REQ-5). Target-shrinking splits during Plan mode are prohibited by REQ-4.

## Plan mode rules

- **Entry condition**: the target issue has been identified (OPEN state confirmed with `gh issue view <n>`) and is in sync with the remote (no need to pre-attach `wip`; Task 1 attaches it).
- **First task of the implementation plan (fixed)**:
  - **Task 1**: attach the `wip` label to the issue via `/git-claim-issue`.
  - Feature-branch creation is performed automatically on the first `/git-push` call, so do not list it as an independent task in the plan.
- **Implementation-plan scope**: through self-verification only (do not include git add / commit / push / PR creation).
- **Plan abstraction level (WHAT/HOW separation)**: the plan stays at "what to achieve" (WHAT); "how to implement" (HOW) is deferred to the implementation phase. Excessive HOW detail in the plan is detected by `/plan-rubric`.
- **The implementation plan MUST include**:
  - The first task matches the fixed template (Task 1 = `/git-claim-issue`). Feature-branch creation is performed automatically on the first `/git-push` call.
  - Whether you consulted the relevant entries in `.claude/rules/<name>.md` / `.claude/skills/<name>/SKILL.md` for the paths you intend to edit (if such entries exist, quote them in the plan body and explain how you will use them).
  - A self-verification task confirming the implementation matches the specification.
  - An update to the English documentation (README.md / docs) — or a confirmation that no update is needed.
  - When a terminology change or identifier rename is involved: include `/horizontal-sweep` as a plan task (the 4-step procedure is in `.claude/skills/horizontal-sweep/SKILL.md`).
- **Handling side findings**: follow "Handling side findings" under "Separation of Concerns" (`/triage-side-finding`). **Only when Claude Code autonomously decides `/triage-side-finding` Q4(b) "request permission to file"** should an "open a separate issue" task be included in the implementation plan (in the Q1 hard-to-reproduce / Q2 user instruction → immediate-fix branches, the work is absorbed into the same PR, so no plan task is needed). **However, the actual filing waits for explicit user permission** — even when the plan contains an "open a separate issue" task, Claude Code only presents the proposed issue contents and waits for user permission (see "Separation of Concerns" §What the user explicitly directs / §Prohibition on presenting choices in filing decisions).
- **Prohibition on splitting the target issue (target-shrinking)**: proposing to **split the target issue itself to shrink the scope** during Plan mode is prohibited (it derails the implementation plan). The Q4(b) separate-issue filing for orthogonal side findings is outside this rule (it does not change the target issue's scope). Details: `/scope-decomposition` REQ-4 / `/plan-rubric` Axis 2. If the issue is found to be oversized, resolve the split decision **before entering Plan mode** via `/scope-decomposition` REQ-5.
- **Prohibition on splitting TDD cycles**: do not split Red / Green / Refactor into separate plan tasks; bundle them into a single "TDD cycle" task (turn the cycle internally per case).

## Use trace to analyze internal behavior

Trace usage (`--trace` / `--trace-out` / JSON Lines / analyzing internal behavior, import resolution, JIT execution) is in `.claude/skills/ry-trace/SKILL.md` (or `/ry-trace`).

## Bash execution rules

### Total ban on Claude-initiated background execution

Any background execution initiated by Claude (the main agent) is **totally banned** (#1947). No exceptions.

**Prohibited targets:**
- Use of `Bash(run_in_background=true)` (regardless of purpose or command)
- Background startup via trailing `&` in the shell (`cmake --build &`, etc.)
- `nohup` / `disown` / any other detach mechanism
- `Agent({run_in_background: true, ...})` (subagent background)
- Builds (`cmake --build`) / tests (`./build/ry_tests`) / fuzzers / any long-running process — all foreground-synchronous only

**When parallelization is needed:**
Put multiple `Agent` tool calls in a single message and **launch subagents concurrently in foreground**. Each subagent runs foreground in an independent context, and the main agent synchronizes on all return values. Dedicated subagents for the verification steps in `/pre-commit-checklist` (sanitizer / test / fuzzer / PR review response, etc.) are pre-provisioned in `.claude/agents/` — choose between them using the catalog (see AGENTS.md §"Knowledge base").

**Why:** background execution carries a structural risk of `task_id` recording loss (Bash-launched processes land in the OS process table and force reliance on OS-level scans, which misdetect separate Claude Code sessions — #1944). Subagent background can be `TaskStop`-ed inside the task framework, but the "misuse-of-mechanism" risk remains. Eliminating the concept of background execution outright removes the cognitive cost and risk at the root. Parallelization is fully achievable via foreground subagents.

> **Side note (heredoc input has an independent rule)**: heredoc input such as `./build/ry -c <<'EOF' ... EOF` MUST be run foreground, or replaced with file input (`./build/ry script.ry`). This is an independent rule that predates the background ban and is not the subject of this section (the heredoc + background hang risk is a historical matter).

### Timeout settings

- The Bash tool's `timeout` parameter MUST be set even for foreground execution (the 120,000 ms = 2 min default can be too short).
- Build-class commands: `timeout: 300000` (5 min); even long tests cap at `timeout: 600000` (10 min).
- For work exceeding these caps, split the script / step it, or parallelize via foreground subagents (do not work around with background execution).

### Prohibition on temporary file creation

- **Temporary-file creation inside the project (under the repository working tree) is prohibited without exception**. Files created on the assumption that they will be deleted later — `tmp_*.ry`, scratch files at the repo root, throwaway verification `*.ry`, scratch scripts — MUST NOT be placed inside the working tree. The user has repeatedly objected to "creating and deleting files in a loop".
- For ad-hoc verification of Ry code, follow the `/ry-playground` skill (`.claude/skills/ry-playground/SKILL.md`) and use the `./build/ry -c <<'EOF' ... EOF` heredoc form (single-line or multi-line both work; the single-quoted `'EOF'` suppresses shell expansion). **Run inline without creating a file.**
- To pin a specification or behavior (i.e., persist it), append directly to the body of `tests/spec/*.test.ry` (spec tests are durable assets).
- For C++-side verification, append to `tests/test_runtime_*.cpp` and run with `./build/ry_tests --gtest_filter=...`.
- **Limited `/tmp` exception**: when the GitHub CLI or another external tool's interface requires a file path that cannot be expressed via command-line arguments or heredocs alone, you MAY use a file under `/tmp/`. **However, do not delete the created file and do not attempt to delete it** (do not write `rm /tmp/...` / `unlink` / cleanup traps). Defer to OS tmp cleanup.
- Intentionally creating a "file to be deleted in the end" as a workaround is prohibited (inside the project completely; even in `/tmp` do not write delete commands). For verification, choose one of `/ry-playground` (heredoc) / appending into an existing test file / `/tmp` (no deletion).

## Prohibited terminology: flake / flaky

CI #2578, in which Claude Code concluded that a crash in `tests/spec/collection_meta_propagation.test.ry` was "flake; re-run", triggered this rule (#1990). A term meant for non-deterministic phenomena occurring at under 1% has been repeatedly applied to phenomena that occur over 50% of the time. Truly flaky events do not exist on a deterministic Von Neumann machine, and calling a phenomenon that necessarily occurs when its conditions are met "flaky" is equivalent to **abandoning root-cause analysis**. After repeated objections without improvement, total prohibition is the only remaining option.

### Prohibition rule (no exceptions)

- **MUST**: Claude Code MUST NOT use the words `flake` / `flaky` in any explanation or output (responses / commit messages / PR descriptions / new KNOWLEDGE entries / code comments / `.claude/skills/*.md` / `.claude/agents/*.md` / `.claude/rules/*.md` etc.) **in any language**.
  - This includes Japanese transliterations, katakana renderings, and any rendering in any other language.
  - Substituting another-language synonym (e.g., `unstable`, `intermittent`) **as a stand-in for `flake` / `flaky`** is also prohibited (using it in any way that does not follow the "Required alternative wording" rule below is not allowed).
- **MUST**: Claude Code MUST NOT use `flake` / `flaky` as the **reason or conclusion for a CI failure or test failure** (whether spoken, written, or as part of an autonomous triage decision).

### Required alternative wording

When explaining a CI failure or test failure, Claude Code MUST use one of the following:

- **(a) Stating the occurrence condition**: e.g., "occurs when the timing at which heap consolidation runs during LLVM ORC JIT teardown collides with the state of glibc tcache" — specifically identify the conditions under which the phenomenon occurs.
- **(b) An explicit link to an existing `KNOWLEDGE.md` entry**: always include the issue number + line number (e.g., `KNOWLEDGE.md` L261, #1895). A vague reference by entry name alone is not acceptable.
- **(c) When the root cause is not yet identified**: write explicitly "**Occurrence condition not yet identified. Investigation of the reproduction condition is incomplete.**" In this case, **a casual re-run suggestion is prohibited**. Instead, either file the investigation as a task, or ask the user for permission to begin investigation.

### Handling of existing text

- Historical text in `KNOWLEDGE.md` / `CHANGELOG.md` (e.g., `KNOWLEDGE.md` L261 #1895's `~5-10 % Linux CI flake`, `CHANGELOG.md` L598's `Linux CI flake (~5-10 %)`, etc.) MUST NOT be modified. Reason: searchability. Cross-references with upstream LLVM issues, CI analysis logs, and GitHub Issues would break.
- This rule applies only to **new writing (future additions to `KNOWLEDGE.md` / commit messages / PR descriptions / Claude Code responses / `.claude/skills/*.md` / `.claude/agents/*.md` / `.claude/rules/*.md` etc.)**.
- **MUST**: **reintroducing `flake` as a conclusion by quoting or referencing historical flake text** is also prohibited (it is the same accident path as CI #2578). When describing a symptom that hits historical text, convert it into one of (a)/(b)/(c) before writing.

## Git branch policy

- Feature branches are created from `main`, and PRs target `main`. Direct commits to `main` are prohibited.
- **MUST (no exceptions)**: a feature-branch name MUST NOT contain the string `main`. The check is performed against the branch name after lowercasing and stripping all non-alphabetic characters (`/`, `-`, `_`, digits, symbols, etc.); if `m`, `a`, `i`, `n` appear consecutively in that order in the resulting string, it is a violation (no evasion via symbols, case, or kebab segment boundaries; and even when the substring appears incidentally inside a natural word such as `domain-driven`, it is still prohibited). Reason: to fully eliminate search noise from queries like `git branch | grep -i main` and false matches in script-based detection. On a violation, rename via `git branch -m <new>` before pushing.
- To bring the latest `main` into a feature branch, use **`git rebase origin/main`** (to keep history linear). Do not merge `main` into the feature branch. The concrete procedure is in `/git-push` / `/git-create-pr` / `/git-resolve-conflicts`.
- For pushes after a rebase, use **`git push --force-with-lease`** (a force push is required from the second push onwards because SHAs are rewritten; `--force-with-lease` detects unexpected progression of the remote and blocks the overwrite). Do not re-run `git fetch` between `fetch` and `push` (it relaxes the lease guard).
- The rebase policy above applies only to bringing `main` into a feature branch. The update of `main` itself in `/preparing-for-release` is on a separate path and remains `git pull --ff-only origin main` (linearity is guaranteed, so no change is needed).
- Before merging a PR, verify there are no untracked files or uncommitted changes. If there are, report to the user before merging and confirm whether to commit them.
- When there are diffs under `.serena/`, commit them together with the other changes.

## Separation of Concerns

### What Claude Code does autonomously

- Implementation
- Test execution
- Self-verification
- Documentation updates
- Removing the `wip` label after PR merge (consolidated into `git-close-pr` Step 7. Executed autonomously immediately after merge completion, without waiting for user instruction. Issue closure is performed automatically by GitHub via the `Closes #xx` keyword. Note that this is a record of the feature landing on `main`, not a release completion — see "Release Workflow").

#### Handling side findings

The early short-circuit flow when detecting a side finding (Q1 hard-to-reproduce CI issue → Q2 explicit user instruction → Q3 `bug-forensics-analyst` → Q4 Claude Code autonomous decision; **phase-aware**) and the Issue Creation Steps are in `.claude/skills/triage-side-finding/SKILL.md` (or `/triage-side-finding`). Q4 branches by the phase in which triage occurred — **Phase A** (during filing / splitting / Plan mode) has two branches (a) immediate fix and (b) request permission to file; **Phase B** (during implementation / review response) defaults to same-PR absorption, with crash-class findings (ASan/UBSan/TSan/libFuzzer + abort/SEGV/UAF/memory leak/corruption) escalating unconditionally, and non-crash findings escalating via the **1000-line threshold** (added + removed raw line count) re-routing to Q2. The autonomous-decision requirements of Q4 are in §"Prohibition on presenting choices in filing decisions".

#### Priority order for side-finding decisions

The following priority order applies to side-finding triage (Q1–Q4). **The rules in this subsection apply only to "decisions about how to handle a side finding"** — quality-gate rules (sanitizer-error ban / TDD-cycle split ban / direct-commit-to-`main` ban / committing `.serena/` diffs together / etc.) are NOT overridden by this subsection.

1. **User wish takes priority**: when the user has explicitly directed how to handle the side finding (`/triage-side-finding` Q2 = Yes), the user's instruction takes priority over the decision flow. Skill / agent / advisor judgments MUST NOT be used as a reason to override a user instruction. **However, this applies only to side-finding decisions** — quality gates such as sanitizer-error ban / TDD-cycle split ban are NOT overridden.
2. **Immediate fix for hard-to-reproduce issues**: hard-to-reproduce CI-detected memory corruption / concurrency races / fuzz crashes etc. (matching `/triage-side-finding` Q1 = Yes) prioritize the timing of the fix over origin determination (regression vs. pre-existing). The principle is not missing the reproduction window. **However, this applies only to side-finding decisions.**
3. **Fixing takes priority over analysis**: when Q1 / Q2 applies, do not invoke `bug-forensics-analyst` / advisor. The meaning is "after choosing the immediate fix, do not burn time on unnecessary analysis"; it does not conflict with the root-cause analysis investment principle (`/plan-rubric` etc.) and does not impede starting the fix after the analysis via Q3 completes.

> **Terminology note**: `bug-forensics-analyst` is a subagent under `.claude/agents/` (see the catalog in §"Knowledge base"; written in backticks). `advisor` refers to either the advisor tool built into Claude Code or to an external reviewer in a generic role; it has no dedicated file in `.claude/agents/`, so it is written without backticks.

#### Prohibition on presenting choices in filing decisions

- **MUST (no exceptions)**: the choices Claude Code presents to the user MUST NOT include "open as a separate issue". This applies to `AskUserQuestion` options, textual enumerations of choices ("(a)... (b)... (c)..." / "either of: ..." etc.), spoken three-way prompts, and any other form. Filing decisions are Claude Code's autonomous responsibility, and MUST NOT be substituted by presenting choices to the user.
- When Claude Code autonomously decides a filing is needed, follow `/git-create-issue` Step 1 (preview of 6 items [filing reason / overview / granularity / resolution confidence / proposed labels / milestone candidate] → wait for explicit permission). The preview is "presentation of the issue contents", not "presentation of choices" (the user judges only permit/decline against a single issue proposal).
- **Applicable scope**: "When the user spontaneously asks 'should this be a separate issue?'" is treated as a user instruction via Q2 (informed-consent gate), and the flow of `/triage-side-finding` Q2 (present What / Where / estimated size / Dependency risk before soliciting the instruction) is followed. This rule prohibits "Claude Code initiating a three-or-more-way choice"; it does not affect user-initiated questions.
- **Why**: regression prevention for the autonomous-induction failure of #1851 (where Claude Code presented "file as a separate issue" as a choice and steered the user); removing duplication with the `/git-create-issue` permission gate; eliminating the problem of users being asked to decide on the spot without basis.
- Related: `/triage-side-finding` Q4 (two-branch autonomous decision); `/triage-side-finding` Issue Creation Steps Step 1 "escalate" subsection (single-choice recommendation); `/git-create-issue` Step 1 (preview gate).

### What the user explicitly directs

- External review (GitHub PR review, etc.)
- git add / commit / push
- PR creation
- **Filing a new issue (`gh issue create`)** — Claude Code only presents the proposed issue contents (reason / overview / granularity / resolution confidence / proposed labels / milestone candidate) and waits for explicit user permission ("file it" / "OK" etc.). Repo-wide incidents such as CI failures, sanitizer detections, fuzz crashes etc. are reported in text only; do not file autonomously. The detailed procedure is in `/git-create-issue`. **When deciding whether to file, MUST NOT present "open as a separate issue" as a choice to the user** → see also §"Prohibition on presenting choices in filing decisions".
  - **Exception**: via the `preparing-for-release` skill (Release prep / Release / Cleanup issue), the user invocation of `/preparing-for-release <X.Y.Z>` doubles as filing permission, so this is outside the permission-required rule.

### PR review response

- **Committing/pushing is mandatory**: a fix that is not committed and pushed is not reflected in the PR. When review response is complete, if there are uncommitted changes, report them to the user without fail and encourage commit and push.
- **Resolve decisions are delegated to the reviewer**: CodeRabbit auto-verifies reply content and resolves the conversation itself, so if Claude Code resolves preemptively, the verification flow does not work. Human reviewer comments are the same — reply only and delegate the resolve decision.
- **Pre-merge unresolved check**: `git-close-pr` Step 6 automatically detects unresolved conversations and aborts the merge if any remain.

### Accumulating lessons from PR reviews

For comments received in PR review that are likely to recur in other PRs, append: to the corresponding `.claude/rules/<name>.md` if it fits a path-scope, or to `.claude/skills/pr-review-recurring-patterns/SKILL.md` if it is cross-cutting. Append autonomously and push it together with the review-response commit. Single local comments do not require an addition.

## Pre-completion checklist

The procedure to execute without fail before completing a task (documentation reflection / CHANGELOG / rules+skills update / full test / ASan+UBSan / TSan / libFuzzer / background-task check / label cleanup) is in `.claude/skills/pre-commit-checklist/SKILL.md` (or `/pre-commit-checklist`).

## Release Workflow

> **Note**: merging into `main` = mainline landing only. The release (tag push → GitHub Release) is a separate process.

Details on the release startup procedure, the tag-push-driven mechanism, and the milestone-close policy are in `.claude/skills/release-orchestrator/SKILL.md` (or `/release-orchestrator`). Once feature-complete, launch `/preparing-for-release <X.Y.Z>`.
