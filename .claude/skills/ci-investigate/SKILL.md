---
name: ci-investigate
description: Investigate CI failures on a PR in parallel with a re-run. Classifies failures as PR-caused / Pre-existing / Indeterminate, proposes next actions, and never concludes "flaky" without positive infrastructure evidence.
allowed-tools: Bash, Read, Grep
metadata:
  short-description: Investigate CI failures on a PR with parallel re-run
---

# CI Investigate

Investigate CI failures on a PR and classify each failure so that re-runs, fixes, and triage happen in parallel — not serialised behind guesswork.

## Why this skill exists

The anti-pattern this skill breaks:

```text
CI fails
  → "can't reproduce locally, probably flaky"
  → re-run (no investigation)
  → fails again
  → investigate → root cause found
```

The goal: re-run and investigation run **in parallel**. By the time the re-run finishes, the investigation is already done.

"Cannot reproduce locally" is a signal to investigate CI-specific conditions — not evidence of flakiness.

## Context

- Current branch: !`git branch --show-current`
- PR for this branch: !`gh pr view --json number,title,headRefName,headRefOid,state 2>/dev/null || echo "No PR found"`

## Inputs

User input: $ARGUMENTS

## Steps

### Step 1: Identify the PR

- If `$ARGUMENTS` is a number (e.g. `123` or `#123`), use that PR number.
- Otherwise, use the PR from the Context above.
- If no PR is found:
  > No PR found for the current branch. Provide a PR number (e.g. `/ci-investigate 123`) or push the branch and open a PR first.

  Stop.

### Step 2: List CI check states

```bash
gh pr checks <PR> --json name,status,conclusion,detailsUrl
```

- Extract all jobs where `conclusion == "failure"` → **failed job list**.
- Record jobs where `conclusion == "success"` for comparison.
- If **all** jobs are green:
  > CI is green — all checks passed on PR #<PR>. Nothing to investigate.

  Stop.

### Step 3: Trigger re-run in parallel with investigation

Identify the failing workflow runs, scoped to the PR's head commit (use `headRefOid` from the Context preflight):

```bash
gh run list --branch <headRefName> --limit 20 \
  --json databaseId,headSha,name,status,conclusion,workflowName \
  | jq --arg sha "<headRefOid>" '[.[] | select(.headSha == $sha)]'
```

For each run in the result where `conclusion == "failure"`, trigger a re-run immediately:

```bash
gh run rerun <runId> --failed
```

After triggering:

> Re-run triggered for run #<runId> (`<workflowName>`). Continuing investigation in parallel — **do not wait** for the re-run to complete.

Proceed directly to Step 4.

### Step 4: Fetch PR diff (changed files)

```bash
gh pr diff <PR> --name-only
```

Store this list. It is used in Step 5-3 to determine whether an error file is in scope.

### Step 5: Analyse failure logs

Process each failed job **in parallel**. For each job:

#### 5-1: Get failure log

Use the run ID from Step 3. To avoid context pressure from very long output, redirect to a temporary file and use `Read` or `Grep` to scan the relevant section:

```bash
gh run view <runId> --log-failed > /tmp/ci-investigate-<runId>.log
```

Then scan with `Grep` for error keywords, or `Read` the file in targeted ranges.

If the run ID is not known yet, derive it from `detailsUrl` in Step 2 (extract the numeric run ID with `grep -oE '/runs/([0-9]+)' | grep -oE '[0-9]+'`), or re-query with headSha filtering:

```bash
gh run list --branch <headRefName> --limit 20 \
  --json databaseId,headSha,name,conclusion,workflowName \
  | jq --arg sha "<headRefOid>" '[.[] | select(.headSha == $sha)]'
```

#### 5-2: Classify the failure type

| Type | Detection pattern |
|------|-------------------|
| **Build error** | `error:` or `fatal error:` with file/line reference |
| **Test failure** | `FAILED`, `ASSERTION FAILED`, `Expected ... but got ...`, GoogleTest `[ FAILED ]` |
| **Lint error** | `clang-tidy:`, `cppcheck:`, warning flagged as error |
| **Sanitizer error** | `AddressSanitizer:`, `UndefinedBehaviorSanitizer:`, `ThreadSanitizer:` |
| **Link error** | `undefined reference to`, `linker command failed` |
| **Ry self-test failure** | `FAIL` in `.test.ry` output, `describe` / `it` blocks |

#### 5-3: Determine PR-caused / Pre-existing / Indeterminate

- Extract the error file path(s) and line number(s) from the log.
- Compare against the changed file list from Step 4:
  - **PR-caused** — error location is in a file modified by this PR.
  - **Pre-existing** — error location is in a file **not** touched by this PR.
  - **Indeterminate** — no file location can be extracted (CI config error, infrastructure issue, etc.).

> **Forbidden conclusion**: "Cannot reproduce locally → flaky." That fact is a trigger for Step 5-4, not evidence of flakiness.

#### 5-4: Local reproduction (when the failure cannot be reproduced with a default build)

Map the CI job name to its local reproduction command and run it:

| CI job | Local reproduction command |
|--------|---------------------------|
| `test (asan)` / `sanitizer` | `cmake --preset asan && cmake --build build-asan && ASAN_OPTIONS=detect_container_overflow=0 UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 ./build-asan/ry_tests && ASAN_OPTIONS=detect_container_overflow=0 UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 ./build-asan/ry test -p` |
| `test (tsan)` / `tsan` | `cmake --preset tsan && cmake --build build-tsan && TSAN_OPTIONS=halt_on_error=1:second_deadlock_stack=1 ./build-tsan/ry_tests` |
| `clang-tidy` | `find src -name '*.cpp' \| xargs clang-tidy -p build --quiet` |
| `cppcheck` / `lint` | `cppcheck --enable=warning,performance,portability --std=c++17 --suppressions-list=.cppcheck-suppressions --inline-suppr -i build -i build-asan -i build-tsan -j "$(nproc)" --quiet src/ include/` |
| `scan-build` (fast, PR) | `scan-build --use-analyzer=/usr/local/llvm/bin/clang --use-cc=/usr/local/llvm/bin/clang --use-c++=/usr/local/llvm/bin/clang++ cmake --build build --target ry --parallel` |
| `scan-build` (full, push to main) | `scan-build --use-analyzer=/usr/local/llvm/bin/clang --use-cc=/usr/local/llvm/bin/clang --use-c++=/usr/local/llvm/bin/clang++ cmake --build build --parallel` |

**Known CI-only failure patterns (do not misclassify as PR-caused):**

- **TSan `ry test -p` aborts on Linux** with `LargeMmapAllocator CHECK` — upstream TSan runtime bug, not a regression. See `.claude/skills/tsan-known-issues/SKILL.md`. Only `ry_tests` is required; `ry test -p` is warn-only.
- **TSan gate** — `ry_tests` is required, `ry test -p` is warn-only. See `.claude/skills/tsan-known-issues/SKILL.md`.
- **UBSan vptr/function violations** — built with `-fno-sanitize=vptr,function`; these are LLVM false positives. See `.claude/rules/ci-workflows.md`.

**Environment-caused (flaky) — positive evidence required:**

Classify as environment-caused **only** when the log contains one of:

- `OOM Killed`
- `No space left on device`
- `Connection timed out`
- `curl: (6) Could not resolve host`
- `Error: The process '/usr/bin/apt-get' failed`

If **none** of the above appear and local reproduction also fails, leave the classification as **Indeterminate** and present findings to the user. Do **not** conclude "flaky" without positive evidence.

### Step 6: Report and propose next actions

Report in the following format, replacing placeholders with actual values:

---

**CI Investigation Report: PR #\<PR\>**

> Re-run triggered for run #\<runId\>. Investigating in parallel.

**Summary**

| Job | Status | Type | Cause | Local reproduction |
|-----|--------|------|-------|--------------------|
| build (linux) | ❌ Failed | Build error | PR-caused | — |
| test (asan) | ❌ Failed | Sanitizer error | PR-caused | `cmake --preset asan && ...` |
| clang-tidy | ❌ Failed | Lint error | Pre-existing | `find src ... \| xargs clang-tidy` |

---

**[N] \<job-name\> — \<Type\> — \<Cause\>**

Error:
```text
<relevant log excerpt — error message, file, line number>
```

Changed files involved: `<file>` (or "none — Pre-existing")

Local reproduction:
```bash
<exact command from the table above>
```

Proposed action: \<1–2 sentences describing what to do\>

---

**Next steps**

1. **PR-caused failures** — Fix using the investigation above. Push and let the re-run or a new run verify.
2. **Pre-existing failures** — Triage via the `/triage-side-finding` skill. **Hard-to-reproduce CI failures** (sanitizer / fuzz / TSan race that does not reliably reproduce locally) hit Q1 = Yes and should be fixed in the current PR to capture the reproduction window. Locally-reproducible pre-existing bugs follow Q3-Q4 (either fold into this PR via Q4(a) or file a separate issue via Q4(b) using the current PR's milestone) to decouple from this PR's CI.
3. **Flaky / environment-caused (positive evidence confirmed)** — Check the re-run result triggered in Step 3.
4. **Indeterminate** — Review the log excerpts above and decide with the user whether to investigate further or file an issue.

---

## Re-run policy

- Re-run is triggered in Step 3 **before** investigation begins, to eliminate flakiness as a confounding variable.
- Re-run is **not** a substitute for investigation. Proceed to Step 4 immediately after triggering.
- If a failure is **PR-caused**, fixing the code is required regardless of whether the re-run passes.

> **Important:** This skill does NOT commit, push, or fix code. It investigates, reports, and triggers re-runs only. For Pre-existing issues, follow the `/triage-side-finding` skill (which may resolve to immediate fix via Q1 for hard-to-reproduce CI failures, not just to filing a separate issue).

---

*Canonical source for sanitizer commands and environment flags: AGENTS.md "ASan + UBSan" / "TSan" sections and `/pre-commit-checklist` §3.5 (Sanitizer Verification).*
