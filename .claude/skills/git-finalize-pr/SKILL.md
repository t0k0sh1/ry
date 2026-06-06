---
name: git-finalize-pr
description: PR finalization in one pass — addresses review comments, follows up unresolved threads, verifies CI, runs pre-commit checklist, pushes, and merges. Stops at the first blocker; never auto-loops fix→push→CI-wait. Use when the user wants to finalize a PR, "PR を仕上げる", "マージまで一気に", "指摘対応してマージ", "レビュー対応からマージまで", "finalize PR <number>".
allowed-tools: Bash(gh:*), Bash(git:*), Read, Edit
metadata:
  short-description: Review → CI → push → merge in one pass
---

# Git Finalize PR

Take a pull request end-to-end: address review comments, follow up unresolved threads, verify CI, run the pre-commit checklist, push, and merge.

## One-pass policy

This skill runs straight through. On any blocker it reports and stops — fix → push → CI wait → re-fix loops are never started automatically. Invoking this skill is the merge consent; no additional "merge しますか？" prompt is shown.

Priority order: (1) CI failures are caught first (Step 2) and stop the flow immediately so investigation takes precedence; (2) review comments are addressed next (Step 3); (3) CI completion is verified strictly at the merge gate (Step 6). Step 1 is a structural-only pre-check that stops on conflicts; transient states like `BLOCKED` (CI pending) warn and proceed so Step 2 and Step 3 run in parallel with CI. When Step 3 produces review-fix commits, Step 5 pushes them and triggers fresh CI runs, so Step 6 will typically stop on `mergeStateStatus ∉ {CLEAN, HAS_HOOKS}` until those checks complete. Rerun once CI is green.

## Repository

- owner: `t0k0sh1`
- repo: `ry`

## Inputs

User input: $ARGUMENTS (optional PR number)

## Context

- Current branch: !`git branch --show-current`
- PR info: !`gh pr view --json number,title,state,mergeable,mergeStateStatus,headRefName,body 2>/dev/null || echo "No PR found"`

If no PR number is supplied, use the PR from Context. If none exists, stop:

> No PR found. Run this skill on a branch with an associated PR, or pass a PR number.

## Steps

### Step 1: Pre-check

From Context (or `gh pr view <PR> --json state,mergeable,mergeStateStatus,headRefName,body,title`):

- `state != OPEN` → stop:
  > PR #\<PR\> is not open (state: `<state>`). Aborting.
- `mergeable == CONFLICTING` **or** `mergeStateStatus == DIRTY` → stop:
  > PR #\<PR\> has merge conflicts (mergeable: `<mergeable>`, mergeStateStatus: `<mergeStateStatus>`). Resolve conflicts and rerun.
- `mergeStateStatus` ∉ {`CLEAN`, `HAS_HOOKS`} (e.g. `BLOCKED`, `BEHIND`, `UNSTABLE`, `UNKNOWN`, `DRAFT`) → warn and proceed:
  > PR #\<PR\>: mergeStateStatus=`<mergeStateStatus>` (transient). Proceeding to Step 2 (CI failure check) and Step 3 (review handling); the strict merge gate is re-checked at Step 6.

### Step 2: Check CI for failures

```bash
gh pr checks <PR> --json name,bucket,state,link
```

- Any `bucket == "fail"` → list each failed job (`name` / `state` / `link`) and stop:
  > CI failure(s) detected on PR #\<PR\> (above). Fix PR-caused failures and rerun. For pre-existing failures, triage via `/triage-side-finding` first.
- Otherwise (all `pass` and/or `pending`) → proceed to Step 3. Pending checks do not block here; the strict completion gate is enforced at Step 6.

Do not auto-rerun, auto-fix, or loop.

### Step 3: Address reviews and unresolved threads

Fetch in one pass:

```bash
gh api --paginate "repos/t0k0sh1/ry/pulls/<PR>/comments"   # inline review comments
gh api --paginate "repos/t0k0sh1/ry/pulls/<PR>/reviews"    # review summaries (incl. CodeRabbit nitpicks)
gh api graphql -f query='{ repository(owner: "t0k0sh1", name: "ry") { pullRequest(number: <PR>) { reviewThreads(first: 100) { nodes { isResolved comments(last: 50) { nodes { databaseId author { login } body path originalLine } } } } } } viewer { login } }'
```

For each unique reviewer comment, and each unresolved thread whose last commenter is **not** the viewer, classify into two buckets:

- **Auto-apply** — clear typo / lint suggestion / tracking-issue reference (`tracked in #N`) / nit / optional / not-blocking comments. Apply via `Edit` if a code change is required, otherwise reply only. Always reply via:
  ```bash
  gh api repos/t0k0sh1/ry/pulls/<PR>/comments/<comment_id>/replies -f body='<reply>'
  ```
- **Needs-confirmation** — design judgment / large rewrite / ambiguous direction. Present the comment to the user and wait for an approved approach before applying.

Do **not** resolve threads yourself — leave that to the reviewer (CodeRabbit auto-verifies on reply; humans verify manually).

CodeRabbit `<summary>🧹 Nitpick comments</summary>` blocks inside review summaries are treated as ordinary comments: extract each suggestion from the body and route through the same triage.

If there are no review comments and no unresolved threads, proceed to Step 4 without action.

### Step 4: Pre-commit checklist

Invoke `/pre-commit-checklist`. If it reports outstanding items, stop:

> Pre-commit checklist reported outstanding items (above). Address them, then rerun.

### Step 5: Push

Invoke `/git-push`. It commits any working-tree changes left by Step 3, rebases onto `origin/main`, and pushes with `--force-with-lease`. On any failure (rebase conflict, lease rejection, etc.) it stops with its own report — surface that message and stop:

> Push failed. Resolve the issue as reported above, then rerun.

### Step 6: Merge

Re-verify status (push triggered fresh CI):

```bash
gh pr view <PR> --json state,mergeable,mergeStateStatus
```

- `mergeable != MERGEABLE` **or** `mergeStateStatus` ∉ {`CLEAN`, `HAS_HOOKS`} → stop:
  > PR #\<PR\> is not mergeable (mergeable: `<mergeable>`, mergeStateStatus: `<mergeStateStatus>`). Wait for CI to settle and rerun.

Re-fetch unresolved threads (new comments may have arrived during push):

```bash
gh api graphql -f query='{ repository(owner: "t0k0sh1", name: "ry") { pullRequest(number: <PR>) { reviewThreads(first: 100) { nodes { isResolved comments(first: 1) { nodes { path originalLine body author { login } } } } } } } }'
```

- Any `isResolved == false` → list each (path / line / first body line) and stop:
  > Found N unresolved review thread(s) (above). Address and wait for the reviewer to resolve, then rerun.

Merge:

```bash
gh pr merge <PR> --merge --delete-branch
```

### Step 7: Linked issue cleanup

Identify the linked issue number `<n>` from PR body (`Closes #<n>` / `Fixes #<n>` / `Refs #<n>`) or branch name. If none can be determined, skip and note it in the report.

```bash
gh issue edit <n> --remove-label wip
gh issue view <n> --json labels --jq '[.labels[].name]'
```

> **Critical**: use `--remove-label`. **Never** `--label` alone — `gh issue edit <n> --label wip` would replace all labels with the single value, destroying `bug`, `enhancement`, milestone-shadow labels, etc. Verify that `wip` is gone and every pre-existing label is preserved.

GitHub's `Closes #xx` keyword auto-closes the issue when the PR merges into the default branch; manual `gh issue close` is not needed.

Execute autonomously immediately after merge — do not wait for user confirmation.

### Step 8: Report

```text
PR #<PR> "<title>" merged.
wip label removed from issue #<n>.
Branch <headRefName> deleted.
```

If the linked issue could not be determined, note it here.
