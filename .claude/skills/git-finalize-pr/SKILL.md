---
name: git-finalize-pr
description: User-invoked slash command that finalizes an already-pushed PR by addressing review comments, verifying CI, running pre-commit checks, merging, and cleaning up. Never invoke autonomously, from another skill, or merely because a PR is ready.
allowed-tools: Bash(gh:*), Bash(git status:*), Bash(git diff:*), Bash(git add:*), Bash(git commit:*), Bash(git fetch:*), Bash(git rebase:*), Bash(git push:*), Bash(git log:*), Bash(git branch:*), Read, Edit
metadata:
  short-description: Review → CI → merge in one pass
---

# Git Finalize PR

## Invocation Gate

- Run only when the user directly invokes `/git-finalize-pr`.
- Never invoke this skill autonomously or from another skill.
- Never propose this skill, present it as an option, include it in a plan, or list it as a next step.
- Do not invoke or suggest `/git-push` from within this skill — the Step 5-7 actions are inlined; delegation is prohibited per #2176.

> **Sync with `/git-create-pr`**: Steps 5-7 below mirror the *actions* of `.claude/skills/git-create-pr/SKILL.md` Steps 2-4 (Commit / Rebase / Push). Intentional differences: finalize has no branch-ensure (it always runs on a published PR branch), stages only the files touched by Step 3's auto-apply (specific `git add`, not all changes), and uses a fixed commit subject. Keep the rebase/push mechanics in sync at the action level — not byte-for-byte.

Finalize an already-pushed pull request: address review comments, follow up unresolved threads, verify CI, run the pre-commit checklist, publish any auto-applied review fixes inline, and merge.

## One-pass policy

This skill runs straight through. On any blocker it reports and stops — fix → push → CI wait → re-fix loops are never started automatically. Invoking this skill is the merge consent; no additional "merge しますか？" prompt is shown.

Priority order: (1) CI failures are caught first (Step 2) and stop the flow immediately so investigation takes precedence; (2) review comments are addressed next (Step 3); (3) CI completion is verified strictly at the merge gate (Step 8). Step 1 is a structural-only pre-check that stops on conflicts; transient states like `BLOCKED` (CI pending) warn and proceed so Step 2 and Step 3 run in parallel with CI. When Step 3 applies review fixes, they are committed, rebased, and pushed inline (Steps 5-7) before the merge gate; the force-push re-triggers CI, so Step 8 then stops on `BLOCKED`/`UNSTABLE` and returns control until checks settle.

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

## Behavior Contract

| State | Action |
|---|---|
| Step 3 reply-only; working tree clean; nothing ahead of `@{u}` | Skip Step 5-7 → Step 8 |
| Step 3 applied Edits (review fixes carried over) | Step 5 (commit touched files) → Step 6 → Step 7 → Step 8 |
| Step 3 reply-only but pre-existing commits ahead of `@{u}` | Skip Step 5 → Step 6 → Step 7 → Step 8 |
| After Step 7 push: `mergeStateStatus` ∈ {`BLOCKED`, `UNSTABLE`, `UNKNOWN`} | Step 8 STOPs and returns control to the user |
| Step 8: `MERGEABLE` + `CLEAN`/`HAS_HOOKS` + no unresolved threads | `gh pr merge --merge --delete-branch` |

Rule: **commit only when Step 3 edited files; rebase + push only when something needs pushing; merge gate always re-checks.**

## Steps

### Step 1: Pre-check

From Context (or `gh pr view <PR> --json state,mergeable,mergeStateStatus,headRefName,body,title`):

- `state != OPEN` → stop:
  > PR #\<PR\> is not open (state: `<state>`). Aborting.
- `mergeable == CONFLICTING` **or** `mergeStateStatus == DIRTY` → stop:
  > PR #\<PR\> has merge conflicts (mergeable: `<mergeable>`, mergeStateStatus: `<mergeStateStatus>`). Resolve conflicts and rerun.
- `mergeStateStatus` ∉ {`CLEAN`, `HAS_HOOKS`} (e.g. `BLOCKED`, `BEHIND`, `UNSTABLE`, `UNKNOWN`, `DRAFT`) → warn and proceed:
  > PR #\<PR\>: mergeStateStatus=`<mergeStateStatus>` (transient). Proceeding to Step 2 (CI failure check) and Step 3 (review handling); the strict merge gate is re-checked at Step 8.

### Step 2: Check CI for failures

```bash
gh pr checks <PR> --json name,bucket,state,link
```

- Any `bucket == "fail"` → list each failed job (`name` / `state` / `link`) and stop:
  > CI failure(s) detected on PR #\<PR\> (above). Fix PR-caused failures and rerun. For pre-existing failures, triage via `/triage-side-finding` first.
- Otherwise (all `pass` and/or `pending`) → proceed to Step 3. Pending checks do not block here; the strict completion gate is enforced at Step 8.

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

While auto-applying, maintain an explicit list of files touched via `Edit`. Step 5 stages that exact list (not `git status` output) so any pre-existing dirty files outside Step 3's scope are not silently swept into the review-fix commit.

### Step 4: Pre-commit checklist

Invoke `/pre-commit-checklist`. If it reports outstanding items, stop:

> Pre-commit checklist reported outstanding items (above). Address them, then rerun.

### Step 5: Commit

- **Working-tree guard (always runs, before the skip):** If the working tree contains changes outside Step 3's touched-file list, stop:
  > Unexpected working-tree changes found before commit (files outside Step 3's auto-apply list). Address the unexpected state and rerun.
  When Step 3 touched no files, the list is empty and any dirty state trips this guard.
- **Skip the remainder when** Step 3 applied no `Edit` calls (no review-fix changes carried over from Step 3).
- Stage only the explicit list of files Step 3 touched via `Edit`:
  ```bash
  git add <file1> <file2> ...   # never git add -A or git add .
  ```
- Create a single commit with a fixed subject:
  ```bash
  git commit -m "fix: address review comment from PR #<PR>"
  ```

### Step 6: Rebase onto `origin/main`

- **Skip when** working tree clean **and** upstream set **and** no commits ahead of `@{u}`.
- `git fetch origin`
- `git rebase origin/main`
- **Do not re-run `git fetch` between rebase and push** — it weakens the `--force-with-lease` guard in Step 7.
- On conflict:
  - `git diff --name-only --diff-filter=U` to list conflicting files
  - `Read` + `Edit` to resolve
  - `git add <file>` per resolved file → `git rebase --continue`
  - If you cannot resolve: STOP and report to the user (do **not** auto-`git rebase --abort`)

### Step 7: Push

- **Skip when** working tree clean **and** upstream set **and** no commits ahead of `@{u}`.
- `git push --force-with-lease`
- Force push is required because rebase rewrites SHAs. `--force-with-lease` (no argument) rejects the push if `origin/<branch>` advanced since the last `git fetch`.

After pushing, CI re-runs. Step 8 re-checks `mergeStateStatus`; if it is `BLOCKED`/`UNSTABLE`/`UNKNOWN`, Step 8 STOPs and returns control to the user. Do not wait for CI to complete — one-pass policy.

### Step 8: Merge

Re-verify status:

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

### Step 9: Linked issue cleanup

Identify the linked issue number `<n>` from PR body (`Closes #<n>` / `Fixes #<n>` / `Refs #<n>`) or branch name. If none can be determined, skip and note it in the report.

```bash
gh issue edit <n> --remove-label wip
gh issue view <n> --json labels --jq '[.labels[].name]'
```

> **Critical**: use `--remove-label`. **Never** `--label` alone — `gh issue edit <n> --label wip` would replace all labels with the single value, destroying `bug`, `enhancement`, milestone-shadow labels, etc. Verify that `wip` is gone and every pre-existing label is preserved.

GitHub's `Closes #xx` keyword auto-closes the issue when the PR merges into the default branch; manual `gh issue close` is not needed.

Execute autonomously immediately after merge — do not wait for user confirmation.

### Step 10: Report

```text
PR #<PR> "<title>" merged.
wip label removed from issue #<n>.
Branch <headRefName> deleted.
```

If the linked issue could not be determined, note it here.
