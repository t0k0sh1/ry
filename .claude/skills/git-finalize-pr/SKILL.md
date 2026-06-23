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
- Do not invoke or suggest `/git-push` from within this skill — Steps 5-7 are inlined; delegation is prohibited per #2176.

> **Sync with `/git-create-pr`**: Steps 5-7 mirror the *actions* of `.claude/skills/git-create-pr/SKILL.md` Steps 2-4 (Commit / Rebase / Push). Intentional differences: no branch-ensure, stages only Step 3's touched files, fixed commit subject. Keep rebase/push mechanics in sync at the action level.

## One-pass policy

Runs straight through. On any blocker: report and stop — no fix→push→CI loops. Invoking is merge consent; no merge-confirmation prompt is shown.

Priority order: (1) CI failures caught first (Step 2) stop the flow; (2) review comments addressed next (Step 3); (3) CI completion verified at merge gate (Step 8). `BLOCKED` (CI pending) at Step 1 warns and proceeds so Steps 2-3 run in parallel with CI. Step 3 fix commits are pushed inline (Steps 5-7) before the merge gate; force-push re-triggers CI, so Step 8 then stops on `BLOCKED`/`UNSTABLE`.

## Repository

- owner: `t0k0sh1`
- repo: `ry`

## Inputs

User input: $ARGUMENTS (optional PR number)

## Context

- Current branch: !`git branch --show-current`
- PR info: !`gh pr view --json number,title,state,mergeable,mergeStateStatus,headRefName,body 2>/dev/null || echo "No PR found"`

If no PR number supplied, use PR from Context. If none: stop with "No PR found. Run this skill on a branch with an associated PR, or pass a PR number."

## Behavior Contract

| State | Action |
|---|---|
| Step 3 reply-only; working tree clean; nothing ahead of `@{u}` | Skip Step 5-7 → Step 8 |
| Step 3 applied Edits | Step 5 → Step 6 → Step 7 → Step 8 |
| Step 3 reply-only but pre-existing commits ahead of `@{u}` | Skip Step 5 → Step 6 → Step 7 → Step 8 |
| After Step 7 push: `mergeStateStatus` ∈ {`BLOCKED`, `UNSTABLE`, `UNKNOWN`} | Step 8 STOPs |
| Step 8: `MERGEABLE` + `CLEAN`/`HAS_HOOKS` + no unresolved threads | `gh pr merge --merge --delete-branch` |

Rule: **commit only when Step 3 edited files; rebase + push only when something needs pushing; merge gate always re-checks.**

## Steps

### Step 1: Pre-check

From Context (or `gh pr view <PR> --json state,mergeable,mergeStateStatus,headRefName,body,title`):

- `state != OPEN` → stop: "PR #\<PR\> is not open (state: `<state>`). Aborting."
- `mergeable == CONFLICTING` **or** `mergeStateStatus == DIRTY` → stop: "PR #\<PR\> has merge conflicts. Resolve conflicts and rerun."
- `mergeStateStatus` ∉ {`CLEAN`, `HAS_HOOKS`} → warn and proceed: "PR #\<PR\>: mergeStateStatus=`<mergeStateStatus>` (transient). Proceeding to Steps 2-3; strict merge gate re-checked at Step 8."

### Step 2: Check CI for failures

```bash
gh pr checks <PR> --json name,bucket,state,link
```

- Any `bucket == "fail"` → list each failed job and stop: "CI failure(s) detected on PR #\<PR\>. Fix PR-caused failures and rerun. For pre-existing failures, triage via `/triage-side-finding` first."
- Otherwise (all `pass` / `pending`) → proceed. Do not auto-rerun, auto-fix, or loop.

### Step 3: Address reviews and unresolved threads

```bash
gh api --paginate "repos/t0k0sh1/ry/pulls/<PR>/comments"
gh api --paginate "repos/t0k0sh1/ry/pulls/<PR>/reviews"
gh api graphql -f query='{ repository(owner: "t0k0sh1", name: "ry") { pullRequest(number: <PR>) { reviewThreads(first: 100) { nodes { isResolved comments(last: 50) { nodes { databaseId author { login } body path originalLine } } } } } } viewer { login } }'
```

For each unique reviewer comment and each unresolved thread whose last commenter is **not** the viewer, classify:

- **Auto-apply** — typo / lint suggestion / tracking-issue reference / nit / optional / not-blocking. Apply via `Edit` if code change needed; always reply via:
  ```bash
  gh api repos/t0k0sh1/ry/pulls/<PR>/comments/<comment_id>/replies -f body='<reply>'
  ```
- **Needs-confirmation** — design judgment / large rewrite / ambiguous direction. Present to user and wait.

Do **not** resolve threads yourself. CodeRabbit `<summary>🧹 Nitpick comments</summary>` blocks are treated as ordinary comments.

Maintain an explicit list of files touched via `Edit`. Step 5 stages that exact list (not `git status` output) to avoid sweeping pre-existing dirty files.

### Step 4: Pre-commit checklist

Invoke `/pre-commit-checklist`. Outstanding items → stop.

### Step 5: Commit

- **Working-tree guard (always runs before the skip):** Changes outside Step 3's touched-file list → stop: "Unexpected working-tree changes found before commit. Address and rerun." Empty list means any dirty state trips this guard.
- **Skip remainder when** Step 3 applied no `Edit` calls.
- Stage only Step 3's touched files:
  ```bash
  git add <file1> <file2> ...   # never git add -A or git add .
  ```
- Commit:
  ```bash
  git commit -m "fix: address review comment from PR #<PR>"
  ```

### Step 6: Rebase onto `origin/main`

- **Skip when** working tree clean **and** upstream set **and** no commits ahead of `@{u}`.
- `git fetch origin`
- `git rebase origin/main`
- **Do not re-run `git fetch` between rebase and push** — weakens the `--force-with-lease` guard.
- On conflict: `git diff --name-only --diff-filter=U` → `Read` + `Edit` to resolve → `git add <file>` → `git rebase --continue`. If unresolvable: STOP and report (do **not** auto-`git rebase --abort`).

### Step 7: Push

- **Skip when** working tree clean **and** upstream set **and** no commits ahead of `@{u}`.
- `git push --force-with-lease`
- Force required because rebase rewrites SHAs. `--force-with-lease` rejects if `origin/<branch>` advanced since last fetch.

After push, CI re-runs. If Step 8 sees `BLOCKED`/`UNSTABLE`/`UNKNOWN`, it STOPs. Do not wait for CI — one-pass policy.

### Step 8: Merge

```bash
gh pr view <PR> --json state,mergeable,mergeStateStatus
```

- `mergeable != MERGEABLE` **or** `mergeStateStatus` ∉ {`CLEAN`, `HAS_HOOKS`} → stop: "PR #\<PR\> is not mergeable. Wait for CI to settle and rerun."

Re-fetch unresolved threads:

```bash
gh api graphql -f query='{ repository(owner: "t0k0sh1", name: "ry") { pullRequest(number: <PR>) { reviewThreads(first: 100) { nodes { isResolved comments(first: 1) { nodes { path originalLine body author { login } } } } } } } }'
```

- Any `isResolved == false` → list each and stop: "Found N unresolved review thread(s). Address and wait for reviewer to resolve, then rerun."

```bash
gh pr merge <PR> --merge --delete-branch
```

### Step 9: Linked issue cleanup

Identify `<n>` from PR body (`Closes #<n>` / `Fixes #<n>` / `Refs #<n>`) or branch name. If none, skip and note in report.

```bash
gh issue edit <n> --remove-label wip
gh issue view <n> --json labels --jq '[.labels[].name]'
```

> **Critical**: use `--remove-label`. **Never** `--label` alone — `gh issue edit <n> --label wip` replaces all labels with the single value, destroying `bug`, `enhancement`, milestone-shadow labels, etc. Verify `wip` is gone and all pre-existing labels are preserved.

GitHub's `Closes #xx` keyword auto-closes the issue; manual `gh issue close` is not needed. Execute autonomously immediately after merge.

### Step 10: Report

```text
PR #<PR> "<title>" merged.
wip label removed from issue #<n>.
Branch <headRefName> deleted.
```

If the linked issue could not be determined, note it here.
