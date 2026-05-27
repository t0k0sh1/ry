---
name: git-close-pr
description: One-shot PR finalization — fetches review comments, follows up unresolved threads, checks CI, runs pre-commit checklist, pushes, and merges. Delegates to /git-fix-pr-reviews → /git-followup-unresolved-reviews → CI check → /pre-commit-checklist → /git-push → /git-merge-pr in that order. Stops at the first blocker; never auto-loops fix→push→CI-wait. Use when the user wants to finalize a PR, "PR を仕上げる", "マージまで一気に", "指摘対応してマージ", "レビュー対応からマージまで", "close PR <number>".
allowed-tools: Bash(git branch:*), Bash(gh pr view:*), Bash(gh pr checks:*)
metadata:
  short-description: Review → CI → push → merge in one pass
---

# Git Close PR

Finalize a pull request end-to-end: address review comments, follow up unresolved threads, verify CI, run the pre-commit checklist, push, and merge.

## One-pass policy

This skill runs straight through. On any blocker it reports and stops — fix → push → CI wait → re-fix loops are never started automatically. Invoking this skill is the merge consent; no additional "merge しますか？" prompt is shown. Sub-skills retain their own safety checks (unresolved-thread detection, mergeable state, force-with-lease push, `wip` label removal); this skill does not duplicate them.

When Step 2 produces review-fix commits, Step 6 pushes them and triggers fresh CI runs, so Step 7 will typically stop on `mergeable != MERGEABLE` until those checks complete. Rerun this skill once CI is green.

## Context

- Current branch: !`git branch --show-current`
- PR info: !`gh pr view --json number,title,state,mergeable,mergeStateStatus 2>/dev/null || echo "No PR found"`

## Inputs

User input: $ARGUMENTS (optional PR number)

If no PR number is supplied, use the PR from Context. If no PR exists, stop:

> No PR found. Run this skill on a branch with an associated PR, or specify a PR number.

## Steps

### Step 1: Pre-check

Verify the PR is mergeable before running any fix/push/CI work.

- If `state != OPEN`, stop:
  > PR #\<PR\> is not open (state: `<state>`). Aborting.
- If `mergeable != MERGEABLE`, stop:
  > PR #\<PR\> is not mergeable (mergeStateStatus: `<mergeStateStatus>`). Resolve conflicts or required checks, then rerun.

### Step 2: Address review comments

Invoke `/git-fix-pr-reviews <PR>`. Needs-confirmation items prompt the user inside that skill — no additional confirmation layer here. If it surfaces an error, stop and surface the message.

### Step 3: Follow up unresolved threads

Invoke `/git-followup-unresolved-reviews <PR>`. Category A/B threads are presented with an approval table inside that skill.

### Step 4: Check CI

```bash
gh pr checks <PR> --json name,status,conclusion
```

- All green → proceed to Step 5.
- Any failure → invoke `/ci-investigate <PR>` to classify each failure, then stop:
  > CI failure(s) detected on PR #\<PR\>. Investigation report above. Fix PR-caused failures and rerun `/git-close-pr`. Triage pre-existing failures via `/triage-side-finding` before rerunning.

Do not auto-retry, auto-fix, or rerun pushes after investigation.

### Step 5: Pre-commit checklist

Invoke `/pre-commit-checklist`. If it reports outstanding items, stop:

> Pre-commit checklist reported outstanding items (above). Address them, then rerun `/git-close-pr`.

### Step 6: Push

Invoke `/git-push`. It commits any working-tree changes left by Step 2, rebases onto `origin/main`, and pushes with `--force-with-lease`. On rebase conflict or any other failure, `/git-push` stops with its own report — surface that message and stop:

> Push failed. Resolve the issue as reported above, then rerun `/git-close-pr`.

### Step 7: Merge

Invoke `/git-merge-pr <PR>`. It performs its own mergeable / unresolved-thread / `wip`-label checks. If it aborts for any reason, surface its message and stop.

### Step 8: Report

After successful merge, display:

```text
PR #<PR> "<title>" merged.
wip label removed from issue #<n>.
Branch <branch> deleted.
```

If the linked issue number cannot be determined, note it here.
