---
name: git-finalize-pr
description: User-invoked slash command that finalizes an already-pushed PR by addressing reviews, checking CI, merging, and cleaning up.
allowed-tools: Bash(gh:*), Bash(git status:*), Bash(git diff:*), Bash(git add:*), Bash(git commit:*), Bash(git fetch:*), Bash(git rebase:*), Bash(git push:*), Bash(git log:*), Bash(git branch:*), Read, Edit
metadata:
  short-description: Review -> CI -> merge
---

# Git Finalize PR

Run only on direct `/git-finalize-pr` invocation. Invocation is merge consent. Run once through; on any blocker, report and stop.

## Inputs

`$ARGUMENTS` may contain a PR number. Otherwise use the PR for the current branch:

```bash
gh pr view --json number,title,state,mergeable,mergeStateStatus,headRefName,body
```

If no PR is found, stop.

## Steps

1. Pre-check PR:
   - Stop unless `state == OPEN`.
   - Stop on `mergeable == CONFLICTING` or `mergeStateStatus == DIRTY`.
   - Warn, but continue to review handling, for transient states such as `BLOCKED`.

2. Check CI:
   ```bash
   gh pr checks <PR> --json name,bucket,state,link
   ```
   Any `bucket == "fail"` stops the flow. Pending checks may proceed to review handling; merge gate re-checks later.

3. Address review comments and unresolved threads:
   - Fetch PR comments, reviews, and review threads with `gh api`.
   - For each unique reviewer comment or unresolved thread whose last commenter is not the viewer:
     - Auto-apply typos, lint suggestions, tracking references, nits, optional comments, and not-blocking comments; reply to the comment.
     - Ask the user for design judgment, large rewrites, or ambiguous directions.
   - Do not resolve threads yourself.
   - Track every file edited in this step; Step 5 stages only those files.

4. Run applicable checks from `/pre-commit-checklist`. Stop on outstanding failures.

5. Commit only Step 3 edits:
   - Stop if the working tree has changes outside the Step 3 touched-file list.
   - Skip when Step 3 made no edits.
   - `git add <touched files>`; never `git add -A` / `git add .`.
   - `git commit -m "fix: address review comment from PR #<PR>"`.

6. Rebase and push only when there is something to push:
   ```bash
   git fetch origin
   git rebase origin/main
   git push --force-with-lease
   ```
   Do not fetch again between rebase and push. On conflicts, resolve with `Read` / `Edit`, `git add`, and `git rebase --continue`; if unresolvable, stop.

7. Merge gate:
   - Re-check `gh pr view <PR> --json state,mergeable,mergeStateStatus`.
   - Stop unless `mergeable == MERGEABLE` and `mergeStateStatus` is `CLEAN` or `HAS_HOOKS`.
   - Re-fetch review threads; stop if any remain unresolved.
   - Merge:
     ```bash
     gh pr merge <PR> --merge --delete-branch
     ```

8. Linked issue cleanup:
   - Identify issue from PR body (`Closes #<n>` / `Fixes #<n>` / `Refs #<n>`) or branch name.
   - If found, remove `wip` and verify labels:
     ```bash
     gh issue edit <n> --remove-label wip
     gh issue view <n> --json labels --jq '[.labels[].name]'
     ```

9. Report PR title, merge result, branch deletion, and linked issue cleanup status.
