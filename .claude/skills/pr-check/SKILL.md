---
name: pr-check
description: User-invoked slash command that checks an already-pushed PR by resolving conflicts, addressing reviews, checking CI, merging, and cleaning up.
allowed-tools: Bash(gh:*), Bash(git status:*), Bash(git diff:*), Bash(git add:*), Bash(git commit:*), Bash(git fetch:*), Bash(git rebase:*), Bash(git push:*), Bash(git log:*), Bash(git branch:*), Bash(git switch:*), Read, Edit
metadata:
  short-description: Review -> CI -> merge
---

# PR Check

Run only on direct `/pr-check` invocation. Invocation is merge consent. Run once through; on any blocker, report and stop.

## Inputs

`$ARGUMENTS` may contain a PR number. Otherwise use the PR for the current branch:

```bash
gh pr view --json number,title,state,mergeable,mergeStateStatus,headRefName,body
```

If no PR is found, stop.

## Steps

1. Identify and enter the PR head branch:
   - Stop unless `state == OPEN`.
   - If the current branch differs from `headRefName`, switch only when the PR number was explicit; otherwise stop to avoid rebasing the wrong branch.
   - Stop on dirty worktree unless resuming an in-progress rebase.

2. Rebase, resolve conflicts, and push when needed:
   - If a rebase is already in progress, resume conflict resolution; do not start another rebase.
   - Otherwise run `git fetch origin` then `git rebase origin/main`.
   - Do not fetch again before push.
   - On conflicts: list `git diff --name-only --diff-filter=U`, resolve with `Read` / `Edit`, `git add <file>`, and `git rebase --continue`.
   - If unresolvable, stop and report; do not auto-`git rebase --abort`.
   - If commits changed, push with `git push --force-with-lease`.

3. Check CI:
   ```bash
   gh pr checks <PR> --json name,bucket,state,link
   ```
   Any `bucket == "fail"` stops the flow. Pending checks may proceed to review handling; merge gate re-checks later.

4. Address review comments and unresolved threads:
   - Fetch PR comments, reviews, and review threads with `gh api`.
   - For each unique reviewer comment or unresolved thread whose last commenter is not the viewer:
     - Auto-apply typos, lint suggestions, tracking references, nits, optional comments, and not-blocking comments; reply to the comment.
     - Ask the user for design judgment, large rewrites, or ambiguous directions.
   - Do not resolve threads yourself.
   - Track every file edited in this step; Step 6 stages only those files.

5. Run applicable checks from `/pre-commit-checklist`. Stop on outstanding failures.

6. Commit review edits only:
   - Stop if the working tree has changes outside the Step 4 touched-file list.
   - Skip when Step 4 made no edits.
   - `git add <touched files>`; never `git add -A` / `git add .`.
   - `git commit -m "fix: address review comment from PR #<PR>"`.

7. Rebase and push review-fix commits only when needed:
   ```bash
   git fetch origin
   git rebase origin/main
   git push --force-with-lease
   ```
   Do not fetch again between rebase and push. On conflicts, resolve with `Read` / `Edit`, `git add`, and `git rebase --continue`; if unresolvable, stop.

8. Merge gate:
   - Re-check `gh pr view <PR> --json state,mergeable,mergeStateStatus`.
   - Stop unless `mergeable == MERGEABLE` and `mergeStateStatus` is `CLEAN` or `HAS_HOOKS`.
   - Re-fetch review threads; stop if any remain unresolved.
   - Merge:
     ```bash
     gh pr merge <PR> --merge --delete-branch
     ```

9. Linked issue cleanup:
   - Identify issue from PR body (`Closes #<n>` / `Fixes #<n>` / `Refs #<n>`) or branch name.
   - If found, remove `wip` and verify labels:
     ```bash
     gh issue edit <n> --remove-label wip
     gh issue view <n> --json labels --jq '[.labels[].name]'
     ```

10. Report PR title, merge result, conflict-resolution summary if any, branch deletion, and linked issue cleanup status.
