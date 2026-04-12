---
name: git-merge-pr
description: Merge a pull request with safety checks. Warns about manual issue close when merging to non-default branches.
allowed-tools: Bash(gh pr:*), Bash(gh issue:*), Bash(git branch:*)
metadata:
  short-description: Merge a pull request
---

# Git Merge PR

Merge a pull request after verifying its status.

## Context

- Current branch: !`git branch --show-current`
- Current branch PR info: !`gh pr view --json number,title,state,mergeable,mergeStateStatus,baseRefName 2>/dev/null || echo "No PR found"`

## Inputs

User input: $ARGUMENTS

## Steps

### Step 1: Identify PR

- If the user specified a PR number (e.g. `#123` or `123`), use that number
- Otherwise, use the PR associated with the current branch (from the context above)
- If no PR is found, inform the user and stop

### Step 2: Check PR status

Use the Context data above (or run `gh pr view <PR> --json state,mergeable,mergeStateStatus,title,number,baseRefName` if a different PR number was specified):
- If `state` is not `OPEN`, inform the user that the PR is not open and stop
- If `mergeable` is not `MERGEABLE`, inform the user and show the reason (`mergeStateStatus`) and stop

### Step 3: Merge

Execute `gh pr merge <PR> --merge --delete-branch`

### Step 4: Non-default branch warning

Resolve the repository's actual default branch dynamically rather than hardcoding `main`:

```shell
gh repo view --json defaultBranchRef --jq '.defaultBranchRef.name'
```

Then compare the PR's `baseRefName` against that:
- If the base branch is **not** the repository's default branch (e.g. merging into `v0.0.8` when the default is `main`):
  > **Note**: This PR was merged into `<baseRefName>`, not the default branch `<defaultBranch>`. GitHub's `Closes #xx` auto-close does not work for non-default branches. Remember to:
  > - Manually close the related issue
  > - Remove the `wip` label from the issue

### Step 5: Report

Report the result to the user (PR number, title, and whether the merge succeeded).
