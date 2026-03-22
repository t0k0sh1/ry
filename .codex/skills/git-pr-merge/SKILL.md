---
name: git-pr-merge
description: Merge an open pull request with GitHub CLI after verifying that it is mergeable. Use when the user explicitly asks to merge a PR.
metadata:
  short-description: Merge a PR safely with gh
---

# Merge Pull Request

Merge an open PR only after verifying that it is valid and mergeable.

## Inputs

- A PR number like `123` or `#123`, or no number to use the PR associated with the current branch

## Steps

1. Determine the target PR.
If the user specifies a PR number, use it.
Otherwise, use the PR associated with the current branch from `gh pr view`.
If no PR is found, stop and report that no target PR was found.

2. Inspect the PR with `gh pr view <PR> --json state,mergeable,mergeStateStatus,title,number`.

3. Stop if any of the following is true.
- `state` is not `OPEN`
- `mergeable` is not `MERGEABLE`

When stopping, report the PR number, title, and the blocking status such as `mergeStateStatus`.

4. If the PR is mergeable, run `gh pr merge <PR> --merge --delete-branch`.

5. Report whether the merge succeeded, including the PR number and title.

## Repository Rules

- Only use this skill when the user explicitly asks to merge a PR
- Do not perform unrelated branch cleanup or follow-up pushes
