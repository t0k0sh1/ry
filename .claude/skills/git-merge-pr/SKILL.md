---
name: git-merge-pr
description: Merge a pull request to main with safety checks, then remove the wip label from the linked issue.
allowed-tools: Bash(gh pr:*), Bash(gh issue:*), Bash(gh repo:*), Bash(gh api:*), Bash(git branch:*)
metadata:
  short-description: Merge a pull request
---

# Git Merge PR

Merge a pull request after verifying its status.

> **Note:** Merging to main = incorporating into mainline only. Release (tag push → GitHub Release) is a separate process.

## Context

- Current branch: !`git branch --show-current`
- Current branch PR info: !`gh pr view --json number,title,state,mergeable,mergeStateStatus 2>/dev/null || echo "No PR found"`

## Inputs

User input: $ARGUMENTS

## Steps

### Step 1: Identify PR

- If the user specified a PR number (e.g. `#123` or `123`), use that number
- Otherwise, use the PR associated with the current branch (from the context above)
- If no PR is found, inform the user and stop

### Step 2: Check PR status

Use the Context data above (or run `gh pr view <PR> --json state,mergeable,mergeStateStatus,title,number` if a different PR number was specified):
- If `state` is not `OPEN`, inform the user that the PR is not open and stop
- If `mergeable` is not `MERGEABLE`, inform the user and show the reason (`mergeStateStatus`) and stop

### Step 3: Check unresolved review threads

Fetch repository metadata for the GraphQL query:

```shell
gh repo view --json owner,name
```

Then query review threads via GraphQL (note: limited to 100 threads; PRs with more than 100 threads may need manual verification):

```shell
gh api graphql -f query='{ repository(owner: "<owner>", name: "<repo>") { pullRequest(number: <PR>) { reviewThreads(first: 100) { nodes { isResolved comments(first: 1) { nodes { path originalLine body author { login } } } } } } } }'
```

- Count nodes where `isResolved == false`
- If **any unresolved threads exist**, report the count along with the file path, line, and first line of the body for each unresolved thread, then **stop and do not merge**:
  > Found N unresolved review thread(s). Aborting merge. Address the comments, push any fixes, and wait for CodeRabbit (or the reviewer) to verify and resolve the threads, then rerun.
- If all threads are resolved (or there are no threads), proceed to Step 4.

### Step 4: Merge

Execute `gh pr merge <PR> --merge --delete-branch`

### Step 5: Post-merge issue cleanup

Identify the linked issue number `<n>` from the PR body (`Closes #<n>` / `Fixes #<n>` / `Refs #<n>`) or from the branch name. If no linked issue can be determined, skip this step and note it in the report.

Run:

```bash
gh issue edit <n> --remove-label wip
```

Use `--remove-label`. **Never** use `--label` alone — it would replace all existing labels with an empty set, destroying `bug`, `enhancement`, etc. Verify with:

```bash
gh issue view <n> --json labels --jq '[.labels[].name]'
```

Confirm `wip` is gone and every other pre-existing label is still present. GitHub's `Closes #xx` keyword auto-closes the issue when the PR merges into the default branch, so manual `gh issue close` is not needed.

> **Note:** Issue close records that the feature has entered main — it is not a signal that the release is complete. Release (tag push → GitHub Release) is a separate process. See `AGENTS.md` リリースワークフロー section.

Execute this step autonomously, immediately after merge, without waiting for user confirmation.

### Step 6: Report

Report the result to the user (PR number, title, and whether the merge succeeded).
