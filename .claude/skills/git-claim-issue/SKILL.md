---
name: git-claim-issue
description: Claim a GitHub issue by safely adding the `wip` label without erasing existing labels. Invoked as **Task 1 of the implementation plan** (immediately after Plan approval, before the first `/git-push` call), or on explicit user request: "start working on #123", "claim issue 456", "issue に取り組む", "wip つけて", "着手".
allowed-tools: Bash(gh issue view:*), Bash(gh issue edit:*)
metadata:
  short-description: Add wip label to an issue without clobbering other labels
---

# Git Claim Issue

Mark a GitHub issue as in-progress by adding the `wip` label. This is the counterpart of `git-close-pr` Step 7, which removes `wip` after merge.

## Scope (what this skill does NOT do)

This skill only adds the `wip` label to an **existing** GitHub issue. Creating a brand-new issue is **out of scope** — see `/git-create-issue`, which enforces an explicit user-permission gate before running `gh issue create` (AGENTS.md §責務の分離「ユーザーが明示的に指示すること」). Do not use this skill to file new issues, even if the user references both flows in the same message.

## When to use

- As **Task 1** of the implementation plan, immediately after Plan approval (before the first `/git-push` call, which auto-creates the feature branch)
- When the user says 「取り組む」 / 「着手」 / "start working" / "claim" / 「wip つけて」 for a specific issue

## Repository

- owner: `t0k0sh1`
- repo: `ry`

## Inputs

User input: `$ARGUMENTS` (issue number, e.g. `907` or `#907`)

If no issue number is supplied, ask the user before proceeding. Do NOT guess.

## Critical safety rule

Always use `--add-label`. **Never** use `--label` with `gh issue edit`:

| Command | Effect |
|---|---|
| `gh issue edit <n> --label wip` | **Replaces all labels** (destructive — do NOT use) |
| `gh issue edit <n> --add-label wip` | Appends, preserves existing labels (correct) |
| `gh issue edit <n> --remove-label wip` | Removes only the named label (used by `git-close-pr` Step 7) |

Note: `gh issue create --label foo` is safe because there are no pre-existing labels to overwrite. The destructive case applies only to `gh issue edit --label`.

Violating this rule wipes labels such as `bug`, `enhancement`, milestone-shadow labels, etc.

## Steps

### Step 1: Read current labels

```bash
gh issue view <n> --json number,title,state,labels --jq '{number, title, state, labels: [.labels[].name]}'
```

- If `state` is not `OPEN`, report to the user and stop.
- Record the current label array as `BEFORE`.

### Step 2: Idempotency check

- If `BEFORE` already contains `wip`, report "issue #<n> is already claimed (wip present). No change." and stop. Do NOT re-run `gh issue edit`.

### Step 3: Add the `wip` label

```bash
gh issue edit <n> --add-label wip
```

Do not pass any other flags. Do not combine with `--remove-label` or `--milestone` in the same invocation.

### Step 4: Verify

```bash
gh issue view <n> --json labels --jq '[.labels[].name]'
```

Record as `AFTER`. Assert:
- `wip` ∈ `AFTER`
- `BEFORE ⊆ AFTER` (every label present before is still present)

If either assertion fails, STOP and report the diff to the user. Do not attempt automatic rollback; the user decides.

### Step 5: Report

Report to the user: issue number, title, `BEFORE` labels, `AFTER` labels, and confirmation that `wip` was added.
