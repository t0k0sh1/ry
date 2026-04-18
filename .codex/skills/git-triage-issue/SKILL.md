---
name: git-triage-issue
description: When a problem is found outside the current task scope, classify and act. Enforces strict triage rules: only direct regressions stay in the branch; everything else becomes a filed issue with full context, proper size split, and milestone set to match the current PR.
metadata:
  short-description: Triage out-of-scope issues with strict filing rules
---

# Git Triage Issue

When you encounter a problem, bug, review comment, or improvement opportunity that may be outside the current task scope, classify it and act according to the Decision Logic below. For clear-cut cases (Case 1 and Case 2), act autonomously. Only ask the user when the classification is genuinely ambiguous (Case 3).

## When to use

- A review comment suggests a change unrelated to the current fix
- You discover a bug or code smell while working on something else
- A fix requires changes that extend beyond the original scope
- Any situation where you are unsure whether something should be addressed now

## Decision Logic

Classify the issue and act **autonomously** (do not ask the user for permission):

### Case 1: Direct regression caused by the current change

**Only** if the issue is a regression that did not exist before the current change AND is directly caused by the code introduced in this branch:
- Fix it in the current feature branch immediately
- Report what was fixed

**What does NOT qualify as Case 1** (treat as Case 2 instead):
- Pre-existing bugs that the current change exposed but did not introduce
- Code smells, style issues, or refactoring opportunities discovered while reading related code
- Improvements that would be nice to make alongside the current change
- Indirect effects or behavior that was already broken before

When in doubt, default to Case 2. PR size discipline takes priority over convenience.

### Case 2: Anything else (existing bugs, improvements, refactoring, style, etc.)

File a GitHub issue. Do **not** fix it in the current branch.

#### Step 1 — Determine milestone

Find the milestone of the current PR or base issue so the new issue can use the same milestone:

```bash
# If working in a PR:
gh pr view --json milestone --jq '.milestone.title'
# Or check the base release branch milestone:
gh issue list --milestone <milestone-title> --limit 1
```

Note the milestone title for use in Step 4.

#### Step 2 — Check for duplicates

```bash
gh search issues --repo t0k0sh1/ry "<keywords>" --state open
```

Or use MCP `search_issues` to scan open issues. If a duplicate exists:
- Add a comment to the existing issue with additional context
- If the existing issue lacks the current milestone, update it: `gh issue edit <number> --milestone "<title>"`
- Skip Step 3 and go to Step 5

#### Step 3 — Decide on split

If the finding covers multiple independent concerns or the estimated scope exceeds roughly 1 PR worth of work, split it into separate issues — one per logical change. Aim for: **1 issue ≈ 1 PR**.

Examples of split-worthy findings:
- A parser bug AND a codegen improvement discovered together → 2 issues
- A runtime function that needs both a correctness fix and a performance improvement → 2 issues

#### Step 4 — Create issue(s)

For each issue, use `gh issue create` with the following body template. All sections are required; omit **Expected vs Actual** only for non-bug items.

```bash
gh issue create \
  --title "<clear descriptive title>" \
  --milestone "<milestone-title>" \
  --body "$(cat <<'EOF'
## Context

<!-- Which file / function / code path was involved? Which PR or issue were you working on when you found this? -->

## Reproduction

<!-- Minimum snippet or step-by-step procedure to trigger the issue -->

## Expected vs Actual

**Expected:** <!-- what should happen -->
**Actual:** <!-- what actually happens -->

## Discovery timing

<!-- One of: during implementation / during self-verification / during PR review response -->
EOF
)"
```

#### Step 5 — Report

Report to the user: issue number(s), title(s), and the milestone set. If multiple issues were filed, list all of them.

### Case 3: Ambiguous

If you genuinely cannot determine whether the issue is caused by the current change:
- Present the issue to the user with **What**, **Where**, and **Context**
- Ask the user whether to fix now or file for later
- Wait for the user's response before proceeding
