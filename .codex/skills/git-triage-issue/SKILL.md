---
name: git-triage-issue
description: When a problem is found outside the current task scope, ask the user how to handle it. Aligns with AGENTS.md scope-out rules.
metadata:
  short-description: Triage out-of-scope issues
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

### Case 1: Caused by the current change

If the issue is caused by the current change (including indirect causes), or if it did not exist prior to this change:
- Fix it in the current feature branch immediately
- Report what was fixed

### Case 2: Future improvement (not caused by the current change)

If the issue is a future improvement unrelated to the current change:
1. Search existing issues with `gh search issues` to avoid duplicates
2. If a duplicate exists, add a comment to the existing issue with relevant context
3. Otherwise, create a new issue with `gh issue create`:
   - A clear, descriptive title
   - A body that includes the context (what, where, how it was found)
4. Report the issue number and title to the user

### Case 3: Ambiguous

If you genuinely cannot determine whether the issue is caused by the current change:
- Present the issue to the user with **What**, **Where**, and **Context**
- Ask the user whether to fix now or file for later
- Wait for the user's response before proceeding
