---
name: triage-side-finding
description: Decide how to handle a side finding during planning, implementation, verification, or review.
allowed-tools: Bash(gh issue:*), Bash(gh search:*), Bash(gh pr:*), Bash(gh api:*), Agent
---

# Triage Side Finding

## Decision Flow

1. **Reproduction window may close**
   - Examples: current CI-only crash, sanitizer finding, race, fuzzer crash.
   - Fix immediately in the current work.
   - Do not delay for origin analysis.

2. **User explicitly directed handling**
   - Explain material consequences.
   - Follow the confirmed direction.

3. **Determine origin**
   - Use `bug-forensics-analyst`.
   - Record origin, impact, test gap, and likely fix scope.

4. **Choose by phase**

| Phase | Default |
|---|---|
| Before implementation | Fix now or propose one separate issue |
| During implementation / verification / review | Absorb into current work |

## During Active Work

- Crash, corruption, race, leak, sanitizer, or fuzzer finding: fix now.
- Non-crash finding estimated at 1000 changed lines or less: fix now.
- Larger non-crash finding: present one recommended action and request user direction.
- If a fix grows beyond the estimate, stop and request direction.

## Separate Issue Proposal

- Confirm it is orthogonal with `/scope-decomposition`.
- Check whether an open PR already owns the required change; fold it there when viable.
- Present one concrete proposal, not a menu.
- Use `/git-create-issue`; wait for explicit approval before filing.
- Check duplicates before creation.

## Output

Report:

- Finding and location.
- Reproduction condition.
- Origin evidence when investigated.
- Chosen action and reason.
