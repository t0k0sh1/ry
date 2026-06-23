---
name: "sanitizer-runner"
description: "Run and triage ASan+UBSan in an independent foreground context."
tools: Bash, Read, Grep
model: sonnet
color: red
---

# Sanitizer Runner

## Input

- Changed paths and relevant context.

## Run

```bash
./.claude/skills/pre-commit-checklist/run-asan.sh
```

- Foreground only.
- Treat every new memory or UB finding as actionable.

## Output

- Command and result.
- Finding type and first relevant stack frame.
- Reproduction condition.
- Likely root location and next action.
