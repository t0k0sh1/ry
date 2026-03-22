---
name: branch-naming
description: Create a new git branch with a concise Conventional Commit style name for this repository. Use when starting work on a new fix, feature, docs update, refactor, or test change.
metadata:
  short-description: Create well-named feature branches
---

# Branch Naming

Create a new branch using the format `<type>/<short-description>`.

## Format

```text
<type>/<short-description>
```

- `type`: choose from `feat`, `fix`, `docs`, `style`, `refactor`, `test`, or `chore`
- `short-description`: concise English kebab-case summary, ideally 2 to 4 words

## Repository Rules

- Do not create commits on `main` or release branches matching `v*.*.*`
- Prefer branch names that align with the likely commit subject
- Keep the branch focused on a single task or issue

## Examples

- `fix/utf8-overread`
- `docs/issue-workflow`
- `refactor/parser-cleanup`
- `test/add-http-cases`

## Steps

1. Check the current branch with `git branch --show-current`.
2. Infer the branch `type` from the user's request or the task context.
3. Generate a short kebab-case description.
4. Create the branch with `git checkout -b <type>/<short-description>`.
5. Report the created branch name.
