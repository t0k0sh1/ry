---
name: git-branch-naming
description: Create a new git branch with a well-named branch following the convention. Use when starting work on a new feature/fix/task.
metadata:
  short-description: Create well-named feature branches
---

# Git Branch Naming

Create a new git branch following the `<type>/<short-description>` naming convention.

## Branch Name Format

```text
<type>/<short-description>
```

- **type**: one of the Conventional Commits types listed below
- **short-description**: concise English summary in kebab-case (2-4 words ideal)

## Types

| Type | When to use |
|------|-------------|
| `feat` | New feature or functionality |
| `fix` | Bug fix |
| `docs` | Documentation only |
| `style` | Formatting, whitespace, no code change |
| `refactor` | Code restructuring without behavior change |
| `test` | Adding or updating tests |
| `chore` | Build, CI, dependencies, tooling |

## Examples

- `feat/add-crypto-stdlib` — new standard library package
- `fix/utf8-overread` — runtime bug fix
- `docs/update-reference-types` — documentation update
- `refactor/parser-cleanup` — internal restructuring
- `chore/pre-release-v0.0.8` — release preparation
- `test/add-list-edge-cases` — test coverage improvement

## Repository Rules

- Always branch from the current release branch (`vx.x.x`) or `main`
- Never commit directly on `main` or `vx.x.x` branches
- The feature branch will be PR'd back to its parent branch

## Steps

1. Check the current branch with `git branch --show-current`
2. Determine the type from the user's intent or the changes in progress
3. Generate a short, descriptive kebab-case summary
4. Run `git checkout -b <type>/<short-description>`
5. Report the created branch name
