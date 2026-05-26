---
name: git-branch-naming
description: Create a new git branch with a well-named branch following the convention. Use when starting work on a new feature/fix/task.
allowed-tools: Bash(git checkout -b:*), Bash(git branch:*), Bash(git status:*)
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
| `refactor` | Code restructuring without behavior change |
| `test` | Adding or updating tests |
| `chore` | Build, CI, dependencies, tooling |

## Examples

- `feat/add-crypto-stdlib` — new standard library module
- `fix/utf8-overread` — runtime bug fix
- `docs/update-reference-types` — documentation update
- `refactor/parser-cleanup` — internal restructuring
- `chore/pre-release-v0.0.8` — release preparation
- `test/add-list-edge-cases` — test coverage improvement

## Repository Rules

- Always branch from `main`
- Never commit directly on `main`
- The feature branch will be PR'd to `main`

## Steps

1. Check the current branch with `git branch --show-current`.
2. **Only create a new branch when necessary**:
   - If the current branch is `main`, proceed to create a new feature branch.
   - Otherwise (already on an existing feature branch), **reuse the current branch** and report it instead of creating a new one. This keeps the skill aligned with `git-create-pr`, which also operates on the current branch when it is not main.
3. Determine the type from the user's intent or the changes in progress.
4. Generate a short, descriptive kebab-case summary.
5. Run `git checkout -b <type>/<short-description>` (only when Step 2 decided to create a new branch).
6. Report the active branch name (new or reused).
