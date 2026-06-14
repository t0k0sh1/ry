---
name: git-push
description: User-invoked slash command that ensures a feature branch, commits, rebases onto main, and pushes. Never invoke autonomously, from another skill, or merely because changes are ready to publish.
allowed-tools: Bash(git add:*), Bash(git status:*), Bash(git push:*), Bash(git commit:*), Bash(git fetch:*), Bash(git rebase:*), Bash(git diff:*), Bash(git branch:*), Bash(git log:*), Bash(git checkout -b:*), Bash(git rev-parse:*), Read, Edit
metadata:
  short-description: Branch, commit and push
---

# Git Push

## Invocation Gate

- Run only when the user directly invokes `/git-push`.
- Never invoke this skill autonomously or from another skill.
- Never propose this skill, present it as an option, include it in a plan, or list it as a next step.

## Context

- Current git status: !`git status`
- Current git diff: !`git diff HEAD`
- Current branch: !`git branch --show-current`

## Steps

### 0. Branch ensure

> AGENTS.md "Git And GitHub": do not commit directly on `main`; use this skill to create the feature branch first.

- Run `git rev-parse --abbrev-ref HEAD` to determine the current branch.
- If the current branch is **not** `main`, skip the rest of Step 0 and proceed to Step 1 on the existing branch.
- If the current branch is `main`, create a feature branch before any commit:
  1. **Infer `type`** from user intent and working-tree changes. Pick one of:

     | Type | When to use |
     |------|-------------|
     | `feat` | New feature or functionality |
     | `fix` | Bug fix |
     | `docs` | Documentation only |
     | `refactor` | Code restructuring without behavior change |
     | `test` | Adding or updating tests |
     | `chore` | Build, CI, dependencies, tooling |

  2. **Generate a short kebab-case description** (2-4 words ideal). Examples: `feat/add-crypto-stdlib`, `fix/utf8-overread`, `refactor/parser-cleanup`.

     > The generated branch name `<type>/<short-description>` must satisfy AGENTS.md "Git And GitHub".

  3. **Validate** the chosen branch name before `git checkout -b`: lowercase it, strip every non-alphabetic character, and check that the result does NOT contain the substring `main`. If it does, regenerate a different `<short-description>` and re-validate. Repeat until clean. This is a hard MUST — never bypass.
  4. Run `git checkout -b <type>/<short-description>` and report the chosen branch name in the next message. Do **not** stop to ask for approval — auto-progress matches the legacy `git-branch-naming` behavior. If the user wants a different name afterwards, they can rename via `git branch -m <new>` (the rename must also satisfy the MUST rule above).

### 1. Commit

- Stage and create a single commit using **Conventional Commits** (`feat:` / `fix:` / `refactor:` / `chore:` / etc.).

### 2. Rebase onto `origin/main`

- `git fetch origin`
- `git rebase origin/main`
- **Do not re-run `git fetch` between rebase and push** — it weakens the `--force-with-lease` guard in Step 3.
- On conflict:
  - `git diff --name-only --diff-filter=U` to list conflicting files
  - `Read` + `Edit` to resolve
  - `git add <file>` per resolved file → `git rebase --continue`
  - If you cannot resolve: STOP and report to the user (do **not** auto-`git rebase --abort`)

### 3. Push

- Upstream already set: `git push --force-with-lease`
- First push (no upstream): `git push -u --force-with-lease origin <branch>`
- Force push is required because rebase rewrites SHAs. `--force-with-lease` (no argument) rejects the push if `origin/<branch>` advanced since the last `git fetch`.
