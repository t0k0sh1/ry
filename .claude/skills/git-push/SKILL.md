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

> **Sync with `/git-create-pr`**: `.claude/skills/git-create-pr/SKILL.md` inlines Steps 0-3 below as its Steps 1-4. Keep them in sync at the action level — not byte-for-byte.

## Context

- Current git status: !`git status`
- Current git diff: !`git diff HEAD`
- Current branch: !`git branch --show-current`

## Steps

### 0. Branch ensure

> AGENTS.md "Git And GitHub": do not commit directly on `main`.

- Run `git rev-parse --abbrev-ref HEAD`. If not `main`, skip to Step 1.
- If on `main`, create a feature branch:
  1. **Infer `type`** from user intent and working-tree changes:

     | Type | When to use |
     |------|-------------|
     | `feat` | New feature or functionality |
     | `fix` | Bug fix |
     | `docs` | Documentation only |
     | `refactor` | Code restructuring without behavior change |
     | `test` | Adding or updating tests |
     | `chore` | Build, CI, dependencies, tooling |

  2. **Generate a short kebab-case description** (2-4 words ideal). Examples: `feat/add-crypto-stdlib`, `fix/utf8-overread`, `refactor/parser-cleanup`.
  3. **Validate** before `git checkout -b`: lowercase it, strip every non-alphabetic character, and confirm the result does NOT contain `main`. If it does, regenerate and re-validate. Hard MUST — never bypass.
  4. Run `git checkout -b <type>/<short-description>` and report the branch name. Do **not** stop to ask for approval.

### 1. Commit

- Stage and create a single commit using **Conventional Commits** (`feat:` / `fix:` / `refactor:` / `chore:` / etc.).

### 2. Rebase onto `origin/main`

- `git fetch origin`
- `git rebase origin/main`
- **Do not re-run `git fetch` between rebase and push** — weakens the `--force-with-lease` guard in Step 3.
- On conflict:
  - `git diff --name-only --diff-filter=U` to list conflicting files
  - `Read` + `Edit` to resolve
  - `git add <file>` per resolved file → `git rebase --continue`
  - If unresolvable: STOP and report (do **not** auto-`git rebase --abort`)

### 3. Push

- Upstream already set: `git push --force-with-lease`
- First push (no upstream): `git push -u --force-with-lease origin <branch>`
- Force push is required because rebase rewrites SHAs. `--force-with-lease` rejects the push if `origin/<branch>` advanced since the last `git fetch`.
