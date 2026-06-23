---
name: git-create-pr
description: User-invoked slash command that ensures a feature branch, commits any pending work, rebases onto main, pushes, and opens a PR — all in one pass. Never invoke autonomously, from another skill, or merely because implementation is complete.
allowed-tools: Bash(git status:*), Bash(gh pr create:*), Bash(git diff:*), Bash(git branch:*), Bash(git log:*), Bash(git rev-parse:*), Bash(git add:*), Bash(git push:*), Bash(git commit:*), Bash(git fetch:*), Bash(git rebase:*), Bash(git checkout -b:*), Read, Edit
metadata:
  short-description: Branch, push, and open a PR
---

# Git Create PR

## Invocation Gate

- Run only when the user directly invokes `/git-create-pr`.
- Never invoke this skill autonomously or from another skill.
- Never propose this skill, present it as an option, include it in a plan, or list it as a next step.
- Do not invoke or suggest `/git-push` from within this skill — Steps 1-4 are inlined; delegation is prohibited per #2176.

> **Sync with `/git-push`**: Steps 1-4 below mirror the *actions* of `.claude/skills/git-push/SKILL.md` Steps 0-3, with added skip guards. Keep them in sync at the action level — not byte-for-byte.

## Context

- Current branch: !`git branch --show-current`
- Working tree: !`git status --short`
- Unpushed commits: !`git log @{u}..HEAD --oneline 2>/dev/null || echo "(no upstream)"`

## Behavior Contract

| State | Action |
|---|---|
| On `main` + clean + nothing ahead of `origin/main` | **STOP — "no target"** (only stop case) |
| On `main` + dirty or commits ahead | Step 1 → Step 2 (if dirty) → Step 3 → Step 4 → Step 5 |
| Feature branch + dirty | Step 2 → Step 3 → Step 4 → Step 5 |
| Feature branch + clean + (no upstream or unpushed commits) | Step 3 → Step 4 → Step 5 |
| Feature branch + clean + upstream set + everything pushed | Step 5 only |

Rule: **commit only when dirty; rebase + push only when something needs pushing; PR always**.

## Steps

> **Stop guard (before Step 1)**: If on `main` with a clean working tree and nothing ahead of `origin/main`, STOP with "no target — nothing to push or PR."

### 1. Branch ensure

> AGENTS.md "Git And GitHub": do not commit directly on `main`.

- Run `git rev-parse --abbrev-ref HEAD`. If not `main`, skip to Step 2.
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

### 2. Commit

- **Skip when** working tree is clean.
- Stage and create a single commit using **Conventional Commits** (`feat:` / `fix:` / `refactor:` / `chore:` / etc.).

### 3. Rebase onto `origin/main`

- **Skip when** Step 4 has nothing to push: clean + upstream set + no commits ahead of `@{u}`.
- `git fetch origin`
- `git rebase origin/main`
- **Do not re-run `git fetch` between rebase and push** — weakens the `--force-with-lease` guard.
- On conflict:
  - `git diff --name-only --diff-filter=U` to list conflicting files
  - `Read` + `Edit` to resolve
  - `git add <file>` per resolved file → `git rebase --continue`
  - If unresolvable: STOP and report (do **not** auto-`git rebase --abort`)

### 4. Push

- **Skip when** clean + upstream set + no commits ahead of `@{u}`.
- Upstream already set: `git push --force-with-lease`
- First push: `git push -u --force-with-lease origin <branch>`

### 5. Open PR

- Run `gh pr create --base main` (open, not draft).
- Title: Conventional Commits format.
- Body: 1-3 line summary + `Closes #<issue>` for the linked issue.
