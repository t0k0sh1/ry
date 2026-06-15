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
- Do not invoke or suggest `/git-push` from within this skill — the Step 1-4 actions are inlined; delegation is prohibited per #2176.

> **Sync with `/git-push`**: Step 1-4 below mirror the *actions* of `.claude/skills/git-push/SKILL.md` Step 0-3, with added skip guards. When editing the inlined actions, keep them in sync with the standalone `/git-push` at the action level — not byte-for-byte. The standalone skill intentionally lacks the skip guards because it assumes there is work to do.

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

> **Stop guard (before Step 1)**: If currently on `main` with a clean working tree and nothing ahead of `origin/main`, STOP with "no target — nothing to push or PR." This is the only stop case (see Behavior Contract).

### 1. Branch ensure

> AGENTS.md "Git And GitHub": do not commit directly on `main`.

- Run `git rev-parse --abbrev-ref HEAD` to determine the current branch.
- If the current branch is **not** `main`, skip the rest of Step 1 and proceed to Step 2 on the existing branch.
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

### 2. Commit

- **Skip when** the working tree is clean (no staged or unstaged changes).
- Stage and create a single commit using **Conventional Commits** (`feat:` / `fix:` / `refactor:` / `chore:` / etc.).

### 3. Rebase onto `origin/main`

- **Skip when** Step 4 has nothing to push: working tree clean **and** upstream set **and** no commits ahead of `@{u}`.
- `git fetch origin`
- `git rebase origin/main`
- **Do not re-run `git fetch` between rebase and push** — it weakens the `--force-with-lease` guard in Step 4.
- On conflict:
  - `git diff --name-only --diff-filter=U` to list conflicting files
  - `Read` + `Edit` to resolve
  - `git add <file>` per resolved file → `git rebase --continue`
  - If you cannot resolve: STOP and report to the user (do **not** auto-`git rebase --abort`)

### 4. Push

- **Skip when** working tree clean **and** upstream set **and** no commits ahead of `@{u}`.
- Upstream already set: `git push --force-with-lease`
- First push (no upstream): `git push -u --force-with-lease origin <branch>`
- Force push is required because rebase rewrites SHAs. `--force-with-lease` (no argument) rejects the push if `origin/<branch>` advanced since the last `git fetch`.

### 5. Open PR

- Run `gh pr create --base main` (open, not draft).
- Title: Conventional Commits format.
- Body: 1-3 line summary + `Closes #<issue>` for the linked issue.
