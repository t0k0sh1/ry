---
name: release-prep
description: Prepare a release by assembling changelog and merging to the release branch.
allowed-tools: Bash(git checkout -b:*), Bash(git add:*), Bash(git status:*), Bash(git push:*), Bash(git commit:*), Bash(git fetch:*), Bash(git merge:*), Bash(git branch:*), Bash(git diff:*), Bash(git log:*), Bash(gh pr:*), Bash(cat:*), Bash(bash:*), Bash(ls:*), Bash(date:*), Read, Edit
metadata:
  short-description: Run release preparation (changelog, merge)
---

# Release Prep

Prepare a release from a `vx.x.x` branch through changelog assembly and pre-release merge.

## Context

- Current branch: !`git branch --show-current`
- VERSION file: !`cat VERSION`
- Pending changelog fragments: !`ls changelog.d/ 2>/dev/null || echo "(none)"`
- Release branches: !`git branch --list 'v*.*.*' | head -5`

## Inputs

User input: $ARGUMENTS

Expected: version number like `0.0.9`, or nothing to read from VERSION file.

## Steps

### Step 1: Determine version

- If user provided a version in $ARGUMENTS, use it
- Otherwise read from `VERSION` file
- Validate it matches semver pattern `x.x.x`
- Set RELEASE_VERSION to this value
- Confirm we are on the `v<RELEASE_VERSION>` branch (if not, inform user and stop)

### Step 2: Create pre-release branch

- Create `chore/pre-release-v<RELEASE_VERSION>` from the current `v<RELEASE_VERSION>` branch

### Step 3: Update VERSION file

- Write RELEASE_VERSION into the `VERSION` file (if not already correct)
- Stage and commit: `chore: bump VERSION to <RELEASE_VERSION>`

### Step 4: Assemble changelog

1. Run `bash scripts/assemble-changelog.sh`
2. Edit `CHANGELOG.md`:
   - Replace the `[Unreleased]` header with `[<RELEASE_VERSION>] - <YYYY-MM-DD>` (use today's date)
   - Add a new empty `[Unreleased]` section above it with standard subsection headers
   - Update the comparison links at the bottom of the file
3. Stage and commit: `chore: assemble changelog for v<RELEASE_VERSION>`

### Step 5: Push and merge to release branch

1. Push `chore/pre-release-v<RELEASE_VERSION>` to origin
2. Create PR targeting `v<RELEASE_VERSION>` branch
3. **Ask user for confirmation before merging**
4. Merge the PR using `gh pr merge --merge --delete-branch`

### Completion

Report summary to the user:
- Version prepared
- Number of changelog fragments assembled
- PR URL and merge status
