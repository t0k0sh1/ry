---
name: release
description: Create and merge the release PR from vx.x.x to main, then verify release notes.
allowed-tools: Bash(gh pr:*), Bash(gh release:*), Bash(gh repo:*), Bash(gh issue:*), Bash(git branch:*), Bash(git fetch:*), Bash(git log:*), Bash(git diff:*), Bash(cat:*), Read
metadata:
  short-description: Merge release branch to main and verify release
---

# Release

Create the release PR from `vx.x.x` to `main`, merge it, and verify that the GitHub Release is generated correctly.

## Context

- Current branch: !`git branch --show-current`
- VERSION file: !`cat VERSION`
- Release branches: !`git branch --list 'v*.*.*' | head -5`
- Recent releases: !`gh release list --limit 3 2>/dev/null || echo "(none)"`

## Inputs

User input: $ARGUMENTS

Expected: version number like `0.0.9`, or nothing to auto-detect from VERSION file.

## Steps

### Step 1: Determine version

- If user provided a version in $ARGUMENTS, use it
- Otherwise read from `VERSION` file
- Validate the branch `v<RELEASE_VERSION>` exists on remote (`git fetch origin v<RELEASE_VERSION>`)

### Step 2: Verify release readiness

- Confirm that the CHANGELOG.md contains a `[<RELEASE_VERSION>]` section (i.e., `/release-prep` has been run)
- If not found, inform the user that `/release-prep` should be run first and stop

### Step 3: Create release PR

Create a PR from `v<RELEASE_VERSION>` to `main`:

```bash
gh pr create --base main --head "v<RELEASE_VERSION>" --title "Release v<RELEASE_VERSION>" --body "<body>"
```

The body should include the CHANGELOG.md section for this version (extracted between the version header and the next version header or end of relevant content).

### Step 4: Merge release PR

- **Ask user for confirmation before merging**
- Merge with: `gh pr merge <PR_NUMBER> --merge`
- Do NOT delete the branch (release branches are kept for history)

### Step 5: Verify release

After merging, check if the GitHub Release was auto-created:

```bash
gh release view "v<RELEASE_VERSION>" 2>/dev/null
```

- If the release exists, verify the release notes content matches CHANGELOG.md
- If not yet created, inform the user that the release workflow may still be running (check `.github/workflows/release.yml`)

### Step 6: Report

Report to the user:
- PR URL and merge status
- Release URL (if available)
- Any discrepancies in release notes
