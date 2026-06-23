---
name: preparing-for-release
description: Open Release prep, Release, and Release cleanup issues for a target version milestone.
allowed-tools: Read, Bash(gh issue:*), Bash(gh api:*)
metadata:
  short-description: Open release workflow issues
---

# Preparing for Release

Creates three milestone issues for `t0k0sh1/ry`: Release prep, Release, and Release cleanup.

## Inputs

`$ARGUMENTS` is the release version (`0.0.14` or `v0.0.14`). If absent, ask the user.

## Templates

- Prep: `.claude/skills/preparing-for-release/templates/release-prep.md`
- Release: `.claude/skills/preparing-for-release/templates/release.md`
- Cleanup: `.claude/skills/preparing-for-release/templates/release-cleanup.md`

## Steps

1. Normalize version:
   - Strip leading `v`.
   - Require `^[0-9]+\.[0-9]+\.[0-9]+$`; reject prerelease forms.
   - Use `<X.Y.Z>` for the bare version and `v<X.Y.Z>` for tag/milestone.

2. Verify milestone exists:
   ```bash
   gh api "repos/t0k0sh1/ry/milestones?state=open" \
     --jq '.[] | select(.title=="v<X.Y.Z>") | .number'
   ```
   If absent, ask the user to create it.

3. Stop if any target issue already exists:
   ```bash
   gh issue list --milestone "v<X.Y.Z>" --state open --json number,title \
     --jq '[.[] | select(.title == "Release prep: v<X.Y.Z>" or .title == "Release: v<X.Y.Z>" or .title == "Release cleanup: v<X.Y.Z>")]'
   ```

4. Read the prep template, replace `<X.Y.Z>` only, and create the prep issue. Leave `<PREV>` and `YYYY-MM-DD` literal.
   ```bash
   gh issue create --repo t0k0sh1/ry --title "Release prep: v<X.Y.Z>" \
     --milestone "v<X.Y.Z>" --body "<rendered prep template>"
   ```
   Record issue number as `<P>`.

5. Read the release template, replace `<X.Y.Z>` and `<P>`, create the release issue, and record it as `<R>`. Leave `<NEXT>` literal.

6. Read the cleanup template, replace `<X.Y.Z>` and `<R>`, create the cleanup issue, and record it as `<C>`. Leave `<this-issue>` literal.

7. Report `#<P>`, `#<R>`, and `#<C>` with URLs. Start work from `#<P>` after `scripts/claim-issue.sh '#<P>'`; address `#<C>` only after `release.yml` finishes for tag `v<X.Y.Z>`.

## Notes

- `release.yml` enforces the CodeQL push-run gate; no manual pre-tag CodeQL check is required.
- To bypass CodeQL for an outage only, rerun `release.yml` via `workflow_dispatch` with `skip_codeql_gate=true`.
