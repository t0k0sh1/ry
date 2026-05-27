---
name: release-orchestrator
description: Entry point for the ry release flow. Routes to /preparing-for-release, documents the tag-push driven release.yml build, and explains the milestone-close policy. Use for "リリース" / "タグ push" / "リリース手順" / "milestone close" / "バージョンリリース" / "v0.x.y".
allowed-tools: Bash(gh issue:*), Bash(gh milestone:*), Bash(git tag:*), Bash(git push:*)
---

# Release Orchestrator

Entry-point reference for the ry release flow (moved from `AGENTS.md` by #1384). Routes to `/preparing-for-release` and documents the tag-push driven `release.yml` mechanism plus the milestone-close policy.

## Overview

Trigger this skill when the `v<X.Y.Z>` milestone is feature-complete (all issues under it closed). Note: merging to main is mainline uptake only — releases (tag push → GitHub Release) are a separate workflow.

Releases are tag-push driven. Pushing a `v*.*.*`-glob tag (e.g., `v0.0.14`) to `main` runs `.github/workflows/release.yml`, which builds, tests, and publishes the GitHub Release. The glob also matches prereleases (`v0.0.14-rc.1`), so the `build` job's first step strictly validates `^v[0-9]+\.[0-9]+\.[0-9]+$` and rejects non-semver tags.

`ry -v` returns `0.0.0` when `-DRY_VERSION` is unset (local default); CI injects the version via `-DRY_VERSION=${GITHUB_REF_NAME#v}`. `workflow_dispatch` remains available for CI-failure retries only, guarded by `github.ref_type == 'tag'`.

## Hand-off

1. Invoke `/preparing-for-release <X.Y.Z>`. The skill creates three issues under the milestone:
   - **Release prep: v<X.Y.Z>** — assemble `changelog.d/` fragments into `CHANGELOG.md` under the `[X.Y.Z] - YYYY-MM-DD` section. Standard issue flow (claim → feature branch → PR → merge).
   - **Release: v<X.Y.Z>** — once prep merges, confirm no issues remain in the milestone and push the tag.
   - **Release cleanup: v<X.Y.Z>** — after tag push, verify `release.yml` completion and GitHub Release publication, then close the milestone (verification only — no branch/PR).
2. Progress the Release prep issue through the normal flow (`git-claim-issue` → Plan → implement → `git-close-pr`).
3. After Release prep merges to main, work on the Release issue and push the tag per its instructions.
4. After Release closes, work on the Release cleanup issue. Close the milestone only after release artifacts (tag + GitHub Release) are verified published — "all issues closed ≠ release complete." Follow the cleanup issue's steps.
