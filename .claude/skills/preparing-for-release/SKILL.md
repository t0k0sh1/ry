---
name: preparing-for-release
description: Open three issues (Release prep + Release + Release cleanup) under a version's milestone to drive the tag-push release flow. Pass the version as the argument (e.g. `0.0.14` or `v0.0.14`); the skill asks if absent.
allowed-tools: Bash(gh issue:*), Bash(gh api:*)
metadata:
  short-description: Open prep + release + cleanup issues for a target version
---

# Preparing for Release

Open **Release prep** + **Release** + **Release cleanup** issues under the target version's milestone (AGENTS.md "リリースワークフロー"). The skill only files issues — prep & release use the standard `git-claim-issue` → branch → PR → `git-merge-pr` flow; cleanup is verification-only (`gh run list`, `gh release view`, `gh api PATCH milestone`).

## Inputs

Repository: `t0k0sh1/ry`. User input `$ARGUMENTS` (release version, e.g. `0.0.14` or `v0.0.14`). If absent, **ask the user** — do NOT guess.

## Steps

### Step 1: Resolve and validate the version

- Strip a leading `v` from `$ARGUMENTS` to obtain `<X.Y.Z>`.
- Validate against the regex `^[0-9]+\.[0-9]+\.[0-9]+$`. This mirrors the `^v[0-9]+\.[0-9]+\.[0-9]+$` filter that `release.yml`'s `build` job applies to the tag, so prerelease forms like `0.0.14-rc.1` and other invalid inputs are rejected here.
- If validation fails, report the offending input and stop.

For the rest of this skill, `<X.Y.Z>` denotes the validated semver string and `v<X.Y.Z>` the prefixed tag/milestone form.

### Step 2: Verify the milestone exists

```bash
gh api "repos/t0k0sh1/ry/milestones?state=open" \
  --jq '.[] | select(.title=="v<X.Y.Z>") | .number'
```

- If the result is empty, the milestone does not exist (or is closed). **Stop and ask the user** to create it before re-running. Do not auto-create — milestone creation is intentionally a deliberate, manual act.
- Otherwise record the milestone number for cross-checking.

### Step 3: Check for duplicate issues

```bash
gh issue list --milestone "v<X.Y.Z>" --state open \
  --json number,title \
  --jq '[.[] | select(.title == "Release prep: v<X.Y.Z>" or .title == "Release: v<X.Y.Z>" or .title == "Release cleanup: v<X.Y.Z>")]'
```

- Strict title match is intentional — `--search` is full-text and can hit unrelated issues.
- If any element is returned, report the existing issue number(s) and stop. Do not create duplicates.

### Note: CodeQL gate is enforced by `release.yml`

Since #1542, `release.yml` runs a `codeql-gate` preflight job that verifies the `codeql.yml` workflow run for the exact `github.sha` finished with `conclusion=success` before the `release` job is allowed to publish artifacts. This removes the previous race where a tag push could publish a Release before the CodeQL analysis on the same commit had even started (v0.0.18 / #1539 follow-up).

Operational consequences for this skill:

- **No need to pre-check** "is the latest main CodeQL run green?" in Steps 4/5 — the release workflow itself will block publish until it is.
- The gate filters CodeQL runs by `event=push`. PR-triggered and `workflow_dispatch`-triggered `codeql.yml` runs are **not** counted toward the gate; only mainline analysis state matters.
- If CodeQL itself is broken / unavailable and a release must ship, run `release.yml` via `workflow_dispatch` with `skip_codeql_gate=true`. This is an escape hatch — leave the input at its `false` default for normal tag-push releases.
- If the tag points to a commit that was never pushed to main (so `event=push` CodeQL never ran for it), the gate fails by default. This is intentional — fix the tag's target commit rather than bypassing the gate.

### Step 4: Create the Release prep issue

**Substitution rules before invocation:**

- Replace every `<X.Y.Z>` placeholder with the validated version (e.g. `0.0.14`).
- Leave `<PREV>` and `YYYY-MM-DD` as literals — the prep worker fills those in.

````bash
gh issue create \
  --repo t0k0sh1/ry \
  --title "Release prep: v<X.Y.Z>" \
  --milestone "v<X.Y.Z>" \
  --body "$(cat <<'EOF'
## Goal

Aggregate `changelog.d/` fragments into `CHANGELOG.md` and finalize the `[<X.Y.Z>] - YYYY-MM-DD` section so the release tag can be cut. Standard issue-driven flow (`git-claim-issue` → branch → PR → `git-merge-pr`); the release tag itself is out of scope (sibling Release issue).

## Tasks

### 1. Run `scripts/assemble-changelog.sh`

Collapses `changelog.d/*.md` fragments into `[Unreleased]` and deletes them.

### 2. Rename `[Unreleased]` → `[<X.Y.Z>] - YYYY-MM-DD` (today's UTC date)

```diff
-## [Unreleased]
+## [<X.Y.Z>] - YYYY-MM-DD
```

### 3. Insert a fresh empty `[Unreleased]` heading

Above the new `[<X.Y.Z>]` heading, insert `## [Unreleased]` (body empty; future fragments repopulate).

### 4. Update comparison links at the bottom of `CHANGELOG.md`

```diff
-[Unreleased]: https://github.com/t0k0sh1/ry/compare/v<PREV>...HEAD
+[Unreleased]: https://github.com/t0k0sh1/ry/compare/v<X.Y.Z>...HEAD
+[<X.Y.Z>]: https://github.com/t0k0sh1/ry/compare/v<PREV>...v<X.Y.Z>
```

`<PREV>` = previous released version (top of the existing list).

### 5. Verify `release.yml`'s container pin is fresh

`release.yml` pins the Linux release container to an immutable
`:llvm-<MAJOR>-rev<N>` tag (#1508) for byte-reproducibility across
re-runs. Look up the latest published rev on GHCR (public registry —
no `gh` auth scope required):

```bash
curl -s "https://ghcr.io/token?scope=repository:t0k0sh1/ry-ci-glibc-old:pull" \
  | jq -r '.token' \
  | { read TOKEN; curl -s -H "Authorization: Bearer ${TOKEN}" "https://ghcr.io/v2/t0k0sh1/ry-ci-glibc-old/tags/list"; } \
  | jq -r '.tags[]' | grep -E '^llvm-[0-9]+-rev[0-9]+$' | sort -V | tail -1
```

Compare with the literal in `.github/workflows/release.yml` (the
`format(...)` argument on the `container:` line). If they match, no
change needed. If different, bump the pin in this same Release prep
PR. Add `changelog.d/<this-issue>-bump-release-image-rev.md` (a `###
Fixed` or `### Changed` entry describing the bump) so `assemble-changelog.sh`
in Task 1 folds it into the `[<X.Y.Z>]` section.

The curl command uses the anonymous public GHCR token endpoint
rather than `gh api .../packages/container/.../versions` because
the latter requires the PAT to carry `read:packages` scope, which
`gh auth login` does not grant by default. The curl pattern works
for any maintainer.

## Verification

- `git diff CHANGELOG.md` shows new `[<X.Y.Z>]` + empty `[Unreleased]` + updated comparison links; `changelog.d/` no longer contains the assembled fragments
- `cmake --preset default && cmake --build build && ./build/ry_tests && ./build/ry test -p` passes (sanitizer/fuzzer runs not required for a docs-only change; see `/pre-commit-checklist`)

## Note

`release.yml` enforces a CodeQL preflight gate on the released SHA (#1542). No manual "is CodeQL green on main?" check is needed in this prep issue — the gate runs at tag-push time and blocks publish on its own.

## Out of scope

Bumping VERSION files (none exists; CMake defaults `RY_VERSION` to `0.0.0`, CI injects from tag); pushing `v<X.Y.Z>` (Release issue).
EOF
)"
````

Capture the new issue number from the URL printed by `gh issue create` — call it `<P>` — for the cross-link in Step 5.

### Step 5: Create the Release issue

**Substitution:** as Step 4 + replace `<P>` with the prep issue number; leave `<NEXT>` literal.

````bash
gh issue create \
  --repo t0k0sh1/ry \
  --title "Release: v<X.Y.Z>" \
  --milestone "v<X.Y.Z>" \
  --body "$(cat <<'EOF'
## Goal

Push the `v<X.Y.Z>` tag to trigger `release.yml` and complete the GitHub Release for v<X.Y.Z>.

## Prerequisites

- Release prep issue #<P> is merged (CHANGELOG finalized)
- Local `main` is up to date with `origin/main`

## Tasks

### 1. Verify CHANGELOG matches the release

```bash
git fetch origin main
git checkout main
git pull --ff-only origin main
head -30 CHANGELOG.md
```

Confirm that the topmost dated section is `## [<X.Y.Z>] - YYYY-MM-DD` and that the `[Unreleased]` section above it is empty. If not, return to the prep issue.

### 2. Verify no other open issues remain in the milestone

```bash
gh issue list --milestone "v<X.Y.Z>" --state open --json number,title
```

Excluding **this issue's own number**, the list must be empty. If any other open issues remain, **ask the user** whether to:

- (a) defer them to a later milestone (`gh issue edit <n> --milestone "v<NEXT>"`), or
- (b) close them, or
- (c) abort the release.

Do not proceed without explicit user direction.

### 3. Create and push the tag

```bash
git tag v<X.Y.Z>
git push origin v<X.Y.Z>
```

### 4. Report to the user

- Tag `v<X.Y.Z>` pushed; `release.yml` running: <https://github.com/t0k0sh1/ry/actions/workflows/release.yml>
- Once `release.yml` finishes and the Release publishes, proceed to the Release cleanup issue to verify artifacts and close the milestone

Then **stop** — do not poll the workflow or auto-close the milestone (the human owner does both).

## Note

`release.yml` runs a `codeql-gate` preflight job (#1542) that confirms the `codeql.yml` `event=push` run for the tag's exact SHA finished with `conclusion=success` before the release is published. Manual pre-tag CodeQL verification is therefore unnecessary. If the gate fails (CodeQL outage, broken analysis, or the tag pointing to a commit that was never pushed to main), the workflow will fail loudly. To override only when CodeQL itself is the problem, re-run `release.yml` via `workflow_dispatch` with `skip_codeql_gate=true`.

## Out of scope

Editing `CHANGELOG.md` (prep issue #<P>); closing the milestone (cleanup issue).
EOF
)"
````

Capture the new issue number from the URL printed by `gh issue create` — call it `<R>`.

### Step 6: Create the Release cleanup issue

**Substitution:** as Step 4 + replace `<R>` with the release issue number.

````bash
gh issue create \
  --repo t0k0sh1/ry \
  --title "Release cleanup: v<X.Y.Z>" \
  --milestone "v<X.Y.Z>" \
  --body "$(cat <<'EOF'
## Goal

Verify the v<X.Y.Z> release artifacts are healthy and close the milestone.

## Prerequisites

- Release issue #<R> is closed (= the tag has been pushed and `release.yml` has finished)

## Tasks

### 1. Verify release.yml run

```bash
gh run list --repo t0k0sh1/ry --workflow release.yml --limit 5 \
  --json status,conclusion,headBranch \
  --jq '.[] | select(.headBranch == "v<X.Y.Z>")'
```

Expect `status=completed`, `conclusion=success`.

### 2. Verify the GitHub Release

```bash
gh release view v<X.Y.Z> --repo t0k0sh1/ry \
  --json tagName,name,isDraft,isPrerelease,publishedAt
```

Expect `isDraft=false`, `isPrerelease=false`, `publishedAt` populated.

### 3. Close the milestone

```bash
MS_NUM=$(gh api 'repos/t0k0sh1/ry/milestones?state=open' \
  --jq '.[] | select(.title == "v<X.Y.Z>") | .number')
gh api -X PATCH "repos/t0k0sh1/ry/milestones/$MS_NUM" -f state=closed
```

## Out of scope

Creating the next milestone (deliberately separate); bumping docs/version files (none today).
EOF
)"
````

Capture the new issue number from the URL printed by `gh issue create` — call it `<C>`.

### Step 7: Report

Report `#<P>` (Release prep), `#<R>` (Release), `#<C>` (Release cleanup) with their URLs. Work starts at `#<P>` (claim via `git-claim-issue`); `#<C>` is addressed after `#<R>` closes.
