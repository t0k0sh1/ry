---
name: preparing-for-release
description: Open three issues (Release prep + Release + Release cleanup) under a version's milestone to drive the tag-push release flow. Pass the version as the argument (e.g. `0.0.14` or `v0.0.14`); the skill asks if absent.
allowed-tools: Bash(gh issue:*), Bash(gh api:*)
metadata:
  short-description: Open prep + release + cleanup issues for a target version
---

# Preparing for Release

Creates three issues — **Release prep**, **Release**, and **Release cleanup** — under the target version's milestone. The **Release cleanup** issue closes the Release issue, the milestone, and itself in order.

## Inputs

Repository: `t0k0sh1/ry`. `$ARGUMENTS` contains the release version (e.g. `0.0.14` / `v0.0.14`). If absent, **ask the user** — do not guess.

## Steps

### Step 1: Resolve and validate the version

- Strip any leading `v` from `$ARGUMENTS` to get `<X.Y.Z>`.
- Match against `^[0-9]+\.[0-9]+\.[0-9]+$` (symmetric with the `^v[0-9]+\.[0-9]+\.[0-9]+$` pattern used by the `build` job in `release.yml`). Reject forms like `0.0.14-rc.1`.
- On validation failure, report the input value and stop.

Going forward, `<X.Y.Z>` denotes the validated semver string; `v<X.Y.Z>` denotes the prefixed tag/milestone form.

### Step 2: Verify the milestone exists

```bash
gh api "repos/t0k0sh1/ry/milestones?state=open" \
  --jq '.[] | select(.title=="v<X.Y.Z>") | .number'
```

If empty, **stop and ask the user to create the milestone** — do not create it automatically. If found, record the milestone number.

### Step 3: Check for duplicate issues

```bash
gh issue list --milestone "v<X.Y.Z>" --state open \
  --json number,title \
  --jq '[.[] | select(.title == "Release prep: v<X.Y.Z>" or .title == "Release: v<X.Y.Z>" or .title == "Release cleanup: v<X.Y.Z>")]'
```

Exact title matching is intentional (`--search` performs full-text matching and may hit unrelated issues). If any results are returned, report the existing issue numbers and stop.

### Note: CodeQL gate is enforced by `release.yml`

`release.yml` has a `codeql-gate` preflight job that blocks the release job until a `codeql.yml` run with `event=push` completes with `conclusion=success` (#1542). No manual verification required.

- PR-triggered and `workflow_dispatch` runs do not count. Only `event=push`.
- To bypass CodeQL if necessary, run `release.yml` via `workflow_dispatch` with `skip_codeql_gate=true`.
- If the tag points to a commit not on main, the gate fails by default. Fix the target commit rather than bypassing.

### Step 4: Create the Release prep issue

**Substitution:** Replace `<X.Y.Z>` with the validated version. Leave `<PREV>` and `YYYY-MM-DD` as literals.

````bash
gh issue create \
  --repo t0k0sh1/ry \
  --title "Release prep: v<X.Y.Z>" \
  --milestone "v<X.Y.Z>" \
  --body "$(cat <<'EOF'
## Goal

Aggregate `changelog.d/` fragments into `CHANGELOG.md` and finalize the `[<X.Y.Z>] - YYYY-MM-DD` section. Git publication, PR creation, and the release tag itself are out of scope (sibling Release issue).

## Tasks

### 1. Run `scripts/assemble-changelog.sh`

Collapses `changelog.d/*.md` fragments into `[Unreleased]` and deletes them.

### 2. Verify `share/std/manifest.json` matches the on-disk stdlib

Drift is not caught by CI — verify here.

```bash
diff <(jq -r '.files[] | "share/std/" + .' share/std/manifest.json | sort) \
     <(find share/std -name '*.ry' | sort)
```

- Empty diff → no action.
- Files on disk but missing from manifest → add to `files` array (top-level flat entries, then per-module subdirectories).
- Files in manifest but missing on disk → remove from `files` array.

Include the manifest edit in the same Release prep PR.

### 3. Rename `[Unreleased]` → `[<X.Y.Z>] - YYYY-MM-DD` (today's UTC date)

```diff
-## [Unreleased]
+## [<X.Y.Z>] - YYYY-MM-DD
```

### 4. Insert a fresh empty `[Unreleased]` heading

Above the new `[<X.Y.Z>]` heading, insert `## [Unreleased]` (body empty; future fragments repopulate).

### 5. Update comparison links at the bottom of `CHANGELOG.md`

```diff
-[Unreleased]: https://github.com/t0k0sh1/ry/compare/v<PREV>...HEAD
+[Unreleased]: https://github.com/t0k0sh1/ry/compare/v<X.Y.Z>...HEAD
+[<X.Y.Z>]: https://github.com/t0k0sh1/ry/compare/v<PREV>...v<X.Y.Z>
```

`<PREV>` = previous released version (top of the existing list).

### 6. Verify `release.yml`'s container pin is fresh

`release.yml` pins the Linux release container to `:llvm-<MAJOR>-rev<N>` (#1508). Look up the latest rev on GHCR:

```bash
curl -s "https://ghcr.io/token?scope=repository:t0k0sh1/ry-ci-glibc-old:pull" \
  | jq -r '.token' \
  | { read TOKEN; curl -s -H "Authorization: Bearer ${TOKEN}" "https://ghcr.io/v2/t0k0sh1/ry-ci-glibc-old/tags/list"; } \
  | jq -r '.tags[]' | grep -E '^llvm-[0-9]+-rev[0-9]+$' | sort -V | tail -1
```

Compare with the `container:` line `format(...)` argument in `.github/workflows/release.yml`. If different, update the pin in the same PR and add `changelog.d/<this-issue>-bump-release-image-rev.md` to be folded into `[<X.Y.Z>]` by Task 1.

## Verification

- `git diff CHANGELOG.md` shows new `[<X.Y.Z>]` + empty `[Unreleased]` + updated comparison links; `changelog.d/` no longer contains the assembled fragments
- `/pre-commit-checklist` passes (sanitizer/fuzzer runs not required for a docs-only change)

## Out of scope

Bumping VERSION files (none exists); pushing `v<X.Y.Z>` (Release issue).
EOF
)"
````

Extract the issue number from the URL printed by `gh issue create` and record it as `<P>` (for cross-linking in Step 5).

### Step 5: Create the Release issue

**Substitution:** Same as Step 4; additionally replace `<P>` with the prep issue number. Leave `<NEXT>` as a literal.

````bash
gh issue create \
  --repo t0k0sh1/ry \
  --title "Release: v<X.Y.Z>" \
  --milestone "v<X.Y.Z>" \
  --body "$(cat <<'EOF'
## Goal

Push the `v<X.Y.Z>` tag to trigger `release.yml` and publish the GitHub Release.

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

Tag `v<X.Y.Z>` pushed; `release.yml` running: <https://github.com/t0k0sh1/ry/actions/workflows/release.yml>. Once the Release publishes, proceed to the cleanup issue.

Then **stop**. Leave this Release issue **open with its `wip` label**. Do not poll the workflow.

## Note

`release.yml` runs a `codeql-gate` preflight job (#1542): CodeQL `event=push` run for the tag's exact SHA must finish `conclusion=success` before publish. If the gate fails, the workflow fails loudly. To override (CodeQL outage only), re-run `release.yml` via `workflow_dispatch` with `skip_codeql_gate=true`.

## Out of scope

Editing `CHANGELOG.md` (prep issue #<P>); closing this Release issue and the milestone (cleanup issue).
EOF
)"
````

Extract the issue number from the URL printed by `gh issue create` and record it as `<R>`.

### Step 6: Create the Release cleanup issue

**Substitution:** Same as Step 4; additionally replace `<R>` with the release issue number. Leave `<this-issue>` as a literal (the cleanup executor substitutes their own issue number at run time).

````bash
gh issue create \
  --repo t0k0sh1/ry \
  --title "Release cleanup: v<X.Y.Z>" \
  --milestone "v<X.Y.Z>" \
  --body "$(cat <<'EOF'
## Goal

Verify v<X.Y.Z> release artifacts and close the milestone.

## Prerequisites

- The `v<X.Y.Z>` tag has been pushed and `release.yml` has finished. Release issue #<R> is still **open with its `wip` label** — closing it is Task 3 below.

## Tasks

In the commands below, `<R>` is the Release issue number and `<this-issue>` is this cleanup issue's own number — substitute both before running.

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

### 3. Close the Release issue

```bash
gh issue edit <R> --remove-label wip
gh issue close <R>
```

### 4. Close the milestone

Precondition: no other open issues in the milestone besides **this cleanup issue itself**.

```bash
MS_NUM=$(gh api 'repos/t0k0sh1/ry/milestones?state=open' \
  --jq '.[] | select(.title == "v<X.Y.Z>") | .number')
gh issue list --milestone "v<X.Y.Z>" --state open --json number,title
```

Excluding **this cleanup issue's own number** (`<this-issue>`), the list must be empty. If any other open issues remain, **ask the user** whether to:

- (a) defer them to a later milestone (`gh issue edit <n> --milestone "v<NEXT>"`), or
- (b) close them, or
- (c) abort the cleanup.

Do not proceed without explicit user direction. Once only this cleanup issue remains open:

```bash
gh api -X PATCH "repos/t0k0sh1/ry/milestones/$MS_NUM" -f state=closed
```

### 5. Close this cleanup issue

```bash
gh issue edit <this-issue> --remove-label wip
gh issue close <this-issue>
```

## Out of scope

Creating the next milestone (deliberately separate); bumping docs/version files (none today).
EOF
)"
````

Extract the issue number from the URL printed by `gh issue create` and record it as `<C>`.

### Step 7: Report

Report `#<P>` (Release prep), `#<R>` (Release), and `#<C>` (Release cleanup) with their URLs. Start work from `#<P>` after running `scripts/claim-issue.sh '#<P>'`. Address `#<C>` only after `release.yml` completes following the `v<X.Y.Z>` tag push.
