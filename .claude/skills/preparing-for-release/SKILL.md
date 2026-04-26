---
name: preparing-for-release
description: Open the two issues (Release prep + Release) under a version's milestone to drive the tag-push release flow. Pass the version as the argument (e.g. `0.0.14` or `v0.0.14`); the skill asks if absent.
allowed-tools: Bash(gh issue:*), Bash(gh api:*)
metadata:
  short-description: Open prep + release issues for a target version
---

# Preparing for Release

Open the **Release prep** issue and the **Release** issue under the target version's milestone, kicking off the tag-push driven release flow described in `AGENTS.md` "リリースワークフロー".

> **Note:** This skill only files issues. The actual CHANGELOG aggregation and tag push are performed in those issues using the standard issue-driven flow (`git-claim-issue` → feature branch → PR → `git-merge-pr`).

## Repository

- owner: `t0k0sh1`
- repo: `ry`

## Inputs

User input: `$ARGUMENTS` (release version, e.g. `0.0.14` or `v0.0.14`).

If no version is supplied, **ask the user** before proceeding. Do NOT guess.

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
  --jq '[.[] | select(.title == "Release prep: v<X.Y.Z>" or .title == "Release: v<X.Y.Z>")]'
```

- Strict title match is intentional — `--search` is full-text and can hit unrelated issues.
- If any element is returned, report the existing issue number(s) and stop. Do not create duplicates.

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

Aggregate `changelog.d/` fragments into `CHANGELOG.md` and finalize the `[<X.Y.Z>] - YYYY-MM-DD` section so that the release tag can be cut.

## Scope

This issue follows the standard issue-driven development flow:

1. Claim with `git-claim-issue`
2. Create a feature branch from `main`
3. Do the work below
4. Self-verify
5. Open a PR to `main` and merge with `git-merge-pr`

The release tag itself is **out of scope** — it is handled by the sibling Release issue.

## Tasks

### 1. Run the assembly script

```bash
scripts/assemble-changelog.sh
```

This collapses every fragment in `changelog.d/*.md` into the `[Unreleased]` section of `CHANGELOG.md` and deletes the consumed fragments.

### 2. Rename `[Unreleased]` to the released version

In `CHANGELOG.md`, change the heading:

```diff
-## [Unreleased]
+## [<X.Y.Z>] - YYYY-MM-DD
```

Use today's date (UTC) in `YYYY-MM-DD` form.

### 3. Insert a fresh empty `[Unreleased]` section

Above the new `[<X.Y.Z>]` heading, insert:

```markdown
## [Unreleased]
```

(Body intentionally empty — future fragments will repopulate it.)

### 4. Update comparison links at the bottom of `CHANGELOG.md`

```diff
-[Unreleased]: https://github.com/t0k0sh1/ry/compare/v<PREV>...HEAD
+[Unreleased]: https://github.com/t0k0sh1/ry/compare/v<X.Y.Z>...HEAD
+[<X.Y.Z>]: https://github.com/t0k0sh1/ry/compare/v<PREV>...v<X.Y.Z>
```

`<PREV>` is the previous released version (top of the existing comparison link list).

## Verification

- `git diff CHANGELOG.md` shows: new `[<X.Y.Z>]` heading + new empty `[Unreleased]` + updated comparison links
- `changelog.d/` no longer contains assembled fragments
- `git status` reflects the deleted fragment files
- `cmake --preset default && cmake --build build && ./build/ry_tests && ./build/ry test -p` passes (sanity check; sanitizer / fuzzer runs are not required for a docs-only change)

## Out of scope

- Bumping any VERSION file (none exists; CMake defaults `RY_VERSION` to `0.0.0` and CI injects from the tag)
- Pushing the `v<X.Y.Z>` tag — handled by the Release issue
EOF
)"
````

Capture the new issue number from the URL printed by `gh issue create` — call it `<P>` — for the cross-link in Step 5.

### Step 5: Create the Release issue

**Substitution rules before invocation:**

- Replace every `<X.Y.Z>` placeholder with the validated version.
- Replace every `<P>` placeholder with the prep issue number captured in Step 4.
- Leave `<NEXT>` as a literal — the release worker fills it in only if they choose to defer remaining open issues.

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

Tell the user:

- The tag `v<X.Y.Z>` has been pushed
- `release.yml` is now running — link: <https://github.com/t0k0sh1/ry/actions/workflows/release.yml>
- They should watch the workflow, and once the GitHub Release is published, close the milestone (per `/release-orchestrator` "マイルストーン close ポリシー")

Then **stop**. Do not poll the workflow, do not auto-close the milestone — those steps belong to the human owner.

## Out of scope

- Editing `CHANGELOG.md` (done by prep issue #<P>)
- Closing the milestone (done by the human owner after the GitHub Release publishes)
EOF
)"
````

Capture the new issue number from the URL printed by `gh issue create` — call it `<R>`.

### Step 6: Report

Report to the user with:

- Release prep issue: `#<P> Release prep: v<X.Y.Z>` and its URL
- Release issue: `#<R> Release: v<X.Y.Z>` and its URL
- A note that work should start with `#<P>` (claim with `git-claim-issue`).
