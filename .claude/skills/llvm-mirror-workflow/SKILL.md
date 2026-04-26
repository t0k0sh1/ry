---
name: llvm-mirror-workflow
description: LLVM mirror tarball build/update workflow and version-bump checklist. Use when bumping the LLVM version in CI, rebuilding the mirror tarball, or diagnosing setup-llvm action failures. Also covers the --cleanup-tag ban scope.
allowed-tools: Bash(gh release:*), Bash(gh workflow:*)
---

# LLVM Mirror Workflow

Reference for the non-destructive LLVM mirror workflow and version-bump checklist.

---

### LLVM mirror workflow and version-bump checklist

**Source**: #892 / #919 / #934 / #1246
**Tags**: llvm, ci, cache, mirror, version-bump, race-condition

**Rule**: CI fetches LLVM from a GitHub Releases mirror via
`.github/actions/setup-llvm/`. The mirror tarball is built by
`.github/workflows/mirror-llvm-toolchain.yml` (manual `workflow_dispatch`).

The mirror tarball includes `clang-tidy` (added in #934) and `scan-build` / `analyze-build` via `clang-tools-{MAJOR}` (added in #898). **FileCheck is NOT bundled** in the mirror tarball (#897) — the `filecheck` CI job installs it separately via `apt-get install llvm-{MAJOR}-tools` from `apt.llvm.org`. The
`setup-llvm` action accepts an optional `extra-packages` input for
the apt fallback path; the mirror/cache path already contains all
tools. If new tools are needed, add them to
`mirror-llvm-toolchain.yml`'s apt-get line, bump the cache key
version suffix (e.g. `v2` → `v3`), and re-dispatch the mirror workflow.
A follow-up issue tracks adding `llvm-{MAJOR}-tools` to the mirror
tarball to eliminate the extra apt install step.

**Mirror flow is non-destructive (#1246)**: Do NOT reintroduce
`gh release delete --cleanup-tag` in the mirror workflow. The flow is:

1. Each dispatch computes `next_rev = max(existing rev<N>) + 1` via
   `gh release list | jq` with an anchored regex
   `^llvm-toolchain-<V>-rev[0-9]+$` so malformed tags are skipped.
2. Creates an immutable `llvm-toolchain-<V>-rev<N>` release that is
   never deleted (audit trail + rollback source).
3. Promotes the stable pointer `llvm-toolchain-<V>` in place via
   `gh release upload --clobber`. This is NOT atomic — GitHub's API
   has no atomic replace, so `--clobber` is implemented as a
   sequential DELETE-then-POST per file. The missing-asset window
   per file is the POST duration: sub-second for the `.sha256`, but
   seconds to a few minutes for the LLVM tarball (300–500 MB). That
   is still orders of magnitude better than the previous 3–5 minute
   full-release gap produced by `gh release delete --cleanup-tag`.
4. A workflow-level `concurrency: group: mirror-llvm-${{ inputs.llvm_version }}`
   guard serialises dispatches for the same version so two racing
   runs cannot both compute the same `next_rev`.

The `force` input was removed — idempotent re-dispatches simply append
a new rev. Rollback is a manual GitHub UI operation: edit the stable
release and replace its assets with those of a prior `rev<N>` release.

**Known follow-up (setup-llvm hardening)**: During the tarball upload
window the stable release itself still exists, so
`.github/actions/setup-llvm/action.yml`'s `gh release view` check
succeeds and does not trigger the `apt.llvm.org` fallback — but the
subsequent `gh release download` gets a 404 and the job hard-fails.
The previous `--cleanup-tag` flow was actually softer here because
the release was fully gone, so `gh release view` failed and the apt
fallback kicked in. Because mirror dispatches are rare and manual,
this was accepted in #1246, but a follow-up issue should harden
`setup-llvm` to fall back on download failure too (e.g. retry the
`gh release view`/`download` pair, or unconditionally try apt on any
download failure). Do NOT re-introduce `--cleanup-tag` as a fix for
this — that would regress the window from seconds to minutes back
to 3–5 minutes.

Version bump checklist — update `env.LLVM_VERSION` (and
`env.LLVM_SHA256_SHORT` when non-empty) in:
- `.github/workflows/ci.yml`
- `.github/workflows/codeql.yml`

Cache key format: `llvm-${VERSION}-linux-x86_64-v3-${SHA256_SHORT}`.
`restore-keys` is intentionally omitted: a partial cache hit would
restore a mismatched LLVM version, causing build failures or silent ABI
mismatches. An exact-match-only policy guarantees the correct toolchain.

`release.yml` still uses `apt.llvm.org` directly and is not yet
migrated — tracked as a separate follow-up from #892.

---

### `--cleanup-tag` ban is scoped to LLVM mirror, not project-wide

**Source**: #1365, #1372
**Tags**: ci, github-actions, release, cleanup-tag

**Rule**: The "do not reintroduce `--cleanup-tag`" warning above
applies only to the LLVM mirror workflow, where the stable release
pointer must remain downloadable continuously for many concurrent CI
consumers. It is **not** a project-wide ban.

For future workflows that need a one-shot retirement of an obsolete
tag (e.g. dropping a leftover prerelease tag once it is replaced by a
stable one), `--cleanup-tag` is the cleaner choice — it removes both
the release and the matching git tag in one call, avoiding stale
entries in `git tag -l`. After the dev-release nightly retirement
(#1372), the project currently has no non-mirror use sites, but this
guidance applies if one is added later.

When evaluating a new use site, ask: does the deletion target compete
with concurrent fetchers that need the tag/release available
continuously? If yes (mirror-style), avoid `--cleanup-tag`. If no
(one-shot retirement), `--cleanup-tag` is the cleaner choice.
