---
name: ci-image-workflow
description: GHCR pre-baked CI image (ry-ci, ry-ci-glibc-old) build/update workflow, LLVM version-bump procedure, and rollback. Use when "LLVM 21 を 22 に上げる" / "ci.Dockerfile を変更する" / "ry-ci image rebuild" / "image tag rev<N>" / "container CI が動かない" / image bootstrapping / no-apt 制約のいずれかを扱うとき。
allowed-tools: Bash
---

# CI Image Build & Update Workflow

This project ships two GHCR-hosted CI base images that pre-bake the
entire Linux toolchain so workflows do **not** invoke `apt` /
`apt-get`. They were introduced by #1505 after the 2026-05-02 Ubuntu
apt mirror outage broke CI for hours.

## Image overview

| Image | Base | glibc | Used by | Source |
|-------|------|-------|---------|--------|
| `ghcr.io/<owner>/ry-ci` | `gcc:14-trixie` | 2.40 | `ci.yml`, `codeql.yml`, `docker/Dockerfile` (dev) | `docker/ci.Dockerfile` |
| `ghcr.io/<owner>/ry-ci-glibc-old` | `gcc:14-bookworm` | 2.36 | `release.yml` Linux jobs | `docker/ci-glibc-old.Dockerfile` |

Both images bake in:

- `clang/clang++/clang-tidy/scan-build/FileCheck` (LLVM 21 source-built,
  installed at `/usr/local/llvm/`)
- `cmake` (`/opt/cmake/bin/cmake`), `ninja` (`/opt/ninja/bin/ninja`),
  `ccache` (`/usr/local/bin/ccache`) — all from upstream
  `github.com` releases (no apt)
- `OpenSSL 3.x` source build at `/opt/openssl`
- `cppcheck` source build at `/opt/cppcheck` (only `ry-ci`; release
  does not lint)
- A pinned GoogleTest source tarball at `$RY_VENDORED_GTEST_TARBALL`
  so `FetchContent_Declare(googletest URL ...)` skips the network

`CC`, `CXX`, `PATH`, `LD_LIBRARY_PATH`, `OPENSSL_ROOT_DIR`, `LLVM_DIR`
are pre-set as ENV in the final stage of each Dockerfile.

## Tag scheme

```
ghcr.io/<owner>/ry-ci:llvm-<MAJOR>-rev<N>   ← immutable
ghcr.io/<owner>/ry-ci:llvm-<MAJOR>          ← stable pointer (mutable)
ghcr.io/<owner>/ry-ci:latest                ← stable pointer (mutable)
```

`<N>` is computed by `build-ci-image.yml` as
`max(existing rev<N> for LLVM_MAJOR) + 1` across both images, mirroring
the convention used by the now-removed LLVM mirror. Stable pointers
(`llvm-<MAJOR>` and `latest`) are updated via
`docker buildx imagetools create`, which is sub-second per push (it
just rewrites the manifest list, not image layers).

- **CI workflows** (`ci.yml`, `codeql.yml`, dev `docker/Dockerfile`)
  pin to the **stable pointer** (e.g. `:llvm-21`). Artifacts never
  leave the runner, so auto-tracking the latest image is desirable
  and roll-forward/back happens without editing workflow YAML.
- **`release.yml`** pins to the **immutable `:llvm-<MAJOR>-rev<N>`**
  (e.g. `:llvm-21-rev3`). Release artifacts must be byte-reproducible
  across re-runs of the workflow on older `vX.Y.Z` tags, which the
  mutable pointer cannot guarantee. See
  `.claude/rules/ci-workflows.md` "Release container must pin to
  immutable `:llvm-<MAJOR>-rev<N>` tag" (#1508). The pin is bumped
  during Release prep — see "How to update release.yml's pin" below.

## Build trigger

`build-ci-image.yml` runs:

1. **`workflow_dispatch`** — manual UI-driven build with
   `llvm_version` input. Canonical way to refresh the image after a
   Dockerfile change or LLVM bump.
2. **`push` to `main`** — auto-rebuild when `docker/ci.Dockerfile`,
   `docker/ci-glibc-old.Dockerfile`, or `build-ci-image.yml` changes
   on `main`. Guards against a Dockerfile-affecting PR being merged
   without a subsequent rebuild.

The workflow has 3 jobs:

- `compute-tag` — queries GHCR for the highest existing `rev<N>` and
  outputs `llvm_major` and `rev_tag` (e.g. `llvm-21-rev3`)
- `build` — 4-way matrix (image × arch); each cell builds on a
  **native** runner (no QEMU emulation, which would extend LLVM
  source-build by 5-10×) and pushes per-arch by digest
- `manifest` — combines the per-arch digests into a single multi-arch
  manifest list and tags it `<rev>`, `llvm-<MAJOR>`, `latest`

## When to rebuild

| Change | Rebuild trigger |
|--------|-----------------|
| LLVM minor/patch version bump | Manual `workflow_dispatch` with new `llvm_version` |
| OpenSSL / cmake / ninja / ccache / cppcheck version bump | Manual `workflow_dispatch` (pinned versions live in `ENV`/`ARG` lines in the Dockerfile) |
| New tool added to image | Manual `workflow_dispatch` |
| Bug fix in Dockerfile (tarball URL, build flag) | Auto-rebuild on `push` to `main` (path filter triggers it) |

For PR-feature-branch testing of Dockerfile changes, the
`workflow_dispatch` button is also accessible from the PR's Actions
tab — point it at the branch ref.

## How to bump LLVM

1. Update `LLVM_VERSION` in both Dockerfiles
   (`docker/ci.Dockerfile`, `docker/ci-glibc-old.Dockerfile`) — they
   share the same major version pinning.
2. Update the default in `build-ci-image.yml`
   (`inputs.llvm_version.default`).
3. Open a PR with the Dockerfile changes. CI will fail because the
   `:llvm-<MAJOR>` tag does not yet have the new version — that is
   expected.
4. **Before merging**, manually trigger `Build CI Image` workflow on
   the feature branch (Actions → Build CI Image → Run workflow → pick
   branch). Wait for it (60-90 minutes for both arches × both images).
5. Verify the new tag is visible:
   ```bash
   gh api /users/<owner>/packages/container/ry-ci/versions \
     --jq '.[].metadata.container.tags[]'
   ```
6. Re-run CI on the PR — it should now pass.
7. Merge the PR. The `push` to `main` will auto-trigger another
   rebuild; this is fine (idempotent, the `rev<N>` just bumps).
8. If the LLVM **major** version changed (e.g. 21 → 22), also update
   every `:llvm-21*` reference (both the stable `:llvm-21` and any
   immutable `:llvm-21-rev<N>` pin in `release.yml`) across
   `.github/workflows/`, `docker/Dockerfile`, and `AGENTS.md` to the
   `:llvm-22*` form in the same PR. After `build-ci-image.yml`
   publishes the first new-major rev, set `release.yml`'s pin to
   `:llvm-22-rev<N>` (the new rev number — typically `rev1` since
   `compute-tag` resets per major).

## How to add a new tool to the image

1. Add a build / install stage to `docker/ci.Dockerfile` (and
   `docker/ci-glibc-old.Dockerfile` if release also needs it). Always
   pull from a `github.com` release — never `apt-get install`.
2. If the tool is configured by environment, add an `ENV` line in
   the final stage. Document in the Dockerfile header.
3. Manually trigger `Build CI Image` on the feature branch and wait
   for it to publish.
4. Re-run CI on the PR.

## Rollback

If a new image breaks CI:

1. Find the previous `rev<N>`:
   ```bash
   gh api /users/<owner>/packages/container/ry-ci/versions \
     --jq '.[].metadata.container.tags[]' | sort -V
   ```
2. Re-tag the stable pointer at the older revision (run from a
   workstation logged in to GHCR, or paste into a `workflow_dispatch`
   shell job):
   ```bash
   docker buildx imagetools create \
     -t ghcr.io/<owner>/ry-ci:llvm-21 \
     ghcr.io/<owner>/ry-ci:llvm-21-rev<previous-N>
   docker buildx imagetools create \
     -t ghcr.io/<owner>/ry-ci:latest \
     ghcr.io/<owner>/ry-ci:llvm-21-rev<previous-N>
   ```
3. CI re-runs without changing any workflow YAML — they all reference
   the stable pointer.
4. File a follow-up issue to fix forward; do not leave the rollback
   pinned indefinitely (immutable `rev<N>` tags are not garbage-collected
   automatically, but the stable pointer should track the latest known-good).

## How to update release.yml's pin

`release.yml`'s `container:` line (#1508) uses an immutable
`:llvm-<MAJOR>-rev<N>` tag for byte-reproducibility across re-runs.
Update it during Release prep, **not** during regular feature work:
a stale pin during feature work just means the next release inherits
the previous image, which is correct (CI work and release re-runs
have different reproducibility constraints — see
`.claude/rules/ci-workflows.md`).

1. Find the latest published rev. Use the public GHCR registry —
   no auth needed, so this works for any maintainer regardless of
   `gh` PAT scope:

   ```bash
   curl -s "https://ghcr.io/token?scope=repository:t0k0sh1/ry-ci-glibc-old:pull" \
     | jq -r '.token' \
     | { read TOKEN; curl -s -H "Authorization: Bearer ${TOKEN}" "https://ghcr.io/v2/t0k0sh1/ry-ci-glibc-old/tags/list"; } \
     | jq -r '.tags[]' | grep -E '^llvm-[0-9]+-rev[0-9]+$' | sort -V | tail -1
   ```

2. Compare the output with the literal in `release.yml`'s `container:`
   line (currently around L32, the `format(...)` argument). If
   different, open a PR that updates only this line. Use a
   `chore/<issue>-bump-release-image-rev` branch.

3. Do **not** auto-update from a workflow run. Even though
   `build-ci-image.yml` knows the new rev, the value must be a literal
   string in `release.yml` at the release tag's commit — that is what
   makes re-runs reproducible. Dynamic resolution
   (`needs.<job>.outputs.<n>`, file reads, etc.) defeats the purpose
   because re-runs would resolve the dynamic value at re-run time.

4. The `preparing-for-release` skill embeds this check in the Release
   prep issue body's task list — see
   `.claude/skills/preparing-for-release/SKILL.md` Step 4.

## Local development image

`docker/run.sh` builds a thin local-dev image (`ry-linux-dev:latest`)
that **inherits** from `ghcr.io/<owner>/ry-ci:llvm-21`. The dev image
adds: a non-root `ubuntu` user matching the host UID, a writable
ccache directory, and an entrypoint script. Build time is ~30 seconds
(vs. 60-90 minutes for the base image) because the heavy toolchain is
pulled, not built.

To test base-image changes locally:

```bash
docker build \
  --build-arg CI_IMAGE_OWNER=<your-fork-owner> \
  --build-arg CI_IMAGE_TAG=llvm-21-rev<N> \
  -t ry-linux-dev:test \
  docker/
```

## Pitfalls

- **No apt anywhere**. The central property of the images is that no
  Linux job ever invokes `apt-get`. If you find yourself adding
  `apt-get install` to a Dockerfile or workflow, you are violating
  the property the images were built to provide. Pre-build from
  source or pull a release binary in the appropriate Dockerfile
  stage instead.
- **Forgetting to rebuild before merging**. The path filter on `push`
  to `main` covers post-merge auto-rebuild, but a PR that bumps a
  tarball URL will fail CI until the rebuild succeeds. Always
  manually `workflow_dispatch` on the feature branch first.
- **`actions/checkout@v4` requires `git`**. Both Dockerfiles install
  `git` in the final stage; do not remove it even if it looks unused.
- **Multi-arch `imagetools create` requires both per-arch builds to
  succeed**. If one arch fails, the manifest job will not push, and
  the previous `rev<N-1>` remains latest. This is the correct
  failure mode — do not work around it.
- **`cache-to: type=gha,mode=max`** can fill the GitHub Actions cache
  quota on heavy LLVM rebuilds. The cache is per-(image, arch) scope,
  so old caches age out automatically; do not aggressively delete
  caches manually.
- **release.yml uses `ry-ci-glibc-old`, not `ry-ci`**. Mixing them up
  will break Linux release artifacts on older distros (Ubuntu 22.04,
  RHEL 9). See the entry in `.claude/rules/ci-workflows.md`.
