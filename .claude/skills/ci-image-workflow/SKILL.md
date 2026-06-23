---
name: ci-image-workflow
description: GHCR pre-baked CI image (ry-ci, ry-ci-glibc-old) build/update workflow, LLVM version-bump procedure, and rollback. Use when bumping LLVM versions, changing ci.Dockerfile, rebuilding ry-ci images, working with image tag rev<N>, troubleshooting container CI, bootstrapping images, or dealing with the no-apt constraint.
allowed-tools: Bash
---

# CI Image Build & Update Workflow

## Image overview

| Image | Base | glibc | Used by | Source |
|-------|------|-------|---------|--------|
| `ghcr.io/<owner>/ry-ci` | `gcc:14-trixie` | 2.40 | `ci.yml`, `codeql.yml`, `docker/Dockerfile` (dev) | `docker/ci.Dockerfile` |
| `ghcr.io/<owner>/ry-ci-glibc-old` | `gcc:14-bookworm` | 2.36 | `release.yml` Linux jobs | `docker/ci-glibc-old.Dockerfile` |

Both images bake in:

- `clang/clang++/FileCheck` + ASan/UBSan/fuzzer compiler-rt (LLVM 21 source-built, installed at `/usr/local/llvm/`). `clang-tools-extra` (clang-tidy / clangd) is not built; `scan-build` / `scan-view` are removed in post-install cleanup; TSan / MSan / DFSan / HWASan are excluded via `COMPILER_RT_SANITIZERS_TO_BUILD`. Only ASan + UBSan + libFuzzer are included for CI use.
- `cmake` (`/opt/cmake/bin/cmake`), `ninja` (`/opt/ninja/bin/ninja`), `ccache` (`/usr/local/bin/ccache`) — all from upstream `github.com` releases (no apt)
- `OpenSSL 3.x` source build at `/opt/openssl`
- `cppcheck` source build at `/opt/cppcheck` (only `ry-ci`; release does not lint)
- A pinned GoogleTest source tarball at `$RY_VENDORED_GTEST_TARBALL`

`CC`, `CXX`, `PATH`, `LD_LIBRARY_PATH`, `OPENSSL_ROOT_DIR`, `LLVM_DIR` are pre-set as ENV in the final stage of each Dockerfile.

## Tag scheme

```
ghcr.io/<owner>/ry-ci:llvm-<MAJOR>-rev<N>   ← immutable
ghcr.io/<owner>/ry-ci:llvm-<MAJOR>          ← stable pointer (mutable)
ghcr.io/<owner>/ry-ci:latest                ← stable pointer (mutable)
```

`<N>` is computed by `build-ci-image.yml` as `max(existing rev<N> for LLVM_MAJOR) + 1` across both images. Stable pointers are updated via `docker buildx imagetools create` (manifest rewrite only, sub-second).

- **CI workflows** (`ci.yml`, `codeql.yml`, dev `docker/Dockerfile`) pin to the **stable pointer** (e.g. `:llvm-21`).
- **`release.yml`** pins to the **immutable `:llvm-<MAJOR>-rev<N>`** (e.g. `:llvm-21-rev3`) for byte-reproducibility. See `.claude/rules/ci-workflows.md` "Release container must pin to immutable `:llvm-<MAJOR>-rev<N>` tag" (#1508). The pin is bumped during Release prep — see "How to update release.yml's pin" below.

## Build trigger

`build-ci-image.yml` runs on:

1. **`workflow_dispatch`** — manual UI-driven build with `llvm_version` input. Canonical way to refresh the image after a Dockerfile change or LLVM bump.
2. **`push` to `main`** — auto-rebuild when `docker/ci.Dockerfile`, `docker/ci-glibc-old.Dockerfile`, or `build-ci-image.yml` changes on `main`.

Jobs: `compute-tag` (queries GHCR for highest existing `rev<N>`) → `build` (4-way matrix: image × arch, native runners, no QEMU) → `manifest` (combines per-arch digests into multi-arch manifest list).

## When to rebuild

| Change | Rebuild trigger |
|--------|-----------------|
| LLVM minor/patch version bump | Manual `workflow_dispatch` with new `llvm_version` |
| OpenSSL / cmake / ninja / ccache / cppcheck version bump | Manual `workflow_dispatch` |
| New tool added to image | Manual `workflow_dispatch` |
| Bug fix in Dockerfile (tarball URL, build flag) | Auto-rebuild on `push` to `main` |

For PR-feature-branch testing of Dockerfile changes, `workflow_dispatch` is also accessible from the PR's Actions tab.

## How to bump LLVM

1. Update `LLVM_VERSION` in both Dockerfiles (`docker/ci.Dockerfile`, `docker/ci-glibc-old.Dockerfile`).
2. Update the default in `build-ci-image.yml` (`inputs.llvm_version.default`).
3. PR creation is outside this skill. Do not create, propose, or include PR creation in a plan. Once a PR already exists, its CI will fail because the `:llvm-<MAJOR>` tag does not yet have the new version — that is expected.
4. **Before merging an existing PR**, manually trigger `Build CI Image` workflow on the feature branch (Actions → Build CI Image → Run workflow → pick branch). Wait for it (60-90 minutes for both arches × both images).
5. Verify the new tag is visible:
   ```bash
   gh api /users/<owner>/packages/container/ry-ci/versions \
     --jq '.[].metadata.container.tags[]'
   ```
6. Re-run CI on the PR — it should now pass.
7. Merge the PR. The `push` to `main` will auto-trigger another rebuild (idempotent, `rev<N>` just bumps).
8. If the LLVM **major** version changed (e.g. 21 → 22), also update every `:llvm-21*` reference across `.github/workflows/`, `docker/Dockerfile`, and `AGENTS.md` to the `:llvm-22*` form in the same PR. After `build-ci-image.yml` publishes the first new-major rev, set `release.yml`'s pin to `:llvm-22-rev<N>` (typically `rev1`).

## How to add a new tool to the image

1. Add a build/install stage to `docker/ci.Dockerfile` (and `docker/ci-glibc-old.Dockerfile` if release also needs it). Pull from a `github.com` release — never `apt-get install`.
2. If the tool is configured by environment, add an `ENV` line in the final stage.
3. Manually trigger `Build CI Image` on the feature branch and wait for it to publish.
4. Re-run CI on the PR.

## Rollback

If a new image breaks CI:

1. Find the previous `rev<N>`:
   ```bash
   gh api /users/<owner>/packages/container/ry-ci/versions \
     --jq '.[].metadata.container.tags[]' | sort -V
   ```
2. Re-tag the stable pointer:
   ```bash
   docker buildx imagetools create \
     -t ghcr.io/<owner>/ry-ci:llvm-21 \
     ghcr.io/<owner>/ry-ci:llvm-21-rev<previous-N>
   docker buildx imagetools create \
     -t ghcr.io/<owner>/ry-ci:latest \
     ghcr.io/<owner>/ry-ci:llvm-21-rev<previous-N>
   ```
3. CI re-runs without changing any workflow YAML — they all reference the stable pointer.
4. File a follow-up issue to fix forward; do not leave the rollback pinned indefinitely.

## How to update release.yml's pin

`release.yml`'s `container:` line uses an immutable `:llvm-<MAJOR>-rev<N>` tag for byte-reproducibility. Update during Release prep, **not** during regular feature work.

1. Find the latest published rev:
   ```bash
   curl -s "https://ghcr.io/token?scope=repository:t0k0sh1/ry-ci-glibc-old:pull" \
     | jq -r '.token' \
     | { read TOKEN; curl -s -H "Authorization: Bearer ${TOKEN}" "https://ghcr.io/v2/t0k0sh1/ry-ci-glibc-old/tags/list"; } \
     | jq -r '.tags[]' | grep -E '^llvm-[0-9]+-rev[0-9]+$' | sort -V | tail -1
   ```
2. Compare with the literal in `release.yml`'s `container:` line (currently around L32). If different, update only this line. PR creation is outside this skill.
3. Do **not** auto-update from a workflow run. The value must be a literal string in `release.yml` at the release tag's commit — dynamic resolution (`needs.<job>.outputs.<n>`, file reads) defeats reproducibility.
4. The `preparing-for-release` skill embeds this check in the Release prep issue body's task list — see `.claude/skills/preparing-for-release/SKILL.md` Step 4.

## Local development image

`docker/run.sh` builds a thin local-dev image (`ry-linux-dev:latest`) that inherits from `ghcr.io/<owner>/ry-ci:llvm-21`. Build time is ~30 seconds (vs. 60-90 minutes for the base image).

To test base-image changes locally:

```bash
docker build \
  --build-arg CI_IMAGE_OWNER=<your-fork-owner> \
  --build-arg CI_IMAGE_TAG=llvm-21-rev<N> \
  -t ry-linux-dev:test \
  docker/
```

## Pitfalls

- **No apt anywhere** — including Debian's own apt. The central property of the images is that no Linux job ever invokes `apt-get`, in Dockerfiles or in workflow steps. Pre-build from source or pull a release binary in the appropriate Dockerfile stage instead. The policy is **distro-agnostic**: the base images are Debian (`gcc:14-trixie` and `gcc:14-bookworm`), not Ubuntu, so #1505's original Ubuntu mirror outage did not directly hit image builds — but Debian mirrors can also fail, and a partial unban was re-evaluated in #1506 and rejected because: (a) `gcc:14-bookworm`'s `bookworm-updates` / `bookworm-security` apt repositories already return GPG signature errors out of the box, so `apt-get update` is non-functional in `ci-glibc-old.Dockerfile` without keyring fixes — partial unban would break release CI image builds today; (b) `cppcheck` advanced from `2.16.0` (source-baked) to trixie-shipped `2.17.1`, with unknown impact on the `normalCheckLevelMaxBranches` suppression in `.claude/rules/ci-workflows.md` ("cppcheck 2.16 raises `normalCheckLevelMaxBranches` to a hard exit") — asymmetric lint risk for a 12-line stage; (c) `snapshot.debian.org` does not state reproducible builds as a design goal nor a long-term retention policy — apt version float would silently degrade scratch-rebuild reproducibility; (d) the realistic apt-eligible delta is only `cmake` + `ninja` (~18 Dockerfile lines) and not enough to justify the policy-boundary maintenance cost. Re-open only if the bookworm GPG situation is resolved upstream **and** the `cppcheck` suppression is decoupled from a specific minor version.
- **Forgetting to rebuild before merging**. Always manually `workflow_dispatch` on the feature branch first — the path filter on `push` to `main` covers post-merge auto-rebuild, but a PR that bumps a tarball URL will fail CI until the rebuild succeeds.
- **`actions/checkout@v4` requires `git`**. Both Dockerfiles install `git` in the final stage; do not remove it even if it looks unused.
- **Multi-arch `imagetools create` requires both per-arch builds to succeed**. If one arch fails, the manifest job will not push, and the previous `rev<N-1>` remains latest. Do not work around it.
- **`cache-to: type=gha,mode=max`** can fill the GitHub Actions cache quota on heavy LLVM rebuilds. Old caches age out automatically; do not aggressively delete caches manually.
- **release.yml uses `ry-ci-glibc-old`, not `ry-ci`**. Mixing them up will break Linux release artifacts on older distros (Ubuntu 22.04, RHEL 9). See the entry in `.claude/rules/ci-workflows.md`.
- **Stale distro / glibc references in `docker/**` and `.claude/skills/**/SKILL.md`**. When the Dockerfile base image changes, sweep both directories for the previous distro name and glibc version. Suggested grep: `grep -rni '<old-distro>\|<old-glibc>' docker/ .claude/skills/`.
