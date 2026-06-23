---
name: ci-image-workflow
description: GHCR CI image update workflow for ry-ci and ry-ci-glibc-old.
allowed-tools: Bash
---

# CI Image Workflow

Images:

- `ghcr.io/<owner>/ry-ci`: normal CI, CodeQL, dev Docker; source `docker/ci.Dockerfile`.
- `ghcr.io/<owner>/ry-ci-glibc-old`: release Linux artifacts; source `docker/ci-glibc-old.Dockerfile`.

Tags:

- Immutable: `:llvm-<MAJOR>-rev<N>`
- Mutable pointers: `:llvm-<MAJOR>`, `:latest`
- `release.yml` uses immutable `ry-ci-glibc-old`; CI/dev use mutable pointers.

## Build Flow

`build-ci-image.yml`:

1. `compute-tag`: finds next `rev<N>`.
2. `build`: image x arch matrix.
3. `manifest`: publishes multi-arch manifests and updates mutable pointers.

Trigger manually with `workflow_dispatch` for feature-branch testing. Push to `main` auto-rebuilds when Dockerfiles or `build-ci-image.yml` change.

## LLVM Bump

1. Update `LLVM_VERSION` in both Dockerfiles.
2. Update `build-ci-image.yml` `inputs.llvm_version.default`.
3. Trigger `Build CI Image` on the feature branch and wait for publication.
4. Verify tags:
   ```bash
   gh api /users/<owner>/packages/container/ry-ci/versions \
     --jq '.[].metadata.container.tags[]'
   ```
5. Re-run PR CI.
6. If LLVM major changed, update every `:llvm-<old>*` reference in `.github/workflows/` and `docker/Dockerfile`; set `release.yml` to the first new-major immutable rev.

## Tool Or Version Change

1. Edit `docker/ci.Dockerfile`; also edit `docker/ci-glibc-old.Dockerfile` when release needs the tool.
2. Add final-stage `ENV` only when the tool requires it.
3. Trigger `Build CI Image` on the feature branch.
4. Re-run PR CI after the tag publishes.

## Rollback

If a new image breaks CI, re-point mutable tags to the previous immutable rev:

```bash
docker buildx imagetools create \
  -t ghcr.io/<owner>/ry-ci:llvm-21 \
  ghcr.io/<owner>/ry-ci:llvm-21-rev<previous-N>
docker buildx imagetools create \
  -t ghcr.io/<owner>/ry-ci:latest \
  ghcr.io/<owner>/ry-ci:llvm-21-rev<previous-N>
```

File a fix-forward issue; do not leave rollback as the long-term state.

## Release Pin

Update `.github/workflows/release.yml` only during release prep:

```bash
curl -s "https://ghcr.io/token?scope=repository:t0k0sh1/ry-ci-glibc-old:pull" \
  | jq -r '.token' \
  | { read TOKEN; curl -s -H "Authorization: Bearer ${TOKEN}" "https://ghcr.io/v2/t0k0sh1/ry-ci-glibc-old/tags/list"; } \
  | jq -r '.tags[]' | grep -E '^llvm-[0-9]+-rev[0-9]+$' | sort -V | tail -1
```

The pin must be a literal immutable tag at the release commit.

## Local Dev Image

`docker/run.sh` builds `ry-linux-dev:latest` from `ghcr.io/<owner>/ry-ci:llvm-21`. To test a base-image rev locally:

```bash
docker build --build-arg CI_IMAGE_OWNER=<owner> --build-arg CI_IMAGE_TAG=llvm-21-rev<N> -t ry-linux-dev:test docker/
```
