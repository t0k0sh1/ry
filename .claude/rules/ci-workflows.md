---
paths:
  - ".github/workflows/**/*.yml"
  - ".github/actions/**/*.yml"
  - ".github/codeql/**/*.yml"
---

# CI Workflows

### CI images

**Tags**: ci, github-actions, container, ghcr

Linux CI uses `ghcr.io/${{ github.repository_owner }}/ry-ci:llvm-21`; release Linux uses `ry-ci-glibc-old:llvm-21-rev<N>`. Common tools go in `docker/ci.Dockerfile`; release-needed tools also go in `docker/ci-glibc-old.Dockerfile`. Rebuild with `build-ci-image.yml`.

### ccache

**Tags**: ci, ccache, container, github-actions, cache

Linux `actions/cache@v4` path is `/root/.cache/ccache` because jobs run inside the container. macOS release jobs may use `hendrikmuhs/ccache-action@v1`.

### Release Linux image

**Tags**: ci, release, glibc, container, abi, linux

Release artifacts must use `ry-ci-glibc-old` (bookworm, glibc 2.36), not normal `ry-ci` (trixie, glibc 2.40).

### Release image pin

**Tags**: ci, release, container, ghcr, immutable-tag, reproducibility

`release.yml` must pin a literal immutable `:llvm-<MAJOR>-rev<N>` tag. Do not use mutable `:llvm-<MAJOR>` or dynamic lookup for releases.

### Scheduled workflows

**Tags**: ci, github-actions, schedule, ccache, gotcha

In `on: schedule:` workflows, `github.ref_name` is the default branch even if checkout uses another `ref:`. Use the resolved checkout ref output, or set cache `save: true` unconditionally.

### CodeQL dashboard

**Tags**: ci, github-actions, codeql, dashboard, schedule, security

Keep CodeQL `push: branches: [main]`; PR/manual-only analysis leaves Code Scanning stale after merge.

### Release CodeQL gate

**Tags**: ci, github-actions, gh-api, cross-workflow-gate, codeql, race-condition, polling

Release gating on CodeQL must filter runs to `event=push`, poll first for run existence and then completion, grant `actions: read` at job scope, and let downstream release accept gate `success` or `skipped` via `always()`.

### CodeQL build target

**Tags**: ci, codeql, static-analysis, performance, target

C/C++ CodeQL builds target `ry` on both `pull_request` and push to `main`. This keeps Code Scanning focused on production CLI/runtime TUs; tests, fuzz harnesses, and generated build-tree probes are covered by their own jobs. For compiled languages with manual builds, the build step is the enforcement point; `.github/codeql/codeql-config.yml` `paths-ignore` is a supporting filter.

Verify: `grep -n 'cmake --build build --target ry --parallel' .github/workflows/codeql.yml` matches.

### Artifact patterns

**Tags**: ci, github-actions, actions/download-artifact, glob, multi-image, multi-arch

`actions/download-artifact@v4` `pattern:` is a glob. Use separators that cannot appear in matrix values; prefer `__` over `-` for image/arch artifact names.

### cppcheck

**Tags**: ci, cppcheck, lint, gotcha, error-exitcode

Suppress cppcheck 2.16 `normalCheckLevelMaxBranches` at the workflow invocation, not in `.cppcheck-suppressions`.

### Warn-only jobs

**Tags**: ci, github-actions, warn-only, artifact-upload, continue-on-error, transient-failure

Warn-only jobs also need `continue-on-error: true` on GitHub service steps such as upload/cache/code-scanning save. Do not apply to required jobs.

### Rust lint

**Tags**: ci, rust, clippy, rustfmt, emit, native_base64, workspace, container, ghcr, toolchain-pin

The `lint` job runs `cargo fmt --all -- --check`, `cargo clippy --workspace --all-targets -- -D warnings`, and `cargo check --workspace --all-targets`. Keep scope aligned with `.claude/skills/pre-commit-checklist/run-rust-lint.sh`. Keep `rust-toolchain.toml` in sync with `RUST_VERSION` in `docker/ci.Dockerfile`.

### YAML step names

**Tags**: ci, github-actions, yaml, gotcha, step-name

Quote step names containing space-`#`, e.g. `name: 'Rust lint (clippy, #2015)'`; otherwise YAML treats the suffix as a comment.

### Linux `ry test`

**Tags**: ci, github-actions, ry-test, jit-leak, isolation, worker-count

Linux `test` / `asan` jobs omit `-p`; `macos-smoke-rust` may keep it. Worker=1 keeps output localizable and avoids JIT teardown leak accumulation.
