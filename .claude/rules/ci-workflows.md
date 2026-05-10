---
paths:
  - ".github/workflows/**/*.yml"
  - ".github/actions/**/*.yml"
---

# CI Workflows

### UBSan must disable `vptr` and `function` checks on this project

**Source**: #630 (2026-04-11)
**Tags**: ubsan, sanitizer, cmake, llvm, cxx-flags

**Rule**: When adding or extending UBSan flags in `CMakeLists.txt`, always pair `-fsanitize=undefined` with `-fno-sanitize=vptr,function`. Do not try to fix the false positives by changing the LLVM interop code — the checks themselves are the wrong tool for this codebase:

1. `vptr` — ry compiles with `-fno-rtti` (matches LLVM's build flags, see `CMakeLists.txt:25`). The `vptr` check requires RTTI to resolve virtual-call types, so enabling it under `-fno-rtti` produces no coverage and in some toolchains hard-errors.
2. `function` — LLVM exposes many C-style function pointers that ry casts through `void *` (JIT symbol resolution, runtime dispatch). UBSan's `function` check flags every one as a type mismatch, drowning out real signal.

**Also**: UBSan and ASan are compatible and enabled together via the `asan` CMake preset (`ENABLE_ASAN=ON` + `ENABLE_UBSAN=ON`). TSan is **not** compatible with either and lives in its own `tsan` preset (`build-tsan/`). `CMakeLists.txt` enforces this with a `FATAL_ERROR` if `ENABLE_TSAN` is combined with the others.

### Linux CI runs in pre-baked GHCR container images, no apt usage

**Source**: #1505 (2026-05-02)
**Tags**: ci, github-actions, container, ghcr, no-apt

**Rule**: Every Linux job in `.github/workflows/*.yml` (CI, CodeQL, release) must use `container: ghcr.io/${{ github.repository_owner }}/ry-ci:llvm-21` (or `ry-ci-glibc-old:llvm-21` for release Linux jobs). Never add `apt-get install` or `sudo apt-get` to a Linux job step. The container pre-installs the entire toolchain: clang/clang++ 21 (`/usr/local/llvm`), cmake (`/opt/cmake`), ninja (`/opt/ninja`), ccache (`/usr/local/bin`), OpenSSL (`/opt/openssl`), cppcheck (`/opt/cppcheck`), and a vendored gtest tarball at `$RY_VENDORED_GTEST_TARBALL`. `CC` / `CXX` / `PATH` / `LD_LIBRARY_PATH` / `OPENSSL_ROOT_DIR` / `LLVM_DIR` are pre-set as ENV.

**Why**: An Ubuntu apt mirror outage on 2026-05-02 caused intermittent `apt-get install` failures from `archive.ubuntu.com` and `apt.llvm.org`. Pre-baking isolates CI from upstream Ubuntu/Debian repository availability. Image rebuild is on demand via `build-ci-image.yml` (`workflow_dispatch` + `push` on Dockerfile changes).

**How to apply**:
- Linux CI / CodeQL / release jobs: set `container: ghcr.io/.../ry-ci:llvm-21`
- Release Linux only: use `ry-ci-glibc-old:llvm-21` (`gcc:14-bookworm` base, glibc 2.36) so binaries remain runnable on older Linux distros (Ubuntu 22.04 with glibc 2.35, RHEL 9 with glibc 2.34, etc.).
- macOS jobs continue using Homebrew on the host runner — only Linux is containerised.
- New tool needed in CI: add to `docker/ci.Dockerfile` (and `docker/ci-glibc-old.Dockerfile` if release also needs it), trigger manual image rebuild via `workflow_dispatch`, wait for the new image before merging the workflow change. See `.claude/skills/ci-image-workflow/SKILL.md`.

**How to verify**: `grep -rnE 'apt(-get)?\b' .github/workflows/ docker/` must return zero hits. CI logs should show every Linux job running inside `ghcr.io/.../ry-ci:llvm-21` (visible at the start of each step as `Set up job` / `Initialize containers`).

### ccache cache path inside container is `/root/.cache/ccache`, not the runner's home

**Source**: #1505 (2026-05-02)
**Tags**: ci, ccache, container, github-actions, cache

**Rule**: When adding ccache to a container-based job, use `actions/cache@v4` directly with `path: /root/.cache/ccache`. The container runs as root by default, so ccache writes to `/root/.cache/ccache`, not `/home/runner/.cache/ccache` (the bare-runner path). `actions/cache@v4` reads/writes paths inside the container's filesystem, not the host runner's. The `ccache` binary is already at `/usr/local/bin/ccache` (installed during image build), so no install step is needed. ccache picks up `~/.cache/ccache` which expands to `/root/.cache/ccache` for root automatically; setting `CCACHE_DIR` is unnecessary.

`hendrikmuhs/ccache-action@v1` cannot be used in the container because it internally invokes `sudo apt-get install ccache`, violating the no-apt rule above.

**How to apply**:

```yaml
- name: Restore ccache
  uses: actions/cache@v4
  with:
    path: /root/.cache/ccache
    key: ci-${{ matrix.job }}-${{ github.ref }}-${{ github.sha }}
    restore-keys: |
      ci-${{ matrix.job }}-${{ github.ref }}-
      ci-${{ matrix.job }}-
```

macOS jobs (in `release.yml`) keep using `hendrikmuhs/ccache-action@v1` because they run on host runners (no container) and Homebrew is the canonical install method.

### Release Linux binaries must use `ry-ci-glibc-old` for older-glibc compatibility

**Source**: #1505 (2026-05-02)
**Tags**: ci, release, glibc, container, abi, linux

**Rule**: Release Linux build jobs (`release.yml`) must use `ry-ci-glibc-old` (Debian bookworm via `gcc:14-bookworm`, glibc 2.36) as the container, **not** `ry-ci`. Regular CI's `ry-ci` (Debian trixie, glibc 2.40) produces binaries that fail at startup on older Linux distros (Ubuntu 22.04 with glibc 2.35, RHEL 9 with glibc 2.34) with `version 'GLIBC_2.40' not found`. The `ry-ci-glibc-old` image is built from `docker/ci-glibc-old.Dockerfile` and rebuilt by the same `build-ci-image.yml` `workflow_dispatch` (matrix includes both image names).

**How to apply**: in `release.yml`, the Linux container is wired as

```yaml
container: ${{ matrix.llvm_install == 'container' && format('ghcr.io/{0}/ry-ci-glibc-old:llvm-21-rev<N>', github.repository_owner) || null }}
```

`<N>` is the current immutable rev — see "Release container must pin to immutable `:llvm-<MAJOR>-rev<N>` tag" for why release uses the rev-suffixed form.

Other workflows (`ci.yml`, `codeql.yml`) use `ry-ci:llvm-21` because glibc compat does not matter for CI artifacts that never leave the runner. The dev `docker/Dockerfile` also inherits from `ry-ci` since dev work does not produce distributable binaries.

### Release container must pin to immutable `:llvm-<MAJOR>-rev<N>` tag

**Source**: #1508 (2026-05-02)
**Tags**: ci, release, container, ghcr, immutable-tag, reproducibility

**Rule**: `release.yml`'s `container:` expression must reference the immutable `:llvm-<MAJOR>-rev<N>` tag (e.g. `:llvm-21-rev3`), never the mutable `:llvm-<MAJOR>` (e.g. `:llvm-21`). Other workflows that don't produce externally-distributed artifacts (`ci.yml`, `codeql.yml`) keep the mutable `:llvm-<MAJOR>` pointer because they don't have the reproducibility constraint and should track the latest image automatically.

**Why**: `build-ci-image.yml`'s `manifest` job publishes both tags simultaneously per build, but `:llvm-<MAJOR>` is overwritten on every rebuild while `:llvm-<MAJOR>-rev<N>` is monotonically increasing and never reused. If `release.yml` referenced the mutable tag, re-running the workflow on an older `vX.Y.Z` source tag would pull whatever image the pointer resolves to today, not the one used for the original release — bit-different binaries invalidate downstream `sha256sums.txt`, signatures, and distro-packager checksum mirrors.

`@sha256:...` digest pin is stronger but not required: `compute-tag` only emits monotonically increasing rev numbers and never reuses an existing `rev<N>` (effectively immutable in practice). Digest pinning may be revisited as security hardening separately.

**How to apply**:

1. Static-pin the rev tag inside `release.yml` (`container:` line). Workflow files at the original tag's commit are what GitHub replays during a re-run, so a hardcoded literal preserves the image bits used at release time. Dynamic resolution (`needs.<job>.outputs.<n>`, file reads) cannot solve the problem — they still resolve at re-run time.
2. Bump only at Release prep (`.claude/skills/preparing-for-release/SKILL.md` Step 4). Mid-cycle merges leave the pin alone — a stale pin during feature work just means the next release inherits the previous image, which is correct.
3. On LLVM major version bumps, replace **both** the `:llvm-<OLD>` reference (other workflows) and the `:llvm-<OLD>-rev<N>` pin (release.yml) with the new major's tag form. See `.claude/skills/ci-image-workflow/SKILL.md` "How to bump LLVM".

**How to verify**:

1. `grep -nE ':llvm-[0-9]+-rev[0-9]+' .github/workflows/release.yml` must show one match per release Linux container line.
2. `grep -nE ':llvm-[0-9]+([^-]|$)' .github/workflows/release.yml` must return zero hits — `release.yml` should never have a rev-less mutable tag.
3. The pinned `:llvm-<MAJOR>-rev<N>` must exist in GHCR (public, no auth):

   ```bash
   curl -s "https://ghcr.io/token?scope=repository:t0k0sh1/ry-ci-glibc-old:pull" \
     | jq -r '.token' \
     | { read TOKEN; curl -s -H "Authorization: Bearer ${TOKEN}" "https://ghcr.io/v2/t0k0sh1/ry-ci-glibc-old/tags/list"; } \
     | jq -r '.tags[]' | grep -E '^llvm-[0-9]+-rev[0-9]+$' | sort -V
   ```

### `github.ref_name` points to `main` in scheduled workflows, not the checked-out branch

**Source**: #970 (2026-04-15)
**Tags**: ci, github-actions, schedule, ccache, gotcha

**Rule**: In a GitHub Actions scheduled workflow (`on: schedule:`), `github.ref_name` is always the **default branch** (`main`), regardless of which branch the job checks out via `actions/checkout` with an explicit `ref:`. Expressions like `github.ref_name == 'main' || startsWith(github.ref_name, 'v')` therefore evaluate based on the workflow file's source branch, not the checked-out branch.

**How to apply**: In scheduled workflows that dynamically check out a branch, replace conditional `save:` expressions with `save: true` (always save) or compute the condition from the resolved `ref` output rather than `github.ref_name`. Copying a `ccache save:` expression like `${{ github.ref_name == 'main' || startsWith(github.ref_name, 'v') }}` from a push-triggered workflow is the canonical pitfall — on a scheduled run it always evaluates to `true` (if the workflow file lives on `main`) regardless of resolved branch choice.

### CodeQL must run on default branch to keep Code Scanning dashboard fresh

**Source**: #1367 (2026-04-25)
**Tags**: ci, github-actions, codeql, dashboard, schedule, security

**Rule**: Always pair `pull_request:` with `push: branches: [<default>]` when removing a `schedule:` from a CodeQL workflow. Keep `workflow_dispatch:` as a manual escape hatch but never rely on it for dashboard freshness.

**Why**: GitHub's Code Scanning Security alerts dashboard reflects the **latest analysis on the default branch**, not on PR branches. A CodeQL workflow whose only triggers are `pull_request:` and `workflow_dispatch:` will analyze every PR but never refresh the dashboard after merge — alerts fixed (or newly introduced) on `main` will not appear until someone manually dispatches the workflow. The merge-commit run that GitHub used to get from a daily `schedule:` cron is gone otherwise.

**How to verify**: After merge, the next push to the default branch should produce a CodeQL run in the Actions tab.

### Cross-workflow gating via `gh api`: filter by `event=push` and absorb trigger-arrival skew with a Phase 1 initial wait

**Source**: #1542 (2026-05-03)
**Tags**: ci, github-actions, gh-api, cross-workflow-gate, codeql, race-condition, polling

**Rule**: Cross-workflow gates that poll another workflow's run state must:

1. **Always** filter by `event=push` (or whatever trigger represents the "real" mainline run) when querying via `actions/workflows/<file>/runs?head_sha=<SHA>&event=<event>`. PR / manual / scheduled runs do not satisfy a mainline gate.
2. Use **two-phase polling** with separate timeouts:
   - **Phase 1 (initial wait)**: poll for run *existence* up to `INITIAL_WAIT_SECONDS` (default 120s). Absorbs trigger-arrival skew between the two workflows. If still not found at the deadline, fail-closed — that's the legitimate "tag points to a commit never pushed to main" case.
   - **Phase 2 (completion poll)**: once a run is found, poll `status=completed` up to `POLL_TIMEOUT_SECONDS` (default 1800s, 30 min) at `POLL_INTERVAL_SECONDS` intervals (default 30s).
3. Expose all three timeouts as job-level `env:` so the polling script body can be exercised locally (`INITIAL_WAIT_SECONDS=10 POLL_INTERVAL_SECONDS=2 POLL_TIMEOUT_SECONDS=20 bash ...`) without editing the script. YAML defaults stay production-sized.
4. Set `permissions: { actions: read }` at job scope (not workflow top-level) — required to query workflow runs in private/public repos via `gh api`. Keep workflow-top-level `permissions: contents: read` as the minimal baseline.

**Skip-aware downstream gating**: when adding the gate as a new job, the downstream `release` (or equivalent publish) job's `if:` must use `always() && needs.<gate>.result in ('success', 'skipped')` so an intentional `workflow_dispatch` skip path (escape hatch) doesn't also skip the publish. Without `always()`, the default `success()` semantics treat `skipped` as not-success and silently cancel downstream.

**Why this design** — gating against `actions/workflows/<file>/runs?head_sha=<SHA>` returns runs from **every trigger** for that SHA, so a green PR run on the same head SHA would satisfy a naive gate even though no mainline analysis ever ran. Separately, when a main push and a tag push fire `release.yml` and `codeql.yml` near-simultaneously, arrival order is non-deterministic — `release.yml` may start before `codeql.yml` has been enqueued, so a single one-shot query returns zero runs and would fail-closed before the run had a chance to appear.

**How to apply** (canonical pattern from `release.yml` `codeql-gate` job):

```yaml
codeql-gate:
  if: github.ref_type == 'tag' && (github.event_name != 'workflow_dispatch' || inputs.skip_codeql_gate != true)
  runs-on: ubuntu-latest
  timeout-minutes: 35
  permissions:
    contents: read
    actions: read
  env:
    INITIAL_WAIT_SECONDS: '120'
    POLL_INTERVAL_SECONDS: '30'
    POLL_TIMEOUT_SECONDS: '1800'
  steps:
    - name: Wait for CodeQL run on this SHA
      env:
        GH_TOKEN: ${{ github.token }}
        OWNER: ${{ github.repository_owner }}
        REPO: ${{ github.event.repository.name }}
        SHA: ${{ github.sha }}
      shell: bash
      run: |
        set -euo pipefail
        # Phase 1: wait for run existence; Phase 2: poll until completed.
        # See full body in release.yml.

release:
  needs: [build, codeql-gate]
  if: |
    always() &&
    needs.build.result == 'success' &&
    (needs.codeql-gate.result == 'success' || needs.codeql-gate.result == 'skipped') &&
    github.ref_type == 'tag'
```

**Why not just one query**: a one-shot `gh api` returning empty cannot distinguish "run hasn't started yet" from "run will never exist". Phase 1 collapses both into the same legitimate timeout boundary, then Phase 2 can assume the run exists and only watch its completion.

**Why not include PR runs in the gate**: a PR's CodeQL run analyses the *PR head* (potentially a merge-commit synthesised by GitHub), not the post-merge default-branch state. A green PR run can coexist with a red post-merge run if the merge introduces new findings (rare but real — consider a PR that adds tests but a concurrent main-push that introduces the analysed code). Mainline `event=push` is the only run that proves the released SHA itself is clean.

### scan-build: pass `--use-analyzer=/usr/local/llvm/bin/clang` explicitly

**Source**: #1247 (2026-04-20), updated for #1505 container migration (2026-05-02)
**Tags**: ci, scan-build, llvm, container, static-analysis

**Rule**: Always pass `--use-analyzer=/usr/local/llvm/bin/clang` explicitly to every `scan-build` invocation in `.github/workflows/ci.yml` and every example in `.claude/skills/static-analysis-tools/SKILL.md`. The `--use-cc` flag controls only the build-time compiler and is ignored by the analyzer-clang lookup.

**Why**: `scan-build`'s `FindClang()` Perl function tries `$RealBin/bin/clang`, `/usr/lib/llvm-{MAJOR}/bin/clang`, and the Xcode toolchain in order. None resolve in the `ghcr.io/.../ry-ci:llvm-21` container — LLVM is source-built and installed at `/usr/local/llvm/`, scan-build itself is not in the parent of an adjacent `bin/clang` (so `$RealBin/bin/clang` mis-expands), and `/usr/lib/llvm-{MAJOR}` is a Debian-specific path that does not exist on the container's debian-trixie base because clang was never apt-installed. Without `--use-analyzer`, scan-build leaves `$Clang` undefined and emits `Use of uninitialized value $Clang in concatenation` followed by `scan-build: error: Cannot find an executable 'clang' relative to scan-build`.

**How to apply**: In every CI `scan-build` invocation (`.github/workflows/ci.yml`) and every local-execution example (`.claude/skills/static-analysis-tools/SKILL.md`), include `--use-analyzer=/usr/local/llvm/bin/clang` alongside `--use-cc` / `--use-c++`. macOS Homebrew `scan-build` resolves clang via `$RealBin/bin/clang` correctly when scan-build is at `/opt/homebrew/opt/llvm@21/bin/scan-build`, so the flag is redundant there — but keeping it uniform prevents drift between local-macOS and CI invocations.

**How to verify**: `grep -n 'scan-build' .github/workflows/*.yml .claude/skills/static-analysis-tools/SKILL.md` and confirm `--use-analyzer` accompanies every invocation. CI logs must not contain `Use of uninitialized value $Clang`.

### `actions/download-artifact@v4` `pattern:` is a glob — beware prefix collisions across matrix dimensions

**Source**: #1505 manifest job failure (2026-05-02)
**Tags**: ci, github-actions, actions/download-artifact, glob, multi-image, multi-arch

**Rule**: When `actions/download-artifact@v4`'s `pattern:` is used to filter across a matrix, the separator between the discriminator and the rest of the artifact name must be a character that **cannot appear inside the discriminator**. Hyphens are unsafe when matrix values themselves contain hyphens (image names, hyphenated tags). Use `__` (double underscore) as a sentinel separator unlikely to appear inside any matrix value.

**Why**: `build-ci-image.yml` uploaded digests as `digests-${image}-${arch}` and the manifest job downloaded them with `pattern: digests-${image}-*`. With two image variants sharing a prefix (`ry-ci` and `ry-ci-glibc-old`), the pattern `digests-ry-ci-*` glob-matched **both** sets — `*` greedily matched `glibc-old-amd64`. The `ry-ci` manifest job pulled in 4 digests (2 per image) and `docker buildx imagetools create` exited with `not found`. The reverse direction (`digests-ry-ci-glibc-old-*`) had no false matches and succeeded — asymmetric, surprising failure mode.

**How to apply**:

```yaml
# Upload — use __ to bracket matrix dimensions whose values may share prefixes
- uses: actions/upload-artifact@v4
  with:
    name: digests-${{ matrix.image }}__${{ matrix.arch }}

# Download — pattern with the same sentinel
- uses: actions/download-artifact@v4
  with:
    pattern: digests-${{ matrix.image }}__*
    merge-multiple: true
```

This applies any time you have two matrix dimensions whose values include user-controlled strings (image / package / module names), not just to digests. General rule: between glob-discriminated prefixes, the separator must be a character class disjoint from every matrix value.

**How to verify**: After uploading, the artifacts UI should show names like `digests-ry-ci__amd64` (not `digests-ry-ci-amd64`). In the manifest job log, the `for f in /tmp/digests/*; do …` loop should iterate exactly the expected number of digests (2 per arch matrix per image, not the cross-product).

### LLVM 17+ source build: `compiler-rt` belongs in `LLVM_ENABLE_RUNTIMES`, not `LLVM_ENABLE_PROJECTS`

**Source**: #1505 follow-up (2026-05-02)
**Tags**: ci, docker, llvm, compiler-rt, cmake, sanitizer, asan, tsan

**Rule**: When source-building LLVM in `docker/ci.Dockerfile` / `docker/ci-glibc-old.Dockerfile`, always include `-DLLVM_ENABLE_RUNTIMES="compiler-rt"` alongside `-DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra"`. From LLVM 17 onward, the runtime libraries (`compiler-rt`, `libcxx`, `libcxxabi`, `libunwind`) must be built via `LLVM_ENABLE_RUNTIMES` rather than `LLVM_ENABLE_PROJECTS` so the *just-built* clang is used to compile them — putting `compiler-rt` in `PROJECTS` is silently legacy and produces broken or no libraries on modern LLVM (CI `asan` job fails at link with `cannot find libclang_rt.asan_static.a`; `tsan` fails the same way for `libclang_rt.tsan{,_cxx}.a`).

Disable the sub-projects ry doesn't use to keep build time bounded (`COMPILER_RT_BUILD_PROFILE=OFF`, `COMPILER_RT_BUILD_XRAY=OFF`, `COMPILER_RT_BUILD_MEMPROF=OFF`, `COMPILER_RT_BUILD_ORC=OFF`) but keep `BUILD_SANITIZERS=ON`, `BUILD_BUILTINS=ON`, `BUILD_LIBFUZZER=ON` (libFuzzer is gated off in CI but the harness build script needs the static archive to exist).

**How to verify**: After image rebuild, `docker run --rm ghcr.io/<owner>/ry-ci:llvm-21 ls /usr/local/llvm/lib/clang/21/lib/x86_64-unknown-linux-gnu/` should list `libclang_rt.asan.a`, `libclang_rt.asan_static.a`, `libclang_rt.tsan.a`, `libclang_rt.tsan_cxx.a`, `libclang_rt.ubsan_standalone.a`, and `libclang_rt.fuzzer.a`. CI `asan` and `tsan` jobs link cleanly.

### cppcheck 2.16 raises `normalCheckLevelMaxBranches` to a hard exit under `--error-exitcode=1`

**Source**: #1505 follow-up (2026-05-02)
**Tags**: ci, cppcheck, lint, gotcha, error-exitcode

**Rule**: In every cppcheck invocation that uses `--error-exitcode=1`, add `--suppress=normalCheckLevelMaxBranches` at the workflow level. Do **not** put the suppression in `.cppcheck-suppressions` — keeping it in the workflow makes the version-specific reason visible at the call site and avoids polluting the project's defect-suppression file with a CI-environment shim.

**Why**: Earlier CI used Ubuntu's apt-installed cppcheck 2.13 which printed the "function exceeds the analysis branch budget" notice as `information` severity and exited 0. The pre-baked `ry-ci` image source-builds cppcheck 2.16.0 which changed the semantics — the same notice is now hard enough to fire `--error-exitcode=1` and fail the `lint` job, even though the notice flags **no defect** (it just informs that some functions were not fully analyzed at the default check level).

**How to verify**: `grep -n 'normalCheckLevelMaxBranches' .github/workflows/*.yml` should match the `Run Cppcheck` step in `ci.yml`. The `lint` job should exit 0 even when functions hit the branch budget; cppcheck's stdout will still print the notice.
