---
paths:
  - ".github/workflows/**/*.yml"
  - ".github/actions/**/*.yml"
---

# CI Workflows

### Linux CI runs in pre-baked GHCR container images, no apt usage

**Source**: #1505 (2026-05-02)
**Tags**: ci, github-actions, container, ghcr, no-apt

**Rule**: All Linux jobs use `container: ghcr.io/${{ github.repository_owner }}/ry-ci:llvm-21` (release: `ry-ci-glibc-old:llvm-21`). No `apt-get install` / `sudo apt-get`. The container pre-installs clang/clang++ 21 at `/usr/local/llvm`, cmake at `/opt/cmake`, ninja, ccache at `/usr/local/bin`, OpenSSL at `/opt/openssl`, cppcheck at `/opt/cppcheck`, vendored gtest at `$RY_VENDORED_GTEST_TARBALL`, and exports `CC` / `CXX` / `PATH` / `LD_LIBRARY_PATH` / `OPENSSL_ROOT_DIR` / `LLVM_DIR`. macOS stays on host runner with Homebrew.

**Why**: An apt mirror outage (#1505, 2026-05-02) killed CI via `archive.ubuntu.com` / `apt.llvm.org`. A partial un-ban was rejected in #1506 (bookworm GPG errors, cppcheck drift, no snapshot.debian.org retention — `.claude/skills/ci-image-workflow/SKILL.md`).

New tool: add to `docker/ci.Dockerfile` (and `docker/ci-glibc-old.Dockerfile` if release needs it) → `workflow_dispatch` rebuild → merge after new image is live. Verify: `grep -rnE 'apt(-get)?\b' .github/workflows/ docker/` returns 0 hits.

### ccache cache path inside container is `/root/.cache/ccache`, not the runner's home

**Source**: #1505 (2026-05-02)
**Tags**: ci, ccache, container, github-actions, cache

`actions/cache@v4` reads/writes the container filesystem. Container runs as root → ccache writes to `/root/.cache/ccache`, not `/home/runner/.cache/ccache`. Do not use `hendrikmuhs/ccache-action@v1` — it calls `sudo apt-get install ccache`. `ccache` is pre-installed at `/usr/local/bin/ccache`; `CCACHE_DIR` need not be set.

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

macOS job (`release.yml`) keeps `hendrikmuhs/ccache-action@v1` (Homebrew is the canonical install path there).

### Release Linux binaries must use `ry-ci-glibc-old` for older-glibc compatibility

**Source**: #1505 (2026-05-02)
**Tags**: ci, release, glibc, container, abi, linux

Normal CI uses `ry-ci` (Debian trixie, glibc 2.40); that binary fails on Ubuntu 22.04 / RHEL 9 with `version 'GLIBC_2.40' not found`. Release Linux uses `ry-ci-glibc-old` (Debian bookworm, glibc 2.36; `docker/ci-glibc-old.Dockerfile`, rebuilt alongside `ry-ci` in `build-ci-image.yml`).

```yaml
container: ${{ matrix.llvm_install == 'container' && format('ghcr.io/{0}/ry-ci-glibc-old:llvm-21-rev<N>', github.repository_owner) || null }}
```

`<N>` is an immutable rev (see next entry). `ci.yml` / `codeql.yml` use `ry-ci:llvm-21`; dev `docker/Dockerfile` inherits `ry-ci` (dev builds are not distributed).

### Release container must pin to immutable `:llvm-<MAJOR>-rev<N>` tag

**Source**: #1508 (2026-05-02)
**Tags**: ci, release, container, ghcr, immutable-tag, reproducibility

`build-ci-image.yml` publishes `:llvm-<MAJOR>` (mutable) and `:llvm-<MAJOR>-rev<N>` (immutable). Using the mutable tag in `release.yml` means a re-run of an old `vX.Y.Z` pulls the current image — invalidating `sha256sums.txt`, signatures, and mirror checksums.

**Rule**: `release.yml` `container:` must use `:llvm-<MAJOR>-rev<N>` (e.g. `:llvm-21-rev3`), never `:llvm-<MAJOR>`. Static literal only — dynamic resolution re-resolves at re-run time. Bump only during release prep (`.claude/skills/preparing-for-release/SKILL.md` Step 4); on LLVM major bump update all workflows simultaneously (`.claude/skills/ci-image-workflow/SKILL.md` "How to bump LLVM").

Verify: `grep -nE ':llvm-[0-9]+-rev[0-9]+' .github/workflows/release.yml` matches each container line; `grep -nE ':llvm-[0-9]+([^-]|$)' .github/workflows/release.yml` returns 0 hits. Confirm the tag exists in GHCR:

```bash
curl -s "https://ghcr.io/token?scope=repository:t0k0sh1/ry-ci-glibc-old:pull" \
  | jq -r '.token' \
  | { read TOKEN; curl -s -H "Authorization: Bearer ${TOKEN}" "https://ghcr.io/v2/t0k0sh1/ry-ci-glibc-old/tags/list"; } \
  | jq -r '.tags[]' | grep -E '^llvm-[0-9]+-rev[0-9]+$' | sort -V
```

### `github.ref_name` points to `main` in scheduled workflows, not the checked-out branch

**Source**: #970 (2026-04-15)
**Tags**: ci, github-actions, schedule, ccache, gotcha

In `on: schedule:` workflows, `github.ref_name` always returns the default branch (`main`) regardless of what `actions/checkout` checked out via an explicit `ref:`. When dynamically checking out a branch in a scheduled workflow, set `save: true` unconditionally or build the condition from the resolved `ref` output, not `github.ref_name`.

### CodeQL must run on default branch to keep Code Scanning dashboard fresh

**Source**: #1367 (2026-04-25)
**Tags**: ci, github-actions, codeql, dashboard, schedule, security

The Code Scanning dashboard reflects only the latest analysis on the default branch. A workflow triggered only by `pull_request:` + `workflow_dispatch:` leaves the dashboard stale after merge. When removing `schedule:`, always pair `pull_request:` with `push: branches: [<default>]`; keep `workflow_dispatch:` as a manual escape hatch.

### Cross-workflow gating via `gh api`: filter by `event=push` and absorb trigger-arrival skew with a Phase 1 initial wait

**Source**: #1542 (2026-05-03)
**Tags**: ci, github-actions, gh-api, cross-workflow-gate, codeql, race-condition, polling

When `release.yml` gates on `codeql.yml` for the same `github.sha`, `gh api '.../runs?head_sha=<SHA>'` returns runs for all triggers. A green PR run can satisfy a naive gate before the mainline analysis runs. When a main-branch push and tag push fire near-simultaneously, a one-shot query may return empty and fail the gate closed.

**Rule**: Cross-workflow gate polling must:

1. **Filter by `event=push`** — PR / manual / scheduled runs do not satisfy a mainline gate.
2. **Two-phase polling**: Phase 1 — poll for run *existence* up to `INITIAL_WAIT_SECONDS` (default 120 s); timeout fails closed (legitimate: tag points at a commit not yet pushed). Phase 2 — poll for `status=completed` up to `POLL_TIMEOUT_SECONDS` (default 1800 s) every `POLL_INTERVAL_SECONDS` (default 30 s).
3. Expose all three timeouts as job-level `env:` vars for local testing. YAML defaults are production-sized.
4. `permissions: { actions: read }` at job scope (not workflow top). Workflow-top baseline: `permissions: contents: read`.

**Skip-aware gating**: downstream `release` `if:` must use `always() && needs.<gate>.result in ('success', 'skipped')` — without `always()`, `success()` treats `skipped` as failure and silently cancels the downstream job.

**Canonical pattern** (`release.yml` `codeql-gate` job):

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

release:
  needs: [build, codeql-gate]
  if: |
    always() &&
    needs.build.result == 'success' &&
    (needs.codeql-gate.result == 'success' || needs.codeql-gate.result == 'skipped') &&
    github.ref_type == 'tag'
```

**Why not PR runs**: PR analysis covers the PR head, not post-merge state. Green PR + red post-merge can coexist — only mainline `event=push` proves cleanliness of the release SHA.

### CodeQL PR scope: `--target ry` on PR, full target on push to main

**Source**: #1740 (2026-05-15)
**Tags**: ci, codeql, static-analysis, performance, target

CodeQL scales with TU count. The default `all` target (~140 TU) includes test / native-lib / fuzz alongside the production compiler/runtime. Test and plugin TUs dominate CI time with little benefit to the shipped path.

**Rule**: In the CodeQL c-cpp matrix Build step, branch on `github.event_name`: `pull_request` → `--target ry --parallel` (analyzes `ry` / `ry_lib` only); `push` to `main` → `--parallel` only (full coverage for dashboard and `codeql-gate`).

```yaml
cmake --build build ${{ github.event_name == 'pull_request' && '--target ry' || '' }} --parallel
```

The `actions` matrix entry (`build-mode: none`) has no build step; the ternary does not apply. Do not invert the mapping; do not narrow the push side.

Verify: `grep -nE 'cmake --build build .*github\.event_name' .github/workflows/codeql.yml` matches the c-cpp Build step.

### `actions/download-artifact@v4` `pattern:` is a glob — beware prefix collisions across matrix dimensions

**Source**: #1505 manifest job failure (2026-05-02)
**Tags**: ci, github-actions, actions/download-artifact, glob, multi-image, multi-arch

`build-ci-image.yml` uploads digests as `digests-${image}-${arch}`. Because `ry-ci` and `ry-ci-glibc-old` share the `ry-ci` prefix, `pattern: digests-ry-ci-*` greedily matched both (`*` consumed `glibc-old-amd64`), causing the manifest job to pull 4 digests across 2 images. `docker buildx imagetools create` then rejected digests from the wrong namespace. The reverse pattern had no false match — an asymmetric failure mode.

**Rule**: `pattern:` separators must not appear in any matrix value. Hyphens are unsafe when image names contain hyphens. Use `__` (double underscore) instead.

```yaml
# Upload
- uses: actions/upload-artifact@v4
  with:
    name: digests-${{ matrix.image }}__${{ matrix.arch }}

# Download
- uses: actions/download-artifact@v4
  with:
    pattern: digests-${{ matrix.image }}__*
    merge-multiple: true
```

Apply whenever two matrix dimensions include a user-controlled string (image / package / module name). General rule: separator must be disjoint from the character class of all matrix values.

### cppcheck 2.16 raises `normalCheckLevelMaxBranches` to a hard exit under `--error-exitcode=1`

**Source**: #1505 follow-up (2026-05-02)
**Tags**: ci, cppcheck, lint, gotcha, error-exitcode

The old CI used cppcheck 2.13 (Ubuntu apt), which emitted "function exceeds the analysis branch budget" at `information` severity (exit 0). `ry-ci` builds cppcheck 2.16.0 from source; in 2.16 the same notice is a hard error under `--error-exitcode=1`, failing `lint` on a non-defect notice.

**Rule**: Add `--suppress=normalCheckLevelMaxBranches` to the cppcheck invocation in the workflow, not to `.cppcheck-suppressions` — keeps the version-specific rationale at the call site.

Verify: `grep -n 'normalCheckLevelMaxBranches' .github/workflows/*.yml` matches the Run Cppcheck step in `ci.yml`.

### Warn-only jobs: `continue-on-error: true` must extend to artifact upload steps

**Source**: #1750 (2026-05-15)
**Tags**: ci, github-actions, warn-only, artifact-upload, continue-on-error, transient-failure

In a warn-only job, apply `continue-on-error: true` to all GitHub-side service steps (artifact upload / release upload / code-scanning result upload / cache save, etc.). GitHub-side calls have a non-zero transient failure rate; a transient failure must not turn a warn-only job red. Warn-only-ness applies to the **job**, not a single step. Do not apply to fully required jobs (e.g. `ry_tests`) — upload failure there is a real signal.

Verify: `grep -n 'continue-on-error' .github/workflows/*.yml` shows analysis and upload steps paired in warn-only jobs; a mismatch warrants an audit.

### Rust crate quality gate runs in the `lint` job; adding clippy/rustfmt needs an image rebuild first

**Source**: #2015 (2026-06-03); scope-aligned with local wrapper in #2344 (2026-06-23)
**Tags**: ci, rust, clippy, rustfmt, emit, native_base64, workspace, container, ghcr, toolchain-pin

All workspace crates (currently `crates/emit` + `crates/native_base64`) are gated in the `ci.yml` `lint` job: `cargo fmt --all -- --check` + `cargo clippy --workspace --all-targets -- -D warnings` + `cargo check --workspace --all-targets` (#1995). The three are orthogonal — do not merge. `clippy` needs the baked `LLVM_SYS_211_PREFIX` + shared libLLVM (via the `emit` crate's `llvm-sys` dep); `cargo fmt --check` does not. The local wrapper `.claude/skills/pre-commit-checklist/run-rust-lint.sh` runs the same `--all` / `--workspace --all-targets` scope; do not split scope between CI and local — drift was fixed in #2344 after `crates/native_base64` (#2282) sat fmt-unchecked in CI.

**Toolchain pin**: Keep `rust-toolchain.toml` in sync with `RUST_VERSION` in `docker/ci.Dockerfile`; a mismatch flags clean code. CI's `/opt/rust/bin/cargo` ignores the file (pin only steers rustup). **Bump both simultaneously.**

`ry-ci` must bake `clippy-preview` and `rustfmt-preview`: `--components="rustc,cargo,rust-std-${RARCH},clippy-preview,rustfmt-preview"`. If `install.sh` rejects, consult `channel-rust-<RUST_VERSION>.toml` `[pkg.*]`. Push Dockerfile first → auto-rebuild (~60–90 min) → re-run CI (`.claude/skills/ci-image-workflow/SKILL.md` "new tool added to image"; only `ry-ci` needs updating). Local: `./.claude/skills/pre-commit-checklist/run-rust-lint.sh`. Policy: root `Cargo.toml` `[workspace.lints]`; FFI carve-outs: `#![allow(...)]` in `lib.rs`.

### Quote workflow step `name:` values that contain ` #` — YAML silently treats them as comments

**Source**: #2344 (2026-06-23) side-finding
**Tags**: ci, github-actions, yaml, gotcha, step-name

In a plain (unquoted) YAML scalar, `#` preceded by whitespace starts a comment — so `- name: Foo (rustfmt, #2015)` is parsed as `"Foo (rustfmt,"` and the rest is dropped on the floor. The step still executes (the `run:` body is independent), but the Actions UI / logs show the truncated name, and any downstream tooling that matches on the full name silently misses. `#2344` repaired four such names in `ci.yml` (`(rustfmt, #2015)`, `(clippy, #2015)`, `(JSONL schema, #2300)`, `(#2109 AC #5/#7)`).

**Rule**: wrap step `name:` values that contain ` #` in single quotes — e.g. `name: 'Rust lint (clippy, #2015)'`. `( #NNNN)` with no preceding space (e.g. `(#2069)`) is safe — `#` is only a comment marker after whitespace.

Verify: `ruby -ryaml -e 'YAML.load_file("..yml")["jobs"].each{|jn,j| j["steps"].each{|s| puts "#{jn}: #{s["name"].inspect}" if s["name"]&.include?("#")}}'` — every step name must end with `)` (or whatever closing character its body intends). A bare `(` near the tail is a truncation tell.

### Linux Ry self-test: pass no `-p`

**Source**: #2237 (2026-06-18); blocker #2234 (subprocess fan-out unification, v0.0.29), background #2232 (design verdict); policy invariant #2238 (code-side guard)
**Tags**: ci, github-actions, ry-test, jit-leak, isolation, worker-count

**Rule**: All `ry test` invocations in Linux jobs (`test` / `asan`) must omit `-p`. `macos-smoke-rust` keeps `-p` (local fast path).

**Why**: After #2234, `ry test` always dispatches per-file subprocesses; `-p` controls only worker count (#2216). At worker=1: output is interleave-free (crash localisation is straightforward), and per-file isolation defeats the 6-step JIT teardown leak accumulation (`.claude/rules/test-runner-isolation.md`). The legacy in-process path hit `bad_alloc` (~7 GB) on ~42/181 spec files.

Verify: `grep -nE '\bry test\b' .github/workflows/ci.yml | grep -- '-p'` matches only the `macos-smoke-rust` line.

**See also**: `.claude/rules/test-runner-isolation.md` — the implementation-side guard paired with this CI default.
