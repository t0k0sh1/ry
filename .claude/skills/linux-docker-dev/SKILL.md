---
name: linux-docker-dev
description: macOS から Linux 環境 (Debian trixie + glibc 2.40、pre-baked ry-ci GHCR イメージ経由) で ry をビルド・テストする Docker 開発環境。Use when "Docker" / "Linux 環境" / "glibc" / "docker/run.sh" / "Ubuntu" / "Debian" / "trixie" / "ASan を Linux で確認" / Linux 固有の挙動を再現したいとき。
allowed-tools: Bash
---

# Linux Docker Development Environment

Run tests under Linux (Debian trixie + glibc 2.40, via the pre-baked `ry-ci` GHCR image) from macOS using the scripts in `docker/`. This reproduces the CI `asan` / `tsan` / `fuzz` / `clang-tidy` / `lint` / `scan-build` job environments locally and exposes Linux-only behaviour such as glibc heap consolidation checks that are invisible under macOS libSystem malloc. It is also the canonical workflow for sanitizer / libFuzzer / static-analysis runs because macOS-host execution hits known environment issues (issue #1865 — `fuzz_json` hang under ASan, TSan `LargeMmapAllocator` bug, clang-tidy PCH incompatibility, scan-build PATH absence, libFuzzer `SDKROOT` requirement).

> **Source-of-truth note**: previously in `AGENTS.md`; relocated by #1384.

See [`docker/README.md`](../../../docker/README.md) for a quick-start reference.

## Commands

```bash
# Build the image once; subsequent runs reuse ccache (~1-2 min)
./docker/run.sh default ry_tests                                  # default preset, C++ tests
./docker/run.sh default ry test -p                                # default preset, Ry self-tests

./docker/run.sh asan ry_tests                                     # ASan + UBSan, C++ tests
./docker/run.sh asan ry test -p                                   # ASan + UBSan, Ry self-tests
./docker/run.sh asan ry test tests/spec/some.test.ry              # single file

./docker/run.sh tsan ry_tests                                     # TSan, C++ tests

./docker/run.sh fuzz fuzz_parser  -max_total_time=30 \
    -artifact_prefix=tests/fuzz/regressions/parser/ \
    tests/fuzz/corpus/parser                                      # libFuzzer parser harness
./docker/run.sh fuzz fuzz_json    -max_total_time=30 \
    -artifact_prefix=tests/fuzz/regressions/json/ \
    tests/fuzz/corpus/json
./docker/run.sh fuzz fuzz_utf8    -max_total_time=30 \
    -artifact_prefix=tests/fuzz/regressions/utf8/ \
    tests/fuzz/corpus/utf8
./docker/run.sh fuzz fuzz_io_open -max_total_time=30 \
    -artifact_prefix=tests/fuzz/regressions/fuzz_io_open/ \
    tests/fuzz/corpus/fuzz_io_open                                # libFuzzer I/O syscall harness

./docker/run.sh default bash                                      # interactive shell

./docker/run.sh --rebuild asan ry_tests                           # force image rebuild
```

## Static analysis subcommand

`./docker/run.sh static-analysis <tool>` invokes Clang-Tidy / Cppcheck / scan-build inside the container, reusing the same LLVM 21 toolchain as CI.

```bash
./docker/run.sh static-analysis clang-tidy                        # uses build-docker/compile_commands.json
./docker/run.sh static-analysis cppcheck                          # no build required
./docker/run.sh static-analysis scan-build                        # HTML report in build-scan-docker/scan-build-report/<timestamp>/ (host bind-mount)
./docker/run.sh static-analysis all                               # clang-tidy → cppcheck → scan-build
```

> `scan-build` and `all` isolate their analyzer-wrapped CMake configuration in `build-scan-docker/` (host) ↔ `build-scan/` (container), separate from `build-docker/`. This means a subsequent `./docker/run.sh default ...` works immediately without an intervening `rm -rf` — only delete `build-scan-docker/` if you want to discard the analyzer state and report.

## Preset summary

| Preset / subcommand | Sanitizers | Sanitizer env vars (auto-set by run.sh) | Host build dir |
|---------------------|-----------|----------------------------------------|----------------|
| `default` | none | — | `build-docker/` |
| `asan` | ASan + UBSan | `ASAN_OPTIONS=detect_container_overflow=0:detect_leaks=0:halt_on_error=1`, `UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1` | `build-asan-docker/` |
| `tsan` | TSan | `TSAN_OPTIONS=halt_on_error=1:second_deadlock_stack=1` | `build-tsan-docker/` |
| `fuzz` | ASan + UBSan | same as `asan` | `build-fuzz-docker/` |
| `static-analysis` clang-tidy / cppcheck | — | — | `build-docker/` (shared with `default`) |
| `static-analysis` scan-build / all | — | — | `build-scan-docker/` (dedicated; `build-docker/` also mounted for clang-tidy step in `all` mode) |

## Mount strategy

`docker/run.sh` bind-mounts each top-level entry the build/tests/static analysis actually read, instead of the entire project root. This closes the cross-OS contamination path that issue #1876 documents: the old `-v PROJECT_DIR:/workspace` outer mount let host `build/`, `build-asan/`, `build-fuzz/` (macOS Mach-O binaries, macOS paths in `compile_commands.json`) appear inside the container alongside the per-preset `-v build-asan-docker:/workspace/build-asan` inner mount. Under OrbStack VirtioFS this leak intermittently surfaced as `clang-tidy` chdir failures into `/Users/...` directories and as `rm -rf build-fuzz-docker/` recovery cycles — see PR #1873 for the failure log.

What gets mounted, per invocation:

- Directories: `src/`, `include/`, `tests/`, `share/`, `crates/` (the `ry_codegen` Rust cdylib source, built via corrosion on every preset since the #1993 cutover)
- Config files (individual file mounts): `CMakeLists.txt`, `CMakePresets.json`, `package.toml`, `Cargo.toml`, `Cargo.lock`, `.clang-tidy`, `.cppcheck-suppressions`
- Per-preset build output: `$BUILD_DIR_HOST` (host) → `$BUILD_DIR_CONTAINER` (container) — unchanged from before
- ccache: named volume `ry-ccache-docker` → `/home/ubuntu/.cache/ccache`
- scan-build subcommand additionally mounts `build-scan-docker/` → `/workspace/build-scan`

What is **not** mounted (and therefore invisible inside the container): everything else under the project root — `docker/`, `scripts/`, `editor/`, `docs/`, `.git/`, `.serena/`, `.github/`, `.claude/`, `changelog.d/`, all top-level Markdown / LICENSE / install.sh, and all top-level dotfiles other than the two static analysis configs listed above. Host macOS native build dirs (`build/`, `build-asan/`, `build-tsan/`, `build-fuzz/`) are likewise invisible — recovery no longer needs to `mv build/ build.host-bak/` or `rm -rf` macOS build artifacts to keep the container clean.

`entrypoint.sh` adds three fail-fast guards on startup, all consulting `$RY_HOST_BUILD_DIR` (set by `run.sh`) so the recovery hint names the host-side directory the user can `rm -rf`:

1. **Required-mount presence** (`exit 70`) — verifies `/workspace/{CMakeLists.txt,CMakePresets.json,package.toml,src,include,tests,share}` all exist. Trips when `run.sh`'s mount list drifts (e.g. a new top-level config file is referenced from CMake without being added to `MOUNT_ARGS`).
2. **ELF magic** (`exit 71`) — if `BUILD_DIR/ry` or `BUILD_DIR/ry_tests` exists, the first four bytes must be `\x7fELF`. Catches Mach-O binaries that a developer copied into the per-preset build dir or that leaked through a misconfigured mount.
3. **macOS path in `compile_commands.json`** (`exit 72`) — refuses to run when `BUILD_DIR/compile_commands.json` lists any `"directory": "/Users/..."` entry, since that is exactly what previously broke clang-tidy.

**Maintenance rule when adding a new top-level entry to the repo**: if your change adds (a) a new source / test / stdlib subdirectory at the repo root, (b) a new top-level config file that CMake or a static analyser reads, or (c) a new top-level dotfile that the build consumes, you **must** update both `docker/run.sh`'s `MOUNT_ARGS` list and `docker/entrypoint.sh`'s stage-1 required-mount loop in the same PR. Without the mount, the container sees the source tree without the new file and either silently skips it or fails late inside CMake configure; without the guard update, the silent skip slips past CI.

## Known limitations

- **First image build takes ~30 seconds** because `docker/Dockerfile` inherits from `ghcr.io/<owner>/ry-ci:llvm-21` — the heavy toolchain (LLVM 21, cmake, ninja, ccache, OpenSSL, cppcheck, gtest tarball) is pulled, not built. The first compile of ry itself takes 1-2 minutes; subsequent runs use ccache and complete in ~10-30 seconds. If GHCR is unreachable, Docker falls back to whatever image layers are already cached locally.
- On Apple Silicon, the container runs **arm64 Linux natively**. To test x86_64-specific behaviour, pass `--platform linux/amd64` manually to `docker run` (not wired into `run.sh` by default — qemu emulation is 5-10× slower and rarely needed).
- macOS builds (`build/`, `build-asan/`, `build-tsan/`, `build-fuzz/`) and Docker builds (`build-docker/`, `build-asan-docker/`, `build-tsan-docker/`, `build-fuzz-docker/`, `build-scan-docker/`) are **fully separate**. Running Docker commands will not overwrite your local CMake build.
- The dev image inherits from `ry-ci`, **not** `ry-ci-glibc-old`. Local Linux builds therefore link against glibc 2.40 (Debian trixie). To reproduce a release-style binary against glibc 2.36, override the base image: `docker build --build-arg CI_IMAGE_OWNER=<owner> --build-arg CI_IMAGE_TAG=llvm-21 -t ry-linux-dev:glibc-old docker/` — but only do this if you are debugging release-specific symbol issues; everyday dev does not need it.
- **No silent fallback to macOS-native execution**: `run.sh` hard-fails if no Docker daemon is reachable. This is intentional (issue #1865) — falling back to native would re-introduce the platform-specific breakage this script is meant to escape.
- **Recommended Docker runtime: OrbStack** — its VirtioFS-backed bind-mount delivers materially better throughput than Docker Desktop's gRPC-FUSE, which matters for fuzz corpus I/O. Colima and Docker Desktop are supported alternatives if OrbStack is unavailable.
- **`/tmp` inside the container is a 2 GB tmpfs**, not the overlay filesystem. `run.sh` mounts it via `--tmpfs /tmp:rw,exec,size=2g` so test temp dirs (`/tmp/ry_entry_test_*`, `mkdtemp` callers in `tests/test_paths.cpp`) and analyzer scratch (`scan-build`'s timestamped working dir) never compete with the Docker storage pool. If a test needs more than 2 GB of `/tmp` it must opt out explicitly — bump the size flag locally for that run.
- **`--rebuild` invalidates host `build-*-docker/` caches when the image's toolchain layout changes** (e.g. ninja moved from `/usr/bin/ninja` to `/opt/ninja/bin/ninja` between image revisions). Symptom: `cmake: Running '/usr/bin/ninja' '--version' failed`. Remediation: `rm -rf build-docker/ build-asan-docker/ build-tsan-docker/ build-fuzz-docker/ build-scan-docker/` and rerun. CMake reuses `CMAKE_MAKE_PROGRAM` from the cache; only `--fresh` or a wipe forces re-discovery.
- **Host `build/compile_commands.json` can leak into container clang-tidy runs under OrbStack VirtioFS** (issue #1854 sighting, 2026-05-23). Symptom: `Cannot chdir into "/Users/t0k0sh1/Workspace/ry/build": No such file or directory` from clang-tidy, even with `build-docker/` freshly rebuilt. Diagnosis: `docker/entrypoint.sh` calls `clang-tidy -p build` (relative, resolved to `/workspace/build/compile_commands.json`). The two-layer bind (`$PROJECT_DIR:/workspace` outer + `$PROJECT_DIR/build-docker:/workspace/build` inner) should overlay so only the container-generated file is visible at that path, but in at least one observed OrbStack VirtioFS configuration the host `build/compile_commands.json` was visible inside the container and its `"directory": "/Users/.../build"` entries caused clang-tidy to chdir to a non-existent host path. Empirical remediation: temporarily move the host file aside (`mv build/compile_commands.json build/compile_commands.json.host-bak`) before running `./docker/run.sh static-analysis clang-tidy`, then restore it afterward. Verified clean clang-tidy run after the move (EXIT=0). The root-cause mount behavior is not fully understood — re-investigate if recurrence triggers further work; tracked separately.
- **`--user $(id -u):$(id -g)` intentionally not passed** to `docker run`. The base image sets `CCACHE_DIR=/home/ubuntu/.cache/ccache` owned by container UID 1000; passing the macOS host UID (typically 502) would break ccache writes inside the named volume `ry-ccache-docker`. macOS Docker runtimes (OrbStack VirtioFS, Docker Desktop gRPC-FUSE) transparently translate bind-mount UIDs, so files created under `/workspace` and `build-*-docker/` end up owned by the host user without needing `--user`. This is a deliberate deviation from the original Plan in issue #1865, which proposed `--user` to fix UID mismatch — that path would have required either chowning the ccache volume per run or relocating ccache into the bind-mount (both worse than relying on runtime UID translation). On Linux hosts where UIDs do not get translated, ccache will simply be skipped or files will be owned by `1000:1000` — acceptable for ad-hoc Linux dev, and out of scope for this script's macOS-centric target.
