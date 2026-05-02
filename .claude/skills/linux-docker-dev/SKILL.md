---
name: linux-docker-dev
description: macOS から Linux 環境 (Debian trixie + glibc 2.40、pre-baked ry-ci GHCR イメージ経由) で ry をビルド・テストする Docker 開発環境。Use when "Docker" / "Linux 環境" / "glibc" / "docker/run.sh" / "Ubuntu" / "Debian" / "trixie" / "ASan を Linux で確認" / Linux 固有の挙動を再現したいとき。
allowed-tools: Bash
---

# Linux Docker Development Environment

Run tests under Linux (Debian trixie + glibc 2.40, via the pre-baked `ry-ci` GHCR image) from macOS using the scripts in `docker/`. This reproduces the CI `asan`/`tsan` job environment locally and exposes Linux-only behaviour such as glibc heap consolidation checks that are invisible under macOS libSystem malloc.

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
./docker/run.sh default bash                                      # interactive shell

./docker/run.sh --rebuild asan ry_tests                           # force image rebuild
```

## Preset summary

| Preset | Sanitizers | Sanitizer env vars (auto-set by run.sh) | Host build dir |
|--------|-----------|----------------------------------------|----------------|
| `default` | none | — | `build-docker/` |
| `asan` | ASan + UBSan | `ASAN_OPTIONS=detect_container_overflow=0:detect_leaks=0:halt_on_error=1`, `UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1` | `build-asan-docker/` |
| `tsan` | TSan | `TSAN_OPTIONS=halt_on_error=1:second_deadlock_stack=1` | `build-tsan-docker/` |

## Known limitations

- **First image build takes ~30 seconds** because `docker/Dockerfile` inherits from `ghcr.io/<owner>/ry-ci:llvm-21` — the heavy toolchain (LLVM 21, cmake, ninja, ccache, OpenSSL, cppcheck, gtest tarball) is pulled, not built. The first compile of ry itself takes 1-2 minutes; subsequent runs use ccache and complete in ~10-30 seconds. If GHCR is unreachable, Docker falls back to whatever image layers are already cached locally.
- On Apple Silicon, the container runs **arm64 Linux natively**. To test x86_64-specific behaviour, pass `--platform linux/amd64` manually to `docker run` (not wired into `run.sh` by default — qemu emulation is 5-10× slower and rarely needed).
- macOS builds (`build/`, `build-asan/`, `build-tsan/`) and Docker builds (`build-docker/`, `build-asan-docker/`, `build-tsan-docker/`) are **fully separate**. Running Docker commands will not overwrite your local CMake build.
- The dev image inherits from `ry-ci`, **not** `ry-ci-glibc-old`. Local Linux builds therefore link against glibc 2.40 (Debian trixie). To reproduce a release-style binary against glibc 2.36, override the base image: `docker build --build-arg CI_IMAGE_OWNER=<owner> --build-arg CI_IMAGE_TAG=llvm-21 -t ry-linux-dev:glibc-old docker/` — but only do this if you are debugging release-specific symbol issues; everyday dev does not need it.
