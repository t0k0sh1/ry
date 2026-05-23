# ry Linux Docker Development Environment

Run tests in a Linux environment (Debian trixie + glibc 2.40, via the pre-baked `ry-ci` GHCR image) from macOS, matching CI conditions for all sanitizer presets, libFuzzer, and static analysis.

See [`.claude/skills/linux-docker-dev/SKILL.md`](../.claude/skills/linux-docker-dev/SKILL.md) for full workflow documentation.

## Quick start

```bash
# First run pulls the GHCR base image and builds a thin local layer (~30s);
# the first ry compile takes 1-2 min, subsequent runs use ccache (~10-30s)
./docker/run.sh default ry_tests
./docker/run.sh default ry test -p

# ASan + UBSan (mirrors CI asan job)
./docker/run.sh asan ry_tests
./docker/run.sh asan ry test -p
./docker/run.sh asan ry test tests/spec/combinatorial/collection_element.test.ry

# TSan (mirrors CI tsan job)
./docker/run.sh tsan ry_tests

# libFuzzer (mirrors CI fuzz job)
./docker/run.sh fuzz fuzz_parser -max_total_time=30 -artifact_prefix=tests/fuzz/regressions/parser/ tests/fuzz/corpus/parser
./docker/run.sh fuzz fuzz_json   -max_total_time=30 -artifact_prefix=tests/fuzz/regressions/json/   tests/fuzz/corpus/json
./docker/run.sh fuzz fuzz_utf8   -max_total_time=30 -artifact_prefix=tests/fuzz/regressions/utf8/   tests/fuzz/corpus/utf8

# Static analysis (mirrors CI clang-tidy / lint / scan-build jobs)
./docker/run.sh static-analysis clang-tidy
./docker/run.sh static-analysis cppcheck
./docker/run.sh static-analysis scan-build
./docker/run.sh static-analysis all

# Interactive shell
./docker/run.sh default bash

# Force image rebuild (after Dockerfile changes)
./docker/run.sh --rebuild asan ry_tests
```

## Presets

| Preset | Sanitizers | Sanitizer env vars | Host build dir |
|--------|-----------|-------------------|----------------|
| `default` | none | — | `build-docker/` |
| `asan` | ASan + UBSan | `ASAN_OPTIONS=detect_container_overflow=0:detect_leaks=0:halt_on_error=1`<br>`UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1` | `build-asan-docker/` |
| `tsan` | TSan | `TSAN_OPTIONS=halt_on_error=1:second_deadlock_stack=1` | `build-tsan-docker/` |
| `fuzz` | ASan + UBSan | same as `asan` | `build-fuzz-docker/` |

## Static analysis tools

`./docker/run.sh static-analysis <tool>` invokes the requested tool inside the container using the LLVM 21 toolchain pre-baked in `ry-ci:llvm-21`:

| Tool | Source | Notes |
|------|--------|-------|
| `clang-tidy` | `/usr/local/llvm/bin/clang-tidy` | Reuses `build-docker/` for `compile_commands.json` |
| `cppcheck` | `/opt/cppcheck/bin/cppcheck` | No build required |
| `scan-build` | `/usr/local/llvm/bin/scan-build` | Configures+builds in dedicated `build-scan-docker/` (host bind-mount), HTML report at `build-scan-docker/scan-build-report/<timestamp>/`, exits non-zero on findings via `--status-bugs` |
| `all` | — | Runs clang-tidy → cppcheck → scan-build in sequence |

> **`scan-build` and `all`** isolate their analyzer-wrapped CMake configuration in `build-scan-docker/` (host) ↔ `build-scan/` (container). `build-docker/` is untouched, so `./docker/run.sh default ...` works without an intervening cleanup. To wipe the analyzer report, remove `build-scan-docker/` directly (the directory is also recreated on the next scan-build run).

## Notes

- Host build dirs (`build-docker/`, `build-asan-docker/`, `build-tsan-docker/`, `build-fuzz-docker/`, `build-scan-docker/`) are separate from native macOS builds (`build/`, `build-asan/`, `build-tsan/`, `build-fuzz/`). They will not interfere with each other.
- On Apple Silicon the container runs arm64 Linux natively (no x86_64 QEMU emulation).
- ccache is persisted in a named Docker volume (`ry-ccache-docker`). The first build compiles everything; subsequent runs reuse the cache.
- Image name: `ry-linux-dev:latest`. Built locally; not pushed to any registry.
- The script hard-fails when no Docker daemon is reachable (no silent fallback to macOS-native execution — see issue #1865 for the rationale).
- **Recommended runtime: OrbStack** for VirtioFS bind-mount throughput (matters for fuzz corpus I/O). Colima and Docker Desktop are supported alternatives.
