# ry Linux Docker Development Environment

Run tests in a Linux environment (Debian trixie + glibc 2.40, via the pre-baked `ry-ci` GHCR image) from macOS, matching CI conditions for ASan/UBSan, libFuzzer, and cppcheck.

On macOS, use this environment for sanitizer, fuzzer, and static-analysis
verification. Do not silently fall back to native execution. Reset stale
Docker build directories with the owning command's `--clean` option; do not
manually delete them.

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

# libFuzzer (mirrors CI fuzz job)
./docker/run.sh fuzz fuzz_parser  -max_total_time=30 -artifact_prefix=tests/fuzz/regressions/parser/       tests/fuzz/corpus/parser
./docker/run.sh fuzz fuzz_json    -max_total_time=30 -artifact_prefix=tests/fuzz/regressions/json/         tests/fuzz/corpus/json
./docker/run.sh fuzz fuzz_utf8    -max_total_time=30 -artifact_prefix=tests/fuzz/regressions/utf8/         tests/fuzz/corpus/utf8
./docker/run.sh fuzz fuzz_io_open -max_total_time=30 -artifact_prefix=tests/fuzz/regressions/fuzz_io_open/ tests/fuzz/corpus/fuzz_io_open

# Static analysis (mirrors CI lint job)
./docker/run.sh static-analysis cppcheck

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
| `fuzz` | ASan + UBSan | same as `asan` | `build-fuzz-docker/` |

## Static analysis tools

`./docker/run.sh static-analysis <tool>` invokes the requested tool inside the container using the LLVM 21 toolchain pre-baked in `ry-ci:llvm-21`:

| Tool | Source | Notes |
|------|--------|-------|
| `cppcheck` | `/opt/cppcheck/bin/cppcheck` | No build required |
| `all` | — | Same as `cppcheck` for now |

## Notes

- Host build dirs (`build-docker/`, `build-asan-docker/`, `build-fuzz-docker/`) are separate from native macOS builds (`build/`, `build-asan/`, `build-fuzz/`). The container only sees the per-preset Docker build dir, never the host macOS ones.
- `run.sh` bind-mounts source/config entries individually (`src/`, `include/`, `tests/`, `share/`, `CMakeLists.txt`, `CMakePresets.json`, `package.toml`, `.cppcheck-suppressions`) rather than the whole project root, preventing host macOS build artifacts from appearing inside the container.
- When the build consumes a new top-level source directory, config file, or dotfile, add it to `run.sh` mount arguments and, when required at startup, `entrypoint.sh` required-mount checks. Verify the container sees the new path.
- `entrypoint.sh` fails fast (exit codes 70/71/72) if a required mount is missing, a macOS Mach-O binary slips into the per-preset build dir, or `/Users/...` paths appear in `compile_commands.json`.
- On Apple Silicon the container runs arm64 Linux natively (no x86_64 QEMU emulation).
- ccache is persisted in a named Docker volume (`ry-ccache-docker`). The first build compiles everything; subsequent runs reuse the cache.
- Image name: `ry-linux-dev:latest`. Built locally; not pushed to any registry.
- The script hard-fails when no Docker daemon is reachable (no silent fallback to macOS-native execution — see issue #1865 for the rationale).
- **Recommended runtime: OrbStack** for VirtioFS bind-mount throughput (matters for fuzz corpus I/O). Colima and Docker Desktop are supported alternatives.
