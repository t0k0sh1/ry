# ry Linux Docker Development Environment

Run tests in a Linux (Ubuntu 24.04 + glibc) environment from macOS, matching CI conditions for all sanitizer presets.

See [AGENTS.md](../AGENTS.md) (search for "Linux Docker") for full workflow documentation.

## Quick start

```bash
# First run builds the image (~5-10 min); subsequent runs use ccache (1-2 min)
./docker/run.sh default ry_tests
./docker/run.sh default ry test -p

# ASan + UBSan (mirrors CI asan job)
./docker/run.sh asan ry_tests
./docker/run.sh asan ry test -p
./docker/run.sh asan ry test tests/spec/combinatorial/collection_element.test.ry

# TSan (mirrors CI tsan job)
./docker/run.sh tsan ry_tests

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

## Notes

- Host build dirs (`build-docker/`, `build-asan-docker/`, `build-tsan-docker/`) are separate from native macOS builds (`build/`, `build-asan/`, `build-tsan/`). They will not interfere with each other.
- On Apple Silicon the container runs arm64 Linux natively (no x86_64 QEMU emulation).
- ccache is persisted in a named Docker volume (`ry-ccache-docker`). The first build compiles everything; subsequent runs reuse the cache.
- Image name: `ry-linux-dev:latest`. Built locally; not pushed to any registry.
