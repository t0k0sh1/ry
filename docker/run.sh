#!/usr/bin/env bash
# Run ry tests inside a Linux (Debian trixie + glibc 2.40, via the ry-ci GHCR image) container from macOS.
# Usage: docker/run.sh [--rebuild] [--clean] <preset> [cmd [args...]]
#        docker/run.sh [--rebuild] [--clean] static-analysis <tool>
#   --rebuild: rebuild the Docker image before running
#   --clean:   remove the host build-*-docker/ dir(s) for this preset before building
#              (the sanctioned replacement for an ad-hoc `rm -rf build-*-docker/` —
#               AGENTS.md §"Total ban on Claude-initiated ad-hoc deletion")
#   preset: default | asan | fuzz
#   cmd:    ry_tests | ry | bash | fuzz_parser | fuzz_json | fuzz_json5 | fuzz_utf8 | fuzz_io_open  (omit for build-only)
#   tool:   cppcheck | all
#   Examples:
#     ./docker/run.sh asan ry_tests
#     ./docker/run.sh asan ry test -p
#     ./docker/run.sh asan ry test tests/spec/combinatorial/collection_element.test.ry
#     ./docker/run.sh fuzz fuzz_parser -max_total_time=30 -artifact_prefix=tests/fuzz/regressions/parser/ tests/fuzz/corpus/parser
#     ./docker/run.sh static-analysis cppcheck
#     ./docker/run.sh default bash
#     ./docker/run.sh --rebuild asan ry_tests
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
IMAGE="ry-linux-dev:latest"
CCACHE_VOLUME="ry-ccache-docker"
# Persists the Rust cargo registry/cache across runs (CARGO_HOME is set to
# /home/ubuntu/.cargo by docker/Dockerfile; corrosion builds the emit
# cdylib on every preset since the #1993 cutover).
CARGO_VOLUME="ry-cargo-docker"

# Hard-fail when no Docker daemon is reachable. Issue #1865 documents
# macOS-host breakage (fuzz_json hang under ASan, libFuzzer SDKROOT) that
# this script is meant to escape — silently falling back to native would
# defeat its purpose.
if ! docker info >/dev/null 2>&1; then
  echo "error: Docker daemon is not running — start OrbStack, Colima, or Docker Desktop" >&2
  exit 1
fi

# Parse leading flags (--rebuild / --clean) in any order, before the subcommand.
REBUILD=0
CLEAN=0
while [[ "${1:-}" == --* ]]; do
  case "$1" in
    --rebuild) REBUILD=1; shift ;;
    --clean)   CLEAN=1;   shift ;;
    *) echo "error: unknown flag '$1' (supported: --rebuild, --clean)" >&2; exit 1 ;;
  esac
done

SUBCOMMAND="${1:-default}"

# Resolve subcommand: either a CMake preset or the static-analysis dispatch.
if [[ "$SUBCOMMAND" == "static-analysis" ]]; then
  PRESET="default"
  BUILD_DIR_HOST="build-docker"
  BUILD_DIR_CONTAINER="build"
  TOOL="${2:-all}"
  if [[ "$TOOL" != "cppcheck" && "$TOOL" != "all" ]]; then
    echo "error: unknown static-analysis tool '$TOOL' (supported: cppcheck, all)" >&2
    exit 1
  fi
  # Pass to entrypoint.sh as: <preset> static-analysis (TOOL value is ignored
  # downstream since cppcheck is the only remaining tool).
  set -- "$PRESET" static-analysis
else
  PRESET="$SUBCOMMAND"
  case "$PRESET" in
    default) BUILD_DIR_HOST="build-docker";      BUILD_DIR_CONTAINER="build" ;;
    asan)    BUILD_DIR_HOST="build-asan-docker"; BUILD_DIR_CONTAINER="build-asan" ;;
    fuzz)    BUILD_DIR_HOST="build-fuzz-docker"; BUILD_DIR_CONTAINER="build-fuzz" ;;
    *)       echo "error: unknown preset '$PRESET' (supported: default, asan, fuzz; or use 'static-analysis')" >&2; exit 1 ;;
  esac
fi

# Build image if absent or --rebuild requested
if [[ "$REBUILD" -eq 1 ]] || ! docker image inspect "$IMAGE" >/dev/null 2>&1; then
  echo "Building Docker image $IMAGE..."
  docker build -t "$IMAGE" "$SCRIPT_DIR"
fi

# When --clean is requested, remove the host build dir(s) for this preset before
# recreating them. This is the sanctioned replacement for an ad-hoc
# `rm -rf build-*-docker/` typed by hand (AGENTS.md §"Total ban on
# Claude-initiated ad-hoc deletion"): the only layer that knows the
# preset→host-dir mapping owns the removal. The ${PROJECT_DIR:?} guard refuses to
# run when PROJECT_DIR is somehow empty (defense against rm -rf /<dir>).
if [[ "$CLEAN" -eq 1 ]]; then
  echo "==> --clean: removing host build dir $BUILD_DIR_HOST" >&2
  rm -rf "${PROJECT_DIR:?}/$BUILD_DIR_HOST"
fi

# Ensure host build dir exists so Docker doesn't create it as root
mkdir -p "$PROJECT_DIR/$BUILD_DIR_HOST"

# Sanitizer env vars matching CI jobs.
# RY_HOST_BUILD_DIR is consumed by entrypoint.sh to print host-side rm -rf
# recovery commands when its leak/contamination guards fire.
ENV_ARGS=(-e "RY_HOST_BUILD_DIR=$BUILD_DIR_HOST")
case "$PRESET" in
  asan|fuzz)
    ENV_ARGS+=(
      -e "ASAN_OPTIONS=detect_container_overflow=0:detect_leaks=0:halt_on_error=1"
      -e "UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1"
    )
    ;;
esac

# Allocate a pseudo-TTY only when stdin is a terminal (omit -t in CI / non-interactive)
TTY_FLAG=()
if [[ -t 0 ]]; then
  TTY_FLAG=(-it)
else
  TTY_FLAG=(-i)
fi

# Note: --user $(id -u):$(id -g) intentionally not passed. The Dockerfile sets
# CCACHE_DIR=/home/ubuntu/.cache/ccache owned by container UID 1000; overriding
# the user would break ccache writes. macOS Docker runtimes (OrbStack VirtioFS,
# Docker Desktop gRPC-FUSE) transparently translate bind-mount UIDs.
# --tmpfs /tmp keeps test-created temp dirs and analyzer scratch off the overlay
# (Docker storage pools fill up; /workspace bind-mount uses host disk).
#
# Per-entry bind mounts (issue #1876): the previous "-v PROJECT_DIR:/workspace"
# outer mount let host build/, build-asan/, etc. (macOS Mach-O) leak into the
# container alongside the inner build-*-docker/ mount, breaking tools that
# read compile_commands.json when its paths still referenced /Users/...
# (historical clang-tidy failure in PR #1873 — clang-tidy job has since been
# retired). The mount list below exposes only what the build, tests, and
# static analysis actually read; adding a new top-level source dir or config
# file requires updating this list AND entrypoint.sh's required-mount guard.
MOUNT_ARGS=(
  -v "$PROJECT_DIR/src:/workspace/src"
  -v "$PROJECT_DIR/include:/workspace/include"
  -v "$PROJECT_DIR/tests:/workspace/tests"
  -v "$PROJECT_DIR/share:/workspace/share"
  # crates/ + Cargo.{toml,lock}: the LLVM IR emission lib (emit) is a
  # Rust cdylib built via corrosion on every preset (#1993 cutover), so corrosion
  # needs the workspace manifest, lockfile, and crate source on all builds.
  -v "$PROJECT_DIR/crates:/workspace/crates"
  -v "$PROJECT_DIR/Cargo.toml:/workspace/Cargo.toml"
  -v "$PROJECT_DIR/Cargo.lock:/workspace/Cargo.lock"
  -v "$PROJECT_DIR/CMakeLists.txt:/workspace/CMakeLists.txt"
  -v "$PROJECT_DIR/CMakePresets.json:/workspace/CMakePresets.json"
  -v "$PROJECT_DIR/package.toml:/workspace/package.toml"
  -v "$PROJECT_DIR/.cppcheck-suppressions:/workspace/.cppcheck-suppressions"
  # scripts/ + LICENSE-LLVM.txt (#2005): scripts/bundle-dist.sh + verify-bundle.sh
  # assemble and check the self-contained dist/ tree (bundling libLLVM), and the
  # LLVM license text ships in the tarball — so a local `docker/run.sh default
  # bash -c '... bundle-dist.sh linux build dist ...'` can validate the Linux path.
  -v "$PROJECT_DIR/scripts:/workspace/scripts"
  -v "$PROJECT_DIR/LICENSE-LLVM.txt:/workspace/LICENSE-LLVM.txt"
  -v "$PROJECT_DIR/$BUILD_DIR_HOST:/workspace/$BUILD_DIR_CONTAINER"
  -v "$CCACHE_VOLUME:/home/ubuntu/.cache/ccache"
  -v "$CARGO_VOLUME:/home/ubuntu/.cargo"
)

docker run --rm "${TTY_FLAG[@]}" \
  "${MOUNT_ARGS[@]}" \
  --tmpfs /tmp:rw,exec,size=2g \
  "${ENV_ARGS[@]+"${ENV_ARGS[@]}"}" \
  "$IMAGE" \
  "$@"
