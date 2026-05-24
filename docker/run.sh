#!/usr/bin/env bash
# Run ry tests inside a Linux (Debian trixie + glibc 2.40, via the ry-ci GHCR image) container from macOS.
# Usage: docker/run.sh [--rebuild] <preset> [cmd [args...]]
#        docker/run.sh [--rebuild] static-analysis <tool>
#   preset: default | asan | tsan | fuzz
#   cmd:    ry_tests | ry | bash | fuzz_parser | fuzz_json | fuzz_utf8  (omit for build-only)
#   tool:   clang-tidy | cppcheck | scan-build | all
#   Examples:
#     ./docker/run.sh asan ry_tests
#     ./docker/run.sh asan ry test -p
#     ./docker/run.sh asan ry test tests/spec/combinatorial/collection_element.test.ry
#     ./docker/run.sh tsan ry_tests
#     ./docker/run.sh fuzz fuzz_parser -max_total_time=30 -artifact_prefix=tests/fuzz/regressions/parser/ tests/fuzz/corpus/parser
#     ./docker/run.sh static-analysis clang-tidy
#     ./docker/run.sh static-analysis all
#     ./docker/run.sh default bash
#     ./docker/run.sh --rebuild asan ry_tests
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
IMAGE="ry-linux-dev:latest"
CCACHE_VOLUME="ry-ccache-docker"

# Hard-fail when no Docker daemon is reachable. Issue #1865 documents
# macOS-host breakage (fuzz_json hang under ASan, TSan allocator bug,
# clang-tidy PCH incompatibility, scan-build PATH friction, libFuzzer SDKROOT)
# that this script is meant to escape — silently falling back to native would
# defeat its purpose.
if ! docker info >/dev/null 2>&1; then
  echo "error: Docker daemon is not running — start OrbStack, Colima, or Docker Desktop" >&2
  exit 1
fi

# Parse --rebuild flag
REBUILD=0
if [[ "${1:-}" == "--rebuild" ]]; then
  REBUILD=1
  shift
fi

SUBCOMMAND="${1:-default}"
SCAN_BUILD_DIR_HOST=""
SCAN_BUILD_DIR_CONTAINER=""

# Resolve subcommand: either a CMake preset or the static-analysis dispatch.
if [[ "$SUBCOMMAND" == "static-analysis" ]]; then
  PRESET="default"
  BUILD_DIR_HOST="build-docker"
  BUILD_DIR_CONTAINER="build"
  TOOL="${2:-all}"
  case "$TOOL" in
    clang-tidy|cppcheck|scan-build|all) ;;
    *) echo "error: unknown static-analysis tool '$TOOL' (supported: clang-tidy, cppcheck, scan-build, all)" >&2; exit 1 ;;
  esac
  # scan-build and 'all' use a dedicated build dir so the analyzer-wrapped
  # CMakeCache never contaminates build-docker/. The HTML report lands here
  # too (build-scan-docker/scan-build-report/<timestamp>/).
  case "$TOOL" in
    scan-build|all)
      SCAN_BUILD_DIR_HOST="build-scan-docker"
      SCAN_BUILD_DIR_CONTAINER="build-scan"
      ;;
  esac
  # Pass to entrypoint.sh as: <preset> static-analysis <tool>
  set -- "$PRESET" static-analysis "$TOOL"
else
  PRESET="$SUBCOMMAND"
  case "$PRESET" in
    default) BUILD_DIR_HOST="build-docker";      BUILD_DIR_CONTAINER="build" ;;
    asan)    BUILD_DIR_HOST="build-asan-docker"; BUILD_DIR_CONTAINER="build-asan" ;;
    tsan)    BUILD_DIR_HOST="build-tsan-docker"; BUILD_DIR_CONTAINER="build-tsan" ;;
    fuzz)    BUILD_DIR_HOST="build-fuzz-docker"; BUILD_DIR_CONTAINER="build-fuzz" ;;
    *)       echo "error: unknown preset '$PRESET' (supported: default, asan, tsan, fuzz; or use 'static-analysis')" >&2; exit 1 ;;
  esac
fi

# Build image if absent or --rebuild requested
if [[ "$REBUILD" -eq 1 ]] || ! docker image inspect "$IMAGE" >/dev/null 2>&1; then
  echo "Building Docker image $IMAGE..."
  docker build -t "$IMAGE" "$SCRIPT_DIR"
fi

# Ensure host build dir exists so Docker doesn't create it as root
mkdir -p "$PROJECT_DIR/$BUILD_DIR_HOST"
if [[ -n "$SCAN_BUILD_DIR_HOST" ]]; then
  mkdir -p "$PROJECT_DIR/$SCAN_BUILD_DIR_HOST"
fi

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
  tsan)
    ENV_ARGS+=(
      -e "TSAN_OPTIONS=halt_on_error=1:second_deadlock_stack=1"
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
# container alongside the inner build-*-docker/ mount, which caused clang-tidy
# to fail when compile_commands.json referenced /Users/... paths. The mount
# list below exposes only what the build, tests, and static analysis actually
# read; adding a new top-level source dir or config file requires updating
# this list AND entrypoint.sh's required-mount guard.
MOUNT_ARGS=(
  -v "$PROJECT_DIR/src:/workspace/src"
  -v "$PROJECT_DIR/include:/workspace/include"
  -v "$PROJECT_DIR/tests:/workspace/tests"
  -v "$PROJECT_DIR/share:/workspace/share"
  -v "$PROJECT_DIR/CMakeLists.txt:/workspace/CMakeLists.txt"
  -v "$PROJECT_DIR/CMakePresets.json:/workspace/CMakePresets.json"
  -v "$PROJECT_DIR/package.toml:/workspace/package.toml"
  -v "$PROJECT_DIR/.clang-tidy:/workspace/.clang-tidy"
  -v "$PROJECT_DIR/.cppcheck-suppressions:/workspace/.cppcheck-suppressions"
  -v "$PROJECT_DIR/$BUILD_DIR_HOST:/workspace/$BUILD_DIR_CONTAINER"
  -v "$CCACHE_VOLUME:/home/ubuntu/.cache/ccache"
)
if [[ -n "$SCAN_BUILD_DIR_HOST" ]]; then
  MOUNT_ARGS+=(-v "$PROJECT_DIR/$SCAN_BUILD_DIR_HOST:/workspace/$SCAN_BUILD_DIR_CONTAINER")
fi

docker run --rm "${TTY_FLAG[@]}" \
  "${MOUNT_ARGS[@]}" \
  --tmpfs /tmp:rw,exec,size=2g \
  "${ENV_ARGS[@]+"${ENV_ARGS[@]}"}" \
  "$IMAGE" \
  "$@"
