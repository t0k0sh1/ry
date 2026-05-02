#!/usr/bin/env bash
# Run ry tests inside a Linux (Debian trixie + glibc 2.40, via the ry-ci GHCR image) container from macOS.
# Usage: docker/run.sh [--rebuild] <preset> [cmd [args...]]
#   preset: default | asan | tsan
#   cmd:    ry_tests | ry | bash  (omit for build-only)
#   Examples:
#     ./docker/run.sh asan ry_tests
#     ./docker/run.sh asan ry test -p
#     ./docker/run.sh asan ry test tests/spec/combinatorial/collection_element.test.ry
#     ./docker/run.sh default bash
#     ./docker/run.sh --rebuild asan ry_tests
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
IMAGE="ry-linux-dev:latest"
CCACHE_VOLUME="ry-ccache-docker"

# Parse --rebuild flag
REBUILD=0
if [[ "${1:-}" == "--rebuild" ]]; then
  REBUILD=1
  shift
fi

PRESET="${1:-default}"

# Validate preset and resolve host build dir
case "$PRESET" in
  default) BUILD_DIR_HOST="build-docker";     BUILD_DIR_CONTAINER="build" ;;
  asan)    BUILD_DIR_HOST="build-asan-docker"; BUILD_DIR_CONTAINER="build-asan" ;;
  tsan)    BUILD_DIR_HOST="build-tsan-docker"; BUILD_DIR_CONTAINER="build-tsan" ;;
  *)       echo "error: unknown preset '$PRESET' (supported: default, asan, tsan)" >&2; exit 1 ;;
esac

# Build image if absent or --rebuild requested
if [[ "$REBUILD" -eq 1 ]] || ! docker image inspect "$IMAGE" >/dev/null 2>&1; then
  echo "Building Docker image $IMAGE..."
  docker build -t "$IMAGE" "$SCRIPT_DIR"
fi

# Ensure host build dir exists so Docker doesn't create it as root
mkdir -p "$PROJECT_DIR/$BUILD_DIR_HOST"

# Sanitizer env vars matching CI jobs
ENV_ARGS=()
case "$PRESET" in
  asan)
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

docker run --rm "${TTY_FLAG[@]}" \
  -v "$PROJECT_DIR:/workspace" \
  -v "$PROJECT_DIR/$BUILD_DIR_HOST:/workspace/$BUILD_DIR_CONTAINER" \
  -v "$CCACHE_VOLUME:/home/ubuntu/.cache/ccache" \
  "${ENV_ARGS[@]+"${ENV_ARGS[@]}"}" \
  "$IMAGE" \
  "$@"
