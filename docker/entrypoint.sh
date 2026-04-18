#!/usr/bin/env bash
set -euo pipefail

# Ensure ccache dir exists (volume may be new or owned by a previous root-run)
mkdir -p "${CCACHE_DIR:-/home/ubuntu/.cache/ccache}"

PRESET="${1:-default}"
shift || true

case "$PRESET" in
  default) BUILD_DIR="build" ;;
  asan)    BUILD_DIR="build-asan" ;;
  tsan)    BUILD_DIR="build-tsan" ;;
  *)       echo "error: unknown preset '$PRESET' (supported: default, asan, tsan)" >&2; exit 1 ;;
esac

# Build
cmake --preset "$PRESET"
cmake --build "$BUILD_DIR"

# If no command given, exit after build
if [[ $# -eq 0 ]]; then
  exit 0
fi

CMD="$1"
shift

case "$CMD" in
  ry_tests)
    exec "./$BUILD_DIR/ry_tests" "$@"
    ;;
  ry)
    exec "./$BUILD_DIR/ry" "$@"
    ;;
  bash)
    exec bash "$@"
    ;;
  *)
    echo "error: unknown command '$CMD' (supported: ry_tests, ry, bash)" >&2
    exit 1
    ;;
esac
