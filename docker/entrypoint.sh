#!/usr/bin/env bash
set -euo pipefail

# Guard stage 1 (issue #1876): required source mounts must be present and of
# the expected type. run.sh switched to per-entry bind mounts to keep the
# host's macOS Mach-O build/ from leaking via an outer PROJECT_DIR:/workspace
# mount; if its MOUNT_ARGS drifts (e.g. a new top-level config file is added
# without updating run.sh, or a file mount is accidentally written as a dir
# mount), fail loudly here instead of silently producing a misconfigured CMake
# cache. Type-checking with -f / -d (rather than -e) catches a directory
# mounted where a file was expected, which would otherwise pass -e and break
# later in less obvious ways.
for _required_file in \
    /workspace/CMakeLists.txt \
    /workspace/CMakePresets.json \
    /workspace/package.toml \
    /workspace/Cargo.toml \
    /workspace/Cargo.lock \
    /workspace/LICENSE-LLVM.txt; do
  if [[ ! -f "$_required_file" ]]; then
    echo "fatal: required file mount $_required_file is missing or not a regular file — docker/run.sh MOUNT_ARGS drifted" >&2
    exit 70
  fi
done
for _required_dir in \
    /workspace/src \
    /workspace/include \
    /workspace/tests \
    /workspace/share \
    /workspace/crates \
    /workspace/scripts; do
  if [[ ! -d "$_required_dir" ]]; then
    echo "fatal: required directory mount $_required_dir is missing or not a directory — docker/run.sh MOUNT_ARGS drifted" >&2
    exit 70
  fi
done
unset _required_file _required_dir

# Ensure ccache dir exists (volume may be new or owned by a previous root-run)
mkdir -p "${CCACHE_DIR:-/home/ubuntu/.cache/ccache}"

PRESET="${1:-default}"
shift || true

case "$PRESET" in
  default) BUILD_DIR="build" ;;
  asan)    BUILD_DIR="build-asan" ;;
  fuzz)    BUILD_DIR="build-fuzz" ;;
  *)       echo "error: unknown preset '$PRESET' (supported: default, asan, fuzz)" >&2; exit 1 ;;
esac

# Guard stages 2-3 (issue #1876): detect host build artifact leaks into the
# container's BUILD_DIR. Stage 2 catches macOS Mach-O binaries left over from
# a host `cmake --build`; stage 3 catches compile_commands.json entries with
# /Users/... paths (the symptom that historically broke clang-tidy in PR
# #1873 — the clang-tidy job has since been retired but the same leak would
# break any downstream tool that reads compile_commands.json).
# $RY_HOST_BUILD_DIR is set by run.sh so the recovery hint names the host-side
# directory (e.g. build-asan-docker/) instead of the container path.
_host_build_dir="${RY_HOST_BUILD_DIR:-$BUILD_DIR}"
for _bin in "$BUILD_DIR/ry" "$BUILD_DIR/ry_tests"; do
  if [[ -f "$_bin" ]]; then
    # ELF magic is 0x7f 0x45 0x4c 0x46. Use `od` for a locale-independent
    # byte-level comparison; piping head|grep would mis-match on systems
    # where grep treats the input as text under a non-C locale.
    _magic=$(od -An -N 4 -tx1 "$_bin" 2>/dev/null | tr -d ' \n')
    if [[ "$_magic" != "7f454c46" ]]; then
      echo "fatal: $_bin is not an ELF binary (magic=$_magic) — macOS host build artifact leaked into container" >&2
      echo "  recovery: re-run the same docker/run.sh command on the host with --clean (wipes $_host_build_dir/ and rebuilds)" >&2
      exit 71
    fi
  fi
done
unset _bin _magic
if [[ -f "$BUILD_DIR/compile_commands.json" ]] \
    && grep -Eq '"directory"[[:space:]]*:[[:space:]]*"/Users/' "$BUILD_DIR/compile_commands.json"; then
  echo "fatal: $BUILD_DIR/compile_commands.json references macOS host paths" >&2
  echo "  recovery: re-run the same docker/run.sh command on the host with --clean (wipes $_host_build_dir/ and rebuilds)" >&2
  exit 72
fi
unset _host_build_dir

# static-analysis subcommand: cppcheck only (build-free; no prebuild step needed).
# Flags mirror the CI lint job in `.github/workflows/ci.yml`.
if [[ "${1:-}" == "static-analysis" ]]; then
  echo "=== static-analysis: cppcheck ==="
  cppcheck --enable=warning,performance,portability --std=c++17 \
    --error-exitcode=1 \
    --suppressions-list=.cppcheck-suppressions \
    --suppress=normalCheckLevelMaxBranches \
    --inline-suppr \
    -i build -i build-asan -i build-fuzz -i build-debug \
    -j "$(nproc)" --quiet src/ include/
  exit $?
fi

# Normal preset path: build then dispatch command
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
  fuzz_parser|fuzz_json|fuzz_json5|fuzz_utf8|fuzz_io_open)
    exec "./$BUILD_DIR/$CMD" "$@"
    ;;
  bash)
    exec bash "$@"
    ;;
  *)
    echo "error: unknown command '$CMD' (supported: ry_tests, ry, bash, fuzz_parser, fuzz_json, fuzz_json5, fuzz_utf8, fuzz_io_open)" >&2
    exit 1
    ;;
esac
