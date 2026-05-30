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
    /workspace/package.toml; do
  if [[ ! -f "$_required_file" ]]; then
    echo "fatal: required file mount $_required_file is missing or not a regular file — docker/run.sh MOUNT_ARGS drifted" >&2
    exit 70
  fi
done
for _required_dir in \
    /workspace/src \
    /workspace/include \
    /workspace/tests \
    /workspace/share; do
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
  tsan)    BUILD_DIR="build-tsan" ;;
  fuzz)    BUILD_DIR="build-fuzz" ;;
  *)       echo "error: unknown preset '$PRESET' (supported: default, asan, tsan, fuzz)" >&2; exit 1 ;;
esac

# Guard stages 2-3 (issue #1876): detect host build artifact leaks into the
# container's BUILD_DIR. Stage 2 catches macOS Mach-O binaries left over from
# a host `cmake --build`; stage 3 catches compile_commands.json entries with
# /Users/... paths (the symptom that broke clang-tidy in PR #1873).
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
      echo "  recovery: rm -rf $_host_build_dir/ (on host) and rerun" >&2
      exit 71
    fi
  fi
done
unset _bin _magic
if [[ -f "$BUILD_DIR/compile_commands.json" ]] \
    && grep -Eq '"directory"[[:space:]]*:[[:space:]]*"/Users/' "$BUILD_DIR/compile_commands.json"; then
  echo "fatal: $BUILD_DIR/compile_commands.json references macOS host paths" >&2
  echo "  recovery: rm -rf $_host_build_dir/ (on host) and rerun" >&2
  exit 72
fi
unset _host_build_dir

# static-analysis subcommand: each tool manages its own build needs
if [[ "${1:-}" == "static-analysis" ]]; then
  shift
  TOOL="${1:-all}"
  shift || true
  TOOLS=()
  case "$TOOL" in
    clang-tidy|cppcheck|scan-build) TOOLS=("$TOOL") ;;
    all)                            TOOLS=(clang-tidy cppcheck scan-build) ;;
    *) echo "error: unknown static-analysis tool '$TOOL' (supported: clang-tidy, cppcheck, scan-build, all)" >&2; exit 1 ;;
  esac

  # clang-tidy needs compile_commands.json from a real build; cppcheck does not;
  # scan-build wraps its own configure+build below.
  NEEDS_PREBUILD=0
  for t in "${TOOLS[@]}"; do
    [[ "$t" == "clang-tidy" ]] && NEEDS_PREBUILD=1
  done
  if [[ "$NEEDS_PREBUILD" -eq 1 ]]; then
    cmake --preset "$PRESET"
    cmake --build "$BUILD_DIR"
  fi

  FAIL=0
  for t in "${TOOLS[@]}"; do
    echo "=== static-analysis: $t ==="
    set +e
    case "$t" in
      clang-tidy)
        find src -name '*.cpp' -print0 \
          | xargs -0 -n 1 -P "$(nproc)" clang-tidy -p "$BUILD_DIR" --quiet
        ;;
      cppcheck)
        cppcheck --enable=warning,performance,portability --std=c++17 \
          --suppressions-list=.cppcheck-suppressions --inline-suppr \
          -i build -i build-asan -i build-tsan -i build-fuzz -i build-debug \
          -j "$(nproc)" --quiet src/ include/
        ;;
      scan-build)
        # Use a dedicated build dir (bind-mounted to host build-scan-docker/) so
        # the analyzer-wrapped CMakeCache never touches $BUILD_DIR. Without this
        # split, a subsequent `./docker/run.sh default ...` would either rebuild
        # against scan-build's wrapper CC/CXX or require `rm -rf build-docker/`
        # to recover.
        SCAN_BUILD_DIR="build-scan"
        REPORT_DIR="/workspace/$SCAN_BUILD_DIR/scan-build-report"
        mkdir -p "$REPORT_DIR"
        # Mirror CMakePresets.json "default" cache vars (LLVM_DIR + Release)
        # rather than `--preset default` because the preset hardcodes binaryDir.
        scan-build --use-analyzer=/usr/local/llvm/bin/clang \
                   --use-cc=/usr/local/llvm/bin/clang \
                   --use-c++=/usr/local/llvm/bin/clang++ \
                   -o "$REPORT_DIR" \
                   cmake -S /workspace -B "/workspace/$SCAN_BUILD_DIR" -G Ninja \
                     -DLLVM_DIR=/usr/local/llvm/lib/cmake/llvm \
                     -DCMAKE_BUILD_TYPE=Release \
          && scan-build --use-analyzer=/usr/local/llvm/bin/clang \
                        --use-cc=/usr/local/llvm/bin/clang \
                        --use-c++=/usr/local/llvm/bin/clang++ \
                        -o "$REPORT_DIR" \
                        --status-bugs \
                        cmake --build "/workspace/$SCAN_BUILD_DIR" --target ry --parallel
        ;;
    esac
    RC=$?
    set -e
    if [[ $RC -ne 0 ]]; then
      echo "=== static-analysis: $t exited with $RC ==="
      FAIL=$RC
    fi
  done
  exit $FAIL
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
  fuzz_parser|fuzz_json|fuzz_utf8|fuzz_io_open)
    exec "./$BUILD_DIR/$CMD" "$@"
    ;;
  bash)
    exec bash "$@"
    ;;
  *)
    echo "error: unknown command '$CMD' (supported: ry_tests, ry, bash, fuzz_parser, fuzz_json, fuzz_utf8, fuzz_io_open)" >&2
    exit 1
    ;;
esac
