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
  fuzz)    BUILD_DIR="build-fuzz" ;;
  *)       echo "error: unknown preset '$PRESET' (supported: default, asan, tsan, fuzz)" >&2; exit 1 ;;
esac

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
  fuzz_parser|fuzz_json|fuzz_utf8)
    exec "./$BUILD_DIR/$CMD" "$@"
    ;;
  bash)
    exec bash "$@"
    ;;
  *)
    echo "error: unknown command '$CMD' (supported: ry_tests, ry, bash, fuzz_parser, fuzz_json, fuzz_utf8)" >&2
    exit 1
    ;;
esac
