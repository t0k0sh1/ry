#!/usr/bin/env bash
# §3 Run All Tests — cmake default preset + ry_tests + ry test -p
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."

CLEAN=0
for arg in "$@"; do
  case "$arg" in
    --clean) CLEAN=1 ;;
    -h|--help)
      sed -n '2p;3,8p' "$0"
      echo "Usage: run-tests.sh [--clean]"
      exit 0
      ;;
    *) echo "unknown option: $arg" >&2; exit 2 ;;
  esac
done

# Auto-heal build/ when its CMakeCache.txt was generated for a sanitizer/fuzzer
# preset (e.g. user ran cmake --preset asan in build/ by mistake). This is the
# only host-native build dir; sanitizer / fuzz / static-analysis go through
# docker/run.sh which uses separate build-*-docker/ dirs.
if (( CLEAN == 1 )); then
  rm -rf build
elif [[ -f build/CMakeCache.txt ]] && \
     grep -qE '^(ENABLE_ASAN|ENABLE_TSAN|ENABLE_UBSAN|ENABLE_FUZZER):BOOL=ON$' build/CMakeCache.txt; then
  echo "==> build/CMakeCache.txt has sanitizer/fuzzer preset enabled — removing" >&2
  rm -rf build
fi

cmake --preset default
cmake --build build
./build/ry_tests
./build/ry test -p
