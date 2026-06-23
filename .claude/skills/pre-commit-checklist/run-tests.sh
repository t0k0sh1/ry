#!/usr/bin/env bash
# §3 Run All Tests — host build (Linux: default/build, macOS: rust-emit/build-rust) + ry_tests + ry test -p
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."

CLEAN=0
for arg in "$@"; do
  case "$arg" in
    --clean) CLEAN=1 ;;
    -h|--help)
      sed -n '2p;3,8p' "$0"
      echo "Usage: run-tests.sh [--clean]"
      echo "  --clean: remove the host build dir before building"
      echo "Env: RY_TEST_PRESET (default|rust-emit), RY_BUILD_DIR, RY_RUST_COMPILER"
      exit 0
      ;;
    *) echo "unknown option: $arg" >&2; exit 2 ;;
  esac
done

# Select the cmake preset + build dir for the host. Linux/CI links the static
# /usr/local/llvm via the `default` preset (build/). macOS's /usr/local/llvm is
# static-only (no libLLVM.dylib), so the Rust emit cdylib's shared-libLLVM
# requirement (#1997) forces the `rust-emit` preset (build-rust/, Homebrew llvm@21).
# Override with RY_TEST_PRESET / RY_BUILD_DIR. (#2011)
PRESET="${RY_TEST_PRESET:-}"
if [[ -z "$PRESET" ]]; then
  case "$(uname -s)" in
    Darwin) PRESET=rust-emit ;;
    *)      PRESET=default ;;
  esac
fi
case "$PRESET" in
  default)   BUILD_DIR=build ;;
  rust-emit) BUILD_DIR=build-rust ;;
  *) echo "unsupported preset: $PRESET (expected default|rust-emit)" >&2; exit 2 ;;
esac
BUILD_DIR="${RY_BUILD_DIR:-$BUILD_DIR}"

# corrosion's FindRust can choke on a broken cross toolchain (e.g. a rustup-managed
# Windows target); pin the host rustc on the rust-emit path when rustup manages the
# toolchain. Harmless when FindRust would have succeeded, and never added on the
# Linux/default path. Override the path with RY_RUST_COMPILER. (#2011)
CONFIGURE_ARGS=()
if [[ "$PRESET" == "rust-emit" ]]; then
  if [[ -n "${RY_RUST_COMPILER:-}" ]]; then
    CONFIGURE_ARGS+=("-DRust_COMPILER=$RY_RUST_COMPILER")
  elif command -v rustup >/dev/null 2>&1 \
       && rustc_path="$(rustup which rustc 2>/dev/null)" && [[ -n "$rustc_path" ]]; then
    CONFIGURE_ARGS+=("-DRust_COMPILER=$rustc_path")
  fi
fi

echo "==> preset=$PRESET build-dir=$BUILD_DIR" >&2

# Auto-heal the host build dir when its CMakeCache.txt was generated for a
# sanitizer/fuzzer preset (e.g. user ran cmake --preset asan in it by mistake).
# This is the only host-native build dir; sanitizer / fuzz / static-analysis go
# through docker/run.sh which uses separate build-*-docker/ dirs.
if (( CLEAN == 1 )); then
  rm -rf "$BUILD_DIR"
elif [[ -f "$BUILD_DIR/CMakeCache.txt" ]] && \
     grep -qE '^(ENABLE_ASAN|ENABLE_UBSAN|ENABLE_FUZZER):BOOL=ON$' "$BUILD_DIR/CMakeCache.txt"; then
  echo "==> $BUILD_DIR/CMakeCache.txt has sanitizer/fuzzer preset enabled — removing" >&2
  rm -rf "$BUILD_DIR"
fi

cmake --preset "$PRESET" "${CONFIGURE_ARGS[@]+"${CONFIGURE_ARGS[@]}"}"
cmake --build "$BUILD_DIR"

# Sweep orphan cdylibs (#2041): a corrosion crate rename (libry_codegen -> libemit,
# #2040) leaves the old cdylib in $BUILD_DIR/lib. A non-destructive reconfigure
# self-heals build.ninja to libemit but does NOT GC the stale output, so without
# --clean the orphan lingers (confusing, and bundle-dist would ship it). The cdylib
# is the ONLY native lib that links libLLVM — stdlib libry_* libs do not — and
# libemit is outside the libry_* glob, so any libry_* that links libLLVM is an
# orphan former-cdylib. rm here is a persistent, reviewed script deletion (same
# class as the --clean rm above), not an ad-hoc cleanup.
_links_libllvm() {
  case "$(uname -s)" in
    Darwin) otool -L "$1" 2>/dev/null | grep -q 'libLLVM' ;;
    *)      readelf -d "$1" 2>/dev/null | grep -q 'NEEDED.*libLLVM' ;;
  esac
}
for f in "$BUILD_DIR"/lib/libry_*.dylib "$BUILD_DIR"/lib/libry_*.so; do
  [[ -e "$f" ]] || continue
  if _links_libllvm "$f"; then
    echo "==> removing orphan cdylib $(basename "$f") (links libLLVM but is not libemit; #2041)" >&2
    rm -f "$f"
  fi
done

"./$BUILD_DIR/ry_tests"
"./$BUILD_DIR/ry" test -p
