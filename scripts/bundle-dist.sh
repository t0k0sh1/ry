#!/usr/bin/env bash
# Assemble a self-contained distribution tree from a build tree (#2005).
#
# After the #1999 C++->Rust cutover, `ry` requires a SHARED libLLVM at runtime
# (static link hangs ConstantFP::get via the APFloat fltSemantics singleton
# split, #1997). release.yml packages build/ry directly (not via cmake --install),
# so the shipped binary carries build-machine-absolute install names / rpaths.
# This script copies libLLVM (+ its non-system chain dep libzstd on macOS) and the
# Rust cdylib next to `ry`, then rewrites every absolute reference to a relocatable
# rpath so the tarball runs on an end-user system without a Homebrew/-installed LLVM.
#
# Layout produced (consumed by scripts/verify-bundle.sh and install.sh):
#   DIST_DIR/ry
#   DIST_DIR/lib/{libLLVM.*,libzstd.1.dylib(macOS),libry_llvm_emit.*,libry_*.*}
#   DIST_DIR/share/std/
#   DIST_DIR/LICENSE-LLVM.txt
#
# `ry` gets TWO rpaths so one binary works both unpacked-in-place and installed:
#   @loader_path/lib            / $ORIGIN/lib            -> tarball-direct `./ry`
#   @loader_path/../../.ry/lib  / $ORIGIN/../../.ry/lib  -> installed (~/.local/bin/ry -> ~/.ry/lib)
# (~/.local/bin and ~/.ry are both directly under $HOME, so the relative offset is fixed.)
#
# openssl is intentionally NOT bundled (v0.0.25 parity; bundling crypto blocks OS
# security updates) — see verify-bundle.sh header.
#
# Usage: bundle-dist.sh PLATFORM BUILD_DIR DIST_DIR
#          PLATFORM   darwin | linux
#          BUILD_DIR  cmake build dir (build-rust locally on macOS, build in CI)
#          DIST_DIR   output dir (created/overwritten)
set -euo pipefail

PLATFORM="${1:?usage: bundle-dist.sh PLATFORM BUILD_DIR DIST_DIR}"
BUILD_DIR="${2:?usage: bundle-dist.sh PLATFORM BUILD_DIR DIST_DIR}"
DIST_DIR="${3:?usage: bundle-dist.sh PLATFORM BUILD_DIR DIST_DIR}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RPATH_TARBALL_MAC='@loader_path/lib'
RPATH_INSTALL_MAC='@loader_path/../../.ry/lib'

log() { echo "bundle-dist: $*"; }

rm -rf "$DIST_DIR"
mkdir -p "$DIST_DIR/lib" "$DIST_DIR/share"

[[ -f "$BUILD_DIR/ry" ]] || { echo "bundle-dist: $BUILD_DIR/ry not found — build first" >&2; exit 1; }
cp "$BUILD_DIR/ry" "$DIST_DIR/ry"

# Stdlib native libs (libry_*, including libry_llvm_emit) ship as-is from the build tree.
shopt -s nullglob
native_libs=("$BUILD_DIR"/lib/libry_*.*)
shopt -u nullglob
if [[ ${#native_libs[@]} -eq 0 ]]; then
    echo "::warning::bundle-dist: no native libs at $BUILD_DIR/lib/libry_*.*" >&2
else
    cp "${native_libs[@]}" "$DIST_DIR/lib/"
fi

cp -r "$REPO_ROOT/share/std" "$DIST_DIR/share/std"
cp "$REPO_ROOT/LICENSE-LLVM.txt" "$DIST_DIR/LICENSE-LLVM.txt"

case "$PLATFORM" in
darwin)
    RY="$DIST_DIR/ry"
    LIB="$DIST_DIR/lib"

    # Resolve the absolute install names `ry` currently records, so we rewrite the
    # exact strings regardless of which preset built it (@rpath vs absolute).
    llvm_ref="$(otool -L "$RY" | awk '/libLLVM\.dylib/{print $1; exit}')"
    emit_ref="$(otool -L "$RY" | awk '/libry_llvm_emit\.dylib/{print $1; exit}')"
    [[ -n "$llvm_ref" ]] || { echo "bundle-dist: ry does not reference libLLVM.dylib" >&2; exit 1; }
    [[ -n "$emit_ref" ]] || { echo "bundle-dist: ry does not reference libry_llvm_emit.dylib" >&2; exit 1; }

    # Locate the real libLLVM.dylib (follow the install name if absolute, else Homebrew).
    if [[ "$llvm_ref" == /* ]]; then
        llvm_src="$llvm_ref"
    else
        llvm_src="$(brew --prefix llvm@21)/lib/libLLVM.dylib"
    fi
    log "copying libLLVM from $llvm_src"
    cp -L "$llvm_src" "$LIB/libLLVM.dylib"
    chmod u+w "$LIB/libLLVM.dylib"

    # libLLVM's only non-system chain dep on macOS is Homebrew libzstd.
    zstd_ref="$(otool -L "$LIB/libLLVM.dylib" | awk '/libzstd/{print $1; exit}')"
    if [[ -n "$zstd_ref" ]]; then
        zstd_src="$zstd_ref"; [[ "$zstd_src" == /* ]] || zstd_src="$(brew --prefix zstd)/lib/libzstd.1.dylib"
        log "copying libzstd from $zstd_src (libLLVM chain dep)"
        cp -L "$zstd_src" "$LIB/libzstd.1.dylib"
        chmod u+w "$LIB/libzstd.1.dylib"
        install_name_tool -id '@rpath/libzstd.1.dylib' "$LIB/libzstd.1.dylib"
        install_name_tool -change "$zstd_ref" '@loader_path/libzstd.1.dylib' "$LIB/libLLVM.dylib"
        codesign --force --sign - "$LIB/libzstd.1.dylib"
    else
        log "libLLVM has no libzstd dependency on this build — skipping"
    fi

    # libLLVM: relocatable id.
    install_name_tool -id '@rpath/libLLVM.dylib' "$LIB/libLLVM.dylib"
    codesign --force --sign - "$LIB/libLLVM.dylib"

    # libry_llvm_emit: relocatable id + sibling libLLVM via @loader_path (same lib/).
    emit_self="$(otool -D "$LIB/libry_llvm_emit.dylib" | tail -1)"
    emit_llvm_ref="$(otool -L "$LIB/libry_llvm_emit.dylib" | awk '/libLLVM\.dylib/{print $1; exit}')"
    install_name_tool -id '@rpath/libry_llvm_emit.dylib' "$LIB/libry_llvm_emit.dylib"
    [[ -n "$emit_llvm_ref" ]] && install_name_tool -change "$emit_llvm_ref" '@loader_path/libLLVM.dylib' "$LIB/libry_llvm_emit.dylib"
    codesign --force --sign - "$LIB/libry_llvm_emit.dylib"

    # ry: point libLLVM / libry_llvm_emit at @rpath, drop build-tree rpaths, add the two.
    install_name_tool -change "$llvm_ref" '@rpath/libLLVM.dylib' "$RY"
    install_name_tool -change "$emit_ref" '@rpath/libry_llvm_emit.dylib' "$RY"
    while IFS= read -r r; do
        [[ -n "$r" ]] && install_name_tool -delete_rpath "$r" "$RY" 2>/dev/null || true
    done < <(otool -l "$RY" | awk '/LC_RPATH/{getline; getline; print $2}')
    install_name_tool -add_rpath "$RPATH_TARBALL_MAC" "$RY"
    install_name_tool -add_rpath "$RPATH_INSTALL_MAC" "$RY"
    codesign --force --sign - "$RY"
    ;;

linux)
    RY="$DIST_DIR/ry"
    LIB="$DIST_DIR/lib"

    # Copy libLLVM under the exact soname `ry` records as NEEDED (e.g. libLLVM.so.21).
    soname="$(readelf -d "$RY" | awk -F'[][]' '/NEEDED/ && /libLLVM/ {print $2; exit}')"
    [[ -n "$soname" ]] || { echo "bundle-dist: ry has no libLLVM NEEDED entry" >&2; exit 1; }
    llvm_path="$(ldd "$RY" | awk -v s="$soname" '$1==s{print $3; exit}')"
    [[ -n "$llvm_path" && -e "$llvm_path" ]] || { echo "bundle-dist: cannot resolve $soname via ldd" >&2; exit 1; }
    log "copying libLLVM ($soname) from $llvm_path"
    cp -L "$llvm_path" "$LIB/$soname"
    chmod u+w "$LIB/$soname"

    # libLLVM's non-system chain deps on the glibc-old image are resolved by the
    # target distro (libz/libzstd/libstdc++/libm/libc). Surface anything unexpected.
    log "libLLVM NEEDED (verify only system libs remain):"
    readelf -d "$LIB/$soname" | awk '/NEEDED/{print "  " $0}'

    # rpath: ry and libry_llvm_emit get $ORIGIN-relative rpaths from CMakeLists
    # (BUILD_RPATH); patchelf is absent on ry-ci-glibc-old (no-apt). If patchelf is
    # available, set them explicitly as a belt-and-braces; otherwise rely on CMake.
    if command -v patchelf >/dev/null 2>&1; then
        log "patchelf present — setting rpaths explicitly"
        patchelf --set-rpath '$ORIGIN/lib:$ORIGIN/../../.ry/lib' "$RY"
        [[ -f "$LIB/libry_llvm_emit.so" ]] && patchelf --set-rpath '$ORIGIN' "$LIB/libry_llvm_emit.so"
    else
        log "patchelf absent — relying on CMakeLists BUILD_RPATH (verify-bundle.sh asserts result)"
    fi
    ;;

*)
    echo "bundle-dist: unknown platform '$PLATFORM' (expected darwin|linux)" >&2
    exit 2
    ;;
esac

log "assembled $DIST_DIR for $PLATFORM"
