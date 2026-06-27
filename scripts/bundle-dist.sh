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
#   DIST_DIR/lib/{libLLVM.*,libzstd.1.dylib(macOS),libemit.*,liblower.*,libry_*.*}
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

# The Rust cdylibs (libemit, liblower) + stdlib native libs (libry_*) ship as-is
# from the build tree. The cdylibs are matched separately from the libry_* glob
# because their names do not start with libry_: libemit was renamed from
# libry_codegen (#2040), and liblower was added by #2397 as the upper-codegen
# Rust kickoff. Any future Rust cdylib whose name doesn't start with libry_ must
# be added here (and to install.sh / self_update.cpp / verify-bundle.sh — see
# .claude/rules/distribution-packaging.md).
shopt -s nullglob
candidate_libs=("$BUILD_DIR"/lib/libemit.* "$BUILD_DIR"/lib/liblower.* "$BUILD_DIR"/lib/libry_*.*)
shopt -u nullglob

# Drop orphan cdylibs (#2041): a corrosion crate rename (libry_codegen -> libemit,
# #2040) can leave the old cdylib in a non-clean build tree, and the libry_* glob
# would ship it. The cdylib is the ONLY native lib that links libLLVM — stdlib
# libry_* libs do not — so skip any non-libemit lib that links libLLVM (warn so the
# stale build tree is surfaced). verify-bundle.sh asserts this same invariant.
links_libllvm() {
    case "$PLATFORM" in
        darwin) otool -L "$1" 2>/dev/null | grep -q 'libLLVM' ;;
        linux)  readelf -d "$1" 2>/dev/null | grep -q 'NEEDED.*libLLVM' ;;
        *) return 1 ;;
    esac
}
native_libs=()
for f in "${candidate_libs[@]}"; do
    base="$(basename "$f")"
    if [[ "$base" != libemit.* ]] && links_libllvm "$f"; then
        log "skipping orphan cdylib $base (links libLLVM but is not libemit; #2041)"
        continue
    fi
    native_libs+=("$f")
done

if [[ ${#native_libs[@]} -eq 0 ]]; then
    echo "::warning::bundle-dist: no native libs at $BUILD_DIR/lib/{libemit,libry_*}.*" >&2
else
    cp "${native_libs[@]}" "$DIST_DIR/lib/"
fi

cp -r "$REPO_ROOT/share/std" "$DIST_DIR/share/std"
cp "$REPO_ROOT/LICENSE-LLVM.txt" "$DIST_DIR/LICENSE-LLVM.txt"

# ry-rescue: emergency recovery script (#2455). Ships at the tarball top
# level so install.sh / self_update.cpp / install.sh-style consumers can
# drop it next to `ry`. Standalone POSIX shell — does NOT link libLLVM,
# so it survives the failure mode that ry-rescue exists to fix.
install -m 755 "$REPO_ROOT/scripts/rescue.sh" "$DIST_DIR/ry-rescue"

case "$PLATFORM" in
darwin)
    RY="$DIST_DIR/ry"
    LIB="$DIST_DIR/lib"

    # Resolve the absolute install names `ry` currently records, so we rewrite the
    # exact strings regardless of which preset built it (@rpath vs absolute).
    llvm_ref="$(otool -L "$RY" | awk '/libLLVM\.dylib/{print $1; exit}')"
    emit_ref="$(otool -L "$RY" | awk '/libemit\.dylib/{print $1; exit}')"
    [[ -n "$llvm_ref" ]] || { echo "bundle-dist: ry does not reference libLLVM.dylib" >&2; exit 1; }
    [[ -n "$emit_ref" ]] || { echo "bundle-dist: ry does not reference libemit.dylib" >&2; exit 1; }

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

    # libemit: relocatable id + sibling libLLVM via @loader_path (same lib/).
    emit_self="$(otool -D "$LIB/libemit.dylib" | tail -1)"
    emit_llvm_ref="$(otool -L "$LIB/libemit.dylib" | awk '/libLLVM\.dylib/{print $1; exit}')"
    install_name_tool -id '@rpath/libemit.dylib' "$LIB/libemit.dylib"
    [[ -n "$emit_llvm_ref" ]] && install_name_tool -change "$emit_llvm_ref" '@loader_path/libLLVM.dylib' "$LIB/libemit.dylib"
    codesign --force --sign - "$LIB/libemit.dylib"

    # liblower (#2397): same treatment as libemit, but it has NO libLLVM
    # dependency to chain-rewrite. Only the self-id needs to become @rpath.
    if [[ -f "$LIB/liblower.dylib" ]]; then
        install_name_tool -id '@rpath/liblower.dylib' "$LIB/liblower.dylib"
        codesign --force --sign - "$LIB/liblower.dylib"
    fi

    # Every bundled libry_*.dylib (process-linked cdylib like libry_xid #2314,
    # or stdlib dlopen libs like libry_io / libry_base64) gets a relocatable
    # @rpath self-id. Cargo emits an absolute build-tree id for cdylibs, and
    # dlopen tolerates the mismatch on macOS today, but matching the @rpath
    # refs the loader uses is the convention and avoids future surprises.
    for f in "$LIB"/libry_*.dylib; do
        [[ -f "$f" ]] || continue
        base="$(basename "$f")"
        install_name_tool -id "@rpath/$base" "$f"
        codesign --force --sign - "$f"
    done

    # ry: point libLLVM / libemit / liblower at @rpath, drop build-tree rpaths,
    # add the two. liblower may not be present in legacy build trees (#2397 is the
    # crate's introducing PR), so the awk match guards against an empty ref.
    install_name_tool -change "$llvm_ref" '@rpath/libLLVM.dylib' "$RY"
    install_name_tool -change "$emit_ref" '@rpath/libemit.dylib' "$RY"
    lower_ref="$(otool -L "$RY" | awk '/liblower\.dylib/{print $1; exit}')"
    [[ -n "$lower_ref" ]] && install_name_tool -change "$lower_ref" '@rpath/liblower.dylib' "$RY"

    # Process-linked libry_* cdylibs (libry_xid today; any future ones with the
    # same model are picked up automatically). Cargo records absolute build-tree
    # paths in `ry`'s LC_LOAD_DYLIB for these, which verify-bundle.sh's
    # forbidden-token check rejects. dlopen-loaded stdlib libry_* never appear
    # in `otool -L`, so they are untouched by this loop.
    while IFS= read -r ref; do
        [[ -n "$ref" ]] || continue
        base="$(basename "$ref")"
        install_name_tool -change "$ref" "@rpath/$base" "$RY"
    done < <(otool -L "$RY" | awk '/libry_[A-Za-z0-9_]+\.dylib/{print $1}' | grep -v '^@rpath/')
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

    # rpath: ry and libemit get $ORIGIN-relative rpaths from CMakeLists
    # (BUILD_RPATH); patchelf is absent on ry-ci-glibc-old (no-apt). If patchelf is
    # available, set them explicitly as a belt-and-braces; otherwise rely on CMake.
    if command -v patchelf >/dev/null 2>&1; then
        log "patchelf present — setting rpaths explicitly"
        patchelf --set-rpath '$ORIGIN/lib:$ORIGIN/../../.ry/lib' "$RY"
        [[ -f "$LIB/libemit.so" ]] && patchelf --set-rpath '$ORIGIN' "$LIB/libemit.so"
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
