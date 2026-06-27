#!/usr/bin/env bash
# Verify a packaged dist/ tree is self-contained for distribution (#2005).
#
# After the #1999 C++->Rust cutover, `ry` requires a SHARED libLLVM at runtime
# (static link hangs ConstantFP::get via the APFloat fltSemantics singleton
# split, #1997). The release tarball must therefore bundle libLLVM and the Rust
# cdylib (libemit), with every build-machine-absolute reference rewritten
# to a relocatable rpath, so the binary starts on an end-user system that has no
# Homebrew/-installed LLVM. This script asserts the result of scripts/bundle-dist.sh.
#
# openssl (libssl/libcrypto) is intentionally NOT bundled and absolute openssl
# references are allowed: `ry` depended on it the same way before the cutover
# (v0.0.25 parity, CMakeLists.txt:236 `ry_lib` PUBLIC link) and bundling crypto
# would block OS security updates. openssl is out of scope for #2005.
#
# Usage: verify-bundle.sh DIST_DIR [darwin|linux]
# Exit:  0 all assertions pass; 1 a check failed.
set -euo pipefail

DIST_DIR="${1:?usage: verify-bundle.sh DIST_DIR [darwin|linux]}"
PLATFORM="${2:-$(uname -s | tr '[:upper:]' '[:lower:]')}"

fail=0

# The helpers take the haystack as the THIRD argument (not via stdin/pipe): a
# `echo "$x" | helper` form would run the helper in a subshell, so its `fail=1`
# would never reach this parent scope and a real FAIL would be silently dropped.
# want "<desc>" "<extended-regex>" "<haystack>"
want() {
    if printf '%s\n' "$3" | grep -qE "$2"; then
        echo "  ok: $1"
    else
        echo "  FAIL: $1 (pattern not found: $2)" >&2
        fail=1
    fi
}

# want_fixed "<desc>" "<fixed-string>" "<haystack>" — literal match (for rpaths
# containing regex metacharacters like '../' or '$ORIGIN').
want_fixed() {
    if printf '%s\n' "$3" | grep -qF "$2"; then
        echo "  ok: $1"
    else
        echo "  FAIL: $1 (string not found: $2)" >&2
        fail=1
    fi
}

# absent "<desc>" "<extended-regex>" "<haystack>" — pass if NO match.
absent() {
    if printf '%s\n' "$3" | grep -qE "$2"; then
        echo "  FAIL: $1 (forbidden token present)" >&2
        printf '%s\n' "$3" | grep -E "$2" | sed 's/^/      /' >&2 || true
        fail=1
    else
        echo "  ok: $1"
    fi
}

echo "verify-bundle: checking $DIST_DIR (platform=$PLATFORM)"

# --- common: LLVM license attribution must ship in the tarball ---
if [[ -f "$DIST_DIR/LICENSE-LLVM.txt" ]]; then
    echo "  ok: LICENSE-LLVM.txt bundled"
else
    echo "  FAIL: LICENSE-LLVM.txt missing from $DIST_DIR" >&2
    fail=1
fi

# --- common: standalone updater (#2459) ---
if [[ -f "$DIST_DIR/ry-self-update" ]]; then
    if [[ -x "$DIST_DIR/ry-self-update" ]]; then
        echo "  ok: ry-self-update bundled (executable)"
    else
        echo "  FAIL: ry-self-update is not executable" >&2
        fail=1
    fi
else
    echo "  FAIL: ry-self-update missing from $DIST_DIR (#2459)" >&2
    fail=1
fi

# --- common: ry-rescue emergency recovery script (#2455) ---
# Must be present at the tarball top level, executable, and POSIX-shell
# parseable (so it stays runnable in the libLLVM-broken state it exists
# to recover from — `sh -n` only checks syntax, no LLVM linkage).
if [[ -f "$DIST_DIR/ry-rescue" ]]; then
    if [[ -x "$DIST_DIR/ry-rescue" ]]; then
        echo "  ok: ry-rescue bundled (executable)"
    else
        echo "  FAIL: ry-rescue is not executable" >&2
        fail=1
    fi
    if sh -n "$DIST_DIR/ry-rescue" 2>/dev/null; then
        echo "  ok: ry-rescue parses as POSIX shell"
    else
        echo "  FAIL: ry-rescue fails POSIX shell parse (sh -n)" >&2
        fail=1
    fi
else
    echo "  FAIL: ry-rescue missing from $DIST_DIR (#2455)" >&2
    fail=1
fi

case "$PLATFORM" in
darwin)
    RY="$DIST_DIR/ry"
    RY_SELF_UPDATE="$DIST_DIR/ry-self-update"
    LIB="$DIST_DIR/lib"

    for f in "$RY" "$LIB/libLLVM.dylib" "$LIB/libzstd.1.dylib" "$LIB/libemit.dylib" "$LIB/liblower.dylib"; do
        [[ -f "$f" ]] && echo "  ok: present $(basename "$f")" || { echo "  FAIL: missing $f" >&2; fail=1; }
    done

    ry_deps="$(otool -L "$RY" 2>/dev/null || true)"
    ry_load="$(otool -l "$RY" 2>/dev/null || true)"
    self_update_deps="$(otool -L "$RY_SELF_UPDATE" 2>/dev/null || true)"
    want       "ry -> @rpath/libLLVM.dylib"         '@rpath/libLLVM\.dylib'          "$ry_deps"
    want       "ry -> @rpath/libemit.dylib"  '@rpath/libemit\.dylib'  "$ry_deps"
    # tarball-direct (./ry) rpath and installed (~/.local/bin -> ~/.ry/lib) rpath
    want_fixed "ry rpath @loader_path/lib"           '@loader_path/lib'               "$ry_load"
    want_fixed "ry rpath @loader_path/../../.ry/lib"  '@loader_path/../../.ry/lib'     "$ry_load"
    # no build-machine-absolute LLVM/zstd/build-tree refs survive (openssl allowed)
    absent     "ry has no absolute LLVM/zstd/build-tree refs" 'llvm@21|/zstd/|build-rust|cargo/build|Cellar/llvm' "$ry_deps"
    absent     "ry-self-update has no runtime/libLLVM dependency" 'libLLVM|libemit|liblower|libry_|libzstd' "$self_update_deps"

    llvm_id="$(otool -D "$LIB/libLLVM.dylib" 2>/dev/null || true)"
    llvm_deps="$(otool -L "$LIB/libLLVM.dylib" 2>/dev/null || true)"
    want   "libLLVM id = @rpath/libLLVM.dylib"      '@rpath/libLLVM\.dylib'         "$llvm_id"
    want   "libLLVM -> @loader_path/libzstd"        '@loader_path/libzstd\.1\.dylib' "$llvm_deps"
    absent "libLLVM has no absolute zstd ref"       '/opt/homebrew.*zstd|/zstd/lib' "$llvm_deps"

    emit_id="$(otool -D "$LIB/libemit.dylib" 2>/dev/null || true)"
    emit_deps="$(otool -L "$LIB/libemit.dylib" 2>/dev/null || true)"
    want   "libemit id = @rpath/..."        '@rpath/libemit\.dylib'  "$emit_id"
    want   "libemit -> @loader_path/libLLVM" '@loader_path/libLLVM\.dylib'    "$emit_deps"
    absent "libemit no absolute LLVM/build-tree ref" 'llvm@21|build-rust|cargo/build' "$emit_deps"

    # Orphan cdylib guard (#2041): a corrosion crate rename (libry_codegen -> libemit,
    # #2040) leaves the old cdylib in a non-clean build tree, and bundle-dist's libry_*
    # glob would ship it. The cdylib is the ONLY bundled native lib that links libLLVM
    # — stdlib libry_* libs do not — so any non-libemit lib in lib/ that links libLLVM
    # is an orphan. Case-sensitive 'libLLVM' matches the dependency line, never a
    # self-name containing lowercase 'llvm' (e.g. a future libry_llvm_*).
    orphan_cdylibs=""
    for f in "$LIB"/lib*.dylib; do
        [[ -e "$f" ]] || continue
        base="$(basename "$f")"
        case "$base" in libLLVM.dylib|libzstd.*|libemit.*) continue ;; esac
        if otool -L "$f" 2>/dev/null | grep -q 'libLLVM'; then
            orphan_cdylibs+="$base"$'\n'
        fi
    done
    if [[ -n "$orphan_cdylibs" ]]; then
        echo "  FAIL: orphan cdylib(s) in lib/ link libLLVM but are not libemit (#2041):" >&2
        printf '%s' "$orphan_cdylibs" | sed 's/^/      /' >&2
        fail=1
    else
        echo "  ok: lib/ has no orphan cdylib (only libemit links libLLVM)"
    fi

    want "libzstd id = @rpath/libzstd.1.dylib" '@rpath/libzstd\.1\.dylib' "$(otool -D "$LIB/libzstd.1.dylib" 2>/dev/null || true)"

    # ad-hoc signatures must be valid after install_name_tool rewrites
    for f in "$RY" "$RY_SELF_UPDATE" "$LIB/libLLVM.dylib" "$LIB/libemit.dylib" "$LIB/libzstd.1.dylib" "$LIB/liblower.dylib"; do
        if codesign --verify "$f" 2>/dev/null; then
            echo "  ok: codesign $(basename "$f")"
        else
            echo "  FAIL: codesign --verify failed for $f" >&2
            fail=1
        fi
    done
    ;;

linux)
    RY="$DIST_DIR/ry"
    RY_SELF_UPDATE="$DIST_DIR/ry-self-update"
    LIB="$DIST_DIR/lib"

    # the bundled soname must match what `ry` records as NEEDED
    soname="$(readelf -d "$RY" 2>/dev/null | awk -F'[][]' '/NEEDED/ && /libLLVM/ {print $2; exit}')"
    if [[ -n "$soname" && -f "$LIB/$soname" ]]; then
        echo "  ok: bundled $soname matches ry NEEDED"
    else
        echo "  FAIL: ry NEEDED libLLVM soname='$soname' not bundled at $LIB/" >&2
        fail=1
    fi
    ls "$LIB"/libemit.so >/dev/null 2>&1 && echo "  ok: present libemit.so" || { echo "  FAIL: missing libemit.so" >&2; fail=1; }
    ls "$LIB"/liblower.so >/dev/null 2>&1 && echo "  ok: present liblower.so" || { echo "  FAIL: missing liblower.so" >&2; fail=1; }

    # Orphan cdylib guard (#2041): see the darwin branch. On Linux the cdylib NEEDs
    # libLLVM.so.X (stdlib libry_* do not); libzstd is a system lib (not bundled), so
    # only libLLVM* and libemit* are excluded. A leftover libry_codegen.so from a
    # pre-rename build tree would be caught here before tar.
    orphan_cdylibs=""
    for f in "$LIB"/lib*.so*; do
        [[ -e "$f" ]] || continue
        base="$(basename "$f")"
        case "$base" in libLLVM.so*|libemit.so*) continue ;; esac
        if readelf -d "$f" 2>/dev/null | grep -q 'NEEDED.*libLLVM'; then
            orphan_cdylibs+="$base"$'\n'
        fi
    done
    if [[ -n "$orphan_cdylibs" ]]; then
        echo "  FAIL: orphan cdylib(s) in lib/ NEED libLLVM but are not libemit (#2041):" >&2
        printf '%s' "$orphan_cdylibs" | sed 's/^/      /' >&2
        fail=1
    else
        echo "  ok: lib/ has no orphan cdylib (only libemit needs libLLVM)"
    fi

    ry_dyn="$(readelf -d "$RY" 2>/dev/null || true)"
    self_update_dyn="$(readelf -d "$RY_SELF_UPDATE" 2>/dev/null || true)"
    # CMake emits DT_RUNPATH (new dtags); accept either RUNPATH or RPATH.
    want_fixed "ry runpath \$ORIGIN/lib"            '$ORIGIN/lib'            "$ry_dyn"
    want_fixed "ry runpath \$ORIGIN/../../.ry/lib"  '$ORIGIN/../../.ry/lib'  "$ry_dyn"
    # BUILD_WITH_INSTALL_RPATH strips CMake's auto link-path rpath, so no
    # build-machine-absolute LLVM path should survive in the shipped binary.
    absent     "ry runpath has no absolute LLVM path" '/usr/local/llvm|/opt/[^:]*llvm' "$ry_dyn"
    absent     "ry-self-update has no runtime/libLLVM dependency" 'libLLVM|libemit|liblower|libry_|libzstd' "$self_update_dyn"
    ;;

*)
    echo "verify-bundle: unknown platform '$PLATFORM' (expected darwin|linux)" >&2
    exit 2
    ;;
esac

if [[ $fail -ne 0 ]]; then
    echo "verify-bundle: FAILED" >&2
    exit 1
fi
echo "verify-bundle: all assertions passed for $DIST_DIR ($PLATFORM)."
