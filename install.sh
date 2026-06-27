#!/bin/sh
set -eu

REPO="t0k0sh1/ry"
INSTALL_DIR="${RY_INSTALL_DIR:-$HOME/.local/bin}"
RY_HOME="${RY_HOME:-$HOME/.ry}"
VERSION="${1:-latest}"

OS="$(uname -s | tr '[:upper:]' '[:lower:]')"
ARCH="$(uname -m)"
case "$ARCH" in arm64|aarch64) ARCH="arm64" ;; x86_64|amd64) ARCH="amd64" ;; esac

if [ "$VERSION" = "latest" ]; then
  DOWNLOAD_URL="$(curl -fsSL "https://api.github.com/repos/${REPO}/releases/latest" \
    | grep "browser_download_url.*${OS}-${ARCH}.tar.gz\"" \
    | head -1 | cut -d '"' -f 4)"
else
  DOWNLOAD_URL="https://github.com/${REPO}/releases/download/${VERSION}/ry-${VERSION}-${OS}-${ARCH}.tar.gz"
fi

mkdir -p "$INSTALL_DIR"
TMPDIR="$(mktemp -d)"
STD_STAGING=""
trap 'rm -rf "$TMPDIR" ${STD_STAGING:+"$STD_STAGING"}' EXIT

echo "Downloading ry..."
curl -fsSL "$DOWNLOAD_URL" -o "$TMPDIR/ry.tar.gz"
tar xzf "$TMPDIR/ry.tar.gz" -C "$TMPDIR"

# Validate archive layout BEFORE mutating any installed artifacts.
# Detect stdlib layout in the extracted archive and bail out early if it's
# missing — otherwise a partial install (new ry binary, stale/missing stdlib)
# would leave the user with a broken runtime.
STD_DIR=""
SRC_STD=""
NEW_LAYOUT=0
if [ -d "$TMPDIR/share/std" ]; then
    STD_DIR="$RY_HOME/share/std"
    SRC_STD="$TMPDIR/share/std"
    NEW_LAYOUT=1
elif [ -d "$TMPDIR/lib/std" ]; then
    STD_DIR="$RY_HOME/lib/std"
    SRC_STD="$TMPDIR/lib/std"
fi
if [ -z "$SRC_STD" ] || [ ! -d "$SRC_STD" ]; then
    echo "ERROR: release archive does not contain a standard library (expected share/std or lib/std)" >&2
    exit 1
fi

# Archive validated — safe to mutate installed artifacts.
install -m 755 "$TMPDIR/ry" "$INSTALL_DIR/ry"

# ry-rescue: emergency recovery script (#2455). Older tarballs (pre-#2455)
# do not carry it — skip silently in that case so installing an older
# version stays a no-op for rescue.
if [ -f "$TMPDIR/ry-rescue" ]; then
    install -m 755 "$TMPDIR/ry-rescue" "$INSTALL_DIR/ry-rescue"
    echo "ry-rescue installed to $INSTALL_DIR/ry-rescue"
fi

# Install native shared libraries
if [ -d "$TMPDIR/lib" ]; then
    NATIVE_LIBS_INSTALLED=0
    mkdir -p "$RY_HOME/lib"
    # Install the Rust cdylibs (libemit, liblower) + stdlib native libs (libry_*),
    # and — post-#1999 cutover — the bundled shared libLLVM (plus its macOS chain
    # dep libzstd), so the installed runtime is self-contained (#2005). libemit
    # and liblower are globbed separately: libemit was renamed from libry_codegen
    # (#2040) and liblower is the upper-codegen Rust kickoff (#2397), so neither
    # matches the libry_* prefix. $RY_HOME/lib is one of ry's rpath targets
    # (@loader_path/../../.ry/lib on macOS, $ORIGIN/../../.ry/lib on Linux).
    # Unmatched globs (e.g. libzstd.* on Linux) stay literal and are filtered out
    # by the [ -f ] test below.
    for f in "$TMPDIR"/lib/libemit.* "$TMPDIR"/lib/liblower.* "$TMPDIR"/lib/libry_*.* "$TMPDIR"/lib/libLLVM.* "$TMPDIR"/lib/libzstd.*; do
        if [ -f "$f" ]; then
            install -m 755 "$f" "$RY_HOME/lib/"
            NATIVE_LIBS_INSTALLED=1
        fi
    done
    if [ "$NATIVE_LIBS_INSTALLED" = 1 ]; then
        echo "Native libraries installed to $RY_HOME/lib"
    fi
fi

# Install standard library (clean replace).
# Copy to a staging directory first, then atomically swap into place.
mkdir -p "$(dirname "$STD_DIR")"
STD_STAGING="$(mktemp -d "${STD_DIR}.XXXXXX")"
cp -r "$SRC_STD/." "$STD_STAGING/"
rm -rf "$STD_DIR"
mv "$STD_STAGING" "$STD_DIR"
echo "Standard library installed to $STD_DIR"
# Clean up old lib/std layout only after successful install (migration)
if [ "$NEW_LAYOUT" = 1 ]; then
    rm -rf "$RY_HOME/lib/std"
    # rmdir only removes empty directories — safe when native libs
    # (libry_*) are installed in $RY_HOME/lib (no-op in that case).
    rmdir "$RY_HOME/lib" 2>/dev/null || true
fi

echo "ry installed to ${INSTALL_DIR}/ry"
case ":$PATH:" in
  *":${INSTALL_DIR}:"*) ;;
  *) echo "WARNING: ${INSTALL_DIR} is not in PATH. Add: export PATH=\"${INSTALL_DIR}:\$PATH\"" ;;
esac
"${INSTALL_DIR}/ry" --version 2>/dev/null || true
