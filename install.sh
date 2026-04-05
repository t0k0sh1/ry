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
install -m 755 "$TMPDIR/ry" "$INSTALL_DIR/ry"

# Install native shared libraries
if [ -d "$TMPDIR/lib" ]; then
    mkdir -p "$RY_HOME/lib"
    for f in "$TMPDIR"/lib/libry_*.*; do
        [ -f "$f" ] && install -m 755 "$f" "$RY_HOME/lib/"
    done
    echo "Native libraries installed to $RY_HOME/lib"
fi

# Install standard library (clean replace)
# Detect archive layout and install to the matching destination so the
# corresponding binary can find stdlib at the path it expects.
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
if [ -n "$SRC_STD" ] && [ -d "$SRC_STD" ]; then
    # Copy to a staging directory first, then atomically swap into place.
    # On copy failure the previous stdlib remains intact.
    mkdir -p "$(dirname "$STD_DIR")"
    STD_STAGING="$(mktemp -d "${STD_DIR}.XXXXXX")"
    cp -r "$SRC_STD/." "$STD_STAGING/"
    rm -rf "$STD_DIR"
    mv "$STD_STAGING" "$STD_DIR"
    echo "Standard library installed to $STD_DIR"
    # Clean up old lib/std layout only after successful install (migration)
    if [ "$NEW_LAYOUT" = 1 ]; then
        rm -rf "$RY_HOME/lib/std"
        rmdir "$RY_HOME/lib" 2>/dev/null || true
    fi
fi

echo "ry installed to ${INSTALL_DIR}/ry"
case ":$PATH:" in
  *":${INSTALL_DIR}:"*) ;;
  *) echo "WARNING: ${INSTALL_DIR} is not in PATH. Add: export PATH=\"${INSTALL_DIR}:\$PATH\"" ;;
esac
"${INSTALL_DIR}/ry" --version 2>/dev/null || true
