#!/bin/sh
set -eu

REPO="t0k0sh1/ry"
INSTALL_DIR="${RY_INSTALL_DIR:-$HOME/.local/bin}"
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
trap 'rm -rf "$TMPDIR"' EXIT

echo "Downloading ry..."
curl -fsSL "$DOWNLOAD_URL" -o "$TMPDIR/ry.tar.gz"
tar xzf "$TMPDIR/ry.tar.gz" -C "$TMPDIR"
install -m 755 "$TMPDIR/ry" "$INSTALL_DIR/ry"

echo "ry installed to ${INSTALL_DIR}/ry"
case ":$PATH:" in
  *":${INSTALL_DIR}:"*) ;;
  *) echo "WARNING: ${INSTALL_DIR} is not in PATH. Add: export PATH=\"${INSTALL_DIR}:\$PATH\"" ;;
esac
"${INSTALL_DIR}/ry" --version 2>/dev/null || true
