#!/bin/sh
# ry-rescue — emergency recovery script for broken ry installs (#2455).
#
# Background: the #2005 cutover made `ry` link a shared libLLVM, and pre-#2005
# binaries' `install_native_libs` could leave new binaries on disk without the
# matching `~/.ry/lib/libLLVM.{dylib,so.*}`. The resulting binary aborts at
# load time (dyld can't resolve @rpath/libLLVM.dylib), so `ry self-update`
# itself cannot recover from the broken state — it lives inside the same
# binary that fails to start.
#
# This script is intentionally self-contained and depends ONLY on curl, tar,
# and shasum (and POSIX shell built-ins). It does NOT link libLLVM. It always
# fetches the LATEST stable release; pinning to an arbitrary version is
# explicitly out of scope (recovery should land on a known-good current).
#
# Verification model (acceptance criteria from #2455):
#   - SHA-256 checksum verification is MANDATORY in every path.
#   - Ed25519 signature verification is OPTIONAL: performed iff a working
#     OpenSSL 3.x (`openssl pkeyutl -rawin` supported, Ed25519 SPKI loadable)
#     is found. macOS `/usr/bin/openssl` is LibreSSL 3.3.x and does NOT meet
#     this bar — we probe capability, not presence.
#   - When openssl is incapable or the signature file is unavailable, we
#     emit a clear warning and continue with checksum-only verification.
#
# Layout: matches install.sh — binary -> $INSTALL_DIR/ry, native libs ->
# $RY_HOME/lib/, stdlib -> $RY_HOME/share/std/. Only new-layout archives are
# supported (share/std); a tarball missing share/std aborts the rescue.
#
# Orphan cleanup: before installing new native libs we remove every existing
# file in $RY_HOME/lib/ matching the known native-lib globs (libLLVM*,
# libemit*, liblower*, libry_*, libzstd*). `lib/std` (old-layout stdlib) is
# NEVER touched — the explicit globs guarantee we only sweep native shared
# libraries, not stdlib source.
#
# Usage: ry-rescue
# Environment:
#   RY_INSTALL_DIR  Override install location for `ry` (default: ~/.local/bin)
#   RY_HOME         Override layout root for libs/stdlib (default: ~/.ry)
#   RY_RESCUE_REPO  Override GitHub repo (default: t0k0sh1/ry; for CI tests)

set -eu

REPO="${RY_RESCUE_REPO:-t0k0sh1/ry}"
INSTALL_DIR="${RY_INSTALL_DIR:-$HOME/.local/bin}"
RY_HOME="${RY_HOME:-$HOME/.ry}"

OS="$(uname -s | tr '[:upper:]' '[:lower:]')"
ARCH="$(uname -m)"
case "$ARCH" in arm64|aarch64) ARCH="arm64" ;; x86_64|amd64) ARCH="amd64" ;; esac

# Ed25519 public key (raw 32 bytes hex). MUST equal SIGNING_PUBLIC_KEY in
# src/cli/self_update.cpp — if you rotate one, rotate both.
RY_RELEASE_PUBKEY_HEX="6620a3e129ff973b5af965fb097f7de48424e56be36e4b3d139708f710b93fc4"
# SPKI DER prefix for Ed25519: AlgorithmIdentifier {1.3.101.112} + BIT STRING tag.
# 0x302a 3005 06032b6570 032100 + 32-byte key -> 44-byte SPKI DER.
RY_SPKI_PREFIX_HEX="302a300506032b6570032100"

log()  { printf 'ry-rescue: %s\n' "$*"; }
warn() { printf 'ry-rescue: warning: %s\n' "$*" >&2; }
die()  { printf 'ry-rescue: error: %s\n' "$*" >&2; exit 1; }

require_cmd() {
    command -v "$1" >/dev/null 2>&1 || die "required command not found: $1"
}

require_cmd curl
require_cmd tar
require_cmd shasum
require_cmd uname
require_cmd mktemp

case "$OS" in darwin|linux) ;; *) die "unsupported OS: $OS" ;; esac
case "$ARCH" in arm64|amd64) ;; *) die "unsupported architecture: $ARCH" ;; esac

TMPDIR_RESCUE="$(mktemp -d)"
STD_STAGING=""
cleanup() { rm -rf "$TMPDIR_RESCUE" ${STD_STAGING:+"$STD_STAGING"}; }
trap cleanup EXIT INT TERM

API_URL="https://api.github.com/repos/${REPO}/releases/latest"
log "querying latest release..."
if ! curl -fsSL -H 'Accept: application/json' "$API_URL" -o "$TMPDIR_RESCUE/release.json"; then
    die "failed to query $API_URL — check network connectivity"
fi

# tag_name lives in the JSON as `"tag_name": "vX.Y.Z"`. Extract via grep/cut so
# we do not need jq (every extra dependency is one more thing the rescue can
# trip on). The release.json is GitHub-controlled, not user-controlled, so the
# extraction does not need to be a full parser.
TAG="$(grep -m1 '"tag_name"' "$TMPDIR_RESCUE/release.json" | cut -d '"' -f 4)"
[ -n "$TAG" ] || die "could not parse tag_name from release metadata"
log "latest release: $TAG"

ASSET="ry-${TAG}-${OS}-${ARCH}.tar.gz"
DOWNLOAD_URL="https://github.com/${REPO}/releases/download/${TAG}/${ASSET}"
CHECKSUMS_URL="https://github.com/${REPO}/releases/download/${TAG}/checksums.txt"
SIG_URL="https://github.com/${REPO}/releases/download/${TAG}/checksums.txt.sig"

log "downloading $ASSET..."
curl -fsSL "$DOWNLOAD_URL" -o "$TMPDIR_RESCUE/ry.tar.gz" \
    || die "failed to download $DOWNLOAD_URL"

log "downloading checksums.txt..."
curl -fsSL "$CHECKSUMS_URL" -o "$TMPDIR_RESCUE/checksums.txt" \
    || die "failed to download $CHECKSUMS_URL (checksum verification is mandatory)"

# Optional signature. Missing signature = warn + continue (issue #2455 defers
# mandatory signature to a separate issue); failed-verify on a present sig = abort.
SIG_DOWNLOADED=0
if curl -fsSL "$SIG_URL" -o "$TMPDIR_RESCUE/checksums.txt.sig" 2>/dev/null; then
    SIG_DOWNLOADED=1
fi

# SHA-256 verification — mandatory.
# checksums.txt lines: "<hex>  <filename>" (two spaces is the canonical
# shasum format; some toolchains emit one space — accept both).
EXPECTED_HASH=""
while IFS= read -r line; do
    case "$line" in
        *"  ${ASSET}"|*" ${ASSET}")
            EXPECTED_HASH="$(printf '%s\n' "$line" | awk '{print $1}')"
            break
            ;;
    esac
done < "$TMPDIR_RESCUE/checksums.txt"
[ -n "$EXPECTED_HASH" ] || die "no checksum entry for $ASSET in checksums.txt"

log "verifying SHA-256 checksum..."
ACTUAL_HASH="$(shasum -a 256 "$TMPDIR_RESCUE/ry.tar.gz" | awk '{print $1}')"
[ "$ACTUAL_HASH" = "$EXPECTED_HASH" ] \
    || die "checksum mismatch (expected $EXPECTED_HASH, got $ACTUAL_HASH)"
log "checksum ok"

# Ed25519 signature verification — best-effort.
#
# Capability gate: macOS /usr/bin/openssl is LibreSSL 3.3.x which has neither
# `-rawin` nor Ed25519 support; we must probe, not assume. We try `command -v
# openssl` first, then a couple of common Homebrew prefixes for the rescue
# case where openssl lives outside PATH.
find_capable_openssl() {
    for cand in \
        "${RY_OPENSSL:-}" \
        "$(command -v openssl 2>/dev/null || true)" \
        /opt/homebrew/opt/openssl/bin/openssl \
        /opt/homebrew/opt/openssl@3/bin/openssl \
        /usr/local/opt/openssl/bin/openssl \
        /usr/local/opt/openssl@3/bin/openssl
    do
        [ -n "$cand" ] && [ -x "$cand" ] || continue
        # -rawin is OpenSSL 3.0+. LibreSSL's pkeyutl rejects unknown flags;
        # also confirm the binary can actually load an Ed25519 SPKI PEM —
        # LibreSSL 3.3.x fails this with "unsupported algorithm".
        "$cand" pkeyutl -help 2>&1 | grep -q -- '-rawin' || continue
        if printf -- '-----BEGIN PUBLIC KEY-----\nMCowBQYDK2VwAyEAZiCj4Sn/lzta+WX7CX995IQk5Wvjbks9E5cI9xC5P8Q=\n-----END PUBLIC KEY-----\n' \
            | "$cand" pkey -pubin -noout >/dev/null 2>&1; then
            printf '%s' "$cand"
            return 0
        fi
    done
    return 1
}

if [ "$SIG_DOWNLOADED" = 0 ]; then
    warn "signature file not available; relying on SHA-256 checksum only"
elif OPENSSL_BIN="$(find_capable_openssl)" && [ -n "$OPENSSL_BIN" ]; then
    log "verifying Ed25519 signature with $OPENSSL_BIN..."
    # Build the SPKI PEM at runtime from the embedded hex constant so the
    # in-script key visibly matches src/cli/self_update.cpp's
    # SIGNING_PUBLIC_KEY (no separate PEM blob to drift).
    SPKI_DER="$TMPDIR_RESCUE/pubkey.der"
    SPKI_PEM="$TMPDIR_RESCUE/pubkey.pem"
    SIG_RAW="$TMPDIR_RESCUE/checksums.sig.raw"
    # POSIX printf has no portable %x bytes — do hex->bin via a tiny awk pass
    # that mirrors what `xxd -r -p` would do. `strtonum` is gawk-only, so we
    # use an index() lookup against a 0123456789abcdef0123456789ABCDEF table
    # to stay POSIX awk compatible (BSD awk on macOS lacks strtonum).
    # LC_ALL=C is mandatory: gawk in a UTF-8 locale interprets `printf "%c",
    # 0xe1` as a Unicode code point and emits multi-byte UTF-8 (corrupting
    # the DER, which fails openssl pkey -inform DER and would abort rescue
    # on every Fedora/RHEL/Arch box). C locale forces raw byte output.
    printf '%s%s' "$RY_SPKI_PREFIX_HEX" "$RY_RELEASE_PUBKEY_HEX" | LC_ALL=C awk '
BEGIN { hex = "0123456789abcdef0123456789ABCDEF" }
{
    for (i = 1; i <= length($0); i += 2) {
        h = substr($0, i, 1); l = substr($0, i + 1, 1)
        hi = index(hex, h) - 1; if (hi >= 16) hi -= 16
        lo = index(hex, l) - 1; if (lo >= 16) lo -= 16
        printf "%c", hi * 16 + lo
    }
}' > "$SPKI_DER"
    "$OPENSSL_BIN" pkey -pubin -inform DER -in "$SPKI_DER" -out "$SPKI_PEM" 2>/dev/null \
        || die "failed to build Ed25519 SPKI PEM from embedded pubkey"
    # checksums.txt.sig is base64-encoded raw 64-byte signature (matches
    # base64_decode() in src/cli/self_update.cpp). openssl base64 -d -A
    # works on both OpenSSL 3.x and LibreSSL — only the verify step needs
    # the capability gate above.
    "$OPENSSL_BIN" base64 -d -A -in "$TMPDIR_RESCUE/checksums.txt.sig" -out "$SIG_RAW" \
        || die "failed to base64-decode signature"
    if "$OPENSSL_BIN" pkeyutl -verify -pubin -inkey "$SPKI_PEM" -rawin \
            -in "$TMPDIR_RESCUE/checksums.txt" -sigfile "$SIG_RAW" >/dev/null 2>&1; then
        log "signature ok"
    else
        die "Ed25519 signature verification FAILED — release artifacts may be tampered with"
    fi
else
    warn "no Ed25519-capable openssl found (macOS /usr/bin/openssl is LibreSSL); skipping signature verification"
fi

log "extracting archive..."
tar xzf "$TMPDIR_RESCUE/ry.tar.gz" -C "$TMPDIR_RESCUE" \
    || die "failed to extract $TMPDIR_RESCUE/ry.tar.gz"

# Validate archive layout BEFORE mutating any installed artifacts. The rescue
# target is always the latest release, which uses share/std (post-#2005).
# An archive missing share/std would leave a partial install (new ry binary,
# stale stdlib) — abort with a clear message instead.
[ -f "$TMPDIR_RESCUE/ry" ] || die "release archive does not contain a ry binary"
[ -d "$TMPDIR_RESCUE/share/std" ] \
    || die "release archive does not contain share/std (old-layout archive not supported by rescue)"
[ -d "$TMPDIR_RESCUE/lib" ] || die "release archive does not contain lib/ (native libraries missing)"

# A `lib/` that exists but is empty (or missing libLLVM specifically) would
# pass the directory check, then the orphan-cleanup step a few lines below
# would delete the user's currently installed native libs and replace them
# with nothing — leaving ry in a strictly more broken state than before
# rescue ran. Assert the load-time-required globs are populated before any
# mutation. libzstd is macOS-only (Linux resolves zstd via system libs) and
# stdlib libry_*.dylib files are dlopen-loaded with softer failures, so the
# gating set is the process-linked trio: libLLVM, libemit, liblower.
have_any() {
    # Return 0 if at least one of "$@" is a regular file. Used to assert
    # that an unquoted glob actually matched at least one entry (under
    # `set -u` an unmatched glob expands to the literal pattern, which
    # this test then rejects via `[ -f ]`).
    for f do
        [ -f "$f" ] && return 0
    done
    return 1
}
have_any "$TMPDIR_RESCUE"/lib/libLLVM.* \
    || die "release archive missing libLLVM in lib/ — rescue would brick the install"
have_any "$TMPDIR_RESCUE"/lib/libemit.* \
    || die "release archive missing libemit in lib/ — rescue would brick the install"
have_any "$TMPDIR_RESCUE"/lib/liblower.* \
    || die "release archive missing liblower in lib/ — rescue would brick the install"

# Archive validated — now safe to mutate. Order matches install.sh: native
# libs first (rpath target), then ry binary, then stdlib via atomic swap.

mkdir -p "$INSTALL_DIR" "$RY_HOME/lib" "$RY_HOME/share"

# Orphan cleanup. Sweep every existing native-lib filename in $RY_HOME/lib
# matching the known release globs BEFORE installing new ones. This drops
# stale files left behind by older releases (e.g. libry_codegen.dylib
# pre-#2040) that the new tarball no longer contains. lib/std (old-layout
# stdlib) is deliberately not in the glob set — old stdlib stays put until
# the new share/std install replaces it via the migration step below.
log "cleaning up stale native libs in $RY_HOME/lib/..."
for f in "$RY_HOME"/lib/libLLVM* "$RY_HOME"/lib/libemit* \
         "$RY_HOME"/lib/liblower* "$RY_HOME"/lib/libry_* \
         "$RY_HOME"/lib/libzstd*; do
    [ -f "$f" ] && rm -f "$f"
done

# Install the new native libs. Mirror install.sh's glob list verbatim — the
# explicit-lib list lives in .claude/rules/distribution-packaging.md and
# changes here must propagate everywhere. Unmatched globs (e.g. libzstd.*
# on Linux) stay literal and are filtered out by the [ -f ] test.
log "installing native libraries to $RY_HOME/lib/..."
for f in "$TMPDIR_RESCUE"/lib/libemit.* "$TMPDIR_RESCUE"/lib/liblower.* \
         "$TMPDIR_RESCUE"/lib/libry_*.* "$TMPDIR_RESCUE"/lib/libLLVM.* \
         "$TMPDIR_RESCUE"/lib/libzstd.*; do
    [ -f "$f" ] || continue
    install -m 755 "$f" "$RY_HOME/lib/" \
        || die "failed to install $(basename "$f") to $RY_HOME/lib/"
done

# Install the ry binary. install(1) handles cross-fs and atomic replacement.
log "installing ry binary to $INSTALL_DIR/ry..."
install -m 755 "$TMPDIR_RESCUE/ry" "$INSTALL_DIR/ry" \
    || die "failed to install ry binary to $INSTALL_DIR/ry"

# Install stdlib via staged swap (atomic from the loader's perspective).
STD_DIR="$RY_HOME/share/std"
STD_STAGING="$(mktemp -d "${STD_DIR}.XXXXXX")"
cp -R "$TMPDIR_RESCUE/share/std/." "$STD_STAGING/"
rm -rf "$STD_DIR"
mv "$STD_STAGING" "$STD_DIR"
STD_STAGING=""
log "stdlib installed to $STD_DIR"

# Migration: clean up legacy lib/std if it lingers from a pre-#2005 install.
# This mirrors the tail of install.sh and is bounded to lib/std (NOT lib/).
if [ -d "$RY_HOME/lib/std" ]; then
    rm -rf "$RY_HOME/lib/std"
fi

# Refresh ry-rescue itself if the archive ships it (post-#2455 tarballs do).
if [ -f "$TMPDIR_RESCUE/ry-rescue" ]; then
    install -m 755 "$TMPDIR_RESCUE/ry-rescue" "$INSTALL_DIR/ry-rescue" \
        || warn "failed to refresh ry-rescue at $INSTALL_DIR/ry-rescue"
fi

log "recovery complete — $TAG installed"
case ":$PATH:" in
    *":${INSTALL_DIR}:"*) ;;
    *) warn "$INSTALL_DIR is not on PATH — add it: export PATH=\"$INSTALL_DIR:\$PATH\"" ;;
esac

"$INSTALL_DIR/ry" --version 2>/dev/null || warn "ry --version still fails; please file a bug report with the output of '$INSTALL_DIR/ry --version' redirected stderr"
