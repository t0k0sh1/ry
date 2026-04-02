#!/bin/sh
# assemble-changelog.sh — Merge changelog fragment files into CHANGELOG.md
#
# Usage: scripts/assemble-changelog.sh [--dry-run] [--keep]
#   --dry-run  Print the assembled [Unreleased] section to stdout without modifying CHANGELOG.md
#   --keep     Do not delete fragment files after assembly
#
# Fragment files live in changelog.d/*.md and contain only ### sections
# (Added, Changed, Deprecated, Removed, Fixed, Security).

set -eu

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
CHANGELOG="$REPO_ROOT/CHANGELOG.md"
FRAGMENT_DIR="$REPO_ROOT/changelog.d"
TMPDIR_ASSEMBLE="${TMPDIR:-/tmp}/changelog-assemble.$$"

dry_run=false
keep_fragments=false

for arg in "$@"; do
  case "$arg" in
    --dry-run) dry_run=true ;;
    --keep)    keep_fragments=true ;;
    *)         echo "Unknown option: $arg" >&2; exit 1 ;;
  esac
done

cleanup() {
  rm -rf "$TMPDIR_ASSEMBLE"
}
trap cleanup EXIT

mkdir -p "$TMPDIR_ASSEMBLE"

# Canonical section order per Keep a Changelog
SECTIONS="Added Changed Deprecated Removed Fixed Security"

# Initialize empty section files
for sec in $SECTIONS; do
  : > "$TMPDIR_ASSEMBLE/$sec"
done

# ── Phase 1: Extract existing [Unreleased] entries from CHANGELOG.md ──

extract_existing_entries() {
  awk '
    /^## \[Unreleased\]/ { found=1; next }
    /^## \[/             { if (found) exit }
    found                { print }
  ' "$CHANGELOG"
}

# Parse section entries (from existing or fragment) and append to section files
parse_entries() {
  current_section=""
  while IFS= read -r line; do
    case "$line" in
      "### Added"*)      current_section="Added" ;;
      "### Changed"*)    current_section="Changed" ;;
      "### Deprecated"*) current_section="Deprecated" ;;
      "### Removed"*)    current_section="Removed" ;;
      "### Fixed"*)      current_section="Fixed" ;;
      "### Security"*)   current_section="Security" ;;
      *)
        if [ -n "$current_section" ]; then
          # Only append non-empty lines (bullet items and continuation lines)
          case "$line" in
            "- "*|"  "*)
              echo "$line" >> "$TMPDIR_ASSEMBLE/$current_section"
              ;;
          esac
        fi
        ;;
    esac
  done
}

# Parse existing entries
extract_existing_entries | parse_entries

# ── Phase 2: Parse fragment files ──

fragment_count=0
# shellcheck disable=SC2044
for frag in "$FRAGMENT_DIR"/*.md; do
  [ -f "$frag" ] || continue
  parse_entries < "$frag"
  fragment_count=$((fragment_count + 1))
done

if [ "$fragment_count" -eq 0 ] && [ "$dry_run" = true ]; then
  echo "No fragment files found in $FRAGMENT_DIR/" >&2
fi

# ── Phase 3: Assemble the [Unreleased] section ──

assemble() {
  echo "## [Unreleased]"
  echo ""
  emitted=false
  for sec in $SECTIONS; do
    if [ -s "$TMPDIR_ASSEMBLE/$sec" ]; then
      if [ "$emitted" = true ]; then
        echo ""
      fi
      echo "### $sec"
      echo ""
      cat "$TMPDIR_ASSEMBLE/$sec"
      emitted=true
    fi
  done
}

if [ "$dry_run" = true ]; then
  assemble
  exit 0
fi

# ── Phase 4: Replace [Unreleased] section in CHANGELOG.md ──

new_changelog="$TMPDIR_ASSEMBLE/CHANGELOG.md"

awk '
  /^## \[Unreleased\]/ { skip=1; next }
  /^## \[/             { if (skip) { skip=0 } }
  !skip                { print }
' "$CHANGELOG" > "$TMPDIR_ASSEMBLE/rest.md"

# Build new CHANGELOG: header + assembled unreleased + rest (from first versioned section)
{
  # Header (lines before [Unreleased])
  awk '/^## \[Unreleased\]/ { exit } { print }' "$CHANGELOG"

  # Assembled [Unreleased] section
  assemble
  echo ""

  # Rest of the file (from first versioned ## section onward)
  awk '
    /^## \[Unreleased\]/ { skip=1; next }
    /^## \[/ {
      if (skip) { skip=0; print; next }
      else { print; next }
    }
    !skip { next }
    { print }
  ' "$CHANGELOG"
  # Actually the above awk is wrong; let me use a simpler approach
} > /dev/null 2>&1 || true

# Simpler approach: split into 3 parts
# Part 1: Everything before "## [Unreleased]"
# Part 2: The assembled [Unreleased] section (generated)
# Part 3: Everything after the [Unreleased] section (starting from the next ## [x.x.x])

awk '/^## \[Unreleased\]/ { exit } { print }' "$CHANGELOG" > "$new_changelog"
assemble >> "$new_changelog"
echo "" >> "$new_changelog"
awk '
  BEGIN { in_unreleased=0; past_unreleased=0 }
  /^## \[Unreleased\]/ { in_unreleased=1; next }
  /^## \[/ {
    if (in_unreleased) { in_unreleased=0; past_unreleased=1; print; next }
    if (past_unreleased) { print; next }
  }
  in_unreleased { next }
  past_unreleased { print }
' "$CHANGELOG" >> "$new_changelog"

cp "$new_changelog" "$CHANGELOG"
echo "Updated $CHANGELOG"

# ── Phase 5: Remove used fragment files ──

if [ "$keep_fragments" = false ] && [ "$fragment_count" -gt 0 ]; then
  for frag in "$FRAGMENT_DIR"/*.md; do
    [ -f "$frag" ] || continue
    rm "$frag"
  done
  echo "Removed $fragment_count fragment file(s) from $FRAGMENT_DIR/"
fi
