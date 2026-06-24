#!/usr/bin/env bash
# Verify that canonical examples/*.ry programs still run cleanly (#2329).
#
# Runs `ry run` on every *.ry file directly under examples/ (non-recursive)
# and reports failures by file name. Exit code only — no golden-output
# matching. The non-recursive glob is the exclusion mechanism: future
# negative / review / intentional-failure examples live in subdirectories
# such as examples/negative/ or examples/review/ (see examples/README.md)
# and are skipped automatically.
#
# Usage:
#   scripts/check-examples.sh
# Env:
#   RY_BUILD_DIR — override build dir (default: build-rust/ if present,
#                  else build/). Must be a repo-relative path.
# Exit codes:
#   0  every example exited 0
#   1  one or more examples failed, or the ry binary was not located
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."

if [[ -n "${RY_BUILD_DIR:-}" ]]; then
  RY_BUILD_DIR="${RY_BUILD_DIR%/}"
  if [[ ! -x "$RY_BUILD_DIR/ry" ]]; then
    echo "error: ry binary not found at $RY_BUILD_DIR/ry" >&2
    exit 1
  fi
elif [[ -x "build-rust/ry" ]]; then
  RY_BUILD_DIR="build-rust"
elif [[ -x "build/ry" ]]; then
  RY_BUILD_DIR="build"
else
  echo "error: ry binary not found in build-rust/ or build/; set RY_BUILD_DIR" >&2
  exit 1
fi

RY_BIN="$RY_BUILD_DIR/ry"

shopt -s nullglob
EXAMPLES=(examples/*.ry)
shopt -u nullglob

(( ${#EXAMPLES[@]} > 0 )) || { echo "error: no examples/*.ry files found" >&2; exit 1; }

TOTAL=${#EXAMPLES[@]}
PASS=0
FAIL=0

for f in "${EXAMPLES[@]}"; do
  if out="$("$RY_BIN" run "$f" 2>&1)"; then
    PASS=$((PASS + 1))
  else
    FAIL=$((FAIL + 1))
    printf 'FAIL %s\n' "$f" >&2
    printf '%s\n' "$out" >&2
    echo "::error::canonical example failed: $f" >&2
  fi
done

echo "$PASS/$TOTAL examples passed" >&2
(( FAIL == 0 )) || exit 1
