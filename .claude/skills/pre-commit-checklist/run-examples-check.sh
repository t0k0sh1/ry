#!/usr/bin/env bash
# Canonical examples verification (#2329) — runs scripts/check-examples.sh
# against every examples/*.ry file (non-recursive). Run before pushing any
# change under examples/ or scripts/check-examples.sh.
#
# Requires: a built ry binary in build-rust/ or build/ (or $RY_BUILD_DIR).
# No --clean flag: no build dir owned by this wrapper.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."

echo "==> scripts/check-examples.sh" >&2
bash scripts/check-examples.sh

echo "==> canonical examples OK" >&2
