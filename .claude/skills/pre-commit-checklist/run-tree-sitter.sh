#!/usr/bin/env bash
# §3.6.5 tree-sitter Grammar Regression Check
#
# No --clean flag: no CMake build dir — build.sh/install.sh/check.sh regenerate
# ry.so from the grammar on each run. (#2042)
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."

./editor/tree-sitter/build.sh
./editor/tree-sitter/install.sh --no-build
./editor/tree-sitter/check.sh --no-build
