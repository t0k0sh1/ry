#!/usr/bin/env bash
# §3.5.8 export-run-logs JSONL schema contract test (#2300) — runs the harness
# at tests/scripts/test-export-run-logs.sh against a tiny fixture, asserts the
# run_meta + command record invariants documented in
# docs/architecture/jsonl-run-logs.md. Run before pushing any change under
# scripts/export-run-logs.sh, tests/scripts/, or src/test_runtime.cpp's
# __ry_test_summary output format.
#
# Requires: a built ry binary in build-rust/ or build/ (or $RY_BUILD_DIR).
# No --clean flag: shell + jq harness, no build dir to wipe.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."

echo "==> tests/scripts/test-export-run-logs.sh" >&2
bash tests/scripts/test-export-run-logs.sh

echo "==> export-run-logs JSONL schema OK" >&2
