#!/usr/bin/env bash
# §3.5 ASan + UBSan (Docker)
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."

./docker/run.sh asan ry_tests
./docker/run.sh asan ry test -p
