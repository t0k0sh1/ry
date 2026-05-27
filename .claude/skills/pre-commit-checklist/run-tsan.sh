#!/usr/bin/env bash
# §3.5 TSan (Docker) — C++ run is required, Ry self-test is warn-only
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."

./docker/run.sh tsan ry_tests
./docker/run.sh tsan ry test -p
