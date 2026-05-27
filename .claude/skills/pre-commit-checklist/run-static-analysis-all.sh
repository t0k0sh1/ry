#!/usr/bin/env bash
# §3.5.5 all (clang-tidy + cppcheck + scan-build via Docker)
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."
exec ./docker/run.sh static-analysis all
