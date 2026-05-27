#!/usr/bin/env bash
# §3.5.5 clang-tidy (Docker)
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."
exec ./docker/run.sh static-analysis clang-tidy
