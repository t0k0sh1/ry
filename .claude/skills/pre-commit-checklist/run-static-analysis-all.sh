#!/usr/bin/env bash
# §3.5.5 all (clang-tidy + cppcheck + scan-build via Docker)
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."

CLEAN_FLAG=""
for arg in "$@"; do
  case "$arg" in
    --clean) CLEAN_FLAG="--clean" ;;
    -h|--help)
      sed -n '2p' "$0"
      echo "Usage: run-static-analysis-all.sh [--clean]"
      echo "  --clean: remove host build-docker/ + build-scan-docker/ before building (forwarded to docker/run.sh)"
      exit 0
      ;;
    *) echo "unknown option: $arg" >&2; exit 2 ;;
  esac
done

exec ./docker/run.sh $CLEAN_FLAG static-analysis all
