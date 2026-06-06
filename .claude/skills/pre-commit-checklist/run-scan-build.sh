#!/usr/bin/env bash
# §3.5.5 scan-build (Docker) — warn-only
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."

CLEAN_FLAG=""
for arg in "$@"; do
  case "$arg" in
    --clean) CLEAN_FLAG="--clean" ;;
    -h|--help)
      sed -n '2p' "$0"
      echo "Usage: run-scan-build.sh [--clean]"
      echo "  --clean: remove host build-docker/ + build-scan-docker/ before building (forwarded to docker/run.sh)"
      exit 0
      ;;
    *) echo "unknown option: $arg" >&2; exit 2 ;;
  esac
done

exec ./docker/run.sh $CLEAN_FLAG static-analysis scan-build
