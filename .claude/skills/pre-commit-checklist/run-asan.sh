#!/usr/bin/env bash
# §3.5 ASan + UBSan (Docker)
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."

CLEAN_FLAG=""
for arg in "$@"; do
  case "$arg" in
    --clean) CLEAN_FLAG="--clean" ;;
    -h|--help)
      sed -n '2p' "$0"
      echo "Usage: run-asan.sh [--clean]"
      echo "  --clean: remove host build-asan-docker/ before building (forwarded to docker/run.sh)"
      exit 0
      ;;
    *) echo "unknown option: $arg" >&2; exit 2 ;;
  esac
done

# --clean goes to the FIRST docker/run.sh call only; passing it to the second
# would wipe the freshly-built build-asan-docker/ and force a full rebuild.
./docker/run.sh $CLEAN_FLAG asan ry_tests
./docker/run.sh asan ry test -p
