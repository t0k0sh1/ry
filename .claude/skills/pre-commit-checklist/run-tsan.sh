#!/usr/bin/env bash
# §3.5 TSan (Docker) — C++ run is required, Ry self-test is warn-only
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."

CLEAN_FLAG=""
for arg in "$@"; do
  case "$arg" in
    --clean) CLEAN_FLAG="--clean" ;;
    -h|--help)
      sed -n '2p' "$0"
      echo "Usage: run-tsan.sh [--clean]"
      echo "  --clean: remove host build-tsan-docker/ before building (forwarded to docker/run.sh)"
      exit 0
      ;;
    *) echo "unknown option: $arg" >&2; exit 2 ;;
  esac
done

# --clean goes to the FIRST docker/run.sh call only; passing it to the second
# would wipe the freshly-built build-tsan-docker/ and force a full rebuild.
./docker/run.sh $CLEAN_FLAG tsan ry_tests
./docker/run.sh tsan ry test -p
