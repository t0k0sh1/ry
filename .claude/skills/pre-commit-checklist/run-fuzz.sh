#!/usr/bin/env bash
# §3.6 libFuzzer (fuzz_parser / fuzz_json / fuzz_utf8 / fuzz_io_open — 60s each, via Docker)
# fuzz_json5 (#1855) is built and validated locally but not yet in the docker
# allowlist — re-add the docker/run.sh call after the next /ci-image-workflow.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."

CLEAN_FLAG=""
for arg in "$@"; do
  case "$arg" in
    --clean) CLEAN_FLAG="--clean" ;;
    -h|--help)
      sed -n '2p' "$0"
      echo "Usage: run-fuzz.sh [--clean]"
      echo "  --clean: remove host build-fuzz-docker/ before building (forwarded to docker/run.sh)"
      exit 0
      ;;
    *) echo "unknown option: $arg" >&2; exit 2 ;;
  esac
done

# --clean goes to the FIRST docker/run.sh call only; passing it to the later
# targets would wipe the freshly-built build-fuzz-docker/ and force a rebuild each.
./docker/run.sh $CLEAN_FLAG fuzz fuzz_parser -max_total_time=60 -rss_limit_mb=2048 \
    -artifact_prefix=tests/fuzz/regressions/parser/ tests/fuzz/corpus/parser

./docker/run.sh fuzz fuzz_json -max_total_time=60 -rss_limit_mb=2048 \
    -artifact_prefix=tests/fuzz/regressions/json/ tests/fuzz/corpus/json

./docker/run.sh fuzz fuzz_utf8 -max_total_time=60 -rss_limit_mb=2048 \
    -artifact_prefix=tests/fuzz/regressions/utf8/ tests/fuzz/corpus/utf8

./docker/run.sh fuzz fuzz_io_open -max_total_time=60 -rss_limit_mb=2048 \
    -artifact_prefix=tests/fuzz/regressions/fuzz_io_open/ tests/fuzz/corpus/fuzz_io_open
