#!/usr/bin/env bash
# §3.6 libFuzzer (fuzz_parser / fuzz_json / fuzz_utf8 / fuzz_io_open — 60s each, via Docker)
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."

./docker/run.sh fuzz fuzz_parser -max_total_time=60 -rss_limit_mb=2048 \
    -artifact_prefix=tests/fuzz/regressions/parser/ tests/fuzz/corpus/parser

./docker/run.sh fuzz fuzz_json -max_total_time=60 -rss_limit_mb=2048 \
    -artifact_prefix=tests/fuzz/regressions/json/ tests/fuzz/corpus/json

./docker/run.sh fuzz fuzz_utf8 -max_total_time=60 -rss_limit_mb=2048 \
    -artifact_prefix=tests/fuzz/regressions/utf8/ tests/fuzz/corpus/utf8

./docker/run.sh fuzz fuzz_io_open -max_total_time=60 -rss_limit_mb=2048 \
    -artifact_prefix=tests/fuzz/regressions/fuzz_io_open/ tests/fuzz/corpus/fuzz_io_open
