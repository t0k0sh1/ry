#!/usr/bin/env bash
# §3.5.7 Prompt/instruction reference lint — reproduce the CI `lint` job's
# "Check prompt/instruction reference integrity" step locally (#2029):
# fails on stale inline-code paths and dead /skill links across .claude/ +
# AGENTS.md + CLAUDE.md.
#
# Run before pushing any change under .claude/, AGENTS.md, or CLAUDE.md.
#
# No --clean flag: pure bash/grep/awk/find, no build dir to wipe. (#2042)
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."

echo "==> scripts/check-prompt-refs.sh" >&2
bash scripts/check-prompt-refs.sh

echo "==> prompt-refs lint OK" >&2
