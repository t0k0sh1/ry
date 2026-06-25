#!/usr/bin/env bash
# Contract test for scripts/export-run-logs.sh JSONL schema (#2300).
#
# Runs the script against a 1-case fixture and asserts the run_meta + command
# record invariants documented in docs/architecture/jsonl-run-logs.md. Drift
# in any of these surfaces (schema_version literal, summary-line format in
# src/test_runtime.cpp, summary parser regex in scripts/export-run-logs.sh)
# fails this test with a focused diagnostic.

set -euo pipefail

if ! command -v jq >/dev/null 2>&1; then
  echo "error: 'jq' is required by tests/scripts/test-export-run-logs.sh" >&2
  exit 1
fi

cd "$(dirname "${BASH_SOURCE[0]}")/../.."

# Build-dir resolution (RY_BUILD_DIR → build-rust/ → build/) and the
# "no ry binary" fast-fail are delegated to scripts/export-run-logs.sh
# itself (lines 121-142). Inheriting RY_BUILD_DIR from our env is enough.

# ---- Scratch output dir ----------------------------------------------------
# RY_LOG_DIR must be repo-relative (script enforces this at :160). Use a
# per-PID subdir under .ry-eval/ (which is git-ignored) so concurrent
# invocations don't collide and so cleanup is local. rm in committed scripts
# is permitted per AGENTS.md.

SCRATCH=".ry-eval/test-export-run-logs-$$"
rm -rf "$SCRATCH"
mkdir -p "$SCRATCH"
trap 'rm -rf "$SCRATCH"' EXIT

FIXTURE="tests/scripts/fixtures/sample.test.ry"
if [[ ! -f "$FIXTURE" ]]; then
  echo "error: fixture not found: $FIXTURE" >&2
  exit 1
fi

# ---- Run the script under test ---------------------------------------------
# Baseline runs go in their own subdir so later sub-tests (e.g. the symlink
# regression below) can use sibling subdirs without breaking the
# "exactly one run dir" glob.

BASELINE_LOG_DIR="$SCRATCH/baseline"
mkdir -p "$BASELINE_LOG_DIR"
RY_LOG_DIR="$BASELINE_LOG_DIR" bash scripts/export-run-logs.sh "$FIXTURE" >/dev/null

# ---- Locate the produced run.jsonl -----------------------------------------

shopt -s nullglob
candidates=("$BASELINE_LOG_DIR"/*/)
shopt -u nullglob
if (( ${#candidates[@]} != 1 )); then
  echo "error: expected exactly one run dir under $BASELINE_LOG_DIR, found ${#candidates[@]}" >&2
  exit 1
fi
RUN_DIR="${candidates[0]%/}"
RUN_JSONL="$RUN_DIR/run.jsonl"

if [[ ! -f "$RUN_JSONL" ]]; then
  echo "error: $RUN_JSONL was not produced" >&2
  exit 1
fi

# `mapfile` is bash 4+; macOS ships bash 3.2 by default, so use a portable
# read-into-array loop instead.
records=()
while IFS= read -r line; do records+=("$line"); done <"$RUN_JSONL"
if (( ${#records[@]} < 2 )); then
  echo "error: expected >= 2 JSONL records (run_meta + >= 1 command), got ${#records[@]}" >&2
  cat "$RUN_JSONL" >&2
  exit 1
fi

fail_with_record() {
  echo "error: $1" >&2
  echo "  record: $2" >&2
  exit 1
}

# ---- Every line parses as JSON (1 jq fork over the whole stream) ----------

if ! jq -e . "$RUN_JSONL" >/dev/null; then
  echo "error: $RUN_JSONL contains an invalid JSON line (see jq diagnostic above)" >&2
  exit 1
fi

# ---- First record: run_meta -----------------------------------------------

first="${records[0]}"
rt="$(jq -r '.record_type' <<<"$first")"
if [[ "$rt" != "run_meta" ]]; then
  fail_with_record "expected first record_type='run_meta', got '$rt'" "$first"
fi

schema="$(jq -r '.schema_version' <<<"$first")"
if [[ "$schema" != "1" ]]; then
  echo "error: run_meta.schema_version != \"1\" (got: '$schema')" >&2
  echo "       if this is an intentional schema bump, update tests/scripts/test-export-run-logs.sh and docs/architecture/jsonl-run-logs.md." >&2
  echo "  record: $first" >&2
  exit 1
fi

for key in run_id started_at host_os git_sha git_branch git_dirty ry_build_dir; do
  if ! jq -e --arg k "$key" 'has($k)' <<<"$first" >/dev/null; then
    fail_with_record "run_meta missing key '$key'" "$first"
  fi
done

# ---- Subsequent records: command ------------------------------------------

cmd_count=0
for line in "${records[@]:1}"; do
  rt="$(jq -r '.record_type' <<<"$line")"
  if [[ "$rt" != "command" ]]; then
    fail_with_record "expected record_type='command' after run_meta, got '$rt'" "$line"
  fi

  cs="$(jq -r '.schema_version' <<<"$line")"
  if [[ "$cs" != "1" ]]; then
    echo "error: command.schema_version != \"1\" (got: '$cs')" >&2
    echo "       drift between run_meta and command schema_version is the exact failure mode #2300 prevents." >&2
    echo "  record: $line" >&2
    exit 1
  fi

  for key in run_id target command exit_code duration_ms started_at finished_at \
             stdout_path stderr_path stdout_byte_count summary; do
    if ! jq -e --arg k "$key" 'has($k)' <<<"$line" >/dev/null; then
      fail_with_record "command missing key '$key'" "$line"
    fi
  done

  cmd_count=$((cmd_count + 1))
done

if (( cmd_count == 0 )); then
  echo "error: no command records produced" >&2
  exit 1
fi

# ---- Last command: summary parsed AND matches exit_code -------------------

last_cmd="${records[${#records[@]}-1]}"
summary_json="$(jq -c '.summary' <<<"$last_cmd")"
exit_code="$(jq -r '.exit_code' <<<"$last_cmd")"

if [[ "$summary_json" == "null" ]]; then
  echo "error: last command's summary is null." >&2
  echo "       this indicates drift between the summary parser (scripts/export-run-logs.sh:295-297)" >&2
  echo "       and the format emitted by src/test_runtime.cpp (__ry_test_summary)." >&2
  fail_with_record "summary parse failed" "$last_cmd"
fi

for key in passed failed skipped todo; do
  if ! jq -e --arg k "$key" 'has($k)' <<<"$summary_json" >/dev/null; then
    fail_with_record "summary missing key '$key' (got: $summary_json)" "$last_cmd"
  fi
done

failed_count="$(jq -r '.failed' <<<"$summary_json")"
if [[ "$failed_count" == "0" && "$exit_code" != "0" ]]; then
  echo "error: summary.failed=0 but exit_code=$exit_code." >&2
  echo "       drift between summary parser and ry exit code." >&2
  fail_with_record "summary/exit_code mismatch" "$last_cmd"
fi

# Trivial-pass fixture: exit_code must be 0 and passed must be 1.
if [[ "$exit_code" != "0" ]]; then
  fail_with_record "expected exit_code=0 for trivial-pass fixture, got '$exit_code'" "$last_cmd"
fi
passed_count="$(jq -r '.passed' <<<"$summary_json")"
if [[ "$passed_count" != "1" ]]; then
  fail_with_record "expected summary.passed=1 for trivial-pass fixture, got '$passed_count'" "$last_cmd"
fi

echo "OK: tests/scripts/test-export-run-logs.sh — ${#records[@]} JSONL records validated ($cmd_count command)"

# ---- Regression: symlinked .test.ry must be discovered (#2403) ------------
# Reproduce the discovery bug where `find` (without -L) skips symlinks pointing
# at .test.ry files. The fix is `find -L` in scripts/export-run-logs.sh's
# target-expansion loop. This sub-test asserts *discovery*, not ry execution:
# it only requires that a directory containing a symlinked .test.ry produces
# >= 1 command record. Don't assert summary.passed here — that couples the
# discovery test to ry's open()-follows-symlinks behavior.

LINK_DIR="$SCRATCH/symlink-fixture"
SYMLINK_LOG_DIR="$SCRATCH/symlink-runs"
mkdir -p "$LINK_DIR" "$SYMLINK_LOG_DIR"
# Use an absolute path so the symlink resolves regardless of $PWD at use time.
# $PWD is the repo root (set by the cd above); $FIXTURE is repo-relative.
ln -s "$PWD/$FIXTURE" "$LINK_DIR/linked.test.ry"

set +e
RY_LOG_DIR="$SYMLINK_LOG_DIR" \
  bash scripts/export-run-logs.sh "$LINK_DIR" \
  >"$SCRATCH/symlink.stdout" 2>"$SCRATCH/symlink.stderr"
sym_exit_code=$?
set -e

if [[ "$sym_exit_code" != "0" ]]; then
  echo "error: export-run-logs.sh failed on a dir whose only .test.ry is a symlink (exit_code=$sym_exit_code, #2403)" >&2
  echo "       find in scripts/export-run-logs.sh's target-expansion loop must use -L to follow symlinks." >&2
  echo "       stderr from the failed invocation:" >&2
  sed 's/^/         /' "$SCRATCH/symlink.stderr" >&2
  exit 1
fi

shopt -s nullglob
sym_candidates=("$SYMLINK_LOG_DIR"/*/)
shopt -u nullglob
if (( ${#sym_candidates[@]} != 1 )); then
  echo "error: expected exactly one run dir under $SYMLINK_LOG_DIR, found ${#sym_candidates[@]}" >&2
  exit 1
fi
SYM_RUN_JSONL="${sym_candidates[0]%/}/run.jsonl"

if ! jq -e 'select(.record_type=="command")' "$SYM_RUN_JSONL" >/dev/null; then
  echo "error: no command record produced from symlinked .test.ry target (#2403)" >&2
  echo "       symlinked .test.ry under $LINK_DIR was not discovered by export-run-logs.sh." >&2
  exit 1
fi

# The discovered target's repo-relative path must match the symlink we placed,
# not the resolved fixture path. This guards against a future "-L combined with
# real-path resolution" regression that would change the recorded target. Use
# --slurp + .[0] instead of `jq … | head -1` to avoid an early-close SIGPIPE
# turning jq into a failure under `pipefail`.
sym_target="$(jq -rs 'map(select(.record_type=="command") | .target) | .[0]' "$SYM_RUN_JSONL")"
if [[ "$sym_target" != "$LINK_DIR/linked.test.ry" ]]; then
  echo "error: command.target=$sym_target, expected $LINK_DIR/linked.test.ry (#2403)" >&2
  exit 1
fi

echo "OK: tests/scripts/test-export-run-logs.sh — symlinked .test.ry discovery (#2403)"

# ---- RUN_NONCE in-process uniqueness (#2402) ------------------------------
# Source the helper and call gen_run_nonce many times within this same shell
# (constant PID / $$). A nonce derived from $$ alone would yield the same
# value for every call here and fail this check; a wall-clock-only nonce with
# microsecond resolution can also collide under tight in-process loops. The
# real /dev/urandom-backed helper trivially passes.

# shellcheck source=../../scripts/lib/run-nonce.sh
source "scripts/lib/run-nonce.sh"

NONCE_ITERATIONS=50
nonces=()
for ((i = 0; i < NONCE_ITERATIONS; i++)); do
  nonces+=("$(gen_run_nonce)")
done

unique_nonce_count="$(printf '%s\n' "${nonces[@]}" | sort -u | wc -l | tr -d ' ')"
if [[ "$unique_nonce_count" != "$NONCE_ITERATIONS" ]]; then
  echo "error: gen_run_nonce produced duplicates within a single shell —" >&2
  echo "       expected $NONCE_ITERATIONS unique values, got $unique_nonce_count." >&2
  echo "       this catches regressions where RUN_NONCE depends on per-process state" >&2
  echo "       like \$\$ (PID) instead of a true entropy source (#2402)." >&2
  exit 1
fi

echo "OK: tests/scripts/test-export-run-logs.sh — ${NONCE_ITERATIONS} in-process nonces unique (#2402)"
