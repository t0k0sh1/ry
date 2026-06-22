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

RY_LOG_DIR="$SCRATCH" bash scripts/export-run-logs.sh "$FIXTURE" >/dev/null

# ---- Locate the produced run.jsonl -----------------------------------------

shopt -s nullglob
candidates=("$SCRATCH"/*/)
shopt -u nullglob
if (( ${#candidates[@]} != 1 )); then
  echo "error: expected exactly one run dir under $SCRATCH, found ${#candidates[@]}" >&2
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
