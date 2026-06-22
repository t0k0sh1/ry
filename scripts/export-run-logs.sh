#!/usr/bin/env bash
# Export structured JSON Lines run logs from ry test commands (#1731).
# See `--help` for usage; schema lives in docs/architecture/jsonl-run-logs.md.
#
# Exit codes:
#   0   one or more commands were captured (a command's own non-zero exit is
#       *data* and is recorded in the JSONL; it is not a script failure).
#   1   bad arguments, missing dependency, missing binary, no targets were
#       captured (e.g. all paths missing), or an unrecoverable I/O error.

set -euo pipefail

# ---- Dependencies ----------------------------------------------------------

if ! command -v jq >/dev/null 2>&1; then
  echo "error: 'jq' is required by scripts/export-run-logs.sh" >&2
  exit 1
fi

# ---- Constants -------------------------------------------------------------

# JSONL schema version (single source of truth for both record builders, #2300).
# Schema contract: docs/architecture/jsonl-run-logs.md.
SCHEMA_VERSION="1"

# ---- Repo root -------------------------------------------------------------

REPO_ROOT="$PWD"
if root="$(git rev-parse --show-toplevel 2>/dev/null)"; then
  REPO_ROOT="$root"
fi
cd "$REPO_ROOT"

# Paths recorded in JSONL must be repo-relative. Reject absolute and
# parent-escaping paths up-front so a misconfigured RY_BUILD_DIR /
# RY_LOG_DIR / positional target cannot leak into the metadata.
is_repo_relative_path() {
  local p="$1"
  [[ "$p" != /* && "$p" != ../* && "$p" != */../* ]]
}

# ---- Help ------------------------------------------------------------------

print_help() {
  cat <<'EOF'
Usage:
  scripts/export-run-logs.sh [--trace] <target> [<target>...]
  scripts/export-run-logs.sh --target=ry_tests
  scripts/export-run-logs.sh -h | --help

<target>      a *.test.ry file, or a directory expanded recursively.
--trace       enable per-invocation JSONL execution trace (incompatible
              with --target=ry_tests).
--target=ry_tests   run the C++ gtest binary; gtest JSON goes to
                    artifacts/gtest.json.

Env vars:
  RY_BUILD_DIR  build directory (default: build-rust/ if present else build/)
  RY_LOG_DIR    output root      (default: .ry-eval/runs)

Output:
  $RY_LOG_DIR/<run-id>/run.jsonl + artifacts/*
EOF
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  print_help
  exit 0
fi

# ---- Argument parse --------------------------------------------------------

TRACE_MODE=false
TARGET_MODE="files"
declare -a TARGETS=()

while (( $# > 0 )); do
  case "$1" in
    --trace)
      TRACE_MODE=true
      shift
      ;;
    --target=ry_tests)
      TARGET_MODE="ry_tests"
      shift
      ;;
    --target=*)
      echo "error: unsupported --target value: ${1#--target=}" >&2
      exit 1
      ;;
    -*)
      echo "error: unknown flag: $1" >&2
      exit 1
      ;;
    *)
      TARGETS+=("$1")
      shift
      ;;
  esac
done

if [[ "$TARGET_MODE" == "ry_tests" ]]; then
  if [[ "$TRACE_MODE" == "true" ]]; then
    echo "error: --trace is not supported with --target=ry_tests" >&2
    exit 1
  fi
  if (( ${#TARGETS[@]} > 0 )); then
    echo "error: positional targets are not allowed with --target=ry_tests" >&2
    exit 1
  fi
elif (( ${#TARGETS[@]} == 0 )); then
  echo "error: no targets provided" >&2
  print_help >&2
  exit 1
fi

# ---- Build dir & binaries --------------------------------------------------

if [[ -n "${RY_BUILD_DIR:-}" ]]; then
  RY_BUILD_DIR="${RY_BUILD_DIR%/}"
  if ! is_repo_relative_path "$RY_BUILD_DIR"; then
    echo "error: RY_BUILD_DIR must be a repo-relative path (got: $RY_BUILD_DIR)" >&2
    exit 1
  fi
elif [[ -x "build-rust/ry" || -x "build-rust/ry_tests" ]]; then
  RY_BUILD_DIR="build-rust"
elif [[ -x "build/ry" || -x "build/ry_tests" ]]; then
  RY_BUILD_DIR="build"
else
  echo "error: ry binaries not found in build-rust/ or build/; set RY_BUILD_DIR" >&2
  exit 1
fi

RY_BIN="$RY_BUILD_DIR/ry"
RY_TESTS_BIN="$RY_BUILD_DIR/ry_tests"

if [[ "$TARGET_MODE" == "files" && ! -x "$RY_BIN" ]]; then
  echo "error: ry binary not found at $RY_BIN" >&2
  exit 1
fi
if [[ "$TARGET_MODE" == "ry_tests" && ! -x "$RY_TESTS_BIN" ]]; then
  echo "error: ry_tests binary not found at $RY_TESTS_BIN" >&2
  exit 1
fi

# ---- Git metadata ----------------------------------------------------------

GIT_SHA=""
GIT_SHORT="nogit"
GIT_BRANCH=""
GIT_DIRTY=false
if sha="$(git rev-parse HEAD 2>/dev/null)"; then
  GIT_SHA="$sha"
  GIT_SHORT="${sha:0:7}"
fi
if br="$(git rev-parse --abbrev-ref HEAD 2>/dev/null)"; then
  GIT_BRANCH="$br"
fi
if dirty_out="$(git status --porcelain 2>/dev/null)"; then
  [[ -n "$dirty_out" ]] && GIT_DIRTY=true
fi

# ---- Output layout ---------------------------------------------------------

LOG_DIR_BASE="${RY_LOG_DIR:-.ry-eval/runs}"
if ! is_repo_relative_path "$LOG_DIR_BASE"; then
  echo "error: RY_LOG_DIR must be a repo-relative path (got: $LOG_DIR_BASE)" >&2
  exit 1
fi
RUN_NONCE="$(jq -nr 'now * 1000000 | floor')"
RUN_ID="$(date -u +%Y%m%d-%H%M%S)-$GIT_SHORT-$RUN_NONCE"
RUN_DIR="$LOG_DIR_BASE/$RUN_ID"
ARTIFACTS_DIR="$RUN_DIR/artifacts"
RUN_JSONL="$RUN_DIR/run.jsonl"
mkdir -p "$ARTIFACTS_DIR"

# ---- Timing & metadata helpers --------------------------------------------

now_ms() {
  jq -n 'now * 1000 | floor'
}

RY_VERSION=""
if [[ -x "$RY_BIN" ]]; then
  if rv="$("$RY_BIN" --version 2>/dev/null | head -n1)"; then
    RY_VERSION="$rv"
  fi
fi

# ---- run_meta record -------------------------------------------------------

STARTED_AT="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
HOST_OS="$(uname -s)"

jq -nc \
  --arg schema_version "$SCHEMA_VERSION" \
  --arg run_id "$RUN_ID" \
  --arg started_at "$STARTED_AT" \
  --arg host_os "$HOST_OS" \
  --arg git_sha "$GIT_SHA" \
  --arg git_branch "$GIT_BRANCH" \
  --argjson git_dirty "$GIT_DIRTY" \
  --arg ry_build_dir "$RY_BUILD_DIR" \
  --arg ry_version "$RY_VERSION" \
  '{
    record_type: "run_meta",
    schema_version: $schema_version,
    run_id: $run_id,
    started_at: $started_at,
    host_os: $host_os,
    git_sha: $git_sha,
    git_branch: $git_branch,
    git_dirty: $git_dirty,
    ry_build_dir: $ry_build_dir,
    ry_version: (if $ry_version == "" then null else $ry_version end)
  }' >> "$RUN_JSONL"

# ---- Target expansion ------------------------------------------------------

declare -a EXPANDED=()
if [[ "$TARGET_MODE" == "files" ]]; then
  for raw in "${TARGETS[@]}"; do
    target="${raw#./}"
    if ! is_repo_relative_path "$target"; then
      echo "warning: target is not repo-relative, skipping: $raw" >&2
      continue
    fi
    if [[ -d "$target" ]]; then
      while IFS= read -r -d '' f; do
        EXPANDED+=("${f#./}")
      done < <(find "$target" -type f -name '*.test.ry' -print0 | sort -z)
    elif [[ -f "$target" ]]; then
      EXPANDED+=("$target")
    else
      echo "warning: target not found, skipping: $raw" >&2
    fi
  done
fi

# ---- Run-one helper --------------------------------------------------------

CAPTURED=0
ATTEMPTED=0
TOTAL_PLANNED=0
if [[ "$TARGET_MODE" == "ry_tests" ]]; then
  TOTAL_PLANNED=1
else
  TOTAL_PLANNED=${#EXPANDED[@]}
fi

run_one() {
  local target="$1"
  local slug="$2"
  shift 2
  local -a cmd=("$@")
  local attempt_id
  attempt_id="$(printf '%04d' "$((ATTEMPTED + 1))")"
  local artifact_prefix="${attempt_id}-${slug}"

  local stdout_file="$ARTIFACTS_DIR/$artifact_prefix.stdout.txt"
  local stderr_file="$ARTIFACTS_DIR/$artifact_prefix.stderr.txt"
  local stdout_path="${stdout_file#"$REPO_ROOT/"}"
  local stderr_path="${stderr_file#"$REPO_ROOT/"}"

  local trace_path=""
  local gtest_json_path=""

  if [[ "$TRACE_MODE" == "true" && "$target" != "ry_tests" ]]; then
    local trace_file="$ARTIFACTS_DIR/$artifact_prefix.trace.jsonl"
    cmd+=("--trace" "--trace-out=$trace_file")
    trace_path="${trace_file#"$REPO_ROOT/"}"
  fi
  if [[ "$target" == "ry_tests" ]]; then
    local gtest_file="$ARTIFACTS_DIR/gtest.json"
    cmd+=("--gtest_output=json:$gtest_file")
    gtest_json_path="${gtest_file#"$REPO_ROOT/"}"
  fi

  ATTEMPTED=$((ATTEMPTED + 1))
  echo "[$ATTEMPTED/$TOTAL_PLANNED] $target" >&2

  local started_at finished_at start_ms end_ms duration_ms exit_code
  started_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  start_ms="$(now_ms)"

  set +e
  "${cmd[@]}" >"$stdout_file" 2>"$stderr_file"
  exit_code=$?
  set -e

  end_ms="$(now_ms)"
  finished_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  duration_ms=$((end_ms - start_ms))

  local stdout_bytes
  stdout_bytes="$(wc -c <"$stdout_file")"
  stdout_bytes="${stdout_bytes// /}"

  local summary_json="null"
  if [[ "$target" != "ry_tests" ]]; then
    local summary_line
    summary_line="$(awk '{ gsub(/\033\[[0-9;]*m/, "") } /^[0-9]+ passed, [0-9]+ failed, [0-9]+ skipped, [0-9]+ todo$/ { last=$0 } END { print last }' "$stdout_file")"
    if [[ "$summary_line" =~ ^([0-9]+)\ passed,\ ([0-9]+)\ failed,\ ([0-9]+)\ skipped,\ ([0-9]+)\ todo$ ]]; then
      summary_json="{\"passed\":${BASH_REMATCH[1]},\"failed\":${BASH_REMATCH[2]},\"skipped\":${BASH_REMATCH[3]},\"todo\":${BASH_REMATCH[4]}}"
    fi
  fi

  jq -nc \
    --arg schema_version "$SCHEMA_VERSION" \
    --arg run_id "$RUN_ID" \
    --arg target "$target" \
    --argjson exit_code "$exit_code" \
    --argjson duration_ms "$duration_ms" \
    --arg started_at "$started_at" \
    --arg finished_at "$finished_at" \
    --arg stdout_path "$stdout_path" \
    --arg stderr_path "$stderr_path" \
    --argjson stdout_byte_count "$stdout_bytes" \
    --arg trace_path "$trace_path" \
    --arg gtest_json_path "$gtest_json_path" \
    --argjson summary "$summary_json" \
    --args \
    '{
      record_type: "command",
      schema_version: $schema_version,
      run_id: $run_id,
      target: $target,
      command: $ARGS.positional,
      exit_code: $exit_code,
      duration_ms: $duration_ms,
      started_at: $started_at,
      finished_at: $finished_at,
      stdout_path: $stdout_path,
      stderr_path: $stderr_path,
      stdout_byte_count: $stdout_byte_count,
      trace_path: (if $trace_path == "" then null else $trace_path end),
      gtest_json_path: (if $gtest_json_path == "" then null else $gtest_json_path end),
      summary: $summary
    }' -- "${cmd[@]}" >>"$RUN_JSONL"

  CAPTURED=$((CAPTURED + 1))
}

# ---- Run --------------------------------------------------------------------

if [[ "$TARGET_MODE" == "ry_tests" ]]; then
  run_one "ry_tests" "gtest" "$RY_TESTS_BIN"
else
  for t in "${EXPANDED[@]+"${EXPANDED[@]}"}"; do
    slug="$(printf '%s' "$t" | sed -e 's|/|-|g' -e 's|\.test\.ry$||' -e 's|\.|-|g')"
    run_one "$t" "$slug" "$RY_BIN" "test" "$t"
  done
fi

if (( CAPTURED == 0 )); then
  echo "error: no commands were captured (all targets missing or invalid)" >&2
  exit 1
fi

echo "captured $CAPTURED/$TOTAL_PLANNED command(s) -> $RUN_JSONL" >&2
