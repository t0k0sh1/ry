#!/usr/bin/env bash
#
# tests/spec/**/*.test.ry を tree-sitter ry.so に通して grammar の regression を検出する。
# expected-fail.txt に列挙されているファイルは grammar 未対応として除外する。
#
#   - 非 expected-fail で ERROR/MISSING ノードが出る → FAIL: <path> 表示 + exit 1
#   - expected-fail なのに parse 通る → WARN: <path> 表示 (exit は 0、エントリ削除を促す)
#
# Usage:
#   ./check.sh             # ry.so が無ければ自動で build.sh を呼ぶ
#   ./check.sh --no-build  # ビルドをスキップして既存の ry.so で実行
#   ./check.sh --verbose   # 期待通り fail している (SKIP) ファイルも表示する

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

REPO_ROOT="$(cd ../.. && pwd)"
SPEC_DIR="$REPO_ROOT/tests/spec"
EXPECTED_FAIL="$SCRIPT_DIR/expected-fail.txt"

DO_BUILD=1
VERBOSE=0
for arg in "$@"; do
  case "$arg" in
    --no-build)   DO_BUILD=0 ;;
    -v|--verbose) VERBOSE=1 ;;
    -h|--help)
      sed -n '3,12p' "$0"
      exit 0
      ;;
    *)
      echo "unknown option: $arg" >&2
      exit 2
      ;;
  esac
done

if [[ "$DO_BUILD" -eq 1 && ! -f ry.so ]]; then
  echo "==> ry.so not found; building via ./build.sh"
  ./build.sh
fi

if [[ ! -f ry.so ]]; then
  echo "ERROR: ry.so not found. Run ./build.sh first or omit --no-build." >&2
  exit 1
fi

if [[ ! -d "$SPEC_DIR" ]]; then
  echo "ERROR: $SPEC_DIR not found" >&2
  exit 1
fi

# expected-fail.txt を pipe-delimited 文字列に正規化する (bash 3.2 互換のため
# associative array を使わない)。tests/spec/ の path は '|' を含まないので衝突しない。
EXPECTED_FAIL_STR="|"
if [[ -f "$EXPECTED_FAIL" ]]; then
  while IFS= read -r line || [[ -n "$line" ]]; do
    [[ "$line" =~ ^[[:space:]]*(#|$) ]] && continue
    raw="${line%%#*}"
    # awk で先頭/末尾の whitespace を除去 (bash 3.2 互換)
    path="$(printf '%s' "$raw" | awk '{$1=$1; print}')"
    [[ -z "$path" ]] && continue
    EXPECTED_FAIL_STR="${EXPECTED_FAIL_STR}${path}|"
  done < "$EXPECTED_FAIL"
fi

test_files=()
while IFS= read -r f; do
  [[ -z "$f" ]] && continue
  test_files+=("$f")
done < <(find "$SPEC_DIR" -type f -name '*.test.ry' | sort)

if (( ${#test_files[@]} == 0 )); then
  echo "ERROR: no *.test.ry files found under $SPEC_DIR" >&2
  exit 1
fi

echo "==> checking ${#test_files[@]} files"

fail_paths=()
warn_paths=()
total_pass=0
total_skip=0

for f in "${test_files[@]}"; do
  rel="${f#$REPO_ROOT/}"

  output="$(tree-sitter parse "$f" 2>&1 || true)"
  has_error=0
  if printf '%s' "$output" | grep -qE 'ERROR|MISSING'; then
    has_error=1
  fi

  in_xfail=0
  [[ "$EXPECTED_FAIL_STR" == *"|$rel|"* ]] && in_xfail=1

  if (( in_xfail == 1 && has_error == 1 )); then
    total_skip=$((total_skip + 1))
    [[ "$VERBOSE" -eq 1 ]] && echo "SKIP: $rel"
  elif (( in_xfail == 1 && has_error == 0 )); then
    warn_paths+=("$rel")
  elif (( in_xfail == 0 && has_error == 1 )); then
    fail_paths+=("$rel")
  else
    total_pass=$((total_pass + 1))
  fi
done

echo "==> result: pass=$total_pass skip=$total_skip warn=${#warn_paths[@]} fail=${#fail_paths[@]}"

if (( ${#warn_paths[@]} > 0 )); then
  echo "WARN: the following files are in expected-fail.txt but now parse cleanly."
  echo "      Remove them from expected-fail.txt:"
  for p in "${warn_paths[@]}"; do
    echo "  - $p"
  done
fi

if (( ${#fail_paths[@]} > 0 )); then
  echo "FAIL: tree-sitter grammar regression detected for files NOT in expected-fail.txt:" >&2
  for p in "${fail_paths[@]}"; do
    echo "  - $p" >&2
  done
  echo "" >&2
  echo "If these failures are intentional grammar gaps, add the paths to:" >&2
  echo "  $EXPECTED_FAIL" >&2
  echo "Otherwise fix grammar.js / src/scanner.c and rebuild via ./build.sh." >&2
  exit 1
fi

echo "==> done"
