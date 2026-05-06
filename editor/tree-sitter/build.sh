#!/usr/bin/env bash
#
# tree-sitter-ry を grammar.js から再生成し、共有ライブラリ ry.so を生成する。
#
# Usage:
#   ./build.sh             # generate + build (ry.so)
#   ./build.sh --no-gen    # build のみ (scanner.c など C 側だけ変更したとき)
#   ./build.sh --debug     # debug ビルド (-0 / -O0 -g 相当)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# tree-sitter build は内部で `cc` を呼ぶ (cc-rs)。macOS の `cc` / `gcc` は
# どちらも Apple clang のラッパーなので、GNU gcc を使うために Homebrew の
# バージョン付き gcc を CC に明示する。CC が既に設定されていれば尊重する。
if [[ -z "${CC:-}" ]]; then
  for v in 15 14 13 12; do
    if command -v "gcc-$v" >/dev/null 2>&1; then
      export CC="gcc-$v"
      break
    fi
  done
  if [[ -z "${CC:-}" ]]; then
    echo "ERROR: GNU gcc not found (looked for gcc-15..gcc-12). Run 'brew install gcc'." >&2
    exit 1
  fi
fi

DO_GEN=1
DEBUG=0

for arg in "$@"; do
  case "$arg" in
    --no-gen) DO_GEN=0 ;;
    --debug)  DEBUG=1 ;;
    -h|--help)
      sed -n '3,9p' "$0"
      exit 0
      ;;
    *)
      echo "unknown option: $arg" >&2
      exit 2
      ;;
  esac
done

if [[ "$DO_GEN" -eq 1 ]]; then
  echo "==> tree-sitter generate"
  tree-sitter generate
fi

echo "==> tree-sitter build -o ry.so (CC=$CC)"
if [[ "$DEBUG" -eq 1 ]]; then
  tree-sitter build --debug -o ry.so
else
  tree-sitter build -o ry.so
fi

echo "==> done: $(ls -la ry.so | awk '{print $5, $NF}')"
