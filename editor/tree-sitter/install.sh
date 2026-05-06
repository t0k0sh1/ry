#!/usr/bin/env bash
#
# Neovim の parser / queries ディレクトリに ry.so と queries を配置する。
#
#   ry.so            -> $XDG_CONFIG_HOME/nvim/parser/ry.so
#   highlights.scm   -> $XDG_CONFIG_HOME/nvim/queries/ry/highlights.scm
#   indents.scm      -> $XDG_CONFIG_HOME/nvim/queries/ry/indents.scm
#
# Usage:
#   ./install.sh        # ry.so が無ければ自動で build.sh を呼ぶ
#   ./install.sh --no-build  # ビルドをスキップして既存の ry.so をコピーする

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

NVIM_CONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/nvim"
PARSER_DIR="$NVIM_CONFIG/parser"
QUERIES_DIR="$NVIM_CONFIG/queries/ry"

DO_BUILD=1
for arg in "$@"; do
  case "$arg" in
    --no-build) DO_BUILD=0 ;;
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

if [[ "$DO_BUILD" -eq 1 ]]; then
  echo "==> building ry.so via ./build.sh"
  ./build.sh
fi

if [[ ! -f ry.so ]]; then
  echo "ERROR: ry.so not found. Run ./build.sh first or omit --no-build." >&2
  exit 1
fi

if [[ ! -f queries/highlights.scm ]]; then
  echo "ERROR: queries/highlights.scm not found." >&2
  exit 1
fi

if [[ ! -f queries/indents.scm ]]; then
  echo "ERROR: queries/indents.scm not found." >&2
  exit 1
fi

echo "==> mkdir -p $PARSER_DIR $QUERIES_DIR"
mkdir -p "$PARSER_DIR" "$QUERIES_DIR"

echo "==> install ry.so -> $PARSER_DIR/ry.so"
install -m 0755 ry.so "$PARSER_DIR/ry.so"

echo "==> install queries/highlights.scm -> $QUERIES_DIR/highlights.scm"
install -m 0644 queries/highlights.scm "$QUERIES_DIR/highlights.scm"

echo "==> install queries/indents.scm -> $QUERIES_DIR/indents.scm"
install -m 0644 queries/indents.scm "$QUERIES_DIR/indents.scm"

echo "==> done"
ls -la "$PARSER_DIR/ry.so" "$QUERIES_DIR/highlights.scm" "$QUERIES_DIR/indents.scm"
