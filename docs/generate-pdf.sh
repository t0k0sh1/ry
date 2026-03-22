#!/usr/bin/env bash
# Generate PDF documents from Markdown sources.
# Usage: ./docs/generate-pdf.sh
#
# Prerequisites:
#   brew install pandoc
#   brew install --cask basictex
#   sudo /Library/TeX/texbin/tlmgr install \
#     lualatex-math selnolig luatexja haranoaji fvextra unicode-math upquote

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
export PATH="/Library/TeX/texbin:$PATH"

JA_HEADER=$(mktemp)
ZH_HEADER=$(mktemp)
trap 'rm -f "$JA_HEADER" "$ZH_HEADER"' EXIT

cat > "$JA_HEADER" <<'TEX'
\usepackage{luatexja-fontspec}
\setmainjfont{HaranoAjiMincho}
\setsansjfont{HaranoAjiGothic}
TEX

cat > "$ZH_HEADER" <<'TEX'
\usepackage{luatexja-fontspec}
\setmainjfont{Songti TC}
\setsansjfont{Heiti TC}
TEX

PANDOC_COMMON=(
    --pdf-engine=lualatex
    -V geometry:margin=2.5cm
    -V colorlinks=true
    --toc
    --toc-depth=2
)

generate_pdf() {
    local label="$1"   # e.g. "tutorial-en"
    local header="$2"  # e.g. "" or path to .tex header
    local output="$SCRIPT_DIR/$label.pdf"
    shift 2
    local files=("$@")

    local opts=("${PANDOC_COMMON[@]}")
    if [[ -n "$header" ]]; then
        opts+=(-H "$header")
    fi

    echo "Generating $label.pdf ..."
    pandoc "${files[@]}" -o "$output" "${opts[@]}"
    echo "  -> $output"
}

# --- English ---
generate_pdf "tutorial-en" "$JA_HEADER" "$SCRIPT_DIR"/tutorial/[0-9]*.md
generate_pdf "reference-en" "$JA_HEADER" "$SCRIPT_DIR"/reference/*.md

# --- Japanese ---
generate_pdf "tutorial-ja" "$JA_HEADER" "$SCRIPT_DIR"/ja/tutorial/[0-9]*.md
generate_pdf "reference-ja" "$JA_HEADER" "$SCRIPT_DIR"/ja/reference/*.md

# --- Chinese ---
generate_pdf "tutorial-zh" "$ZH_HEADER" "$SCRIPT_DIR"/zh/tutorial/[0-9]*.md
generate_pdf "reference-zh" "$ZH_HEADER" "$SCRIPT_DIR"/zh/reference/*.md

echo "Done. Generated 6 PDFs in $SCRIPT_DIR/"
