#!/usr/bin/env bash
# Reference-integrity lint for prompt / instruction definition files.
#
# Scans .claude/**/*.md + AGENTS.md + CLAUDE.md and fails (exit 1) on:
#   (a) inline-code paths that do not exist on disk (#1827-class path
#       drift, e.g. `src/parser.cpp` after the move to
#       `src/parser/parser.cpp`),
#   (b) `/skill` slash-command references with no matching
#       .claude/skills/<name>/SKILL.md (dead skill links, e.g. a deleted
#       skill still referenced in prose),
#   (c) `## …` KNOWLEDGE.md section references whose heading text is not
#       present verbatim in KNOWLEDGE.md (English/Japanese name drift that
#       makes a grep for the cited section silently miss).
#
# DETECTION IS INLINE-CODE-SPAN ONLY. Only tokens wrapped in single
# backticks are inspected; triple-backtick fenced blocks (``` / ~~~) are
# stripped before scanning, exactly like scripts/check-llvm-emit-abi-header.sh
# strips comments. This is the intentional, documented ESCAPE HATCH: to
# legitimately mention a path / skill / section that does not (yet) exist,
# write it inside a fenced block or as plain prose WITHOUT inline backticks.
# Tokens containing a `<...>` placeholder, a glob (`*` / `?`), or a
# `:line` / `::symbol` suffix are also skipped (see docs-reference-conventions.md
# for the <...> placeholder convention).
#
# Usage:
#   scripts/check-prompt-refs.sh              # default instruction-file scope
#   scripts/check-prompt-refs.sh FILE...      # explicit files (for testing;
#                                             # /dev/stdin reads piped text and
#                                             # exercises check (a))
#
# No `set -e`: greps that legitimately find nothing exit 1 mid-pipeline, so
# correctness is by explicit violation accumulation + a single final exit 1,
# not by errexit.
set -uo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.." || {
  echo "::error::check-prompt-refs: failed to change to repository root" >&2
  exit 1
}

KB="KNOWLEDGE.md"
TAB="$(printf '\t')"

# Slash-commands that are valid even without a .claude/skills/<name>/ dir
# (Claude Code built-ins / plugin skills). Extend if a built-in is cited.
BUILTIN_CMDS="clear help config model fast loop run init review verify simplify schedule security-review code-review compact cost memory resume status doctor login logout ide vim terminal-setup bug pr-comments release-notes add-dir mcp agents hooks export permissions fewer-permission-prompts deep-research skill-creator rule-creator update-config keybindings-help claude-api"
# Backtick `/word` tokens that are filesystem paths, not slash-commands.
NON_SKILL_SLASH="tmp workspace"

# File list: explicit args (testing) or the default instruction-file scope.
# KNOWLEDGE.md is deliberately NOT scanned for (a)/(b): it is a prose-heavy
# knowledge base that discusses path naming (including intentionally
# non-existent counter-example paths). It is used only as the (c) source of
# truth.
if [ "$#" -gt 0 ]; then
  FILES="$(printf '%s\n' "$@")"
else
  FILES="$( { find .claude -name '*.md'; [ -f AGENTS.md ] && echo AGENTS.md; [ -f CLAUDE.md ] && echo CLAUDE.md; } )"
fi

# Strip fenced code blocks (``` / ~~~); keep all other lines verbatim.
strip_fences() {
  awk '
    /^[[:space:]]*```/ { fence = !fence; next }
    /^[[:space:]]*~~~/ { fence = !fence; next }
    !fence            { print }
  ' "$1"
}

readable() { [ -e "$1" ] || [ "$1" = /dev/stdin ]; }

line_of() { grep -nF -- "$2" "$1" 2>/dev/null | head -1 | cut -d: -f1; }

# (a) Emit "<file>\t<token>" for every inline-code path that does not exist.
scan_paths() {
  printf '%s\n' "$FILES" | while IFS= read -r f; do
    [ -n "$f" ] || continue
    readable "$f" || continue
    strip_fences "$f" | grep -oE '`[^`]+`' | tr -d '`' | tr ' \t' '\n\n' | while IFS= read -r tok; do
      tok="${tok#(}"          # leading paren
      tok="${tok#./}"         # leading ./
      tok="${tok%%:*}"        # drop :line / ::symbol suffix
      tok="${tok%[]).,;]}"    # drop one trailing ) ] . , ;
      case "$tok" in
        src/*|include/*|tests/*|share/*|scripts/*|crates/*|editor/*|docker/*|cmake/*|.github/*|.claude/*|docs/*) ;;
        *) continue ;;
      esac
      case "$tok" in
        *'*'*|*'?'*|*'<'*|*'>'*|*'{'*|*'}'*|*'$'*|*'|'*|build/*|build-*) continue ;;
      esac
      [ -e "$tok" ] || printf '%s%s%s\n' "$f" "$TAB" "$tok"
    done
  done | sort -u
}

# (b) Emit "<file>\t<line>\t<cmd>" for every dead `/skill` link.
scan_skills() {
  printf '%s\n' "$FILES" | while IFS= read -r f; do
    [ -n "$f" ] || continue
    readable "$f" || continue
    strip_fences "$f" | grep -oE '`/[a-z][a-z0-9-]+`' | tr -d '`' | while IFS= read -r cmd; do
      name="${cmd#/}"
      [ -f ".claude/skills/$name/SKILL.md" ] && continue
      case " $NON_SKILL_SLASH " in *" $name "*) continue ;; esac
      case " $BUILTIN_CMDS " in *" $name "*) continue ;; esac
      ln="$(line_of "$f" "$cmd")"
      printf '%s%s%s%s%s\n' "$f" "$TAB" "${ln:-?}" "$TAB" "$cmd"
    done
  done | sort -u
}

# (c) Emit "<file>\t<line>\t<heading>" for KNOWLEDGE.md section refs that
# do not resolve to a verbatim heading in KNOWLEDGE.md.
scan_sections() {
  printf '%s\n' "$FILES" | while IFS= read -r f; do
    [ -n "$f" ] || continue
    readable "$f" || continue
    strip_fences "$f" | grep -E 'KNOWLEDGE\.md' | grep -oE '`#{2,6} [^`]+`' | tr -d '`' | while IFS= read -r h; do
      case "$h" in *'...'*|*'<'*|*'>'*) continue ;; esac   # skip placeholder headings
      grep -qxF -- "$h" "$KB" 2>/dev/null && continue
      ln="$(line_of "$f" "$h")"
      printf '%s%s%s%s%s\n' "$f" "$TAB" "${ln:-?}" "$TAB" "$h"
    done
  done | sort -u
}

bad=0

viol_a="$(scan_paths)"
if [ -n "$viol_a" ]; then
  bad=1
  echo "::error::check-prompt-refs (a): inline-code path(s) reference files that do not exist on disk." >&2
  echo "  Fix: correct to the real path, or move it to a fenced block / plain prose / a <...> placeholder if illustrative." >&2
  printf '%s\n' "$viol_a" | while IFS="$TAB" read -r f tok; do
    ln="$(line_of "$f" "$tok")"
    echo "  ${f}:${ln:-?}: ${tok}" >&2
  done
fi

viol_b="$(scan_skills)"
if [ -n "$viol_b" ]; then
  bad=1
  echo "::error::check-prompt-refs (b): /skill reference(s) with no matching .claude/skills/<name>/SKILL.md." >&2
  echo "  Fix: point to an existing skill; or if it is a built-in command / filesystem path, add it to BUILTIN_CMDS / NON_SKILL_SLASH in this script." >&2
  printf '%s\n' "$viol_b" | while IFS="$TAB" read -r f ln cmd; do
    echo "  ${f}:${ln}: ${cmd}" >&2
  done
fi

viol_c="$(scan_sections)"
if [ -n "$viol_c" ]; then
  bad=1
  echo "::error::check-prompt-refs (c): KNOWLEDGE.md section reference(s) whose heading is not present verbatim in ${KB}." >&2
  echo "  Fix: match the exact heading text in ${KB} (watch for English vs Japanese section names)." >&2
  printf '%s\n' "$viol_c" | while IFS="$TAB" read -r f ln h; do
    echo "  ${f}:${ln}: ${h}" >&2
  done
fi

if [ "$bad" -ne 0 ]; then
  echo "check-prompt-refs: FAILED (see ::error:: annotations above)." >&2
  exit 1
fi
echo "check-prompt-refs: clean (paths, /skill links, and KNOWLEDGE.md section names all resolve)."
