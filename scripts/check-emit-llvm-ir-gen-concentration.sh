#!/usr/bin/env bash
# IR-generation concentration gate for the emit crate (#2109 AC #5 / #7):
# enforce that `llvm_sys::core` IR-gen and direct `LLVMBuild*` calls inside
# crates/emit/src/composite/ are confined to an explicit allowlist.
#
# Why allowlist instead of strict ban: issue #2109 reorganises the emit crate
# into composite / primitive / context, with the long-term goal that composite
# express each op as a sequence of `primitive::*` method calls (so all
# `LLVMBuild*` lives in the primitive layer). The current refactor is a verbatim
# move that preserves byte-exact LLVM IR — composite modules continue to call
# `LLVMBuild*` directly because rewriting them as primitive method sequences
# would risk the IR-byte-exact guarantee. Issue #2109 explicitly anticipates
# this carve-out:
#
#   全変換が単一 PR の合理的範囲を超える場合でも、残存箇所と許可理由を明示し、
#   新規追加を防止する gate を設ける。
#
# This gate freezes the current set of composite files that still call
# `llvm_sys::core` and blocks new ones. A follow-on issue migrates each file
# off the allowlist by replacing direct `LLVMBuild*` with primitive method calls
# (the AC #5 "primitive API の組み合わせとして表現する" goal).
#
# Forbidden in crates/emit/src/composite/<file>.rs where <file> is NOT in
# ALLOWLIST below:
#   - `use llvm_sys::core`     (the IR-generation API path)
#   - `LLVMBuild[A-Z]`         (any IR builder call)
#   - `LLVMAppendBasicBlock*`  (basic-block creation)
#   - `LLVMPositionBuilder*`   (builder positioning)
#   - `LLVMAddIncoming`        (PHI plumbing)
#   - `LLVMAddCase`            (switch plumbing)
#
# Allowed:
#   - `use llvm_sys::prelude`  (type-cast handles only, emits no IR)
#
# Comment stripping mirrors scripts/check-emit-abi-no-ir.sh.
#
# Adding a NEW composite file: prefer expressing its op via primitive methods
# (call `self.build_load(...)` instead of `LLVMBuildLoad2(...)`). If a verbatim
# move is required because a follow-on migration is planned, add the file to
# ALLOWLIST below WITH an inline comment naming the tracking issue.
#
# Exit code:
#   0  clean (no LLVM IR-gen outside the allowlist)
#   1  unallowed `llvm_sys::core` / `LLVMBuild*` usage found

set -euo pipefail

COMP_DIR="${1:-crates/emit/src/composite}"

# Allowlist: composite files permitted to call `llvm_sys::core` / `LLVMBuild*`
# directly, with the issue under which the carve-out was recorded. Migration
# off the allowlist must be done file-by-file, preserving byte-exact IR.
ALLOWLIST=(
    "any.rs"        # #2109 verbatim move — migrate to primitive methods in a follow-on
    "arc.rs"        # #2109 verbatim move — migrate to primitive methods in a follow-on
    "bounds.rs"     # #2109 verbatim move — migrate to primitive methods in a follow-on
    "cast.rs"       # #2109 verbatim move — migrate to primitive methods in a follow-on
    "collection.rs" # #2109 verbatim move — migrate to primitive methods in a follow-on
    "cow.rs"        # #2109 verbatim move — migrate to primitive methods in a follow-on
    "header.rs"     # #2109 — header struct construction stays here (Ry layout knowledge)
    "option.rs"     # #2109 verbatim move — migrate to primitive methods in a follow-on
    "reduce.rs"     # #2109 verbatim move — migrate to primitive methods in a follow-on
    "result.rs"     # #2109 verbatim move; also #2069 re-entrant free fn discipline
)

if [[ ! -d "$COMP_DIR" ]]; then
    echo "check-emit-llvm-ir-gen-concentration: composite dir not found: $COMP_DIR" >&2
    exit 1
fi

# Membership check via case (portable to macOS bash 3.x, which lacks `declare
# -A`). Trims the per-name trailing comment in the case pattern automatically
# because we match on basename only.
is_allowed() {
    local base="$1"
    local name
    for name in "${ALLOWLIST[@]}"; do
        if [[ "$name" == "$base" ]]; then
            return 0
        fi
    done
    return 1
}

strip_comments() {
    awk 'BEGIN { in_block = 0 }
         {
            line = $0
            gsub(/\/\*[^*]*\*\//, "", line)
            if (in_block) {
                idx = index(line, "*/")
                if (idx == 0) next
                line = substr(line, idx + 2)
                in_block = 0
            }
            idx = index(line, "/*")
            if (idx > 0) {
                line = substr(line, 1, idx - 1)
                in_block = 1
            }
            sub(/\/\/.*/, "", line)
            print line
         }' "$1"
}

# Same FORBIDDEN pattern as check-emit-abi-no-ir.sh — LLVMBuild[A-Z] matches IR
# builders but not the LLVMBuilderRef handle type. The trailing alternative
# catches `use llvm_sys::{core, prelude};` grouped imports that the literal
# `llvm_sys::core` substring misses.
FORBIDDEN='LLVMBuild[A-Z]|LLVMAppendBasicBlock|LLVMPositionBuilder|LLVMAddIncoming|LLVMAddCase|LLVMGetInsertBlock|LLVMGetBasicBlockParent|llvm_sys::core|use[[:space:]]+llvm_sys::\{[^}]*core([[:space:]:,}]|$)'

bad=0
while IFS= read -r f; do
    base="$(basename "$f")"
    [[ "$base" == "mod.rs" ]] && continue
    if is_allowed "$base"; then
        continue
    fi
    stripped="$(strip_comments "$f")"
    if printf '%s\n' "$stripped" | grep -nE "$FORBIDDEN" >&2; then
        echo "::error::check-emit-llvm-ir-gen-concentration: $f calls llvm_sys::core / LLVMBuild* directly but is not in ALLOWLIST" >&2
        bad=1
    fi
done < <(find "$COMP_DIR" -name '*.rs' | sort)

if [[ $bad -ne 0 ]]; then
    echo "check-emit-llvm-ir-gen-concentration: AC #5/#7 violated." >&2
    echo "  New composite modules must express their op as a sequence of primitive method calls" >&2
    echo "  (e.g. self.build_load(...) instead of LLVMBuildLoad2(...))." >&2
    echo "  If a verbatim move with a planned follow-on migration is required, add the file" >&2
    echo "  to ALLOWLIST in $(basename "$0") WITH an inline tracking-issue comment." >&2
    echo "  See docs/architecture/codegen-layering-plan.md § 'Composite and primitive emission sub-layers'." >&2
    exit 1
fi

echo "check-emit-llvm-ir-gen-concentration: composite layer clean (LLVM C API direct calls confined to ALLOWLIST)."
