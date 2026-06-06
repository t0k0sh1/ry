#!/usr/bin/env bash
# Header-level lint for include/ry/llvm_emit/api.h
#
# Enforces #1973 Stage 2-C acceptance criterion: the public ABI of the
# emission shared library (libemit) must not expose any LLVM-owned
# types or transitional `void *` parameters in its function signatures.
# Opaque pointer typedefs (RyValueRef / RyTypeRef / RyModuleHandle / ...) are
# the contract; `RyEmitCtx` is the per-compile() context struct.
#
# This script greps the header for forbidden tokens. Comments / doc lines
# starting with `//` may legitimately reference `llvm::*` / `void *` as
# documentation prose, so we strip comment-only lines before matching.
#
# Carve-outs (allowed `void *` slots):
# - `RyBuildValueFn` (callback type): `void *user_ctx`
# - `ry_emit_result_branch`: `void *user_ctx` (forwarded to the build_ok /
#   build_err callbacks; opaque user data, not an LLVM type).
#
# Exit code:
#   0  clean (or only allowed carve-outs)
#   1  forbidden token found in a function signature line

set -euo pipefail

HEADER="${1:-include/ry/llvm_emit/api.h}"

if [[ ! -f "$HEADER" ]]; then
    echo "check-llvm-emit-abi-header: file not found: $HEADER" >&2
    exit 1
fi

# Strip C/C++ comments (single-line and the simple // form is enough — the
# header uses // exclusively in its prose comments).
non_comment="$(
    awk 'BEGIN { in_block = 0 }
         {
            line = $0
            # Strip /* ... */ block comments on a single line.
            gsub(/\/\*[^*]*\*\//, "", line)
            # Block comment start without end → mark and strip rest.
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
            # Strip // line comments.
            sub(/\/\/.*/, "", line)
            print line
         }' "$HEADER"
)"

bad=0

# Check for LLVM types in non-comment lines.
if echo "$non_comment" | grep -nE '\bllvm::' >&2; then
    echo "check-llvm-emit-abi-header: LLVM types must NOT appear in $HEADER" >&2
    echo "  Use opaque typedefs (RyValueRef / RyTypeRef / RyModuleHandle / ...)." >&2
    bad=1
fi

# Check for `void *` outside the user_ctx carve-outs.
# Carve-outs are matched first and removed before the forbidden check.
# The regex tolerates `void*` / `void *` / `void  *` / `const void *` / etc.
# so the lint doesn't get bypassed by whitespace or const-qualifier variants.
# NB: `\b` is not portable in BSD/macOS sed -E; use surrounding context
# (start-of-pattern via non-word chars, `user_ctx` is followed by a
# non-word boundary anyway) to anchor.
filtered="$(
    echo "$non_comment" |
    sed -E \
        -e 's/(const[[:space:]]+)?void[[:space:]]*\*[[:space:]]*user_ctx//g'
)"

if echo "$filtered" | grep -E '(const[[:space:]]+)?void[[:space:]]*\*+' >&2; then
    echo "check-llvm-emit-abi-header: transitional 'void *' must NOT appear in $HEADER" >&2
    echo "  Allowed carve-outs: 'void *user_ctx' in RyBuildValueFn / ry_emit_result_branch." >&2
    echo "  All other transitional 'void *' parameters from Stage 2-A / 2-B must be migrated to typed handles." >&2
    bad=1
fi

if [[ $bad -ne 0 ]]; then
    exit 1
fi

echo "check-llvm-emit-abi-header: $HEADER is clean (LLVM types: 0; transitional void *: 0)."
