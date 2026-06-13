#!/usr/bin/env bash
# Layering gate for the emit crate (#2109): enforce that the primitive layer
# (crates/emit/src/primitive/**) and the context layer (crates/emit/src/context.rs)
# do NOT import from `crate::composite`. The dependency direction is
# `abi → composite → primitive → context`; the reverse edges are forbidden.
#
# Why import-path enforcement: composite and primitive both impl methods on the
# same `EmitCtx`, so `self.composite_method()` from a primitive `impl EmitCtx`
# block is silently permitted by the type system — the gate cannot catch that
# form. What it CAN catch is an explicit `use crate::composite::*` import path,
# which is the most likely accidental edge a refactor would introduce. The
# future physical crate split (#2023) turns this into a Cargo-enforced
# constraint; until then, this gate covers the practical risk.
#
# Forbidden in crates/emit/src/primitive/**.rs and crates/emit/src/context.rs:
#   - `use crate::composite`   (any submodule path)
# Additionally in crates/emit/src/context.rs:
#   - `use crate::abi`         (context must not depend on the C shell)
#   - `use crate::primitive`   (context sits below every sibling layer)
#
# Allowed:
#   - primitive/**: `use crate::context`, `use crate::primitive::*` siblings,
#                   `use llvm_sys::core`, `use llvm_sys::prelude`.
#   - context.rs:   `use llvm_sys::prelude` only (the IR-free carve-out
#                   mirroring scripts/check-emit-abi-no-ir.sh).
#
# Comment lines (`//`, `//!`, `///`, and `/* */` blocks) are stripped before
# matching so prose that names a forbidden path for documentation does not trip
# the gate (mirrors scripts/check-emit-abi-no-ir.sh's comment stripping).
#
# Exit code:
#   0  clean (no forbidden import paths found)
#   1  forbidden import path found

set -euo pipefail

PRIM_DIR="${1:-crates/emit/src/primitive}"
CTX_FILE="${2:-crates/emit/src/context.rs}"

# Collect target sources.
files=()
if [[ -d "$PRIM_DIR" ]]; then
    while IFS= read -r f; do files+=("$f"); done < <(find "$PRIM_DIR" -name '*.rs' | sort)
fi
[[ -f "$CTX_FILE" ]] && files+=("$CTX_FILE")

if [[ ${#files[@]} -eq 0 ]]; then
    echo "check-emit-composite-no-primitive: no target sources found (looked for $PRIM_DIR/**.rs and $CTX_FILE)" >&2
    exit 1
fi

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

bad=0
for f in "${files[@]}"; do
    stripped="$(strip_comments "$f")"
    # primitive/** and context.rs: forbid composite import. The second arm of
    # each regex catches a path nested inside a brace-group import
    # (`use crate::{composite::..., context::...};`) so the grouped form
    # cannot bypass the gate.
    if printf '%s\n' "$stripped" | grep -nE 'use[[:space:]]+crate::composite|use[[:space:]]+crate::\{[^}]*composite([[:space:]:,}]|$)' >&2; then
        echo "::error::check-emit-composite-no-primitive: forbidden 'use crate::composite' in $f" >&2
        bad=1
    fi
    # context.rs gets two additional bans.
    if [[ "$f" == "$CTX_FILE" ]]; then
        if printf '%s\n' "$stripped" | grep -nE 'use[[:space:]]+crate::(abi|primitive)|use[[:space:]]+crate::\{[^}]*(abi|primitive)([[:space:]:,}]|$)' >&2; then
            echo "::error::check-emit-composite-no-primitive: forbidden 'use crate::{abi|primitive}' in $f (context must not depend on sibling layers)" >&2
            bad=1
        fi
    fi
done

if [[ $bad -ne 0 ]]; then
    echo "check-emit-composite-no-primitive: layering invariants violated." >&2
    echo "  primitive/** and context.rs must not import from crate::composite." >&2
    echo "  context.rs additionally must not import crate::abi or crate::primitive." >&2
    echo "  See docs/architecture/codegen-layering-plan.md § 'Composite and primitive emission sub-layers'." >&2
    exit 1
fi

echo "check-emit-composite-no-primitive: layering directions clean (primitive ⇏ composite; context ⇏ {abi, primitive, composite})."
