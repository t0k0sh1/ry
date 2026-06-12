#!/usr/bin/env bash
# IR-generation lint for the abi boundary layer (crates/emit/src/abi.rs + abi/**).
#
# Enforces the #2069 acceptance criterion: the abi shell must contain ZERO LLVM
# IR generation. It is a pure mechanical-translation boundary — resolve u32 ids,
# wrap opaque type handles, build callback closures, intern results — over the
# `core` engine, which owns all IR emission (LLVMBuild* / basic-block creation /
# builder positioning / phi incoming). Keeping the abi layer uniform and logic-
# free lets it stay deletable as the migrated-op surface grows (#2023), so a
# future pure-Rust library (abi removal) remains a mechanical sweep rather than a
# logic port. Same enforcement culture as scripts/check-llvm-emit-abi-header.sh
# (no LLVM types in the public C signatures) and the boundary layout asserts.
#
# Forbidden in crates/emit/src/abi.rs and crates/emit/src/abi/**.rs:
#   - LLVMBuild<Upper>     IR builder calls (LLVMBuildCondBr / LLVMBuildPhi / ...).
#                          The LLVMBuilderRef *type* is allowed: its 10th char is
#                          a lowercase 'e', so LLVMBuild[A-Z] does not match it.
#   - LLVMAppendBasicBlock* / LLVMPositionBuilder* / LLVMAddIncoming / LLVMAddCase
#   - LLVMGetInsertBlock / LLVMGetBasicBlockParent   (BB navigation for IR shaping)
#   - llvm_sys::core       the IR-generation API path. Matched as a substring so it
#                          catches both `use llvm_sys::core::*;` and a fully-
#                          qualified `llvm_sys::core::LLVMxxx(...)` call. Without
#                          this import none of the LLVMBuild* symbols can be named.
#
# Allowed:
#   - use llvm_sys::prelude   type-cast handles in abi.rs / abi/lifecycle.rs
#                             (RyTypeRef -> LLVMTypeRef etc.); emits no IR.
#
# Comment lines (`//`, including `//!` / `///` doc comments, and `/* */` blocks)
# are stripped before matching so the gate never trips on its own explanatory
# prose (mirrors check-llvm-emit-abi-header.sh's comment stripping).
#
# Exit code:
#   0  clean (the abi layer emits no IR)
#   1  forbidden IR-generation symbol or llvm_sys::core path found

set -euo pipefail

ABI_ROOT="${1:-crates/emit/src/abi.rs}"
ABI_DIR="${2:-crates/emit/src/abi}"

# Collect the abi-layer Rust sources: the module root + every submodule.
files=()
[[ -f "$ABI_ROOT" ]] && files+=("$ABI_ROOT")
if [[ -d "$ABI_DIR" ]]; then
    while IFS= read -r f; do files+=("$f"); done < <(find "$ABI_DIR" -name '*.rs' | sort)
fi

if [[ ${#files[@]} -eq 0 ]]; then
    echo "check-emit-abi-no-ir: no abi sources found (looked for $ABI_ROOT and $ABI_DIR/**.rs)" >&2
    exit 1
fi

# LLVMBuild[A-Z] matches the IR builders but NOT the LLVMBuilderRef type handle.
FORBIDDEN='LLVMBuild[A-Z]|LLVMAppendBasicBlock|LLVMPositionBuilder|LLVMAddIncoming|LLVMAddCase|LLVMGetInsertBlock|LLVMGetBasicBlockParent|llvm_sys::core'

bad=0
for f in "${files[@]}"; do
    # Strip // line comments (covers //! and /// doc comments) and /* */ blocks,
    # so prose that names an IR symbol for documentation does not trip the gate.
    stripped="$(
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
             }' "$f"
    )"
    if echo "$stripped" | grep -nE "$FORBIDDEN" >&2; then
        echo "::error::check-emit-abi-no-ir: IR-generation symbol found in $f" >&2
        bad=1
    fi
done

if [[ $bad -ne 0 ]]; then
    echo "check-emit-abi-no-ir: the abi boundary layer must emit ZERO LLVM IR (#2069)." >&2
    echo "  Move IR generation (LLVMBuild* / basic-block ops / phi) into a core engine" >&2
    echo "  method or free function; the abi shell should only resolve ids, wrap handles," >&2
    echo "  build closures, and intern results." >&2
    echo "  Allowed: 'use llvm_sys::prelude' for type-cast handles (no IR emission)." >&2
    exit 1
fi

echo "check-emit-abi-no-ir: abi layer is clean (IR-generation symbols: 0; llvm_sys::core paths: 0)."
