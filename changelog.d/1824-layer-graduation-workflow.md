### Changed

- Added `docs/architecture/layer-graduation-workflow.md` and `docs/architecture/codegen-layering-plan.md` to define when a compiler/runtime layer is graduate-ready (criteria + per-layer document template) and to record the codegen 2-layer split working hypothesis (Ry semantic lowering vs LLVM IR emission, lowered IR vocabulary, bounds-check pilot). `docs/architecture/compiler-layers.md` is updated to forward-reference the planned split. Documentation-only; no behavior change. Preparation for #1949 (LLVM IR emission shared library) and #1950 (Rust reimplementation). (#1824)
