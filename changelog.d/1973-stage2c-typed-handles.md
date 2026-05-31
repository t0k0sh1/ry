### Changed

- Completed Stage 2-C of the codegen layering plan: the LLVM IR emission
  ABI (`include/ry/llvm_emit/api.h`) no longer exposes any LLVM-owned
  types or transitional `void *` parameters in its public signatures.
  Category 1 (LLVM context handles: `RyModuleHandle` / `RyBuilderHandle`
  / `RyContextHandle` / `RyFunctionHandle`) and category 2 (primitive
  type accessors: `RyTypeRef` / `RyFuncTypeRef`) cross the ABI as
  opaque pointer typedefs. The 22 existing ABI entries (Stage 2-A / 2-B
  helpers + OptionWrap / ARC / RuntimeCall / CollectionMutate /
  CowEnsureUnique / AnyWrap / AnyUnwrap / AnyTryUnwrap) had their
  `void *` type parameters swept to typed handles. ControlFlow
  primitive ops (`ry_emit_create_basic_block` / `ry_emit_branch_cond` /
  `ry_emit_branch_uncond` / `ry_emit_create_phi`) cross the ABI; every
  `IRBuilder<>::Create{CondBr,Br,PHI}` / `BasicBlock::Create` call in
  `src/codegen_*.cpp` now goes through the `CodeGen::createBB` /
  `emitBranchCond` / `emitBranchUncond` / `createPhi` wrappers. A
  header-level lint script (`scripts/check-llvm-emit-abi-header.sh`,
  wired into the `lint` CI job) enforces the AC by failing on any
  `llvm::*` or non-carve-out `void *` token in the ABI surface.
  Primitive arithmetic / lexical scope / module-level symbol
  declarations remain outside the ABI per
  `docs/architecture/codegen-layering-plan.md` §"Explicit non-inclusion"
  — those are not part of #1973's AC. (#1973)
