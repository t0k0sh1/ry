#pragma once

#include <llvm/ADT/ArrayRef.h>

namespace llvm {
class Type;
class Value;
}

namespace ry {
class CodeGen;
}

namespace ry::codegen::lowered {

// Pre-lowered runtime-call op. Carries the runtime symbol name plus the
// structured signature (return type + parameter types) and the actual
// argument values. Mirrors the BoundsCheck / ResultBranch / OptionWrap /
// ARC family in shape — emission resolves it to a single
// `mod_->getOrInsertFunction(name, fnTy) + builder_.CreateCall(callee, args)`
// pair via the libemit boundary.
//
// The op intentionally keeps `ret_ty` / `arg_tys` as bare `llvm::Type *`
// transitional handles (category 2 of the llvm-ir-emission-boundary roadmap
// has not crossed the boundary yet; #1973 will replace them with typed opaque
// handles). The `args` ArrayRef is consumed at emission time; lowering does
// not copy it, so the caller must keep the underlying storage live.
struct RuntimeCallOp {
    const char *name;
    llvm::Type *ret_ty;
    llvm::ArrayRef<llvm::Type *> arg_tys;
    llvm::ArrayRef<llvm::Value *> args;
};

} // namespace ry::codegen::lowered

namespace ry::codegen::lowering {

// Passthrough lowering — no IRBuilder usage, just struct construction.
// Mirrors lowerOptionWrap (#1967) / lowerArcRetain / lowerArcRelease (#1968)
// in shape. No constant-fold path: runtime calls are by definition runtime,
// so there is no compile-time collapse opportunity.
lowered::RuntimeCallOp lowerRuntimeCall(CodeGen &cg, const char *name,
                                        llvm::Type *ret_ty,
                                        llvm::ArrayRef<llvm::Type *> arg_tys,
                                        llvm::ArrayRef<llvm::Value *> args);

} // namespace ry::codegen::lowering

namespace ry::codegen::emission {

// Emit a runtime call via the libemit boundary (ry_emit_runtime_call).
// The boundary resolves the symbol through `mod_->getOrInsertFunction(name, fnTy)`
// and emits a `CreateCall` against it; the returned llvm::Value is the call
// instruction (suitable as a value handle even for void-returning calls —
// callers that care should check `op.ret_ty->isVoidTy()` themselves).
//
// `name_hint` is the LLVM SSA name hint for the call result (e.g. "tmp" or
// the desired result variable name); pass nullptr for no hint. Matches the
// third positional argument of `IRBuilder<>::CreateCall(callee, args, name)`.
//
// No `ry_emit_ctx_set_function` call: this op creates no basic blocks
// (CreateCall is a single instruction that stays in the current block), so
// the currently-active LLVM function pointer is not consulted.
llvm::Value *emitRuntimeCall(CodeGen &cg, const lowered::RuntimeCallOp &op,
                             const char *name_hint);

} // namespace ry::codegen::emission
