#include "ry/codegen/lowered_result_branch.hpp"
#include "ry/codegen.hpp"
#include "ry/llvm_emit/api.h"

#include <llvm/IR/Value.h>

namespace ry::codegen::emission {

namespace {

// Trampoline glue between the C++-side `llvm::function_ref<>` shape used by
// callers (e.g. wrapPtrAsResult) and the C-side function-pointer + user_ctx
// shape consumed by ry_emit_result_branch. The ABI keeps both ok and err
// builders self-contained; this struct simply lets one trampoline invocation
// look up which closure to call.
struct ResultBranchTrampolineCtx {
    llvm::function_ref<llvm::Value *()> build_ok;
    llvm::function_ref<llvm::Value *()> build_err;
    RyEmitCtx *emit_ctx;
};

RyValueId trampolineOk(void *user) {
    auto *t = static_cast<ResultBranchTrampolineCtx *>(user);
    llvm::Value *v = t->build_ok();
    return ry_emit_intern(t->emit_ctx, v);
}

RyValueId trampolineErr(void *user) {
    auto *t = static_cast<ResultBranchTrampolineCtx *>(user);
    llvm::Value *v = t->build_err();
    return ry_emit_intern(t->emit_ctx, v);
}

} // namespace

llvm::Value *emitResultBranch(CodeGen &cg, const lowered::ResultBranchOp &op,
                              llvm::function_ref<llvm::Value *()> build_ok,
                              llvm::function_ref<llvm::Value *()> build_err) {
    ry_emit_ctx_set_function(cg.emit_ctx_, cg.fn_);

    ResultBranchTrampolineCtx tctx{build_ok, build_err, cg.emit_ctx_};
    RyValueId isErrId = ry_emit_intern(cg.emit_ctx_, op.is_err);
    RyValueId resultId = ry_emit_result_branch(
        cg.emit_ctx_, isErrId, op.res_ty, &trampolineOk, &trampolineErr, &tctx);
    return static_cast<llvm::Value *>(ry_emit_resolve(cg.emit_ctx_, resultId));
}

} // namespace ry::codegen::emission
