// Implementation TU for the LLVM IR emission shared library.
//
// Built as `ry_llvm_emit` (SHARED). At runtime the symbols here are reached
// from the main `ry` / `ry_tests` binary either by direct link (so the JIT
// host process resolves them) or by `-undefined dynamic_lookup` / `-rdynamic`
// like the existing native stdlib libs. This TU is the only consumer of
// `<ry/llvm_emit/api.h>` and the only place where the ABI handle types are
// resolved to concrete LLVM objects.
//
// Stage 2-A note: module / builder / context / function pointers cross the
// ABI as `void*` for now (see api.h for the migration roadmap). The cast
// helpers below centralize the reinterpretation so a single later edit can
// replace them once categories 1/2 cross the ABI.

#include "ry/llvm_emit/api.h"

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

#include <string>
#include <vector>

namespace {

llvm::Module *asModule(void *p) { return static_cast<llvm::Module *>(p); }
llvm::IRBuilder<> *asBuilder(void *p) { return static_cast<llvm::IRBuilder<> *>(p); }
llvm::LLVMContext *asContext(void *p) { return static_cast<llvm::LLVMContext *>(p); }
llvm::Function *asFunction(void *p) { return static_cast<llvm::Function *>(p); }

} // namespace

struct RyEmitCtx {
    llvm::Module *module;
    llvm::IRBuilder<> *builder;
    llvm::LLVMContext *context;
    llvm::Function *function;
    std::vector<llvm::Value *> values;
    RyEmitCallbacks cbs;
};

extern "C" {

RyEmitCtx *ry_emit_ctx_create(void *module_ptr, void *builder_ptr,
                              void *context_ptr, void *function_ptr) {
    auto *ctx = new RyEmitCtx{};
    ctx->module = asModule(module_ptr);
    ctx->builder = asBuilder(builder_ptr);
    ctx->context = asContext(context_ptr);
    ctx->function = asFunction(function_ptr);
    // Reserve handle 0 as the "invalid" sentinel; ry_emit_resolve(_, 0) returns NULL.
    ctx->values.push_back(nullptr);
    ctx->cbs = RyEmitCallbacks{};
    return ctx;
}

void ry_emit_ctx_destroy(RyEmitCtx *ctx) { delete ctx; }

void ry_emit_ctx_set_function(RyEmitCtx *ctx, void *function_ptr) {
    ctx->function = asFunction(function_ptr);
}

void ry_emit_ctx_set_callbacks(RyEmitCtx *ctx, const RyEmitCallbacks *cbs) {
    if (cbs)
        ctx->cbs = *cbs;
    else
        ctx->cbs = RyEmitCallbacks{};
}

RyValueId ry_emit_intern(RyEmitCtx *ctx, void *value_ptr) {
    if (value_ptr == nullptr)
        return 0;
    auto id = static_cast<RyValueId>(ctx->values.size());
    ctx->values.push_back(static_cast<llvm::Value *>(value_ptr));
    return id;
}

void *ry_emit_resolve(RyEmitCtx *ctx, RyValueId id) {
    if (id == 0 || id >= ctx->values.size())
        return nullptr;
    return ctx->values[id];
}

RyValueId ry_emit_build_error_from_runtime(RyEmitCtx *ctx, const char *err_fn_name,
                                           void *error_ty_ptr) {
    auto *errorTy = static_cast<llvm::StructType *>(error_ty_ptr);
    auto *ptrTy = llvm::PointerType::getUnqual(*ctx->context);
    auto *i64Ty = llvm::Type::getInt64Ty(*ctx->context);
    auto *errFnTy = llvm::FunctionType::get(ptrTy, {}, false);
    auto errFn = ctx->module->getOrInsertFunction(err_fn_name, errFnTy);
    llvm::Value *errMsg = ctx->builder->CreateCall(errFn, {}, "err_msg");
    llvm::Value *errStruct = llvm::UndefValue::get(errorTy);
    errStruct = ctx->builder->CreateInsertValue(errStruct, errMsg, 0, "err.msg");
    errStruct = ctx->builder->CreateInsertValue(
        errStruct, llvm::ConstantInt::get(i64Ty, 0), 1, "err.code");
    return ry_emit_intern(ctx, errStruct);
}

void *ry_emit_get_runtime_fn(RyEmitCtx *ctx, const char *name, void *fn_ty_ptr) {
    auto *fnTy = static_cast<llvm::FunctionType *>(fn_ty_ptr);
    auto callee = ctx->module->getOrInsertFunction(name, fnTy);
    return callee.getCallee();
}

RyValueId ry_emit_bounds_check(RyEmitCtx *ctx, RyValueId idx_id, RyValueId len_id,
                               RyBoundsKind kind, const char *global_name,
                               const char *bb_prefix) {
    (void)global_name; // Forwarded via callback below; kept as a parameter to
                       // pin the ABI shape ahead of cache-keyed lookups landing
                       // on the LLVM side in a successor PR.
    auto *idx = static_cast<llvm::Value *>(ry_emit_resolve(ctx, idx_id));
    auto *len = static_cast<llvm::Value *>(ry_emit_resolve(ctx, len_id));
    auto *i1Ty = llvm::Type::getInt1Ty(*ctx->context);
    auto *i64Ty = llvm::Type::getInt64Ty(*ctx->context);
    if (idx->getType() == i1Ty)
        idx = ctx->builder->CreateZExt(idx, i64Ty, "idx_ext");

    llvm::Value *origIndex = idx;

    // Negative-index wrap is still owned by CodeGen; reach it through the
    // callback slot (transitional — successor PRs will turn this into a
    // proper ABI function).
    if (ctx->cbs.emit_negative_index_wrap) {
        RyValueId wrappedId = ctx->cbs.emit_negative_index_wrap(
            ctx->cbs.user_ctx, ry_emit_intern(ctx, idx),
            ry_emit_intern(ctx, len), bb_prefix);
        idx = static_cast<llvm::Value *>(ry_emit_resolve(ctx, wrappedId));
    }

    llvm::Value *zero = llvm::ConstantInt::get(i64Ty, 0);
    std::string negLabel = std::string(bb_prefix) + "_neg";
    std::string overLabel = std::string(bb_prefix) + "_over";
    std::string oobLabel = std::string(bb_prefix) + "_oob";
    llvm::Value *negCheck = ctx->builder->CreateICmpSLT(idx, zero, negLabel);
    llvm::Value *overCheck = ctx->builder->CreateICmpSGE(idx, len, overLabel);
    llvm::Value *oob = ctx->builder->CreateOr(negCheck, overCheck, oobLabel);

    std::string oobBlockName = std::string(bb_prefix) + ".oob";
    std::string okBlockName = std::string(bb_prefix) + ".ok";
    auto *oobBB =
        llvm::BasicBlock::Create(*ctx->context, oobBlockName, ctx->function);
    auto *okBB =
        llvm::BasicBlock::Create(*ctx->context, okBlockName, ctx->function);
    ctx->builder->CreateCondBr(oob, oobBB, okBB);
    ctx->builder->SetInsertPoint(oobBB);

    const char *fmtMsg = (kind == RY_BOUNDS_LIST)
        ? "runtime error: index %lld out of bounds for list of length %lld\n"
        : "runtime error: index %lld out of bounds for array of length %lld\n";

    if (ctx->cbs.emit_bounds_error)
        ctx->cbs.emit_bounds_error(ctx->cbs.user_ctx,
                                   ry_emit_intern(ctx, origIndex),
                                   ry_emit_intern(ctx, len), fmtMsg,
                                   global_name);
    else
        // Defensive: if the caller forgot to register a callback, terminate
        // oobBB with unreachable so LLVM verify still accepts the function.
        ctx->builder->CreateUnreachable();

    ctx->builder->SetInsertPoint(okBB);
    return ry_emit_intern(ctx, idx);
}

} // extern "C"
