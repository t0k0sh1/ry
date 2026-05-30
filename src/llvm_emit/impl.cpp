// Implementation TU for the LLVM IR emission shared library.
//
// Built as `ry_llvm_emit` (SHARED). At runtime the symbols here are reached
// from the main `ry` / `ry_tests` binary either by direct link (so the JIT
// host process resolves them) or by `-undefined dynamic_lookup` / `-rdynamic`
// like the existing native stdlib libs. This TU is the only consumer of
// `<ry/llvm_emit/api.h>` and the only place where the ABI handle types are
// resolved to concrete LLVM objects.
//
// Stage 2-B note: module / builder / context / function pointers still cross
// the ABI as `void*` (see api.h for the migration roadmap). The cast helpers
// below centralize the reinterpretation so a single later edit can replace
// them once categories 1/2 cross the ABI.

#include "ry/llvm_emit/api.h"

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

#include <string>
#include <unordered_map>
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
    // Dedup cache for ry_emit_bounds_error format-string globals. The emission
    // layer owns its own cache (separate from CodeGen's global_string_cache_)
    // to keep this TU self-contained; functional impact is limited to
    // potential duplication of identical fmt_msg globals across the
    // CodeGen-managed and emission-managed sides, which LLVM tolerates.
    std::unordered_map<std::string, llvm::Constant *> bounds_msg_cache;
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
    return ctx;
}

void ry_emit_ctx_destroy(RyEmitCtx *ctx) { delete ctx; }

void ry_emit_ctx_set_function(RyEmitCtx *ctx, void *function_ptr) {
    ctx->function = asFunction(function_ptr);
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

RyValueId ry_emit_negative_index_wrap(RyEmitCtx *ctx, RyValueId idx_id,
                                      RyValueId wrap_base_id,
                                      const char *prefix) {
    auto *idx = static_cast<llvm::Value *>(ry_emit_resolve(ctx, idx_id));
    auto *wrapBase =
        static_cast<llvm::Value *>(ry_emit_resolve(ctx, wrap_base_id));
    auto *i64Ty = llvm::Type::getInt64Ty(*ctx->context);
    llvm::Value *zero = llvm::ConstantInt::get(i64Ty, 0);
    std::string p = prefix ? prefix : "";
    llvm::Value *isNeg = ctx->builder->CreateICmpSLT(idx, zero, p + "_is_neg");
    llvm::Value *wrapped = ctx->builder->CreateAdd(idx, wrapBase, p + "_wrapped");
    llvm::Value *result =
        ctx->builder->CreateSelect(isNeg, wrapped, idx, p + "_idx");
    return ry_emit_intern(ctx, result);
}

void ry_emit_bounds_error(RyEmitCtx *ctx, RyValueId orig_idx_id,
                          RyValueId len_id, const char *fmt_msg,
                          const char *global_name) {
    auto *origIdx =
        static_cast<llvm::Value *>(ry_emit_resolve(ctx, orig_idx_id));
    auto *len = static_cast<llvm::Value *>(ry_emit_resolve(ctx, len_id));
    auto *ptrTy = llvm::PointerType::getUnqual(*ctx->context);
    auto *i32Ty = llvm::Type::getInt32Ty(*ctx->context);
#ifdef __APPLE__
    const char *stdoutName = "__stdoutp";
    const char *stderrName = "__stderrp";
#else
    const char *stdoutName = "stdout";
    const char *stderrName = "stderr";
#endif
    auto *stderrGlobal = ctx->module->getOrInsertGlobal(stderrName, ptrTy);
    auto *stdoutGlobal = ctx->module->getOrInsertGlobal(stdoutName, ptrTy);
    llvm::Value *stderrVal =
        ctx->builder->CreateLoad(ptrTy, stderrGlobal, "stderr");
    llvm::Value *stdoutVal =
        ctx->builder->CreateLoad(ptrTy, stdoutGlobal, "stdout");

    // Dedup the format-string global within this RyEmitCtx so repeated
    // emissions of the same message reuse a single private constant.
    std::string fmtKey = fmt_msg ? fmt_msg : "";
    llvm::Constant *errMsg;
    auto it = ctx->bounds_msg_cache.find(fmtKey);
    if (it != ctx->bounds_msg_cache.end()) {
        errMsg = it->second;
    } else {
        auto *strData =
            llvm::ConstantDataArray::getString(*ctx->context, fmtKey);
        std::string name = global_name ? global_name : ".bounds_err_msg";
        auto *gv = new llvm::GlobalVariable(
            *ctx->module, strData->getType(), /*isConstant=*/true,
            llvm::GlobalValue::PrivateLinkage, strData, name);
        gv->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
        gv->setAlignment(llvm::Align(1));
        errMsg = gv;
        ctx->bounds_msg_cache[fmtKey] = errMsg;
    }

    auto fprintfTy = llvm::FunctionType::get(i32Ty, {ptrTy, ptrTy}, true);
    auto fprintfFn = ctx->module->getOrInsertFunction("fprintf", fprintfTy);
    ctx->builder->CreateCall(fprintfFn, {stderrVal, errMsg, origIdx, len});

    auto fflushTy = llvm::FunctionType::get(i32Ty, {ptrTy}, false);
    auto fflushFn = ctx->module->getOrInsertFunction("fflush", fflushTy);
    ctx->builder->CreateCall(fflushFn, {stdoutVal});
    ctx->builder->CreateCall(fflushFn, {stderrVal});

    auto exitTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx->context), {i32Ty}, false);
    auto exitFn = ctx->module->getOrInsertFunction("_Exit", exitTy);
    ctx->builder->CreateCall(exitFn, {llvm::ConstantInt::get(i32Ty, 1)});
    ctx->builder->CreateUnreachable();
}

RyValueId ry_emit_bounds_check(RyEmitCtx *ctx, RyValueId idx_id, RyValueId len_id,
                               RyBoundsKind kind, const char *global_name,
                               const char *bb_prefix) {
    auto *idx = static_cast<llvm::Value *>(ry_emit_resolve(ctx, idx_id));
    auto *len = static_cast<llvm::Value *>(ry_emit_resolve(ctx, len_id));
    auto *i1Ty = llvm::Type::getInt1Ty(*ctx->context);
    auto *i64Ty = llvm::Type::getInt64Ty(*ctx->context);
    if (idx->getType() == i1Ty)
        idx = ctx->builder->CreateZExt(idx, i64Ty, "idx_ext");

    llvm::Value *origIndex = idx;

    // Negative-index wrap is now a proper ABI function (Stage 2-B).
    RyValueId wrappedId = ry_emit_negative_index_wrap(
        ctx, ry_emit_intern(ctx, idx), ry_emit_intern(ctx, len), bb_prefix);
    idx = static_cast<llvm::Value *>(ry_emit_resolve(ctx, wrappedId));

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

    ry_emit_bounds_error(ctx, ry_emit_intern(ctx, origIndex),
                         ry_emit_intern(ctx, len), fmtMsg, global_name);

    ctx->builder->SetInsertPoint(okBB);
    return ry_emit_intern(ctx, idx);
}

RyValueId ry_emit_result_branch(RyEmitCtx *ctx, RyValueId is_err_id,
                                void *res_ty_ptr, RyBuildValueFn build_ok,
                                RyBuildValueFn build_err, void *user_ctx) {
    auto *isErr = static_cast<llvm::Value *>(ry_emit_resolve(ctx, is_err_id));
    auto *resTy = static_cast<llvm::StructType *>(res_ty_ptr);

    auto *okBB = llvm::BasicBlock::Create(*ctx->context, "res.ok", ctx->function);
    auto *errBB = llvm::BasicBlock::Create(*ctx->context, "res.err", ctx->function);
    auto *mergeBB =
        llvm::BasicBlock::Create(*ctx->context, "res.merge", ctx->function);
    ctx->builder->CreateCondBr(isErr, errBB, okBB);

    ctx->builder->SetInsertPoint(okBB);
    auto *okVal = static_cast<llvm::Value *>(
        ry_emit_resolve(ctx, build_ok(user_ctx)));
    ctx->builder->CreateBr(mergeBB);
    okBB = ctx->builder->GetInsertBlock();

    ctx->builder->SetInsertPoint(errBB);
    auto *errVal = static_cast<llvm::Value *>(
        ry_emit_resolve(ctx, build_err(user_ctx)));
    ctx->builder->CreateBr(mergeBB);
    errBB = ctx->builder->GetInsertBlock();

    ctx->builder->SetInsertPoint(mergeBB);
    llvm::PHINode *phi = ctx->builder->CreatePHI(resTy, 2, "result");
    phi->addIncoming(okVal, okBB);
    phi->addIncoming(errVal, errBB);
    return ry_emit_intern(ctx, phi);
}

} // extern "C"
