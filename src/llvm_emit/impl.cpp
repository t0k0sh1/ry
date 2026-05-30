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
#include "ry/ry_layout.hpp"

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>

// Address of the relaxed-atomic ARC live-count counter. Mirrors the extern
// in src/codegen_arc.cpp so the emission layer can emit the same inline
// inttoptr + atomicrmw sequence as CodeGen::emitArcCounterDeltaIR.
extern "C" int64_t *__ry_arc_counter_address();

namespace {

llvm::Module *asModule(void *p) { return static_cast<llvm::Module *>(p); }
llvm::IRBuilder<> *asBuilder(void *p) { return static_cast<llvm::IRBuilder<> *>(p); }
llvm::LLVMContext *asContext(void *p) { return static_cast<llvm::LLVMContext *>(p); }
llvm::Function *asFunction(void *p) { return static_cast<llvm::Function *>(p); }

// Local mirror of CodeGen::emitArcCounterDeltaIR. Kept ABI-private (anonymous
// namespace) because the counter address is a process-global symbol and there
// is no value in exposing the helper across the boundary.
void emitArcCounterDeltaIR(llvm::IRBuilder<> &builder, llvm::Type *i64Ty,
                           llvm::Type *ptrTy, int64_t delta) {
    // NOLINTNEXTLINE(performance-no-int-to-ptr)
    auto *ctrAddrConst = llvm::ConstantInt::get(
        i64Ty, static_cast<uint64_t>(
                   reinterpret_cast<uintptr_t>(__ry_arc_counter_address())));
    auto *ctrPtr = builder.CreateIntToPtr(ctrAddrConst, ptrTy, "arc_ctr");
    builder.CreateAtomicRMW(llvm::AtomicRMWInst::Add, ctrPtr,
        llvm::ConstantInt::get(i64Ty, static_cast<uint64_t>(delta)),
        llvm::MaybeAlign(8), llvm::AtomicOrdering::Monotonic);
}

// Local mirror of CodeGen::emitAtomicI64Load. NotAtomic ordering falls back to
// plain CreateLoad (alignment = 1, ABI default) to match CodeGen's behaviour
// — forcing Align(8) here would assert a stronger alignment than callers
// guarantee and crashes on Linux glibc when the pointer is not 8-byte aligned
// (cf. #630 CI regression note in src/codegen_arc.cpp:105-109).
llvm::LoadInst *emitAtomicI64Load(llvm::IRBuilder<> &builder, llvm::Type *i64Ty,
                                  llvm::Value *ptr,
                                  llvm::AtomicOrdering ordering,
                                  const llvm::Twine &name) {
    if (ordering == llvm::AtomicOrdering::NotAtomic)
        return builder.CreateLoad(i64Ty, ptr, name);
    auto *ld = builder.CreateAlignedLoad(i64Ty, ptr, llvm::Align(8), name);
    ld->setAtomic(ordering);
    return ld;
}

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
    std::string p = prefix ? prefix : "";
    auto *i64Ty = llvm::Type::getInt64Ty(*ctx->context);
    // Standalone ABI helper — normalize narrow operands to i64 defensively
    // (callers that go through ry_emit_bounds_check already widen, but direct
    // callers from future caller-side migrations are not guaranteed to).
    if (idx->getType() != i64Ty)
        idx = ctx->builder->CreateIntCast(idx, i64Ty, /*isSigned=*/true,
                                          p + "_idx_i64");
    if (wrapBase->getType() != i64Ty)
        wrapBase = ctx->builder->CreateIntCast(wrapBase, i64Ty,
                                               /*isSigned=*/true,
                                               p + "_wrap_base_i64");
    llvm::Value *zero = llvm::ConstantInt::get(i64Ty, 0);
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

void ry_emit_arc_retain(RyEmitCtx *ctx, RyValueId header_ptr_id,
                        RyArcAtomic atomic) {
    auto *headerPtr =
        static_cast<llvm::Value *>(ry_emit_resolve(ctx, header_ptr_id));
    auto *i64Ty = llvm::Type::getInt64Ty(*ctx->context);
    auto *arcHeaderTy = llvm::StructType::get(*ctx->context, {i64Ty, i64Ty});

    auto *strongPtr = ctx->builder->CreateStructGEP(arcHeaderTy, headerPtr, 0,
                                                    "arc_retain_ptr");

    // Skip immortal objects (strong_count == INT64_MAX). Monotonic is
    // sufficient because ARC_IMMORTAL is a sticky sentinel — but the load
    // must still be atomic in atomic mode so it doesn't race with a
    // concurrent atomicrmw (#630).
    auto *cur = emitAtomicI64Load(
        *ctx->builder, i64Ty, strongPtr,
        atomic == RY_ARC_ATOMIC ? llvm::AtomicOrdering::Monotonic
                                : llvm::AtomicOrdering::NotAtomic,
        "arc_strong");
    auto *isImmortal = ctx->builder->CreateICmpEQ(
        cur, llvm::ConstantInt::get(i64Ty, ry::ARC_IMMORTAL), "arc_immortal");

    // Derive parent function from the builder's current insertion block, not
    // from ctx->function: ARC ops are emitted inside destructor / lambda /
    // thunk bodies where cg.fn_ tracks the outer function while the builder
    // has already been retargeted to the nested function. Using ctx->function
    // would place new BBs in the wrong function and produce cross-function
    // references that fail LLVM verify.
    auto *fn = ctx->builder->GetInsertBlock()->getParent();
    auto *retainBB = llvm::BasicBlock::Create(*ctx->context, "arc.retain", fn);
    auto *doneBB =
        llvm::BasicBlock::Create(*ctx->context, "arc.retain.done", fn);
    ctx->builder->CreateCondBr(isImmortal, doneBB, retainBB);

    ctx->builder->SetInsertPoint(retainBB);
    if (atomic == RY_ARC_ATOMIC) {
        ctx->builder->CreateAtomicRMW(
            llvm::AtomicRMWInst::Add, strongPtr,
            llvm::ConstantInt::get(i64Ty, 1), llvm::MaybeAlign(),
            llvm::AtomicOrdering::SequentiallyConsistent);
    } else {
        auto *inc = ctx->builder->CreateAdd(
            cur, llvm::ConstantInt::get(i64Ty, 1), "arc_inc");
        ctx->builder->CreateStore(inc, strongPtr);
    }
    ctx->builder->CreateBr(doneBB);

    ctx->builder->SetInsertPoint(doneBB);
}

void ry_emit_arc_release(RyEmitCtx *ctx, RyValueId header_ptr_id,
                         RyArcAtomic atomic, void *destructor_callee,
                         void *gc_visit_fn) {
    auto *headerPtr =
        static_cast<llvm::Value *>(ry_emit_resolve(ctx, header_ptr_id));
    auto *i64Ty = llvm::Type::getInt64Ty(*ctx->context);
    auto *i8Ty = llvm::Type::getInt8Ty(*ctx->context);
    auto *voidTy = llvm::Type::getVoidTy(*ctx->context);
    auto *ptrTy = llvm::PointerType::getUnqual(*ctx->context);
    auto *arcHeaderTy = llvm::StructType::get(*ctx->context, {i64Ty, i64Ty});

    auto *strongPtr = ctx->builder->CreateStructGEP(arcHeaderTy, headerPtr, 0,
                                                    "arc_rel_ptr");

    // Skip immortal objects (strong_count == INT64_MAX). See ry_emit_arc_retain
    // for the atomic-mode rationale (#630).
    auto *curCheck = emitAtomicI64Load(
        *ctx->builder, i64Ty, strongPtr,
        atomic == RY_ARC_ATOMIC ? llvm::AtomicOrdering::Monotonic
                                : llvm::AtomicOrdering::NotAtomic,
        "arc_strong_check");
    auto *isImmortal = ctx->builder->CreateICmpEQ(
        curCheck, llvm::ConstantInt::get(i64Ty, ry::ARC_IMMORTAL),
        "arc_immortal");

    // See ry_emit_arc_retain for why we derive fn from the builder rather
    // than ctx->function (cross-function reference hazard in destructor /
    // lambda / thunk contexts where cg.fn_ tracks the outer function).
    auto *fn = ctx->builder->GetInsertBlock()->getParent();
    auto *releaseBB =
        llvm::BasicBlock::Create(*ctx->context, "arc.release.body", fn);
    auto *doneBB = llvm::BasicBlock::Create(*ctx->context, "arc.done", fn);
    ctx->builder->CreateCondBr(isImmortal, doneBB, releaseBB);

    ctx->builder->SetInsertPoint(releaseBB);
    llvm::Value *isZero;
    if (atomic == RY_ARC_ATOMIC) {
        // atomicrmw returns the OLD value; object is dead when old == 1
        auto *old = ctx->builder->CreateAtomicRMW(
            llvm::AtomicRMWInst::Sub, strongPtr,
            llvm::ConstantInt::get(i64Ty, 1), llvm::MaybeAlign(),
            llvm::AtomicOrdering::SequentiallyConsistent);
        isZero = ctx->builder->CreateICmpEQ(
            old, llvm::ConstantInt::get(i64Ty, 1), "arc_dead");
    } else {
        auto *cur =
            ctx->builder->CreateLoad(i64Ty, strongPtr, "arc_strong");
        auto *dec = ctx->builder->CreateSub(
            cur, llvm::ConstantInt::get(i64Ty, 1), "arc_dec");
        ctx->builder->CreateStore(dec, strongPtr);
        isZero = ctx->builder->CreateICmpEQ(
            dec, llvm::ConstantInt::get(i64Ty, 0), "arc_dead");
    }

    auto *freeBB = llvm::BasicBlock::Create(*ctx->context, "arc.release", fn);

    // When gc_visit_fn is provided, track the object as a GC candidate when
    // strong_count > 0 (potential cycle member).
    if (gc_visit_fn != nullptr) {
        auto *trackBB =
            llvm::BasicBlock::Create(*ctx->context, "arc.gc_track", fn);
        ctx->builder->CreateCondBr(isZero, freeBB, trackBB);

        ctx->builder->SetInsertPoint(trackBB);
        // Call __ry_gc_track(headerPtr, visitFn, dtorFn)
        auto *gcTrackFnTy =
            llvm::FunctionType::get(voidTy, {ptrTy, ptrTy, ptrTy}, false);
        auto gcTrackFn =
            ctx->module->getOrInsertFunction("__ry_gc_track", gcTrackFnTy);
        llvm::Value *dtorPtr = (destructor_callee != nullptr)
            ? static_cast<llvm::Value *>(destructor_callee)
            : llvm::cast<llvm::Value>(llvm::ConstantPointerNull::get(
                  llvm::cast<llvm::PointerType>(ptrTy)));
        auto *gcVisitFnVal = static_cast<llvm::Value *>(gc_visit_fn);
        ctx->builder->CreateCall(gcTrackFn,
                                 {headerPtr, gcVisitFnVal, dtorPtr});
        ctx->builder->CreateBr(doneBB);
    } else {
        ctx->builder->CreateCondBr(isZero, freeBB, doneBB);
    }

    ctx->builder->SetInsertPoint(freeBB);
    // Untrack from GC candidate set before freeing. This is safe even if the
    // object was never tracked or has already been untracked.
    auto *gcUntrackFnTy = llvm::FunctionType::get(voidTy, {ptrTy}, false);
    auto gcUntrackFn =
        ctx->module->getOrInsertFunction("__ry_gc_untrack", gcUntrackFnTy);
    ctx->builder->CreateCall(gcUntrackFn, {headerPtr});
    if (destructor_callee != nullptr) {
        // dataPtr = headerPtr + ARC_HEADER_SIZE. We deliberately do NOT
        // register dataPtr in CodeGen::arc_owned_values_ here (the codegen
        // helper does for alloc paths); release-site dataPtr is dead after
        // the destructor call below, so the registration would have no
        // observable effect.
        auto *dataPtr = ctx->builder->CreateGEP(
            i8Ty, headerPtr,
            llvm::ConstantInt::get(i64Ty, ry::ARC_HEADER_SIZE), "arc_data");
        auto *destructorFnTy =
            llvm::FunctionType::get(voidTy, {ptrTy}, false);
        auto destructor = llvm::FunctionCallee(
            destructorFnTy, static_cast<llvm::Value *>(destructor_callee));
        ctx->builder->CreateCall(destructor, {dataPtr});
    }
    // Only free the entire block when no weak references remain.
    // When weak_count > 0, the header must stay alive for weak ref resolution.
    auto *weakPtr = ctx->builder->CreateStructGEP(arcHeaderTy, headerPtr, 1,
                                                  "arc_weak_ptr");
    auto *weakCount = ctx->builder->CreateLoad(i64Ty, weakPtr, "arc_weak");
    auto *noWeak = ctx->builder->CreateICmpEQ(
        weakCount, llvm::ConstantInt::get(i64Ty, 0), "arc_no_weak");

    auto *realFreeBB =
        llvm::BasicBlock::Create(*ctx->context, "arc.free", fn);
    auto *skipFreeBB =
        llvm::BasicBlock::Create(*ctx->context, "arc.skip_free", fn);
    ctx->builder->CreateCondBr(noWeak, realFreeBB, skipFreeBB);

    ctx->builder->SetInsertPoint(realFreeBB);
    // Decrement the ARC live-count balance counter inline (no new symbol).
    emitArcCounterDeltaIR(*ctx->builder, i64Ty, ptrTy, -1);
    auto freeFnTy = llvm::FunctionType::get(voidTy, {ptrTy}, false);
    auto freeFn = ctx->module->getOrInsertFunction("free", freeFnTy);
    ctx->builder->CreateCall(freeFn, {headerPtr});
    ctx->builder->CreateBr(doneBB);

    ctx->builder->SetInsertPoint(skipFreeBB);
    ctx->builder->CreateBr(doneBB);

    ctx->builder->SetInsertPoint(doneBB);
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

RyValueId ry_emit_option_wrap_some(RyEmitCtx *ctx, RyValueId inner_id,
                                   void *opt_ty_ptr) {
    auto *optTy = static_cast<llvm::StructType *>(opt_ty_ptr);
    auto *inner = static_cast<llvm::Value *>(ry_emit_resolve(ctx, inner_id));
    auto *i1Ty = llvm::Type::getInt1Ty(*ctx->context);
    llvm::Value *val = llvm::UndefValue::get(optTy);
    val = ctx->builder->CreateInsertValue(val, llvm::ConstantInt::get(i1Ty, 1), 0);
    val = ctx->builder->CreateInsertValue(val, inner, 1);
    return ry_emit_intern(ctx, val);
}

RyValueId ry_emit_option_wrap_none(RyEmitCtx *ctx, void *opt_ty_ptr) {
    auto *optTy = static_cast<llvm::StructType *>(opt_ty_ptr);
    auto *i1Ty = llvm::Type::getInt1Ty(*ctx->context);
    llvm::Value *val = llvm::UndefValue::get(optTy);
    val = ctx->builder->CreateInsertValue(val, llvm::ConstantInt::get(i1Ty, 0), 0);
    val = ctx->builder->CreateInsertValue(
        val, llvm::UndefValue::get(optTy->getElementType(1)), 1);
    return ry_emit_intern(ctx, val);
}

} // extern "C"
