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
    // In atomic mode the load must be atomic — weak retain / weak rel in
    // codegen_arc.cpp update this slot with atomicrmw Monotonic, so a plain
    // load races with them and would be flagged as a data race.
    auto *weakPtr = ctx->builder->CreateStructGEP(arcHeaderTy, headerPtr, 1,
                                                  "arc_weak_ptr");
    auto *weakCount = emitAtomicI64Load(
        *ctx->builder, i64Ty, weakPtr,
        atomic == RY_ARC_ATOMIC ? llvm::AtomicOrdering::Acquire
                                : llvm::AtomicOrdering::NotAtomic,
        "arc_weak");
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

namespace {

// loadListHeader mirror — produces the same {len, cap, data} loads + GEPs as
// CodeGen::loadListHeader, so impl-side IR matches the original byte-for-byte
// (FileCheck-stable). prefix gets concatenated with "_len_ptr" / "_cap_ptr" /
// "_data_ptr" / "_len" / "_cap" / "_data".
struct ListHeaderLoad {
    llvm::Value *lenPtr;
    llvm::Value *capPtr;
    llvm::Value *dataPtr;
    llvm::Value *len;
    llvm::Value *cap;
    llvm::Value *data;
};

ListHeaderLoad loadListHeaderImpl(llvm::IRBuilder<> &builder,
                                  llvm::StructType *listHeaderTy,
                                  llvm::Value *listPtr, llvm::Type *i64Ty,
                                  llvm::Type *ptrTy, const char *prefix) {
    ListHeaderLoad h{};
    h.lenPtr =
        builder.CreateStructGEP(listHeaderTy, listPtr, 0,
                                std::string(prefix) + "_len_ptr");
    h.capPtr =
        builder.CreateStructGEP(listHeaderTy, listPtr, 1,
                                std::string(prefix) + "_cap_ptr");
    h.dataPtr =
        builder.CreateStructGEP(listHeaderTy, listPtr, 2,
                                std::string(prefix) + "_data_ptr");
    h.len = builder.CreateLoad(i64Ty, h.lenPtr, std::string(prefix) + "_len");
    h.cap = builder.CreateLoad(i64Ty, h.capPtr, std::string(prefix) + "_cap");
    h.data =
        builder.CreateLoad(ptrTy, h.dataPtr, std::string(prefix) + "_data");
    return h;
}

} // namespace

void ry_emit_collection_append(RyEmitCtx *ctx, RyValueId list_ptr_id,
                               RyValueId val_id, void *list_header_ty_ptr,
                               void *elem_ty_ptr, uint64_t elem_size) {
    auto *listPtr =
        static_cast<llvm::Value *>(ry_emit_resolve(ctx, list_ptr_id));
    auto *val = static_cast<llvm::Value *>(ry_emit_resolve(ctx, val_id));
    auto *listHeaderTy = static_cast<llvm::StructType *>(list_header_ty_ptr);
    auto *elemTy = static_cast<llvm::Type *>(elem_ty_ptr);
    auto *i64Ty = llvm::Type::getInt64Ty(*ctx->context);
    auto *ptrTy = llvm::PointerType::getUnqual(*ctx->context);
    auto *voidTy = llvm::Type::getVoidTy(*ctx->context);
    auto &builder = *ctx->builder;
    auto *fn = builder.GetInsertBlock()->getParent();

    auto h =
        loadListHeaderImpl(builder, listHeaderTy, listPtr, i64Ty, ptrTy, "app");

    auto *needGrow = builder.CreateICmpEQ(h.len, h.cap, "app_need_grow");
    auto *growBB = llvm::BasicBlock::Create(*ctx->context, "app.grow", fn);
    auto *storeBB = llvm::BasicBlock::Create(*ctx->context, "app.store", fn);
    builder.CreateCondBr(needGrow, growBB, storeBB);

    builder.SetInsertPoint(growBB);
    auto *four = llvm::ConstantInt::get(i64Ty, 4);
    auto *doubled = builder.CreateMul(
        h.cap, llvm::ConstantInt::get(i64Ty, 2), "app_doubled");
    auto *gt4 = builder.CreateICmpSGT(doubled, four, "cap_gt4");
    auto *newCap = builder.CreateSelect(gt4, doubled, four, "app_new_cap");
    auto *newSize = builder.CreateMul(
        newCap, llvm::ConstantInt::get(i64Ty, elem_size), "app_new_size");
    auto mallocFn = ctx->module->getOrInsertFunction(
        "malloc", llvm::FunctionType::get(ptrTy, {i64Ty}, false));
    auto *newData = builder.CreateCall(mallocFn, {newSize}, "app_new_data");
    auto *oldSize = builder.CreateMul(
        h.len, llvm::ConstantInt::get(i64Ty, elem_size), "app_old_size");
    auto memcpyFn = ctx->module->getOrInsertFunction(
        "memcpy",
        llvm::FunctionType::get(ptrTy, {ptrTy, ptrTy, i64Ty}, false));
    builder.CreateCall(memcpyFn, {newData, h.data, oldSize});
    auto freeFn = ctx->module->getOrInsertFunction(
        "free", llvm::FunctionType::get(voidTy, {ptrTy}, false));
    builder.CreateCall(freeFn, {h.data});
    builder.CreateStore(newData, h.dataPtr);
    builder.CreateStore(newCap, h.capPtr);
    builder.CreateBr(storeBB);

    builder.SetInsertPoint(storeBB);
    auto *curData = builder.CreateLoad(ptrTy, h.dataPtr, "app_cur_data");
    auto *curLen = builder.CreateLoad(i64Ty, h.lenPtr, "app_cur_len");
    auto *elemPtr =
        builder.CreateGEP(elemTy, curData, curLen, "app_elem_ptr");
    builder.CreateStore(val, elemPtr);
    auto *newLen = builder.CreateAdd(
        curLen, llvm::ConstantInt::get(i64Ty, 1), "app_new_len");
    builder.CreateStore(newLen, h.lenPtr);
}

void ry_emit_collection_insert(RyEmitCtx *ctx, RyValueId list_ptr_id,
                               RyValueId idx_id, RyValueId val_id,
                               void *list_header_ty_ptr, void *elem_ty_ptr,
                               uint64_t elem_size) {
    auto *listPtr =
        static_cast<llvm::Value *>(ry_emit_resolve(ctx, list_ptr_id));
    auto *origIdx = static_cast<llvm::Value *>(ry_emit_resolve(ctx, idx_id));
    auto *val = static_cast<llvm::Value *>(ry_emit_resolve(ctx, val_id));
    auto *listHeaderTy = static_cast<llvm::StructType *>(list_header_ty_ptr);
    auto *elemTy = static_cast<llvm::Type *>(elem_ty_ptr);
    auto *i64Ty = llvm::Type::getInt64Ty(*ctx->context);
    auto *ptrTy = llvm::PointerType::getUnqual(*ctx->context);
    auto *voidTy = llvm::Type::getVoidTy(*ctx->context);
    auto &builder = *ctx->builder;
    auto *fn = builder.GetInsertBlock()->getParent();

    auto h =
        loadListHeaderImpl(builder, listHeaderTy, listPtr, i64Ty, ptrTy, "ins");

    // insert() valid range is [0, len], so negative -k maps to len+1-k.
    auto *wrapBase = builder.CreateAdd(
        h.len, llvm::ConstantInt::get(i64Ty, 1), "ins_wrap_base");
    RyValueId wrappedId = ry_emit_negative_index_wrap(
        ctx, ry_emit_intern(ctx, origIdx), ry_emit_intern(ctx, wrapBase),
        "ins");
    auto *idx = static_cast<llvm::Value *>(ry_emit_resolve(ctx, wrappedId));

    auto *zero = llvm::ConstantInt::get(i64Ty, 0);
    auto *negCheck = builder.CreateICmpSLT(idx, zero);
    auto *overCheck = builder.CreateICmpSGT(idx, h.len);
    auto *outOfBounds = builder.CreateOr(negCheck, overCheck, "ins_oob");
    auto *errBB = llvm::BasicBlock::Create(*ctx->context, "ins.err", fn);
    auto *okBB = llvm::BasicBlock::Create(*ctx->context, "ins.ok", fn);
    builder.CreateCondBr(outOfBounds, errBB, okBB);
    builder.SetInsertPoint(errBB);
    ry_emit_bounds_error(
        ctx, ry_emit_intern(ctx, origIdx), ry_emit_intern(ctx, h.len),
        "runtime error: index %lld out of bounds for insert() on list of "
        "length %lld\n",
        ".ins_oob_err");

    builder.SetInsertPoint(okBB);
    auto *needGrow = builder.CreateICmpEQ(h.len, h.cap, "ins_need_grow");
    auto *growBB = llvm::BasicBlock::Create(*ctx->context, "ins.grow", fn);
    auto *moveBB = llvm::BasicBlock::Create(*ctx->context, "ins.move", fn);
    builder.CreateCondBr(needGrow, growBB, moveBB);

    builder.SetInsertPoint(growBB);
    auto *four = llvm::ConstantInt::get(i64Ty, 4);
    auto *doubled = builder.CreateMul(
        h.cap, llvm::ConstantInt::get(i64Ty, 2), "ins_doubled");
    auto *gt4 = builder.CreateICmpSGT(doubled, four);
    auto *newCap = builder.CreateSelect(gt4, doubled, four, "ins_new_cap");
    auto *newSize = builder.CreateMul(
        newCap, llvm::ConstantInt::get(i64Ty, elem_size), "ins_new_size");
    auto mallocFn = ctx->module->getOrInsertFunction(
        "malloc", llvm::FunctionType::get(ptrTy, {i64Ty}, false));
    auto *newData = builder.CreateCall(mallocFn, {newSize}, "ins_new_data");
    auto *oldSize = builder.CreateMul(
        h.len, llvm::ConstantInt::get(i64Ty, elem_size), "ins_old_size");
    auto memcpyFn = ctx->module->getOrInsertFunction(
        "memcpy",
        llvm::FunctionType::get(ptrTy, {ptrTy, ptrTy, i64Ty}, false));
    builder.CreateCall(memcpyFn, {newData, h.data, oldSize});
    auto freeFn = ctx->module->getOrInsertFunction(
        "free", llvm::FunctionType::get(voidTy, {ptrTy}, false));
    builder.CreateCall(freeFn, {h.data});
    builder.CreateStore(newData, h.dataPtr);
    builder.CreateStore(newCap, h.capPtr);
    builder.CreateBr(moveBB);

    builder.SetInsertPoint(moveBB);
    auto *curData = builder.CreateLoad(ptrTy, h.dataPtr, "ins_cur_data");
    auto *srcPtr = builder.CreateGEP(elemTy, curData, {idx}, "ins_src");
    auto *idxPlusOne =
        builder.CreateAdd(idx, llvm::ConstantInt::get(i64Ty, 1));
    auto *dstPtr =
        builder.CreateGEP(elemTy, curData, {idxPlusOne}, "ins_dst");
    auto *moveCount = builder.CreateSub(h.len, idx, "ins_move_count");
    auto *moveBytes = builder.CreateMul(
        moveCount, llvm::ConstantInt::get(i64Ty, elem_size),
        "ins_move_bytes");
    auto memmoveFn = ctx->module->getOrInsertFunction(
        "memmove",
        llvm::FunctionType::get(ptrTy, {ptrTy, ptrTy, i64Ty}, false));
    builder.CreateCall(memmoveFn, {dstPtr, srcPtr, moveBytes});
    auto *insertPtr = builder.CreateGEP(elemTy, curData, {idx}, "ins_ptr");
    builder.CreateStore(val, insertPtr);
    auto *newLen = builder.CreateAdd(
        h.len, llvm::ConstantInt::get(i64Ty, 1), "ins_new_len");
    builder.CreateStore(newLen, h.lenPtr);
}

RyValueId ry_emit_collection_remove_at(RyEmitCtx *ctx, RyValueId list_ptr_id,
                                        RyValueId idx_id,
                                        void *list_header_ty_ptr,
                                        void *elem_ty_ptr,
                                        uint64_t elem_size) {
    auto *listPtr =
        static_cast<llvm::Value *>(ry_emit_resolve(ctx, list_ptr_id));
    auto *origIdx = static_cast<llvm::Value *>(ry_emit_resolve(ctx, idx_id));
    auto *listHeaderTy = static_cast<llvm::StructType *>(list_header_ty_ptr);
    auto *elemTy = static_cast<llvm::Type *>(elem_ty_ptr);
    auto *i64Ty = llvm::Type::getInt64Ty(*ctx->context);
    auto *ptrTy = llvm::PointerType::getUnqual(*ctx->context);
    auto &builder = *ctx->builder;
    auto *fn = builder.GetInsertBlock()->getParent();

    auto h = loadListHeaderImpl(builder, listHeaderTy, listPtr, i64Ty, ptrTy,
                                "rmat");

    RyValueId wrappedId = ry_emit_negative_index_wrap(
        ctx, ry_emit_intern(ctx, origIdx), ry_emit_intern(ctx, h.len), "rmat");
    auto *idx = static_cast<llvm::Value *>(ry_emit_resolve(ctx, wrappedId));

    auto *zero = llvm::ConstantInt::get(i64Ty, 0);
    auto *negCheck = builder.CreateICmpSLT(idx, zero);
    auto *overCheck = builder.CreateICmpSGE(idx, h.len);
    auto *outOfBounds = builder.CreateOr(negCheck, overCheck, "rmat_oob");
    auto *errBB = llvm::BasicBlock::Create(*ctx->context, "rmat.err", fn);
    auto *okBB = llvm::BasicBlock::Create(*ctx->context, "rmat.ok", fn);
    builder.CreateCondBr(outOfBounds, errBB, okBB);
    builder.SetInsertPoint(errBB);
    ry_emit_bounds_error(
        ctx, ry_emit_intern(ctx, origIdx), ry_emit_intern(ctx, h.len),
        "runtime error: index %lld out of bounds for removeAt() on list of "
        "length %lld\n",
        ".rmat_oob_err");

    builder.SetInsertPoint(okBB);
    auto *elemPtr =
        builder.CreateGEP(elemTy, h.data, {idx}, "rmat_elem_ptr");
    auto *removedVal = builder.CreateLoad(elemTy, elemPtr, "rmat_val");
    auto *idxPlusOne =
        builder.CreateAdd(idx, llvm::ConstantInt::get(i64Ty, 1));
    auto *srcPtr = builder.CreateGEP(elemTy, h.data, {idxPlusOne}, "rmat_src");
    auto *lenMinusIdx = builder.CreateSub(h.len, idx);
    auto *moveCount = builder.CreateSub(
        lenMinusIdx, llvm::ConstantInt::get(i64Ty, 1), "rmat_move_count");
    auto *moveBytes = builder.CreateMul(
        moveCount, llvm::ConstantInt::get(i64Ty, elem_size),
        "rmat_move_bytes");
    auto memmoveFn = ctx->module->getOrInsertFunction(
        "memmove",
        llvm::FunctionType::get(ptrTy, {ptrTy, ptrTy, i64Ty}, false));
    builder.CreateCall(memmoveFn, {elemPtr, srcPtr, moveBytes});
    auto *newLen = builder.CreateSub(
        h.len, llvm::ConstantInt::get(i64Ty, 1), "rmat_new_len");
    builder.CreateStore(newLen, h.lenPtr);

    return ry_emit_intern(ctx, removedVal);
}

void ry_emit_list_slice(RyEmitCtx *ctx, RyValueId list_ptr_id,
                        RyValueId start_id, RyValueId end_excl_id,
                        void *list_header_ty_ptr, void *elem_ty_ptr,
                        uint64_t elem_size, RyValueId *out_count,
                        RyValueId *out_new_data) {
    auto *listPtr =
        static_cast<llvm::Value *>(ry_emit_resolve(ctx, list_ptr_id));
    auto *startVal =
        static_cast<llvm::Value *>(ry_emit_resolve(ctx, start_id));
    auto *endExclVal =
        static_cast<llvm::Value *>(ry_emit_resolve(ctx, end_excl_id));
    auto *listHeaderTy = static_cast<llvm::StructType *>(list_header_ty_ptr);
    auto *elemTy = static_cast<llvm::Type *>(elem_ty_ptr);
    auto *i64Ty = llvm::Type::getInt64Ty(*ctx->context);
    auto *ptrTy = llvm::PointerType::getUnqual(*ctx->context);

    auto &builder = *ctx->builder;
    auto *slLenPtr =
        builder.CreateStructGEP(listHeaderTy, listPtr, 0, "sl_len_ptr");
    auto *slDataPtr =
        builder.CreateStructGEP(listHeaderTy, listPtr, 2, "sl_data_ptr");
    auto *slLen = builder.CreateLoad(i64Ty, slLenPtr, "sl_len");
    auto *slData = builder.CreateLoad(ptrTy, slDataPtr, "sl_data");

    auto *zero = llvm::ConstantInt::get(i64Ty, 0);
    auto *startNeg = builder.CreateICmpSLT(startVal, zero);
    auto *cStart =
        builder.CreateSelect(startNeg, zero, startVal, "sl_cstart");
    auto *startOver = builder.CreateICmpSGT(cStart, slLen);
    cStart = builder.CreateSelect(startOver, slLen, cStart, "sl_cstart2");
    auto *endNeg = builder.CreateICmpSLT(endExclVal, zero);
    auto *cEnd = builder.CreateSelect(endNeg, zero, endExclVal, "sl_cend");
    auto *endOver = builder.CreateICmpSGT(cEnd, slLen);
    cEnd = builder.CreateSelect(endOver, slLen, cEnd, "sl_cend2");

    auto *diff = builder.CreateSub(cEnd, cStart, "sl_diff");
    auto *diffGt0 = builder.CreateICmpSGT(diff, zero);
    auto *count = builder.CreateSelect(diffGt0, diff, zero, "sl_count");
    auto *dataSize = builder.CreateMul(
        count, llvm::ConstantInt::get(i64Ty, elem_size), "sl_dsize");
    auto mallocFnTy = llvm::FunctionType::get(ptrTy, {i64Ty}, false);
    auto mallocFn = ctx->module->getOrInsertFunction("malloc", mallocFnTy);
    auto *newData = builder.CreateCall(mallocFn, {dataSize}, "sl_data");
    auto *srcOffset =
        builder.CreateGEP(elemTy, slData, cStart, "sl_src_off");
    auto memcpyFnTy =
        llvm::FunctionType::get(ptrTy, {ptrTy, ptrTy, i64Ty}, false);
    auto memcpyFn = ctx->module->getOrInsertFunction("memcpy", memcpyFnTy);
    builder.CreateCall(memcpyFn, {newData, srcOffset, dataSize});

    *out_count = ry_emit_intern(ctx, count);
    *out_new_data = ry_emit_intern(ctx, newData);
}

RyValueId ry_emit_result_branch(RyEmitCtx *ctx, RyValueId is_err_id,
                                void *res_ty_ptr, RyBuildValueFn build_ok,
                                RyBuildValueFn build_err, void *user_ctx) {
    auto *isErr = static_cast<llvm::Value *>(ry_emit_resolve(ctx, is_err_id));
    auto *resTy = static_cast<llvm::StructType *>(res_ty_ptr);

    // Derive parent function from the builder's current insertion block, not
    // from ctx->function: like ARC ops, result-branch helpers may be invoked
    // inside destructor / lambda / thunk bodies where cg.fn_ tracks the outer
    // function while the builder has already been retargeted to the nested
    // function. Using ctx->function would place new BBs in the wrong function
    // and produce cross-function references that fail LLVM verify (the same
    // hazard fixed for ry_emit_arc_retain / ry_emit_arc_release in #1968).
    auto *fn = ctx->builder->GetInsertBlock()->getParent();
    auto *okBB = llvm::BasicBlock::Create(*ctx->context, "res.ok", fn);
    auto *errBB = llvm::BasicBlock::Create(*ctx->context, "res.err", fn);
    auto *mergeBB =
        llvm::BasicBlock::Create(*ctx->context, "res.merge", fn);
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

RyValueId ry_emit_runtime_call(RyEmitCtx *ctx, const char *name,
                               void *ret_ty_ptr,
                               void *const *arg_ty_ptrs,
                               uint32_t arg_ty_count,
                               const RyValueId *arg_ids,
                               uint32_t arg_count, const char *name_hint) {
    // ABI input validation: malformed external callers (Rust reimplementation
    // in #1950, fuzz harnesses, etc.) get the invalid sentinel (0) instead of
    // crashing the emitter. Per .claude/rules/runtime-memory-safety.md
    // "外部入力の NULL チェック". `name_hint` is optional and may be NULL.
    if (ctx == nullptr || ctx->module == nullptr || ctx->builder == nullptr ||
        name == nullptr || ret_ty_ptr == nullptr)
        return 0;
    if ((arg_ty_count > 0 && arg_ty_ptrs == nullptr) ||
        (arg_count > 0 && arg_ids == nullptr))
        return 0;
    if (arg_ty_count != arg_count)
        return 0;

    auto *retTy = static_cast<llvm::Type *>(ret_ty_ptr);
    std::vector<llvm::Type *> argTys;
    argTys.reserve(arg_ty_count);
    for (uint32_t i = 0; i < arg_ty_count; ++i) {
        if (arg_ty_ptrs[i] == nullptr)
            return 0;
        argTys.push_back(static_cast<llvm::Type *>(arg_ty_ptrs[i]));
    }

    auto *fnTy = llvm::FunctionType::get(retTy, argTys, /*isVarArg=*/false);
    auto callee = ctx->module->getOrInsertFunction(name, fnTy);

    std::vector<llvm::Value *> args;
    args.reserve(arg_count);
    for (uint32_t i = 0; i < arg_count; ++i) {
        auto *arg = static_cast<llvm::Value *>(ry_emit_resolve(ctx, arg_ids[i]));
        if (arg == nullptr)
            return 0;
        args.push_back(arg);
    }

    // CreateCall asserts when given a non-empty SSA name for a void-returning
    // call (LLVM forbids naming void instructions). Pass empty name in that
    // case regardless of what the caller supplied.
    llvm::Value *result;
    if (retTy->isVoidTy()) {
        result = ctx->builder->CreateCall(callee, args);
    } else {
        result = ctx->builder->CreateCall(callee, args,
                                          name_hint ? name_hint : "");
    }
    return ry_emit_intern(ctx, result);
}

} // extern "C"
