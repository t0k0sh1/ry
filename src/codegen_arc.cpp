#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"
#include <cassert>
#include <cstdint>

// Exposes the address of the relaxed-atomic ARC live-count counter so that
// codegen can embed it as an inttoptr constant and emit inline atomicrmw
// instructions.  Using inttoptr avoids introducing __ry_arc_alloc_counted /
// __ry_arc_free_counted as new function-call symbols in the JIT module, which
// would cause JITLink to create GOT stubs that crash Linux teardown.
extern "C" int64_t *__ry_arc_counter_address();

namespace ry {

// Emits an inline `atomicrmw add` on the ARC live-count counter with
// `delta` (+1 for alloc, -1 for free).  No new function symbol is added to
// the IR module — the counter address is encoded as an i64 constant.
static void emitArcCounterDeltaIR(llvm::IRBuilder<> &builder,
                                   llvm::Type *i64Ty, llvm::Type *ptrTy,
                                   int64_t delta) {
    // NOLINTNEXTLINE(performance-no-int-to-ptr)
    auto *ctrAddrConst = llvm::ConstantInt::get(
        i64Ty, static_cast<uint64_t>(
                   reinterpret_cast<uintptr_t>(__ry_arc_counter_address())));
    auto *ctrPtr = builder.CreateIntToPtr(ctrAddrConst, ptrTy, "arc_ctr");
    builder.CreateAtomicRMW(llvm::AtomicRMWInst::Add, ctrPtr,
        llvm::ConstantInt::get(i64Ty, static_cast<uint64_t>(delta)),
        llvm::MaybeAlign(8), llvm::AtomicOrdering::Monotonic);
}

llvm::Value *CodeGen::emitArcAlloc(llvm::Value *dataSize) {
    auto *headerSize = llvm::ConstantInt::get(i64Ty_, ARC_HEADER_SIZE);
    auto *totalSize = builder_.CreateAdd(dataSize, headerSize, "arc_total");
    auto *headerPtr = builder_.CreateCall(getStdlibMalloc(), {totalSize}, "arc_hdr");
    // Increment the ARC live-count balance counter inline (no new symbol).
    emitArcCounterDeltaIR(builder_, i64Ty_, ptrTy_, +1);

    auto *strongPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 0, "arc_strong_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 1), strongPtr);

    auto *weakPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 1, "arc_weak_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), weakPtr);

    return headerPtr;
}

llvm::Value *CodeGen::emitArcGetDataPtr(llvm::Value *headerPtr) {
    auto *dataPtr = builder_.CreateGEP(i8Ty_, headerPtr,
                              llvm::ConstantInt::get(i64Ty_, ARC_HEADER_SIZE),
                              "arc_data");
    arc_owned_values_.insert(dataPtr);
    return dataPtr;
}

llvm::Value *CodeGen::emitArcAllocCollectionHeader(llvm::Type *headerTy) {
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t size = dl.getTypeAllocSize(headerTy);
    auto *arcHdr = emitArcAlloc(llvm::ConstantInt::get(i64Ty_, size));
    return emitArcGetDataPtr(arcHdr);
}

llvm::Value *CodeGen::emitArcGetHeaderFromData(llvm::Value *dataPtr) {
    return builder_.CreateGEP(i8Ty_, dataPtr,
                              llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(-static_cast<int64_t>(ARC_HEADER_SIZE))),
                              "arc_hdr_from_data");
}

llvm::Value *CodeGen::emitStrGetHeaderFromData(llvm::Value *strHandle) {
    // Strings have STRING_HEADER_SIZE (24) bytes before the data pointer:
    // { strong_count: i64, weak_count: i64, byte_len: i64, data... }
    return builder_.CreateGEP(i8Ty_, strHandle,
        llvm::ConstantInt::get(i64Ty_,
            static_cast<uint64_t>(-static_cast<int64_t>(STRING_HEADER_SIZE))),
        "str_hdr_from_data");
}

llvm::Value *CodeGen::emitArcHeaderForAlloca(llvm::Value *handle, llvm::AllocaInst *srcAlloca) {
    if (srcAlloca && arc_str_managed_vars_.count(srcAlloca))
        return emitStrGetHeaderFromData(handle);
    return emitArcGetHeaderFromData(handle);
}

llvm::Value *CodeGen::emitStrGetDataPtr(llvm::Value *strHeaderPtr) {
    // Recover the str handle from the StringHeader pointer.
    auto *dataPtr = builder_.CreateGEP(i8Ty_, strHeaderPtr,
        llvm::ConstantInt::get(i64Ty_, STRING_HEADER_SIZE),
        "str_data");
    arc_str_owned_values_.insert(dataPtr);
    return dataPtr;
}

llvm::Value *CodeGen::emitStringByteLen(llvm::Value *handle) {
    // The byte_len field lives STRING_BYTELEN_OFFSET (8) bytes before the
    // string data pointer.  Emit: load i64, (handle - 8)
    auto *bytelenPtr = builder_.CreateGEP(
        i8Ty_, handle,
        llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(-static_cast<int64_t>(STRING_BYTELEN_OFFSET))),
        "str_bytelen_ptr");
    return builder_.CreateLoad(i64Ty_, bytelenPtr, "str_bytelen");
}

llvm::LoadInst *CodeGen::emitAtomicI64Load(llvm::Value *ptr,
                                           llvm::AtomicOrdering ordering,
                                           const llvm::Twine &name) {
    // Non-atomic path must match the old plain CreateLoad behaviour
    // (alignment=1, ABI-default). Forcing Align(8) here would assert a
    // stronger alignment than the surrounding code actually guarantees
    // and crashes on Linux glibc when the underlying pointer happens not
    // to be 8-byte aligned (#630 CI regression).
    if (ordering == llvm::AtomicOrdering::NotAtomic)
        return builder_.CreateLoad(i64Ty_, ptr, name);
    // Atomic i64 loads must be at least 8-byte aligned per LLVM's atomic
    // rules; asserting Align(8) is correct here because the ARC header
    // start is always 16-byte aligned (malloc'd).
    auto *ld = builder_.CreateAlignedLoad(i64Ty_, ptr, llvm::Align(8), name);
    ld->setAtomic(ordering);
    return ld;
}

void CodeGen::emitArcRetain(llvm::Value *headerPtr, bool atomic) {
    auto *strongPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 0, "arc_retain_ptr");

    // Skip immortal objects (strong_count == INT64_MAX). Monotonic is
    // sufficient because ARC_IMMORTAL is a sticky sentinel — but the load
    // must still be atomic in atomic mode so it doesn't race with a
    // concurrent atomicrmw (#630).
    auto *cur = emitAtomicI64Load(strongPtr,
        atomic ? llvm::AtomicOrdering::Monotonic : llvm::AtomicOrdering::NotAtomic,
        "arc_strong");
    auto *isImmortal = builder_.CreateICmpEQ(cur, llvm::ConstantInt::get(i64Ty_, ARC_IMMORTAL), "arc_immortal");

    auto *fn = builder_.GetInsertBlock()->getParent();
    auto *retainBB = llvm::BasicBlock::Create(*ctx_, "arc.retain", fn);
    auto *doneBB = llvm::BasicBlock::Create(*ctx_, "arc.retain.done", fn);
    builder_.CreateCondBr(isImmortal, doneBB, retainBB);

    builder_.SetInsertPoint(retainBB);
    if (atomic) {
        builder_.CreateAtomicRMW(llvm::AtomicRMWInst::Add, strongPtr,
                                 llvm::ConstantInt::get(i64Ty_, 1),
                                 llvm::MaybeAlign(),
                                 llvm::AtomicOrdering::SequentiallyConsistent);
    } else {
        auto *inc = builder_.CreateAdd(cur, llvm::ConstantInt::get(i64Ty_, 1), "arc_inc");
        builder_.CreateStore(inc, strongPtr);
    }
    builder_.CreateBr(doneBB);

    builder_.SetInsertPoint(doneBB);
}

void CodeGen::emitArcRelease(llvm::Value *headerPtr, bool atomic,
                              llvm::FunctionCallee destructor,
                              llvm::Function *gcVisitFn) {
    auto *strongPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 0, "arc_rel_ptr");

    // Skip immortal objects (strong_count == INT64_MAX). See emitArcRetain
    // for the atomic-mode rationale (#630).
    auto *curCheck = emitAtomicI64Load(strongPtr,
        atomic ? llvm::AtomicOrdering::Monotonic : llvm::AtomicOrdering::NotAtomic,
        "arc_strong_check");
    auto *isImmortal = builder_.CreateICmpEQ(curCheck, llvm::ConstantInt::get(i64Ty_, ARC_IMMORTAL), "arc_immortal");

    auto *fn = builder_.GetInsertBlock()->getParent();
    auto *releaseBB = llvm::BasicBlock::Create(*ctx_, "arc.release.body", fn);
    auto *doneBB = llvm::BasicBlock::Create(*ctx_, "arc.done", fn);
    builder_.CreateCondBr(isImmortal, doneBB, releaseBB);

    builder_.SetInsertPoint(releaseBB);
    llvm::Value *isZero;
    if (atomic) {
        // atomicrmw returns the OLD value; object is dead when old == 1
        auto *old = builder_.CreateAtomicRMW(
            llvm::AtomicRMWInst::Sub, strongPtr,
            llvm::ConstantInt::get(i64Ty_, 1),
            llvm::MaybeAlign(),
            llvm::AtomicOrdering::SequentiallyConsistent);
        isZero = builder_.CreateICmpEQ(old, llvm::ConstantInt::get(i64Ty_, 1), "arc_dead");
    } else {
        auto *cur = builder_.CreateLoad(i64Ty_, strongPtr, "arc_strong");
        auto *dec = builder_.CreateSub(cur, llvm::ConstantInt::get(i64Ty_, 1), "arc_dec");
        builder_.CreateStore(dec, strongPtr);
        isZero = builder_.CreateICmpEQ(dec, llvm::ConstantInt::get(i64Ty_, 0), "arc_dead");
    }

    auto *freeBB = llvm::BasicBlock::Create(*ctx_, "arc.release", fn);

    // When gcVisitFn is provided, track the object as a GC candidate when
    // strong_count > 0 (potential cycle member).
    if (gcVisitFn) {
        auto *trackBB = llvm::BasicBlock::Create(*ctx_, "arc.gc_track", fn);
        builder_.CreateCondBr(isZero, freeBB, trackBB);

        builder_.SetInsertPoint(trackBB);
        // Call __ry_gc_track(headerPtr, visitFn, dtorFn)
        auto *gcTrackFnTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_),
            {ptrTy_, ptrTy_, ptrTy_}, false);
        auto gcTrackFn = mod_->getOrInsertFunction("__ry_gc_track", gcTrackFnTy);
        used_native_libraries_.insert("gc");
        llvm::Value *dtorPtr = destructor
            ? llvm::cast<llvm::Value>(destructor.getCallee())
            : llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
        builder_.CreateCall(gcTrackFn, {headerPtr, gcVisitFn, dtorPtr});
        builder_.CreateBr(doneBB);
    } else {
        builder_.CreateCondBr(isZero, freeBB, doneBB);
    }

    builder_.SetInsertPoint(freeBB);
    // Untrack from GC candidate set before freeing. This is safe even if the
    // object was never tracked or has already been untracked.
    auto *gcUntrackFnTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
    auto gcUntrackFn = mod_->getOrInsertFunction("__ry_gc_untrack", gcUntrackFnTy);
    builder_.CreateCall(gcUntrackFn, {headerPtr});
    used_native_libraries_.insert("gc");
    if (destructor) {
        auto *dataPtr = emitArcGetDataPtr(headerPtr);
        builder_.CreateCall(destructor, {dataPtr});
    }
    // Only free the entire block when no weak references remain.
    // When weak_count > 0, the header must stay alive for weak ref resolution.
    auto *weakPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 1, "arc_weak_ptr");
    auto *weakCount = builder_.CreateLoad(i64Ty_, weakPtr, "arc_weak");
    auto *noWeak = builder_.CreateICmpEQ(weakCount, llvm::ConstantInt::get(i64Ty_, 0), "arc_no_weak");

    auto *realFreeBB = llvm::BasicBlock::Create(*ctx_, "arc.free", fn);
    auto *skipFreeBB = llvm::BasicBlock::Create(*ctx_, "arc.skip_free", fn);
    builder_.CreateCondBr(noWeak, realFreeBB, skipFreeBB);

    builder_.SetInsertPoint(realFreeBB);
    // Decrement the ARC live-count balance counter inline (no new symbol).
    emitArcCounterDeltaIR(builder_, i64Ty_, ptrTy_, -1);
    builder_.CreateCall(getStdlibFree(), {headerPtr});
    builder_.CreateBr(doneBB);

    builder_.SetInsertPoint(skipFreeBB);
    builder_.CreateBr(doneBB);

    builder_.SetInsertPoint(doneBB);
}

bool CodeGen::isArcAtomic(llvm::Value *val) const {
    // Inside a @parallel for thunk every ARC op must be atomic because
    // captured values are shared across workers. The per-value
    // `arc_atomic_values_` tracking only sees one alias depth and would
    // miss values flowing through helper calls or nested expressions.
    // See #630.
    if (parallel_for_depth_ > 0)
        return true;
    if (arc_atomic_values_.count(val))
        return true;
    if (auto *stripped = val->stripPointerCasts())
        if (arc_atomic_values_.count(stripped))
            return true;
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
        auto *ptr = load->getPointerOperand();
        if (arc_atomic_values_.count(ptr))
            return true;
        if (auto *strippedPtr = ptr->stripPointerCasts())
            return arc_atomic_values_.count(strippedPtr) > 0;
    }
    return false;
}

void CodeGen::markArcAtomic(llvm::Value *val) {
    arc_atomic_values_.insert(val);
}

void CodeGen::markArcManaged(llvm::AllocaInst *alloca) {
    arc_managed_vars_.insert(alloca);
}

bool CodeGen::isArcManaged(llvm::AllocaInst *alloca) const {
    return arc_managed_vars_.count(alloca) > 0;
}

bool CodeGen::isStrHandle(llvm::Value *v) const {
    if (v->getType() != ptrTy_) return false;
    if (arc_str_owned_values_.count(v) > 0) return true;
    if (auto *ld = llvm::dyn_cast<llvm::LoadInst>(v)) {
        auto *src = llvm::dyn_cast<llvm::AllocaInst>(ld->getPointerOperand());
        if (src && arc_str_managed_vars_.count(src) > 0) return true;
    }
    return false;
}

llvm::FunctionCallee CodeGen::resolveDestructor(llvm::AllocaInst *alloca) {
    auto collDtor = resolveCollectionDestructor(alloca);
    if (collDtor)
        return collDtor;
    auto it = resource_managed_vars_.find(alloca);
    if (it != resource_managed_vars_.end())
        return getOrCreateResourceDestructor(it->second);
    if (closure_managed_vars_.count(alloca)) {
        auto *meta = getMeta(alloca);
        if (meta && meta->fn_type_info) {
            if (meta->fn_type_info->isUniformClosure) {
                auto *dtorFn = getOrCreateUniformClosureDestructor();
                return llvm::FunctionCallee(dtorFn->getFunctionType(), dtorFn);
            }
            if (!meta->fn_type_info->capturedArcKinds.empty())
                return getOrCreateClosureDestructor(*meta->fn_type_info);
        }
    }
    return {};
}

int CodeGen::detectResourceKind(llvm::Value *val) {
    auto *meta = getMeta(val);
    if (meta && !meta->resource_kinds.empty())
        return meta->resource_kinds[0];
    return ResourceKindRegistry::NONE;
}


void CodeGen::nullifyResourceVar(const ExprNode &argExpr) {
    // After explicit free/close, null out the variable's alloca so that
    // ARC scope cleanup (emitArcReleaseVar) skips it via its null check.
    if (auto *varExpr = std::get_if<VariableExpr>(&argExpr.data)) {
        auto *alloca = findVar(varExpr->name);
        if (alloca && resource_managed_vars_.count(alloca)) {
            builder_.CreateStore(
                llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
                alloca);
        }
    }
}

llvm::Value *CodeGen::emitResourceFree(llvm::Value *dataPtr, int rk,
                                        const ExprNode &argExpr) {
    // Null check — already freed (nullified) variable is a no-op
    auto *isNull = builder_.CreateICmpEQ(
        dataPtr,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
        "res_free_null");
    auto *fn = builder_.GetInsertBlock()->getParent();
    auto *releaseBB = llvm::BasicBlock::Create(*ctx_, "res_free.release", fn);
    auto *doneBB = llvm::BasicBlock::Create(*ctx_, "res_free.done", fn);
    builder_.CreateCondBr(isNull, doneBB, releaseBB);

    builder_.SetInsertPoint(releaseBB);
    auto *hdr = emitArcGetHeaderFromData(dataPtr);
    bool atomic = isArcAtomic(dataPtr);
    emitArcRelease(hdr, atomic, getOrCreateResourceDestructor(rk));
    builder_.CreateBr(doneBB);

    builder_.SetInsertPoint(doneBB);
    nullifyResourceVar(argExpr);
    return llvm::ConstantInt::get(i8Ty_, 0); // Unit
}

llvm::FunctionCallee CodeGen::getOrCreateResourceDestructor(int rk) {
    auto it = resource_destructors_cache_.find(rk);
    if (it != resource_destructors_cache_.end())
        return it->second;

    auto *info = ResourceKindRegistry::instance().getInfo(rk);
    if (!info || !info->dtorName)
        return {};

    const char *dtorName = info->dtorName;
    const char *cleanupFnName = info->cleanupFnName;
    if (info->library)
        used_native_libraries_.insert(info->library);

    auto *dtorTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
    auto *dtorFn = llvm::Function::Create(
        dtorTy, llvm::Function::InternalLinkage, dtorName, mod_.get());
    dtorFn->addFnAttr(llvm::Attribute::NoUnwind);

    auto *entryBB = llvm::BasicBlock::Create(*ctx_, "entry", dtorFn);

    auto *savedBB = builder_.GetInsertBlock();
    auto savedPt = builder_.GetInsertPoint();
    builder_.SetInsertPoint(entryBB);

    auto *dataPtr = dtorFn->getArg(0);
    auto cleanupTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
    auto cleanupFn = mod_->getOrInsertFunction(cleanupFnName, cleanupTy);
    builder_.CreateCall(cleanupFn, {dataPtr});
    builder_.CreateRetVoid();

    if (savedBB)
        builder_.SetInsertPoint(savedBB, savedPt);

    llvm::FunctionCallee callee(dtorTy, dtorFn);
    resource_destructors_cache_[rk] = callee;
    return callee;
}

llvm::FunctionCallee CodeGen::resolveCollectionDestructor(llvm::AllocaInst *alloca) {
    auto *meta = getMeta(alloca);
    if (getTypeMeta(TypeMeta::ListElem, alloca)) {
        std::string elem = (meta && !meta->list_elem_type_name.empty())
            ? resolveTypeAlias(meta->list_elem_type_name) : "";
        return getOrCreateCollectionDestructor(CollectionKind::List, elem, "");
    }
    if (getTypeMeta(TypeMeta::MapKey, alloca)) {
        std::string key = (meta && !meta->map_key_type_name.empty())
            ? resolveTypeAlias(meta->map_key_type_name) : "";
        std::string val = (meta && !meta->map_value_type_name.empty())
            ? resolveTypeAlias(meta->map_value_type_name) : "";
        return getOrCreateCollectionDestructor(CollectionKind::Map, key, val);
    }
    if (getTypeMeta(TypeMeta::SetElem, alloca)) {
        std::string elem = (meta && !meta->set_elem_type_name.empty())
            ? resolveTypeAlias(meta->set_elem_type_name) : "";
        return getOrCreateCollectionDestructor(CollectionKind::Set, elem, "");
    }
    return {};
}

void CodeGen::emitArcReleaseVar(const std::string &name, llvm::AllocaInst *alloca) {
    auto *val = builder_.CreateLoad(ptrTy_, alloca, name + ".arc_cleanup");
    auto *isNull = builder_.CreateICmpEQ(val,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
        "arc_null_check");

    auto *fn = builder_.GetInsertBlock()->getParent();
    auto *releaseBB = llvm::BasicBlock::Create(*ctx_, "arc.var_release", fn);
    auto *skipBB = llvm::BasicBlock::Create(*ctx_, "arc.var_skip", fn);
    builder_.CreateCondBr(isNull, skipBB, releaseBB);

    builder_.SetInsertPoint(releaseBB);
    const bool isStrVar = arc_str_managed_vars_.count(alloca) > 0;
    auto *headerPtr = isStrVar ? emitStrGetHeaderFromData(val)
                               : emitArcGetHeaderFromData(val);

    // Look up GC visit function for potentially cyclic types.
    llvm::Function *gcVisitFn = nullptr;
    auto *meta = getMeta(alloca);
    if (meta && !meta->enum_value_type.empty() && isPotentiallyCyclic(meta->enum_value_type)) {
        gcVisitFn = getOrCreateVisitFunction(meta->enum_value_type);
    }

    // str values have no inner destructor (StringHeader has no child allocations).
    llvm::FunctionCallee destructor = isStrVar ? llvm::FunctionCallee{}
                                               : resolveDestructor(alloca);
    emitArcRelease(headerPtr, isArcAtomic(val), destructor, gcVisitFn);
    builder_.CreateBr(skipBB);

    builder_.SetInsertPoint(skipBB);
}

// ===== Collection type name helpers =====

bool CodeGen::isListTypeName(const std::string &typeName) {
    return typeName.size() > 5 && typeName.compare(0, 5, "List<") == 0;
}

bool CodeGen::isMapTypeName(const std::string &typeName) {
    return typeName.size() > 4 && typeName.compare(0, 4, "Map<") == 0;
}

bool CodeGen::isSetTypeName(const std::string &typeName) {
    return typeName.size() > 4 && typeName.compare(0, 4, "Set<") == 0;
}

bool CodeGen::isCollectionTypeName(const std::string &typeName) {
    return isListTypeName(typeName) || isMapTypeName(typeName) || isSetTypeName(typeName);
}

bool CodeGen::fieldTypeIsArcManaged(const std::string &fieldTypeName,
                                     CollectionKind *outFieldKind) {
    // Element-level / field-level ARC management is decided by the Ry *type
    // name*, not the LLVM type — ARC-managed slots are opaque ptr and the
    // LLVM type alone tells us nothing. Only non-weak nested collections
    // own an allocation per slot; strings, weak refs, closures, and records
    // are excluded and handled by different fix paths.
    //
    // Resolve type aliases first: `type Ints = List<int>` used as a field
    // type shows up here as `"Ints"`, which matches none of the prefix
    // predicates. Without this step alias-backed ARC fields would be
    // classified as non-ARC and their overwrite would silently leak.
    if (fieldTypeName.empty())
        return false;
    const std::string resolved = resolveTypeAlias(fieldTypeName);
    if (isWeakTypeName(resolved))
        return false;
    if (resolved == "str") {
        // str is ARC-managed via StringHeader (handle - STRING_HEADER_SIZE = 24).
        // CollectionKind::Str signals callers to use emitStrGetHeaderFromData.
        if (outFieldKind) *outFieldKind = CollectionKind::Str;
        return true;
    }
    if (isListTypeName(resolved)) {
        if (outFieldKind) *outFieldKind = CollectionKind::List;
        return true;
    }
    if (isMapTypeName(resolved)) {
        if (outFieldKind) *outFieldKind = CollectionKind::Map;
        return true;
    }
    if (isSetTypeName(resolved)) {
        if (outFieldKind) *outFieldKind = CollectionKind::Set;
        return true;
    }
    return false;
}

// ===== Record ARC field retain/release (#854 Layer 2) =====

bool CodeGen::recordHasArcFields(llvm::StructType *st) {
    if (!st || !st->hasName())
        return false;
    auto it = record_types_.find(st->getName().str());
    if (it == record_types_.end())
        return false;
    for (const auto &fd : it->second.fields) {
        if (!fd.type) continue;
        if (fieldTypeIsArcManaged(fd.type->toString()))
            return true;
    }
    return false;
}

// Trace an InsertValue chain to find the original value inserted at a given
// field index.  LLVM's ConstantFolder does NOT fold
//   ExtractValue(InsertValue(agg, v, {i}), {i}) → v
// for non-constant aggregates, so the ExtractValue result is a fresh
// instruction that is never in arc_owned_values_ / arc_str_owned_values_.
// This helper recovers the original inserted operand so that ownership
// checks work correctly for record construction IR.
static llvm::Value *traceInsertValueField(llvm::Value *agg, unsigned idx) {
    while (auto *iv = llvm::dyn_cast<llvm::InsertValueInst>(agg)) {
        if (iv->getIndices().size() == 1 && iv->getIndices()[0] == idx)
            return iv->getInsertedValueOperand();
        agg = iv->getAggregateOperand();
    }
    return nullptr;
}

void CodeGen::emitRecordArcFieldsRetain(llvm::Value *recordVal,
                                          llvm::StructType *st) {
    if (!st || !st->hasName())
        return;
    auto it = record_types_.find(st->getName().str());
    if (it == record_types_.end())
        return;
    const auto &info = it->second;
    for (unsigned i = 0; i < info.fields.size(); ++i) {
        const auto &fd = info.fields[i];
        if (!fd.type) continue;
        CollectionKind fk;
        if (!fieldTypeIsArcManaged(fd.type->toString(), &fk))
            continue;
        llvm::Value *fieldVal = builder_.CreateExtractValue(
            recordVal, i, fd.name + ".record_retain");
        // Skip freshly-owned values: inline list/str allocations (in arc_owned_values_ /
        // arc_str_owned_values_) are being transferred into the record — the record
        // becomes the sole owner so no retain is needed.  Named-variable values (loaded
        // from arc_backed_vars_ / arc_str_managed_vars_ allocas) are reference copies
        // that require a retain.
        //
        // For InsertValue construction IR, CreateExtractValue yields a fresh
        // instruction not in the owned sets.  Trace the original operand instead.
        llvm::Value *checkVal = fieldVal;
        if (llvm::isa<llvm::InsertValueInst>(recordVal))
            if (auto *orig = traceInsertValueField(recordVal, i))
                checkVal = orig;
        if (arc_owned_values_.count(checkVal) || arc_str_owned_values_.count(checkVal))
            continue;
        // Null guard — freshly-inserted fields are always non-null in
        // practice but cheap to defend against for robustness.
        auto *isNull = builder_.CreateICmpEQ(
            fieldVal,
            llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
            fd.name + ".record_retain_null");
        auto *fn = builder_.GetInsertBlock()->getParent();
        auto *retainBB = llvm::BasicBlock::Create(*ctx_, "record.field_retain", fn);
        auto *skipBB = llvm::BasicBlock::Create(*ctx_, "record.field_retain_skip", fn);
        builder_.CreateCondBr(isNull, skipBB, retainBB);
        builder_.SetInsertPoint(retainBB);
        auto *hdr = (fk == CollectionKind::Str) ? emitStrGetHeaderFromData(fieldVal)
                                                 : emitArcGetHeaderFromData(fieldVal);
        emitArcRetain(hdr, /*atomic=*/false);
        builder_.CreateBr(skipBB);
        builder_.SetInsertPoint(skipBB);
    }
}

void CodeGen::emitRecordArcFieldsRelease(llvm::Value *recordVal,
                                           llvm::StructType *st) {
    if (!st || !st->hasName())
        return;
    auto it = record_types_.find(st->getName().str());
    if (it == record_types_.end())
        return;
    const auto &info = it->second;
    for (unsigned i = 0; i < info.fields.size(); ++i) {
        const auto &fd = info.fields[i];
        if (!fd.type) continue;
        CollectionKind fk;
        if (!fieldTypeIsArcManaged(fd.type->toString(), &fk))
            continue;
        llvm::Value *fieldVal = builder_.CreateExtractValue(
            recordVal, i, fd.name + ".record_release");
        emitArcReleaseLoadedElement(fieldVal, fk, fd.type->toString(), fd.name);
    }
}

// #1640: Releases the active payload slot of a tagged-union (Result/Option)
// subject alloca at scope exit. Loads the struct, extracts the i1 tag, and
// branches: tag=true picks Ok/Some (slot index 1), tag=false picks Err
// (slot index 2 for Result; Option's None has no payload). Each active
// slot is released via `emitArcReleaseLoadedElement` only when the
// corresponding inner type is ARC-managed; non-ARC slots emit no IR in
// their branch.
void CodeGen::emitTaggedUnionRelease(llvm::AllocaInst *alloca,
                                      const std::string &sourceTypeName) {
    if (!alloca) return;
    llvm::Type *ty = alloca->getAllocatedType();
    auto *st = llvm::dyn_cast<llvm::StructType>(ty);
    if (!st) return;

    const bool isResult = isResultType(ty);
    const bool isOption = isOptionType(ty);
    if (!isResult && !isOption) return;

    // Resolve inner Ok/Err (or Some) type names from the source type string.
    // No fallback to reverseResolveTypeName: per #1156, that channel is lossy
    // and would misclassify Option<List<int>> as Option<str> (wrong header
    // offset, heap corruption).
    std::string resolvedSource = resolveTypeAlias(sourceTypeName);
    std::string okName, errName;
    if (isOption && resolvedSource.size() > 1 && resolvedSource.back() == '?') {
        // T? shorthand for Option<T>
        okName = trimTypeNameSpaces(
            resolvedSource.substr(0, resolvedSource.size() - 1));
    } else {
        std::string head;
        std::vector<std::string> innerArgs;
        if (splitGenericTypeName(resolvedSource, head, innerArgs)) {
            if (isResult && head == "Result") {
                if (!innerArgs.empty()) okName = trimTypeNameSpaces(innerArgs[0]);
                if (innerArgs.size() >= 2) errName = trimTypeNameSpaces(innerArgs[1]);
            } else if (isOption && head == "Option") {
                if (!innerArgs.empty()) okName = trimTypeNameSpaces(innerArgs[0]);
            }
        }
    }

    CollectionKind okKind = CollectionKind::List;
    CollectionKind errKind = CollectionKind::List;
    const bool okArc = !okName.empty() && fieldTypeIsArcManaged(okName, &okKind);
    const bool errArc = isResult && !errName.empty() &&
                         fieldTypeIsArcManaged(errName, &errKind);
    if (!okArc && !errArc) return;

    auto *parentFn = builder_.GetInsertBlock()->getParent();
    auto *okBB = llvm::BasicBlock::Create(*ctx_, "tu.ok", parentFn);
    auto *errBB = llvm::BasicBlock::Create(*ctx_, "tu.err", parentFn);
    auto *mergeBB = llvm::BasicBlock::Create(*ctx_, "tu.merge", parentFn);

    llvm::Value *loaded = builder_.CreateLoad(ty, alloca, "tu.load");
    llvm::Value *tag = builder_.CreateExtractValue(loaded, 0, "tu.tag");
    builder_.CreateCondBr(tag, okBB, errBB);

    // Ok / Some path
    builder_.SetInsertPoint(okBB);
    if (okArc) {
        llvm::Value *okVal = builder_.CreateExtractValue(loaded, 1, "tu.ok_val");
        emitArcReleaseLoadedElement(okVal, okKind, okName, "tu.ok");
    }
    builder_.CreateBr(mergeBB);

    // Err path (Result only; Option's None has no payload)
    builder_.SetInsertPoint(errBB);
    if (errArc) {
        llvm::Value *errVal = builder_.CreateExtractValue(loaded, 2, "tu.err_val");
        emitArcReleaseLoadedElement(errVal, errKind, errName, "tu.err");
    }
    builder_.CreateBr(mergeBB);

    builder_.SetInsertPoint(mergeBB);
}

bool CodeGen::elementTypeIsArcManaged(llvm::Value *containerPtr,
                                       CollectionKind containerKind,
                                       CollectionKind *outElemKind) {
    // Callers reach this only after `objPtr->getType() != ptrTy_` has
    // already been rejected with `codegenError("index assignment requires
    // list or map")`, so `containerPtr` is guaranteed non-null and of
    // pointer type. An assert documents the invariant without paying for
    // a runtime check in release builds.
    assert(containerPtr && containerPtr->getType() == ptrTy_ &&
           "elementTypeIsArcManaged expects a pointer-typed container");

    auto *meta = getMeta(containerPtr);
    if (!meta)
        return false;

    const std::string *elemTypeName = nullptr;
    switch (containerKind) {
    case CollectionKind::List:
        elemTypeName = &meta->list_elem_type_name;
        break;
    case CollectionKind::Map:
        elemTypeName = &meta->map_value_type_name;
        break;
    case CollectionKind::Set:
        elemTypeName = &meta->set_elem_type_name;
        break;
    case CollectionKind::Str:
        // str is a scalar, not a container — no nested element type.
        return false;
    }
    if (!elemTypeName)
        return false;
    return fieldTypeIsArcManaged(*elemTypeName, outElemKind);
}

// ===== Weak reference operations =====

bool CodeGen::isWeakTypeName(const std::string &typeName) {
    return typeName.size() > 5 && typeName.compare(0, 5, "weak ") == 0;
}

std::string CodeGen::weakInnerTypeName(const std::string &typeName) {
    return typeName.substr(5);
}

void CodeGen::markWeakManaged(llvm::AllocaInst *alloca) {
    weak_managed_vars_.insert(alloca);
}

bool CodeGen::isWeakManaged(llvm::AllocaInst *alloca) const {
    return weak_managed_vars_.count(alloca) > 0;
}

void CodeGen::emitWeakRetain(llvm::Value *headerPtr) {
    // Skip immortal objects (strong_count == ARC_IMMORTAL) — e.g. string literals
    auto *strongPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 0, "weak_retain_strong");
    auto *strong = builder_.CreateLoad(i64Ty_, strongPtr, "weak_retain_sc");
    auto *isImmortal = builder_.CreateICmpEQ(strong, llvm::ConstantInt::get(i64Ty_, ARC_IMMORTAL), "weak_retain_immortal");

    auto *fn = builder_.GetInsertBlock()->getParent();
    auto *retainBB = llvm::BasicBlock::Create(*ctx_, "weak.retain", fn);
    auto *doneBB = llvm::BasicBlock::Create(*ctx_, "weak.retain.done", fn);
    builder_.CreateCondBr(isImmortal, doneBB, retainBB);

    builder_.SetInsertPoint(retainBB);
    auto *weakPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 1, "weak_retain_ptr");
    builder_.CreateAtomicRMW(llvm::AtomicRMWInst::Add, weakPtr,
                             llvm::ConstantInt::get(i64Ty_, 1),
                             llvm::MaybeAlign(),
                             llvm::AtomicOrdering::SequentiallyConsistent);
    builder_.CreateBr(doneBB);

    builder_.SetInsertPoint(doneBB);
}

void CodeGen::emitWeakRelease(llvm::Value *headerPtr) {
    // Skip immortal objects (strong_count == ARC_IMMORTAL) — e.g. string literals
    auto *strongCheckPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 0, "weak_rel_strong");
    auto *strongCheck = builder_.CreateLoad(i64Ty_, strongCheckPtr, "weak_rel_sc");
    auto *isImmortal = builder_.CreateICmpEQ(strongCheck, llvm::ConstantInt::get(i64Ty_, ARC_IMMORTAL), "weak_rel_immortal");

    auto *fn = builder_.GetInsertBlock()->getParent();
    auto *releaseBB = llvm::BasicBlock::Create(*ctx_, "weak.release.body", fn);
    auto *doneBB = llvm::BasicBlock::Create(*ctx_, "weak.done", fn);
    builder_.CreateCondBr(isImmortal, doneBB, releaseBB);

    builder_.SetInsertPoint(releaseBB);
    auto *weakPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 1, "weak_rel_ptr");
    auto *oldWeak = builder_.CreateAtomicRMW(
        llvm::AtomicRMWInst::Sub, weakPtr,
        llvm::ConstantInt::get(i64Ty_, 1),
        llvm::MaybeAlign(),
        llvm::AtomicOrdering::SequentiallyConsistent);
    auto *isZeroWeak = builder_.CreateICmpEQ(oldWeak, llvm::ConstantInt::get(i64Ty_, 1), "weak_zero");

    auto *checkStrongBB = llvm::BasicBlock::Create(*ctx_, "weak.check_strong", fn);
    auto *freeBB = llvm::BasicBlock::Create(*ctx_, "weak.free", fn);
    builder_.CreateCondBr(isZeroWeak, checkStrongBB, doneBB);

    builder_.SetInsertPoint(checkStrongBB);
    auto *strongPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 0, "weak_strong_ptr");
    auto *strong = builder_.CreateLoad(i64Ty_, strongPtr, "weak_strong");
    strong->setAtomic(llvm::AtomicOrdering::Acquire);
    auto *isZeroStrong = builder_.CreateICmpEQ(strong, llvm::ConstantInt::get(i64Ty_, 0), "strong_zero");
    builder_.CreateCondBr(isZeroStrong, freeBB, doneBB);

    builder_.SetInsertPoint(freeBB);
    // Decrement the ARC live-count balance counter inline (weak release path).
    emitArcCounterDeltaIR(builder_, i64Ty_, ptrTy_, -1);
    builder_.CreateCall(getStdlibFree(), {headerPtr});
    builder_.CreateBr(doneBB);

    builder_.SetInsertPoint(doneBB);
}

llvm::Value *CodeGen::emitWeakUpgrade(llvm::Value *headerPtr,
                                       const std::string &innerTypeName) {
    const std::string resolvedInner = resolveTypeAlias(innerTypeName);
    auto *innerTy = resolveType(innerTypeName);
    auto *optionTy = getOptionType(innerTy);

    auto *resultAlloca = builder_.CreateAlloca(optionTy, nullptr, "weak_upgrade_result");

    auto *strongPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 0, "weak_up_strong_ptr");

    auto *fn = builder_.GetInsertBlock()->getParent();
    auto *immortalBB = llvm::BasicBlock::Create(*ctx_, "weak.immortal", fn);
    auto *loopBB = llvm::BasicBlock::Create(*ctx_, "weak.cas_loop", fn);
    auto *tryIncBB = llvm::BasicBlock::Create(*ctx_, "weak.try_inc", fn);
    auto *successBB = llvm::BasicBlock::Create(*ctx_, "weak.success", fn);
    auto *deadBB = llvm::BasicBlock::Create(*ctx_, "weak.dead", fn);
    auto *doneBB = llvm::BasicBlock::Create(*ctx_, "weak.upgrade_done", fn);

    // Immortal objects are always alive — skip CAS and return Some directly
    auto *initCur = builder_.CreateLoad(i64Ty_, strongPtr, "weak_up_init");
    initCur->setAtomic(llvm::AtomicOrdering::Acquire);
    auto *isImmortal = builder_.CreateICmpEQ(initCur, llvm::ConstantInt::get(i64Ty_, ARC_IMMORTAL), "weak_up_immortal");
    builder_.CreateCondBr(isImmortal, immortalBB, loopBB);

    // Immortal path: return Some(data_ptr) without incrementing
    builder_.SetInsertPoint(immortalBB);
    // str uses StringHeader (24 bytes); other ARC types use ArcHeader (16 bytes).
    auto *immortalDataPtr = (resolvedInner == "str")
        ? emitStrGetDataPtr(headerPtr)
        : emitArcGetDataPtr(headerPtr);
    auto *immortalSome = buildSomeValue(immortalDataPtr, optionTy);
    builder_.CreateStore(immortalSome, resultAlloca);
    builder_.CreateBr(doneBB);

    // CAS loop
    builder_.SetInsertPoint(loopBB);
    auto *cur = builder_.CreateLoad(i64Ty_, strongPtr, "weak_up_cur");
    cur->setAtomic(llvm::AtomicOrdering::Acquire);
    auto *isAlive = builder_.CreateICmpSGT(cur, llvm::ConstantInt::get(i64Ty_, 0), "weak_alive");
    builder_.CreateCondBr(isAlive, tryIncBB, deadBB);

    // Try CAS: compare_exchange(strongPtr, cur, cur+1)
    builder_.SetInsertPoint(tryIncBB);
    auto *desired = builder_.CreateAdd(cur, llvm::ConstantInt::get(i64Ty_, 1), "weak_desired");
    auto *cmpxchg = builder_.CreateAtomicCmpXchg(
        strongPtr, cur, desired,
        llvm::MaybeAlign(),
        llvm::AtomicOrdering::AcquireRelease,
        llvm::AtomicOrdering::Monotonic);
    auto *success = builder_.CreateExtractValue(cmpxchg, 1, "weak_cas_ok");
    builder_.CreateCondBr(success, successBB, loopBB);

    // Success: strong_count incremented, return Some(data_ptr)
    builder_.SetInsertPoint(successBB);
    auto *dataPtr = (resolvedInner == "str")
        ? emitStrGetDataPtr(headerPtr)
        : emitArcGetDataPtr(headerPtr);
    auto *someVal = buildSomeValue(dataPtr, optionTy);
    builder_.CreateStore(someVal, resultAlloca);
    builder_.CreateBr(doneBB);

    // Dead: strong_count == 0, return None
    builder_.SetInsertPoint(deadBB);
    auto *noneVal = buildNoneValue(optionTy);
    builder_.CreateStore(noneVal, resultAlloca);
    builder_.CreateBr(doneBB);

    builder_.SetInsertPoint(doneBB);
    return builder_.CreateLoad(optionTy, resultAlloca, "weak_upgraded");
}

void CodeGen::emitWeakReleaseVar(const std::string &name, llvm::AllocaInst *alloca) {
    auto *val = builder_.CreateLoad(ptrTy_, alloca, name + ".weak_cleanup");
    auto *isNull = builder_.CreateICmpEQ(val,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
        "weak_null_check");

    auto *fn = builder_.GetInsertBlock()->getParent();
    auto *releaseBB = llvm::BasicBlock::Create(*ctx_, "weak.var_release", fn);
    auto *skipBB = llvm::BasicBlock::Create(*ctx_, "weak.var_skip", fn);
    builder_.CreateCondBr(isNull, skipBB, releaseBB);

    builder_.SetInsertPoint(releaseBB);
    emitWeakRelease(val);
    builder_.CreateBr(skipBB);

    builder_.SetInsertPoint(skipBB);
}

void CodeGen::retainArcValue(llvm::Value *val) {
    if (tryRetainArcSource(val))
        return;
    const bool isStr = arc_str_owned_values_.count(val) > 0;
    auto *hdr = isStr ? emitStrGetHeaderFromData(val) : emitArcGetHeaderFromData(val);
    emitArcRetain(hdr, /*atomic=*/false);
}

bool CodeGen::tryRetainArcSource(llvm::Value *val) {
    // Case 1: LoadInst from an ARC-managed alloca
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
        auto *srcAlloca = llvm::dyn_cast<llvm::AllocaInst>(load->getPointerOperand());
        if (srcAlloca && isArcManaged(srcAlloca)) {
            const bool isStr = arc_str_managed_vars_.count(srcAlloca) > 0;
            auto *hdr = isStr ? emitStrGetHeaderFromData(val)
                              : emitArcGetHeaderFromData(val);
            emitArcRetain(hdr, isArcAtomic(val));
            return true;
        }
    }
    // Case 2: Value produced by emitArcAlloc (e.g., f-string, interpolated string)
    // These already have strong_count=1 from allocation, no retain needed,
    // but signal to caller that this is ARC-owned
    if (arc_owned_values_.count(val))
        return true;
    // Case 2b: str value produced by a runtime function (makeString-backed).
    // strong_count=1 from makeString, so no retain needed.
    if (arc_str_owned_values_.count(val))
        return true;
    // Case 3: ExtractValueInst — record/tuple field access (CreateExtractValue).
    // Guard on collection metadata so closures, weak refs, and other non-ARC
    // ptrTy_ values are not incorrectly retained (#999).
    if (llvm::isa<llvm::ExtractValueInst>(val)) {
        auto *meta = getMeta(val);
        if (meta && (meta->list_elem || meta->map_key || meta->set_elem)) {
            auto *hdr = emitArcGetHeaderFromData(val);
            emitArcRetain(hdr, /*atomic=*/false);
            return true;
        }
    }
    // Case 4: GEP-loaded container element borrowed from a long-lived
    // container. Two metadata gates dispatch to different headers:
    //   - list_elem / map_key / map_value / set_elem → ArcHeader (-16)
    //   - str_elem                                   → StringHeader (-24)
    // Gated on metadata so non-ARC ptrTy_ loads (weak refs, bare fn pointers)
    // are not incorrectly retained — same guard pattern as Case 3 (#1266).
    if (llvm::isa<llvm::LoadInst>(val) && val->getType() == ptrTy_) {
        auto *meta = getMeta(val);
        if (meta) {
            const bool isArcContainerElem =
                meta->list_elem || meta->map_key ||
                meta->map_value || meta->set_elem;
            const bool isStrElem = meta->str_elem;
            if (isArcContainerElem || isStrElem) {
                auto *hdr = isStrElem ? emitStrGetHeaderFromData(val)
                                      : emitArcGetHeaderFromData(val);
                emitArcRetain(hdr, isArcAtomic(val));
                return true;
            }
        }
    }
    return false;
}

llvm::FunctionCallee CodeGen::getOrCreateCollectionDestructor(CollectionKind kind,
                                                               const std::string &elemSig,
                                                               const std::string &valSig) {
    auto cacheKey = std::make_tuple(kind, elemSig, valSig);
    auto it = arc_destructors_cache_.find(cacheKey);
    if (it != arc_destructors_cache_.end())
        return it->second;

    // Destructor signature: void(ptr dataPtr)
    // dataPtr points to the type-specific header (after ARC header)
    auto *dtorTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
    std::string name;

    switch (kind) {
    case CollectionKind::List:
        name = "__ry_arc_dtor_list";
        if (elemSig == "str") name += "_str";
        break;
    case CollectionKind::Map:
        name = "__ry_arc_dtor_map";
        if (elemSig == "str") name += "_kstr";
        if (valSig == "str") name += "_vstr";
        break;
    case CollectionKind::Set:
        name = "__ry_arc_dtor_set";
        if (elemSig == "str") name += "_str";
        break;
    case CollectionKind::Str:
        llvm_unreachable("getOrCreateCollectionDestructor called with CollectionKind::Str");
    }

    auto *dtorFn = llvm::Function::Create(
        dtorTy, llvm::Function::InternalLinkage, name, mod_.get());
    dtorFn->addFnAttr(llvm::Attribute::NoUnwind);

    auto *entryBB = llvm::BasicBlock::Create(*ctx_, "entry", dtorFn);

    // Save current builder state
    auto *savedBB = builder_.GetInsertBlock();
    auto savedPt = builder_.GetInsertPoint();
    builder_.SetInsertPoint(entryBB);

    auto *dataPtr = dtorFn->getArg(0);
    auto freeFn = getStdlibFree();

    // Helper: ARC-release each collection element in a dense ptr-array
    // [0, len) via the inner destructor.  Split from `emitStrElemLoop`
    // (below) because str uses a different header layout (StringHeader
    // at elem - 24) and needs no inner destructor. (#1242)
    auto emitCollectionElemLoop = [&](llvm::Value *arrayPtr, llvm::Value *len,
                                       const char *tag,
                                       CollectionKind innerKind,
                                       const std::string &innerElemSig,
                                       const std::string &innerValSig) {
        auto *loopHdrBB  = llvm::BasicBlock::Create(*ctx_,
            std::string("dtor_clhdr_") + tag, dtorFn);
        auto *loopBodyBB = llvm::BasicBlock::Create(*ctx_,
            std::string("dtor_clbody_") + tag, dtorFn);
        auto *doRelBB    = llvm::BasicBlock::Create(*ctx_,
            std::string("dtor_cldorel_") + tag, dtorFn);
        auto *latchBB    = llvm::BasicBlock::Create(*ctx_,
            std::string("dtor_cllatch_") + tag, dtorFn);
        auto *postBB     = llvm::BasicBlock::Create(*ctx_,
            std::string("dtor_clpost_") + tag, dtorFn);

        auto *prevBB = builder_.GetInsertBlock();
        builder_.CreateBr(loopHdrBB);

        builder_.SetInsertPoint(loopHdrBB);
        auto *iPhi = builder_.CreatePHI(i64Ty_, 2,
            std::string("dtor_ci_") + tag);
        iPhi->addIncoming(llvm::ConstantInt::get(i64Ty_, 0), prevBB);
        auto *done = builder_.CreateICmpEQ(iPhi, len,
            std::string("dtor_cdone_") + tag);
        builder_.CreateCondBr(done, postBB, loopBodyBB);

        builder_.SetInsertPoint(loopBodyBB);
        auto *elemGEP = builder_.CreateGEP(ptrTy_, arrayPtr, {iPhi},
            std::string("dtor_cegep_") + tag);
        auto *elem = builder_.CreateLoad(ptrTy_, elemGEP,
            std::string("dtor_celem_") + tag);
        auto *isNull = builder_.CreateICmpEQ(elem,
            llvm::ConstantPointerNull::get(
                llvm::cast<llvm::PointerType>(ptrTy_)),
            std::string("dtor_cnull_") + tag);
        builder_.CreateCondBr(isNull, latchBB, doRelBB);

        builder_.SetInsertPoint(doRelBB);
        auto *hdr = emitArcGetHeaderFromData(elem);
        auto innerDtor = getOrCreateCollectionDestructor(innerKind, innerElemSig, innerValSig);
        emitArcRelease(hdr, /*atomic=*/false, innerDtor, nullptr);
        // emitArcRelease leaves builder_ in its doneBB
        builder_.CreateBr(latchBB);

        builder_.SetInsertPoint(latchBB);
        auto *iNext = builder_.CreateAdd(iPhi,
            llvm::ConstantInt::get(i64Ty_, 1),
            std::string("dtor_cinext_") + tag);
        iPhi->addIncoming(iNext, latchBB);
        builder_.CreateBr(loopHdrBB);

        builder_.SetInsertPoint(postBB);
    };

    // Helper: parse `sig` (e.g. "List<int>", "Map<str,int>", "Set<List<int>>")
    // into inner kind + inner elem/val sigs and emit a collection release
    // loop, or fall through for non-ARC-managed element types.  str element
    // types dispatch to `emitStrElemLoop` (below) instead.
    auto emitInnerReleaseLoop = [&](llvm::Value *arrayPtr, llvm::Value *len,
                                     const char *tag,
                                     const std::string &sig) -> bool {
        if (sig.empty()) return false;
        std::string resolved = resolveTypeAlias(sig);
        if (resolved.size() >= 2 && resolved.front() == '(' &&
            resolved.back() == ')') {
            auto *tupleTy = llvm::dyn_cast<llvm::StructType>(resolveType(resolved));
            if (!tupleTy) return false;
            emitTupleElemReleaseLoop(arrayPtr, len, tag, resolved, tupleTy);
            return true;
        }
        CollectionKind innerKind;
        if (!fieldTypeIsArcManaged(resolved, &innerKind)) return false;
        if (innerKind == CollectionKind::Str) return false;  // str path handled by caller

        std::string innerElemSig;
        std::string innerValSig;
        std::string head;
        std::vector<std::string> innerArgs;
        if (splitGenericTypeName(resolved, head, innerArgs)) {
            if ((innerKind == CollectionKind::List || innerKind == CollectionKind::Set) &&
                !innerArgs.empty()) {
                innerElemSig = resolveTypeAlias(innerArgs[0]);
            } else if (innerKind == CollectionKind::Map && innerArgs.size() >= 2) {
                innerElemSig = resolveTypeAlias(innerArgs[0]);
                innerValSig  = resolveTypeAlias(innerArgs[1]);
            }
        }
        emitCollectionElemLoop(arrayPtr, len, tag, innerKind, innerElemSig, innerValSig);
        return true;
    };

    // Helper: emit a counted loop that ARC-releases each str element in a
    // dense ptr-array [0, len).  After the call builder_ is in post_loop BB.
    auto emitStrElemLoop = [&](llvm::Value *arrayPtr, llvm::Value *len,
                                const char *tag) {
        auto *loopHdrBB  = llvm::BasicBlock::Create(*ctx_,
            std::string("dtor_lhdr_") + tag, dtorFn);
        auto *loopBodyBB = llvm::BasicBlock::Create(*ctx_,
            std::string("dtor_lbody_") + tag, dtorFn);
        auto *doRelBB    = llvm::BasicBlock::Create(*ctx_,
            std::string("dtor_dorel_") + tag, dtorFn);
        auto *latchBB    = llvm::BasicBlock::Create(*ctx_,
            std::string("dtor_latch_") + tag, dtorFn);
        auto *postBB     = llvm::BasicBlock::Create(*ctx_,
            std::string("dtor_post_") + tag, dtorFn);

        auto *prevBB = builder_.GetInsertBlock();
        builder_.CreateBr(loopHdrBB);

        // loop header: phi i=0/i_next, exit when i == len
        builder_.SetInsertPoint(loopHdrBB);
        auto *iPhi = builder_.CreatePHI(i64Ty_, 2,
            std::string("dtor_i_") + tag);
        iPhi->addIncoming(llvm::ConstantInt::get(i64Ty_, 0), prevBB);
        auto *done = builder_.CreateICmpEQ(iPhi, len,
            std::string("dtor_done_") + tag);
        builder_.CreateCondBr(done, postBB, loopBodyBB);

        // loop body: load element, skip if null
        builder_.SetInsertPoint(loopBodyBB);
        auto *elemGEP = builder_.CreateGEP(ptrTy_, arrayPtr, {iPhi},
            std::string("dtor_egep_") + tag);
        auto *elem = builder_.CreateLoad(ptrTy_, elemGEP,
            std::string("dtor_elem_") + tag);
        auto *isNull = builder_.CreateICmpEQ(elem,
            llvm::ConstantPointerNull::get(
                llvm::cast<llvm::PointerType>(ptrTy_)),
            std::string("dtor_null_") + tag);
        builder_.CreateCondBr(isNull, latchBB, doRelBB);

        // do_rel: ARC-release the str element
        builder_.SetInsertPoint(doRelBB);
        auto *hdr = emitStrGetHeaderFromData(elem);
        emitArcRelease(hdr, /*atomic=*/false, {}, nullptr);
        // emitArcRelease leaves builder_ in its doneBB
        builder_.CreateBr(latchBB);

        // latch: increment i and loop back
        builder_.SetInsertPoint(latchBB);
        auto *iNext = builder_.CreateAdd(iPhi,
            llvm::ConstantInt::get(i64Ty_, 1),
            std::string("dtor_inext_") + tag);
        iPhi->addIncoming(iNext, latchBB);
        builder_.CreateBr(loopHdrBB);

        builder_.SetInsertPoint(postBB);
    };

    switch (kind) {
    case CollectionKind::List: {
        // ListHeader { i64 len, i64 cap, ptr data }
        auto *lenField = builder_.CreateStructGEP(listHeaderTy_, dataPtr, 0, "dtor_len_f");
        auto *len = builder_.CreateLoad(i64Ty_, lenField, "dtor_len");
        auto *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, dataPtr, 2, "dtor_data_field");
        auto *dataBuf = builder_.CreateLoad(ptrTy_, dataPtrField, "dtor_data_buf");
        if (elemSig == "str")
            emitStrElemLoop(dataBuf, len, "lst");
        else
            emitInnerReleaseLoop(dataBuf, len, "lst", elemSig);
        builder_.CreateCall(freeFn, {dataBuf});
        break;
    }
    case CollectionKind::Map: {
        // MapHeader { i64 len, i64 cap, ptr keys, ptr vals, i64 bucket_count, ptr buckets }
        auto *lenField = builder_.CreateStructGEP(mapHeaderTy_, dataPtr, 0, "dtor_len_f");
        auto *len = builder_.CreateLoad(i64Ty_, lenField, "dtor_len");

        auto *keysField = builder_.CreateStructGEP(mapHeaderTy_, dataPtr, 2, "dtor_keys_field");
        auto *keys = builder_.CreateLoad(ptrTy_, keysField, "dtor_keys");
        if (elemSig == "str")
            emitStrElemLoop(keys, len, "mkey");
        else
            emitInnerReleaseLoop(keys, len, "mkey", elemSig);
        builder_.CreateCall(freeFn, {keys});

        auto *valsField = builder_.CreateStructGEP(mapHeaderTy_, dataPtr, 3, "dtor_vals_field");
        auto *vals = builder_.CreateLoad(ptrTy_, valsField, "dtor_vals");
        if (valSig == "str")
            emitStrElemLoop(vals, len, "mval");
        else
            emitInnerReleaseLoop(vals, len, "mval", valSig);
        builder_.CreateCall(freeFn, {vals});

        auto *bucketsField = builder_.CreateStructGEP(mapHeaderTy_, dataPtr, 5, "dtor_buckets_field");
        auto *buckets = builder_.CreateLoad(ptrTy_, bucketsField, "dtor_buckets");
        builder_.CreateCall(freeFn, {buckets});
        break;
    }
    case CollectionKind::Set: {
        // SetHeader { i64 len, i64 cap, ptr elems, i64 bucket_count, ptr buckets }
        auto *lenField = builder_.CreateStructGEP(setHeaderTy_, dataPtr, 0, "dtor_len_f");
        auto *len = builder_.CreateLoad(i64Ty_, lenField, "dtor_len");

        auto *elemsField = builder_.CreateStructGEP(setHeaderTy_, dataPtr, 2, "dtor_elems_field");
        auto *elems = builder_.CreateLoad(ptrTy_, elemsField, "dtor_elems");
        if (elemSig == "str")
            emitStrElemLoop(elems, len, "set");
        else
            emitInnerReleaseLoop(elems, len, "set", elemSig);
        builder_.CreateCall(freeFn, {elems});

        auto *bucketsField = builder_.CreateStructGEP(setHeaderTy_, dataPtr, 4, "dtor_buckets_field");
        auto *buckets = builder_.CreateLoad(ptrTy_, bucketsField, "dtor_buckets");
        builder_.CreateCall(freeFn, {buckets});
        break;
    }
    case CollectionKind::Str:
        llvm_unreachable("getOrCreateCollectionDestructor: Str has no collection destructor");
    }

    builder_.CreateRetVoid();

    // Restore builder state
    if (savedBB)
        builder_.SetInsertPoint(savedBB, savedPt);

    llvm::FunctionCallee callee(dtorTy, dtorFn);
    arc_destructors_cache_[cacheKey] = callee;
    return callee;
}

// ====== Tuple element ARC helpers (#1667) ======
//
// Tuple-element lists (`List<(K, V)>` produced by enumerate/zip/items)
// hold an inline array of tuple struct values, NOT pointers. Per-component
// retain/release dispatch happens by source-level type name so we never
// route a non-ARC component through `fieldTypeIsArcManaged` (which would
// pull in pointer-array predicates that mis-handle inline struct slots).

void CodeGen::emitTupleComponentRetain(llvm::Value *val,
                                        const std::string &fSig) {
    if (fSig.empty() || !val) return;
    const std::string resolved = resolveTypeAlias(fSig);
    if (resolved.empty() || isWeakTypeName(resolved)) return;
    if (resolved == "str") {
        emitArcRetain(emitStrGetHeaderFromData(val), isArcAtomic(val));
    } else if (isListTypeName(resolved) || isMapTypeName(resolved) ||
               isSetTypeName(resolved)) {
        emitArcRetain(emitArcGetHeaderFromData(val), isArcAtomic(val));
    }
    // int / bool / f64 / weak / record / unknown: no-op
}

namespace {
// Build a counted loop over [0, len) inside `fn` that branches into
// `bodyEmitter(iPhi)` for each iteration. The body emitter is responsible
// for branching back to the latch (the lambda receives the latch BB so it
// can fall through naturally). Both `pre` and `post` BBs are returned via
// the builder cursor (post is current after the call).
struct CountedLoopBlocks {
    llvm::BasicBlock *body;
    llvm::BasicBlock *latch;
    llvm::BasicBlock *post;
    llvm::PHINode    *iPhi;
};
} // anonymous

static CountedLoopBlocks emitCountedLoopShell(llvm::IRBuilder<> &b,
                                               llvm::LLVMContext &ctx,
                                               llvm::Function *fn,
                                               llvm::Value *len,
                                               llvm::Type *i64Ty,
                                               const std::string &tagPrefix) {
    auto *hdr   = llvm::BasicBlock::Create(ctx, tagPrefix + "_hdr",   fn);
    auto *body  = llvm::BasicBlock::Create(ctx, tagPrefix + "_body",  fn);
    auto *latch = llvm::BasicBlock::Create(ctx, tagPrefix + "_latch", fn);
    auto *post  = llvm::BasicBlock::Create(ctx, tagPrefix + "_post",  fn);

    auto *prevBB = b.GetInsertBlock();
    b.CreateBr(hdr);

    b.SetInsertPoint(hdr);
    auto *iPhi = b.CreatePHI(i64Ty, 2, tagPrefix + "_i");
    iPhi->addIncoming(llvm::ConstantInt::get(i64Ty, 0), prevBB);
    auto *done = b.CreateICmpEQ(iPhi, len, tagPrefix + "_done");
    b.CreateCondBr(done, post, body);

    b.SetInsertPoint(latch);
    auto *iNext = b.CreateAdd(iPhi, llvm::ConstantInt::get(i64Ty, 1),
                               tagPrefix + "_inext");
    iPhi->addIncoming(iNext, latch);
    b.CreateBr(hdr);

    b.SetInsertPoint(body);
    return {body, latch, post, iPhi};
}

void CodeGen::emitTupleElemReleaseLoop(llvm::Value *arrayPtr, llvm::Value *len,
                                        const char *tag,
                                        const std::string &tupleSig,
                                        llvm::StructType *tupleTy) {
    if (!arrayPtr || !len || !tupleTy) return;
    std::vector<std::string> components = splitTupleSig(tupleSig);
    if (components.empty()) return;

    // Skip the loop entirely if no component needs ARC release.
    bool anyArc = false;
    for (const auto &c : components) {
        const std::string r = resolveTypeAlias(c);
        if (r.empty() || isWeakTypeName(r)) continue;
        if (r == "str" || isListTypeName(r) || isMapTypeName(r) ||
            isSetTypeName(r) ||
            (r.size() >= 2 && r.front() == '(' && r.back() == ')')) {
            anyArc = true;
            break;
        }
    }
    if (!anyArc) return;

    llvm::Function *fn = builder_.GetInsertBlock()->getParent();
    std::string tagPrefix = std::string("dtor_tup_") + tag;
    auto loop = emitCountedLoopShell(builder_, *ctx_, fn, len, i64Ty_,
                                       tagPrefix);

    // body: GEP into the i'th tuple slot, then release each component.
    auto *slotPtr = builder_.CreateGEP(tupleTy, arrayPtr, {loop.iPhi},
                                         tagPrefix + "_slot");
    const unsigned n = static_cast<unsigned>(
        std::min<size_t>(components.size(), tupleTy->getNumElements()));
    for (unsigned i = 0; i < n; ++i) {
        const std::string fSig = resolveTypeAlias(components[i]);
        if (fSig.empty() || isWeakTypeName(fSig)) continue;

        const bool isStr  = (fSig == "str");
        const bool isColl = isListTypeName(fSig) || isMapTypeName(fSig) ||
                            isSetTypeName(fSig);
        const bool isTup  = (fSig.size() >= 2 && fSig.front() == '(' &&
                              fSig.back() == ')');
        if (!isStr && !isColl && !isTup) continue;

        auto *fieldGEP = builder_.CreateStructGEP(tupleTy, slotPtr, i,
            tagPrefix + "_f" + std::to_string(i));

        if (isTup) {
            // Recursive tuple component: load nothing (inline struct), recurse
            // with the field's own StructType.
            auto *nestedTy = llvm::dyn_cast<llvm::StructType>(
                tupleTy->getElementType(i));
            if (!nestedTy) continue;
            // Treat a single nested tuple slot as a 1-element array.
            llvm::Value *one = llvm::ConstantInt::get(i64Ty_, 1);
            std::string subTag = std::string(tag) + "_n" + std::to_string(i);
            emitTupleElemReleaseLoop(fieldGEP, one, subTag.c_str(), fSig,
                                       nestedTy);
            continue;
        }

        // ARC pointer component (str / List / Map / Set): load, null-guard,
        // release with the appropriate header offset.
        auto *val = builder_.CreateLoad(ptrTy_, fieldGEP,
            tagPrefix + "_v" + std::to_string(i));
        auto *nullBB = llvm::BasicBlock::Create(*ctx_,
            tagPrefix + "_skip" + std::to_string(i), fn);
        auto *relBB  = llvm::BasicBlock::Create(*ctx_,
            tagPrefix + "_rel"  + std::to_string(i), fn);
        auto *isNull = builder_.CreateICmpEQ(val,
            llvm::ConstantPointerNull::get(
                llvm::cast<llvm::PointerType>(ptrTy_)),
            tagPrefix + "_isnull" + std::to_string(i));
        builder_.CreateCondBr(isNull, nullBB, relBB);

        builder_.SetInsertPoint(relBB);
        if (isStr) {
            emitArcRelease(emitStrGetHeaderFromData(val),
                            /*atomic=*/false, {}, nullptr);
        } else {
            // Resolve inner destructor for nested collection.
            std::string head;
            std::vector<std::string> innerArgs;
            CollectionKind innerKind = CollectionKind::List;
            std::string innerElemSig, innerValSig;
            if (isListTypeName(fSig)) innerKind = CollectionKind::List;
            else if (isMapTypeName(fSig)) innerKind = CollectionKind::Map;
            else if (isSetTypeName(fSig)) innerKind = CollectionKind::Set;
            if (splitGenericTypeName(fSig, head, innerArgs)) {
                if ((innerKind == CollectionKind::List ||
                     innerKind == CollectionKind::Set) &&
                    !innerArgs.empty()) {
                    innerElemSig = resolveTypeAlias(innerArgs[0]);
                } else if (innerKind == CollectionKind::Map &&
                            innerArgs.size() >= 2) {
                    innerElemSig = resolveTypeAlias(innerArgs[0]);
                    innerValSig  = resolveTypeAlias(innerArgs[1]);
                }
            }
            auto innerDtor = getOrCreateCollectionDestructor(
                innerKind, innerElemSig, innerValSig);
            emitArcRelease(emitArcGetHeaderFromData(val),
                            /*atomic=*/false, innerDtor, nullptr);
        }
        // emitArcRelease leaves cursor in its own done BB.
        builder_.CreateBr(nullBB);

        builder_.SetInsertPoint(nullBB);
    }

    // After all components, branch to the loop latch.
    builder_.CreateBr(loop.latch);
    builder_.SetInsertPoint(loop.post);
}

void CodeGen::emitTupleElemRetainLoop(llvm::Value *arrayPtr, llvm::Value *len,
                                       const char *tag,
                                       const std::string &tupleSig,
                                       llvm::StructType *tupleTy) {
    if (!arrayPtr || !len || !tupleTy) return;
    std::vector<std::string> components = splitTupleSig(tupleSig);
    if (components.empty()) return;

    bool anyArc = false;
    for (const auto &c : components) {
        const std::string r = resolveTypeAlias(c);
        if (r.empty() || isWeakTypeName(r)) continue;
        if (r == "str" || isListTypeName(r) || isMapTypeName(r) ||
            isSetTypeName(r) ||
            (r.size() >= 2 && r.front() == '(' && r.back() == ')')) {
            anyArc = true;
            break;
        }
    }
    if (!anyArc) return;

    llvm::Function *fn = builder_.GetInsertBlock()->getParent();
    std::string tagPrefix = std::string("ret_tup_") + tag;
    auto loop = emitCountedLoopShell(builder_, *ctx_, fn, len, i64Ty_,
                                       tagPrefix);

    auto *slotPtr = builder_.CreateGEP(tupleTy, arrayPtr, {loop.iPhi},
                                         tagPrefix + "_slot");
    const unsigned n = static_cast<unsigned>(
        std::min<size_t>(components.size(), tupleTy->getNumElements()));
    for (unsigned i = 0; i < n; ++i) {
        const std::string fSig = resolveTypeAlias(components[i]);
        if (fSig.empty() || isWeakTypeName(fSig)) continue;

        const bool isStr  = (fSig == "str");
        const bool isColl = isListTypeName(fSig) || isMapTypeName(fSig) ||
                            isSetTypeName(fSig);
        const bool isTup  = (fSig.size() >= 2 && fSig.front() == '(' &&
                              fSig.back() == ')');
        if (!isStr && !isColl && !isTup) continue;

        auto *fieldGEP = builder_.CreateStructGEP(tupleTy, slotPtr, i,
            tagPrefix + "_f" + std::to_string(i));

        if (isTup) {
            auto *nestedTy = llvm::dyn_cast<llvm::StructType>(
                tupleTy->getElementType(i));
            if (!nestedTy) continue;
            llvm::Value *one = llvm::ConstantInt::get(i64Ty_, 1);
            std::string subTag = std::string(tag) + "_n" + std::to_string(i);
            emitTupleElemRetainLoop(fieldGEP, one, subTag.c_str(), fSig,
                                     nestedTy);
            continue;
        }

        auto *val = builder_.CreateLoad(ptrTy_, fieldGEP,
            tagPrefix + "_v" + std::to_string(i));
        auto *nullBB = llvm::BasicBlock::Create(*ctx_,
            tagPrefix + "_skip" + std::to_string(i), fn);
        auto *retBB  = llvm::BasicBlock::Create(*ctx_,
            tagPrefix + "_do"   + std::to_string(i), fn);
        auto *isNull = builder_.CreateICmpEQ(val,
            llvm::ConstantPointerNull::get(
                llvm::cast<llvm::PointerType>(ptrTy_)),
            tagPrefix + "_isnull" + std::to_string(i));
        builder_.CreateCondBr(isNull, nullBB, retBB);

        builder_.SetInsertPoint(retBB);
        if (isStr) {
            emitArcRetain(emitStrGetHeaderFromData(val), isArcAtomic(val));
        } else {
            emitArcRetain(emitArcGetHeaderFromData(val), isArcAtomic(val));
        }
        builder_.CreateBr(nullBB);

        builder_.SetInsertPoint(nullBB);
    }

    builder_.CreateBr(loop.latch);
    builder_.SetInsertPoint(loop.post);
}

} // namespace ry
