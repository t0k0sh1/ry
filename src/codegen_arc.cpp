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
    if (getTypeMeta(TypeMeta::ListElem, alloca))
        return getOrCreateCollectionDestructor(CollectionKind::List);
    if (getTypeMeta(TypeMeta::MapKey, alloca))
        return getOrCreateCollectionDestructor(CollectionKind::Map);
    if (getTypeMeta(TypeMeta::SetElem, alloca))
        return getOrCreateCollectionDestructor(CollectionKind::Set);
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
    auto *headerPtr = emitArcGetHeaderFromData(val);

    // Look up GC visit function for potentially cyclic types.
    llvm::Function *gcVisitFn = nullptr;
    auto *meta = getMeta(alloca);
    if (meta && !meta->enum_value_type.empty() && isPotentiallyCyclic(meta->enum_value_type)) {
        gcVisitFn = getOrCreateVisitFunction(meta->enum_value_type);
    }

    emitArcRelease(headerPtr, isArcAtomic(val), resolveDestructor(alloca), gcVisitFn);
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
    // are excluded and handled by different fix paths. See KNOWLEDGE.md
    // "Element-slot writes must release the overwritten ARC pointer"
    // (#855 / #857).
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
        auto *hdr = emitArcGetHeaderFromData(fieldVal);
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
        emitArcReleaseLoadedElement(fieldVal, fk, fd.name);
    }
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
    auto *immortalDataPtr = emitArcGetDataPtr(headerPtr);
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
    auto *dataPtr = emitArcGetDataPtr(headerPtr);
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
    auto *hdr = emitArcGetHeaderFromData(val);
    emitArcRetain(hdr, /*atomic=*/false);
}

bool CodeGen::tryRetainArcSource(llvm::Value *val) {
    // Case 1: LoadInst from an ARC-managed alloca
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
        auto *srcAlloca = llvm::dyn_cast<llvm::AllocaInst>(load->getPointerOperand());
        if (srcAlloca && isArcManaged(srcAlloca)) {
            auto *hdr = emitArcGetHeaderFromData(val);
            emitArcRetain(hdr, isArcAtomic(val));
            return true;
        }
    }
    // Case 2: Value produced by emitArcAlloc (e.g., f-string, interpolated string)
    // These already have strong_count=1 from allocation, no retain needed,
    // but signal to caller that this is ARC-owned
    if (arc_owned_values_.count(val))
        return true;
    return false;
}

llvm::FunctionCallee CodeGen::getOrCreateCollectionDestructor(CollectionKind kind) {
    auto it = arc_destructors_cache_.find(kind);
    if (it != arc_destructors_cache_.end())
        return it->second;

    // Destructor signature: void(ptr dataPtr)
    // dataPtr points to the type-specific header (after ARC header)
    auto *dtorTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
    std::string name;

    switch (kind) {
    case CollectionKind::List:
        name = "__ry_arc_dtor_list";
        break;
    case CollectionKind::Map:
        name = "__ry_arc_dtor_map";
        break;
    case CollectionKind::Set:
        name = "__ry_arc_dtor_set";
        break;
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

    switch (kind) {
    case CollectionKind::List: {
        // Free data buffer: ListHeader { len, cap, data }
        auto *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, dataPtr, 2, "dtor_data_field");
        auto *dataBuf = builder_.CreateLoad(ptrTy_, dataPtrField, "dtor_data_buf");
        builder_.CreateCall(freeFn, {dataBuf});
        break;
    }
    case CollectionKind::Map: {
        // Free keys, values, and buckets buffers
        auto *keysField = builder_.CreateStructGEP(mapHeaderTy_, dataPtr, 2, "dtor_keys_field");
        auto *keys = builder_.CreateLoad(ptrTy_, keysField, "dtor_keys");
        builder_.CreateCall(freeFn, {keys});

        auto *valsField = builder_.CreateStructGEP(mapHeaderTy_, dataPtr, 3, "dtor_vals_field");
        auto *vals = builder_.CreateLoad(ptrTy_, valsField, "dtor_vals");
        builder_.CreateCall(freeFn, {vals});

        auto *bucketsField = builder_.CreateStructGEP(mapHeaderTy_, dataPtr, 5, "dtor_buckets_field");
        auto *buckets = builder_.CreateLoad(ptrTy_, bucketsField, "dtor_buckets");
        builder_.CreateCall(freeFn, {buckets});
        break;
    }
    case CollectionKind::Set: {
        // Free elements and buckets buffers
        auto *elemsField = builder_.CreateStructGEP(setHeaderTy_, dataPtr, 2, "dtor_elems_field");
        auto *elems = builder_.CreateLoad(ptrTy_, elemsField, "dtor_elems");
        builder_.CreateCall(freeFn, {elems});

        auto *bucketsField = builder_.CreateStructGEP(setHeaderTy_, dataPtr, 4, "dtor_buckets_field");
        auto *buckets = builder_.CreateLoad(ptrTy_, bucketsField, "dtor_buckets");
        builder_.CreateCall(freeFn, {buckets});
        break;
    }
    }

    builder_.CreateRetVoid();

    // Restore builder state
    if (savedBB)
        builder_.SetInsertPoint(savedBB, savedPt);

    llvm::FunctionCallee callee(dtorTy, dtorFn);
    arc_destructors_cache_[kind] = callee;
    return callee;
}

} // namespace ry
