#include "ry/codegen.hpp"

llvm::Value *CodeGen::emitArcAlloc(llvm::Value *dataSize) {
    auto *headerSize = llvm::ConstantInt::get(i64Ty_, ARC_HEADER_SIZE);
    auto *totalSize = builder_.CreateAdd(dataSize, headerSize, "arc_total");
    auto *headerPtr = builder_.CreateCall(getStdlibMalloc(), {totalSize}, "arc_hdr");

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

llvm::Value *CodeGen::emitArcGetHeaderFromData(llvm::Value *dataPtr) {
    return builder_.CreateGEP(i8Ty_, dataPtr,
                              llvm::ConstantInt::get(i64Ty_, -static_cast<int64_t>(ARC_HEADER_SIZE)),
                              "arc_hdr_from_data");
}

void CodeGen::emitArcRetain(llvm::Value *headerPtr, bool atomic) {
    auto *strongPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 0, "arc_retain_ptr");

    // Skip immortal objects (strong_count == INT64_MAX)
    auto *cur = builder_.CreateLoad(i64Ty_, strongPtr, "arc_strong");
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
                              llvm::FunctionCallee destructor) {
    auto *strongPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 0, "arc_rel_ptr");

    // Skip immortal objects (strong_count == INT64_MAX)
    auto *curCheck = builder_.CreateLoad(i64Ty_, strongPtr, "arc_strong_check");
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
    builder_.CreateCondBr(isZero, freeBB, doneBB);

    builder_.SetInsertPoint(freeBB);
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
    builder_.CreateCall(getStdlibFree(), {headerPtr});
    builder_.CreateBr(doneBB);

    builder_.SetInsertPoint(skipFreeBB);
    builder_.CreateBr(doneBB);

    builder_.SetInsertPoint(doneBB);
}

bool CodeGen::isArcAtomic(llvm::Value *val) const {
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
        auto fnIt = fn_type_info_.find(alloca);
        if (fnIt != fn_type_info_.end() && !fnIt->second.capturedArcKinds.empty())
            return getOrCreateClosureDestructor(fnIt->second);
    }
    return {};
}

CodeGen::ResourceKind CodeGen::detectResourceKind(llvm::Value *val) {
    for (int i = 0; i < RK_COUNT; ++i)
        if (resource_sets_[i].count(val))
            return static_cast<ResourceKind>(i);
    // Resolve through LoadInst (e.g., `b = a` where `a` is a resource variable)
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
        auto *ptr = load->getPointerOperand();
        for (int i = 0; i < RK_COUNT; ++i)
            if (resource_sets_[i].count(ptr))
                return static_cast<ResourceKind>(i);
    }
    return RK_COUNT;
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

llvm::FunctionCallee CodeGen::getOrCreateResourceDestructor(ResourceKind rk) {
    auto it = resource_destructors_cache_.find(rk);
    if (it != resource_destructors_cache_.end())
        return it->second;

    static const struct {
        ResourceKind kind;
        const char *dtorName;
        const char *cleanupFnName;
    } table[] = {
        {RK_TcpListener,        "__ry_arc_dtor_tcp_listener",          "__ry_tcp_listener_cleanup"},
        {RK_TcpStream,           "__ry_arc_dtor_tcp_stream",            "__ry_tcp_cleanup"},
        {RK_TlsStream,           "__ry_arc_dtor_tls_stream",            "__ry_tls_cleanup"},
        {RK_HttpRequest,         "__ry_arc_dtor_http_request",          "__ry_http_request_cleanup"},
        {RK_HttpResponse,        "__ry_arc_dtor_http_response",         "__ry_http_response_cleanup"},
        {RK_HttpClientResponse,  "__ry_arc_dtor_http_client_response",  "__ry_http_client_response_cleanup"},
        {RK_JsonValue,           "__ry_arc_dtor_json_value",            "__ry_json_cleanup"},
        {RK_Thread,              "__ry_arc_dtor_thread",                "__ry_thread_cleanup"},
        {RK_Lock,                "__ry_arc_dtor_lock",                  "__ry_lock_cleanup"},
        {RK_RWLock,              "__ry_arc_dtor_rwlock",                "__ry_rwlock_cleanup"},
        {RK_Semaphore,           "__ry_arc_dtor_semaphore",             "__ry_semaphore_cleanup"},
        {RK_Barrier,             "__ry_arc_dtor_barrier",               "__ry_barrier_cleanup"},
        {RK_AtomicInt,           "__ry_arc_dtor_atomic_int",            "__ry_atomic_int_cleanup"},
        {RK_AtomicBool,          "__ry_arc_dtor_atomic_bool",           "__ry_atomic_bool_cleanup"},
    };

    const char *dtorName = nullptr;
    const char *cleanupFnName = nullptr;
    for (auto &entry : table) {
        if (entry.kind == rk) {
            dtorName = entry.dtorName;
            cleanupFnName = entry.cleanupFnName;
            break;
        }
    }
    if (!dtorName)
        return {};

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
    if (type_meta_[TM_ListElem].count(alloca))
        return getOrCreateCollectionDestructor(CollectionKind::List);
    if (type_meta_[TM_MapKey].count(alloca))
        return getOrCreateCollectionDestructor(CollectionKind::Map);
    if (type_meta_[TM_SetElem].count(alloca))
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
    emitArcRelease(headerPtr, isArcAtomic(val), resolveDestructor(alloca));
    builder_.CreateBr(skipBB);

    builder_.SetInsertPoint(skipBB);
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

// ===== Copy-on-Write (CoW) support =====

llvm::AllocaInst *CodeGen::tryGetReceiverAlloca(const ExprNode &expr) {
    if (auto *ve = std::get_if<VariableExpr>(&expr.data))
        return findVar(ve->name);
    return nullptr;
}

void CodeGen::emitCowRetainArcElements(llvm::Value *buf, llvm::Value *len,
                                        const std::string &tag) {
    auto *fn = builder_.GetInsertBlock()->getParent();
    auto *loopBB = llvm::BasicBlock::Create(*ctx_, "cow." + tag + "_loop", fn);
    auto *bodyBB = llvm::BasicBlock::Create(*ctx_, "cow." + tag + "_body", fn);
    auto *doneBB = llvm::BasicBlock::Create(*ctx_, "cow." + tag + "_done", fn);

    auto *preLoopBB = builder_.GetInsertBlock();
    builder_.CreateBr(loopBB);
    builder_.SetInsertPoint(loopBB);
    auto *idx = builder_.CreatePHI(i64Ty_, 2, "cow_" + tag + "_idx");
    idx->addIncoming(llvm::ConstantInt::get(i64Ty_, 0), preLoopBB);
    auto *cond = builder_.CreateICmpSLT(idx, len, "cow_" + tag + "_cond");
    builder_.CreateCondBr(cond, bodyBB, doneBB);

    builder_.SetInsertPoint(bodyBB);
    auto *elemPtr = builder_.CreateGEP(ptrTy_, buf, idx, "cow_" + tag + "_ptr");
    auto *elem = builder_.CreateLoad(ptrTy_, elemPtr, "cow_" + tag + "_val");
    auto *hdr = emitArcGetHeaderFromData(elem);
    emitArcRetain(hdr, false);
    auto *next = builder_.CreateAdd(idx, llvm::ConstantInt::get(i64Ty_, 1), "cow_" + tag + "_next");
    idx->addIncoming(next, builder_.GetInsertBlock());
    builder_.CreateBr(loopBB);

    builder_.SetInsertPoint(doneBB);
}

llvm::Value *CodeGen::emitCowDeepCopyList(llvm::Value *oldDataPtr, llvm::Type *elemTy) {
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
    uint64_t elemSize = dl.getTypeAllocSize(elemTy);

    auto oldFields = loadListHeader(oldDataPtr, "cow_old");

    auto *arcHdr = emitArcAlloc(llvm::ConstantInt::get(i64Ty_, headerSize));
    auto *newDataPtr = emitArcGetDataPtr(arcHdr);

    // Tight copy: allocate len (not cap) elements; cap = len
    auto *bufSize = builder_.CreateMul(oldFields.len,
        llvm::ConstantInt::get(i64Ty_, elemSize), "cow_buf_size");
    auto *newBuf = builder_.CreateCall(getStdlibMalloc(), {bufSize}, "cow_new_buf");
    builder_.CreateCall(getStdlibMemcpy(), {newBuf, oldFields.data, bufSize});

    auto *newLenPtr = builder_.CreateStructGEP(listHeaderTy_, newDataPtr, 0, "cow_new_len_ptr");
    builder_.CreateStore(oldFields.len, newLenPtr);
    auto *newCapPtr = builder_.CreateStructGEP(listHeaderTy_, newDataPtr, 1, "cow_new_cap_ptr");
    builder_.CreateStore(oldFields.len, newCapPtr);
    auto *newDataField = builder_.CreateStructGEP(listHeaderTy_, newDataPtr, 2, "cow_new_data_ptr");
    builder_.CreateStore(newBuf, newDataField);

    // Note: we do NOT retain ARC elements here. Collection destructors only
    // free internal buffers and do not release ARC-managed elements, so
    // retaining here would cause an ARC imbalance (leak).

    return newDataPtr;
}

llvm::Value *CodeGen::emitCowDeepCopyMap(llvm::Value *oldDataPtr,
                                          llvm::Type *keyTy, llvm::Type *valTy) {
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t headerSize = dl.getTypeAllocSize(mapHeaderTy_);
    uint64_t keySize = dl.getTypeAllocSize(keyTy);
    uint64_t valSize = dl.getTypeAllocSize(valTy);
    uint64_t bucketElemSize = dl.getTypeAllocSize(i64Ty_);

    auto oldFields = loadMapHeader(oldDataPtr, "cow_old");
    auto *bucketCountPtr = builder_.CreateStructGEP(mapHeaderTy_, oldDataPtr, 4, "cow_old_bc_ptr");
    auto *bucketCount = builder_.CreateLoad(i64Ty_, bucketCountPtr, "cow_old_bc");
    auto *bucketsFieldPtr = builder_.CreateStructGEP(mapHeaderTy_, oldDataPtr, 5, "cow_old_bk_ptr");
    auto *oldBuckets = builder_.CreateLoad(ptrTy_, bucketsFieldPtr, "cow_old_bk");

    auto *arcHdr = emitArcAlloc(llvm::ConstantInt::get(i64Ty_, headerSize));
    auto *newDataPtr = emitArcGetDataPtr(arcHdr);

    // Tight copy: allocate len (not cap) for keys/vals
    auto *keysBufSize = builder_.CreateMul(oldFields.len,
        llvm::ConstantInt::get(i64Ty_, keySize), "cow_keys_size");
    auto *newKeys = builder_.CreateCall(getStdlibMalloc(), {keysBufSize}, "cow_new_keys");
    builder_.CreateCall(getStdlibMemcpy(), {newKeys, oldFields.keys, keysBufSize});

    auto *valsBufSize = builder_.CreateMul(oldFields.len,
        llvm::ConstantInt::get(i64Ty_, valSize), "cow_vals_size");
    auto *newVals = builder_.CreateCall(getStdlibMalloc(), {valsBufSize}, "cow_new_vals");
    builder_.CreateCall(getStdlibMemcpy(), {newVals, oldFields.vals, valsBufSize});

    auto *bucketsBufSize = builder_.CreateMul(bucketCount,
        llvm::ConstantInt::get(i64Ty_, bucketElemSize), "cow_bk_size");
    auto *newBuckets = builder_.CreateCall(getStdlibMalloc(), {bucketsBufSize}, "cow_new_bk");
    builder_.CreateCall(getStdlibMemcpy(), {newBuckets, oldBuckets, bucketsBufSize});

    auto *newLenPtr = builder_.CreateStructGEP(mapHeaderTy_, newDataPtr, 0, "cow_m_len_ptr");
    builder_.CreateStore(oldFields.len, newLenPtr);
    auto *newCapPtr = builder_.CreateStructGEP(mapHeaderTy_, newDataPtr, 1, "cow_m_cap_ptr");
    builder_.CreateStore(oldFields.len, newCapPtr);
    auto *newKeysField = builder_.CreateStructGEP(mapHeaderTy_, newDataPtr, 2, "cow_m_keys_ptr");
    builder_.CreateStore(newKeys, newKeysField);
    auto *newValsField = builder_.CreateStructGEP(mapHeaderTy_, newDataPtr, 3, "cow_m_vals_ptr");
    builder_.CreateStore(newVals, newValsField);
    auto *newBcPtr = builder_.CreateStructGEP(mapHeaderTy_, newDataPtr, 4, "cow_m_bc_ptr");
    builder_.CreateStore(bucketCount, newBcPtr);
    auto *newBkField = builder_.CreateStructGEP(mapHeaderTy_, newDataPtr, 5, "cow_m_bk_ptr");
    builder_.CreateStore(newBuckets, newBkField);

    // Note: no ARC element retain — see emitCowDeepCopyList for rationale.

    return newDataPtr;
}

llvm::Value *CodeGen::emitCowDeepCopySet(llvm::Value *oldDataPtr, llvm::Type *elemTy) {
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t headerSize = dl.getTypeAllocSize(setHeaderTy_);
    uint64_t elemSz = dl.getTypeAllocSize(elemTy);
    uint64_t bucketElemSize = dl.getTypeAllocSize(i64Ty_);

    auto oldFields = loadSetHeader(oldDataPtr, "cow_old");
    auto *bucketCountPtr = builder_.CreateStructGEP(setHeaderTy_, oldDataPtr, 3, "cow_old_bc_ptr");
    auto *bucketCount = builder_.CreateLoad(i64Ty_, bucketCountPtr, "cow_old_bc");
    auto *bucketsFieldPtr = builder_.CreateStructGEP(setHeaderTy_, oldDataPtr, 4, "cow_old_bk_ptr");
    auto *oldBuckets = builder_.CreateLoad(ptrTy_, bucketsFieldPtr, "cow_old_bk");

    auto *arcHdr = emitArcAlloc(llvm::ConstantInt::get(i64Ty_, headerSize));
    auto *newDataPtr = emitArcGetDataPtr(arcHdr);

    // Tight copy: allocate len (not cap) for elems
    auto *elemsBufSize = builder_.CreateMul(oldFields.len,
        llvm::ConstantInt::get(i64Ty_, elemSz), "cow_elems_size");
    auto *newElems = builder_.CreateCall(getStdlibMalloc(), {elemsBufSize}, "cow_new_elems");
    builder_.CreateCall(getStdlibMemcpy(), {newElems, oldFields.elems, elemsBufSize});

    auto *bucketsBufSize = builder_.CreateMul(bucketCount,
        llvm::ConstantInt::get(i64Ty_, bucketElemSize), "cow_bk_size");
    auto *newBuckets = builder_.CreateCall(getStdlibMalloc(), {bucketsBufSize}, "cow_new_bk");
    builder_.CreateCall(getStdlibMemcpy(), {newBuckets, oldBuckets, bucketsBufSize});

    auto *newLenPtr = builder_.CreateStructGEP(setHeaderTy_, newDataPtr, 0, "cow_s_len_ptr");
    builder_.CreateStore(oldFields.len, newLenPtr);
    auto *newCapPtr = builder_.CreateStructGEP(setHeaderTy_, newDataPtr, 1, "cow_s_cap_ptr");
    builder_.CreateStore(oldFields.len, newCapPtr);
    auto *newElemsField = builder_.CreateStructGEP(setHeaderTy_, newDataPtr, 2, "cow_s_elems_ptr");
    builder_.CreateStore(newElems, newElemsField);
    auto *newBcPtr = builder_.CreateStructGEP(setHeaderTy_, newDataPtr, 3, "cow_s_bc_ptr");
    builder_.CreateStore(bucketCount, newBcPtr);
    auto *newBkField = builder_.CreateStructGEP(setHeaderTy_, newDataPtr, 4, "cow_s_bk_ptr");
    builder_.CreateStore(newBuckets, newBkField);

    // Note: no ARC element retain — see emitCowDeepCopyList for rationale.

    return newDataPtr;
}

llvm::Value *CodeGen::emitCowCheck(llvm::Value *dataPtr,
                                    llvm::AllocaInst *alloca,
                                    CollectionKind kind) {
    if (!alloca)
        return dataPtr;
    // Only apply CoW to ARC-backed collections. Fall back to checking whether
    // the loaded value originates from an ARC-backed alloca (covers parameters
    // and other allocation paths not tracked directly).
    if (!arc_backed_vars_.count(alloca)) {
        if (auto *load = llvm::dyn_cast<llvm::LoadInst>(dataPtr)) {
            auto *src = llvm::dyn_cast<llvm::AllocaInst>(load->getPointerOperand());
            if (!src || !arc_backed_vars_.count(src))
                return dataPtr;
        } else {
            return dataPtr;
        }
    }

    auto *fn = builder_.GetInsertBlock()->getParent();
    auto *headerPtr = emitArcGetHeaderFromData(dataPtr);
    auto *strongPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 0, "cow_strong_ptr");
    auto *strong = builder_.CreateLoad(i64Ty_, strongPtr, "cow_strong");

    // Skip if unique (strong_count == 1) or immortal (string literals, etc.)
    auto *isUnique = builder_.CreateICmpEQ(strong, llvm::ConstantInt::get(i64Ty_, 1), "cow_unique");
    auto *isImmortal = builder_.CreateICmpEQ(strong, llvm::ConstantInt::get(i64Ty_, ARC_IMMORTAL), "cow_immortal");
    auto *skipCow = builder_.CreateOr(isUnique, isImmortal, "cow_skip");

    auto *copyBB = llvm::BasicBlock::Create(*ctx_, "cow.copy", fn);
    auto *contBB = llvm::BasicBlock::Create(*ctx_, "cow.cont", fn);
    auto *origBB = builder_.GetInsertBlock();
    builder_.CreateCondBr(skipCow, contBB, copyBB);

    builder_.SetInsertPoint(copyBB);

    llvm::Value *newDataPtr = nullptr;
    switch (kind) {
    case CollectionKind::List: {
        auto *elemTy = getListElementType(dataPtr);
        if (!elemTy) elemTy = i64Ty_;
        newDataPtr = emitCowDeepCopyList(dataPtr, elemTy);
        break;
    }
    case CollectionKind::Map: {
        auto *keyTy = getMapKeyType(dataPtr);
        auto *valTy = getMapValueType(dataPtr);
        if (!keyTy) keyTy = i64Ty_;
        if (!valTy) valTy = i64Ty_;
        newDataPtr = emitCowDeepCopyMap(dataPtr, keyTy, valTy);
        break;
    }
    case CollectionKind::Set: {
        auto *elemTy = getSetElementType(dataPtr);
        if (!elemTy) elemTy = i64Ty_;
        newDataPtr = emitCowDeepCopySet(dataPtr, elemTy);
        break;
    }
    }

    // Reuse headerPtr (dominates copyBB) instead of re-computing
    emitArcRelease(headerPtr, isArcAtomic(dataPtr),
                   getOrCreateCollectionDestructor(kind));

    builder_.CreateStore(newDataPtr, alloca);
    arc_owned_values_.insert(newDataPtr);

    auto *copyEndBB = builder_.GetInsertBlock();
    builder_.CreateBr(contBB);

    builder_.SetInsertPoint(contBB);
    auto *phi = builder_.CreatePHI(ptrTy_, 2, "cow_ptr");
    phi->addIncoming(dataPtr, origBB);
    phi->addIncoming(newDataPtr, copyEndBB);

    // Propagate all metadata (type_meta_, fn_type_info_, etc.) to the PHI
    propagateCollectionMetadata(alloca, phi);

    return phi;
}

// ===== Closure ARC support =====

CodeGen::CapturedArcKind CodeGen::detectCapturedArcKind(llvm::AllocaInst *alloca) const {
    if (type_meta_[TM_ListElem].count(alloca))
        return CAK_List;
    if (type_meta_[TM_MapKey].count(alloca))
        return CAK_Map;
    if (type_meta_[TM_SetElem].count(alloca))
        return CAK_Set;
    if (closure_managed_vars_.count(alloca))
        return CAK_Closure;
    if (resource_managed_vars_.count(alloca))
        return CAK_Resource;
    // Unknown ARC-managed types: skip sub-destructor (safe no-op; avoids wrong destructor)
    return CAK_None;
}

llvm::FunctionCallee CodeGen::getOrCreateClosureDestructor(const FnTypeInfo &info) {
    // Check if any captured variable needs ARC release
    bool hasArc = false;
    for (auto k : info.capturedArcKinds)
        if (k != CAK_None) { hasArc = true; break; }
    if (!hasArc)
        return {};

    // Cache key: the capturedArcKinds vector (deduplicates identical capture signatures)
    auto it = closure_destructors_cache_.find(info.capturedArcKinds);
    if (it != closure_destructors_cache_.end())
        return it->second;

    auto *dtorTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
    std::string name = "__ry_arc_dtor_closure_" + std::to_string(closure_destructors_cache_.size());
    auto *dtorFn = llvm::Function::Create(
        dtorTy, llvm::Function::InternalLinkage, name, mod_.get());
    dtorFn->addFnAttr(llvm::Attribute::NoUnwind);

    auto *entryBB = llvm::BasicBlock::Create(*ctx_, "entry", dtorFn);

    auto *savedBB = builder_.GetInsertBlock();
    auto savedPt = builder_.GetInsertPoint();
    builder_.SetInsertPoint(entryBB);

    auto *dataPtr = dtorFn->getArg(0); // points to closure struct (after ARC header)

    // Reconstruct closure struct type
    std::vector<llvm::Type*> closureFields;
    closureFields.push_back(ptrTy_); // fn_ptr
    for (auto *ct : info.capturedTypes)
        closureFields.push_back(ct);
    auto *closureTy = llvm::StructType::get(*ctx_, closureFields);

    for (size_t i = 0; i < info.capturedArcKinds.size(); ++i) {
        if (info.capturedArcKinds[i] == CAK_None)
            continue;

        auto *capField = builder_.CreateStructGEP(
            closureTy, dataPtr, i + 1, "dtor.cap." + std::to_string(i));
        auto *capVal = builder_.CreateLoad(info.capturedTypes[i], capField,
                                            "dtor.cap_val." + std::to_string(i));

        // Null check
        auto *isNull = builder_.CreateICmpEQ(capVal,
            llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
            "dtor.null_check." + std::to_string(i));

        auto *releaseBB = llvm::BasicBlock::Create(*ctx_, "dtor.release." + std::to_string(i), dtorFn);
        auto *skipBB = llvm::BasicBlock::Create(*ctx_, "dtor.skip." + std::to_string(i), dtorFn);
        builder_.CreateCondBr(isNull, skipBB, releaseBB);

        builder_.SetInsertPoint(releaseBB);
        auto *hdr = emitArcGetHeaderFromData(capVal);

        // Resolve sub-destructor based on captured ARC kind
        llvm::FunctionCallee subDtor;
        switch (info.capturedArcKinds[i]) {
        case CAK_List:
            subDtor = getOrCreateCollectionDestructor(CollectionKind::List);
            break;
        case CAK_Map:
            subDtor = getOrCreateCollectionDestructor(CollectionKind::Map);
            break;
        case CAK_Set:
            subDtor = getOrCreateCollectionDestructor(CollectionKind::Set);
            break;
        case CAK_Closure:
        case CAK_Resource:
        case CAK_None:
            // Closures/resources: pass empty destructor — their own release
            // path handles destructor dispatch via resolveDestructor
            subDtor = {};
            break;
        }

        emitArcRelease(hdr, false, subDtor);
        builder_.CreateBr(skipBB);

        builder_.SetInsertPoint(skipBB);
    }

    builder_.CreateRetVoid();

    if (savedBB)
        builder_.SetInsertPoint(savedBB, savedPt);

    llvm::FunctionCallee callee(dtorTy, dtorFn);
    closure_destructors_cache_[info.capturedArcKinds] = callee;
    return callee;
}
