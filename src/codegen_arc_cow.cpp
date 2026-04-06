#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"
#include <cassert>

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
    uint64_t elemSize = dl.getTypeAllocSize(elemTy);

    auto oldFields = loadListHeader(oldDataPtr, "cow_old");

    auto *newDataPtr = emitArcAllocCollectionHeader(listHeaderTy_);

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
    uint64_t keySize = dl.getTypeAllocSize(keyTy);
    uint64_t valSize = dl.getTypeAllocSize(valTy);
    uint64_t bucketElemSize = dl.getTypeAllocSize(i64Ty_);

    auto oldFields = loadMapHeader(oldDataPtr, "cow_old");
    auto *bucketCountPtr = builder_.CreateStructGEP(mapHeaderTy_, oldDataPtr, 4, "cow_old_bc_ptr");
    auto *bucketCount = builder_.CreateLoad(i64Ty_, bucketCountPtr, "cow_old_bc");
    auto *bucketsFieldPtr = builder_.CreateStructGEP(mapHeaderTy_, oldDataPtr, 5, "cow_old_bk_ptr");
    auto *oldBuckets = builder_.CreateLoad(ptrTy_, bucketsFieldPtr, "cow_old_bk");

    auto *newDataPtr = emitArcAllocCollectionHeader(mapHeaderTy_);

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
    uint64_t elemSz = dl.getTypeAllocSize(elemTy);
    uint64_t bucketElemSize = dl.getTypeAllocSize(i64Ty_);

    auto oldFields = loadSetHeader(oldDataPtr, "cow_old");
    auto *bucketCountPtr = builder_.CreateStructGEP(setHeaderTy_, oldDataPtr, 3, "cow_old_bc_ptr");
    auto *bucketCount = builder_.CreateLoad(i64Ty_, bucketCountPtr, "cow_old_bc");
    auto *bucketsFieldPtr = builder_.CreateStructGEP(setHeaderTy_, oldDataPtr, 4, "cow_old_bk_ptr");
    auto *oldBuckets = builder_.CreateLoad(ptrTy_, bucketsFieldPtr, "cow_old_bk");

    auto *newDataPtr = emitArcAllocCollectionHeader(setHeaderTy_);

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
    if (isArcManaged(alloca))
        return CAK_Generic; // ARC-managed but no sub-destructor (e.g., f-strings)
    return CAK_None;
}

llvm::FunctionCallee CodeGen::getOrCreateClosureDestructor(const FnTypeInfo &info) {
    // Check if any captured variable needs ARC release
    bool hasArc = false;
    for (auto k : info.capturedArcKinds)
        if (k != CAK_None) { hasArc = true; break; }
    if (!hasArc)
        return {};

    // Cache key: capturedArcKinds + capturedTypes + capturedResourceKinds + nested closure shapes
    std::vector<NestedClosureShape> nestedShapes;
    if (info.capturedClosureInfos)
        for (auto &[idx, ci] : *info.capturedClosureInfos)
            nestedShapes.push_back({idx, ci.capturedArcKinds, ci.capturedTypes,
                                    ci.capturedResourceKinds});
    std::sort(nestedShapes.begin(), nestedShapes.end());
    ClosureDtorKey cacheKey{info.capturedArcKinds, info.capturedTypes,
                            info.capturedResourceKinds, std::move(nestedShapes)};
    auto it = closure_destructors_cache_.find(cacheKey);
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
        case CAK_Resource: {
            assert(i < info.capturedResourceKinds.size());
            ResourceKind rk = info.capturedResourceKinds[i];
            if (rk != ResourceKindRegistry::NONE)
                subDtor = getOrCreateResourceDestructor(rk);
            break;
        }
        case CAK_Closure: {
            if (info.capturedClosureInfos) {
                auto cit = info.capturedClosureInfos->find(i);
                if (cit != info.capturedClosureInfos->end() &&
                    !cit->second.capturedArcKinds.empty())
                    subDtor = getOrCreateClosureDestructor(cit->second);
            }
            break;
        }
        case CAK_Generic:
        case CAK_None:
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
    closure_destructors_cache_[cacheKey] = callee;
    return callee;
}
