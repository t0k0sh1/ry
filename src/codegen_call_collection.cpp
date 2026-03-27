#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"


// ===== Builtin Collection =====

llvm::Value *CodeGen::emitBuiltinCollection(const CallExpr &e) {
    using Handler = llvm::Value *(CodeGen::*)(const CallExpr &);
    static const std::unordered_map<std::string, Handler> dispatch = {
        {"add",       &CodeGen::emitCollOp_add},
        {"remove",    &CodeGen::emitCollOp_remove},
        {"append",    &CodeGen::emitCollOp_append},
        {"append!",   &CodeGen::emitCollOp_append},
        {"appended",  &CodeGen::emitCollOp_appended},
        {"pop",       &CodeGen::emitCollOp_pop},
        {"slice",     &CodeGen::emitCollOp_slice},
        {"take",      &CodeGen::emitCollOp_take},
        {"insert",    &CodeGen::emitCollOp_insert},
        {"remove_at", &CodeGen::emitCollOp_remove_at},
        {"distinct",  &CodeGen::emitCollOp_distinct},
        {"flatten",   &CodeGen::emitCollOp_flatten},
        {"items",     &CodeGen::emitCollOp_items},
        {"get",       &CodeGen::emitCollOp_get},
        {"merge",     &CodeGen::emitCollOp_merge},
    };
    auto it = dispatch.find(e.callee);
    if (it == dispatch.end()) return nullptr;
    return (this->*it->second)(e);
}

// ===== Collection operation handlers =====

llvm::Value *CodeGen::emitCollOp_add(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;
    // add(set, val) -> add element to set (no-op if already present)
    // Only intercept if first arg is a set (fall through to user function otherwise)
    llvm::Value *setPtr = emitExpr(*e.args[0]);
    llvm::Type *elemTy = getSetElementType(setPtr);
    if (elemTy) {
        llvm::Value *elem = emitExpr(*e.args[1]);
        if (elem->getType() != elemTy)
            codegenError("add() element type mismatch");

        llvm::Value *idx = emitSetElementLookup(setPtr, elem, elemTy);
        llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "found");

        llvm::BasicBlock *insertBB = llvm::BasicBlock::Create(*ctx_, "set.insert", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "set.add_end", fn_);
        builder_.CreateCondBr(found, endBB, insertBB);

        builder_.SetInsertPoint(insertBB);
        auto sf = loadSetHeader(setPtr, "set");

        llvm::Value *needGrow = builder_.CreateICmpEQ(sf.len, sf.cap, "need_grow");
        llvm::BasicBlock *growBB = llvm::BasicBlock::Create(*ctx_, "set.grow", fn_);
        llvm::BasicBlock *storeBB = llvm::BasicBlock::Create(*ctx_, "set.store", fn_);
        builder_.CreateCondBr(needGrow, growBB, storeBB);

        builder_.SetInsertPoint(growBB);
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        llvm::Value *newCap = builder_.CreateMul(sf.cap, llvm::ConstantInt::get(i64Ty_, 2), "new_cap");
        auto mallocFn = getStdlibMalloc();
        llvm::Value *newSize = builder_.CreateMul(newCap, llvm::ConstantInt::get(i64Ty_, elemSize), "new_size");
        llvm::Value *newElemsPtr = builder_.CreateCall(mallocFn, {newSize}, "new_elems");

        auto memcpyFn = getStdlibMemcpy();
        llvm::Value *oldElemsPtr = sf.elems;
        llvm::Value *oldSize = builder_.CreateMul(sf.len, llvm::ConstantInt::get(i64Ty_, elemSize), "old_size");
        builder_.CreateCall(memcpyFn, {newElemsPtr, oldElemsPtr, oldSize});

        auto freeFn = getStdlibFree();
        builder_.CreateCall(freeFn, {oldElemsPtr});

        builder_.CreateStore(newElemsPtr, sf.elemsPtr);
        builder_.CreateStore(newCap, sf.capPtr);
        builder_.CreateBr(storeBB);

        builder_.SetInsertPoint(storeBB);
        llvm::Value *curLen = builder_.CreateLoad(i64Ty_, sf.lenPtr, "cur_len");
        llvm::Value *curElemsPtr = builder_.CreateLoad(ptrTy_, sf.elemsPtr, "cur_elems");
        llvm::Value *newElemPtr = builder_.CreateGEP(elemTy, curElemsPtr, {curLen}, "new_elem_ptr");
        builder_.CreateStore(elem, newElemPtr);

        llvm::Value *newLen = builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1), "new_len");
        builder_.CreateStore(newLen, sf.lenPtr);

        // Insert into hash table buckets and check rehash
        emitBucketInsertAndRehashCheck(setPtr, setHeaderTy_, kSetLayout.lenIdx, kSetLayout.bucketCountIdx, kSetLayout.bucketsPtrIdx, elem, elemTy, curLen);

        builder_.CreateBr(endBB);

        builder_.SetInsertPoint(endBB);
        return llvm::ConstantInt::get(i64Ty_, 0);
    }
    // Not a set -- fall through to user function resolution
    return nullptr;
}

// ===== Hash table bucket helpers for remove operations =====

CodeGen::BucketContext CodeGen::emitHashTableRemoveBucket(
    llvm::Value *headerPtr, llvm::StructType *headerTy,
    const HashTableLayout &layout,
    llvm::Value *key, llvm::Type *keyTy, llvm::Value *denseIndex,
    const std::string &prefix) {
    llvm::Twine p(prefix);
    auto hfi = resolveHashFn(keyTy);
    llvm::Value *hashKey = coerceHashKey(key, keyTy, hfi.hashArgTy, p);
    llvm::FunctionType *hashTy = llvm::FunctionType::get(i64Ty_, {hfi.hashArgTy}, false);
    llvm::FunctionCallee hashFn = mod_->getOrInsertFunction(hfi.hashFnName, hashTy);
    llvm::Value *hashVal = builder_.CreateCall(hashFn, {hashKey}, p + "_hash");

    llvm::Value *bucketsField = builder_.CreateStructGEP(headerTy, headerPtr, layout.bucketsPtrIdx, p + "_bp");
    llvm::Value *bucketsPtr = builder_.CreateLoad(ptrTy_, bucketsField, p + "_buckets");
    llvm::Value *bcField = builder_.CreateStructGEP(headerTy, headerPtr, layout.bucketCountIdx, p + "_bc_field");
    llvm::Value *bucketCount = builder_.CreateLoad(i64Ty_, bcField, p + "_bc");
    llvm::Value *bucketMask = builder_.CreateSub(bucketCount, llvm::ConstantInt::get(i64Ty_, 1), p + "_bmask");

    llvm::FunctionType *removeTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_, i64Ty_, i64Ty_}, false);
    llvm::FunctionCallee removeFn = mod_->getOrInsertFunction("__ry_ht_remove", removeTy);
    builder_.CreateCall(removeFn, {bucketsPtr, bucketMask, hashVal, denseIndex});

    return {bucketsPtr, bucketMask, hfi.hashArgTy, hashFn};
}

void CodeGen::emitHashTableUpdateIndex(
    const BucketContext &bc,
    llvm::Value *value, llvm::Type *valueTy,
    llvm::Value *oldIndex, llvm::Value *newIndex,
    const std::string &prefix) {
    llvm::Twine p(prefix);
    llvm::Value *hashValue = coerceHashKey(value, valueTy, bc.hashArgTy, p);
    llvm::Value *hashVal = builder_.CreateCall(bc.hashFn, {hashValue}, p + "_hash");

    llvm::FunctionType *updateTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_, i64Ty_, i64Ty_, i64Ty_}, false);
    llvm::FunctionCallee updateFn = mod_->getOrInsertFunction("__ry_ht_update_index", updateTy);
    builder_.CreateCall(updateFn, {bc.bucketsPtr, bc.bucketMask, hashVal, oldIndex, newIndex});
}

// ===== Per-type remove implementations =====

llvm::Value *CodeGen::emitSetRemove(llvm::Value *containerPtr, llvm::Value *elem, llvm::Type *elemTy) {
    llvm::Value *idx = emitSetElementLookup(containerPtr, elem, elemTy);
    llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "found");

    llvm::BasicBlock *removeBB = llvm::BasicBlock::Create(*ctx_, "set.remove", fn_);
    llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "set.remove_end", fn_);
    builder_.CreateCondBr(found, removeBB, endBB);

    builder_.SetInsertPoint(removeBB);
    auto sf = loadSetHeader(containerPtr, "set");

    auto bc = emitHashTableRemoveBucket(containerPtr, setHeaderTy_, kSetLayout,
                                         elem, elemTy, idx, "rm");

    // Swap-remove: move last element to idx position
    llvm::Value *lastIdx = builder_.CreateSub(sf.len, llvm::ConstantInt::get(i64Ty_, 1), "last_idx");
    llvm::Value *isNotLast = builder_.CreateICmpNE(idx, lastIdx, "is_not_last");

    llvm::BasicBlock *swapBB = llvm::BasicBlock::Create(*ctx_, "set.swap", fn_);
    llvm::BasicBlock *decBB = llvm::BasicBlock::Create(*ctx_, "set.dec", fn_);
    builder_.CreateCondBr(isNotLast, swapBB, decBB);

    builder_.SetInsertPoint(swapBB);
    llvm::Value *lastPtr = builder_.CreateGEP(elemTy, sf.elems, {lastIdx}, "last_ptr");
    llvm::Value *lastVal = builder_.CreateLoad(elemTy, lastPtr, "last_val");
    llvm::Value *dstPtr = builder_.CreateGEP(elemTy, sf.elems, {idx}, "swap_dst");
    builder_.CreateStore(lastVal, dstPtr);

    emitHashTableUpdateIndex(bc, lastVal, elemTy, lastIdx, idx, "swap");
    builder_.CreateBr(decBB);

    builder_.SetInsertPoint(decBB);
    llvm::Value *newLen = builder_.CreateSub(sf.len, llvm::ConstantInt::get(i64Ty_, 1), "new_len");
    builder_.CreateStore(newLen, sf.lenPtr);
    builder_.CreateBr(endBB);

    builder_.SetInsertPoint(endBB);
    return llvm::ConstantInt::get(i64Ty_, 0);
}

llvm::Value *CodeGen::emitListRemove(llvm::Value *containerPtr, llvm::Value *val, llvm::Type *listElemTy) {
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t elemSize = dl.getTypeAllocSize(listElemTy);

    auto lf = loadListHeader(containerPtr, "lrem");

    // Linear search for the value
    llvm::AllocaInst *foundIdx = builder_.CreateAlloca(i64Ty_, nullptr, "lrem_found_idx");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, -1), foundIdx);
    llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "lrem_i");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "lrem.cond", fn_);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "lrem.body", fn_);
    llvm::BasicBlock *endSearchBB = llvm::BasicBlock::Create(*ctx_, "lrem.end_search", fn_);

    builder_.CreateBr(condBB);
    builder_.SetInsertPoint(condBB);
    llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "lrem_iv");
    llvm::Value *notYetFound = builder_.CreateICmpSLT(
        builder_.CreateLoad(i64Ty_, foundIdx, "lrem_fi"), llvm::ConstantInt::get(i64Ty_, 0), "lrem_not_found");
    llvm::Value *inBounds = builder_.CreateICmpSLT(iVal, lf.len, "lrem_in_bounds");
    llvm::Value *cont = builder_.CreateAnd(notYetFound, inBounds, "lrem_cont");
    builder_.CreateCondBr(cont, bodyBB, endSearchBB);

    builder_.SetInsertPoint(bodyBB);
    llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "lrem_ic");
    llvm::Value *elemPtr = builder_.CreateGEP(listElemTy, lf.data, {iCur}, "lrem_elem_ptr");
    llvm::Value *listElem = builder_.CreateLoad(listElemTy, elemPtr, "lrem_elem");

    llvm::Value *match;
    if (listElemTy == ptrTy_) {
        if (getNestedListElementType(containerPtr))
            codegenError("remove() is not supported for lists of non-string pointer elements");
        auto strcmpFn = getStdlibStrcmp();
        llvm::Value *cmpResult = builder_.CreateCall(strcmpFn, {val, listElem}, "lrem_strcmp");
        match = builder_.CreateICmpEQ(cmpResult, llvm::ConstantInt::get(i32Ty_, 0), "lrem_match");
    } else if (listElemTy->isDoubleTy()) {
        match = builder_.CreateFCmpOEQ(val, listElem, "lrem_match");
    } else {
        match = builder_.CreateICmpEQ(val, listElem, "lrem_match");
    }

    llvm::BasicBlock *foundBB = llvm::BasicBlock::Create(*ctx_, "lrem.found", fn_);
    llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "lrem.next", fn_);
    builder_.CreateCondBr(match, foundBB, nextBB);

    builder_.SetInsertPoint(foundBB);
    builder_.CreateStore(iCur, foundIdx);
    builder_.CreateBr(condBB);

    builder_.SetInsertPoint(nextBB);
    llvm::Value *iNext = builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1), "lrem_inext");
    builder_.CreateStore(iNext, iVar);
    builder_.CreateBr(condBB);

    // After search: if found, memmove to close the gap
    builder_.SetInsertPoint(endSearchBB);
    llvm::Value *idx = builder_.CreateLoad(i64Ty_, foundIdx, "lrem_idx");
    llvm::Value *wasFound = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "lrem_was_found");

    llvm::BasicBlock *removeBB = llvm::BasicBlock::Create(*ctx_, "lrem.remove", fn_);
    llvm::BasicBlock *doneBB = llvm::BasicBlock::Create(*ctx_, "lrem.done", fn_);
    builder_.CreateCondBr(wasFound, removeBB, doneBB);

    builder_.SetInsertPoint(removeBB);
    auto memmoveFn = getStdlibMemmove();
    llvm::Value *dstPtr = builder_.CreateGEP(listElemTy, lf.data, {idx}, "lrem_dst");
    llvm::Value *srcPtr = builder_.CreateGEP(listElemTy, lf.data,
        {builder_.CreateAdd(idx, llvm::ConstantInt::get(i64Ty_, 1))}, "lrem_src");
    llvm::Value *moveCount = builder_.CreateSub(
        builder_.CreateSub(lf.len, idx), llvm::ConstantInt::get(i64Ty_, 1), "lrem_move_count");
    llvm::Value *moveBytes = builder_.CreateMul(moveCount, llvm::ConstantInt::get(i64Ty_, elemSize), "lrem_move_bytes");
    builder_.CreateCall(memmoveFn, {dstPtr, srcPtr, moveBytes});
    llvm::Value *newLen = builder_.CreateSub(lf.len, llvm::ConstantInt::get(i64Ty_, 1), "lrem_new_len");
    builder_.CreateStore(newLen, lf.lenPtr);
    builder_.CreateBr(doneBB);

    builder_.SetInsertPoint(doneBB);
    return llvm::ConstantInt::get(i64Ty_, 0);
}

llvm::Value *CodeGen::emitMapRemove(llvm::Value *containerPtr, llvm::Value *key, llvm::Type *keyTy, llvm::Type *valTy) {
    llvm::Value *idx = emitMapKeyLookup(containerPtr, key, keyTy);
    llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "mrem_found");

    llvm::BasicBlock *removeBB = llvm::BasicBlock::Create(*ctx_, "mrem.do", fn_);
    llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "mrem.end", fn_);
    builder_.CreateCondBr(found, removeBB, endBB);

    builder_.SetInsertPoint(removeBB);
    auto mf = loadMapHeader(containerPtr, "mrem");

    auto bc = emitHashTableRemoveBucket(containerPtr, mapHeaderTy_, kMapLayout,
                                         key, keyTy, idx, "mrem");

    // Swap-remove from keys and values arrays
    llvm::Value *lastIdx = builder_.CreateSub(mf.len, llvm::ConstantInt::get(i64Ty_, 1), "mrem_last_idx");
    llvm::Value *isNotLast = builder_.CreateICmpNE(idx, lastIdx, "mrem_not_last");

    llvm::BasicBlock *swapBB = llvm::BasicBlock::Create(*ctx_, "mrem.swap", fn_);
    llvm::BasicBlock *decBB = llvm::BasicBlock::Create(*ctx_, "mrem.dec", fn_);
    builder_.CreateCondBr(isNotLast, swapBB, decBB);

    builder_.SetInsertPoint(swapBB);
    llvm::Value *lastKeyPtr = builder_.CreateGEP(keyTy, mf.keys, {lastIdx}, "mrem_last_kp");
    llvm::Value *lastKey = builder_.CreateLoad(keyTy, lastKeyPtr, "mrem_last_key");
    llvm::Value *dstKeyPtr = builder_.CreateGEP(keyTy, mf.keys, {idx}, "mrem_dst_kp");
    builder_.CreateStore(lastKey, dstKeyPtr);
    llvm::Value *lastValPtr = builder_.CreateGEP(valTy, mf.vals, {lastIdx}, "mrem_last_vp");
    llvm::Value *lastVal = builder_.CreateLoad(valTy, lastValPtr, "mrem_last_val");
    llvm::Value *dstValPtr = builder_.CreateGEP(valTy, mf.vals, {idx}, "mrem_dst_vp");
    builder_.CreateStore(lastVal, dstValPtr);

    emitHashTableUpdateIndex(bc, lastKey, keyTy, lastIdx, idx, "mrem_swap");
    builder_.CreateBr(decBB);

    builder_.SetInsertPoint(decBB);
    llvm::Value *newLen = builder_.CreateSub(mf.len, llvm::ConstantInt::get(i64Ty_, 1), "mrem_new_len");
    builder_.CreateStore(newLen, mf.lenPtr);
    builder_.CreateBr(endBB);

    builder_.SetInsertPoint(endBB);
    return llvm::ConstantInt::get(i64Ty_, 0);
}

// ===== Dispatcher =====

llvm::Value *CodeGen::emitCollOp_remove(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;
    llvm::Value *containerPtr = emitExpr(*e.args[0]);

    if (llvm::Type *elemTy = getSetElementType(containerPtr)) {
        llvm::Value *elem = emitExpr(*e.args[1]);
        if (elem->getType() != elemTy)
            codegenError("remove() element type mismatch");
        return emitSetRemove(containerPtr, elem, elemTy);
    }
    if (llvm::Type *listElemTy = getListElementType(containerPtr)) {
        llvm::Value *val = emitExpr(*e.args[1]);
        if (val->getType() != listElemTy)
            codegenError("remove() value type mismatch with list element type");
        return emitListRemove(containerPtr, val, listElemTy);
    }
    llvm::Type *keyTy = getMapKeyType(containerPtr);
    llvm::Type *valTy = getMapValueType(containerPtr);
    if (keyTy && valTy) {
        llvm::Value *key = emitExpr(*e.args[1]);
        if (key->getType() != keyTy)
            codegenError("remove() key type mismatch");
        return emitMapRemove(containerPtr, key, keyTy, valTy);
    }
    return nullptr;
}

llvm::Value *CodeGen::emitCollOp_append(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;
    // append(list, val) -> mutating append
    llvm::Value *listPtr = emitExpr(*e.args[0]);
    llvm::Type *elemTy = getListElementType(listPtr);
    if (elemTy) {
        llvm::Value *val = emitExpr(*e.args[1]);
        if (val->getType() != elemTy)
            codegenError("append() element type mismatch");

        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        auto mallocFn = getStdlibMalloc();
        auto memcpyFn = getStdlibMemcpy();
        auto freeFn = getStdlibFree();

        auto lf = loadListHeader(listPtr, "app");

        // Check if realloc needed
        llvm::Value *needGrow = builder_.CreateICmpEQ(lf.len, lf.cap, "app_need_grow");
        llvm::BasicBlock *growBB = llvm::BasicBlock::Create(*ctx_, "app.grow", fn_);
        llvm::BasicBlock *storeBB = llvm::BasicBlock::Create(*ctx_, "app.store", fn_);

        builder_.CreateCondBr(needGrow, growBB, storeBB);

        // Grow: new_cap = cap * 2 (min 4)
        builder_.SetInsertPoint(growBB);
        llvm::Value *four = llvm::ConstantInt::get(i64Ty_, 4);
        llvm::Value *doubled = builder_.CreateMul(lf.cap, llvm::ConstantInt::get(i64Ty_, 2), "app_doubled");
        llvm::Value *newCap = builder_.CreateSelect(
            builder_.CreateICmpSGT(doubled, four, "cap_gt4"), doubled, four, "app_new_cap");
        llvm::Value *newSize = builder_.CreateMul(newCap, llvm::ConstantInt::get(i64Ty_, elemSize), "app_new_size");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {newSize}, "app_new_data");
        llvm::Value *oldSize = builder_.CreateMul(lf.len, llvm::ConstantInt::get(i64Ty_, elemSize), "app_old_size");
        builder_.CreateCall(memcpyFn, {newData, lf.data, oldSize});
        builder_.CreateCall(freeFn, {lf.data});
        builder_.CreateStore(newData, lf.dataPtr);
        builder_.CreateStore(newCap, lf.capPtr);
        builder_.CreateBr(storeBB);

        // Store the new element
        builder_.SetInsertPoint(storeBB);
        llvm::Value *curData = builder_.CreateLoad(ptrTy_, lf.dataPtr, "app_cur_data");
        llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lf.lenPtr, "app_cur_len");
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, curData, curLen, "app_elem_ptr");
        builder_.CreateStore(val, elemPtr);
        llvm::Value *newLen = builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1), "app_new_len");
        builder_.CreateStore(newLen, lf.lenPtr);

        return llvm::ConstantInt::get(i64Ty_, 0);
    }
    return nullptr;
}

llvm::Value *CodeGen::emitCollOp_appended(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;
    // appended(list, elem) -> new list with element added
    llvm::Value *listPtr = emitExpr(*e.args[0]);
    llvm::Type *elemTy = getListElementType(listPtr);
    if (elemTy) {
        llvm::Value *val = emitExpr(*e.args[1]);
        if (val->getType() != elemTy)
            codegenError("appended() element type mismatch");

        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        auto mallocFn = getStdlibMalloc();
        auto memcpyFn = getStdlibMemcpy();

        auto lf = loadListHeader(listPtr, "apd");
        llvm::Value *newLen = builder_.CreateAdd(lf.len, llvm::ConstantInt::get(i64Ty_, 1), "apd_new_len");

        llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "apd_header");
        llvm::Value *newDataSize = builder_.CreateMul(newLen, llvm::ConstantInt::get(i64Ty_, elemSize), "apd_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {newDataSize}, "apd_nd");

        llvm::Value *oldDataSize = builder_.CreateMul(lf.len, llvm::ConstantInt::get(i64Ty_, elemSize), "apd_ods");
        builder_.CreateCall(memcpyFn, {newData, lf.data, oldDataSize});

        llvm::Value *newElemPtr = builder_.CreateGEP(elemTy, newData, lf.len, "apd_new_ep");
        builder_.CreateStore(val, newElemPtr);

        builder_.CreateStore(newLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 0));
        builder_.CreateStore(newLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 1));
        builder_.CreateStore(newData, builder_.CreateStructGEP(listHeaderTy_, newHeader, 2));

        list_element_types_[newHeader] = elemTy;
        return newHeader;
    }
    return nullptr;
}

llvm::Value *CodeGen::emitCollOp_pop(const CallExpr &e) {
    if (e.args.size() != 1) return nullptr;
    // pop(list) -> Option<T>: remove and return last element
    llvm::Value *listPtr = emitExpr(*e.args[0]);
    llvm::Type *elemTy = getListElementType(listPtr);
    if (elemTy) {
        llvm::StructType *optTy = getOptionType(elemTy);
        auto lf = loadListHeader(listPtr, "pop");

        llvm::Value *isEmpty = builder_.CreateICmpEQ(lf.len, llvm::ConstantInt::get(i64Ty_, 0), "pop_empty");
        llvm::BasicBlock *emptyBB = llvm::BasicBlock::Create(*ctx_, "pop.empty", fn_);
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "pop.ok", fn_);
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "pop.merge", fn_);
        builder_.CreateCondBr(isEmpty, emptyBB, okBB);

        builder_.SetInsertPoint(emptyBB);
        llvm::Value *noneVal = buildNoneValue(optTy);
        builder_.CreateBr(mergeBB);
        llvm::BasicBlock *emptyEndBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(okBB);
        llvm::Value *lastIdx = builder_.CreateSub(lf.len, llvm::ConstantInt::get(i64Ty_, 1), "pop_last_idx");
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, lf.data, lastIdx, "pop_elem_ptr");
        llvm::Value *val = builder_.CreateLoad(elemTy, elemPtr, "pop_val");
        builder_.CreateStore(lastIdx, lf.lenPtr);
        llvm::Value *someVal = buildSomeValue(val, optTy);
        builder_.CreateBr(mergeBB);
        llvm::BasicBlock *okEndBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = builder_.CreatePHI(optTy, 2, "pop_result");
        phi->addIncoming(noneVal, emptyEndBB);
        phi->addIncoming(someVal, okEndBB);
        return phi;
    }
    return nullptr;
}

llvm::Value *CodeGen::emitCollOp_slice(const CallExpr &e) {
    if (e.args.size() != 3) return nullptr;
    // slice(list, start, end) -> new sub-list
    llvm::Value *listPtr = emitExpr(*e.args[0]);
    llvm::Type *elemTy = getListElementType(listPtr);
    if (elemTy) {
        llvm::Value *startVal = emitExpr(*e.args[1]);
        llvm::Value *endVal = emitExpr(*e.args[2]);

        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        auto mallocFn = getStdlibMalloc();
        auto memcpyFn = getStdlibMemcpy();

        auto lf = loadListHeader(listPtr, "sl");

        // Clamp start and end
        llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
        llvm::Value *clampedStart = builder_.CreateSelect(
            builder_.CreateICmpSLT(startVal, zero), zero, startVal, "sl_cstart");
        clampedStart = builder_.CreateSelect(
            builder_.CreateICmpSGT(clampedStart, lf.len), lf.len, clampedStart, "sl_cstart2");
        llvm::Value *clampedEnd = builder_.CreateSelect(
            builder_.CreateICmpSLT(endVal, zero), zero, endVal, "sl_cend");
        clampedEnd = builder_.CreateSelect(
            builder_.CreateICmpSGT(clampedEnd, lf.len), lf.len, clampedEnd, "sl_cend2");

        // Compute count = max(0, end - start)
        llvm::Value *diff = builder_.CreateSub(clampedEnd, clampedStart, "sl_diff");
        llvm::Value *count = builder_.CreateSelect(
            builder_.CreateICmpSGT(diff, zero), diff, zero, "sl_count");

        // Allocate new list
        llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "sl_header");
        llvm::Value *dataSize = builder_.CreateMul(count, llvm::ConstantInt::get(i64Ty_, elemSize), "sl_dsize");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "sl_data");

        // Copy elements
        llvm::Value *srcOffset = builder_.CreateGEP(elemTy, lf.data, clampedStart, "sl_src_off");
        builder_.CreateCall(memcpyFn, {newData, srcOffset, dataSize});

        // Set header fields
        llvm::Value *newLenPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 0, "sl_new_len");
        builder_.CreateStore(count, newLenPtr);
        llvm::Value *newCapPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 1, "sl_new_cap");
        builder_.CreateStore(count, newCapPtr);
        llvm::Value *newDataField = builder_.CreateStructGEP(listHeaderTy_, newHeader, 2, "sl_new_data");
        builder_.CreateStore(newData, newDataField);

        list_element_types_[newHeader] = elemTy;
        return newHeader;
    }
    return nullptr;
}

llvm::Value *CodeGen::emitCollOp_take(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;
    // take(list, n) -> new list with first n elements
    llvm::Value *listPtr = emitExpr(*e.args[0]);
    llvm::Type *elemTy = getListElementType(listPtr);
    if (elemTy) {
        llvm::Value *nVal = emitExpr(*e.args[1]);

        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        auto mallocFn = getStdlibMalloc();
        auto memcpyFn = getStdlibMemcpy();

        auto lf = loadListHeader(listPtr, "tk");

        // Clamp n: max(0, min(n, len))
        llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
        llvm::Value *clampedN = builder_.CreateSelect(
            builder_.CreateICmpSLT(nVal, zero), zero, nVal, "tk_cn");
        clampedN = builder_.CreateSelect(
            builder_.CreateICmpSGT(clampedN, lf.len), lf.len, clampedN, "tk_cn2");

        // Allocate new list
        llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "tk_header");
        llvm::Value *dataSize = builder_.CreateMul(clampedN, llvm::ConstantInt::get(i64Ty_, elemSize), "tk_dsize");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "tk_data");

        // Copy elements
        builder_.CreateCall(memcpyFn, {newData, lf.data, dataSize});

        // Set header fields
        llvm::Value *newLenPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 0, "tk_new_len");
        builder_.CreateStore(clampedN, newLenPtr);
        llvm::Value *newCapPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 1, "tk_new_cap");
        builder_.CreateStore(clampedN, newCapPtr);
        llvm::Value *newDataField = builder_.CreateStructGEP(listHeaderTy_, newHeader, 2, "tk_new_data");
        builder_.CreateStore(newData, newDataField);

        list_element_types_[newHeader] = elemTy;
        return newHeader;
    }
    return nullptr;
}

llvm::Value *CodeGen::emitCollOp_insert(const CallExpr &e) {
    if (e.args.size() != 3) return nullptr;
    // insert(list, index, value)
    llvm::Value *listPtr = emitExpr(*e.args[0]);
    llvm::Type *elemTy = getListElementType(listPtr);
    if (elemTy) {
        llvm::Value *idx = emitExpr(*e.args[1]);
        if (idx->getType() != i64Ty_)
            codegenError("insert() index must be int");
        llvm::Value *val = emitExpr(*e.args[2]);
        if (val->getType() != elemTy)
            codegenError("insert() element type mismatch");

        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);

        auto mallocFn = getStdlibMalloc();
        auto memcpyFn = getStdlibMemcpy();
        auto freeFn = getStdlibFree();
        auto memmoveFn = getStdlibMemmove();

        auto lf = loadListHeader(listPtr, "ins");

        // Bounds check
        llvm::Value *outOfBounds = builder_.CreateOr(
            builder_.CreateICmpSLT(idx, llvm::ConstantInt::get(i64Ty_, 0)),
            builder_.CreateICmpSGT(idx, lf.len), "ins_oob");
        llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "ins.err", fn_);
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "ins.ok", fn_);
        builder_.CreateCondBr(outOfBounds, errBB, okBB);
        builder_.SetInsertPoint(errBB);
        emitRuntimeError("runtime error: insert() index out of bounds\n", ".ins_oob_err");

        builder_.SetInsertPoint(okBB);
        // Check if realloc needed
        llvm::Value *needGrow = builder_.CreateICmpEQ(lf.len, lf.cap, "ins_need_grow");
        llvm::BasicBlock *growBB = llvm::BasicBlock::Create(*ctx_, "ins.grow", fn_);
        llvm::BasicBlock *moveBB = llvm::BasicBlock::Create(*ctx_, "ins.move", fn_);
        builder_.CreateCondBr(needGrow, growBB, moveBB);

        builder_.SetInsertPoint(growBB);
        llvm::Value *four = llvm::ConstantInt::get(i64Ty_, 4);
        llvm::Value *doubled = builder_.CreateMul(lf.cap, llvm::ConstantInt::get(i64Ty_, 2), "ins_doubled");
        llvm::Value *newCap = builder_.CreateSelect(
            builder_.CreateICmpSGT(doubled, four), doubled, four, "ins_new_cap");
        llvm::Value *newSize = builder_.CreateMul(newCap, llvm::ConstantInt::get(i64Ty_, elemSize), "ins_new_size");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {newSize}, "ins_new_data");
        llvm::Value *oldSize = builder_.CreateMul(lf.len, llvm::ConstantInt::get(i64Ty_, elemSize), "ins_old_size");
        builder_.CreateCall(memcpyFn, {newData, lf.data, oldSize});
        builder_.CreateCall(freeFn, {lf.data});
        builder_.CreateStore(newData, lf.dataPtr);
        builder_.CreateStore(newCap, lf.capPtr);
        builder_.CreateBr(moveBB);

        builder_.SetInsertPoint(moveBB);
        llvm::Value *curData = builder_.CreateLoad(ptrTy_, lf.dataPtr, "ins_cur_data");
        // memmove elements from idx to idx+1
        llvm::Value *srcPtr = builder_.CreateGEP(elemTy, curData, {idx}, "ins_src");
        llvm::Value *dstPtr = builder_.CreateGEP(elemTy, curData,
            {builder_.CreateAdd(idx, llvm::ConstantInt::get(i64Ty_, 1))}, "ins_dst");
        llvm::Value *moveCount = builder_.CreateSub(lf.len, idx, "ins_move_count");
        llvm::Value *moveBytes = builder_.CreateMul(moveCount, llvm::ConstantInt::get(i64Ty_, elemSize), "ins_move_bytes");
        builder_.CreateCall(memmoveFn, {dstPtr, srcPtr, moveBytes});
        // Store new element at idx
        llvm::Value *insertPtr = builder_.CreateGEP(elemTy, curData, {idx}, "ins_ptr");
        builder_.CreateStore(val, insertPtr);
        // len++
        llvm::Value *newLen = builder_.CreateAdd(lf.len, llvm::ConstantInt::get(i64Ty_, 1), "ins_new_len");
        builder_.CreateStore(newLen, lf.lenPtr);

        return llvm::ConstantInt::get(i64Ty_, 0);
    }
    return nullptr;
}

llvm::Value *CodeGen::emitCollOp_remove_at(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;
    // remove_at(list, index)
    llvm::Value *listPtr = emitExpr(*e.args[0]);
    llvm::Type *elemTy = getListElementType(listPtr);
    if (elemTy) {
        llvm::Value *idx = emitExpr(*e.args[1]);
        if (idx->getType() != i64Ty_)
            codegenError("remove_at() index must be int");

        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);

        auto memmoveFn = getStdlibMemmove();

        auto lf = loadListHeader(listPtr, "rmat");

        // Bounds check
        llvm::Value *outOfBounds = builder_.CreateOr(
            builder_.CreateICmpSLT(idx, llvm::ConstantInt::get(i64Ty_, 0)),
            builder_.CreateICmpSGE(idx, lf.len), "rmat_oob");
        llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "rmat.err", fn_);
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "rmat.ok", fn_);
        builder_.CreateCondBr(outOfBounds, errBB, okBB);
        builder_.SetInsertPoint(errBB);
        emitRuntimeError("runtime error: remove_at() index out of bounds\n", ".rmat_oob_err");

        builder_.SetInsertPoint(okBB);
        // Save element to return
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, lf.data, {idx}, "rmat_elem_ptr");
        llvm::Value *removedVal = builder_.CreateLoad(elemTy, elemPtr, "rmat_val");
        // memmove elements from idx+1 to idx
        llvm::Value *srcPtr = builder_.CreateGEP(elemTy, lf.data,
            {builder_.CreateAdd(idx, llvm::ConstantInt::get(i64Ty_, 1))}, "rmat_src");
        llvm::Value *moveCount = builder_.CreateSub(
            builder_.CreateSub(lf.len, idx), llvm::ConstantInt::get(i64Ty_, 1), "rmat_move_count");
        llvm::Value *moveBytes = builder_.CreateMul(moveCount, llvm::ConstantInt::get(i64Ty_, elemSize), "rmat_move_bytes");
        builder_.CreateCall(memmoveFn, {elemPtr, srcPtr, moveBytes});
        // len--
        llvm::Value *newLen = builder_.CreateSub(lf.len, llvm::ConstantInt::get(i64Ty_, 1), "rmat_new_len");
        builder_.CreateStore(newLen, lf.lenPtr);

        return removedVal;
    }
    return nullptr;
}

llvm::Value *CodeGen::emitCollOp_distinct(const CallExpr &e) {
    if (e.args.size() != 1) return nullptr;
    // distinct(list) -> new list with duplicates removed
    llvm::Value *listVal = emitExpr(*e.args[0]);
    llvm::Type *elemTy = getListElementType(listVal);
    if (!elemTy)
        codegenError("distinct() requires a list as argument");

    // Reject non-string pointer elements (e.g. list-of-lists) -- strcmp would be UB
    if (elemTy == ptrTy_ && getNestedListElementType(listVal))
        codegenError("distinct() is not supported for lists of non-string pointer elements");

    auto lf = loadListHeader(listVal, "dist_src");

    // Allocate new list (capacity = source length)
    auto mallocFn = getStdlibMalloc();
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
    uint64_t elemSize = dl.getTypeAllocSize(elemTy);

    llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "dist_header");
    llvm::Value *dataSize = builder_.CreateMul(lf.len, llvm::ConstantInt::get(i64Ty_, elemSize), "dist_data_size");
    llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "dist_data");

    builder_.CreateStore(newData, builder_.CreateStructGEP(listHeaderTy_, newHeader, 2, "dist_data_field"));
    builder_.CreateStore(lf.len, builder_.CreateStructGEP(listHeaderTy_, newHeader, 1, "dist_cap_ptr"));

    // Output length counter
    llvm::AllocaInst *outLen = builder_.CreateAlloca(i64Ty_, nullptr, "dist_out_len");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), outLen);

    // Outer loop: for each source element
    llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "dist_i");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

    llvm::BasicBlock *outerCondBB = llvm::BasicBlock::Create(*ctx_, "dist.ocond", fn_);
    llvm::BasicBlock *outerBodyBB = llvm::BasicBlock::Create(*ctx_, "dist.obody", fn_);
    llvm::BasicBlock *outerEndBB = llvm::BasicBlock::Create(*ctx_, "dist.oend", fn_);

    builder_.CreateBr(outerCondBB);
    builder_.SetInsertPoint(outerCondBB);
    llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "dist_iv");
    builder_.CreateCondBr(builder_.CreateICmpSLT(iVal, lf.len), outerBodyBB, outerEndBB);

    builder_.SetInsertPoint(outerBodyBB);
    llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "dist_ic");
    llvm::Value *srcElemPtr = builder_.CreateGEP(elemTy, lf.data, {iCur}, "dist_src_ep");
    llvm::Value *srcElem = builder_.CreateLoad(elemTy, srcElemPtr, "dist_src_elem");

    // Inner loop: check if srcElem already exists in output
    llvm::AllocaInst *dupFound = builder_.CreateAlloca(i1Ty_, nullptr, "dist_dup");
    builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 0), dupFound);
    llvm::AllocaInst *jVar = builder_.CreateAlloca(i64Ty_, nullptr, "dist_j");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), jVar);

    llvm::BasicBlock *innerCondBB = llvm::BasicBlock::Create(*ctx_, "dist.icond", fn_);
    llvm::BasicBlock *innerBodyBB = llvm::BasicBlock::Create(*ctx_, "dist.ibody", fn_);
    llvm::BasicBlock *innerEndBB = llvm::BasicBlock::Create(*ctx_, "dist.iend", fn_);

    llvm::Value *curOutLen = builder_.CreateLoad(i64Ty_, outLen, "dist_cur_out");
    builder_.CreateBr(innerCondBB);

    builder_.SetInsertPoint(innerCondBB);
    llvm::Value *jVal = builder_.CreateLoad(i64Ty_, jVar, "dist_jv");
    llvm::Value *notDup = builder_.CreateICmpEQ(builder_.CreateLoad(i1Ty_, dupFound), llvm::ConstantInt::get(i1Ty_, 0), "dist_not_dup");
    llvm::Value *jInBounds = builder_.CreateICmpSLT(jVal, curOutLen, "dist_j_inb");
    llvm::Value *innerCont = builder_.CreateAnd(notDup, jInBounds, "dist_icont");
    builder_.CreateCondBr(innerCont, innerBodyBB, innerEndBB);

    builder_.SetInsertPoint(innerBodyBB);
    llvm::Value *jCur = builder_.CreateLoad(i64Ty_, jVar, "dist_jc");
    llvm::Value *outElemPtr = builder_.CreateGEP(elemTy, newData, {jCur}, "dist_out_ep");
    llvm::Value *outElem = builder_.CreateLoad(elemTy, outElemPtr, "dist_out_elem");

    llvm::Value *match;
    if (elemTy == ptrTy_) {
        auto strcmpFn = getStdlibStrcmp();
        llvm::Value *cmpResult = builder_.CreateCall(strcmpFn, {srcElem, outElem}, "dist_strcmp");
        match = builder_.CreateICmpEQ(cmpResult, llvm::ConstantInt::get(i32Ty_, 0), "dist_match");
    } else if (elemTy->isDoubleTy()) {
        match = builder_.CreateFCmpOEQ(srcElem, outElem, "dist_match");
    } else {
        match = builder_.CreateICmpEQ(srcElem, outElem, "dist_match");
    }

    llvm::BasicBlock *dupBB = llvm::BasicBlock::Create(*ctx_, "dist.dup", fn_);
    llvm::BasicBlock *innerNextBB = llvm::BasicBlock::Create(*ctx_, "dist.inext", fn_);
    builder_.CreateCondBr(match, dupBB, innerNextBB);

    builder_.SetInsertPoint(dupBB);
    builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 1), dupFound);
    builder_.CreateBr(innerCondBB);

    builder_.SetInsertPoint(innerNextBB);
    builder_.CreateStore(builder_.CreateAdd(jCur, llvm::ConstantInt::get(i64Ty_, 1)), jVar);
    builder_.CreateBr(innerCondBB);

    // After inner loop: if not duplicate, add to output
    builder_.SetInsertPoint(innerEndBB);
    llvm::Value *isDup = builder_.CreateLoad(i1Ty_, dupFound, "dist_is_dup");

    llvm::BasicBlock *addBB = llvm::BasicBlock::Create(*ctx_, "dist.add", fn_);
    llvm::BasicBlock *outerNextBB = llvm::BasicBlock::Create(*ctx_, "dist.onext", fn_);
    builder_.CreateCondBr(isDup, outerNextBB, addBB);

    builder_.SetInsertPoint(addBB);
    llvm::Value *outIdx = builder_.CreateLoad(i64Ty_, outLen, "dist_out_idx");
    llvm::Value *dstPtr = builder_.CreateGEP(elemTy, newData, {outIdx}, "dist_dst");
    builder_.CreateStore(srcElem, dstPtr);
    builder_.CreateStore(builder_.CreateAdd(outIdx, llvm::ConstantInt::get(i64Ty_, 1)), outLen);
    builder_.CreateBr(outerNextBB);

    builder_.SetInsertPoint(outerNextBB);
    builder_.CreateStore(builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
    builder_.CreateBr(outerCondBB);

    // End: set final length
    builder_.SetInsertPoint(outerEndBB);
    llvm::Value *finalLen = builder_.CreateLoad(i64Ty_, outLen, "dist_final_len");
    builder_.CreateStore(finalLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 0, "dist_len_ptr"));

    list_element_types_[newHeader] = elemTy;
    return newHeader;
}

llvm::Value *CodeGen::emitCollOp_flatten(const CallExpr &e) {
    if (e.args.size() != 1) return nullptr;
    // flatten(list) -> flatten nested list one level
    llvm::Value *listVal = emitExpr(*e.args[0]);
    llvm::Type *outerElemTy = getListElementType(listVal);
    if (!outerElemTy || outerElemTy != ptrTy_)
        codegenError("flatten() requires a list of lists");

    // Look up the inner element type
    llvm::Type *innerElemTy = getNestedListElementType(listVal);
    if (!innerElemTy)
        codegenError("flatten() cannot determine inner list element type; use a list literal (e.g. [[1, 2], [3, 4]])");

    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
    uint64_t innerElemSize = dl.getTypeAllocSize(innerElemTy);

    auto mallocFn = getStdlibMalloc();
    auto memcpyFn = getStdlibMemcpy();

    auto lf = loadListHeader(listVal, "flat_o");
    llvm::Value *outerLen = lf.len;
    llvm::Value *outerData = lf.data;

    // Pass 1: sum all inner lengths
    llvm::AllocaInst *totalLen = builder_.CreateAlloca(i64Ty_, nullptr, "flat_total");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), totalLen);
    {
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "flat_s_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "flat.s.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "flat.s.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "flat.s.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "flat_si");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, outerLen), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *innerPtr = builder_.CreateGEP(ptrTy_, outerData, {i}, "flat_inner_ptr");
        llvm::Value *innerList = builder_.CreateLoad(ptrTy_, innerPtr, "flat_inner");
        llvm::Value *innerLenPtr = builder_.CreateStructGEP(listHeaderTy_, innerList, 0, "flat_ilen_ptr");
        llvm::Value *innerLen = builder_.CreateLoad(i64Ty_, innerLenPtr, "flat_ilen");
        llvm::Value *curTotal = builder_.CreateLoad(i64Ty_, totalLen, "flat_cur_total");
        builder_.CreateStore(builder_.CreateAdd(curTotal, innerLen), totalLen);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(endBB);
    }

    // Allocate new list
    llvm::Value *total = builder_.CreateLoad(i64Ty_, totalLen, "flat_total_len");
    llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "flat_hdr");
    llvm::Value *dataSize = builder_.CreateMul(total, llvm::ConstantInt::get(i64Ty_, innerElemSize), "flat_ds");
    llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "flat_data");

    // Set header
    builder_.CreateStore(total, builder_.CreateStructGEP(listHeaderTy_, newHeader, 0));
    builder_.CreateStore(total, builder_.CreateStructGEP(listHeaderTy_, newHeader, 1));
    builder_.CreateStore(newData, builder_.CreateStructGEP(listHeaderTy_, newHeader, 2));

    // Pass 2: copy each inner list's data
    llvm::AllocaInst *offset = builder_.CreateAlloca(i64Ty_, nullptr, "flat_off");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), offset);
    {
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "flat_c_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "flat.c.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "flat.c.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "flat.c.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "flat_ci");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, outerLen), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *innerPtr = builder_.CreateGEP(ptrTy_, outerData, {i}, "flat_c_inner_ptr");
        llvm::Value *innerList = builder_.CreateLoad(ptrTy_, innerPtr, "flat_c_inner");
        llvm::Value *innerLenPtr = builder_.CreateStructGEP(listHeaderTy_, innerList, 0, "flat_c_ilen_ptr");
        llvm::Value *innerLen = builder_.CreateLoad(i64Ty_, innerLenPtr, "flat_c_ilen");
        llvm::Value *innerDataField = builder_.CreateStructGEP(listHeaderTy_, innerList, 2, "flat_c_idata_field");
        llvm::Value *innerData = builder_.CreateLoad(ptrTy_, innerDataField, "flat_c_idata");

        llvm::Value *curOff = builder_.CreateLoad(i64Ty_, offset, "flat_cur_off");
        llvm::Value *dstPtr = builder_.CreateGEP(innerElemTy, newData, {curOff}, "flat_dst");
        llvm::Value *copyBytes = builder_.CreateMul(innerLen, llvm::ConstantInt::get(i64Ty_, innerElemSize), "flat_cb");
        builder_.CreateCall(memcpyFn, {dstPtr, innerData, copyBytes});
        builder_.CreateStore(builder_.CreateAdd(curOff, innerLen), offset);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(endBB);
    }

    list_element_types_[newHeader] = innerElemTy;
    return newHeader;
}

llvm::Value *CodeGen::emitCollOp_items(const CallExpr &e) {
    if (e.args.size() != 1) return nullptr;
    // items(map) -> List<(K, V)>
    llvm::Value *mapPtr = emitExpr(*e.args[0]);
    llvm::Type *keyTy = getMapKeyType(mapPtr);
    llvm::Type *valTy = getMapValueType(mapPtr);
    if (keyTy && valTy) {
        auto mf = loadMapHeader(mapPtr, "items");

        llvm::StructType *tupleTy = llvm::StructType::get(*ctx_, {keyTy, valTy});
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        uint64_t tupleSize = dl.getTypeAllocSize(tupleTy);

        auto mallocFn = getStdlibMalloc();

        llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "items_hdr");
        llvm::Value *dataSize = builder_.CreateMul(mf.len, llvm::ConstantInt::get(i64Ty_, tupleSize), "items_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "items_data");

        // Fill tuples
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "items_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "items.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "items.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "items.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "items_ci");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, mf.len), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *kp = builder_.CreateGEP(keyTy, mf.keys, {i}, "items_kp");
        llvm::Value *vp = builder_.CreateGEP(valTy, mf.vals, {i}, "items_vp");
        llvm::Value *k = builder_.CreateLoad(keyTy, kp, "items_k");
        llvm::Value *v = builder_.CreateLoad(valTy, vp, "items_v");
        llvm::Value *tuple = llvm::UndefValue::get(tupleTy);
        tuple = builder_.CreateInsertValue(tuple, k, 0);
        tuple = builder_.CreateInsertValue(tuple, v, 1);
        llvm::Value *dp = builder_.CreateGEP(tupleTy, newData, {i}, "items_dp");
        builder_.CreateStore(tuple, dp);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(endBB);

        builder_.CreateStore(mf.len, builder_.CreateStructGEP(listHeaderTy_, newHeader, 0));
        builder_.CreateStore(mf.len, builder_.CreateStructGEP(listHeaderTy_, newHeader, 1));
        builder_.CreateStore(newData, builder_.CreateStructGEP(listHeaderTy_, newHeader, 2));
        list_element_types_[newHeader] = tupleTy;
        return newHeader;
    }
    return nullptr;
}

llvm::Value *CodeGen::emitCollOp_get(const CallExpr &e) {
    // get(map, key) -- 2-arg -> Option<V>
    if (e.args.size() == 2) {
        llvm::Value *mapPtr = emitExpr(*e.args[0]);
        llvm::Type *keyTy = getMapKeyType(mapPtr);
        llvm::Type *valTy = getMapValueType(mapPtr);
        if (keyTy && valTy) {
            llvm::Value *key = emitExpr(*e.args[1]);
            if (key->getType() != keyTy)
                codegenError("get() key type mismatch");
            llvm::StructType *optTy = getOptionType(valTy);
            llvm::Value *idx = emitMapKeyLookup(mapPtr, key, keyTy);
            llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "get2_found");

            llvm::BasicBlock *foundBB = llvm::BasicBlock::Create(*ctx_, "get2.found", fn_);
            llvm::BasicBlock *notFoundBB = llvm::BasicBlock::Create(*ctx_, "get2.notfound", fn_);
            llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "get2.merge", fn_);
            builder_.CreateCondBr(found, foundBB, notFoundBB);

            builder_.SetInsertPoint(foundBB);
            llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, mapPtr, 3, "get2_vals_field");
            llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "get2_vals");
            llvm::Value *valPtr = builder_.CreateGEP(valTy, valsPtr, {idx}, "get2_val_ptr");
            llvm::Value *foundVal = builder_.CreateLoad(valTy, valPtr, "get2_val");
            llvm::Value *someVal = buildSomeValue(foundVal, optTy);
            builder_.CreateBr(mergeBB);
            llvm::BasicBlock *foundEndBB = builder_.GetInsertBlock();

            builder_.SetInsertPoint(notFoundBB);
            llvm::Value *noneVal = buildNoneValue(optTy);
            builder_.CreateBr(mergeBB);
            llvm::BasicBlock *notFoundEndBB = builder_.GetInsertBlock();

            builder_.SetInsertPoint(mergeBB);
            llvm::PHINode *phi = builder_.CreatePHI(optTy, 2, "get2_result");
            phi->addIncoming(someVal, foundEndBB);
            phi->addIncoming(noneVal, notFoundEndBB);
            return phi;
        }
    }

    // get(map, key, default) -- 3-arg
    if (e.args.size() == 3) {
        llvm::Value *mapPtr = emitExpr(*e.args[0]);
        llvm::Type *keyTy = getMapKeyType(mapPtr);
        llvm::Type *valTy = getMapValueType(mapPtr);
        if (keyTy && valTy) {
            llvm::Value *key = emitExpr(*e.args[1]);
            if (key->getType() != keyTy)
                codegenError("get() key type mismatch");
            llvm::Value *defaultVal = emitExpr(*e.args[2]);
            if (defaultVal->getType() != valTy)
                codegenError("get() default value type must match map's value type");
            llvm::Value *idx = emitMapKeyLookup(mapPtr, key, keyTy);
            llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "get_found");

            llvm::BasicBlock *foundBB = llvm::BasicBlock::Create(*ctx_, "get.found", fn_);
            llvm::BasicBlock *notFoundBB = llvm::BasicBlock::Create(*ctx_, "get.notfound", fn_);
            llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "get.merge", fn_);
            builder_.CreateCondBr(found, foundBB, notFoundBB);

            builder_.SetInsertPoint(foundBB);
            llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, mapPtr, 3, "get_vals_field");
            llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "get_vals");
            llvm::Value *valPtr = builder_.CreateGEP(valTy, valsPtr, {idx}, "get_val_ptr");
            llvm::Value *foundVal = builder_.CreateLoad(valTy, valPtr, "get_val");
            llvm::BasicBlock *foundEndBB = builder_.GetInsertBlock();
            builder_.CreateBr(mergeBB);

            builder_.SetInsertPoint(notFoundBB);
            llvm::BasicBlock *notFoundEndBB = builder_.GetInsertBlock();
            builder_.CreateBr(mergeBB);

            builder_.SetInsertPoint(mergeBB);
            llvm::PHINode *phi = builder_.CreatePHI(valTy, 2, "get_result");
            phi->addIncoming(foundVal, foundEndBB);
            phi->addIncoming(defaultVal, notFoundEndBB);
            return phi;
        }
    }

    return nullptr;
}

llvm::Value *CodeGen::emitCollOp_merge(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;
    // merge(map1, map2) -> new map
    llvm::Value *map1 = emitExpr(*e.args[0]);
    llvm::Value *map2 = emitExpr(*e.args[1]);
    llvm::Type *keyTy = getMapKeyType(map1);
    llvm::Type *valTy = getMapValueType(map1);
    if (!keyTy || !valTy)
        codegenError("merge() requires maps as arguments");
    {
        llvm::Type *keyTy2 = getMapKeyType(map2);
        llvm::Type *valTy2 = getMapValueType(map2);
        if (!keyTy2 || keyTy2 != keyTy || !valTy2 || valTy2 != valTy)
            codegenError("merge() requires two maps with the same key and value types");

        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(mapHeaderTy_);
        uint64_t keySize = dl.getTypeAllocSize(keyTy);
        uint64_t valSize = dl.getTypeAllocSize(valTy);

        auto mallocFn = getStdlibMalloc();
        auto memcpyFn = getStdlibMemcpy();

        auto mf1 = loadMapHeader(map1, "mg1");
        auto mf2 = loadMapHeader(map2, "mg2");

        // Allocate new map with capacity = len1 + len2
        llvm::Value *maxCap = builder_.CreateAdd(mf1.len, mf2.len, "mg_max_cap");
        llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "mg_hdr");
        llvm::Value *newKeysSize = builder_.CreateMul(maxCap, llvm::ConstantInt::get(i64Ty_, keySize), "mg_ks");
        llvm::Value *newKeys = builder_.CreateCall(mallocFn, {newKeysSize}, "mg_keys");
        llvm::Value *newValsSize = builder_.CreateMul(maxCap, llvm::ConstantInt::get(i64Ty_, valSize), "mg_vs");
        llvm::Value *newVals = builder_.CreateCall(mallocFn, {newValsSize}, "mg_vals");

        // Copy all of map1
        llvm::Value *copy1KeySize = builder_.CreateMul(mf1.len, llvm::ConstantInt::get(i64Ty_, keySize), "mg_ck1");
        builder_.CreateCall(memcpyFn, {newKeys, mf1.keys, copy1KeySize});
        llvm::Value *copy1ValSize = builder_.CreateMul(mf1.len, llvm::ConstantInt::get(i64Ty_, valSize), "mg_cv1");
        builder_.CreateCall(memcpyFn, {newVals, mf1.vals, copy1ValSize});

        // Set up header
        llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, newHeader, 0, "mg_len_ptr");
        builder_.CreateStore(mf1.len, lenPtr);
        builder_.CreateStore(maxCap, builder_.CreateStructGEP(mapHeaderTy_, newHeader, 1));
        builder_.CreateStore(newKeys, builder_.CreateStructGEP(mapHeaderTy_, newHeader, 2));
        builder_.CreateStore(newVals, builder_.CreateStructGEP(mapHeaderTy_, newHeader, 3));

        // Init hash buckets
        emitBucketInit(newHeader, mapHeaderTy_, kMapLayout.bucketCountIdx, kMapLayout.bucketsPtrIdx, 16);

        // Re-hash map1 keys into new map's buckets
        {
            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "mg_rh_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            llvm::BasicBlock *rCondBB = llvm::BasicBlock::Create(*ctx_, "mg.rh.cond", fn_);
            llvm::BasicBlock *rBodyBB = llvm::BasicBlock::Create(*ctx_, "mg.rh.body", fn_);
            llvm::BasicBlock *rEndBB = llvm::BasicBlock::Create(*ctx_, "mg.rh.end", fn_);
            builder_.CreateBr(rCondBB);
            builder_.SetInsertPoint(rCondBB);
            llvm::Value *ri = builder_.CreateLoad(i64Ty_, iVar, "mg_ri");
            builder_.CreateCondBr(builder_.CreateICmpSLT(ri, mf1.len), rBodyBB, rEndBB);
            builder_.SetInsertPoint(rBodyBB);
            llvm::Value *kp = builder_.CreateGEP(keyTy, newKeys, {ri}, "mg_rh_kp");
            llvm::Value *kv = builder_.CreateLoad(keyTy, kp, "mg_rh_kv");
            emitBucketInsertAndRehashCheck(newHeader, mapHeaderTy_, kMapLayout.lenIdx, kMapLayout.bucketCountIdx, kMapLayout.bucketsPtrIdx, kv, keyTy, ri);
            builder_.CreateStore(builder_.CreateAdd(ri, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            builder_.CreateBr(rCondBB);
            builder_.SetInsertPoint(rEndBB);
        }

        // Add/update entries from map2
        {
            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "mg_i2");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "mg.add.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "mg.add.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "mg.add.end", fn_);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(condBB);
            llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "mg_ci");
            builder_.CreateCondBr(builder_.CreateICmpSLT(i, mf2.len), bodyBB, endBB);

            builder_.SetInsertPoint(bodyBB);
            llvm::Value *kp = builder_.CreateGEP(keyTy, mf2.keys, {i}, "mg_kp2");
            llvm::Value *kv = builder_.CreateLoad(keyTy, kp, "mg_kv2");
            llvm::Value *vp = builder_.CreateGEP(valTy, mf2.vals, {i}, "mg_vp2");
            llvm::Value *vv = builder_.CreateLoad(valTy, vp, "mg_vv2");

            // Check if key exists in new map
            llvm::Value *lookupIdx = emitMapKeyLookup(newHeader, kv, keyTy);
            llvm::Value *exists = builder_.CreateICmpSGE(lookupIdx, llvm::ConstantInt::get(i64Ty_, 0), "mg_exists");

            llvm::BasicBlock *updateBB = llvm::BasicBlock::Create(*ctx_, "mg.update", fn_);
            llvm::BasicBlock *insertBB = llvm::BasicBlock::Create(*ctx_, "mg.insert", fn_);
            llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "mg.next", fn_);
            builder_.CreateCondBr(exists, updateBB, insertBB);

            // Update existing key's value
            builder_.SetInsertPoint(updateBB);
            llvm::Value *curVals = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, newHeader, 3), "mg_cur_vals");
            llvm::Value *updPtr = builder_.CreateGEP(valTy, curVals, {lookupIdx}, "mg_upd_ptr");
            builder_.CreateStore(vv, updPtr);
            builder_.CreateBr(nextBB);

            // Insert new key-value pair
            builder_.SetInsertPoint(insertBB);
            llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "mg_cur_len");
            llvm::Value *curKeys = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, newHeader, 2), "mg_cur_keys");
            llvm::Value *newKeyPtr = builder_.CreateGEP(keyTy, curKeys, {curLen}, "mg_new_kp");
            builder_.CreateStore(kv, newKeyPtr);
            llvm::Value *curVals2 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, newHeader, 3), "mg_cur_vals2");
            llvm::Value *newValPtr = builder_.CreateGEP(valTy, curVals2, {curLen}, "mg_new_vp");
            builder_.CreateStore(vv, newValPtr);
            builder_.CreateStore(builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1)), lenPtr);
            emitBucketInsertAndRehashCheck(newHeader, mapHeaderTy_, kMapLayout.lenIdx, kMapLayout.bucketCountIdx, kMapLayout.bucketsPtrIdx, kv, keyTy, curLen);
            builder_.CreateBr(nextBB);

            builder_.SetInsertPoint(nextBB);
            builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(endBB);
        }

        map_key_types_[newHeader] = keyTy;
        map_value_types_[newHeader] = valTy;
        return newHeader;
    }
    return nullptr;
}

// ===== Builtin Set Ops =====

// Shared helper: check all elements of iterSet exist in lookupSet
llvm::Value *CodeGen::emitSubsetCheck(llvm::Value *iterSet, llvm::Value *lookupSet,
                                       const std::string &prefix) {
    llvm::Type *elemTy = getSetElementType(iterSet);
    if (!elemTy) return nullptr;
    llvm::Type *elemTy2 = getSetElementType(lookupSet);
    if (!elemTy2 || elemTy2 != elemTy)
        codegenError(prefix + "() requires two sets with the same element type");
    auto sf = loadSetHeader(iterSet, prefix);

    llvm::AllocaInst *resultVar = builder_.CreateAlloca(i1Ty_, nullptr, prefix + "_result");
    builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 1), resultVar);

    llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, prefix + "_i");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, prefix + ".cond", fn_);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, prefix + ".body", fn_);
    llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, prefix + ".end", fn_);
    builder_.CreateBr(condBB);
    builder_.SetInsertPoint(condBB);
    llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, prefix + "_ci");
    builder_.CreateCondBr(builder_.CreateICmpSLT(i, sf.len), bodyBB, endBB);
    builder_.SetInsertPoint(bodyBB);
    llvm::Value *ep = builder_.CreateGEP(elemTy, sf.elems, {i}, prefix + "_ep");
    llvm::Value *ev = builder_.CreateLoad(elemTy, ep, prefix + "_ev");
    llvm::Value *found = emitSetElementLookup(lookupSet, ev, elemTy);
    llvm::Value *notFound = builder_.CreateICmpSLT(found, llvm::ConstantInt::get(i64Ty_, 0), prefix + "_nf");
    llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, prefix + ".fail", fn_);
    llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, prefix + ".next", fn_);
    builder_.CreateCondBr(notFound, failBB, nextBB);
    builder_.SetInsertPoint(failBB);
    builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 0), resultVar);
    builder_.CreateBr(endBB);
    builder_.SetInsertPoint(nextBB);
    builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
    builder_.CreateBr(condBB);
    builder_.SetInsertPoint(endBB);

    return builder_.CreateLoad(i1Ty_, resultVar, prefix + "_result");
}

llvm::Value *CodeGen::emitBuiltinSetOps(const CallExpr &e) {
    using Handler = llvm::Value *(CodeGen::*)(const CallExpr &);
    static const std::unordered_map<std::string, Handler> dispatch = {
        {"union",                &CodeGen::emitSetOp_union},
        {"intersection",         &CodeGen::emitSetOp_intersection},
        {"difference",           &CodeGen::emitSetOp_difference},
        {"symmetric_difference", &CodeGen::emitSetOp_symmetric_difference},
        {"is_subset",            &CodeGen::emitSetOp_is_subset},
        {"is_superset",          &CodeGen::emitSetOp_is_superset},
    };
    auto it = dispatch.find(e.callee);
    if (it == dispatch.end()) return nullptr;
    return (this->*it->second)(e);
}

// ===== Set Operation Handlers =====

// union(set1, set2)
llvm::Value *CodeGen::emitSetOp_union(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;
    llvm::Value *set1 = emitExpr(*e.args[0]);
    llvm::Value *set2 = emitExpr(*e.args[1]);
    llvm::Type *elemTy = getSetElementType(set1);
    if (elemTy) {
        llvm::Type *elemTy2 = getSetElementType(set2);
        if (!elemTy2 || elemTy2 != elemTy)
            codegenError("union() requires two sets with the same element type");
        // Create new set with all elements from set1, then add elements from set2
        auto sf1 = loadSetHeader(set1, "u1");
        auto sf2 = loadSetHeader(set2, "u2");

        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(setHeaderTy_);
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        auto mallocFn = getStdlibMalloc();

        // Allocate max possible size (len1 + len2)
        llvm::Value *maxLen = builder_.CreateAdd(sf1.len, sf2.len, "u_max_len");
        llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "u_hdr");
        llvm::Value *dataSize = builder_.CreateMul(maxLen, llvm::ConstantInt::get(i64Ty_, elemSize), "u_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "u_data");

        // Copy all of set1
        auto memcpyFn = getStdlibMemcpy();
        llvm::Value *copy1Size = builder_.CreateMul(sf1.len, llvm::ConstantInt::get(i64Ty_, elemSize), "u_copy1_size");
        builder_.CreateCall(memcpyFn, {newData, sf1.elems, copy1Size});

        // Init header with len1
        llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, newHeader, 0, "u_len_ptr");
        builder_.CreateStore(sf1.len, lenPtr);
        builder_.CreateStore(maxLen, builder_.CreateStructGEP(setHeaderTy_, newHeader, 1));
        builder_.CreateStore(newData, builder_.CreateStructGEP(setHeaderTy_, newHeader, 2));

        // Init buckets for the new set
        emitBucketInit(newHeader, setHeaderTy_, kSetLayout.bucketCountIdx, kSetLayout.bucketsPtrIdx, 16);

        // Re-hash all elements from set1 into new set's buckets
        {
            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "u_rehash_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            llvm::BasicBlock *rCondBB = llvm::BasicBlock::Create(*ctx_, "u.rehash.cond", fn_);
            llvm::BasicBlock *rBodyBB = llvm::BasicBlock::Create(*ctx_, "u.rehash.body", fn_);
            llvm::BasicBlock *rEndBB = llvm::BasicBlock::Create(*ctx_, "u.rehash.end", fn_);
            builder_.CreateBr(rCondBB);
            builder_.SetInsertPoint(rCondBB);
            llvm::Value *ri = builder_.CreateLoad(i64Ty_, iVar, "u_ri");
            builder_.CreateCondBr(builder_.CreateICmpSLT(ri, sf1.len), rBodyBB, rEndBB);
            builder_.SetInsertPoint(rBodyBB);
            llvm::Value *ep = builder_.CreateGEP(elemTy, newData, {ri}, "u_rehash_ep");
            llvm::Value *ev = builder_.CreateLoad(elemTy, ep, "u_rehash_ev");
            emitBucketInsertAndRehashCheck(newHeader, setHeaderTy_, kSetLayout.lenIdx, kSetLayout.bucketCountIdx, kSetLayout.bucketsPtrIdx, ev, elemTy, ri);
            builder_.CreateStore(builder_.CreateAdd(ri, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            builder_.CreateBr(rCondBB);
            builder_.SetInsertPoint(rEndBB);
        }

        // Add elements from set2 that are not in set1
        {
            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "u_i2");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "u.add.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "u.add.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "u.add.end", fn_);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(condBB);
            llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "u_ci");
            builder_.CreateCondBr(builder_.CreateICmpSLT(i, sf2.len), bodyBB, endBB);
            builder_.SetInsertPoint(bodyBB);
            llvm::Value *ep = builder_.CreateGEP(elemTy, sf2.elems, {i}, "u_ep2");
            llvm::Value *ev = builder_.CreateLoad(elemTy, ep, "u_ev2");

            // Check if already in new set
            llvm::Value *lookupIdx = emitSetElementLookup(newHeader, ev, elemTy);
            llvm::Value *notFound = builder_.CreateICmpSLT(lookupIdx, llvm::ConstantInt::get(i64Ty_, 0), "u_not_found");
            llvm::BasicBlock *addBB = llvm::BasicBlock::Create(*ctx_, "u.add.do", fn_);
            llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "u.add.next", fn_);
            builder_.CreateCondBr(notFound, addBB, nextBB);

            builder_.SetInsertPoint(addBB);
            llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "u_cur_len");
            llvm::Value *storePtr = builder_.CreateGEP(elemTy, newData, {curLen}, "u_store_ptr");
            builder_.CreateStore(ev, storePtr);
            emitBucketInsertAndRehashCheck(newHeader, setHeaderTy_, kSetLayout.lenIdx, kSetLayout.bucketCountIdx, kSetLayout.bucketsPtrIdx, ev, elemTy, curLen);
            builder_.CreateStore(builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1)), lenPtr);
            builder_.CreateBr(nextBB);

            builder_.SetInsertPoint(nextBB);
            builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(endBB);
        }

        set_element_types_[newHeader] = elemTy;
        return newHeader;
    }
    return nullptr;
}

// intersection(set1, set2)
llvm::Value *CodeGen::emitSetOp_intersection(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;
    llvm::Value *set1 = emitExpr(*e.args[0]);
    llvm::Value *set2 = emitExpr(*e.args[1]);
    llvm::Type *elemTy = getSetElementType(set1);
    if (elemTy) {
        llvm::Type *elemTy2 = getSetElementType(set2);
        if (!elemTy2 || elemTy2 != elemTy)
            codegenError("intersection() requires two sets with the same element type");
        auto sf = loadSetHeader(set1, "is");

        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(setHeaderTy_);
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        auto mallocFn = getStdlibMalloc();

        llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "is_hdr");
        llvm::Value *dataSize = builder_.CreateMul(sf.len, llvm::ConstantInt::get(i64Ty_, elemSize), "is_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "is_data");

        llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, newHeader, 0, "is_len_ptr");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), lenPtr);
        builder_.CreateStore(sf.len, builder_.CreateStructGEP(setHeaderTy_, newHeader, 1));
        builder_.CreateStore(newData, builder_.CreateStructGEP(setHeaderTy_, newHeader, 2));
        emitBucketInit(newHeader, setHeaderTy_, kSetLayout.bucketCountIdx, kSetLayout.bucketsPtrIdx, 16);

        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "is_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "is.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "is.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "is.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "is_ci");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, sf.len), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *ep = builder_.CreateGEP(elemTy, sf.elems, {i}, "is_ep");
        llvm::Value *ev = builder_.CreateLoad(elemTy, ep, "is_ev");

        llvm::Value *inSet2 = emitSetElementLookup(set2, ev, elemTy);
        llvm::Value *found = builder_.CreateICmpSGE(inSet2, llvm::ConstantInt::get(i64Ty_, 0), "is_found");
        llvm::BasicBlock *addBB = llvm::BasicBlock::Create(*ctx_, "is.add", fn_);
        llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "is.next", fn_);
        builder_.CreateCondBr(found, addBB, nextBB);

        builder_.SetInsertPoint(addBB);
        llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "is_cur_len");
        llvm::Value *storePtr = builder_.CreateGEP(elemTy, newData, {curLen}, "is_store_ptr");
        builder_.CreateStore(ev, storePtr);
        emitBucketInsertAndRehashCheck(newHeader, setHeaderTy_, kSetLayout.lenIdx, kSetLayout.bucketCountIdx, kSetLayout.bucketsPtrIdx, ev, elemTy, curLen);
        builder_.CreateStore(builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1)), lenPtr);
        builder_.CreateBr(nextBB);

        builder_.SetInsertPoint(nextBB);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(endBB);

        set_element_types_[newHeader] = elemTy;
        return newHeader;
    }
    return nullptr;
}

// difference(set1, set2) -- elements in set1 not in set2
llvm::Value *CodeGen::emitSetOp_difference(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;
    llvm::Value *set1 = emitExpr(*e.args[0]);
    llvm::Value *set2 = emitExpr(*e.args[1]);
    llvm::Type *elemTy = getSetElementType(set1);
    if (elemTy) {
        llvm::Type *elemTy2 = getSetElementType(set2);
        if (!elemTy2 || elemTy2 != elemTy)
            codegenError("difference() requires two sets with the same element type");
        auto sf = loadSetHeader(set1, "df");

        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(setHeaderTy_);
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        auto mallocFn = getStdlibMalloc();

        llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "df_hdr");
        llvm::Value *dataSize = builder_.CreateMul(sf.len, llvm::ConstantInt::get(i64Ty_, elemSize), "df_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "df_data");

        llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, newHeader, 0, "df_len_ptr");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), lenPtr);
        builder_.CreateStore(sf.len, builder_.CreateStructGEP(setHeaderTy_, newHeader, 1));
        builder_.CreateStore(newData, builder_.CreateStructGEP(setHeaderTy_, newHeader, 2));
        emitBucketInit(newHeader, setHeaderTy_, kSetLayout.bucketCountIdx, kSetLayout.bucketsPtrIdx, 16);

        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "df_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "df.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "df.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "df.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "df_ci");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, sf.len), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *ep = builder_.CreateGEP(elemTy, sf.elems, {i}, "df_ep");
        llvm::Value *ev = builder_.CreateLoad(elemTy, ep, "df_ev");

        llvm::Value *inSet2 = emitSetElementLookup(set2, ev, elemTy);
        llvm::Value *notFound = builder_.CreateICmpSLT(inSet2, llvm::ConstantInt::get(i64Ty_, 0), "df_not_found");
        llvm::BasicBlock *addBB = llvm::BasicBlock::Create(*ctx_, "df.add", fn_);
        llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "df.next", fn_);
        builder_.CreateCondBr(notFound, addBB, nextBB);

        builder_.SetInsertPoint(addBB);
        llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "df_cur_len");
        llvm::Value *storePtr = builder_.CreateGEP(elemTy, newData, {curLen}, "df_store_ptr");
        builder_.CreateStore(ev, storePtr);
        emitBucketInsertAndRehashCheck(newHeader, setHeaderTy_, kSetLayout.lenIdx, kSetLayout.bucketCountIdx, kSetLayout.bucketsPtrIdx, ev, elemTy, curLen);
        builder_.CreateStore(builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1)), lenPtr);
        builder_.CreateBr(nextBB);

        builder_.SetInsertPoint(nextBB);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(endBB);

        set_element_types_[newHeader] = elemTy;
        return newHeader;
    }
    return nullptr;
}

// symmetric_difference(set1, set2)
llvm::Value *CodeGen::emitSetOp_symmetric_difference(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;
    llvm::Value *set1 = emitExpr(*e.args[0]);
    llvm::Value *set2 = emitExpr(*e.args[1]);
    llvm::Type *elemTy = getSetElementType(set1);
    if (elemTy) {
        llvm::Type *elemTy2 = getSetElementType(set2);
        if (!elemTy2 || elemTy2 != elemTy)
            codegenError("symmetric_difference() requires two sets with the same element type");
        auto sf1 = loadSetHeader(set1, "sd1");
        auto sf2 = loadSetHeader(set2, "sd2");

        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(setHeaderTy_);
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        auto mallocFn = getStdlibMalloc();

        llvm::Value *maxLen = builder_.CreateAdd(sf1.len, sf2.len, "sd_max_len");
        llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "sd_hdr");
        llvm::Value *dataSize = builder_.CreateMul(maxLen, llvm::ConstantInt::get(i64Ty_, elemSize), "sd_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "sd_data");

        llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, newHeader, 0, "sd_len_ptr");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), lenPtr);
        builder_.CreateStore(maxLen, builder_.CreateStructGEP(setHeaderTy_, newHeader, 1));
        builder_.CreateStore(newData, builder_.CreateStructGEP(setHeaderTy_, newHeader, 2));
        emitBucketInit(newHeader, setHeaderTy_, kSetLayout.bucketCountIdx, kSetLayout.bucketsPtrIdx, 16);

        // Add elements from set1 not in set2
        auto emitSetDiffLoop = [&](llvm::Value *srcData, llvm::Value *srcLen, llvm::Value *otherSet, const std::string &prefix) {
            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, prefix + "_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            llvm::BasicBlock *cBB = llvm::BasicBlock::Create(*ctx_, prefix + ".cond", fn_);
            llvm::BasicBlock *bBB = llvm::BasicBlock::Create(*ctx_, prefix + ".body", fn_);
            llvm::BasicBlock *eBB = llvm::BasicBlock::Create(*ctx_, prefix + ".end", fn_);
            builder_.CreateBr(cBB);
            builder_.SetInsertPoint(cBB);
            llvm::Value *ci = builder_.CreateLoad(i64Ty_, iVar, prefix + "_ci");
            builder_.CreateCondBr(builder_.CreateICmpSLT(ci, srcLen), bBB, eBB);
            builder_.SetInsertPoint(bBB);
            llvm::Value *ePtr = builder_.CreateGEP(elemTy, srcData, {ci}, prefix + "_ep");
            llvm::Value *eVal = builder_.CreateLoad(elemTy, ePtr, prefix + "_ev");
            llvm::Value *inOther = emitSetElementLookup(otherSet, eVal, elemTy);
            llvm::Value *notInOther = builder_.CreateICmpSLT(inOther, llvm::ConstantInt::get(i64Ty_, 0), prefix + "_nf");
            llvm::BasicBlock *aBB = llvm::BasicBlock::Create(*ctx_, prefix + ".add", fn_);
            llvm::BasicBlock *nBB = llvm::BasicBlock::Create(*ctx_, prefix + ".next", fn_);
            builder_.CreateCondBr(notInOther, aBB, nBB);
            builder_.SetInsertPoint(aBB);
            llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, prefix + "_cl");
            llvm::Value *sp = builder_.CreateGEP(elemTy, newData, {curLen}, prefix + "_sp");
            builder_.CreateStore(eVal, sp);
            emitBucketInsertAndRehashCheck(newHeader, setHeaderTy_, kSetLayout.lenIdx, kSetLayout.bucketCountIdx, kSetLayout.bucketsPtrIdx, eVal, elemTy, curLen);
            builder_.CreateStore(builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1)), lenPtr);
            builder_.CreateBr(nBB);
            builder_.SetInsertPoint(nBB);
            builder_.CreateStore(builder_.CreateAdd(ci, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            builder_.CreateBr(cBB);
            builder_.SetInsertPoint(eBB);
        };

        emitSetDiffLoop(sf1.elems, sf1.len, set2, "sd1");
        emitSetDiffLoop(sf2.elems, sf2.len, set1, "sd2");

        set_element_types_[newHeader] = elemTy;
        return newHeader;
    }
    return nullptr;
}

// is_subset(set1, set2)
llvm::Value *CodeGen::emitSetOp_is_subset(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;
    llvm::Value *set1 = emitExpr(*e.args[0]);
    llvm::Value *set2 = emitExpr(*e.args[1]);
    return emitSubsetCheck(set1, set2, "is_subset");
}

llvm::Value *CodeGen::emitSetOp_is_superset(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;
    llvm::Value *set1 = emitExpr(*e.args[0]);
    llvm::Value *set2 = emitExpr(*e.args[1]);
    // is_superset(a, b) == is_subset(b, a)
    return emitSubsetCheck(set2, set1, "is_superset");
}
