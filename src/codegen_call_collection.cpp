#include "ry/codegen.hpp"
#include "ry/codegen/lowered_collection_mutate.hpp"
#include "ry/diagnostic/diagnostic.hpp"



namespace ry {

// ===== Builtin Collection =====

llvm::Value *CodeGen::emitBuiltinCollection(const CallExpr &e,
                                              llvm::Value *preEmittedArg0) {
    if (preEmittedArg0 && e.callee == "take")
        return emitCollOp_take_impl(e, preEmittedArg0);

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
        {"removeAt",  &CodeGen::emitCollOp_remove_at},
        {"distinct",  &CodeGen::emitCollOp_distinct},
        {"flat",      &CodeGen::emitCollOp_flatten},
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
    llvm::AllocaInst *receiverAlloca = tryGetReceiverAlloca(*e.args[0]);
    llvm::Value *setPtr = emitExpr(*e.args[0]);
    llvm::Type *elemTy = getSetElementType(setPtr);
    if (elemTy) {
        setPtr = emitCowCheck(setPtr, receiverAlloca, CollectionKind::Set);
        llvm::Value *elem = emitExpr(*e.args[1]);
        if (elem->getType() != elemTy) {
            if (isAnyType(elemTy))
                elem = wrapInAny(elem);
            else if (isAnyType(elem->getType()) && canAnyHoldType(elemTy))
                elem = unwrapFromAny(elem, elemTy);
            else
                codegenError("add() element type mismatch");
        }

        std::string addElemName = getSetElemName(setPtr);
        validateSetElemType(addElemName, elem, "add()");
        llvm::Value *idx = emitSetElementLookup(setPtr, elem, elemTy, addElemName);
        llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "found");

        llvm::BasicBlock *insertBB = createBB("set.insert");
        llvm::BasicBlock *endBB = createBB("set.add_end");
        emitBranchCond(found, endBB, insertBB);

        builder_.SetInsertPoint(insertBB);
        auto sf = loadSetHeader(setPtr, "set");

        llvm::Value *needGrow = builder_.CreateICmpEQ(sf.len, sf.cap, "need_grow");
        llvm::BasicBlock *growBB = createBB("set.grow");
        llvm::BasicBlock *storeBB = createBB("set.store");
        emitBranchCond(needGrow, growBB, storeBB);

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
        emitBranchUncond(storeBB);

        builder_.SetInsertPoint(storeBB);
        llvm::Value *curLen = builder_.CreateLoad(i64Ty_, sf.lenPtr, "cur_len");
        llvm::Value *curElemsPtr = builder_.CreateLoad(ptrTy_, sf.elemsPtr, "cur_elems");
        llvm::Value *newElemPtr = builder_.CreateGEP(elemTy, curElemsPtr, {curLen}, "new_elem_ptr");
        if (elemTy == ptrTy_ && !addElemName.empty()) {
            CollectionKind addArcKind = CollectionKind::Str;
            if (fieldTypeIsArcManaged(addElemName, &addArcKind) &&
                addArcKind != CollectionKind::Str) {
                retainArcValue(elem);
            }
        }
        builder_.CreateStore(elem, newElemPtr);

        llvm::Value *newLen = builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1), "new_len");
        builder_.CreateStore(newLen, sf.lenPtr);

        // Insert into hash table buckets and check rehash
        emitBucketInsertAndRehashCheck(setPtr, setHeaderTy_, kSetLayout.lenIdx, kSetLayout.bucketCountIdx, kSetLayout.bucketsPtrIdx, elem, elemTy, curLen);

        emitBranchUncond(endBB);

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
    llvm::FunctionCallee hashFn = getRuntimeFn(hfi.hashFnName.c_str(), i64Ty_, {hfi.hashArgTy});
    llvm::Value *hashVal = builder_.CreateCall(hashFn, {hashKey}, p + "_hash");

    llvm::Value *bucketsField = builder_.CreateStructGEP(headerTy, headerPtr, layout.bucketsPtrIdx, p + "_bp");
    llvm::Value *bucketsPtr = builder_.CreateLoad(ptrTy_, bucketsField, p + "_buckets");
    llvm::Value *bcField = builder_.CreateStructGEP(headerTy, headerPtr, layout.bucketCountIdx, p + "_bc_field");
    llvm::Value *bucketCount = builder_.CreateLoad(i64Ty_, bcField, p + "_bc");
    llvm::Value *bucketMask = builder_.CreateSub(bucketCount, llvm::ConstantInt::get(i64Ty_, 1), p + "_bmask");

    llvm::FunctionCallee removeFn = getRuntimeFn("__ry_ht_remove",
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_, i64Ty_, i64Ty_});
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

    llvm::FunctionCallee updateFn = getRuntimeFn("__ry_ht_update_index",
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_, i64Ty_, i64Ty_, i64Ty_});
    builder_.CreateCall(updateFn, {bc.bucketsPtr, bc.bucketMask, hashVal, oldIndex, newIndex});
}

// ===== Per-type remove implementations =====

llvm::Value *CodeGen::emitSetRemove(llvm::Value *containerPtr, llvm::Value *elem, llvm::Type *elemTy) {
    std::string rmElemName = getSetElemName(containerPtr);
    validateSetElemType(rmElemName, elem, "remove()");
    llvm::Value *idx = emitSetElementLookup(containerPtr, elem, elemTy, rmElemName);
    llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "found");

    llvm::BasicBlock *removeBB = createBB("set.remove");
    llvm::BasicBlock *endBB = createBB("set.remove_end");
    emitBranchCond(found, removeBB, endBB);

    builder_.SetInsertPoint(removeBB);
    auto sf = loadSetHeader(containerPtr, "set");

    auto bc = emitHashTableRemoveBucket(containerPtr, setHeaderTy_, kSetLayout,
                                         elem, elemTy, idx, "rm");

    // Swap-remove: move last element to idx position
    llvm::Value *lastIdx = builder_.CreateSub(sf.len, llvm::ConstantInt::get(i64Ty_, 1), "last_idx");
    llvm::Value *isNotLast = builder_.CreateICmpNE(idx, lastIdx, "is_not_last");

    llvm::BasicBlock *swapBB = createBB("set.swap");
    llvm::BasicBlock *decBB = createBB("set.dec");
    emitBranchCond(isNotLast, swapBB, decBB);

    builder_.SetInsertPoint(swapBB);
    llvm::Value *lastPtr = builder_.CreateGEP(elemTy, sf.elems, {lastIdx}, "last_ptr");
    llvm::Value *lastVal = builder_.CreateLoad(elemTy, lastPtr, "last_val");
    llvm::Value *dstPtr = builder_.CreateGEP(elemTy, sf.elems, {idx}, "swap_dst");
    builder_.CreateStore(lastVal, dstPtr);

    emitHashTableUpdateIndex(bc, lastVal, elemTy, lastIdx, idx, "swap");
    emitBranchUncond(decBB);

    builder_.SetInsertPoint(decBB);
    llvm::Value *newLen = builder_.CreateSub(sf.len, llvm::ConstantInt::get(i64Ty_, 1), "new_len");
    builder_.CreateStore(newLen, sf.lenPtr);
    emitBranchUncond(endBB);

    builder_.SetInsertPoint(endBB);
    return llvm::ConstantInt::get(i64Ty_, 0);
}

llvm::Value *CodeGen::emitListRemove(llvm::Value *containerPtr, llvm::Value *val, llvm::Type *listElemTy) {
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t elemSize = dl.getTypeAllocSize(listElemTy);

    auto lf = loadListHeader(containerPtr, "lrem");

    // Linear search for the value
    llvm::AllocaInst *foundIdx = builder_.CreateAlloca(i64Ty_, nullptr, "lrem_found_idx");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(-1)), foundIdx);
    llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "lrem_i");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

    llvm::BasicBlock *condBB = createBB("lrem.cond");
    llvm::BasicBlock *bodyBB = createBB("lrem.body");
    llvm::BasicBlock *endSearchBB = createBB("lrem.end_search");

    emitBranchUncond(condBB);
    builder_.SetInsertPoint(condBB);
    llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "lrem_iv");
    llvm::Value *notYetFound = builder_.CreateICmpSLT(
        builder_.CreateLoad(i64Ty_, foundIdx, "lrem_fi"), llvm::ConstantInt::get(i64Ty_, 0), "lrem_not_found");
    llvm::Value *inBounds = builder_.CreateICmpSLT(iVal, lf.len, "lrem_in_bounds");
    llvm::Value *cont = builder_.CreateAnd(notYetFound, inBounds, "lrem_cont");
    emitBranchCond(cont, bodyBB, endSearchBB);

    builder_.SetInsertPoint(bodyBB);
    llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "lrem_ic");
    llvm::Value *elemPtr = builder_.CreateGEP(listElemTy, lf.data, {iCur}, "lrem_elem_ptr");
    llvm::Value *listElem = builder_.CreateLoad(listElemTy, elemPtr, "lrem_elem");

    llvm::Value *match;
    if (listElemTy == ptrTy_) {
        // Reject non-str pointer elements: the comparison path below calls strcmp, which is
        // UB on Map/Set/List/closure/resource headers. Positive allowlist on list_elem_type_name
        // (empty or "str" counts as str) with structural fallbacks for NestedListElem /
        // list_elem_fn_type_info in case the name is unset. Mirrors #1262 distinct() guard.
        const ValueMetadata *meta = getMeta(containerPtr);
        const std::string &elemName = meta ? meta->list_elem_type_name : std::string{};
        const bool isNonStrName = !elemName.empty() && elemName != "str";
        const bool hasNestedList = meta && meta->nested_list_elem != nullptr;
        const bool hasFnInfo = meta && meta->list_elem_fn_type_info.has_value();
        if (isNonStrName || hasNestedList || hasFnInfo)
            codegenError("remove() is only supported for lists of primitive values or strings");
        auto strcmpFn = getStdlibStrcmp();
        llvm::Value *cmpResult = builder_.CreateCall(strcmpFn, {val, listElem}, "lrem_strcmp");
        match = builder_.CreateICmpEQ(cmpResult, llvm::ConstantInt::get(i32Ty_, 0), "lrem_match");
    } else if (listElemTy->isDoubleTy()) {
        match = builder_.CreateFCmpOEQ(val, listElem, "lrem_match");
    } else {
        match = builder_.CreateICmpEQ(val, listElem, "lrem_match");
    }

    llvm::BasicBlock *foundBB = createBB("lrem.found");
    llvm::BasicBlock *nextBB = createBB("lrem.next");
    emitBranchCond(match, foundBB, nextBB);

    builder_.SetInsertPoint(foundBB);
    builder_.CreateStore(iCur, foundIdx);
    emitBranchUncond(condBB);

    builder_.SetInsertPoint(nextBB);
    llvm::Value *iNext = builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1), "lrem_inext");
    builder_.CreateStore(iNext, iVar);
    emitBranchUncond(condBB);

    // After search: if found, memmove to close the gap
    builder_.SetInsertPoint(endSearchBB);
    llvm::Value *idx = builder_.CreateLoad(i64Ty_, foundIdx, "lrem_idx");
    llvm::Value *wasFound = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "lrem_was_found");

    llvm::BasicBlock *removeBB = createBB("lrem.remove");
    llvm::BasicBlock *doneBB = createBB("lrem.done");
    emitBranchCond(wasFound, removeBB, doneBB);

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
    emitBranchUncond(doneBB);

    builder_.SetInsertPoint(doneBB);
    return llvm::ConstantInt::get(i64Ty_, 0);
}

llvm::Value *CodeGen::emitMapRemove(llvm::Value *containerPtr, llvm::Value *key, llvm::Type *keyTy, llvm::Type *valTy) {
    llvm::Value *idx = emitMapKeyLookup(containerPtr, key, keyTy);
    llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "mrem_found");

    llvm::BasicBlock *removeBB = createBB("mrem.do");
    llvm::BasicBlock *endBB = createBB("mrem.end");
    emitBranchCond(found, removeBB, endBB);

    builder_.SetInsertPoint(removeBB);
    auto mf = loadMapHeader(containerPtr, "mrem");

    auto bc = emitHashTableRemoveBucket(containerPtr, mapHeaderTy_, kMapLayout,
                                         key, keyTy, idx, "mrem");

    // Swap-remove from keys and values arrays
    llvm::Value *lastIdx = builder_.CreateSub(mf.len, llvm::ConstantInt::get(i64Ty_, 1), "mrem_last_idx");
    llvm::Value *isNotLast = builder_.CreateICmpNE(idx, lastIdx, "mrem_not_last");

    llvm::BasicBlock *swapBB = createBB("mrem.swap");
    llvm::BasicBlock *decBB = createBB("mrem.dec");
    emitBranchCond(isNotLast, swapBB, decBB);

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
    emitBranchUncond(decBB);

    builder_.SetInsertPoint(decBB);
    llvm::Value *newLen = builder_.CreateSub(mf.len, llvm::ConstantInt::get(i64Ty_, 1), "mrem_new_len");
    builder_.CreateStore(newLen, mf.lenPtr);
    emitBranchUncond(endBB);

    builder_.SetInsertPoint(endBB);
    return llvm::ConstantInt::get(i64Ty_, 0);
}

// ===== Dispatcher =====

llvm::Value *CodeGen::emitCollOp_remove(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;
    llvm::AllocaInst *receiverAlloca = tryGetReceiverAlloca(*e.args[0]);
    llvm::Value *containerPtr = emitExpr(*e.args[0]);

    if (llvm::Type *elemTy = getSetElementType(containerPtr)) {
        containerPtr = emitCowCheck(containerPtr, receiverAlloca, CollectionKind::Set);
        llvm::Value *elem = emitExpr(*e.args[1]);
        if (elem->getType() != elemTy) {
            if (isAnyType(elemTy))
                elem = wrapInAny(elem);
            else if (isAnyType(elem->getType()) && canAnyHoldType(elemTy))
                elem = unwrapFromAny(elem, elemTy);
            else
                codegenError("remove() element type mismatch");
        }
        return emitSetRemove(containerPtr, elem, elemTy);
    }
    if (llvm::Type *listElemTy = getListElementType(containerPtr)) {
        containerPtr = emitCowCheck(containerPtr, receiverAlloca, CollectionKind::List);
        llvm::Value *val = emitExpr(*e.args[1]);
        if (val->getType() != listElemTy)
            codegenError("remove() value type mismatch with list element type");
        return emitListRemove(containerPtr, val, listElemTy);
    }
    llvm::Type *keyTy = getMapKeyType(containerPtr);
    llvm::Type *valTy = getMapValueType(containerPtr);
    if (keyTy && valTy) {
        containerPtr = emitCowCheck(containerPtr, receiverAlloca, CollectionKind::Map);
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
    llvm::AllocaInst *receiverAlloca = tryGetReceiverAlloca(*e.args[0]);
    llvm::Value *listPtr = emitExpr(*e.args[0]);
    llvm::Type *elemTy = getListElementType(listPtr);
    if (elemTy) {
        listPtr = emitCowCheck(listPtr, receiverAlloca, CollectionKind::List);
        llvm::Value *val = emitExpr(*e.args[1]);
        if (val->getType() != elemTy) {
            if (isAnyType(elemTy))
                val = wrapInAny(val);
            else if (isAnyType(val->getType()) && canAnyHoldType(elemTy))
                val = unwrapFromAny(val, elemTy);
            else
                codegenError("append() element type mismatch");
        }

        // ARC retain decision uses ValueMetadata which does not cross the ABI,
        // so it must happen on the codegen side BEFORE delegating to emission.
        // Reordering relative to the original (which retained inside storeBB)
        // is semantically equivalent because `val` is not mutated by grow.
        if (elemTy == ptrTy_) {
            auto *listMeta = getMeta(listPtr);
            const std::string &appElemName =
                listMeta ? listMeta->list_elem_type_name : std::string{};
            CollectionKind appArcKind = CollectionKind::Str;
            if (!appElemName.empty() &&
                fieldTypeIsArcManaged(appElemName, &appArcKind) &&
                appArcKind != CollectionKind::Str) {
                retainArcValue(val);
            }
        }

        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        auto op = codegen::lowering::lowerCollectionAppend(
            *this, listPtr, val, listHeaderTy_, elemTy, elemSize);
        codegen::emission::emitCollectionAppend(*this, op);

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
        if (val->getType() != elemTy) {
            if (isAnyType(elemTy))
                val = wrapInAny(val);
            else if (isAnyType(val->getType()) && canAnyHoldType(elemTy))
                val = unwrapFromAny(val, elemTy);
            else
                codegenError("appended() element type mismatch");
        }

        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        auto mallocFn = getStdlibMalloc();
        auto memcpyFn = getStdlibMemcpy();

        auto lf = loadListHeader(listPtr, "apd");
        llvm::Value *newLen = builder_.CreateAdd(lf.len, llvm::ConstantInt::get(i64Ty_, 1), "apd_new_len");

        llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);
        llvm::Value *newDataSize = builder_.CreateMul(newLen, llvm::ConstantInt::get(i64Ty_, elemSize), "apd_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {newDataSize}, "apd_nd");

        llvm::Value *oldDataSize = builder_.CreateMul(lf.len, llvm::ConstantInt::get(i64Ty_, elemSize), "apd_ods");
        builder_.CreateCall(memcpyFn, {newData, lf.data, oldDataSize});

        // The new list co-owns the memcpy'd range AND the appended value
        // with the source.  After #1242 the destructor recursively releases
        // inner ARC elements, so missing either retain would cause a UAF on
        // rebind.  str excluded per #1266.  #1667: extends the same retain
        // discipline to tuple-elem lists (List<(K, V)>) — propagateMeta
        // inherits the tuple-aware destructor, so each tuple slot's ARC
        // components need symmetric retain.
        const ValueMetadata *apdSrcMeta = getMeta(listPtr);
        // Resolve type aliases so `type Pair = (int, List<int>)` is recognized
        // as a tuple here (#1667 follow-up). The destructor side already runs
        // through resolveCollectionDestructor, so without this resolution
        // alias-backed tuple lists would skip the retain path while the
        // destructor still releases tuple fields — reintroducing the asymmetry.
        const std::string apdElemSigSnap =
            apdSrcMeta ? resolveTypeAlias(apdSrcMeta->list_elem_type_name)
                        : std::string{};
        const bool apdElemIsTuple =
            apdElemSigSnap.size() >= 2 &&
            apdElemSigSnap.front() == '(' && apdElemSigSnap.back() == ')';
        CollectionKind apdElemArcKind = CollectionKind::List;
        const bool apdElemIsArc =
            !apdElemIsTuple &&
            elementTypeIsArcManaged(listPtr, CollectionKind::List, &apdElemArcKind) &&
            apdElemArcKind != CollectionKind::Str;
        if (apdElemIsTuple) {
            if (auto *tupleTy = llvm::dyn_cast<llvm::StructType>(elemTy)) {
                emitTupleElemRetainLoop(newData, lf.len, "apd_telem",
                                         apdElemSigSnap, tupleTy);
            }
        } else if (apdElemIsArc) {
            emitCowRetainArcElements(newData, lf.len, "apd_elem", apdElemArcKind);
        }

        llvm::Value *newElemPtr = builder_.CreateGEP(elemTy, newData, lf.len, "apd_new_ep");
        if (apdElemIsTuple) {
            // Per-component retain on the tuple value being appended.
            std::vector<std::string> comps = splitTupleSig(apdElemSigSnap);
            auto *tupleTy = llvm::dyn_cast<llvm::StructType>(elemTy);
            if (tupleTy && tupleTy->getNumElements() == comps.size()) {
                for (unsigned i = 0; i < comps.size(); ++i) {
                    if (tupleTy->getElementType(i) != ptrTy_) continue;
                    llvm::Value *comp =
                        builder_.CreateExtractValue(val, {i},
                            "apd_tcomp_" + std::to_string(i));
                    emitTupleComponentRetain(comp, comps[i]);
                }
            }
        } else if (apdElemIsArc) {
            retainArcValue(val);
        }
        builder_.CreateStore(val, newElemPtr);

        storeListHeaderFields(newHeader, newLen, newLen, newData);

        setTypeMeta(TypeMeta::ListElem, newHeader, elemTy);
        propagateMeta(listPtr, newHeader);
        return newHeader;
    }
    return nullptr;
}

llvm::Value *CodeGen::emitCollOp_pop(const CallExpr &e) {
    if (e.args.size() != 1) return nullptr;
    // pop(list) -> Option<T>: remove and return last element
    llvm::AllocaInst *receiverAlloca = tryGetReceiverAlloca(*e.args[0]);
    llvm::Value *listPtr = emitExpr(*e.args[0]);
    llvm::Type *elemTy = getListElementType(listPtr);
    if (elemTy) {
        listPtr = emitCowCheck(listPtr, receiverAlloca, CollectionKind::List);
        llvm::StructType *optTy = getOptionType(elemTy);
        auto lf = loadListHeader(listPtr, "pop");

        llvm::Value *isEmpty = builder_.CreateICmpEQ(lf.len, llvm::ConstantInt::get(i64Ty_, 0), "pop_empty");
        llvm::BasicBlock *emptyBB = createBB("pop.empty");
        llvm::BasicBlock *okBB = createBB("pop.ok");
        llvm::BasicBlock *mergeBB = createBB("pop.merge");
        emitBranchCond(isEmpty, emptyBB, okBB);

        builder_.SetInsertPoint(emptyBB);
        llvm::Value *noneVal = buildNoneValue(optTy);
        emitBranchUncond(mergeBB);
        llvm::BasicBlock *emptyEndBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(okBB);
        llvm::Value *lastIdx = builder_.CreateSub(lf.len, llvm::ConstantInt::get(i64Ty_, 1), "pop_last_idx");
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, lf.data, lastIdx, "pop_elem_ptr");
        llvm::Value *val = builder_.CreateLoad(elemTy, elemPtr, "pop_val");
        builder_.CreateStore(lastIdx, lf.lenPtr);
        llvm::Value *someVal = buildSomeValue(val, optTy);
        emitBranchUncond(mergeBB);
        llvm::BasicBlock *okEndBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = createPhi(optTy, {}, "pop_result");
        phi->addIncoming(noneVal, emptyEndBB);
        phi->addIncoming(someVal, okEndBB);
        return phi;
    }
    return nullptr;
}

llvm::Value *CodeGen::emitCollOp_slice(const CallExpr &e) {
    if (e.args.size() != 3) return nullptr;
    llvm::Value *listPtr = emitExpr(*e.args[0]);
    llvm::Type *elemTy = getListElementType(listPtr);
    if (!elemTy) return nullptr;
    llvm::Value *startVal = emitExpr(*e.args[1]);
    llvm::Value *endVal   = emitExpr(*e.args[2]);
    llvm::Value *length = loadListHeader(listPtr, "sc").len;
    llvm::Value *startWrapped = emitNegativeIndexWrap(startVal, length, "sl_start");
    llvm::Value *endWrapped   = emitNegativeIndexWrap(endVal,   length, "sl_end");
    return emitListSlice(listPtr, startWrapped, endWrapped, elemTy);
}

llvm::Value *CodeGen::emitListSlice(llvm::Value *listPtr,
                                     llvm::Value *startVal,
                                     llvm::Value *endExclVal,
                                     llvm::Type *elemTy) {
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t elemSize = dl.getTypeAllocSize(elemTy);

    // Delegate the clamp + malloc + memcpy chain to the llvm_emit ABI. The
    // ABI returns (count, new_data); header allocation, metadata propagation
    // and per-element ARC retain stay on the codegen side because they need
    // ValueMetadata that does not cross the ABI boundary.
    auto op = codegen::lowering::lowerListSlice(*this, listPtr, startVal,
                                                endExclVal, listHeaderTy_,
                                                elemTy, elemSize);
    auto result = codegen::emission::emitListSlice(*this, op);
    llvm::Value *count = result.count;
    llvm::Value *newData = result.new_data;

    llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);

    // Reference-typed elements share ownership with the source list.  memcpy
    // duplicates raw pointers without bumping refcounts; without retention,
    // releasing the source (or a dropped alias) frees the elements that the
    // new slice still points at (#1204).
    {
        const ValueMetadata *srcMeta = getMeta(listPtr);
        // resolveTypeAlias so alias-backed tuple types take the tuple path
        // (#1667 follow-up — destructor resolves aliases via
        // resolveCollectionDestructor, retain side must mirror).
        const std::string elemSigSnap =
            srcMeta ? resolveTypeAlias(srcMeta->list_elem_type_name)
                     : std::string{};
        if (elemSigSnap.size() >= 2 && elemSigSnap.front() == '(' &&
            elemSigSnap.back() == ')') {
            // #1667: tuple-elem List<(K, V)> destructor releases inner ARC
            // tuple components, so the memcpy'd buffer must retain each
            // component (mirrors propagateMeta-induced destructor inheritance).
            if (auto *tupleTy = llvm::dyn_cast<llvm::StructType>(elemTy)) {
                emitTupleElemRetainLoop(newData, count, "sl_telem",
                                         elemSigSnap, tupleTy);
            }
        } else {
            CollectionKind elemArcKind = CollectionKind::List;
            if (elementTypeIsArcManaged(listPtr, CollectionKind::List,
                                         &elemArcKind)) {
                emitCowRetainArcElements(newData, count, "sl_elem",
                                          elemArcKind);
            }
        }
    }

    storeListHeaderFields(newHeader, count, count, newData);
    setTypeMeta(TypeMeta::ListElem, newHeader, elemTy);
    propagateMeta(listPtr, newHeader);
    return newHeader;
}

llvm::Value *CodeGen::emitCollOp_take(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;
    return emitCollOp_take_impl(e, emitExpr(*e.args[0]));
}

llvm::Value *CodeGen::emitCollOp_take_impl(const CallExpr &e,
                                            llvm::Value *listPtr) {
    llvm::Type *elemTy = getListElementType(listPtr);
    if (elemTy) {
        llvm::Value *nVal = emitExpr(*e.args[1]);

        const llvm::DataLayout &dl = mod_->getDataLayout();
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
        llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);
        llvm::Value *dataSize = builder_.CreateMul(clampedN, llvm::ConstantInt::get(i64Ty_, elemSize), "tk_dsize");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "tk_data");

        // Copy elements
        builder_.CreateCall(memcpyFn, {newData, lf.data, dataSize});

        // Reference-typed elements share ownership with the source list.
        // memcpy duplicates raw pointers without bumping refcounts; without
        // retention, releasing the source (or a dropped alias) frees the
        // elements that the new prefix still points at (#1235, same defect
        // class as #1204 for emitListSlice).
        {
            const ValueMetadata *srcMeta = getMeta(listPtr);
            // resolveTypeAlias so alias-backed tuple types take the tuple path
            // (#1667 follow-up — destructor resolves aliases, retain mirrors).
            const std::string elemSigSnap =
                srcMeta ? resolveTypeAlias(srcMeta->list_elem_type_name)
                         : std::string{};
            if (elemSigSnap.size() >= 2 && elemSigSnap.front() == '(' &&
                elemSigSnap.back() == ')') {
                // #1667: tuple-elem propagation extends the destructor to
                // inner tuple components; mirror retain on the memcpy'd buf.
                if (auto *tupleTy = llvm::dyn_cast<llvm::StructType>(elemTy)) {
                    emitTupleElemRetainLoop(newData, clampedN, "tk_telem",
                                             elemSigSnap, tupleTy);
                }
            } else {
                CollectionKind elemArcKind = CollectionKind::List;
                if (elementTypeIsArcManaged(listPtr, CollectionKind::List,
                                             &elemArcKind)) {
                    emitCowRetainArcElements(newData, clampedN, "tk_elem",
                                              elemArcKind);
                }
            }
        }

        // Set header fields
        storeListHeaderFields(newHeader, clampedN, clampedN, newData);

        setTypeMeta(TypeMeta::ListElem, newHeader, elemTy);
        propagateMeta(listPtr, newHeader);
        return newHeader;
    }
    return nullptr;
}

llvm::Value *CodeGen::emitCollOp_insert(const CallExpr &e) {
    if (e.args.size() != 3) return nullptr;
    // insert(list, index, value)
    llvm::AllocaInst *receiverAlloca = tryGetReceiverAlloca(*e.args[0]);
    llvm::Value *listPtr = emitExpr(*e.args[0]);
    llvm::Type *elemTy = getListElementType(listPtr);
    if (elemTy) {
        listPtr = emitCowCheck(listPtr, receiverAlloca, CollectionKind::List);
        llvm::Value *idx = emitExpr(*e.args[1]);
        if (idx->getType() != i64Ty_)
            codegenError("insert() index must be int");
        llvm::Value *val = emitExpr(*e.args[2]);
        if (val->getType() != elemTy) {
            if (isAnyType(elemTy))
                val = wrapInAny(val);
            else if (isAnyType(val->getType()) && canAnyHoldType(elemTy))
                val = unwrapFromAny(val, elemTy);
            else
                codegenError("insert() element type mismatch");
        }

        // ARC retain decision uses ValueMetadata which does not cross the ABI,
        // so it must happen on the codegen side BEFORE delegating to emission.
        // The retain is safe to hoist out of the previous post-bounds-check
        // position because `val` is independent of len/cap/idx and a bounds
        // failure aborts the program without observable refcount differences.
        if (elemTy == ptrTy_) {
            auto *insMeta = getMeta(listPtr);
            const std::string &insElemName =
                insMeta ? insMeta->list_elem_type_name : std::string{};
            CollectionKind insElemKind = CollectionKind::Str;
            if (!insElemName.empty() &&
                fieldTypeIsArcManaged(insElemName, &insElemKind) &&
                insElemKind != CollectionKind::Str) {
                retainArcValue(val);
            }
        }

        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        auto op = codegen::lowering::lowerCollectionInsert(
            *this, listPtr, idx, val, listHeaderTy_, elemTy, elemSize);
        codegen::emission::emitCollectionInsert(*this, op);

        return llvm::ConstantInt::get(i64Ty_, 0);
    }
    return nullptr;
}

llvm::Value *CodeGen::emitCollOp_remove_at(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;
    // removeAt(list, index)
    llvm::AllocaInst *receiverAlloca = tryGetReceiverAlloca(*e.args[0]);
    llvm::Value *listPtr = emitExpr(*e.args[0]);
    llvm::Type *elemTy = getListElementType(listPtr);
    if (elemTy) {
        listPtr = emitCowCheck(listPtr, receiverAlloca, CollectionKind::List);
        llvm::Value *idx = emitExpr(*e.args[1]);
        if (idx->getType() != i64Ty_)
            codegenError("removeAt() index must be int");

        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        auto op = codegen::lowering::lowerCollectionRemoveAt(
            *this, listPtr, idx, listHeaderTy_, elemTy, elemSize);
        return codegen::emission::emitCollectionRemoveAt(*this, op);
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

    // Reject non-str pointer elements: the dedup inner loop calls strcmp, which is
    // UB on Map/Set/List/closure headers. Positive allowlist on list_elem_type_name
    // (empty or "str" counts as str) with structural fallbacks for NestedListElem /
    // list_elem_fn_type_info in case the name is unset.
    if (elemTy == ptrTy_) {
        const ValueMetadata *meta = getMeta(listVal);
        const std::string &elemName = meta ? meta->list_elem_type_name : std::string{};
        const bool isNonStrName = !elemName.empty() && elemName != "str";
        const bool hasNestedList = meta && meta->nested_list_elem != nullptr;
        const bool hasFnInfo = meta && meta->list_elem_fn_type_info.has_value();
        if (isNonStrName || hasNestedList || hasFnInfo)
            codegenError("distinct() is only supported for lists of primitive values or strings");
    }

    auto lf = loadListHeader(listVal, "dist_src");

    // Allocate new list (capacity = source length)
    auto mallocFn = getStdlibMalloc();
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t elemSize = dl.getTypeAllocSize(elemTy);

    llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);
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

    llvm::BasicBlock *outerCondBB = createBB("dist.ocond");
    llvm::BasicBlock *outerBodyBB = createBB("dist.obody");
    llvm::BasicBlock *outerEndBB = createBB("dist.oend");

    emitBranchUncond(outerCondBB);
    builder_.SetInsertPoint(outerCondBB);
    llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "dist_iv");
    emitBranchCond(builder_.CreateICmpSLT(iVal, lf.len), outerBodyBB, outerEndBB);

    builder_.SetInsertPoint(outerBodyBB);
    llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "dist_ic");
    llvm::Value *srcElemPtr = builder_.CreateGEP(elemTy, lf.data, {iCur}, "dist_src_ep");
    llvm::Value *srcElem = builder_.CreateLoad(elemTy, srcElemPtr, "dist_src_elem");

    // Inner loop: check if srcElem already exists in output
    llvm::AllocaInst *dupFound = builder_.CreateAlloca(i1Ty_, nullptr, "dist_dup");
    builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 0), dupFound);
    llvm::AllocaInst *jVar = builder_.CreateAlloca(i64Ty_, nullptr, "dist_j");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), jVar);

    llvm::BasicBlock *innerCondBB = createBB("dist.icond");
    llvm::BasicBlock *innerBodyBB = createBB("dist.ibody");
    llvm::BasicBlock *innerEndBB = createBB("dist.iend");

    llvm::Value *curOutLen = builder_.CreateLoad(i64Ty_, outLen, "dist_cur_out");
    emitBranchUncond(innerCondBB);

    builder_.SetInsertPoint(innerCondBB);
    llvm::Value *jVal = builder_.CreateLoad(i64Ty_, jVar, "dist_jv");
    llvm::Value *notDup = builder_.CreateICmpEQ(builder_.CreateLoad(i1Ty_, dupFound), llvm::ConstantInt::get(i1Ty_, 0), "dist_not_dup");
    llvm::Value *jInBounds = builder_.CreateICmpSLT(jVal, curOutLen, "dist_j_inb");
    llvm::Value *innerCont = builder_.CreateAnd(notDup, jInBounds, "dist_icont");
    emitBranchCond(innerCont, innerBodyBB, innerEndBB);

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

    llvm::BasicBlock *dupBB = createBB("dist.dup");
    llvm::BasicBlock *innerNextBB = createBB("dist.inext");
    emitBranchCond(match, dupBB, innerNextBB);

    builder_.SetInsertPoint(dupBB);
    builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 1), dupFound);
    emitBranchUncond(innerCondBB);

    builder_.SetInsertPoint(innerNextBB);
    builder_.CreateStore(builder_.CreateAdd(jCur, llvm::ConstantInt::get(i64Ty_, 1)), jVar);
    emitBranchUncond(innerCondBB);

    // After inner loop: if not duplicate, add to output
    builder_.SetInsertPoint(innerEndBB);
    llvm::Value *isDup = builder_.CreateLoad(i1Ty_, dupFound, "dist_is_dup");

    llvm::BasicBlock *addBB = createBB("dist.add");
    llvm::BasicBlock *outerNextBB = createBB("dist.onext");
    emitBranchCond(isDup, outerNextBB, addBB);

    builder_.SetInsertPoint(addBB);
    llvm::Value *outIdx = builder_.CreateLoad(i64Ty_, outLen, "dist_out_idx");
    llvm::Value *dstPtr = builder_.CreateGEP(elemTy, newData, {outIdx}, "dist_dst");
    builder_.CreateStore(srcElem, dstPtr);
    builder_.CreateStore(builder_.CreateAdd(outIdx, llvm::ConstantInt::get(i64Ty_, 1)), outLen);
    emitBranchUncond(outerNextBB);

    builder_.SetInsertPoint(outerNextBB);
    builder_.CreateStore(builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
    emitBranchUncond(outerCondBB);

    // End: set final length
    builder_.SetInsertPoint(outerEndBB);
    llvm::Value *finalLen = builder_.CreateLoad(i64Ty_, outLen, "dist_final_len");
    builder_.CreateStore(finalLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 0, "dist_len_ptr"));

    setTypeMeta(TypeMeta::ListElem, newHeader, elemTy);
    propagateMeta(listVal, newHeader);
    return newHeader;
}

llvm::Value *CodeGen::emitCollOp_flatten(const CallExpr &e) {
    if (e.args.size() != 1) return nullptr;
    // flat(list) -> flatten nested list one level
    llvm::Value *listVal = emitExpr(*e.args[0]);
    llvm::Type *outerElemTy = getListElementType(listVal);
    if (!outerElemTy || outerElemTy != ptrTy_)
        codegenError("flat() requires a list of lists");

    // Look up the inner element type
    llvm::Type *innerElemTy = getNestedListElementType(listVal);
    if (!innerElemTy)
        codegenError("flat() cannot determine inner list element type; use a list literal (e.g. [[1, 2], [3, 4]])");

    const llvm::DataLayout &dl = mod_->getDataLayout();
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
        llvm::BasicBlock *condBB = createBB("flat.s.cond");
        llvm::BasicBlock *bodyBB = createBB("flat.s.body");
        llvm::BasicBlock *endBB = createBB("flat.s.end");
        emitBranchUncond(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "flat_si");
        emitBranchCond(builder_.CreateICmpSLT(i, outerLen), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *innerPtr = builder_.CreateGEP(ptrTy_, outerData, {i}, "flat_inner_ptr");
        llvm::Value *innerList = builder_.CreateLoad(ptrTy_, innerPtr, "flat_inner");
        llvm::Value *innerLenPtr = builder_.CreateStructGEP(listHeaderTy_, innerList, 0, "flat_ilen_ptr");
        llvm::Value *innerLen = builder_.CreateLoad(i64Ty_, innerLenPtr, "flat_ilen");
        llvm::Value *curTotal = builder_.CreateLoad(i64Ty_, totalLen, "flat_cur_total");
        builder_.CreateStore(builder_.CreateAdd(curTotal, innerLen), totalLen);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        emitBranchUncond(condBB);
        builder_.SetInsertPoint(endBB);
    }

    // Allocate new list
    llvm::Value *total = builder_.CreateLoad(i64Ty_, totalLen, "flat_total_len");
    llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);
    llvm::Value *dataSize = builder_.CreateMul(total, llvm::ConstantInt::get(i64Ty_, innerElemSize), "flat_ds");
    llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "flat_data");

    // Set header
    storeListHeaderFields(newHeader, total, total, newData);

    // Pass 2: copy each inner list's data
    llvm::AllocaInst *offset = builder_.CreateAlloca(i64Ty_, nullptr, "flat_off");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), offset);
    {
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "flat_c_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
        llvm::BasicBlock *condBB = createBB("flat.c.cond");
        llvm::BasicBlock *bodyBB = createBB("flat.c.body");
        llvm::BasicBlock *endBB = createBB("flat.c.end");
        emitBranchUncond(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "flat_ci");
        emitBranchCond(builder_.CreateICmpSLT(i, outerLen), bodyBB, endBB);
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
        emitBranchUncond(condBB);
        builder_.SetInsertPoint(endBB);
    }

    setTypeMeta(TypeMeta::ListElem, newHeader, innerElemTy);
    return newHeader;
}

llvm::Value *CodeGen::emitCollOp_items(const CallExpr &e) {
    if (e.args.size() != 1) return nullptr;
    // items(map) -> List<(K, V)>
    llvm::Value *mapPtr = emitExpr(*e.args[0]);
    llvm::Type *keyTy = getMapKeyType(mapPtr);
    llvm::Type *valTy = getMapValueType(mapPtr);
    if (keyTy && valTy) {
        // Snapshot source-map element names before any getOrCreateMeta call
        // (which may rehash value_metadata_ and invalidate the raw pointer
        // returned by getMeta — see #858 / keys()/values() precedent).
        std::string keyName, valName;
        if (auto *srcMeta = getMeta(mapPtr)) {
            keyName = srcMeta->map_key_type_name;
            valName = srcMeta->map_value_type_name;
        }

        auto mf = loadMapHeader(mapPtr, "items");

        llvm::StructType *tupleTy = llvm::StructType::get(*ctx_, {keyTy, valTy});
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t tupleSize = dl.getTypeAllocSize(tupleTy);

        auto mallocFn = getStdlibMalloc();

        llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);
        llvm::Value *dataSize = builder_.CreateMul(mf.len, llvm::ConstantInt::get(i64Ty_, tupleSize), "items_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "items_data");

        // Fill tuples
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "items_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
        llvm::BasicBlock *condBB = createBB("items.cond");
        llvm::BasicBlock *bodyBB = createBB("items.body");
        llvm::BasicBlock *endBB = createBB("items.end");
        emitBranchUncond(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "items_ci");
        emitBranchCond(builder_.CreateICmpSLT(i, mf.len), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *kp = builder_.CreateGEP(keyTy, mf.keys, {i}, "items_kp");
        llvm::Value *vp = builder_.CreateGEP(valTy, mf.vals, {i}, "items_vp");
        llvm::Value *k = builder_.CreateLoad(keyTy, kp, "items_k");
        llvm::Value *v = builder_.CreateLoad(valTy, vp, "items_v");
        // #1667: tuple-elem List<(K, V)> destructor releases each component
        // per slot; retain on store keeps refcount symmetric (#1242 pattern,
        // tuple-sig path). The retain helper recurses into nested tuple K/V
        // (e.g. Map<str, (List<int>, int)>) so inline tuple-struct values
        // are not skipped.
        if (!keyName.empty())
            emitTupleComponentRetain(k, keyName);
        if (!valName.empty())
            emitTupleComponentRetain(v, valName);
        llvm::Value *tuple = llvm::UndefValue::get(tupleTy);
        tuple = builder_.CreateInsertValue(tuple, k, 0);
        tuple = builder_.CreateInsertValue(tuple, v, 1);
        llvm::Value *dp = builder_.CreateGEP(tupleTy, newData, {i}, "items_dp");
        builder_.CreateStore(tuple, dp);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        emitBranchUncond(condBB);
        builder_.SetInsertPoint(endBB);

        storeListHeaderFields(newHeader, mf.len, mf.len, newData);
        setTypeMeta(TypeMeta::ListElem, newHeader, tupleTy);
        // Stamp the tuple type name so downstream tuple-field access can
        // dispatch nested index/key lookup (#1659). Mirrors enumerate / zip
        // (codegen_call.cpp). The List<(K, V)> destructor now recurses into
        // tuple fields and releases each ARC component (#1667 — extension of
        // #1242 to the tuple-sig path), so the per-component
        // emitTupleComponentRetain calls above are required to keep the
        // retain/release symmetry.
        if (!keyName.empty() && !valName.empty())
            getOrCreateMeta(newHeader).list_elem_type_name =
                "(" + keyName + ", " + valName + ")";
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

            llvm::BasicBlock *foundBB = createBB("get2.found");
            llvm::BasicBlock *notFoundBB = createBB("get2.notfound");
            llvm::BasicBlock *mergeBB = createBB("get2.merge");
            emitBranchCond(found, foundBB, notFoundBB);

            builder_.SetInsertPoint(foundBB);
            llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, mapPtr, 3, "get2_vals_field");
            llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "get2_vals");
            llvm::Value *valPtr = builder_.CreateGEP(valTy, valsPtr, {idx}, "get2_val_ptr");
            llvm::Value *foundVal = builder_.CreateLoad(valTy, valPtr, "get2_val");
            llvm::Value *someVal = buildSomeValue(foundVal, optTy);
            emitBranchUncond(mergeBB);
            llvm::BasicBlock *foundEndBB = builder_.GetInsertBlock();

            builder_.SetInsertPoint(notFoundBB);
            llvm::Value *noneVal = buildNoneValue(optTy);
            emitBranchUncond(mergeBB);
            llvm::BasicBlock *notFoundEndBB = builder_.GetInsertBlock();

            builder_.SetInsertPoint(mergeBB);
            llvm::PHINode *phi = createPhi(optTy, {}, "get2_result");
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

            llvm::BasicBlock *foundBB = createBB("get.found");
            llvm::BasicBlock *notFoundBB = createBB("get.notfound");
            llvm::BasicBlock *mergeBB = createBB("get.merge");
            emitBranchCond(found, foundBB, notFoundBB);

            builder_.SetInsertPoint(foundBB);
            llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, mapPtr, 3, "get_vals_field");
            llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "get_vals");
            llvm::Value *valPtr = builder_.CreateGEP(valTy, valsPtr, {idx}, "get_val_ptr");
            llvm::Value *foundVal = builder_.CreateLoad(valTy, valPtr, "get_val");
            llvm::BasicBlock *foundEndBB = builder_.GetInsertBlock();
            emitBranchUncond(mergeBB);

            builder_.SetInsertPoint(notFoundBB);
            llvm::BasicBlock *notFoundEndBB = builder_.GetInsertBlock();
            emitBranchUncond(mergeBB);

            builder_.SetInsertPoint(mergeBB);
            llvm::PHINode *phi = createPhi(valTy, {}, "get_result");
            phi->addIncoming(foundVal, foundEndBB);
            phi->addIncoming(defaultVal, notFoundEndBB);
            return phi;
        }
    }

    return nullptr;
}

llvm::Value *CodeGen::emitMapMergeCore(llvm::Value *map1, llvm::Value *map2,
                                        llvm::Type *keyTy, llvm::Type *valTy) {
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t keySize = dl.getTypeAllocSize(keyTy);
    uint64_t valSize = dl.getTypeAllocSize(valTy);

    auto mallocFn = getStdlibMalloc();
    auto memcpyFn = getStdlibMemcpy();

    const ValueMetadata *map1Meta = getMeta(map1);
    const std::string keyName = map1Meta ? map1Meta->map_key_type_name : std::string{};
    const std::string valName = map1Meta ? map1Meta->map_value_type_name : std::string{};

    CollectionKind mgKeyArcKind = CollectionKind::Str;
    const bool mgKeyIsArc = keyTy == ptrTy_ && !keyName.empty() &&
        fieldTypeIsArcManaged(keyName, &mgKeyArcKind) &&
        mgKeyArcKind != CollectionKind::Str;
    CollectionKind mgValArcKind = CollectionKind::Str;
    const bool mgValIsArc = valTy == ptrTy_ && !valName.empty() &&
        fieldTypeIsArcManaged(valName, &mgValArcKind) &&
        mgValArcKind != CollectionKind::Str;

    auto mf1 = loadMapHeader(map1, "mg1");
    auto mf2 = loadMapHeader(map2, "mg2");

    // Allocate new map with capacity = len1 + len2
    llvm::Value *maxCap = builder_.CreateAdd(mf1.len, mf2.len, "mg_max_cap");
    llvm::Value *newHeader = emitArcAllocCollectionHeader(mapHeaderTy_);
    llvm::Value *newKeysSize = builder_.CreateMul(maxCap, llvm::ConstantInt::get(i64Ty_, keySize), "mg_ks");
    llvm::Value *newKeys = builder_.CreateCall(mallocFn, {newKeysSize}, "mg_keys");
    llvm::Value *newValsSize = builder_.CreateMul(maxCap, llvm::ConstantInt::get(i64Ty_, valSize), "mg_vs");
    llvm::Value *newVals = builder_.CreateCall(mallocFn, {newValsSize}, "mg_vals");

    // Copy all of map1
    llvm::Value *copy1KeySize = builder_.CreateMul(mf1.len, llvm::ConstantInt::get(i64Ty_, keySize), "mg_ck1");
    builder_.CreateCall(memcpyFn, {newKeys, mf1.keys, copy1KeySize});
    llvm::Value *copy1ValSize = builder_.CreateMul(mf1.len, llvm::ConstantInt::get(i64Ty_, valSize), "mg_cv1");
    builder_.CreateCall(memcpyFn, {newVals, mf1.vals, copy1ValSize});

    // Retain memcpy'd ARC-managed keys/values (#1242). Same defect class as
    // slice/take (#1204/#1235): memcpy duplicates pointers without bumping
    // refcounts.  The destructor now recursively releases inner elements, so
    // without retain here, releasing map1 (or the merged result) after merge
    // would free elements the other still points at.  str excluded per #1266.
    if (mgKeyIsArc) {
        emitCowRetainArcElements(newKeys, mf1.len, "mg_k1", mgKeyArcKind);
    }
    if (mgValIsArc) {
        emitCowRetainArcElements(newVals, mf1.len, "mg_v1", mgValArcKind);
    }

    // Set up header
    storeMapHeaderFields(newHeader, mf1.len, maxCap, newKeys, newVals);
    llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, newHeader, 0, "mg_len_ptr");

    // Init hash buckets
    emitBucketInit(newHeader, mapHeaderTy_, kMapLayout.bucketCountIdx, kMapLayout.bucketsPtrIdx, 16);

    // Re-hash map1 keys into new map's buckets
    {
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "mg_rh_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
        llvm::BasicBlock *rCondBB = createBB("mg.rh.cond");
        llvm::BasicBlock *rBodyBB = createBB("mg.rh.body");
        llvm::BasicBlock *rEndBB = createBB("mg.rh.end");
        emitBranchUncond(rCondBB);
        builder_.SetInsertPoint(rCondBB);
        llvm::Value *ri = builder_.CreateLoad(i64Ty_, iVar, "mg_ri");
        emitBranchCond(builder_.CreateICmpSLT(ri, mf1.len), rBodyBB, rEndBB);
        builder_.SetInsertPoint(rBodyBB);
        llvm::Value *kp = builder_.CreateGEP(keyTy, newKeys, {ri}, "mg_rh_kp");
        llvm::Value *kv = builder_.CreateLoad(keyTy, kp, "mg_rh_kv");
        if (!keyName.empty()) propagateTypeMeta(keyName, kv);
        emitBucketInsertAndRehashCheck(newHeader, mapHeaderTy_, kMapLayout.lenIdx, kMapLayout.bucketCountIdx, kMapLayout.bucketsPtrIdx, kv, keyTy, ri);
        builder_.CreateStore(builder_.CreateAdd(ri, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        emitBranchUncond(rCondBB);
        builder_.SetInsertPoint(rEndBB);
    }

    // Add/update entries from map2 (rhs-wins on key collision)
    {
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "mg_i2");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
        llvm::BasicBlock *condBB = createBB("mg.add.cond");
        llvm::BasicBlock *bodyBB = createBB("mg.add.body");
        llvm::BasicBlock *endBB = createBB("mg.add.end");
        emitBranchUncond(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "mg_ci");
        emitBranchCond(builder_.CreateICmpSLT(i, mf2.len), bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *kp = builder_.CreateGEP(keyTy, mf2.keys, {i}, "mg_kp2");
        llvm::Value *kv = builder_.CreateLoad(keyTy, kp, "mg_kv2");
        if (!keyName.empty()) propagateTypeMeta(keyName, kv);
        llvm::Value *vp = builder_.CreateGEP(valTy, mf2.vals, {i}, "mg_vp2");
        llvm::Value *vv = builder_.CreateLoad(valTy, vp, "mg_vv2");

        // Check if key exists in new map
        llvm::Value *lookupIdx = emitMapKeyLookup(newHeader, kv, keyTy, keyName);
        llvm::Value *exists = builder_.CreateICmpSGE(lookupIdx, llvm::ConstantInt::get(i64Ty_, 0), "mg_exists");

        llvm::BasicBlock *updateBB = createBB("mg.update");
        llvm::BasicBlock *insertBB = createBB("mg.insert");
        llvm::BasicBlock *nextBB = createBB("mg.next");
        emitBranchCond(exists, updateBB, insertBB);

        // Update existing key's value
        builder_.SetInsertPoint(updateBB);
        llvm::Value *curVals = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, newHeader, 3), "mg_cur_vals");
        llvm::Value *updPtr = builder_.CreateGEP(valTy, curVals, {lookupIdx}, "mg_upd_ptr");
        // Release the old ARC-managed value before overwriting (#1242 leak).
        if (mgValIsArc) {
            llvm::Value *oldVal = builder_.CreateLoad(valTy, updPtr, "mg_upd_old");
            llvm::Value *oldHdr = (mgValArcKind == CollectionKind::Str)
                ? emitStrGetHeaderFromData(oldVal) : emitArcGetHeaderFromData(oldVal);
            emitArcRelease(oldHdr, false, nullptr, nullptr);
            retainArcValue(vv);
        }
        builder_.CreateStore(vv, updPtr);
        emitBranchUncond(nextBB);

        // Insert new key-value pair
        builder_.SetInsertPoint(insertBB);
        llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "mg_cur_len");
        llvm::Value *curKeys = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, newHeader, 2), "mg_cur_keys");
        llvm::Value *newKeyPtr = builder_.CreateGEP(keyTy, curKeys, {curLen}, "mg_new_kp");
        if (mgKeyIsArc) retainArcValue(kv);
        builder_.CreateStore(kv, newKeyPtr);
        llvm::Value *curVals2 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, newHeader, 3), "mg_cur_vals2");
        llvm::Value *newValPtr = builder_.CreateGEP(valTy, curVals2, {curLen}, "mg_new_vp");
        if (mgValIsArc) retainArcValue(vv);
        builder_.CreateStore(vv, newValPtr);
        builder_.CreateStore(builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1)), lenPtr);
        emitBucketInsertAndRehashCheck(newHeader, mapHeaderTy_, kMapLayout.lenIdx, kMapLayout.bucketCountIdx, kMapLayout.bucketsPtrIdx, kv, keyTy, curLen);
        emitBranchUncond(nextBB);

        builder_.SetInsertPoint(nextBB);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        emitBranchUncond(condBB);
        builder_.SetInsertPoint(endBB);
    }

    setTypeMeta(TypeMeta::MapKey, newHeader, keyTy);
    setTypeMeta(TypeMeta::MapValue, newHeader, valTy);
    // Carry Ry type names (key/value) from map1 so equality on the merged result
    // works correctly for complex key and value types (#961).
    propagateMeta(map1, newHeader);
    return newHeader;
}

llvm::Value *CodeGen::emitCollOp_merge(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;
    llvm::Value *map1 = emitExpr(*e.args[0]);
    llvm::Value *map2 = emitExpr(*e.args[1]);
    llvm::Type *keyTy = getMapKeyType(map1);
    llvm::Type *valTy = getMapValueType(map1);
    if (!keyTy || !valTy)
        codegenError("merge() requires maps as arguments");
    llvm::Type *keyTy2 = getMapKeyType(map2);
    llvm::Type *valTy2 = getMapValueType(map2);
    if (!keyTy2 || keyTy2 != keyTy || !valTy2 || valTy2 != valTy)
        codegenError("merge() requires two maps with the same key and value types");
    return emitMapMergeCore(map1, map2, keyTy, valTy);
}

} // namespace ry
