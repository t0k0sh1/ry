#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"

namespace {
// RAII guard to restore moved-out args after proxy CallExpr delegation
struct ArgsRestoreGuard {
    std::vector<ExprPtr> &dst;
    std::vector<ExprPtr> &src;
    ~ArgsRestoreGuard() { dst = std::move(src); }
};
} // namespace

// ===== Builtin Collection =====

llvm::Value *CodeGen::emitBuiltinCollection(const CallExpr &e) {
    // add(set, val) → add element to set (no-op if already present)
    // Only intercept if first arg is a set (fall through to user function otherwise)
    if (e.callee == "add" && e.args.size() == 2) {
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
            llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, setPtr, 0, "set_len_ptr");
            llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "set_len");
            llvm::Value *capPtr = builder_.CreateStructGEP(setHeaderTy_, setPtr, 1, "set_cap_ptr");
            llvm::Value *cap = builder_.CreateLoad(i64Ty_, capPtr, "set_cap");

            llvm::Value *needGrow = builder_.CreateICmpEQ(length, cap, "need_grow");
            llvm::BasicBlock *growBB = llvm::BasicBlock::Create(*ctx_, "set.grow", fn_);
            llvm::BasicBlock *storeBB = llvm::BasicBlock::Create(*ctx_, "set.store", fn_);
            builder_.CreateCondBr(needGrow, growBB, storeBB);

            builder_.SetInsertPoint(growBB);
            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);
            llvm::Value *newCap = builder_.CreateMul(cap, llvm::ConstantInt::get(i64Ty_, 2), "new_cap");
            auto mallocFn = getStdlibMalloc();
            llvm::Value *newSize = builder_.CreateMul(newCap, llvm::ConstantInt::get(i64Ty_, elemSize), "new_size");
            llvm::Value *newElemsPtr = builder_.CreateCall(mallocFn, {newSize}, "new_elems");

            auto memcpyFn = getStdlibMemcpy();
            llvm::Value *elemsPtrField = builder_.CreateStructGEP(setHeaderTy_, setPtr, 2, "elems_field");
            llvm::Value *oldElemsPtr = builder_.CreateLoad(ptrTy_, elemsPtrField, "old_elems");
            llvm::Value *oldSize = builder_.CreateMul(length, llvm::ConstantInt::get(i64Ty_, elemSize), "old_size");
            builder_.CreateCall(memcpyFn, {newElemsPtr, oldElemsPtr, oldSize});

            auto freeFn = getStdlibFree();
            builder_.CreateCall(freeFn, {oldElemsPtr});

            builder_.CreateStore(newElemsPtr, elemsPtrField);
            builder_.CreateStore(newCap, capPtr);
            builder_.CreateBr(storeBB);

            builder_.SetInsertPoint(storeBB);
            llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "cur_len");
            llvm::Value *elemsPtrField2 = builder_.CreateStructGEP(setHeaderTy_, setPtr, 2, "elems_field2");
            llvm::Value *curElemsPtr = builder_.CreateLoad(ptrTy_, elemsPtrField2, "cur_elems");
            llvm::Value *newElemPtr = builder_.CreateGEP(elemTy, curElemsPtr, {curLen}, "new_elem_ptr");
            builder_.CreateStore(elem, newElemPtr);

            llvm::Value *newLen = builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1), "new_len");
            builder_.CreateStore(newLen, lenPtr);

            // Insert into hash table buckets and check rehash
            emitBucketInsertAndRehashCheck(setPtr, setHeaderTy_, 0, 3, 4, elem, elemTy, curLen);

            builder_.CreateBr(endBB);

            builder_.SetInsertPoint(endBB);
            return llvm::ConstantInt::get(i64Ty_, 0);
        }
        // Not a set — fall through to user function resolution
    }

    // remove(set, val) → remove element from set
    if (e.callee == "remove" && e.args.size() == 2) {
        llvm::Value *containerPtr = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getSetElementType(containerPtr);
        if (elemTy) {
            llvm::Value *elem = emitExpr(*e.args[1]);
            if (elem->getType() != elemTy)
                codegenError("remove() element type mismatch");

            llvm::Value *idx = emitSetElementLookup(containerPtr, elem, elemTy);
            llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "found");

            llvm::BasicBlock *removeBB = llvm::BasicBlock::Create(*ctx_, "set.remove", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "set.remove_end", fn_);
            builder_.CreateCondBr(found, removeBB, endBB);

            builder_.SetInsertPoint(removeBB);
            llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, containerPtr, 0, "set_len_ptr");
            llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "set_len");
            llvm::Value *elemsPtrField = builder_.CreateStructGEP(setHeaderTy_, containerPtr, 2, "set_elems_ptr");
            llvm::Value *elemsPtr = builder_.CreateLoad(ptrTy_, elemsPtrField, "set_elems");

            // Remove from bucket: set tombstone for this element
            {
                auto hfi = resolveHashFn(elemTy);
                llvm::Value *hashElem = elem;
                if (elemTy != hfi.hashArgTy && elemTy->isIntegerTy() && hfi.hashArgTy->isIntegerTy())
                    hashElem = builder_.CreateZExt(elem, hfi.hashArgTy, "rm_hash_zext");
                llvm::FunctionType *hashTy = llvm::FunctionType::get(i64Ty_, {hfi.hashArgTy}, false);
                llvm::FunctionCallee hashFn = mod_->getOrInsertFunction(hfi.hashFnName, hashTy);
                llvm::Value *hashVal = builder_.CreateCall(hashFn, {hashElem}, "rm_hash");

                llvm::Value *bucketsField = builder_.CreateStructGEP(setHeaderTy_, containerPtr, 4, "rm_bp");
                llvm::Value *bucketsPtr = builder_.CreateLoad(ptrTy_, bucketsField, "rm_buckets");
                llvm::Value *bcField = builder_.CreateStructGEP(setHeaderTy_, containerPtr, 3, "rm_bc_field");
                llvm::Value *bucketCount = builder_.CreateLoad(i64Ty_, bcField, "rm_bc");
                llvm::Value *bucketMask = builder_.CreateSub(bucketCount, llvm::ConstantInt::get(i64Ty_, 1), "rm_bmask");

                llvm::FunctionType *removeTy = llvm::FunctionType::get(
                    llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_, i64Ty_, i64Ty_}, false);
                llvm::FunctionCallee removeFn = mod_->getOrInsertFunction("__ry_ht_remove", removeTy);
                builder_.CreateCall(removeFn, {bucketsPtr, bucketMask, hashVal, idx});
            }

            // Swap-remove: move last element to idx position
            llvm::Value *lastIdx = builder_.CreateSub(length, llvm::ConstantInt::get(i64Ty_, 1), "last_idx");
            llvm::Value *isNotLast = builder_.CreateICmpNE(idx, lastIdx, "is_not_last");

            llvm::BasicBlock *swapBB = llvm::BasicBlock::Create(*ctx_, "set.swap", fn_);
            llvm::BasicBlock *decBB = llvm::BasicBlock::Create(*ctx_, "set.dec", fn_);
            builder_.CreateCondBr(isNotLast, swapBB, decBB);

            builder_.SetInsertPoint(swapBB);
            // Load last element
            llvm::Value *lastPtr = builder_.CreateGEP(elemTy, elemsPtr, {lastIdx}, "last_ptr");
            llvm::Value *lastVal = builder_.CreateLoad(elemTy, lastPtr, "last_val");
            // Store at idx position
            llvm::Value *dstPtr = builder_.CreateGEP(elemTy, elemsPtr, {idx}, "swap_dst");
            builder_.CreateStore(lastVal, dstPtr);

            // Update bucket for the moved element: change lastIdx -> idx
            {
                auto hfi2 = resolveHashFn(elemTy);
                llvm::Value *hashLastVal = lastVal;
                if (elemTy != hfi2.hashArgTy && elemTy->isIntegerTy() && hfi2.hashArgTy->isIntegerTy())
                    hashLastVal = builder_.CreateZExt(lastVal, hfi2.hashArgTy, "swap_hash_zext");
                llvm::FunctionType *hashTy = llvm::FunctionType::get(i64Ty_, {hfi2.hashArgTy}, false);
                llvm::FunctionCallee hashFn = mod_->getOrInsertFunction(hfi2.hashFnName, hashTy);
                llvm::Value *lastHash = builder_.CreateCall(hashFn, {hashLastVal}, "last_hash");

                llvm::Value *bucketsField = builder_.CreateStructGEP(setHeaderTy_, containerPtr, 4, "swap_bp");
                llvm::Value *bucketsPtr = builder_.CreateLoad(ptrTy_, bucketsField, "swap_buckets");
                llvm::Value *bcField = builder_.CreateStructGEP(setHeaderTy_, containerPtr, 3, "swap_bc_field");
                llvm::Value *bucketCount = builder_.CreateLoad(i64Ty_, bcField, "swap_bc");
                llvm::Value *bucketMask = builder_.CreateSub(bucketCount, llvm::ConstantInt::get(i64Ty_, 1), "swap_bmask");

                llvm::FunctionType *updateTy = llvm::FunctionType::get(
                    llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_, i64Ty_, i64Ty_, i64Ty_}, false);
                llvm::FunctionCallee updateFn = mod_->getOrInsertFunction("__ry_ht_update_index", updateTy);
                builder_.CreateCall(updateFn, {bucketsPtr, bucketMask, lastHash, lastIdx, idx});
            }
            builder_.CreateBr(decBB);

            builder_.SetInsertPoint(decBB);
            llvm::Value *newLen = builder_.CreateSub(length, llvm::ConstantInt::get(i64Ty_, 1), "new_len");
            builder_.CreateStore(newLen, lenPtr);
            builder_.CreateBr(endBB);

            builder_.SetInsertPoint(endBB);
            return llvm::ConstantInt::get(i64Ty_, 0);
        }
        // Not a set — try list remove
        llvm::Type *listElemTy = getListElementType(containerPtr);
        if (listElemTy) {
            llvm::Value *val = emitExpr(*e.args[1]);
            if (val->getType() != listElemTy)
                codegenError("remove() value type mismatch with list element type");

            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t elemSize = dl.getTypeAllocSize(listElemTy);

            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, containerPtr, 0, "lrem_len_ptr");
            llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "lrem_len");
            llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, containerPtr, 2, "lrem_data_field");
            llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "lrem_data");

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
            llvm::Value *inBounds = builder_.CreateICmpSLT(iVal, length, "lrem_in_bounds");
            llvm::Value *cont = builder_.CreateAnd(notYetFound, inBounds, "lrem_cont");
            builder_.CreateCondBr(cont, bodyBB, endSearchBB);

            builder_.SetInsertPoint(bodyBB);
            llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "lrem_ic");
            llvm::Value *elemPtr = builder_.CreateGEP(listElemTy, dataPtr, {iCur}, "lrem_elem_ptr");
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
            llvm::Value *dstPtr = builder_.CreateGEP(listElemTy, dataPtr, {idx}, "lrem_dst");
            llvm::Value *srcPtr = builder_.CreateGEP(listElemTy, dataPtr,
                {builder_.CreateAdd(idx, llvm::ConstantInt::get(i64Ty_, 1))}, "lrem_src");
            llvm::Value *moveCount = builder_.CreateSub(
                builder_.CreateSub(length, idx), llvm::ConstantInt::get(i64Ty_, 1), "lrem_move_count");
            llvm::Value *moveBytes = builder_.CreateMul(moveCount, llvm::ConstantInt::get(i64Ty_, elemSize), "lrem_move_bytes");
            builder_.CreateCall(memmoveFn, {dstPtr, srcPtr, moveBytes});
            llvm::Value *newLen = builder_.CreateSub(length, llvm::ConstantInt::get(i64Ty_, 1), "lrem_new_len");
            builder_.CreateStore(newLen, lenPtr);
            builder_.CreateBr(doneBB);

            builder_.SetInsertPoint(doneBB);
            return llvm::ConstantInt::get(i64Ty_, 0);
        }
        // Not a set or list — try map remove
        llvm::Type *keyTy = getMapKeyType(containerPtr);
        llvm::Type *valTy = getMapValueType(containerPtr);
        if (keyTy && valTy) {
            llvm::Value *key = emitExpr(*e.args[1]);
            if (key->getType() != keyTy)
                codegenError("remove() key type mismatch");
            llvm::Value *idx = emitMapKeyLookup(containerPtr, key, keyTy);
            llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "mrem_found");

            llvm::BasicBlock *removeBB = llvm::BasicBlock::Create(*ctx_, "mrem.do", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "mrem.end", fn_);
            builder_.CreateCondBr(found, removeBB, endBB);

            builder_.SetInsertPoint(removeBB);
            llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, containerPtr, 0, "mrem_len_ptr");
            llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "mrem_len");
            llvm::Value *keysPtrField = builder_.CreateStructGEP(mapHeaderTy_, containerPtr, 2, "mrem_keys_field");
            llvm::Value *keysPtr = builder_.CreateLoad(ptrTy_, keysPtrField, "mrem_keys");
            llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, containerPtr, 3, "mrem_vals_field");
            llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "mrem_vals");

            // Remove from hash table
            auto hfi = resolveHashFn(keyTy);
            llvm::Value *hashKey = key;
            if (keyTy != hfi.hashArgTy && keyTy->isIntegerTy() && hfi.hashArgTy->isIntegerTy())
                hashKey = builder_.CreateZExt(key, hfi.hashArgTy, "mrem_hash_zext");
            llvm::FunctionType *hashTy = llvm::FunctionType::get(i64Ty_, {hfi.hashArgTy}, false);
            llvm::FunctionCallee hashFn = mod_->getOrInsertFunction(hfi.hashFnName, hashTy);
            llvm::Value *hashVal = builder_.CreateCall(hashFn, {hashKey}, "mrem_hash");

            llvm::Value *bucketsField = builder_.CreateStructGEP(mapHeaderTy_, containerPtr, 5, "mrem_bp");
            llvm::Value *bucketsPtr = builder_.CreateLoad(ptrTy_, bucketsField, "mrem_buckets");
            llvm::Value *bcField = builder_.CreateStructGEP(mapHeaderTy_, containerPtr, 4, "mrem_bc_field");
            llvm::Value *bucketCount = builder_.CreateLoad(i64Ty_, bcField, "mrem_bc");
            llvm::Value *bucketMask = builder_.CreateSub(bucketCount, llvm::ConstantInt::get(i64Ty_, 1), "mrem_bmask");

            llvm::FunctionType *htRemoveTy = llvm::FunctionType::get(
                llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_, i64Ty_, i64Ty_}, false);
            llvm::FunctionCallee htRemoveFn = mod_->getOrInsertFunction("__ry_ht_remove", htRemoveTy);
            builder_.CreateCall(htRemoveFn, {bucketsPtr, bucketMask, hashVal, idx});

            // Swap-remove from keys and values arrays
            llvm::Value *lastIdx = builder_.CreateSub(length, llvm::ConstantInt::get(i64Ty_, 1), "mrem_last_idx");
            llvm::Value *isNotLast = builder_.CreateICmpNE(idx, lastIdx, "mrem_not_last");

            llvm::BasicBlock *swapBB = llvm::BasicBlock::Create(*ctx_, "mrem.swap", fn_);
            llvm::BasicBlock *decBB = llvm::BasicBlock::Create(*ctx_, "mrem.dec", fn_);
            builder_.CreateCondBr(isNotLast, swapBB, decBB);

            builder_.SetInsertPoint(swapBB);
            llvm::Value *lastKeyPtr = builder_.CreateGEP(keyTy, keysPtr, {lastIdx}, "mrem_last_kp");
            llvm::Value *lastKey = builder_.CreateLoad(keyTy, lastKeyPtr, "mrem_last_key");
            llvm::Value *dstKeyPtr = builder_.CreateGEP(keyTy, keysPtr, {idx}, "mrem_dst_kp");
            builder_.CreateStore(lastKey, dstKeyPtr);
            llvm::Value *lastValPtr = builder_.CreateGEP(valTy, valsPtr, {lastIdx}, "mrem_last_vp");
            llvm::Value *lastVal = builder_.CreateLoad(valTy, lastValPtr, "mrem_last_val");
            llvm::Value *dstValPtr = builder_.CreateGEP(valTy, valsPtr, {idx}, "mrem_dst_vp");
            builder_.CreateStore(lastVal, dstValPtr);

            // Update bucket for moved element
            llvm::Value *hashLastKey = lastKey;
            if (keyTy != hfi.hashArgTy && keyTy->isIntegerTy() && hfi.hashArgTy->isIntegerTy())
                hashLastKey = builder_.CreateZExt(lastKey, hfi.hashArgTy, "mrem_lk_hash_zext");
            llvm::Value *lastKeyHash = builder_.CreateCall(hashFn, {hashLastKey}, "mrem_lk_hash");
            llvm::FunctionType *updateTy = llvm::FunctionType::get(
                llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_, i64Ty_, i64Ty_, i64Ty_}, false);
            llvm::FunctionCallee updateFn = mod_->getOrInsertFunction("__ry_ht_update_index", updateTy);
            builder_.CreateCall(updateFn, {bucketsPtr, bucketMask, lastKeyHash, lastIdx, idx});
            builder_.CreateBr(decBB);

            builder_.SetInsertPoint(decBB);
            llvm::Value *newLen = builder_.CreateSub(length, llvm::ConstantInt::get(i64Ty_, 1), "mrem_new_len");
            builder_.CreateStore(newLen, lenPtr);
            builder_.CreateBr(endBB);

            builder_.SetInsertPoint(endBB);
            return llvm::ConstantInt::get(i64Ty_, 0);
        }
        // Not a set, list, or map — fall through to user function resolution
    }

    // append(list, val) → mutating append
    if (e.callee == "append" && e.args.size() == 2) {
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

            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, listPtr, 0, "app_len_ptr");
            llvm::Value *capPtr = builder_.CreateStructGEP(listHeaderTy_, listPtr, 1, "app_cap_ptr");
            llvm::Value *dataField = builder_.CreateStructGEP(listHeaderTy_, listPtr, 2, "app_data_field");
            llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "app_len");
            llvm::Value *cap = builder_.CreateLoad(i64Ty_, capPtr, "app_cap");
            llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataField, "app_data");

            // Check if realloc needed
            llvm::Value *needGrow = builder_.CreateICmpEQ(len, cap, "app_need_grow");
            llvm::BasicBlock *growBB = llvm::BasicBlock::Create(*ctx_, "app.grow", fn_);
            llvm::BasicBlock *storeBB = llvm::BasicBlock::Create(*ctx_, "app.store", fn_);

            builder_.CreateCondBr(needGrow, growBB, storeBB);

            // Grow: new_cap = cap * 2 (min 4)
            builder_.SetInsertPoint(growBB);
            llvm::Value *four = llvm::ConstantInt::get(i64Ty_, 4);
            llvm::Value *doubled = builder_.CreateMul(cap, llvm::ConstantInt::get(i64Ty_, 2), "app_doubled");
            llvm::Value *newCap = builder_.CreateSelect(
                builder_.CreateICmpSGT(doubled, four, "cap_gt4"), doubled, four, "app_new_cap");
            llvm::Value *newSize = builder_.CreateMul(newCap, llvm::ConstantInt::get(i64Ty_, elemSize), "app_new_size");
            llvm::Value *newData = builder_.CreateCall(mallocFn, {newSize}, "app_new_data");
            llvm::Value *oldSize = builder_.CreateMul(len, llvm::ConstantInt::get(i64Ty_, elemSize), "app_old_size");
            builder_.CreateCall(memcpyFn, {newData, dataPtr, oldSize});
            builder_.CreateCall(freeFn, {dataPtr});
            builder_.CreateStore(newData, dataField);
            builder_.CreateStore(newCap, capPtr);
            builder_.CreateBr(storeBB);

            // Store the new element
            builder_.SetInsertPoint(storeBB);
            llvm::Value *curData = builder_.CreateLoad(ptrTy_, dataField, "app_cur_data");
            llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "app_cur_len");
            llvm::Value *elemPtr = builder_.CreateGEP(elemTy, curData, curLen, "app_elem_ptr");
            builder_.CreateStore(val, elemPtr);
            llvm::Value *newLen = builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1), "app_new_len");
            builder_.CreateStore(newLen, lenPtr);

            return llvm::ConstantInt::get(i64Ty_, 0);
        }
    }

    // appended(list, elem) → new list with element added
    if (e.callee == "appended" && e.args.size() == 2) {
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

            llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(listHeaderTy_, listPtr, 0), "apd_len");
            llvm::Value *srcData = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(listHeaderTy_, listPtr, 2), "apd_data");
            llvm::Value *newLen = builder_.CreateAdd(srcLen, llvm::ConstantInt::get(i64Ty_, 1), "apd_new_len");

            llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "apd_header");
            llvm::Value *newDataSize = builder_.CreateMul(newLen, llvm::ConstantInt::get(i64Ty_, elemSize), "apd_ds");
            llvm::Value *newData = builder_.CreateCall(mallocFn, {newDataSize}, "apd_nd");

            llvm::Value *oldDataSize = builder_.CreateMul(srcLen, llvm::ConstantInt::get(i64Ty_, elemSize), "apd_ods");
            builder_.CreateCall(memcpyFn, {newData, srcData, oldDataSize});

            llvm::Value *newElemPtr = builder_.CreateGEP(elemTy, newData, srcLen, "apd_new_ep");
            builder_.CreateStore(val, newElemPtr);

            builder_.CreateStore(newLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 0));
            builder_.CreateStore(newLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 1));
            builder_.CreateStore(newData, builder_.CreateStructGEP(listHeaderTy_, newHeader, 2));

            list_element_types_[newHeader] = elemTy;
            return newHeader;
        }
    }

    // append!(list, elem) → alias for append
    if (e.callee == "append!" && e.args.size() == 2) {
        auto &mutArgs = const_cast<CallExpr &>(e).args;
        CallExpr appendProxy;
        appendProxy.callee = "append";
        appendProxy.args = std::move(mutArgs);
        ArgsRestoreGuard guard{mutArgs, appendProxy.args};
        return emitBuiltinCollection(appendProxy);
    }

    // pop(list) → Option<T>: remove and return last element
    if (e.callee == "pop" && e.args.size() == 1) {
        llvm::Value *listPtr = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listPtr);
        if (elemTy) {
            llvm::StructType *optTy = getOptionType(elemTy);
            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, listPtr, 0, "pop_len_ptr");
            llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "pop_len");
            llvm::Value *dataField = builder_.CreateStructGEP(listHeaderTy_, listPtr, 2, "pop_data_field");
            llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataField, "pop_data");

            llvm::Value *isEmpty = builder_.CreateICmpEQ(len, llvm::ConstantInt::get(i64Ty_, 0), "pop_empty");
            llvm::BasicBlock *emptyBB = llvm::BasicBlock::Create(*ctx_, "pop.empty", fn_);
            llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "pop.ok", fn_);
            llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "pop.merge", fn_);
            builder_.CreateCondBr(isEmpty, emptyBB, okBB);

            builder_.SetInsertPoint(emptyBB);
            llvm::Value *noneVal = buildNoneValue(optTy);
            builder_.CreateBr(mergeBB);
            llvm::BasicBlock *emptyEndBB = builder_.GetInsertBlock();

            builder_.SetInsertPoint(okBB);
            llvm::Value *lastIdx = builder_.CreateSub(len, llvm::ConstantInt::get(i64Ty_, 1), "pop_last_idx");
            llvm::Value *elemPtr = builder_.CreateGEP(elemTy, dataPtr, lastIdx, "pop_elem_ptr");
            llvm::Value *val = builder_.CreateLoad(elemTy, elemPtr, "pop_val");
            builder_.CreateStore(lastIdx, lenPtr);
            llvm::Value *someVal = buildSomeValue(val, optTy);
            builder_.CreateBr(mergeBB);
            llvm::BasicBlock *okEndBB = builder_.GetInsertBlock();

            builder_.SetInsertPoint(mergeBB);
            llvm::PHINode *phi = builder_.CreatePHI(optTy, 2, "pop_result");
            phi->addIncoming(noneVal, emptyEndBB);
            phi->addIncoming(someVal, okEndBB);
            return phi;
        }
    }

    // slice(list, start, end) → new sub-list
    if (e.callee == "slice" && e.args.size() == 3) {
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

            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, listPtr, 0, "sl_len_ptr");
            llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "sl_len");
            llvm::Value *dataField = builder_.CreateStructGEP(listHeaderTy_, listPtr, 2, "sl_data_field");
            llvm::Value *srcData = builder_.CreateLoad(ptrTy_, dataField, "sl_src_data");

            // Clamp start and end
            llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
            llvm::Value *clampedStart = builder_.CreateSelect(
                builder_.CreateICmpSLT(startVal, zero), zero, startVal, "sl_cstart");
            clampedStart = builder_.CreateSelect(
                builder_.CreateICmpSGT(clampedStart, len), len, clampedStart, "sl_cstart2");
            llvm::Value *clampedEnd = builder_.CreateSelect(
                builder_.CreateICmpSLT(endVal, zero), zero, endVal, "sl_cend");
            clampedEnd = builder_.CreateSelect(
                builder_.CreateICmpSGT(clampedEnd, len), len, clampedEnd, "sl_cend2");

            // Compute count = max(0, end - start)
            llvm::Value *diff = builder_.CreateSub(clampedEnd, clampedStart, "sl_diff");
            llvm::Value *count = builder_.CreateSelect(
                builder_.CreateICmpSGT(diff, zero), diff, zero, "sl_count");

            // Allocate new list
            llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "sl_header");
            llvm::Value *dataSize = builder_.CreateMul(count, llvm::ConstantInt::get(i64Ty_, elemSize), "sl_dsize");
            llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "sl_data");

            // Copy elements
            llvm::Value *srcOffset = builder_.CreateGEP(elemTy, srcData, clampedStart, "sl_src_off");
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
    }

    // take(list, n) → new list with first n elements
    if (e.callee == "take" && e.args.size() == 2) {
        llvm::Value *listPtr = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listPtr);
        if (elemTy) {
            llvm::Value *nVal = emitExpr(*e.args[1]);

            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);
            auto mallocFn = getStdlibMalloc();
            auto memcpyFn = getStdlibMemcpy();

            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, listPtr, 0, "tk_len_ptr");
            llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "tk_len");
            llvm::Value *dataField = builder_.CreateStructGEP(listHeaderTy_, listPtr, 2, "tk_data_field");
            llvm::Value *srcData = builder_.CreateLoad(ptrTy_, dataField, "tk_src_data");

            // Clamp n: max(0, min(n, len))
            llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
            llvm::Value *clampedN = builder_.CreateSelect(
                builder_.CreateICmpSLT(nVal, zero), zero, nVal, "tk_cn");
            clampedN = builder_.CreateSelect(
                builder_.CreateICmpSGT(clampedN, len), len, clampedN, "tk_cn2");

            // Allocate new list
            llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "tk_header");
            llvm::Value *dataSize = builder_.CreateMul(clampedN, llvm::ConstantInt::get(i64Ty_, elemSize), "tk_dsize");
            llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "tk_data");

            // Copy elements
            builder_.CreateCall(memcpyFn, {newData, srcData, dataSize});

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
    }
    // ===== insert(list, index, value) =====
    if (e.callee == "insert" && e.args.size() == 3) {
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

            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, listPtr, 0, "ins_len_ptr");
            llvm::Value *capPtr = builder_.CreateStructGEP(listHeaderTy_, listPtr, 1, "ins_cap_ptr");
            llvm::Value *dataField = builder_.CreateStructGEP(listHeaderTy_, listPtr, 2, "ins_data_field");
            llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "ins_len");
            llvm::Value *cap = builder_.CreateLoad(i64Ty_, capPtr, "ins_cap");
            llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataField, "ins_data");

            // Bounds check
            llvm::Value *outOfBounds = builder_.CreateOr(
                builder_.CreateICmpSLT(idx, llvm::ConstantInt::get(i64Ty_, 0)),
                builder_.CreateICmpSGT(idx, len), "ins_oob");
            llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "ins.err", fn_);
            llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "ins.ok", fn_);
            builder_.CreateCondBr(outOfBounds, errBB, okBB);
            builder_.SetInsertPoint(errBB);
            emitRuntimeError("runtime error: insert() index out of bounds\n", ".ins_oob_err");

            builder_.SetInsertPoint(okBB);
            // Check if realloc needed
            llvm::Value *needGrow = builder_.CreateICmpEQ(len, cap, "ins_need_grow");
            llvm::BasicBlock *growBB = llvm::BasicBlock::Create(*ctx_, "ins.grow", fn_);
            llvm::BasicBlock *moveBB = llvm::BasicBlock::Create(*ctx_, "ins.move", fn_);
            builder_.CreateCondBr(needGrow, growBB, moveBB);

            builder_.SetInsertPoint(growBB);
            llvm::Value *four = llvm::ConstantInt::get(i64Ty_, 4);
            llvm::Value *doubled = builder_.CreateMul(cap, llvm::ConstantInt::get(i64Ty_, 2), "ins_doubled");
            llvm::Value *newCap = builder_.CreateSelect(
                builder_.CreateICmpSGT(doubled, four), doubled, four, "ins_new_cap");
            llvm::Value *newSize = builder_.CreateMul(newCap, llvm::ConstantInt::get(i64Ty_, elemSize), "ins_new_size");
            llvm::Value *newData = builder_.CreateCall(mallocFn, {newSize}, "ins_new_data");
            llvm::Value *oldSize = builder_.CreateMul(len, llvm::ConstantInt::get(i64Ty_, elemSize), "ins_old_size");
            builder_.CreateCall(memcpyFn, {newData, dataPtr, oldSize});
            builder_.CreateCall(freeFn, {dataPtr});
            builder_.CreateStore(newData, dataField);
            builder_.CreateStore(newCap, capPtr);
            builder_.CreateBr(moveBB);

            builder_.SetInsertPoint(moveBB);
            llvm::Value *curData = builder_.CreateLoad(ptrTy_, dataField, "ins_cur_data");
            // memmove elements from idx to idx+1
            llvm::Value *srcPtr = builder_.CreateGEP(elemTy, curData, {idx}, "ins_src");
            llvm::Value *dstPtr = builder_.CreateGEP(elemTy, curData,
                {builder_.CreateAdd(idx, llvm::ConstantInt::get(i64Ty_, 1))}, "ins_dst");
            llvm::Value *moveCount = builder_.CreateSub(len, idx, "ins_move_count");
            llvm::Value *moveBytes = builder_.CreateMul(moveCount, llvm::ConstantInt::get(i64Ty_, elemSize), "ins_move_bytes");
            builder_.CreateCall(memmoveFn, {dstPtr, srcPtr, moveBytes});
            // Store new element at idx
            llvm::Value *insertPtr = builder_.CreateGEP(elemTy, curData, {idx}, "ins_ptr");
            builder_.CreateStore(val, insertPtr);
            // len++
            llvm::Value *newLen = builder_.CreateAdd(len, llvm::ConstantInt::get(i64Ty_, 1), "ins_new_len");
            builder_.CreateStore(newLen, lenPtr);

            return llvm::ConstantInt::get(i64Ty_, 0);
        }
    }

    // ===== remove_at(list, index) =====
    if (e.callee == "remove_at" && e.args.size() == 2) {
        llvm::Value *listPtr = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listPtr);
        if (elemTy) {
            llvm::Value *idx = emitExpr(*e.args[1]);
            if (idx->getType() != i64Ty_)
                codegenError("remove_at() index must be int");

            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);

            auto memmoveFn = getStdlibMemmove();

            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, listPtr, 0, "rmat_len_ptr");
            llvm::Value *dataField = builder_.CreateStructGEP(listHeaderTy_, listPtr, 2, "rmat_data_field");
            llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "rmat_len");
            llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataField, "rmat_data");

            // Bounds check
            llvm::Value *outOfBounds = builder_.CreateOr(
                builder_.CreateICmpSLT(idx, llvm::ConstantInt::get(i64Ty_, 0)),
                builder_.CreateICmpSGE(idx, len), "rmat_oob");
            llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "rmat.err", fn_);
            llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "rmat.ok", fn_);
            builder_.CreateCondBr(outOfBounds, errBB, okBB);
            builder_.SetInsertPoint(errBB);
            emitRuntimeError("runtime error: remove_at() index out of bounds\n", ".rmat_oob_err");

            builder_.SetInsertPoint(okBB);
            // Save element to return
            llvm::Value *elemPtr = builder_.CreateGEP(elemTy, dataPtr, {idx}, "rmat_elem_ptr");
            llvm::Value *removedVal = builder_.CreateLoad(elemTy, elemPtr, "rmat_val");
            // memmove elements from idx+1 to idx
            llvm::Value *srcPtr = builder_.CreateGEP(elemTy, dataPtr,
                {builder_.CreateAdd(idx, llvm::ConstantInt::get(i64Ty_, 1))}, "rmat_src");
            llvm::Value *moveCount = builder_.CreateSub(
                builder_.CreateSub(len, idx), llvm::ConstantInt::get(i64Ty_, 1), "rmat_move_count");
            llvm::Value *moveBytes = builder_.CreateMul(moveCount, llvm::ConstantInt::get(i64Ty_, elemSize), "rmat_move_bytes");
            builder_.CreateCall(memmoveFn, {elemPtr, srcPtr, moveBytes});
            // len--
            llvm::Value *newLen = builder_.CreateSub(len, llvm::ConstantInt::get(i64Ty_, 1), "rmat_new_len");
            builder_.CreateStore(newLen, lenPtr);

            return removedVal;
        }
    }

    // ===== distinct(list) → new list with duplicates removed =====
    if (e.callee == "distinct" && e.args.size() == 1) {
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy)
            codegenError("distinct() requires a list as argument");

        // Reject non-string pointer elements (e.g. list-of-lists) — strcmp would be UB
        if (elemTy == ptrTy_ && getNestedListElementType(listVal))
            codegenError("distinct() is not supported for lists of non-string pointer elements");

        llvm::Value *srcLenPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 0, "dist_src_len_ptr");
        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, srcLenPtr, "dist_src_len");
        llvm::Value *srcDataPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 2, "dist_src_data_field");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, srcDataPtr, "dist_src_data");

        // Allocate new list (capacity = source length)
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);

        llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "dist_header");
        llvm::Value *dataSize = builder_.CreateMul(srcLen, llvm::ConstantInt::get(i64Ty_, elemSize), "dist_data_size");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "dist_data");

        builder_.CreateStore(newData, builder_.CreateStructGEP(listHeaderTy_, newHeader, 2, "dist_data_field"));
        builder_.CreateStore(srcLen, builder_.CreateStructGEP(listHeaderTy_, newHeader, 1, "dist_cap_ptr"));

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
        builder_.CreateCondBr(builder_.CreateICmpSLT(iVal, srcLen), outerBodyBB, outerEndBB);

        builder_.SetInsertPoint(outerBodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "dist_ic");
        llvm::Value *srcElemPtr = builder_.CreateGEP(elemTy, srcData, {iCur}, "dist_src_ep");
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

    // ===== flatten(list) → flatten nested list one level =====
    if (e.callee == "flatten" && e.args.size() == 1) {
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

        llvm::Value *outerLenPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 0, "flat_olen_ptr");
        llvm::Value *outerLen = builder_.CreateLoad(i64Ty_, outerLenPtr, "flat_olen");
        llvm::Value *outerDataField = builder_.CreateStructGEP(listHeaderTy_, listVal, 2, "flat_odata_field");
        llvm::Value *outerData = builder_.CreateLoad(ptrTy_, outerDataField, "flat_odata");

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

    // ===== items(map) → List<(K, V)> =====
    if (e.callee == "items" && e.args.size() == 1) {
        llvm::Value *mapPtr = emitExpr(*e.args[0]);
        llvm::Type *keyTy = getMapKeyType(mapPtr);
        llvm::Type *valTy = getMapValueType(mapPtr);
        if (keyTy && valTy) {
            llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, mapPtr, 0, "items_len_ptr");
            llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "items_len");
            llvm::Value *keysPtrField = builder_.CreateStructGEP(mapHeaderTy_, mapPtr, 2, "items_keys_field");
            llvm::Value *keysPtr = builder_.CreateLoad(ptrTy_, keysPtrField, "items_keys");
            llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, mapPtr, 3, "items_vals_field");
            llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "items_vals");

            llvm::StructType *tupleTy = llvm::StructType::get(*ctx_, {keyTy, valTy});
            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
            uint64_t tupleSize = dl.getTypeAllocSize(tupleTy);

            auto mallocFn = getStdlibMalloc();

            llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "items_hdr");
            llvm::Value *dataSize = builder_.CreateMul(len, llvm::ConstantInt::get(i64Ty_, tupleSize), "items_ds");
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
            builder_.CreateCondBr(builder_.CreateICmpSLT(i, len), bodyBB, endBB);
            builder_.SetInsertPoint(bodyBB);
            llvm::Value *kp = builder_.CreateGEP(keyTy, keysPtr, {i}, "items_kp");
            llvm::Value *vp = builder_.CreateGEP(valTy, valsPtr, {i}, "items_vp");
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

            builder_.CreateStore(len, builder_.CreateStructGEP(listHeaderTy_, newHeader, 0));
            builder_.CreateStore(len, builder_.CreateStructGEP(listHeaderTy_, newHeader, 1));
            builder_.CreateStore(newData, builder_.CreateStructGEP(listHeaderTy_, newHeader, 2));
            list_element_types_[newHeader] = tupleTy;
            return newHeader;
        }
    }

    // ===== get(map, key) — 2-arg → Option<V> =====
    if (e.callee == "get" && e.args.size() == 2) {
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

    // ===== get(map, key, default) — 3-arg =====
    if (e.callee == "get" && e.args.size() == 3) {
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

    // ===== merge(map1, map2) → new map =====
    if (e.callee == "merge" && e.args.size() == 2) {
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

            llvm::Value *len1 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(mapHeaderTy_, map1, 0), "mg_len1");
            llvm::Value *len2 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(mapHeaderTy_, map2, 0), "mg_len2");
            llvm::Value *keys1 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, map1, 2), "mg_keys1");
            llvm::Value *vals1 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, map1, 3), "mg_vals1");
            llvm::Value *keys2 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, map2, 2), "mg_keys2");
            llvm::Value *vals2 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(mapHeaderTy_, map2, 3), "mg_vals2");

            // Allocate new map with capacity = len1 + len2
            llvm::Value *maxCap = builder_.CreateAdd(len1, len2, "mg_max_cap");
            llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "mg_hdr");
            llvm::Value *newKeysSize = builder_.CreateMul(maxCap, llvm::ConstantInt::get(i64Ty_, keySize), "mg_ks");
            llvm::Value *newKeys = builder_.CreateCall(mallocFn, {newKeysSize}, "mg_keys");
            llvm::Value *newValsSize = builder_.CreateMul(maxCap, llvm::ConstantInt::get(i64Ty_, valSize), "mg_vs");
            llvm::Value *newVals = builder_.CreateCall(mallocFn, {newValsSize}, "mg_vals");

            // Copy all of map1
            llvm::Value *copy1KeySize = builder_.CreateMul(len1, llvm::ConstantInt::get(i64Ty_, keySize), "mg_ck1");
            builder_.CreateCall(memcpyFn, {newKeys, keys1, copy1KeySize});
            llvm::Value *copy1ValSize = builder_.CreateMul(len1, llvm::ConstantInt::get(i64Ty_, valSize), "mg_cv1");
            builder_.CreateCall(memcpyFn, {newVals, vals1, copy1ValSize});

            // Set up header
            llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, newHeader, 0, "mg_len_ptr");
            builder_.CreateStore(len1, lenPtr);
            builder_.CreateStore(maxCap, builder_.CreateStructGEP(mapHeaderTy_, newHeader, 1));
            builder_.CreateStore(newKeys, builder_.CreateStructGEP(mapHeaderTy_, newHeader, 2));
            builder_.CreateStore(newVals, builder_.CreateStructGEP(mapHeaderTy_, newHeader, 3));

            // Init hash buckets
            emitBucketInit(newHeader, mapHeaderTy_, 4, 5, 16);

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
                builder_.CreateCondBr(builder_.CreateICmpSLT(ri, len1), rBodyBB, rEndBB);
                builder_.SetInsertPoint(rBodyBB);
                llvm::Value *kp = builder_.CreateGEP(keyTy, newKeys, {ri}, "mg_rh_kp");
                llvm::Value *kv = builder_.CreateLoad(keyTy, kp, "mg_rh_kv");
                emitBucketInsertAndRehashCheck(newHeader, mapHeaderTy_, 0, 4, 5, kv, keyTy, ri);
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
                builder_.CreateCondBr(builder_.CreateICmpSLT(i, len2), bodyBB, endBB);

                builder_.SetInsertPoint(bodyBB);
                llvm::Value *kp = builder_.CreateGEP(keyTy, keys2, {i}, "mg_kp2");
                llvm::Value *kv = builder_.CreateLoad(keyTy, kp, "mg_kv2");
                llvm::Value *vp = builder_.CreateGEP(valTy, vals2, {i}, "mg_vp2");
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
                emitBucketInsertAndRehashCheck(newHeader, mapHeaderTy_, 0, 4, 5, kv, keyTy, curLen);
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
    }


    return nullptr;
}

// ===== Builtin Set Ops =====

llvm::Value *CodeGen::emitBuiltinSetOps(const CallExpr &e) {
    // ===== Set operations =====

    // Helper lambda to create a new set from iteration
    // union(set1, set2)
    if (e.callee == "union" && e.args.size() == 2) {
        llvm::Value *set1 = emitExpr(*e.args[0]);
        llvm::Value *set2 = emitExpr(*e.args[1]);
        llvm::Type *elemTy = getSetElementType(set1);
        if (elemTy) {
            llvm::Type *elemTy2 = getSetElementType(set2);
            if (!elemTy2 || elemTy2 != elemTy)
                codegenError("union() requires two sets with the same element type");
            // Create new set with all elements from set1, then add elements from set2
            llvm::Value *len1 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(setHeaderTy_, set1, 0), "u_len1");
            llvm::Value *len2 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(setHeaderTy_, set2, 0), "u_len2");
            llvm::Value *data1 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(setHeaderTy_, set1, 2), "u_data1");
            llvm::Value *data2 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(setHeaderTy_, set2, 2), "u_data2");

            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t headerSize = dl.getTypeAllocSize(setHeaderTy_);
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);
            auto mallocFn = getStdlibMalloc();

            // Allocate max possible size (len1 + len2)
            llvm::Value *maxLen = builder_.CreateAdd(len1, len2, "u_max_len");
            llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "u_hdr");
            llvm::Value *dataSize = builder_.CreateMul(maxLen, llvm::ConstantInt::get(i64Ty_, elemSize), "u_ds");
            llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "u_data");

            // Copy all of set1
            auto memcpyFn = getStdlibMemcpy();
            llvm::Value *copy1Size = builder_.CreateMul(len1, llvm::ConstantInt::get(i64Ty_, elemSize), "u_copy1_size");
            builder_.CreateCall(memcpyFn, {newData, data1, copy1Size});

            // Init header with len1
            llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, newHeader, 0, "u_len_ptr");
            builder_.CreateStore(len1, lenPtr);
            builder_.CreateStore(maxLen, builder_.CreateStructGEP(setHeaderTy_, newHeader, 1));
            builder_.CreateStore(newData, builder_.CreateStructGEP(setHeaderTy_, newHeader, 2));

            // Init buckets for the new set
            emitBucketInit(newHeader, setHeaderTy_, 3, 4, 16);

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
                builder_.CreateCondBr(builder_.CreateICmpSLT(ri, len1), rBodyBB, rEndBB);
                builder_.SetInsertPoint(rBodyBB);
                llvm::Value *ep = builder_.CreateGEP(elemTy, newData, {ri}, "u_rehash_ep");
                llvm::Value *ev = builder_.CreateLoad(elemTy, ep, "u_rehash_ev");
                emitBucketInsertAndRehashCheck(newHeader, setHeaderTy_, 0, 3, 4, ev, elemTy, ri);
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
                builder_.CreateCondBr(builder_.CreateICmpSLT(i, len2), bodyBB, endBB);
                builder_.SetInsertPoint(bodyBB);
                llvm::Value *ep = builder_.CreateGEP(elemTy, data2, {i}, "u_ep2");
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
                emitBucketInsertAndRehashCheck(newHeader, setHeaderTy_, 0, 3, 4, ev, elemTy, curLen);
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
    }

    // intersection(set1, set2)
    if (e.callee == "intersection" && e.args.size() == 2) {
        llvm::Value *set1 = emitExpr(*e.args[0]);
        llvm::Value *set2 = emitExpr(*e.args[1]);
        llvm::Type *elemTy = getSetElementType(set1);
        if (elemTy) {
            llvm::Type *elemTy2 = getSetElementType(set2);
            if (!elemTy2 || elemTy2 != elemTy)
                codegenError("intersection() requires two sets with the same element type");
            llvm::Value *len1 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(setHeaderTy_, set1, 0), "is_len1");
            llvm::Value *data1 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(setHeaderTy_, set1, 2), "is_data1");

            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t headerSize = dl.getTypeAllocSize(setHeaderTy_);
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);
            auto mallocFn = getStdlibMalloc();

            llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "is_hdr");
            llvm::Value *dataSize = builder_.CreateMul(len1, llvm::ConstantInt::get(i64Ty_, elemSize), "is_ds");
            llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "is_data");

            llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, newHeader, 0, "is_len_ptr");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), lenPtr);
            builder_.CreateStore(len1, builder_.CreateStructGEP(setHeaderTy_, newHeader, 1));
            builder_.CreateStore(newData, builder_.CreateStructGEP(setHeaderTy_, newHeader, 2));
            emitBucketInit(newHeader, setHeaderTy_, 3, 4, 16);

            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "is_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "is.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "is.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "is.end", fn_);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(condBB);
            llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "is_ci");
            builder_.CreateCondBr(builder_.CreateICmpSLT(i, len1), bodyBB, endBB);
            builder_.SetInsertPoint(bodyBB);
            llvm::Value *ep = builder_.CreateGEP(elemTy, data1, {i}, "is_ep");
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
            emitBucketInsertAndRehashCheck(newHeader, setHeaderTy_, 0, 3, 4, ev, elemTy, curLen);
            builder_.CreateStore(builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1)), lenPtr);
            builder_.CreateBr(nextBB);

            builder_.SetInsertPoint(nextBB);
            builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(endBB);

            set_element_types_[newHeader] = elemTy;
            return newHeader;
        }
    }

    // difference(set1, set2) — elements in set1 not in set2
    if (e.callee == "difference" && e.args.size() == 2) {
        llvm::Value *set1 = emitExpr(*e.args[0]);
        llvm::Value *set2 = emitExpr(*e.args[1]);
        llvm::Type *elemTy = getSetElementType(set1);
        if (elemTy) {
            llvm::Type *elemTy2 = getSetElementType(set2);
            if (!elemTy2 || elemTy2 != elemTy)
                codegenError("difference() requires two sets with the same element type");
            llvm::Value *len1 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(setHeaderTy_, set1, 0), "df_len1");
            llvm::Value *data1 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(setHeaderTy_, set1, 2), "df_data1");

            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t headerSize = dl.getTypeAllocSize(setHeaderTy_);
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);
            auto mallocFn = getStdlibMalloc();

            llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "df_hdr");
            llvm::Value *dataSize = builder_.CreateMul(len1, llvm::ConstantInt::get(i64Ty_, elemSize), "df_ds");
            llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "df_data");

            llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, newHeader, 0, "df_len_ptr");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), lenPtr);
            builder_.CreateStore(len1, builder_.CreateStructGEP(setHeaderTy_, newHeader, 1));
            builder_.CreateStore(newData, builder_.CreateStructGEP(setHeaderTy_, newHeader, 2));
            emitBucketInit(newHeader, setHeaderTy_, 3, 4, 16);

            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "df_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "df.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "df.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "df.end", fn_);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(condBB);
            llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "df_ci");
            builder_.CreateCondBr(builder_.CreateICmpSLT(i, len1), bodyBB, endBB);
            builder_.SetInsertPoint(bodyBB);
            llvm::Value *ep = builder_.CreateGEP(elemTy, data1, {i}, "df_ep");
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
            emitBucketInsertAndRehashCheck(newHeader, setHeaderTy_, 0, 3, 4, ev, elemTy, curLen);
            builder_.CreateStore(builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1)), lenPtr);
            builder_.CreateBr(nextBB);

            builder_.SetInsertPoint(nextBB);
            builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(endBB);

            set_element_types_[newHeader] = elemTy;
            return newHeader;
        }
    }

    // symmetric_difference(set1, set2)
    if (e.callee == "symmetric_difference" && e.args.size() == 2) {
        llvm::Value *set1 = emitExpr(*e.args[0]);
        llvm::Value *set2 = emitExpr(*e.args[1]);
        llvm::Type *elemTy = getSetElementType(set1);
        if (elemTy) {
            llvm::Type *elemTy2 = getSetElementType(set2);
            if (!elemTy2 || elemTy2 != elemTy)
                codegenError("symmetric_difference() requires two sets with the same element type");
            llvm::Value *len1 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(setHeaderTy_, set1, 0), "sd_len1");
            llvm::Value *len2 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(setHeaderTy_, set2, 0), "sd_len2");
            llvm::Value *data1 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(setHeaderTy_, set1, 2), "sd_data1");
            llvm::Value *data2 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(setHeaderTy_, set2, 2), "sd_data2");

            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t headerSize = dl.getTypeAllocSize(setHeaderTy_);
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);
            auto mallocFn = getStdlibMalloc();

            llvm::Value *maxLen = builder_.CreateAdd(len1, len2, "sd_max_len");
            llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "sd_hdr");
            llvm::Value *dataSize = builder_.CreateMul(maxLen, llvm::ConstantInt::get(i64Ty_, elemSize), "sd_ds");
            llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "sd_data");

            llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, newHeader, 0, "sd_len_ptr");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), lenPtr);
            builder_.CreateStore(maxLen, builder_.CreateStructGEP(setHeaderTy_, newHeader, 1));
            builder_.CreateStore(newData, builder_.CreateStructGEP(setHeaderTy_, newHeader, 2));
            emitBucketInit(newHeader, setHeaderTy_, 3, 4, 16);

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
                emitBucketInsertAndRehashCheck(newHeader, setHeaderTy_, 0, 3, 4, eVal, elemTy, curLen);
                builder_.CreateStore(builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1)), lenPtr);
                builder_.CreateBr(nBB);
                builder_.SetInsertPoint(nBB);
                builder_.CreateStore(builder_.CreateAdd(ci, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
                builder_.CreateBr(cBB);
                builder_.SetInsertPoint(eBB);
            };

            emitSetDiffLoop(data1, len1, set2, "sd1");
            emitSetDiffLoop(data2, len2, set1, "sd2");

            set_element_types_[newHeader] = elemTy;
            return newHeader;
        }
    }

    // is_subset(set1, set2)
    if (e.callee == "is_subset" && e.args.size() == 2) {
        llvm::Value *set1 = emitExpr(*e.args[0]);
        llvm::Value *set2 = emitExpr(*e.args[1]);
        llvm::Type *elemTy = getSetElementType(set1);
        if (elemTy) {
            llvm::Type *elemTy2 = getSetElementType(set2);
            if (!elemTy2 || elemTy2 != elemTy)
                codegenError("is_subset() requires two sets with the same element type");
            llvm::Value *len1 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(setHeaderTy_, set1, 0), "sub_len1");
            llvm::Value *data1 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(setHeaderTy_, set1, 2), "sub_data1");

            llvm::AllocaInst *resultVar = builder_.CreateAlloca(i1Ty_, nullptr, "sub_result");
            builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 1), resultVar);

            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "sub_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "sub.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "sub.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "sub.end", fn_);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(condBB);
            llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "sub_ci");
            builder_.CreateCondBr(builder_.CreateICmpSLT(i, len1), bodyBB, endBB);
            builder_.SetInsertPoint(bodyBB);
            llvm::Value *ep = builder_.CreateGEP(elemTy, data1, {i}, "sub_ep");
            llvm::Value *ev = builder_.CreateLoad(elemTy, ep, "sub_ev");
            llvm::Value *inSet2 = emitSetElementLookup(set2, ev, elemTy);
            llvm::Value *notFound = builder_.CreateICmpSLT(inSet2, llvm::ConstantInt::get(i64Ty_, 0), "sub_nf");
            llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "sub.fail", fn_);
            llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "sub.next", fn_);
            builder_.CreateCondBr(notFound, failBB, nextBB);
            builder_.SetInsertPoint(failBB);
            builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 0), resultVar);
            builder_.CreateBr(endBB);  // early exit
            builder_.SetInsertPoint(nextBB);
            builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(endBB);

            return builder_.CreateLoad(i1Ty_, resultVar, "is_subset_result");
        }
    }

    // is_superset(set1, set2) = is_subset(set2, set1)
    if (e.callee == "is_superset" && e.args.size() == 2) {
        llvm::Value *set1 = emitExpr(*e.args[0]);
        llvm::Value *set2 = emitExpr(*e.args[1]);
        llvm::Type *elemTy = getSetElementType(set1);
        if (elemTy) {
            llvm::Type *elemTy2 = getSetElementType(set2);
            if (!elemTy2 || elemTy2 != elemTy)
                codegenError("is_superset() requires two sets with the same element type");
            // Check all elements of set2 are in set1
            llvm::Value *len2 = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(setHeaderTy_, set2, 0), "sup_len2");
            llvm::Value *data2 = builder_.CreateLoad(ptrTy_, builder_.CreateStructGEP(setHeaderTy_, set2, 2), "sup_data2");

            llvm::AllocaInst *resultVar = builder_.CreateAlloca(i1Ty_, nullptr, "sup_result");
            builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 1), resultVar);

            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "sup_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "sup.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "sup.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "sup.end", fn_);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(condBB);
            llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "sup_ci");
            builder_.CreateCondBr(builder_.CreateICmpSLT(i, len2), bodyBB, endBB);
            builder_.SetInsertPoint(bodyBB);
            llvm::Value *ep = builder_.CreateGEP(elemTy, data2, {i}, "sup_ep");
            llvm::Value *ev = builder_.CreateLoad(elemTy, ep, "sup_ev");
            llvm::Value *inSet1 = emitSetElementLookup(set1, ev, elemTy);
            llvm::Value *notFound = builder_.CreateICmpSLT(inSet1, llvm::ConstantInt::get(i64Ty_, 0), "sup_nf");
            llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "sup.fail", fn_);
            llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "sup.next", fn_);
            builder_.CreateCondBr(notFound, failBB, nextBB);
            builder_.SetInsertPoint(failBB);
            builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 0), resultVar);
            builder_.CreateBr(endBB);
            builder_.SetInsertPoint(nextBB);
            builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(endBB);

            return builder_.CreateLoad(i1Ty_, resultVar, "is_superset_result");
        }
    }

    return nullptr;
}
