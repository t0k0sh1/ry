#include "ry/codegen.hpp"
#include <stdexcept>

// ===== CallExpr =====

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<CallExpr> &e) {
    // range(n), range(start, end), or range(start, end, step) → List<int>
    if (e->callee == "range") {
        if (e->args.size() < 1 || e->args.size() > 3)
            throw std::runtime_error("range() takes 1, 2, or 3 arguments");

        llvm::Value *start, *end, *step;
        llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
        llvm::Value *one = llvm::ConstantInt::get(i64Ty_, 1);

        if (e->args.size() == 1) {
            start = zero;
            end = emitExpr(*e->args[0]);
            step = one;
        } else if (e->args.size() == 2) {
            start = emitExpr(*e->args[0]);
            end = emitExpr(*e->args[1]);
            step = one;
        } else {
            start = emitExpr(*e->args[0]);
            end = emitExpr(*e->args[1]);
            step = emitExpr(*e->args[2]);
        }

        // Runtime check: step == 0 → error
        if (e->args.size() == 3) {
            llvm::Value *stepZero = builder_.CreateICmpEQ(step, zero, "step_zero");
            llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "range.step_err", fn_);
            llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "range.step_ok", fn_);
            builder_.CreateCondBr(stepZero, errBB, okBB);
            builder_.SetInsertPoint(errBB);
            emitRuntimeError("runtime error: range() step must not be zero\n", ".range_step_err");
            builder_.SetInsertPoint(okBB);
        }

        // Compute count based on step sign
        // step > 0: count = max(0, (end - start + step - 1) / step)
        // step < 0: count = max(0, (start - end + (-step) - 1) / (-step))
        llvm::Value *stepPos = builder_.CreateICmpSGT(step, zero, "step_pos");

        // Positive step case
        llvm::Value *diffPos = builder_.CreateSub(end, start, "diff_pos");
        llvm::Value *numPos = builder_.CreateAdd(diffPos, builder_.CreateSub(step, one, "step_m1"), "num_pos");
        llvm::Value *countPos = builder_.CreateSDiv(numPos, step, "count_pos");
        llvm::Value *countPosClamped = builder_.CreateSelect(
            builder_.CreateICmpSGT(countPos, zero, "pos_gt0"), countPos, zero, "count_pos_c");

        // Negative step case
        llvm::Value *negStep = builder_.CreateNeg(step, "neg_step");
        llvm::Value *diffNeg = builder_.CreateSub(start, end, "diff_neg");
        llvm::Value *numNeg = builder_.CreateAdd(diffNeg, builder_.CreateSub(negStep, one, "negstep_m1"), "num_neg");
        llvm::Value *countNeg = builder_.CreateSDiv(numNeg, negStep, "count_neg");
        llvm::Value *countNegClamped = builder_.CreateSelect(
            builder_.CreateICmpSGT(countNeg, zero, "neg_gt0"), countNeg, zero, "count_neg_c");

        llvm::Value *count = builder_.CreateSelect(stepPos, countPosClamped, countNegClamped, "range_count");

        // Allocate list header
        llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *headerPtr = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "range_header");

        // Allocate data array
        uint64_t elemSize = dl.getTypeAllocSize(i64Ty_);
        llvm::Value *dataSize = builder_.CreateMul(count, llvm::ConstantInt::get(i64Ty_, elemSize), "range_data_size");
        llvm::Value *dataPtr = builder_.CreateCall(mallocFn, {dataSize}, "range_data");

        // Fill data with start, start+step, start+2*step, ... using a loop
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "range_i");
        builder_.CreateStore(zero, iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "range.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "range.body", fn_);
        llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, "range.end", fn_);

        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "ri");
        llvm::Value *cond = builder_.CreateICmpSLT(iVal, count, "range_cond");
        builder_.CreateCondBr(cond, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "ri_cur");
        llvm::Value *offset = builder_.CreateMul(iCur, step, "range_offset");
        llvm::Value *val = builder_.CreateAdd(start, offset, "range_val");
        llvm::Value *elemPtr = builder_.CreateGEP(i64Ty_, dataPtr, {iCur}, "range_elem_ptr");
        builder_.CreateStore(val, elemPtr);
        llvm::Value *iNext = builder_.CreateAdd(iCur, one, "ri_next");
        builder_.CreateStore(iNext, iVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);

        // Store header fields
        llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 0, "range_len_ptr");
        builder_.CreateStore(count, lenPtr);
        llvm::Value *capPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 1, "range_cap_ptr");
        builder_.CreateStore(count, capPtr);
        llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 2, "range_data_field");
        builder_.CreateStore(dataPtr, dataPtrField);

        list_element_types_[headerPtr] = i64Ty_;
        return headerPtr;
    }

    // len(xs) → list/map length
    if (e->callee == "len") {
        if (e->args.size() != 1)
            throw std::runtime_error("len() takes exactly 1 argument");
        llvm::Value *ptr = emitExpr(*e->args[0]);
        if (ptr->getType() != ptrTy_)
            throw std::runtime_error("len() requires list, map, or str argument");
        // Check if it's a set
        if (getSetElementType(ptr)) {
            llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, ptr, 0, "set_len_ptr");
            return builder_.CreateLoad(i64Ty_, lenPtr, "set_len");
        }
        // Check if it's a map
        llvm::Type *mapKeyTy = getMapKeyType(ptr);
        if (mapKeyTy) {
            llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, ptr, 0, "map_len_ptr");
            return builder_.CreateLoad(i64Ty_, lenPtr, "map_len");
        }
        // Check if it's a list
        if (getListElementType(ptr)) {
            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, ptr, 0, "len_ptr");
            return builder_.CreateLoad(i64Ty_, lenPtr, "len");
        }
        // String: call strlen
        auto strlenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto strlenFn = mod_->getOrInsertFunction("strlen", strlenTy);
        return builder_.CreateCall(strlenFn, {ptr}, "str_len");
    }

    // Some(x) → Option<T> constructor
    if (e->callee == "Some") {
        if (e->args.size() != 1)
            throw std::runtime_error("Some() takes exactly 1 argument");
        llvm::Value *inner = emitExpr(*e->args[0]);
        llvm::StructType *optTy = getOptionType(inner->getType());
        llvm::Value *result = llvm::UndefValue::get(optTy);
        result = builder_.CreateInsertValue(result, llvm::ConstantInt::get(i1Ty_, 1), 0);
        result = builder_.CreateInsertValue(result, inner, 1);
        return result;
    }

    // Ok(x) / Err(x) → Result<T, E> constructor
    if (e->callee == "Ok" || e->callee == "Err") {
        if (e->args.size() != 1)
            throw std::runtime_error(e->callee + "() takes exactly 1 argument");
        llvm::Value *inner = emitExpr(*e->args[0]);
        uint64_t tag = (e->callee == "Ok") ? 0 : 1;
        std::string prefix = (e->callee == "Ok") ? "ok" : "err";

        // Try to use the enclosing function's return type for correct Result sizing
        llvm::StructType *resultTy = nullptr;
        if (fn_) {
            llvm::Type *retTy = fn_->getReturnType();
            if (isResultType(retTy))
                resultTy = llvm::cast<llvm::StructType>(retTy);
        }
        if (!resultTy) {
            // Fallback: size from inner value (works when T and E have same size)
            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t innerSize = dl.getTypeAllocSize(inner->getType());
            if (innerSize < 8) innerSize = 8;
            auto *dataTy = llvm::ArrayType::get(i8Ty_, innerSize);
            resultTy = llvm::StructType::get(*ctx_, {i64Ty_, dataTy});
        }

        llvm::Value *alloca = builder_.CreateAlloca(resultTy, nullptr, prefix + "_tmp");
        llvm::Value *tagPtr = builder_.CreateStructGEP(resultTy, alloca, 0, prefix + "_tag_ptr");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, tag), tagPtr);
        llvm::Value *dataPtr = builder_.CreateStructGEP(resultTy, alloca, 1, prefix + "_data_ptr");
        builder_.CreateStore(inner, dataPtr);
        return builder_.CreateLoad(resultTy, alloca, prefix + "_val");
    }

    // unwrap(opt) → extract value or runtime error
    if (e->callee == "unwrap") {
        if (e->args.size() != 1)
            throw std::runtime_error("unwrap() takes exactly 1 argument");
        llvm::Value *opt = emitExpr(*e->args[0]);
        if (!isOptionType(opt->getType()))
            throw std::runtime_error("unwrap() requires Option type argument");

        llvm::Value *hasValue = builder_.CreateExtractValue(opt, 0, "has_value");

        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "unwrap.ok", fn_);
        llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "unwrap.fail", fn_);

        builder_.CreateCondBr(hasValue, okBB, failBB);

        // fail: print error and exit
        builder_.SetInsertPoint(failBB);
        emitRuntimeError("runtime error: unwrap() called on None\n", ".unwrap_err");

        // ok: extract value
        builder_.SetInsertPoint(okBB);
        return builder_.CreateExtractValue(opt, 1, "unwrap_val");
    }

    // has_key(map, key) → bool
    if (e->callee == "has_key") {
        if (e->args.size() != 2)
            throw std::runtime_error("has_key() takes exactly 2 arguments");
        llvm::Value *mapPtr = emitExpr(*e->args[0]);
        if (mapPtr->getType() != ptrTy_)
            throw std::runtime_error("has_key() requires map as first argument");
        llvm::Type *keyTy = getMapKeyType(mapPtr);
        if (!keyTy)
            throw std::runtime_error("has_key() requires map as first argument");
        llvm::Value *key = emitExpr(*e->args[1]);
        if (key->getType() != keyTy)
            throw std::runtime_error("has_key() key type mismatch");
        llvm::Value *idx = emitMapKeyLookup(mapPtr, key, keyTy);
        return builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "has_key");
    }

    // add(set, val) → add element to set (no-op if already present)
    // Only intercept if first arg is a set (fall through to user function otherwise)
    if (e->callee == "add" && e->args.size() == 2) {
        llvm::Value *setPtr = emitExpr(*e->args[0]);
        llvm::Type *elemTy = getSetElementType(setPtr);
        if (elemTy) {
            llvm::Value *elem = emitExpr(*e->args[1]);
            if (elem->getType() != elemTy)
                throw std::runtime_error("add() element type mismatch");

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
            llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
            llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
            llvm::Value *newSize = builder_.CreateMul(newCap, llvm::ConstantInt::get(i64Ty_, elemSize), "new_size");
            llvm::Value *newElemsPtr = builder_.CreateCall(mallocFn, {newSize}, "new_elems");

            llvm::FunctionType *memcpyTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
            llvm::FunctionCallee memcpyFn = mod_->getOrInsertFunction("memcpy", memcpyTy);
            llvm::Value *elemsPtrField = builder_.CreateStructGEP(setHeaderTy_, setPtr, 2, "elems_field");
            llvm::Value *oldElemsPtr = builder_.CreateLoad(ptrTy_, elemsPtrField, "old_elems");
            llvm::Value *oldSize = builder_.CreateMul(length, llvm::ConstantInt::get(i64Ty_, elemSize), "old_size");
            builder_.CreateCall(memcpyFn, {newElemsPtr, oldElemsPtr, oldSize});

            llvm::FunctionType *freeTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
            llvm::FunctionCallee freeFn = mod_->getOrInsertFunction("free", freeTy);
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
    if (e->callee == "remove" && e->args.size() == 2) {
        llvm::Value *setPtr = emitExpr(*e->args[0]);
        llvm::Type *elemTy = getSetElementType(setPtr);
        if (elemTy) {
            llvm::Value *elem = emitExpr(*e->args[1]);
            if (elem->getType() != elemTy)
                throw std::runtime_error("remove() element type mismatch");

            llvm::Value *idx = emitSetElementLookup(setPtr, elem, elemTy);
            llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "found");

            llvm::BasicBlock *removeBB = llvm::BasicBlock::Create(*ctx_, "set.remove", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "set.remove_end", fn_);
            builder_.CreateCondBr(found, removeBB, endBB);

            builder_.SetInsertPoint(removeBB);
            llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, setPtr, 0, "set_len_ptr");
            llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "set_len");
            llvm::Value *elemsPtrField = builder_.CreateStructGEP(setHeaderTy_, setPtr, 2, "set_elems_ptr");
            llvm::Value *elemsPtr = builder_.CreateLoad(ptrTy_, elemsPtrField, "set_elems");

            // Remove from bucket: set tombstone for this element
            {
                std::string hashFnName;
                llvm::Type *hashArgTy;
                if (elemTy == ptrTy_) {
                    hashFnName = "__ry_hash_str";
                    hashArgTy = ptrTy_;
                } else if (elemTy->isDoubleTy()) {
                    hashFnName = "__ry_hash_f64";
                    hashArgTy = f64Ty_;
                } else {
                    hashFnName = "__ry_hash_i64";
                    hashArgTy = i64Ty_;
                }
                llvm::FunctionType *hashTy = llvm::FunctionType::get(i64Ty_, {hashArgTy}, false);
                llvm::FunctionCallee hashFn = mod_->getOrInsertFunction(hashFnName, hashTy);
                llvm::Value *hashVal = builder_.CreateCall(hashFn, {elem}, "rm_hash");

                llvm::Value *bucketsField = builder_.CreateStructGEP(setHeaderTy_, setPtr, 4, "rm_bp");
                llvm::Value *bucketsPtr = builder_.CreateLoad(ptrTy_, bucketsField, "rm_buckets");
                llvm::Value *bcField = builder_.CreateStructGEP(setHeaderTy_, setPtr, 3, "rm_bc_field");
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
                std::string hashFnName;
                llvm::Type *hashArgTy;
                if (elemTy == ptrTy_) {
                    hashFnName = "__ry_hash_str";
                    hashArgTy = ptrTy_;
                } else if (elemTy->isDoubleTy()) {
                    hashFnName = "__ry_hash_f64";
                    hashArgTy = f64Ty_;
                } else {
                    hashFnName = "__ry_hash_i64";
                    hashArgTy = i64Ty_;
                }
                llvm::FunctionType *hashTy = llvm::FunctionType::get(i64Ty_, {hashArgTy}, false);
                llvm::FunctionCallee hashFn = mod_->getOrInsertFunction(hashFnName, hashTy);
                llvm::Value *lastHash = builder_.CreateCall(hashFn, {lastVal}, "last_hash");

                llvm::Value *bucketsField = builder_.CreateStructGEP(setHeaderTy_, setPtr, 4, "swap_bp");
                llvm::Value *bucketsPtr = builder_.CreateLoad(ptrTy_, bucketsField, "swap_buckets");
                llvm::Value *bcField = builder_.CreateStructGEP(setHeaderTy_, setPtr, 3, "swap_bc_field");
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
        // Not a set — fall through to user function resolution
    }

    // append(list, val) → mutating append
    if (e->callee == "append" && e->args.size() == 2) {
        llvm::Value *listPtr = emitExpr(*e->args[0]);
        llvm::Type *elemTy = getListElementType(listPtr);
        if (elemTy) {
            llvm::Value *val = emitExpr(*e->args[1]);
            if (val->getType() != elemTy)
                throw std::runtime_error("append() element type mismatch");

            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);

            auto mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
            auto mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
            auto memcpyTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
            auto memcpyFn = mod_->getOrInsertFunction("memcpy", memcpyTy);
            auto freeTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
            auto freeFn = mod_->getOrInsertFunction("free", freeTy);

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

    // pop(list) → remove and return last element
    if (e->callee == "pop" && e->args.size() == 1) {
        llvm::Value *listPtr = emitExpr(*e->args[0]);
        llvm::Type *elemTy = getListElementType(listPtr);
        if (elemTy) {
            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, listPtr, 0, "pop_len_ptr");
            llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "pop_len");
            llvm::Value *dataField = builder_.CreateStructGEP(listHeaderTy_, listPtr, 2, "pop_data_field");
            llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataField, "pop_data");

            // Check empty
            llvm::Value *isEmpty = builder_.CreateICmpEQ(len, llvm::ConstantInt::get(i64Ty_, 0), "pop_empty");
            llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "pop.err", fn_);
            llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "pop.ok", fn_);
            builder_.CreateCondBr(isEmpty, errBB, okBB);

            builder_.SetInsertPoint(errBB);
            emitRuntimeError("runtime error: pop() on empty list\n", ".pop_empty_err");

            builder_.SetInsertPoint(okBB);
            llvm::Value *lastIdx = builder_.CreateSub(len, llvm::ConstantInt::get(i64Ty_, 1), "pop_last_idx");
            llvm::Value *elemPtr = builder_.CreateGEP(elemTy, dataPtr, lastIdx, "pop_elem_ptr");
            llvm::Value *val = builder_.CreateLoad(elemTy, elemPtr, "pop_val");
            builder_.CreateStore(lastIdx, lenPtr); // len - 1

            return val;
        }
    }

    // slice(list, start, end) → new sub-list
    if (e->callee == "slice" && e->args.size() == 3) {
        llvm::Value *listPtr = emitExpr(*e->args[0]);
        llvm::Type *elemTy = getListElementType(listPtr);
        if (elemTy) {
            llvm::Value *startVal = emitExpr(*e->args[1]);
            llvm::Value *endVal = emitExpr(*e->args[2]);

            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);

            auto mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
            auto mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
            auto memcpyTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
            auto memcpyFn = mod_->getOrInsertFunction("memcpy", memcpyTy);

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

    // contains(s, sub) → bool
    if (e->callee == "contains") {
        if (e->args.size() != 2)
            throw std::runtime_error("contains() takes exactly 2 arguments");
        llvm::Value *s = emitExpr(*e->args[0]);
        llvm::Value *sub = emitExpr(*e->args[1]);
        if (s->getType() != ptrTy_ || sub->getType() != ptrTy_)
            throw std::runtime_error("contains() requires str arguments");
        auto strstrTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
        auto strstrFn = mod_->getOrInsertFunction("strstr", strstrTy);
        llvm::Value *result = builder_.CreateCall(strstrFn, {s, sub}, "strstr");
        llvm::Value *null = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
        return builder_.CreateICmpNE(result, null, "contains");
    }

    // starts_with(s, prefix) → bool
    if (e->callee == "starts_with") {
        if (e->args.size() != 2)
            throw std::runtime_error("starts_with() takes exactly 2 arguments");
        llvm::Value *s = emitExpr(*e->args[0]);
        llvm::Value *prefix = emitExpr(*e->args[1]);
        if (s->getType() != ptrTy_ || prefix->getType() != ptrTy_)
            throw std::runtime_error("starts_with() requires str arguments");
        auto strlenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto strlenFn = mod_->getOrInsertFunction("strlen", strlenTy);
        auto strncmpTy = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_, i64Ty_}, false);
        auto strncmpFn = mod_->getOrInsertFunction("strncmp", strncmpTy);
        llvm::Value *prefixLen = builder_.CreateCall(strlenFn, {prefix}, "prefix_len");
        llvm::Value *cmp = builder_.CreateCall(strncmpFn, {s, prefix, prefixLen}, "strncmp");
        return builder_.CreateICmpEQ(cmp, llvm::ConstantInt::get(i32Ty_, 0), "starts_with");
    }

    // ends_with(s, suffix) → bool
    if (e->callee == "ends_with") {
        if (e->args.size() != 2)
            throw std::runtime_error("ends_with() takes exactly 2 arguments");
        llvm::Value *s = emitExpr(*e->args[0]);
        llvm::Value *suffix = emitExpr(*e->args[1]);
        if (s->getType() != ptrTy_ || suffix->getType() != ptrTy_)
            throw std::runtime_error("ends_with() requires str arguments");
        auto strlenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto strlenFn = mod_->getOrInsertFunction("strlen", strlenTy);
        auto strncmpTy = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_, i64Ty_}, false);
        auto strncmpFn = mod_->getOrInsertFunction("strncmp", strncmpTy);
        llvm::Value *sLen = builder_.CreateCall(strlenFn, {s}, "s_len");
        llvm::Value *suffixLen = builder_.CreateCall(strlenFn, {suffix}, "suffix_len");

        // if suffixLen > sLen, return false; else strncmp(s + offset, suffix, suffixLen) == 0
        llvm::Value *tooLong = builder_.CreateICmpUGT(suffixLen, sLen, "too_long");

        llvm::BasicBlock *checkBB = llvm::BasicBlock::Create(*ctx_, "ew.check", fn_);
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "ew.merge", fn_);
        llvm::BasicBlock *curBB = builder_.GetInsertBlock();

        builder_.CreateCondBr(tooLong, mergeBB, checkBB);

        // checkBB: compute strncmp
        builder_.SetInsertPoint(checkBB);
        llvm::Value *offset = builder_.CreateSub(sLen, suffixLen, "offset");
        llvm::Value *tailPtr = builder_.CreateGEP(builder_.getInt8Ty(), s, offset, "tail_ptr");
        llvm::Value *cmp = builder_.CreateCall(strncmpFn, {tailPtr, suffix, suffixLen}, "strncmp");
        llvm::Value *match = builder_.CreateICmpEQ(cmp, llvm::ConstantInt::get(i32Ty_, 0), "match");
        builder_.CreateBr(mergeBB);

        // mergeBB: PHI
        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = builder_.CreatePHI(i1Ty_, 2, "ends_with");
        phi->addIncoming(llvm::ConstantInt::get(i1Ty_, 0), curBB);
        phi->addIncoming(match, checkBB);
        return phi;
    }

    // ===== Phase 1: 基本文字列操作 =====

    // to_int(s) → int
    if (e->callee == "to_int") {
        if (e->args.size() != 1)
            throw std::runtime_error("to_int() takes exactly 1 argument");
        llvm::Value *s = emitExpr(*e->args[0]);
        if (s->getType() != ptrTy_)
            throw std::runtime_error("to_int() requires str argument");
        auto atolTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto atolFn = mod_->getOrInsertFunction("atol", atolTy);
        return builder_.CreateCall(atolFn, {s}, "to_int");
    }

    // to_float(s) → float
    if (e->callee == "to_float") {
        if (e->args.size() != 1)
            throw std::runtime_error("to_float() takes exactly 1 argument");
        llvm::Value *s = emitExpr(*e->args[0]);
        if (s->getType() != ptrTy_)
            throw std::runtime_error("to_float() requires str argument");
        auto atofTy = llvm::FunctionType::get(f64Ty_, {ptrTy_}, false);
        auto atofFn = mod_->getOrInsertFunction("atof", atofTy);
        return builder_.CreateCall(atofFn, {s}, "to_float");
    }

    // to_str(v) → str (int/float/bool/str → str)
    if (e->callee == "to_str") {
        if (e->args.size() != 1)
            throw std::runtime_error("to_str() takes exactly 1 argument");
        return valueToString(emitExpr(*e->args[0]));
    }

    // find(s, sub) → int (-1 if not found)
    if (e->callee == "find") {
        if (e->args.size() != 2)
            throw std::runtime_error("find() takes exactly 2 arguments");
        llvm::Value *s = emitExpr(*e->args[0]);
        llvm::Value *sub = emitExpr(*e->args[1]);
        if (s->getType() != ptrTy_ || sub->getType() != ptrTy_)
            throw std::runtime_error("find() requires str arguments");
        auto strstrTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
        auto strstrFn = mod_->getOrInsertFunction("strstr", strstrTy);
        llvm::Value *result = builder_.CreateCall(strstrFn, {s, sub}, "find_ptr");
        llvm::Value *null = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
        llvm::Value *isNull = builder_.CreateICmpEQ(result, null, "find_null");

        llvm::Value *sInt = builder_.CreatePtrToInt(s, i64Ty_, "s_int");
        llvm::Value *rInt = builder_.CreatePtrToInt(result, i64Ty_, "r_int");
        llvm::Value *offset = builder_.CreateSub(rInt, sInt, "find_offset");

        return builder_.CreateSelect(isNull, llvm::ConstantInt::get(i64Ty_, -1), offset, "find_result");
    }

    // substring(s, start, end) → str
    if (e->callee == "substring") {
        if (e->args.size() != 3)
            throw std::runtime_error("substring() takes exactly 3 arguments");
        llvm::Value *s = emitExpr(*e->args[0]);
        llvm::Value *start = emitExpr(*e->args[1]);
        llvm::Value *end = emitExpr(*e->args[2]);
        if (s->getType() != ptrTy_)
            throw std::runtime_error("substring() requires str as first argument");

        auto mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        auto mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
        auto memcpyTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
        auto memcpyFn = mod_->getOrInsertFunction("memcpy", memcpyTy);

        llvm::Value *len = builder_.CreateSub(end, start, "sub_len");
        llvm::Value *bufSize = builder_.CreateAdd(len, llvm::ConstantInt::get(i64Ty_, 1), "sub_buf_size");
        llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "sub_buf");
        llvm::Value *srcPtr = builder_.CreateGEP(builder_.getInt8Ty(), s, start, "sub_src");
        builder_.CreateCall(memcpyFn, {buf, srcPtr, len});
        // null terminate
        llvm::Value *endPtr = builder_.CreateGEP(builder_.getInt8Ty(), buf, len, "sub_end");
        builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), endPtr);
        return buf;
    }

    // char_at(s, i) → str (single character as string)
    if (e->callee == "char_at") {
        if (e->args.size() != 2)
            throw std::runtime_error("char_at() takes exactly 2 arguments");
        llvm::Value *s = emitExpr(*e->args[0]);
        llvm::Value *idx = emitExpr(*e->args[1]);
        if (s->getType() != ptrTy_)
            throw std::runtime_error("char_at() requires str as first argument");

        auto mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        auto mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);

        llvm::Value *buf = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, 2)}, "char_buf");
        llvm::Value *srcPtr = builder_.CreateGEP(builder_.getInt8Ty(), s, idx, "char_src");
        llvm::Value *ch = builder_.CreateLoad(i8Ty_, srcPtr, "char_val");
        builder_.CreateStore(ch, buf);
        llvm::Value *endPtr = builder_.CreateGEP(builder_.getInt8Ty(), buf, llvm::ConstantInt::get(i64Ty_, 1), "char_end");
        builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), endPtr);
        return buf;
    }

    // replace(s, old, new) → str
    if (e->callee == "replace") {
        if (e->args.size() != 3)
            throw std::runtime_error("replace() takes exactly 3 arguments");
        llvm::Value *s = emitExpr(*e->args[0]);
        llvm::Value *oldStr = emitExpr(*e->args[1]);
        llvm::Value *newStr = emitExpr(*e->args[2]);
        if (s->getType() != ptrTy_ || oldStr->getType() != ptrTy_ || newStr->getType() != ptrTy_)
            throw std::runtime_error("replace() requires str arguments");

        auto strlenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto strlenFn = mod_->getOrInsertFunction("strlen", strlenTy);
        auto strstrTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
        auto strstrFn = mod_->getOrInsertFunction("strstr", strstrTy);
        auto mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        auto mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
        auto memcpyTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
        auto memcpyFn = mod_->getOrInsertFunction("memcpy", memcpyTy);

        llvm::Value *sLen = builder_.CreateCall(strlenFn, {s}, "repl_s_len");
        llvm::Value *oldLen = builder_.CreateCall(strlenFn, {oldStr}, "repl_old_len");
        llvm::Value *newLen = builder_.CreateCall(strlenFn, {newStr}, "repl_new_len");

        // Pass 1: count occurrences
        llvm::AllocaInst *countVar = builder_.CreateAlloca(i64Ty_, nullptr, "repl_count");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), countVar);
        llvm::AllocaInst *searchVar = builder_.CreateAlloca(ptrTy_, nullptr, "repl_search");
        builder_.CreateStore(s, searchVar);

        llvm::BasicBlock *countCondBB = llvm::BasicBlock::Create(*ctx_, "repl.count_cond", fn_);
        llvm::BasicBlock *countBodyBB = llvm::BasicBlock::Create(*ctx_, "repl.count_body", fn_);
        llvm::BasicBlock *countEndBB = llvm::BasicBlock::Create(*ctx_, "repl.count_end", fn_);

        builder_.CreateBr(countCondBB);
        builder_.SetInsertPoint(countCondBB);
        llvm::Value *searchPtr = builder_.CreateLoad(ptrTy_, searchVar, "search_ptr");
        llvm::Value *found = builder_.CreateCall(strstrFn, {searchPtr, oldStr}, "found_ptr");
        llvm::Value *null = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
        llvm::Value *notNull = builder_.CreateICmpNE(found, null, "found_not_null");
        builder_.CreateCondBr(notNull, countBodyBB, countEndBB);

        builder_.SetInsertPoint(countBodyBB);
        llvm::Value *cnt = builder_.CreateLoad(i64Ty_, countVar, "cnt");
        builder_.CreateStore(builder_.CreateAdd(cnt, llvm::ConstantInt::get(i64Ty_, 1), "cnt_inc"), countVar);
        llvm::Value *nextSearch = builder_.CreateGEP(builder_.getInt8Ty(), found, oldLen, "next_search");
        builder_.CreateStore(nextSearch, searchVar);
        builder_.CreateBr(countCondBB);

        builder_.SetInsertPoint(countEndBB);
        llvm::Value *count = builder_.CreateLoad(i64Ty_, countVar, "final_count");

        // Calculate new length: sLen + count * (newLen - oldLen) + 1
        llvm::Value *diff = builder_.CreateSub(newLen, oldLen, "len_diff");
        llvm::Value *totalDiff = builder_.CreateMul(count, diff, "total_diff");
        llvm::Value *resultLen = builder_.CreateAdd(sLen, totalDiff, "result_len");
        llvm::Value *bufSize = builder_.CreateAdd(resultLen, llvm::ConstantInt::get(i64Ty_, 1), "buf_size");
        llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "repl_buf");

        // Pass 2: build result
        llvm::AllocaInst *srcVar = builder_.CreateAlloca(ptrTy_, nullptr, "repl_src");
        builder_.CreateStore(s, srcVar);
        llvm::AllocaInst *dstVar = builder_.CreateAlloca(ptrTy_, nullptr, "repl_dst");
        builder_.CreateStore(buf, dstVar);

        llvm::BasicBlock *buildCondBB = llvm::BasicBlock::Create(*ctx_, "repl.build_cond", fn_);
        llvm::BasicBlock *buildBodyBB = llvm::BasicBlock::Create(*ctx_, "repl.build_body", fn_);
        llvm::BasicBlock *buildEndBB = llvm::BasicBlock::Create(*ctx_, "repl.build_end", fn_);

        builder_.CreateBr(buildCondBB);
        builder_.SetInsertPoint(buildCondBB);
        llvm::Value *curSrc = builder_.CreateLoad(ptrTy_, srcVar, "cur_src");
        llvm::Value *foundBuild = builder_.CreateCall(strstrFn, {curSrc, oldStr}, "found_build");
        llvm::Value *notNullBuild = builder_.CreateICmpNE(foundBuild, null, "found_build_nn");
        builder_.CreateCondBr(notNullBuild, buildBodyBB, buildEndBB);

        builder_.SetInsertPoint(buildBodyBB);
        llvm::Value *curDst = builder_.CreateLoad(ptrTy_, dstVar, "cur_dst");
        // Copy bytes before match
        llvm::Value *srcInt = builder_.CreatePtrToInt(curSrc, i64Ty_, "src_int");
        llvm::Value *foundInt = builder_.CreatePtrToInt(foundBuild, i64Ty_, "found_int");
        llvm::Value *prefixLen = builder_.CreateSub(foundInt, srcInt, "prefix_len");
        builder_.CreateCall(memcpyFn, {curDst, curSrc, prefixLen});
        llvm::Value *dstAfterPrefix = builder_.CreateGEP(builder_.getInt8Ty(), curDst, prefixLen, "dst_after_prefix");
        // Copy new string
        builder_.CreateCall(memcpyFn, {dstAfterPrefix, newStr, newLen});
        llvm::Value *dstAfterNew = builder_.CreateGEP(builder_.getInt8Ty(), dstAfterPrefix, newLen, "dst_after_new");
        builder_.CreateStore(dstAfterNew, dstVar);
        // Advance source past old
        llvm::Value *srcAfterOld = builder_.CreateGEP(builder_.getInt8Ty(), foundBuild, oldLen, "src_after_old");
        builder_.CreateStore(srcAfterOld, srcVar);
        builder_.CreateBr(buildCondBB);

        builder_.SetInsertPoint(buildEndBB);
        // Copy remaining bytes (including null terminator)
        llvm::Value *finalSrc = builder_.CreateLoad(ptrTy_, srcVar, "final_src");
        llvm::Value *finalDst = builder_.CreateLoad(ptrTy_, dstVar, "final_dst");
        llvm::Value *remainLen = builder_.CreateCall(strlenFn, {finalSrc}, "remain_len");
        llvm::Value *remainPlusNull = builder_.CreateAdd(remainLen, llvm::ConstantInt::get(i64Ty_, 1), "remain_plus_null");
        builder_.CreateCall(memcpyFn, {finalDst, finalSrc, remainPlusNull});

        return buf;
    }

    // ===== Phase 2: テキスト変換 =====

    // to_upper(s) → str
    if (e->callee == "to_upper") {
        if (e->args.size() != 1)
            throw std::runtime_error("to_upper() takes exactly 1 argument");
        llvm::Value *s = emitExpr(*e->args[0]);
        if (s->getType() != ptrTy_)
            throw std::runtime_error("to_upper() requires str argument");

        auto strlenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto strlenFn = mod_->getOrInsertFunction("strlen", strlenTy);
        auto mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        auto mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);

        llvm::Value *len = builder_.CreateCall(strlenFn, {s}, "upper_len");
        llvm::Value *bufSize = builder_.CreateAdd(len, llvm::ConstantInt::get(i64Ty_, 1), "upper_buf_size");
        llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "upper_buf");

        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "upper_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "upper.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "upper.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "upper.end", fn_);

        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "upper_idx");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, len, "upper_cond"), bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "upper_i_cur");
        llvm::Value *srcPtr = builder_.CreateGEP(builder_.getInt8Ty(), s, iCur, "upper_src");
        llvm::Value *ch = builder_.CreateLoad(i8Ty_, srcPtr, "upper_ch");
        // if ch >= 'a' && ch <= 'z' then ch - 32
        llvm::Value *isLowerA = builder_.CreateICmpUGE(ch, llvm::ConstantInt::get(i8Ty_, 'a'), "is_lower_a");
        llvm::Value *isLowerZ = builder_.CreateICmpULE(ch, llvm::ConstantInt::get(i8Ty_, 'z'), "is_lower_z");
        llvm::Value *isLower = builder_.CreateAnd(isLowerA, isLowerZ, "is_lower");
        llvm::Value *upper = builder_.CreateSub(ch, llvm::ConstantInt::get(i8Ty_, 32), "upper_ch_val");
        llvm::Value *result = builder_.CreateSelect(isLower, upper, ch, "upper_result");
        llvm::Value *dstPtr = builder_.CreateGEP(builder_.getInt8Ty(), buf, iCur, "upper_dst");
        builder_.CreateStore(result, dstPtr);
        builder_.CreateStore(builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1), "upper_next"), iVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);
        llvm::Value *nullPtr = builder_.CreateGEP(builder_.getInt8Ty(), buf, len, "upper_null");
        builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), nullPtr);
        return buf;
    }

    // to_lower(s) → str
    if (e->callee == "to_lower") {
        if (e->args.size() != 1)
            throw std::runtime_error("to_lower() takes exactly 1 argument");
        llvm::Value *s = emitExpr(*e->args[0]);
        if (s->getType() != ptrTy_)
            throw std::runtime_error("to_lower() requires str argument");

        auto strlenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto strlenFn = mod_->getOrInsertFunction("strlen", strlenTy);
        auto mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        auto mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);

        llvm::Value *len = builder_.CreateCall(strlenFn, {s}, "lower_len");
        llvm::Value *bufSize = builder_.CreateAdd(len, llvm::ConstantInt::get(i64Ty_, 1), "lower_buf_size");
        llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "lower_buf");

        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "lower_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "lower.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "lower.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "lower.end", fn_);

        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "lower_idx");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, len, "lower_cond"), bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "lower_i_cur");
        llvm::Value *srcPtr = builder_.CreateGEP(builder_.getInt8Ty(), s, iCur, "lower_src");
        llvm::Value *ch = builder_.CreateLoad(i8Ty_, srcPtr, "lower_ch");
        llvm::Value *isUpperA = builder_.CreateICmpUGE(ch, llvm::ConstantInt::get(i8Ty_, 'A'), "is_upper_a");
        llvm::Value *isUpperZ = builder_.CreateICmpULE(ch, llvm::ConstantInt::get(i8Ty_, 'Z'), "is_upper_z");
        llvm::Value *isUpper = builder_.CreateAnd(isUpperA, isUpperZ, "is_upper");
        llvm::Value *lower = builder_.CreateAdd(ch, llvm::ConstantInt::get(i8Ty_, 32), "lower_ch_val");
        llvm::Value *result = builder_.CreateSelect(isUpper, lower, ch, "lower_result");
        llvm::Value *dstPtr = builder_.CreateGEP(builder_.getInt8Ty(), buf, iCur, "lower_dst");
        builder_.CreateStore(result, dstPtr);
        builder_.CreateStore(builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1), "lower_next"), iVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);
        llvm::Value *nullPtr = builder_.CreateGEP(builder_.getInt8Ty(), buf, len, "lower_null");
        builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), nullPtr);
        return buf;
    }

    // trim(s) → str
    if (e->callee == "trim") {
        if (e->args.size() != 1)
            throw std::runtime_error("trim() takes exactly 1 argument");
        llvm::Value *s = emitExpr(*e->args[0]);
        if (s->getType() != ptrTy_)
            throw std::runtime_error("trim() requires str argument");

        auto strlenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto strlenFn = mod_->getOrInsertFunction("strlen", strlenTy);
        auto mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        auto mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
        auto memcpyTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
        auto memcpyFn = mod_->getOrInsertFunction("memcpy", memcpyTy);

        llvm::Value *len = builder_.CreateCall(strlenFn, {s}, "trim_len");

        // Find start: skip leading whitespace
        llvm::AllocaInst *startVar = builder_.CreateAlloca(i64Ty_, nullptr, "trim_start");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), startVar);

        llvm::BasicBlock *startCondBB = llvm::BasicBlock::Create(*ctx_, "trim.start_cond", fn_);
        llvm::BasicBlock *startBodyBB = llvm::BasicBlock::Create(*ctx_, "trim.start_body", fn_);
        llvm::BasicBlock *startEndBB = llvm::BasicBlock::Create(*ctx_, "trim.start_end", fn_);

        builder_.CreateBr(startCondBB);
        builder_.SetInsertPoint(startCondBB);
        llvm::Value *startIdx = builder_.CreateLoad(i64Ty_, startVar, "start_idx");
        llvm::Value *startInBounds = builder_.CreateICmpSLT(startIdx, len, "start_in_bounds");

        llvm::BasicBlock *startCheckBB = llvm::BasicBlock::Create(*ctx_, "trim.start_check", fn_);
        builder_.CreateCondBr(startInBounds, startCheckBB, startEndBB);

        builder_.SetInsertPoint(startCheckBB);
        llvm::Value *startPtr = builder_.CreateGEP(builder_.getInt8Ty(), s, startIdx, "start_ptr");
        llvm::Value *startCh = builder_.CreateLoad(i8Ty_, startPtr, "start_ch");
        // Check whitespace: ' ', '\t', '\n', '\r'
        llvm::Value *isSp = builder_.CreateICmpEQ(startCh, llvm::ConstantInt::get(i8Ty_, ' '), "is_sp");
        llvm::Value *isTab = builder_.CreateICmpEQ(startCh, llvm::ConstantInt::get(i8Ty_, '\t'), "is_tab");
        llvm::Value *isNl = builder_.CreateICmpEQ(startCh, llvm::ConstantInt::get(i8Ty_, '\n'), "is_nl");
        llvm::Value *isCr = builder_.CreateICmpEQ(startCh, llvm::ConstantInt::get(i8Ty_, '\r'), "is_cr");
        llvm::Value *isWs = builder_.CreateOr(builder_.CreateOr(isSp, isTab), builder_.CreateOr(isNl, isCr), "is_ws");
        builder_.CreateCondBr(isWs, startBodyBB, startEndBB);

        builder_.SetInsertPoint(startBodyBB);
        llvm::Value *startNext = builder_.CreateAdd(startIdx, llvm::ConstantInt::get(i64Ty_, 1), "start_next");
        builder_.CreateStore(startNext, startVar);
        builder_.CreateBr(startCondBB);

        builder_.SetInsertPoint(startEndBB);
        llvm::Value *finalStart = builder_.CreateLoad(i64Ty_, startVar, "final_start");

        // Find end: skip trailing whitespace
        llvm::AllocaInst *endVar = builder_.CreateAlloca(i64Ty_, nullptr, "trim_end");
        builder_.CreateStore(len, endVar);

        llvm::BasicBlock *endCondBB = llvm::BasicBlock::Create(*ctx_, "trim.end_cond", fn_);
        llvm::BasicBlock *endBodyBB = llvm::BasicBlock::Create(*ctx_, "trim.end_body", fn_);
        llvm::BasicBlock *endEndBB = llvm::BasicBlock::Create(*ctx_, "trim.end_end", fn_);

        builder_.CreateBr(endCondBB);
        builder_.SetInsertPoint(endCondBB);
        llvm::Value *endIdx = builder_.CreateLoad(i64Ty_, endVar, "end_idx");
        llvm::Value *endGtStart = builder_.CreateICmpSGT(endIdx, finalStart, "end_gt_start");

        llvm::BasicBlock *endCheckBB = llvm::BasicBlock::Create(*ctx_, "trim.end_check", fn_);
        builder_.CreateCondBr(endGtStart, endCheckBB, endEndBB);

        builder_.SetInsertPoint(endCheckBB);
        llvm::Value *endPrev = builder_.CreateSub(endIdx, llvm::ConstantInt::get(i64Ty_, 1), "end_prev");
        llvm::Value *endPtr = builder_.CreateGEP(builder_.getInt8Ty(), s, endPrev, "end_ptr");
        llvm::Value *endCh = builder_.CreateLoad(i8Ty_, endPtr, "end_ch");
        llvm::Value *isSp2 = builder_.CreateICmpEQ(endCh, llvm::ConstantInt::get(i8Ty_, ' '), "is_sp2");
        llvm::Value *isTab2 = builder_.CreateICmpEQ(endCh, llvm::ConstantInt::get(i8Ty_, '\t'), "is_tab2");
        llvm::Value *isNl2 = builder_.CreateICmpEQ(endCh, llvm::ConstantInt::get(i8Ty_, '\n'), "is_nl2");
        llvm::Value *isCr2 = builder_.CreateICmpEQ(endCh, llvm::ConstantInt::get(i8Ty_, '\r'), "is_cr2");
        llvm::Value *isWs2 = builder_.CreateOr(builder_.CreateOr(isSp2, isTab2), builder_.CreateOr(isNl2, isCr2), "is_ws2");
        builder_.CreateCondBr(isWs2, endBodyBB, endEndBB);

        builder_.SetInsertPoint(endBodyBB);
        builder_.CreateStore(endPrev, endVar);
        builder_.CreateBr(endCondBB);

        builder_.SetInsertPoint(endEndBB);
        llvm::Value *finalEnd = builder_.CreateLoad(i64Ty_, endVar, "final_end");

        // Build result
        llvm::Value *resultLen = builder_.CreateSub(finalEnd, finalStart, "trim_result_len");
        llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
        llvm::Value *isNeg = builder_.CreateICmpSLT(resultLen, zero, "trim_neg");
        llvm::Value *safeLen = builder_.CreateSelect(isNeg, zero, resultLen, "trim_safe_len");
        llvm::Value *bufSize = builder_.CreateAdd(safeLen, llvm::ConstantInt::get(i64Ty_, 1), "trim_buf_size");
        llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "trim_buf");
        llvm::Value *srcPtr = builder_.CreateGEP(builder_.getInt8Ty(), s, finalStart, "trim_src");
        builder_.CreateCall(memcpyFn, {buf, srcPtr, safeLen});
        llvm::Value *nullEnd = builder_.CreateGEP(builder_.getInt8Ty(), buf, safeLen, "trim_null");
        builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), nullEnd);
        return buf;
    }

    // trim_start(s) → str
    if (e->callee == "trim_start") {
        if (e->args.size() != 1)
            throw std::runtime_error("trim_start() takes exactly 1 argument");
        llvm::Value *s = emitExpr(*e->args[0]);
        if (s->getType() != ptrTy_)
            throw std::runtime_error("trim_start() requires str argument");

        auto strlenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto strlenFn = mod_->getOrInsertFunction("strlen", strlenTy);
        auto mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        auto mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
        auto memcpyTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
        auto memcpyFn = mod_->getOrInsertFunction("memcpy", memcpyTy);

        llvm::Value *len = builder_.CreateCall(strlenFn, {s}, "tstart_len");

        llvm::AllocaInst *startVar = builder_.CreateAlloca(i64Ty_, nullptr, "tstart_start");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), startVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "tstart.cond", fn_);
        llvm::BasicBlock *checkBB = llvm::BasicBlock::Create(*ctx_, "tstart.check", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "tstart.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "tstart.end", fn_);

        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *idx = builder_.CreateLoad(i64Ty_, startVar, "tstart_idx");
        builder_.CreateCondBr(builder_.CreateICmpSLT(idx, len, "tstart_bound"), checkBB, endBB);

        builder_.SetInsertPoint(checkBB);
        llvm::Value *ptr = builder_.CreateGEP(builder_.getInt8Ty(), s, idx, "tstart_ptr");
        llvm::Value *ch = builder_.CreateLoad(i8Ty_, ptr, "tstart_ch");
        llvm::Value *isWs = builder_.CreateOr(
            builder_.CreateOr(
                builder_.CreateICmpEQ(ch, llvm::ConstantInt::get(i8Ty_, ' ')),
                builder_.CreateICmpEQ(ch, llvm::ConstantInt::get(i8Ty_, '\t'))),
            builder_.CreateOr(
                builder_.CreateICmpEQ(ch, llvm::ConstantInt::get(i8Ty_, '\n')),
                builder_.CreateICmpEQ(ch, llvm::ConstantInt::get(i8Ty_, '\r'))),
            "tstart_ws");
        builder_.CreateCondBr(isWs, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        builder_.CreateStore(builder_.CreateAdd(idx, llvm::ConstantInt::get(i64Ty_, 1), "tstart_next"), startVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);
        llvm::Value *finalStart = builder_.CreateLoad(i64Ty_, startVar, "tstart_final");
        llvm::Value *resultLen = builder_.CreateSub(len, finalStart, "tstart_rlen");
        llvm::Value *bufSize = builder_.CreateAdd(resultLen, llvm::ConstantInt::get(i64Ty_, 1), "tstart_bsize");
        llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "tstart_buf");
        llvm::Value *srcPtr = builder_.CreateGEP(builder_.getInt8Ty(), s, finalStart, "tstart_src");
        builder_.CreateCall(memcpyFn, {buf, srcPtr, resultLen});
        llvm::Value *nullEnd = builder_.CreateGEP(builder_.getInt8Ty(), buf, resultLen, "tstart_null");
        builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), nullEnd);
        return buf;
    }

    // trim_end(s) → str
    if (e->callee == "trim_end") {
        if (e->args.size() != 1)
            throw std::runtime_error("trim_end() takes exactly 1 argument");
        llvm::Value *s = emitExpr(*e->args[0]);
        if (s->getType() != ptrTy_)
            throw std::runtime_error("trim_end() requires str argument");

        auto strlenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto strlenFn = mod_->getOrInsertFunction("strlen", strlenTy);
        auto mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        auto mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
        auto memcpyTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
        auto memcpyFn = mod_->getOrInsertFunction("memcpy", memcpyTy);

        llvm::Value *len = builder_.CreateCall(strlenFn, {s}, "tend_len");

        llvm::AllocaInst *endVar = builder_.CreateAlloca(i64Ty_, nullptr, "tend_end");
        builder_.CreateStore(len, endVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "tend.cond", fn_);
        llvm::BasicBlock *checkBB = llvm::BasicBlock::Create(*ctx_, "tend.check", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "tend.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "tend.end", fn_);

        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *endIdx = builder_.CreateLoad(i64Ty_, endVar, "tend_idx");
        llvm::Value *gtZero = builder_.CreateICmpSGT(endIdx, llvm::ConstantInt::get(i64Ty_, 0), "tend_gt0");
        builder_.CreateCondBr(gtZero, checkBB, endBB);

        builder_.SetInsertPoint(checkBB);
        llvm::Value *prevIdx = builder_.CreateSub(endIdx, llvm::ConstantInt::get(i64Ty_, 1), "tend_prev");
        llvm::Value *ptr = builder_.CreateGEP(builder_.getInt8Ty(), s, prevIdx, "tend_ptr");
        llvm::Value *ch = builder_.CreateLoad(i8Ty_, ptr, "tend_ch");
        llvm::Value *isWs = builder_.CreateOr(
            builder_.CreateOr(
                builder_.CreateICmpEQ(ch, llvm::ConstantInt::get(i8Ty_, ' ')),
                builder_.CreateICmpEQ(ch, llvm::ConstantInt::get(i8Ty_, '\t'))),
            builder_.CreateOr(
                builder_.CreateICmpEQ(ch, llvm::ConstantInt::get(i8Ty_, '\n')),
                builder_.CreateICmpEQ(ch, llvm::ConstantInt::get(i8Ty_, '\r'))),
            "tend_ws");
        builder_.CreateCondBr(isWs, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        builder_.CreateStore(prevIdx, endVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);
        llvm::Value *finalEnd = builder_.CreateLoad(i64Ty_, endVar, "tend_final");
        llvm::Value *bufSize = builder_.CreateAdd(finalEnd, llvm::ConstantInt::get(i64Ty_, 1), "tend_bsize");
        llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "tend_buf");
        builder_.CreateCall(memcpyFn, {buf, s, finalEnd});
        llvm::Value *nullEnd2 = builder_.CreateGEP(builder_.getInt8Ty(), buf, finalEnd, "tend_null");
        builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), nullEnd2);
        return buf;
    }

    // repeat(s, n) → str
    if (e->callee == "repeat") {
        if (e->args.size() != 2)
            throw std::runtime_error("repeat() takes exactly 2 arguments");
        llvm::Value *s = emitExpr(*e->args[0]);
        llvm::Value *n = emitExpr(*e->args[1]);
        if (s->getType() != ptrTy_)
            throw std::runtime_error("repeat() requires str as first argument");

        auto strlenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto strlenFn = mod_->getOrInsertFunction("strlen", strlenTy);
        auto mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        auto mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
        auto memcpyTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
        auto memcpyFn = mod_->getOrInsertFunction("memcpy", memcpyTy);

        llvm::Value *sLen = builder_.CreateCall(strlenFn, {s}, "repeat_slen");
        llvm::Value *totalLen = builder_.CreateMul(sLen, n, "repeat_total");
        llvm::Value *bufSize = builder_.CreateAdd(totalLen, llvm::ConstantInt::get(i64Ty_, 1), "repeat_bsize");
        llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "repeat_buf");

        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "repeat_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "repeat.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "repeat.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "repeat.end", fn_);

        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "repeat_idx");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, n, "repeat_cond"), bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "repeat_i_cur");
        llvm::Value *offset = builder_.CreateMul(iCur, sLen, "repeat_offset");
        llvm::Value *dstPtr = builder_.CreateGEP(builder_.getInt8Ty(), buf, offset, "repeat_dst");
        builder_.CreateCall(memcpyFn, {dstPtr, s, sLen});
        builder_.CreateStore(builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1), "repeat_next"), iVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);
        llvm::Value *nullPtr = builder_.CreateGEP(builder_.getInt8Ty(), buf, totalLen, "repeat_null");
        builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), nullPtr);
        return buf;
    }

    // reverse(list) → new reversed list, or reverse(str) → str
    if (e->callee == "reverse") {
        if (e->args.size() != 1)
            throw std::runtime_error("reverse() takes exactly 1 argument");
        llvm::Value *arg = emitExpr(*e->args[0]);

        // List reverse
        llvm::Type *elemTy = getListElementType(arg);
        if (elemTy) {
            auto mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
            auto mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
            const llvm::DataLayout &dl = mod_->getDataLayout();
            uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);

            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, arg, 0, "rev_len_ptr");
            llvm::Value *len = builder_.CreateLoad(i64Ty_, lenPtr, "rev_len");
            llvm::Value *dataField = builder_.CreateStructGEP(listHeaderTy_, arg, 2, "rev_data_field");
            llvm::Value *srcData = builder_.CreateLoad(ptrTy_, dataField, "rev_src_data");

            // Allocate new list
            llvm::Value *newHeader = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "rev_header");
            llvm::Value *dataSize = builder_.CreateMul(len, llvm::ConstantInt::get(i64Ty_, elemSize), "rev_dsize");
            llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "rev_data");

            // Copy in reverse order
            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "rev_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "lrev.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "lrev.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "lrev.end", fn_);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(condBB);
            llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "rev_idx");
            builder_.CreateCondBr(builder_.CreateICmpSLT(i, len, "rev_cond"), bodyBB, endBB);

            builder_.SetInsertPoint(bodyBB);
            llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "rev_i_cur");
            llvm::Value *srcIdx = builder_.CreateSub(builder_.CreateSub(len, llvm::ConstantInt::get(i64Ty_, 1)), iCur, "rev_src_idx");
            llvm::Value *srcPtr = builder_.CreateGEP(elemTy, srcData, srcIdx, "rev_src");
            llvm::Value *val = builder_.CreateLoad(elemTy, srcPtr, "rev_val");
            llvm::Value *dstPtr = builder_.CreateGEP(elemTy, newData, iCur, "rev_dst");
            builder_.CreateStore(val, dstPtr);
            builder_.CreateStore(builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(endBB);
            llvm::Value *newLenPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 0, "rev_new_len");
            builder_.CreateStore(len, newLenPtr);
            llvm::Value *newCapPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 1, "rev_new_cap");
            builder_.CreateStore(len, newCapPtr);
            llvm::Value *newDataField = builder_.CreateStructGEP(listHeaderTy_, newHeader, 2, "rev_new_data");
            builder_.CreateStore(newData, newDataField);

            list_element_types_[newHeader] = elemTy;
            return newHeader;
        }

        // String reverse
        llvm::Value *s = arg;
        if (s->getType() != ptrTy_)
            throw std::runtime_error("reverse() requires list or str argument");

        auto strlenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto strlenFn = mod_->getOrInsertFunction("strlen", strlenTy);
        auto mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        auto mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);

        llvm::Value *len = builder_.CreateCall(strlenFn, {s}, "rev_len");
        llvm::Value *bufSize = builder_.CreateAdd(len, llvm::ConstantInt::get(i64Ty_, 1), "rev_bsize");
        llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "rev_buf");

        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "rev_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "rev.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "rev.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "rev.end", fn_);

        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "rev_idx");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, len, "rev_cond"), bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "rev_i_cur");
        // src[len - 1 - i]
        llvm::Value *srcIdx = builder_.CreateSub(
            builder_.CreateSub(len, llvm::ConstantInt::get(i64Ty_, 1), "len_m1"),
            iCur, "rev_src_idx");
        llvm::Value *srcPtr = builder_.CreateGEP(builder_.getInt8Ty(), s, srcIdx, "rev_src");
        llvm::Value *ch = builder_.CreateLoad(i8Ty_, srcPtr, "rev_ch");
        llvm::Value *dstPtr = builder_.CreateGEP(builder_.getInt8Ty(), buf, iCur, "rev_dst");
        builder_.CreateStore(ch, dstPtr);
        builder_.CreateStore(builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1), "rev_next"), iVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);
        llvm::Value *nullPtr = builder_.CreateGEP(builder_.getInt8Ty(), buf, len, "rev_null");
        builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), nullPtr);
        return buf;
    }

    // ===== Phase 3: List<str> 連携 =====

    // split(s, delim) → List<str>
    if (e->callee == "split") {
        if (e->args.size() != 2)
            throw std::runtime_error("split() takes exactly 2 arguments");
        llvm::Value *s = emitExpr(*e->args[0]);
        llvm::Value *delim = emitExpr(*e->args[1]);
        if (s->getType() != ptrTy_ || delim->getType() != ptrTy_)
            throw std::runtime_error("split() requires str arguments");

        auto strlenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto strlenFn = mod_->getOrInsertFunction("strlen", strlenTy);
        auto strstrTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
        auto strstrFn = mod_->getOrInsertFunction("strstr", strstrTy);
        auto mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        auto mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
        auto memcpyTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
        auto memcpyFn = mod_->getOrInsertFunction("memcpy", memcpyTy);

        llvm::Value *delimLen = builder_.CreateCall(strlenFn, {delim}, "split_dlen");
        llvm::Value *null = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));

        // Pass 1: count delimiters
        llvm::AllocaInst *countVar = builder_.CreateAlloca(i64Ty_, nullptr, "split_count");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), countVar);
        llvm::AllocaInst *searchVar = builder_.CreateAlloca(ptrTy_, nullptr, "split_search");
        builder_.CreateStore(s, searchVar);

        llvm::BasicBlock *countCondBB = llvm::BasicBlock::Create(*ctx_, "split.count_cond", fn_);
        llvm::BasicBlock *countBodyBB = llvm::BasicBlock::Create(*ctx_, "split.count_body", fn_);
        llvm::BasicBlock *countEndBB = llvm::BasicBlock::Create(*ctx_, "split.count_end", fn_);

        builder_.CreateBr(countCondBB);
        builder_.SetInsertPoint(countCondBB);
        llvm::Value *sp = builder_.CreateLoad(ptrTy_, searchVar, "split_sp");
        llvm::Value *found = builder_.CreateCall(strstrFn, {sp, delim}, "split_found");
        builder_.CreateCondBr(builder_.CreateICmpNE(found, null, "split_nn"), countBodyBB, countEndBB);

        builder_.SetInsertPoint(countBodyBB);
        llvm::Value *cnt = builder_.CreateLoad(i64Ty_, countVar, "split_cnt");
        builder_.CreateStore(builder_.CreateAdd(cnt, llvm::ConstantInt::get(i64Ty_, 1)), countVar);
        builder_.CreateStore(builder_.CreateGEP(builder_.getInt8Ty(), found, delimLen, "split_adv"), searchVar);
        builder_.CreateBr(countCondBB);

        builder_.SetInsertPoint(countEndBB);
        llvm::Value *delimCount = builder_.CreateLoad(i64Ty_, countVar, "split_delim_count");
        llvm::Value *elemCount = builder_.CreateAdd(delimCount, llvm::ConstantInt::get(i64Ty_, 1), "split_elem_count");

        // Allocate list header
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *headerPtr = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "split_header");

        // Allocate data array (ptr*)
        uint64_t ptrSize = dl.getTypeAllocSize(ptrTy_);
        llvm::Value *dataSize = builder_.CreateMul(elemCount, llvm::ConstantInt::get(i64Ty_, ptrSize), "split_data_size");
        llvm::Value *dataPtr = builder_.CreateCall(mallocFn, {dataSize}, "split_data");

        // Pass 2: fill data array
        llvm::AllocaInst *srcVar = builder_.CreateAlloca(ptrTy_, nullptr, "split_src");
        builder_.CreateStore(s, srcVar);
        llvm::AllocaInst *idxVar = builder_.CreateAlloca(i64Ty_, nullptr, "split_idx");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), idxVar);

        llvm::BasicBlock *buildCondBB = llvm::BasicBlock::Create(*ctx_, "split.build_cond", fn_);
        llvm::BasicBlock *buildBodyBB = llvm::BasicBlock::Create(*ctx_, "split.build_body", fn_);
        llvm::BasicBlock *buildEndBB = llvm::BasicBlock::Create(*ctx_, "split.build_end", fn_);

        builder_.CreateBr(buildCondBB);
        builder_.SetInsertPoint(buildCondBB);
        llvm::Value *curSrc = builder_.CreateLoad(ptrTy_, srcVar, "split_cur_src");
        llvm::Value *foundBuild = builder_.CreateCall(strstrFn, {curSrc, delim}, "split_found_build");
        builder_.CreateCondBr(builder_.CreateICmpNE(foundBuild, null, "split_build_nn"), buildBodyBB, buildEndBB);

        builder_.SetInsertPoint(buildBodyBB);
        // Calculate segment length
        llvm::Value *curSrcInt = builder_.CreatePtrToInt(curSrc, i64Ty_, "split_src_int");
        llvm::Value *foundInt = builder_.CreatePtrToInt(foundBuild, i64Ty_, "split_found_int");
        llvm::Value *segLen = builder_.CreateSub(foundInt, curSrcInt, "split_seg_len");
        // Allocate and copy segment
        llvm::Value *segBufSize = builder_.CreateAdd(segLen, llvm::ConstantInt::get(i64Ty_, 1), "split_seg_bsize");
        llvm::Value *segBuf = builder_.CreateCall(mallocFn, {segBufSize}, "split_seg_buf");
        builder_.CreateCall(memcpyFn, {segBuf, curSrc, segLen});
        llvm::Value *segNull = builder_.CreateGEP(builder_.getInt8Ty(), segBuf, segLen, "split_seg_null");
        builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), segNull);
        // Store in data array
        llvm::Value *curIdx = builder_.CreateLoad(i64Ty_, idxVar, "split_cur_idx");
        llvm::Value *elemPtr = builder_.CreateGEP(ptrTy_, dataPtr, {curIdx}, "split_elem_ptr");
        builder_.CreateStore(segBuf, elemPtr);
        // Advance
        builder_.CreateStore(builder_.CreateAdd(curIdx, llvm::ConstantInt::get(i64Ty_, 1)), idxVar);
        builder_.CreateStore(builder_.CreateGEP(builder_.getInt8Ty(), foundBuild, delimLen, "split_adv2"), srcVar);
        builder_.CreateBr(buildCondBB);

        builder_.SetInsertPoint(buildEndBB);
        // Store last segment
        llvm::Value *lastSrc = builder_.CreateLoad(ptrTy_, srcVar, "split_last_src");
        llvm::Value *lastLen = builder_.CreateCall(strlenFn, {lastSrc}, "split_last_len");
        llvm::Value *lastBufSize = builder_.CreateAdd(lastLen, llvm::ConstantInt::get(i64Ty_, 1), "split_last_bsize");
        llvm::Value *lastBuf = builder_.CreateCall(mallocFn, {lastBufSize}, "split_last_buf");
        builder_.CreateCall(memcpyFn, {lastBuf, lastSrc, lastLen});
        llvm::Value *lastNull = builder_.CreateGEP(builder_.getInt8Ty(), lastBuf, lastLen, "split_last_null");
        builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), lastNull);
        llvm::Value *lastIdx = builder_.CreateLoad(i64Ty_, idxVar, "split_last_idx");
        llvm::Value *lastElemPtr = builder_.CreateGEP(ptrTy_, dataPtr, {lastIdx}, "split_last_elem_ptr");
        builder_.CreateStore(lastBuf, lastElemPtr);

        // Store header fields
        llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 0, "split_len_ptr");
        builder_.CreateStore(elemCount, lenPtr);
        llvm::Value *capPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 1, "split_cap_ptr");
        builder_.CreateStore(elemCount, capPtr);
        llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 2, "split_data_field");
        builder_.CreateStore(dataPtr, dataPtrField);

        list_element_types_[headerPtr] = ptrTy_;
        return headerPtr;
    }

    // join(list, sep) → str
    if (e->callee == "join") {
        if (e->args.size() != 2)
            throw std::runtime_error("join() takes exactly 2 arguments");
        llvm::Value *listPtr = emitExpr(*e->args[0]);
        llvm::Value *sep = emitExpr(*e->args[1]);
        if (listPtr->getType() != ptrTy_ || sep->getType() != ptrTy_)
            throw std::runtime_error("join() requires List<str> and str arguments");
        if (getListElementType(listPtr) != ptrTy_)
            throw std::runtime_error("join() requires List<str> as first argument");

        auto strlenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto strlenFn = mod_->getOrInsertFunction("strlen", strlenTy);
        auto mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        auto mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
        auto memcpyTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
        auto memcpyFn = mod_->getOrInsertFunction("memcpy", memcpyTy);

        llvm::Value *listLen = builder_.CreateLoad(i64Ty_,
            builder_.CreateStructGEP(listHeaderTy_, listPtr, 0, "join_len_ptr"), "join_len");
        llvm::Value *listData = builder_.CreateLoad(ptrTy_,
            builder_.CreateStructGEP(listHeaderTy_, listPtr, 2, "join_data_ptr"), "join_data");
        llvm::Value *sepLen = builder_.CreateCall(strlenFn, {sep}, "join_sep_len");

        // Pass 1: calculate total length
        llvm::AllocaInst *totalVar = builder_.CreateAlloca(i64Ty_, nullptr, "join_total");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), totalVar);
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "join_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *len1CondBB = llvm::BasicBlock::Create(*ctx_, "join.len_cond", fn_);
        llvm::BasicBlock *len1BodyBB = llvm::BasicBlock::Create(*ctx_, "join.len_body", fn_);
        llvm::BasicBlock *len1EndBB = llvm::BasicBlock::Create(*ctx_, "join.len_end", fn_);

        builder_.CreateBr(len1CondBB);
        builder_.SetInsertPoint(len1CondBB);
        llvm::Value *i1 = builder_.CreateLoad(i64Ty_, iVar, "join_i1");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i1, listLen, "join_len_cond"), len1BodyBB, len1EndBB);

        builder_.SetInsertPoint(len1BodyBB);
        llvm::Value *i1Cur = builder_.CreateLoad(i64Ty_, iVar, "join_i1_cur");
        llvm::Value *elemPtr = builder_.CreateGEP(ptrTy_, listData, {i1Cur}, "join_elem_ptr");
        llvm::Value *elem = builder_.CreateLoad(ptrTy_, elemPtr, "join_elem");
        llvm::Value *elemLen = builder_.CreateCall(strlenFn, {elem}, "join_elem_len");
        llvm::Value *total = builder_.CreateLoad(i64Ty_, totalVar, "join_total_cur");
        llvm::Value *newTotal = builder_.CreateAdd(total, elemLen, "join_total_add");
        builder_.CreateStore(newTotal, totalVar);
        builder_.CreateStore(builder_.CreateAdd(i1Cur, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(len1CondBB);

        builder_.SetInsertPoint(len1EndBB);
        llvm::Value *elemTotal = builder_.CreateLoad(i64Ty_, totalVar, "join_elem_total");
        // Add separator lengths: sepLen * (listLen - 1), but only if listLen > 0
        llvm::Value *sepCount = builder_.CreateSub(listLen, llvm::ConstantInt::get(i64Ty_, 1), "join_sep_count");
        llvm::Value *isPositive = builder_.CreateICmpSGT(listLen, llvm::ConstantInt::get(i64Ty_, 0), "join_has_elems");
        llvm::Value *safeSepCount = builder_.CreateSelect(isPositive, sepCount, llvm::ConstantInt::get(i64Ty_, 0), "safe_sep_count");
        llvm::Value *sepTotal = builder_.CreateMul(safeSepCount, sepLen, "join_sep_total");
        llvm::Value *grandTotal = builder_.CreateAdd(elemTotal, sepTotal, "join_grand_total");
        llvm::Value *bufSize = builder_.CreateAdd(grandTotal, llvm::ConstantInt::get(i64Ty_, 1), "join_bsize");
        llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "join_buf");

        // Pass 2: build result
        llvm::AllocaInst *dstVar = builder_.CreateAlloca(ptrTy_, nullptr, "join_dst");
        builder_.CreateStore(buf, dstVar);
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *buildCondBB = llvm::BasicBlock::Create(*ctx_, "join.build_cond", fn_);
        llvm::BasicBlock *buildBodyBB = llvm::BasicBlock::Create(*ctx_, "join.build_body", fn_);
        llvm::BasicBlock *buildEndBB = llvm::BasicBlock::Create(*ctx_, "join.build_end", fn_);

        builder_.CreateBr(buildCondBB);
        builder_.SetInsertPoint(buildCondBB);
        llvm::Value *i2 = builder_.CreateLoad(i64Ty_, iVar, "join_i2");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i2, listLen, "join_build_cond"), buildBodyBB, buildEndBB);

        builder_.SetInsertPoint(buildBodyBB);
        llvm::Value *i2Cur = builder_.CreateLoad(i64Ty_, iVar, "join_i2_cur");
        llvm::Value *curDst = builder_.CreateLoad(ptrTy_, dstVar, "join_cur_dst");

        // Add separator before element (if not first)
        llvm::Value *notFirst = builder_.CreateICmpSGT(i2Cur, llvm::ConstantInt::get(i64Ty_, 0), "join_not_first");
        llvm::BasicBlock *sepBB = llvm::BasicBlock::Create(*ctx_, "join.sep", fn_);
        llvm::BasicBlock *elemBB = llvm::BasicBlock::Create(*ctx_, "join.elem", fn_);
        builder_.CreateCondBr(notFirst, sepBB, elemBB);

        builder_.SetInsertPoint(sepBB);
        llvm::Value *dstBeforeSep = builder_.CreateLoad(ptrTy_, dstVar, "dst_before_sep");
        builder_.CreateCall(memcpyFn, {dstBeforeSep, sep, sepLen});
        llvm::Value *dstAfterSep = builder_.CreateGEP(builder_.getInt8Ty(), dstBeforeSep, sepLen, "dst_after_sep");
        builder_.CreateStore(dstAfterSep, dstVar);
        builder_.CreateBr(elemBB);

        builder_.SetInsertPoint(elemBB);
        llvm::Value *dstForElem = builder_.CreateLoad(ptrTy_, dstVar, "dst_for_elem");
        llvm::Value *elemPtr2 = builder_.CreateGEP(ptrTy_, listData, {i2Cur}, "join_elem_ptr2");
        llvm::Value *elem2 = builder_.CreateLoad(ptrTy_, elemPtr2, "join_elem2");
        llvm::Value *elem2Len = builder_.CreateCall(strlenFn, {elem2}, "join_elem2_len");
        builder_.CreateCall(memcpyFn, {dstForElem, elem2, elem2Len});
        llvm::Value *dstAfterElem = builder_.CreateGEP(builder_.getInt8Ty(), dstForElem, elem2Len, "dst_after_elem");
        builder_.CreateStore(dstAfterElem, dstVar);
        builder_.CreateStore(builder_.CreateAdd(i2Cur, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(buildCondBB);

        builder_.SetInsertPoint(buildEndBB);
        // Null terminate
        llvm::Value *finalDst = builder_.CreateLoad(ptrTy_, dstVar, "join_final_dst");
        builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), finalDst);
        return buf;
    }

    // filter(list, predicate) → new list with elements matching predicate
    if (e->callee == "filter") {
        if (e->args.size() != 2)
            throw std::runtime_error("filter() takes exactly 2 arguments");

        llvm::Value *listVal = emitExpr(*e->args[0]);
        llvm::Value *lambdaVal = emitExpr(*e->args[1]);

        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy)
            throw std::runtime_error("filter() requires a list as first argument");

        // Get lambda type info (handle LoadInst for variable-passed functions)
        auto fnIt = fn_type_info_.find(lambdaVal);
        if (fnIt == fn_type_info_.end()) {
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(lambdaVal))
                fnIt = fn_type_info_.find(load->getPointerOperand());
        }
        if (fnIt == fn_type_info_.end())
            throw std::runtime_error("filter() requires a function as second argument");
        auto &info = fnIt->second;

        if (info.paramTypes.size() != 1 || info.returnType != i1Ty_)
            throw std::runtime_error("filter() predicate must take 1 argument and return bool");

        // Read source list
        llvm::Value *srcLenPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 0, "filter_src_len_ptr");
        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, srcLenPtr, "filter_src_len");
        llvm::Value *srcDataPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 2, "filter_src_data_field");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, srcDataPtr, "filter_src_data");

        // Allocate new list header + data (capacity = source length)
        llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *newHeader = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "filter_header");

        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        llvm::Value *dataSize = builder_.CreateMul(srcLen, llvm::ConstantInt::get(i64Ty_, elemSize), "filter_data_size");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "filter_data");

        // Set up data pointer in header
        llvm::Value *newDataField = builder_.CreateStructGEP(listHeaderTy_, newHeader, 2, "filter_data_field");
        builder_.CreateStore(newData, newDataField);
        llvm::Value *newCapPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 1, "filter_cap_ptr");
        builder_.CreateStore(srcLen, newCapPtr);

        // Loop counter and output counter
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "filter_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
        llvm::AllocaInst *outVar = builder_.CreateAlloca(i64Ty_, nullptr, "filter_out");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), outVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "filter.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "filter.body", fn_);
        llvm::BasicBlock *storeBB = llvm::BasicBlock::Create(*ctx_, "filter.store", fn_);
        llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "filter.next", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "filter.end", fn_);

        builder_.CreateBr(condBB);

        // Condition: i < srcLen
        builder_.SetInsertPoint(condBB);
        llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "fi");
        llvm::Value *cond = builder_.CreateICmpSLT(iVal, srcLen, "filter_cond");
        builder_.CreateCondBr(cond, bodyBB, endBB);

        // Body: load element, call predicate
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "fi_cur");
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {iCur}, "filter_elem_ptr");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "filter_elem");
        llvm::Value *pred = emitLambdaCall(lambdaVal, info, {elem}, "filter_pred");
        builder_.CreateCondBr(pred, storeBB, nextBB);

        // Store: add element to output
        builder_.SetInsertPoint(storeBB);
        llvm::Value *outIdx = builder_.CreateLoad(i64Ty_, outVar, "filter_out_idx");
        llvm::Value *dstPtr = builder_.CreateGEP(elemTy, newData, {outIdx}, "filter_dst_ptr");
        builder_.CreateStore(elem, dstPtr);
        llvm::Value *outNext = builder_.CreateAdd(outIdx, llvm::ConstantInt::get(i64Ty_, 1), "filter_out_next");
        builder_.CreateStore(outNext, outVar);
        builder_.CreateBr(nextBB);

        // Next: increment i
        builder_.SetInsertPoint(nextBB);
        llvm::Value *iCur2 = builder_.CreateLoad(i64Ty_, iVar, "fi_cur2");
        llvm::Value *iNext = builder_.CreateAdd(iCur2, llvm::ConstantInt::get(i64Ty_, 1), "fi_next");
        builder_.CreateStore(iNext, iVar);
        builder_.CreateBr(condBB);

        // End: set final length
        builder_.SetInsertPoint(endBB);
        llvm::Value *finalLen = builder_.CreateLoad(i64Ty_, outVar, "filter_final_len");
        llvm::Value *newLenPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 0, "filter_len_ptr");
        builder_.CreateStore(finalLen, newLenPtr);

        list_element_types_[newHeader] = elemTy;
        return newHeader;
    }

    // map(list, transform) → new list with transformed elements
    if (e->callee == "map") {
        if (e->args.size() != 2)
            throw std::runtime_error("map() takes exactly 2 arguments");

        llvm::Value *listVal = emitExpr(*e->args[0]);
        llvm::Value *lambdaVal = emitExpr(*e->args[1]);

        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy)
            throw std::runtime_error("map() requires a list as first argument");

        auto fnIt = fn_type_info_.find(lambdaVal);
        if (fnIt == fn_type_info_.end()) {
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(lambdaVal))
                fnIt = fn_type_info_.find(load->getPointerOperand());
        }
        if (fnIt == fn_type_info_.end())
            throw std::runtime_error("map() requires a function as second argument");
        auto &info = fnIt->second;

        if (info.paramTypes.size() != 1)
            throw std::runtime_error("map() transform must take exactly 1 argument");

        llvm::Type *outElemTy = info.returnType;

        // Read source list
        llvm::Value *srcLenPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 0, "map_src_len_ptr");
        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, srcLenPtr, "map_src_len");
        llvm::Value *srcDataPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 2, "map_src_data_field");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, srcDataPtr, "map_src_data");

        // Allocate new list
        llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *newHeader = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "map_header");

        uint64_t outElemSize = dl.getTypeAllocSize(outElemTy);
        llvm::Value *dataSize = builder_.CreateMul(srcLen, llvm::ConstantInt::get(i64Ty_, outElemSize), "map_data_size");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "map_data");

        // Set header fields
        llvm::Value *newLenPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 0, "map_len_ptr");
        builder_.CreateStore(srcLen, newLenPtr);
        llvm::Value *newCapPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 1, "map_cap_ptr");
        builder_.CreateStore(srcLen, newCapPtr);
        llvm::Value *newDataField = builder_.CreateStructGEP(listHeaderTy_, newHeader, 2, "map_data_field");
        builder_.CreateStore(newData, newDataField);

        // Loop: transform each element
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "map_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "map.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "map.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "map.end", fn_);

        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "mi");
        llvm::Value *cond = builder_.CreateICmpSLT(iVal, srcLen, "map_cond");
        builder_.CreateCondBr(cond, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "mi_cur");
        llvm::Value *srcElemPtr = builder_.CreateGEP(elemTy, srcData, {iCur}, "map_src_elem_ptr");
        llvm::Value *srcElem = builder_.CreateLoad(elemTy, srcElemPtr, "map_src_elem");
        llvm::Value *mapped = emitLambdaCall(lambdaVal, info, {srcElem}, "map_result");
        llvm::Value *dstElemPtr = builder_.CreateGEP(outElemTy, newData, {iCur}, "map_dst_elem_ptr");
        builder_.CreateStore(mapped, dstElemPtr);
        llvm::Value *iNext = builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1), "mi_next");
        builder_.CreateStore(iNext, iVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);
        list_element_types_[newHeader] = outElemTy;
        return newHeader;
    }

    // sort(list) or sort(list, comparator) → new sorted list
    if (e->callee == "sort") {
        if (e->args.size() < 1 || e->args.size() > 2)
            throw std::runtime_error("sort() takes 1 or 2 arguments");

        llvm::Value *listVal = emitExpr(*e->args[0]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy)
            throw std::runtime_error("sort() requires a list as first argument");

        bool hasComparator = (e->args.size() == 2);
        llvm::Value *compVal = nullptr;
        FnTypeInfo compInfo;
        if (hasComparator) {
            compVal = emitExpr(*e->args[1]);
            auto fnIt = fn_type_info_.find(compVal);
            if (fnIt == fn_type_info_.end()) {
                if (auto *load = llvm::dyn_cast<llvm::LoadInst>(compVal))
                    fnIt = fn_type_info_.find(load->getPointerOperand());
            }
            if (fnIt == fn_type_info_.end())
                throw std::runtime_error("sort() comparator must be a function");
            compInfo = fnIt->second;
            if (compInfo.paramTypes.size() != 2 || compInfo.returnType != i1Ty_)
                throw std::runtime_error("sort() comparator must take 2 arguments and return bool");
        }

        // Read source list
        llvm::Value *srcLenPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 0, "sort_src_len_ptr");
        llvm::Value *srcLen = builder_.CreateLoad(i64Ty_, srcLenPtr, "sort_src_len");
        llvm::Value *srcDataPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 2, "sort_src_data_field");
        llvm::Value *srcData = builder_.CreateLoad(ptrTy_, srcDataPtr, "sort_src_data");

        // Allocate new list and copy data
        llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        llvm::Value *newHeader = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "sort_header");

        uint64_t elemSz = dl.getTypeAllocSize(elemTy);
        llvm::Value *dataSize = builder_.CreateMul(srcLen, llvm::ConstantInt::get(i64Ty_, elemSz), "sort_data_size");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "sort_data");

        // memcpy source data to new data
        llvm::FunctionType *memcpyTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
        llvm::FunctionCallee memcpyFn = mod_->getOrInsertFunction("memcpy", memcpyTy);
        builder_.CreateCall(memcpyFn, {newData, srcData, dataSize});

        // Set header
        llvm::Value *newLenPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 0, "sort_len_ptr");
        builder_.CreateStore(srcLen, newLenPtr);
        llvm::Value *newCapPtr = builder_.CreateStructGEP(listHeaderTy_, newHeader, 1, "sort_cap_ptr");
        builder_.CreateStore(srcLen, newCapPtr);
        llvm::Value *newDataField = builder_.CreateStructGEP(listHeaderTy_, newHeader, 2, "sort_data_field");
        builder_.CreateStore(newData, newDataField);

        // Iterative Quick Sort with Hoare partition and median-of-three pivot
        // Helper lambda: emit less-than comparison IR (reused in 3 places)
        auto emitLessThan = [&](llvm::Value *a, llvm::Value *b) -> llvm::Value* {
            if (hasComparator) {
                return emitLambdaCall(compVal, compInfo, {a, b}, "sort_comp");
            } else if (elemTy == i64Ty_) {
                return builder_.CreateICmpSLT(a, b, "sort_lt");
            } else if (elemTy == f64Ty_) {
                return builder_.CreateFCmpOLT(a, b, "sort_lt");
            } else if (elemTy == ptrTy_) {
                auto strcmpTy = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_}, false);
                auto strcmpFn = mod_->getOrInsertFunction("strcmp", strcmpTy);
                llvm::Value *cmpResult = builder_.CreateCall(strcmpFn, {a, b}, "sort_strcmp");
                return builder_.CreateICmpSLT(cmpResult, llvm::ConstantInt::get(i32Ty_, 0), "sort_lt");
            } else {
                throw std::runtime_error("sort() does not support this element type");
            }
        };

        auto emitSwap = [&](llvm::Value *idxA, llvm::Value *idxB) {
            llvm::Value *pA = builder_.CreateGEP(elemTy, newData, {idxA}, "swap_ptr_a");
            llvm::Value *pB = builder_.CreateGEP(elemTy, newData, {idxB}, "swap_ptr_b");
            llvm::Value *vA = builder_.CreateLoad(elemTy, pA, "swap_val_a");
            llvm::Value *vB = builder_.CreateLoad(elemTy, pB, "swap_val_b");
            builder_.CreateStore(vB, pA);
            builder_.CreateStore(vA, pB);
        };

        llvm::Value *one = llvm::ConstantInt::get(i64Ty_, 1);
        llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
        llvm::Value *two = llvm::ConstantInt::get(i64Ty_, 2);

        // BasicBlocks
        llvm::BasicBlock *qsCheckLenBB = llvm::BasicBlock::Create(*ctx_, "qs.check.len", fn_);
        llvm::BasicBlock *qsInitBB = llvm::BasicBlock::Create(*ctx_, "qs.init", fn_);
        llvm::BasicBlock *qsLoopCondBB = llvm::BasicBlock::Create(*ctx_, "qs.loop.cond", fn_);
        llvm::BasicBlock *qsPopBB = llvm::BasicBlock::Create(*ctx_, "qs.pop", fn_);
        llvm::BasicBlock *qsCheckTrivialBB = llvm::BasicBlock::Create(*ctx_, "qs.check.trivial", fn_);
        llvm::BasicBlock *qsPivotBB = llvm::BasicBlock::Create(*ctx_, "qs.pivot", fn_);
        llvm::BasicBlock *qsPartInitBB = llvm::BasicBlock::Create(*ctx_, "qs.part.init", fn_);
        llvm::BasicBlock *qsAdvIBB = llvm::BasicBlock::Create(*ctx_, "qs.adv.i", fn_);
        llvm::BasicBlock *qsAdvJBB = llvm::BasicBlock::Create(*ctx_, "qs.adv.j", fn_);
        llvm::BasicBlock *qsPartCheckBB = llvm::BasicBlock::Create(*ctx_, "qs.part.check", fn_);
        llvm::BasicBlock *qsPartSwapBB = llvm::BasicBlock::Create(*ctx_, "qs.part.swap", fn_);
        llvm::BasicBlock *qsPushBB = llvm::BasicBlock::Create(*ctx_, "qs.push", fn_);
        llvm::BasicBlock *qsEndBB = llvm::BasicBlock::Create(*ctx_, "qs.end", fn_);
        llvm::BasicBlock *qsDoneBB = llvm::BasicBlock::Create(*ctx_, "qs.done", fn_);

        builder_.CreateBr(qsCheckLenBB);

        // qs.check.len: if len <= 1, skip sort
        builder_.SetInsertPoint(qsCheckLenBB);
        llvm::Value *lenLE1 = builder_.CreateICmpSLE(srcLen, one, "len_le_1");
        builder_.CreateCondBr(lenLE1, qsDoneBB, qsInitBB);

        // qs.init: allocate explicit stack, push (0, len-1)
        builder_.SetInsertPoint(qsInitBB);
        // Stack: array of i64 pairs, max 128 levels deep (push-smaller-first bounds depth to O(log n))
        llvm::Value *stackSize = llvm::ConstantInt::get(i64Ty_, 128 * 2 * 8);
        llvm::Value *stack = builder_.CreateCall(mallocFn, {stackSize}, "qs_stack");
        llvm::AllocaInst *stackTopVar = builder_.CreateAlloca(i64Ty_, nullptr, "qs_stack_top");
        builder_.CreateStore(zero, stackTopVar);
        // Alloca for partition indices (hoisted out of loop to avoid stack growth)
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "qs_i");
        llvm::AllocaInst *jVar = builder_.CreateAlloca(i64Ty_, nullptr, "qs_j");
        // Push initial range (0, len-1)
        llvm::Value *lenM1 = builder_.CreateSub(srcLen, one, "len_m1");
        // stack[0] = 0 (lo)
        llvm::Value *stackSlot0 = builder_.CreateGEP(i64Ty_, stack, {zero}, "stack_slot0");
        builder_.CreateStore(zero, stackSlot0);
        // stack[1] = len-1 (hi)
        llvm::Value *stackSlot1 = builder_.CreateGEP(i64Ty_, stack, {one}, "stack_slot1");
        builder_.CreateStore(lenM1, stackSlot1);
        builder_.CreateStore(two, stackTopVar); // stackTop = 2
        builder_.CreateBr(qsLoopCondBB);

        // qs.loop.cond: while stackTop > 0
        builder_.SetInsertPoint(qsLoopCondBB);
        llvm::Value *stackTop = builder_.CreateLoad(i64Ty_, stackTopVar, "stack_top");
        llvm::Value *stackNotEmpty = builder_.CreateICmpSGT(stackTop, zero, "stack_not_empty");
        builder_.CreateCondBr(stackNotEmpty, qsPopBB, qsEndBB);

        // qs.pop: pop (lo, hi) from stack
        builder_.SetInsertPoint(qsPopBB);
        llvm::Value *st1 = builder_.CreateLoad(i64Ty_, stackTopVar, "st1");
        llvm::Value *hiIdx = builder_.CreateSub(st1, one, "hi_idx");
        llvm::Value *loIdx = builder_.CreateSub(st1, two, "lo_idx");
        llvm::Value *loSlot = builder_.CreateGEP(i64Ty_, stack, {loIdx}, "lo_slot");
        llvm::Value *hiSlot = builder_.CreateGEP(i64Ty_, stack, {hiIdx}, "hi_slot");
        llvm::Value *lo = builder_.CreateLoad(i64Ty_, loSlot, "lo");
        llvm::Value *hi = builder_.CreateLoad(i64Ty_, hiSlot, "hi");
        llvm::Value *newST = builder_.CreateSub(st1, two, "new_st");
        builder_.CreateStore(newST, stackTopVar);
        builder_.CreateBr(qsCheckTrivialBB);

        // qs.check.trivial: if lo >= hi, skip
        builder_.SetInsertPoint(qsCheckTrivialBB);
        llvm::Value *loGeHi = builder_.CreateICmpSGE(lo, hi, "lo_ge_hi");
        builder_.CreateCondBr(loGeHi, qsLoopCondBB, qsPivotBB);

        // qs.pivot: median-of-three pivot selection
        builder_.SetInsertPoint(qsPivotBB);
        llvm::Value *mid = builder_.CreateAdd(lo, builder_.CreateSDiv(builder_.CreateSub(hi, lo), two), "mid");
        // Load arr[lo], arr[mid], arr[hi]
        llvm::Value *ptrLo = builder_.CreateGEP(elemTy, newData, {lo}, "ptr_lo");
        llvm::Value *ptrMid = builder_.CreateGEP(elemTy, newData, {mid}, "ptr_mid");
        llvm::Value *ptrHi = builder_.CreateGEP(elemTy, newData, {hi}, "ptr_hi");
        llvm::Value *valLo = builder_.CreateLoad(elemTy, ptrLo, "val_lo");
        llvm::Value *valMid = builder_.CreateLoad(elemTy, ptrMid, "val_mid");
        llvm::Value *valHi = builder_.CreateLoad(elemTy, ptrHi, "val_hi");
        // Sort the three: if arr[lo] > arr[mid], swap
        llvm::BasicBlock *qsMot1BB = llvm::BasicBlock::Create(*ctx_, "qs.mot1", fn_);
        llvm::BasicBlock *qsMot2BB = llvm::BasicBlock::Create(*ctx_, "qs.mot2", fn_);
        llvm::BasicBlock *qsMot3BB = llvm::BasicBlock::Create(*ctx_, "qs.mot3", fn_);
        llvm::BasicBlock *qsMotDoneBB = llvm::BasicBlock::Create(*ctx_, "qs.mot.done", fn_);
        llvm::Value *loGtMid = emitLessThan(valMid, valLo); // mid < lo => lo > mid
        builder_.CreateCondBr(loGtMid, qsMot1BB, qsMot2BB);

        builder_.SetInsertPoint(qsMot1BB);
        emitSwap(lo, mid);
        builder_.CreateBr(qsMot2BB);

        // if arr[lo] > arr[hi], swap
        builder_.SetInsertPoint(qsMot2BB);
        llvm::Value *valLo2 = builder_.CreateLoad(elemTy, ptrLo, "val_lo2");
        llvm::Value *valHi2 = builder_.CreateLoad(elemTy, ptrHi, "val_hi2");
        llvm::Value *loGtHi = emitLessThan(valHi2, valLo2);
        builder_.CreateCondBr(loGtHi, qsMot3BB, qsMotDoneBB);

        builder_.SetInsertPoint(qsMot3BB);
        emitSwap(lo, hi);
        builder_.CreateBr(qsMotDoneBB);

        // if arr[mid] > arr[hi], swap
        builder_.SetInsertPoint(qsMotDoneBB);
        llvm::BasicBlock *qsMot4BB = llvm::BasicBlock::Create(*ctx_, "qs.mot4", fn_);
        llvm::BasicBlock *qsMot5BB = llvm::BasicBlock::Create(*ctx_, "qs.mot5", fn_);
        llvm::Value *valMid3 = builder_.CreateLoad(elemTy, ptrMid, "val_mid3");
        llvm::Value *valHi3 = builder_.CreateLoad(elemTy, ptrHi, "val_hi3");
        llvm::Value *midGtHi = emitLessThan(valHi3, valMid3);
        builder_.CreateCondBr(midGtHi, qsMot4BB, qsMot5BB);

        builder_.SetInsertPoint(qsMot4BB);
        emitSwap(mid, hi);
        builder_.CreateBr(qsMot5BB);

        // Pivot = arr[mid], swap mid to lo+1 position for Hoare partition
        builder_.SetInsertPoint(qsMot5BB);
        // Use arr[mid] as pivot value
        llvm::Value *pivotVal = builder_.CreateLoad(elemTy, ptrMid, "pivot_val");
        builder_.CreateBr(qsPartInitBB);

        // qs.part.init: i = lo - 1, j = hi + 1
        builder_.SetInsertPoint(qsPartInitBB);
        builder_.CreateStore(builder_.CreateSub(lo, one, "lo_m1"), iVar);
        builder_.CreateStore(builder_.CreateAdd(hi, one, "hi_p1"), jVar);
        builder_.CreateBr(qsAdvIBB);

        // qs.adv.i: do { i++ } while arr[i] < pivot
        builder_.SetInsertPoint(qsAdvIBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "i_cur");
        llvm::Value *iNext = builder_.CreateAdd(iCur, one, "i_next");
        builder_.CreateStore(iNext, iVar);
        llvm::Value *ptrI = builder_.CreateGEP(elemTy, newData, {iNext}, "ptr_i");
        llvm::Value *valI = builder_.CreateLoad(elemTy, ptrI, "val_i");
        llvm::Value *iLtPivot = emitLessThan(valI, pivotVal);
        builder_.CreateCondBr(iLtPivot, qsAdvIBB, qsAdvJBB);

        // qs.adv.j: do { j-- } while arr[j] > pivot (i.e., pivot < arr[j])
        builder_.SetInsertPoint(qsAdvJBB);
        llvm::Value *jCur = builder_.CreateLoad(i64Ty_, jVar, "j_cur");
        llvm::Value *jNext = builder_.CreateSub(jCur, one, "j_next");
        builder_.CreateStore(jNext, jVar);
        llvm::Value *ptrJ = builder_.CreateGEP(elemTy, newData, {jNext}, "ptr_j");
        llvm::Value *valJ = builder_.CreateLoad(elemTy, ptrJ, "val_j");
        llvm::Value *pivotLtJ = emitLessThan(pivotVal, valJ);
        builder_.CreateCondBr(pivotLtJ, qsAdvJBB, qsPartCheckBB);

        // qs.part.check: if i >= j, partition done
        builder_.SetInsertPoint(qsPartCheckBB);
        llvm::Value *iFinal = builder_.CreateLoad(i64Ty_, iVar, "i_final");
        llvm::Value *jFinal = builder_.CreateLoad(i64Ty_, jVar, "j_final");
        llvm::Value *iGeJ = builder_.CreateICmpSGE(iFinal, jFinal, "i_ge_j");
        builder_.CreateCondBr(iGeJ, qsPushBB, qsPartSwapBB);

        // qs.part.swap: swap arr[i] and arr[j], continue partitioning
        builder_.SetInsertPoint(qsPartSwapBB);
        llvm::Value *iSwap = builder_.CreateLoad(i64Ty_, iVar, "i_swap");
        llvm::Value *jSwap = builder_.CreateLoad(i64Ty_, jVar, "j_swap");
        emitSwap(iSwap, jSwap);
        builder_.CreateBr(qsAdvIBB);

        // qs.push: push larger sub-range first (bounds stack depth to O(log n))
        builder_.SetInsertPoint(qsPushBB);
        llvm::Value *partJ = builder_.CreateLoad(i64Ty_, jVar, "part_j");
        llvm::Value *partJp1 = builder_.CreateAdd(partJ, one, "part_j_p1");
        // Compute sizes: leftSize = partJ - lo, rightSize = hi - partJp1
        llvm::Value *leftSize = builder_.CreateSub(partJ, lo, "left_size");
        llvm::Value *rightSize = builder_.CreateSub(hi, partJp1, "right_size");
        // Push larger sub-range first (processed last), smaller second (processed next)
        llvm::BasicBlock *qsPushLeftFirstBB = llvm::BasicBlock::Create(*ctx_, "qs.push.left_first", fn_);
        llvm::BasicBlock *qsPushRightFirstBB = llvm::BasicBlock::Create(*ctx_, "qs.push.right_first", fn_);
        llvm::BasicBlock *qsPushDoneBB = llvm::BasicBlock::Create(*ctx_, "qs.push.done", fn_);
        llvm::Value *leftGtRight = builder_.CreateICmpSGT(leftSize, rightSize, "left_gt_right");
        builder_.CreateCondBr(leftGtRight, qsPushLeftFirstBB, qsPushRightFirstBB);

        // Left is larger: push (lo, partJ) first, then (partJp1, hi)
        builder_.SetInsertPoint(qsPushLeftFirstBB);
        {
            llvm::Value *st = builder_.CreateLoad(i64Ty_, stackTopVar, "st_lf");
            llvm::Value *s0 = builder_.CreateGEP(i64Ty_, stack, {st}, "s0_lf");
            builder_.CreateStore(lo, s0);
            llvm::Value *st1 = builder_.CreateAdd(st, one, "st1_lf");
            llvm::Value *s1 = builder_.CreateGEP(i64Ty_, stack, {st1}, "s1_lf");
            builder_.CreateStore(partJ, s1);
            llvm::Value *st2 = builder_.CreateAdd(st, two, "st2_lf");
            llvm::Value *s2 = builder_.CreateGEP(i64Ty_, stack, {st2}, "s2_lf");
            builder_.CreateStore(partJp1, s2);
            llvm::Value *st3 = builder_.CreateAdd(st2, one, "st3_lf");
            llvm::Value *s3 = builder_.CreateGEP(i64Ty_, stack, {st3}, "s3_lf");
            builder_.CreateStore(hi, s3);
            llvm::Value *st4 = builder_.CreateAdd(st2, two, "st4_lf");
            builder_.CreateStore(st4, stackTopVar);
        }
        builder_.CreateBr(qsPushDoneBB);

        // Right is larger: push (partJp1, hi) first, then (lo, partJ)
        builder_.SetInsertPoint(qsPushRightFirstBB);
        {
            llvm::Value *st = builder_.CreateLoad(i64Ty_, stackTopVar, "st_rf");
            llvm::Value *s0 = builder_.CreateGEP(i64Ty_, stack, {st}, "s0_rf");
            builder_.CreateStore(partJp1, s0);
            llvm::Value *st1 = builder_.CreateAdd(st, one, "st1_rf");
            llvm::Value *s1 = builder_.CreateGEP(i64Ty_, stack, {st1}, "s1_rf");
            builder_.CreateStore(hi, s1);
            llvm::Value *st2 = builder_.CreateAdd(st, two, "st2_rf");
            llvm::Value *s2 = builder_.CreateGEP(i64Ty_, stack, {st2}, "s2_rf");
            builder_.CreateStore(lo, s2);
            llvm::Value *st3 = builder_.CreateAdd(st2, one, "st3_rf");
            llvm::Value *s3 = builder_.CreateGEP(i64Ty_, stack, {st3}, "s3_rf");
            builder_.CreateStore(partJ, s3);
            llvm::Value *st4 = builder_.CreateAdd(st2, two, "st4_rf");
            builder_.CreateStore(st4, stackTopVar);
        }
        builder_.CreateBr(qsPushDoneBB);

        builder_.SetInsertPoint(qsPushDoneBB);
        builder_.CreateBr(qsLoopCondBB);

        // qs.end: free stack
        builder_.SetInsertPoint(qsEndBB);
        llvm::FunctionType *freeTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
        llvm::FunctionCallee freeFn = mod_->getOrInsertFunction("free", freeTy);
        builder_.CreateCall(freeFn, {stack});
        builder_.CreateBr(qsDoneBB);

        // qs.done: return sorted list
        builder_.SetInsertPoint(qsDoneBB);
        list_element_types_[newHeader] = elemTy;
        return newHeader;
    }

    auto sit = struct_types_.find(e->callee);
    if (sit != struct_types_.end()) {
        if (deprecated_types_.count(e->callee))
            emitDeprecationWarning(e->callee);
        return emitStructConstructor(sit->second, e->callee, e->args);
    }

    // Try indirect call via variable (function pointer / lambda)
    if (llvm::AllocaInst *varPtr = findVar(e->callee)) {
        auto fnIt = fn_type_info_.find(varPtr);
        if (fnIt != fn_type_info_.end()) {
            auto &info = fnIt->second;

            // Emit arguments
            std::vector<llvm::Value*> argVals;
            for (auto &arg : e->args)
                argVals.push_back(emitExpr(*arg));

            if (argVals.size() != info.paramTypes.size())
                throw std::runtime_error(
                    "lambda call: expected " + std::to_string(info.paramTypes.size()) +
                    " arguments, got " + std::to_string(argVals.size()));

            for (size_t i = 0; i < argVals.size(); ++i) {
                if (argVals[i]->getType() != info.paramTypes[i])
                    throw std::runtime_error(
                        "lambda call: argument " + std::to_string(i) + " type mismatch");
            }

            llvm::Value *loaded = builder_.CreateLoad(ptrTy_, varPtr, e->callee + ".fn");
            return emitLambdaCall(loaded, info, argVals, "indirect_call");
        }
    }

    return emitUserFnCall(e->callee, e->args);
}

// ===== Lambda call helper =====

llvm::Value *CodeGen::emitLambdaCall(llvm::Value *lambdaVal, const FnTypeInfo &info,
                                      std::vector<llvm::Value*> args, const std::string &name) {
    if (info.capturedVars.empty()) {
        // Simple function pointer call
        llvm::FunctionType *ft = llvm::FunctionType::get(
            info.returnType, info.paramTypes, false);
        if (info.returnType->isVoidTy())
            return builder_.CreateCall(ft, lambdaVal, args);
        return builder_.CreateCall(ft, lambdaVal, args, name);
    } else {
        // Closure call: load fn_ptr and captured values from closure struct
        std::vector<llvm::Type*> closureFields;
        closureFields.push_back(ptrTy_);  // fn ptr slot
        for (auto *ct : info.capturedTypes)
            closureFields.push_back(ct);
        llvm::StructType *closureTy = llvm::StructType::get(*ctx_, closureFields);

        llvm::Value *fnPtrField = builder_.CreateStructGEP(
            closureTy, lambdaVal, 0, "lcall.fn_ptr");
        llvm::Value *fnPtr = builder_.CreateLoad(ptrTy_, fnPtrField, "lcall.fn");

        // Build full arg list: user args + captured values
        std::vector<llvm::Value*> fullArgs = args;
        std::vector<llvm::Type*> allParamTypes = info.paramTypes;
        for (size_t i = 0; i < info.capturedTypes.size(); ++i) {
            llvm::Value *capField = builder_.CreateStructGEP(
                closureTy, lambdaVal, i + 1, "lcall.cap." + std::to_string(i));
            llvm::Value *capVal = builder_.CreateLoad(
                info.capturedTypes[i], capField, "lcall.cap_val." + std::to_string(i));
            fullArgs.push_back(capVal);
            allParamTypes.push_back(info.capturedTypes[i]);
        }

        llvm::FunctionType *ft = llvm::FunctionType::get(
            info.returnType, allParamTypes, false);
        if (info.returnType->isVoidTy())
            return builder_.CreateCall(ft, fnPtr, fullArgs);
        return builder_.CreateCall(ft, fnPtr, fullArgs, name);
    }
}
