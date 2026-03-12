#include "ry/codegen.hpp"
#include <stdexcept>

// ===== CallExpr =====

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<CallExpr> &e) {
    // range(n) or range(start, end) → list[int]
    if (e->callee == "range") {
        if (e->args.size() < 1 || e->args.size() > 2)
            throw std::runtime_error("range() takes 1 or 2 arguments");

        llvm::Value *start, *end;
        if (e->args.size() == 1) {
            start = llvm::ConstantInt::get(i64Ty_, 0);
            end = emitExpr(*e->args[0]);
        } else {
            start = emitExpr(*e->args[0]);
            end = emitExpr(*e->args[1]);
        }

        // count = max(0, end - start)
        llvm::Value *diff = builder_.CreateSub(end, start, "range_diff");
        llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
        llvm::Value *isPos = builder_.CreateICmpSGT(diff, zero, "is_pos");
        llvm::Value *count = builder_.CreateSelect(isPos, diff, zero, "range_count");

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

        // Fill data with start..end using a loop
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
        llvm::Value *val = builder_.CreateAdd(start, iCur, "range_val");
        llvm::Value *elemPtr = builder_.CreateGEP(i64Ty_, dataPtr, {iCur}, "range_elem_ptr");
        builder_.CreateStore(val, elemPtr);
        llvm::Value *iNext = builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1), "ri_next");
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

            const llvm::DataLayout &dl2 = mod_->getDataLayout();
            uint64_t elemSize2 = dl2.getTypeAllocSize(elemTy);
            llvm::Value *idxPlusOne = builder_.CreateAdd(idx, llvm::ConstantInt::get(i64Ty_, 1), "idx_plus_one");
            llvm::Value *dst = builder_.CreateGEP(elemTy, elemsPtr, {idx}, "remove_dst");
            llvm::Value *src = builder_.CreateGEP(elemTy, elemsPtr, {idxPlusOne}, "remove_src");
            llvm::Value *remaining = builder_.CreateSub(length, idxPlusOne, "remaining");
            llvm::Value *moveSize = builder_.CreateMul(remaining, llvm::ConstantInt::get(i64Ty_, elemSize2), "move_size");

            llvm::FunctionType *memmoveTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
            llvm::FunctionCallee memmoveFn = mod_->getOrInsertFunction("memmove", memmoveTy);
            builder_.CreateCall(memmoveFn, {dst, src, moveSize});

            llvm::Value *newLen = builder_.CreateSub(length, llvm::ConstantInt::get(i64Ty_, 1), "new_len");
            builder_.CreateStore(newLen, lenPtr);
            builder_.CreateBr(endBB);

            builder_.SetInsertPoint(endBB);
            return llvm::ConstantInt::get(i64Ty_, 0);
        }
        // Not a set — fall through to user function resolution
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

    auto sit = struct_types_.find(e->callee);
    if (sit != struct_types_.end())
        return emitStructConstructor(sit->second, e->callee, e->args);

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

            if (info.capturedVars.empty()) {
                // Simple function pointer call
                llvm::FunctionType *ft = llvm::FunctionType::get(
                    info.returnType, info.paramTypes, false);
                if (info.returnType->isVoidTy())
                    return builder_.CreateCall(ft, loaded, argVals);
                return builder_.CreateCall(ft, loaded, argVals, "indirect_call");
            } else {
                // Closure call: load fn_ptr and captured values from closure struct
                std::vector<llvm::Type*> closureFields;
                closureFields.push_back(ptrTy_);  // fn ptr slot
                for (auto *ct : info.capturedTypes)
                    closureFields.push_back(ct);
                llvm::StructType *closureTy = llvm::StructType::get(*ctx_, closureFields);

                llvm::Value *fnPtrField = builder_.CreateStructGEP(
                    closureTy, loaded, 0, "clos.fn_ptr");
                llvm::Value *fnPtr = builder_.CreateLoad(ptrTy_, fnPtrField, "clos.fn");

                // Build full arg list: user args + captured values
                std::vector<llvm::Value*> fullArgs = argVals;
                std::vector<llvm::Type*> allParamTypes = info.paramTypes;
                for (size_t i = 0; i < info.capturedTypes.size(); ++i) {
                    llvm::Value *capField = builder_.CreateStructGEP(
                        closureTy, loaded, i + 1, "clos.cap." + std::to_string(i));
                    llvm::Value *capVal = builder_.CreateLoad(
                        info.capturedTypes[i], capField, "clos.cap_val." + std::to_string(i));
                    fullArgs.push_back(capVal);
                    allParamTypes.push_back(info.capturedTypes[i]);
                }

                llvm::FunctionType *ft = llvm::FunctionType::get(
                    info.returnType, allParamTypes, false);
                if (info.returnType->isVoidTy())
                    return builder_.CreateCall(ft, fnPtr, fullArgs);
                return builder_.CreateCall(ft, fnPtr, fullArgs, "closure_call");
            }
        }
    }

    return emitUserFnCall(e->callee, e->args);
}
