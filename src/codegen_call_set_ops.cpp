#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"


namespace ry {

// ===== Builtin Set Ops =====

// Shared helper: check all elements of iterSet exist in lookupSet.
// Delegates to emitSetElementLookup for membership test; that function
// selects hash-based or linear-scan lookup based on element type.
llvm::Value *CodeGen::emitSubsetCheck(llvm::Value *iterSet, llvm::Value *lookupSet,
                                       const std::string &prefix) {
    llvm::Type *elemTy = getSetElementType(iterSet);
    if (!elemTy) return nullptr;
    llvm::Type *elemTy2 = getSetElementType(lookupSet);
    if (!elemTy2 || elemTy2 != elemTy)
        codegenError(prefix + "() requires two sets with the same element type");

    const std::string elemName = getSetElemName(iterSet);

    auto sf = loadSetHeader(iterSet, prefix);

    llvm::AllocaInst *resultVar = builder_.CreateAlloca(i1Ty_, nullptr, prefix + "_result");
    builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 1), resultVar);

    llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, prefix + "_i");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, prefix + ".cond", fn_);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, prefix + ".body", fn_);
    llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, prefix + ".end",  fn_);
    builder_.CreateBr(condBB);
    builder_.SetInsertPoint(condBB);
    llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, prefix + "_ci");
    builder_.CreateCondBr(builder_.CreateICmpSLT(i, sf.len), bodyBB, endBB);
    builder_.SetInsertPoint(bodyBB);
    llvm::Value *ep = builder_.CreateGEP(elemTy, sf.elems, {i}, prefix + "_ep");
    llvm::Value *ev = builder_.CreateLoad(elemTy, ep, prefix + "_ev");

    // Pointer elements lose ValueMetadata on GEP load; rebuild from the
    // parent set's stored type name before the lookup. Without this,
    // emitSetElementLookup falls into hash-by-ptr-as-C-string and treats
    // distinct nested collections as duplicates.
    if (!elemName.empty())
        propagateTypeMeta(elemName, ev);

    llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, prefix + ".fail", fn_);
    llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, prefix + ".next", fn_);

    llvm::Value *found = emitSetElementLookup(lookupSet, ev, elemTy, elemName);
    llvm::Value *notFound = builder_.CreateICmpSLT(found, llvm::ConstantInt::get(i64Ty_, 0), prefix + "_nf");
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

llvm::Value *CodeGen::emitSetUnionCore(llvm::Value *set1, llvm::Value *set2,
                                        llvm::Type *elemTy) {
    const std::string elemName = getSetElemName(set1);
    // Create new set with all elements from set1, then add elements from set2
    auto sf1 = loadSetHeader(set1, "u1");
    auto sf2 = loadSetHeader(set2, "u2");

    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t elemSize = dl.getTypeAllocSize(elemTy);
    auto mallocFn = getStdlibMalloc();

    // Allocate max possible size (len1 + len2)
    llvm::Value *maxLen = builder_.CreateAdd(sf1.len, sf2.len, "u_max_len");
    llvm::Value *newHeader = emitArcAllocCollectionHeader(setHeaderTy_);
    llvm::Value *dataSize = builder_.CreateMul(maxLen, llvm::ConstantInt::get(i64Ty_, elemSize), "u_ds");
    llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "u_data");

    // Copy all of set1
    auto memcpyFn = getStdlibMemcpy();
    llvm::Value *copy1Size = builder_.CreateMul(sf1.len, llvm::ConstantInt::get(i64Ty_, elemSize), "u_copy1_size");
    builder_.CreateCall(memcpyFn, {newData, sf1.elems, copy1Size});

    // Init header with len1
    storeSetHeaderFields(newHeader, sf1.len, maxLen, newData);
    llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, newHeader, 0, "u_len_ptr");

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
        if (!elemName.empty()) propagateTypeMeta(elemName, ev);
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
        if (!elemName.empty())
            propagateTypeMeta(elemName, ev);

        // Check if already in new set
        llvm::Value *lookupIdx = emitSetElementLookup(newHeader, ev, elemTy, elemName);
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

    setTypeMeta(TypeMeta::SetElem, newHeader, elemTy);
    if (!elemName.empty())
        getOrCreateMeta(newHeader).set_elem_type_name = elemName;
    return newHeader;
}

// union(set1, set2)
llvm::Value *CodeGen::emitSetOp_union(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;
    llvm::Value *set1 = emitExpr(*e.args[0]);
    llvm::Value *set2 = emitExpr(*e.args[1]);
    llvm::Type *elemTy = getSetElementType(set1);
    if (!elemTy)
        return nullptr;
    llvm::Type *elemTy2 = getSetElementType(set2);
    if (!elemTy2 || elemTy2 != elemTy)
        codegenError("union() requires two sets with the same element type");
    return emitSetUnionCore(set1, set2, elemTy);
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
        const std::string elemName = getSetElemName(set1);
        auto sf = loadSetHeader(set1, "is");

        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        auto mallocFn = getStdlibMalloc();

        llvm::Value *newHeader = emitArcAllocCollectionHeader(setHeaderTy_);
        llvm::Value *dataSize = builder_.CreateMul(sf.len, llvm::ConstantInt::get(i64Ty_, elemSize), "is_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "is_data");

        storeSetHeaderFields(newHeader, llvm::ConstantInt::get(i64Ty_, 0), sf.len, newData);
        llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, newHeader, 0, "is_len_ptr");
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
        if (!elemName.empty())
            propagateTypeMeta(elemName, ev);

        llvm::Value *inSet2 = emitSetElementLookup(set2, ev, elemTy, elemName);
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

        setTypeMeta(TypeMeta::SetElem, newHeader, elemTy);
        if (!elemName.empty())
            getOrCreateMeta(newHeader).set_elem_type_name = elemName;
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
        const std::string elemName = getSetElemName(set1);
        auto sf = loadSetHeader(set1, "df");

        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        auto mallocFn = getStdlibMalloc();

        llvm::Value *newHeader = emitArcAllocCollectionHeader(setHeaderTy_);
        llvm::Value *dataSize = builder_.CreateMul(sf.len, llvm::ConstantInt::get(i64Ty_, elemSize), "df_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "df_data");

        storeSetHeaderFields(newHeader, llvm::ConstantInt::get(i64Ty_, 0), sf.len, newData);
        llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, newHeader, 0, "df_len_ptr");
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
        if (!elemName.empty())
            propagateTypeMeta(elemName, ev);

        llvm::Value *inSet2 = emitSetElementLookup(set2, ev, elemTy, elemName);
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

        setTypeMeta(TypeMeta::SetElem, newHeader, elemTy);
        if (!elemName.empty())
            getOrCreateMeta(newHeader).set_elem_type_name = elemName;
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
        const std::string elemName = getSetElemName(set1);
        auto sf1 = loadSetHeader(set1, "sd1");
        auto sf2 = loadSetHeader(set2, "sd2");

        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        auto mallocFn = getStdlibMalloc();

        llvm::Value *maxLen = builder_.CreateAdd(sf1.len, sf2.len, "sd_max_len");
        llvm::Value *newHeader = emitArcAllocCollectionHeader(setHeaderTy_);
        llvm::Value *dataSize = builder_.CreateMul(maxLen, llvm::ConstantInt::get(i64Ty_, elemSize), "sd_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "sd_data");

        storeSetHeaderFields(newHeader, llvm::ConstantInt::get(i64Ty_, 0), maxLen, newData);
        llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, newHeader, 0, "sd_len_ptr");
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
            if (!elemName.empty())
                propagateTypeMeta(elemName, eVal);
            llvm::Value *inOther = emitSetElementLookup(otherSet, eVal, elemTy, elemName);
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

        setTypeMeta(TypeMeta::SetElem, newHeader, elemTy);
        if (!elemName.empty())
            getOrCreateMeta(newHeader).set_elem_type_name = elemName;
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

} // namespace ry
