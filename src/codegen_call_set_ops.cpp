#include "ry/codegen.hpp"
#include "ry/diagnostic/diagnostic.hpp"


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

    // #2101 ([C] = (ii) boundary move): the subset loop scaffold crosses via
    // generic primitives. emitSetElementLookup (already migrated) does the
    // membership test; propagateTypeMeta stays C++-side (type meta does not
    // cross — the #2100 precedent), and loadSetHeader's header read is a
    // separate header-load capability deferred as follow-on.
    llvm::Value *resultVar = emitAlloca(i1Ty_, (prefix + "_result").c_str());
    emitStore(emitConstInt(i1Ty_, 1), resultVar);

    llvm::Value *iVar = emitAlloca(i64Ty_, (prefix + "_i").c_str());
    emitStore(emitConstInt(i64Ty_, 0), iVar);
    llvm::BasicBlock *condBB = createBBInFn((prefix + ".cond").c_str(), fn_);
    llvm::BasicBlock *bodyBB = createBBInFn((prefix + ".body").c_str(), fn_);
    llvm::BasicBlock *endBB  = createBBInFn((prefix + ".end").c_str(), fn_);
    emitBranchUncond(condBB);
    builder_.SetInsertPoint(condBB);
    llvm::Value *i = emitLoad(i64Ty_, iVar, (prefix + "_ci").c_str());
    emitBranchCond(emitICmpSLT(i, sf.len, ""), bodyBB, endBB);
    builder_.SetInsertPoint(bodyBB);
    llvm::Value *ep = emitGEP(elemTy, sf.elems, i, (prefix + "_ep").c_str());
    llvm::Value *ev = emitLoad(elemTy, ep, (prefix + "_ev").c_str());

    // Pointer elements lose ValueMetadata on GEP load; rebuild from the
    // parent set's stored type name before the lookup. Without this,
    // emitSetElementLookup falls into hash-by-ptr-as-C-string and treats
    // distinct nested collections as duplicates.
    if (!elemName.empty())
        propagateTypeMeta(elemName, ev);

    llvm::BasicBlock *failBB = createBBInFn((prefix + ".fail").c_str(), fn_);
    llvm::BasicBlock *nextBB = createBBInFn((prefix + ".next").c_str(), fn_);

    llvm::Value *found = emitSetElementLookup(lookupSet, ev, elemTy, elemName);
    llvm::Value *notFound = emitICmpSLT(found, emitConstInt(i64Ty_, 0), (prefix + "_nf").c_str());
    emitBranchCond(notFound, failBB, nextBB);

    builder_.SetInsertPoint(failBB);
    emitStore(emitConstInt(i1Ty_, 0), resultVar);
    emitBranchUncond(endBB);
    builder_.SetInsertPoint(nextBB);
    emitStore(emitAdd(i, emitConstInt(i64Ty_, 1), ""), iVar);
    emitBranchUncond(condBB);
    builder_.SetInsertPoint(endBB);

    return emitLoad(i1Ty_, resultVar, (prefix + "_result").c_str());
}

llvm::Value *CodeGen::emitBuiltinSetOps(const CallExpr &e) {
    using Handler = llvm::Value *(CodeGen::*)(const CallExpr &);
    static const std::unordered_map<std::string, Handler> dispatch = {
        {"union",                &CodeGen::emitSetOp_union},
        {"intersection",         &CodeGen::emitSetOp_intersection},
        {"difference",           &CodeGen::emitSetOp_difference},
        {"symmetricDifference",  &CodeGen::emitSetOp_symmetric_difference},
        {"isSubset",             &CodeGen::emitSetOp_is_subset},
        {"isSuperset",           &CodeGen::emitSetOp_is_superset},
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
        llvm::BasicBlock *rCondBB = createBB("u.rehash.cond");
        llvm::BasicBlock *rBodyBB = createBB("u.rehash.body");
        llvm::BasicBlock *rEndBB = createBB("u.rehash.end");
        emitBranchUncond(rCondBB);
        builder_.SetInsertPoint(rCondBB);
        llvm::Value *ri = builder_.CreateLoad(i64Ty_, iVar, "u_ri");
        emitBranchCond(builder_.CreateICmpSLT(ri, sf1.len), rBodyBB, rEndBB);
        builder_.SetInsertPoint(rBodyBB);
        llvm::Value *ep = builder_.CreateGEP(elemTy, newData, {ri}, "u_rehash_ep");
        llvm::Value *ev = builder_.CreateLoad(elemTy, ep, "u_rehash_ev");
        if (!elemName.empty()) propagateTypeMeta(elemName, ev);
        emitBucketInsertAndRehashCheck(newHeader, setHeaderTy_, kSetLayout.lenIdx, kSetLayout.bucketCountIdx, kSetLayout.bucketsPtrIdx, ev, elemTy, ri);
        builder_.CreateStore(builder_.CreateAdd(ri, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        emitBranchUncond(rCondBB);
        builder_.SetInsertPoint(rEndBB);
    }

    // Add elements from set2 that are not in set1
    {
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "u_i2");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
        llvm::BasicBlock *condBB = createBB("u.add.cond");
        llvm::BasicBlock *bodyBB = createBB("u.add.body");
        llvm::BasicBlock *endBB = createBB("u.add.end");
        emitBranchUncond(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "u_ci");
        emitBranchCond(builder_.CreateICmpSLT(i, sf2.len), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *ep = builder_.CreateGEP(elemTy, sf2.elems, {i}, "u_ep2");
        llvm::Value *ev = builder_.CreateLoad(elemTy, ep, "u_ev2");
        if (!elemName.empty())
            propagateTypeMeta(elemName, ev);

        // Check if already in new set
        llvm::Value *lookupIdx = emitSetElementLookup(newHeader, ev, elemTy, elemName);
        llvm::Value *notFound = builder_.CreateICmpSLT(lookupIdx, llvm::ConstantInt::get(i64Ty_, 0), "u_not_found");
        llvm::BasicBlock *addBB = createBB("u.add.do");
        llvm::BasicBlock *nextBB = createBB("u.add.next");
        emitBranchCond(notFound, addBB, nextBB);

        builder_.SetInsertPoint(addBB);
        llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "u_cur_len");
        llvm::Value *storePtr = builder_.CreateGEP(elemTy, newData, {curLen}, "u_store_ptr");
        builder_.CreateStore(ev, storePtr);
        emitBucketInsertAndRehashCheck(newHeader, setHeaderTy_, kSetLayout.lenIdx, kSetLayout.bucketCountIdx, kSetLayout.bucketsPtrIdx, ev, elemTy, curLen);
        builder_.CreateStore(builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1)), lenPtr);
        emitBranchUncond(nextBB);

        builder_.SetInsertPoint(nextBB);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        emitBranchUncond(condBB);
        builder_.SetInsertPoint(endBB);
    }

    setTypeMeta(TypeMeta::SetElem, newHeader, elemTy);
    propagateMeta(set1, newHeader);
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
        llvm::BasicBlock *condBB = createBB("is.cond");
        llvm::BasicBlock *bodyBB = createBB("is.body");
        llvm::BasicBlock *endBB = createBB("is.end");
        emitBranchUncond(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "is_ci");
        emitBranchCond(builder_.CreateICmpSLT(i, sf.len), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *ep = builder_.CreateGEP(elemTy, sf.elems, {i}, "is_ep");
        llvm::Value *ev = builder_.CreateLoad(elemTy, ep, "is_ev");
        if (!elemName.empty())
            propagateTypeMeta(elemName, ev);

        llvm::Value *inSet2 = emitSetElementLookup(set2, ev, elemTy, elemName);
        llvm::Value *found = builder_.CreateICmpSGE(inSet2, llvm::ConstantInt::get(i64Ty_, 0), "is_found");
        llvm::BasicBlock *addBB = createBB("is.add");
        llvm::BasicBlock *nextBB = createBB("is.next");
        emitBranchCond(found, addBB, nextBB);

        builder_.SetInsertPoint(addBB);
        llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "is_cur_len");
        llvm::Value *storePtr = builder_.CreateGEP(elemTy, newData, {curLen}, "is_store_ptr");
        builder_.CreateStore(ev, storePtr);
        emitBucketInsertAndRehashCheck(newHeader, setHeaderTy_, kSetLayout.lenIdx, kSetLayout.bucketCountIdx, kSetLayout.bucketsPtrIdx, ev, elemTy, curLen);
        builder_.CreateStore(builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1)), lenPtr);
        emitBranchUncond(nextBB);

        builder_.SetInsertPoint(nextBB);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        emitBranchUncond(condBB);
        builder_.SetInsertPoint(endBB);

        setTypeMeta(TypeMeta::SetElem, newHeader, elemTy);
        propagateMeta(set1, newHeader);
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
        llvm::BasicBlock *condBB = createBB("df.cond");
        llvm::BasicBlock *bodyBB = createBB("df.body");
        llvm::BasicBlock *endBB = createBB("df.end");
        emitBranchUncond(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "df_ci");
        emitBranchCond(builder_.CreateICmpSLT(i, sf.len), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *ep = builder_.CreateGEP(elemTy, sf.elems, {i}, "df_ep");
        llvm::Value *ev = builder_.CreateLoad(elemTy, ep, "df_ev");
        if (!elemName.empty())
            propagateTypeMeta(elemName, ev);

        llvm::Value *inSet2 = emitSetElementLookup(set2, ev, elemTy, elemName);
        llvm::Value *notFound = builder_.CreateICmpSLT(inSet2, llvm::ConstantInt::get(i64Ty_, 0), "df_not_found");
        llvm::BasicBlock *addBB = createBB("df.add");
        llvm::BasicBlock *nextBB = createBB("df.next");
        emitBranchCond(notFound, addBB, nextBB);

        builder_.SetInsertPoint(addBB);
        llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "df_cur_len");
        llvm::Value *storePtr = builder_.CreateGEP(elemTy, newData, {curLen}, "df_store_ptr");
        builder_.CreateStore(ev, storePtr);
        emitBucketInsertAndRehashCheck(newHeader, setHeaderTy_, kSetLayout.lenIdx, kSetLayout.bucketCountIdx, kSetLayout.bucketsPtrIdx, ev, elemTy, curLen);
        builder_.CreateStore(builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1)), lenPtr);
        emitBranchUncond(nextBB);

        builder_.SetInsertPoint(nextBB);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        emitBranchUncond(condBB);
        builder_.SetInsertPoint(endBB);

        setTypeMeta(TypeMeta::SetElem, newHeader, elemTy);
        propagateMeta(set1, newHeader);
        return newHeader;
    }
    return nullptr;
}

// symmetricDifference(set1, set2)
llvm::Value *CodeGen::emitSetOp_symmetric_difference(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;
    llvm::Value *set1 = emitExpr(*e.args[0]);
    llvm::Value *set2 = emitExpr(*e.args[1]);
    llvm::Type *elemTy = getSetElementType(set1);
    if (elemTy) {
        llvm::Type *elemTy2 = getSetElementType(set2);
        if (!elemTy2 || elemTy2 != elemTy)
            codegenError("symmetricDifference() requires two sets with the same element type");
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
            llvm::BasicBlock *cBB = createBBInFn((prefix + ".cond").c_str(), fn_);
            llvm::BasicBlock *bBB = createBBInFn((prefix + ".body").c_str(), fn_);
            llvm::BasicBlock *eBB = createBBInFn((prefix + ".end").c_str(), fn_);
            emitBranchUncond(cBB);
            builder_.SetInsertPoint(cBB);
            llvm::Value *ci = builder_.CreateLoad(i64Ty_, iVar, prefix + "_ci");
            emitBranchCond(builder_.CreateICmpSLT(ci, srcLen), bBB, eBB);
            builder_.SetInsertPoint(bBB);
            llvm::Value *ePtr = builder_.CreateGEP(elemTy, srcData, {ci}, prefix + "_ep");
            llvm::Value *eVal = builder_.CreateLoad(elemTy, ePtr, prefix + "_ev");
            if (!elemName.empty())
                propagateTypeMeta(elemName, eVal);
            llvm::Value *inOther = emitSetElementLookup(otherSet, eVal, elemTy, elemName);
            llvm::Value *notInOther = builder_.CreateICmpSLT(inOther, llvm::ConstantInt::get(i64Ty_, 0), prefix + "_nf");
            llvm::BasicBlock *aBB = createBBInFn((prefix + ".add").c_str(), fn_);
            llvm::BasicBlock *nBB = createBBInFn((prefix + ".next").c_str(), fn_);
            emitBranchCond(notInOther, aBB, nBB);
            builder_.SetInsertPoint(aBB);
            llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, prefix + "_cl");
            llvm::Value *sp = builder_.CreateGEP(elemTy, newData, {curLen}, prefix + "_sp");
            builder_.CreateStore(eVal, sp);
            emitBucketInsertAndRehashCheck(newHeader, setHeaderTy_, kSetLayout.lenIdx, kSetLayout.bucketCountIdx, kSetLayout.bucketsPtrIdx, eVal, elemTy, curLen);
            builder_.CreateStore(builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1)), lenPtr);
            emitBranchUncond(nBB);
            builder_.SetInsertPoint(nBB);
            builder_.CreateStore(builder_.CreateAdd(ci, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            emitBranchUncond(cBB);
            builder_.SetInsertPoint(eBB);
        };

        emitSetDiffLoop(sf1.elems, sf1.len, set2, "sd1");
        emitSetDiffLoop(sf2.elems, sf2.len, set1, "sd2");

        setTypeMeta(TypeMeta::SetElem, newHeader, elemTy);
        propagateMeta(set1, newHeader);
        return newHeader;
    }
    return nullptr;
}

// isSubset(set1, set2)
llvm::Value *CodeGen::emitSetOp_is_subset(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;
    llvm::Value *set1 = emitExpr(*e.args[0]);
    llvm::Value *set2 = emitExpr(*e.args[1]);
    return emitSubsetCheck(set1, set2, "isSubset");
}

llvm::Value *CodeGen::emitSetOp_is_superset(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;
    llvm::Value *set1 = emitExpr(*e.args[0]);
    llvm::Value *set2 = emitExpr(*e.args[1]);
    // isSuperset(a, b) == isSubset(b, a)
    return emitSubsetCheck(set2, set1, "isSuperset");
}

} // namespace ry
