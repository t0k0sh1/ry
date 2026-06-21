#include "ry/codegen.hpp"
#include "ry/diagnostic/diagnostic.hpp"
#include "ry/llvm_emit/api.h" // RY_LISTCOPY_TAKE / ry_emit_list_remove (#2095)
#include "ry/llvm_emit/cast_helpers.hpp" // asRyValue / asRyType / asLlvmValue (#2095)



namespace ry {

namespace {

// Split a `getPath` / `setPath` path string on '.' at codegen time.
// Empty path or any empty segment is a static error (matches the issue
// #1701 grammar: every segment must be a non-empty identifier-like str).
// `callee` flows into the diagnostic ("getPath" / "setPath").
std::vector<std::string> splitDotPath(CodeGen &cg, const std::string &pathStr,
                                       const char *callee) {
    std::vector<std::string> segments;
    std::string cur;
    for (char c : pathStr) {
        if (c == '.') {
            if (cur.empty())
                cg.codegenError(std::string(callee) + "() path has empty segment");
            segments.push_back(std::move(cur));
            cur.clear();
        } else {
            cur.push_back(c);
        }
    }
    if (cur.empty())
        cg.codegenError(std::string(callee) + "() path has empty trailing segment");
    segments.push_back(std::move(cur));
    return segments;
}

// Compile-time numeric-segment parser for issue #1701 List-index dispatch.
// Returns the segment's int form when it is a non-empty all-digits str;
// nullopt otherwise. Shared between getPath now and setPath when its
// List-index support lands.
std::optional<int64_t> tryParseSegmentInt(const std::string &s) {
    if (s.empty()) return std::nullopt;
    for (char c : s)
        if (!std::isdigit(static_cast<unsigned char>(c)))
            return std::nullopt;
    try { return std::stoll(s); } catch (...) { return std::nullopt; }
}

} // namespace

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
        {"getPath",   &CodeGen::emitCollOp_getPath},
        {"setPath",   &CodeGen::emitCollOp_setPath},
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

    // Reject non-str pointer elements before crossing the boundary: the
    // boundary's strcmp path is UB on Map/Set/List/closure/resource headers.
    // Positive allowlist on list_elem_type_name (empty or "str" counts as str)
    // with structural fallbacks for NestedListElem / list_elem_fn_type_info in
    // case the name is unset. Mirrors #1262 distinct() guard. The guard reads
    // ValueMetadata which does not cross the boundary, so it stays C++-side.
    if (listElemTy == ptrTy_) {
        const ValueMetadata *meta = getMeta(containerPtr);
        const std::string &elemName = meta ? meta->list_elem_type_name : std::string{};
        const bool isNonStrName = !elemName.empty() && elemName != "str";
        const bool hasNestedList = meta && meta->nested_list_elem != nullptr;
        const bool hasFnInfo = meta && meta->list_elem_fn_type_info.has_value();
        if (isNonStrName || hasNestedList || hasFnInfo)
            codegenError("remove() is only supported for lists of primitive values or strings");
    }

    // #2095 boundary migration. The engine method reproduces the C++ baseline
    // byte-for-byte (interleaved loadListHeader, identical SSA names, the
    // search/remove BB scaffold). `elem_is_str` selects strcmp; `elem_is_double`
    // selects FCmpOEQ; otherwise plain ICmpEQ.
    RyValueId containerId = ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(containerPtr));
    RyValueId valId = ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(val));
    RyValueId resultId = ry_emit_list_remove(
        emit_ctx_, containerId, valId,
        ry::llvm_emit::asRyType(listHeaderTy_),
        ry::llvm_emit::asRyType(listElemTy), elemSize,
        listElemTy == ptrTy_ ? 1 : 0,
        listElemTy->isDoubleTy() ? 1 : 0);
    return ry::llvm_emit::asLlvmValue(ry_emit_resolve(emit_ctx_, resultId));
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

        // ARC retain decision uses ValueMetadata which does not cross the boundary,
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
        RyValueId listId =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(listPtr));
        RyValueId valId =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(val));
        ry_emit_collection_append(emit_ctx_, listId, valId,
                                  ry::llvm_emit::asRyType(listHeaderTy_),
                                  ry::llvm_emit::asRyType(elemTy),
                                  elemSize);

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

        auto lf = loadListHeader(listPtr, "apd");
        llvm::Value *newLen = builder_.CreateAdd(lf.len, llvm::ConstantInt::get(i64Ty_, 1), "apd_new_len");

        llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);
        // Copy generation (malloc + memcpy of the old range) is delegated to the
        // llvm_emit boundary (#2093). newLen/lf.len/lf.data are loaded above and
        // reused after; the appended-element store + ARC retains stay below.
        RyValueId apdNewLenId =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(newLen));
        RyValueId apdOldLenId =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(lf.len));
        RyValueId apdSrcId =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(lf.data));
        RyValueId apdNewDataId = ry_emit_list_appended(
            emit_ctx_, apdNewLenId, apdOldLenId, apdSrcId, elemSize);
        llvm::Value *newData = ry::llvm_emit::asLlvmValue(
            ry_emit_resolve(emit_ctx_, apdNewDataId));

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

        // #2094 ([C] = (ii) boundary move): condition compute, last-index sub,
        // element GEP + load, and destructive len store cross via generic
        // primitives. lf.lenPtr (from loadListHeader, header-load capability is
        // a follow-on) is passed across as the C++-side llvm::Value*.
        llvm::Value *isEmpty = emitICmpEQ(lf.len, emitConstInt(i64Ty_, 0), "pop_empty");
        llvm::BasicBlock *emptyBB = createBB("pop.empty");
        llvm::BasicBlock *okBB = createBB("pop.ok");
        llvm::BasicBlock *mergeBB = createBB("pop.merge");
        emitBranchCond(isEmpty, emptyBB, okBB);

        builder_.SetInsertPoint(emptyBB);
        llvm::Value *noneVal = buildNoneValue(optTy);
        emitBranchUncond(mergeBB);
        llvm::BasicBlock *emptyEndBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(okBB);
        llvm::Value *lastIdx = emitSub(lf.len, emitConstInt(i64Ty_, 1), "pop_last_idx");
        llvm::Value *elemPtr = emitGEP(elemTy, lf.data, lastIdx, "pop_elem_ptr");
        llvm::Value *val = emitLoad(elemTy, elemPtr, "pop_val");
        emitStore(lastIdx, lf.lenPtr);
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

    // Delegate the clamp + malloc + memcpy chain to the llvm_emit boundary. The
    // boundary returns (count, new_data); header allocation, metadata propagation
    // and per-element ARC retain stay on the codegen side because they need
    // ValueMetadata that does not cross the boundary.
    RyValueId sliceListId =
        ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(listPtr));
    RyValueId sliceStartId =
        ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(startVal));
    RyValueId sliceEndId =
        ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(endExclVal));
    RyValueId sliceCountId = 0;
    RyValueId sliceNewDataId = 0;
    ry_emit_list_slice(emit_ctx_, sliceListId, sliceStartId, sliceEndId,
                       ry::llvm_emit::asRyType(listHeaderTy_),
                       ry::llvm_emit::asRyType(elemTy), elemSize,
                       &sliceCountId, &sliceNewDataId);
    llvm::Value *count =
        ry::llvm_emit::asLlvmValue(ry_emit_resolve(emit_ctx_, sliceCountId));
    llvm::Value *newData = ry::llvm_emit::asLlvmValue(
        ry_emit_resolve(emit_ctx_, sliceNewDataId));

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

        auto lf = loadListHeader(listPtr, "tk");

        // Clamp n: max(0, min(n, len))
        llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
        llvm::Value *clampedN = builder_.CreateSelect(
            builder_.CreateICmpSLT(nVal, zero), zero, nVal, "tk_cn");
        clampedN = builder_.CreateSelect(
            builder_.CreateICmpSGT(clampedN, lf.len), lf.len, clampedN, "tk_cn2");

        // Allocate new list
        llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);
        // Copy generation delegated to the llvm_emit boundary (#2093); clampedN
        // (alloc == copy) and lf.data feed the malloc + memcpy.
        RyValueId tkSrcId =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(lf.data));
        RyValueId tkCountId =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(clampedN));
        RyValueId tkNewDataId = ry_emit_list_copy_full(
            emit_ctx_, tkSrcId, tkCountId, elemSize, RY_LISTCOPY_TAKE);
        llvm::Value *newData = ry::llvm_emit::asLlvmValue(
            ry_emit_resolve(emit_ctx_, tkNewDataId));

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

        // ARC retain decision uses ValueMetadata which does not cross the boundary,
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
        RyValueId insListId =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(listPtr));
        RyValueId insIdxId =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(idx));
        RyValueId insValId =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(val));
        ry_emit_collection_insert(emit_ctx_, insListId, insIdxId, insValId,
                                  ry::llvm_emit::asRyType(listHeaderTy_),
                                  ry::llvm_emit::asRyType(elemTy),
                                  elemSize);

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
        RyValueId rmListId =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(listPtr));
        RyValueId rmIdxId =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(idx));
        RyValueId rmRemovedId = ry_emit_collection_remove_at(
            emit_ctx_, rmListId, rmIdxId,
            ry::llvm_emit::asRyType(listHeaderTy_),
            ry::llvm_emit::asRyType(elemTy), elemSize);
        return ry::llvm_emit::asLlvmValue(
            ry_emit_resolve(emit_ctx_, rmRemovedId));
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

    // Match the C++ baseline byte-for-byte: loadListHeader (interleaved)
    // BEFORE emitArcAllocCollectionHeader. The boundary then runs the dedup
    // loop body. #2095 migration — see ry_emit_list_distinct.
    auto lf = loadListHeader(listVal, "dist_src");

    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t elemSize = dl.getTypeAllocSize(elemTy);

    // Allocate the ARC collection header (registers in arc_owned_values_).
    llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);

    RyValueId srcLenId = ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(lf.len));
    RyValueId srcDataId = ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(lf.data));
    RyValueId newHeaderId = ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(newHeader));
    ry_emit_list_distinct(emit_ctx_, srcLenId, srcDataId, newHeaderId,
                          ry::llvm_emit::asRyType(listHeaderTy_),
                          ry::llvm_emit::asRyType(elemTy), elemSize,
                          elemTy == ptrTy_ ? 1 : 0,
                          elemTy->isDoubleTy() ? 1 : 0);

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

    // #2095 migration: outer loadListHeader stays C++-side to preserve the
    // baseline instruction order; the boundary runs pass 1 + inline ARC alloc
    // + pass 2 and returns the new collection header pointer. arc_owned_values_
    // registration mirrors the original emitArcGetDataPtr bookkeeping.
    auto lf = loadListHeader(listVal, "flat_o");
    RyValueId outerLenId = ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(lf.len));
    RyValueId outerDataId = ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(lf.data));
    RyValueId newHeaderId = ry_emit_list_flatten(
        emit_ctx_, outerLenId, outerDataId,
        ry::llvm_emit::asRyType(listHeaderTy_),
        ry::llvm_emit::asRyType(arcHeaderTy_),
        ry::llvm_emit::asRyType(innerElemTy), innerElemSize);
    llvm::Value *newHeader =
        ry::llvm_emit::asLlvmValue(ry_emit_resolve(emit_ctx_, newHeaderId));
    arc_owned_values_.insert(newHeader);

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

        llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);

        // #2095 — trampoline pattern with retain_key / retain_val callbacks.
        struct ItemsRetainCtx {
            CodeGen *cg;
            std::string keyName;
            std::string valName;
        } retainCtx{this, keyName, valName};
        RyRetainFn retainKeyFn = nullptr;
        if (!keyName.empty()) {
            retainKeyFn = [](RyValueId valId, void *uc) {
                auto *r = static_cast<ItemsRetainCtx *>(uc);
                llvm::Value *v =
                    ry::llvm_emit::asLlvmValue(ry_emit_resolve(r->cg->emit_ctx_, valId));
                r->cg->emitTupleComponentRetain(v, r->keyName);
            };
        }
        RyRetainFn retainValFn = nullptr;
        if (!valName.empty()) {
            retainValFn = [](RyValueId valId, void *uc) {
                auto *r = static_cast<ItemsRetainCtx *>(uc);
                llvm::Value *v =
                    ry::llvm_emit::asLlvmValue(ry_emit_resolve(r->cg->emit_ctx_, valId));
                r->cg->emitTupleComponentRetain(v, r->valName);
            };
        }
        ry_emit_map_items(emit_ctx_,
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(mf.len)),
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(mf.keys)),
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(mf.vals)),
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(newHeader)),
            ry::llvm_emit::asRyType(listHeaderTy_),
            ry::llvm_emit::asRyType(keyTy),
            ry::llvm_emit::asRyType(valTy),
            ry::llvm_emit::asRyType(tupleTy), tupleSize,
            retainKeyFn, retainValFn,
            (retainKeyFn || retainValFn) ? static_cast<void *>(&retainCtx) : nullptr);

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
    if (e.args.size() != 2 && e.args.size() != 3)
        return nullptr;

    llvm::Value *recv = emitExpr(*e.args[0]);

    // Map branch ---------------------------------------------------------
    if (llvm::Type *keyTy = getMapKeyType(recv)) {
        llvm::Type *valTy = getMapValueType(recv);
        if (!valTy)
            return nullptr;

        // #2247: snapshot Map value metadata BEFORE the foundBB body so the
        // loaded `get*_val` SSA carries its source-level type name and
        // closure info — mirrors the List branch's `stampLoadedElem`
        // lambda (codegen_call_collection.cpp around L1027). Without this
        // the Some payload (or the 3-arg PHI result) has no metadata,
        // downstream `inner[key]` / dispatch routes through
        // `isStrLike` and rejects with "str does not support index
        // access" (`Map<str, Map<...>>` etc.).
        std::string mapValTypeName;
        std::optional<FnTypeInfo> mapValFnTypeInfo;
        if (auto *recvMeta = getMeta(recv)) {
            mapValTypeName = recvMeta->map_value_type_name;
            mapValFnTypeInfo = recvMeta->map_value_fn_type_info;
        }

        auto stampLoadedMapValue = [&](llvm::Value *val) {
            if (val->getType() != ptrTy_)
                return;
            if (!mapValTypeName.empty() && mapValTypeName != "str")
                propagateTypeMeta(mapValTypeName, val);
            if (mapValFnTypeInfo)
                getOrCreateMeta(val).fn_type_info = *mapValFnTypeInfo;
            if (mapValTypeName == "str")
                getOrCreateMeta(val).str_elem = true;
        };

        // get(map, key) -- 2-arg -> Option<V>
        if (e.args.size() == 2) {
            llvm::Value *key = emitExpr(*e.args[1]);
            if (key->getType() != keyTy)
                codegenError("get() key type mismatch");
            llvm::StructType *optTy = getOptionType(valTy);
            // #2094 ([C] = (ii) boundary move): the found/not-found condition,
            // mapHeader vals-field struct GEP + load, and value GEP + load
            // cross via generic primitives. emitMapKeyLookup is already
            // boundary-emitted via #2101 (hash lookup capability).
            llvm::Value *idx = emitMapKeyLookup(recv, key, keyTy);
            llvm::Value *found = emitICmpSGE(idx, emitConstInt(i64Ty_, 0), "get2_found");

            llvm::BasicBlock *foundBB = createBB("get2.found");
            llvm::BasicBlock *notFoundBB = createBB("get2.notfound");
            llvm::BasicBlock *mergeBB = createBB("get2.merge");
            emitBranchCond(found, foundBB, notFoundBB);

            builder_.SetInsertPoint(foundBB);
            llvm::Value *valsPtrField = emitStructGEP(mapHeaderTy_, recv, 3, "get2_vals_field");
            llvm::Value *valsPtr = emitLoad(ptrTy_, valsPtrField, "get2_vals");
            llvm::Value *valPtr = emitGEP(valTy, valsPtr, idx, "get2_val_ptr");
            llvm::Value *foundVal = emitLoad(valTy, valPtr, "get2_val");
            // #2247: stamp Map value metadata onto the freshly loaded value
            // before buildSomeValue boxes it for the Option result.
            stampLoadedMapValue(foundVal);
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
            // #2247: propagate metadata from the Some-incoming through the
            // PHI so the Option<V> result carries the inner value's source-
            // level name (mirrors emitCollOp_get List branch at L1114).
            propagateMeta(someVal, phi);
            return phi;
        }

        // get(map, key, default) -- 3-arg
        llvm::Value *key = emitExpr(*e.args[1]);
        if (key->getType() != keyTy)
            codegenError("get() key type mismatch");
        // #2094 ([C] = (ii) boundary move): same primitives as the 2-arg
        // arm — the only structural difference is the merge PHI types
        // (V vs Option<V>) and the default-value tail vs None.
        llvm::Value *idx = emitMapKeyLookup(recv, key, keyTy);
        llvm::Value *found = emitICmpSGE(idx, emitConstInt(i64Ty_, 0), "get_found");

        llvm::BasicBlock *foundBB = createBB("get.found");
        llvm::BasicBlock *notFoundBB = createBB("get.notfound");
        llvm::BasicBlock *mergeBB = createBB("get.merge");
        emitBranchCond(found, foundBB, notFoundBB);

        builder_.SetInsertPoint(foundBB);
        llvm::Value *valsPtrField = emitStructGEP(mapHeaderTy_, recv, 3, "get_vals_field");
        llvm::Value *valsPtr = emitLoad(ptrTy_, valsPtrField, "get_vals");
        llvm::Value *valPtr = emitGEP(valTy, valsPtr, idx, "get_val_ptr");
        llvm::Value *foundVal = emitLoad(valTy, valPtr, "get_val");
        // #2247: stamp Map value metadata onto the freshly loaded value
        // before it flows into the 3-arg PHI merged with the default arm.
        stampLoadedMapValue(foundVal);
        llvm::BasicBlock *foundEndBB = builder_.GetInsertBlock();
        emitBranchUncond(mergeBB);

        builder_.SetInsertPoint(notFoundBB);
        llvm::Value *defaultVal = emitExpr(*e.args[2]);
        if (defaultVal->getType() != valTy)
            codegenError("get() default value type must match map's value type");
        emitBranchUncond(mergeBB);
        llvm::BasicBlock *notFoundEndBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = createPhi(valTy, {}, "get_result");
        phi->addIncoming(foundVal, foundEndBB);
        phi->addIncoming(defaultVal, notFoundEndBB);
        // #2247: propagate metadata from the found-incoming through the PHI
        // so the value carries the Map value's source-level name (mirrors
        // the 2-arg arm above and the List branch at L1114).
        propagateMeta(foundVal, phi);
        return phi;
    }

    // List branch --------------------------------------------------------
    // get(list, index) -> Option<T> / get(list, index, default) -> T
    // Mirrors `xs[i]?` semantics (codegen_expr_literal.cpp): negative-index
    // wrap, OOB → None / default, in-range → Some(elem) / elem.
    llvm::Type *elemTy = getListElementType(recv);
    if (!elemTy)
        return nullptr;

    // Snapshot list element metadata BEFORE any call that may rehash
    // value_metadata_ (propagateTypeMeta / buildSomeValue / getOptionType / emitExpr).
    std::string elemTypeName;
    std::optional<FnTypeInfo> elemFnTypeInfo;
    bool listElemIsStr = false;
    if (auto *listMeta = getMeta(recv)) {
        elemTypeName   = listMeta->list_elem_type_name;
        elemFnTypeInfo = listMeta->list_elem_fn_type_info;
        listElemIsStr  = listMeta->list_elem_is_str;
    }
    llvm::Type *nestedElemTy = getNestedListElementType(recv);

    auto stampLoadedElem = [&](llvm::Value *elem) {
        // Mirror codegen_expr_literal.cpp's stampLoadedElem so List<str> ARC,
        // List<List<T>> nested element type, and List<closure> fn_type_info
        // all survive the load. Without these, `Some(elem)` would drop
        // metadata and the Option-returning path would skip the str retain
        // (→ use-after-free post-source-mutation).
        if (nestedElemTy)
            setTypeMeta(TypeMeta::ListElem, elem, nestedElemTy);
        if (!elemTypeName.empty())
            propagateTypeMeta(elemTypeName, elem);
        if (!elemTypeName.empty() && elemTypeName.size() >= 2 &&
            elemTypeName.front() == '(' && elemTypeName.back() == ')')
            getOrCreateMeta(elem).source_type_name = elemTypeName;
        if (elemFnTypeInfo)
            getOrCreateMeta(elem).fn_type_info = *elemFnTypeInfo;
        if (listElemIsStr)
            getOrCreateMeta(elem).str_elem = true;
    };

    llvm::Value *index = emitExpr(*e.args[1]);
    if (index->getType() != i64Ty_)
        codegenError("get() index must be int");

    ListFields lf = loadListHeader(recv, "getl");
    llvm::Value *wrapped = emitNegativeIndexWrap(index, lf.len, "getl");
    llvm::Value *zero = emitConstInt(i64Ty_, 0);
    llvm::Value *negCheck = emitICmpSLT(wrapped, zero, "getl_neg");
    llvm::Value *overCheck = emitICmpSGE(wrapped, lf.len, "getl_over");
    llvm::Value *oob = emitOr(negCheck, overCheck, "getl_oob");

    if (e.args.size() == 2) {
        // 2-arg: -> Option<T>
        llvm::StructType *optTy = getOptionType(elemTy);
        llvm::BasicBlock *oobBB = createBB("getl2.oob");
        llvm::BasicBlock *okBB = createBB("getl2.ok");
        llvm::BasicBlock *mergeBB = createBB("getl2.merge");
        emitBranchCond(oob, oobBB, okBB);

        builder_.SetInsertPoint(okBB);
        llvm::Value *elemPtr = emitGEP(elemTy, lf.data, wrapped, "getl2_elem_ptr");
        llvm::Value *elem = emitLoad(elemTy, elemPtr, "getl2_elem");
        stampLoadedElem(elem);
        llvm::Value *someVal = buildSomeValue(elem, optTy);
        emitBranchUncond(mergeBB);
        llvm::BasicBlock *okEndBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(oobBB);
        llvm::Value *noneVal = buildNoneValue(optTy);
        emitBranchUncond(mergeBB);
        llvm::BasicBlock *oobEndBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = createPhi(optTy, {}, "getl2_result");
        phi->addIncoming(someVal, okEndBB);
        phi->addIncoming(noneVal, oobEndBB);
        propagateMeta(someVal, phi);
        return phi;
    }

    // 3-arg: -> T
    llvm::BasicBlock *oobBB = createBB("getl3.oob");
    llvm::BasicBlock *okBB = createBB("getl3.ok");
    llvm::BasicBlock *mergeBB = createBB("getl3.merge");
    emitBranchCond(oob, oobBB, okBB);

    builder_.SetInsertPoint(okBB);
    llvm::Value *elemPtr = emitGEP(elemTy, lf.data, wrapped, "getl3_elem_ptr");
    llvm::Value *foundVal = emitLoad(elemTy, elemPtr, "getl3_elem");
    stampLoadedElem(foundVal);
    emitBranchUncond(mergeBB);
    llvm::BasicBlock *okEndBB = builder_.GetInsertBlock();

    builder_.SetInsertPoint(oobBB);
    llvm::Value *defaultVal = emitExpr(*e.args[2]);
    if (defaultVal->getType() != elemTy) {
        if (isAnyType(elemTy))
            defaultVal = wrapInAny(defaultVal);
        else if (isAnyType(defaultVal->getType()) && canAnyHoldType(elemTy))
            defaultVal = unwrapFromAny(defaultVal, elemTy);
        else
            codegenError("get() default value type must match list element type");
    }
    emitBranchUncond(mergeBB);
    llvm::BasicBlock *oobEndBB = builder_.GetInsertBlock();

    builder_.SetInsertPoint(mergeBB);
    llvm::PHINode *phi = createPhi(elemTy, {}, "getl3_result");
    phi->addIncoming(foundVal, okEndBB);
    phi->addIncoming(defaultVal, oobEndBB);
    propagateMeta(foundVal, phi);
    return phi;
}

llvm::Value *CodeGen::emitMapMergeCore(llvm::Value *map1, llvm::Value *map2,
                                        llvm::Type *keyTy, llvm::Type *valTy) {
    // (ii) primitives sweep — see boundary doc #2193. C++-stay carve-outs:
    // loadMapHeader, emitMapKeyLookup, propagateTypeMeta / setTypeMeta /
    // propagateMeta, ARC helpers, emitBucketInit / storeMapHeaderFields.
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t keySize = dl.getTypeAllocSize(keyTy);
    uint64_t valSize = dl.getTypeAllocSize(valTy);

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

    // #2226: tuple keys/vals are inline StructType, not ptrTy_, so the
    // mg*IsArc gates above never fire. Local dispatch via tuple sig +
    // resolveTypeAlias (#1670 follow-up) handles them in parallel.
    const std::string resolvedKeyName = keyName.empty() ? std::string{}
        : resolveTypeAlias(keyName);
    const std::string resolvedValName = valName.empty() ? std::string{}
        : resolveTypeAlias(valName);
    llvm::StructType *keyTupleTy =
        (resolvedKeyName.size() >= 2 && resolvedKeyName.front() == '(' &&
         resolvedKeyName.back() == ')')
            ? llvm::dyn_cast<llvm::StructType>(keyTy) : nullptr;
    llvm::StructType *valTupleTy =
        (resolvedValName.size() >= 2 && resolvedValName.front() == '(' &&
         resolvedValName.back() == ')')
            ? llvm::dyn_cast<llvm::StructType>(valTy) : nullptr;
    const bool mgKeyIsTuple = keyTupleTy != nullptr;
    const bool mgValIsTuple = valTupleTy != nullptr;

    auto mf1 = loadMapHeader(map1, "mg1");
    auto mf2 = loadMapHeader(map2, "mg2");

    // Allocate new map with capacity = len1 + len2
    llvm::Value *maxCap = emitAdd(mf1.len, mf2.len, "mg_max_cap");
    llvm::Value *newHeader = emitArcAllocCollectionHeader(mapHeaderTy_);
    llvm::Value *newKeysSize = emitMul(maxCap, emitConstInt(i64Ty_, keySize), "mg_ks");
    llvm::Value *newKeys = emitRuntimeCallDirect("malloc", ptrTy_, {i64Ty_},
                                                  {newKeysSize}, "mg_keys");
    llvm::Value *newValsSize = emitMul(maxCap, emitConstInt(i64Ty_, valSize), "mg_vs");
    llvm::Value *newVals = emitRuntimeCallDirect("malloc", ptrTy_, {i64Ty_},
                                                  {newValsSize}, "mg_vals");

    // Copy all of map1
    llvm::Value *copy1KeySize = emitMul(mf1.len, emitConstInt(i64Ty_, keySize), "mg_ck1");
    emitRuntimeCallDirect("memcpy", ptrTy_, {ptrTy_, ptrTy_, i64Ty_},
                          {newKeys, mf1.keys, copy1KeySize}, "");
    llvm::Value *copy1ValSize = emitMul(mf1.len, emitConstInt(i64Ty_, valSize), "mg_cv1");
    emitRuntimeCallDirect("memcpy", ptrTy_, {ptrTy_, ptrTy_, i64Ty_},
                          {newVals, mf1.vals, copy1ValSize}, "");

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
    // #2226: tuple keys/vals — memcpy duplicates the inline struct, but its
    // inner ARC pointer fields are unretained; emitTupleElemRetainLoop scans
    // the array and retains each ARC field per slot (mirrors #1667 appended).
    if (mgKeyIsTuple) {
        emitTupleElemRetainLoop(newKeys, mf1.len, "mg_k1_telem",
                                  resolvedKeyName, keyTupleTy);
    }
    if (mgValIsTuple) {
        emitTupleElemRetainLoop(newVals, mf1.len, "mg_v1_telem",
                                  resolvedValName, valTupleTy);
    }

    // Set up header
    storeMapHeaderFields(newHeader, mf1.len, maxCap, newKeys, newVals);
    llvm::Value *lenPtr = emitStructGEP(mapHeaderTy_, newHeader, 0, "mg_len_ptr");

    // Init hash buckets
    emitBucketInit(newHeader, mapHeaderTy_, kMapLayout.bucketCountIdx, kMapLayout.bucketsPtrIdx, 16);

    // #2188 ([4a] = outer loop scaffold migration; insert+rehash deferred to #2193).
    // Re-hash map1 keys into new map's buckets
    {
        llvm::Value *iVar = emitAlloca(i64Ty_, "mg_rh_i");
        emitStore(emitConstInt(i64Ty_, 0), iVar);
        llvm::BasicBlock *rCondBB = createBB("mg.rh.cond");
        llvm::BasicBlock *rBodyBB = createBB("mg.rh.body");
        llvm::BasicBlock *rEndBB = createBB("mg.rh.end");
        emitBranchUncond(rCondBB);
        builder_.SetInsertPoint(rCondBB);
        llvm::Value *ri = emitLoad(i64Ty_, iVar, "mg_ri");
        emitBranchCond(emitICmpSLT(ri, mf1.len, ""), rBodyBB, rEndBB);
        builder_.SetInsertPoint(rBodyBB);
        llvm::Value *kp = emitGEP(keyTy, newKeys, ri, "mg_rh_kp");
        llvm::Value *kv = emitLoad(keyTy, kp, "mg_rh_kv");
        if (!keyName.empty()) propagateTypeMeta(keyName, kv);
        emitBucketInsertAndRehashCheck(newHeader, mapHeaderTy_, kMapLayout.lenIdx, kMapLayout.bucketCountIdx, kMapLayout.bucketsPtrIdx, kv, keyTy, ri);
        emitStore(emitAdd(ri, emitConstInt(i64Ty_, 1), ""), iVar);
        emitBranchUncond(rCondBB);
        builder_.SetInsertPoint(rEndBB);
    }

    // Add/update entries from map2 (rhs-wins on key collision)
    {
        llvm::Value *iVar = emitAlloca(i64Ty_, "mg_i2");
        emitStore(emitConstInt(i64Ty_, 0), iVar);
        llvm::BasicBlock *condBB = createBB("mg.add.cond");
        llvm::BasicBlock *bodyBB = createBB("mg.add.body");
        llvm::BasicBlock *endBB = createBB("mg.add.end");
        emitBranchUncond(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = emitLoad(i64Ty_, iVar, "mg_ci");
        emitBranchCond(emitICmpSLT(i, mf2.len, ""), bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *kp = emitGEP(keyTy, mf2.keys, i, "mg_kp2");
        llvm::Value *kv = emitLoad(keyTy, kp, "mg_kv2");
        if (!keyName.empty()) propagateTypeMeta(keyName, kv);
        llvm::Value *vp = emitGEP(valTy, mf2.vals, i, "mg_vp2");
        llvm::Value *vv = emitLoad(valTy, vp, "mg_vv2");

        // Check if key exists in new map
        llvm::Value *lookupIdx = emitMapKeyLookup(newHeader, kv, keyTy, keyName);
        llvm::Value *exists = emitICmpSGE(lookupIdx, emitConstInt(i64Ty_, 0), "mg_exists");

        llvm::BasicBlock *updateBB = createBB("mg.update");
        llvm::BasicBlock *insertBB = createBB("mg.insert");
        llvm::BasicBlock *nextBB = createBB("mg.next");
        emitBranchCond(exists, updateBB, insertBB);

        // Update existing key's value
        builder_.SetInsertPoint(updateBB);
        llvm::Value *curValsField = emitStructGEP(mapHeaderTy_, newHeader, 3, "");
        llvm::Value *curVals = emitLoad(ptrTy_, curValsField, "mg_cur_vals");
        llvm::Value *updPtr = emitGEP(valTy, curVals, lookupIdx, "mg_upd_ptr");
        // Release the old ARC-managed value before overwriting (#1242 leak).
        if (mgValIsArc) {
            llvm::Value *oldVal = emitLoad(valTy, updPtr, "mg_upd_old");
            llvm::Value *oldHdr = (mgValArcKind == CollectionKind::Str)
                ? emitStrGetHeaderFromData(oldVal) : emitArcGetHeaderFromData(oldVal);
            emitArcRelease(oldHdr, false, nullptr, nullptr);
            retainArcValue(vv);
        }
        // #2226: tuple-typed val on update — retain the new tuple's ARC
        // components first (self-assignment safety, #1670 ordering), then
        // release the old slot's components via emitTupleElemReleaseSlot.
        // emitTupleComponentRetain on the aggregate sig recurses into each
        // ptr field internally (no need to splitTupleSig at the call site).
        if (mgValIsTuple) {
            emitTupleComponentRetain(vv, resolvedValName);
            emitTupleElemReleaseSlot(updPtr, "mg_upd_old_telem",
                                       resolvedValName, valTupleTy);
        }
        emitStore(vv, updPtr);
        emitBranchUncond(nextBB);

        // Insert new key-value pair
        builder_.SetInsertPoint(insertBB);
        llvm::Value *curLen = emitLoad(i64Ty_, lenPtr, "mg_cur_len");
        llvm::Value *curKeysField = emitStructGEP(mapHeaderTy_, newHeader, 2, "");
        llvm::Value *curKeys = emitLoad(ptrTy_, curKeysField, "mg_cur_keys");
        llvm::Value *newKeyPtr = emitGEP(keyTy, curKeys, curLen, "mg_new_kp");
        if (mgKeyIsArc) retainArcValue(kv);
        // #2226: tuple-typed key on insert — per-component retain.
        if (mgKeyIsTuple) emitTupleComponentRetain(kv, resolvedKeyName);
        emitStore(kv, newKeyPtr);
        llvm::Value *curVals2Field = emitStructGEP(mapHeaderTy_, newHeader, 3, "");
        llvm::Value *curVals2 = emitLoad(ptrTy_, curVals2Field, "mg_cur_vals2");
        llvm::Value *newValPtr = emitGEP(valTy, curVals2, curLen, "mg_new_vp");
        if (mgValIsArc) retainArcValue(vv);
        // #2226: tuple-typed val on insert — per-component retain.
        if (mgValIsTuple) emitTupleComponentRetain(vv, resolvedValName);
        emitStore(vv, newValPtr);
        emitStore(emitAdd(curLen, emitConstInt(i64Ty_, 1), ""), lenPtr);
        emitBucketInsertAndRehashCheck(newHeader, mapHeaderTy_, kMapLayout.lenIdx, kMapLayout.bucketCountIdx, kMapLayout.bucketsPtrIdx, kv, keyTy, curLen);
        emitBranchUncond(nextBB);

        builder_.SetInsertPoint(nextBB);
        emitStore(emitAdd(i, emitConstInt(i64Ty_, 1), ""), iVar);
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

// getPath (issue #1701): jq-style nested-Map<str, any> read access.
//
// `m.getPath("a.b.c")` walks each dot-separated segment through the
// any-held Map chain via `emitAnyPathStep`. Returns Option<any> with
// None on the first miss. Currently requires a str-literal path
// (compile-time split); a runtime-split overload follows in a later
// commit on this issue.
llvm::Value *CodeGen::emitCollOp_getPath(const CallExpr &e) {
    if (e.args.size() != 2) return nullptr;

    llvm::Value *recv = emitExpr(*e.args[0]);
    llvm::Type *keyTy = getMapKeyType(recv);
    llvm::Type *valTy = getMapValueType(recv);
    if (!keyTy || !valTy) return nullptr;
    if (keyTy != ptrTy_)
        codegenError("getPath() receiver must be Map<str, any>");
    if (!isAnyType(valTy))
        codegenError("getPath() receiver value type must be `any`");

    // Path arg must be a str literal in this iteration. Runtime str
    // (e.g. a variable holding the path) → follow-up.
    const StringExpr *pathLit = std::get_if<StringExpr>(&e.args[1]->data);
    if (!pathLit)
        codegenError("getPath() currently requires a string literal path");
    const std::string &pathStr = pathLit->value;

    std::vector<std::string> segments = splitDotPath(*this, pathStr, "getPath");

    llvm::Function *fn = builder_.GetInsertBlock()->getParent();
    llvm::StructType *optAnyTy = getOptionType(anyTy_);
    auto *mergeBB = createBBInFn("getpath.merge", fn);
    std::vector<std::pair<llvm::Value *, llvm::BasicBlock *>> phiIncomings;

    // First hop: receiver is Map<str, any>, look up first segment directly
    // (no wrapInAny so we avoid extra ARC retain on the receiver).
    llvm::Value *firstSegStr = cachedGlobalString(segments[0]);
    llvm::Value *firstSlot = emitHashTableLookup(
        recv, mapHeaderTy_, kMapLayout, firstSegStr, ptrTy_);
    llvm::Value *firstFound = builder_.CreateICmpSGE(
        firstSlot, llvm::ConstantInt::get(i64Ty_, 0), "getpath.first.found");

    auto *firstHitBB  = createBBInFn("getpath.first.hit", fn);
    auto *firstMissBB = createBBInFn("getpath.first.miss", fn);
    emitBranchCond(firstFound, firstHitBB, firstMissBB);

    builder_.SetInsertPoint(firstMissBB);
    llvm::Value *firstNone = buildNoneValue(optAnyTy);
    phiIncomings.emplace_back(firstNone, builder_.GetInsertBlock());
    emitBranchUncond(mergeBB);

    builder_.SetInsertPoint(firstHitBB);
    llvm::Value *firstValsField = builder_.CreateStructGEP(
        mapHeaderTy_, recv, 3, "getpath.first.vals.field");
    llvm::Value *firstValsPtr = builder_.CreateLoad(
        ptrTy_, firstValsField, "getpath.first.vals.ptr");
    llvm::Value *firstElemPtr = builder_.CreateGEP(
        anyTy_, firstValsPtr, firstSlot, "getpath.first.elem.ptr");
    llvm::Value *currentAny = builder_.CreateLoad(
        anyTy_, firstElemPtr, "getpath.first.elem");

    // The returned Option<any>'s inner payload is borrowed from the
    // receiver Map. Downstream consumers — `?` unwrap into a let-binding
    // (`unwrapFromAny` emits the retain) or pattern bind into an `any`
    // local (existing var-decl path handles it) — are responsible for
    // retaining when ownership transfers out of the receiver's lifetime.
    if (segments.size() == 1) {
        llvm::Value *someVal = buildSomeValue(currentAny, optAnyTy);
        phiIncomings.emplace_back(someVal, builder_.GetInsertBlock());
        emitBranchUncond(mergeBB);
    } else {
        for (size_t i = 1; i < segments.size(); ++i) {
            const std::string &seg = segments[i];
            llvm::Value *segStr = cachedGlobalString(seg);
            std::optional<int64_t> intSeg = tryParseSegmentInt(seg);
            llvm::Value *stepResult = emitAnyPathStep(
                currentAny, segStr, intSeg, /*tryMode=*/true, pathStr, seg);
            if (i + 1 == segments.size()) {
                phiIncomings.emplace_back(stepResult, builder_.GetInsertBlock());
                emitBranchUncond(mergeBB);
            } else {
                llvm::Value *disc = builder_.CreateExtractValue(
                    stepResult, 0, "getpath.hop.disc");
                auto *contBB = createBBInFn("getpath.hop.cont", fn);
                auto *noneBB = createBBInFn("getpath.hop.none", fn);
                emitBranchCond(disc, contBB, noneBB);

                builder_.SetInsertPoint(noneBB);
                llvm::Value *noneVal = buildNoneValue(optAnyTy);
                phiIncomings.emplace_back(noneVal, builder_.GetInsertBlock());
                emitBranchUncond(mergeBB);

                builder_.SetInsertPoint(contBB);
                currentAny = builder_.CreateExtractValue(
                    stepResult, 1, "getpath.hop.some");
            }
        }
    }

    builder_.SetInsertPoint(mergeBB);
    llvm::PHINode *phi = createPhi(optAnyTy, {}, "getpath.result");
    for (auto &p : phiIncomings) phi->addIncoming(p.first, p.second);
    return phi;
}

// setPath (issue #1701): jq-style nested Map<str, any> write.
// `m.setPath("a.b.c", v)` walks the dot path; intermediate segments must
// resolve to Map<str, any> at runtime (otherwise runtime error). The
// leaf segment is inserted or updated. Top-level receiver is COW-checked;
// intermediate Maps mutate in place (callers sharing references to a
// nested Map see the change). Returns Unit.
llvm::Value *CodeGen::emitCollOp_setPath(const CallExpr &e) {
    if (e.args.size() != 3) return nullptr;

    llvm::AllocaInst *recvAlloca = tryGetReceiverAlloca(*e.args[0]);
    llvm::Value *recv = emitExpr(*e.args[0]);
    llvm::Type *keyTy = getMapKeyType(recv);
    llvm::Type *valTy = getMapValueType(recv);
    if (!keyTy || !valTy) return nullptr;
    if (keyTy != ptrTy_)
        codegenError("setPath() receiver must be Map<str, any>");
    if (!isAnyType(valTy))
        codegenError("setPath() receiver value type must be `any`");

    const StringExpr *pathLit = std::get_if<StringExpr>(&e.args[1]->data);
    if (!pathLit)
        codegenError("setPath() currently requires a string literal path");
    const std::string &pathStr = pathLit->value;

    std::vector<std::string> segments = splitDotPath(*this, pathStr, "setPath");

    // RHS value: must be `any` or wrap-eligible scalar / collection.
    // Track whether the value was already an `any` (borrowed payload, needs
    // retain on store) versus freshly wrapped via `wrapInAny` (which already
    // emits the retain itself — a second retain at the store site would
    // leak by one refcount per setPath call).
    llvm::Value *value = emitExpr(*e.args[2]);
    const bool valueWasAny = isAnyType(value->getType());
    if (!valueWasAny) {
        if (value->getType() == ptrTy_ || canAnyHoldType(value->getType()))
            value = wrapInAny(value);
        else
            codegenError("setPath() value type cannot be stored in `any`");
    }

    // Top-level COW (matches the IndexAssignStmt Map case).
    recv = emitCowCheck(recv, recvAlloca, CollectionKind::Map);

    llvm::Function *fn = builder_.GetInsertBlock()->getParent();

    // Walk-down through intermediates: lookup each segment, verify Map
    // tag, advance currentMap. Hoist the `any → data ptr` scratch slot
    // out of the loop so depth-N paths emit one alloca instead of N-1.
    llvm::AllocaInst *anyTmp = segments.size() > 1
        ? builder_.CreateAlloca(anyTy_, nullptr, "sp.any.tmp")
        : nullptr;
    // String-concat-free intermediate error builder (avoids
    // performance-inefficient-string-concatenation under -warnings-as-errors).
    auto intermediateErr = [&](const std::string &seg, const char *suffix) {
        std::string msg;
        msg.reserve(pathStr.size() + seg.size() + 40);
        msg += "setPath '";
        msg += pathStr;
        msg += "': intermediate segment '";
        msg += seg;
        msg += "' ";
        msg += suffix;
        return msg;
    };
    llvm::Value *currentMap = recv;
    for (size_t i = 0; i + 1 < segments.size(); ++i) {
        const std::string &seg = segments[i];
        llvm::Value *segStr = cachedGlobalString(seg);
        llvm::Value *slot = emitHashTableLookup(
            currentMap, mapHeaderTy_, kMapLayout, segStr, ptrTy_);
        llvm::Value *found = builder_.CreateICmpSGE(
            slot, llvm::ConstantInt::get(i64Ty_, 0), "sp.found");

        auto *hitBB  = createBBInFn("sp.hit", fn);
        auto *missBB = createBBInFn("sp.miss", fn);
        emitBranchCond(found, hitBB, missBB);

        builder_.SetInsertPoint(missBB);
        emitRuntimeError(intermediateErr(seg, "not found"),
                         "err.setpath.miss");

        builder_.SetInsertPoint(hitBB);
        llvm::Value *valsField = builder_.CreateStructGEP(
            mapHeaderTy_, currentMap, 3, "sp.vals.field");
        llvm::Value *valsPtr = builder_.CreateLoad(
            ptrTy_, valsField, "sp.vals.ptr");
        llvm::Value *elemPtr = builder_.CreateGEP(
            anyTy_, valsPtr, slot, "sp.elem.ptr");
        llvm::Value *interAny = builder_.CreateLoad(
            anyTy_, elemPtr, "sp.elem");

        llvm::Value *tag = builder_.CreateExtractValue(interAny, 0, "sp.tag");
        llvm::Value *isMap = builder_.CreateICmpEQ(
            tag,
            llvm::ConstantInt::get(i64Ty_, static_cast<int64_t>(RyAnyTag::Map)),
            "sp.is_map");
        auto *tagOkBB  = createBBInFn("sp.tag_ok", fn);
        auto *tagErrBB = createBBInFn("sp.tag_err", fn);
        emitBranchCond(isMap, tagOkBB, tagErrBB);

        builder_.SetInsertPoint(tagErrBB);
        emitRuntimeError(intermediateErr(seg, "is not a Map"),
                         "err.setpath.tag");

        builder_.SetInsertPoint(tagOkBB);
        builder_.CreateStore(interAny, anyTmp);
        llvm::Value *anyDataSlot = builder_.CreateStructGEP(
            anyTy_, anyTmp, 1, "sp.any.data.slot");
        currentMap = builder_.CreateLoad(
            ptrTy_, anyDataSlot, "sp.next.map");
    }

    // Leaf insert-or-update on currentMap. Inlined Map<str, any> insert
    // (mirrors IndexAssignStmt's Map branch but specialized for any /
    // str-key, no compound-op).
    const std::string &leafSeg = segments.back();
    llvm::Value *leafKey = cachedGlobalString(leafSeg);
    llvm::Value *leafIdx = emitMapKeyLookup(
        currentMap, leafKey, ptrTy_);
    llvm::Value *leafFound = builder_.CreateICmpSGE(
        leafIdx, llvm::ConstantInt::get(i64Ty_, 0), "sp.leaf.found");

    auto *updateBB = createBBInFn("sp.leaf.update", fn);
    auto *insertBB = createBBInFn("sp.leaf.insert", fn);
    auto *doneBB   = createBBInFn("sp.leaf.done", fn);
    emitBranchCond(leafFound, updateBB, insertBB);

    // Update arm: retain new payload, release old slot's payload, store.
    builder_.SetInsertPoint(updateBB);
    llvm::Value *upValsField = builder_.CreateStructGEP(
        mapHeaderTy_, currentMap, 3, "sp.upd.vals.field");
    llvm::Value *upValsPtr = builder_.CreateLoad(
        ptrTy_, upValsField, "sp.upd.vals.ptr");
    llvm::Value *upElemPtr = builder_.CreateGEP(
        anyTy_, upValsPtr, leafIdx, "sp.upd.elem.ptr");
    llvm::Value *oldAny = builder_.CreateLoad(
        anyTy_, upElemPtr, "sp.upd.old.any");
    // wrapInAny already retained when valueWasAny is false; only retain
    // here when the caller passed an already-`any` (borrowed) value.
    if (valueWasAny)
        emitAnyRetainPayload(value, "sp.upd.new");
    emitAnyReleasePayload(oldAny, "", "sp.upd.old");
    builder_.CreateStore(value, upElemPtr);
    emitBranchUncond(doneBB);

    // Insert arm: grow if needed, store key & value at length, length++,
    // hash-table bucket insert + rehash check.
    builder_.SetInsertPoint(insertBB);
    llvm::Value *inLenPtr = builder_.CreateStructGEP(
        mapHeaderTy_, currentMap, 0, "sp.in.len.ptr");
    llvm::Value *inLen = builder_.CreateLoad(i64Ty_, inLenPtr, "sp.in.len");
    llvm::Value *inCapPtr = builder_.CreateStructGEP(
        mapHeaderTy_, currentMap, 1, "sp.in.cap.ptr");
    llvm::Value *inCap = builder_.CreateLoad(i64Ty_, inCapPtr, "sp.in.cap");
    llvm::Value *needGrow = builder_.CreateICmpEQ(
        inLen, inCap, "sp.in.need_grow");
    auto *growBB = createBBInFn("sp.in.grow", fn);
    auto *storeBB = createBBInFn("sp.in.store", fn);
    emitBranchCond(needGrow, growBB, storeBB);

    builder_.SetInsertPoint(growBB);
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t inKeySize = dl.getTypeAllocSize(ptrTy_);
    uint64_t inValSize = dl.getTypeAllocSize(anyTy_);
    llvm::Value *newCap = builder_.CreateMul(
        inCap, llvm::ConstantInt::get(i64Ty_, 2), "sp.in.new_cap");
    auto mallocFn = getStdlibMalloc();
    llvm::Value *newKeySize = builder_.CreateMul(
        newCap, llvm::ConstantInt::get(i64Ty_, inKeySize), "sp.in.new_key_sz");
    llvm::Value *newKeysPtr = builder_.CreateCall(
        mallocFn, {newKeySize}, "sp.in.new_keys");
    llvm::Value *newValSize = builder_.CreateMul(
        newCap, llvm::ConstantInt::get(i64Ty_, inValSize), "sp.in.new_val_sz");
    llvm::Value *newValsPtr = builder_.CreateCall(
        mallocFn, {newValSize}, "sp.in.new_vals");
    auto memcpyFn = getStdlibMemcpy();
    llvm::Value *kPtrField = builder_.CreateStructGEP(
        mapHeaderTy_, currentMap, 2, "sp.in.k.field");
    llvm::Value *oldKeysPtr = builder_.CreateLoad(
        ptrTy_, kPtrField, "sp.in.old.keys");
    llvm::Value *oldKeySize = builder_.CreateMul(
        inLen, llvm::ConstantInt::get(i64Ty_, inKeySize), "sp.in.old.k.sz");
    builder_.CreateCall(memcpyFn, {newKeysPtr, oldKeysPtr, oldKeySize});
    llvm::Value *vPtrField = builder_.CreateStructGEP(
        mapHeaderTy_, currentMap, 3, "sp.in.v.field");
    llvm::Value *oldValsPtr = builder_.CreateLoad(
        ptrTy_, vPtrField, "sp.in.old.vals");
    llvm::Value *oldValSize = builder_.CreateMul(
        inLen, llvm::ConstantInt::get(i64Ty_, inValSize), "sp.in.old.v.sz");
    builder_.CreateCall(memcpyFn, {newValsPtr, oldValsPtr, oldValSize});
    auto freeFn = getStdlibFree();
    builder_.CreateCall(freeFn, {oldKeysPtr});
    builder_.CreateCall(freeFn, {oldValsPtr});
    builder_.CreateStore(newKeysPtr, kPtrField);
    builder_.CreateStore(newValsPtr, vPtrField);
    builder_.CreateStore(newCap, inCapPtr);
    emitBranchUncond(storeBB);

    builder_.SetInsertPoint(storeBB);
    llvm::Value *curLen = builder_.CreateLoad(i64Ty_, inLenPtr, "sp.in.cur_len");
    llvm::Value *keysField3 = builder_.CreateStructGEP(
        mapHeaderTy_, currentMap, 2, "sp.in.keys.f3");
    llvm::Value *curKeysPtr = builder_.CreateLoad(
        ptrTy_, keysField3, "sp.in.cur.keys");
    llvm::Value *newKeyPtr = builder_.CreateGEP(
        ptrTy_, curKeysPtr, {curLen}, "sp.in.new.key.ptr");
    // leafKey is from cachedGlobalString → ARC_IMMORTAL, retain is no-op
    // but conventional via retainArcValue (matches IndexAssignStmt str-key path).
    retainArcValue(leafKey);
    builder_.CreateStore(leafKey, newKeyPtr);

    llvm::Value *valsField3 = builder_.CreateStructGEP(
        mapHeaderTy_, currentMap, 3, "sp.in.vals.f3");
    llvm::Value *curValsPtr = builder_.CreateLoad(
        ptrTy_, valsField3, "sp.in.cur.vals");
    llvm::Value *newValPtr = builder_.CreateGEP(
        anyTy_, curValsPtr, {curLen}, "sp.in.new.val.ptr");
    if (valueWasAny)
        emitAnyRetainPayload(value, "sp.in.new");
    builder_.CreateStore(value, newValPtr);

    llvm::Value *newLen = builder_.CreateAdd(
        curLen, llvm::ConstantInt::get(i64Ty_, 1), "sp.in.new_len");
    builder_.CreateStore(newLen, inLenPtr);

    emitBucketInsertAndRehashCheck(
        currentMap, mapHeaderTy_, kMapLayout.lenIdx,
        kMapLayout.bucketCountIdx, kMapLayout.bucketsPtrIdx,
        leafKey, ptrTy_, curLen);

    emitBranchUncond(doneBB);

    builder_.SetInsertPoint(doneBB);
    return llvm::ConstantInt::get(i64Ty_, 0);  // Unit
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
