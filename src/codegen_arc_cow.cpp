#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"
#include <cassert>


namespace ry {

// ===== Copy-on-Write (CoW) support =====

llvm::AllocaInst *CodeGen::tryGetReceiverAlloca(const ExprNode &expr) {
    if (auto *ve = std::get_if<VariableExpr>(&expr.data))
        return findVar(ve->name);
    return nullptr;
}

void CodeGen::emitCowRetainArcElements(llvm::Value *buf, llvm::Value *len,
                                        const std::string &tag,
                                        CollectionKind elemArcKind) {
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
    // str elements have StringHeader at offset -24; other ARC objects at -16.
    auto *hdr = (elemArcKind == CollectionKind::Str)
        ? emitStrGetHeaderFromData(elem) : emitArcGetHeaderFromData(elem);
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

    // Delegate to the generalized slot-based form. The trivial `var[i] = v`
    // path does NOT retain elements during clone: the canonical #855
    // retain-then-release-old slot protocol at the leaf mutation still owns
    // the "transfer ownership from old to new slot value" semantics, and
    // preserving that keeps existing 1-hop CoW tests unchanged.
    return emitCowCheckSlot(dataPtr, alloca, kind, /*retainElements=*/false);
}

// ===== Slot-based CoW (#854 path CoW support) =====

llvm::Value *CodeGen::emitCowCheckSlot(llvm::Value *dataPtr,
                                         llvm::Value *slotPtr,
                                         CollectionKind kind,
                                         bool retainElements) {
    if (!slotPtr)
        return dataPtr;

    auto *fn = builder_.GetInsertBlock()->getParent();
    auto *headerPtr = emitArcGetHeaderFromData(dataPtr);
    auto *strongPtr = builder_.CreateStructGEP(arcHeaderTy_, headerPtr, 0, "cow_strong_ptr");
    // Acquire ordering in atomic context pairs with the atomicrmw retain/
    // release in emitArcRetain/Release and closes the TOCTOU window TSan
    // flagged when multiple workers CoW on the same captured value (#630).
    auto *strong = emitAtomicI64Load(strongPtr,
        isArcAtomic(dataPtr) ? llvm::AtomicOrdering::Acquire : llvm::AtomicOrdering::NotAtomic,
        "cow_strong");

    // Skip if unique (strong_count == 1) or immortal (string literals, etc.)
    auto *isUnique = builder_.CreateICmpEQ(strong, llvm::ConstantInt::get(i64Ty_, 1), "cow_unique");
    auto *isImmortal = builder_.CreateICmpEQ(strong, llvm::ConstantInt::get(i64Ty_, ARC_IMMORTAL), "cow_immortal");
    auto *skipCow = builder_.CreateOr(isUnique, isImmortal, "cow_skip");

    auto *copyBB = llvm::BasicBlock::Create(*ctx_, "cow.copy", fn);
    auto *contBB = llvm::BasicBlock::Create(*ctx_, "cow.cont", fn);
    auto *origBB = builder_.GetInsertBlock();
    builder_.CreateCondBr(skipCow, contBB, copyBB);

    builder_.SetInsertPoint(copyBB);

    // Determine whether elements need retention BEFORE cloning: the
    // metadata query uses `dataPtr` which is the *old* container ptr that
    // still carries the element-type metadata propagated at construction.
    // The cloned buffer does not receive metadata automatically — we have
    // to drive the retain loop from here rather than from
    // `emitCowDeepCopyList` so the decision uses the correct source.
    CollectionKind elemArcKind = CollectionKind::List;
    bool hasArcElems = elementTypeIsArcManaged(dataPtr, kind, &elemArcKind);
    // str elements: destructor now releases them, so we must always retain
    // during CoW to keep reference counts balanced (#1046).
    bool doElemRetain = hasArcElems &&
        (retainElements || elemArcKind == CollectionKind::Str);

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
    case CollectionKind::Str:
        // str is immutable — CoW is not applicable; this path should never be reached.
        llvm_unreachable("emitCowClone: CollectionKind::Str is not a CoW container");
    }

    // Retain each ARC-managed element in the cloned buffer so the clone
    // shares ownership of nested state with the original. Without this,
    // the subsequent release of the old header would drop the strong
    // count of elements still reachable through the original alias to
    // zero, corrupting heap state. This is what wires up the previously-
    // unused `emitCowRetainArcElements` helper (#854 path CoW).
    if (doElemRetain) {
        llvm::Value *elemBuf = nullptr;
        llvm::Value *elemLen = nullptr;
        switch (kind) {
        case CollectionKind::List: {
            auto *lenPtr = builder_.CreateStructGEP(listHeaderTy_, newDataPtr, 0, "cow_ret_len_ptr");
            elemLen = builder_.CreateLoad(i64Ty_, lenPtr, "cow_ret_len");
            auto *dataField = builder_.CreateStructGEP(listHeaderTy_, newDataPtr, 2, "cow_ret_data_field");
            elemBuf = builder_.CreateLoad(ptrTy_, dataField, "cow_ret_data");
            break;
        }
        case CollectionKind::Map: {
            auto *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, newDataPtr, 0, "cow_ret_len_ptr");
            elemLen = builder_.CreateLoad(i64Ty_, lenPtr, "cow_ret_len");
            auto *valsField = builder_.CreateStructGEP(mapHeaderTy_, newDataPtr, 3, "cow_ret_vals_field");
            elemBuf = builder_.CreateLoad(ptrTy_, valsField, "cow_ret_vals");
            break;
        }
        case CollectionKind::Set: {
            auto *lenPtr = builder_.CreateStructGEP(setHeaderTy_, newDataPtr, 0, "cow_ret_len_ptr");
            elemLen = builder_.CreateLoad(i64Ty_, lenPtr, "cow_ret_len");
            auto *elemsField = builder_.CreateStructGEP(setHeaderTy_, newDataPtr, 2, "cow_ret_elems_field");
            elemBuf = builder_.CreateLoad(ptrTy_, elemsField, "cow_ret_elems");
            break;
        }
        case CollectionKind::Str:
            llvm_unreachable("emitCowClone retain loop: CollectionKind::Str is not a CoW container");
        }
        emitCowRetainArcElements(elemBuf, elemLen, "cow_elem", elemArcKind);
    }

    // Reuse headerPtr (dominates copyBB) instead of re-computing
    emitArcRelease(headerPtr, isArcAtomic(dataPtr),
                   getOrCreateCollectionDestructor(kind));

    builder_.CreateStore(newDataPtr, slotPtr);
    arc_owned_values_.insert(newDataPtr);

    auto *copyEndBB = builder_.GetInsertBlock();
    builder_.CreateBr(contBB);

    builder_.SetInsertPoint(contBB);
    auto *phi = builder_.CreatePHI(ptrTy_, 2, "cow_ptr");
    phi->addIncoming(dataPtr, origBB);
    phi->addIncoming(newDataPtr, copyEndBB);

    // Propagate metadata so downstream type queries on the result work.
    // When the slot is an alloca we can use the existing propagateMeta
    // keyed on the alloca; otherwise we copy from the source ptr which
    // still has its metadata.
    if (auto *alloca = llvm::dyn_cast<llvm::AllocaInst>(slotPtr)) {
        propagateMeta(alloca, phi);
    } else {
        propagateMetaWide(dataPtr, phi);
    }

    return phi;
}

// ===== Path CoW driver (#854) =====

// Walks `chain` (typically `s.object` from IndexAssignStmt / FieldAssignStmt)
// inside-out, privatizing every container level whose strong_count > 1 so
// the final mutation on the leaf cannot leak through aliases. Returns the
// privatized leaf container pointer. Throws codegenError for unsupported
// shapes (method-call roots, non-lvalue chains, etc.).
llvm::Value *CodeGen::emitPathCowForChain(ExprNode &chain) {
    // Base case: VariableExpr is the chain root. Privatize via the slot
    // (alloca or module-global storage) with retainElements=true so that
    // subsequent hops observe the correct refcount on inner containers.
    if (auto *ve = std::get_if<VariableExpr>(&chain.data)) {
        llvm::Value *slotPtr = nullptr;
        // Metadata anchor: for an alloca it's the alloca itself; for a
        // module global it's `ModuleBinding.original_alloca` (the real
        // storage in __ry_main__). `loadModuleGlobalStorage` returns the
        // trampoline-loaded storage pointer which has no metadata.
        llvm::AllocaInst *metaAnchor = nullptr;
        if (llvm::AllocaInst *alloca = findVar(ve->name)) {
            slotPtr = alloca;
            metaAnchor = alloca;
        } else if (auto *b = findModuleGlobal(ve->name)) {
            slotPtr = loadModuleGlobalStorage(*b, ve->name);
            metaAnchor = b->original_alloca;
        } else {
            codegenError("undefined variable: " + ve->name);
        }
        llvm::Value *containerPtr = builder_.CreateLoad(ptrTy_, slotPtr, ve->name + ".pcow_root");
        if (metaAnchor)
            propagateMeta(metaAnchor, containerPtr);
        CollectionKind kind = CollectionKind::List;
        if (getMapKeyType(containerPtr))
            kind = CollectionKind::Map;
        else if (getSetElementType(containerPtr))
            kind = CollectionKind::Set;
        return emitCowCheckSlot(containerPtr, slotPtr, kind, /*retainElements=*/true);
    }

    // Recursive case: IndexExpr hop. Privatize the parent first, then
    // reach into the new parent's data/vals buffer to find the child slot
    // and privatize the child stored there.
    if (auto *idxPtr = std::get_if<std::unique_ptr<IndexExpr>>(&chain.data)) {
        IndexExpr *idx = idxPtr->get();
        if (idx->indices.size() != 1)
            codegenError("path CoW: multi-index hop not supported");
        llvm::Value *parent = emitPathCowForChain(*idx->object);
        if (parent->getType() != ptrTy_)
            codegenError("path CoW: parent is not a heap collection");
        // Determine parent kind from metadata (propagated by recursive call).
        CollectionKind parentKind = CollectionKind::List;
        llvm::Type *mapKeyTy = getMapKeyType(parent);
        if (mapKeyTy)
            parentKind = CollectionKind::Map;
        llvm::Value *indexVal = emitExpr(*idx->indices[0]);

        llvm::Value *childSlot = nullptr;
        if (parentKind == CollectionKind::Map) {
            if (indexVal->getType() != mapKeyTy)
                codegenError("map key type mismatch in nested assignment");
            llvm::Type *mapValTy = getMapValueType(parent);
            if (!mapValTy)
                codegenError("cannot determine map value type for nested assignment");
            llvm::Value *slot = emitMapKeyLookup(parent, indexVal, mapKeyTy);
            llvm::Value *missing = builder_.CreateICmpSLT(
                slot, llvm::ConstantInt::get(i64Ty_, 0), "pcow_map_missing");
            auto *fn = builder_.GetInsertBlock()->getParent();
            auto *errBB = llvm::BasicBlock::Create(*ctx_, "pcow_map_err", fn);
            auto *okBB = llvm::BasicBlock::Create(*ctx_, "pcow_map_ok", fn);
            builder_.CreateCondBr(missing, errBB, okBB);
            builder_.SetInsertPoint(errBB);
            emitRuntimeError("runtime error: missing map key in nested assignment\n",
                             ".pcow_map_missing");
            builder_.SetInsertPoint(okBB);
            llvm::Value *valsField = builder_.CreateStructGEP(
                mapHeaderTy_, parent, 3, "pcow_vals_field");
            llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsField, "pcow_vals");
            childSlot = builder_.CreateGEP(mapValTy, valsPtr, {slot}, "pcow_slot");
        } else {
            // List hop
            llvm::Type *elemTy = getListElementType(parent);
            if (!elemTy)
                codegenError("cannot determine list element type for nested assignment");
            llvm::Value *lenPtr = builder_.CreateStructGEP(
                listHeaderTy_, parent, 0, "pcow_len_ptr");
            llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "pcow_length");
            emitBoundsCheck(indexVal, length,
                            "runtime error: index %lld out of bounds for list of length %lld\n",
                            ".pcow_list_err", "pcow_list");
            llvm::Value *dataField = builder_.CreateStructGEP(
                listHeaderTy_, parent, 2, "pcow_data_field");
            llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataField, "pcow_data");
            childSlot = builder_.CreateGEP(elemTy, dataPtr, {indexVal}, "pcow_slot");
        }

        llvm::Value *child = builder_.CreateLoad(ptrTy_, childSlot, "pcow_child");
        // Propagate inner-container type metadata from the parent's
        // element_type_name so downstream emitCowCheckSlot / recursive
        // hops can detect the child's kind and element ARC-ness.
        std::string childTypeName;
        if (auto *parentMeta = getMeta(parent)) {
            if (parentKind == CollectionKind::Map)
                childTypeName = parentMeta->map_value_type_name;
            else
                childTypeName = parentMeta->list_elem_type_name;
        }
        if (!childTypeName.empty())
            propagateTypeMeta(childTypeName, child);
        CollectionKind childKind = CollectionKind::List;
        if (getMapKeyType(child))
            childKind = CollectionKind::Map;
        else if (getSetElementType(child))
            childKind = CollectionKind::Set;
        return emitCowCheckSlot(child, childSlot, childKind, /*retainElements=*/true);
    }

    // Recursive case: FieldAccessExpr hop reaching an ARC container
    // field through a (possibly nested) record path. The chain root
    // must be a simple variable so we can GEP into its backing
    // storage — we then walk the entire FAE chain from outermost
    // variable down to the ARC-container field and compute a single
    // multi-level struct GEP that yields a writable slot.
    // Method-call roots (`f().items[i] = v`) and field chains
    // interleaved with IndexExpr hops (`rec.arr[0].items[i] = v`)
    // are not supported in the narrow #854 scope.
    if (std::get_if<std::unique_ptr<FieldAccessExpr>>(&chain.data)) {
        // `fieldChain` is ordered leaf-first (outermost FAE at index 0).
        std::vector<FieldAccessExpr *> fieldChain;
        ExprNode *cur = &chain;
        while (auto *fp = std::get_if<std::unique_ptr<FieldAccessExpr>>(&cur->data)) {
            fieldChain.push_back(fp->get());
            cur = fp->get()->object.get();
        }
        auto *ve = std::get_if<VariableExpr>(&cur->data);
        if (!ve)
            codegenError("path CoW: record base must be a variable "
                         "(method-call roots and interleaved index hops not supported)");
        llvm::Value *storagePtr = nullptr;
        llvm::Type *curTy = nullptr;
        if (llvm::AllocaInst *alloca = findVar(ve->name)) {
            storagePtr = alloca;
            curTy = alloca->getAllocatedType();
        } else if (auto *b = findModuleGlobal(ve->name)) {
            storagePtr = loadModuleGlobalStorage(*b, ve->name);
            curTy = b->valueTy();
        } else {
            codegenError("undefined variable: " + ve->name);
        }
        // Walk from root (closest to variable) to leaf (ARC field).
        // `fieldChain` is ordered leaf-first, so iterate in reverse.
        llvm::StructType *curSt = nullptr;
        llvm::Type *fieldTy = nullptr;
        std::string fieldTypeName;
        for (int i = static_cast<int>(fieldChain.size()) - 1; i >= 0; --i) {
            FieldAccessExpr *thisFa = fieldChain[static_cast<size_t>(i)];
            curSt = llvm::dyn_cast<llvm::StructType>(curTy);
            if (!curSt)
                codegenError("path CoW: non-record intermediate in record chain");
            auto sit = record_types_.find(curSt->getName().str());
            if (sit == record_types_.end())
                codegenError("unknown record type: " + curSt->getName().str());
            int fieldIdx = sit->second.findField(thisFa->field);
            if (fieldIdx < 0)
                codegenError("type '" + sit->first + "' has no field '" + thisFa->field + "'");
            storagePtr = builder_.CreateStructGEP(
                curSt, storagePtr, static_cast<unsigned>(fieldIdx),
                "pcow_" + thisFa->field + "_slot");
            fieldTy = curSt->getElementType(static_cast<unsigned>(fieldIdx));
            if (sit->second.fields[static_cast<size_t>(fieldIdx)].type)
                fieldTypeName = sit->second.fields[static_cast<size_t>(fieldIdx)].type->toString();
            else
                fieldTypeName.clear();
            curTy = fieldTy;
        }
        // `storagePtr` is now the slot pointer for the innermost field.
        // If that field is not ARC-managed (e.g. `d.tag = ...` where
        // tag is a str), path CoW has nothing to do — fall through to
        // a plain no-op that just returns the loaded value. The caller
        // will treat that as a non-lvalue leaf and let the regular
        // index/field assignment code paths fire.
        if (!fieldTypeName.empty() && fieldTypeIsArcManaged(fieldTypeName)) {
            llvm::Value *fieldContainer = builder_.CreateLoad(
                fieldTy, storagePtr, "pcow_field_val");
            propagateTypeMeta(fieldTypeName, fieldContainer);
            CollectionKind fieldKind = CollectionKind::List;
            if (getMapKeyType(fieldContainer))
                fieldKind = CollectionKind::Map;
            else if (getSetElementType(fieldContainer))
                fieldKind = CollectionKind::Set;
            return emitCowCheckSlot(fieldContainer, storagePtr, fieldKind,
                                    /*retainElements=*/true);
        }
        // Non-ARC field: just load and return. No CoW is applicable —
        // the caller's leaf mutation path will handle this.
        llvm::Value *fieldContainer = builder_.CreateLoad(
            fieldTy, storagePtr, "pcow_nonarc_field");
        if (!fieldTypeName.empty())
            propagateTypeMeta(fieldTypeName, fieldContainer);
        return fieldContainer;
    }

    codegenError("path CoW: chain root must be an lvalue (variable, field access, or index)");
    return nullptr;
}

// ===== Closure ARC support =====

CodeGen::CapturedArcKind CodeGen::detectCapturedArcKind(llvm::AllocaInst *alloca) const {
    if (getTypeMeta(TypeMeta::ListElem, alloca))
        return CapturedArcKind::List;
    if (getTypeMeta(TypeMeta::MapKey, alloca))
        return CapturedArcKind::Map;
    if (getTypeMeta(TypeMeta::SetElem, alloca))
        return CapturedArcKind::Set;
    if (closure_managed_vars_.count(alloca))
        return CapturedArcKind::Closure;
    // Uniform closures stored in function-type params are ARC-managed
    {
        auto *meta = getMeta(alloca);
        if (meta && meta->fn_type_info && meta->fn_type_info->isUniformClosure)
            return CapturedArcKind::Closure;
    }
    if (resource_managed_vars_.count(alloca))
        return CapturedArcKind::Resource;
    if (arc_str_managed_vars_.count(alloca))
        return CapturedArcKind::Str;
    if (isArcManaged(alloca))
        return CapturedArcKind::Generic; // ARC-managed but no sub-destructor (e.g., f-strings)
    return CapturedArcKind::None;
}

llvm::FunctionCallee CodeGen::getOrCreateClosureDestructor(const FnTypeInfo &info) {
    // Check if any captured variable needs ARC release
    bool hasArc = false;
    for (auto k : info.capturedArcKinds)
        if (k != CapturedArcKind::None) { hasArc = true; break; }
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
        if (info.capturedArcKinds[i] == CapturedArcKind::None)
            continue;

        auto *capField = builder_.CreateStructGEP(
            closureTy, dataPtr, static_cast<unsigned>(i + 1), "dtor.cap." + std::to_string(i));
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
        auto *hdr = (info.capturedArcKinds[i] == CapturedArcKind::Str)
            ? emitStrGetHeaderFromData(capVal)
            : emitArcGetHeaderFromData(capVal);

        // Resolve sub-destructor based on captured ARC kind
        llvm::FunctionCallee subDtor;
        switch (info.capturedArcKinds[i]) {
        case CapturedArcKind::List:
            subDtor = getOrCreateCollectionDestructor(CollectionKind::List);
            break;
        case CapturedArcKind::Map:
            subDtor = getOrCreateCollectionDestructor(CollectionKind::Map);
            break;
        case CapturedArcKind::Set:
            subDtor = getOrCreateCollectionDestructor(CollectionKind::Set);
            break;
        case CapturedArcKind::Resource: {
            assert(i < info.capturedResourceKinds.size());
            ResourceKind rk = info.capturedResourceKinds[i];
            if (rk != ResourceKindRegistry::NONE)
                subDtor = getOrCreateResourceDestructor(rk);
            break;
        }
        case CapturedArcKind::Closure: {
            if (info.capturedClosureInfos) {
                auto cit = info.capturedClosureInfos->find(i);
                if (cit != info.capturedClosureInfos->end()) {
                    if (cit->second.isUniformClosure) {
                        auto *ucDtor = getOrCreateUniformClosureDestructor();
                        subDtor = llvm::FunctionCallee(ucDtor->getFunctionType(), ucDtor);
                    } else if (!cit->second.capturedArcKinds.empty()) {
                        subDtor = getOrCreateClosureDestructor(cit->second);
                    }
                }
            }
            break;
        }
        case CapturedArcKind::Str:
        case CapturedArcKind::Generic:
        case CapturedArcKind::None:
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

} // namespace ry
