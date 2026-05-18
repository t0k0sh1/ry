#include "ry/codegen.hpp"


namespace ry {

bool CodeGen::isAnyType(llvm::Type *ty) const {
    return ty == anyTy_;
}

bool CodeGen::canAnyHoldType(llvm::Type *ty) const {
    return ty == i64Ty_ || ty == f64Ty_ || ty == i1Ty_ || ty == ptrTy_;
}

int64_t CodeGen::getAnyTypeTag(llvm::Type *ty) {
    if (ty == i64Ty_)  return static_cast<int64_t>(RyAnyTag::Int);
    if (ty == f64Ty_)  return static_cast<int64_t>(RyAnyTag::Float);
    if (ty == i1Ty_)   return static_cast<int64_t>(RyAnyTag::Bool);
    if (ty == ptrTy_)  return static_cast<int64_t>(RyAnyTag::Str);
    codegenError("type error: 'any' can only hold int/float/bool/str");
}

int64_t CodeGen::getAnyTypeTagForValue(llvm::Value *val) {
    if (val->getType() == ptrTy_) {
        if (auto *meta = getMeta(val)) {
            if (meta->list_elem) return static_cast<int64_t>(RyAnyTag::List);
            if (meta->map_key || meta->map_value)
                return static_cast<int64_t>(RyAnyTag::Map);
            if (meta->set_elem) return static_cast<int64_t>(RyAnyTag::Set);
        }
    }
    return getAnyTypeTag(val->getType());
}

bool CodeGen::isNonStrPointer(llvm::Value *val) {
    if (val->getType() != ptrTy_) return false;
    auto *meta = getMeta(val);
    return meta && meta->hasAnyMeta();
}

bool CodeGen::isStringValue(llvm::Value *val) {
    return val->getType() == ptrTy_ && !isNonStrPointer(val);
}

llvm::Value *CodeGen::wrapInAny(llvm::Value *val) {
    // Record types are stored on the heap as `[ ArcHeader (16B) | descriptor ptr
    // (8B) | record struct ]` because the inner struct is generally larger than
    // the 8-byte `data[8]` slot. The slot holds the box's data-region pointer
    // (i.e. headerPtr + 16). Cross-function-boundary type info is preserved by
    // the per-type descriptor at offset 0 of the data region, so `any`-typed
    // function returns / aliases survive even when the static type name is lost.
    if (auto *recordStructTy = llvm::dyn_cast<llvm::StructType>(val->getType())) {
        const RecordInfo *info = findRecordInfoForType(recordStructTy);
        if (!info)
            codegenError("type error: 'any' cannot hold this struct type — "
                         "only record types declared with `record` are supported");
        std::string typeName = findRecordTypeName(recordStructTy);
        if (typeName.empty())
            codegenError("type error: 'any' record wrap could not resolve "
                         "source-level type name");

        const llvm::DataLayout &dl = mod_->getDataLayout();
        auto *layoutTy = recordBoxLayoutType(recordStructTy);
        uint64_t boxDataSize = dl.getTypeAllocSize(layoutTy);
        auto *boxDataSizeC = llvm::ConstantInt::get(i64Ty_, boxDataSize);

        auto *desc = getOrCreateRecordDescriptor(typeName, recordStructTy);
        auto *headerPtr = emitArcAlloc(boxDataSizeC);
        auto *dataPtr = emitArcGetDataPtr(headerPtr);

        auto *descPtrSlot = builder_.CreateStructGEP(layoutTy, dataPtr, 0,
                                                     "any.rec.desc.slot");
        builder_.CreateStore(desc, descPtrSlot);
        auto *fieldsSlot = builder_.CreateStructGEP(layoutTy, dataPtr, 1,
                                                    "any.rec.fields.slot");
        builder_.CreateStore(val, fieldsSlot);

        // Per [[codegen-arc-cow]] "Record ARC reassignment guard": every value
        // that is not a fresh `CallInst` / `InvokeInst` is treated as a view —
        // including `InsertValueInst` chains from record literals. The helper
        // internally skips inline-owned values via `arc_owned_values_` /
        // `arc_str_owned_values_`, so plain literal field values (newly-built
        // collections, fresh strings) are not over-retained.
        if (!llvm::isa<llvm::CallInst>(val) && !llvm::isa<llvm::InvokeInst>(val)) {
            emitRecordArcFieldsRetain(val, recordStructTy);
        }

        llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr, "any.rec.tmp");
        auto *tagSlot = builder_.CreateStructGEP(anyTy_, tmp, 0, "any.rec.tag");
        builder_.CreateStore(llvm::ConstantInt::get(
                                 i64Ty_, static_cast<uint64_t>(RyAnyTag::Record)),
                             tagSlot);
        auto *anyDataSlot = builder_.CreateStructGEP(anyTy_, tmp, 1, "any.rec.data");
        builder_.CreateStore(dataPtr, anyDataSlot);
        return builder_.CreateLoad(anyTy_, tmp, "any.rec.val");
    }

    // Collections (List / Map / Set) are wrap-eligible from #1697; resources,
    // function pointers, enums and other non-collection pointer-shaped
    // values remain rejected. Discriminate via metadata.
    bool isCollection = false;
    auto *meta = (val->getType() == ptrTy_) ? getMeta(val) : nullptr;
    if (meta) {
        isCollection = meta->list_elem || meta->map_key || meta->map_value ||
                       meta->set_elem;
    }
    if (isNonStrPointer(val) && !isCollection)
        codegenError("type error: 'any' can only hold int/float/bool/str, "
                     "List/Map/Set, and record types; non-collection pointer "
                     "types (resources, function pointers, enums, etc.) are "
                     "not supported");

    int64_t tag = isCollection ? getAnyTypeTagForValue(val)
                               : getAnyTypeTag(val->getType());

    // Bool (i1) must be zero-extended to i64 so that the runtime's 8-byte
    // memcpy reads a well-defined 0/1 value instead of uninitialized bytes.
    if (val->getType()->isIntegerTy(1))
        val = builder_.CreateZExt(val, i64Ty_, "any.bool.zext");

    // Collection wrap retains the underlying header so the inner List/Map/Set
    // outlives the source binding even if the source goes out of scope first.
    // Pairs with the scope-end release emitted by `emitAnyReleaseVar`.
    // str payloads are also retained — they are ARC-managed via StringHeader
    // (offset -24, NOT -16 — use emitStrGetHeaderFromData not
    // emitArcGetHeaderFromData). Literals are guarded by ARC_IMMORTAL inside
    // emitArcRetain so the increment is a no-op.
    if (isCollection) {
        auto *hdr = emitArcGetHeaderFromData(val);
        emitArcRetain(hdr);
    } else if (isStringValue(val)) {
        auto *hdr = emitStrGetHeaderFromData(val);
        emitArcRetain(hdr);
    }

    // alloca required: data field is [8 x i8], val type differs (type punning)
    llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr, "any.tmp");
    auto *tagPtr = builder_.CreateStructGEP(anyTy_, tmp, 0, "any.tag");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(tag)), tagPtr);
    auto *dataPtr = builder_.CreateStructGEP(anyTy_, tmp, 1, "any.data");
    builder_.CreateStore(val, dataPtr);
    return builder_.CreateLoad(anyTy_, tmp, "any.val");
}

llvm::Value *CodeGen::buildUnitAny() {
    llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr, "any.unit.tmp");
    auto *tagPtr = builder_.CreateStructGEP(anyTy_, tmp, 0, "any.unit.tag");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, static_cast<int64_t>(RyAnyTag::Unit)), tagPtr);
    auto *dataPtr = builder_.CreateStructGEP(anyTy_, tmp, 1, "any.unit.data");
    builder_.CreateStore(
        llvm::Constant::getNullValue(anyTy_->getElementType(1)),
        dataPtr);
    return builder_.CreateLoad(anyTy_, tmp, "any.unit.val");
}

llvm::Value *CodeGen::unwrapFromAny(llvm::Value *anyVal, llvm::Type *targetTy,
                                     const std::string &targetTypeName) {
    llvm::Value *tag = builder_.CreateExtractValue(anyVal, 0, "any.tag.val");
    llvm::Function *fn = builder_.GetInsertBlock()->getParent();

    // Record unwrap must dispatch at entry: `getAnyTypeTag(targetTy)` below
    // emits `codegenError` when `targetTy` is a `StructType`, so the standard
    // 2-way path never sees record targets. Compare against the expected
    // descriptor pointer to enforce exact-type unwrap; cross-type projection
    // (`let p: Parent = anyHoldingChild`) is intentionally out of scope and
    // traps at runtime via the descriptor-mismatch branch.
    if (auto *recordStructTy = llvm::dyn_cast<llvm::StructType>(targetTy)) {
        const RecordInfo *info = findRecordInfoForType(recordStructTy);
        if (!info)
            codegenError("type error: unwrapping 'any' to non-record struct "
                         "type is not supported");
        std::string typeName = findRecordTypeName(recordStructTy);
        if (typeName.empty())
            codegenError("type error: 'any' record unwrap could not resolve "
                         "source-level type name");

        auto *expectedDesc = getOrCreateRecordDescriptor(typeName, recordStructTy);
        auto *layoutTy = recordBoxLayoutType(recordStructTy);

        auto *tagMatchBB = llvm::BasicBlock::Create(*ctx_, "any.rec.tag_ok", fn);
        auto *tagMismatchBB = llvm::BasicBlock::Create(*ctx_, "any.rec.tag_err", fn);
        auto *descCheckBB = llvm::BasicBlock::Create(*ctx_, "any.rec.desc_check", fn);
        auto *descMismatchBB = llvm::BasicBlock::Create(*ctx_, "any.rec.desc_err", fn);

        llvm::Value *isRecord = builder_.CreateICmpEQ(
            tag, llvm::ConstantInt::get(
                     i64Ty_, static_cast<uint64_t>(RyAnyTag::Record)),
            "any.is_record");
        builder_.CreateCondBr(isRecord, tagMatchBB, tagMismatchBB);

        builder_.SetInsertPoint(tagMismatchBB);
        emitRuntimeError("runtime error: any type mismatch (expected " + typeName +
                             ", got non-record)\n",
                         ".any_type_err");

        // Stash anyVal into an alloca so we can re-read the data field as ptr.
        builder_.SetInsertPoint(tagMatchBB);
        llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr, "any.rec.tmp");
        builder_.CreateStore(anyVal, tmp);
        auto *anyDataSlot = builder_.CreateStructGEP(anyTy_, tmp, 1, "any.rec.data.ptr");
        auto *dataPtr = builder_.CreateLoad(ptrTy_, anyDataSlot, "any.rec.data");
        auto *descSlot = builder_.CreateStructGEP(layoutTy, dataPtr, 0,
                                                  "any.rec.desc.slot");
        auto *actualDesc = builder_.CreateLoad(ptrTy_, descSlot, "any.rec.desc");
        // Walk the runtime descriptor `parent_desc` chain to admit subtype
        // unwrap (`let a: Parent = anyHoldingChild`) (#1802). Exact-type
        // unwrap stays valid: actual == expected returns true on the first
        // iteration; unrelated records walk to null and return false.
        llvm::FunctionType *isSubtypeTy =
            llvm::FunctionType::get(i64Ty_, {ptrTy_, ptrTy_}, false);
        llvm::FunctionCallee isSubtypeFn = mod_->getOrInsertFunction(
            "__ry_record_is_subtype_desc", isSubtypeTy);
        llvm::Value *chainOk = builder_.CreateCall(
            isSubtypeFn, {actualDesc, expectedDesc}, "any.rec.chain.ok");
        llvm::Value *chainBool = builder_.CreateICmpNE(
            chainOk, llvm::ConstantInt::get(i64Ty_, 0), "any.rec.chain.bool");
        builder_.CreateCondBr(chainBool, descCheckBB, descMismatchBB);

        builder_.SetInsertPoint(descMismatchBB);
        // Unrelated record types still trap. Descriptor pointer identity ==
        // record-type identity (one global per record type), and the chain
        // walk reaches null without finding `expectedDesc`.
        emitRuntimeError("runtime error: any record type mismatch (expected " +
                             typeName + ", got a different record type)\n",
                         ".any_rec_err");

        builder_.SetInsertPoint(descCheckBB);
        auto *fieldsSlot = builder_.CreateStructGEP(layoutTy, dataPtr, 1,
                                                    "any.rec.fields.slot");
        llvm::Value *recordVal = builder_.CreateLoad(recordStructTy, fieldsSlot,
                                                     "any.rec.unwrap.val");
        // The unwrapped record becomes a new alias to the boxed value.
        // ARC fields in the inner record need to be retained so the unwrapped
        // value can release them independently from the box's own dtor when
        // the box is later dropped.
        emitRecordArcFieldsRetain(recordVal, recordStructTy);
        return recordVal;
    }

    // int→float auto-promotion: accept both static_cast<int64_t>(RyAnyTag::Float) and static_cast<int64_t>(RyAnyTag::Int)
    if (targetTy == f64Ty_) {
        auto *floatBB = llvm::BasicBlock::Create(*ctx_, "any.float", fn);
        auto *checkIntBB = llvm::BasicBlock::Create(*ctx_, "any.check_int", fn);
        auto *intPromoteBB = llvm::BasicBlock::Create(*ctx_, "any.int2float", fn);
        auto *mismatchBB = llvm::BasicBlock::Create(*ctx_, "any.mismatch", fn);
        auto *mergeBB = llvm::BasicBlock::Create(*ctx_, "any.merge", fn);

        // Shared alloca for type-punning the data field (used by both branches)
        llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr, "any.tmp.fp");
        builder_.CreateStore(anyVal, tmp);
        auto *dataPtr = builder_.CreateStructGEP(anyTy_, tmp, 1, "any.data.fp");

        llvm::Value *isFloat = builder_.CreateICmpEQ(
            tag, llvm::ConstantInt::get(i64Ty_, static_cast<int64_t>(RyAnyTag::Float)), "is.float");
        builder_.CreateCondBr(isFloat, floatBB, checkIntBB);

        builder_.SetInsertPoint(checkIntBB);
        llvm::Value *isInt = builder_.CreateICmpEQ(
            tag, llvm::ConstantInt::get(i64Ty_, static_cast<int64_t>(RyAnyTag::Int)), "is.int");
        builder_.CreateCondBr(isInt, intPromoteBB, mismatchBB);

        builder_.SetInsertPoint(mismatchBB);
        emitRuntimeError("runtime error: any type mismatch (expected float or int)\n", ".any_type_err");

        builder_.SetInsertPoint(floatBB);
        llvm::Value *floatVal = builder_.CreateLoad(f64Ty_, dataPtr, "any.f64");
        builder_.CreateBr(mergeBB);

        builder_.SetInsertPoint(intPromoteBB);
        llvm::Value *intVal = builder_.CreateLoad(i64Ty_, dataPtr, "any.i64");
        llvm::Value *promoted = builder_.CreateSIToFP(intVal, f64Ty_, "any.i2f");
        builder_.CreateBr(mergeBB);

        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = builder_.CreatePHI(f64Ty_, 2, "any.unwrap.f64");
        phi->addIncoming(floatVal, floatBB);
        phi->addIncoming(promoted, intPromoteBB);
        return phi;
    }

    // Standard 2-way: exact tag match or error. For collection unwraps the
    // expected tag is derived from `targetTypeName` (List<…> / Map<…> /
    // Set<…>); empty / "str" / primitive names use the type-driven default
    // (Str tag for ptr).
    int64_t expectedTag = getAnyTypeTag(targetTy);
    bool isCollectionUnwrap = false;
    if (targetTy == ptrTy_ && !targetTypeName.empty()) {
        std::string resolved = resolveTypeAlias(targetTypeName);
        if (isListTypeName(resolved)) {
            expectedTag = static_cast<int64_t>(RyAnyTag::List);
            isCollectionUnwrap = true;
        } else if (isMapTypeName(resolved)) {
            expectedTag = static_cast<int64_t>(RyAnyTag::Map);
            isCollectionUnwrap = true;
        } else if (isSetTypeName(resolved)) {
            expectedTag = static_cast<int64_t>(RyAnyTag::Set);
            isCollectionUnwrap = true;
        }
    }
    // str unwrap creates a new alias to the inner StringHeader. Mirror the
    // collection retain so the unwrapped destination can release at its own
    // scope exit without double-freeing the source.
    bool isStrUnwrap =
        targetTy == ptrTy_ && !isCollectionUnwrap &&
        expectedTag == static_cast<int64_t>(RyAnyTag::Str);
    llvm::Value *cmp = builder_.CreateICmpEQ(
        tag, llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(expectedTag)), "any.tag.check");

    auto *matchBB = llvm::BasicBlock::Create(*ctx_, "any.match", fn);
    auto *mismatchBB = llvm::BasicBlock::Create(*ctx_, "any.mismatch", fn);

    builder_.CreateCondBr(cmp, matchBB, mismatchBB);

    builder_.SetInsertPoint(mismatchBB);
    std::string typeName;
    if (!targetTypeName.empty()) {
        typeName = targetTypeName;
    } else {
        typeName = (targetTy == i64Ty_) ? "int"
                 : (targetTy == i1Ty_)  ? "bool"
                 : (targetTy == ptrTy_) ? "str"
                                        : "unknown";
    }
    emitRuntimeError("runtime error: any type mismatch (expected " + typeName + ")\n",
                     ".any_type_err");

    // Use anyTy_ alloca for proper alignment when type-punning the data field
    builder_.SetInsertPoint(matchBB);
    llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr, "any.tmp");
    builder_.CreateStore(anyVal, tmp);
    auto *dataPtr = builder_.CreateStructGEP(anyTy_, tmp, 1, "any.data.ptr");
    llvm::Value *unwrapped = builder_.CreateLoad(targetTy, dataPtr, "any.unwrap.val");

    // Collection unwrap creates a new alias to the inner header — retain so
    // both the source `any` and the unwrapped destination can release
    // independently at their respective scope exits. str follows the same
    // pattern but uses the StringHeader offset (-24) via
    // emitStrGetHeaderFromData.
    if (isCollectionUnwrap) {
        auto *hdr = emitArcGetHeaderFromData(unwrapped);
        emitArcRetain(hdr);
    } else if (isStrUnwrap) {
        auto *hdr = emitStrGetHeaderFromData(unwrapped);
        emitArcRetain(hdr);
    }
    return unwrapped;
}

void CodeGen::registerAnyManagedVar(llvm::AllocaInst *alloca,
                                     const std::string &sourceTypeName) {
    if (!alloca || alloca->getAllocatedType() != anyTy_) return;
    auto it = arc_any_managed_vars_.find(alloca);
    if (it == arc_any_managed_vars_.end()) {
        arc_any_managed_vars_.emplace(alloca, sourceTypeName);
    } else if (it->second.empty() && !sourceTypeName.empty()) {
        // Upgrade from "unknown source" to a concrete type name when later
        // declarations supply it.
        it->second = sourceTypeName;
    }
}

void CodeGen::emitAnyReleaseVar(const std::string &name,
                                 llvm::AllocaInst *alloca,
                                 const std::string &sourceTypeName) {
    if (!alloca || alloca->getAllocatedType() != anyTy_) return;
    auto *parentFn = builder_.GetInsertBlock()->getParent();

    auto *loaded = builder_.CreateLoad(anyTy_, alloca, name + ".any.load");
    auto *tagVal = builder_.CreateExtractValue(loaded, 0, name + ".any.tag");
    auto *dataPtr = builder_.CreateStructGEP(anyTy_, alloca, 1, name + ".any.data");

    auto *strBB  = llvm::BasicBlock::Create(*ctx_, name + ".any.rel.str",  parentFn);
    auto *listBB = llvm::BasicBlock::Create(*ctx_, name + ".any.rel.list", parentFn);
    auto *mapBB  = llvm::BasicBlock::Create(*ctx_, name + ".any.rel.map",  parentFn);
    auto *setBB  = llvm::BasicBlock::Create(*ctx_, name + ".any.rel.set",  parentFn);
    auto *recBB  = llvm::BasicBlock::Create(*ctx_, name + ".any.rel.rec",  parentFn);
    auto *doneBB = llvm::BasicBlock::Create(*ctx_, name + ".any.rel.done", parentFn);

    auto *sw = builder_.CreateSwitch(tagVal, doneBB, 5);
    auto *intTy = llvm::cast<llvm::IntegerType>(i64Ty_);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Str)),    strBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::List)),   listBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Map)),    mapBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Set)),    setBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Record)), recBB);

    // str payload release: load the handle, get the StringHeader (offset -24
    // via emitStrGetHeaderFromData — NOT emitArcGetHeaderFromData which would
    // read the wrong word), and emitArcRelease without a destructor (str has
    // no inner allocations). Mirrors emitArcReleaseVar's str path.
    builder_.SetInsertPoint(strBB);
    auto *strPtr = builder_.CreateLoad(ptrTy_, dataPtr, name + ".any.str.ptr");
    auto *strHdr = emitStrGetHeaderFromData(strPtr);
    emitArcRelease(strHdr, isArcAtomic(strPtr), llvm::FunctionCallee{}, nullptr);
    builder_.CreateBr(doneBB);

    auto releaseSlot = [&](llvm::BasicBlock *bb, CollectionKind kind,
                           const std::string &fallbackTypeName) {
        builder_.SetInsertPoint(bb);
        auto *ptr = builder_.CreateLoad(ptrTy_, dataPtr, name + ".any.ptr");
        // Element-type metadata is erased once the value enters `any`; use the
        // declared source type from registration when available, else fall
        // back to the generic destructor for that kind. Only use the source
        // name when its collection kind matches the runtime tag — an `any`
        // alloca may be reassigned to a different collection kind (e.g.
        // declared with a List value then later assigned a Map), and using
        // the stale source name would route through the wrong destructor.
        std::string useTypeName = sourceTypeName;
        bool sourceMatchesKind = false;
        if (!useTypeName.empty()) {
            std::string canon = resolveTypeAlias(useTypeName);
            switch (kind) {
                case CollectionKind::List:
                    sourceMatchesKind = isListTypeName(canon);
                    break;
                case CollectionKind::Map:
                    sourceMatchesKind = isMapTypeName(canon);
                    break;
                case CollectionKind::Set:
                    sourceMatchesKind = isSetTypeName(canon);
                    break;
                case CollectionKind::Str:
                    sourceMatchesKind = false;
                    break;
            }
        }
        if (!sourceMatchesKind) {
            useTypeName = fallbackTypeName;
        }
        emitArcReleaseLoadedElement(ptr, kind, useTypeName,
                                     name + ".any." + fallbackTypeName);
        // emitArcReleaseLoadedElement leaves builder_ on its own continuation
        // block; branch from there to the merge.
        builder_.CreateBr(doneBB);
    };

    releaseSlot(listBB, CollectionKind::List, "List");
    releaseSlot(mapBB,  CollectionKind::Map,  "Map");
    releaseSlot(setBB,  CollectionKind::Set,  "Set");

    // Record release: the box stores `[ArcHeader | descriptor ptr | record_struct]`,
    // and the data slot holds the data-region pointer (= headerPtr + 16). Recover
    // the ArcHeader via the standard `-16` offset and dispatch through the
    // runtime trampoline `__ry_arc_dtor_record_dispatch`, which loads the
    // descriptor and calls the per-type dtor. Sole indirection: `emitArcRelease`
    // binds a compile-time FunctionCallee, so we cannot pass the per-type LLVM
    // dtor directly — the descriptor mediation lets every record box share one
    // ARC release call site.
    builder_.SetInsertPoint(recBB);
    auto *recPtr = builder_.CreateLoad(ptrTy_, dataPtr, name + ".any.rec.ptr");
    auto *recHdr = emitArcGetHeaderFromData(recPtr);
    auto *trampolineTy = llvm::FunctionType::get(builder_.getVoidTy(), {ptrTy_}, false);
    auto trampoline = mod_->getOrInsertFunction("__ry_arc_dtor_record_dispatch",
                                                trampolineTy);
    emitArcRelease(recHdr, isArcAtomic(recPtr), trampoline, nullptr);
    builder_.CreateBr(doneBB);

    builder_.SetInsertPoint(doneBB);
}

void CodeGen::emitAnyRetainPayload(llvm::Value *anyVal,
                                    const std::string &siteLabel) {
    if (!anyVal || anyVal->getType() != anyTy_) return;
    auto *parentFn = builder_.GetInsertBlock()->getParent();

    // Stash the value into a fresh alloca so we can re-GEP the data field as
    // a ptr regardless of the original tag.
    llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr,
                                                  siteLabel + ".any.retain.tmp");
    builder_.CreateStore(anyVal, tmp);
    auto *tagVal = builder_.CreateExtractValue(anyVal, 0,
                                                siteLabel + ".any.retain.tag");
    auto *dataPtr = builder_.CreateStructGEP(anyTy_, tmp, 1,
                                              siteLabel + ".any.retain.data");

    auto *strBB  = llvm::BasicBlock::Create(*ctx_, siteLabel + ".any.retain.str",
                                             parentFn);
    auto *collBB = llvm::BasicBlock::Create(*ctx_, siteLabel + ".any.retain.coll",
                                             parentFn);
    auto *doneBB = llvm::BasicBlock::Create(*ctx_, siteLabel + ".any.retain.done",
                                             parentFn);

    auto *sw = builder_.CreateSwitch(tagVal, doneBB, 5);
    auto *intTy = llvm::cast<llvm::IntegerType>(i64Ty_);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Str)),
                strBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::List)),
                collBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Map)),
                collBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Set)),
                collBB);
    // Record boxes use the same `-16` ARC header offset as collections, so a
    // single retain block covers all four (List / Map / Set / Record).
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Record)),
                collBB);

    // str retain uses the StringHeader offset (-24) via
    // emitStrGetHeaderFromData — distinct from collection retain which uses
    // the ARC header offset (-16) via emitArcGetHeaderFromData. Confusing the
    // two corrupts the StringHeader / ArcHeader words.
    builder_.SetInsertPoint(strBB);
    auto *strPtrR = builder_.CreateLoad(ptrTy_, dataPtr,
                                         siteLabel + ".any.retain.str.ptr");
    auto *strHdrR = emitStrGetHeaderFromData(strPtrR);
    emitArcRetain(strHdrR);
    builder_.CreateBr(doneBB);

    // All three collection tags route to the same retain block — the inner
    // header layout is identical (ARC_HEADER_SIZE prefix) for List / Map /
    // Set, so a single `emitArcGetHeaderFromData` + `emitArcRetain` covers
    // every kind.
    builder_.SetInsertPoint(collBB);
    auto *ptr = builder_.CreateLoad(ptrTy_, dataPtr, siteLabel + ".any.retain.ptr");
    auto *hdr = emitArcGetHeaderFromData(ptr);
    emitArcRetain(hdr);
    builder_.CreateBr(doneBB);

    builder_.SetInsertPoint(doneBB);
}

void CodeGen::emitAnyReleasePayload(llvm::Value *anyVal,
                                     const std::string &sourceTypeName,
                                     const std::string &siteLabel) {
    if (!anyVal || anyVal->getType() != anyTy_) return;
    auto *parentFn = builder_.GetInsertBlock()->getParent();

    llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr,
                                                   siteLabel + ".any.rel.tmp");
    builder_.CreateStore(anyVal, tmp);
    auto *tagVal = builder_.CreateExtractValue(anyVal, 0,
                                                siteLabel + ".any.rel.tag");
    auto *dataPtr = builder_.CreateStructGEP(anyTy_, tmp, 1,
                                              siteLabel + ".any.rel.data");

    auto *strBB  = llvm::BasicBlock::Create(*ctx_, siteLabel + ".any.rel.str",  parentFn);
    auto *listBB = llvm::BasicBlock::Create(*ctx_, siteLabel + ".any.rel.list", parentFn);
    auto *mapBB  = llvm::BasicBlock::Create(*ctx_, siteLabel + ".any.rel.map",  parentFn);
    auto *setBB  = llvm::BasicBlock::Create(*ctx_, siteLabel + ".any.rel.set",  parentFn);
    auto *recBB  = llvm::BasicBlock::Create(*ctx_, siteLabel + ".any.rel.rec",  parentFn);
    auto *doneBB = llvm::BasicBlock::Create(*ctx_, siteLabel + ".any.rel.done", parentFn);

    auto *sw = builder_.CreateSwitch(tagVal, doneBB, 5);
    auto *intTy = llvm::cast<llvm::IntegerType>(i64Ty_);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Str)),    strBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::List)),   listBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Map)),    mapBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Set)),    setBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Record)), recBB);

    // str payload release: StringHeader offset (-24), no inner destructor.
    // Mirrors emitArcReleaseVar's str path (codegen_arc.cpp:427-441).
    builder_.SetInsertPoint(strBB);
    auto *strPtrRel = builder_.CreateLoad(ptrTy_, dataPtr,
                                           siteLabel + ".any.rel.str.ptr");
    auto *strHdrRel = emitStrGetHeaderFromData(strPtrRel);
    emitArcRelease(strHdrRel, isArcAtomic(strPtrRel), llvm::FunctionCallee{}, nullptr);
    builder_.CreateBr(doneBB);

    auto releaseSlot = [&](llvm::BasicBlock *bb, CollectionKind kind,
                           const std::string &fallbackTypeName) {
        builder_.SetInsertPoint(bb);
        auto *ptr = builder_.CreateLoad(ptrTy_, dataPtr,
                                         siteLabel + ".any.rel.ptr");
        // Mirror emitAnyReleaseVar: only use the supplied source name when
        // its kind matches the runtime tag, else fall back to the generic
        // destructor for that kind. This guards against stale names from
        // reassigned `any` slots.
        std::string useTypeName = sourceTypeName;
        bool sourceMatchesKind = false;
        if (!useTypeName.empty()) {
            std::string canon = resolveTypeAlias(useTypeName);
            switch (kind) {
                case CollectionKind::List:
                    sourceMatchesKind = isListTypeName(canon);
                    break;
                case CollectionKind::Map:
                    sourceMatchesKind = isMapTypeName(canon);
                    break;
                case CollectionKind::Set:
                    sourceMatchesKind = isSetTypeName(canon);
                    break;
                case CollectionKind::Str:
                    sourceMatchesKind = false;
                    break;
            }
        }
        if (!sourceMatchesKind) {
            useTypeName = fallbackTypeName;
        }
        emitArcReleaseLoadedElement(ptr, kind, useTypeName,
                                     siteLabel + ".any." + fallbackTypeName);
        builder_.CreateBr(doneBB);
    };

    releaseSlot(listBB, CollectionKind::List, "List");
    releaseSlot(mapBB,  CollectionKind::Map,  "Map");
    releaseSlot(setBB,  CollectionKind::Set,  "Set");

    // Record release path — see emitAnyReleaseVar for the same reasoning.
    builder_.SetInsertPoint(recBB);
    auto *recPtr = builder_.CreateLoad(ptrTy_, dataPtr, siteLabel + ".any.rec.ptr");
    auto *recHdr = emitArcGetHeaderFromData(recPtr);
    auto *trampolineTy = llvm::FunctionType::get(builder_.getVoidTy(), {ptrTy_}, false);
    auto trampoline = mod_->getOrInsertFunction("__ry_arc_dtor_record_dispatch",
                                                trampolineTy);
    emitArcRelease(recHdr, isArcAtomic(recPtr), trampoline, nullptr);
    builder_.CreateBr(doneBB);

    builder_.SetInsertPoint(doneBB);
}

llvm::Value *CodeGen::emitAnyToString(llvm::Value *anyVal, bool inCollection) {
    llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr, "any.ts");
    builder_.CreateStore(anyVal, tmp);
    llvm::FunctionType *fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
    const char *fnName = inCollection ? "__ry_any_to_string_in_collection"
                                      : "__ry_any_to_string";
    llvm::FunctionCallee fn = mod_->getOrInsertFunction(fnName, fnTy);
    return builder_.CreateCall(fn, {tmp}, "any.ts.str");
}

llvm::Value *CodeGen::emitAnyBinaryOp(const std::string &op,
                                       llvm::Value *lhs, llvm::Value *rhs) {
    llvm::AllocaInst *lhsPtr = builder_.CreateAlloca(anyTy_, nullptr, "any.lhs");
    builder_.CreateStore(lhs, lhsPtr);
    llvm::AllocaInst *rhsPtr = builder_.CreateAlloca(anyTy_, nullptr, "any.rhs");
    builder_.CreateStore(rhs, rhsPtr);

    static const std::unordered_map<std::string, std::string> arithOps = {
        {"+", "__ry_any_add"}, {"-", "__ry_any_sub"},
        {"*", "__ry_any_mul"}, {"/", "__ry_any_div"},
        {"%", "__ry_any_mod"}, {"//", "__ry_any_floordiv"},
        {"**", "__ry_any_pow"},
    };
    static const std::unordered_map<std::string, std::string> cmpOps = {
        {"==", "__ry_any_eq"}, {"!=", "__ry_any_ne"},
        {"<", "__ry_any_lt"},  {"<=", "__ry_any_le"},
        {">", "__ry_any_gt"},  {">=", "__ry_any_ge"},
    };

    auto ait = arithOps.find(op);
    if (ait != arithOps.end()) {
        llvm::AllocaInst *resultPtr = builder_.CreateAlloca(anyTy_, nullptr, "any.result");
        llvm::FunctionType *fnTy = llvm::FunctionType::get(
            builder_.getVoidTy(), {ptrTy_, ptrTy_, ptrTy_}, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction(ait->second, fnTy);
        builder_.CreateCall(fn, {resultPtr, lhsPtr, rhsPtr});
        return builder_.CreateLoad(anyTy_, resultPtr, "any.binop");
    }

    auto cit = cmpOps.find(op);
    if (cit != cmpOps.end()) {
        llvm::FunctionType *fnTy = llvm::FunctionType::get(
            i64Ty_, {ptrTy_, ptrTy_}, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction(cit->second, fnTy);
        llvm::Value *result = builder_.CreateCall(fn, {lhsPtr, rhsPtr}, "any.cmp");
        llvm::Value *zero = builder_.getInt64(0);
        return builder_.CreateICmpNE(result, zero, "any.cmp.bool");
    }

    codegenError("operator '" + op + "' not supported for any type");
}

llvm::Value *CodeGen::emitAnyUnaryNeg(llvm::Value *operand) {
    llvm::AllocaInst *opPtr = builder_.CreateAlloca(anyTy_, nullptr, "any.neg.op");
    builder_.CreateStore(operand, opPtr);
    llvm::AllocaInst *resultPtr = builder_.CreateAlloca(anyTy_, nullptr, "any.neg.result");

    llvm::FunctionType *fnTy = llvm::FunctionType::get(
        builder_.getVoidTy(), {ptrTy_, ptrTy_}, false);
    llvm::FunctionCallee fn = mod_->getOrInsertFunction("__ry_any_neg", fnTy);
    builder_.CreateCall(fn, {resultPtr, opPtr});
    return builder_.CreateLoad(anyTy_, resultPtr, "any.neg");
}

} // namespace ry
