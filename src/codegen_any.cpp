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
    // Enum types (simple, ADT, `Option<T>`, `Result<V,E>`) share the
    // record-style heap-box layout (#1798): `[ ArcHeader | descriptor ptr |
    // payload ]`. Detect first so an organic-enum / Option / Result struct
    // does not fall into the record path's `findRecordInfoForType` rejection.
    // Simple enums (LLVM i64 with `enum_value_type` metadata) are boxed too —
    // tagging them as Int would lose enum identity and break equality / unwrap.
    if (std::string enumName = findEnumLikeTypeNameForBoxing(val); !enumName.empty()) {
        llvm::Type *payloadTy = val->getType();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        auto *layoutTy = enumBoxLayoutType(payloadTy);
        uint64_t boxDataSize = dl.getTypeAllocSize(layoutTy);
        auto *boxDataSizeC = llvm::ConstantInt::get(i64Ty_, boxDataSize);

        auto *desc = getOrCreateEnumDescriptor(enumName, payloadTy);
        auto *headerPtr = emitArcAlloc(boxDataSizeC);
        auto *dataPtr = emitArcGetDataPtr(headerPtr);

        auto *descPtrSlot = builder_.CreateStructGEP(layoutTy, dataPtr, 0,
                                                     "any.enum.desc.slot");
        builder_.CreateStore(desc, descPtrSlot);
        auto *payloadSlot = builder_.CreateStructGEP(layoutTy, dataPtr, 1,
                                                     "any.enum.payload.slot");
        builder_.CreateStore(val, payloadSlot);

        // Field-wise retain on existing aliases. Fresh constructions (Call /
        // Invoke) are sole owners and need no retain — mirror the record
        // ARC reassignment guard.
        if (!llvm::isa<llvm::CallInst>(val) && !llvm::isa<llvm::InvokeInst>(val)) {
            emitEnumBoxArcFieldsRetain(val, enumName, payloadTy);
        }

        llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr, "any.enum.tmp");
        auto *tagSlot = builder_.CreateStructGEP(anyTy_, tmp, 0, "any.enum.tag");
        builder_.CreateStore(llvm::ConstantInt::get(
                                 i64Ty_, static_cast<uint64_t>(RyAnyTag::Enum)),
                             tagSlot);
        auto *anyDataSlot = builder_.CreateStructGEP(anyTy_, tmp, 1, "any.enum.data");
        builder_.CreateStore(dataPtr, anyDataSlot);
        return builder_.CreateLoad(anyTy_, tmp, "any.enum.val");
    }

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
                     "List/Map/Set, record, and enum types; non-collection "
                     "pointer types (resources, function pointers, etc.) are "
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
        // Element-type-erasure (#1697) means the stringify_any path cannot
        // see whether the inner buffer uses 16-byte RyAny stride or a
        // narrower native stride. For typed-non-any collections, record
        // the source-level type name in a side-table keyed by the inner
        // header pointer so `__ry_json_stringify_any` can panic with an
        // actionable message instead of reading OOB (#1811).
        bool typedNonAnyList = meta->list_elem && meta->list_elem != anyTy_;
        bool typedNonAnyMapVal = meta->map_value && meta->map_value != anyTy_;
        // Map keys collapse to `ptrTy_` for both `Map<str, V>` and
        // `Map<List<_>, V>` / `Map<Map<_,_>, V>` / `Map<Set<_>, V>` under
        // opaque pointers, so LLVM-type equality cannot pick `str` keys out.
        // `stringify_any`'s Map arm reads `hdr->keys[i]` as `char**` and
        // calls `stringByteLen(keys[i])` on it, which is OOB for `int` keys
        // (8-byte int read as pointer) and for any non-`str` pointer key.
        // Use the stamped source-level type name as the discriminator;
        // require a non-empty name so unknown / unstamped cases keep their
        // existing behavior (no false-positive register on legitimate
        // `Map<str, any>` whose name happened to be lost).
        bool typedNonStrMapKey = meta->map_key &&
                                 !meta->map_key_type_name.empty() &&
                                 meta->map_key_type_name != "str";
        bool typedNonAnySet = meta->set_elem && meta->set_elem != anyTy_;
        if (typedNonAnyList || typedNonAnyMapVal || typedNonStrMapKey ||
            typedNonAnySet) {
            std::string typeName = buildTypeNameFromMeta(val);
            if (!typeName.empty()) {
                auto *nameGlobal = cachedGlobalString(typeName, ".any.typed_coll.name");
                llvm::FunctionType *regTy = llvm::FunctionType::get(
                    builder_.getVoidTy(), {ptrTy_, ptrTy_}, false);
                llvm::FunctionCallee regFn = mod_->getOrInsertFunction(
                    "__ry_any_register_typed_coll", regTy);
                builder_.CreateCall(regFn, {val, nameGlobal});
            }
        }
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
                                     const std::string &rawTargetTypeName) {
    // Substitute generic type-parameter names at the helper's entry so every
    // downstream dispatch (collection-arm via `ry::util::isListTypeName` /
    // `ry::util::isMapTypeName` / `ry::util::isSetTypeName`, enum-descriptor cache key in
    // `unwrapEnumFromAny`, error message, record `findRecordTypeName`) sees
    // the concrete type rather than the raw "T". No-op when `type_param_scope_`
    // is empty, so non-generic call sites stay byte-identical. Mirrors the
    // entry-point substitution in `resolveType` (codegen_type.cpp:18).
    const std::string targetTypeName = substituteTypeParamsInName(rawTargetTypeName);

    // Collection element-type guard (#1698): `any` slots are 16 bytes (RyAny);
    // collection unwrap currently does not coerce per-element, so a
    // `List<int>` unwrap of a `List<any>` payload strides 8-byte ints over a
    // 16-byte buffer and silently produces garbage (outcome b in the original
    // advisor analysis). The only context where this PR newly exposes the
    // hazard is generic monomorphization of `load[T]` (and any user-defined
    // `fn f<T>(a: any) -> T` analog): the JSON parser materializes
    // `List<any>` but the substituted `T` claims `List<int>`. Direct unwraps
    // like `let ys: List<int> = a` where the source was actually a `List<int>`
    // remain legitimate (covered by the any.test.ry roundtrip tests), so we
    // only reject when substitution rewrote a type parameter — i.e. the
    // user-written annotation referred to a generic `T` rather than the
    // concrete shape.
    const bool fromGenericSubstitution =
        targetTypeName != rawTargetTypeName;
    if (fromGenericSubstitution && targetTy == ptrTy_ && !targetTypeName.empty()) {
        std::string resolved = resolveTypeAlias(targetTypeName);
        if (ry::util::isListTypeName(resolved)) {
            std::string inner = ry::util::trimTypeNameSpaces(
                resolved.substr(5, resolved.size() - 6));
            if (inner != "any") {
                codegenError(
                    "unwrapping 'any' to '" + targetTypeName +
                    "' is not supported in this release: element type must "
                    "be 'any' (use 'List<any>' and unwrap elements "
                    "individually)");
            }
        } else if (ry::util::isSetTypeName(resolved)) {
            std::string inner = ry::util::trimTypeNameSpaces(
                resolved.substr(4, resolved.size() - 5));
            if (inner != "any") {
                codegenError(
                    "unwrapping 'any' to '" + targetTypeName +
                    "' is not supported in this release: element type must "
                    "be 'any' (use 'Set<any>' and unwrap elements "
                    "individually)");
            }
        } else if (ry::util::isMapTypeName(resolved)) {
            std::string innerArgs = resolved.substr(4, resolved.size() - 5);
            auto parts = splitTypeArgs(innerArgs);
            if (parts.size() == 2) {
                std::string k = ry::util::trimTypeNameSpaces(parts[0]);
                std::string v = ry::util::trimTypeNameSpaces(parts[1]);
                if (k != "str" || v != "any") {
                    codegenError(
                        "unwrapping 'any' to '" + targetTypeName +
                        "' is not supported in this release: must be "
                        "'Map<str, any>' (unwrap values individually)");
                }
            }
        }
    }

    llvm::Value *tag = builder_.CreateExtractValue(anyVal, 0, "any.tag.val");
    llvm::Function *fn = builder_.GetInsertBlock()->getParent();

    // Enum (organic / Option / Result) unwrap. Detect BEFORE the record
    // StructType branch so an enum-struct target does not error out via
    // `findRecordInfoForType` rejection. Symmetric to wrapInAny.
    if (auto *st = llvm::dyn_cast<llvm::StructType>(targetTy)) {
        bool isEnumStruct = !findAdtEnumName(st).empty() ||
                            isOptionType(st) || isResultType(st);
        if (isEnumStruct) {
            return unwrapEnumFromAny(anyVal, st, targetTypeName);
        }
    }
    if (targetTy == i64Ty_ && !targetTypeName.empty() &&
        isSimpleEnumTypeName(targetTypeName)) {
        return unwrapEnumFromAny(anyVal, targetTy, targetTypeName);
    }

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
        if (ry::util::isListTypeName(resolved)) {
            expectedTag = static_cast<int64_t>(RyAnyTag::List);
            isCollectionUnwrap = true;
        } else if (ry::util::isMapTypeName(resolved)) {
            expectedTag = static_cast<int64_t>(RyAnyTag::Map);
            isCollectionUnwrap = true;
        } else if (ry::util::isSetTypeName(resolved)) {
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

llvm::Value *CodeGen::tryUnwrapFromAny(llvm::Value *anyVal, llvm::Type *targetTy,
                                        const std::string &rawTargetTypeName) {
    // Substitute generic type-parameter names so downstream dispatches see the
    // concrete type. Mirrors `unwrapFromAny`'s entry-point substitution.
    const std::string targetTypeName = substituteTypeParamsInName(rawTargetTypeName);
    llvm::StructType *resTy = getResultType(targetTy, errorTy_);

    // Construct an inline `Error{message, code}` value (code = 0 sentinel).
    auto buildInlineError = [&](const std::string &msg) -> llvm::Value * {
        llvm::Value *errStr = cachedGlobalString(msg);
        llvm::Value *errVal = llvm::UndefValue::get(errorTy_);
        errVal = builder_.CreateInsertValue(errVal, errStr, {0});
        errVal = builder_.CreateInsertValue(
            errVal, llvm::ConstantInt::get(i64Ty_, 0), {1});
        return errVal;
    };
    auto emitUnsupportedErr = [&](const std::string &kindLabel) -> llvm::Value * {
        std::string label = targetTypeName.empty() ? "?" : targetTypeName;
        std::string msg = "load[" + label + "]: " + kindLabel +
                          " target not yet supported";
        return buildErrValue(buildInlineError(msg), resTy);
    };

    // Records dispatch to the dedicated Map<str,any>→Record reconstructor.
    // Result / enum (struct & simple) / Option are deferred to follow-ups.
    if (auto *st = llvm::dyn_cast<llvm::StructType>(targetTy)) {
        bool isEnumOrResult = !findAdtEnumName(st).empty() || isResultType(st);
        if (isEnumOrResult) {
            return emitUnsupportedErr("Result/enum");
        }
        if (isOptionType(st)) {
            auto it = reverse_option_types_.find(st);
            if (it == reverse_option_types_.end())
                return emitUnsupportedErr("Option");
            llvm::Type *innerTy = it->second;
            // Recover the inner Ry type name from `targetTypeName` when it is
            // an `Option<...>` so the recursive call can route correctly
            // (record / typed list / typed map / nested Option).
            std::string innerName;
            std::string resolved = resolveTypeAlias(targetTypeName);
            constexpr const char *kPrefix = "Option<";
            constexpr size_t kPrefixLen = 7;
            if (resolved.size() > kPrefixLen + 1 &&
                resolved.compare(0, kPrefixLen, kPrefix) == 0 &&
                resolved.back() == '>') {
                innerName = ry::util::trimTypeNameSpaces(
                    resolved.substr(kPrefixLen,
                                    resolved.size() - kPrefixLen - 1));
            } else if (resolved.size() > 1 && resolved.back() == '?') {
                // `T?` shorthand for `Option<T>`.
                innerName = ry::util::trimTypeNameSpaces(
                    resolved.substr(0, resolved.size() - 1));
            }
            return tryUnwrapOptionFromAny(anyVal, st, innerTy, innerName,
                                            targetTypeName, resTy);
        }
        const RecordInfo *info = findRecordInfoForType(st);
        if (!info) {
            return emitUnsupportedErr("non-record struct");
        }
        return tryUnwrapRecordFromAny(anyVal, st, *info, targetTypeName, resTy);
    }
    if (targetTy == i64Ty_ && !targetTypeName.empty() &&
        isSimpleEnumTypeName(targetTypeName)) {
        return emitUnsupportedErr("simple enum");
    }

    // Typed `List<T>` / `Map<str, V>` dispatch (#1852): a fresh dest header
    // is allocated and each element is recursively unwrapped via
    // `tryUnwrapFromAny` to overcome the 16B-vs-native stride mismatch of
    // #1698. `Set<T>` and `Map<non-str, ...>` are still rejected — the JSON
    // parser does not produce sets, and Map<non-str, _> requires hashing the
    // unwrapped key through the right `__ry_ht_rehash_*` variant which is out
    // of scope for #1852.
    if (targetTy == ptrTy_ && !targetTypeName.empty()) {
        std::string resolved = resolveTypeAlias(targetTypeName);
        if (ry::util::isListTypeName(resolved)) {
            std::string inner = ry::util::trimTypeNameSpaces(
                resolved.substr(5, resolved.size() - 6));
            if (inner == "any") {
                // Falls through to the standard pointer-tag dispatch below
                // (allocates a fresh header sharing the original data ptr,
                // see `isCollectionUnwrap` branch).
            } else {
                llvm::Type *elemTy = resolveType(inner);
                if (!elemTy)
                    return emitUnsupportedErr("typed List<" + inner + ">");
                return tryUnwrapListFromAny(anyVal, elemTy, inner,
                                              targetTypeName, resTy);
            }
        } else if (ry::util::isSetTypeName(resolved)) {
            std::string inner = ry::util::trimTypeNameSpaces(
                resolved.substr(4, resolved.size() - 5));
            if (inner != "any") {
                return emitUnsupportedErr("typed Set<" + inner + ">");
            }
        } else if (ry::util::isMapTypeName(resolved)) {
            std::string innerArgs = resolved.substr(4, resolved.size() - 5);
            auto parts = splitTypeArgs(innerArgs);
            if (parts.size() == 2) {
                std::string k = ry::util::trimTypeNameSpaces(parts[0]);
                std::string v = ry::util::trimTypeNameSpaces(parts[1]);
                if (k != "str") {
                    return emitUnsupportedErr(
                        "typed Map<" + k + ", " + v + ">");
                }
                if (v == "any") {
                    // Falls through to standard pointer-tag dispatch below.
                } else {
                    llvm::Type *valTy = resolveType(v);
                    if (!valTy)
                        return emitUnsupportedErr(
                            "typed Map<str, " + v + ">");
                    return tryUnwrapMapFromAny(anyVal, valTy, v,
                                                 targetTypeName, resTy);
                }
            }
        }
    }

    llvm::Value *tag = builder_.CreateExtractValue(anyVal, 0, "tryany.tag");

    // Float target: accept both `Float` and `Int` tags (Int auto-promote).
    // Both loads from the same dataPtr alloca are safe — we choose the
    // representation via `select(isFloat, fVal, promoted)`.
    if (targetTy == f64Ty_) {
        llvm::AllocaInst *tmp =
            builder_.CreateAlloca(anyTy_, nullptr, "tryany.fp.tmp");
        builder_.CreateStore(anyVal, tmp);
        auto *dataPtr =
            builder_.CreateStructGEP(anyTy_, tmp, 1, "tryany.fp.data");
        llvm::Value *fVal =
            builder_.CreateLoad(f64Ty_, dataPtr, "tryany.fp.fval");
        llvm::Value *iVal =
            builder_.CreateLoad(i64Ty_, dataPtr, "tryany.fp.ival");
        llvm::Value *promoted =
            builder_.CreateSIToFP(iVal, f64Ty_, "tryany.fp.i2f");
        llvm::Value *isFloat = builder_.CreateICmpEQ(
            tag,
            llvm::ConstantInt::get(
                i64Ty_, static_cast<int64_t>(RyAnyTag::Float)),
            "tryany.fp.is_float");
        llvm::Value *isInt = builder_.CreateICmpEQ(
            tag,
            llvm::ConstantInt::get(
                i64Ty_, static_cast<int64_t>(RyAnyTag::Int)),
            "tryany.fp.is_int");
        llvm::Value *isAccept =
            builder_.CreateOr(isFloat, isInt, "tryany.fp.is_accept");
        llvm::Value *isErr =
            builder_.CreateNot(isAccept, "tryany.fp.is_err");
        std::string typeForMsg =
            targetTypeName.empty() ? "float" : targetTypeName;
        std::string msg =
            "load[" + typeForMsg + "]: expected float or int";
        return emitResultBranch(
            isErr, resTy,
            [&]() -> llvm::Value * {
                llvm::Value *chosen = builder_.CreateSelect(
                    isFloat, fVal, promoted, "tryany.fp.val");
                return buildOkValue(chosen, resTy);
            },
            [&]() -> llvm::Value * {
                return buildErrValue(buildInlineError(msg), resTy);
            });
    }

    // Standard 2-way path: tag comparison → Ok(extracted) / Err(msg).
    // Handles int (i64), bool (i1), str (ptr+Str tag), and `List<any>` /
    // `Map<str,any>` / `Set<any>` (ptr+collection tag, requires ARC retain
    // on the unwrapped alias).
    int64_t expectedTag = getAnyTypeTag(targetTy);
    bool isCollectionUnwrap = false;
    if (targetTy == ptrTy_ && !targetTypeName.empty()) {
        std::string resolved = resolveTypeAlias(targetTypeName);
        if (ry::util::isListTypeName(resolved)) {
            expectedTag = static_cast<int64_t>(RyAnyTag::List);
            isCollectionUnwrap = true;
        } else if (ry::util::isMapTypeName(resolved)) {
            expectedTag = static_cast<int64_t>(RyAnyTag::Map);
            isCollectionUnwrap = true;
        } else if (ry::util::isSetTypeName(resolved)) {
            expectedTag = static_cast<int64_t>(RyAnyTag::Set);
            isCollectionUnwrap = true;
        }
    }
    bool isStrUnwrap = targetTy == ptrTy_ && !isCollectionUnwrap &&
                       expectedTag == static_cast<int64_t>(RyAnyTag::Str);

    llvm::Value *cmp = builder_.CreateICmpEQ(
        tag,
        llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(expectedTag)),
        "tryany.tag.eq");
    llvm::Value *isErr = builder_.CreateNot(cmp, "tryany.is_err");

    std::string typeForMsg;
    if (!targetTypeName.empty()) {
        typeForMsg = targetTypeName;
    } else {
        typeForMsg = (targetTy == i64Ty_)   ? "int"
                     : (targetTy == i1Ty_)  ? "bool"
                     : (targetTy == ptrTy_) ? "str"
                                            : "unknown";
    }
    std::string msg = "load[" + typeForMsg + "]: expected " + typeForMsg;

    return emitResultBranch(
        isErr, resTy,
        [&]() -> llvm::Value * {
            llvm::AllocaInst *tmp =
                builder_.CreateAlloca(anyTy_, nullptr, "tryany.tmp");
            builder_.CreateStore(anyVal, tmp);
            auto *dataPtr =
                builder_.CreateStructGEP(anyTy_, tmp, 1, "tryany.data");
            llvm::Value *unwrapped =
                builder_.CreateLoad(targetTy, dataPtr, "tryany.val");
            if (isCollectionUnwrap) {
                auto *hdr = emitArcGetHeaderFromData(unwrapped);
                emitArcRetain(hdr);
            } else if (isStrUnwrap) {
                auto *hdr = emitStrGetHeaderFromData(unwrapped);
                emitArcRetain(hdr);
            }
            return buildOkValue(unwrapped, resTy);
        },
        [&]() -> llvm::Value * {
            return buildErrValue(buildInlineError(msg), resTy);
        });
}

llvm::Value *CodeGen::tryUnwrapRecordFromAny(llvm::Value *anyVal,
                                               llvm::StructType *recordStructTy,
                                               const RecordInfo &info,
                                               const std::string &targetTypeName,
                                               llvm::StructType *resTy) {
    llvm::Function *fn = builder_.GetInsertBlock()->getParent();
    const std::string typeLabel = targetTypeName.empty() ? "?" : targetTypeName;
    const std::string prefix = "load[" + typeLabel + "]: ";

    // Field-kind classification drives ARC release on every err exit. Records
    // are built field-by-field via InsertValue; if iter k fails, every
    // already-extracted ARC-bearing value in iter 0..k-1 must be released or
    // ASan flags a leak. Resources, weak refs, closures, fn-pointers, and
    // primitives are conservatively classified `None` (no release needed).
    enum class FieldKind { None, Str, Collection, RecordArc };
    auto classifyField = [&](llvm::Type *fieldLlvmTy,
                             const std::string &fieldTypeName) -> FieldKind {
        if (auto *fst = llvm::dyn_cast<llvm::StructType>(fieldLlvmTy)) {
            if (findRecordInfoForType(fst) && recordHasArcFields(fst))
                return FieldKind::RecordArc;
            return FieldKind::None;
        }
        if (fieldLlvmTy != ptrTy_) return FieldKind::None;
        std::string resolved = resolveTypeAlias(fieldTypeName);
        if (ry::util::isListTypeName(resolved) || ry::util::isMapTypeName(resolved) ||
            ry::util::isSetTypeName(resolved))
            return FieldKind::Collection;
        if (resolved == "str") return FieldKind::Str;
        return FieldKind::None;
    };

    auto buildInlineError = [&](const std::string &msg) -> llvm::Value * {
        llvm::Value *errStr = cachedGlobalString(msg);
        llvm::Value *errVal = llvm::UndefValue::get(errorTy_);
        errVal = builder_.CreateInsertValue(errVal, errStr, {0});
        errVal = builder_.CreateInsertValue(
            errVal, llvm::ConstantInt::get(i64Ty_, 0), {1});
        return errVal;
    };

    struct CollectedField {
        llvm::Value *val;
        FieldKind kind;
        std::string resolvedTypeName;  // for Collection: routes to right destructor
    };
    std::vector<CollectedField> arcCollected;

    auto releaseAllCollected = [&]() {
        for (const auto &p : arcCollected) {
            switch (p.kind) {
                case FieldKind::None: break;
                case FieldKind::Str:
                    emitArcReleaseLoadedElement(p.val, CollectionKind::Str,
                                                "str", "tryrec.rel.str");
                    break;
                case FieldKind::Collection: {
                    CollectionKind ck = CollectionKind::List;
                    if (ry::util::isMapTypeName(p.resolvedTypeName))
                        ck = CollectionKind::Map;
                    else if (ry::util::isSetTypeName(p.resolvedTypeName))
                        ck = CollectionKind::Set;
                    emitArcReleaseLoadedElement(p.val, ck, p.resolvedTypeName,
                                                "tryrec.rel.col");
                    break;
                }
                case FieldKind::RecordArc:
                    emitRecordArcFieldsRelease(
                        p.val, llvm::cast<llvm::StructType>(p.val->getType()));
                    break;
            }
        }
    };

    // Tag check (Map=6): JSON parser produces Map for objects; reject all else.
    llvm::Value *tag = builder_.CreateExtractValue(anyVal, 0, "tryrec.tag");
    llvm::Value *isMap = builder_.CreateICmpEQ(
        tag,
        llvm::ConstantInt::get(i64Ty_, static_cast<int64_t>(RyAnyTag::Map)),
        "tryrec.is_map");

    auto *tagOkBB  = llvm::BasicBlock::Create(*ctx_, "tryrec.tag_ok",  fn);
    auto *tagErrBB = llvm::BasicBlock::Create(*ctx_, "tryrec.tag_err", fn);
    auto *doneBB   = llvm::BasicBlock::Create(*ctx_, "tryrec.done",    fn);

    builder_.CreateCondBr(isMap, tagOkBB, tagErrBB);

    // Tag-mismatch arm: build Err and branch to done. No collected ARC yet.
    builder_.SetInsertPoint(tagErrBB);
    llvm::Value *tagErrVal = buildErrValue(
        buildInlineError(prefix + "expected JSON object"), resTy);
    llvm::BasicBlock *tagErrEndBB = builder_.GetInsertBlock();
    builder_.CreateBr(doneBB);

    // Tag-ok: extract Map pointer from any.data[8].
    builder_.SetInsertPoint(tagOkBB);
    llvm::AllocaInst *anyTmp =
        builder_.CreateAlloca(anyTy_, nullptr, "tryrec.any.tmp");
    builder_.CreateStore(anyVal, anyTmp);
    llvm::Value *anyDataSlot =
        builder_.CreateStructGEP(anyTy_, anyTmp, 1, "tryrec.any.data.slot");
    llvm::Value *mapPtr =
        builder_.CreateLoad(ptrTy_, anyDataSlot, "tryrec.map.ptr");

    // Load vals* from MapHeader (index 3 — keys at 2, vals at 3).
    llvm::Value *valsSlot = builder_.CreateStructGEP(
        mapHeaderTy_, mapPtr, 3, "tryrec.vals.slot");
    llvm::Value *valsPtr =
        builder_.CreateLoad(ptrTy_, valsSlot, "tryrec.vals.ptr");

    // Accumulate err-exit incomings for the merge PHI.
    std::vector<std::pair<llvm::Value *, llvm::BasicBlock *>> errIncomings;
    errIncomings.emplace_back(tagErrVal, tagErrEndBB);

    llvm::Value *rec = llvm::UndefValue::get(recordStructTy);

    for (unsigned i = 0; i < info.fields.size(); ++i) {
        const FieldDef &f = info.fields[i];
        llvm::Type *fieldLlvmTy = recordStructTy->getElementType(i);
        std::string fieldTypeName =
            f.type ? f.type->toString() : std::string();
        std::string resolvedFieldTy = resolveTypeAlias(fieldTypeName);
        FieldKind fk = classifyField(fieldLlvmTy, fieldTypeName);

        // Step 1: key lookup via __ry_ht_find_str.
        llvm::Value *keyStr = cachedGlobalString(f.name);
        llvm::Value *slot = emitHashTableLookup(
            mapPtr, mapHeaderTy_, kMapLayout, keyStr, ptrTy_);
        llvm::Value *isMiss = builder_.CreateICmpEQ(
            slot, llvm::ConstantInt::getSigned(i64Ty_, -1),
            ("tryrec." + f.name + ".miss").c_str());

        auto *missBB  = llvm::BasicBlock::Create(
            *ctx_, "tryrec.fld_" + f.name + ".miss",  fn);
        auto *foundBB = llvm::BasicBlock::Create(
            *ctx_, "tryrec.fld_" + f.name + ".found", fn);
        builder_.CreateCondBr(isMiss, missBB, foundBB);

        // Missing-field arm.
        builder_.SetInsertPoint(missBB);
        releaseAllCollected();
        llvm::Value *missErr = buildErrValue(
            buildInlineError(prefix + "field '" + f.name + "' missing"),
            resTy);
        llvm::BasicBlock *missEndBB = builder_.GetInsertBlock();
        builder_.CreateBr(doneBB);
        errIncomings.emplace_back(missErr, missEndBB);

        // Found arm: load slot value (vals is RyAny[], stride = anyTy_).
        builder_.SetInsertPoint(foundBB);
        llvm::Value *elemPtr = builder_.CreateGEP(
            anyTy_, valsPtr, slot, "tryrec.fld_" + f.name + ".elem.ptr");
        llvm::Value *elemAny = builder_.CreateLoad(
            anyTy_, elemPtr, "tryrec.fld_" + f.name + ".elem");

        // Recursive unwrap.
        llvm::Value *subResult =
            tryUnwrapFromAny(elemAny, fieldLlvmTy, fieldTypeName);
        llvm::Value *subDisc = builder_.CreateExtractValue(
            subResult, {0}, "tryrec.fld_" + f.name + ".sub.disc");

        auto *subOkBB  = llvm::BasicBlock::Create(
            *ctx_, "tryrec.fld_" + f.name + ".sub_ok",  fn);
        auto *subErrBB = llvm::BasicBlock::Create(
            *ctx_, "tryrec.fld_" + f.name + ".sub_err", fn);
        builder_.CreateCondBr(subDisc, subOkBB, subErrBB);

        // SubErr arm: release collected, build prefixed Err via runtime concat.
        builder_.SetInsertPoint(subErrBB);
        releaseAllCollected();
        llvm::Value *innerErr = builder_.CreateExtractValue(
            subResult, {2}, "tryrec.fld_" + f.name + ".sub.err");
        llvm::Value *innerMsg = builder_.CreateExtractValue(
            innerErr, {0}, "tryrec.fld_" + f.name + ".sub.err.msg");

        std::string fieldPrefix = prefix + "field '" + f.name + "': ";
        llvm::Value *prefixStr = cachedGlobalString(fieldPrefix);
        llvm::Value *prefixLen = emitStringByteLen(prefixStr);
        llvm::Value *innerMsgLen = emitStringByteLen(innerMsg);
        llvm::Value *totalLen = builder_.CreateAdd(
            prefixLen, innerMsgLen, "tryrec.fld_" + f.name + ".msg.total");
        auto makeUninitTy =
            llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        auto makeUninitFn = mod_->getOrInsertFunction(
            "__ry_string_make_uninit", makeUninitTy);
        llvm::Value *newBuf = builder_.CreateCall(
            makeUninitFn, {totalLen}, "tryrec.fld_" + f.name + ".msg.buf");
        builder_.CreateCall(getStdlibMemcpy(),
                            {newBuf, prefixStr, prefixLen});
        llvm::Value *dst = builder_.CreateGEP(
            i8Ty_, newBuf, prefixLen,
            "tryrec.fld_" + f.name + ".msg.dst");
        builder_.CreateCall(getStdlibMemcpy(), {dst, innerMsg, innerMsgLen});
        // Release the inner message. cachedGlobalString-backed messages are
        // ARC_IMMORTAL so emitArcRelease is a safe no-op; heap-allocated
        // nested messages (e.g. Outer → Middle → Inner missing field) get
        // their refcount decremented and freed via this path.
        emitArcRelease(emitStrGetHeaderFromData(innerMsg));

        llvm::Value *prefixedErr = llvm::UndefValue::get(errorTy_);
        prefixedErr = builder_.CreateInsertValue(prefixedErr, newBuf, {0});
        prefixedErr = builder_.CreateInsertValue(
            prefixedErr, llvm::ConstantInt::get(i64Ty_, 0), {1});
        llvm::Value *subErrVal = buildErrValue(prefixedErr, resTy);
        llvm::BasicBlock *subErrEndBB = builder_.GetInsertBlock();
        builder_.CreateBr(doneBB);
        errIncomings.emplace_back(subErrVal, subErrEndBB);

        // SubOk arm: extract Ok value, InsertValue into rec, track for ARC.
        builder_.SetInsertPoint(subOkBB);
        llvm::Value *subOkVal = builder_.CreateExtractValue(
            subResult, {1}, "tryrec.fld_" + f.name + ".sub.ok");
        rec = builder_.CreateInsertValue(
            rec, subOkVal, {i},
            "tryrec.rec.f" + std::to_string(i));
        if (fk != FieldKind::None) {
            arcCollected.push_back({subOkVal, fk, resolvedFieldTy});
        }
    }

    // All fields succeeded. buildOkValue does not double-retain for record
    // StructType inputs (its `tryRetainArcSource` runs only for ptrTy_), and
    // we built rec from scratch so every field's refcount is owned by us
    // exactly once already.
    llvm::Value *okVal = buildOkValue(rec, resTy);
    llvm::BasicBlock *okEndBB = builder_.GetInsertBlock();
    builder_.CreateBr(doneBB);

    // Merge.
    builder_.SetInsertPoint(doneBB);
    llvm::PHINode *phi = builder_.CreatePHI(
        resTy, static_cast<unsigned>(errIncomings.size()) + 1,
        "tryrec.result");
    for (auto &p : errIncomings)
        phi->addIncoming(p.first, p.second);
    phi->addIncoming(okVal, okEndBB);
    return phi;
}

// Helper shared by tryUnwrapListFromAny / tryUnwrapMapFromAny: classify an
// element/value type for ARC release on partial failure. Mirrors
// `tryUnwrapRecordFromAny`'s classifyField but at the element granularity.
namespace {
enum class TryUnwrapElemKind { None, Str, Collection, RecordArc };
}

static TryUnwrapElemKind classifyTryUnwrapElem(
    ry::CodeGen &cg, llvm::Type *elemLlvmTy,
    const std::string &elemTypeName) {
    if (auto *fst = llvm::dyn_cast<llvm::StructType>(elemLlvmTy)) {
        if (cg.findRecordInfoForType(fst) && cg.recordHasArcFields(fst))
            return TryUnwrapElemKind::RecordArc;
        return TryUnwrapElemKind::None;
    }
    if (elemLlvmTy != cg.ptrTy_) return TryUnwrapElemKind::None;
    std::string resolved = cg.resolveTypeAlias(elemTypeName);
    if (ry::util::isListTypeName(resolved) || ry::util::isMapTypeName(resolved) ||
        ry::util::isSetTypeName(resolved))
        return TryUnwrapElemKind::Collection;
    if (resolved == "str") return TryUnwrapElemKind::Str;
    return TryUnwrapElemKind::None;
}

llvm::Value *CodeGen::tryUnwrapListFromAny(llvm::Value *anyVal,
                                             llvm::Type *elemTy,
                                             const std::string &elemTypeName,
                                             const std::string &targetTypeName,
                                             llvm::StructType *resTy) {
    llvm::Function *fn = builder_.GetInsertBlock()->getParent();
    const std::string typeLabel =
        targetTypeName.empty() ? ("List<" + elemTypeName + ">") : targetTypeName;
    const std::string prefix = "load[" + typeLabel + "]: ";

    auto buildInlineError = [&](const std::string &msg) -> llvm::Value * {
        llvm::Value *errStr = cachedGlobalString(msg);
        llvm::Value *errVal = llvm::UndefValue::get(errorTy_);
        errVal = builder_.CreateInsertValue(errVal, errStr, {0});
        errVal = builder_.CreateInsertValue(
            errVal, llvm::ConstantInt::get(i64Ty_, 0), {1});
        return errVal;
    };

    // Tag check (List=5).
    llvm::Value *tag = builder_.CreateExtractValue(anyVal, 0, "trylst.tag");
    llvm::Value *isList = builder_.CreateICmpEQ(
        tag,
        llvm::ConstantInt::get(i64Ty_, static_cast<int64_t>(RyAnyTag::List)),
        "trylst.is_list");

    auto *tagOkBB  = llvm::BasicBlock::Create(*ctx_, "trylst.tag_ok",  fn);
    auto *tagErrBB = llvm::BasicBlock::Create(*ctx_, "trylst.tag_err", fn);
    auto *doneBB   = llvm::BasicBlock::Create(*ctx_, "trylst.done",    fn);

    builder_.CreateCondBr(isList, tagOkBB, tagErrBB);

    builder_.SetInsertPoint(tagErrBB);
    llvm::Value *tagErrVal = buildErrValue(
        buildInlineError(prefix + "expected JSON array"), resTy);
    llvm::BasicBlock *tagErrEndBB = builder_.GetInsertBlock();
    builder_.CreateBr(doneBB);

    // Tag-ok: load source list header from any.data[8].
    builder_.SetInsertPoint(tagOkBB);
    llvm::AllocaInst *anyTmp =
        builder_.CreateAlloca(anyTy_, nullptr, "trylst.any.tmp");
    builder_.CreateStore(anyVal, anyTmp);
    llvm::Value *anyDataSlot =
        builder_.CreateStructGEP(anyTy_, anyTmp, 1, "trylst.any.data.slot");
    llvm::Value *srcHdrPtr =
        builder_.CreateLoad(ptrTy_, anyDataSlot, "trylst.src.hdr");

    // Source: len at field 0, data ptr at field 2 (listHeaderTy_).
    llvm::Value *srcLenSlot =
        builder_.CreateStructGEP(listHeaderTy_, srcHdrPtr, 0, "trylst.src.len.slot");
    llvm::Value *srcLen =
        builder_.CreateLoad(i64Ty_, srcLenSlot, "trylst.src.len");
    llvm::Value *srcDataSlot =
        builder_.CreateStructGEP(listHeaderTy_, srcHdrPtr, 2, "trylst.src.data.slot");
    llvm::Value *srcDataPtr =
        builder_.CreateLoad(ptrTy_, srcDataSlot, "trylst.src.data");

    // Allocate fresh dest list header + data buffer.
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t elemSize = dl.getTypeAllocSize(elemTy);
    llvm::Value *destHdr = emitArcAllocCollectionHeader(listHeaderTy_);
    auto mallocFn = getStdlibMalloc();
    llvm::Value *dataBytes = builder_.CreateMul(
        srcLen, llvm::ConstantInt::get(i64Ty_, elemSize), "trylst.dst.bytes");
    llvm::Value *destDataPtr =
        builder_.CreateCall(mallocFn, {dataBytes}, "trylst.dst.data");

    // Element kind for ARC release on partial failure / final stamp.
    TryUnwrapElemKind elemKind =
        classifyTryUnwrapElem(*this, elemTy, elemTypeName);
    std::string resolvedElemTy = resolveTypeAlias(elemTypeName);

    // Loop: i = 0; while (i < srcLen) ...
    llvm::AllocaInst *iSlot =
        builder_.CreateAlloca(i64Ty_, nullptr, "trylst.i.slot");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iSlot);
    // count_done tracks how many dest slots are successfully populated, so a
    // partial-failure cleanup can release exactly those (and no more).
    llvm::AllocaInst *countSlot =
        builder_.CreateAlloca(i64Ty_, nullptr, "trylst.count.slot");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), countSlot);

    auto *headerBB = llvm::BasicBlock::Create(*ctx_, "trylst.loop.header", fn);
    auto *bodyBB   = llvm::BasicBlock::Create(*ctx_, "trylst.loop.body",   fn);
    auto *errBB    = llvm::BasicBlock::Create(*ctx_, "trylst.loop.err",    fn);
    auto *exitBB   = llvm::BasicBlock::Create(*ctx_, "trylst.loop.exit",   fn);

    builder_.CreateBr(headerBB);
    builder_.SetInsertPoint(headerBB);
    llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iSlot, "trylst.i");
    llvm::Value *iLt = builder_.CreateICmpSLT(iCur, srcLen, "trylst.i.lt");
    builder_.CreateCondBr(iLt, bodyBB, exitBB);

    builder_.SetInsertPoint(bodyBB);
    // GEP source slot (any stride = 16B) and load any value.
    llvm::Value *srcElemPtr = builder_.CreateGEP(
        anyTy_, srcDataPtr, iCur, "trylst.src.elem.ptr");
    llvm::Value *srcAny =
        builder_.CreateLoad(anyTy_, srcElemPtr, "trylst.src.elem");

    // Recursive unwrap.
    llvm::Value *subResult =
        tryUnwrapFromAny(srcAny, elemTy, elemTypeName);
    llvm::Value *subDisc = builder_.CreateExtractValue(
        subResult, {0}, "trylst.sub.disc");
    auto *subOkBB = llvm::BasicBlock::Create(*ctx_, "trylst.sub.ok", fn);

    // On sub-Err, propagate to errBB carrying the inner Error value so we can
    // prefix its message before returning.
    auto *prevBlock = builder_.GetInsertBlock();
    builder_.CreateCondBr(subDisc, subOkBB, errBB);

    builder_.SetInsertPoint(subOkBB);
    llvm::Value *subOkVal = builder_.CreateExtractValue(
        subResult, {1}, "trylst.sub.ok.val");
    // Store into dest buffer at native stride.
    llvm::Value *destElemPtr = builder_.CreateGEP(
        elemTy, destDataPtr, iCur, "trylst.dst.elem.ptr");
    builder_.CreateStore(subOkVal, destElemPtr);
    // Bump count_done before incrementing i so cleanup walks exactly the
    // populated prefix.
    llvm::Value *newCount = builder_.CreateAdd(
        iCur, llvm::ConstantInt::get(i64Ty_, 1), "trylst.count.next");
    builder_.CreateStore(newCount, countSlot);
    llvm::Value *iNext = builder_.CreateAdd(
        iCur, llvm::ConstantInt::get(i64Ty_, 1), "trylst.i.next");
    builder_.CreateStore(iNext, iSlot);
    builder_.CreateBr(headerBB);

    // Err arm: release all populated elements [0, count_done), free dest
    // buffers, build prefixed Err.
    builder_.SetInsertPoint(errBB);
    llvm::Value *innerErr = builder_.CreateExtractValue(
        subResult, {2}, "trylst.sub.err.val");
    llvm::Value *innerMsg = builder_.CreateExtractValue(
        innerErr, {0}, "trylst.sub.err.msg");
    (void)prevBlock;

    // Release loop: walk dest buffer [0, count_done) and release ARC fields.
    if (elemKind != TryUnwrapElemKind::None) {
        llvm::Value *populated = builder_.CreateLoad(i64Ty_, countSlot, "trylst.count");
        llvm::AllocaInst *jSlot =
            builder_.CreateAlloca(i64Ty_, nullptr, "trylst.rel.j.slot");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), jSlot);
        auto *relHdrBB  = llvm::BasicBlock::Create(*ctx_, "trylst.rel.header", fn);
        auto *relBodyBB = llvm::BasicBlock::Create(*ctx_, "trylst.rel.body",   fn);
        auto *relExitBB = llvm::BasicBlock::Create(*ctx_, "trylst.rel.exit",   fn);
        builder_.CreateBr(relHdrBB);
        builder_.SetInsertPoint(relHdrBB);
        llvm::Value *jCur = builder_.CreateLoad(i64Ty_, jSlot, "trylst.rel.j");
        llvm::Value *jLt = builder_.CreateICmpSLT(jCur, populated, "trylst.rel.j.lt");
        builder_.CreateCondBr(jLt, relBodyBB, relExitBB);
        builder_.SetInsertPoint(relBodyBB);
        llvm::Value *relSlot = builder_.CreateGEP(
            elemTy, destDataPtr, jCur, "trylst.rel.slot");
        llvm::Value *relVal = builder_.CreateLoad(elemTy, relSlot, "trylst.rel.val");
        switch (elemKind) {
            case TryUnwrapElemKind::None: break;
            case TryUnwrapElemKind::Str:
                emitArcReleaseLoadedElement(relVal, CollectionKind::Str,
                                            "str", "trylst.rel.str");
                break;
            case TryUnwrapElemKind::Collection: {
                CollectionKind ck = CollectionKind::List;
                if (ry::util::isMapTypeName(resolvedElemTy)) ck = CollectionKind::Map;
                else if (ry::util::isSetTypeName(resolvedElemTy)) ck = CollectionKind::Set;
                emitArcReleaseLoadedElement(relVal, ck, resolvedElemTy,
                                            "trylst.rel.col");
                break;
            }
            case TryUnwrapElemKind::RecordArc:
                emitRecordArcFieldsRelease(
                    relVal, llvm::cast<llvm::StructType>(elemTy));
                break;
        }
        llvm::Value *jNext = builder_.CreateAdd(
            jCur, llvm::ConstantInt::get(i64Ty_, 1), "trylst.rel.j.next");
        builder_.CreateStore(jNext, jSlot);
        builder_.CreateBr(relHdrBB);
        builder_.SetInsertPoint(relExitBB);
    }

    // Free dest header (ArcHeader 16B prefix; the helper drops the strong
    // refcount → 0 and frees both header and data buffer when we also free
    // the data buffer below). Since the dest list isn't yet a valid handle
    // (header fields uninitialized), call free directly on each buffer.
    auto freeFn = getStdlibFree();
    builder_.CreateCall(freeFn, {destDataPtr});
    // The dest header was obtained from `emitArcAllocCollectionHeader`, which
    // calls `emitArcAlloc` (malloc) + advances past ArcHeader. Free the raw
    // allocation by stepping back ARC_HEADER_SIZE.
    llvm::Value *destHdrRaw = builder_.CreateGEP(
        i8Ty_, destHdr,
        llvm::ConstantInt::getSigned(i64Ty_,
            -static_cast<int64_t>(ARC_HEADER_SIZE)),
        "trylst.dst.hdr.raw");
    builder_.CreateCall(freeFn, {destHdrRaw});

    // Build prefixed Err: `prefix + "element: " + innerMsg`. The failed
    // element index is omitted from the prefix — the inner message from the
    // recursive `tryUnwrapFromAny` carries enough context (e.g. "field 'age'
    // missing") and there is no runtime i64→str helper at this layer.
    std::string elemPrefix = prefix + "element: ";
    llvm::Value *prefixStr = cachedGlobalString(elemPrefix);
    llvm::Value *prefixLen = emitStringByteLen(prefixStr);
    llvm::Value *innerMsgLen = emitStringByteLen(innerMsg);
    llvm::Value *totalLen =
        builder_.CreateAdd(prefixLen, innerMsgLen, "trylst.tot");
    auto makeUninitTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    auto makeUninitFn = mod_->getOrInsertFunction(
        "__ry_string_make_uninit", makeUninitTy);
    llvm::Value *newBuf =
        builder_.CreateCall(makeUninitFn, {totalLen}, "trylst.err.buf");
    auto memcpyFn = getStdlibMemcpy();
    builder_.CreateCall(memcpyFn, {newBuf, prefixStr, prefixLen});
    llvm::Value *dst = builder_.CreateGEP(
        i8Ty_, newBuf, prefixLen, "trylst.err.dst");
    builder_.CreateCall(memcpyFn, {dst, innerMsg, innerMsgLen});
    // Release innerMsg (owned by the failed sub-Result). cachedGlobalString
    // sources are ARC_IMMORTAL so the release is a safe no-op.
    emitArcRelease(emitStrGetHeaderFromData(innerMsg));

    llvm::Value *prefixedErr = llvm::UndefValue::get(errorTy_);
    prefixedErr = builder_.CreateInsertValue(prefixedErr, newBuf, {0});
    prefixedErr = builder_.CreateInsertValue(
        prefixedErr, llvm::ConstantInt::get(i64Ty_, 0), {1});
    llvm::Value *errResVal = buildErrValue(prefixedErr, resTy);
    llvm::BasicBlock *errEndBB = builder_.GetInsertBlock();
    builder_.CreateBr(doneBB);

    // Exit arm: store header fields and Ok.
    builder_.SetInsertPoint(exitBB);
    storeListHeaderFields(destHdr, srcLen, srcLen, destDataPtr);
    setTypeMeta(TypeMeta::ListElem, destHdr, elemTy);
    if (!elemTypeName.empty() && elemTypeName != "int" &&
        elemTypeName != "float" && elemTypeName != "bool") {
        getOrCreateMeta(destHdr).list_elem_type_name = elemTypeName;
    }
    llvm::Value *okVal = buildOkValue(destHdr, resTy);
    llvm::BasicBlock *okEndBB = builder_.GetInsertBlock();
    builder_.CreateBr(doneBB);

    // Merge.
    builder_.SetInsertPoint(doneBB);
    llvm::PHINode *phi = builder_.CreatePHI(resTy, 3, "trylst.result");
    phi->addIncoming(tagErrVal, tagErrEndBB);
    phi->addIncoming(errResVal, errEndBB);
    phi->addIncoming(okVal, okEndBB);
    return phi;
}

llvm::Value *CodeGen::tryUnwrapMapFromAny(llvm::Value *anyVal,
                                            llvm::Type *valTy,
                                            const std::string &valTypeName,
                                            const std::string &targetTypeName,
                                            llvm::StructType *resTy) {
    llvm::Function *fn = builder_.GetInsertBlock()->getParent();
    const std::string typeLabel = targetTypeName.empty()
        ? ("Map<str, " + valTypeName + ">") : targetTypeName;
    const std::string prefix = "load[" + typeLabel + "]: ";

    auto buildInlineError = [&](const std::string &msg) -> llvm::Value * {
        llvm::Value *errStr = cachedGlobalString(msg);
        llvm::Value *errVal = llvm::UndefValue::get(errorTy_);
        errVal = builder_.CreateInsertValue(errVal, errStr, {0});
        errVal = builder_.CreateInsertValue(
            errVal, llvm::ConstantInt::get(i64Ty_, 0), {1});
        return errVal;
    };

    // Tag check (Map=6).
    llvm::Value *tag = builder_.CreateExtractValue(anyVal, 0, "trymap.tag");
    llvm::Value *isMap = builder_.CreateICmpEQ(
        tag,
        llvm::ConstantInt::get(i64Ty_, static_cast<int64_t>(RyAnyTag::Map)),
        "trymap.is_map");

    auto *tagOkBB  = llvm::BasicBlock::Create(*ctx_, "trymap.tag_ok",  fn);
    auto *tagErrBB = llvm::BasicBlock::Create(*ctx_, "trymap.tag_err", fn);
    auto *doneBB   = llvm::BasicBlock::Create(*ctx_, "trymap.done",    fn);

    builder_.CreateCondBr(isMap, tagOkBB, tagErrBB);

    builder_.SetInsertPoint(tagErrBB);
    llvm::Value *tagErrVal = buildErrValue(
        buildInlineError(prefix + "expected JSON object"), resTy);
    llvm::BasicBlock *tagErrEndBB = builder_.GetInsertBlock();
    builder_.CreateBr(doneBB);

    // Tag-ok: load source map header from any.data[8].
    builder_.SetInsertPoint(tagOkBB);
    llvm::AllocaInst *anyTmp =
        builder_.CreateAlloca(anyTy_, nullptr, "trymap.any.tmp");
    builder_.CreateStore(anyVal, anyTmp);
    llvm::Value *anyDataSlot =
        builder_.CreateStructGEP(anyTy_, anyTmp, 1, "trymap.any.data.slot");
    llvm::Value *srcHdrPtr =
        builder_.CreateLoad(ptrTy_, anyDataSlot, "trymap.src.hdr");

    // Source: len at field 0, keys at field 2, vals at field 3.
    llvm::Value *srcLenSlot = builder_.CreateStructGEP(
        mapHeaderTy_, srcHdrPtr, 0, "trymap.src.len.slot");
    llvm::Value *srcLen =
        builder_.CreateLoad(i64Ty_, srcLenSlot, "trymap.src.len");
    llvm::Value *srcKeysSlot = builder_.CreateStructGEP(
        mapHeaderTy_, srcHdrPtr, 2, "trymap.src.keys.slot");
    llvm::Value *srcKeysPtr =
        builder_.CreateLoad(ptrTy_, srcKeysSlot, "trymap.src.keys");
    llvm::Value *srcValsSlot = builder_.CreateStructGEP(
        mapHeaderTy_, srcHdrPtr, 3, "trymap.src.vals.slot");
    llvm::Value *srcValsPtr =
        builder_.CreateLoad(ptrTy_, srcValsSlot, "trymap.src.vals");

    // Allocate fresh dest header + keys/vals buffers.
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t keyStride = dl.getTypeAllocSize(ptrTy_);
    uint64_t valStride = dl.getTypeAllocSize(valTy);
    llvm::Value *destHdr = emitArcAllocCollectionHeader(mapHeaderTy_);
    auto mallocFn = getStdlibMalloc();
    llvm::Value *keyBytes = builder_.CreateMul(
        srcLen, llvm::ConstantInt::get(i64Ty_, keyStride), "trymap.keys.bytes");
    llvm::Value *destKeysPtr =
        builder_.CreateCall(mallocFn, {keyBytes}, "trymap.dst.keys");
    llvm::Value *valBytes = builder_.CreateMul(
        srcLen, llvm::ConstantInt::get(i64Ty_, valStride), "trymap.vals.bytes");
    llvm::Value *destValsPtr =
        builder_.CreateCall(mallocFn, {valBytes}, "trymap.dst.vals");

    TryUnwrapElemKind valKind =
        classifyTryUnwrapElem(*this, valTy, valTypeName);
    std::string resolvedValTy = resolveTypeAlias(valTypeName);

    llvm::AllocaInst *iSlot =
        builder_.CreateAlloca(i64Ty_, nullptr, "trymap.i.slot");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iSlot);
    llvm::AllocaInst *countSlot =
        builder_.CreateAlloca(i64Ty_, nullptr, "trymap.count.slot");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), countSlot);

    auto *headerBB = llvm::BasicBlock::Create(*ctx_, "trymap.loop.header", fn);
    auto *bodyBB   = llvm::BasicBlock::Create(*ctx_, "trymap.loop.body",   fn);
    auto *errBB    = llvm::BasicBlock::Create(*ctx_, "trymap.loop.err",    fn);
    auto *exitBB   = llvm::BasicBlock::Create(*ctx_, "trymap.loop.exit",   fn);

    builder_.CreateBr(headerBB);
    builder_.SetInsertPoint(headerBB);
    llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iSlot, "trymap.i");
    llvm::Value *iLt = builder_.CreateICmpSLT(iCur, srcLen, "trymap.i.lt");
    builder_.CreateCondBr(iLt, bodyBB, exitBB);

    builder_.SetInsertPoint(bodyBB);
    // Load source key (str ptr, native stride 8B).
    llvm::Value *srcKeyPtr = builder_.CreateGEP(
        ptrTy_, srcKeysPtr, iCur, "trymap.src.key.ptr");
    llvm::Value *srcKey =
        builder_.CreateLoad(ptrTy_, srcKeyPtr, "trymap.src.key");
    // Retain the key (alias semantics: dest map shares the key with source).
    {
        llvm::Value *keyHdr = emitStrGetHeaderFromData(srcKey);
        emitArcRetain(keyHdr);
    }
    // GEP source value slot (any stride = 16B) and load any value.
    llvm::Value *srcValAnyPtr = builder_.CreateGEP(
        anyTy_, srcValsPtr, iCur, "trymap.src.val.ptr");
    llvm::Value *srcAny =
        builder_.CreateLoad(anyTy_, srcValAnyPtr, "trymap.src.val");

    llvm::Value *subResult =
        tryUnwrapFromAny(srcAny, valTy, valTypeName);
    llvm::Value *subDisc = builder_.CreateExtractValue(
        subResult, {0}, "trymap.sub.disc");
    auto *subOkBB = llvm::BasicBlock::Create(*ctx_, "trymap.sub.ok", fn);
    builder_.CreateCondBr(subDisc, subOkBB, errBB);

    builder_.SetInsertPoint(subOkBB);
    llvm::Value *subOkVal = builder_.CreateExtractValue(
        subResult, {1}, "trymap.sub.ok.val");
    // Store key (already retained) and value into dest buffers.
    llvm::Value *dstKeyPtr = builder_.CreateGEP(
        ptrTy_, destKeysPtr, iCur, "trymap.dst.key.ptr");
    builder_.CreateStore(srcKey, dstKeyPtr);
    llvm::Value *dstValPtr = builder_.CreateGEP(
        valTy, destValsPtr, iCur, "trymap.dst.val.ptr");
    builder_.CreateStore(subOkVal, dstValPtr);
    llvm::Value *newCount = builder_.CreateAdd(
        iCur, llvm::ConstantInt::get(i64Ty_, 1), "trymap.count.next");
    builder_.CreateStore(newCount, countSlot);
    llvm::Value *iNext = builder_.CreateAdd(
        iCur, llvm::ConstantInt::get(i64Ty_, 1), "trymap.i.next");
    builder_.CreateStore(iNext, iSlot);
    builder_.CreateBr(headerBB);

    // Err arm.
    builder_.SetInsertPoint(errBB);
    llvm::Value *innerErr = builder_.CreateExtractValue(
        subResult, {2}, "trymap.sub.err.val");
    llvm::Value *innerMsg = builder_.CreateExtractValue(
        innerErr, {0}, "trymap.sub.err.msg");
    // The current iteration's key was retained but never stored; release it
    // (matched count_done does NOT include this iteration).
    {
        llvm::Value *keyHdr = emitStrGetHeaderFromData(srcKey);
        emitArcRelease(keyHdr);
    }

    // Release populated keys + vals.
    llvm::Value *populated = builder_.CreateLoad(i64Ty_, countSlot, "trymap.count");
    {
        llvm::AllocaInst *jSlot =
            builder_.CreateAlloca(i64Ty_, nullptr, "trymap.rel.j.slot");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), jSlot);
        auto *relHdrBB  = llvm::BasicBlock::Create(*ctx_, "trymap.rel.header", fn);
        auto *relBodyBB = llvm::BasicBlock::Create(*ctx_, "trymap.rel.body",   fn);
        auto *relExitBB = llvm::BasicBlock::Create(*ctx_, "trymap.rel.exit",   fn);
        builder_.CreateBr(relHdrBB);
        builder_.SetInsertPoint(relHdrBB);
        llvm::Value *jCur = builder_.CreateLoad(i64Ty_, jSlot, "trymap.rel.j");
        llvm::Value *jLt = builder_.CreateICmpSLT(jCur, populated, "trymap.rel.j.lt");
        builder_.CreateCondBr(jLt, relBodyBB, relExitBB);
        builder_.SetInsertPoint(relBodyBB);
        // Release key.
        llvm::Value *relKeyPtr = builder_.CreateGEP(
            ptrTy_, destKeysPtr, jCur, "trymap.rel.key.ptr");
        llvm::Value *relKey =
            builder_.CreateLoad(ptrTy_, relKeyPtr, "trymap.rel.key");
        emitArcReleaseLoadedElement(relKey, CollectionKind::Str,
                                    "str", "trymap.rel.k");
        // Release val if ARC.
        if (valKind != TryUnwrapElemKind::None) {
            llvm::Value *relValPtr = builder_.CreateGEP(
                valTy, destValsPtr, jCur, "trymap.rel.val.ptr");
            llvm::Value *relVal =
                builder_.CreateLoad(valTy, relValPtr, "trymap.rel.val");
            switch (valKind) {
                case TryUnwrapElemKind::None: break;
                case TryUnwrapElemKind::Str:
                    emitArcReleaseLoadedElement(relVal, CollectionKind::Str,
                                                "str", "trymap.rel.v.str");
                    break;
                case TryUnwrapElemKind::Collection: {
                    CollectionKind ck = CollectionKind::List;
                    if (ry::util::isMapTypeName(resolvedValTy)) ck = CollectionKind::Map;
                    else if (ry::util::isSetTypeName(resolvedValTy)) ck = CollectionKind::Set;
                    emitArcReleaseLoadedElement(relVal, ck, resolvedValTy,
                                                "trymap.rel.v.col");
                    break;
                }
                case TryUnwrapElemKind::RecordArc:
                    emitRecordArcFieldsRelease(
                        relVal, llvm::cast<llvm::StructType>(valTy));
                    break;
            }
        }
        llvm::Value *jNext = builder_.CreateAdd(
            jCur, llvm::ConstantInt::get(i64Ty_, 1), "trymap.rel.j.next");
        builder_.CreateStore(jNext, jSlot);
        builder_.CreateBr(relHdrBB);
        builder_.SetInsertPoint(relExitBB);
    }

    auto freeFn = getStdlibFree();
    builder_.CreateCall(freeFn, {destKeysPtr});
    builder_.CreateCall(freeFn, {destValsPtr});
    llvm::Value *destHdrRaw = builder_.CreateGEP(
        i8Ty_, destHdr,
        llvm::ConstantInt::getSigned(i64Ty_,
            -static_cast<int64_t>(ARC_HEADER_SIZE)),
        "trymap.dst.hdr.raw");
    builder_.CreateCall(freeFn, {destHdrRaw});

    // Build prefixed Err: `prefix + "value: " + innerMsg`. The failing key
    // is not included in the prefix — there is no runtime i64→str helper at
    // this layer, and the inner message carries enough context.
    std::string elemPrefix = prefix + "value: ";
    llvm::Value *prefixStr = cachedGlobalString(elemPrefix);
    llvm::Value *prefixLen = emitStringByteLen(prefixStr);
    llvm::Value *innerMsgLen = emitStringByteLen(innerMsg);
    llvm::Value *totalLen =
        builder_.CreateAdd(prefixLen, innerMsgLen, "trymap.tot");
    auto makeUninitTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    auto makeUninitFn = mod_->getOrInsertFunction(
        "__ry_string_make_uninit", makeUninitTy);
    llvm::Value *newBuf =
        builder_.CreateCall(makeUninitFn, {totalLen}, "trymap.err.buf");
    auto memcpyFn = getStdlibMemcpy();
    builder_.CreateCall(memcpyFn, {newBuf, prefixStr, prefixLen});
    llvm::Value *dst = builder_.CreateGEP(
        i8Ty_, newBuf, prefixLen, "trymap.err.dst");
    builder_.CreateCall(memcpyFn, {dst, innerMsg, innerMsgLen});
    emitArcRelease(emitStrGetHeaderFromData(innerMsg));

    llvm::Value *prefixedErr = llvm::UndefValue::get(errorTy_);
    prefixedErr = builder_.CreateInsertValue(prefixedErr, newBuf, {0});
    prefixedErr = builder_.CreateInsertValue(
        prefixedErr, llvm::ConstantInt::get(i64Ty_, 0), {1});
    llvm::Value *errResVal = buildErrValue(prefixedErr, resTy);
    llvm::BasicBlock *errEndBB = builder_.GetInsertBlock();
    builder_.CreateBr(doneBB);

    // Exit: rebuild hash index via __ry_ht_rehash_str, store fields, Ok.
    builder_.SetInsertPoint(exitBB);
    // Compute initBucketCount = max(8, smallest power-of-2 with 3*bc >= 4*len)
    // — replicate the literal pattern at runtime via a simple loop.
    // For simplicity we use 4*len as the initial bucket count rounded up to
    // the next power of two of at least 8. The runtime fn handles arbitrary
    // counts; we just need bc large enough to avoid immediate rehash on
    // first insert.
    llvm::Value *fourLen = builder_.CreateMul(
        srcLen, llvm::ConstantInt::get(i64Ty_, 4), "trymap.bc.4len");
    // bc starts at 8 and doubles while 3*bc < 4*len.
    llvm::AllocaInst *bcSlot =
        builder_.CreateAlloca(i64Ty_, nullptr, "trymap.bc.slot");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 8), bcSlot);
    auto *bcHdrBB  = llvm::BasicBlock::Create(*ctx_, "trymap.bc.header", fn);
    auto *bcBodyBB = llvm::BasicBlock::Create(*ctx_, "trymap.bc.body",   fn);
    auto *bcExitBB = llvm::BasicBlock::Create(*ctx_, "trymap.bc.exit",   fn);
    builder_.CreateBr(bcHdrBB);
    builder_.SetInsertPoint(bcHdrBB);
    llvm::Value *bcCur = builder_.CreateLoad(i64Ty_, bcSlot, "trymap.bc.cur");
    llvm::Value *bcTimes3 = builder_.CreateMul(
        bcCur, llvm::ConstantInt::get(i64Ty_, 3), "trymap.bc.3x");
    llvm::Value *bcLt =
        builder_.CreateICmpSLT(bcTimes3, fourLen, "trymap.bc.lt");
    builder_.CreateCondBr(bcLt, bcBodyBB, bcExitBB);
    builder_.SetInsertPoint(bcBodyBB);
    llvm::Value *bcDoubled = builder_.CreateShl(
        bcCur, llvm::ConstantInt::get(i64Ty_, 1), "trymap.bc.x2");
    builder_.CreateStore(bcDoubled, bcSlot);
    builder_.CreateBr(bcHdrBB);
    builder_.SetInsertPoint(bcExitBB);
    llvm::Value *bcFinal = builder_.CreateLoad(i64Ty_, bcSlot, "trymap.bc.final");

    auto rehashTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, i64Ty_, i64Ty_}, false);
    auto rehashFn = mod_->getOrInsertFunction("__ry_ht_rehash_str", rehashTy);
    llvm::Value *buckets = builder_.CreateCall(rehashFn,
        {destKeysPtr, srcLen, bcFinal}, "trymap.buckets");

    storeMapHeaderFields(destHdr, srcLen, srcLen, destKeysPtr, destValsPtr);
    llvm::Value *bcPtr = builder_.CreateStructGEP(
        mapHeaderTy_, destHdr, 4, "trymap.dst.bc.ptr");
    builder_.CreateStore(bcFinal, bcPtr);
    llvm::Value *bpPtr = builder_.CreateStructGEP(
        mapHeaderTy_, destHdr, 5, "trymap.dst.bp.ptr");
    builder_.CreateStore(buckets, bpPtr);

    setTypeMeta(TypeMeta::MapKey, destHdr, ptrTy_);
    setTypeMeta(TypeMeta::MapValue, destHdr, valTy);
    getOrCreateMeta(destHdr).map_key_type_name = "str";
    if (!valTypeName.empty() && valTypeName != "int" &&
        valTypeName != "float" && valTypeName != "bool") {
        getOrCreateMeta(destHdr).map_value_type_name = valTypeName;
    }
    llvm::Value *okVal = buildOkValue(destHdr, resTy);
    llvm::BasicBlock *okEndBB = builder_.GetInsertBlock();
    builder_.CreateBr(doneBB);

    builder_.SetInsertPoint(doneBB);
    llvm::PHINode *phi = builder_.CreatePHI(resTy, 3, "trymap.result");
    phi->addIncoming(tagErrVal, tagErrEndBB);
    phi->addIncoming(errResVal, errEndBB);
    phi->addIncoming(okVal, okEndBB);
    return phi;
}

llvm::Value *CodeGen::tryUnwrapOptionFromAny(llvm::Value *anyVal,
                                               llvm::StructType *optTy,
                                               llvm::Type *innerTy,
                                               const std::string &innerTypeName,
                                               const std::string &targetTypeName,
                                               llvm::StructType *resTy) {
    llvm::Function *fn = builder_.GetInsertBlock()->getParent();
    const std::string typeLabel = targetTypeName.empty() ? "?" : targetTypeName;
    const std::string prefix = "load[" + typeLabel + "]: ";

    // Three-way dispatch on the source `any` tag:
    //   Unit → Ok(None)
    //   anything else → recurse into innerTy via tryUnwrapFromAny
    //     inner Ok  → Ok(Some(_))
    //     inner Err → Err(prefixed message)
    llvm::Value *tag = builder_.CreateExtractValue(anyVal, 0, "tryopt.tag");
    llvm::Value *isUnit = builder_.CreateICmpEQ(
        tag,
        llvm::ConstantInt::get(i64Ty_, static_cast<int64_t>(RyAnyTag::Unit)),
        "tryopt.is_unit");

    auto *noneBB     = llvm::BasicBlock::Create(*ctx_, "tryopt.none",     fn);
    auto *recurseBB  = llvm::BasicBlock::Create(*ctx_, "tryopt.recurse",  fn);
    auto *innerOkBB  = llvm::BasicBlock::Create(*ctx_, "tryopt.inner_ok", fn);
    auto *innerErrBB = llvm::BasicBlock::Create(*ctx_, "tryopt.inner_err",fn);
    auto *doneBB     = llvm::BasicBlock::Create(*ctx_, "tryopt.done",     fn);

    builder_.CreateCondBr(isUnit, noneBB, recurseBB);

    // None arm: build Ok(None).
    builder_.SetInsertPoint(noneBB);
    llvm::Value *noneVal = buildNoneValue(optTy);
    llvm::Value *noneOk = buildOkValue(noneVal, resTy);
    llvm::BasicBlock *noneEndBB = builder_.GetInsertBlock();
    builder_.CreateBr(doneBB);

    // Recurse arm: hand the same `any` to the inner unwrap. Note the inner
    // call inspects the tag itself and emits its own type-mismatch errors
    // (e.g. "expected JSON object" for record innerTy when tag = Int).
    builder_.SetInsertPoint(recurseBB);
    llvm::Value *subResult =
        tryUnwrapFromAny(anyVal, innerTy, innerTypeName);
    llvm::Value *subDisc = builder_.CreateExtractValue(
        subResult, {0}, "tryopt.sub.disc");
    builder_.CreateCondBr(subDisc, innerOkBB, innerErrBB);

    // Inner Ok: extract value, propagate metadata (so str-prefixed metadata
    // from a Result<str, _>-inner doesn't get lost when wrapped in Some), then
    // wrap in Some(_) and Ok. buildSomeValue retains ptr inner once — we do
    // not double-retain because the recursive unwrap already owns one
    // refcount on the extracted value (see tryUnwrapListFromAny / record).
    builder_.SetInsertPoint(innerOkBB);
    llvm::Value *innerOkVal = builder_.CreateExtractValue(
        subResult, {1}, "tryopt.inner.ok");
    if (!innerTypeName.empty())
        propagateTypeMeta(innerTypeName, innerOkVal);
    llvm::Value *someVal = buildSomeValue(innerOkVal, optTy);
    llvm::Value *someOk = buildOkValue(someVal, resTy);
    llvm::BasicBlock *innerOkEndBB = builder_.GetInsertBlock();
    builder_.CreateBr(doneBB);

    // Inner Err: prepend `load[Option<X>]: expected null or ` to the inner
    // message via __ry_string_make_uninit + memcpy (the same prefix-concat
    // pattern used by tryUnwrapRecordFromAny / tryUnwrapMapFromAny). The
    // resulting wording satisfies the test's "null" + "JSON object"
    // simultaneous toContain assertions for a primitive (int) source.
    builder_.SetInsertPoint(innerErrBB);
    llvm::Value *innerErr = builder_.CreateExtractValue(
        subResult, {2}, "tryopt.inner.err");
    llvm::Value *innerMsg = builder_.CreateExtractValue(
        innerErr, {0}, "tryopt.inner.err.msg");

    std::string errPrefix = prefix + "expected null or ";
    llvm::Value *prefixStr = cachedGlobalString(errPrefix);
    llvm::Value *prefixLen = emitStringByteLen(prefixStr);
    llvm::Value *innerMsgLen = emitStringByteLen(innerMsg);
    llvm::Value *totalLen = builder_.CreateAdd(
        prefixLen, innerMsgLen, "tryopt.err.total");
    auto makeUninitTy =
        llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    auto makeUninitFn = mod_->getOrInsertFunction(
        "__ry_string_make_uninit", makeUninitTy);
    llvm::Value *newBuf =
        builder_.CreateCall(makeUninitFn, {totalLen}, "tryopt.err.buf");
    auto memcpyFn = getStdlibMemcpy();
    builder_.CreateCall(memcpyFn, {newBuf, prefixStr, prefixLen});
    llvm::Value *dst = builder_.CreateGEP(
        i8Ty_, newBuf, prefixLen, "tryopt.err.dst");
    builder_.CreateCall(memcpyFn, {dst, innerMsg, innerMsgLen});
    // Release the inner message (no-op for cachedGlobalString-backed strings
    // since they are ARC_IMMORTAL; required for runtime-built nested messages).
    emitArcRelease(emitStrGetHeaderFromData(innerMsg));

    llvm::Value *prefixedErr = llvm::UndefValue::get(errorTy_);
    prefixedErr = builder_.CreateInsertValue(prefixedErr, newBuf, {0});
    prefixedErr = builder_.CreateInsertValue(
        prefixedErr, llvm::ConstantInt::get(i64Ty_, 0), {1});
    llvm::Value *errResVal = buildErrValue(prefixedErr, resTy);
    llvm::BasicBlock *errEndBB = builder_.GetInsertBlock();
    builder_.CreateBr(doneBB);

    // Merge.
    builder_.SetInsertPoint(doneBB);
    llvm::PHINode *phi = builder_.CreatePHI(resTy, 3, "tryopt.result");
    phi->addIncoming(noneOk, noneEndBB);
    phi->addIncoming(someOk, innerOkEndBB);
    phi->addIncoming(errResVal, errEndBB);
    return phi;
}

llvm::Value *CodeGen::unwrapEnumFromAny(llvm::Value *anyVal, llvm::Type *targetTy,
                                          const std::string &targetTypeName) {
    // Enum unwrap path. Symmetric to wrapInAny's enum-box arm: the box
    // stores `[ArcHeader 16B][descriptor ptr 8B][payload]`. The data
    // pointer in `any.data[8]` is the data-region pointer (= headerPtr +
    // 16); descriptor identity at offset +0 is the runtime type check.
    // Cross-type unwrap is intentionally rejected — enums have no
    // inheritance, so descriptor pointer equality is the type identity.
    llvm::Function *fn = builder_.GetInsertBlock()->getParent();
    llvm::Value *tag = builder_.CreateExtractValue(anyVal, 0, "any.enum.tag.val");

    std::string typeName = targetTypeName;
    if (typeName.empty()) {
        // Recover the source-level enum name from the target LLVM type.
        if (auto *st = llvm::dyn_cast<llvm::StructType>(targetTy)) {
            typeName = findAdtEnumName(st);
            if (typeName.empty()) {
                auto optIt = reverse_option_types_.find(st);
                if (optIt != reverse_option_types_.end()) {
                    std::string inner = reverseResolveTypeName(optIt->second);
                    if (!inner.empty())
                        typeName = "Option<" + inner + ">";
                }
            }
            if (typeName.empty()) {
                auto resIt = reverse_result_types_.find(st);
                if (resIt != reverse_result_types_.end()) {
                    std::string okName = reverseResolveTypeName(resIt->second.first);
                    std::string errName = reverseResolveTypeName(resIt->second.second);
                    if (!okName.empty() && !errName.empty())
                        typeName = "Result<" + okName + ", " + errName + ">";
                }
            }
        }
    }
    if (typeName.empty())
        codegenError("type error: 'any' enum unwrap could not resolve "
                     "source-level type name");

    auto *expectedDesc = getOrCreateEnumDescriptor(typeName, targetTy);
    auto *layoutTy = enumBoxLayoutType(targetTy);

    auto *tagMatchBB = llvm::BasicBlock::Create(*ctx_, "any.enum.tag_ok", fn);
    auto *tagMismatchBB = llvm::BasicBlock::Create(*ctx_, "any.enum.tag_err", fn);
    auto *descMatchBB = llvm::BasicBlock::Create(*ctx_, "any.enum.desc_ok", fn);
    auto *descMismatchBB = llvm::BasicBlock::Create(*ctx_, "any.enum.desc_err", fn);

    llvm::Value *isEnum = builder_.CreateICmpEQ(
        tag, llvm::ConstantInt::get(
                 i64Ty_, static_cast<uint64_t>(RyAnyTag::Enum)),
        "any.is_enum");
    builder_.CreateCondBr(isEnum, tagMatchBB, tagMismatchBB);

    builder_.SetInsertPoint(tagMismatchBB);
    emitRuntimeError("runtime error: any type mismatch (expected " + typeName +
                         ", got non-enum)\n",
                     ".any_enum_type_err");

    builder_.SetInsertPoint(tagMatchBB);
    llvm::AllocaInst *tmp = builder_.CreateAlloca(anyTy_, nullptr, "any.enum.tmp");
    builder_.CreateStore(anyVal, tmp);
    auto *anyDataSlot = builder_.CreateStructGEP(anyTy_, tmp, 1, "any.enum.data.ptr");
    auto *dataPtr = builder_.CreateLoad(ptrTy_, anyDataSlot, "any.enum.data");
    auto *descSlot = builder_.CreateStructGEP(layoutTy, dataPtr, 0,
                                              "any.enum.desc.slot");
    auto *actualDesc = builder_.CreateLoad(ptrTy_, descSlot, "any.enum.desc");
    llvm::Value *descEq = builder_.CreateICmpEQ(actualDesc, expectedDesc,
                                                "any.enum.desc.eq");
    builder_.CreateCondBr(descEq, descMatchBB, descMismatchBB);

    builder_.SetInsertPoint(descMismatchBB);
    emitRuntimeError("runtime error: any enum type mismatch (expected " +
                         typeName + ", got a different enum type)\n",
                     ".any_enum_desc_err");

    builder_.SetInsertPoint(descMatchBB);
    auto *payloadSlot = builder_.CreateStructGEP(layoutTy, dataPtr, 1,
                                                 "any.enum.payload.slot");
    llvm::Value *payloadVal = builder_.CreateLoad(targetTy, payloadSlot,
                                                  "any.enum.payload");
    // Field-wise retain on ARC payload fields — the unwrapped enum becomes a
    // new alias to the boxed value, so any inner str / List / Map / Set must
    // be retained independently so both the box dtor (when the box is later
    // released) and the unwrapped enum's owner can release safely.
    emitEnumBoxArcFieldsRetain(payloadVal, typeName, targetTy);
    return payloadVal;
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
    auto *enumBB = llvm::BasicBlock::Create(*ctx_, name + ".any.rel.enum", parentFn);
    auto *doneBB = llvm::BasicBlock::Create(*ctx_, name + ".any.rel.done", parentFn);

    auto *sw = builder_.CreateSwitch(tagVal, doneBB, 6);
    auto *intTy = llvm::cast<llvm::IntegerType>(i64Ty_);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Str)),    strBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::List)),   listBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Map)),    mapBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Set)),    setBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Record)), recBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Enum)),   enumBB);

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
                    sourceMatchesKind = ry::util::isListTypeName(canon);
                    break;
                case CollectionKind::Map:
                    sourceMatchesKind = ry::util::isMapTypeName(canon);
                    break;
                case CollectionKind::Set:
                    sourceMatchesKind = ry::util::isSetTypeName(canon);
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

    // Enum release path — symmetric to Record. The box layout
    // `[ArcHeader 16B | descriptor ptr 8B | payload]` matches Record's
    // (only the descriptor type differs: enum is 3 ptrs vs record's 4).
    // The data slot holds the data-region pointer (= headerPtr + 16);
    // recover the ArcHeader via the standard `-16` offset and dispatch
    // through `__ry_arc_dtor_enum_dispatch`, which loads the descriptor
    // and calls the per-type enum dtor (no-op for simple enums, ARC
    // field release for ADT / Option / Result with ARC payload).
    builder_.SetInsertPoint(enumBB);
    auto *enumPtr = builder_.CreateLoad(ptrTy_, dataPtr, name + ".any.enum.ptr");
    auto *enumHdr = emitArcGetHeaderFromData(enumPtr);
    auto enumTrampoline = mod_->getOrInsertFunction(
        "__ry_arc_dtor_enum_dispatch", trampolineTy);
    emitArcRelease(enumHdr, isArcAtomic(enumPtr), enumTrampoline, nullptr);
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

    auto *sw = builder_.CreateSwitch(tagVal, doneBB, 6);
    auto *intTy = llvm::cast<llvm::IntegerType>(i64Ty_);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Str)),
                strBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::List)),
                collBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Map)),
                collBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Set)),
                collBB);
    // Record and Enum boxes use the same `-16` ARC header offset as
    // collections, so a single retain block covers all five (List / Map /
    // Set / Record / Enum). The retain only increments the box's strong
    // count — inner ARC field retains happen at unwrap time, not here.
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Record)),
                collBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Enum)),
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
    auto *enumBB = llvm::BasicBlock::Create(*ctx_, siteLabel + ".any.rel.enum", parentFn);
    auto *doneBB = llvm::BasicBlock::Create(*ctx_, siteLabel + ".any.rel.done", parentFn);

    auto *sw = builder_.CreateSwitch(tagVal, doneBB, 6);
    auto *intTy = llvm::cast<llvm::IntegerType>(i64Ty_);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Str)),    strBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::List)),   listBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Map)),    mapBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Set)),    setBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Record)), recBB);
    sw->addCase(llvm::ConstantInt::get(intTy, static_cast<uint64_t>(RyAnyTag::Enum)),   enumBB);

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
                    sourceMatchesKind = ry::util::isListTypeName(canon);
                    break;
                case CollectionKind::Map:
                    sourceMatchesKind = ry::util::isMapTypeName(canon);
                    break;
                case CollectionKind::Set:
                    sourceMatchesKind = ry::util::isSetTypeName(canon);
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

    // Enum release path — symmetric to Record.
    builder_.SetInsertPoint(enumBB);
    auto *enumPtr = builder_.CreateLoad(ptrTy_, dataPtr, siteLabel + ".any.enum.ptr");
    auto *enumHdr = emitArcGetHeaderFromData(enumPtr);
    auto enumTrampoline = mod_->getOrInsertFunction(
        "__ry_arc_dtor_enum_dispatch", trampolineTy);
    emitArcRelease(enumHdr, isArcAtomic(enumPtr), enumTrampoline, nullptr);
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
