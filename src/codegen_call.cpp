#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"
#include "ry/diagnostic/diagnostic.hpp"
#include "ry/llvm_emit/api.h" // RY_LISTCOPY_KEYS / _VALUES / ry_emit_list_enumerate (#2095)
#include "ry/llvm_emit/cast_helpers.hpp" // asRyValue / asRyType / asLlvmValue (#2095)


namespace ry {

// ===== Builtin Conversion =====

llvm::Value *CodeGen::emitBuiltinConversion(const CallExpr &e) {
    // int(s) → Result<int, Error>.  Parses a str; for number→int use `x as int`.
    if (e.callee == "int") {
        requireArgs(e, 1);
        llvm::Value *s = emitExpr(*e.args[0]);
        if (s->getType() != ptrTy_)
            codegenError("int() requires a str argument; use 'value as int' to convert a number to int");
        llvm::AllocaInst *outSlot = builder_.CreateAlloca(i64Ty_, nullptr, "to_int_out");
        auto fn = getRuntimeFn("__ry_str_to_int", i64Ty_, {ptrTy_, ptrTy_});
        used_native_libraries_.insert("convert");
        llvm::Value *status = builder_.CreateCall(fn, {s, outSlot}, "to_int_status");
        llvm::Value *isErr = builder_.CreateICmpNE(status,
            llvm::ConstantInt::get(i64Ty_, 0), "to_int_err");
        llvm::StructType *resTy = getResultType(i64Ty_, errorTy_);
        return emitResultBranch(isErr, resTy,
            [&]() {
                llvm::Value *loaded = builder_.CreateLoad(i64Ty_, outSlot, "to_int_val");
                return buildOkValue(loaded, resTy);
            },
            [&]() { return buildErrValue(buildErrorFromRuntime("__ry_convert_get_last_error"), resTy); });
    }

    // float(s) → Result<float, Error>.  Parses a str; for number→float use `x as float`.
    if (e.callee == "float") {
        requireArgs(e, 1);
        llvm::Value *s = emitExpr(*e.args[0]);
        if (s->getType() != ptrTy_)
            codegenError("float() requires a str argument; use 'value as float' to convert a number to float");
        llvm::AllocaInst *outSlot = builder_.CreateAlloca(f64Ty_, nullptr, "to_float_out");
        auto fn = getRuntimeFn("__ry_str_to_float", i64Ty_, {ptrTy_, ptrTy_});
        used_native_libraries_.insert("convert");
        llvm::Value *status = builder_.CreateCall(fn, {s, outSlot}, "to_float_status");
        llvm::Value *isErr = builder_.CreateICmpNE(status,
            llvm::ConstantInt::get(i64Ty_, 0), "to_float_err");
        llvm::StructType *resTy = getResultType(f64Ty_, errorTy_);
        return emitResultBranch(isErr, resTy,
            [&]() {
                llvm::Value *loaded = builder_.CreateLoad(f64Ty_, outSlot, "to_float_val");
                return buildOkValue(loaded, resTy);
            },
            [&]() { return buildErrValue(buildErrorFromRuntime("__ry_convert_get_last_error"), resTy); });
    }

    // str(v) → str
    if (e.callee == "str") {
        requireArgs(e, 1);
        llvm::Value *v = emitExpr(*e.args[0]);
        return valueToString(v);
    }

    return nullptr;
}

// ===== typeOf builtin =====

llvm::Value *CodeGen::buildTypeValue(int64_t id, const std::string &name) {
    llvm::Constant *nameStr = cachedGlobalString(name, ".type_of_name");
    return llvm::ConstantStruct::get(
        typeTy_,
        {llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(id)), nameStr});
}

std::pair<int64_t, std::string> CodeGen::resolveTypeOfKey(llvm::Value *val) {
    const std::string &llName = getLowLevelTypeName(val);
    if (!llName.empty())
        return {getOrAllocateCanonicalTypeId(llName), llName};

    // Metadata probes are type-guarded: LLVM interns ConstantInt across the
    // module, so metadata attached to one constant site can alias to unrelated
    // sites that share the same bit pattern.
    if (auto *meta = getMeta(val)) {
        llvm::Type *vt = val->getType();
        if (!meta->enum_value_type.empty() && vt == i64Ty_) {
            auto eit = enum_types_.find(meta->enum_value_type);
            if (eit != enum_types_.end())
                return {eit->second.type_id, meta->enum_value_type};
        }
        if (!meta->union_value_type.empty() && vt != i64Ty_) {
            return {getOrAllocateCanonicalTypeId(meta->union_value_type),
                    meta->union_value_type};
        }
        if (meta->fn_type_info.has_value() && vt == ptrTy_)
            return {getOrAllocateCanonicalTypeId("fn"), "fn"};
    }

    // Collection kinds collapse to their base name without constructing the
    // fully-qualified generic form that inferCollectionTypeName would build.
    if (getMapKeyType(val))      return {getOrAllocateCanonicalTypeId("Map"), "Map"};
    if (getListElementType(val)) return {getOrAllocateCanonicalTypeId("List"), "List"};
    if (getSetElementType(val))  return {getOrAllocateCanonicalTypeId("Set"), "Set"};

    llvm::Type *ty = val->getType();
    if (ty == typeTy_)    return {getOrAllocateCanonicalTypeId("Type"), "Type"};
    if (isOptionType(ty)) return {getOrAllocateCanonicalTypeId("Option"), "Option"};
    if (isResultType(ty)) return {getOrAllocateCanonicalTypeId("Result"), "Result"};

    if (auto *st = llvm::dyn_cast<llvm::StructType>(ty)) {
        std::string adtName = findAdtEnumName(st);
        if (!adtName.empty()) {
            auto eit = enum_types_.find(adtName);
            if (eit != enum_types_.end())
                return {eit->second.type_id, adtName};
        }
        std::string name = findRecordTypeName(st);
        if (!name.empty()) {
            auto sit = record_types_.find(name);
            if (sit != record_types_.end())
                return {sit->second.type_id, name};
        }
    }

    std::string name = reverseResolveTypeName(ty);
    return {getOrAllocateCanonicalTypeId(name), name};
}

llvm::Value *CodeGen::emitTypeOf(const CallExpr &e) {
    requireArgs(e, 1);
    const ExprNode &arg = *e.args[0];

    // AST-level fast paths — these handle cases where running emitExpr would
    // either destroy type information (low-level literal suffixes are dropped
    // onto interned ConstantInt/ConstantFP values) or attach metadata to
    // interned constants that then leaks to unrelated sites.

    if (std::holds_alternative<NoneExpr>(arg.data))
        return buildTypeValue(getOrAllocateCanonicalTypeId("None"), "None");

    // Literal numeric suffix (e.g. 1u16, 3.14f32) — read from the AST node
    // directly because constant uniquing prevents metadata from surviving on
    // the emitted Value.
    {
        std::string suffix = getExprLowLevelSuffix(arg);
        if (!suffix.empty())
            return buildTypeValue(getOrAllocateCanonicalTypeId(suffix), suffix);
    }

    // Enum variant access (e.g. Color::Red) — decide the type from the AST so
    // we never rely on metadata attached to interned i64 constants.
    if (auto *ea = std::get_if<EnumAccessExpr>(&arg.data)) {
        auto eit = enum_types_.find(ea->enum_name);
        if (eit != enum_types_.end())
            return buildTypeValue(eit->second.type_id, ea->enum_name);
    }

    llvm::Value *val = emitExpr(arg);
    auto [id, name] = resolveTypeOfKey(val);
    return buildTypeValue(id, name);
}

// ===== Builtin Query =====

llvm::Value *CodeGen::emitBuiltinQuery(const CallExpr &e) {
    if (e.callee == "typeOf") {
        return emitTypeOf(e);
    }
    // ===== keys(map) =====
    if (e.callee == "keys") {
        requireArgs(e, 1);
        llvm::Value *mapVal = emitExpr(*e.args[0]);
        llvm::Type *keyTy = getMapKeyType(mapVal);
        if (!keyTy) codegenError("keys() requires a map");

        auto mf = loadMapHeader(mapVal, "keys");
        llvm::Value *mapLen = mf.len;
        llvm::Value *keysData = mf.keys;

        // Snapshot key type name before any propagateTypeMeta call:
        // getOrCreateMeta inside it may rehash value_metadata_ and
        // invalidate a raw pointer from getMeta (mirrors the pattern in
        // codegen_stmt_misc.cpp's IndexAssignStmt).
        std::string keyName;
        if (auto *containerMeta = getMeta(mapVal))
            keyName = containerMeta->map_key_type_name;

        const llvm::DataLayout &dl = mod_->getDataLayout();
        llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);
        uint64_t elemSize = dl.getTypeAllocSize(keyTy);
        // Copy generation delegated to the llvm_emit boundary (#2093); mapLen
        // (alloc == copy) and keysData feed the malloc + memcpy.
        RyValueId keysSrcId =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(keysData));
        RyValueId keysLenId =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(mapLen));
        RyValueId keysNewDataId = ry_emit_list_copy_full(
            emit_ctx_, keysSrcId, keysLenId, elemSize, RY_LISTCOPY_KEYS);
        llvm::Value *newData = ry::llvm_emit::asLlvmValue(
            ry_emit_resolve(emit_ctx_, keysNewDataId));

        // memcpy duplicates raw pointers without bumping refcounts. Once
        // propagateTypeMeta below stamps list_elem_type_name on the result,
        // its destructor recurses into the inner ARC elements (#1242), so
        // the duplicated pointers must be retained or rebinding the source
        // map will free them out from under the result (#1204).
        // elementTypeIsArcManaged inspects the value side of a Map, so
        // re-derive ARC kind from the key type name directly via
        // fieldTypeIsArcManaged (the shared name-based predicate).
        CollectionKind keyArcKind;
        if (!keyName.empty() && fieldTypeIsArcManaged(keyName, &keyArcKind))
            emitCowRetainArcElements(newData, mapLen, "keys_elem", keyArcKind);

        storeListHeaderFields(newHeader, mapLen, mapLen, newData);
        setTypeMeta(TypeMeta::ListElem, newHeader, keyTy);
        if (!keyName.empty())
            propagateTypeMeta("List<" + keyName + ">", newHeader);
        return newHeader;
    }

    // ===== values(map) =====
    if (e.callee == "values") {
        requireArgs(e, 1);
        llvm::Value *mapVal = emitExpr(*e.args[0]);
        llvm::Type *valTy = getMapValueType(mapVal);
        if (!valTy) codegenError("values() requires a map");

        auto mf = loadMapHeader(mapVal, "vals");
        llvm::Value *mapLen = mf.len;
        llvm::Value *valsData = mf.vals;

        // Snapshot value type name before any propagateTypeMeta call:
        // getOrCreateMeta inside it may rehash value_metadata_ and
        // invalidate a raw pointer from getMeta.
        std::string valName;
        if (auto *containerMeta = getMeta(mapVal))
            valName = containerMeta->map_value_type_name;

        const llvm::DataLayout &dl = mod_->getDataLayout();
        llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);
        uint64_t elemSize = dl.getTypeAllocSize(valTy);
        // Copy generation delegated to the llvm_emit boundary (#2093); mapLen
        // (alloc == copy) and valsData feed the malloc + memcpy.
        RyValueId valsSrcId =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(valsData));
        RyValueId valsLenId =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(mapLen));
        RyValueId valsNewDataId = ry_emit_list_copy_full(
            emit_ctx_, valsSrcId, valsLenId, elemSize, RY_LISTCOPY_VALUES);
        llvm::Value *newData = ry::llvm_emit::asLlvmValue(
            ry_emit_resolve(emit_ctx_, valsNewDataId));

        // memcpy of an ARC-managed value buffer must be paired with retain;
        // see keys() above for the rationale (#1204 / #1242).
        CollectionKind valArcKind = CollectionKind::List;
        if (elementTypeIsArcManaged(mapVal, CollectionKind::Map, &valArcKind)) {
            emitCowRetainArcElements(newData, mapLen, "vals_elem", valArcKind);
        }

        storeListHeaderFields(newHeader, mapLen, mapLen, newData);
        setTypeMeta(TypeMeta::ListElem, newHeader, valTy);
        if (!valName.empty())
            propagateTypeMeta("List<" + valName + ">", newHeader);
        return newHeader;
    }

    // ===== first(list) → Option<T> =====
    if (e.callee == "first") {
        requireArgs(e, 1);
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("first() requires a list");
        llvm::StructType *optTy = getOptionType(elemTy);
        auto lf = loadListHeader(listVal, "first");
        llvm::Value *srcLen = lf.len;
        llvm::Value *srcData = lf.data;

        // #2094 ([C] = (ii) boundary move): condition compute and element load
        // cross via generic primitives; BB scaffold, PHI, and Some/None aggregate
        // build were already boundary-emitted.
        llvm::Value *isEmptyF = emitICmpEQ(srcLen, emitConstInt(i64Ty_, 0), "first_empty");
        llvm::BasicBlock *emptyBB = createBB("first.empty");
        llvm::BasicBlock *okBB = createBB("first.ok");
        llvm::BasicBlock *mergeBB = createBB("first.merge");
        emitBranchCond(isEmptyF, emptyBB, okBB);

        builder_.SetInsertPoint(emptyBB);
        llvm::Value *noneVal = buildNoneValue(optTy);
        emitBranchUncond(mergeBB);
        llvm::BasicBlock *emptyEndBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(okBB);
        llvm::Value *firstVal = emitLoad(elemTy, srcData, "first_val");
        llvm::Value *someVal = buildSomeValue(firstVal, optTy);
        emitBranchUncond(mergeBB);
        llvm::BasicBlock *okEndBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = createPhi(optTy, {}, "first_result");
        phi->addIncoming(noneVal, emptyEndBB);
        phi->addIncoming(someVal, okEndBB);
        return phi;
    }

    // ===== last(list) → Option<T> =====
    if (e.callee == "last") {
        requireArgs(e, 1);
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listVal);
        if (!elemTy) codegenError("last() requires a list");
        llvm::StructType *optTy = getOptionType(elemTy);
        auto lf = loadListHeader(listVal, "last");
        llvm::Value *srcLen = lf.len;
        llvm::Value *srcData = lf.data;

        // #2094 ([C] = (ii) boundary move): condition compute, last-index sub,
        // element GEP + load cross via generic primitives.
        llvm::Value *isEmptyL = emitICmpEQ(srcLen, emitConstInt(i64Ty_, 0), "last_empty");
        llvm::BasicBlock *emptyBBL = createBB("last.empty");
        llvm::BasicBlock *okBBL = createBB("last.ok");
        llvm::BasicBlock *mergeBBL = createBB("last.merge");
        emitBranchCond(isEmptyL, emptyBBL, okBBL);

        builder_.SetInsertPoint(emptyBBL);
        llvm::Value *noneValL = buildNoneValue(optTy);
        emitBranchUncond(mergeBBL);
        llvm::BasicBlock *emptyEndBBL = builder_.GetInsertBlock();

        builder_.SetInsertPoint(okBBL);
        llvm::Value *lastIdx = emitSub(srcLen, emitConstInt(i64Ty_, 1), "last_idx");
        llvm::Value *elemPtr = emitGEP(elemTy, srcData, lastIdx, "last_ep");
        llvm::Value *lastVal = emitLoad(elemTy, elemPtr, "last_val");
        llvm::Value *someValL = buildSomeValue(lastVal, optTy);
        emitBranchUncond(mergeBBL);
        llvm::BasicBlock *okEndBBL = builder_.GetInsertBlock();

        builder_.SetInsertPoint(mergeBBL);
        llvm::PHINode *phiL = createPhi(optTy, {}, "last_result");
        phiL->addIncoming(noneValL, emptyEndBBL);
        phiL->addIncoming(someValL, okEndBBL);
        return phiL;
    }

    // ===== isEmpty(list/map/set/str) =====
    if (e.callee == "isEmpty") {
        requireArgs(e, 1);
        llvm::Value *val = emitExpr(*e.args[0]);
        llvm::Type *headerTy = nullptr;
        if (getListElementType(val)) headerTy = listHeaderTy_;
        else if (getMapKeyType(val)) headerTy = mapHeaderTy_;
        else if (getSetElementType(val)) headerTy = setHeaderTy_;
        if (headerTy) {
            llvm::Value *lenPtr = emitStructGEP(headerTy, val, 0, "");
            llvm::Value *len = emitLoad(i64Ty_, lenPtr, "ie_len");
            return emitICmpEQ(len, emitConstInt(i64Ty_, 0), "isEmpty");
        }
        // String (#831, #1022, #1069): read byte_len from the StringHeader instead of
        // peeking the first data byte — embedded NUL bytes are valid string content
        // (tracked by byte_len since #1022) and must not be mistaken for an empty
        // string. emitStringByteLen is also O(1) (a single i64 load from handle - 8).
        if (val->getType() == ptrTy_) {
            llvm::Value *len = emitStringByteLen(val);
            return emitICmpEQ(len, emitConstInt(i64Ty_, 0), "isEmpty");
        }
        codegenError("isEmpty() requires a collection (list, map, set) or str");
    }

    // ===== enumerate(list) =====
    if (e.callee == "enumerate") {
        requireArgs(e, 1);
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listVal);
        // String fallback (#746, #827): enumerate over a str yields
        // `(int, str)` pairs per UTF-8 code point.
        if (!elemTy && isStrLike(listVal)) {
            listVal = emitStringToCharList(listVal, "enum_str_chars");
            elemTy = ptrTy_;
        }
        if (!elemTy) {
            codegenErrorNoMatchingOverload(
                "enumerate",
                collectNativeOverloadCandidateSigs("enumerate"),
                {formatActualArgTypeName(listVal)});
        }

        // Snapshot the source list's element name so we can rebuild a tuple
        // type string "(int, <elem>)" for the result (#813). See
        // snapshotListElemName for the fallback rules.
        std::string srcElemName = snapshotListElemName(listVal, elemTy);

        auto lf = loadListHeader(listVal, "enum");
        llvm::StructType *tupleTy = llvm::StructType::get(*ctx_, {i64Ty_, elemTy});
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t tupleSize = dl.getTypeAllocSize(tupleTy);

        llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);

        // #2095 migration. The boundary owns the loop body + tuple build +
        // storeListHeaderFields; the C++ side retains the metadata propagation.
        // emitTupleComponentRetain (re-entrant: it recurses through nested
        // tuple components) crosses via a stack-struct trampoline (see #2069 /
        // ResultBranchTrampolineCtx pattern).
        struct EnumRetainCtx {
            CodeGen *cg;
            std::string sig;
        } retainCtx{this, srcElemName};
        RyRetainFn retainFn = nullptr;
        if (!srcElemName.empty()) {
            retainFn = [](RyValueId valId, void *uc) {
                auto *r = static_cast<EnumRetainCtx *>(uc);
                llvm::Value *v =
                    ry::llvm_emit::asLlvmValue(ry_emit_resolve(r->cg->emit_ctx_, valId));
                r->cg->emitTupleComponentRetain(v, r->sig);
            };
        }
        RyValueId srcLenId = ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(lf.len));
        RyValueId srcDataId = ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(lf.data));
        RyValueId newHeaderId =
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(newHeader));
        ry_emit_list_enumerate(emit_ctx_, srcLenId, srcDataId, newHeaderId,
                                ry::llvm_emit::asRyType(listHeaderTy_),
                                ry::llvm_emit::asRyType(elemTy),
                                ry::llvm_emit::asRyType(tupleTy), tupleSize,
                                retainFn,
                                retainFn ? static_cast<void *>(&retainCtx) : nullptr);

        setTypeMeta(TypeMeta::ListElem, newHeader, tupleTy);
        if (!srcElemName.empty())
            getOrCreateMeta(newHeader).list_elem_type_name =
                "(int, " + srcElemName + ")";
        return newHeader;
    }

    // ===== zip(list1, list2) =====
    if (e.callee == "zip") {
        requireArgs(e, 2);
        llvm::Value *list1 = emitExpr(*e.args[0]);
        llvm::Value *list2 = emitExpr(*e.args[1]);
        llvm::Type *elemTy1 = getListElementType(list1);
        llvm::Type *elemTy2 = getListElementType(list2);
        // String fallback (#746, #827): either (or both) zip arguments may
        // be a str; each is desugared independently to a List<str>.
        auto stringifyIfStr = [&](llvm::Value *&val, llvm::Type *&ty,
                                   const char *name) {
            if (!ty && isStrLike(val)) {
                val = emitStringToCharList(val, name);
                ty = ptrTy_;
            }
        };
        stringifyIfStr(list1, elemTy1, "zip_str_chars1");
        stringifyIfStr(list2, elemTy2, "zip_str_chars2");
        if (!elemTy1 || !elemTy2) codegenError("zip() requires two lists or strs");

        // Snapshot both source element names before entering the IR loop
        // (same rationale as enumerate — see snapshotListElemName).
        std::string n1 = snapshotListElemName(list1, elemTy1);
        std::string n2 = snapshotListElemName(list2, elemTy2);

        auto lf1 = loadListHeader(list1, "zip1");
        auto lf2 = loadListHeader(list2, "zip2");
        // Compute zip_minlen on the C++ side BEFORE the ARC alloc to preserve
        // the C++ baseline's instruction order byte-for-byte.
        llvm::Value *minLen = builder_.CreateSelect(
            builder_.CreateICmpSLT(lf1.len, lf2.len), lf1.len, lf2.len, "zip_minlen");

        llvm::StructType *tupleTy = llvm::StructType::get(*ctx_, {elemTy1, elemTy2});
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t tupleSize = dl.getTypeAllocSize(tupleTy);
        llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);

        // #2095 — same trampoline pattern as enumerate, with 2 callbacks.
        struct ZipRetainCtx {
            CodeGen *cg;
            std::string n1;
            std::string n2;
        } retainCtx{this, n1, n2};
        RyRetainFn retain1Fn = nullptr;
        if (!n1.empty()) {
            retain1Fn = [](RyValueId valId, void *uc) {
                auto *r = static_cast<ZipRetainCtx *>(uc);
                llvm::Value *v =
                    ry::llvm_emit::asLlvmValue(ry_emit_resolve(r->cg->emit_ctx_, valId));
                r->cg->emitTupleComponentRetain(v, r->n1);
            };
        }
        RyRetainFn retain2Fn = nullptr;
        if (!n2.empty()) {
            retain2Fn = [](RyValueId valId, void *uc) {
                auto *r = static_cast<ZipRetainCtx *>(uc);
                llvm::Value *v =
                    ry::llvm_emit::asLlvmValue(ry_emit_resolve(r->cg->emit_ctx_, valId));
                r->cg->emitTupleComponentRetain(v, r->n2);
            };
        }
        ry_emit_list_zip(emit_ctx_,
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(minLen)),
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(lf1.data)),
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(lf2.data)),
            ry_emit_intern(emit_ctx_, ry::llvm_emit::asRyValue(newHeader)),
            ry::llvm_emit::asRyType(listHeaderTy_),
            ry::llvm_emit::asRyType(elemTy1),
            ry::llvm_emit::asRyType(elemTy2),
            ry::llvm_emit::asRyType(tupleTy), tupleSize,
            retain1Fn, retain2Fn,
            (retain1Fn || retain2Fn) ? static_cast<void *>(&retainCtx) : nullptr);

        setTypeMeta(TypeMeta::ListElem, newHeader, tupleTy);
        if (!n1.empty() && !n2.empty())
            getOrCreateMeta(newHeader).list_elem_type_name =
                "(" + n1 + ", " + n2 + ")";
        return newHeader;
    }

    return nullptr;
}

// ===== Builtin Core =====

llvm::Value *CodeGen::emitBuiltinCore(const CallExpr &e) {
    // exit(code) as expression — emitExit() switches the insert point to a
    // fresh dead block internally (see codegen_match.cpp), so any trailing
    // statements still land on a valid (unreachable) block.
    if (e.callee == "exit") {
        emitExit(e.args);
        return llvm::UndefValue::get(i64Ty_);
    }

    // args() → List<str>
    if (e.callee == "args") {
        if (!e.args.empty())
            codegenError("args() takes no arguments");

        // Call __ry_args_count()
        llvm::Value *count32 = emitRuntimeCallDirect("__ry_args_count", i32Ty_, {}, {}, "argc");
        llvm::Value *count = emitSExt(count32, i64Ty_, "argc64");

        // Allocate list header
        const llvm::DataLayout &dl = mod_->getDataLayout();
        llvm::Value *headerPtr = emitArcAllocCollectionHeader(listHeaderTy_);

        // Allocate data array (ptr per element)
        uint64_t elemSize = dl.getTypeAllocSize(ptrTy_);
        llvm::Value *dataSize = emitMul(count, emitConstInt(i64Ty_, elemSize), "args_data_size");
        llvm::Value *dataPtr = emitRuntimeCallDirect("malloc", ptrTy_, {i64Ty_}, {dataSize}, "args_data");

        // Loop: for i in 0..count, get arg pointer. Capture zero/one once so
        // each `emitConstInt` boundary trip runs once rather than per use
        // (mirrors range()'s pattern).
        llvm::Value *zero = emitConstInt(i64Ty_, 0);
        llvm::Value *one = emitConstInt(i64Ty_, 1);
        llvm::Value *iVar = emitAlloca(i64Ty_, "args_i");
        emitStore(zero, iVar);

        llvm::BasicBlock *condBB = createBB("args.cond");
        llvm::BasicBlock *bodyBB = createBB("args.body");
        llvm::BasicBlock *endBB  = createBB("args.end");

        emitBranchUncond(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *iVal = emitLoad(i64Ty_, iVar, "ai");
        llvm::Value *cond = emitICmpSLT(iVal, count, "args_cond");
        emitBranchCond(cond, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = emitLoad(i64Ty_, iVar, "ai_cur");
        llvm::Value *iCur32 = emitTrunc(iCur, i32Ty_, "ai_cur32");
        llvm::Value *argStr = emitRuntimeCallDirect("__ry_args_get", ptrTy_, {i32Ty_}, {iCur32}, "arg_str");
        llvm::Value *elemPtr = emitGEP(ptrTy_, dataPtr, iCur, "args_elem_ptr");
        emitStore(argStr, elemPtr);
        llvm::Value *iNext = emitAdd(iCur, one, "ai_next");
        emitStore(iNext, iVar);
        emitBranchUncond(condBB);

        builder_.SetInsertPoint(endBB);

        // Store header fields
        storeListHeaderFields(headerPtr, count, count, dataPtr);

        setTypeMeta(TypeMeta::ListElem, headerPtr, ptrTy_);
        return headerPtr;
    }

    // availableParallelism() -> int
    if (e.callee == "availableParallelism") {
        if (!e.args.empty())
            codegenError("availableParallelism() takes no arguments");

        llvm::FunctionCallee fn = getRuntimeFn("__ry_available_parallelism", i64Ty_, {});
        return builder_.CreateCall(fn, {}, "availableParallelism");
    }

    // blockOn(task) -> T — block the current thread until the Task<T> completes
    if (e.callee == "blockOn") {
        requireArgs(e, 1);
        return emitTaskWait(emitExpr(*e.args[0]), "__ry_block_on", "blockOn");
    }

    // sleep(duration_ms) -> Unit
    if (e.callee == "sleep") {
        requireArgs(e, 1);
        llvm::Value *duration = emitExpr(*e.args[0]);
        if (duration->getType() != i64Ty_)
            codegenError("sleep() requires int argument");

        llvm::FunctionCallee fn = getRuntimeFn("__ry_sleep", llvm::Type::getVoidTy(*ctx_), {i64Ty_});
        return builder_.CreateCall(fn, {duration});
    }

    if (e.callee == "input") {
        if (e.args.size() > 1)
            codegenError("input() takes 0 or 1 arguments");
        // Runtime lives in libry_io — without this insert the JIT fails to
        // resolve __ry_io_read_line / __ry_io_input_prompt for programs that
        // never `import` from io. Bare builtins are not declared as
        // @native("io"), so library registration must happen here.
        used_native_libraries_.insert("io");

        llvm::Value *outAlloca = emitAlloca(ptrTy_, "inp_out");
        emitStore(emitConstNull(ptrTy_), outAlloca);

        llvm::Value *status;
        if (e.args.size() == 1) {
            llvm::Value *prompt = emitExpr(*e.args[0]);
            if (prompt->getType() != ptrTy_)
                codegenError("input() prompt must be str");
            status = emitRuntimeCallDirect("__ry_io_input_prompt", i64Ty_,
                                           {ptrTy_, ptrTy_}, {prompt, outAlloca},
                                           "inp_status");
        } else {
            status = emitRuntimeCallDirect("__ry_io_read_line", i64Ty_,
                                           {ptrTy_}, {outAlloca}, "inp_status");
        }

        llvm::Value *isErr = emitICmpSLT(status, emitConstInt(i64Ty_, 0), "inp_iserr");
        llvm::StructType *optTy = getOptionType(ptrTy_);
        llvm::StructType *resTy = getResultType(optTy, errorTy_);
        return emitResultBranch(isErr, resTy,
            [&]() -> llvm::Value * {
                llvm::Value *linePtr = emitLoad(ptrTy_, outAlloca, "inp_line");
                return buildOkValue(wrapPtrAsOption(linePtr, "input"), resTy);
            },
            [&]() -> llvm::Value * {
                return buildErrValue(buildErrorFromRuntime(), resTy);
            });
    }

    if (e.callee == "env") {
        if (e.args.empty() || e.args.size() > 2)
            codegenError("env() takes 1 or 2 arguments");
        llvm::Value *key = emitExpr(*e.args[0]);
        if (key->getType() != ptrTy_)
            codegenError("env() key must be str");

        // __ry_env_get wraps getenv() result in a StringHeader-managed handle
        // so that byte_len / length / etc. work correctly on the returned str.
        llvm::Value *result = emitRuntimeCallDirect("__ry_env_get", ptrTy_,
                                                    {ptrTy_}, {key}, "env_result");
        llvm::Value *isNull = emitICmpEQ(result, emitConstNull(ptrTy_), "env_null");

        if (e.args.size() == 1) {
            return wrapPtrAsOption(result, "env");
        } else {
            llvm::Value *def = emitExpr(*e.args[1]);
            if (def->getType() != ptrTy_)
                codegenError("env() default must be str");
            return emitSelect(isNull, def, result, "env_val");
        }
    }

    if (e.callee == "send") {
        requireArgs(e, 2);
        llvm::Value *firstArg = emitExpr(*e.args[0]);
        if (!isTcpStream(firstArg) && !isTlsStream(firstArg))
            codegenError("send() requires TcpStream or TlsStream as first argument");
        llvm::Value *data = emitExpr(*e.args[1]);
        if (!getListElementType(data) || getListElementType(data) != i8Ty_)
            codegenError("send() with TcpStream/TlsStream requires List<u8> as second argument");
        std::string rtFn = isTlsStream(firstArg) ? "__ry_tls_send" : "__ry_tcp_send";
        auto fn = getRuntimeFn(rtFn.c_str(), i64Ty_, {ptrTy_, ptrTy_});
        llvm::Value *sent = builder_.CreateCall(fn, {firstArg, data}, "tcp_send");
        // Wrap in Result<int, Error>
        llvm::Value *isErr = builder_.CreateICmpSLT(sent,
            llvm::ConstantInt::get(i64Ty_, 0), "send_err");
        llvm::StructType *resTy = getResultType(i64Ty_, errorTy_);
        llvm::Value *okVal = buildOkValue(sent, resTy);
        llvm::Value *errVal = buildErrValue(buildStaticError("send failed", ".send_err_msg"), resTy);
        return builder_.CreateSelect(isErr, errVal, okVal, "send_result");
    }

    if (e.callee == "receive") {
        requireArgs(e, 2);
        // TCP/TLS receive(stream, max_bytes) -> Result<List<u8>, Error>
        llvm::Value *streamVal = emitExpr(*e.args[0]);
        if (!isTcpStream(streamVal) && !isTlsStream(streamVal))
            codegenError("receive() requires TcpStream or TlsStream as first argument");
        llvm::Value *maxBytes = emitExpr(*e.args[1]);
        std::string rtFn = isTlsStream(streamVal) ? "__ry_tls_receive" : "__ry_tcp_receive";
        auto fn = getRuntimeFn(rtFn.c_str(), ptrTy_, {ptrTy_, i64Ty_});
        llvm::Value *ptr = builder_.CreateCall(fn, {streamVal, maxBytes}, "tcp_receive");
        // Wrap in Result<List<u8>, Error>: nullptr = Err, non-null = Ok
        llvm::Value *isNull = builder_.CreateICmpEQ(ptr,
            llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)), "receive_null");
        llvm::StructType *resTy = getResultType(ptrTy_, errorTy_);
        llvm::Value *okVal = buildOkValue(ptr, resTy);
        llvm::Value *errVal = buildErrValue(buildStaticError("receive failed", ".receive_err_msg"), resTy);
        llvm::Value *result = builder_.CreateSelect(isNull, errVal, okVal, "receive_result");
        setTypeMeta(TypeMeta::ListElem, result, i8Ty_);
        return result;
    }

    if (e.callee == "close") {
        requireArgs(e, 1);
        llvm::Value *val = emitExpr(*e.args[0]);
        if (isTcpListener(val))
            return emitResourceFree(val, detectResourceKind(val), *e.args[0]);
        if (isTcpStream(val))
            return emitResourceFree(val, detectResourceKind(val), *e.args[0]);
        if (isTlsStream(val))
            return emitResourceFree(val, detectResourceKind(val), *e.args[0]);
        if (isFile(val)) {
            // close() = invalidate fp inside the still-alive ARC box. ARC release
            // happens at scope exit (or when the last alias drops); decoupling
            // the user-facing close from refcount-drop keeps lines()/readLine()
            // iterators valid after close (they observe fp==nullptr and finish).
            // The io runtime symbol lives in libry_io — register the library so
            // bare close(f) calls (no enclosing io import path) resolve.
            used_native_libraries_.insert("io");
            auto fn = getRuntimeFn("__ry_io_file_close", llvm::Type::getVoidTy(*ctx_), {ptrTy_});
            builder_.CreateCall(fn, {val});
            return llvm::ConstantInt::get(i8Ty_, 0); // Unit
        }
        codegenError("close() requires a File, TcpStream, TlsStream, or TcpListener argument");
    }

    // range(n), range(start, end), or range(start, end, step) → List<int>
    if (e.callee == "range") {
        if (e.args.size() < 1 || e.args.size() > 3)
            codegenError("range() takes 1, 2, or 3 arguments");

        // Emit all argument values first, then verify each is int (i64).
        // Custom-emitter builtins bypass the table-driven type check, so this
        // guard is what produces "no matching overload" diagnostics for
        // calls like `range(1..n)` instead of letting LLVM verify reject
        // the resulting IR with "Both operands ... not of the same type".
        std::vector<llvm::Value *> argVals;
        argVals.reserve(e.args.size());
        for (auto &a : e.args)
            argVals.push_back(emitExpr(*a));
        for (auto *v : argVals) {
            if (v->getType() != i64Ty_) {
                std::vector<std::string> actuals;
                actuals.reserve(argVals.size());
                for (auto *vv : argVals)
                    actuals.push_back(formatActualArgTypeName(vv));
                codegenErrorNoMatchingOverload(
                    "range",
                    collectNativeOverloadCandidateSigs("range"),
                    actuals);
            }
        }

        llvm::Value *start, *end, *step;
        llvm::Value *zero = emitConstInt(i64Ty_, 0);
        llvm::Value *one = emitConstInt(i64Ty_, 1);

        if (e.args.size() == 1) {
            start = zero;
            end = argVals[0];
            step = one;
        } else if (e.args.size() == 2) {
            start = argVals[0];
            end = argVals[1];
            step = one;
        } else {
            start = argVals[0];
            end = argVals[1];
            step = argVals[2];
        }

        // Runtime check: step == 0 → error
        if (e.args.size() == 3) {
            llvm::Value *stepZero = emitICmpEQ(step, zero, "step_zero");
            llvm::BasicBlock *errBB = createBB("range.step_err");
            llvm::BasicBlock *okBB = createBB("range.step_ok");
            emitBranchCond(stepZero, errBB, okBB);
            builder_.SetInsertPoint(errBB);
            emitRuntimeError("runtime error: range() step must not be zero\n", ".range_step_err");
            builder_.SetInsertPoint(okBB);
        }

        // Compute count based on step sign
        // step > 0: count = max(0, (end - start + step - 1) / step)
        // step < 0: count = max(0, (start - end + (-step) - 1) / (-step))
        llvm::Value *stepPos = emitICmpSGT(step, zero, "step_pos");

        // Positive step case
        llvm::Value *diffPos = emitSub(end, start, "diff_pos");
        llvm::Value *numPos = emitAdd(diffPos, emitSub(step, one, "step_m1"), "num_pos");
        llvm::Value *countPos = emitSDiv(numPos, step, "count_pos");
        llvm::Value *countPosClamped = emitSelect(
            emitICmpSGT(countPos, zero, "pos_gt0"), countPos, zero, "count_pos_c");

        // Negative step case. CreateNeg(x) is definitionally CreateSub(0, x) in
        // LLVM (see llvm::BinaryOperator::CreateNeg); using emitSub keeps the
        // textual IR `sub i64 0, %step` byte-identical with the C++ baseline
        // without introducing a dedicated ry_emit_neg primitive (the remaining
        // CreateNeg sites in codegen_call.cpp:1139 / codegen_expr.cpp:325 are
        // out of scope for #2192).
        llvm::Value *negStep = emitSub(zero, step, "neg_step");
        llvm::Value *diffNeg = emitSub(start, end, "diff_neg");
        llvm::Value *numNeg = emitAdd(diffNeg, emitSub(negStep, one, "negstep_m1"), "num_neg");
        llvm::Value *countNeg = emitSDiv(numNeg, negStep, "count_neg");
        llvm::Value *countNegClamped = emitSelect(
            emitICmpSGT(countNeg, zero, "neg_gt0"), countNeg, zero, "count_neg_c");

        llvm::Value *count = emitSelect(stepPos, countPosClamped, countNegClamped, "range_count");

        // Allocate list header
        const llvm::DataLayout &dl = mod_->getDataLayout();
        llvm::Value *headerPtr = emitArcAllocCollectionHeader(listHeaderTy_);

        // Allocate data array
        uint64_t elemSize = dl.getTypeAllocSize(i64Ty_);
        llvm::Value *dataSize = emitMul(count, emitConstInt(i64Ty_, elemSize), "range_data_size");
        llvm::Value *dataPtr = emitRuntimeCallDirect("malloc", ptrTy_, {i64Ty_}, {dataSize}, "range_data");

        // Fill data with start, start+step, start+2*step, ... using a loop
        llvm::Value *iVar = emitAlloca(i64Ty_, "range_i");
        emitStore(zero, iVar);

        llvm::BasicBlock *condBB = createBB("range.cond");
        llvm::BasicBlock *bodyBB = createBB("range.body");
        llvm::BasicBlock *endBB  = createBB("range.end");

        emitBranchUncond(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *iVal = emitLoad(i64Ty_, iVar, "ri");
        llvm::Value *cond = emitICmpSLT(iVal, count, "range_cond");
        emitBranchCond(cond, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = emitLoad(i64Ty_, iVar, "ri_cur");
        llvm::Value *offset = emitMul(iCur, step, "range_offset");
        llvm::Value *val = emitAdd(start, offset, "range_val");
        llvm::Value *elemPtr = emitGEP(i64Ty_, dataPtr, iCur, "range_elem_ptr");
        emitStore(val, elemPtr);
        llvm::Value *iNext = emitAdd(iCur, one, "ri_next");
        emitStore(iNext, iVar);
        emitBranchUncond(condBB);

        builder_.SetInsertPoint(endBB);

        // Store header fields
        storeListHeaderFields(headerPtr, count, count, dataPtr);

        setTypeMeta(TypeMeta::ListElem, headerPtr, i64Ty_);
        return headerPtr;
    }

    // len(xs) → list/map/array/set/str length
    if (e.callee == "len") {
        requireArgs(e, 1);
        llvm::Value *ptr = emitExpr(*e.args[0]);
        // Fixed-length array: return compile-time constant
        if (auto *ai = llvm::dyn_cast<llvm::AllocaInst>(ptr)) {
            if (auto *arrTy = llvm::dyn_cast<llvm::ArrayType>(ai->getAllocatedType()))
                return llvm::ConstantInt::get(i64Ty_, arrTy->getNumElements());
        }
        if (ptr->getType() != ptrTy_) {
            codegenErrorNoMatchingOverload(
                "len",
                collectNativeOverloadCandidateSigs("len"),
                {formatActualArgTypeName(ptr)});
        }
        // Check if it's a set
        if (getSetElementType(ptr))
            return loadSetHeader(ptr, "set").len;
        // Check if it's a map
        llvm::Type *mapKeyTy = getMapKeyType(ptr);
        if (mapKeyTy)
            return loadMapHeader(ptr, "map").len;
        // Check if it's a list
        if (getListElementType(ptr))
            return loadListHeader(ptr, "list").len;
        // String: call __ry_utf8_len_n (NUL-safe character count)
        llvm::Value *byteLen = emitStringByteLen(ptr);
        auto utf8LenFn = getRuntimeFn("__ry_utf8_len_n", i64Ty_, {ptrTy_, i64Ty_});
        return builder_.CreateCall(utf8LenFn, {ptr, byteLen}, "str_len");
    }

    // byteLen(str) → int (byte length — NUL-safe, reads from StringHeader)
    if (e.callee == "byteLen") {
        requireArgs(e, 1);
        llvm::Value *ptr = emitExpr(*e.args[0]);
        if (ptr->getType() != ptrTy_)
            codegenError("byteLen() requires str argument");
        return emitStringByteLen(ptr);
    }

    // None() → Option<T> constructor (T derived from hint, then return type)
    if (e.callee == "None") {
        if (!e.args.empty())
            codegenError("None() takes no arguments");
        // Prefer branch-merge hint (#1154), then enclosing function return type.
        if (option_none_hint_inner_)
            return buildNoneValue(getOptionType(option_none_hint_inner_));
        llvm::Type *innerTy = i8Ty_;
        if (fn_) {
            llvm::Type *retTy = fn_->getReturnType();
            if (isOptionType(retTy)) {
                auto *retStructTy = llvm::cast<llvm::StructType>(retTy);
                innerTy = retStructTy->getElementType(1);
            }
        }
        return buildNoneValue(getOptionType(innerTy));
    }

    // Some(x) → Option<T> constructor
    if (e.callee == "Some") {
        requireArgs(e, 1);
        llvm::Value *inner = emitExpr(*e.args[0]);
        llvm::Type *expectedInnerTy = nullptr;
        if (fn_) {
            llvm::Type *retTy = fn_->getReturnType();
            if (isOptionType(retTy)) {
                auto *retStructTy = llvm::cast<llvm::StructType>(retTy);
                expectedInnerTy = retStructTy->getElementType(1);
            }
        }
        // Unwrap any-typed value to the expected inner type when the function return type
        // is more specific (mirror of Ok emitter for #1115).
        if (expectedInnerTy && isAnyType(inner->getType()) &&
            !isAnyType(expectedInnerTy) && expectedInnerTy != i8Ty_ &&
            canAnyHoldType(expectedInnerTy))
            inner = unwrapFromAny(inner, expectedInnerTy);
        llvm::StructType *optTy = getOptionType(inner->getType());
        return buildSomeValue(inner, optTy);
    }

    // Ok(value) → Result<V, Error> constructor
    if (e.callee == "Ok") {
        requireArgs(e, 1);
        llvm::Value *inner = emitExpr(*e.args[0]);
        // Determine the error type (and expected ok type) from the enclosing function's return type
        llvm::Type *errTy = errorTy_;
        llvm::Type *expectedOkTy = nullptr;
        if (fn_) {
            llvm::Type *retTy = fn_->getReturnType();
            if (isResultType(retTy)) {
                auto *retStructTy = llvm::cast<llvm::StructType>(retTy);
                errTy = retStructTy->getElementType(2);
                expectedOkTy = retStructTy->getElementType(1);
            }
        }
        // Unwrap any-typed value to the expected ok type when the function return type
        // is more specific (e.g. unannotated param `x` emits as anyTy_ but Ok(x) in a
        // branch alongside Ok(0) must produce the same Result struct type).
        if (expectedOkTy && isAnyType(inner->getType()) &&
            !isAnyType(expectedOkTy) && expectedOkTy != i8Ty_ &&
            canAnyHoldType(expectedOkTy))
            inner = unwrapFromAny(inner, expectedOkTy);
        llvm::StructType *resTy = getResultType(inner->getType(), errTy);
        return buildOkValue(inner, resTy);
    }

    // Err(error) → Result<V, E> constructor
    if (e.callee == "Err") {
        requireArgs(e, 1);
        llvm::Value *inner = emitExpr(*e.args[0]);
        llvm::Type *okTy = i8Ty_; // default: Unit (i8 dummy)
        llvm::Type *expectedErrTy = nullptr;
        if (fn_) {
            llvm::Type *retTy = fn_->getReturnType();
            if (isResultType(retTy)) {
                auto *retStructTy = llvm::cast<llvm::StructType>(retTy);
                okTy = retStructTy->getElementType(1);
                expectedErrTy = retStructTy->getElementType(2);
                if (auto *sliced = tryEmitSubtypeCoerce(inner, expectedErrTy))
                    inner = sliced;
            }
        }
        // Unwrap any-typed value to the expected err type (mirrors Ok emitter):
        // Err(x) with x: anyTy_ (unannotated param) into a concrete primitive Err slot
        // must emit the same Result struct as Ok(concrete) in the sibling branch.
        if (expectedErrTy && isAnyType(inner->getType()) &&
            !isAnyType(expectedErrTy) && expectedErrTy != i8Ty_ &&
            canAnyHoldType(expectedErrTy))
            inner = unwrapFromAny(inner, expectedErrTy);
        llvm::StructType *resTy = getResultType(okTy, inner->getType());
        return buildErrValue(inner, resTy);
    }

    // Error("msg") / Error("msg", code) → Error struct constructor
    if (e.callee == "Error") {
        if (e.args.empty() || e.args.size() > 2)
            codegenError("Error() takes 1 or 2 arguments");
        llvm::Value *msg = emitExpr(*e.args[0]);
        if (msg->getType() != ptrTy_)
            codegenError("Error() first argument must be a string");
        llvm::Value *code;
        if (e.args.size() == 2) {
            code = emitExpr(*e.args[1]);
            if (code->getType() != i64Ty_)
                codegenError("Error() second argument must be an integer");
        } else {
            code = llvm::ConstantInt::get(i64Ty_, 0);
        }
        llvm::Value *result = llvm::UndefValue::get(errorTy_);
        result = builder_.CreateInsertValue(result, msg, 0, "err.msg");
        result = builder_.CreateInsertValue(result, code, 1, "err.code");
        return result;
    }

    // unwrap() has been removed — use when or ?? instead
    if (e.callee == "unwrap") {
        codegenError("unwrap() has been removed. Use when or ?? instead");
    }

    // hasKey(map, key) → bool
    if (e.callee == "hasKey") {
        requireArgs(e, 2);
        llvm::Value *mapPtr = emitExpr(*e.args[0]);
        if (mapPtr->getType() != ptrTy_)
            codegenError("hasKey() requires map as first argument");
        llvm::Type *keyTy = getMapKeyType(mapPtr);
        if (!keyTy)
            codegenError("hasKey() requires map as first argument");
        llvm::Value *key = emitExpr(*e.args[1]);
        if (key->getType() != keyTy)
            codegenError("hasKey() key type mismatch");
        llvm::Value *idx = emitMapKeyLookup(mapPtr, key, keyTy);
        // #2188 ([4a] = i1 result cross via emitICmpSGE; lookup already #2101).
        return emitICmpSGE(idx, emitConstInt(i64Ty_, 0), "hasKey");
    }

    // checked/saturating/wrapping arithmetic dispatch
    // Common pattern: emit args, propagate suffix metadata (#311), call handler
    auto dispatchArith = [&](auto emitFn) -> llvm::Value* {
        requireArgs(e, 2);
        llvm::Value *lhs = emitExpr(*e.args[0]);
        llvm::Value *rhs = emitExpr(*e.args[1]);
        std::string hint = getExprLowLevelSuffix(*e.args[0]);
        if (hint.empty()) hint = getExprLowLevelSuffix(*e.args[1]);
        if (!hint.empty()) {
            if (getLowLevelTypeName(lhs).empty()) getOrCreateMeta(lhs).low_level_type_name = hint;
            if (getLowLevelTypeName(rhs).empty()) getOrCreateMeta(rhs).low_level_type_name = hint;
        }
        return (this->*emitFn)(e.callee, lhs, rhs);
    };

    if (e.callee == "checkedAdd" || e.callee == "checkedSub" || e.callee == "checkedMul")
        return dispatchArith(&CodeGen::emitCheckedArithmetic);
    if (e.callee == "saturatingAdd" || e.callee == "saturatingSub" || e.callee == "saturatingMul")
        return dispatchArith(&CodeGen::emitSaturatingArithmetic);
    if (e.callee == "wrappingAdd" || e.callee == "wrappingSub" || e.callee == "wrappingMul")
        return dispatchArith(&CodeGen::emitWrappingArithmetic);

    return nullptr;
}


// ===== Header loaders =====

CodeGen::ListFields CodeGen::loadListHeader(llvm::Value *listVal, const std::string &prefix) {
    ListFields f;
    llvm::Twine p(prefix);
    f.lenPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 0, p + "_len_ptr");
    f.len = builder_.CreateLoad(i64Ty_, f.lenPtr, p + "_len");
    f.capPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 1, p + "_cap_ptr");
    f.cap = builder_.CreateLoad(i64Ty_, f.capPtr, p + "_cap");
    f.dataPtr = builder_.CreateStructGEP(listHeaderTy_, listVal, 2, p + "_data_ptr");
    f.data = builder_.CreateLoad(ptrTy_, f.dataPtr, p + "_data");
    return f;
}

CodeGen::SetFields CodeGen::loadSetHeader(llvm::Value *setVal, const std::string &prefix) {
    SetFields f;
    llvm::Twine p(prefix);
    f.lenPtr = builder_.CreateStructGEP(setHeaderTy_, setVal, 0, p + "_len_ptr");
    f.len = builder_.CreateLoad(i64Ty_, f.lenPtr, p + "_len");
    f.capPtr = builder_.CreateStructGEP(setHeaderTy_, setVal, 1, p + "_cap_ptr");
    f.cap = builder_.CreateLoad(i64Ty_, f.capPtr, p + "_cap");
    f.elemsPtr = builder_.CreateStructGEP(setHeaderTy_, setVal, 2, p + "_elems_ptr");
    f.elems = builder_.CreateLoad(ptrTy_, f.elemsPtr, p + "_elems");
    return f;
}

CodeGen::MapFields CodeGen::loadMapHeader(llvm::Value *mapVal, const std::string &prefix) {
    MapFields f;
    llvm::Twine p(prefix);
    f.lenPtr = builder_.CreateStructGEP(mapHeaderTy_, mapVal, 0, p + "_len_ptr");
    f.len = builder_.CreateLoad(i64Ty_, f.lenPtr, p + "_len");
    f.capPtr = builder_.CreateStructGEP(mapHeaderTy_, mapVal, 1, p + "_cap_ptr");
    f.cap = builder_.CreateLoad(i64Ty_, f.capPtr, p + "_cap");
    f.keysPtr = builder_.CreateStructGEP(mapHeaderTy_, mapVal, 2, p + "_keys_ptr");
    f.keys = builder_.CreateLoad(ptrTy_, f.keysPtr, p + "_keys");
    f.valsPtr = builder_.CreateStructGEP(mapHeaderTy_, mapVal, 3, p + "_vals_ptr");
    f.vals = builder_.CreateLoad(ptrTy_, f.valsPtr, p + "_vals");
    return f;
}

// ===== Header stores =====

void CodeGen::storeListHeaderFields(llvm::Value *headerPtr, llvm::Value *len,
                                    llvm::Value *cap, llvm::Value *data) {
    builder_.CreateStore(len, builder_.CreateStructGEP(listHeaderTy_, headerPtr, 0));
    builder_.CreateStore(cap, builder_.CreateStructGEP(listHeaderTy_, headerPtr, 1));
    builder_.CreateStore(data, builder_.CreateStructGEP(listHeaderTy_, headerPtr, 2));
}

void CodeGen::storeSetHeaderFields(llvm::Value *headerPtr, llvm::Value *len,
                                   llvm::Value *cap, llvm::Value *elems) {
    builder_.CreateStore(len, builder_.CreateStructGEP(setHeaderTy_, headerPtr, 0));
    builder_.CreateStore(cap, builder_.CreateStructGEP(setHeaderTy_, headerPtr, 1));
    builder_.CreateStore(elems, builder_.CreateStructGEP(setHeaderTy_, headerPtr, 2));
}

void CodeGen::storeMapHeaderFields(llvm::Value *headerPtr, llvm::Value *len,
                                   llvm::Value *cap, llvm::Value *keys,
                                   llvm::Value *vals) {
    builder_.CreateStore(len, builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 0));
    builder_.CreateStore(cap, builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 1));
    builder_.CreateStore(keys, builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 2));
    builder_.CreateStore(vals, builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 3));
}

// ===== Builtin Math =====

// ===== Math custom emitters =====

static llvm::Value *emitMathAbs(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *x = cg.emitExpr(*e.args[0]);
    if (x->getType() == cg.f64Ty_) {
        auto fn = cg.getRuntimeFn("fabs", cg.f64Ty_, {cg.f64Ty_});
        return cg.builder_.CreateCall(fn, {x}, "abs");
    }
    if (x->getType()->isIntegerTy(64)) {
        llvm::Value *intMin = llvm::ConstantInt::get(
            cg.i64Ty_, llvm::APInt::getSignedMinValue(64));
        llvm::Value *isIntMin = cg.builder_.CreateICmpEQ(x, intMin, "abs_is_imin");
        llvm::BasicBlock *errBB = cg.createBB("abs.imin_err");
        llvm::BasicBlock *okBB  = cg.createBB("abs.imin_ok");
        cg.emitBranchCond(isIntMin, errBB, okBB);
        cg.builder_.SetInsertPoint(errBB);
        cg.emitRuntimeError("runtime error: integer overflow\n",
                            ".math_abs_overflow_err_" + std::to_string(cg.overflow_err_counter_++));
        cg.builder_.SetInsertPoint(okBB);
        llvm::Value *neg = cg.builder_.CreateNeg(x, "neg");
        llvm::Value *isNeg = cg.builder_.CreateICmpSLT(x, llvm::ConstantInt::get(cg.i64Ty_, 0), "is_neg");
        return cg.builder_.CreateSelect(isNeg, neg, x, "abs");
    }
    cg.codegenError("abs() requires int or float argument");
}

static llvm::Value *emitMathFloorCeilRound(CodeGen &cg, const CallExpr &e) {
    if (e.args.size() != 1 && e.args.size() != 2)
        cg.codegenError(e.callee + "() expects 1 or 2 arguments");

    llvm::Value *x = cg.emitExpr(*e.args[0]);
    if (e.args.size() == 1 && x->getType()->isIntegerTy(64))
        return x;
    if (cg.isWideningConversion(x, cg.f64Ty_, "float"))
        x = cg.emitWideningConversion(x, cg.f64Ty_);
    if (x->getType() != cg.f64Ty_)
        cg.codegenError(e.callee + "() requires int or float argument");

    if (e.args.size() == 2) {
        // round(x * 10^digits) / 10^digits. Stays in float (no OOR check).
        llvm::Value *digits = cg.emitExpr(*e.args[1]);
        if (!digits->getType()->isIntegerTy(64))
            cg.codegenError(e.callee + "() second argument must be int");

        llvm::Value *digitsF = cg.builder_.CreateSIToFP(digits, cg.f64Ty_, "digits_f");
        auto powFn = cg.getRuntimeFn("pow", cg.f64Ty_, {cg.f64Ty_, cg.f64Ty_});
        llvm::Value *ten = llvm::ConstantFP::get(cg.f64Ty_, 10.0);
        llvm::Value *scale = cg.builder_.CreateCall(powFn, {ten, digitsF}, "scale");
        llvm::Value *scaled = cg.builder_.CreateFMul(x, scale, "scaled");
        auto roundFn = cg.getRuntimeFn(e.callee.c_str(), cg.f64Ty_, {cg.f64Ty_});
        llvm::Value *rounded = cg.builder_.CreateCall(roundFn, {scaled}, e.callee);
        llvm::Value *divided = cg.builder_.CreateFDiv(rounded, scale, e.callee + "_unscaled");

        // Guard: 10^digits overflows to +Inf when digits > 308 and underflows
        // to 0 when digits < -323, turning the multiply/divide into NaN.
        // Collapse to a sensible value in both extremes (Python-compatible):
        // scale==Inf → precision finer than double can represent → return x;
        // scale==0 → precision coarser than any finite value → rounding at
        //           a step larger than any representable magnitude. The
        //           correct limit depends on callee/sign:
        //             round: 0 for all finite x
        //             floor: -Inf if x is finite negative, else 0
        //             ceil : +Inf if x is finite positive, else 0
        //           Non-finite x (NaN / ±Inf) passes through unchanged.
        llvm::Value *zeroD = llvm::ConstantFP::get(cg.f64Ty_, 0.0);
        llvm::Value *infV  = llvm::ConstantFP::getInfinity(cg.f64Ty_);
        llvm::Value *negInfV = llvm::ConstantFP::getInfinity(cg.f64Ty_, /*Negative=*/true);
        auto fabsFn = cg.getRuntimeFn("fabs", cg.f64Ty_, {cg.f64Ty_});
        llvm::Value *xAbs = cg.builder_.CreateCall(fabsFn, {x}, "x_abs");
        llvm::Value *xIsNaN = cg.builder_.CreateFCmpUNO(x, x, "x_is_nan");
        llvm::Value *xIsInf = cg.builder_.CreateFCmpOEQ(xAbs, infV, "x_is_inf");
        llvm::Value *xIsNonFinite = cg.builder_.CreateOr(xIsNaN, xIsInf, "x_nonfinite");
        llvm::Value *scaleIsInf  = cg.builder_.CreateFCmpOEQ(scale, infV, "scale_is_inf");
        llvm::Value *scaleIsZero = cg.builder_.CreateFCmpOEQ(scale, zeroD, "scale_is_zero");

        // Base fallback: non-finite x passes through, everything else is 0.
        llvm::Value *scaleZeroVal =
            cg.builder_.CreateSelect(xIsNonFinite, x, zeroD, "scale_zero_val");

        // Callee-specific override: ceil(positive finite) → +Inf,
        // floor(negative finite) → -Inf. Both FCmp ordered comparisons
        // return false for non-finite x, so the base fallback wins there.
        if (e.callee == "floor") {
            llvm::Value *xIsNeg = cg.builder_.CreateFCmpOLT(x, zeroD, "x_is_neg");
            llvm::Value *xIsNegFinite = cg.builder_.CreateAnd(
                xIsNeg,
                cg.builder_.CreateNot(xIsNonFinite, "x_finite"),
                "x_neg_finite");
            scaleZeroVal = cg.builder_.CreateSelect(
                xIsNegFinite, negInfV, scaleZeroVal, "floor_scale_zero_val");
        } else if (e.callee == "ceil") {
            llvm::Value *xIsPos = cg.builder_.CreateFCmpOGT(x, zeroD, "x_is_pos");
            llvm::Value *xIsPosFinite = cg.builder_.CreateAnd(
                xIsPos,
                cg.builder_.CreateNot(xIsNonFinite, "x_finite"),
                "x_pos_finite");
            scaleZeroVal = cg.builder_.CreateSelect(
                xIsPosFinite, infV, scaleZeroVal, "ceil_scale_zero_val");
        }

        llvm::Value *afterZero = cg.builder_.CreateSelect(
            scaleIsZero, scaleZeroVal, divided, "scale_zero_sel");
        return cg.builder_.CreateSelect(scaleIsInf, x, afterZero, "scale_inf_sel");
    }

    auto fn = cg.getRuntimeFn(e.callee.c_str(), cg.f64Ty_, {cg.f64Ty_});
    llvm::Value *result = cg.builder_.CreateCall(fn, {x}, e.callee);
    // Route through the unified helper so the behaviour and error message
    // stay in lockstep with every other float → int site. This also accepts
    // INT64_MIN, which the previous `fabs(x) >= 2^63` guard wrongly rejected.
    return cg.emitCheckedFPToInt(result, cg.i64Ty_, "int", e.callee + "_i",
                                  e.callee + "()");
}

static llvm::Value *emitMathLog(CodeGen &cg, const CallExpr &e) {
    if (e.args.size() != 1 && e.args.size() != 2)
        cg.codegenError("log() expects 1 or 2 arguments");

    llvm::Value *x = cg.emitExpr(*e.args[0]);
    if (cg.isWideningConversion(x, cg.f64Ty_, "float"))
        x = cg.emitWideningConversion(x, cg.f64Ty_);
    if (x->getType() != cg.f64Ty_)
        cg.codegenError("log() requires int or float argument");

    auto logFn = cg.getRuntimeFn("log", cg.f64Ty_, {cg.f64Ty_});
    llvm::Value *logX = cg.builder_.CreateCall(logFn, {x}, "log");

    if (e.args.size() == 1)
        return logX;

    llvm::Value *base = cg.emitExpr(*e.args[1]);
    if (cg.isWideningConversion(base, cg.f64Ty_, "float"))
        base = cg.emitWideningConversion(base, cg.f64Ty_);
    if (base->getType() != cg.f64Ty_)
        cg.codegenError("log() base argument must be int or float");
    llvm::Value *logBase = cg.builder_.CreateCall(logFn, {base}, "log_base");
    return cg.builder_.CreateFDiv(logX, logBase, "log_div");
}

static llvm::Value *emitMathPow(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 2);
    llvm::Value *x = cg.emitExpr(*e.args[0]);
    llvm::Value *y = cg.emitExpr(*e.args[1]);

    if (x->getType() == cg.f64Ty_ && y->getType() == cg.f64Ty_) {
        auto powFn = cg.getRuntimeFn("pow", cg.f64Ty_, {cg.f64Ty_, cg.f64Ty_});
        return cg.builder_.CreateCall(powFn, {x, y}, "pow");
    }

    // (int, int): fast-exp loop. Overflow wraps silently to match Ry's
    // int arithmetic model; negative exponent raises a runtime error.
    if (x->getType()->isIntegerTy(64) && y->getType()->isIntegerTy(64)) {
        llvm::Value *zero = llvm::ConstantInt::get(cg.i64Ty_, 0);
        llvm::Value *one  = llvm::ConstantInt::get(cg.i64Ty_, 1);
        llvm::Value *isNeg = cg.builder_.CreateICmpSLT(y, zero, "pow_neg_exp_chk");

        llvm::BasicBlock *curBB = cg.builder_.GetInsertBlock();
        llvm::BasicBlock *errBB =
            cg.createBB("pow_int.err");
        llvm::BasicBlock *condBB =
            cg.createBB("pow_int.cond");
        llvm::BasicBlock *bodyBB =
            cg.createBB("pow_int.body");
        llvm::BasicBlock *endBB =
            cg.createBB("pow_int.end");
        cg.emitBranchCond(isNeg, errBB, condBB);

        cg.builder_.SetInsertPoint(errBB);
        static int powErrCounter = 0;
        cg.emitRuntimeError(
            "runtime error: pow() integer exponent must be non-negative\n",
            ".pow_int_err_" + std::to_string(powErrCounter++));

        cg.builder_.SetInsertPoint(condBB);
        llvm::PHINode *resultPhi =
            cg.createPhi(cg.i64Ty_, {}, "pow_result");
        llvm::PHINode *basePhi =
            cg.createPhi(cg.i64Ty_, {}, "pow_base");
        llvm::PHINode *expPhi =
            cg.createPhi(cg.i64Ty_, {}, "pow_exp");
        resultPhi->addIncoming(one, curBB);
        basePhi->addIncoming(x, curBB);
        expPhi->addIncoming(y, curBB);
        llvm::Value *done = cg.builder_.CreateICmpEQ(expPhi, zero, "pow_done");
        cg.emitBranchCond(done, endBB, bodyBB);

        cg.builder_.SetInsertPoint(bodyBB);
        llvm::Value *loBit = cg.builder_.CreateAnd(expPhi, one, "pow_lo_bit");
        llvm::Value *isOdd = cg.builder_.CreateICmpNE(loBit, zero, "pow_is_odd");
        llvm::Value *resultMul = cg.builder_.CreateMul(resultPhi, basePhi, "pow_result_mul");
        llvm::Value *resultNext = cg.builder_.CreateSelect(isOdd, resultMul, resultPhi, "pow_result_next");
        llvm::Value *baseSq = cg.builder_.CreateMul(basePhi, basePhi, "pow_base_sq");
        llvm::Value *expShr = cg.builder_.CreateLShr(expPhi, one, "pow_exp_shr");
        resultPhi->addIncoming(resultNext, bodyBB);
        basePhi->addIncoming(baseSq, bodyBB);
        expPhi->addIncoming(expShr, bodyBB);
        cg.emitBranchUncond(condBB);

        cg.builder_.SetInsertPoint(endBB);
        return resultPhi;
    }

    // Pass 2: mixed-type widening fallback. Either arg is an int that can
    // widen to float — coerce and dispatch through the float pow. The
    // (i64, i64) exact-match above runs first, so pow(2, 3) still returns
    // int 8; only truly mixed inputs reach here.
    bool xWiden = cg.isWideningConversion(x, cg.f64Ty_, "float");
    bool yWiden = cg.isWideningConversion(y, cg.f64Ty_, "float");
    if ((x->getType() == cg.f64Ty_ || xWiden) && (y->getType() == cg.f64Ty_ || yWiden)) {
        if (xWiden) x = cg.emitWideningConversion(x, cg.f64Ty_);
        if (yWiden) y = cg.emitWideningConversion(y, cg.f64Ty_);
        auto powFn = cg.getRuntimeFn("pow", cg.f64Ty_, {cg.f64Ty_, cg.f64Ty_});
        return cg.builder_.CreateCall(powFn, {x, y}, "pow");
    }

    cg.codegenError("pow() requires int or float arguments");
}

static llvm::Value *emitMathIsNan(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *x = cg.emitExpr(*e.args[0]);
    if (x->getType() != cg.f64Ty_)
        cg.codegenError("isNan() requires float argument");
    return cg.builder_.CreateFCmpUNO(x, x, "is_nan");
}

static llvm::Value *emitMathIsInf(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *x = cg.emitExpr(*e.args[0]);
    if (x->getType() != cg.f64Ty_)
        cg.codegenError("isInf() requires float argument");
    auto fabsFn = cg.getRuntimeFn("fabs", cg.f64Ty_, {cg.f64Ty_});
    llvm::Value *absVal = cg.builder_.CreateCall(fabsFn, {x}, "abs_for_inf");
    llvm::Value *posInf = llvm::ConstantFP::getInfinity(cg.f64Ty_);
    return cg.builder_.CreateFCmpOEQ(absVal, posInf, "is_inf");
}

static llvm::Value *emitMathDigits(CodeGen &cg, const CallExpr &e) {
    if (e.args.size() != 1 && e.args.size() != 2)
        cg.codegenError("digits() expects 1 or 2 arguments");

    llvm::Value *n = cg.emitExpr(*e.args[0]);
    if (!n->getType()->isIntegerTy(64))
        cg.codegenError("digits() requires int argument");

    llvm::Value *base;
    if (e.args.size() == 2) {
        base = cg.emitExpr(*e.args[1]);
        if (!base->getType()->isIntegerTy(64))
            cg.codegenError("digits() base must be int");
    } else {
        base = llvm::ConstantInt::get(cg.i64Ty_, 10);
    }

    llvm::Value *zero = llvm::ConstantInt::get(cg.i64Ty_, 0);
    llvm::Value *one  = llvm::ConstantInt::get(cg.i64Ty_, 1);
    llvm::Value *two  = llvm::ConstantInt::get(cg.i64Ty_, 2);

    {
        static int negCounter = 0;
        llvm::Value *isNeg = cg.builder_.CreateICmpSLT(n, zero, "digits_n_neg");
        llvm::BasicBlock *errBB = cg.createBB("digits.n_err");
        llvm::BasicBlock *okBB  = cg.createBB("digits.n_ok");
        cg.emitBranchCond(isNeg, errBB, okBB);
        cg.builder_.SetInsertPoint(errBB);
        cg.emitRuntimeError(
            "runtime error: digits() n must be non-negative, got %lld\n",
            ".digits_n_err_" + std::to_string(negCounter++),
            {n});
        cg.builder_.SetInsertPoint(okBB);
    }

    {
        static int baseCounter = 0;
        llvm::Value *isLow = cg.builder_.CreateICmpSLT(base, two, "digits_base_low");
        llvm::BasicBlock *errBB = cg.createBB("digits.base_err");
        llvm::BasicBlock *okBB  = cg.createBB("digits.base_ok");
        cg.emitBranchCond(isLow, errBB, okBB);
        cg.builder_.SetInsertPoint(errBB);
        cg.emitRuntimeError(
            "runtime error: digits() base must be >= 2, got %lld\n",
            ".digits_base_err_" + std::to_string(baseCounter++),
            {base});
        cg.builder_.SetInsertPoint(okBB);
    }

    // do-while loop ensures n=0 yields count=1 (so digits(0) == [0]).
    llvm::AllocaInst *countSlot = cg.builder_.CreateAlloca(cg.i64Ty_, nullptr, "digits_count");
    llvm::AllocaInst *qSlot     = cg.builder_.CreateAlloca(cg.i64Ty_, nullptr, "digits_q");
    cg.builder_.CreateStore(zero, countSlot);
    cg.builder_.CreateStore(n, qSlot);

    llvm::BasicBlock *countBodyBB = cg.createBB("digits.count.body");
    llvm::BasicBlock *countEndBB  = cg.createBB("digits.count.end");
    cg.emitBranchUncond(countBodyBB);

    cg.builder_.SetInsertPoint(countBodyBB);
    llvm::Value *qCur     = cg.builder_.CreateLoad(cg.i64Ty_, qSlot, "digits_q_cur");
    llvm::Value *qNext    = cg.builder_.CreateSDiv(qCur, base, "digits_q_next");
    llvm::Value *cCur     = cg.builder_.CreateLoad(cg.i64Ty_, countSlot, "digits_count_cur");
    llvm::Value *cNext    = cg.builder_.CreateAdd(cCur, one, "digits_count_next");
    cg.builder_.CreateStore(qNext, qSlot);
    cg.builder_.CreateStore(cNext, countSlot);
    llvm::Value *qDone = cg.builder_.CreateICmpEQ(qNext, zero, "digits_count_done");
    cg.emitBranchCond(qDone, countEndBB, countBodyBB);

    cg.builder_.SetInsertPoint(countEndBB);
    llvm::Value *count = cg.builder_.CreateLoad(cg.i64Ty_, countSlot, "digits_count_final");

    llvm::Value *headerPtr = cg.emitArcAllocCollectionHeader(cg.listHeaderTy_);
    auto mallocFn = cg.getStdlibMalloc();
    const llvm::DataLayout &dl = cg.mod_->getDataLayout();
    uint64_t elemSize = dl.getTypeAllocSize(cg.i64Ty_);
    llvm::Value *dataSize = cg.builder_.CreateMul(
        count, llvm::ConstantInt::get(cg.i64Ty_, elemSize), "digits_data_size");
    llvm::Value *dataPtr = cg.builder_.CreateCall(mallocFn, {dataSize}, "digits_data");

    // % and / only — never computes base^k, so input magnitude can't overflow.
    llvm::AllocaInst *iSlot = cg.builder_.CreateAlloca(cg.i64Ty_, nullptr, "digits_i");
    llvm::AllocaInst *fSlot = cg.builder_.CreateAlloca(cg.i64Ty_, nullptr, "digits_fill_q");
    cg.builder_.CreateStore(zero, iSlot);
    cg.builder_.CreateStore(n, fSlot);

    llvm::BasicBlock *fillCondBB = cg.createBB("digits.fill.cond");
    llvm::BasicBlock *fillBodyBB = cg.createBB("digits.fill.body");
    llvm::BasicBlock *fillEndBB  = cg.createBB("digits.fill.end");
    cg.emitBranchUncond(fillCondBB);

    cg.builder_.SetInsertPoint(fillCondBB);
    llvm::Value *iVal = cg.builder_.CreateLoad(cg.i64Ty_, iSlot, "digits_i_val");
    llvm::Value *cont = cg.builder_.CreateICmpSLT(iVal, count, "digits_fill_cond");
    cg.emitBranchCond(cont, fillBodyBB, fillEndBB);

    cg.builder_.SetInsertPoint(fillBodyBB);
    llvm::Value *iCur     = cg.builder_.CreateLoad(cg.i64Ty_, iSlot, "digits_i_cur");
    llvm::Value *fCur     = cg.builder_.CreateLoad(cg.i64Ty_, fSlot, "digits_fill_cur");
    llvm::Value *digit    = cg.builder_.CreateSRem(fCur, base, "digits_digit");
    llvm::Value *fNext    = cg.builder_.CreateSDiv(fCur, base, "digits_fill_next");
    llvm::Value *elemPtr  = cg.builder_.CreateGEP(cg.i64Ty_, dataPtr, {iCur}, "digits_elem_ptr");
    cg.builder_.CreateStore(digit, elemPtr);
    cg.builder_.CreateStore(fNext, fSlot);
    llvm::Value *iNext = cg.builder_.CreateAdd(iCur, one, "digits_i_next");
    cg.builder_.CreateStore(iNext, iSlot);
    cg.emitBranchUncond(fillCondBB);

    cg.builder_.SetInsertPoint(fillEndBB);

    cg.storeListHeaderFields(headerPtr, count, count, dataPtr);
    cg.setTypeMeta(CodeGen::TypeMeta::ListElem, headerPtr, cg.i64Ty_);
    return headerPtr;
}

// ===== Math dispatch table =====

static const CodeGen::NativeDispatchEntry math_table[] = {
    // 1-arg float->float (bare C library names)
    {"sqrt",  nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, nullptr, "sqrt"},
    {"log2",  nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, nullptr, "log2"},
    {"log10", nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, nullptr, "log10"},
    {"exp",   nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, nullptr, "exp"},
    {"sin",   nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, nullptr, "sin"},
    {"cos",   nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, nullptr, "cos"},
    {"tan",   nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, nullptr, "tan"},
    {"asin",  nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, nullptr, "asin"},
    {"acos",  nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, nullptr, "acos"},
    {"atan",  nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, nullptr, "atan"},
    // 2-arg float->float
    {"atan2", nullptr, CodeGen::ReturnWrapping::Direct, 2, nullptr, nullptr, "atan2"},
    {"hypot", nullptr, CodeGen::ReturnWrapping::Direct, 2, nullptr, nullptr, "hypot"},
    // Custom emitters (arity is metadata for the 1-arg legacy overload —
    // actual arity dispatch happens via registered @native sigs at the
    // custom-emitter gate in emitTableDrivenNativeCall; see codegen_call_native.cpp).
    {"abs",    nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, emitMathAbs},
    {"floor",  nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, emitMathFloorCeilRound},
    {"ceil",   nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, emitMathFloorCeilRound},
    {"round",  nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, emitMathFloorCeilRound},
    {"log",    nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, emitMathLog},
    {"pow",    nullptr, CodeGen::ReturnWrapping::Direct, 2, nullptr, emitMathPow},
    {"isNan",  nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, emitMathIsNan},
    {"isInf",  nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, emitMathIsInf},
    {"digits", nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, emitMathDigits},
};

RY_REGISTER_STDLIB_PACKAGE(math, "share/std/math/math.ry", dispatchMath)
static llvm::Value *dispatchMath(CodeGen &cg, const CallExpr &e) {
    return cg.emitTableDrivenNativeCall(e, "math", math_table, std::size(math_table));
}

// Math constants self-registration
namespace {
struct MathConstReg {
    MathConstReg() {
        auto &r = StdlibRegistry::instance();
        r.registerConstant("PI",  {NativeConstantKind::Value, 3.141592653589793});
        r.registerConstant("E",   {NativeConstantKind::Value, 2.718281828459045});
        r.registerConstant("INF", {NativeConstantKind::Infinity, 0.0});
        r.registerConstant("NAN", {NativeConstantKind::NaN, 0.0});
    }
} math_const_reg;
} // namespace

} // namespace ry
