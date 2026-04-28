#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"
#include "ry/diagnostic.hpp"


namespace ry {

// ===== Builtin Conversion =====

llvm::Value *CodeGen::emitBuiltinConversion(const CallExpr &e) {
    // to_int(s) → Result<int, Error> — fall through for JsonValue to let JSON dispatcher handle it
    if (e.callee == "to_int") {
        requireArgs(e, 1);
        llvm::Value *s = emitExpr(*e.args[0]);
        if (isJsonValue(s)) return nullptr;
        if (s->getType() != ptrTy_)
            codegenError("to_int() requires str argument");
        llvm::AllocaInst *outSlot = builder_.CreateAlloca(i64Ty_, nullptr, "to_int_out");
        auto fnTy = fnTy_ptr_ptr_to_i64_;
        auto fn = mod_->getOrInsertFunction("__ry_str_to_int", fnTy);
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

    // to_float(s) → Result<float, Error> — fall through for JsonValue to let JSON dispatcher handle it
    if (e.callee == "to_float") {
        requireArgs(e, 1);
        llvm::Value *s = emitExpr(*e.args[0]);
        if (isJsonValue(s)) return nullptr;
        if (s->getType() != ptrTy_)
            codegenError("to_float() requires str argument");
        llvm::AllocaInst *outSlot = builder_.CreateAlloca(f64Ty_, nullptr, "to_float_out");
        auto fn = mod_->getOrInsertFunction("__ry_str_to_float", fnTy_ptr_ptr_to_i64_);
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

    // to_str(v) → str — fall through for JsonValue to let JSON dispatcher handle it
    if (e.callee == "to_str") {
        requireArgs(e, 1);
        llvm::Value *v = emitExpr(*e.args[0]);
        if (isJsonValue(v)) return nullptr;
        return valueToString(v);
    }

    return nullptr;
}

// ===== type_of builtin =====

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
    if (e.callee == "type_of") {
        return emitTypeOf(e);
    }
    // ===== keys(map) — fall through for JsonValue =====
    if (e.callee == "keys") {
        requireArgs(e, 1);
        llvm::Value *mapVal = emitExpr(*e.args[0]);
        if (isJsonValue(mapVal)) return nullptr;
        llvm::Type *keyTy = getMapKeyType(mapVal);
        if (!keyTy) codegenError("keys() requires a map");

        auto mf = loadMapHeader(mapVal, "keys");
        llvm::Value *mapLen = mf.len;
        llvm::Value *keysData = mf.keys;

        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);
        uint64_t elemSize = dl.getTypeAllocSize(keyTy);
        llvm::Value *dataSize = builder_.CreateMul(mapLen, llvm::ConstantInt::get(i64Ty_, elemSize), "keys_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "keys_nd");
        auto memcpyFn = getStdlibMemcpy();
        builder_.CreateCall(memcpyFn, {newData, keysData, dataSize});

        storeListHeaderFields(newHeader, mapLen, mapLen, newData);
        setTypeMeta(TypeMeta::ListElem, newHeader, keyTy);
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

        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);
        uint64_t elemSize = dl.getTypeAllocSize(valTy);
        llvm::Value *dataSize = builder_.CreateMul(mapLen, llvm::ConstantInt::get(i64Ty_, elemSize), "vals_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "vals_nd");
        auto memcpyFn = getStdlibMemcpy();
        builder_.CreateCall(memcpyFn, {newData, valsData, dataSize});

        storeListHeaderFields(newHeader, mapLen, mapLen, newData);
        setTypeMeta(TypeMeta::ListElem, newHeader, valTy);
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

        llvm::Value *isEmptyF = builder_.CreateICmpEQ(srcLen, llvm::ConstantInt::get(i64Ty_, 0), "first_empty");
        llvm::BasicBlock *emptyBB = llvm::BasicBlock::Create(*ctx_, "first.empty", fn_);
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "first.ok", fn_);
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "first.merge", fn_);
        builder_.CreateCondBr(isEmptyF, emptyBB, okBB);

        builder_.SetInsertPoint(emptyBB);
        llvm::Value *noneVal = buildNoneValue(optTy);
        builder_.CreateBr(mergeBB);
        llvm::BasicBlock *emptyEndBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(okBB);
        llvm::Value *firstVal = builder_.CreateLoad(elemTy, srcData, "first_val");
        llvm::Value *someVal = buildSomeValue(firstVal, optTy);
        builder_.CreateBr(mergeBB);
        llvm::BasicBlock *okEndBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = builder_.CreatePHI(optTy, 2, "first_result");
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

        llvm::Value *isEmptyL = builder_.CreateICmpEQ(srcLen, llvm::ConstantInt::get(i64Ty_, 0), "last_empty");
        llvm::BasicBlock *emptyBBL = llvm::BasicBlock::Create(*ctx_, "last.empty", fn_);
        llvm::BasicBlock *okBBL = llvm::BasicBlock::Create(*ctx_, "last.ok", fn_);
        llvm::BasicBlock *mergeBBL = llvm::BasicBlock::Create(*ctx_, "last.merge", fn_);
        builder_.CreateCondBr(isEmptyL, emptyBBL, okBBL);

        builder_.SetInsertPoint(emptyBBL);
        llvm::Value *noneValL = buildNoneValue(optTy);
        builder_.CreateBr(mergeBBL);
        llvm::BasicBlock *emptyEndBBL = builder_.GetInsertBlock();

        builder_.SetInsertPoint(okBBL);
        llvm::Value *lastIdx = builder_.CreateSub(srcLen, llvm::ConstantInt::get(i64Ty_, 1), "last_idx");
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {lastIdx}, "last_ep");
        llvm::Value *lastVal = builder_.CreateLoad(elemTy, elemPtr, "last_val");
        llvm::Value *someValL = buildSomeValue(lastVal, optTy);
        builder_.CreateBr(mergeBBL);
        llvm::BasicBlock *okEndBBL = builder_.GetInsertBlock();

        builder_.SetInsertPoint(mergeBBL);
        llvm::PHINode *phiL = builder_.CreatePHI(optTy, 2, "last_result");
        phiL->addIncoming(noneValL, emptyEndBBL);
        phiL->addIncoming(someValL, okEndBBL);
        return phiL;
    }

    // ===== is_empty(list/map/set/str) =====
    if (e.callee == "is_empty") {
        requireArgs(e, 1);
        llvm::Value *val = emitExpr(*e.args[0]);
        llvm::Type *headerTy = nullptr;
        if (getListElementType(val)) headerTy = listHeaderTy_;
        else if (getMapKeyType(val)) headerTy = mapHeaderTy_;
        else if (getSetElementType(val)) headerTy = setHeaderTy_;
        if (headerTy) {
            llvm::Value *len = builder_.CreateLoad(i64Ty_, builder_.CreateStructGEP(headerTy, val, 0), "ie_len");
            return builder_.CreateICmpEQ(len, llvm::ConstantInt::get(i64Ty_, 0), "is_empty");
        }
        // String (#831, #1022, #1069): read byte_len from the StringHeader instead of
        // peeking the first data byte — embedded NUL bytes are valid string content
        // (tracked by byte_len since #1022) and must not be mistaken for an empty
        // string. emitStringByteLen is also O(1) (a single i64 load from handle - 8).
        if (val->getType() == ptrTy_) {
            llvm::Value *len = emitStringByteLen(val);
            return builder_.CreateICmpEQ(len, llvm::ConstantInt::get(i64Ty_, 0), "is_empty");
        }
        codegenError("is_empty() requires a collection (list, map, set) or str");
    }

    // ===== enumerate(list) =====
    if (e.callee == "enumerate") {
        requireArgs(e, 1);
        llvm::Value *listVal = emitExpr(*e.args[0]);
        llvm::Type *elemTy = getListElementType(listVal);
        // String fallback (#746, #827): enumerate over a str yields
        // `(int, str)` pairs per UTF-8 code point.
        if (!elemTy && isStringValue(listVal)) {
            listVal = emitStringToCharList(listVal, "enum_str_chars");
            elemTy = ptrTy_;
        }
        if (!elemTy) codegenError("enumerate() requires a list or str");

        // Snapshot the source list's element name so we can rebuild a tuple
        // type string "(int, <elem>)" for the result (#813). See
        // snapshotListElemName for the fallback rules.
        std::string srcElemName = snapshotListElemName(listVal, elemTy);

        auto lf = loadListHeader(listVal, "enum");
        llvm::Value *srcLen = lf.len;
        llvm::Value *srcData = lf.data;

        llvm::StructType *tupleTy = llvm::StructType::get(*ctx_, {i64Ty_, elemTy});
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);
        uint64_t tupleSize = dl.getTypeAllocSize(tupleTy);
        llvm::Value *dataSize = builder_.CreateMul(srcLen, llvm::ConstantInt::get(i64Ty_, tupleSize), "enum_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "enum_nd");

        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "enum_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "enum.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "enum.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "enum.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "ei");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, srcLen), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, srcData, {i}, "enum_ep");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "enum_elem");
        llvm::Value *tupleVal = llvm::UndefValue::get(tupleTy);
        tupleVal = builder_.CreateInsertValue(tupleVal, i, 0);
        tupleVal = builder_.CreateInsertValue(tupleVal, elem, 1);
        llvm::Value *dstPtr = builder_.CreateGEP(tupleTy, newData, {i}, "enum_dp");
        builder_.CreateStore(tupleVal, dstPtr);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(endBB);

        storeListHeaderFields(newHeader, srcLen, srcLen, newData);
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
            if (!ty && isStringValue(val)) {
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
        llvm::Value *len1 = lf1.len;
        llvm::Value *len2 = lf2.len;
        llvm::Value *data1 = lf1.data;
        llvm::Value *data2 = lf2.data;

        llvm::Value *minLen = builder_.CreateSelect(builder_.CreateICmpSLT(len1, len2), len1, len2, "zip_minlen");

        llvm::StructType *tupleTy = llvm::StructType::get(*ctx_, {elemTy1, elemTy2});
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);
        uint64_t tupleSize = dl.getTypeAllocSize(tupleTy);
        llvm::Value *dataSize = builder_.CreateMul(minLen, llvm::ConstantInt::get(i64Ty_, tupleSize), "zip_ds");
        llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "zip_nd");

        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "zip_i");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "zip.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "zip.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "zip.end", fn_);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *i = builder_.CreateLoad(i64Ty_, iVar, "zi");
        builder_.CreateCondBr(builder_.CreateICmpSLT(i, minLen), bodyBB, endBB);
        builder_.SetInsertPoint(bodyBB);
        llvm::Value *ep1 = builder_.CreateGEP(elemTy1, data1, {i}, "zip_ep1");
        llvm::Value *ep2 = builder_.CreateGEP(elemTy2, data2, {i}, "zip_ep2");
        llvm::Value *e1 = builder_.CreateLoad(elemTy1, ep1, "zip_e1");
        llvm::Value *e2 = builder_.CreateLoad(elemTy2, ep2, "zip_e2");
        llvm::Value *tupleVal = llvm::UndefValue::get(tupleTy);
        tupleVal = builder_.CreateInsertValue(tupleVal, e1, 0);
        tupleVal = builder_.CreateInsertValue(tupleVal, e2, 1);
        llvm::Value *dstPtr = builder_.CreateGEP(tupleTy, newData, {i}, "zip_dp");
        builder_.CreateStore(tupleVal, dstPtr);
        builder_.CreateStore(builder_.CreateAdd(i, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(endBB);

        storeListHeaderFields(newHeader, minLen, minLen, newData);
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
        llvm::FunctionType *countTy = llvm::FunctionType::get(i32Ty_, false);
        llvm::FunctionCallee countFn = mod_->getOrInsertFunction("__ry_args_count", countTy);
        llvm::Value *count32 = builder_.CreateCall(countFn, {}, "argc");
        llvm::Value *count = builder_.CreateSExt(count32, i64Ty_, "argc64");

        // Allocate list header
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        llvm::Value *headerPtr = emitArcAllocCollectionHeader(listHeaderTy_);

        // Allocate data array (ptr per element)
        uint64_t elemSize = dl.getTypeAllocSize(ptrTy_);
        llvm::Value *dataSize = builder_.CreateMul(count, llvm::ConstantInt::get(i64Ty_, elemSize), "args_data_size");
        llvm::Value *dataPtr = builder_.CreateCall(mallocFn, {dataSize}, "args_data");

        // Loop: for i in 0..count, get arg pointer
        llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
        llvm::Value *one = llvm::ConstantInt::get(i64Ty_, 1);
        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "args_i");
        builder_.CreateStore(zero, iVar);

        // __ry_args_get function type
        llvm::FunctionType *getTy = llvm::FunctionType::get(ptrTy_, {i32Ty_}, false);
        llvm::FunctionCallee getFn = mod_->getOrInsertFunction("__ry_args_get", getTy);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "args.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "args.body", fn_);
        llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, "args.end", fn_);

        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "ai");
        llvm::Value *cond = builder_.CreateICmpSLT(iVal, count, "args_cond");
        builder_.CreateCondBr(cond, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "ai_cur");
        llvm::Value *iCur32 = builder_.CreateTrunc(iCur, i32Ty_, "ai_cur32");
        llvm::Value *argStr = builder_.CreateCall(getFn, {iCur32}, "arg_str");
        llvm::Value *elemPtr = builder_.CreateGEP(ptrTy_, dataPtr, {iCur}, "args_elem_ptr");
        builder_.CreateStore(argStr, elemPtr);
        llvm::Value *iNext = builder_.CreateAdd(iCur, one, "ai_next");
        builder_.CreateStore(iNext, iVar);
        builder_.CreateBr(condBB);

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

        llvm::FunctionType *fnTy = llvm::FunctionType::get(i64Ty_, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction("__ry_available_parallelism", fnTy);
        return builder_.CreateCall(fn, {}, "availableParallelism");
    }

    // block_on(task) -> T — block the current thread until the Task<T> completes
    if (e.callee == "block_on") {
        requireArgs(e, 1);
        return emitTaskWait(emitExpr(*e.args[0]), "__ry_block_on", "block_on");
    }

    // sleep(duration_ms) -> Unit
    if (e.callee == "sleep") {
        requireArgs(e, 1);
        llvm::Value *duration = emitExpr(*e.args[0]);
        if (duration->getType() != i64Ty_)
            codegenError("sleep() requires int argument");

        llvm::FunctionType *fnTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {i64Ty_}, false);
        llvm::FunctionCallee fn = mod_->getOrInsertFunction("__ry_sleep", fnTy);
        return builder_.CreateCall(fn, {duration});
    }

    if (e.callee == "input") {
        if (e.args.size() > 1)
            codegenError("input() takes 0 or 1 arguments");
        // Runtime lives in libry_io — without this insert the JIT fails to
        // resolve __ry_read_line / __ry_input_prompt for programs that
        // never `import` from io. Bare builtins are not declared as
        // @native("io"), so library registration must happen here.
        used_native_libraries_.insert("io");
        if (e.args.size() == 1) {
            llvm::Value *prompt = emitExpr(*e.args[0]);
            if (prompt->getType() != ptrTy_)
                codegenError("input() prompt must be str");
            auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
            auto fn = mod_->getOrInsertFunction("__ry_input_prompt", fnTy);
            return builder_.CreateCall(fn, {prompt}, "input_result");
        }
        auto fnTy = llvm::FunctionType::get(ptrTy_, false);
        auto fn = mod_->getOrInsertFunction("__ry_read_line", fnTy);
        return builder_.CreateCall(fn, {}, "input_result");
    }

    if (e.callee == "env") {
        if (e.args.empty() || e.args.size() > 2)
            codegenError("env() takes 1 or 2 arguments");
        llvm::Value *key = emitExpr(*e.args[0]);
        if (key->getType() != ptrTy_)
            codegenError("env() key must be str");

        // __ry_env_get wraps getenv() result in a StringHeader-managed handle
        // so that byte_len / length / etc. work correctly on the returned str.
        auto envGetTy = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
        auto envGetFn = mod_->getOrInsertFunction("__ry_env_get", envGetTy);
        llvm::Value *result = builder_.CreateCall(envGetFn, {key}, "env_result");
        llvm::Value *isNull = builder_.CreateICmpEQ(result,
            llvm::ConstantPointerNull::get(
                llvm::cast<llvm::PointerType>(ptrTy_)), "env_null");

        if (e.args.size() == 1) {
            return wrapPtrAsOption(result, "env");
        } else {
            llvm::Value *def = emitExpr(*e.args[1]);
            if (def->getType() != ptrTy_)
                codegenError("env() default must be str");
            return builder_.CreateSelect(isNull, def, result, "env_val");
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
        auto fnTy = llvm::FunctionType::get(i64Ty_, {ptrTy_, ptrTy_}, false);
        std::string rtFn = isTlsStream(firstArg) ? "__ry_tls_send" : "__ry_tcp_send";
        auto fn = mod_->getOrInsertFunction(rtFn, fnTy);
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
        auto fnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, i64Ty_}, false);
        std::string rtFn = isTlsStream(streamVal) ? "__ry_tls_receive" : "__ry_tcp_receive";
        auto fn = mod_->getOrInsertFunction(rtFn, fnTy);
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
        codegenError("close() requires TcpStream, TlsStream, or TcpListener argument");
    }

    // range(n), range(start, end), or range(start, end, step) → List<int>
    if (e.callee == "range") {
        if (e.args.size() < 1 || e.args.size() > 3)
            codegenError("range() takes 1, 2, or 3 arguments");

        llvm::Value *start, *end, *step;
        llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
        llvm::Value *one = llvm::ConstantInt::get(i64Ty_, 1);

        if (e.args.size() == 1) {
            start = zero;
            end = emitExpr(*e.args[0]);
            step = one;
        } else if (e.args.size() == 2) {
            start = emitExpr(*e.args[0]);
            end = emitExpr(*e.args[1]);
            step = one;
        } else {
            start = emitExpr(*e.args[0]);
            end = emitExpr(*e.args[1]);
            step = emitExpr(*e.args[2]);
        }

        // Runtime check: step == 0 → error
        if (e.args.size() == 3) {
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
        auto mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        llvm::Value *headerPtr = emitArcAllocCollectionHeader(listHeaderTy_);

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
        storeListHeaderFields(headerPtr, count, count, dataPtr);

        setTypeMeta(TypeMeta::ListElem, headerPtr, i64Ty_);
        return headerPtr;
    }

    // len(xs) → list/map/array/set/str length — fall through for JsonValue
    if (e.callee == "len") {
        requireArgs(e, 1);
        llvm::Value *ptr = emitExpr(*e.args[0]);
        if (isJsonValue(ptr)) return nullptr;
        // Fixed-length array: return compile-time constant
        if (auto *ai = llvm::dyn_cast<llvm::AllocaInst>(ptr)) {
            if (auto *arrTy = llvm::dyn_cast<llvm::ArrayType>(ai->getAllocatedType()))
                return llvm::ConstantInt::get(i64Ty_, arrTy->getNumElements());
        }
        if (ptr->getType() != ptrTy_)
            codegenError("len() requires list, map, set, array, or str argument");
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
        auto utf8LenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_, i64Ty_}, false);
        auto utf8LenFn = mod_->getOrInsertFunction("__ry_utf8_len_n", utf8LenTy);
        return builder_.CreateCall(utf8LenFn, {ptr, byteLen}, "str_len");
    }

    // byte_len(str) → int (byte length — NUL-safe, reads from StringHeader)
    if (e.callee == "byte_len") {
        requireArgs(e, 1);
        llvm::Value *ptr = emitExpr(*e.args[0]);
        if (ptr->getType() != ptrTy_)
            codegenError("byte_len() requires str argument");
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

    // has_key(map, key) → bool
    if (e.callee == "has_key") {
        requireArgs(e, 2);
        llvm::Value *mapPtr = emitExpr(*e.args[0]);
        if (mapPtr->getType() != ptrTy_)
            codegenError("has_key() requires map as first argument");
        llvm::Type *keyTy = getMapKeyType(mapPtr);
        if (!keyTy)
            codegenError("has_key() requires map as first argument");
        llvm::Value *key = emitExpr(*e.args[1]);
        if (key->getType() != keyTy)
            codegenError("has_key() key type mismatch");
        llvm::Value *idx = emitMapKeyLookup(mapPtr, key, keyTy);
        return builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "has_key");
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

    if (e.callee == "checked_add" || e.callee == "checked_sub" || e.callee == "checked_mul")
        return dispatchArith(&CodeGen::emitCheckedArithmetic);
    if (e.callee == "saturating_add" || e.callee == "saturating_sub" || e.callee == "saturating_mul")
        return dispatchArith(&CodeGen::emitSaturatingArithmetic);
    if (e.callee == "wrapping_add" || e.callee == "wrapping_sub" || e.callee == "wrapping_mul")
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
            llvm::BasicBlock::Create(*cg.ctx_, "pow_int.err", cg.fn_);
        llvm::BasicBlock *condBB =
            llvm::BasicBlock::Create(*cg.ctx_, "pow_int.cond", cg.fn_);
        llvm::BasicBlock *bodyBB =
            llvm::BasicBlock::Create(*cg.ctx_, "pow_int.body", cg.fn_);
        llvm::BasicBlock *endBB =
            llvm::BasicBlock::Create(*cg.ctx_, "pow_int.end", cg.fn_);
        cg.builder_.CreateCondBr(isNeg, errBB, condBB);

        cg.builder_.SetInsertPoint(errBB);
        static int powErrCounter = 0;
        cg.emitRuntimeError(
            "runtime error: pow() integer exponent must be non-negative\n",
            ".pow_int_err_" + std::to_string(powErrCounter++));

        cg.builder_.SetInsertPoint(condBB);
        llvm::PHINode *resultPhi =
            cg.builder_.CreatePHI(cg.i64Ty_, 2, "pow_result");
        llvm::PHINode *basePhi =
            cg.builder_.CreatePHI(cg.i64Ty_, 2, "pow_base");
        llvm::PHINode *expPhi =
            cg.builder_.CreatePHI(cg.i64Ty_, 2, "pow_exp");
        resultPhi->addIncoming(one, curBB);
        basePhi->addIncoming(x, curBB);
        expPhi->addIncoming(y, curBB);
        llvm::Value *done = cg.builder_.CreateICmpEQ(expPhi, zero, "pow_done");
        cg.builder_.CreateCondBr(done, endBB, bodyBB);

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
        cg.builder_.CreateBr(condBB);

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
        cg.codegenError("is_nan() requires float argument");
    return cg.builder_.CreateFCmpUNO(x, x, "is_nan");
}

static llvm::Value *emitMathIsInf(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *x = cg.emitExpr(*e.args[0]);
    if (x->getType() != cg.f64Ty_)
        cg.codegenError("is_inf() requires float argument");
    auto fabsFn = cg.getRuntimeFn("fabs", cg.f64Ty_, {cg.f64Ty_});
    llvm::Value *absVal = cg.builder_.CreateCall(fabsFn, {x}, "abs_for_inf");
    llvm::Value *posInf = llvm::ConstantFP::getInfinity(cg.f64Ty_);
    return cg.builder_.CreateFCmpOEQ(absVal, posInf, "is_inf");
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
    {"is_nan", nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, emitMathIsNan},
    {"is_inf", nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, emitMathIsInf},
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
        r.registerConstant("Inf", {NativeConstantKind::Infinity, 0.0});
        r.registerConstant("NaN", {NativeConstantKind::NaN, 0.0});
    }
} math_const_reg;
} // namespace

} // namespace ry
