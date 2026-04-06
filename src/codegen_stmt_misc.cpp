#include "ry/codegen.hpp"

void CodeGen::emitStmt(FieldAssignStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    emitCoverage(s.loc);
    // Get the variable name from the object expression
    auto *varExpr = std::get_if<VariableExpr>(&s.object->data);
    if (!varExpr)
        codegenError("field assignment requires variable on left side");

    llvm::AllocaInst *ptr = findVar(varExpr->name);
    if (!ptr)
        codegenError("undefined variable: " + varExpr->name);

    if (isImmutable(varExpr->name))
        codegenError("cannot modify field of @const variable: " + varExpr->name);

    llvm::Type *varTy = ptr->getAllocatedType();
    llvm::StructType *structTy = llvm::dyn_cast<llvm::StructType>(varTy);
    if (!structTy)
        codegenError("field assignment on non-struct type");

    std::string typeName = structTy->getName().str();
    auto it = struct_types_.find(typeName);
    if (it == struct_types_.end())
        codegenError("unknown struct type: " + typeName);

    const auto &info = it->second;
    int fieldIdx = -1;
    for (unsigned i = 0; i < info.fields.size(); ++i) {
        if (info.fields[i].name == s.field) {
            fieldIdx = static_cast<int>(i);
            break;
        }
    }
    if (fieldIdx < 0)
        codegenError("type '" + typeName + "' has no field '" + s.field + "'");

    llvm::Value *newVal = emitExpr(*s.value);
    llvm::Type *expectedTy = structTy->getElementType(fieldIdx);
    if (newVal->getType() != expectedTy) {
        if (auto *sliced = tryEmitSubtypeCoerce(newVal, expectedTy))
            newVal = sliced;
        else
            codegenError("field '" + s.field + "' type mismatch");
    }

    // Load current struct value, insert new field value, store back
    llvm::Value *current = builder_.CreateLoad(varTy, ptr, "struct_cur");
    llvm::Value *updated = builder_.CreateInsertValue(current, newVal, fieldIdx, "struct_upd");
    builder_.CreateStore(updated, ptr);

    emitInvariantCheck(typeName, info, updated);
}

void CodeGen::emitStmt(EnumStmt &s) {
    emitTraceSymbolDefine("enum", s.name, s.loc);
    // Generic enum: save as template, don't instantiate yet
    if (!s.type_params.empty()) {
        GenericEnumTemplate tmpl;
        tmpl.name = s.name;
        tmpl.typeParams = s.type_params;
        tmpl.variants = std::move(s.variants);
        generic_enum_templates_[s.name] = std::move(tmpl);
        return;
    }

    if (enum_types_.count(s.name))
        codegenError("enum '" + s.name + "' is already defined");

    EnumInfo info;
    info.name = s.name;
    info.variantCount = s.variants.size();

    // Check if any variant has associated data
    bool hasADT = false;
    for (auto &v : s.variants) {
        if (!v.field_types.empty()) { hasADT = true; break; }
    }
    info.isADT = hasADT;

    // Check if any variant has explicit values
    bool hasExplicit = false;
    for (auto &v : s.variants) {
        if (v.explicit_value.has_value()) { hasExplicit = true; break; }
    }
    info.hasExplicitValues = hasExplicit;

    // Create global string array for variant names (for printing)
    std::vector<llvm::Constant*> nameStrings;
    nameStrings.reserve(s.variants.size());
    info.variantOrder.reserve(s.variants.size());
    std::unordered_set<int64_t> seenValues;
    for (size_t i = 0; i < s.variants.size(); ++i) {
        int64_t val = s.variants[i].explicit_value.value_or(static_cast<int64_t>(i));
        if (!seenValues.insert(val).second)
            codegenError("duplicate enum value " + std::to_string(val) + " in enum '" + s.name + "'");
        info.variants[s.variants[i].name] = val;
        info.variantOrder.push_back(s.variants[i].name);
        llvm::Constant *str = cachedGlobalString(
            s.variants[i].name, ".enum_" + s.name + "_" + s.variants[i].name);
        nameStrings.push_back(str);

        // Resolve field types for ADT variants
        if (!s.variants[i].field_types.empty()) {
            VariantFieldInfo vfi;
            for (auto &ft : s.variants[i].field_types) {
                std::string ftStr = ft->toString();
                vfi.fieldTypes.push_back(resolveType(ftStr));
                vfi.fieldTypeNames.push_back(ftStr);
            }
            info.variantFields[s.variants[i].name] = std::move(vfi);
        }
    }

    // Create global array of name pointers
    auto *arrTy = llvm::ArrayType::get(ptrTy_, s.variants.size());
    auto *init = llvm::ConstantArray::get(arrTy, nameStrings);
    auto *gv = new llvm::GlobalVariable(
        *mod_, arrTy, true, llvm::GlobalValue::PrivateLinkage,
        init, ".enum_names_" + s.name);
    info.nameArray = gv;

    // For ADT enums, create a struct type: { i64 tag, [maxPayloadSize x i8] }
    if (hasADT) {
        const llvm::DataLayout &dl = mod_->getDataLayout();
        size_t maxPayload = 0;
        for (auto &[vname, vfi] : info.variantFields) {
            size_t payloadSize = 0;
            for (auto *ty : vfi.fieldTypes) {
                uint64_t align = dl.getABITypeAlign(ty).value();
                payloadSize = (payloadSize + align - 1) / align * align;
                payloadSize += dl.getTypeAllocSize(ty);
            }
            if (payloadSize > maxPayload) maxPayload = payloadSize;
        }
        info.maxPayloadSize = maxPayload;
        llvm::Type *payloadTy = llvm::ArrayType::get(
            llvm::Type::getInt8Ty(*ctx_), maxPayload > 0 ? maxPayload : 1);
        info.adtType = llvm::StructType::create(
            *ctx_, {i64Ty_, payloadTy}, "enum." + s.name);
    }

    enum_types_[s.name] = std::move(info);
}

void CodeGen::emitStmt(TupleDestructStmt &s) {
    emitCoverage(s.loc);
    llvm::Value *tupleVal = emitExpr(*s.value);
    llvm::StructType *structTy = llvm::dyn_cast<llvm::StructType>(tupleVal->getType());
    if (!structTy)
        codegenError("tuple destructuring requires a tuple value");
    if (structTy->getNumElements() != s.names.size())
        codegenError("tuple destructuring: expected " +
            std::to_string(s.names.size()) + " elements but got " +
            std::to_string(structTy->getNumElements()));

    for (size_t i = 0; i < s.names.size(); ++i) {
        if (s.names[i] == "_")
            continue;
        // Redeclaration check (consistent with emitVarDecl)
        if (scope_stack_.back().count(s.names[i]))
            codegenError("variable '" + s.names[i] + "' already declared in this scope");
        llvm::Value *elem = builder_.CreateExtractValue(tupleVal, i);
        llvm::AllocaInst *ptr = getOrCreateVar(s.names[i], elem->getType());
        builder_.CreateStore(elem, ptr);
        if (s.is_immutable)
            immutable_scope_stack_.back().insert(s.names[i]);
    }
}

void CodeGen::emitStmt(std::unique_ptr<IfStmt> &s) {
    emitCoverage(s->loc);
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "if.end", fn_);
    llvm::Value *cond = emitExpr(*s->branch.condition);
    cond = toBool(cond);
    emitTraceIfBranch(cond, s->loc);

    llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(*ctx_, "if.then", fn_);
    llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(*ctx_, "if.else", fn_);
    builder_.CreateCondBr(cond, thenBB, elseBB);

    builder_.SetInsertPoint(thenBB);
    pushScope();
    for (auto &stmt : s->branch.body)
        std::visit([this](auto &st) { emitStmt(st); }, stmt);
    popScope();
    if (!builder_.GetInsertBlock()->getTerminator())
        builder_.CreateBr(mergeBB);

    builder_.SetInsertPoint(elseBB);

    if (!s->else_body.empty()) {
        pushScope();
        for (auto &stmt : s->else_body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);
        popScope();
    }
    if (!builder_.GetInsertBlock()->getTerminator())
        builder_.CreateBr(mergeBB);

    builder_.SetInsertPoint(mergeBB);
}

void CodeGen::emitStmt(std::unique_ptr<WhenCondStmt> &s) {
    emitCoverage(s->loc);
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "when.end", fn_);
    int armIndex = 0;

    for (auto &arm : s->arms) {
        llvm::Value *cond = emitExpr(*arm.condition);
        cond = toBool(cond);

        llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(*ctx_, "when.then", fn_);
        llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "when.next", fn_);
        builder_.CreateCondBr(cond, thenBB, nextBB);

        builder_.SetInsertPoint(thenBB);
        emitTraceWhenBranch(armIndex++, s->loc);
        pushScope();
        for (auto &stmt : arm.body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);
        popScope();
        if (!builder_.GetInsertBlock()->getTerminator())
            builder_.CreateBr(mergeBB);

        builder_.SetInsertPoint(nextBB);
    }

    if (!s->else_body.empty()) {
        pushScope();
        emitTraceWhenBranch(-1, s->loc);
        for (auto &stmt : s->else_body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);
        popScope();
    }
    if (!builder_.GetInsertBlock()->getTerminator())
        builder_.CreateBr(mergeBB);

    builder_.SetInsertPoint(mergeBB);
}


void CodeGen::emitStmt(ImportStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    codegenError("unresolved import: " + s.module_path +
                             " (ModuleLoader should have resolved this)");
}

void CodeGen::emitStmt(IndexAssignStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    emitCoverage(s.loc);
    llvm::AllocaInst *receiverAlloca = tryGetReceiverAlloca(*s.object);
    llvm::Value *objPtr = emitExpr(*s.object);

    llvm::SmallVector<llvm::Value*, 2> indexValues;
    for (auto &idx : s.indices)
        indexValues.push_back(emitExpr(*idx));
    llvm::Value *val = emitExpr(*s.value);

    if (trySubscriptAssignOperatorCall(objPtr, indexValues, val))
        return;
    if (indexValues.size() > 1)
        codegenError("multi-index requires operator[]= overload");

    llvm::Value *key = indexValues[0];

    // Fixed-length array index assignment
    if (auto *ai = llvm::dyn_cast<llvm::AllocaInst>(objPtr)) {
        if (auto *arrTy = llvm::dyn_cast<llvm::ArrayType>(ai->getAllocatedType())) {
            llvm::Type *elemTy = arrTy->getElementType();
            uint64_t arrSize = arrTy->getNumElements();

            emitBoundsCheck(key, llvm::ConstantInt::get(i64Ty_, arrSize),
                            "runtime error: index %lld out of bounds for array of length %lld\n", ".arr_assign_err", "arr_assign");

            if (val->getType() != elemTy) {
                auto nit = array_elem_type_names_.find(ai);
                std::string tn = (nit != array_elem_type_names_.end()) ? nit->second : "i32";
                llvm::Value *coerced = coerceToLowLevelType(
                    val, elemTy, tn, "", "arr_assign_trunc");
                if (coerced) {
                    val = coerced;
                } else {
                    codegenError("array element type mismatch in index assignment");
                }
            }

            llvm::Value *elemPtr = builder_.CreateGEP(
                arrTy, ai, {llvm::ConstantInt::get(i64Ty_, 0), key}, "arr_assign_ptr");
            builder_.CreateStore(val, elemPtr);
            return;
        }
    }

    if (objPtr->getType() != ptrTy_)
        codegenError("index assignment requires list or map");

    llvm::Type *mapKeyTy = getMapKeyType(objPtr);
    if (mapKeyTy) {
        // CoW check for map index assignment
        objPtr = emitCowCheck(objPtr, receiverAlloca, CollectionKind::Map);

        // Map index assignment
        llvm::Type *mapValTy = getMapValueType(objPtr);
        if (!mapValTy)
            codegenError("cannot determine map value type");
        if (key->getType() != mapKeyTy)
            codegenError("map key type mismatch");
        if (val->getType() != mapValTy)
            codegenError("map value type mismatch");

        // Lookup key
        llvm::Value *idx = emitMapKeyLookup(objPtr, key, mapKeyTy);
        llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "found");

        llvm::BasicBlock *updateBB = llvm::BasicBlock::Create(*ctx_, "map.update", fn_);
        llvm::BasicBlock *insertBB = llvm::BasicBlock::Create(*ctx_, "map.insert", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "map.assign_end", fn_);

        builder_.CreateCondBr(found, updateBB, insertBB);

        // Update existing value
        builder_.SetInsertPoint(updateBB);
        llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 3, "map_vals_ptr");
        llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "map_vals");
        llvm::Value *valElemPtr = builder_.CreateGEP(mapValTy, valsPtr, {idx}, "val_elem_ptr");
        builder_.CreateStore(val, valElemPtr);
        builder_.CreateBr(endBB);

        // Insert new key-value pair
        builder_.SetInsertPoint(insertBB);
        llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 0, "map_len_ptr");
        llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "map_len");
        llvm::Value *capPtr = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 1, "map_cap_ptr");
        llvm::Value *cap = builder_.CreateLoad(i64Ty_, capPtr, "map_cap");

        // Check if we need to grow
        llvm::Value *needGrow = builder_.CreateICmpEQ(length, cap, "need_grow");
        llvm::BasicBlock *growBB = llvm::BasicBlock::Create(*ctx_, "map.grow", fn_);
        llvm::BasicBlock *storeBB = llvm::BasicBlock::Create(*ctx_, "map.store", fn_);
        builder_.CreateCondBr(needGrow, growBB, storeBB);

        // Grow: realloc keys and values arrays
        builder_.SetInsertPoint(growBB);
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t keySize = dl.getTypeAllocSize(mapKeyTy);
        uint64_t valSize = dl.getTypeAllocSize(mapValTy);

        llvm::Value *newCap = builder_.CreateMul(cap, llvm::ConstantInt::get(i64Ty_, 2), "new_cap");

        auto mallocFn = getStdlibMalloc();

        // New keys array
        llvm::Value *newKeySize = builder_.CreateMul(newCap, llvm::ConstantInt::get(i64Ty_, keySize), "new_key_size");
        llvm::Value *newKeysPtr = builder_.CreateCall(mallocFn, {newKeySize}, "new_keys");

        // New values array
        llvm::Value *newValSize = builder_.CreateMul(newCap, llvm::ConstantInt::get(i64Ty_, valSize), "new_val_size");
        llvm::Value *newValsPtr = builder_.CreateCall(mallocFn, {newValSize}, "new_vals");

        // memcpy old data
        auto memcpyFn = getStdlibMemcpy();

        llvm::Value *keysPtrField2 = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 2, "keys_field");
        llvm::Value *oldKeysPtr = builder_.CreateLoad(ptrTy_, keysPtrField2, "old_keys");
        llvm::Value *oldKeySize = builder_.CreateMul(length, llvm::ConstantInt::get(i64Ty_, keySize), "old_key_size");
        builder_.CreateCall(memcpyFn, {newKeysPtr, oldKeysPtr, oldKeySize});

        llvm::Value *valsPtrField2 = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 3, "vals_field");
        llvm::Value *oldValsPtr = builder_.CreateLoad(ptrTy_, valsPtrField2, "old_vals");
        llvm::Value *oldValSize = builder_.CreateMul(length, llvm::ConstantInt::get(i64Ty_, valSize), "old_val_size");
        builder_.CreateCall(memcpyFn, {newValsPtr, oldValsPtr, oldValSize});

        // Free old arrays
        auto freeFn = getStdlibFree();
        builder_.CreateCall(freeFn, {oldKeysPtr});
        builder_.CreateCall(freeFn, {oldValsPtr});

        // Update header pointers and capacity
        builder_.CreateStore(newKeysPtr, keysPtrField2);
        builder_.CreateStore(newValsPtr, valsPtrField2);
        builder_.CreateStore(newCap, capPtr);

        builder_.CreateBr(storeBB);

        // Store new key-value at index = length
        builder_.SetInsertPoint(storeBB);
        llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "cur_len");
        llvm::Value *keysPtrField3 = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 2, "keys_field3");
        llvm::Value *curKeysPtr = builder_.CreateLoad(ptrTy_, keysPtrField3, "cur_keys");
        llvm::Value *newKeyPtr = builder_.CreateGEP(mapKeyTy, curKeysPtr, {curLen}, "new_key_ptr");
        builder_.CreateStore(key, newKeyPtr);

        llvm::Value *valsPtrField3 = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 3, "vals_field3");
        llvm::Value *curValsPtr = builder_.CreateLoad(ptrTy_, valsPtrField3, "cur_vals");
        llvm::Value *newValPtr = builder_.CreateGEP(mapValTy, curValsPtr, {curLen}, "new_val_ptr");
        builder_.CreateStore(val, newValPtr);

        // length++
        llvm::Value *newLen = builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1), "new_len");
        builder_.CreateStore(newLen, lenPtr);

        // Insert into hash table buckets and check rehash
        emitBucketInsertAndRehashCheck(objPtr, mapHeaderTy_, kMapLayout.lenIdx, kMapLayout.bucketCountIdx, kMapLayout.bucketsPtrIdx, key, mapKeyTy, curLen);

        builder_.CreateBr(endBB);

        builder_.SetInsertPoint(endBB);
        return;
    }

    // List index assignment
    objPtr = emitCowCheck(objPtr, receiverAlloca, CollectionKind::List);
    llvm::Type *elemTy = getListElementType(objPtr);
    if (!elemTy)
        codegenError("cannot determine list element type for index assignment");

    llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, objPtr, 0, "len_ptr");
    llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "length");

    emitBoundsCheck(key, length,
                    "runtime error: index %lld out of bounds for list of length %lld\n", ".idx_assign_err", "idx_assign");
    llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, objPtr, 2, "data_ptr");
    llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "data");
    llvm::Value *elemPtr = builder_.CreateGEP(elemTy, dataPtr, {key}, "elem_ptr");
    builder_.CreateStore(val, elemPtr);
}
