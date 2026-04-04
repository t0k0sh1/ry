#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"

void CodeGen::emitStmt(RecordStmt &s) {
    emitTraceSymbolDefine("record", s.name, s.loc);
    if (struct_types_.count(s.name))
        codegenError("redefined type: " + s.name);

    std::string parentName;
    std::vector<FieldDef> allFields;

    if (s.parent_name) {
        parentName = *s.parent_name;
        auto pit = struct_types_.find(parentName);
        if (pit == struct_types_.end())
            codegenError("parent record '" + parentName + "' not defined");

        const auto &parentInfo = pit->second;

        // Parent fields are pre-flattened (include all ancestors), so no recursion needed
        for (auto &pf : parentInfo.fields)
            allFields.push_back({pf.name, TypeNode::clone(pf.type), {}});

        std::unordered_set<std::string> parentFieldNames;
        for (auto &pf : parentInfo.fields)
            parentFieldNames.insert(pf.name);
        for (auto &cf : s.fields) {
            if (parentFieldNames.count(cf.name))
                codegenError("field '" + cf.name + "' in '" + s.name +
                             "' conflicts with inherited field from '" + parentName + "'");
        }
    }

    for (auto &f : s.fields)
        allFields.push_back({f.name, TypeNode::clone(f.type), std::move(f.directives)});

    std::vector<llvm::Type*> fieldTypes;
    for (auto &f : allFields)
        fieldTypes.push_back(resolveType(f.type->toString()));

    llvm::StructType *structTy = llvm::StructType::create(*ctx_, fieldTypes, s.name);
    if (hasDirective(s.directives, "deprecated"))
        deprecated_types_.insert(s.name);
    for (auto &f : allFields) {
        if (hasDirective(f.directives, "deprecated"))
            deprecated_fields_.insert(s.name + "." + f.name);
    }

    struct_types_[s.name] = {structTy, std::move(allFields), std::move(s.invariants),
                             parentName};
}

llvm::Value *CodeGen::emitStructConstructor(const StructInfo &info,
                                             const std::string &name,
                                             const std::vector<ExprPtr> &args) {
    if (args.size() != info.fields.size())
        codegenError("type '" + name + "': expected " +
                                 std::to_string(info.fields.size()) + " arguments, got " +
                                 std::to_string(args.size()));

    llvm::Value *result = llvm::UndefValue::get(info.llvmType);

    for (unsigned i = 0; i < info.fields.size(); ++i) {
        llvm::Value *val = emitExpr(*args[i]);
        llvm::Type *expectedTy = info.llvmType->getElementType(i);
        if (val->getType() != expectedTy)
            codegenError("type '" + name + "': field '" + info.fields[i].name +
                                     "' type mismatch");
        result = builder_.CreateInsertValue(result, val, i);
    }

    emitInvariantCheck(name, info, result);

    return result;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<FieldAccessExpr> &e) {
    llvm::Value *obj = emitExpr(*e->object);
    llvm::Type *objTy = obj->getType();

    llvm::StructType *structTy = llvm::dyn_cast<llvm::StructType>(objTy);
    if (!structTy)
        codegenError("field access on non-struct type");

    // Error type field access: .message (idx 0), .code (idx 1)
    if (structTy == errorTy_) {
        if (e->field == "message")
            return builder_.CreateExtractValue(obj, 0, "err.message");
        if (e->field == "code")
            return builder_.CreateExtractValue(obj, 1, "err.code");
        codegenError("Error type has no field '" + e->field + "'");
    }

    // Numeric index access for tuples (.0, .1, ...)
    if (!e->field.empty() && std::isdigit(static_cast<unsigned char>(e->field[0]))) {
        unsigned idx = std::stoul(e->field);
        if (idx >= structTy->getNumElements())
            codegenError("tuple index " + e->field + " out of range");
        return builder_.CreateExtractValue(obj, idx, "tuple." + e->field);
    }

    std::string typeName = structTy->getName().str();
    auto it = struct_types_.find(typeName);
    if (it == struct_types_.end())
        codegenError("unknown struct type: " + typeName);

    const auto &info = it->second;
    for (unsigned i = 0; i < info.fields.size(); ++i) {
        if (info.fields[i].name == e->field) {
            std::string qualifiedField = typeName + "." + e->field;
            if (deprecated_fields_.count(qualifiedField))
                emitDeprecationWarning(qualifiedField);
            llvm::Value *fieldVal = builder_.CreateExtractValue(obj, i, e->field);
            if (info.fields[i].type)
                propagateTypeMeta(info.fields[i].type->toString(), fieldVal);
            return fieldVal;
        }
    }

    codegenError("type '" + typeName + "' has no field '" + e->field + "'");
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<TupleExpr> &e) {
    std::vector<llvm::Type*> types;
    types.reserve(e->elements.size());
    std::vector<llvm::Value*> vals;
    vals.reserve(e->elements.size());
    for (auto &el : e->elements) {
        llvm::Value *v = emitExpr(*el);
        types.push_back(v->getType());
        vals.push_back(v);
    }
    llvm::StructType *tupleType = llvm::StructType::get(*ctx_, types);
    llvm::Value *result = llvm::UndefValue::get(tupleType);
    for (unsigned i = 0; i < vals.size(); ++i)
        result = builder_.CreateInsertValue(result, vals[i], i);
    return result;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<ListExpr> &e) {
    if (e->elements.empty())
        codegenError("empty list literal requires type annotation (not yet supported)");

    // Evaluate all elements
    std::vector<llvm::Value*> vals;
    vals.reserve(e->elements.size());
    for (auto &el : e->elements)
        vals.push_back(emitExpr(*el));

    // Check all elements have the same type
    llvm::Type *elemTy = vals[0]->getType();
    for (size_t i = 1; i < vals.size(); ++i) {
        if (vals[i]->getType() != elemTy)
            codegenError("list elements must all have the same type");
    }

    int64_t count = static_cast<int64_t>(vals.size());

    // Allocate list header with ARC: [ArcHeader][ListHeader]
    llvm::Value *headerPtr = emitArcAllocCollectionHeader(listHeaderTy_);

    // Allocate data buffer (separate allocation, freed by destructor)
    const llvm::DataLayout &dl = mod_->getDataLayout();
    auto mallocFn = getStdlibMalloc();
    uint64_t elemSize = dl.getTypeAllocSize(elemTy);
    llvm::Value *dataSize = llvm::ConstantInt::get(i64Ty_, elemSize * count);
    llvm::Value *dataPtr = builder_.CreateCall(mallocFn, {dataSize}, "list_data");

    // Store elements into data
    for (int64_t i = 0; i < count; ++i) {
        llvm::Value *elemPtr = builder_.CreateGEP(
            elemTy, dataPtr, {llvm::ConstantInt::get(i64Ty_, i)}, "elem_ptr");
        builder_.CreateStore(vals[i], elemPtr);
    }

    // Store length, capacity, data pointer into header
    storeListHeaderFields(headerPtr, llvm::ConstantInt::get(i64Ty_, count),
                          llvm::ConstantInt::get(i64Ty_, count), dataPtr);

    // Track element type
    type_meta_[TM_ListElem][headerPtr] = elemTy;

    // Track nested list element type (for flatten support)
    // Only set if ALL elements are lists with the same inner element type
    if (elemTy == ptrTy_) {
        llvm::Type *innerElemTy = getListElementType(vals[0]);
        if (innerElemTy) {
            bool allMatch = true;
            for (size_t i = 1; i < vals.size(); ++i) {
                llvm::Type *otherInner = getListElementType(vals[i]);
                if (!otherInner || otherInner != innerElemTy) {
                    allMatch = false;
                    break;
                }
            }
            if (allMatch)
                type_meta_[TM_NestedListElem][headerPtr] = innerElemTy;
        }
    }

    return headerPtr;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<MapExpr> &e) {
    if (e->keys.empty())
        codegenError("empty map literal requires type annotation");

    // Evaluate all keys and values
    std::vector<llvm::Value*> keyVals, valVals;
    keyVals.reserve(e->keys.size());
    valVals.reserve(e->values.size());
    for (auto &k : e->keys) keyVals.push_back(emitExpr(*k));
    for (auto &v : e->values) valVals.push_back(emitExpr(*v));

    // Check all keys have the same type
    llvm::Type *keyTy = keyVals[0]->getType();
    for (size_t i = 1; i < keyVals.size(); ++i) {
        if (keyVals[i]->getType() != keyTy)
            codegenError("map keys must all have the same type");
    }

    // Check all values have the same type
    llvm::Type *valTy = valVals[0]->getType();
    for (size_t i = 1; i < valVals.size(); ++i) {
        if (valVals[i]->getType() != valTy)
            codegenError("map values must all have the same type");
    }

    int64_t count = static_cast<int64_t>(keyVals.size());

    const llvm::DataLayout &dl = mod_->getDataLayout();

    // Allocate MapHeader with ARC: [ArcHeader][MapHeader]
    llvm::Value *headerPtr = emitArcAllocCollectionHeader(mapHeaderTy_);

    // Allocate keys and values arrays (separate allocations, freed by destructor)
    auto mallocFn = getStdlibMalloc();
    uint64_t keySize = dl.getTypeAllocSize(keyTy);
    llvm::Value *keysPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, keySize * count)}, "map_keys");

    uint64_t valSize = dl.getTypeAllocSize(valTy);
    llvm::Value *valsPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, valSize * count)}, "map_vals");

    // Store keys and values
    for (int64_t i = 0; i < count; ++i) {
        llvm::Value *kp = builder_.CreateGEP(keyTy, keysPtr,
            {llvm::ConstantInt::get(i64Ty_, i)}, "key_ptr");
        builder_.CreateStore(keyVals[i], kp);
        llvm::Value *vp = builder_.CreateGEP(valTy, valsPtr,
            {llvm::ConstantInt::get(i64Ty_, i)}, "val_ptr");
        builder_.CreateStore(valVals[i], vp);
    }

    // Store header fields: length, capacity, keys_ptr, values_ptr
    storeMapHeaderFields(headerPtr, llvm::ConstantInt::get(i64Ty_, count),
                         llvm::ConstantInt::get(i64Ty_, count), keysPtr, valsPtr);

    // Initialize hash table buckets via rehash
    int64_t initBucketCount = 8;
    while (initBucketCount * 3 < count * 4) initBucketCount *= 2;
    {
        std::string rehashName;
        llvm::Type *rehashKeyTy;
        if (keyTy == ptrTy_) {
            rehashName = "__ry_ht_rehash_str";
            rehashKeyTy = ptrTy_;
        } else if (keyTy->isDoubleTy()) {
            rehashName = "__ry_ht_rehash_f64";
            rehashKeyTy = f64Ty_;
        } else {
            rehashName = "__ry_ht_rehash_i64";
            rehashKeyTy = i64Ty_;
        }
        llvm::FunctionType *rehashTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, i64Ty_, i64Ty_}, false);
        llvm::FunctionCallee rehashFn = mod_->getOrInsertFunction(rehashName, rehashTy);
        llvm::Value *buckets = builder_.CreateCall(rehashFn,
            {keysPtr, llvm::ConstantInt::get(i64Ty_, count),
             llvm::ConstantInt::get(i64Ty_, initBucketCount)}, "map_buckets");
        llvm::Value *bcPtr = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 4, "map_bc_ptr");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, initBucketCount), bcPtr);
        llvm::Value *bpPtr = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 5, "map_bp_ptr");
        builder_.CreateStore(buckets, bpPtr);
    }

    // Track types
    type_meta_[TM_MapKey][headerPtr] = keyTy;
    type_meta_[TM_MapValue][headerPtr] = valTy;

    if (valTy == ptrTy_ && !valVals.empty()) {
        std::string valTypeName = inferCollectionTypeName(valVals[0]);
        if (!valTypeName.empty())
            map_value_type_names_[headerPtr] = valTypeName;
    }

    return headerPtr;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<SetExpr> &e) {
    if (e->elements.empty()) {
        // Empty set — requires type annotation (handled in emitVarDecl)
        // If reached here directly, error
        codegenError("empty set literal requires type annotation");
    }

    // Evaluate all elements
    std::vector<llvm::Value*> vals;
    vals.reserve(e->elements.size());
    for (auto &el : e->elements)
        vals.push_back(emitExpr(*el));

    // Check all elements have the same type
    llvm::Type *elemTy = vals[0]->getType();
    for (size_t i = 1; i < vals.size(); ++i) {
        if (vals[i]->getType() != elemTy)
            codegenError("set elements must all have the same type");
    }

    int64_t count = static_cast<int64_t>(vals.size());

    const llvm::DataLayout &dl = mod_->getDataLayout();

    // Allocate SetHeader with ARC: [ArcHeader][SetHeader]
    llvm::Value *headerPtr = emitArcAllocCollectionHeader(setHeaderTy_);

    // Allocate elements array (separate allocation, freed by destructor)
    auto mallocFn = getStdlibMalloc();
    uint64_t elemSize = dl.getTypeAllocSize(elemTy);
    llvm::Value *elemsPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, elemSize * count)}, "set_elems");

    // Initialize header: length=0, capacity=count, elements pointer
    storeSetHeaderFields(headerPtr, llvm::ConstantInt::get(i64Ty_, 0),
                         llvm::ConstantInt::get(i64Ty_, count), elemsPtr);
    llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, headerPtr, 0, "set_len_ptr");
    llvm::Value *elemsPtrField = builder_.CreateStructGEP(setHeaderTy_, headerPtr, 2, "set_elems_field");

    // Initialize empty hash table buckets
    int64_t initBucketCount = 8;
    while (initBucketCount * 3 < count * 4) initBucketCount *= 2;
    emitBucketInit(headerPtr, setHeaderTy_, kSetLayout.bucketCountIdx,
                   kSetLayout.bucketsPtrIdx, initBucketCount);

    // Track element type (must be set before emitSetElementLookup)
    type_meta_[TM_SetElem][headerPtr] = elemTy;

    // Insert elements with deduplication (same pattern as add())
    for (int64_t i = 0; i < count; ++i) {
        llvm::Value *idx = emitSetElementLookup(headerPtr, vals[i], elemTy);
        llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "found");

        llvm::BasicBlock *insertBB = llvm::BasicBlock::Create(*ctx_, "setlit.insert", fn_);
        llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "setlit.next", fn_);
        builder_.CreateCondBr(found, nextBB, insertBB);

        builder_.SetInsertPoint(insertBB);
        llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "cur_len");
        llvm::Value *curElems = builder_.CreateLoad(ptrTy_, elemsPtrField, "cur_elems");
        llvm::Value *ep = builder_.CreateGEP(elemTy, curElems, {curLen}, "set_elem_ptr");
        builder_.CreateStore(vals[i], ep);
        llvm::Value *newLen = builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1), "new_len");
        builder_.CreateStore(newLen, lenPtr);
        emitBucketInsertAndRehashCheck(headerPtr, setHeaderTy_,
            kSetLayout.lenIdx, kSetLayout.bucketCountIdx, kSetLayout.bucketsPtrIdx,
            vals[i], elemTy, curLen);
        builder_.CreateBr(nextBB);

        builder_.SetInsertPoint(nextBB);
    }

    return headerPtr;
}

llvm::Value *CodeGen::emitExprVariant(const EnumAccessExpr &e) {
    // Try to instantiate generic enum if not found
    if (!enum_types_.count(e.enum_name)) {
        auto ltPos = e.enum_name.find('<');
        if (ltPos != std::string::npos && e.enum_name.back() == '>') {
            std::string baseName = e.enum_name.substr(0, ltPos);
            std::string argsStr = e.enum_name.substr(ltPos + 1, e.enum_name.size() - ltPos - 2);
            auto typeArgs = splitTypeArgs(argsStr);
            instantiateGenericEnum(e.enum_name, baseName, typeArgs);
        }
    }
    auto it = enum_types_.find(e.enum_name);
    if (it == enum_types_.end())
        codegenError("undefined enum: " + e.enum_name);
    auto vit = it->second.variants.find(e.variant_name);
    if (vit == it->second.variants.end())
        codegenError("enum '" + e.enum_name + "' has no variant '" + e.variant_name + "'");

    if (it->second.isADT) {
        // Reject access to payload-carrying variants without arguments
        auto fit = it->second.variantFields.find(e.variant_name);
        if (fit != it->second.variantFields.end() && !fit->second.fieldTypes.empty())
            codegenError("variant '" + e.enum_name + "::" + e.variant_name +
                "' requires " + std::to_string(fit->second.fieldTypes.size()) +
                " argument(s); use '" + e.enum_name + "::" + e.variant_name + "(...)' instead");
        // ADT enum: create struct { tag, zero-payload } for data-less variants
        llvm::Value *adtVal = llvm::UndefValue::get(it->second.adtType);
        adtVal = builder_.CreateInsertValue(adtVal, llvm::ConstantInt::get(i64Ty_, vit->second), 0, "adt.tag");
        enum_value_types_[adtVal] = e.enum_name;
        return adtVal;
    }

    llvm::Value *val = llvm::ConstantInt::get(i64Ty_, vit->second);
    enum_value_types_[val] = e.enum_name;
    return val;
}


llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<IndexExpr> &e) {
    llvm::Value *objPtr = emitExpr(*e->object);

    llvm::SmallVector<llvm::Value*, 2> indexValues;
    for (auto &idx : e->indices)
        indexValues.push_back(emitExpr(*idx));

    if (llvm::Value *result = trySubscriptOperatorCall(objPtr, indexValues))
        return result;
    if (indexValues.size() > 1)
        codegenError("multi-index requires operator[] overload");

    llvm::Value *index = indexValues[0];

    // Fixed-length array index access
    if (auto *ai = llvm::dyn_cast<llvm::AllocaInst>(objPtr)) {
        if (auto *arrTy = llvm::dyn_cast<llvm::ArrayType>(ai->getAllocatedType())) {
            llvm::Type *elemTy = arrTy->getElementType();
            uint64_t arrSize = arrTy->getNumElements();

            emitBoundsCheck(index, llvm::ConstantInt::get(i64Ty_, arrSize),
                            "runtime error: index %lld out of bounds for array of length %lld\n", ".arr_idx_err", "arr");

            llvm::Value *elemPtr = builder_.CreateGEP(
                arrTy, ai, {llvm::ConstantInt::get(i64Ty_, 0), index}, "arr_elem_ptr");
            llvm::Value *result = builder_.CreateLoad(elemTy, elemPtr, "arr_elem");

            auto ait = array_elem_type_names_.find(ai);
            if (ait != array_elem_type_names_.end())
                low_level_type_names_[result] = ait->second;

            return result;
        }
    }

    if (objPtr->getType() != ptrTy_)
        codegenError("index operator requires list or map");

    // Check if this is a map
    llvm::Type *mapKeyTy = getMapKeyType(objPtr);
    if (mapKeyTy) {
        llvm::Type *mapValTy = getMapValueType(objPtr);
        if (!mapValTy)
            codegenError("cannot determine map value type");

        // Check key type matches
        if (index->getType() != mapKeyTy)
            codegenError("map key type mismatch");

        // Lookup key
        llvm::Value *idx = emitMapKeyLookup(objPtr, index, mapKeyTy);

        // Check if found
        llvm::Value *notFound = builder_.CreateICmpSLT(idx, llvm::ConstantInt::get(i64Ty_, 0), "not_found");

        llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "map.notfound", fn_);
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "map.found", fn_);

        builder_.CreateCondBr(notFound, failBB, okBB);

        // Not found: print error and exit
        builder_.SetInsertPoint(failBB);
        emitRuntimeError("runtime error: map key not found\n", ".map_key_err");

        // Found: get value
        builder_.SetInsertPoint(okBB);
        llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 3, "map_vals_ptr");
        llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "map_vals");
        llvm::Value *valElemPtr = builder_.CreateGEP(mapValTy, valsPtr, {idx}, "val_elem_ptr");
        llvm::Value *mapVal = builder_.CreateLoad(mapValTy, valElemPtr, "map_val");

        auto mvtn = map_value_type_names_.find(objPtr);
        if (mvtn == map_value_type_names_.end()) {
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(objPtr))
                mvtn = map_value_type_names_.find(load->getPointerOperand());
        }
        if (mvtn != map_value_type_names_.end())
            propagateTypeMeta(mvtn->second, mapVal);

        return mapVal;
    }

    // List index access
    llvm::Type *elemTy = getListElementType(objPtr);
    if (!elemTy)
        codegenError("cannot determine list element type for index access");

    llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, objPtr, 0, "len_ptr");
    llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "length");

    emitBoundsCheck(index, length,
                    "runtime error: index %lld out of bounds for list of length %lld\n", ".idx_err", "index");
    llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, objPtr, 2, "data_ptr");
    llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "data");
    llvm::Value *elemPtr = builder_.CreateGEP(elemTy, dataPtr, {index}, "elem_ptr");
    llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "elem");

    // Without this, chained indexing like matrix[i][j] loses element type metadata.
    llvm::Type *nestedElemTy = getNestedListElementType(objPtr);
    if (nestedElemTy)
        type_meta_[TM_ListElem][elem] = nestedElemTy;

    return elem;
}

llvm::Value *CodeGen::valueToString(llvm::Value *val) {
    llvm::Type *ty = val->getType();

    if (ty == anyTy_)
        return emitAnyToString(val);

    // Enum value → variant name string
    {
        auto evIt = enum_value_types_.find(val);
        if (evIt == enum_value_types_.end()) {
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val))
                evIt = enum_value_types_.find(load->getPointerOperand());
        }
        if (evIt != enum_value_types_.end()) {
            auto &einfo = enum_types_[evIt->second];
            if (!einfo.isADT) {
                if (einfo.hasExplicitValues) {
                    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "vts.enum.merge", fn_);
                    llvm::BasicBlock *defaultBB = llvm::BasicBlock::Create(*ctx_, "vts.enum.default", fn_);
                    auto *sw = builder_.CreateSwitch(val, defaultBB, einfo.variantCount);
                    builder_.SetInsertPoint(mergeBB);
                    auto *namePhi = builder_.CreatePHI(ptrTy_, einfo.variantCount + 1, "vts.enum.name");
                    for (size_t i = 0; i < einfo.variantOrder.size(); ++i) {
                        const auto &vname = einfo.variantOrder[i];
                        int64_t vval = einfo.variants.at(vname);
                        llvm::BasicBlock *caseBB = llvm::BasicBlock::Create(*ctx_, "vts.enum." + vname, fn_);
                        sw->addCase(llvm::cast<llvm::ConstantInt>(llvm::ConstantInt::get(i64Ty_, vval, true)), caseBB);
                        builder_.SetInsertPoint(caseBB);
                        llvm::Value *namePtr = builder_.CreateGEP(
                            llvm::ArrayType::get(ptrTy_, einfo.variantCount),
                            einfo.nameArray,
                            {llvm::ConstantInt::get(i64Ty_, 0), llvm::ConstantInt::get(i64Ty_, i)},
                            "enum_name_ptr");
                        llvm::Value *nameStr = builder_.CreateLoad(ptrTy_, namePtr, "enum_name");
                        namePhi->addIncoming(nameStr, caseBB);
                        builder_.CreateBr(mergeBB);
                    }
                    builder_.SetInsertPoint(defaultBB);
                    llvm::Value *unknownStr = builder_.CreateGlobalString("?", ".enum_unknown");
                    namePhi->addIncoming(unknownStr, defaultBB);
                    builder_.CreateBr(mergeBB);
                    builder_.SetInsertPoint(mergeBB);
                    return namePhi;
                }
                llvm::Value *namePtr = builder_.CreateGEP(
                    llvm::ArrayType::get(ptrTy_, einfo.variantCount),
                    einfo.nameArray,
                    {llvm::ConstantInt::get(i64Ty_, 0), val},
                    "enum_name_ptr");
                return builder_.CreateLoad(ptrTy_, namePtr, "enum_name");
            }

            // ADT enum: sprint buffer + recursive valueToString
            auto spf = getSprintPrintf();
            emitSprintBegin();

            llvm::Value *tag = builder_.CreateExtractValue(val, 0, "vts.adt.tag");
            llvm::Value *namePtr = builder_.CreateGEP(
                llvm::ArrayType::get(ptrTy_, einfo.variantCount),
                einfo.nameArray,
                {llvm::ConstantInt::get(i64Ty_, 0), tag},
                "vts.adt.name_ptr");
            llvm::Value *nameStr = builder_.CreateLoad(ptrTy_, namePtr, "vts.adt.name");

            llvm::AllocaInst *adtAlloca = builder_.CreateAlloca(einfo.adtType, nullptr, "vts.adt.tmp");
            builder_.CreateStore(val, adtAlloca);
            llvm::Value *payloadPtr = builder_.CreateStructGEP(einfo.adtType, adtAlloca, 1, "vts.adt.payload");

            bool anyFields = false;
            for (auto &[vn, vf] : einfo.variantFields)
                if (!vf.fieldTypes.empty()) { anyFields = true; break; }

            if (anyFields) {
                llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "vts.adt.end", fn_);
                llvm::BasicBlock *defaultBB = llvm::BasicBlock::Create(*ctx_, "vts.adt.default", fn_);
                auto *switchInst = builder_.CreateSwitch(tag, defaultBB, einfo.variantCount);

                for (auto &[vname, vtag] : einfo.variants) {
                    llvm::BasicBlock *caseBB = llvm::BasicBlock::Create(*ctx_, "vts.adt." + vname, fn_);
                    switchInst->addCase(
                        llvm::cast<llvm::ConstantInt>(llvm::ConstantInt::get(i64Ty_, vtag)), caseBB);
                    builder_.SetInsertPoint(caseBB);

                    auto fit = einfo.variantFields.find(vname);
                    if (fit != einfo.variantFields.end() && !fit->second.fieldTypes.empty()) {
                        llvm::Constant *openFmt = cachedGlobalString("%s(", ".vts_adt_open");
                        builder_.CreateCall(spf, {openFmt, nameStr});

                        const llvm::DataLayout &dl = mod_->getDataLayout();
                        size_t offset = 0;
                        for (size_t fi = 0; fi < fit->second.fieldTypes.size(); ++fi) {
                            llvm::Type *fieldTy = fit->second.fieldTypes[fi];
                            uint64_t align = dl.getABITypeAlign(fieldTy).value();
                            offset = (offset + align - 1) / align * align;
                            llvm::Value *fieldPtr = builder_.CreateGEP(
                                llvm::Type::getInt8Ty(*ctx_), payloadPtr,
                                {llvm::ConstantInt::get(i64Ty_, offset)},
                                "vts.adt.field." + std::to_string(fi));
                            llvm::Value *fieldVal = builder_.CreateLoad(fieldTy, fieldPtr, "vts.adt.fval");

                            // Propagate low-level type metadata for correct signedness
                            if (fi < fit->second.fieldTypeNames.size()) {
                                const auto &ftName = fit->second.fieldTypeNames[fi];
                                if (isLowLevelTypeName(ftName))
                                    low_level_type_names_[fieldVal] = ftName;
                            }

                            if (fi > 0) {
                                llvm::Constant *commaFmt = cachedGlobalString(", ", ".vts_adt_comma");
                                builder_.CreateCall(spf, {commaFmt});
                            }

                            llvm::Value *fieldStr = valueToString(fieldVal);
                            llvm::Constant *sfmt = cachedGlobalString("%s", ".vts_adt_s");
                            builder_.CreateCall(spf, {sfmt, fieldStr});

                            offset += dl.getTypeAllocSize(fieldTy);
                        }

                        llvm::Constant *closeFmt = cachedGlobalString(")", ".vts_adt_close");
                        builder_.CreateCall(spf, {closeFmt});
                    } else {
                        llvm::Constant *fmt = cachedGlobalString("%s", ".vts_adt_nodata");
                        builder_.CreateCall(spf, {fmt, nameStr});
                    }
                    builder_.CreateBr(endBB);
                }

                builder_.SetInsertPoint(defaultBB);
                builder_.CreateBr(endBB);
                builder_.SetInsertPoint(endBB);
            } else {
                llvm::Constant *fmt = cachedGlobalString("%s", ".vts_adt_simple");
                builder_.CreateCall(spf, {fmt, nameStr});
            }

            return emitSprintEnd("vts.adt.str");
        }
    }

    if (ty == errorTy_) {
        auto spf = getSprintPrintf();
        builder_.CreateCall(getRuntimeFn("__ry_sprint_begin",
            llvm::Type::getVoidTy(*ctx_), {}));
        llvm::Value *msg = builder_.CreateExtractValue(val, 0, "vts_err_msg");
        llvm::Value *code = builder_.CreateExtractValue(val, 1, "vts_err_code");
        llvm::Constant *fmt = cachedGlobalString("Error: %s (code: %ld)", ".vts_err_fmt");
        builder_.CreateCall(spf, {fmt, msg, code});
        return emitSprintEnd("vts_err_str");
    }

    if (auto *structTy = llvm::dyn_cast<llvm::StructType>(ty)) {
        for (auto &[uname, uinfo] : union_type_info_) {
            if (uinfo.llvmType != structTy) continue;

            llvm::Value *tag = builder_.CreateExtractValue(val, 0, "vts.union.tag");
            llvm::Value *dataBytes = builder_.CreateExtractValue(val, 1, "vts.union.data");
            auto *dataTy = uinfo.llvmType->getElementType(1);
            llvm::AllocaInst *dataTmp = builder_.CreateAlloca(dataTy, nullptr, "vts.union.data.tmp");
            dataTmp->setAlignment(mod_->getDataLayout().getABITypeAlign(uinfo.llvmType));
            builder_.CreateStore(dataBytes, dataTmp);

            llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "vts.union.merge", fn_);
            llvm::BasicBlock *defaultBB = llvm::BasicBlock::Create(*ctx_, "vts.union.default", fn_);
            llvm::SwitchInst *sw = builder_.CreateSwitch(tag, defaultBB, uinfo.componentTypes.size());

            builder_.SetInsertPoint(defaultBB);
            llvm::Constant *unknownStr = cachedGlobalString("?", ".vts_union_unknown");
            builder_.CreateBr(mergeBB);

            builder_.SetInsertPoint(mergeBB);
            auto *phi = builder_.CreatePHI(ptrTy_, uinfo.componentTypes.size() + 1, "vts.union.str");
            phi->addIncoming(unknownStr, defaultBB);

            for (size_t i = 0; i < uinfo.componentTypes.size(); ++i) {
                llvm::BasicBlock *caseBB = llvm::BasicBlock::Create(
                    *ctx_, "vts.union.case" + std::to_string(i), fn_);
                sw->addCase(llvm::ConstantInt::get(
                    llvm::cast<llvm::IntegerType>(i64Ty_), i), caseBB);
                builder_.SetInsertPoint(caseBB);

                const auto &compName = uinfo.componentNames[i];

                // Reject non-stringifiable pointer-backed variants
                if (uinfo.componentTypes[i]->isPointerTy() && compName != "str") {
                    codegenError("cannot convert " + compName +
                                 " variant of union to string");
                }

                llvm::Value *innerVal = builder_.CreateLoad(
                    uinfo.componentTypes[i], dataTmp, "vts.union.inner");

                // Propagate low-level type metadata for correct signedness formatting
                if (isLowLevelTypeName(compName))
                    low_level_type_names_[innerVal] = compName;

                llvm::Value *innerStr = valueToString(innerVal);

                phi->addIncoming(innerStr, builder_.GetInsertBlock());
                builder_.CreateBr(mergeBB);
            }

            builder_.SetInsertPoint(mergeBB);
            return phi;
        }

        if (isOptionType(ty)) {
            auto spf = getSprintPrintf();
            emitSprintBegin();

            llvm::Value *hasValue = builder_.CreateExtractValue(val, 0, "vts.opt.has");
            llvm::BasicBlock *someBB = llvm::BasicBlock::Create(*ctx_, "vts.opt.some", fn_);
            llvm::BasicBlock *noneBB = llvm::BasicBlock::Create(*ctx_, "vts.opt.none", fn_);
            llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, "vts.opt.end", fn_);

            builder_.CreateCondBr(hasValue, someBB, noneBB);

            builder_.SetInsertPoint(noneBB);
            builder_.CreateCall(spf, {cachedGlobalString("None", ".vts_none")});
            builder_.CreateBr(endBB);

            builder_.SetInsertPoint(someBB);
            llvm::Value *innerVal = builder_.CreateExtractValue(val, 1, "vts.opt.val");
            propagateCollectionMetadata(val, innerVal);
            builder_.CreateCall(spf, {cachedGlobalString("Some(", ".vts_some_pre")});
            llvm::Value *innerStr = valueToString(innerVal);
            builder_.CreateCall(spf, {cachedGlobalString("%s", ".vts_opt_s"), innerStr});
            builder_.CreateCall(spf, {cachedGlobalString(")", ".vts_some_post")});
            builder_.CreateBr(endBB);

            builder_.SetInsertPoint(endBB);
            return emitSprintEnd("vts.opt.str");
        }

        if (isResultType(ty)) {
            auto spf = getSprintPrintf();
            emitSprintBegin();

            llvm::Value *isOk = builder_.CreateExtractValue(val, 0, "vts.res.is_ok");
            llvm::BasicBlock *okBB  = llvm::BasicBlock::Create(*ctx_, "vts.res.ok", fn_);
            llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "vts.res.err", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "vts.res.end", fn_);

            builder_.CreateCondBr(isOk, okBB, errBB);

            builder_.SetInsertPoint(okBB);
            llvm::Value *okVal = builder_.CreateExtractValue(val, 1, "vts.res.ok_val");
            propagateCollectionMetadata(val, okVal);
            builder_.CreateCall(spf, {cachedGlobalString("Ok(", ".vts_ok_pre")});
            llvm::Value *okStr = valueToString(okVal);
            builder_.CreateCall(spf, {cachedGlobalString("%s", ".vts_res_s"), okStr});
            builder_.CreateCall(spf, {cachedGlobalString(")", ".vts_ok_post")});
            builder_.CreateBr(endBB);

            builder_.SetInsertPoint(errBB);
            llvm::Value *errVal = builder_.CreateExtractValue(val, 2, "vts.res.err_val");
            propagateCollectionMetadata(val, errVal);
            builder_.CreateCall(spf, {cachedGlobalString("Err(", ".vts_err_pre")});
            llvm::Value *errStr = valueToString(errVal);
            builder_.CreateCall(spf, {cachedGlobalString("%s", ".vts_res_e"), errStr});
            builder_.CreateCall(spf, {cachedGlobalString(")", ".vts_err_post")});
            builder_.CreateBr(endBB);

            builder_.SetInsertPoint(endBB);
            return emitSprintEnd("vts.res.str");
        }

        std::string name = structTy->getName().str();
        if (struct_types_.count(name))
            return structToString(val);
        if (isTupleStructType(structTy))
            return tupleToString(val, structTy);
        codegenError("cannot convert this struct type to string: " + name);
    }

    // Fixed-length array: sprint buffer + IR loop
    if (auto *ai = llvm::dyn_cast<llvm::AllocaInst>(val)) {
        if (auto *arrTy = llvm::dyn_cast<llvm::ArrayType>(ai->getAllocatedType())) {
            llvm::Type *elemTy = arrTy->getElementType();
            uint64_t arrSize = arrTy->getNumElements();
            auto spf = getSprintPrintf();

            emitSprintBegin();
            builder_.CreateCall(spf, {cachedGlobalString("[", ".vts_arr_lb")});

            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "vts_arr.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "vts_arr.body", fn_);
            llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, "vts_arr.end", fn_);

            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "vts_arr_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(condBB);
            llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "i");
            builder_.CreateCondBr(
                builder_.CreateICmpSLT(iVal, llvm::ConstantInt::get(i64Ty_, arrSize)),
                bodyBB, endBB);

            builder_.SetInsertPoint(bodyBB);
            llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "i_cur");

            // Comma separator if not first element
            llvm::BasicBlock *commaBB = llvm::BasicBlock::Create(*ctx_, "vts_arr.comma", fn_);
            llvm::BasicBlock *elemBB  = llvm::BasicBlock::Create(*ctx_, "vts_arr.elem", fn_);
            builder_.CreateCondBr(
                builder_.CreateICmpSGT(iCur, llvm::ConstantInt::get(i64Ty_, 0)),
                commaBB, elemBB);

            builder_.SetInsertPoint(commaBB);
            builder_.CreateCall(spf, {cachedGlobalString(", ", ".vts_arr_comma")});
            builder_.CreateBr(elemBB);

            builder_.SetInsertPoint(elemBB);
            llvm::Value *elemPtr = builder_.CreateGEP(
                arrTy, ai,
                {llvm::ConstantInt::get(i64Ty_, 0), iCur},
                "vts_arr_ptr");
            llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "vts_arr_elem");
            llvm::Value *elemStr = valueToString(elem);
            builder_.CreateCall(spf, {cachedGlobalString("%s", ".vts_arr_s"), elemStr});

            builder_.CreateStore(
                builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1)),
                iVar);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(endBB);
            builder_.CreateCall(spf, {cachedGlobalString("]", ".vts_arr_rb")});
            return emitSprintEnd("vts_arr_str");
        }
    }

    if (ty->isPointerTy()) {
        // Set: sprint buffer + IR loop → {elem, elem, ...}
        if (llvm::Type *setElemTy = getSetElementType(val)) {
            auto spf = getSprintPrintf();
            emitSprintBegin();

            auto sf = loadSetHeader(val, "vts_set");

            builder_.CreateCall(spf, {cachedGlobalString("{", ".vts_set_lb")});

            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "vts_set.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "vts_set.body", fn_);
            llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, "vts_set.end", fn_);

            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "vts_set_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(condBB);
            llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "i");
            builder_.CreateCondBr(builder_.CreateICmpSLT(iVal, sf.len), bodyBB, endBB);

            builder_.SetInsertPoint(bodyBB);
            llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "i_cur");
            llvm::BasicBlock *commaBB = llvm::BasicBlock::Create(*ctx_, "vts_set.comma", fn_);
            llvm::BasicBlock *elemBB  = llvm::BasicBlock::Create(*ctx_, "vts_set.elem", fn_);
            builder_.CreateCondBr(
                builder_.CreateICmpSGT(iCur, llvm::ConstantInt::get(i64Ty_, 0)),
                commaBB, elemBB);

            builder_.SetInsertPoint(commaBB);
            builder_.CreateCall(spf, {cachedGlobalString(", ", ".vts_set_comma")});
            builder_.CreateBr(elemBB);

            builder_.SetInsertPoint(elemBB);
            llvm::Value *elemPtr = builder_.CreateGEP(setElemTy, sf.elems, {iCur}, "vts_set_elem_ptr");
            llvm::Value *elem = builder_.CreateLoad(setElemTy, elemPtr, "vts_set_elem");
            llvm::Value *elemStr = valueToString(elem);
            builder_.CreateCall(spf, {cachedGlobalString("%s", ".vts_set_s"), elemStr});

            builder_.CreateStore(
                builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(endBB);
            builder_.CreateCall(spf, {cachedGlobalString("}", ".vts_set_rb")});
            return emitSprintEnd("vts_set_str");
        }

        // Map: sprint buffer + IR loop → {key: val, key: val, ...}
        llvm::Type *mapKeyTy = getMapKeyType(val);
        llvm::Type *mapValTy = getMapValueType(val);
        if (mapKeyTy && mapValTy) {
            auto spf = getSprintPrintf();
            emitSprintBegin();

            auto mf = loadMapHeader(val, "vts_map");

            builder_.CreateCall(spf, {cachedGlobalString("{", ".vts_map_lb")});

            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "vts_map.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "vts_map.body", fn_);
            llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, "vts_map.end", fn_);

            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "vts_map_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(condBB);
            llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "i");
            builder_.CreateCondBr(builder_.CreateICmpSLT(iVal, mf.len), bodyBB, endBB);

            builder_.SetInsertPoint(bodyBB);
            llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "i_cur");
            llvm::BasicBlock *commaBB = llvm::BasicBlock::Create(*ctx_, "vts_map.comma", fn_);
            llvm::BasicBlock *kvBB    = llvm::BasicBlock::Create(*ctx_, "vts_map.kv", fn_);
            builder_.CreateCondBr(
                builder_.CreateICmpSGT(iCur, llvm::ConstantInt::get(i64Ty_, 0)),
                commaBB, kvBB);

            builder_.SetInsertPoint(commaBB);
            builder_.CreateCall(spf, {cachedGlobalString(", ", ".vts_map_comma")});
            builder_.CreateBr(kvBB);

            builder_.SetInsertPoint(kvBB);
            llvm::Value *keyPtr = builder_.CreateGEP(mapKeyTy, mf.keys, {iCur}, "vts_map_key_ptr");
            llvm::Value *keyVal = builder_.CreateLoad(mapKeyTy, keyPtr, "vts_map_key");
            llvm::Value *keyStr = valueToString(keyVal);
            builder_.CreateCall(spf, {cachedGlobalString("%s: ", ".vts_map_kv_fmt"), keyStr});

            llvm::Value *valPtr = builder_.CreateGEP(mapValTy, mf.vals, {iCur}, "vts_map_val_ptr");
            llvm::Value *valVal = builder_.CreateLoad(mapValTy, valPtr, "vts_map_val");
            llvm::Value *valStr = valueToString(valVal);
            builder_.CreateCall(spf, {cachedGlobalString("%s", ".vts_map_v_s"), valStr});

            builder_.CreateStore(
                builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(endBB);
            builder_.CreateCall(spf, {cachedGlobalString("}", ".vts_map_rb")});
            return emitSprintEnd("vts_map_str");
        }

        // List: sprint buffer + IR loop → [elem, elem, ...]
        if (llvm::Type *listElemTy = getListElementType(val)) {
            auto spf = getSprintPrintf();
            emitSprintBegin();

            auto lf = loadListHeader(val, "vts_list");

            builder_.CreateCall(spf, {cachedGlobalString("[", ".vts_list_lb")});

            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "vts_list.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "vts_list.body", fn_);
            llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, "vts_list.end", fn_);

            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "vts_list_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(condBB);
            llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "i");
            builder_.CreateCondBr(builder_.CreateICmpSLT(iVal, lf.len), bodyBB, endBB);

            builder_.SetInsertPoint(bodyBB);
            llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "i_cur");
            llvm::BasicBlock *commaBB = llvm::BasicBlock::Create(*ctx_, "vts_list.comma", fn_);
            llvm::BasicBlock *elemBB  = llvm::BasicBlock::Create(*ctx_, "vts_list.elem", fn_);
            builder_.CreateCondBr(
                builder_.CreateICmpSGT(iCur, llvm::ConstantInt::get(i64Ty_, 0)),
                commaBB, elemBB);

            builder_.SetInsertPoint(commaBB);
            builder_.CreateCall(spf, {cachedGlobalString(", ", ".vts_list_comma")});
            builder_.CreateBr(elemBB);

            builder_.SetInsertPoint(elemBB);
            llvm::Value *elemPtr = builder_.CreateGEP(listElemTy, lf.data, {iCur}, "vts_list_elem_ptr");
            llvm::Value *elem = builder_.CreateLoad(listElemTy, elemPtr, "vts_list_elem");
            llvm::Value *elemStr = valueToString(elem);
            builder_.CreateCall(spf, {cachedGlobalString("%s", ".vts_list_s"), elemStr});

            builder_.CreateStore(
                builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1)), iVar);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(endBB);
            builder_.CreateCall(spf, {cachedGlobalString("]", ".vts_list_rb")});
            return emitSprintEnd("vts_list_str");
        }

        if (fn_type_info_.count(val))
            codegenError("cannot convert function to string");
        return val; // string pointer
    }
    auto mallocFn = getStdlibMalloc();
    auto snprintfFn = getStdlibSnprintf();

    if (ty == i1Ty_) {
        llvm::Constant *trueStr = cachedGlobalString("true", ".vts_true");
        llvm::Constant *falseStr = cachedGlobalString("false", ".vts_false");
        return builder_.CreateSelect(val, trueStr, falseStr, "vts_bool");
    }
    if (ty->isDoubleTy()) {
        llvm::Value *buf = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, 64)}, "vts_buf");
        llvm::Constant *fmt = cachedGlobalString("%g", ".vts_float_fmt");
        builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 64), fmt, val});
        return buf;
    }
    // Check low-level type metadata for ambiguous LLVM types
    std::string llName = getLowLevelTypeName(val);

    if (ty == i8Ty_) {
        llvm::Value *buf = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, 32)}, "vts_buf");
        if (llName == "i8") {
            llvm::Constant *fmt = cachedGlobalString("%d", ".vts_i8_fmt");
            llvm::Value *ext = builder_.CreateSExt(val, i32Ty_, "i8_ext");
            builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 32), fmt, ext});
        } else if (llName == "u8") {
            llvm::Constant *fmt = cachedGlobalString("%u", ".vts_u8_fmt");
            llvm::Value *ext = builder_.CreateZExt(val, i32Ty_, "u8_ext");
            builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 32), fmt, ext});
        } else {
            // u8 (default)
            llvm::Constant *fmt = cachedGlobalString("%u", ".vts_u8_def_fmt");
            llvm::Value *ext = builder_.CreateZExt(val, i32Ty_, "u8_def_ext");
            builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 32), fmt, ext});
        }
        return buf;
    }
    if (ty == i16Ty_) {
        llvm::Value *buf = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, 32)}, "vts_buf");
        if (llName == "u16") {
            llvm::Constant *fmt = cachedGlobalString("%u", ".vts_u16_fmt");
            llvm::Value *ext = builder_.CreateZExt(val, i32Ty_, "u16_ext");
            builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 32), fmt, ext});
        } else {
            llvm::Constant *fmt = cachedGlobalString("%d", ".vts_i16_fmt");
            llvm::Value *ext = builder_.CreateSExt(val, i32Ty_, "i16_ext");
            builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 32), fmt, ext});
        }
        return buf;
    }
    if (ty == i32Ty_) {
        llvm::Value *buf = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, 32)}, "vts_buf");
        if (llName == "u32") {
            llvm::Constant *fmt = cachedGlobalString("%u", ".vts_u32_fmt");
            builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 32), fmt, val});
        } else {
            llvm::Constant *fmt = cachedGlobalString("%d", ".vts_i32_fmt");
            builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 32), fmt, val});
        }
        return buf;
    }
    if (ty == f32Ty_) {
        llvm::Value *buf = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, 64)}, "vts_buf");
        llvm::Constant *fmt = cachedGlobalString("%g", ".vts_f32_fmt");
        llvm::Value *ext = builder_.CreateFPExt(val, f64Ty_, "f32_ext");
        builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 64), fmt, ext});
        return buf;
    }
    // default: int (i64) or i64/u64
    llvm::Value *buf = builder_.CreateCall(mallocFn, {llvm::ConstantInt::get(i64Ty_, 32)}, "vts_buf");
    if (llName == "u64") {
        llvm::Constant *fmt = cachedGlobalString("%lu", ".vts_u64_fmt");
        builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 32), fmt, val});
    } else {
        llvm::Constant *fmt = cachedGlobalString("%ld", ".vts_int_fmt");
        builder_.CreateCall(snprintfFn, {buf, llvm::ConstantInt::get(i64Ty_, 32), fmt, val});
    }
    return buf;
}

llvm::Value *CodeGen::structToString(llvm::Value *val) {
    auto *structTy = llvm::cast<llvm::StructType>(val->getType());
    std::string typeName = structTy->getName().str();
    auto it = struct_types_.find(typeName);
    if (it == struct_types_.end())
        codegenError("structToString: unknown record type: " + typeName);

    const auto &info = it->second;

    // Check for user-defined to_str overload
    auto fit = functions_.find("to_str");
    if (fit != functions_.end()) {
        for (auto &entry : fit->second) {
            if (entry.paramTypes.size() == 1 && entry.paramTypes[0] == structTy) {
                return builder_.CreateCall(entry.func, {val}, "user_to_str");
            }
        }
    }

    // Auto-generate: "TypeName(field1: val1, field2: val2)"
    auto strlenFn = getStdlibStrlen();

    using SP = std::pair<llvm::Value*, llvm::Value*>;
    std::vector<SP> parts;

    auto addLiteral = [&](const std::string &s, const char *label) {
        parts.push_back({cachedGlobalString(s, label),
                         llvm::ConstantInt::get(i64Ty_, s.size())});
    };

    addLiteral(typeName + "(", ".sts_prefix");

    for (unsigned i = 0; i < info.fields.size(); ++i) {
        if (i > 0)
            addLiteral(", ", ".sts_sep");
        addLiteral(info.fields[i].name + ": ", ".sts_fname");
        llvm::Value *field = builder_.CreateExtractValue(val, i, info.fields[i].name);
        llvm::Value *fieldStr = valueToString(field);
        parts.push_back({fieldStr, builder_.CreateCall(strlenFn, {fieldStr}, "sts_len")});
    }

    addLiteral(")", ".sts_suffix");

    return concatStringParts(parts, "sts");
}

llvm::Value *CodeGen::tupleToString(llvm::Value *val, llvm::StructType *st) {
    auto strlenFn = getStdlibStrlen();

    using SP = std::pair<llvm::Value*, llvm::Value*>;
    std::vector<SP> parts;

    auto addLiteral = [&](const std::string &s, const char *label) {
        parts.push_back({cachedGlobalString(s, label),
                         llvm::ConstantInt::get(i64Ty_, s.size())});
    };

    unsigned n = st->getNumElements();
    addLiteral("(", ".tts_prefix");
    for (unsigned i = 0; i < n; ++i) {
        if (i > 0)
            addLiteral(", ", ".tts_sep");
        llvm::Value *elem = builder_.CreateExtractValue(val, i, "tts_elem");
        llvm::Value *elemStr = valueToString(elem);
        parts.push_back({elemStr, builder_.CreateCall(strlenFn, {elemStr}, "tts_len")});
    }
    if (n == 1)
        addLiteral(",", ".tts_trail");
    addLiteral(")", ".tts_suffix");

    return concatStringParts(parts, "tts");
}

void CodeGen::emitSprintBegin() {
    builder_.CreateCall(getRuntimeFn("__ry_sprint_begin",
        llvm::Type::getVoidTy(*ctx_), {}));
}

llvm::Value *CodeGen::emitSprintEnd(const llvm::Twine &name) {
    return builder_.CreateCall(
        getRuntimeFn("__ry_sprint_end", ptrTy_, {}), {}, name);
}

llvm::Value *CodeGen::concatStringParts(
    const std::vector<std::pair<llvm::Value*, llvm::Value*>> &parts,
    const std::string &prefix) {
    auto mallocFn = getStdlibMalloc();
    auto memcpyFn = getStdlibMemcpy();

    llvm::Value *totalLen = llvm::ConstantInt::get(i64Ty_, 0);
    for (auto &p : parts)
        totalLen = builder_.CreateAdd(totalLen, p.second, prefix + "_total");

    llvm::Value *bufSize = builder_.CreateAdd(
        totalLen, llvm::ConstantInt::get(i64Ty_, 1), prefix + "_bufsize");
    llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, prefix + "_buf");
    llvm::Value *off = llvm::ConstantInt::get(i64Ty_, 0);
    for (auto &p : parts) {
        llvm::Value *dst = builder_.CreateGEP(
            builder_.getInt8Ty(), buf, {off}, prefix + "_dst");
        builder_.CreateCall(memcpyFn, {dst, p.first, p.second});
        off = builder_.CreateAdd(off, p.second, prefix + "_off");
    }

    llvm::Value *ep = builder_.CreateGEP(
        builder_.getInt8Ty(), buf, {off}, prefix + "_end");
    builder_.CreateStore(llvm::ConstantInt::get(builder_.getInt8Ty(), 0), ep);
    return buf;
}


