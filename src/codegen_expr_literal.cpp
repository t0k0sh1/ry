#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"

void CodeGen::emitStmt(RecordStmt &s) {
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

    // Check invariants after construction
    if (!info.invariants.empty())
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
            return builder_.CreateExtractValue(obj, i, e->field);
        }
    }

    codegenError("type '" + typeName + "' has no field '" + e->field + "'");
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<TupleExpr> &e) {
    std::vector<llvm::Type*> types;
    std::vector<llvm::Value*> vals;
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

    // Allocate list header: { i64 length, i64 capacity, ptr data }
    auto mallocFn = getStdlibMalloc();

    // Allocate header
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
    llvm::Value *headerPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "list_header");

    // Allocate data
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
    llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 0, "len_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, count), lenPtr);

    llvm::Value *capPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 1, "cap_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, count), capPtr);

    llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 2, "data_ptr");
    builder_.CreateStore(dataPtr, dataPtrField);

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

    auto mallocFn = getStdlibMalloc();
    const llvm::DataLayout &dl = mod_->getDataLayout();

    // Allocate MapHeader
    uint64_t headerSize = dl.getTypeAllocSize(mapHeaderTy_);
    llvm::Value *headerPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "map_header");

    // Allocate keys array
    uint64_t keySize = dl.getTypeAllocSize(keyTy);
    llvm::Value *keysPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, keySize * count)}, "map_keys");

    // Allocate values array
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
    llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 0, "map_len_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, count), lenPtr);

    llvm::Value *capPtr = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 1, "map_cap_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, count), capPtr);

    llvm::Value *keysPtrField = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 2, "map_keys_field");
    builder_.CreateStore(keysPtr, keysPtrField);

    llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 3, "map_vals_field");
    builder_.CreateStore(valsPtr, valsPtrField);

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

    auto mallocFn = getStdlibMalloc();
    const llvm::DataLayout &dl = mod_->getDataLayout();

    // Allocate SetHeader
    uint64_t headerSize = dl.getTypeAllocSize(setHeaderTy_);
    llvm::Value *headerPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "set_header");

    // Allocate elements array
    uint64_t elemSize = dl.getTypeAllocSize(elemTy);
    llvm::Value *elemsPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, elemSize * count)}, "set_elems");

    // Store elements
    for (int64_t i = 0; i < count; ++i) {
        llvm::Value *ep = builder_.CreateGEP(elemTy, elemsPtr,
            {llvm::ConstantInt::get(i64Ty_, i)}, "set_elem_ptr");
        builder_.CreateStore(vals[i], ep);
    }

    // Store header fields: length, capacity, elements_ptr
    llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, headerPtr, 0, "set_len_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, count), lenPtr);

    llvm::Value *capPtr = builder_.CreateStructGEP(setHeaderTy_, headerPtr, 1, "set_cap_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, count), capPtr);

    llvm::Value *elemsPtrField = builder_.CreateStructGEP(setHeaderTy_, headerPtr, 2, "set_elems_field");
    builder_.CreateStore(elemsPtr, elemsPtrField);

    // Initialize hash table buckets via rehash
    int64_t initBucketCount = 8;
    while (initBucketCount * 3 < count * 4) initBucketCount *= 2;
    {
        std::string rehashName;
        if (elemTy == ptrTy_) {
            rehashName = "__ry_ht_rehash_str";
        } else if (elemTy->isDoubleTy()) {
            rehashName = "__ry_ht_rehash_f64";
        } else {
            rehashName = "__ry_ht_rehash_i64";
        }
        llvm::FunctionType *rehashTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, i64Ty_, i64Ty_}, false);
        llvm::FunctionCallee rehashFn = mod_->getOrInsertFunction(rehashName, rehashTy);
        llvm::Value *buckets = builder_.CreateCall(rehashFn,
            {elemsPtr, llvm::ConstantInt::get(i64Ty_, count),
             llvm::ConstantInt::get(i64Ty_, initBucketCount)}, "set_buckets");
        llvm::Value *bcPtr = builder_.CreateStructGEP(setHeaderTy_, headerPtr, 3, "set_bc_ptr");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, initBucketCount), bcPtr);
        llvm::Value *bpPtr = builder_.CreateStructGEP(setHeaderTy_, headerPtr, 4, "set_bp_ptr");
        builder_.CreateStore(buckets, bpPtr);
    }

    // Track element type
    type_meta_[TM_SetElem][headerPtr] = elemTy;

    return headerPtr;
}

llvm::Value *CodeGen::emitExprVariant(const EnumAccessExpr &e) {
    // Try to instantiate generic enum if not found
    if (!enum_types_.count(e.enum_name)) {
        auto ltPos = e.enum_name.find('<');
        if (ltPos != std::string::npos && e.enum_name.back() == '>') {
            std::string baseName = e.enum_name.substr(0, ltPos);
            std::string argsStr = e.enum_name.substr(ltPos + 1, e.enum_name.size() - ltPos - 2);
            std::vector<std::string> typeArgs;
            std::string curr;
            int depth = 0;
            for (char c : argsStr) {
                if (c == '<') depth++;
                else if (c == '>') depth--;
                else if (c == ',' && depth == 0) {
                    typeArgs.push_back(curr);
                    curr.clear();
                    continue;
                }
                curr += c;
            }
            if (!curr.empty()) typeArgs.push_back(curr);
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
    llvm::Value *index = emitExpr(*e->index);

    // Fixed-length array index access
    if (auto *ai = llvm::dyn_cast<llvm::AllocaInst>(objPtr)) {
        if (auto *arrTy = llvm::dyn_cast<llvm::ArrayType>(ai->getAllocatedType())) {
            llvm::Type *elemTy = arrTy->getElementType();
            uint64_t arrSize = arrTy->getNumElements();

            emitBoundsCheck(index, llvm::ConstantInt::get(i64Ty_, arrSize),
                            "runtime error: array index out of range\n", ".arr_idx_err", "arr");

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
        return builder_.CreateLoad(mapValTy, valElemPtr, "map_val");
    }

    // List index access
    llvm::Type *elemTy = getListElementType(objPtr);
    if (!elemTy)
        codegenError("cannot determine list element type for index access");

    llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, objPtr, 0, "len_ptr");
    llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "length");

    emitBoundsCheck(index, length,
                    "runtime error: list index out of range\n", ".idx_err", "index");
    llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, objPtr, 2, "data_ptr");
    llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "data");
    llvm::Value *elemPtr = builder_.CreateGEP(elemTy, dataPtr, {index}, "elem_ptr");
    return builder_.CreateLoad(elemTy, elemPtr, "elem");
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
        }
    }

    if (auto *structTy = llvm::dyn_cast<llvm::StructType>(ty)) {
        std::string name = structTy->getName().str();
        if (struct_types_.count(name))
            return structToString(val);
    }

    if (ty->isPointerTy()) {
        // Reject non-string pointer types (collections, function pointers)
        if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
            llvm::Value *src = load->getPointerOperand();
            if (type_meta_[TM_ListElem].count(src) || type_meta_[TM_MapKey].count(src) ||
                type_meta_[TM_SetElem].count(src))
                codegenError("cannot convert collection to string");
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
    auto mallocFn = getStdlibMalloc();
    auto memcpyFn = getStdlibMemcpy();

    // Build string parts with lengths (use constants for literals, strlen for dynamic values)
    struct StringPart { llvm::Value *str; llvm::Value *len; };
    std::vector<StringPart> parts;

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

    // Compute total length
    llvm::Value *totalLen = llvm::ConstantInt::get(i64Ty_, 0);
    for (auto &p : parts)
        totalLen = builder_.CreateAdd(totalLen, p.len, "sts_total");

    // Allocate and concatenate
    llvm::Value *bufSize = builder_.CreateAdd(totalLen, llvm::ConstantInt::get(i64Ty_, 1), "sts_bufsize");
    llvm::Value *buf = builder_.CreateCall(mallocFn, {bufSize}, "sts_buf");
    llvm::Value *offset = llvm::ConstantInt::get(i64Ty_, 0);
    for (auto &p : parts) {
        llvm::Value *dst = builder_.CreateGEP(builder_.getInt8Ty(), buf, {offset}, "sts_dst");
        builder_.CreateCall(memcpyFn, {dst, p.str, p.len});
        offset = builder_.CreateAdd(offset, p.len, "sts_off");
    }

    // Null-terminate
    llvm::Value *endPtr = builder_.CreateGEP(builder_.getInt8Ty(), buf, {offset}, "sts_end");
    builder_.CreateStore(llvm::ConstantInt::get(builder_.getInt8Ty(), 0), endPtr);

    return buf;
}
