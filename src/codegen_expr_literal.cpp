#include "ry/codegen.hpp"


namespace ry {

void CodeGen::emitStmt(RecordStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    validateDirectives(s.directives);
    for (const auto &f : s.fields)
        validateDirectives(f.directives);
    emitTraceSymbolDefine("record", s.name, s.loc);
    if (record_types_.count(s.name))
        codegenError("redefined type: " + s.name);
    rejectIfTypeNameTakenByOtherKind(s.name);

    std::string parentName;
    std::vector<FieldDef> allFields;

    if (s.parent_name) {
        parentName = *s.parent_name;
        auto pit = record_types_.find(parentName);
        if (pit == record_types_.end())
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
    fieldTypes.reserve(allFields.size());
    for (auto &f : allFields)
        fieldTypes.push_back(resolveType(f.type->toString()));

    llvm::StructType *structTy = llvm::StructType::create(*ctx_, fieldTypes, s.name);
    if (hasDirective(s.directives, "deprecated"))
        deprecated_types_.insert(s.name);
    for (auto &f : allFields) {
        if (hasDirective(f.directives, "deprecated"))
            deprecated_fields_.insert(s.name + "." + f.name);
    }

    RecordInfo info{structTy, std::move(allFields), std::move(s.invariants), parentName, next_type_id_++};
    record_types_[s.name] = std::move(info);
}

llvm::Value *CodeGen::emitRecordConstructor(const RecordInfo &info,
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
        codegenError("field access on non-record type");

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
        auto idx = std::stoul(e->field);
        if (idx >= structTy->getNumElements())
            codegenError("tuple index " + e->field + " out of range");
        return builder_.CreateExtractValue(obj, static_cast<unsigned>(idx), "tuple." + e->field);
    }

    std::string typeName = structTy->getName().str();
    auto it = record_types_.find(typeName);
    if (it == record_types_.end())
        codegenError("unknown record type: " + typeName);

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
    llvm::Value *dataSize = llvm::ConstantInt::get(i64Ty_, elemSize * static_cast<uint64_t>(count));
    llvm::Value *dataPtr = builder_.CreateCall(mallocFn, {dataSize}, "list_data");

    // Store elements into data
    for (int64_t i = 0; i < count; ++i) {
        llvm::Value *elemPtr = builder_.CreateGEP(
            elemTy, dataPtr, {llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(i))}, "elem_ptr");
        builder_.CreateStore(vals[static_cast<size_t>(i)], elemPtr);
    }

    // Store length, capacity, data pointer into header
    storeListHeaderFields(headerPtr, llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(count)),
                          llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(count)), dataPtr);

    // Track element type
    setTypeMeta(TypeMeta::ListElem, headerPtr, elemTy);

    // Enum elements (simple i64 or ADT struct) need list_elem_type_name so
    // valueToString() on the list can propagate enum_value_type to each loaded
    // element via propagateTypeMeta (#820). This path runs regardless of the
    // LLVM element type because enum simple values are i64 and ADT enums are
    // structs, neither of which is a pointer type.
    if (auto *firstMeta = getMeta(vals[0]);
            firstMeta && !firstMeta->enum_value_type.empty()) {
        getOrCreateMeta(headerPtr).list_elem_type_name = firstMeta->enum_value_type;
    }

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
                setTypeMeta(TypeMeta::NestedListElem, headerPtr, innerElemTy);
        }

        // Track inferred collection/type names (Map, Set, List, etc.) for metadata
        // propagation on index access. If no named type is inferred, preserve closure
        // function type metadata instead. Snapshot getMeta(vals[0]) fields before any
        // getOrCreateMeta call that may rehash value_metadata_ and invalidate the pointer.
        std::string elemTypeName = inferCollectionTypeName(vals[0]);
        std::optional<FnTypeInfo> elemFnTypeInfo;
        if (elemTypeName.empty()) {
            if (auto *elemMeta = getMeta(vals[0]))
                elemFnTypeInfo = elemMeta->fn_type_info;
        }
        if (!elemTypeName.empty())
            getOrCreateMeta(headerPtr).list_elem_type_name = elemTypeName;
        else if (elemFnTypeInfo)
            getOrCreateMeta(headerPtr).list_elem_fn_type_info = elemFnTypeInfo;
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
        mallocFn, {llvm::ConstantInt::get(i64Ty_, keySize * static_cast<uint64_t>(count))}, "map_keys");

    uint64_t valSize = dl.getTypeAllocSize(valTy);
    llvm::Value *valsPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, valSize * static_cast<uint64_t>(count))}, "map_vals");

    // Store keys and values
    for (int64_t i = 0; i < count; ++i) {
        llvm::Value *kp = builder_.CreateGEP(keyTy, keysPtr,
            {llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(i))}, "key_ptr");
        builder_.CreateStore(keyVals[static_cast<size_t>(i)], kp);
        llvm::Value *vp = builder_.CreateGEP(valTy, valsPtr,
            {llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(i))}, "val_ptr");
        builder_.CreateStore(valVals[static_cast<size_t>(i)], vp);
    }

    // Store header fields: length, capacity, keys_ptr, values_ptr
    storeMapHeaderFields(headerPtr, llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(count)),
                         llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(count)), keysPtr, valsPtr);

    // Initialize hash table buckets via rehash
    int64_t initBucketCount = 8;
    while (initBucketCount * 3 < count * 4) initBucketCount *= 2;
    {
        std::string rehashName;
        if (keyTy == ptrTy_) {
            rehashName = "__ry_ht_rehash_str";
        } else if (keyTy->isDoubleTy()) {
            rehashName = "__ry_ht_rehash_f64";
        } else {
            rehashName = "__ry_ht_rehash_i64";
        }
        llvm::FunctionType *rehashTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, i64Ty_, i64Ty_}, false);
        llvm::FunctionCallee rehashFn = mod_->getOrInsertFunction(rehashName, rehashTy);
        llvm::Value *buckets = builder_.CreateCall(rehashFn,
            {keysPtr, llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(count)),
             llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(initBucketCount))}, "map_buckets");
        llvm::Value *bcPtr = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 4, "map_bc_ptr");
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(initBucketCount)), bcPtr);
        llvm::Value *bpPtr = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 5, "map_bp_ptr");
        builder_.CreateStore(buckets, bpPtr);
    }

    // Track types
    setTypeMeta(TypeMeta::MapKey, headerPtr, keyTy);
    setTypeMeta(TypeMeta::MapValue, headerPtr, valTy);

    if (keyTy == ptrTy_ && !keyVals.empty()) {
        std::string keyTypeName = inferCollectionTypeName(keyVals[0]);
        if (!keyTypeName.empty())
            getOrCreateMeta(headerPtr).map_key_type_name = keyTypeName;
    }
    if (valTy == ptrTy_ && !valVals.empty()) {
        std::string valTypeName = inferCollectionTypeName(valVals[0]);
        if (!valTypeName.empty())
            getOrCreateMeta(headerPtr).map_value_type_name = valTypeName;
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
        mallocFn, {llvm::ConstantInt::get(i64Ty_, elemSize * static_cast<uint64_t>(count))}, "set_elems");

    // Initialize header: length=0, capacity=count, elements pointer
    storeSetHeaderFields(headerPtr, llvm::ConstantInt::get(i64Ty_, 0),
                         llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(count)), elemsPtr);
    llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, headerPtr, 0, "set_len_ptr");
    llvm::Value *elemsPtrField = builder_.CreateStructGEP(setHeaderTy_, headerPtr, 2, "set_elems_field");

    // Initialize empty hash table buckets
    int64_t initBucketCount = 8;
    while (initBucketCount * 3 < count * 4) initBucketCount *= 2;
    emitBucketInit(headerPtr, setHeaderTy_, kSetLayout.bucketCountIdx,
                   kSetLayout.bucketsPtrIdx, initBucketCount);

    // Track element type (must be set before emitSetElementLookup)
    setTypeMeta(TypeMeta::SetElem, headerPtr, elemTy);

    // Pointer-typed elements need set_elem_type_name so emitSetElementLookup
    // takes the structural-equality path instead of hash-by-ptr-as-C-string.
    // All elements must have the same inferred collection type name; a mismatch
    // indicates mixed-kind ptr elements (e.g. List alongside Map) which the Ry
    // type system should have already rejected but we guard here defensively.
    std::string setElemName;
    if (elemTy == ptrTy_ && !vals.empty()) {
        setElemName = inferCollectionTypeName(vals[0]);
        if (!setElemName.empty()) {
            for (size_t i = 1; i < vals.size(); ++i) {
                std::string n = inferCollectionTypeName(vals[i]);
                if (!n.empty() && n != setElemName) {
                    std::string msg = "set literal has inconsistent element types: '";
                    msg += setElemName;
                    msg += "' vs '";
                    msg += n;
                    msg += "'";
                    codegenError(msg);
                }
            }
            getOrCreateMeta(headerPtr).set_elem_type_name = setElemName;
        }
    }

    // Insert elements with deduplication (same pattern as add())
    for (int64_t i = 0; i < count; ++i) {
        if (!setElemName.empty())
            propagateTypeMeta(setElemName, vals[static_cast<size_t>(i)]);
        llvm::Value *idx = emitSetElementLookup(headerPtr, vals[static_cast<size_t>(i)], elemTy, setElemName);
        llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "found");

        llvm::BasicBlock *insertBB = llvm::BasicBlock::Create(*ctx_, "setlit.insert", fn_);
        llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "setlit.next", fn_);
        builder_.CreateCondBr(found, nextBB, insertBB);

        builder_.SetInsertPoint(insertBB);
        llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "cur_len");
        llvm::Value *curElems = builder_.CreateLoad(ptrTy_, elemsPtrField, "cur_elems");
        llvm::Value *ep = builder_.CreateGEP(elemTy, curElems, {curLen}, "set_elem_ptr");
        builder_.CreateStore(vals[static_cast<size_t>(i)], ep);
        llvm::Value *newLen = builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1), "new_len");
        builder_.CreateStore(newLen, lenPtr);
        emitBucketInsertAndRehashCheck(headerPtr, setHeaderTy_,
            kSetLayout.lenIdx, kSetLayout.bucketCountIdx, kSetLayout.bucketsPtrIdx,
            vals[static_cast<size_t>(i)], elemTy, curLen);
        builder_.CreateBr(nextBB);

        builder_.SetInsertPoint(nextBB);
    }

    return headerPtr;
}

llvm::Value *CodeGen::emitExprVariant(const EnumAccessExpr &e) {
    // Try to instantiate generic enum if not found
    ensureEnumInstantiated(e.enum_name);
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
        adtVal = builder_.CreateInsertValue(adtVal, llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(vit->second)), 0, "adt.tag");
        getOrCreateMeta(adtVal).enum_value_type = e.enum_name;
        return adtVal;
    }

    // Wrap the interned ConstantInt in a Freeze instruction so that the
    // enum_value_type metadata attaches to a unique Value. Attaching metadata
    // directly to the interned constant would leak to unrelated int literals
    // with the same bit pattern (see type_of enum misidentification test).
    llvm::Value *val = builder_.CreateFreeze(
        llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(vit->second)),
        "enum." + e.enum_name + "." + e.variant_name);
    getOrCreateMeta(val).enum_value_type = e.enum_name;
    return val;
}


llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<IndexExpr> &e) {
    llvm::Value *objPtr = emitExpr(*e->object);

    // #1184: lst[a..b] arrives as IndexExpr{ object, indices:[RangeExpr{a,b}] }.
    // Without this guard, emitExpr(RangeExpr) would materialize an ARC-allocated
    // List<i64> header (ptr), flowing into emitBoundsCheck/GEP and producing
    // invalid IR (ICmp ptr vs i64). Route to the shared slice helper instead.
    if (e->indices.size() == 1 && objPtr->getType() == ptrTy_) {
        if (const auto *rp = std::get_if<std::unique_ptr<RangeExpr>>(&e->indices[0]->data)) {
            if (isStringValue(objPtr))
                codegenError("str does not support range index; use substring(s, a, b) instead");
            llvm::Type *elemTy = getListElementType(objPtr);
            if (!elemTy)
                codegenError("range index requires a list");
            const auto &range = **rp;
            llvm::Value *startRaw = emitExpr(*range.start);
            llvm::Value *endRaw   = emitExpr(*range.end);
            llvm::Value *length = loadListHeader(objPtr, "ri").len;
            llvm::Value *startWrapped = emitNegativeIndexWrap(startRaw, length, "ri_start");
            llvm::Value *endWrapped   = emitNegativeIndexWrap(endRaw, length, "ri_end");
            // `..` is inclusive; emitListSlice takes [start, endExcl). Convert.
            // Guard against INT64_MAX overflow: if endWrapped >= length, skip
            // the +1 (emitListSlice will clamp to length regardless).
            llvm::Value *endExcl = builder_.CreateSelect(
                builder_.CreateICmpSGE(endWrapped, length),
                length,
                builder_.CreateAdd(endWrapped, llvm::ConstantInt::get(i64Ty_, 1), "ri_end_add"),
                "ri_end_excl");
            return emitListSlice(objPtr, startWrapped, endExcl, elemTy);
        }
    }

    llvm::SmallVector<llvm::Value*, 2> indexValues;
    for (auto &idx : e->indices)
        indexValues.push_back(emitExpr(*idx));

    if (llvm::Value *result = trySubscriptOperatorCall(objPtr, indexValues))
        return result;
    if (indexValues.size() > 1)
        codegenError("multi-index requires operator[] overload");

    llvm::Value *index = indexValues[0];

    // Fixed-length array index access. The original path recognizes array
    // values only through an `AllocaInst` object pointer. Top-level array
    // bindings reached from a function body (#817) come in as a LoadInst
    // (the storage pointer loaded from the module-global trampoline), so we
    // also consult `array_storage_to_alloca_` — a side-table that maps such
    // loaded pointers back to the original alloca. Metadata lookups (e.g.
    // `array_elem_type_names_`) then operate on the original alloca, while
    // the GEP is issued against the actual runtime storage pointer.
    llvm::AllocaInst *ai = llvm::dyn_cast<llvm::AllocaInst>(objPtr);
    llvm::Value *arrPtr = ai;
    if (!ai) {
        auto it = array_storage_to_alloca_.find(objPtr);
        if (it != array_storage_to_alloca_.end()) {
            ai = it->second;
            arrPtr = objPtr;
        }
    }
    if (ai) {
        if (auto *arrTy = llvm::dyn_cast<llvm::ArrayType>(ai->getAllocatedType())) {
            llvm::Type *elemTy = arrTy->getElementType();
            uint64_t arrSize = arrTy->getNumElements();

            emitBoundsCheck(index, llvm::ConstantInt::get(i64Ty_, arrSize),
                            "runtime error: index %lld out of bounds for array of length %lld\n", ".arr_idx_err", "arr");

            llvm::Value *elemPtr = builder_.CreateGEP(
                arrTy, arrPtr, {llvm::ConstantInt::get(i64Ty_, 0), index}, "arr_elem_ptr");
            llvm::Value *result = builder_.CreateLoad(elemTy, elemPtr, "arr_elem");

            auto ait = array_elem_type_names_.find(ai);
            if (ait != array_elem_type_names_.end())
                getOrCreateMeta(result).low_level_type_name = ait->second;

            return result;
        }
    }

    if (objPtr->getType() != ptrTy_)
        codegenError("index operator requires list or map");

    if (isStringValue(objPtr))
        codegenError("str does not support index access; use char_at(s, i) instead");

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

        auto *mvtnMeta = getMeta(objPtr);
        if (mvtnMeta && !mvtnMeta->map_value_type_name.empty())
            propagateTypeMeta(mvtnMeta->map_value_type_name, mapVal);

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
        setTypeMeta(TypeMeta::ListElem, elem, nestedElemTy);

    // Propagate Map/Set/closure element metadata (e.g. List<Map<str,int>>, List<closure>)
    // Copy metadata fields before any call that may rehash value_metadata_ and invalidate
    // the pointer returned by getMeta().
    {
        std::string elemTypeName;
        std::optional<FnTypeInfo> elemFnTypeInfo;
        if (auto *listMeta = getMeta(objPtr)) {
            elemTypeName   = listMeta->list_elem_type_name;
            elemFnTypeInfo = listMeta->list_elem_fn_type_info;
        }
        if (!elemTypeName.empty())
            propagateTypeMeta(elemTypeName, elem);
        if (elemFnTypeInfo)
            getOrCreateMeta(elem).fn_type_info = *elemFnTypeInfo;
    }

    return elem;
}

} // namespace ry
