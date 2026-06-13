#include "ry/codegen.hpp"
#include "ry/codegen/lowered_bounds_check.hpp"


namespace ry {

void CodeGen::emitStmt(RecordStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    validateDirectives(s.directives, DirectiveTarget::Record);
    for (const auto &f : s.fields)
        validateDirectives(f.directives, DirectiveTarget::Field);
    emitTraceSymbolDefine("record", s.name, s.loc);

    // Qualified-import of user-defined module: register into namespace bucket
    // instead of the flat record_types_, preserving AC4 isolation.
    bool isNamespaced = current_namespace_target_ != nullptr;
    auto &record_table = isNamespaced
        ? current_namespace_target_->records
        : record_types_;

    if (record_table.count(s.name))
        codegenError("redefined type: " + s.name);
    if (!isNamespaced)
        rejectIfTypeNameTakenByOtherKind(s.name);

    std::string parentName;
    std::vector<FieldDef> allFields;

    if (s.parent_name) {
        parentName = *s.parent_name;
        // Parent lookup uses the same scope: namespace bucket if active,
        // flat table otherwise. Inherited records must be defined earlier
        // in the same module.
        auto pit = record_table.find(parentName);
        if (pit == record_table.end())
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
    record_table[s.name] = std::move(info);
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
        llvm::Type *expectedTy = info.llvmType->getElementType(i);
        // #1186: propagate Option<T> inner from field type so None()/none args resolve correctly.
        llvm::Type *argHintInner = nullptr;
        if (isOptionType(expectedTy))
            argHintInner = llvm::cast<llvm::StructType>(expectedTy)->getElementType(1);
        OptionNoneHintGuard argHint(*this, argHintInner);
        llvm::Value *val = emitExpr(*args[i]);
        if (val->getType() != expectedTy)
            codegenError("type '" + name + "': field '" + info.fields[i].name +
                                     "' type mismatch");
        result = builder_.CreateInsertValue(result, val, i);
    }

    emitInvariantCheck(name, info, result);

    return result;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<FieldAccessExpr> &e) {
    // #1746: detect `<mod>.<field>` where `<mod>` names a stdlib package
    // the user forgot to `import` (e.g. `math.PI`, `math.E`). Mirrors the
    // CallExpr-dispatch guard in `codegen_call_dispatch.cpp`. Without this
    // the field access falls through to evaluating `e->object` as a
    // VariableExpr and emits the more generic "undefined variable: <mod>"
    // — true but doesn't tell the user which import to add. Skip when
    // `qualified_module` is set (the user wrote a properly imported
    // `<mod>.<field>`, handled by the qualified branch below). Gate on
    // `!findVar(receiver)` so a local that happens to share a stdlib name
    // (e.g. `path: str = "/tmp"; print(path.length)`) is not misdiagnosed.
    //
    // #1747: when an alias is registered for the canonical module (e.g.
    // `import math as m`), the canonical name is hidden per the
    // Python-style contract. Emit a targeted alias suggestion instead of
    // the misleading "add 'import math'" hint.
    if (!e->qualified_module.has_value()) {
        if (auto *ve = std::get_if<VariableExpr>(&e->object->data)) {
            if (isStdlibPackageName(ve->name) && !findVar(ve->name)) {
                std::string msg;
                if (auto alias = findAliasForCanonical(ve->name)) {
                    msg = "'" + ve->name + "' is not defined. Did you mean '"
                        + *alias + "' (aliased from '" + ve->name + "')?";
                } else {
                    msg = "module '" + ve->name + "' is not imported (add 'import "
                        + ve->name + "' at the top of the file)";
                }
                codegenError(msg);
            }
        }
    }

    // Qualified field access `<effective>.x` (#1723, #1724, #1730).
    //
    // - stdlib module: the inlined top-level binding is reachable by bare
    //   name; route through VariableExpr.
    // - user-defined module: load through the namespace's `consts` bucket
    //   (mirrors the bare-name module-global path at codegen_expr.cpp:170).
    if (e->qualified_module.has_value()) {
        const std::string &mod = *e->qualified_module;
        // findModuleNamespace walks `effective_to_canonical_` so aliases
        // resolve to the canonical-keyed bucket (#1730).
        ModuleNamespaceInfo *ns = findModuleNamespace(mod);
        if (!ns)
            codegenError("qualified access on module '" + mod +
                         "' which was not imported via 'import " + mod + "'");
        if (!ns->is_stdlib) {
            auto cit = ns->consts.find(e->field);
            if (cit == ns->consts.end())
                codegenError("module '" + ns->original_name +
                             "' has no constant '" + e->field + "'");
            const ModuleBinding &b = cit->second;
            if (b.is_weak)
                codegenError("weak top-level variables are not yet accessible from functions (#817 follow-up)");
            if (b.is_resource)
                codegenError("resource-typed top-level variables are not yet accessible from functions (#817 follow-up)");
            auto *storagePtr = loadModuleGlobalStorage(b, e->field);
            llvm::Type *valueTy = b.valueTy();
            if (llvm::isa<llvm::ArrayType>(valueTy)) {
                array_storage_to_alloca_[storagePtr] = b.original_alloca;
                return storagePtr;
            }
            auto *loaded = builder_.CreateLoad(valueTy, storagePtr, e->field);
            propagateMeta(b.original_alloca, loaded);
            return loaded;
        }
        // stdlib: the inlined top-level definition is reachable by bare name.
        VariableExpr proxy{e->field};
        return emitExprVariant(proxy);
    }

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
        llvm::Value *fieldVal = builder_.CreateExtractValue(
            obj, static_cast<unsigned>(idx), "tuple." + e->field);
        // #1664: When the tuple was loaded from List<(K, V)> (or similar),
        // the IndexExpr List path stamps source_type_name on the loaded
        // tuple value. Decompose per-component via splitTupleSig and
        // propagate the matching component's name onto the extracted field
        // so chained access like `xs[0].1[0]` carries List/Map/Set metadata
        // through to the next subscript. Snapshot into a local std::string
        // before propagateTypeMeta to dodge value_metadata_ rehash
        // invalidation (#858).
        std::string srcTupleSig;
        if (auto *objMeta = getMeta(obj))
            srcTupleSig = objMeta->source_type_name;
        if (!srcTupleSig.empty() && srcTupleSig.size() >= 2 &&
            srcTupleSig.front() == '(' && srcTupleSig.back() == ')') {
            auto components = splitTupleSig(srcTupleSig);
            if (idx < components.size() && !components[idx].empty())
                propagateTypeMeta(components[idx], fieldVal);
        }
        return fieldVal;
    }

    std::string typeName = structTy->getName().str();
    // Walk both flat record_types_ and module_namespaces_[*].records by LLVM
    // type identity so qualified-imported records (#1730) are findable from
    // user code emitted with current_namespace_target_ unset.
    const RecordInfo *infoPtr = findRecordInfoForType(structTy);
    if (!infoPtr)
        codegenError("unknown record type: " + typeName);

    const auto &info = *infoPtr;
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
    // Stamp `source_type_name` on the tuple result so downstream consumers
    // (notably `verifyCalledWith`'s record/tuple path, #1706) can recover the
    // per-element type names — LLVM's anonymous StructType layout is lossy
    // for ptr-backed elements (str / List / Map / ... all collapse to ptr).
    // Best-effort: derive each element's source name via metadata; leave the
    // stamp empty when any element name is unknown so consumers fall back to
    // LLVM type inference rather than treating opaque ptrs as str.
    bool allNamesKnown = !e->elements.empty();
    std::string tupleSig = "(";
    for (size_t i = 0; i < vals.size(); ++i) {
        std::string elemName;
        llvm::Type *t = types[i];
        if (t == i64Ty_) elemName = "int";
        else if (t == f64Ty_) elemName = "float";
        else if (t == i1Ty_ || t == i8Ty_) elemName = "bool";
        else if (t == ptrTy_ && isStringValue(vals[i])) elemName = "str";
        else elemName = buildTypeNameFromMeta(vals[i]);
        if (elemName.empty()) { allNamesKnown = false; break; }
        if (i > 0) tupleSig += ", ";
        tupleSig += elemName;
    }
    tupleSig += ")";
    if (allNamesKnown)
        getOrCreateMeta(result).source_type_name = tupleSig;
    return result;
}

CodeGen::LiteralAnyHint CodeGen::computeLiteralAnyHintFromAnnot(const std::string &annot) {
    LiteralAnyHint hint;
    std::string resolved = resolveTypeAlias(annot);
    if (ry::util::isListTypeName(resolved) && resolved.size() >= 7 && resolved.back() == '>') {
        std::string inner = ry::util::trimTypeNameSpaces(resolved.substr(5, resolved.size() - 6));
        if (inner == "any")
            hint.list_elem_any = true;
    } else if (ry::util::isSetTypeName(resolved) && resolved.size() >= 6 && resolved.back() == '>') {
        std::string inner = ry::util::trimTypeNameSpaces(resolved.substr(4, resolved.size() - 5));
        if (inner == "any")
            hint.set_elem_any = true;
    } else if (ry::util::isMapTypeName(resolved) && resolved.size() >= 7 && resolved.back() == '>') {
        std::string inner = resolved.substr(4, resolved.size() - 5);
        auto args = splitTypeArgs(inner);
        if (args.size() == 2) {
            // Map<any, V> is intentionally out of scope for #1884: the rehash
            // dispatch (`__ry_ht_rehash_i64` / `_f64` / `_str`) has no anyTy_
            // (16B struct) variant. Lookup falls back to linear scan and is
            // safe, but the literal-time rehash would write wrong hashes into
            // the bucket array. Forcing map_key_any=false keeps the strict
            // same-type key check; only Map<str, any> / Map<int, any> etc.
            // (concrete key, any value) are widened.
            if (ry::util::trimTypeNameSpaces(args[1]) == "any")
                hint.map_value_any = true;
        }
    }
    return hint;
}

CodeGen::LiteralAnyHint CodeGen::computeLiteralAnyHintFromMeta(llvm::Value *ptr) {
    LiteralAnyHint hint;
    auto *meta = getMeta(ptr);
    if (!meta)
        return hint;
    if (meta->list_elem_type_name == "any")
        hint.list_elem_any = true;
    if (meta->set_elem_type_name == "any")
        hint.set_elem_any = true;
    // map_key_any intentionally not propagated — Map<any, V> is out of scope
    // for #1884 (see computeLiteralAnyHintFromAnnot).
    if (meta->map_value_type_name == "any")
        hint.map_value_any = true;
    return hint;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<ListExpr> &e) {
    if (e->elements.empty())
        codegenError("empty list literal requires type annotation (not yet supported)");

    // #1884: snapshot any-element hint then immediately install an empty hint
    // so nested sub-literals do not inherit the outer axis.
    const bool wrapElemAsAny = literal_any_hint_.list_elem_any;
    LiteralAnyHintGuard innerHintGuard(*this, LiteralAnyHint{});

    // Evaluate all elements
    std::vector<llvm::Value*> vals;
    vals.reserve(e->elements.size());
    for (auto &el : e->elements) {
        llvm::Value *v = emitExpr(*el);
        if (wrapElemAsAny && v->getType() != anyTy_)
            v = wrapInAny(v);
        vals.push_back(v);
    }

    // Check all elements have the same type (skipped when wrapping into any —
    // wrapInAny normalises every element to anyTy_, so a per-element check is
    // redundant).
    llvm::Type *elemTy = vals[0]->getType();
    if (!wrapElemAsAny) {
        for (size_t i = 1; i < vals.size(); ++i) {
            if (vals[i]->getType() != elemTy)
                codegenError("list elements must all have the same type");
        }
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

    // Retain ARC-managed collection elements (List/Map/Set) on store so that
    // the outer list owns an independent strong reference.  Without this,
    // `b: List<List<int>> = [a]; b = [[99]]` would free `a` via the
    // destructor (#1242) even though `a` is still live in the outer scope.
    // str is excluded from this primitive gate; the side-table path below
    // handles str retain (#1354).
    std::string elemTypeName;
    if (elemTy == ptrTy_)
        elemTypeName = inferCollectionTypeName(vals[0]);
    CollectionKind elemArcKind = CollectionKind::Str;
    const bool elemNeedsRetain = !elemTypeName.empty() &&
        fieldTypeIsArcManaged(elemTypeName, &elemArcKind) &&
        elemArcKind != CollectionKind::Str;

    // inferCollectionTypeName returns "" for bare str, so the primitive gate
    // skips it. Side-table lookup via isStrHandle fills the gap (#1354).
    // anyElemIsStrLike mirrors the Map/Set path — immortal literal globals from
    // cachedGlobalString are excluded from arc_str_owned_values_, so isStrHandle
    // misses them. isStringValue covers any ptr-backed value without other
    // metadata, which is the correct stamp signal for list_elem_type_name = "str"
    // (consumed by verifyCalledWith arg recording, etc.).
    bool anyElemIsStr = false;
    bool anyElemIsStrLike = false;

    for (int64_t i = 0; i < count; ++i) {
        const bool elemIsStr =
            elemTy == ptrTy_ && isStrHandle(vals[static_cast<size_t>(i)]);
        anyElemIsStr = anyElemIsStr || elemIsStr;
        if (elemTy == ptrTy_ && isStringValue(vals[static_cast<size_t>(i)]))
            anyElemIsStrLike = true;
        llvm::Value *elemPtr = builder_.CreateGEP(
            elemTy, dataPtr, {llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(i))}, "elem_ptr");
        if (elemNeedsRetain || elemIsStr)
            retainArcValue(vals[static_cast<size_t>(i)]);
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

    // Result/Option struct elements: stamp the full source-level type name so
    // higher-order combinators (sequence, traverse, …) can reconstruct
    // metadata for the unwrapped Ok/Some payload. The struct value carries
    // its inner-payload metadata via propagateMeta from buildOkValue/
    // buildSomeValue (#982/#985), so buildTypeNameFromMeta recovers the
    // inner type name; primitive payloads fall back to reverseResolveTypeName
    // on the slot type.
    if (isResultType(elemTy) || isOptionType(elemTy)) {
        auto *st = llvm::cast<llvm::StructType>(elemTy);
        llvm::Type *innerTy = st->getElementType(1);
        std::string innerName = buildTypeNameFromMeta(vals[0]);
        // reverseResolveTypeName(ptrTy_) lossily collapses to "str" — skip the
        // ptr fallback so we don't stamp a wrong name like Result<str, Error>
        // for a Result<List<int>, Error> payload when buildTypeNameFromMeta
        // failed to recover metadata.
        if (innerName.empty() && innerTy != ptrTy_)
            innerName = reverseResolveTypeName(innerTy);
        if (!innerName.empty()) {
            if (isResultType(elemTy)) {
                llvm::Type *errTy = st->getElementType(2);
                std::string errName;
                if (errTy != ptrTy_)
                    errName = reverseResolveTypeName(errTy);
                if (errName.empty())
                    errName = "Error";
                getOrCreateMeta(headerPtr).list_elem_type_name =
                    "Result<" + innerName + ", " + errName + ">";
            } else {
                getOrCreateMeta(headerPtr).list_elem_type_name =
                    "Option<" + innerName + ">";
            }
        }
    }

    // Track nested list element type (for flat support)
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
        std::optional<FnTypeInfo> elemFnTypeInfo;
        if (elemTypeName.empty()) {
            if (auto *elemMeta = getMeta(vals[0]))
                elemFnTypeInfo = elemMeta->fn_type_info;
        }
        if (!elemTypeName.empty())
            getOrCreateMeta(headerPtr).list_elem_type_name = elemTypeName;
        else if (anyElemIsStr || anyElemIsStrLike)
            getOrCreateMeta(headerPtr).list_elem_type_name = "str";
        else if (elemFnTypeInfo)
            getOrCreateMeta(headerPtr).list_elem_fn_type_info = elemFnTypeInfo;
    }

    // #1884: stamp list_elem_type_name = "any" for List<any> literals.
    // Elements are stored as anyTy_ (struct, not ptrTy_), so the ptr-element
    // branch above does not run.
    if (wrapElemAsAny)
        getOrCreateMeta(headerPtr).list_elem_type_name = "any";

    return headerPtr;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<MapExpr> &e) {
    if (e->keys.empty())
        codegenError("empty map literal requires type annotation");

    // #1884: snapshot any-element hint then immediately install an empty hint
    // so nested sub-literals do not inherit the outer axes.
    const bool wrapKeyAsAny = literal_any_hint_.map_key_any;
    const bool wrapValAsAny = literal_any_hint_.map_value_any;
    LiteralAnyHintGuard innerHintGuard(*this, LiteralAnyHint{});

    // Evaluate all keys and values
    std::vector<llvm::Value*> keyVals, valVals;
    keyVals.reserve(e->keys.size());
    valVals.reserve(e->values.size());
    for (auto &k : e->keys) {
        llvm::Value *v = emitExpr(*k);
        if (wrapKeyAsAny && v->getType() != anyTy_)
            v = wrapInAny(v);
        keyVals.push_back(v);
    }
    for (auto &v : e->values) {
        llvm::Value *val = emitExpr(*v);
        if (wrapValAsAny && val->getType() != anyTy_)
            val = wrapInAny(val);
        valVals.push_back(val);
    }

    // Check all keys have the same type (skipped on the any axis — wrapInAny
    // normalises every key to anyTy_).
    llvm::Type *keyTy = keyVals[0]->getType();
    if (!wrapKeyAsAny) {
        for (size_t i = 1; i < keyVals.size(); ++i) {
            if (keyVals[i]->getType() != keyTy)
                codegenError("map keys must all have the same type");
        }
    }

    // Check all values have the same type (skipped on the any axis — wrapInAny
    // normalises every value to anyTy_).
    llvm::Type *valTy = valVals[0]->getType();
    if (!wrapValAsAny) {
        for (size_t i = 1; i < valVals.size(); ++i) {
            if (valVals[i]->getType() != valTy)
                codegenError("map values must all have the same type");
        }
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

    // Retain ARC-managed collection keys/values on store so the outer map
    // owns independent strong references.  See List literal (#1242).
    std::string keyTypeName;
    if (keyTy == ptrTy_) keyTypeName = inferCollectionTypeName(keyVals[0]);
    std::string valTypeName;
    if (valTy == ptrTy_) valTypeName = inferCollectionTypeName(valVals[0]);
    CollectionKind keyArcKind = CollectionKind::Str;
    const bool keyNeedsRetain = !keyTypeName.empty() &&
        fieldTypeIsArcManaged(keyTypeName, &keyArcKind) &&
        keyArcKind != CollectionKind::Str;
    CollectionKind valArcKind = CollectionKind::Str;
    const bool valNeedsRetain = !valTypeName.empty() &&
        fieldTypeIsArcManaged(valTypeName, &valArcKind) &&
        valArcKind != CollectionKind::Str;

    // inferCollectionTypeName returns "" for bare str, so the primitive gate
    // skips it. Side-table lookup via isStrHandle fills the gap (#1347).
    // Per-entry (not first-entry-only): mixed-origin literals like
    // `{extractvalue_k: v1, bound_k: v2}` have entry 0 escape detection but
    // entry 1+ tracked. Aggregate flags drive the post-loop metadata stamp.
    bool anyKeyIsStr = false;
    bool anyValIsStr = false;
    // Wider signal that catches immortal literal globals (cachedGlobalString)
    // which isStrHandle excludes. Required so verifyCalledWith record path
    // sees Map<str, …> / Map<…, str> from plain literals like {"a": 1} (#1705,
    // mirrors the Set<str> stamp fix in 2def9dc7 / #1707).
    bool anyKeyIsStrLike = false;
    bool anyValIsStrLike = false;

    // Store keys and values
    for (int64_t i = 0; i < count; ++i) {
        const bool keyIsStr =
            keyTy == ptrTy_ && isStrHandle(keyVals[static_cast<size_t>(i)]);
        const bool valIsStr =
            valTy == ptrTy_ && isStrHandle(valVals[static_cast<size_t>(i)]);
        anyKeyIsStr = anyKeyIsStr || keyIsStr;
        anyValIsStr = anyValIsStr || valIsStr;
        if (keyTy == ptrTy_ && isStringValue(keyVals[static_cast<size_t>(i)]))
            anyKeyIsStrLike = true;
        if (valTy == ptrTy_ && isStringValue(valVals[static_cast<size_t>(i)]))
            anyValIsStrLike = true;

        llvm::Value *kp = builder_.CreateGEP(keyTy, keysPtr,
            {llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(i))}, "key_ptr");
        if (keyNeedsRetain || keyIsStr)
            retainArcValue(keyVals[static_cast<size_t>(i)]);
        builder_.CreateStore(keyVals[static_cast<size_t>(i)], kp);
        llvm::Value *vp = builder_.CreateGEP(valTy, valsPtr,
            {llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(i))}, "val_ptr");
        if (valNeedsRetain || valIsStr)
            retainArcValue(valVals[static_cast<size_t>(i)]);
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

    if (keyTy == ptrTy_ && !keyTypeName.empty())
        getOrCreateMeta(headerPtr).map_key_type_name = keyTypeName;
    else if (keyTy == ptrTy_ && (anyKeyIsStr || anyKeyIsStrLike))
        getOrCreateMeta(headerPtr).map_key_type_name = "str";
    if (valTy == ptrTy_ && !valTypeName.empty())
        getOrCreateMeta(headerPtr).map_value_type_name = valTypeName;
    else if (valTy == ptrTy_ && (anyValIsStr || anyValIsStrLike))
        getOrCreateMeta(headerPtr).map_value_type_name = "str";

    // #1884: stamp map_key_type_name / map_value_type_name = "any" for
    // Map<any, V> / Map<K, any> literals. Keys/values are stored as anyTy_
    // (struct, not ptrTy_), so the ptr-based branches above do not fire.
    if (wrapKeyAsAny)
        getOrCreateMeta(headerPtr).map_key_type_name = "any";
    if (wrapValAsAny)
        getOrCreateMeta(headerPtr).map_value_type_name = "any";

    return headerPtr;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<SetExpr> &e) {
    if (e->elements.empty()) {
        // Empty set — requires type annotation (handled in emitVarDecl)
        // If reached here directly, error
        codegenError("empty set literal requires type annotation");
    }

    // #1884: snapshot any-element hint then immediately install an empty hint
    // so nested sub-literals do not inherit the outer axis.
    const bool wrapElemAsAny = literal_any_hint_.set_elem_any;
    LiteralAnyHintGuard innerHintGuard(*this, LiteralAnyHint{});

    // Evaluate all elements
    std::vector<llvm::Value*> vals;
    vals.reserve(e->elements.size());
    for (auto &el : e->elements) {
        llvm::Value *v = emitExpr(*el);
        if (wrapElemAsAny && v->getType() != anyTy_)
            v = wrapInAny(v);
        vals.push_back(v);
    }

    // Check all elements have the same type (skipped on the any axis —
    // wrapInAny normalises every element to anyTy_).
    llvm::Type *elemTy = vals[0]->getType();
    if (!wrapElemAsAny) {
        for (size_t i = 1; i < vals.size(); ++i) {
            if (vals[i]->getType() != elemTy)
                codegenError("set elements must all have the same type");
        }
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

    // Retain ARC-managed collection elements on store.  See List literal
    // (#1242).  str is excluded from this primitive gate; the side-table
    // path below handles str retain (#1354).
    CollectionKind setElemArcKind = CollectionKind::Str;
    const bool setElemNeedsRetain = elemTy == ptrTy_ &&
        !setElemName.empty() &&
        fieldTypeIsArcManaged(setElemName, &setElemArcKind) &&
        setElemArcKind != CollectionKind::Str;

    // inferCollectionTypeName returns "" for bare str, so the primitive gate
    // skips it. Side-table lookup via isStrHandle fills the gap (#1354).
    // Retain fires inside insertBB so dedup-skipped iterations do not
    // double-retain.
    bool anyElemIsStr = false;
    // Track str-typed elements (handles + immortal literal globals) so the
    // post-loop set_elem_type_name = "str" stamp fires for plain literals
    // like {"a", "b"} (#1704). isStrHandle excludes immortal globals from
    // cachedGlobalString, so callers downstream (verifyCalledWith record
    // path) need a wider signal — isStringValue rules out known-non-str
    // pointers (collections, resources, closures) by inspecting metadata.
    bool anyElemIsStrLike = false;

    // Insert elements with deduplication (same pattern as add())
    for (int64_t i = 0; i < count; ++i) {
        if (!setElemName.empty())
            propagateTypeMeta(setElemName, vals[static_cast<size_t>(i)]);
        llvm::Value *idx = emitSetElementLookup(headerPtr, vals[static_cast<size_t>(i)], elemTy, setElemName);
        llvm::Value *found = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "found");

        llvm::BasicBlock *insertBB = createBB("setlit.insert");
        llvm::BasicBlock *nextBB = createBB("setlit.next");
        emitBranchCond(found, nextBB, insertBB);

        builder_.SetInsertPoint(insertBB);
        llvm::Value *curLen = builder_.CreateLoad(i64Ty_, lenPtr, "cur_len");
        llvm::Value *curElems = builder_.CreateLoad(ptrTy_, elemsPtrField, "cur_elems");
        llvm::Value *ep = builder_.CreateGEP(elemTy, curElems, {curLen}, "set_elem_ptr");
        const bool elemIsStr =
            elemTy == ptrTy_ && isStrHandle(vals[static_cast<size_t>(i)]);
        anyElemIsStr = anyElemIsStr || elemIsStr;
        if (elemTy == ptrTy_ && isStringValue(vals[static_cast<size_t>(i)]))
            anyElemIsStrLike = true;
        if (setElemNeedsRetain || elemIsStr)
            retainArcValue(vals[static_cast<size_t>(i)]);
        builder_.CreateStore(vals[static_cast<size_t>(i)], ep);
        llvm::Value *newLen = builder_.CreateAdd(curLen, llvm::ConstantInt::get(i64Ty_, 1), "new_len");
        builder_.CreateStore(newLen, lenPtr);
        emitBucketInsertAndRehashCheck(headerPtr, setHeaderTy_,
            kSetLayout.lenIdx, kSetLayout.bucketCountIdx, kSetLayout.bucketsPtrIdx,
            vals[static_cast<size_t>(i)], elemTy, curLen);
        emitBranchUncond(nextBB);

        builder_.SetInsertPoint(nextBB);
    }

    if (elemTy == ptrTy_ && setElemName.empty() && (anyElemIsStr || anyElemIsStrLike))
        getOrCreateMeta(headerPtr).set_elem_type_name = "str";

    // #1884: stamp set_elem_type_name = "any" for Set<any> literals. Elements
    // are stored as anyTy_ (struct, not ptrTy_), so the ptr-element branch
    // above does not run.
    if (wrapElemAsAny)
        getOrCreateMeta(headerPtr).set_elem_type_name = "any";

    return headerPtr;
}

llvm::Value *CodeGen::emitExprVariant(const EnumAccessExpr &e) {
    // Try to instantiate generic enum if not found
    ensureEnumInstantiated(e.enum_name);
    auto *info = findEnumType(e.enum_name);
    if (!info)
        codegenError("undefined enum: " + e.enum_name);
    auto vit = info->variants.find(e.variant_name);
    if (vit == info->variants.end())
        codegenError("enum '" + e.enum_name + "' has no variant '" + e.variant_name + "'");

    if (info->isADT) {
        // Reject access to payload-carrying variants without arguments
        auto fit = info->variantFields.find(e.variant_name);
        if (fit != info->variantFields.end() && !fit->second.fieldTypes.empty())
            codegenError("variant '" + e.enum_name + "::" + e.variant_name +
                "' requires " + std::to_string(fit->second.fieldTypes.size()) +
                " argument(s); use '" + e.enum_name + "::" + e.variant_name + "(...)' instead");
        // ADT enum: create struct { tag, zero-payload } for data-less variants
        llvm::Value *adtVal = llvm::UndefValue::get(info->adtType);
        adtVal = builder_.CreateInsertValue(adtVal, llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(vit->second)), 0, "adt.tag");
        getOrCreateMeta(adtVal).enum_value_type = e.enum_name;
        return adtVal;
    }

    // Wrap the interned ConstantInt in a Freeze instruction so that the
    // enum_value_type metadata attaches to a unique Value. Attaching metadata
    // directly to the interned constant would leak to unrelated int literals
    // with the same bit pattern (see typeOf enum misidentification test).
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
            if (e->try_mode)
                codegenError("'?' index not supported for range slice");
            if (isStringValue(objPtr))
                codegenError("str does not support range index; use substr(s, a, b) instead");
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

    if (llvm::Value *result = trySubscriptOperatorCall(objPtr, indexValues)) {
        if (e->try_mode)
            codegenError("'?' index not supported for operator[] overload");
        return result;
    }
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
            if (e->try_mode)
                codegenError("'?' index not supported for fixed-length arrays");
            llvm::Type *elemTy = arrTy->getElementType();
            uint64_t arrSize = arrTy->getNumElements();

            if (auto bcOp = codegen::lowering::lowerBoundsCheck(
                    *this, index, llvm::ConstantInt::get(i64Ty_, arrSize),
                    codegen::lowered::BoundsKind::Array, ".arr_idx_err"))
                index = codegen::emission::emitBoundsCheck(*this, *bcOp, "arr");

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

    if (isStringValue(objPtr)) {
        if (e->try_mode)
            codegenError("'?' index not supported for str");
        codegenError("str does not support index access; use charAt(s, i) instead");
    }

    // Check if this is a map
    llvm::Type *mapKeyTy = getMapKeyType(objPtr);
    if (mapKeyTy) {
        llvm::Type *mapValTy = getMapValueType(objPtr);
        if (!mapValTy)
            codegenError("cannot determine map value type");

        // Check key type matches
        if (index->getType() != mapKeyTy)
            codegenError("map key type mismatch");

        // Snapshot the value type name before any call that may rehash
        // value_metadata_ and invalidate getMeta(objPtr).
        std::string mapValTypeName;
        if (auto *mvtnMeta = getMeta(objPtr))
            mapValTypeName = mvtnMeta->map_value_type_name;

        // Lookup key
        llvm::Value *idx = emitMapKeyLookup(objPtr, index, mapKeyTy);

        if (e->try_mode) {
            // #1699: m[k]? returns Option<V> instead of aborting on missing key.
            // #2094 ([C] = (ii) boundary move): structurally identical to the
            // 2-arg emitCollOp_get arm — found/not-found compare, mapHeader vals
            // field struct GEP + load, value GEP + load via generic primitives.
            // emitMapKeyLookup is already boundary-emitted via #2101.
            llvm::StructType *optTy = getOptionType(mapValTy);
            llvm::Value *found = emitICmpSGE(idx, emitConstInt(i64Ty_, 0), "trymap_found");

            llvm::BasicBlock *foundBB = createBB("trymap.found");
            llvm::BasicBlock *notFoundBB = createBB("trymap.notfound");
            llvm::BasicBlock *mergeBB = createBB("trymap.merge");
            emitBranchCond(found, foundBB, notFoundBB);

            builder_.SetInsertPoint(foundBB);
            llvm::Value *valsPtrField = emitStructGEP(mapHeaderTy_, objPtr, 3, "trymap_vals_field");
            llvm::Value *valsPtr = emitLoad(ptrTy_, valsPtrField, "trymap_vals");
            llvm::Value *valPtr = emitGEP(mapValTy, valsPtr, idx, "trymap_val_ptr");
            llvm::Value *foundVal = emitLoad(mapValTy, valPtr, "trymap_val");
            // Propagate element metadata onto foundVal BEFORE buildSomeValue,
            // so the retain inside buildSomeValue picks the right kind
            // (e.g. str_elem → StringHeader at -24).
            if (!mapValTypeName.empty()) {
                propagateTypeMeta(mapValTypeName, foundVal);
                if (mapValTypeName == "str")
                    getOrCreateMeta(foundVal).str_elem = true;
            }
            llvm::Value *someVal = buildSomeValue(foundVal, optTy);
            emitBranchUncond(mergeBB);
            llvm::BasicBlock *foundEndBB = builder_.GetInsertBlock();

            builder_.SetInsertPoint(notFoundBB);
            llvm::Value *noneVal = buildNoneValue(optTy);
            emitBranchUncond(mergeBB);
            llvm::BasicBlock *notFoundEndBB = builder_.GetInsertBlock();

            builder_.SetInsertPoint(mergeBB);
            llvm::PHINode *phi = createPhi(optTy, {}, "trymap_result");
            phi->addIncoming(someVal, foundEndBB);
            phi->addIncoming(noneVal, notFoundEndBB);
            propagateMeta(someVal, phi);
            return phi;
        }

        // Check if found
        llvm::Value *notFound = builder_.CreateICmpSLT(idx, llvm::ConstantInt::get(i64Ty_, 0), "not_found");

        llvm::BasicBlock *failBB = createBB("map.notfound");
        llvm::BasicBlock *okBB = createBB("map.found");

        emitBranchCond(notFound, failBB, okBB);

        // Not found: print error and exit
        builder_.SetInsertPoint(failBB);
        emitRuntimeError("runtime error: map key not found\n", ".map_key_err");

        // Found: get value
        builder_.SetInsertPoint(okBB);
        llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 3, "map_vals_ptr");
        llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "map_vals");
        llvm::Value *valElemPtr = builder_.CreateGEP(mapValTy, valsPtr, {idx}, "val_elem_ptr");
        llvm::Value *mapVal = builder_.CreateLoad(mapValTy, valElemPtr, "map_val");

        if (!mapValTypeName.empty()) {
            propagateTypeMeta(mapValTypeName, mapVal);
            // Map<K,str>: stamp str_elem locally so Case 4 retains via
            // StringHeader (-24). MUST be stamped only in the indexer
            // paths, never from a shared helper like propagateTypeMeta —
            // many of its ~50 call sites pass str values that are NOT
            // container-element borrows, and stamping str_elem there
            // makes retain dispatch through emitStrGetHeaderFromData
            // (val - 24) on a pointer that does not own a StringHeader,
            // corrupting adjacent heap state.
            if (mapValTypeName == "str")
                getOrCreateMeta(mapVal).str_elem = true;
        }

        return mapVal;
    }

    // List index access
    llvm::Type *elemTy = getListElementType(objPtr);
    if (!elemTy)
        codegenError("cannot determine list element type for index access");

    llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, objPtr, 0, "len_ptr");
    llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "length");

    // Snapshot list element metadata BEFORE any call (propagateTypeMeta,
    // setTypeMeta, buildSomeValue) that may rehash value_metadata_ and
    // invalidate the pointer returned by getMeta(objPtr).
    std::string elemTypeName;
    std::optional<FnTypeInfo> elemFnTypeInfo;
    bool listElemIsStr = false;
    if (auto *listMeta = getMeta(objPtr)) {
        elemTypeName   = listMeta->list_elem_type_name;
        elemFnTypeInfo = listMeta->list_elem_fn_type_info;
        listElemIsStr  = listMeta->list_elem_is_str;
    }
    llvm::Type *nestedElemTy = getNestedListElementType(objPtr);

    auto stampLoadedElem = [&](llvm::Value *elem) {
        // Without this, chained indexing like matrix[i][j] loses element type metadata.
        if (nestedElemTy)
            setTypeMeta(TypeMeta::ListElem, elem, nestedElemTy);

        // Propagate Map/Set/closure element metadata (e.g. List<Map<str,int>>, List<closure>).
        if (!elemTypeName.empty())
            propagateTypeMeta(elemTypeName, elem);
        // #1664: Stamp tuple sig (e.g. "(int, T)" from enumerate / zip /
        // items) onto the loaded element via source_type_name so that
        // FieldAccessExpr's numeric-index arm can decompose per-component
        // metadata via splitTupleSig. propagateTypeMeta is single-value by
        // design (see "propagateTypeMeta is single-value; callers decompose
        // tuples"), so the decomposition must happen at the call site that
        // owns the bound field, mirroring the for-loop destructure precedent
        // in src/codegen_stmt_loop.cpp.
        if (!elemTypeName.empty() && elemTypeName.size() >= 2 &&
            elemTypeName.front() == '(' && elemTypeName.back() == ')')
            getOrCreateMeta(elem).source_type_name = elemTypeName;
        if (elemFnTypeInfo)
            getOrCreateMeta(elem).fn_type_info = *elemFnTypeInfo;
        // List<str>: read the list_elem_is_str side-channel set by the
        // AssignStmt annotation parser (and propagated from literal stamps
        // post-#1576). Stamp str_elem directly so Case 4 retains the
        // borrowed handle. Do NOT route through propagateTypeMeta — that
        // would expose the str→str_elem mapping to every caller and cause
        // spurious retains on str-typed pattern bindings / enum variant
        // fields / function returns, crashing under glibc. The historical
        // counter asymmetry between __ry_arc_alloc_counted (+1) and
        // makeString (no-op) that motivated the side-channel was resolved
        // in #1576; the side-channel itself is retained as the indexer
        // discriminant. (#1266, #1576)
        if (listElemIsStr)
            getOrCreateMeta(elem).str_elem = true;
    };

    if (e->try_mode) {
        // #1699: xs[i]? returns Option<T> instead of aborting on OOB.
        // Apply the existing negative-index wrap (matches the abort path),
        // then ICmp-only bounds check (no abort) → PHI Some/None.
        // #2094 ([C] = (ii) boundary move): zext key widening, OOB conjunction,
        // data-ptr struct GEP / load, element GEP / load cross via generic
        // primitives. emitNegativeIndexWrap stays C++-side (index-wrap
        // capability, follow-on).
        if (index->getType() == i1Ty_)
            index = emitZExt(index, i64Ty_, "tryidx_ext");
        llvm::Value *wrapped = emitNegativeIndexWrap(index, length, "tryidx");
        llvm::Value *zero = emitConstInt(i64Ty_, 0);
        llvm::Value *negCheck = emitICmpSLT(wrapped, zero, "tryidx_neg");
        llvm::Value *overCheck = emitICmpSGE(wrapped, length, "tryidx_over");
        llvm::Value *oob = emitOr(negCheck, overCheck, "tryidx_oob");

        llvm::StructType *optTy = getOptionType(elemTy);
        llvm::BasicBlock *oobBB = createBB("trylist.oob");
        llvm::BasicBlock *okBB = createBB("trylist.ok");
        llvm::BasicBlock *mergeBB = createBB("trylist.merge");
        emitBranchCond(oob, oobBB, okBB);

        builder_.SetInsertPoint(okBB);
        llvm::Value *dataPtrField = emitStructGEP(listHeaderTy_, objPtr, 2, "data_ptr");
        llvm::Value *dataPtr = emitLoad(ptrTy_, dataPtrField, "data");
        llvm::Value *elemPtr = emitGEP(elemTy, dataPtr, wrapped, "tryelem_ptr");
        llvm::Value *elem = emitLoad(elemTy, elemPtr, "tryelem");
        stampLoadedElem(elem);
        llvm::Value *someVal = buildSomeValue(elem, optTy);
        emitBranchUncond(mergeBB);
        llvm::BasicBlock *okEndBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(oobBB);
        llvm::Value *noneVal = buildNoneValue(optTy);
        emitBranchUncond(mergeBB);
        llvm::BasicBlock *oobEndBB = builder_.GetInsertBlock();

        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = createPhi(optTy, {}, "trylist_result");
        phi->addIncoming(someVal, okEndBB);
        phi->addIncoming(noneVal, oobEndBB);
        propagateMeta(someVal, phi);
        return phi;
    }

    if (auto bcOp = codegen::lowering::lowerBoundsCheck(
            *this, index, length, codegen::lowered::BoundsKind::List, ".idx_err"))
        index = codegen::emission::emitBoundsCheck(*this, *bcOp, "index");
    llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, objPtr, 2, "data_ptr");
    llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "data");
    llvm::Value *elemPtr = builder_.CreateGEP(elemTy, dataPtr, {index}, "elem_ptr");
    llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "elem");

    stampLoadedElem(elem);

    return elem;
}

} // namespace ry
