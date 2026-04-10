#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"
#include "ry/diagnostic.hpp"
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>


namespace ry {

// ===== Directive helpers =====

void CodeGen::emitDeprecationWarning(const std::string &name) {
    warnings_.push_back("warning: '" + name + "' is deprecated");
}

// ===== B3: emitVarDecl =====

void CodeGen::emitVarDecl(const std::string &name,
                           const TypeNodePtr &type_annotation,
                           ExprNode &value, bool is_immutable) {
    if (scope_stack_.back().count(name))
        codegenError("redeclared variable: " + name);

    // Convert TypeNodePtr to string for codegen (Phase 1 bridge)
    std::optional<std::string> annot;
    if (type_annotation)
        annot = type_annotation->toString();

    // Handle empty set/map literal with type annotation
    if (auto *se = std::get_if<std::unique_ptr<SetExpr>>(&value.data); se && (*se)->elements.empty()) {
        if (!annot)
            codegenError("empty {} literal requires type annotation");
        if (isSetTypeName(*annot)) {
            std::string inner = annot->substr(4, annot->size() - 5);
            llvm::Type *elemTy = resolveType(inner);

            const llvm::DataLayout &dl = mod_->getDataLayout();

            // Allocate SetHeader with ARC
            llvm::Value *headerPtr = emitArcAllocCollectionHeader(setHeaderTy_);

            // Initial capacity = 4
            auto mallocFn = getStdlibMalloc();
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);
            llvm::Value *elemsPtr = builder_.CreateCall(
                mallocFn, {llvm::ConstantInt::get(i64Ty_, elemSize * 4)}, "empty_set_elems");

            storeSetHeaderFields(headerPtr, llvm::ConstantInt::get(i64Ty_, 0),
                                 llvm::ConstantInt::get(i64Ty_, 4), elemsPtr);
            emitBucketInit(headerPtr, setHeaderTy_, kSetLayout.bucketCountIdx, kSetLayout.bucketsPtrIdx, 8);

            llvm::AllocaInst *ptr = getOrCreateVar(name, ptrTy_);
            builder_.CreateStore(headerPtr, ptr);
            setTypeMeta(TypeMeta::SetElem, ptr, elemTy);
            markArcManaged(ptr);
            if (is_immutable)
                immutable_scope_stack_.back().insert(name);
            return;
        }
        if (isMapTypeName(*annot)) {
            auto [keyTy, valTy] = parseMapTypeAnnotation(*annot);
            if (!keyTy || !valTy)
                codegenError("invalid map type annotation: " + *annot);

            const llvm::DataLayout &dl = mod_->getDataLayout();

            // Allocate MapHeader with ARC
            llvm::Value *headerPtr = emitArcAllocCollectionHeader(mapHeaderTy_);

            auto mallocFn = getStdlibMalloc();
            uint64_t keySize = dl.getTypeAllocSize(keyTy);
            uint64_t valSize = dl.getTypeAllocSize(valTy);
            llvm::Value *keysPtr = builder_.CreateCall(
                mallocFn, {llvm::ConstantInt::get(i64Ty_, keySize * 4)}, "empty_map_keys");
            llvm::Value *valsPtr = builder_.CreateCall(
                mallocFn, {llvm::ConstantInt::get(i64Ty_, valSize * 4)}, "empty_map_vals");

            storeMapHeaderFields(headerPtr, llvm::ConstantInt::get(i64Ty_, 0),
                                 llvm::ConstantInt::get(i64Ty_, 4), keysPtr, valsPtr);
            emitBucketInit(headerPtr, mapHeaderTy_, kMapLayout.bucketCountIdx, kMapLayout.bucketsPtrIdx, 8);

            llvm::AllocaInst *ptr = getOrCreateVar(name, ptrTy_);
            builder_.CreateStore(headerPtr, ptr);
            setTypeMeta(TypeMeta::MapKey, ptr, keyTy);
            setTypeMeta(TypeMeta::MapValue, ptr, valTy);
            {
                std::string vtn = extractMapValueTypeName(*annot);
                if (!vtn.empty()) getOrCreateMeta(ptr).map_value_type_name = vtn;
            }
            markArcManaged(ptr);
            if (is_immutable)
                immutable_scope_stack_.back().insert(name);
            return;
        }
        codegenError("empty {} requires Set<T> or Map<K, V> type annotation");
    }

    // Handle empty list literal: xs: List<int> = []
    if (auto *le = std::get_if<std::unique_ptr<ListExpr>>(&value.data); le && (*le)->elements.empty()) {
        if (!annot)
            codegenError("empty list literal requires a List<T> type annotation");
        std::string resolvedAnnot = resolveTypeAlias(*annot);
        if (!isListTypeName(resolvedAnnot) || resolvedAnnot.size() < 7 || resolvedAnnot.back() != '>')
            codegenError("empty list literal requires a List<T> type annotation");
        std::string inner = resolvedAnnot.substr(5, resolvedAnnot.size() - 6);
        llvm::Type *elemTy = resolveType(inner);

        const llvm::DataLayout &dl = mod_->getDataLayout();

        llvm::Value *headerPtr = emitArcAllocCollectionHeader(listHeaderTy_);

        auto mallocFn = getStdlibMalloc();
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        llvm::Value *elemsPtr = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, elemSize * 4)}, "empty_list_elems");

        storeListHeaderFields(headerPtr, llvm::ConstantInt::get(i64Ty_, 0),
                              llvm::ConstantInt::get(i64Ty_, 4), elemsPtr);

        llvm::AllocaInst *ptr = getOrCreateVar(name, ptrTy_);
        builder_.CreateStore(headerPtr, ptr);
        setTypeMeta(TypeMeta::ListElem, ptr, elemTy);
        markArcManaged(ptr);
        arc_backed_vars_.insert(ptr);

        // Set nested-list metadata for List<List<T>> annotations
        if (isListTypeName(inner) && inner.back() == '>') {
            std::string nestedInner = inner.substr(5, inner.size() - 6);
            llvm::Type *nestedElemTy = resolveType(nestedInner);
            if (nestedElemTy)
                setTypeMeta(TypeMeta::NestedListElem, ptr, nestedElemTy);
        }

        // Set list element type metadata for List<Map>, List<Set>, List<closure> annotations
        if (isMapTypeName(inner) || isSetTypeName(inner))
            getOrCreateMeta(ptr).list_elem_type_name = inner;
        else if (inner.size() > 9 && inner.substr(0, 9) == "function(")
            getOrCreateMeta(ptr).list_elem_fn_type_info = parseFnTypeAnnotation(inner);

        if (is_immutable)
            immutable_scope_stack_.back().insert(name);
        return;
    }

    if (isNoneLiteral(value)) {
        if (!annot)
            codegenError("type annotation required for None");
        llvm::Type *annotTy = resolveType(*annot);
        if (!isOptionType(annotTy))
            codegenError("None can only be assigned to Option type");
        llvm::Value *val = buildNoneValue(annotTy);
        llvm::AllocaInst *ptr = getOrCreateVar(name, annotTy);
        builder_.CreateStore(val, ptr);
        if (is_immutable)
            immutable_scope_stack_.back().insert(name);
        return;
    }

    // Resolve type alias and parse constraint once for the entire function
    std::string resolvedAnnot;
    std::optional<TypeConstraint> constraint;
    if (annot) {
        resolvedAnnot = resolveTypeAlias(*annot);
        constraint = parseTypeConstraint(resolvedAnnot);

        // Pre-emit compile-time check for string literal constraints
        if (constraint && constraint->kind == TypeConstraint::Kind::StrLiteral) {
            if (auto *se = std::get_if<StringExpr>(&value.data)) {
                bool found = false;
                for (auto &allowed : constraint->str_values) {
                    if (se->value == allowed) { found = true; break; }
                }
                if (!found) {
                    std::string allowed_str;
                    for (size_t i = 0; i < constraint->str_values.size(); ++i) {
                        if (i > 0) allowed_str += " | ";
                        allowed_str += "\"" + constraint->str_values[i] + "\"";
                    }
                    codegenError(
                        "value \"" + se->value + "\" is not in literal type " + allowed_str +
                        " for variable '" + name + "'");
                }
            }
        }
    }

    // Fixed-length array declaration: T[N] = [elem, ...]
    if (annot && !annot->empty() && annot->back() == ']') {
        size_t bracketPos = annot->find('[');
        if (bracketPos != std::string::npos && bracketPos > 0) {
            llvm::Type *annotTy = resolveType(*annot);
            auto *arrTy = llvm::dyn_cast<llvm::ArrayType>(annotTy);
            if (!arrTy) codegenError("invalid array type: " + *annot);

            llvm::Type *elemTy = arrTy->getElementType();
            uint64_t arrSize = arrTy->getNumElements();

            std::string elemTypeName = annot->substr(0, bracketPos);

            auto *le = std::get_if<std::unique_ptr<ListExpr>>(&value.data);
            if (!le)
                codegenError("array type requires list literal initializer");
            if ((*le)->elements.size() != arrSize)
                codegenError("array size mismatch: expected " + std::to_string(arrSize) +
                             " elements, got " + std::to_string((*le)->elements.size()));

            llvm::AllocaInst *ptr = getOrCreateVar(name, arrTy);

            for (uint64_t i = 0; i < arrSize; ++i) {
                llvm::Value *elemVal = emitExpr(*(*le)->elements[i]);
                if (elemVal->getType() != elemTy) {
                    llvm::Value *coerced = coerceToLowLevelType(
                        elemVal, elemTy, elemTypeName,
                        " at index " + std::to_string(i), "arr_trunc");
                    if (coerced) {
                        elemVal = coerced;
                    } else {
                        codegenError("array element type mismatch at index " + std::to_string(i));
                    }
                }
                llvm::Value *elemPtr = builder_.CreateGEP(
                    arrTy, ptr, {llvm::ConstantInt::get(i64Ty_, 0), llvm::ConstantInt::get(i64Ty_, i)}, "arr_init");
                builder_.CreateStore(elemVal, elemPtr);
            }

            array_elem_type_names_[ptr] = elemTypeName;
            if (is_immutable)
                immutable_scope_stack_.back().insert(name);
            return;
        }
    }

    llvm::Value *val = emitExpr(value);
    llvm::Type *newTy = val->getType();

    if (annot) {
        if (constraint) {
            // Literal/range type: resolve to base type and check constraint
            llvm::Type *annotTy = resolveType(resolvedAnnot);
            if (annotTy != newTy)
                codegenError(
                    "type error: annotation '" + *annot +
                    "' does not match expression type for variable '" + name + "'");
            emitConstraintCheck(val, *constraint, name);
        } else {
            llvm::Type *annotTy = resolveType(*annot);
            if (annotTy != newTy) {
                if (llvm::Value *coerced = coerceToLowLevelType(
                        val, annotTy, *annot, "", *annot + "trunc")) {
                    val = coerced;
                    newTy = annotTy;
                } else if (isOptionType(annotTy) && isOptionType(newTy) &&
                           std::holds_alternative<NoneExpr>(value.data)) {
                    // Allow none coercion to target Option type
                    val = buildNoneValue(annotTy);
                    newTy = annotTy;
                } else if (isOptionType(annotTy) && !isOptionType(newTy)) {
                    // Auto-wrap non-Option value in Some() (e.g., x: int? = 42)
                    auto *optTy = llvm::cast<llvm::StructType>(annotTy);
                    llvm::Type *innerTy = optTy->getElementType(1);
                    if (val->getType() != innerTy)
                        codegenError(
                            "type error: annotation '" + *annot +
                            "' does not match expression type for variable '" + name + "'");
                    val = buildSomeValue(val, annotTy);
                    newTy = annotTy;
                } else if (isAnyType(annotTy)) {
                    val = wrapInAny(val);
                    newTy = anyTy_;
                } else if (isAnyType(newTy) && canAnyHoldType(annotTy)) {
                    val = unwrapFromAny(val, annotTy);
                    newTy = annotTy;
                } else if (isUnionType(resolvedAnnot)) {
                    val = wrapInUnion(val, resolvedAnnot);
                    newTy = val->getType();
                } else {
                    codegenError(
                        "type error: annotation '" + *annot +
                        "' does not match expression type for variable '" + name + "'");
                }
            }
        }
    }

    llvm::AllocaInst *ptr = getOrCreateVar(name, newTy);
    builder_.CreateStore(val, ptr);

    // Track low-level type metadata
    if (annot) {
        const std::string &ann = *annot;
        if (isLowLevelTypeName(ann))
            getOrCreateMeta(ptr).low_level_type_name = ann;
    } else {
        // Propagate metadata from initializer expression (e.g., y = x as u32)
        std::string valName = getLowLevelTypeName(val);
        // Fall back to AST suffix for literal constants, since ConstantInt/ConstantFP
        // pointers are shared by LLVM and cannot carry per-use metadata (#311).
        if (valName.empty())
            valName = getExprLowLevelSuffix(value);
        if (!valName.empty())
            getOrCreateMeta(ptr).low_level_type_name = valName;
    }

    // Track type constraint for reassignment checks
    if (constraint)
        getOrCreateMeta(ptr).type_constraint = *constraint;

    // Track union value type (skip literal unions which use base types directly)
    if (annot && isUnionType(resolvedAnnot) && !constraint) {
        getOrCreateMeta(ptr).union_value_type = normalizeUnionType(resolvedAnnot);
    }

    // Track collection metadata for Option/Result wrapping a collection
    // (e.g., Option<Map<str, str>>, Result<List<int>, Error>)
    if (isOptionType(newTy) || isResultType(newTy)) {
        propagateMeta(val, ptr);
        // Extract inner collection type from Option/Result wrapping a collection
        if (annot &&
            !getTypeMeta(TypeMeta::MapKey, ptr) &&
            !getTypeMeta(TypeMeta::ListElem, ptr) &&
            !getTypeMeta(TypeMeta::SetElem, ptr) &&
            !getTypeMeta(TypeMeta::TaskResult, ptr)) {
            std::string ann = *annot;
            std::string inner;
            if (ann.size() > 7 && ann.substr(0, 7) == "Option<" && ann.back() == '>')
                inner = ann.substr(7, ann.size() - 8);
            else if (ann.size() > 7 && ann.substr(0, 7) == "Result<" && ann.back() == '>') {
                // Extract first type param: Result<Map<K,V>, E> → Map<K,V>
                std::string params = ann.substr(7, ann.size() - 8);
                int depth = 0;
                for (size_t i = 0; i < params.size(); ++i) {
                    if (params[i] == '<') ++depth;
                    else if (params[i] == '>') --depth;
                    else if (params[i] == ',' && depth == 0) {
                        inner = params.substr(0, i);
                        break;
                    }
                }
            }
            if (!inner.empty())
                propagateTypeMeta(inner, ptr);
        }
    }

    // Track list/map element types if this is a ptr value
    if (newTy == ptrTy_) {
        // --- List tracking ---
        llvm::Type *elemTy = getListElementType(val);
        if (!elemTy && annot && isListTypeName(*annot)) {
            std::string inner = annot->substr(5, annot->size() - 6);
            elemTy = resolveType(inner);
        }
        if (elemTy)
            setTypeMeta(TypeMeta::ListElem, ptr, elemTy);

        // --- List element type name tracking (for List<Map>, List<Set>, List<closure>) ---
        {
            auto *valMeta = getMeta(val);
            std::string letn;
            std::optional<FnTypeInfo> lefti;
            if (valMeta) {
                if (!valMeta->list_elem_type_name.empty())
                    letn = valMeta->list_elem_type_name;
                if (valMeta->list_elem_fn_type_info)
                    lefti = valMeta->list_elem_fn_type_info;
            }
            // Also derive from annotation: List<Map<str, int>> → inner = "Map<str, int>"
            if (letn.empty() && !lefti && annot) {
                std::string resolved = resolveTypeAlias(*annot);
                if (isListTypeName(resolved) && resolved.size() >= 7 && resolved.back() == '>') {
                    std::string inner = resolved.substr(5, resolved.size() - 6);
                    while (!inner.empty() && inner.front() == ' ') inner = inner.substr(1);
                    if (isMapTypeName(inner) || isSetTypeName(inner))
                        letn = inner;
                    else if (inner.size() > 9 && inner.substr(0, 9) == "function(")
                        lefti = parseFnTypeAnnotation(inner);
                }
            }
            if (!letn.empty())
                getOrCreateMeta(ptr).list_elem_type_name = letn;
            if (lefti)
                getOrCreateMeta(ptr).list_elem_fn_type_info = lefti;
        }

        // --- Nested list tracking (for flatten) ---
        {
            llvm::Type *nestedTy = getTypeMeta(TypeMeta::NestedListElem, val);
            if (!nestedTy) {
                if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val))
                    nestedTy = getTypeMeta(TypeMeta::NestedListElem, load->getPointerOperand());
            }
            if (nestedTy)
                setTypeMeta(TypeMeta::NestedListElem, ptr, nestedTy);
        }

        // --- Map tracking ---
        llvm::Type *keyTy = getTypeMeta(TypeMeta::MapKey, val);
        llvm::Type *valTy = getTypeMeta(TypeMeta::MapValue, val);
        // From variable load
        if (!keyTy) {
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
                keyTy = getTypeMeta(TypeMeta::MapKey, load->getPointerOperand());
                valTy = getTypeMeta(TypeMeta::MapValue, load->getPointerOperand());
            }
        }
        // From type annotation: Map<K, V>
        if (!keyTy && annot && isMapTypeName(*annot)) {
            std::tie(keyTy, valTy) = parseMapTypeAnnotation(*annot);
        }
        if (keyTy) setTypeMeta(TypeMeta::MapKey, ptr, keyTy);
        if (valTy) setTypeMeta(TypeMeta::MapValue, ptr, valTy);
        {
            auto *valMeta = getMeta(val);
            std::string mvtn;
            if (valMeta && !valMeta->map_value_type_name.empty()) {
                mvtn = valMeta->map_value_type_name;
            } else if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
                auto *loadMeta = getMeta(load->getPointerOperand());
                if (loadMeta && !loadMeta->map_value_type_name.empty())
                    mvtn = loadMeta->map_value_type_name;
            }
            if (!mvtn.empty())
                getOrCreateMeta(ptr).map_value_type_name = mvtn;
        }

        // --- Set tracking ---
        llvm::Type *setElemTy = getSetElementType(val);
        if (!setElemTy) {
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
                setElemTy = getSetElementType(load->getPointerOperand());
            }
        }
        if (!setElemTy && annot && isSetTypeName(*annot)) {
            std::string inner = annot->substr(4, annot->size() - 5);
            setElemTy = resolveType(inner);
        }
        if (setElemTy)
            setTypeMeta(TypeMeta::SetElem, ptr, setElemTy);

        // --- Task tracking ---
        llvm::Type *taskTy = getTaskResultType(val);
        if (!taskTy && annot && annot->size() > 5 &&
            annot->substr(0, 5) == "Task<" && annot->back() == '>') {
            std::string inner = annot->substr(5, annot->size() - 6);
            taskTy = resolveType(inner);
        }
        if (taskTy)
            setTypeMeta(TypeMeta::TaskResult, ptr, taskTy);

        // --- Function pointer tracking ---
        {
            auto *valMeta = getMeta(val);
            if (valMeta && valMeta->fn_type_info) {
                getOrCreateMeta(ptr).fn_type_info = *valMeta->fn_type_info;
            } else if (annot) {
                if (resolvedAnnot.size() > 9 && resolvedAnnot.substr(0, 9) == "function(") {
                    getOrCreateMeta(ptr).fn_type_info = parseFnTypeAnnotation(resolvedAnnot);
                }
            }
        }

        // --- Iterator tracking ---
        {
            llvm::Type *iterElemTy = getIteratorElementType(val);
            if (!iterElemTy) {
                if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val))
                    iterElemTy = getIteratorElementType(load->getPointerOperand());
            }
            if (iterElemTy)
                setTypeMeta(TypeMeta::IteratorElem, ptr, iterElemTy);
        }

        // --- Weak reference tracking ---
        if (annot && isWeakTypeName(*annot)) {
            if (!std::get_if<std::unique_ptr<WeakExpr>>(&value.data))
                codegenError("weak-typed variable must be initialized with a 'weak' expression");
            emitWeakRetain(val);
            markWeakManaged(ptr);
            std::string innerName = weakInnerTypeName(*annot);
            weak_inner_type_names_[ptr] = innerName;
            // Set collection metadata on weak alloca so it propagates through upgrade
            propagateTypeMeta(innerName, ptr);
        }
        // --- ARC tracking ---
        else {
        bool isCollection = getTypeMeta(TypeMeta::ListElem, ptr) ||
                            getTypeMeta(TypeMeta::MapKey, ptr) ||
                            getTypeMeta(TypeMeta::SetElem, ptr);
        bool isArcOwned = arc_owned_values_.count(val) > 0;
        auto detectedRK = detectResourceKind(val);
        bool isResource = (detectedRK != ResourceKindRegistry::NONE);
        bool isRetainedArc = tryRetainArcSource(val);
        // Detect closures with captures (ARC-managed closure structs)
        bool isClosure = false;
        {
            auto *fnMeta = getMeta(val);
            if (!fnMeta) {
                if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val))
                    fnMeta = getMeta(load->getPointerOperand());
            }
            if (fnMeta && fnMeta->fn_type_info &&
                (!fnMeta->fn_type_info->capturedVars.empty() || fnMeta->fn_type_info->isUniformClosure)) {
                isClosure = true;
                getOrCreateMeta(ptr).fn_type_info = *fnMeta->fn_type_info; // propagate FnTypeInfo to alloca
            }
        }
        if (isCollection || isArcOwned || isResource || isRetainedArc || isClosure) {
            markArcManaged(ptr);
            if (isResource)
                resource_managed_vars_[ptr] = detectedRK;
            if (isClosure)
                closure_managed_vars_.insert(ptr);
        }
        // Track allocas that are truly ARC-backed (have ARC header prepended)
        if (isArcOwned || isRetainedArc)
            arc_backed_vars_.insert(ptr);
        }
    }

    // --- Resource type tracking ---
    // These must be outside the ptrTy_ guard because resources can be
    // wrapped in Result<T, Error> structs (e.g., http_get() returns a struct).
    propagateMetaWide(val, ptr);
    if (annot)
        registerResourceByTypeName(*annot, ptr);

    // --- Enum value tracking (works for i64 values, not just ptr) ---
    {
        auto *evMeta = getMeta(val);
        if (evMeta && !evMeta->enum_value_type.empty())
            getOrCreateMeta(ptr).enum_value_type = evMeta->enum_value_type;
        else if (annot && enum_types_.count(*annot))
            getOrCreateMeta(ptr).enum_value_type = *annot;
    }

    if (is_immutable)
        immutable_scope_stack_.back().insert(name);
}

void CodeGen::emitStmt(ExprStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    emitCoverage(s.loc);
    llvm::Value *val = emitExpr(*s.expr);

    // Release ARC-owned temporaries that are not stored into a variable.
    // Without this, collection operation results (appended, slice, etc.)
    // and other ARC-owned values would leak when used as bare statements.
    if (val && val->getType() == ptrTy_ && arc_owned_values_.count(val)) {
        auto *hdr = emitArcGetHeaderFromData(val);
        llvm::FunctionCallee dtor = {};
        if (getTypeMeta(TypeMeta::ListElem, val))
            dtor = getOrCreateCollectionDestructor(CollectionKind::List);
        else if (getTypeMeta(TypeMeta::MapKey, val))
            dtor = getOrCreateCollectionDestructor(CollectionKind::Map);
        else if (getTypeMeta(TypeMeta::SetElem, val))
            dtor = getOrCreateCollectionDestructor(CollectionKind::Set);
        emitArcRelease(hdr, false, dtor);
        arc_owned_values_.erase(val);
    }
}

void CodeGen::emitStmt(AssignStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    emitCoverage(s.loc);
    validateDirectives(s.directives);
    bool is_const = hasDirective(s.directives, "const");
    bool is_native = hasDirective(s.directives, "native");

    // @native @const declaration
    if (is_native && !s.value) {
        if (isNativeConstant(s.name)) {
            native_constants_.insert(s.name);
        } else {
            codegenError("unsupported native constant: " + s.name);
        }
        return;
    }

    // Reject assignment to native constants
    if (native_constants_.count(s.name))
        codegenError("cannot reassign native constant: " + s.name);

    llvm::AllocaInst *ptr = findVar(s.name);
    if (!ptr) {
        emitTraceSymbolDefine("variable", s.name, s.loc);
        emitVarDecl(s.name, s.type_annotation, *s.value, is_const);
        if (hasDirective(s.directives, "deprecated"))
            deprecated_variables_.insert(s.name);
        return;
    }

    if (s.type_annotation)
        codegenError("type annotation not allowed on reassignment: " + s.name);
    if (is_const)
        codegenError("@const not allowed on reassignment: " + s.name);
    if (!captured_vars_.empty() && isCapturedVar(ptr))
        codegenError("cannot modify captured variable '" + s.name + "' inside closure");
    if (isImmutable(s.name))
        codegenError("cannot reassign @const variable: " + s.name);

    // Compound assignment resolution: operator+= → operator+ → built-in
    if (s.compound_op) {
        llvm::Value *currentVal = builder_.CreateLoad(ptr->getAllocatedType(), ptr, s.name);
        llvm::Value *rhs = emitExpr(*s.value);

        // Priority 1: user-defined compound assignment operator (e.g., operator+=)
        std::string compoundOpName = "operator" + *s.compound_op + "=";
        llvm::Value *result = tryOperatorCall(compoundOpName, currentVal, rhs);

        if (!result) {
            // Priority 2: user-defined binary operator or built-in (e.g., operator+)
            std::string rhsHint = getExprLowLevelSuffix(*s.value);
            result = emitBinaryOp(*s.compound_op, currentVal, rhs, "", rhsHint);
        }

        // Type compatibility check (same as plain assignment path)
        llvm::Type *varTy = ptr->getAllocatedType();
        if (result->getType() != varTy) {
            if (varTy == i8Ty_ && result->getType() == i64Ty_) {
                result = builder_.CreateTrunc(result, i8Ty_, "bytetrunc");
            } else if (isAnyType(varTy)) {
                result = wrapInAny(result);
            } else {
                codegenError("type error: compound assignment on '" + s.name +
                    "' produces incompatible type");
            }
        }

        builder_.CreateStore(result, ptr);
        return;
    }

    // Handle None literal in assignment
    if (auto *ve = std::get_if<VariableExpr>(&s.value->data); ve && ve->name == "None") {
        llvm::Type *varTy = ptr->getAllocatedType();
        if (!isOptionType(varTy))
            codegenError("None can only be assigned to Option type");
        llvm::Value *val = buildNoneValue(varTy);
        builder_.CreateStore(val, ptr);
        return;
    }

    llvm::Value *val = emitExpr(*s.value);
    llvm::Type *newTy = val->getType();

    if (ptr->getAllocatedType() != newTy) {
        if (llvm::Value *coerced = coerceToLowLevelType(
                val, ptr->getAllocatedType(), getLowLevelTypeName(ptr),
                "", "i8trunc")) {
            val = coerced;
        } else if (isAnyType(ptr->getAllocatedType())) {
            val = wrapInAny(val);
            newTy = val->getType();
        } else if (isAnyType(newTy) && canAnyHoldType(ptr->getAllocatedType())) {
            val = unwrapFromAny(val, ptr->getAllocatedType());
            newTy = val->getType();
        } else {
            auto *uvMeta = getMeta(ptr);
            if (uvMeta && !uvMeta->union_value_type.empty()) {
                val = wrapInUnion(val, uvMeta->union_value_type);
            } else {
                codegenError(
                    "type error: variable '" + s.name +
                    "' cannot be reassigned to a different type");
            }
        }
    }

    // Check type constraint on reassignment
    {
        auto *tcMeta = getMeta(ptr);
        if (tcMeta && tcMeta->type_constraint) {
            emitConstraintCheck(val, *tcMeta->type_constraint, s.name);
        }
    }

    // Weak ref reassignment: retain new, release old
    if (isWeakManaged(ptr)) {
        if (!std::get_if<std::unique_ptr<WeakExpr>>(&s.value->data))
            codegenError("weak variable must be reassigned with a 'weak' expression");
        emitWeakRetain(val);
        auto *oldVal = builder_.CreateLoad(ptrTy_, ptr, s.name + ".weak_old");
        emitWeakRelease(oldVal);
    }
    // ARC: retain new value before releasing old to avoid use-after-free on self-assignment
    else if (isArcManaged(ptr)) {
        tryRetainArcSource(val);
        auto *oldVal = builder_.CreateLoad(ptrTy_, ptr, s.name + ".arc_old");
        // Null check: old value may have been nullified by explicit free/close
        auto *isOldNull = builder_.CreateICmpEQ(
            oldVal,
            llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
            s.name + ".arc_old_null");
        auto *parentFn = builder_.GetInsertBlock()->getParent();
        auto *releaseBB = llvm::BasicBlock::Create(*ctx_, s.name + ".arc_release", parentFn);
        auto *storeBB = llvm::BasicBlock::Create(*ctx_, s.name + ".arc_store", parentFn);
        builder_.CreateCondBr(isOldNull, storeBB, releaseBB);

        builder_.SetInsertPoint(releaseBB);
        auto *oldHdr = emitArcGetHeaderFromData(oldVal);
        // Look up GC visit function for potentially cyclic types on reassignment.
        llvm::Function *gcVisitFn = nullptr;
        {
            auto *evMeta = getMeta(ptr);
            if (evMeta && !evMeta->enum_value_type.empty() && isPotentiallyCyclic(evMeta->enum_value_type)) {
                gcVisitFn = getOrCreateVisitFunction(evMeta->enum_value_type);
            }
        }
        emitArcRelease(oldHdr, isArcAtomic(oldVal), resolveDestructor(ptr), gcVisitFn);
        builder_.CreateBr(storeBB);

        builder_.SetInsertPoint(storeBB);
    }

    builder_.CreateStore(val, ptr);

    // Propagate fn_type_info_
    if (newTy == ptrTy_) {
        auto *fnMeta = getMeta(val);
        if (fnMeta && fnMeta->fn_type_info)
            getOrCreateMeta(ptr).fn_type_info = *fnMeta->fn_type_info;
        llvm::Type *taskTy = getTaskResultType(val);
        if (taskTy)
            setTypeMeta(TypeMeta::TaskResult, ptr, taskTy);
    }
    // Resource tracking: must be outside ptrTy_ guard for Result-wrapped types
    propagateMetaWide(val, ptr);
}

} // namespace ry
