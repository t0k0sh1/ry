#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

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
            uint64_t headerSize = dl.getTypeAllocSize(setHeaderTy_);
            auto *arcHdr = emitArcAlloc(llvm::ConstantInt::get(i64Ty_, headerSize));
            llvm::Value *headerPtr = emitArcGetDataPtr(arcHdr);

            // Initial capacity = 4
            auto mallocFn = getStdlibMalloc();
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);
            llvm::Value *elemsPtr = builder_.CreateCall(
                mallocFn, {llvm::ConstantInt::get(i64Ty_, elemSize * 4)}, "empty_set_elems");

            llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, headerPtr, 0);
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), lenPtr);
            llvm::Value *capPtr = builder_.CreateStructGEP(setHeaderTy_, headerPtr, 1);
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 4), capPtr);
            llvm::Value *elemsPtrField = builder_.CreateStructGEP(setHeaderTy_, headerPtr, 2);
            builder_.CreateStore(elemsPtr, elemsPtrField);
            emitBucketInit(headerPtr, setHeaderTy_, kSetLayout.bucketCountIdx, kSetLayout.bucketsPtrIdx, 8);

            llvm::AllocaInst *ptr = getOrCreateVar(name, ptrTy_);
            builder_.CreateStore(headerPtr, ptr);
            type_meta_[TM_SetElem][ptr] = elemTy;
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
            uint64_t headerSize = dl.getTypeAllocSize(mapHeaderTy_);
            auto *arcHdr = emitArcAlloc(llvm::ConstantInt::get(i64Ty_, headerSize));
            llvm::Value *headerPtr = emitArcGetDataPtr(arcHdr);

            auto mallocFn = getStdlibMalloc();
            uint64_t keySize = dl.getTypeAllocSize(keyTy);
            uint64_t valSize = dl.getTypeAllocSize(valTy);
            llvm::Value *keysPtr = builder_.CreateCall(
                mallocFn, {llvm::ConstantInt::get(i64Ty_, keySize * 4)}, "empty_map_keys");
            llvm::Value *valsPtr = builder_.CreateCall(
                mallocFn, {llvm::ConstantInt::get(i64Ty_, valSize * 4)}, "empty_map_vals");

            llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 0);
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), lenPtr);
            llvm::Value *capPtr = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 1);
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 4), capPtr);
            llvm::Value *keysPtrField = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 2);
            builder_.CreateStore(keysPtr, keysPtrField);
            llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 3);
            builder_.CreateStore(valsPtr, valsPtrField);
            emitBucketInit(headerPtr, mapHeaderTy_, kMapLayout.bucketCountIdx, kMapLayout.bucketsPtrIdx, 8);

            llvm::AllocaInst *ptr = getOrCreateVar(name, ptrTy_);
            builder_.CreateStore(headerPtr, ptr);
            type_meta_[TM_MapKey][ptr] = keyTy;
            type_meta_[TM_MapValue][ptr] = valTy;
            {
                std::string vtn = extractMapValueTypeName(*annot);
                if (!vtn.empty()) map_value_type_names_[ptr] = vtn;
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

        uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
        auto *arcHdr = emitArcAlloc(llvm::ConstantInt::get(i64Ty_, headerSize));
        llvm::Value *headerPtr = emitArcGetDataPtr(arcHdr);

        auto mallocFn = getStdlibMalloc();
        uint64_t elemSize = dl.getTypeAllocSize(elemTy);
        llvm::Value *elemsPtr = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, elemSize * 4)}, "empty_list_elems");

        llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 0);
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), lenPtr);
        llvm::Value *capPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 1);
        builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 4), capPtr);
        llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 2);
        builder_.CreateStore(elemsPtr, dataPtrField);

        llvm::AllocaInst *ptr = getOrCreateVar(name, ptrTy_);
        builder_.CreateStore(headerPtr, ptr);
        type_meta_[TM_ListElem][ptr] = elemTy;
        markArcManaged(ptr);
        arc_backed_vars_.insert(ptr);

        // Set nested-list metadata for List<List<T>> annotations
        if (isListTypeName(inner) && inner.back() == '>') {
            std::string nestedInner = inner.substr(5, inner.size() - 6);
            llvm::Type *nestedElemTy = resolveType(nestedInner);
            if (nestedElemTy)
                type_meta_[TM_NestedListElem][ptr] = nestedElemTy;
        }

        if (is_immutable)
            immutable_scope_stack_.back().insert(name);
        return;
    }

    // Handle None literal (VariableExpr("None") or NoneExpr)
    bool isNone = std::holds_alternative<NoneExpr>(value.data) ||
                  (std::holds_alternative<VariableExpr>(value.data) &&
                   std::get<VariableExpr>(value.data).name == "None");
    if (isNone) {
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
        if (constraint && constraint->kind == TypeConstraint::StrLiteral) {
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
                } else if (isUnionType(*annot)) {
                    val = wrapInUnion(val, *annot);
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
            low_level_type_names_[ptr] = ann;
    } else {
        // Propagate metadata from initializer expression (e.g., y = x as u32)
        std::string valName = getLowLevelTypeName(val);
        // Fall back to AST suffix for literal constants, since ConstantInt/ConstantFP
        // pointers are shared by LLVM and cannot carry per-use metadata (#311).
        if (valName.empty())
            valName = getExprLowLevelSuffix(value);
        if (!valName.empty())
            low_level_type_names_[ptr] = valName;
    }

    // Track type constraint for reassignment checks
    if (constraint)
        type_constraints_[ptr] = *constraint;

    // Track union value type (skip literal unions which use base types directly)
    if (annot && isUnionType(*annot) && !constraint) {
        union_value_types_[ptr] = normalizeUnionType(*annot);
    }

    // Track collection metadata for Option/Result wrapping a collection
    // (e.g., Option<Map<str, str>>, Result<List<int>, Error>)
    if (isOptionType(newTy) || isResultType(newTy)) {
        propagateCollectionMetadata(val, ptr);
        // Extract inner collection type from Option/Result wrapping a collection
        if (annot &&
            !type_meta_[TM_MapKey].count(ptr) &&
            !type_meta_[TM_ListElem].count(ptr) &&
            !type_meta_[TM_SetElem].count(ptr) &&
            !type_meta_[TM_TaskResult].count(ptr)) {
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
            type_meta_[TM_ListElem][ptr] = elemTy;

        // --- Nested list tracking (for flatten) ---
        {
            auto nit = type_meta_[TM_NestedListElem].find(val);
            if (nit != type_meta_[TM_NestedListElem].end())
                type_meta_[TM_NestedListElem][ptr] = nit->second;
            else if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
                auto nit2 = type_meta_[TM_NestedListElem].find(load->getPointerOperand());
                if (nit2 != type_meta_[TM_NestedListElem].end())
                    type_meta_[TM_NestedListElem][ptr] = nit2->second;
            }
        }

        // --- Map tracking ---
        llvm::Type *keyTy = nullptr;
        llvm::Type *valTy = nullptr;
        // Direct mapping (from MapExpr)
        auto mk = type_meta_[TM_MapKey].find(val);
        if (mk != type_meta_[TM_MapKey].end()) keyTy = mk->second;
        auto mv = type_meta_[TM_MapValue].find(val);
        if (mv != type_meta_[TM_MapValue].end()) valTy = mv->second;
        // From variable load
        if (!keyTy) {
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
                auto mk2 = type_meta_[TM_MapKey].find(load->getPointerOperand());
                if (mk2 != type_meta_[TM_MapKey].end()) keyTy = mk2->second;
                auto mv2 = type_meta_[TM_MapValue].find(load->getPointerOperand());
                if (mv2 != type_meta_[TM_MapValue].end()) valTy = mv2->second;
            }
        }
        // From type annotation: Map<K, V>
        if (!keyTy && annot && isMapTypeName(*annot)) {
            std::tie(keyTy, valTy) = parseMapTypeAnnotation(*annot);
        }
        if (keyTy) type_meta_[TM_MapKey][ptr] = keyTy;
        if (valTy) type_meta_[TM_MapValue][ptr] = valTy;
        {
            auto mvtn = map_value_type_names_.find(val);
            if (mvtn == map_value_type_names_.end()) {
                if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val))
                    mvtn = map_value_type_names_.find(load->getPointerOperand());
            }
            if (mvtn != map_value_type_names_.end())
                map_value_type_names_[ptr] = mvtn->second;
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
            type_meta_[TM_SetElem][ptr] = setElemTy;

        // --- Task tracking ---
        llvm::Type *taskTy = getTaskResultType(val);
        if (!taskTy && annot && annot->size() > 5 &&
            annot->substr(0, 5) == "Task<" && annot->back() == '>') {
            std::string inner = annot->substr(5, annot->size() - 6);
            taskTy = resolveType(inner);
        }
        if (taskTy)
            type_meta_[TM_TaskResult][ptr] = taskTy;

        // --- Function pointer tracking ---
        auto fnIt = fn_type_info_.find(val);
        if (fnIt != fn_type_info_.end()) {
            fn_type_info_[ptr] = fnIt->second;
        } else if (annot) {
            if (resolvedAnnot.size() > 9 && resolvedAnnot.substr(0, 9) == "function(") {
                fn_type_info_[ptr] = parseFnTypeAnnotation(resolvedAnnot);
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
                type_meta_[TM_IteratorElem][ptr] = iterElemTy;
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
        bool isCollection = type_meta_[TM_ListElem].count(ptr) ||
                            type_meta_[TM_MapKey].count(ptr) ||
                            type_meta_[TM_SetElem].count(ptr);
        bool isArcOwned = arc_owned_values_.count(val) > 0;
        auto detectedRK = detectResourceKind(val);
        bool isResource = (detectedRK != RK_COUNT);
        bool isRetainedArc = tryRetainArcSource(val);
        // Detect closures with captures (ARC-managed closure structs)
        bool isClosure = false;
        {
            auto fnIt = fn_type_info_.find(val);
            // Also check LoadInst source (e.g., g = f where f is a closure variable)
            if (fnIt == fn_type_info_.end()) {
                if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val))
                    fnIt = fn_type_info_.find(load->getPointerOperand());
            }
            if (fnIt != fn_type_info_.end() && !fnIt->second.capturedVars.empty()) {
                isClosure = true;
                fn_type_info_[ptr] = fnIt->second; // propagate FnTypeInfo to alloca
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
    propagateResourceTrackingWide(val, ptr);
    if (annot)
        registerResourceByTypeName(*annot, ptr);

    // --- Enum value tracking (works for i64 values, not just ptr) ---
    {
        auto evIt = enum_value_types_.find(val);
        if (evIt != enum_value_types_.end())
            enum_value_types_[ptr] = evIt->second;
        else if (annot && enum_types_.count(*annot))
            enum_value_types_[ptr] = *annot;
    }

    if (is_immutable)
        immutable_scope_stack_.back().insert(name);
}

void CodeGen::emitStmt(AssignStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    emitCoverage(s.loc);
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
            result = emitBinaryOp(*s.compound_op, currentVal, rhs);
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
            auto uvIt = union_value_types_.find(ptr);
            if (uvIt != union_value_types_.end()) {
                val = wrapInUnion(val, uvIt->second);
            } else {
                codegenError(
                    "type error: variable '" + s.name +
                    "' cannot be reassigned to a different type");
            }
        }
    }

    // Check type constraint on reassignment
    auto tcIt = type_constraints_.find(ptr);
    if (tcIt != type_constraints_.end()) {
        emitConstraintCheck(val, tcIt->second, s.name);
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
        auto evIt = enum_value_types_.find(ptr);
        if (evIt != enum_value_types_.end() && isPotentiallyCyclic(evIt->second)) {
            gcVisitFn = getOrCreateVisitFunction(evIt->second);
        }
        emitArcRelease(oldHdr, isArcAtomic(oldVal), resolveDestructor(ptr), gcVisitFn);
        builder_.CreateBr(storeBB);

        builder_.SetInsertPoint(storeBB);
    }

    builder_.CreateStore(val, ptr);

    // Propagate fn_type_info_
    if (newTy == ptrTy_) {
        auto fnIt = fn_type_info_.find(val);
        if (fnIt != fn_type_info_.end())
            fn_type_info_[ptr] = fnIt->second;
        llvm::Type *taskTy = getTaskResultType(val);
        if (taskTy)
            type_meta_[TM_TaskResult][ptr] = taskTy;
    }
    // Resource tracking: must be outside ptrTy_ guard for Result-wrapped types
    propagateResourceTrackingWide(val, ptr);
}


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
