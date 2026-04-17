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

// ===== Result coercion helper =====

llvm::Value *CodeGen::coerceResultType(llvm::Value *val,
                                        llvm::StructType *dstResTy) {
    auto *srcResTy = llvm::cast<llvm::StructType>(val->getType());

    llvm::Type *srcOkTy  = srcResTy->getElementType(1);
    llvm::Type *srcErrTy = srcResTy->getElementType(2);
    llvm::Type *dstOkTy  = dstResTy->getElementType(1);
    llvm::Type *dstErrTy = dstResTy->getElementType(2);

    if (srcOkTy == dstOkTy && srcErrTy == dstErrTy)
        return val; // no rebuild needed

    // Both payload types differ: genuine type mismatch.
    if (srcOkTy != dstOkTy && srcErrTy != dstErrTy)
        return nullptr;

    llvm::Value *disc = builder_.CreateExtractValue(val, 0, "res.disc");

    // ConstantAggregateZero zeroes all fields; only the disc and the active
    // (matching) payload need explicit InsertValue — the inactive slot stays 0.
    llvm::Value *coerced = llvm::ConstantAggregateZero::get(dstResTy);
    coerced = builder_.CreateInsertValue(coerced, disc, 0);
    if (srcOkTy == dstOkTy)
        coerced = builder_.CreateInsertValue(
            coerced, builder_.CreateExtractValue(val, 1, "res.ok"), 1);
    else
        coerced = builder_.CreateInsertValue(
            coerced, builder_.CreateExtractValue(val, 2, "res.err"), 2);

    propagateMeta(val, coerced);
    return coerced;
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
            {
                const std::string resolvedInner = resolveTypeAlias(inner);
                if (isFunctionTypeName(resolvedInner))
                    getOrCreateMeta(ptr).set_elem_fn_type_info = parseFnTypeAnnotation(resolvedInner);
                else if (isListTypeName(resolvedInner) || isMapTypeName(resolvedInner) || isSetTypeName(resolvedInner))
                    getOrCreateMeta(ptr).set_elem_type_name = resolvedInner;
            }
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
                std::string ktn = resolveTypeAlias(extractMapKeyTypeName(*annot));
                if (!ktn.empty()) {
                    getOrCreateMeta(ptr).map_key_type_name = ktn;
                    if (isFunctionTypeName(ktn))
                        getOrCreateMeta(ptr).map_key_fn_type_info = parseFnTypeAnnotation(ktn);
                }
            }
            {
                std::string vtn = extractMapValueTypeName(*annot);
                if (!vtn.empty()) {
                    getOrCreateMeta(ptr).map_value_type_name = vtn;
                    if (isFunctionTypeName(vtn))
                        getOrCreateMeta(ptr).map_value_fn_type_info = parseFnTypeAnnotation(vtn);
                }
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
            // Also record the surface-level element type name so compound-op
            // dispatch (which reads list_elem_type_name via getMeta → propagate)
            // can re-derive the inner List's concat semantics on loaded slot
            // values (#858). Symmetric to the List<Map>/List<Set> branch below.
            getOrCreateMeta(ptr).list_elem_type_name = inner;
        }

        // Set list element type metadata. Also covers low-level int names
        // ("i8", "u8", …) so AssignStmt (#1085) can recover them faithfully.
        if (isMapTypeName(inner) || isSetTypeName(inner) || isLowLevelIntTypeName(inner))
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
                // Inject the element's low-level suffix so `buf: u64[1] =
                // [18446744073709551615]` is validated against u64 instead
                // of bare int (mirrors the scalar emitVarDecl path).
                injectLowLevelSuffix(*(*le)->elements[i], elemTypeName);
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

    // #1079: List<T> annotation + non-empty ListExpr — propagate the element
    // type name into each NumberExpr/UnaryExpr element before emitExpr so
    // emitExpr(ListExpr) stamps TypeMeta::ListElem = T (not i64). Mirrors the
    // fixed-size array path above. Non-recursive: List<List<u8>> inner
    // elements stay i64 (cosmetic; #1055 gate checks only top-level elem type).
    if (annot) {
        if (auto *le = std::get_if<std::unique_ptr<ListExpr>>(&value.data);
                le && !(*le)->elements.empty()) {
            std::string resolved = resolveTypeAlias(*annot);
            if (isListTypeName(resolved) && resolved.size() >= 7 &&
                    resolved.back() == '>') {
                std::string inner = resolved.substr(5, resolved.size() - 6);
                while (!inner.empty() && inner.front() == ' ')
                    inner.erase(0, 1);
                while (!inner.empty() && inner.back() == ' ')
                    inner.pop_back();
                if (isLowLevelIntTypeName(inner))
                    injectListExprElemSuffixes(**le, inner);
            }
        }
    }

    // Propagate a low-level integer annotation onto bare integer literals
    // in the initializer so the codegen range check runs against the target
    // type (required for u64 max literals that don't fit in bare i64).
    if (annot)
        injectLowLevelSuffix(value, *annot);

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
                } else if (isResultType(annotTy) && isResultType(newTy)) {
                    auto *dstResTy = llvm::cast<llvm::StructType>(annotTy);
                    llvm::Value *resCoerced = coerceResultType(val, dstResTy);
                    if (resCoerced) {
                        val = resCoerced;
                        newTy = annotTy;
                    } else {
                        codegenError(
                            "type error: annotation '" + *annot +
                            "' does not match expression type for variable '" + name + "'");
                    }
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

    // Record ARC field retain (#854 Layer 2). When declaring a variable
    // of a record type that has at least one ARC-managed field, the
    // *source* of the stored value decides whether ownership is "moved"
    // (fresh construction from a constructor call / insertvalue chain →
    // no retain, the record alloca becomes the sole owner) or "shared"
    // (view of existing state via LoadInst or ExtractValueInst → retain
    // each ARC field so both aliases can observe strong_count > 1 and
    // path CoW at write time correctly clones before mutation).
    //
    // Regardless of retain vs move, any record alloca with ARC fields
    // must be registered in `arc_field_record_vars_` so scope cleanup
    // can release those fields — otherwise the construction path
    // leaves items orphaned (a pre-existing leak) and the copy path
    // compounds it.
    if (auto *recSt = llvm::dyn_cast<llvm::StructType>(newTy)) {
        if (recordHasArcFields(recSt)) {
            if (llvm::isa<llvm::LoadInst>(val) || llvm::isa<llvm::ExtractValueInst>(val)) {
                // Copy from another record alloca or sub-field extract.
                emitRecordArcFieldsRetain(val, recSt);
            }
            arc_field_record_vars_.insert(ptr);
        }
    }

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

    // Track union value type (skip literal unions which use base types directly).
    if (annot && isUnionType(resolvedAnnot) && !constraint) {
        storeFlattenedUnionMeta(ptr, *annot);
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
            const auto &ann = *annot;
            std::string inner;
            if (ann.size() > 7 && ann.substr(0, 7) == "Option<" && ann.back() == '>')
                inner = ann.substr(7, ann.size() - 8);
            else if (ann.size() > 7 && ann.substr(0, 7) == "Result<" && ann.back() == '>') {
                // Pass the full "Result<Ok,Err>" annotation to propagateTypeMeta, which
                // handles both the Ok-payload and Err-payload collection cases with an
                // automatic Ok→Err fallback (added in #985). This covers patterns like
                // Result<int,List<int>> where the Err type carries the collection.
                inner = ann;
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
                    if (isMapTypeName(inner) || isSetTypeName(inner) ||
                            isLowLevelIntTypeName(inner)) {
                        // Also covers low-level int names (e.g. "i8", "u8") so that
                        // AssignStmt (#1085) can recover the source-level element name
                        // faithfully without the lossy reverseResolveTypeName round-trip
                        // (i8Ty_ → "u8" regardless of the declared signedness).
                        letn = inner;
                    } else if (inner.size() > 9 && inner.substr(0, 9) == "function(") {
                        lefti = parseFnTypeAnnotation(inner);
                    } else {
                        // Tuple annotation (or alias resolving to one): record
                        // the resolved tuple signature so for-loop destructure
                        // in #813 can split per-component metadata. PR #853
                        // review.
                        std::string innerResolved = resolveTypeAlias(inner);
                        if (innerResolved.size() >= 2
                                && innerResolved.front() == '('
                                && innerResolved.back() == ')')
                            letn = innerResolved;
                    }
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
            std::optional<FnTypeInfo> mvfti;
            if (valMeta) {
                if (!valMeta->map_value_type_name.empty())
                    mvtn = valMeta->map_value_type_name;
                if (valMeta->map_value_fn_type_info)
                    mvfti = valMeta->map_value_fn_type_info;
            }
            if (mvtn.empty()) {
                if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
                    auto *loadMeta = getMeta(load->getPointerOperand());
                    if (loadMeta) {
                        if (!loadMeta->map_value_type_name.empty())
                            mvtn = loadMeta->map_value_type_name;
                        if (!mvfti && loadMeta->map_value_fn_type_info)
                            mvfti = loadMeta->map_value_fn_type_info;
                    }
                }
            }
            // Also derive from annotation: Map<K, function(int) -> int> → mvtn = "function(int) -> int"
            if (mvtn.empty() && annot && isMapTypeName(resolvedAnnot)) {
                std::string vtn = extractMapValueTypeName(resolvedAnnot);
                if (!vtn.empty())
                    mvtn = vtn;
            }
            if (!mvtn.empty()) {
                getOrCreateMeta(ptr).map_value_type_name = mvtn;
                if (!mvfti && isFunctionTypeName(mvtn))
                    mvfti = parseFnTypeAnnotation(mvtn);
            }
            if (mvfti)
                getOrCreateMeta(ptr).map_value_fn_type_info = mvfti;
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

        // --- Set element type name tracking (for Set<List>, Set<Map>, Set<closure>) ---
        {
            auto *valMeta = getMeta(val);
            std::string setn;
            std::optional<FnTypeInfo> sefti;
            if (valMeta) {
                if (!valMeta->set_elem_type_name.empty())
                    setn = valMeta->set_elem_type_name;
                if (valMeta->set_elem_fn_type_info)
                    sefti = valMeta->set_elem_fn_type_info;
            }
            if (setn.empty()) {
                if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
                    auto *loadMeta = getMeta(load->getPointerOperand());
                    if (loadMeta) {
                        if (!loadMeta->set_elem_type_name.empty())
                            setn = loadMeta->set_elem_type_name;
                        if (!sefti && loadMeta->set_elem_fn_type_info)
                            sefti = loadMeta->set_elem_fn_type_info;
                    }
                }
            }
            if (setn.empty() && annot &&
                isSetTypeName(resolvedAnnot) && resolvedAnnot.size() > 4 && resolvedAnnot.back() == '>') {
                std::string inner = resolvedAnnot.substr(4, resolvedAnnot.size() - 5);
                while (!inner.empty() && inner.front() == ' ') inner = inner.substr(1);
                if (isListTypeName(inner) || isMapTypeName(inner) || isSetTypeName(inner))
                    setn = inner;
                else if (isFunctionTypeName(inner))
                    sefti = parseFnTypeAnnotation(inner);
            }
            if (!setn.empty()) {
                getOrCreateMeta(ptr).set_elem_type_name = setn;
                if (!sefti && isFunctionTypeName(setn))
                    sefti = parseFnTypeAnnotation(setn);
            }
            if (sefti)
                getOrCreateMeta(ptr).set_elem_fn_type_info = sefti;
        }

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
            std::string innerName = resolveTypeAlias(weakInnerTypeName(*annot));
            // str uses StringHeader (24 bytes before the data pointer) while
            // other ARC types use ArcHeader (16 bytes). isStringValue() cannot
            // be used here because captured List/Map/Set values may lack
            // collection metadata and would be misclassified as str.
            // Instead, use the inner type name from the annotation.
            llvm::Value *headerPtr = (innerName == "str")
                ? emitStrGetHeaderFromData(val)
                : emitArcGetHeaderFromData(val);
            // Override the store: we want the header pointer (not the data
            // pointer) in the alloca so that VariableExpr loads the correct
            // pointer to pass to emitWeakUpgrade / emitWeakRetain.
            builder_.CreateStore(headerPtr, ptr);
            emitWeakRetain(headerPtr);
            markWeakManaged(ptr);
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
        // Write-through to a previously declared top-level module global
        // (#817). Only applies to PLAIN assignments (`name = expr`). When the
        // statement carries a type annotation (`name: Type = expr`) or a
        // @const directive, the user is explicitly declaring a new local that
        // shadows the module global, so fall through to emitVarDecl instead.
        if (!s.type_annotation && !is_const) {
            if (auto *b = findModuleGlobal(s.name)) {
                if (b->is_immutable)
                    codegenError("cannot reassign @const variable: " + s.name);
                emitModuleGlobalWriteThrough(*b, s);
                return;
            }
        }
        emitTraceSymbolDefine("variable", s.name, s.loc);
        bool was_top_level = isTopLevelContext();
        emitVarDecl(s.name, s.type_annotation, *s.value, is_const);
        if (hasDirective(s.directives, "deprecated"))
            deprecated_variables_.insert(s.name);
        // Register the newly created alloca as a module global if we were at
        // top level (#817). The alloca lives in __ry_main__'s entry block and
        // its address is captured in a module-level pointer trampoline so any
        // subsequent top-level function can find it via findModuleGlobal.
        if (was_top_level) {
            auto it = scope_stack_[0].find(s.name);
            if (it != scope_stack_[0].end() && it->second != nullptr)
                registerModuleGlobal(s.name, it->second, is_const);
        }
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

    // Compound assignment resolution: operator+= → operator+ → built-in.
    // Shares the load-modify-store core with the chained LHS forms (#812)
    // via `applyCompoundOp` so the operator resolution order stays in sync.
    if (s.compound_op) {
        llvm::Value *currentVal = builder_.CreateLoad(ptr->getAllocatedType(), ptr, s.name);

        // #1102: Propagate List<T> element type onto the loaded LHS value so
        // that getListElementType(currentVal) resolves correctly inside
        // emitListConcat / applyCompoundOp. Same pattern as #858/#862
        // (Compound-op loaded slot values must propagate container metadata).
        // Use the full container type name (e.g. "List<u8>") so propagateTypeMeta
        // stamps TypeMeta::ListElem; list_elem_type_name alone would not do so.
        {
            std::string elemTypeName;
            if (auto *meta = getMeta(ptr))
                elemTypeName = meta->list_elem_type_name;
            if (!elemTypeName.empty())
                propagateTypeMeta("List<" + elemTypeName + ">", currentVal);
        }

        // #1102: Mirror the plain-= RHS suffix injection (lines below) so
        // `bs += [99]` on a List<u8> variable injects the `:u8` suffix before
        // emitExpr evaluates the literal (byte-stride committed inside emitExpr
        // cannot be repaired post-emit).
        if (auto *le = std::get_if<std::unique_ptr<ListExpr>>(&s.value->data);
                le && !(*le)->elements.empty()) {
            std::string inner;
            if (auto *meta = getMeta(ptr); meta && !meta->list_elem_type_name.empty())
                inner = meta->list_elem_type_name;
            else if (llvm::Type *elemTy = getTypeMeta(TypeMeta::ListElem, ptr))
                inner = reverseResolveTypeName(elemTy);
            if (isLowLevelIntTypeName(inner))
                injectListExprElemSuffixes(**le, inner);
        }

        llvm::Value *rhs = emitExpr(*s.value);
        llvm::Value *result = applyCompoundOp(*s.compound_op, currentVal, rhs, *s.value,
                                               ptr->getAllocatedType(), s.name);
        builder_.CreateStore(result, ptr);
        return;
    }

    // Handle None literal in assignment (None, none, or None() call-form)
    if (isNoneLiteral(*s.value)) {
        llvm::Type *varTy = ptr->getAllocatedType();
        if (!isOptionType(varTy))
            codegenError("None can only be assigned to Option type");
        llvm::Value *val = buildNoneValue(varTy);
        builder_.CreateStore(val, ptr);
        return;
    }

    // Mirror the decl-time suffix injection so `x = 18446744073709551615`
    // works on a `u64` variable, not just on the initial declaration.
    {
        const std::string &varLL = getLowLevelTypeName(ptr);
        if (!varLL.empty())
            injectLowLevelSuffix(*s.value, varLL);
    }

    // #1085: List<T> element suffix propagation for reassignment. Mirrors the
    // #1079 decl-time loop (emitVarDecl). Byte stride is committed inside
    // emitExpr(ListExpr); post-emit metadata stamping cannot repair a
    // mis-strided heap allocation, so the suffix must be injected before emit.
    // The target ptr's TypeMeta::ListElem was stamped at declaration time;
    // recover the element type name via list_elem_type_name (source-level) or
    // reverseResolveTypeName (LLVM-level; i8Ty_ → "u8").
    if (auto *le = std::get_if<std::unique_ptr<ListExpr>>(&s.value->data);
            le && !(*le)->elements.empty()) {
        std::string inner;
        if (auto *meta = getMeta(ptr); meta && !meta->list_elem_type_name.empty())
            inner = meta->list_elem_type_name;
        else if (llvm::Type *elemTy = getTypeMeta(TypeMeta::ListElem, ptr))
            inner = reverseResolveTypeName(elemTy);
        if (isLowLevelIntTypeName(inner))
            injectListExprElemSuffixes(**le, inner);
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
        } else if (isResultType(ptr->getAllocatedType()) && isResultType(newTy)) {
            auto *dstResTy = llvm::cast<llvm::StructType>(ptr->getAllocatedType());
            llvm::Value *resCoerced = coerceResultType(val, dstResTy);
            if (resCoerced)
                val = resCoerced;
            else
                codegenError(
                    "type error: variable '" + s.name +
                    "' cannot be reassigned to a different type");
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

    // Record-with-ARC-fields reassignment (#854 Layer 2). Mirrors the
    // retain-then-release-old protocol used for ARC-managed variables but
    // walks each ARC field of the struct rather than a single header.
    // Construction-vs-copy detection: retain when RHS is a view of
    // existing state (LoadInst from another alloca, or ExtractValueInst
    // from a parent record). For fresh constructions (`r2 = CowBox(...)`
    // — an InsertValue / CallInst chain) the new struct is the sole
    // owner of its ARC fields so retain would leak a ref.
    if (arc_field_record_vars_.count(ptr)) {
        auto *recSt = llvm::dyn_cast<llvm::StructType>(ptr->getAllocatedType());
        if (recSt) {
            if (llvm::isa<llvm::LoadInst>(val) || llvm::isa<llvm::ExtractValueInst>(val))
                emitRecordArcFieldsRetain(val, recSt);
            llvm::Value *oldStruct = builder_.CreateLoad(
                recSt, ptr, s.name + ".record_old");
            emitRecordArcFieldsRelease(oldStruct, recSt);
        }
    }
    // Weak ref reassignment: retain new, release old
    else if (isWeakManaged(ptr)) {
        if (!std::get_if<std::unique_ptr<WeakExpr>>(&s.value->data))
            codegenError("weak variable must be reassigned with a 'weak' expression");
        // val is the raw data pointer from emitExprVariant(WeakExpr).
        // Convert to header pointer using the stored inner type name.
        auto itWeak = weak_inner_type_names_.find(ptr);
        const std::string &weakInner = (itWeak != weak_inner_type_names_.end())
            ? itWeak->second : std::string{};
        llvm::Value *headerPtr = (weakInner == "str")
            ? emitStrGetHeaderFromData(val)
            : emitArcGetHeaderFromData(val);
        emitWeakRetain(headerPtr);
        auto *oldVal = builder_.CreateLoad(ptrTy_, ptr, s.name + ".weak_old");
        emitWeakRelease(oldVal);
        val = headerPtr;
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

// ===== Module-global write-through (#817) =====

void CodeGen::emitModuleGlobalWriteThrough(const ModuleBinding &b, AssignStmt &s) {
    llvm::AllocaInst *anchor = b.original_alloca;
    llvm::Type *valueTy = b.valueTy();

    // Weak/resource top-level bindings cannot be reassigned from a foreign
    // function — out of scope for v1 (#817). These flags were captured in
    // `registerModuleGlobal` while still in __ry_main__ context, since
    // FnScope clears the per-function weak_managed_vars_/resource_managed_vars_
    // sets when entering a function body and the live lookups would therefore
    // silently return false here.
    if (b.is_weak)
        codegenError("weak top-level variables cannot be reassigned from a function (#817 follow-up)");
    if (b.is_resource)
        codegenError("resource-typed top-level variables cannot be reassigned from a function (#817 follow-up)");

    // Resolve the storage pointer once up front. The trampoline global never
    // changes after __ry_main__ initializes it, so a single load is enough
    // for every read/write in this function (mirrors how the local-alloca
    // path reuses `ptr` throughout).
    llvm::Value *storagePtr = loadModuleGlobalStorage(b, s.name);

    // Compound assignment shares the resolution order with the local
    // AssignStmt and chained LHS paths via `applyCompoundOp` (#812).
    if (s.compound_op) {
        llvm::Value *currentVal = builder_.CreateLoad(valueTy, storagePtr, s.name);

        // #1102: Propagate List<T> element type onto the loaded LHS value
        // (same pattern as local AssignStmt compound_op fix and #858/#862).
        {
            std::string elemTypeName;
            if (auto *meta = getMeta(anchor))
                elemTypeName = meta->list_elem_type_name;
            if (!elemTypeName.empty())
                propagateTypeMeta("List<" + elemTypeName + ">", currentVal);
        }

        // #1102: Mirror the plain-= RHS suffix injection (lines below) for
        // module-global List<u8> compound assignment.
        if (auto *le = std::get_if<std::unique_ptr<ListExpr>>(&s.value->data);
                le && !(*le)->elements.empty()) {
            std::string inner;
            if (auto *meta = getMeta(anchor); meta && !meta->list_elem_type_name.empty())
                inner = meta->list_elem_type_name;
            else if (llvm::Type *elemTy = getTypeMeta(TypeMeta::ListElem, anchor))
                inner = reverseResolveTypeName(elemTy);
            if (isLowLevelIntTypeName(inner))
                injectListExprElemSuffixes(**le, inner);
        }

        llvm::Value *rhs = emitExpr(*s.value);
        llvm::Value *result = applyCompoundOp(*s.compound_op, currentVal, rhs, *s.value,
                                               valueTy, s.name);
        builder_.CreateStore(result, storagePtr);
        return;
    }

    // None-literal assignment on an Option-typed module global (None, none, or None() call-form)
    if (isNoneLiteral(*s.value)) {
        if (!isOptionType(valueTy))
            codegenError("None can only be assigned to Option type");
        builder_.CreateStore(buildNoneValue(valueTy), storagePtr);
        return;
    }

    // Same suffix injection as the local-variable assign path, so
    // module-global u64 reassignment honours the target type.
    {
        const std::string &anchorLL = getLowLevelTypeName(anchor);
        if (!anchorLL.empty())
            injectLowLevelSuffix(*s.value, anchorLL);
    }

    // Mirror the AssignStmt List<T> branch (#1085) so module-global List<u8>
    // variables reassigned from inside a function also get byte-stride elements.
    if (auto *le = std::get_if<std::unique_ptr<ListExpr>>(&s.value->data);
            le && !(*le)->elements.empty()) {
        std::string inner;
        if (auto *meta = getMeta(anchor); meta && !meta->list_elem_type_name.empty())
            inner = meta->list_elem_type_name;
        else if (llvm::Type *elemTy = getTypeMeta(TypeMeta::ListElem, anchor))
            inner = reverseResolveTypeName(elemTy);
        if (isLowLevelIntTypeName(inner))
            injectListExprElemSuffixes(**le, inner);
    }

    llvm::Value *val = emitExpr(*s.value);
    llvm::Type *newTy = val->getType();

    if (valueTy != newTy) {
        if (llvm::Value *coerced = coerceToLowLevelType(
                val, valueTy, getLowLevelTypeName(anchor),
                "", "i8trunc")) {
            val = coerced;
        } else if (isAnyType(valueTy)) {
            val = wrapInAny(val);
            newTy = val->getType();
        } else if (isAnyType(newTy) && canAnyHoldType(valueTy)) {
            val = unwrapFromAny(val, valueTy);
            newTy = val->getType();
        } else if (isResultType(valueTy) && isResultType(newTy)) {
            auto *dstResTy = llvm::cast<llvm::StructType>(valueTy);
            llvm::Value *resCoerced = coerceResultType(val, dstResTy);
            if (resCoerced)
                val = resCoerced;
            else
                codegenError(
                    "type error: variable '" + s.name +
                    "' cannot be reassigned to a different type");
        } else {
            auto *uvMeta = getMeta(anchor);
            if (uvMeta && !uvMeta->union_value_type.empty()) {
                val = wrapInUnion(val, uvMeta->union_value_type);
            } else {
                codegenError(
                    "type error: variable '" + s.name +
                    "' cannot be reassigned to a different type");
            }
        }
    }

    // Type constraints (literal-range, etc.) tracked on the original alloca.
    if (auto *tcMeta = getMeta(anchor); tcMeta && tcMeta->type_constraint)
        emitConstraintCheck(val, *tcMeta->type_constraint, s.name);

    // ARC retain/release when the top-level variable holds an ARC-managed
    // value. The ARC classification (`is_arc_managed`, `is_arc_atomic`) and
    // the destructor (`b.destructor`) were captured at registration time in
    // __ry_main__ context; live queries against `arc_managed_vars_` /
    // `resource_managed_vars_` are NOT valid here because FnScope cleared
    // those sets on entry to this function. `value_metadata_` is persistent
    // across FnScope, so the enum_value_type lookup via `getMeta(anchor)` is
    // safe.
    if (b.is_arc_managed) {
        tryRetainArcSource(val);
        auto *oldVal = builder_.CreateLoad(ptrTy_, storagePtr, s.name + ".arc_old");
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
        llvm::Function *gcVisitFn = nullptr;
        if (auto *evMeta = getMeta(anchor);
            evMeta && !evMeta->enum_value_type.empty() && isPotentiallyCyclic(evMeta->enum_value_type))
            gcVisitFn = getOrCreateVisitFunction(evMeta->enum_value_type);
        emitArcRelease(oldHdr, b.is_arc_atomic, b.destructor, gcVisitFn);
        builder_.CreateBr(storeBB);

        builder_.SetInsertPoint(storeBB);
    }
    // Module-global record-with-ARC-fields reassignment (#854 Layer 2).
    // Mirrors the local AssignStmt retain-then-release-old protocol so
    // a top-level `global_box = other_box` keeps ARC-field strong counts
    // consistent across aliases. The anchor alloca is registered in
    // `arc_field_record_vars_` during __ry_main__; live queries against
    // that set are valid here because it persists across FnScope (same
    // lifetime as `value_metadata_`).
    else if (arc_field_record_vars_.count(anchor)) {
        auto *recSt = llvm::dyn_cast<llvm::StructType>(valueTy);
        if (recSt) {
            if (llvm::isa<llvm::LoadInst>(val) || llvm::isa<llvm::ExtractValueInst>(val))
                emitRecordArcFieldsRetain(val, recSt);
            llvm::Value *oldStruct = builder_.CreateLoad(
                recSt, storagePtr, s.name + ".record_old");
            emitRecordArcFieldsRelease(oldStruct, recSt);
        }
    }

    builder_.CreateStore(val, storagePtr);
    // Mirror the local-alloca path's propagateMetaWide call so that resource
    // kinds, task result types, and fn_type_info flow back to the metadata
    // anchor after reassignment.
    propagateMetaWide(val, anchor);
}

} // namespace ry
