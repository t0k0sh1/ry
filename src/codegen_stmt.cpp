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
                           const std::optional<std::string> &type_annotation,
                           ExprNode &value, bool is_immutable) {
    if (scope_stack_.back().count(name))
        codegenError("redeclared variable: " + name);

    // Handle empty set/map literal with type annotation
    if (auto *se = std::get_if<std::unique_ptr<SetExpr>>(&value.data); se && (*se)->elements.empty()) {
        if (!type_annotation)
            codegenError("empty {} literal requires type annotation");
        if (type_annotation->size() > 4 && type_annotation->substr(0, 4) == "Set<") {
            std::string inner = type_annotation->substr(4, type_annotation->size() - 5);
            llvm::Type *elemTy = resolveType(inner);

            auto mallocFn = getStdlibMalloc();
            const llvm::DataLayout &dl = mod_->getDataLayout();

            uint64_t headerSize = dl.getTypeAllocSize(setHeaderTy_);
            llvm::Value *headerPtr = builder_.CreateCall(
                mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "empty_set");

            // Initial capacity = 4
            uint64_t elemSize = dl.getTypeAllocSize(elemTy);
            llvm::Value *elemsPtr = builder_.CreateCall(
                mallocFn, {llvm::ConstantInt::get(i64Ty_, elemSize * 4)}, "empty_set_elems");

            llvm::Value *lenPtr = builder_.CreateStructGEP(setHeaderTy_, headerPtr, 0);
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), lenPtr);
            llvm::Value *capPtr = builder_.CreateStructGEP(setHeaderTy_, headerPtr, 1);
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 4), capPtr);
            llvm::Value *elemsPtrField = builder_.CreateStructGEP(setHeaderTy_, headerPtr, 2);
            builder_.CreateStore(elemsPtr, elemsPtrField);
            emitBucketInit(headerPtr, setHeaderTy_, 3, 4, 8);

            llvm::AllocaInst *ptr = getOrCreateVar(name, ptrTy_);
            builder_.CreateStore(headerPtr, ptr);
            set_element_types_[ptr] = elemTy;
            if (is_immutable)
                immutable_scope_stack_.back().insert(name);
            return;
        }
        if (type_annotation->size() > 4 && type_annotation->substr(0, 4) == "Map<") {
            auto [keyTy, valTy] = parseMapTypeAnnotation(*type_annotation);
            if (!keyTy || !valTy)
                codegenError("invalid map type annotation: " + *type_annotation);

            auto mallocFn = getStdlibMalloc();
            const llvm::DataLayout &dl = mod_->getDataLayout();

            uint64_t headerSize = dl.getTypeAllocSize(mapHeaderTy_);
            llvm::Value *headerPtr = builder_.CreateCall(
                mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "empty_map");

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
            emitBucketInit(headerPtr, mapHeaderTy_, 4, 5, 8);

            llvm::AllocaInst *ptr = getOrCreateVar(name, ptrTy_);
            builder_.CreateStore(headerPtr, ptr);
            map_key_types_[ptr] = keyTy;
            map_value_types_[ptr] = valTy;
            if (is_immutable)
                immutable_scope_stack_.back().insert(name);
            return;
        }
        codegenError("empty {} requires Set<T> or Map<K, V> type annotation");
    }

    // Handle None literal (VariableExpr("None") or NoneExpr)
    bool isNone = std::holds_alternative<NoneExpr>(value.data) ||
                  (std::holds_alternative<VariableExpr>(value.data) &&
                   std::get<VariableExpr>(value.data).name == "None");
    if (isNone) {
        if (!type_annotation)
            codegenError("type annotation required for None");
        llvm::Type *annotTy = resolveType(*type_annotation);
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
    if (type_annotation) {
        resolvedAnnot = resolveTypeAlias(*type_annotation);
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

    llvm::Value *val = emitExpr(value);
    llvm::Type *newTy = val->getType();

    if (type_annotation) {
        if (constraint) {
            // Literal/range type: resolve to base type and check constraint
            llvm::Type *annotTy = resolveType(resolvedAnnot);
            if (annotTy != newTy)
                codegenError(
                    "type error: annotation '" + *type_annotation +
                    "' does not match expression type for variable '" + name + "'");
            emitConstraintCheck(val, *constraint, name);
        } else {
            llvm::Type *annotTy = resolveType(*type_annotation);
            if (annotTy != newTy) {
                if (annotTy == i8Ty_ && newTy == i64Ty_) {
                    if (auto *ci = llvm::dyn_cast<llvm::ConstantInt>(val)) {
                        int64_t v = ci->getSExtValue();
                        if (v < 0 || v > 255)
                            codegenError(
                                "byte value out of range (0-255): " + std::to_string(v));
                    }
                    val = builder_.CreateTrunc(val, i8Ty_, "bytetrunc");
                    newTy = i8Ty_;
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
                            "type error: annotation '" + *type_annotation +
                            "' does not match expression type for variable '" + name + "'");
                    val = buildSomeValue(val, annotTy);
                    newTy = annotTy;
                } else if (isUnionType(*type_annotation)) {
                    val = wrapInUnion(val, *type_annotation);
                    newTy = val->getType();
                } else {
                    codegenError(
                        "type error: annotation '" + *type_annotation +
                        "' does not match expression type for variable '" + name + "'");
                }
            }
        }
    }

    llvm::AllocaInst *ptr = getOrCreateVar(name, newTy);
    builder_.CreateStore(val, ptr);

    // Track type constraint for reassignment checks
    if (constraint)
        type_constraints_[ptr] = *constraint;

    // Track union value type (skip literal unions which use base types directly)
    if (type_annotation && isUnionType(*type_annotation) && !constraint) {
        union_value_types_[ptr] = normalizeUnionType(*type_annotation);
    }

    // Track list/map element types if this is a ptr value
    if (newTy == ptrTy_) {
        // --- List tracking ---
        llvm::Type *elemTy = getListElementType(val);
        if (!elemTy && type_annotation && type_annotation->size() > 5 &&
            type_annotation->substr(0, 5) == "List<") {
            std::string inner = type_annotation->substr(5, type_annotation->size() - 6);
            elemTy = resolveType(inner);
        }
        if (elemTy)
            list_element_types_[ptr] = elemTy;

        // --- Nested list tracking (for flatten) ---
        {
            auto nit = nested_list_element_types_.find(val);
            if (nit != nested_list_element_types_.end())
                nested_list_element_types_[ptr] = nit->second;
            else if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
                auto nit2 = nested_list_element_types_.find(load->getPointerOperand());
                if (nit2 != nested_list_element_types_.end())
                    nested_list_element_types_[ptr] = nit2->second;
            }
        }

        // --- Map tracking ---
        llvm::Type *keyTy = nullptr;
        llvm::Type *valTy = nullptr;
        // Direct mapping (from MapExpr)
        auto mk = map_key_types_.find(val);
        if (mk != map_key_types_.end()) keyTy = mk->second;
        auto mv = map_value_types_.find(val);
        if (mv != map_value_types_.end()) valTy = mv->second;
        // From variable load
        if (!keyTy) {
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
                auto mk2 = map_key_types_.find(load->getPointerOperand());
                if (mk2 != map_key_types_.end()) keyTy = mk2->second;
                auto mv2 = map_value_types_.find(load->getPointerOperand());
                if (mv2 != map_value_types_.end()) valTy = mv2->second;
            }
        }
        // From type annotation: Map<K, V>
        if (!keyTy && type_annotation && type_annotation->size() > 4 &&
            type_annotation->substr(0, 4) == "Map<") {
            std::tie(keyTy, valTy) = parseMapTypeAnnotation(*type_annotation);
        }
        if (keyTy) map_key_types_[ptr] = keyTy;
        if (valTy) map_value_types_[ptr] = valTy;

        // --- Set tracking ---
        llvm::Type *setElemTy = getSetElementType(val);
        if (!setElemTy) {
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
                setElemTy = getSetElementType(load->getPointerOperand());
            }
        }
        if (!setElemTy && type_annotation && type_annotation->size() > 4 &&
            type_annotation->substr(0, 4) == "Set<") {
            std::string inner = type_annotation->substr(4, type_annotation->size() - 5);
            setElemTy = resolveType(inner);
        }
        if (setElemTy)
            set_element_types_[ptr] = setElemTy;

        // --- Task tracking ---
        llvm::Type *taskTy = getTaskResultType(val);
        if (!taskTy && type_annotation && type_annotation->size() > 5 &&
            type_annotation->substr(0, 5) == "Task<" && type_annotation->back() == '>') {
            std::string inner = type_annotation->substr(5, type_annotation->size() - 6);
            taskTy = resolveType(inner);
        }
        if (taskTy)
            task_result_types_[ptr] = taskTy;

        // --- Channel tracking ---
        llvm::Type *channelTy = getChannelElementType(val);
        if (!channelTy && type_annotation && type_annotation->size() > 8 &&
            type_annotation->substr(0, 8) == "Channel<" && type_annotation->back() == '>') {
            std::string inner = type_annotation->substr(8, type_annotation->size() - 9);
            channelTy = resolveType(inner);
        }
        if (channelTy)
            channel_element_types_[ptr] = channelTy;

        // --- Function pointer tracking ---
        auto fnIt = fn_type_info_.find(val);
        if (fnIt != fn_type_info_.end()) {
            fn_type_info_[ptr] = fnIt->second;
        } else if (type_annotation) {
            if (resolvedAnnot.size() > 3 && resolvedAnnot.substr(0, 3) == "fn(") {
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
                iterator_element_types_[ptr] = iterElemTy;
        }
    }

    // --- Resource type tracking ---
    // These must be outside the ptrTy_ guard because resources can be
    // wrapped in Result<T, Error> structs (e.g., http_get() returns a struct).
    detectAndRegisterResource(val, ptr);
    if (type_annotation)
        registerResourceByTypeName(*type_annotation, ptr);

    // --- Enum value tracking (works for i64 values, not just ptr) ---
    {
        auto evIt = enum_value_types_.find(val);
        if (evIt != enum_value_types_.end())
            enum_value_types_[ptr] = evIt->second;
        else if (type_annotation && enum_types_.count(*type_annotation))
            enum_value_types_[ptr] = *type_annotation;
    }

    if (is_immutable)
        immutable_scope_stack_.back().insert(name);
}

void CodeGen::emitStmt(AssignStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    bool is_const = hasDirective(s.directives, "const");
    bool is_native = hasDirective(s.directives, "native");

    // @native @const declaration (e.g., PI, E, Inf, NaN)
    if (is_native && !s.value) {
        if (s.name == "PI" || s.name == "E" || s.name == "Inf" || s.name == "NaN") {
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
        // byte variable assigned from int expression
        if (ptr->getAllocatedType() == i8Ty_ && newTy == i64Ty_) {
            if (auto *ci = llvm::dyn_cast<llvm::ConstantInt>(val)) {
                int64_t v = ci->getSExtValue();
                if (v < 0 || v > 255)
                    codegenError(
                        "byte value out of range (0-255): " + std::to_string(v));
            }
            val = builder_.CreateTrunc(val, i8Ty_, "bytetrunc");
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

    builder_.CreateStore(val, ptr);

    // Propagate fn_type_info_
    if (newTy == ptrTy_) {
        auto fnIt = fn_type_info_.find(val);
        if (fnIt != fn_type_info_.end())
            fn_type_info_[ptr] = fnIt->second;
        llvm::Type *taskTy = getTaskResultType(val);
        if (taskTy)
            task_result_types_[ptr] = taskTy;
        llvm::Type *channelTy = getChannelElementType(val);
        if (channelTy)
            channel_element_types_[ptr] = channelTy;
    }
    // Resource tracking: must be outside ptrTy_ guard for Result-wrapped types
    detectAndRegisterResource(val, ptr);
}


void CodeGen::emitStmt(std::unique_ptr<WhileStmt> &s) {
    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "while.cond", fn_);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "while.body", fn_);
    llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, "while.end", fn_);

    builder_.CreateBr(condBB);

    builder_.SetInsertPoint(condBB);
    llvm::Value *cond = emitExpr(*s->condition);
    cond = toBool(cond);
    builder_.CreateCondBr(cond, bodyBB, endBB);

    builder_.SetInsertPoint(bodyBB);
    loop_stack_.push_back({condBB, endBB});
    pushScope();
    for (auto &stmt : s->body)
        std::visit([this](auto &st) { emitStmt(st); }, stmt);
    popScope();
    loop_stack_.pop_back();
    if (!builder_.GetInsertBlock()->getTerminator())
        builder_.CreateBr(condBB);

    builder_.SetInsertPoint(endBB);
}

void CodeGen::emitStmt(std::unique_ptr<ForStmt> &s) {
    if (hasDirective(s->directives, "parallel")) {
        if (s->var_name2.has_value())
            codegenError(s->loc, "@parallel for does not support destructuring iteration");

        validateParallelFor(*s);

        llvm::Value *begin = nullptr;
        llvm::Value *end = nullptr;
        llvm::Value *step = llvm::ConstantInt::get(i64Ty_, 1);

        if (auto *rangeExpr = std::get_if<std::unique_ptr<RangeExpr>>(&s->iterable->data)) {
            begin = emitExpr(*(*rangeExpr)->start);
            llvm::Value *inclusiveEnd = emitExpr(*(*rangeExpr)->end);
            end = builder_.CreateAdd(inclusiveEnd, llvm::ConstantInt::get(i64Ty_, 1), "parallel_inclusive_end");
        } else if (auto *callExpr = std::get_if<std::unique_ptr<CallExpr>>(&s->iterable->data)) {
            if ((*callExpr)->callee != "range")
                codegenError(s->loc, "@parallel for only supports range(...) or .. iterables");
            if ((*callExpr)->args.size() < 1 || (*callExpr)->args.size() > 3)
                codegenError(s->loc, "range() takes 1, 2, or 3 arguments");
            if ((*callExpr)->args.size() == 1) {
                begin = llvm::ConstantInt::get(i64Ty_, 0);
                end = emitExpr(*(*callExpr)->args[0]);
            } else {
                begin = emitExpr(*(*callExpr)->args[0]);
                end = emitExpr(*(*callExpr)->args[1]);
            }
            if ((*callExpr)->args.size() == 3)
                step = emitExpr(*(*callExpr)->args[2]);
        } else {
            codegenError(s->loc, "@parallel for only supports range(...) or .. iterables");
        }

        if (begin->getType() != i64Ty_ || end->getType() != i64Ty_ || step->getType() != i64Ty_)
            codegenError(s->loc, "@parallel for requires integer range bounds");

        emitParallelForRange(*s, begin, end, step);
        return;
    }

    // Evaluate iterable
    llvm::Value *iterable = emitExpr(*s->iterable);

    // Check if this is a pointer-backed iterable (list/set/map/channel)
    if (iterable->getType() != ptrTy_)
        codegenError("for loop requires list, set, map, channel, or iterator iterable");

    if (llvm::Type *channelElemTy = getChannelElementType(iterable)) {
        if (s->var_name2.has_value())
            codegenError("channel iteration does not support destructuring");
        emitChannelForLoop(*s, iterable, channelElemTy);
        return;
    }

    // Check if iterable is an iterator
    llvm::Type *iterElemTy = getIteratorElementType(iterable);
    if (iterElemTy) {
        llvm::Value *nextFnField = builder_.CreateStructGEP(iteratorHeaderTy_, iterable, 0, "for_iter_nf");
        llvm::Value *nextFnPtr = builder_.CreateLoad(ptrTy_, nextFnField, "for_iter_next_fn");
        llvm::Value *stateField = builder_.CreateStructGEP(iteratorHeaderTy_, iterable, 1, "for_iter_st");
        llvm::Value *statePtr = builder_.CreateLoad(ptrTy_, stateField, "for_iter_state");

        llvm::StructType *optTy = getOptionType(iterElemTy);
        llvm::FunctionType *nextCallTy = llvm::FunctionType::get(optTy, {ptrTy_}, false);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "foriter.cond", fn_);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "foriter.body", fn_);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "foriter.end", fn_);

        builder_.CreateBr(condBB);
        builder_.SetInsertPoint(condBB);
        llvm::Value *opt = builder_.CreateCall(nextCallTy, nextFnPtr, {statePtr}, "foriter_opt");
        llvm::Value *hasVal = builder_.CreateExtractValue(opt, 0, "foriter_has");
        builder_.CreateCondBr(hasVal, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        loop_stack_.push_back({condBB, endBB});
        pushScope();

        llvm::Value *elem = builder_.CreateExtractValue(opt, 1, "foriter_elem");

        // Handle two-variable destructuring for map iterators (tuple elements)
        if (s->var_name2.has_value()) {
            auto *structTy = llvm::dyn_cast<llvm::StructType>(iterElemTy);
            if (!structTy || structTy->getNumElements() != 2)
                codegenError("for k, v over iterator requires tuple elements");
            llvm::Value *first = builder_.CreateExtractValue(elem, 0, "foriter_first");
            llvm::Value *second = builder_.CreateExtractValue(elem, 1, "foriter_second");
            if (s->var_name != "_") {
                llvm::AllocaInst *firstVar = getOrCreateVar(s->var_name, structTy->getElementType(0));
                builder_.CreateStore(first, firstVar);
            }
            if (*s->var_name2 != "_") {
                llvm::AllocaInst *secondVar = getOrCreateVar(*s->var_name2, structTy->getElementType(1));
                builder_.CreateStore(second, secondVar);
            }
        } else {
            llvm::AllocaInst *loopVar = getOrCreateVar(s->var_name, iterElemTy);
            builder_.CreateStore(elem, loopVar);
        }

        for (auto &stmt : s->body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);

        popScope();
        loop_stack_.pop_back();
        if (!builder_.GetInsertBlock()->getTerminator())
            builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);
        return;
    }

    // Two-variable iteration: for k, v in map  OR  for i, x in enumerate(xs)
    if (s->var_name2.has_value()) {
        llvm::Type *keyTy = getMapKeyType(iterable);
        llvm::Type *valTy = getMapValueType(iterable);
        if (!keyTy || !valTy) {
            // Try List<Tuple> (e.g. enumerate, zip)
            llvm::Type *elemTy = getListElementType(iterable);
            auto *structTy = llvm::dyn_cast_or_null<llvm::StructType>(elemTy);
            if (!structTy || structTy->getNumElements() != 2)
                codegenError("for k, v requires a map or list of 2-element tuples");

            llvm::Type *firstTy = structTy->getElementType(0);
            llvm::Type *secondTy = structTy->getElementType(1);

            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, iterable, 0, "for_len_ptr");
            llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "for_len");
            llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, iterable, 2, "for_data_ptr");
            llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "for_data");

            emitIndexedForLoop(length, s->body, [&](llvm::Value *iCur) {
                llvm::Value *tuplePtr = builder_.CreateGEP(structTy, dataPtr, {iCur}, "for_tuple_ptr");
                llvm::Value *tuple = builder_.CreateLoad(structTy, tuplePtr, "for_tuple");
                llvm::Value *first = builder_.CreateExtractValue(tuple, 0, "for_first");
                llvm::Value *second = builder_.CreateExtractValue(tuple, 1, "for_second");
                if (s->var_name != "_") {
                    llvm::AllocaInst *firstVar = getOrCreateVar(s->var_name, firstTy);
                    builder_.CreateStore(first, firstVar);
                }
                if (*s->var_name2 != "_") {
                    llvm::AllocaInst *secondVar = getOrCreateVar(*s->var_name2, secondTy);
                    builder_.CreateStore(second, secondVar);
                }
            });
            return;
        }

        llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, iterable, 0, "map_len_ptr");
        llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "map_len");
        llvm::Value *keysPtrField = builder_.CreateStructGEP(mapHeaderTy_, iterable, 2, "keys_ptr_field");
        llvm::Value *keysPtr = builder_.CreateLoad(ptrTy_, keysPtrField, "keys_ptr");
        llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, iterable, 3, "vals_ptr_field");
        llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "vals_ptr");

        emitIndexedForLoop(length, s->body, [&](llvm::Value *iCur) {
            llvm::Value *keyPtr = builder_.CreateGEP(keyTy, keysPtr, {iCur}, "for_key_ptr");
            llvm::Value *key = builder_.CreateLoad(keyTy, keyPtr, "for_key");
            llvm::Value *valPtr = builder_.CreateGEP(valTy, valsPtr, {iCur}, "for_val_ptr");
            llvm::Value *val = builder_.CreateLoad(valTy, valPtr, "for_val");
            llvm::AllocaInst *keyVar = getOrCreateVar(s->var_name, keyTy);
            builder_.CreateStore(key, keyVar);
            llvm::AllocaInst *valVar = getOrCreateVar(*s->var_name2, valTy);
            builder_.CreateStore(val, valVar);
        });
        return;
    }

    // Try set first, then list
    llvm::Type *elemTy = getSetElementType(iterable);
    llvm::StructType *headerTy = setHeaderTy_;
    if (!elemTy) {
        elemTy = getListElementType(iterable);
        headerTy = listHeaderTy_;
    }
    if (!elemTy)
        codegenError("cannot determine element type for for loop iterable");

    llvm::Value *lenPtr = builder_.CreateStructGEP(headerTy, iterable, 0, "for_len_ptr");
    llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "for_len");
    llvm::Value *dataPtrField = builder_.CreateStructGEP(headerTy, iterable, 2, "for_data_ptr");
    llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "for_data");

    emitIndexedForLoop(length, s->body, [&](llvm::Value *iCur) {
        llvm::Value *elemPtr = builder_.CreateGEP(elemTy, dataPtr, {iCur}, "for_elem_ptr");
        llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "for_elem");
        llvm::AllocaInst *loopVar = getOrCreateVar(s->var_name, elemTy);
        builder_.CreateStore(elem, loopVar);
    });
}

void CodeGen::emitChannelForLoop(ForStmt &s, llvm::Value *channel, llvm::Type *elemTy) {
    llvm::FunctionType *recvOptTy = llvm::FunctionType::get(i1Ty_, {ptrTy_, ptrTy_}, false);
    llvm::FunctionCallee recvOptFn = mod_->getOrInsertFunction("__ry_channel_recv_opt", recvOptTy);

    llvm::AllocaInst *recvSlot = nullptr;
    llvm::Value *outPtr = llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(*ctx_));
    if (!elemTy->isVoidTy()) {
        recvSlot = builder_.CreateAlloca(elemTy, nullptr, "for_channel_recv");
        outPtr = recvSlot;
    } else if (s.var_name != "_") {
        codegenError(s.loc, "for x in Channel<Unit> requires '_' loop variable");
    }

    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "for.channel.cond", fn_);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "for.channel.body", fn_);
    llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "for.channel.end", fn_);

    builder_.CreateBr(condBB);

    builder_.SetInsertPoint(condBB);
    llvm::Value *hasValue = builder_.CreateCall(recvOptFn, {channel, outPtr}, "for_channel_has_value");
    builder_.CreateCondBr(hasValue, bodyBB, endBB);

    builder_.SetInsertPoint(bodyBB);
    loop_stack_.push_back({condBB, endBB});
    pushScope();

    if (s.var_name != "_" && recvSlot) {
        llvm::AllocaInst *loopVar = getOrCreateVar(s.var_name, elemTy);
        llvm::Value *elem = builder_.CreateLoad(elemTy, recvSlot, "for_channel_elem");
        builder_.CreateStore(elem, loopVar);
    }

    for (auto &stmt : s.body)
        std::visit([this](auto &st) { emitStmt(st); }, stmt);

    popScope();
    loop_stack_.pop_back();
    if (!builder_.GetInsertBlock()->getTerminator())
        builder_.CreateBr(condBB);

    builder_.SetInsertPoint(endBB);
}

void CodeGen::emitIndexedForLoop(llvm::Value *length,
                                  std::vector<StmtNode> &body,
                                  std::function<void(llvm::Value *iCur)> bindVars) {
    llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "for_i");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "for.cond", fn_);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "for.body", fn_);
    llvm::BasicBlock *stepBB = llvm::BasicBlock::Create(*ctx_, "for.step", fn_);
    llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, "for.end", fn_);

    builder_.CreateBr(condBB);
    builder_.SetInsertPoint(condBB);
    llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "i");
    llvm::Value *cond = builder_.CreateICmpSLT(iVal, length, "for_cond");
    builder_.CreateCondBr(cond, bodyBB, endBB);

    builder_.SetInsertPoint(bodyBB);
    loop_stack_.push_back({stepBB, endBB});
    pushScope();

    llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "i_cur");
    bindVars(iCur);

    for (auto &stmt : body)
        std::visit([this](auto &st) { emitStmt(st); }, stmt);

    popScope();
    loop_stack_.pop_back();
    if (!builder_.GetInsertBlock()->getTerminator())
        builder_.CreateBr(stepBB);

    builder_.SetInsertPoint(stepBB);
    llvm::Value *iNext = builder_.CreateAdd(
        builder_.CreateLoad(i64Ty_, iVar, "i_step"), llvm::ConstantInt::get(i64Ty_, 1), "i_next");
    builder_.CreateStore(iNext, iVar);
    builder_.CreateBr(condBB);

    builder_.SetInsertPoint(endBB);
}

void CodeGen::emitStmt(BreakStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    if (loop_stack_.empty())
        codegenError("break outside of loop");
    builder_.CreateBr(loop_stack_.back().second);
    // Create unreachable block for subsequent code
    llvm::BasicBlock *deadBB = llvm::BasicBlock::Create(*ctx_, "break.dead", fn_);
    builder_.SetInsertPoint(deadBB);
}

void CodeGen::emitStmt(ContinueStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    if (loop_stack_.empty())
        codegenError("continue outside of loop");
    builder_.CreateBr(loop_stack_.back().first);
    llvm::BasicBlock *deadBB = llvm::BasicBlock::Create(*ctx_, "continue.dead", fn_);
    builder_.SetInsertPoint(deadBB);
}

void CodeGen::emitStmt(EllipsisStmt &) {
    // no-op: intentionally does nothing
}

void CodeGen::emitStmt(FieldAssignStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
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
    if (newVal->getType() != expectedTy)
        codegenError("field '" + s.field + "' type mismatch");

    // Load current struct value, insert new field value, store back
    llvm::Value *current = builder_.CreateLoad(varTy, ptr, "struct_cur");
    llvm::Value *updated = builder_.CreateInsertValue(current, newVal, fieldIdx, "struct_upd");
    builder_.CreateStore(updated, ptr);

    // Check invariants after field assignment
    if (!info.invariants.empty())
        emitInvariantCheck(typeName, info, updated);
}

void CodeGen::emitStmt(EnumStmt &s) {
    // Generic enum: save as template, don't instantiate yet
    if (!s.type_params.empty()) {
        GenericEnumTemplate tmpl;
        tmpl.name = s.name;
        tmpl.typeParams = s.type_params;
        tmpl.variants = s.variants;
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

    // Create global string array for variant names (for printing)
    std::vector<llvm::Constant*> nameStrings;
    for (size_t i = 0; i < s.variants.size(); ++i) {
        info.variants[s.variants[i].name] = static_cast<int64_t>(i);
        llvm::Constant *str = builder_.CreateGlobalString(
            s.variants[i].name, ".enum_" + s.name + "_" + s.variants[i].name);
        nameStrings.push_back(str);

        // Resolve field types for ADT variants
        if (!s.variants[i].field_types.empty()) {
            VariantFieldInfo vfi;
            for (auto &ft : s.variants[i].field_types) {
                vfi.fieldTypes.push_back(resolveType(ft));
                vfi.fieldTypeNames.push_back(ft);
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
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "if.end", fn_);

    for (auto &branch : s->branches) {
        llvm::Value *cond = emitExpr(*branch.condition);
        cond = toBool(cond);

        llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(*ctx_, "if.then", fn_);
        llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(*ctx_, "if.else", fn_);
        builder_.CreateCondBr(cond, thenBB, elseBB);

        builder_.SetInsertPoint(thenBB);
        pushScope();
        for (auto &stmt : branch.body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);
        popScope();
        if (!builder_.GetInsertBlock()->getTerminator())
            builder_.CreateBr(mergeBB);

        builder_.SetInsertPoint(elseBB);
    }

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


void CodeGen::emitStmt(ImportStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    codegenError("unresolved import: " + s.module_path +
                             " (ModuleLoader should have resolved this)");
}

void CodeGen::validateParallelFor(const ForStmt &s) {
    std::vector<std::unordered_set<std::string>> localScopes(1);
    localScopes.back().insert(s.var_name);
    if (s.var_name2)
        localScopes.back().insert(*s.var_name2);

    auto isLocal = [&](const std::string &name) {
        for (auto it = localScopes.rbegin(); it != localScopes.rend(); ++it) {
            if (it->count(name))
                return true;
        }
        return false;
    };

    std::function<void(const std::vector<StmtNode>&)> scanBlock;
    std::function<void(const StmtNode&)> scanStmt;

    scanBlock = [&](const std::vector<StmtNode> &body) {
        localScopes.push_back({});
        for (const auto &stmt : body)
            scanStmt(stmt);
        localScopes.pop_back();
    };

    scanStmt = [&](const StmtNode &stmt) {
        std::visit([&](const auto &node) {
            using T = std::decay_t<decltype(node)>;
            if constexpr (std::is_same_v<T, AssignStmt>) {
                if (!isLocal(node.name)) {
                    // Check if this is a first assignment (new local) or outer mutation
                    // If the variable already exists in the outer codegen scope, it's outer mutation
                    if (findVar(node.name))
                        codegenError(s.loc, "parallel for cannot assign to outer variable '" + node.name + "'");
                    // Otherwise it's a new local variable — register it
                    localScopes.back().insert(node.name);
                }
            } else if constexpr (std::is_same_v<T, TupleDestructStmt>) {
                for (const auto &name : node.names) {
                    if (name != "_")
                        localScopes.back().insert(name);
                }
            } else if constexpr (std::is_same_v<T, IndexAssignStmt>) {
                codegenError(node.loc, "parallel for does not allow indexed assignment");
            } else if constexpr (std::is_same_v<T, FieldAssignStmt>) {
                codegenError(node.loc, "parallel for does not allow field assignment");
            } else if constexpr (std::is_same_v<T, BreakStmt>) {
                codegenError(node.loc, "parallel for does not allow break");
            } else if constexpr (std::is_same_v<T, ContinueStmt>) {
                codegenError(node.loc, "parallel for does not allow continue");
            } else if constexpr (std::is_same_v<T, std::unique_ptr<IfStmt>>) {
                for (const auto &branch : node->branches)
                    scanBlock(branch.body);
                scanBlock(node->else_body);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<WhileStmt>>) {
                scanBlock(node->body);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<ForStmt>>) {
                scanBlock(node->body);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<MatchStmt>>) {
                for (const auto &arm : node->arms)
                    scanBlock(arm.body);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<FnStmt>>) {
                codegenError(node->loc, "parallel for does not allow nested function definitions");
            }
        }, stmt);
    };

    for (const auto &stmt : s.body)
        scanStmt(stmt);
}

void CodeGen::emitParallelForRange(ForStmt &s, llvm::Value *begin, llvm::Value *end, llvm::Value *step) {
    std::vector<std::pair<std::string, llvm::AllocaInst*>> captures;
    std::unordered_set<std::string> seen;
    for (auto scopeIt = scope_stack_.rbegin(); scopeIt != scope_stack_.rend(); ++scopeIt) {
        for (const auto &[name, alloca] : *scopeIt) {
            if (name == s.var_name || seen.count(name))
                continue;
            seen.insert(name);
            captures.push_back({name, alloca});
        }
    }

    std::vector<llvm::Type*> envFields;
    if (captures.empty())
        envFields.push_back(i8Ty_);
    else
        for (const auto &[_, alloca] : captures)
            envFields.push_back(alloca->getAllocatedType());
    llvm::StructType *envTy = llvm::StructType::get(*ctx_, envFields);

    llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t envSize = std::max<uint64_t>(1, dl.getTypeAllocSize(envTy));
    llvm::Value *envPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, envSize)}, "parallel_env");

    if (captures.empty()) {
        llvm::Value *dummyField = builder_.CreateStructGEP(envTy, envPtr, 0, "parallel_env_dummy");
        builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), dummyField);
    } else {
        for (size_t i = 0; i < captures.size(); ++i) {
            llvm::Value *fieldPtr = builder_.CreateStructGEP(envTy, envPtr, i, "parallel_env_field");
            llvm::AllocaInst *src = captures[i].second;
            builder_.CreateStore(
                builder_.CreateLoad(src->getAllocatedType(), src, captures[i].first + ".par_cap"),
                fieldPtr);
        }
    }

    llvm::FunctionType *thunkTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_, i64Ty_, i64Ty_}, false);
    llvm::Function *thunk = llvm::Function::Create(
        thunkTy, llvm::Function::InternalLinkage,
        "__ry_parallel_for." + std::to_string(lambda_counter_++), *mod_);

    {
        FnScope guard(*this);
        fn_ = thunk;
        pushScope();

        llvm::BasicBlock *entryBB = llvm::BasicBlock::Create(*ctx_, "entry", thunk);
        builder_.SetInsertPoint(entryBB);

        auto argIt = thunk->arg_begin();
        llvm::Value *envRaw = &*argIt++;
        envRaw->setName("env_raw");
        llvm::Value *chunkBegin = &*argIt++;
        chunkBegin->setName("chunk_begin");
        llvm::Value *chunkEnd = &*argIt++;
        chunkEnd->setName("chunk_end");
        llvm::Value *stepArg = &*argIt;
        stepArg->setName("step");

        llvm::Value *typedEnv = builder_.CreateBitCast(envRaw, ptrTy_, "parallel_env_typed");

        if (!captures.empty()) {
            for (size_t i = 0; i < captures.size(); ++i) {
                const auto &[name, src] = captures[i];
                llvm::Type *capTy = src->getAllocatedType();
                llvm::Value *fieldPtr = builder_.CreateStructGEP(envTy, typedEnv, i, name + ".field");
                llvm::AllocaInst *dst = builder_.CreateAlloca(capTy, nullptr, name);
                builder_.CreateStore(builder_.CreateLoad(capTy, fieldPtr, name + ".cap"), dst);
                scope_stack_.back()[name] = dst;

                if (auto it = list_element_types_.find(src); it != list_element_types_.end())
                    list_element_types_[dst] = it->second;
                if (auto it = nested_list_element_types_.find(src); it != nested_list_element_types_.end())
                    nested_list_element_types_[dst] = it->second;
                if (auto it = map_key_types_.find(src); it != map_key_types_.end())
                    map_key_types_[dst] = it->second;
                if (auto it = map_value_types_.find(src); it != map_value_types_.end())
                    map_value_types_[dst] = it->second;
                if (auto it = set_element_types_.find(src); it != set_element_types_.end())
                    set_element_types_[dst] = it->second;
                if (auto it = fn_type_info_.find(src); it != fn_type_info_.end())
                    fn_type_info_[dst] = it->second;
                if (auto it = task_result_types_.find(src); it != task_result_types_.end())
                    task_result_types_[dst] = it->second;
                if (auto it = union_value_types_.find(src); it != union_value_types_.end())
                    union_value_types_[dst] = it->second;
                if (auto it = enum_value_types_.find(src); it != enum_value_types_.end())
                    enum_value_types_[dst] = it->second;
                if (auto it = channel_element_types_.find(src); it != channel_element_types_.end())
                    channel_element_types_[dst] = it->second;
                propagateResourceTracking(src, dst);
            }
        }

        llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, s.var_name);
        builder_.CreateStore(chunkBegin, iVar);

        llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "parallel.cond", thunk);
        llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "parallel.body", thunk);
        llvm::BasicBlock *stepBB = llvm::BasicBlock::Create(*ctx_, "parallel.step", thunk);
        llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "parallel.end", thunk);

        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(condBB);
        llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "parallel_i");
        llvm::Value *stepPos = builder_.CreateICmpSGT(stepArg, llvm::ConstantInt::get(i64Ty_, 0), "parallel_step_pos");
        llvm::Value *posCond = builder_.CreateICmpSLT(iCur, chunkEnd, "parallel_pos_cond");
        llvm::Value *negCond = builder_.CreateICmpSGT(iCur, chunkEnd, "parallel_neg_cond");
        llvm::Value *loopCond = builder_.CreateSelect(stepPos, posCond, negCond, "parallel_cond");
        builder_.CreateCondBr(loopCond, bodyBB, endBB);

        builder_.SetInsertPoint(bodyBB);
        pushScope();
        scope_stack_.back()[s.var_name] = iVar;
        for (auto &stmt : s.body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);
        popScope();
        if (!builder_.GetInsertBlock()->getTerminator())
            builder_.CreateBr(stepBB);

        builder_.SetInsertPoint(stepBB);
        llvm::Value *iNext = builder_.CreateAdd(
            builder_.CreateLoad(i64Ty_, iVar, "parallel_i_step"), stepArg, "parallel_i_next");
        builder_.CreateStore(iNext, iVar);
        builder_.CreateBr(condBB);

        builder_.SetInsertPoint(endBB);
        builder_.CreateRetVoid();
    }

    llvm::FunctionType *parallelTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {i64Ty_, i64Ty_, i64Ty_, ptrTy_, ptrTy_}, false);
    llvm::FunctionCallee parallelFn = mod_->getOrInsertFunction("__ry_parallel_for_i64", parallelTy);
    builder_.CreateCall(parallelFn, {begin, end, step, envPtr, builder_.CreateBitCast(thunk, ptrTy_)});
}

void CodeGen::emitStmt(IndexAssignStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    llvm::Value *objPtr = emitExpr(*s.object);
    llvm::Value *key = emitExpr(*s.index);
    llvm::Value *val = emitExpr(*s.value);

    if (objPtr->getType() != ptrTy_)
        codegenError("index assignment requires list or map");

    llvm::Type *mapKeyTy = getMapKeyType(objPtr);
    if (mapKeyTy) {
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
        emitBucketInsertAndRehashCheck(objPtr, mapHeaderTy_, 0, 4, 5, key, mapKeyTy, curLen);

        builder_.CreateBr(endBB);

        builder_.SetInsertPoint(endBB);
        return;
    }

    // List index assignment
    llvm::Type *elemTy = getListElementType(objPtr);
    if (!elemTy)
        codegenError("cannot determine list element type for index assignment");

    if (key->getType() == i1Ty_)
        key = builder_.CreateZExt(key, i64Ty_, "idx_ext");

    // Bounds check
    llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, objPtr, 0, "len_ptr");
    llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "length");
    llvm::Value *negCheck = builder_.CreateICmpSLT(key, llvm::ConstantInt::get(i64Ty_, 0), "neg_check");
    llvm::Value *overCheck = builder_.CreateICmpSGE(key, length, "over_check");
    llvm::Value *outOfBounds = builder_.CreateOr(negCheck, overCheck, "oob");

    llvm::BasicBlock *oobBB = llvm::BasicBlock::Create(*ctx_, "idx_assign.oob", fn_);
    llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "idx_assign.ok", fn_);
    builder_.CreateCondBr(outOfBounds, oobBB, okBB);

    builder_.SetInsertPoint(oobBB);
    emitRuntimeError("runtime error: list index out of range\n", ".idx_assign_err");

    builder_.SetInsertPoint(okBB);
    llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, objPtr, 2, "data_ptr");
    llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "data");
    llvm::Value *elemPtr = builder_.CreateGEP(elemTy, dataPtr, {key}, "elem_ptr");
    builder_.CreateStore(val, elemPtr);
}
