#include "ry/codegen.hpp"
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <stdexcept>

// ===== Directive helpers =====

bool CodeGen::hasDirective(const std::vector<Directive> &directives, const std::string &name) {
    for (auto &d : directives)
        if (d.name == name) return true;
    return false;
}

void CodeGen::emitDeprecationWarning(const std::string &name) {
    warnings_.push_back("warning: '" + name + "' is deprecated");
}

// ===== B3: emitVarDecl =====

void CodeGen::emitVarDecl(const std::string &name,
                           const std::optional<std::string> &type_annotation,
                           ExprNode &value, bool is_immutable) {
    if (scope_stack_.back().count(name))
        throw std::runtime_error("redeclared variable: " + name);

    // Handle empty set/map literal with type annotation
    if (auto *se = std::get_if<std::unique_ptr<SetExpr>>(&value.data); se && (*se)->elements.empty()) {
        if (!type_annotation)
            throw std::runtime_error("empty {} literal requires type annotation");
        if (type_annotation->size() > 4 && type_annotation->substr(0, 4) == "Set<") {
            std::string inner = type_annotation->substr(4, type_annotation->size() - 5);
            llvm::Type *elemTy = resolveType(inner);

            llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
            llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
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
                throw std::runtime_error("invalid map type annotation: " + *type_annotation);

            llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
            llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
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
        throw std::runtime_error("empty {} requires Set<T> or Map<K, V> type annotation");
    }

    // Handle None literal
    if (auto *ve = std::get_if<VariableExpr>(&value.data); ve && ve->name == "None") {
        if (!type_annotation)
            throw std::runtime_error("type annotation required for None");
        llvm::Type *annotTy = resolveType(*type_annotation);
        if (!isOptionType(annotTy))
            throw std::runtime_error("None can only be assigned to Option type");
        llvm::Value *val = buildNoneValue(annotTy);
        llvm::AllocaInst *ptr = getOrCreateVar(name, annotTy);
        builder_.CreateStore(val, ptr);
        if (is_immutable)
            immutable_scope_stack_.back().insert(name);
        return;
    }

    llvm::Value *val = emitExpr(value);
    llvm::Type *newTy = val->getType();

    if (type_annotation) {
        llvm::Type *annotTy = resolveType(*type_annotation);
        if (annotTy != newTy) {
            if (annotTy == i8Ty_ && newTy == i64Ty_) {
                // int literal → byte: static range check for constants
                if (auto *ci = llvm::dyn_cast<llvm::ConstantInt>(val)) {
                    int64_t v = ci->getSExtValue();
                    if (v < 0 || v > 255)
                        throw std::runtime_error(
                            "byte value out of range (0-255): " + std::to_string(v));
                }
                val = builder_.CreateTrunc(val, i8Ty_, "bytetrunc");
                newTy = i8Ty_;
            } else if (isUnionType(*type_annotation)) {
                val = wrapInUnion(val, *type_annotation);
                newTy = val->getType();
            } else {
                throw std::runtime_error(
                    "type error: annotation '" + *type_annotation +
                    "' does not match expression type for variable '" + name + "'");
            }
        }
    }

    llvm::AllocaInst *ptr = getOrCreateVar(name, newTy);
    builder_.CreateStore(val, ptr);

    // Track union value type
    if (type_annotation && isUnionType(*type_annotation)) {
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

        // --- Function pointer tracking ---
        auto fnIt = fn_type_info_.find(val);
        if (fnIt != fn_type_info_.end()) {
            fn_type_info_[ptr] = fnIt->second;
        } else if (type_annotation && type_annotation->size() > 3 &&
                   type_annotation->substr(0, 3) == "fn(") {
            fn_type_info_[ptr] = parseFnTypeAnnotation(*type_annotation);
        }
    }

    // --- Enum value tracking (works for i64 values, not just ptr) ---
    {
        auto evIt = enum_value_types_.find(val);
        if (evIt != enum_value_types_.end())
            enum_value_types_[ptr] = evIt->second;
        else if (type_annotation && enum_types_.count(*type_annotation))
            enum_value_types_[ptr] = *type_annotation;
    }

    // --- Result type tracking ---
    {
        auto rvIt = result_value_types_.find(val);
        if (rvIt != result_value_types_.end())
            result_value_types_[ptr] = rvIt->second;
        else if (type_annotation && isResultTypeName(*type_annotation))
            result_value_types_[ptr] = *type_annotation;
    }

    if (is_immutable)
        immutable_scope_stack_.back().insert(name);
}

void CodeGen::emitStmt(LetStmt &s) {
    emitVarDecl(s.name, s.type_annotation, *s.value, true);
    if (hasDirective(s.directives, "deprecated"))
        deprecated_variables_.insert(s.name);
}
void CodeGen::emitStmt(VarStmt &s) {
    emitVarDecl(s.name, s.type_annotation, *s.value, false);
    if (hasDirective(s.directives, "deprecated"))
        deprecated_variables_.insert(s.name);
}

void CodeGen::emitStmt(AssignStmt &s) {
    llvm::AllocaInst *ptr = findVar(s.name);
    if (!ptr)
        throw std::runtime_error("undeclared variable: " + s.name);

    if (isImmutable(s.name))
        throw std::runtime_error("cannot reassign let variable: " + s.name);

    // Handle None literal in assignment
    if (auto *ve = std::get_if<VariableExpr>(&s.value->data); ve && ve->name == "None") {
        llvm::Type *varTy = ptr->getAllocatedType();
        if (!isOptionType(varTy))
            throw std::runtime_error("None can only be assigned to Option type");
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
                    throw std::runtime_error(
                        "byte value out of range (0-255): " + std::to_string(v));
            }
            val = builder_.CreateTrunc(val, i8Ty_, "bytetrunc");
        } else {
            auto uvIt = union_value_types_.find(ptr);
            if (uvIt != union_value_types_.end()) {
                val = wrapInUnion(val, uvIt->second);
            } else {
                throw std::runtime_error(
                    "type error: variable '" + s.name +
                    "' cannot be reassigned to a different type");
            }
        }
    }

    builder_.CreateStore(val, ptr);

    // Propagate fn_type_info_
    if (newTy == ptrTy_) {
        auto fnIt = fn_type_info_.find(val);
        if (fnIt != fn_type_info_.end())
            fn_type_info_[ptr] = fnIt->second;
    }
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
    // Evaluate iterable
    llvm::Value *iterable = emitExpr(*s->iterable);

    // Check if this is a list or set (ptr type with known element type)
    if (iterable->getType() != ptrTy_)
        throw std::runtime_error("for loop requires list or set iterable");

    // Map iteration: for k, v in map
    if (s->var_name2.has_value()) {
        llvm::Type *keyTy = getMapKeyType(iterable);
        llvm::Type *valTy = getMapValueType(iterable);
        if (!keyTy || !valTy)
            throw std::runtime_error("for k, v requires a map iterable");

        llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, iterable, 0, "map_len_ptr");
        llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "map_len");
        llvm::Value *keysPtrField = builder_.CreateStructGEP(mapHeaderTy_, iterable, 2, "keys_ptr_field");
        llvm::Value *keysPtr = builder_.CreateLoad(ptrTy_, keysPtrField, "keys_ptr");
        llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, iterable, 3, "vals_ptr_field");
        llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "vals_ptr");

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
        llvm::Value *keyPtr = builder_.CreateGEP(keyTy, keysPtr, {iCur}, "for_key_ptr");
        llvm::Value *key = builder_.CreateLoad(keyTy, keyPtr, "for_key");
        llvm::Value *valPtr = builder_.CreateGEP(valTy, valsPtr, {iCur}, "for_val_ptr");
        llvm::Value *val = builder_.CreateLoad(valTy, valPtr, "for_val");

        llvm::AllocaInst *keyVar = getOrCreateVar(s->var_name, keyTy);
        builder_.CreateStore(key, keyVar);
        llvm::AllocaInst *valVar = getOrCreateVar(*s->var_name2, valTy);
        builder_.CreateStore(val, valVar);

        for (auto &stmt : s->body)
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
        throw std::runtime_error("cannot determine element type for for loop iterable");

    // Get length
    llvm::Value *lenPtr = builder_.CreateStructGEP(headerTy, iterable, 0, "for_len_ptr");
    llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "for_len");

    // Get data pointer
    llvm::Value *dataPtrField = builder_.CreateStructGEP(headerTy, iterable, 2, "for_data_ptr");
    llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "for_data");

    // Create index variable
    llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "for_i");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "for.cond", fn_);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "for.body", fn_);
    llvm::BasicBlock *stepBB = llvm::BasicBlock::Create(*ctx_, "for.step", fn_);
    llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, "for.end", fn_);

    builder_.CreateBr(condBB);

    // cond: i < length
    builder_.SetInsertPoint(condBB);
    llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "i");
    llvm::Value *cond = builder_.CreateICmpSLT(iVal, length, "for_cond");
    builder_.CreateCondBr(cond, bodyBB, endBB);

    // body: load element and execute body
    builder_.SetInsertPoint(bodyBB);
    loop_stack_.push_back({stepBB, endBB});
    pushScope();

    llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "i_cur");
    llvm::Value *elemPtr = builder_.CreateGEP(elemTy, dataPtr, {iCur}, "for_elem_ptr");
    llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "for_elem");

    // Create loop variable in scope
    llvm::AllocaInst *loopVar = getOrCreateVar(s->var_name, elemTy);
    builder_.CreateStore(elem, loopVar);

    for (auto &stmt : s->body)
        std::visit([this](auto &st) { emitStmt(st); }, stmt);

    popScope();
    loop_stack_.pop_back();
    if (!builder_.GetInsertBlock()->getTerminator())
        builder_.CreateBr(stepBB);

    // step: i++
    builder_.SetInsertPoint(stepBB);
    llvm::Value *iNext = builder_.CreateAdd(
        builder_.CreateLoad(i64Ty_, iVar, "i_step"), llvm::ConstantInt::get(i64Ty_, 1), "i_next");
    builder_.CreateStore(iNext, iVar);
    builder_.CreateBr(condBB);

    builder_.SetInsertPoint(endBB);
}

void CodeGen::emitStmt(BreakStmt &) {
    if (loop_stack_.empty())
        throw std::runtime_error("break outside of loop");
    builder_.CreateBr(loop_stack_.back().second);
    // Create unreachable block for subsequent code
    llvm::BasicBlock *deadBB = llvm::BasicBlock::Create(*ctx_, "break.dead", fn_);
    builder_.SetInsertPoint(deadBB);
}

void CodeGen::emitStmt(ContinueStmt &) {
    if (loop_stack_.empty())
        throw std::runtime_error("continue outside of loop");
    builder_.CreateBr(loop_stack_.back().first);
    llvm::BasicBlock *deadBB = llvm::BasicBlock::Create(*ctx_, "continue.dead", fn_);
    builder_.SetInsertPoint(deadBB);
}

void CodeGen::emitStmt(FieldAssignStmt &s) {
    // Get the variable name from the object expression
    auto *varExpr = std::get_if<VariableExpr>(&s.object->data);
    if (!varExpr)
        throw std::runtime_error("field assignment requires variable on left side");

    llvm::AllocaInst *ptr = findVar(varExpr->name);
    if (!ptr)
        throw std::runtime_error("undefined variable: " + varExpr->name);

    if (isImmutable(varExpr->name))
        throw std::runtime_error("cannot modify field of let variable: " + varExpr->name);

    llvm::Type *varTy = ptr->getAllocatedType();
    llvm::StructType *structTy = llvm::dyn_cast<llvm::StructType>(varTy);
    if (!structTy)
        throw std::runtime_error("field assignment on non-struct type");

    std::string typeName = structTy->getName().str();
    auto it = struct_types_.find(typeName);
    if (it == struct_types_.end())
        throw std::runtime_error("unknown struct type: " + typeName);

    const auto &info = it->second;
    int fieldIdx = -1;
    for (unsigned i = 0; i < info.fields.size(); ++i) {
        if (info.fields[i].name == s.field) {
            fieldIdx = static_cast<int>(i);
            break;
        }
    }
    if (fieldIdx < 0)
        throw std::runtime_error("type '" + typeName + "' has no field '" + s.field + "'");

    llvm::Value *newVal = emitExpr(*s.value);
    llvm::Type *expectedTy = structTy->getElementType(fieldIdx);
    if (newVal->getType() != expectedTy)
        throw std::runtime_error("field '" + s.field + "' type mismatch");

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
        throw std::runtime_error("enum '" + s.name + "' is already defined");

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
        throw std::runtime_error("tuple destructuring requires a tuple value");
    if (structTy->getNumElements() != s.names.size())
        throw std::runtime_error("tuple destructuring: expected " +
            std::to_string(s.names.size()) + " elements but got " +
            std::to_string(structTy->getNumElements()));

    for (size_t i = 0; i < s.names.size(); ++i) {
        if (s.names[i] == "_")
            continue;
        // Redeclaration check (consistent with emitVarDecl)
        if (scope_stack_.back().count(s.names[i]))
            throw std::runtime_error("variable '" + s.names[i] + "' already declared in this scope");
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
    throw std::runtime_error("unresolved import: " + s.module_path +
                             " (ModuleLoader should have resolved this)");
}

void CodeGen::emitStmt(IndexAssignStmt &s) {
    llvm::Value *objPtr = emitExpr(*s.object);
    llvm::Value *key = emitExpr(*s.index);
    llvm::Value *val = emitExpr(*s.value);

    if (objPtr->getType() != ptrTy_)
        throw std::runtime_error("index assignment requires list or map");

    llvm::Type *mapKeyTy = getMapKeyType(objPtr);
    if (mapKeyTy) {
        // Map index assignment
        llvm::Type *mapValTy = getMapValueType(objPtr);
        if (!mapValTy)
            throw std::runtime_error("cannot determine map value type");
        if (key->getType() != mapKeyTy)
            throw std::runtime_error("map key type mismatch");
        if (val->getType() != mapValTy)
            throw std::runtime_error("map value type mismatch");

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

        llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);

        // New keys array
        llvm::Value *newKeySize = builder_.CreateMul(newCap, llvm::ConstantInt::get(i64Ty_, keySize), "new_key_size");
        llvm::Value *newKeysPtr = builder_.CreateCall(mallocFn, {newKeySize}, "new_keys");

        // New values array
        llvm::Value *newValSize = builder_.CreateMul(newCap, llvm::ConstantInt::get(i64Ty_, valSize), "new_val_size");
        llvm::Value *newValsPtr = builder_.CreateCall(mallocFn, {newValSize}, "new_vals");

        // memcpy old data
        llvm::FunctionType *memcpyTy = llvm::FunctionType::get(
            ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
        llvm::FunctionCallee memcpyFn = mod_->getOrInsertFunction("memcpy", memcpyTy);

        llvm::Value *keysPtrField2 = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 2, "keys_field");
        llvm::Value *oldKeysPtr = builder_.CreateLoad(ptrTy_, keysPtrField2, "old_keys");
        llvm::Value *oldKeySize = builder_.CreateMul(length, llvm::ConstantInt::get(i64Ty_, keySize), "old_key_size");
        builder_.CreateCall(memcpyFn, {newKeysPtr, oldKeysPtr, oldKeySize});

        llvm::Value *valsPtrField2 = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 3, "vals_field");
        llvm::Value *oldValsPtr = builder_.CreateLoad(ptrTy_, valsPtrField2, "old_vals");
        llvm::Value *oldValSize = builder_.CreateMul(length, llvm::ConstantInt::get(i64Ty_, valSize), "old_val_size");
        builder_.CreateCall(memcpyFn, {newValsPtr, oldValsPtr, oldValSize});

        // Free old arrays
        llvm::FunctionType *freeTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
        llvm::FunctionCallee freeFn = mod_->getOrInsertFunction("free", freeTy);
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
        throw std::runtime_error("cannot determine list element type for index assignment");

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

void CodeGen::emitStmt(ReturnStmt &s) {
    if (!s.value) {
        if (!fn_->getReturnType()->isVoidTy())
            throw std::runtime_error("return without value in non-Unit function");
        builder_.CreateRetVoid();
    } else {
        llvm::Value *val = emitExpr(*s.value);
        llvm::Type *retTy = fn_->getReturnType();
        if (retTy->isVoidTy())
            throw std::runtime_error("cannot return a value from Unit function '" +
                                     std::string(fn_->getName()) + "'");
        if (val->getType() != retTy) {
            if (isUnionType(current_fn_return_type_)) {
                val = wrapInUnion(val, current_fn_return_type_);
            } else {
                throw std::runtime_error("return type mismatch");
            }
        }

        // Emit ensure checks (postconditions) before return
        if (current_postconditions_ && !current_postconditions_->empty()) {
            if (result_alloca_)
                builder_.CreateStore(val, result_alloca_);
            in_ensure_context_ = true;
            std::string fnName = fn_->getName().str();
            for (int i = 0; i < static_cast<int>(current_postconditions_->size()); ++i)
                emitContractCheck("ensure", fnName, (*current_postconditions_)[i]);
            in_ensure_context_ = false;
            if (result_alloca_)
                val = builder_.CreateLoad(retTy, result_alloca_, "ensure_result");
        }

        builder_.CreateRet(val);
    }
    llvm::BasicBlock *deadBB = llvm::BasicBlock::Create(*ctx_, "dead", fn_);
    builder_.SetInsertPoint(deadBB);
}

// ===== B5: FnStmt using FnScope RAII =====

void CodeGen::emitStmt(std::unique_ptr<FnStmt> &s) {
    std::vector<llvm::Type*> paramTypes;
    for (auto &p : s->params)
        paramTypes.push_back(resolveType(p.type));
    llvm::Type *retTy = resolveType(s->return_type);

    // Check for duplicate signatures
    auto &overloads = functions_[s->name];
    for (auto &entry : overloads) {
        if (entry.paramTypes == paramTypes) {
            if (entry.func->getReturnType() == retTy)
                throw std::runtime_error("function '" + s->name +
                    "' is already defined with the same signature");
            else
                throw std::runtime_error("function '" + s->name +
                    "': overloads with same parameter types but different return types");
        }
    }

    // LLVM IR function name: first overload uses original name, subsequent use name.N
    std::string irName = s->name;
    if (!overloads.empty())
        irName = s->name + "." + std::to_string(overloads.size());

    llvm::FunctionType *ft = llvm::FunctionType::get(retTy, paramTypes, false);
    llvm::Function *func = llvm::Function::Create(
        ft, llvm::Function::ExternalLinkage, irName, *mod_);

    std::vector<std::string> paramTypeNames;
    for (auto &p : s->params)
        paramTypeNames.push_back(p.type);
    overloads.push_back({func, paramTypes, paramTypeNames, s->return_type});

    if (hasDirective(s->directives, "deprecated"))
        deprecated_functions_.insert(s->name);

    {
        FnScope guard(*this);
        fn_ = func;
        current_fn_return_type_ = s->return_type;
        pushScope();

        llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", func);
        builder_.SetInsertPoint(entry);

        unsigned idx = 0;
        for (auto &arg : func->args()) {
            arg.setName(s->params[idx].name);
            llvm::AllocaInst *alloca = builder_.CreateAlloca(
                paramTypes[idx], nullptr, s->params[idx].name);
            builder_.CreateStore(&arg, alloca);
            scope_stack_.back()[s->params[idx].name] = alloca;
            // Track list element type for list parameters
            const std::string &ptype = s->params[idx].type;
            if (ptype.size() > 5 && ptype.substr(0, 5) == "List<" && ptype.back() == '>') {
                std::string inner = ptype.substr(5, ptype.size() - 6);
                list_element_types_[alloca] = resolveType(inner);
            }
            // Track set element type for set parameters
            if (ptype.size() > 4 && ptype.substr(0, 4) == "Set<" && ptype.back() == '>') {
                std::string inner = ptype.substr(4, ptype.size() - 5);
                set_element_types_[alloca] = resolveType(inner);
            }
            // Track enum type for enum parameters
            if (enum_types_.count(ptype)) {
                enum_value_types_[alloca] = ptype;
            }
            // Track map key/value types for map parameters
            if (ptype.size() > 4 && ptype.substr(0, 4) == "Map<" && ptype.back() == '>') {
                auto [kTy, vTy] = parseMapTypeAnnotation(ptype);
                if (kTy) map_key_types_[alloca] = kTy;
                if (vTy) map_value_types_[alloca] = vTy;
            }
            // Track fn type info for function-typed parameters
            if (ptype.size() > 3 && ptype.substr(0, 3) == "fn(") {
                fn_type_info_[alloca] = parseFnTypeAnnotation(ptype);
            }
            // Track union type for union parameters
            if (isUnionType(ptype)) {
                union_value_types_[alloca] = normalizeUnionType(ptype);
            }
            // Track Result type for result parameters
            if (isResultTypeName(ptype)) {
                result_value_types_[alloca] = ptype;
            }
            ++idx;
        }

        // Save old values for postconditions (before require checks)
        old_value_map_.clear();
        if (!s->postconditions.empty()) {
            std::vector<OldExpr*> oldExprs;
            for (auto &post : s->postconditions)
                collectOldExprs(*post, oldExprs);
            for (auto *oe : oldExprs) {
                llvm::Value *val = emitExpr(*oe->expr);
                llvm::AllocaInst *alloca = builder_.CreateAlloca(val->getType(), nullptr, "old_val");
                builder_.CreateStore(val, alloca);
                old_value_map_[oe] = alloca;
            }
        }

        // Emit require checks (preconditions)
        for (int i = 0; i < static_cast<int>(s->preconditions.size()); ++i)
            emitContractCheck("require", s->name, s->preconditions[i]);

        // Set up postcondition context
        current_postconditions_ = s->postconditions.empty() ? nullptr : &s->postconditions;
        if (!s->postconditions.empty() && !retTy->isVoidTy()) {
            result_alloca_ = builder_.CreateAlloca(retTy, nullptr, "contract_result");
        } else {
            result_alloca_ = nullptr;
        }

        for (auto &stmt : s->body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);

        if (!builder_.GetInsertBlock()->getTerminator()) {
            llvm::Value *defaultRet = nullptr;
            if (retTy->isVoidTy()) {
                // no default value needed
            } else if (retTy == i64Ty_) {
                defaultRet = llvm::ConstantInt::get(i64Ty_, 0);
            } else if (retTy == i8Ty_) {
                defaultRet = llvm::ConstantInt::get(i8Ty_, 0);
            } else if (retTy == f64Ty_) {
                defaultRet = llvm::ConstantFP::get(f64Ty_, 0.0);
            } else if (retTy == i1Ty_) {
                defaultRet = llvm::ConstantInt::get(i1Ty_, 0);
            } else if (retTy == ptrTy_) {
                defaultRet = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
            } else if (llvm::isa<llvm::StructType>(retTy)) {
                defaultRet = llvm::UndefValue::get(retTy);
            }

            // Emit ensure checks on implicit return path
            if (defaultRet && current_postconditions_ && !current_postconditions_->empty()) {
                if (result_alloca_)
                    builder_.CreateStore(defaultRet, result_alloca_);
                in_ensure_context_ = true;
                std::string fnName = fn_->getName().str();
                for (int i = 0; i < static_cast<int>(current_postconditions_->size()); ++i)
                    emitContractCheck("ensure", fnName, (*current_postconditions_)[i]);
                in_ensure_context_ = false;
            }

            if (retTy->isVoidTy())
                builder_.CreateRetVoid();
            else if (defaultRet)
                builder_.CreateRet(defaultRet);
        }

        // Reset contract context
        current_postconditions_ = nullptr;
        result_alloca_ = nullptr;
        old_value_map_.clear();

        std::string err;
        llvm::raw_string_ostream errStream(err);
        if (llvm::verifyFunction(*func, &errStream))
            throw std::runtime_error("IR verify error in function '" + s->name + "': " + err);
    }
    // FnScope destructor restores fn_, scope_stack_, immutable_scope_stack_, builder_ insert point
}

// ===== Contract helpers =====

void CodeGen::emitContractCheck(const std::string &kind, const std::string &fn_name,
                                 const ExprPtr &cond) {
    llvm::Value *condVal = emitExpr(*cond);
    condVal = toBool(condVal);

    std::string errName = ".contract_err_" + std::to_string(contract_err_counter_++);
    std::string suffix = (kind == "invariant") ? "" : "()";
    std::string preposition = (kind == "invariant") ? " for " : " in ";
    std::string msg = "Contract violation: " + kind + " failed" + preposition + fn_name + suffix + "\n";

    llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, kind + ".fail", fn_);
    llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, kind + ".ok", fn_);

    builder_.CreateCondBr(condVal, nextBB, failBB);

    builder_.SetInsertPoint(failBB);
    emitRuntimeError(msg, errName);

    builder_.SetInsertPoint(nextBB);
}

void CodeGen::collectOldExprs(const ExprNode &node, std::vector<OldExpr*> &out) {
    std::visit([this, &out](const auto &e) {
        using T = std::decay_t<decltype(e)>;
        if constexpr (std::is_same_v<T, std::unique_ptr<OldExpr>>) {
            out.push_back(e.get());
        } else if constexpr (std::is_same_v<T, std::unique_ptr<BinaryExpr>>) {
            collectOldExprs(*e->lhs, out);
            collectOldExprs(*e->rhs, out);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<UnaryExpr>>) {
            collectOldExprs(*e->operand, out);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<CallExpr>>) {
            for (auto &arg : e->args)
                collectOldExprs(*arg, out);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<FieldAccessExpr>>) {
            collectOldExprs(*e->object, out);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<IndexExpr>>) {
            collectOldExprs(*e->object, out);
            collectOldExprs(*e->index, out);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<TupleExpr>>) {
            for (auto &elem : e->elements)
                collectOldExprs(*elem, out);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<ListExpr>>) {
            for (auto &elem : e->elements)
                collectOldExprs(*elem, out);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<MapExpr>>) {
            for (auto &k : e->keys)
                collectOldExprs(*k, out);
            for (auto &v : e->values)
                collectOldExprs(*v, out);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<SetExpr>>) {
            for (auto &elem : e->elements)
                collectOldExprs(*elem, out);
        }
    }, node.data);
}

void CodeGen::emitInvariantCheck(const std::string &typeName, const StructInfo &info,
                                  llvm::Value *structVal) {
    if (info.invariants.empty()) return;

    pushScope();
    for (unsigned f = 0; f < info.fields.size(); ++f) {
        llvm::Type *fieldTy = info.llvmType->getElementType(f);
        llvm::AllocaInst *fieldAlloca = builder_.CreateAlloca(fieldTy, nullptr, info.fields[f].name);
        llvm::Value *fieldVal = builder_.CreateExtractValue(structVal, f, info.fields[f].name + "_val");
        builder_.CreateStore(fieldVal, fieldAlloca);
        scope_stack_.back()[info.fields[f].name] = fieldAlloca;
    }
    for (int i = 0; i < static_cast<int>(info.invariants.size()); ++i)
        emitContractCheck("invariant", typeName, info.invariants[i]);
    popScope();
}

void CodeGen::instantiateGenericEnum(const std::string &fullName, const std::string &baseName,
                                      const std::vector<std::string> &typeArgs) {
    if (enum_types_.count(fullName))
        return; // already instantiated

    auto it = generic_enum_templates_.find(baseName);
    if (it == generic_enum_templates_.end())
        throw std::runtime_error("unknown generic enum: " + baseName);

    auto &tmpl = it->second;
    if (typeArgs.size() != tmpl.typeParams.size())
        throw std::runtime_error("generic enum '" + baseName + "' expects " +
            std::to_string(tmpl.typeParams.size()) + " type parameters");

    // Build type parameter mapping
    std::unordered_map<std::string, std::string> typeMap;
    for (size_t i = 0; i < tmpl.typeParams.size(); ++i)
        typeMap[tmpl.typeParams[i]] = typeArgs[i];

    // Create a concrete EnumStmt by substituting type parameters
    EnumInfo info;
    info.name = fullName;
    info.variantCount = tmpl.variants.size();

    bool hasADT = false;
    std::vector<llvm::Constant*> nameStrings;
    for (size_t i = 0; i < tmpl.variants.size(); ++i) {
        auto &v = tmpl.variants[i];
        info.variants[v.name] = static_cast<int64_t>(i);
        llvm::Constant *str = builder_.CreateGlobalString(
            v.name, ".enum_" + fullName + "_" + v.name);
        nameStrings.push_back(str);

        if (!v.field_types.empty()) {
            hasADT = true;
            VariantFieldInfo vfi;
            for (auto &ft : v.field_types) {
                std::string resolved = ft;
                auto mit = typeMap.find(ft);
                if (mit != typeMap.end()) resolved = mit->second;
                vfi.fieldTypes.push_back(resolveType(resolved));
                vfi.fieldTypeNames.push_back(resolved);
            }
            info.variantFields[v.name] = std::move(vfi);
        }
    }
    info.isADT = hasADT;

    auto *arrTy = llvm::ArrayType::get(ptrTy_, tmpl.variants.size());
    auto *init = llvm::ConstantArray::get(arrTy, nameStrings);
    auto *gv = new llvm::GlobalVariable(
        *mod_, arrTy, true, llvm::GlobalValue::PrivateLinkage,
        init, ".enum_names_" + fullName);
    info.nameArray = gv;

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
            *ctx_, {i64Ty_, payloadTy}, "enum." + fullName);
    }

    enum_types_[fullName] = std::move(info);
}
