#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"
#include "ry/sema_return.hpp"
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

void CodeGen::registerResourceByTypeName(const std::string &typeName, llvm::Value *val) {
    static const std::pair<const char*, ResourceKind> table[] = {
        {"TcpListener", RK_TcpListener}, {"TcpStream", RK_TcpStream},
        {"TlsStream", RK_TlsStream}, {"HttpRequest", RK_HttpRequest},
        {"HttpResponse", RK_HttpResponse}, {"HttpClientResponse", RK_HttpClientResponse},
        {"JsonValue", RK_JsonValue},
    };
    for (auto &[name, rk] : table)
        if (typeName == name) { resource_sets_[rk].insert(val); return; }
}

void CodeGen::emitStmt(AwaitStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    emitCoverage(s.loc);

    auto awaitExpr = std::make_unique<AwaitExpr>();
    awaitExpr->operand = std::move(s.operand);
    auto node = std::make_unique<ExprNode>();
    node->data = std::move(awaitExpr);
    node->loc = s.loc;
    (void)emitExpr(*node);
}

void CodeGen::emitStmt(ReturnStmt &s) {
    if (s.loc.isValid()) current_loc_ = s.loc;
    emitCoverage(s.loc);
    if (!s.value) {
        if (isAnyType(fn_->getReturnType())) {
            llvm::Value *unitAny = buildUnitAny();
            emitEnsureChecks(unitAny);
            builder_.CreateRet(unitAny);
        } else if (!fn_->getReturnType()->isVoidTy()) {
            codegenError("return without value in non-Unit function");
        } else {
            builder_.CreateRetVoid();
        }
    } else {
        llvm::Value *val = emitExpr(*s.value);
        llvm::Type *retTy = fn_->getReturnType();
        if (retTy->isVoidTy())
            codegenError("cannot return a value from Unit function '" +
                                     std::string(fn_->getName()) + "'");
        if (val->getType() != retTy) {
            if (isAnyType(retTy)) {
                val = wrapInAny(val);
            } else if (isUnionType(current_fn_return_type_)) {
                val = wrapInUnion(val, current_fn_return_type_);
            } else {
                // Try tuple element coercion (e.g., Option<int> none → Option<Error>)
                auto *retST = llvm::dyn_cast<llvm::StructType>(retTy);
                auto *valST = llvm::dyn_cast<llvm::StructType>(val->getType());
                if (!retST || !valST || retST->getNumElements() != valST->getNumElements())
                    codegenError("return type mismatch");

                // Find which elements need coercion
                bool needsCoercion = false;
                for (unsigned i = 0; i < retST->getNumElements(); ++i) {
                    if (valST->getElementType(i) != retST->getElementType(i)) {
                        if (!(isOptionType(valST->getElementType(i)) &&
                              isOptionType(retST->getElementType(i))))
                            codegenError("return type mismatch");
                        needsCoercion = true;
                    }
                }
                if (needsCoercion) {
                    llvm::Value *coerced = llvm::UndefValue::get(retTy);
                    for (unsigned i = 0; i < retST->getNumElements(); ++i) {
                        llvm::Value *elem = builder_.CreateExtractValue(val, i);
                        if (valST->getElementType(i) != retST->getElementType(i))
                            elem = buildNoneValue(retST->getElementType(i));
                        coerced = builder_.CreateInsertValue(coerced, elem, i);
                    }
                    val = coerced;
                }
            }
        }

        // Emit ensure checks (postconditions) before return
        emitEnsureChecks(val);

        builder_.CreateRet(val);
    }
    llvm::BasicBlock *deadBB = llvm::BasicBlock::Create(*ctx_, "dead", fn_);
    builder_.SetInsertPoint(deadBB);
}

// ===== B5: FnStmt using FnScope RAII =====

void CodeGen::emitStmt(std::unique_ptr<FnStmt> &s) {
    if (s->loc.isValid()) current_loc_ = s->loc;
    emitCoverage(s->loc);

    // Generic function: save as template, don't instantiate yet
    if (!s->type_params.empty()) {
        GenericFnTemplate tmpl;
        std::string name = s->name;
        tmpl.fnStmt = std::move(s);
        generic_fn_templates_[name] = std::move(tmpl);
        return;
    }

    if (hasDirective(s->directives, "native")) {
        if (s->is_async)
            codegenError("async native functions are not supported");
        if (hasDirective(s->directives, "inline"))
            codegenError("@inline cannot be used with @native functions");
        if (hasDirective(s->directives, "deprecated"))
            deprecated_functions_.insert(s->name);

        // Validate return type for comparison/logical operators
        if (s->is_operator && isBoolConstrainedOperator(s->name)) {
            if (!s->return_type.empty() && s->return_type != "bool") {
                codegenError("operator '" + operatorSymbol(s->name) + "' must return 'bool', but returns '" +
                             s->return_type + "'");
            }
        }

        // Register argument count for native function overload
        native_fn_arg_counts_[s->name].push_back(s->params.size());
        return;
    }

    std::vector<llvm::Type*> paramTypes;
    for (auto &p : s->params)
        paramTypes.push_back(resolveType(p.type));

    llvm::Type *bodyRetTy;
    if (s->return_type.empty()) {
        // Infer return type from body
        std::unordered_map<std::string, llvm::Type*> paramTypeMap;
        for (auto &p : s->params)
            paramTypeMap[p.name] = resolveType(p.type);
        std::vector<llvm::Type*> retTypes;
        collectReturnTypes(s->body, paramTypeMap, retTypes);
        bodyRetTy = deduceReturnType(retTypes);
        // Build return type name string
        // Deduplicate for name construction
        std::vector<llvm::Type*> unique;
        for (auto *ty : retTypes)
            if (std::find(unique.begin(), unique.end(), ty) == unique.end())
                unique.push_back(ty);
        if (unique.size() <= 1) {
            s->return_type = reverseResolveTypeName(bodyRetTy);
        } else {
            std::string unionName;
            for (size_t i = 0; i < unique.size(); ++i) {
                if (i > 0) unionName += " | ";
                unionName += reverseResolveTypeName(unique[i]);
            }
            s->return_type = unionName;
        }
    } else {
        bodyRetTy = resolveType(s->return_type);
    }

    // Validate return type for comparison/logical operators
    if (s->is_operator && isBoolConstrainedOperator(s->name)) {
        if (bodyRetTy != llvm::Type::getInt1Ty(*ctx_)) {
            codegenError("operator '" + operatorSymbol(s->name) + "' must return 'bool', but returns '" +
                         s->return_type + "'");
        }
    }

    // Check that non-Unit, non-any functions return on all paths
    if (!isAnyType(bodyRetTy) && !bodyRetTy->isVoidTy()
        && !hasDirective(s->directives, "native")) {
        if (!allPathsReturn(s->body))
            codegenError("function '" + s->name + "' with return type '" +
                         s->return_type + "' does not return a value on all code paths");
    }
    std::string exposedReturnTypeName = s->is_async ? "Task<" + s->return_type + ">" : s->return_type;
    llvm::Type *exposedRetTy = s->is_async ? resolveType(exposedReturnTypeName) : bodyRetTy;

    // Check for duplicate signatures
    auto &overloads = functions_[s->name];
    for (auto &entry : overloads) {
        if (entry.paramTypes == paramTypes) {
            if (entry.func->getReturnType() == exposedRetTy)
                codegenError("function '" + s->name +
                    "' is already defined with the same signature");
            else
                codegenError("function '" + s->name +
                    "': overloads with same parameter types but different return types");
        }
    }

    // LLVM IR function name: first overload uses original name, subsequent use name.N
    std::string irName = s->name;
    if (!overloads.empty())
        irName = s->name + "." + std::to_string(overloads.size());

    llvm::FunctionType *ft = llvm::FunctionType::get(exposedRetTy, paramTypes, false);
    llvm::Function *func = llvm::Function::Create(
        ft, llvm::Function::ExternalLinkage, irName, *mod_);

    std::vector<std::string> paramTypeNames;
    for (auto &p : s->params)
        paramTypeNames.push_back(p.type);
    overloads.push_back({func, paramTypes, paramTypeNames, exposedReturnTypeName});

    if (hasDirective(s->directives, "deprecated"))
        deprecated_functions_.insert(s->name);

    if (hasDirective(s->directives, "inline")) {
        std::string mode = "always";
        for (auto &d : s->directives) {
            if (d.name == "inline") {
                for (auto &p : d.params) {
                    if (p.key == "mode") mode = p.value;
                }
            }
        }
        if (mode == "always")
            func->addFnAttr(llvm::Attribute::AlwaysInline);
        else if (mode == "never")
            func->addFnAttr(llvm::Attribute::NoInline);
        else if (mode == "hint")
            func->addFnAttr(llvm::Attribute::InlineHint);
        else
            codegenError("unknown @inline mode: '" + mode +
                         "' (expected 'always', 'never', or 'hint')");
    }

    auto emitFunctionBody = [&](llvm::Function *targetFunc, llvm::Type *retTy,
                                const std::string &returnTypeName, const std::string &fnNameForErrors) {
        FnScope guard(*this);
        fn_ = targetFunc;
        current_fn_return_type_ = returnTypeName;
        pushScope();

        llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", targetFunc);
        builder_.SetInsertPoint(entry);

        unsigned idx = 0;
        for (auto &arg : targetFunc->args()) {
            arg.setName(s->params[idx].name);
            llvm::AllocaInst *alloca = builder_.CreateAlloca(
                paramTypes[idx], nullptr, s->params[idx].name);
            builder_.CreateStore(&arg, alloca);
            scope_stack_.back()[s->params[idx].name] = alloca;
            // Track list element type for list parameters
            const std::string &ptype = s->params[idx].type;
            if (ptype.size() > 5 && ptype.compare(0, 5, "List<") == 0 && ptype.back() == '>') {
                std::string inner = ptype.substr(5, ptype.size() - 6);
                type_meta_[TM_ListElem][alloca] = resolveType(inner);
            }
            // Track set element type for set parameters
            if (ptype.size() > 4 && ptype.compare(0, 4, "Set<") == 0 && ptype.back() == '>') {
                std::string inner = ptype.substr(4, ptype.size() - 5);
                type_meta_[TM_SetElem][alloca] = resolveType(inner);
            }
            // Track enum type for enum parameters
            if (enum_types_.count(ptype)) {
                enum_value_types_[alloca] = ptype;
            }
            // Track map key/value types for map parameters
            if (ptype.size() > 4 && ptype.compare(0, 4, "Map<") == 0 && ptype.back() == '>') {
                auto [kTy, vTy] = parseMapTypeAnnotation(ptype);
                if (kTy) type_meta_[TM_MapKey][alloca] = kTy;
                if (vTy) type_meta_[TM_MapValue][alloca] = vTy;
            }
            if (ptype.size() > 5 && ptype.compare(0, 5, "Task<") == 0 && ptype.back() == '>') {
                std::string inner = ptype.substr(5, ptype.size() - 6);
                type_meta_[TM_TaskResult][alloca] = resolveType(inner);
            }
            registerResourceByTypeName(ptype, alloca);
            // Track low-level type metadata for parameters
            if (isLowLevelTypeName(ptype))
                low_level_type_names_[alloca] = ptype;
            // Track fn type info and constraint check (shared alias resolution)
            {
                std::string resolvedPtype = resolveTypeAlias(ptype);
                if (resolvedPtype.size() > 3 && resolvedPtype.compare(0, 3, "fn(") == 0) {
                    fn_type_info_[alloca] = parseFnTypeAnnotation(resolvedPtype);
                }
                auto constraint = parseTypeConstraint(resolvedPtype);
                if (constraint) {
                    type_constraints_[alloca] = *constraint;
                    llvm::Value *argVal = builder_.CreateLoad(
                        paramTypes[idx], alloca, s->params[idx].name + ".load");
                    emitConstraintCheck(argVal, *constraint, s->params[idx].name);
                } else {
                    // Track union type only for non-literal unions
                    if (isUnionType(ptype))
                        union_value_types_[alloca] = normalizeUnionType(ptype);
                }
            }
            ++idx;
        }

        // Emit require checks (preconditions)
        for (int i = 0; i < static_cast<int>(s->preconditions.size()); ++i)
            emitContractCheck("require", s->name, s->preconditions[i]);

        // Set up postcondition context
        current_postconditions_ = s->postconditions.empty() ? nullptr : &s->postconditions;
        ensure_bindings_ = s->ensure_bindings.empty() ? nullptr : &s->ensure_bindings;

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
            } else if (isAnyType(retTy)) {
                defaultRet = buildUnitAny();
            } else if (llvm::isa<llvm::StructType>(retTy)) {
                defaultRet = llvm::UndefValue::get(retTy);
            }

            // Emit ensure checks on implicit return path
            if (defaultRet)
                emitEnsureChecks(defaultRet);

            if (retTy->isVoidTy())
                builder_.CreateRetVoid();
            else if (defaultRet)
                builder_.CreateRet(defaultRet);
            else
                builder_.CreateRet(llvm::UndefValue::get(retTy));
        }

        std::string err;
        llvm::raw_string_ostream errStream(err);
        if (llvm::verifyFunction(*targetFunc, &errStream))
            codegenError("IR verify error in function '" + fnNameForErrors + "': " + err);
    };

    if (!s->is_async) {
        emitFunctionBody(func, bodyRetTy, s->return_type, s->name);
        return;
    }

    llvm::FunctionType *bodyFt = llvm::FunctionType::get(bodyRetTy, paramTypes, false);
    llvm::Function *bodyFunc = llvm::Function::Create(
        bodyFt, llvm::Function::InternalLinkage, irName + ".__async_body", *mod_);
    emitFunctionBody(bodyFunc, bodyRetTy, s->return_type, s->name);

    std::vector<llvm::Type*> envFields = paramTypes;
    if (envFields.empty())
        envFields.push_back(i8Ty_);
    llvm::StructType *envTy = llvm::StructType::get(*ctx_, envFields);

    llvm::FunctionType *thunkTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_}, false);
    llvm::Function *thunk = llvm::Function::Create(
        thunkTy, llvm::Function::InternalLinkage,
        "__ry_async." + std::to_string(lambda_counter_++), *mod_);

    // FnScope destructor restores fn_, scope_stack_, immutable_scope_stack_, builder_ insert point,
    // and contract state (current_postconditions_, result_alloca_, in_ensure_context_, old_value_map_)

    {
        FnScope guard(*this);
        fn_ = thunk;
        llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", thunk);
        builder_.SetInsertPoint(entry);

        auto argIt = thunk->arg_begin();
        llvm::Value *envRaw = &*argIt++;
        envRaw->setName("env_raw");
        llvm::Value *outRaw = &*argIt;
        outRaw->setName("out_raw");

        llvm::Value *typedEnv = builder_.CreateBitCast(envRaw, ptrTy_, "async_env_typed");
        std::vector<llvm::Value*> thunkArgs;
        for (size_t i = 0; i < paramTypes.size(); ++i) {
            llvm::Value *argField = builder_.CreateStructGEP(
                envTy, typedEnv, i, "async_arg_field." + std::to_string(i));
            thunkArgs.push_back(builder_.CreateLoad(paramTypes[i], argField, "async_arg." + std::to_string(i)));
        }

        llvm::Value *result = builder_.CreateCall(bodyFunc, thunkArgs, bodyRetTy->isVoidTy() ? "" : "async_result");
        if (!bodyRetTy->isVoidTy()) {
            llvm::Value *outTyped = builder_.CreateBitCast(outRaw, ptrTy_, "async_out_typed");
            builder_.CreateStore(result, outTyped);
        }
        builder_.CreateRetVoid();
    }

    {
        FnScope guard(*this);
        fn_ = func;
        current_fn_return_type_ = exposedReturnTypeName;
        llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", func);
        builder_.SetInsertPoint(entry);

        llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
        const llvm::DataLayout &dl = mod_->getDataLayout();
        llvm::Value *envPtr = builder_.CreateCall(
            mallocFn,
            {llvm::ConstantInt::get(i64Ty_, std::max<uint64_t>(1, dl.getTypeAllocSize(envTy)))},
            "async_env");

        if (paramTypes.empty()) {
            llvm::Value *dummyField = builder_.CreateStructGEP(envTy, envPtr, 0, "async_env_dummy");
            builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), dummyField);
        } else {
            size_t idx = 0;
            for (auto &arg : func->args()) {
                llvm::Value *argField = builder_.CreateStructGEP(
                    envTy, envPtr, idx++, "async_env_arg");
                builder_.CreateStore(&arg, argField);
            }
        }

        llvm::FunctionType *spawnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
        llvm::FunctionCallee spawnFn = mod_->getOrInsertFunction("__ry_task_spawn", spawnTy);
        llvm::Value *task = builder_.CreateCall(
            spawnFn,
            {
                builder_.CreateBitCast(thunk, ptrTy_),
                envPtr,
                llvm::ConstantInt::get(i64Ty_, bodyRetTy->isVoidTy() ? 0 : dl.getTypeAllocSize(bodyRetTy))
            },
            "task");
        builder_.CreateRet(task);

        std::string err;
        llvm::raw_string_ostream errStream(err);
        if (llvm::verifyFunction(*func, &errStream))
            codegenError("IR verify error in function '" + s->name + "': " + err);
    }
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

void CodeGen::emitEnsureChecks(llvm::Value *retVal) {
    if (!current_postconditions_ || current_postconditions_->empty() || !ensure_bindings_)
        return;
    pushScope();
    auto &bindings = *ensure_bindings_;
    if (bindings.size() == 1) {
        llvm::AllocaInst *alloca = builder_.CreateAlloca(retVal->getType(), nullptr, bindings[0]);
        builder_.CreateStore(retVal, alloca);
        scope_stack_.back()[bindings[0]] = alloca;
        immutable_scope_stack_.back().insert(bindings[0]);
    } else {
        auto *structTy = llvm::dyn_cast<llvm::StructType>(retVal->getType());
        if (!structTy || !structTy->isLiteral() || structTy->getNumElements() != bindings.size())
            codegenError("ensure destructuring requires tuple return; binding count does not match tuple element count");
        for (unsigned i = 0; i < bindings.size(); ++i) {
            llvm::Value *elem = builder_.CreateExtractValue(retVal, i);
            llvm::AllocaInst *alloca = builder_.CreateAlloca(elem->getType(), nullptr, bindings[i]);
            builder_.CreateStore(elem, alloca);
            scope_stack_.back()[bindings[i]] = alloca;
            immutable_scope_stack_.back().insert(bindings[i]);
        }
    }
    in_ensure_context_ = true;
    std::string fnName = fn_->getName().str();
    for (int i = 0; i < static_cast<int>(current_postconditions_->size()); ++i)
        emitContractCheck("ensure", fnName, (*current_postconditions_)[i]);
    in_ensure_context_ = false;
    popScope();
}

void CodeGen::instantiateGenericEnum(const std::string &fullName, const std::string &baseName,
                                      const std::vector<std::string> &typeArgs) {
    if (enum_types_.count(fullName))
        return; // already instantiated

    auto it = generic_enum_templates_.find(baseName);
    if (it == generic_enum_templates_.end())
        codegenError("unknown generic enum: " + baseName);

    auto &tmpl = it->second;
    if (typeArgs.size() != tmpl.typeParams.size())
        codegenError("generic enum '" + baseName + "' expects " +
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
        llvm::Constant *str = cachedGlobalString(
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

// ===== Generic function type inference =====

std::string CodeGen::reverseResolveType(llvm::Value *val) {
    // Check low-level type metadata first (resolves ambiguous LLVM types)
    std::string llName = getLowLevelTypeName(val);
    if (!llName.empty()) return llName;

    llvm::Type *ty = val->getType();
    if (ty == i64Ty_) return "int";
    if (ty == f64Ty_) return "float";
    if (ty == i1Ty_)  return "bool";
    if (ty == i8Ty_)  return "byte";
    if (ty == i16Ty_) return "i16";
    if (ty == i32Ty_) return "i32";
    if (ty == f32Ty_) return "f32";
    if (isAnyType(ty)) return "any";

    if (ty == ptrTy_) {
        // Look through LoadInst to find metadata on the underlying alloca
        llvm::Value *origin = val;
        if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val))
            origin = load->getPointerOperand();

        auto lit = type_meta_[TM_ListElem].find(origin);
        if (lit == type_meta_[TM_ListElem].end())
            lit = type_meta_[TM_ListElem].find(val);
        if (lit != type_meta_[TM_ListElem].end())
            return "List<" + reverseResolveType(
                llvm::UndefValue::get(lit->second)) + ">";

        auto fit = fn_type_info_.find(origin);
        if (fit == fn_type_info_.end())
            fit = fn_type_info_.find(val);
        if (fit != fn_type_info_.end()) {
            std::string result = "fn(";
            for (size_t i = 0; i < fit->second.paramTypeNames.size(); ++i) {
                if (i > 0) result += ",";
                result += fit->second.paramTypeNames[i];
            }
            result += ")";
            if (fit->second.returnType && !fit->second.returnType->isVoidTy())
                result += " -> " + reverseResolveType(
                    llvm::UndefValue::get(fit->second.returnType));
            return result;
        }
        return "str";
    }

    if (auto *st = llvm::dyn_cast<llvm::StructType>(ty)) {
        for (auto &[name, info] : struct_types_)
            if (info.llvmType == st) return name;
    }

    return "any";
}

std::vector<std::string> CodeGen::inferTypeArgs(
    const std::string &baseName, const std::vector<ExprPtr> &args) {

    auto it = generic_fn_templates_.find(baseName);
    if (it == generic_fn_templates_.end()) return {};

    const FnStmt &tmpl = *it->second.fnStmt;
    if (args.size() != tmpl.params.size()) return {};

    std::unordered_map<std::string, std::string> inferred;
    std::unordered_set<std::string> typeParamSet(
        tmpl.type_params.begin(), tmpl.type_params.end());

    // Use AST-based type inference to avoid emitting IR for arguments
    std::unordered_map<std::string, llvm::Type*> emptyParamMap;
    for (size_t i = 0; i < args.size(); ++i) {
        const std::string &paramType = tmpl.params[i].type;
        llvm::Type *argTy = inferExprType(*args[i], emptyParamMap);

        if (typeParamSet.count(paramType)) {
            std::string resolved;
            if (argTy == i64Ty_)  resolved = "int";
            else if (argTy == f64Ty_) resolved = "float";
            else if (argTy == i1Ty_)  resolved = "bool";
            else if (argTy == i8Ty_)  resolved = "byte";
            else if (argTy == ptrTy_) resolved = "str";
            else if (isAnyType(argTy)) resolved = "any";
            else resolved = "any";

            if (inferred.count(paramType) && inferred[paramType] != resolved)
                codegenError("conflicting type inference for '" + paramType +
                             "': '" + inferred[paramType] + "' vs '" + resolved + "'");
            inferred[paramType] = resolved;
        }
    }

    // Build result in template parameter order
    std::vector<std::string> result;
    for (auto &tp : tmpl.type_params) {
        auto found = inferred.find(tp);
        if (found == inferred.end())
            codegenError("could not infer type parameter '" + tp +
                         "' in call to generic function '" + baseName + "'");
        result.push_back(found->second);
    }
    return result;
}

// ===== Generic function instantiation =====

void CodeGen::instantiateGenericFn(const std::string &baseName,
                                    const std::vector<std::string> &typeArgs) {
    // Build full name: "identity<int>" or "map<int,str>"
    std::string fullName = baseName + "<";
    for (size_t i = 0; i < typeArgs.size(); ++i) {
        if (i > 0) fullName += ",";
        fullName += typeArgs[i];
    }
    fullName += ">";

    // Check cache
    if (generic_fn_instantiated_.count(fullName))
        return;

    auto it = generic_fn_templates_.find(baseName);
    if (it == generic_fn_templates_.end())
        codegenError("undefined generic function: " + baseName);

    FnStmt &s = *it->second.fnStmt;

    if (typeArgs.size() != s.type_params.size())
        codegenError("generic function '" + baseName + "' expects " +
                     std::to_string(s.type_params.size()) + " type argument(s), got " +
                     std::to_string(typeArgs.size()));

    // Set type parameter scope
    auto savedScope = std::move(type_param_scope_);
    type_param_scope_.clear();
    for (size_t i = 0; i < s.type_params.size(); ++i)
        type_param_scope_[s.type_params[i]] = typeArgs[i];

    // Resolve parameter types and return type using type_param_scope_
    std::vector<llvm::Type*> paramTypes;
    for (auto &p : s.params)
        paramTypes.push_back(resolveType(p.type));
    llvm::Type *bodyRetTy = resolveType(s.return_type);

    // Substitute type params in return type name
    std::string exposedReturnTypeName = s.return_type;
    auto retTpit = type_param_scope_.find(s.return_type);
    if (retTpit != type_param_scope_.end())
        exposedReturnTypeName = retTpit->second;
    llvm::Type *exposedRetTy = bodyRetTy;

    // Register in functions_ before body emission (enables recursion)
    std::string irName = fullName;
    llvm::FunctionType *ft = llvm::FunctionType::get(exposedRetTy, paramTypes, false);
    llvm::Function *func = llvm::Function::Create(
        ft, llvm::Function::InternalLinkage, irName, *mod_);

    std::vector<std::string> paramTypeNames;
    for (auto &p : s.params) {
        // Substitute type params in param type names for FnTypeInfo
        std::string resolvedName = p.type;
        auto tpit = type_param_scope_.find(p.type);
        if (tpit != type_param_scope_.end())
            resolvedName = tpit->second;
        paramTypeNames.push_back(resolvedName);
    }
    functions_[fullName].push_back({func, paramTypes, paramTypeNames, exposedReturnTypeName});
    generic_fn_instantiated_.insert(fullName);

    // Emit function body
    {
        FnScope guard(*this);
        fn_ = func;
        current_fn_return_type_ = s.return_type;
        pushScope();

        llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", func);
        builder_.SetInsertPoint(entry);

        unsigned idx = 0;
        for (auto &arg : func->args()) {
            arg.setName(s.params[idx].name);
            llvm::AllocaInst *alloca = builder_.CreateAlloca(
                paramTypes[idx], nullptr, s.params[idx].name);
            builder_.CreateStore(&arg, alloca);
            scope_stack_.back()[s.params[idx].name] = alloca;

            // Resolve the actual param type name (with substitution)
            std::string ptype = paramTypeNames[idx];

            // Track collection element types
            if (ptype.size() > 5 && ptype.compare(0, 5, "List<") == 0 && ptype.back() == '>') {
                std::string inner = ptype.substr(5, ptype.size() - 6);
                type_meta_[TM_ListElem][alloca] = resolveType(inner);
            }
            if (ptype.size() > 4 && ptype.compare(0, 4, "Set<") == 0 && ptype.back() == '>') {
                std::string inner = ptype.substr(4, ptype.size() - 5);
                type_meta_[TM_SetElem][alloca] = resolveType(inner);
            }
            if (enum_types_.count(ptype))
                enum_value_types_[alloca] = ptype;
            if (ptype.size() > 4 && ptype.compare(0, 4, "Map<") == 0 && ptype.back() == '>') {
                auto [kTy, vTy] = parseMapTypeAnnotation(ptype);
                if (kTy) type_meta_[TM_MapKey][alloca] = kTy;
                if (vTy) type_meta_[TM_MapValue][alloca] = vTy;
            }
            registerResourceByTypeName(ptype, alloca);
            // Track low-level type metadata for parameters
            if (isLowLevelTypeName(ptype))
                low_level_type_names_[alloca] = ptype;
            {
                std::string resolvedPtype = resolveTypeAlias(ptype);
                if (resolvedPtype.size() > 3 && resolvedPtype.compare(0, 3, "fn(") == 0)
                    fn_type_info_[alloca] = parseFnTypeAnnotation(resolvedPtype);
                if (isUnionType(ptype))
                    union_value_types_[alloca] = normalizeUnionType(ptype);
            }
            ++idx;
        }

        for (auto &stmt : s.body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);

        if (!builder_.GetInsertBlock()->getTerminator()) {
            llvm::Value *defaultRet = nullptr;
            if (bodyRetTy->isVoidTy()) {
                // no default value needed
            } else if (bodyRetTy == i64Ty_) {
                defaultRet = llvm::ConstantInt::get(i64Ty_, 0);
            } else if (bodyRetTy == f64Ty_) {
                defaultRet = llvm::ConstantFP::get(f64Ty_, 0.0);
            } else if (bodyRetTy == i1Ty_) {
                defaultRet = llvm::ConstantInt::get(i1Ty_, 0);
            } else if (bodyRetTy == ptrTy_) {
                defaultRet = llvm::ConstantPointerNull::get(
                    llvm::cast<llvm::PointerType>(ptrTy_));
            } else if (isAnyType(bodyRetTy)) {
                defaultRet = buildUnitAny();
            } else if (llvm::isa<llvm::StructType>(bodyRetTy)) {
                defaultRet = llvm::UndefValue::get(bodyRetTy);
            }

            if (bodyRetTy->isVoidTy())
                builder_.CreateRetVoid();
            else if (defaultRet)
                builder_.CreateRet(defaultRet);
            else
                builder_.CreateRet(llvm::UndefValue::get(bodyRetTy));
        }

        std::string err;
        llvm::raw_string_ostream errStream(err);
        if (llvm::verifyFunction(*func, &errStream))
            codegenError("IR verify error in generic function '" + fullName + "': " + err);
    }

    // Restore type parameter scope
    type_param_scope_ = std::move(savedScope);
}
