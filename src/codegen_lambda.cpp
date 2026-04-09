#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"
#include "ry/diagnostic.hpp"
#include "ry/sema_return.hpp"
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <functional>


namespace ry {

// ===== Free-variable analysis (shared between lambda and nested named functions) =====

CodeGen::CaptureAnalysisResult CodeGen::analyzeFreeVariables(
    const std::vector<StmtNode> &body,
    const ExprPtr &expr_body,
    const std::unordered_set<std::string> &paramNames,
    bool emitLoads) {

    CaptureAnalysisResult result;
    std::unordered_set<std::string> found;
    // Mutable copy so nested FnStmt params can be temporarily added
    std::unordered_set<std::string> excludedNames = paramNames;

    std::function<void(const ExprNode&)> scanExpr;
    std::function<void(const StmtNode&)> scanStmt;

    auto tryCaptureVar = [&](const std::string &varName) {
        if (excludedNames.count(varName) || found.count(varName))
            return;
        llvm::AllocaInst *alloca = findVar(varName);
        if (!alloca)
            return;
        found.insert(varName);
        result.capturedNames.push_back(varName);
        llvm::Type *capType = alloca->getAllocatedType();
        if (emitLoads) {
            llvm::Value *val = builder_.CreateLoad(capType, alloca, varName + ".cap");
            result.capturedValues.push_back(val);
        } else {
            result.capturedValues.push_back(nullptr);
        }
        result.capturedTypes.push_back(capType);
        auto cak = detectCapturedArcKind(alloca);
        result.capturedArcKinds.push_back(cak);
        if (cak == CapturedArcKind::Resource) {
            auto rmIt = resource_managed_vars_.find(alloca);
            result.capturedResourceKinds.push_back(
                rmIt != resource_managed_vars_.end() ? rmIt->second : ResourceKindRegistry::NONE);
        } else {
            result.capturedResourceKinds.push_back(ResourceKindRegistry::NONE);
        }
        auto *fnMeta = getMeta(alloca);
        if (fnMeta && fnMeta->fn_type_info)
            result.capturedClosureInfos[result.capturedNames.size() - 1] = *fnMeta->fn_type_info;
    };

    std::function<void(const Pattern &)> excludePatternBindings =
        [&](const Pattern &pat) {
        std::visit([&](const auto &p) {
            using P = std::decay_t<decltype(p)>;
            if constexpr (std::is_same_v<P, VariablePattern>) {
                if (p.name != "_") excludedNames.insert(p.name);
            } else if constexpr (std::is_same_v<P, SomePattern>) {
                if (p.binding != "_") excludedNames.insert(p.binding);
            } else if constexpr (std::is_same_v<P, OkPattern>) {
                if (p.binding != "_") excludedNames.insert(p.binding);
            } else if constexpr (std::is_same_v<P, ErrPattern>) {
                if (p.binding != "_") excludedNames.insert(p.binding);
            } else if constexpr (std::is_same_v<P, EnumConstructorPattern>) {
                for (auto &b : p.bindings)
                    if (b != "_") excludedNames.insert(b);
            } else if constexpr (std::is_same_v<P, std::unique_ptr<OrPattern>>) {
                if (p) {
                    for (const auto &alt : p->alternatives)
                        excludePatternBindings(alt);
                }
            }
        }, pat);
    };

    scanExpr = [&](const ExprNode &node) {
        std::visit([&](const auto &v) {
            using T = std::decay_t<decltype(v)>;
            if constexpr (std::is_same_v<T, VariableExpr>) {
                tryCaptureVar(v.name);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<BinaryExpr>>) {
                scanExpr(*v->lhs); scanExpr(*v->rhs);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<UnaryExpr>>) {
                scanExpr(*v->operand);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<CallExpr>>) {
                tryCaptureVar(v->callee);
                for (auto &arg : v->args) scanExpr(*arg);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<FieldAccessExpr>>) {
                scanExpr(*v->object);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<TupleExpr>>) {
                for (auto &el : v->elements) scanExpr(*el);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<ListExpr>>) {
                for (auto &el : v->elements) scanExpr(*el);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<IndexExpr>>) {
                scanExpr(*v->object); for (auto &idx : v->indices) scanExpr(*idx);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<MapExpr>>) {
                for (auto &k : v->keys) scanExpr(*k);
                for (auto &val : v->values) scanExpr(*val);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<LambdaExpr>>) {
                if (v->expr_body) scanExpr(*v->expr_body);
                for (auto &st : v->body) scanStmt(st);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<InterpolatedStringExpr>>) {
                for (auto &expr : v->exprs) scanExpr(*expr);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<SetExpr>>) {
                for (auto &el : v->elements) scanExpr(*el);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<CastExpr>>) {
                scanExpr(*v->value);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<WhenCondExpr>>) {
                for (auto &arm : v->arms) {
                    scanExpr(*arm.condition);
                    scanExpr(*arm.value);
                }
                if (v->else_expr) scanExpr(*v->else_expr);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<MatchExpr>>) {
                scanExpr(*v->subject);
                for (auto &arm : v->arms) {
                    auto savedExcluded = excludedNames;
                    excludePatternBindings(arm.pattern);
                    if (arm.guard) scanExpr(*arm.guard);
                    scanExpr(*arm.value);
                    excludedNames = std::move(savedExcluded);
                }
            } else if constexpr (std::is_same_v<T, std::unique_ptr<RangeExpr>>) {
                if (v->start) scanExpr(*v->start);
                if (v->end) scanExpr(*v->end);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<ErrorPropagateExpr>>) {
                scanExpr(*v->operand);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<AwaitExpr>>) {
                scanExpr(*v->operand);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<WeakExpr>>) {
                scanExpr(*v->operand);
            }
        }, node.data);
    };

    scanStmt = [&](const StmtNode &stmt) {
        std::visit([&](const auto &s) {
            using T = std::decay_t<decltype(s)>;
            if constexpr (std::is_same_v<T, AssignStmt>) {
                tryCaptureVar(s.name);
                if (s.value) scanExpr(*s.value);
            } else if constexpr (std::is_same_v<T, CallStmt>) {
                tryCaptureVar(s.callee);
                for (auto &arg : s.args) scanExpr(*arg);
            } else if constexpr (std::is_same_v<T, ReturnStmt>) {
                if (s.value) scanExpr(*s.value);
            } else if constexpr (std::is_same_v<T, IndexAssignStmt>) {
                scanExpr(*s.object); for (auto &idx : s.indices) scanExpr(*idx); scanExpr(*s.value);
            } else if constexpr (std::is_same_v<T, FieldAssignStmt>) {
                scanExpr(*s.object); scanExpr(*s.value);
            } else if constexpr (std::is_same_v<T, TupleDestructStmt>) {
                scanExpr(*s.value);
            } else if constexpr (std::is_same_v<T, ExprStmt>) {
                scanExpr(*s.expr);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<IfStmt>>) {
                scanExpr(*s->branch.condition);
                for (auto &st : s->branch.body) scanStmt(st);
                for (auto &st : s->else_body) scanStmt(st);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<WhenCondStmt>>) {
                for (auto &arm : s->arms) {
                    scanExpr(*arm.condition);
                    for (auto &st : arm.body) scanStmt(st);
                }
                for (auto &st : s->else_body) scanStmt(st);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<WhileStmt>>) {
                scanExpr(*s->condition);
                for (auto &st : s->body) scanStmt(st);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<ForStmt>>) {
                scanExpr(*s->iterable);
                for (auto &st : s->body) scanStmt(st);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<FnStmt>>) {
                auto savedExcluded = excludedNames;
                for (auto &p : s->params) excludedNames.insert(p.name);
                for (auto &st : s->body) scanStmt(st);
                excludedNames = std::move(savedExcluded);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<MatchStmt>>) {
                scanExpr(*s->subject);
                for (auto &arm : s->arms) {
                    auto savedExcluded = excludedNames;
                    excludePatternBindings(arm.pattern);
                    if (arm.guard) scanExpr(*arm.guard);
                    for (auto &st : arm.body) scanStmt(st);
                    excludedNames = std::move(savedExcluded);
                }
            } else if constexpr (std::is_same_v<T, ExpectStmt>) {
                scanExpr(*s.actual);
                if (s.expected) scanExpr(*s.expected);
            }
        }, stmt);
    };

    if (expr_body) {
        scanExpr(*expr_body);
    } else {
        for (auto &stmt : body)
            scanStmt(stmt);
    }

    // Snapshot @const status before FnScope clears immutable_scope_stack_
    result.capturedIsConst.reserve(result.capturedNames.size());
    for (auto &name : result.capturedNames)
        result.capturedIsConst.push_back(isImmutable(name));

    return result;
}

// ===== Closure struct builder (shared between lambda and nested named functions) =====

llvm::Value *CodeGen::buildClosureStruct(
    llvm::Function *func,
    const FnTypeInfo &info,
    const std::vector<llvm::Value*> &capturedValues) {

    if (capturedValues.empty())
        return func;

    std::vector<llvm::Type*> closureFields;
    closureFields.push_back(ptrTy_);  // function pointer
    for (auto *t : info.capturedTypes)
        closureFields.push_back(t);
    llvm::StructType *closureTy = llvm::StructType::get(*ctx_, closureFields);

    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t closureSize = dl.getTypeAllocSize(closureTy);
    auto *arcHeader = emitArcAlloc(llvm::ConstantInt::get(i64Ty_, closureSize));
    llvm::Value *closurePtr = emitArcGetDataPtr(arcHeader);

    // Store function pointer
    llvm::Value *fnField = builder_.CreateStructGEP(closureTy, closurePtr, 0, "closure.fn");
    builder_.CreateStore(func, fnField);

    // Store captured values and retain ARC-managed ones
    for (size_t i = 0; i < capturedValues.size(); ++i) {
        llvm::Value *capField = builder_.CreateStructGEP(
            closureTy, closurePtr, i + 1, "closure.cap." + std::to_string(i));
        builder_.CreateStore(capturedValues[i], capField);
        if (info.capturedArcKinds[i] != CapturedArcKind::None) {
            auto *hdr = emitArcGetHeaderFromData(capturedValues[i]);
            emitArcRetain(hdr, false);
        }
    }

    getOrCreateMeta(closurePtr).fn_type_info = info;
    return closurePtr;
}

// ===== LambdaExpr =====

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<LambdaExpr> &e) {
    // Build a set of parameter names
    std::unordered_set<std::string> paramNames;
    for (auto &p : e->params)
        paramNames.insert(p.name);

    // Run free-variable analysis
    auto captures = analyzeFreeVariables(e->body, e->expr_body, paramNames);

    // Build parameter types (user params + captured vars)
    std::vector<llvm::Type*> paramTypes;
    for (auto &p : e->params)
        paramTypes.push_back(resolveType(p.type->toString()));
    std::vector<llvm::Type*> allParamTypes = paramTypes;
    for (auto *t : captures.capturedTypes)
        allParamTypes.push_back(t);

    llvm::Type *retTy;
    std::string retTypeStr = e->return_type ? e->return_type->toString() : "";
    std::string returnTypeName;
    if (retTypeStr == "any") {
        retTy = anyTy_;
    } else if (!e->return_type) {
        // Infer return type when omitted
        std::unordered_map<std::string, llvm::Type*> paramTypeMap;
        for (auto &p : e->params)
            paramTypeMap[p.name] = resolveType(p.type->toString());

        if (e->expr_body) {
            retTy = inferExprType(*e->expr_body, paramTypeMap);
            returnTypeName = inferExprTypeName(*e->expr_body, paramTypeMap);
        } else {
            buildLocalTypeMap(e->body, paramTypeMap);
            retTy = inferReturnType(e->body, paramTypeMap);
        }
    } else {
        retTy = resolveType(retTypeStr);
        returnTypeName = retTypeStr;
    }

    // Check that block-bodied lambdas with explicit non-any/Unit return type
    // return on all paths
    if (!e->expr_body && e->return_type
        && !isAnyType(retTy) && !retTy->isVoidTy()) {
        if (!allPathsReturn(e->body, buildEnumVariantRegistry()))
            codegenError("lambda with return type '" + retTypeStr +
                         "' does not return a value on all code paths");
    }

    // Create the LLVM function
    std::string lambdaName = "__lambda." + std::to_string(lambda_counter_++);
    llvm::FunctionType *ft = llvm::FunctionType::get(retTy, allParamTypes, false);
    llvm::Function *func = llvm::Function::Create(
        ft, llvm::Function::InternalLinkage, lambdaName, *mod_);

    {
        FnScope guard(*this);
        fn_ = func;
        current_fn_return_type_ = retTypeStr;
        pushScope();

        llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", func);
        builder_.SetInsertPoint(entry);

        // Set up user parameters
        unsigned idx = 0;
        for (auto &arg : func->args()) {
            if (idx < e->params.size()) {
                arg.setName(e->params[idx].name);
                llvm::AllocaInst *alloca = builder_.CreateAlloca(
                    paramTypes[idx], nullptr, e->params[idx].name);
                builder_.CreateStore(&arg, alloca);
                scope_stack_.back()[e->params[idx].name] = alloca;
                const std::string ptype = e->params[idx].type->toString();
                applyParamTypeMeta(ptype, alloca, paramTypes[idx], e->params[idx].name);
            } else {
                // Captured variable
                size_t capIdx = idx - e->params.size();
                arg.setName(captures.capturedNames[capIdx] + ".cap");
                llvm::AllocaInst *alloca = builder_.CreateAlloca(
                    captures.capturedTypes[capIdx], nullptr, captures.capturedNames[capIdx]);
                builder_.CreateStore(&arg, alloca);
                scope_stack_.back()[captures.capturedNames[capIdx]] = alloca;
                captured_vars_.insert(alloca);
                if (captures.capturedIsConst[capIdx])
                    immutable_scope_stack_.back().insert(captures.capturedNames[capIdx]);
                // Propagate fn_type_info for captured function-type variables
                auto closureIt = captures.capturedClosureInfos.find(capIdx);
                if (closureIt != captures.capturedClosureInfos.end())
                    getOrCreateMeta(alloca).fn_type_info = closureIt->second;
            }
            ++idx;
        }

        // Forward-declare nested functions in multi-line lambda body
        if (!e->expr_body && !e->body.empty())
            forwardDeclareNestedFunctions(e->body);

        // Emit body
        if (e->expr_body) {
            llvm::Value *val = emitExpr(*e->expr_body);
            if (isAnyType(retTy) && !isAnyType(val->getType()))
                val = wrapInAny(val);
            else if (val->getType() != retTy) {
                if (isUnionType(retTypeStr))
                    val = wrapInUnion(val, retTypeStr);
                else if (auto *sliced = tryEmitSubtypeCoerce(val, retTy))
                    val = sliced;
                else {
                    auto *retST = llvm::dyn_cast<llvm::StructType>(retTy);
                    auto *valST = llvm::dyn_cast<llvm::StructType>(val->getType());
                    if (retST && valST &&
                        retST->getNumElements() == valST->getNumElements()) {
                        bool needsCoercion = false;
                        bool canCoerce = true;
                        for (unsigned i = 0; i < retST->getNumElements(); ++i) {
                            if (valST->getElementType(i) != retST->getElementType(i)) {
                                if (isOptionType(valST->getElementType(i)) &&
                                    isOptionType(retST->getElementType(i)))
                                    needsCoercion = true;
                                else
                                    canCoerce = false;
                            }
                        }
                        if (needsCoercion && canCoerce) {
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
            }
            if (val->getType() != retTy)
                codegenError("lambda expression return type mismatch: expected '" +
                    reverseResolveTypeName(retTy) + "', found '" +
                    reverseResolveTypeName(val->getType()) + "'");
            builder_.CreateRet(val);
        } else {
            for (auto &stmt : e->body)
                std::visit([this](auto &st) { emitStmt(st); }, stmt);

            if (!builder_.GetInsertBlock()->getTerminator()) {
                if (retTy->isVoidTy())
                    builder_.CreateRetVoid();
                else if (isAnyType(retTy))
                    builder_.CreateRet(buildUnitAny());
                else if (retTy == i64Ty_)
                    builder_.CreateRet(llvm::ConstantInt::get(i64Ty_, 0));
                else if (retTy == f64Ty_)
                    builder_.CreateRet(llvm::ConstantFP::get(f64Ty_, 0.0));
                else if (retTy == i1Ty_)
                    builder_.CreateRet(llvm::ConstantInt::get(i1Ty_, 0));
                else if (retTy == ptrTy_)
                    builder_.CreateRet(llvm::ConstantPointerNull::get(
                        llvm::cast<llvm::PointerType>(ptrTy_)));
                else
                    builder_.CreateRet(llvm::UndefValue::get(retTy));
            }
        }

        std::string err;
        llvm::raw_string_ostream errStream(err);
        if (llvm::verifyFunction(*func, &errStream))
            codegenError("IR verify error in lambda: " + err);
    }

    // Register fn_type_info for the function pointer value
    FnTypeInfo info;
    info.paramTypes = paramTypes;  // only the user-visible params
    for (auto &p : e->params)
        info.paramTypeNames.push_back(p.type->toString());
    info.returnType = retTy;
    info.returnTypeName = returnTypeName;
    if (!retTypeStr.empty()) {
        std::string resolvedRetType = resolveTypeAlias(retTypeStr);
        if (isFunctionTypeName(resolvedRetType))
            info.returnFnTypeInfo = std::make_unique<FnTypeInfo>(parseFnTypeAnnotation(resolvedRetType));
    }
    info.capturedVars = captures.capturedNames;
    info.capturedTypes = captures.capturedTypes;
    info.capturedArcKinds = captures.capturedArcKinds;
    info.capturedResourceKinds = captures.capturedResourceKinds;
    if (!captures.capturedClosureInfos.empty())
        info.capturedClosureInfos = std::make_unique<std::unordered_map<size_t, FnTypeInfo>>(std::move(captures.capturedClosureInfos));
    info.sourceFn = func;
    getOrCreateMeta(func).fn_type_info = info;

    return buildClosureStruct(func, info, captures.capturedValues);
}

// ===== Lambda return type inference =====

void CodeGen::buildLocalTypeMap(const std::vector<StmtNode> &body,
    std::unordered_map<std::string, llvm::Type*> &typeMap) {
    for (auto &stmt : body) {
        std::visit([&](const auto &s) {
            using T = std::decay_t<decltype(s)>;
            if constexpr (std::is_same_v<T, AssignStmt>) {
                if (s.value && !s.compound_op &&
                    typeMap.find(s.name) == typeMap.end()) {
                    if (s.type_annotation) {
                        if (auto *ty = tryResolveType(s.type_annotation->toString()))
                            typeMap[s.name] = ty;
                    } else {
                        typeMap[s.name] = inferExprType(*s.value, typeMap);
                    }
                }
            } else if constexpr (std::is_same_v<T, std::unique_ptr<IfStmt>>) {
                buildLocalTypeMap(s->branch.body, typeMap);
                buildLocalTypeMap(s->else_body, typeMap);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<WhileStmt>>) {
                buildLocalTypeMap(s->body, typeMap);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<ForStmt>>) {
                buildLocalTypeMap(s->body, typeMap);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<WhenCondStmt>>) {
                for (auto &arm : s->arms)
                    buildLocalTypeMap(arm.body, typeMap);
                buildLocalTypeMap(s->else_body, typeMap);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<MatchStmt>>) {
                for (auto &arm : s->arms)
                    buildLocalTypeMap(arm.body, typeMap);
            }
        }, stmt);
    }
}

llvm::Type *CodeGen::inferExprType(const ExprNode &expr,
    const std::unordered_map<std::string, llvm::Type*> &paramTypeMap) {
    return std::visit([&](const auto &v) -> llvm::Type* {
        using T = std::decay_t<decltype(v)>;
        if constexpr (std::is_same_v<T, NumberExpr>) {
            return v.suffix.empty() ? i64Ty_ : resolveType(v.suffix);
        } else if constexpr (std::is_same_v<T, FloatExpr>) {
            return v.suffix.empty() ? f64Ty_ : resolveType(v.suffix);
        } else if constexpr (std::is_same_v<T, BoolExpr>) {
            return i1Ty_;
        } else if constexpr (std::is_same_v<T, StringExpr>) {
            return ptrTy_;
        } else if constexpr (std::is_same_v<T, VariableExpr>) {
            auto it = paramTypeMap.find(v.name);
            if (it != paramTypeMap.end())
                return it->second;
            if (llvm::AllocaInst *alloca = findVar(v.name))
                return alloca->getAllocatedType();
            return i64Ty_; // fallback
        } else if constexpr (std::is_same_v<T, std::unique_ptr<BinaryExpr>>) {
            const std::string &op = v->op;
            if (op == "==" || op == "!=" || op == "<" || op == "<=" ||
                op == ">" || op == ">=" || op == "and" || op == "or")
                return i1Ty_;
            llvm::Type *lhsTy = inferExprType(*v->lhs, paramTypeMap);
            llvm::Type *rhsTy = inferExprType(*v->rhs, paramTypeMap);
            if (isAnyType(lhsTy) || isAnyType(rhsTy))
                return anyTy_;
            if (op == "+") {
                if (lhsTy == ptrTy_ || rhsTy == ptrTy_)
                    return ptrTy_;
            }
            if (lhsTy == f64Ty_ || rhsTy == f64Ty_)
                return f64Ty_;
            return i64Ty_;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<UnaryExpr>>) {
            if (v->op == "not")
                return i1Ty_;
            llvm::Type *opTy = inferExprType(*v->operand, paramTypeMap);
            if (isAnyType(opTy)) return anyTy_;
            return opTy;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<CallExpr>>) {
            // Look up the function return type
            auto *itOverloads = findFunction(v->callee);
            if (itOverloads && !itOverloads->empty())
                return (*itOverloads)[0].func->getReturnType();
            // Check if it's a struct constructor
            auto sit = struct_types_.find(v->callee);
            if (sit != struct_types_.end())
                return sit->second.llvmType;
            // Known builtin return types
            const std::string &c = v->callee;
            if (c == "length" || c == "to_int" || c == "find")
                return i64Ty_;
            // sum/min/max/first/last return the element type of the list argument
            if (c == "sum" || c == "min" || c == "max" || c == "first" || c == "last") {
                if (!v->args.empty()) {
                    llvm::Type *argTy = inferExprType(*v->args[0], paramTypeMap);
                    // If the argument is a pointer (list), we can't determine element type
                    // at inference time, so default to i64; actual codegen handles correctly
                    (void)argTy;
                }
                return i64Ty_; // conservative default; codegen uses actual element type
            }
            if (c == "to_float")
                return f64Ty_;
            if (c == "contains" || c == "starts_with" || c == "ends_with" ||
                c == "has_key" || c == "any" || c == "all" || c == "is_empty")
                return i1Ty_;
            if (c == "to_str" || c == "to_upper" || c == "to_lower" ||
                c == "trim" || c == "trim_start" || c == "trim_end" ||
                c == "substring" || c == "char_at" || c == "replace" ||
                c == "repeat" || c == "reverse" || c == "join" ||
                c == "filter" || c == "map" || c == "sort" ||
                c == "keys" || c == "values" || c == "enumerate" || c == "zip")
                return ptrTy_;
            return i64Ty_; // fallback
        } else if constexpr (std::is_same_v<T, std::unique_ptr<WhenCondExpr>>) {
            return inferExprType(*v->else_expr, paramTypeMap);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<MatchExpr>>) {
            if (!v->arms.empty())
                return inferExprType(*v->arms[0].value, paramTypeMap);
            return i64Ty_;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<InterpolatedStringExpr>>) {
            return ptrTy_;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<LambdaExpr>>) {
            return ptrTy_;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<FieldAccessExpr>>) {
            llvm::Type *objTy = inferExprType(*v->object, paramTypeMap);
            if (auto *st = llvm::dyn_cast<llvm::StructType>(objTy)) {
                auto it = struct_types_.find(st->getName().str());
                if (it != struct_types_.end() && it->second.llvmType == st) {
                    for (unsigned i = 0; i < it->second.fields.size(); ++i) {
                        if (it->second.fields[i].name == v->field)
                            return st->getElementType(i);
                    }
                }
            }
            return i64Ty_; // fallback
        } else if constexpr (std::is_same_v<T, std::unique_ptr<ListExpr>> ||
                             std::is_same_v<T, std::unique_ptr<MapExpr>> ||
                             std::is_same_v<T, std::unique_ptr<SetExpr>>) {
            return ptrTy_;
        } else if constexpr (std::is_same_v<T, RegexExpr>) {
            return ptrTy_;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<RangeExpr>>) {
            return ptrTy_;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<CastExpr>>) {
            return resolveType(v->target_type->toString());
        } else {
            return i64Ty_; // fallback
        }
    }, expr.data);
}

std::string CodeGen::inferExprTypeName(const ExprNode &expr,
    const std::unordered_map<std::string, llvm::Type*> &paramTypeMap) {
    return std::visit([&](const auto &v) -> std::string {
        using T = std::decay_t<decltype(v)>;
        if constexpr (std::is_same_v<T, std::unique_ptr<ListExpr>>) {
            if (v->elements.empty()) return "";
            std::string elem = inferExprTypeName(*v->elements[0], paramTypeMap);
            return "List<" + elem + ">";
        } else if constexpr (std::is_same_v<T, std::unique_ptr<MapExpr>>) {
            if (v->keys.empty()) return "";
            std::string key = inferExprTypeName(*v->keys[0], paramTypeMap);
            std::string val = inferExprTypeName(*v->values[0], paramTypeMap);
            return "Map<" + key + ", " + val + ">";
        } else if constexpr (std::is_same_v<T, std::unique_ptr<SetExpr>>) {
            if (v->elements.empty()) return "";
            std::string elem = inferExprTypeName(*v->elements[0], paramTypeMap);
            return "Set<" + elem + ">";
        } else if constexpr (std::is_same_v<T, std::unique_ptr<CallExpr>>) {
            auto *overloads = findFunction(v->callee);
            if (overloads && !overloads->empty() && !(*overloads)[0].returnTypeName.empty())
                return (*overloads)[0].returnTypeName;
            return reverseResolveTypeName(inferExprType(expr, paramTypeMap));
        } else if constexpr (std::is_same_v<T, std::unique_ptr<CastExpr>>) {
            return v->target_type->toString();
        } else if constexpr (std::is_same_v<T, std::unique_ptr<WhenCondExpr>>) {
            return inferExprTypeName(*v->else_expr, paramTypeMap);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<MatchExpr>>) {
            if (!v->arms.empty())
                return inferExprTypeName(*v->arms[0].value, paramTypeMap);
            return "";
        } else {
            return reverseResolveTypeName(inferExprType(expr, paramTypeMap));
        }
    }, expr.data);
}

llvm::Type *CodeGen::inferReturnType(const std::vector<StmtNode> &body,
    const std::unordered_map<std::string, llvm::Type*> &paramTypeMap) {
    std::vector<llvm::Type*> types;
    collectReturnTypes(body, paramTypeMap, types);
    return deduceReturnType(types);
}

void CodeGen::collectReturnTypes(const std::vector<StmtNode> &body,
    const std::unordered_map<std::string, llvm::Type*> &paramTypeMap,
    std::vector<llvm::Type*> &out) {
    for (auto &stmt : body) {
        std::visit([&](const auto &s) {
            using T = std::decay_t<decltype(s)>;
            if constexpr (std::is_same_v<T, ReturnStmt>) {
                if (s.value)
                    out.push_back(inferExprType(*s.value, paramTypeMap));
            } else if constexpr (std::is_same_v<T, std::unique_ptr<IfStmt>>) {
                collectReturnTypes(s->branch.body, paramTypeMap, out);
                collectReturnTypes(s->else_body, paramTypeMap, out);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<WhenCondStmt>>) {
                for (auto &arm : s->arms)
                    collectReturnTypes(arm.body, paramTypeMap, out);
                collectReturnTypes(s->else_body, paramTypeMap, out);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<MatchStmt>>) {
                for (auto &arm : s->arms)
                    collectReturnTypes(arm.body, paramTypeMap, out);
            }
        }, stmt);
    }
}

llvm::Type *CodeGen::deduceReturnType(const std::vector<llvm::Type*> &types) {
    if (types.empty())
        return llvm::Type::getVoidTy(*ctx_);

    // Deduplicate types
    std::vector<llvm::Type*> unique;
    for (auto *ty : types) {
        if (std::find(unique.begin(), unique.end(), ty) == unique.end())
            unique.push_back(ty);
    }

    if (unique.size() == 1)
        return unique[0];

    // Build union type name from component types
    std::string unionName;
    for (size_t i = 0; i < unique.size(); ++i) {
        if (i > 0) unionName += " | ";
        unionName += reverseResolveTypeName(unique[i]);
    }
    return resolveType(unionName);
}

std::string CodeGen::reverseResolveTypeName(llvm::Type *ty) {
    if (ty == i64Ty_) return "int";
    if (ty == f64Ty_) return "float";
    if (ty == i1Ty_)  return "bool";
    if (ty == i8Ty_)  return "u8";
    if (ty == i16Ty_) return "i16";
    if (ty == i32Ty_) return "i32";
    if (ty == f32Ty_) return "f32";
    if (ty == ptrTy_) return "str";
    if (isAnyType(ty)) return "any";
    if (ty->isVoidTy()) return "Unit";
    if (auto *st = llvm::dyn_cast<llvm::StructType>(ty)) {
        std::string n = findStructTypeName(st);
        if (!n.empty()) return n;
    }
    return "any"; // fallback
}

// ===== Uniform Closure Support =====

llvm::Function *CodeGen::getOrCreateForwardingThunk(llvm::Function *realFn, const FnTypeInfo &info) {
    auto it = forwarding_thunk_cache_.find(realFn);
    if (it != forwarding_thunk_cache_.end())
        return it->second;

    // Thunk signature: (user_params..., ptr env) -> ret
    std::vector<llvm::Type*> thunkParams = info.paramTypes;
    thunkParams.push_back(ptrTy_); // env (ignored)
    auto *thunkTy = llvm::FunctionType::get(info.returnType, thunkParams, false);

    std::string name = "__ry_uc_fwd_" + realFn->getName().str();
    auto *thunk = llvm::Function::Create(
        thunkTy, llvm::Function::InternalLinkage, name, mod_.get());
    thunk->addFnAttr(llvm::Attribute::AlwaysInline);
    thunk->addFnAttr(llvm::Attribute::NoUnwind);

    auto *savedBB = builder_.GetInsertBlock();
    auto savedPt = builder_.GetInsertPoint();

    auto *entry = llvm::BasicBlock::Create(*ctx_, "entry", thunk);
    builder_.SetInsertPoint(entry);

    // Forward user args to realFn
    std::vector<llvm::Value*> args;
    for (size_t i = 0; i < info.paramTypes.size(); ++i)
        args.push_back(thunk->getArg(i));

    llvm::Value *result = builder_.CreateCall(realFn, args, info.returnType->isVoidTy() ? "" : "fwd_result");
    if (info.returnType->isVoidTy())
        builder_.CreateRetVoid();
    else
        builder_.CreateRet(result);

    if (savedBB)
        builder_.SetInsertPoint(savedBB, savedPt);

    forwarding_thunk_cache_[realFn] = thunk;
    return thunk;
}

llvm::Function *CodeGen::getOrCreateCapturingThunk(llvm::Function *realFn, const FnTypeInfo &info) {
    auto it = capturing_thunk_cache_.find(realFn);
    if (it != capturing_thunk_cache_.end())
        return it->second;

    // Thunk signature: (user_params..., ptr env) -> ret
    std::vector<llvm::Type*> thunkParams = info.paramTypes;
    thunkParams.push_back(ptrTy_); // env = original closure struct pointer
    auto *thunkTy = llvm::FunctionType::get(info.returnType, thunkParams, false);

    std::string name = "__ry_uc_cap_" + realFn->getName().str();
    auto *thunk = llvm::Function::Create(
        thunkTy, llvm::Function::InternalLinkage, name, mod_.get());
    thunk->addFnAttr(llvm::Attribute::NoUnwind);

    auto *savedBB = builder_.GetInsertBlock();
    auto savedPt = builder_.GetInsertPoint();

    auto *entry = llvm::BasicBlock::Create(*ctx_, "entry", thunk);
    builder_.SetInsertPoint(entry);

    // env is the last argument
    llvm::Value *envPtr = thunk->getArg(info.paramTypes.size());

    // Reconstruct the original closure struct type: {fn_ptr, cap1, cap2, ...}
    std::vector<llvm::Type*> closureFields;
    closureFields.push_back(ptrTy_); // fn_ptr
    for (auto *ct : info.capturedTypes)
        closureFields.push_back(ct);
    auto *closureTy = llvm::StructType::get(*ctx_, closureFields);

    // Build full args: user_params + captured values loaded from env
    std::vector<llvm::Value*> fullArgs;
    for (size_t i = 0; i < info.paramTypes.size(); ++i)
        fullArgs.push_back(thunk->getArg(i));

    for (size_t i = 0; i < info.capturedTypes.size(); ++i) {
        auto *capField = builder_.CreateStructGEP(
            closureTy, envPtr, i + 1, "thunk.cap." + std::to_string(i));
        auto *capVal = builder_.CreateLoad(
            info.capturedTypes[i], capField, "thunk.cap_val." + std::to_string(i));
        fullArgs.push_back(capVal);
    }

    llvm::Value *result = builder_.CreateCall(realFn, fullArgs,
        info.returnType->isVoidTy() ? "" : "cap_result");
    if (info.returnType->isVoidTy())
        builder_.CreateRetVoid();
    else
        builder_.CreateRet(result);

    if (savedBB)
        builder_.SetInsertPoint(savedBB, savedPt);

    capturing_thunk_cache_[realFn] = thunk;
    return thunk;
}

llvm::Function *CodeGen::getOrCreateUniformClosureDestructor() {
    if (uniform_closure_dtor_)
        return uniform_closure_dtor_;

    auto *ucTy = getUniformClosureTy();

    auto *dtorTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
    auto *dtorFn = llvm::Function::Create(
        dtorTy, llvm::Function::InternalLinkage, "__ry_arc_dtor_uniform_closure", mod_.get());
    dtorFn->addFnAttr(llvm::Attribute::NoUnwind);

    auto *savedBB = builder_.GetInsertBlock();
    auto savedPt = builder_.GetInsertPoint();

    auto *entryBB = llvm::BasicBlock::Create(*ctx_, "entry", dtorFn);
    builder_.SetInsertPoint(entryBB);

    auto *dataPtr = dtorFn->getArg(0); // points to {thunk, env, env_dtor}
    auto *envField = builder_.CreateStructGEP(ucTy, dataPtr, 1, "uc_dtor.env_field");
    auto *envVal = builder_.CreateLoad(ptrTy_, envField, "uc_dtor.env");
    auto *envDtorField = builder_.CreateStructGEP(ucTy, dataPtr, 2, "uc_dtor.env_dtor_field");
    auto *envDtorVal = builder_.CreateLoad(ptrTy_, envDtorField, "uc_dtor.env_dtor");

    // If env is non-null, release it with its destructor
    auto *nullPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
    auto *isEnvNull = builder_.CreateICmpEQ(envVal, nullPtr, "uc_dtor.env_null");
    auto *releaseBB = llvm::BasicBlock::Create(*ctx_, "uc_dtor.release", dtorFn);
    auto *doneBB = llvm::BasicBlock::Create(*ctx_, "uc_dtor.done", dtorFn);
    builder_.CreateCondBr(isEnvNull, doneBB, releaseBB);

    builder_.SetInsertPoint(releaseBB);
    auto *hdr = emitArcGetHeaderFromData(envVal);

    // Decrement strong count; if zero, call env_dtor (if non-null) and free
    auto *strongPtr = builder_.CreateStructGEP(arcHeaderTy_, hdr, 0, "uc_dtor.strong_ptr");
    auto *cur = builder_.CreateLoad(i64Ty_, strongPtr, "uc_dtor.strong");
    auto *dec = builder_.CreateSub(cur, llvm::ConstantInt::get(i64Ty_, 1), "uc_dtor.dec");
    builder_.CreateStore(dec, strongPtr);
    auto *isDead = builder_.CreateICmpEQ(dec, llvm::ConstantInt::get(i64Ty_, 0), "uc_dtor.dead");

    auto *freeBB = llvm::BasicBlock::Create(*ctx_, "uc_dtor.free", dtorFn);
    builder_.CreateCondBr(isDead, freeBB, doneBB);

    builder_.SetInsertPoint(freeBB);
    // Call env_dtor(envVal) if non-null to release captured ARC values
    auto *isDtorNull = builder_.CreateICmpEQ(envDtorVal, nullPtr, "uc_dtor.dtor_null");
    auto *callDtorBB = llvm::BasicBlock::Create(*ctx_, "uc_dtor.call_dtor", dtorFn);
    auto *afterDtorBB = llvm::BasicBlock::Create(*ctx_, "uc_dtor.after_dtor", dtorFn);
    builder_.CreateCondBr(isDtorNull, afterDtorBB, callDtorBB);

    builder_.SetInsertPoint(callDtorBB);
    auto *envDtorFnTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
    builder_.CreateCall(envDtorFnTy, envDtorVal, {envVal});
    builder_.CreateBr(afterDtorBB);

    builder_.SetInsertPoint(afterDtorBB);
    auto freeFn = mod_->getOrInsertFunction("free",
        llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false));
    builder_.CreateCall(freeFn, {hdr});
    builder_.CreateBr(doneBB);

    builder_.SetInsertPoint(doneBB);
    builder_.CreateRetVoid();

    if (savedBB)
        builder_.SetInsertPoint(savedBB, savedPt);

    uniform_closure_dtor_ = dtorFn;
    return dtorFn;
}

llvm::Value *CodeGen::wrapAsUniformClosure(llvm::Value *val, const FnTypeInfo &info) {
    // Already a uniform closure — pass through
    if (info.isUniformClosure)
        return val;

    auto *ucTy = getUniformClosureTy();

    llvm::Function *thunk = nullptr;
    llvm::Value *envVal = nullptr;
    llvm::Value *envDtorVal = nullptr;
    auto *nullPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));

    if (info.capturedVars.empty()) {
        // Non-capturing: env = null, env_dtor = null
        llvm::Function *realFn = info.sourceFn;
        if (!realFn)
            realFn = llvm::dyn_cast<llvm::Function>(val);
        if (!realFn)
            codegenError("cannot wrap non-capturing function: sourceFn unknown");
        thunk = getOrCreateForwardingThunk(realFn, info);
        envVal = nullPtr;
        envDtorVal = nullPtr;
    } else {
        // Capturing: env = original closure struct pointer, env_dtor = closure destructor
        llvm::Function *realFn = info.sourceFn;
        if (!realFn)
            codegenError("cannot wrap capturing closure: sourceFn unknown");
        thunk = getOrCreateCapturingThunk(realFn, info);
        envVal = val;
        auto envDtor = getOrCreateClosureDestructor(info);
        envDtorVal = envDtor ? llvm::cast<llvm::Value>(envDtor.getCallee()) : nullPtr;
    }

    // Allocate ARC-managed uniform closure struct {thunk, env, env_dtor}
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t ucSize = dl.getTypeAllocSize(ucTy);
    auto *arcHeader = emitArcAlloc(llvm::ConstantInt::get(i64Ty_, ucSize));
    llvm::Value *ucPtr = emitArcGetDataPtr(arcHeader);

    // Store thunk and env
    auto *thunkField = builder_.CreateStructGEP(ucTy, ucPtr, 0, "uc.thunk_store");
    builder_.CreateStore(thunk, thunkField);
    auto *envField = builder_.CreateStructGEP(ucTy, ucPtr, 1, "uc.env_store");
    builder_.CreateStore(envVal, envField);
    auto *envDtorField = builder_.CreateStructGEP(ucTy, ucPtr, 2, "uc.env_dtor_store");
    builder_.CreateStore(envDtorVal, envDtorField);

    // If env is an ARC-managed closure, retain it
    if (!info.capturedVars.empty()) {
        auto *envHdr = emitArcGetHeaderFromData(envVal);
        emitArcRetain(envHdr, false);
    }

    // Register as uniform closure in fn_type_info_
    // (ucPtr is already in arc_owned_values_ via emitArcGetDataPtr)
    FnTypeInfo ucInfo;
    ucInfo.paramTypes = info.paramTypes;
    ucInfo.paramTypeNames = info.paramTypeNames;
    ucInfo.returnType = info.returnType;
    if (info.returnFnTypeInfo)
        ucInfo.returnFnTypeInfo = std::make_unique<FnTypeInfo>(*info.returnFnTypeInfo);
    ucInfo.isUniformClosure = true;
    getOrCreateMeta(ucPtr).fn_type_info = ucInfo;

    return ucPtr;
}

std::vector<llvm::Value*> CodeGen::wrapFnTypedArgs(
    std::vector<llvm::Value*> &argVals,
    const std::vector<std::string> &paramTypeNames) {
    std::vector<llvm::Value*> temps;
    for (size_t i = 0; i < argVals.size() && i < paramTypeNames.size(); ++i) {
        std::string resolved = resolveTypeAlias(paramTypeNames[i]);
        if (isFunctionTypeName(resolved)) {
            auto *fnInfo = lookupFnTypeInfo(argVals[i]);
            if (fnInfo && !fnInfo->isUniformClosure) {
                argVals[i] = wrapAsUniformClosure(argVals[i], *fnInfo);
                temps.push_back(argVals[i]);
            }
        }
    }
    return temps;
}

void CodeGen::releaseUniformClosureTemps(const std::vector<llvm::Value*> &temps) {
    if (temps.empty()) return;
    auto *dtorFn = getOrCreateUniformClosureDestructor();
    llvm::FunctionCallee dtor(dtorFn->getFunctionType(), dtorFn);
    for (auto *uc : temps) {
        auto *hdr = emitArcGetHeaderFromData(uc);
        emitArcRelease(hdr, false, dtor);
    }
}

} // namespace ry
