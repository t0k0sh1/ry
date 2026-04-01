#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"
#include "ry/sema_return.hpp"
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <functional>

// ===== LambdaExpr =====

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<LambdaExpr> &e) {
    // Collect free variables (captured from outer scope)
    std::vector<std::string> capturedNames;
    std::vector<llvm::Value*> capturedValues;
    std::vector<llvm::Type*> capturedTypes;
    std::vector<CapturedArcKind> capturedArcKinds;
    std::vector<ResourceKind> capturedResourceKinds;
    std::unordered_map<size_t, FnTypeInfo> capturedClosureInfos;

    // Build a set of parameter names
    std::unordered_set<std::string> paramNames;
    for (auto &p : e->params)
        paramNames.insert(p.name);

    // Simple free variable analysis: scan for VariableExpr in the body
    // We use a lambda to recursively walk the AST
    std::function<void(const ExprNode&)> scanExpr;
    std::function<void(const StmtNode&)> scanStmt;
    std::unordered_set<std::string> found;

    // Try to capture a variable by name (used for both VariableExpr and CallExpr callee)
    auto tryCaptureVar = [&](const std::string &varName) {
        if (paramNames.count(varName) || found.count(varName))
            return;
        llvm::AllocaInst *alloca = findVar(varName);
        if (!alloca)
            return;
        found.insert(varName);
        capturedNames.push_back(varName);
        llvm::Value *val = builder_.CreateLoad(
            alloca->getAllocatedType(), alloca, varName + ".cap");
        capturedValues.push_back(val);
        capturedTypes.push_back(val->getType());
        auto cak = detectCapturedArcKind(alloca);
        capturedArcKinds.push_back(cak);
        if (cak == CAK_Resource) {
            auto rmIt = resource_managed_vars_.find(alloca);
            capturedResourceKinds.push_back(
                rmIt != resource_managed_vars_.end() ? rmIt->second : RK_COUNT);
        } else {
            capturedResourceKinds.push_back(RK_COUNT);
        }
        // Store fn_type_info for any function-typed capture (closure or plain fn pointer)
        auto fnIt = fn_type_info_.find(alloca);
        if (fnIt != fn_type_info_.end())
            capturedClosureInfos[capturedNames.size() - 1] = fnIt->second;
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
            }
        }, node.data);
    };

    scanStmt = [&](const StmtNode &stmt) {
        std::visit([&](const auto &s) {
            using T = std::decay_t<decltype(s)>;
            if constexpr (std::is_same_v<T, AssignStmt>) {
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
                for (auto &st : s->body) scanStmt(st);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<MatchStmt>>) {
                scanExpr(*s->subject);
                for (auto &arm : s->arms) {
                    if (arm.guard) scanExpr(*arm.guard);
                    for (auto &st : arm.body) scanStmt(st);
                }
            }
        }, stmt);
    };

    // Scan the lambda body for free variables
    if (e->expr_body) {
        scanExpr(*e->expr_body);
    } else {
        for (auto &stmt : e->body)
            scanStmt(stmt);
    }

    // Build parameter types (user params + captured vars)
    std::vector<llvm::Type*> paramTypes;
    for (auto &p : e->params)
        paramTypes.push_back(resolveType(p.type->toString()));
    std::vector<llvm::Type*> allParamTypes = paramTypes;
    for (auto *t : capturedTypes)
        allParamTypes.push_back(t);

    llvm::Type *retTy;
    std::string retTypeStr = e->return_type ? e->return_type->toString() : "";
    if (retTypeStr == "any") {
        retTy = anyTy_;
    } else if (!e->return_type) {
        // Infer return type when omitted
        std::unordered_map<std::string, llvm::Type*> paramTypeMap;
        for (auto &p : e->params)
            paramTypeMap[p.name] = resolveType(p.type->toString());

        if (e->expr_body) {
            retTy = inferExprType(*e->expr_body, paramTypeMap);
        } else {
            retTy = inferReturnType(e->body, paramTypeMap);
        }
    } else {
        retTy = resolveType(retTypeStr);
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
                // Track fn type info for fn-typed parameters
                const std::string ptype = e->params[idx].type->toString();
                std::string resolvedPtype = resolveTypeAlias(ptype);
                if (resolvedPtype.size() > 9 && resolvedPtype.substr(0, 9) == "function(") {
                    fn_type_info_[alloca] = parseFnTypeAnnotation(resolvedPtype);
                }
                registerResourceByTypeName(ptype, alloca);
            } else {
                // Captured variable
                size_t capIdx = idx - e->params.size();
                arg.setName(capturedNames[capIdx] + ".cap");
                llvm::AllocaInst *alloca = builder_.CreateAlloca(
                    capturedTypes[capIdx], nullptr, capturedNames[capIdx]);
                builder_.CreateStore(&arg, alloca);
                scope_stack_.back()[capturedNames[capIdx]] = alloca;
                // Propagate fn_type_info for captured function-type variables
                auto closureIt = capturedClosureInfos.find(capIdx);
                if (closureIt != capturedClosureInfos.end())
                    fn_type_info_[alloca] = closureIt->second;
            }
            ++idx;
        }

        // Emit body
        if (e->expr_body) {
            llvm::Value *val = emitExpr(*e->expr_body);
            if (isAnyType(retTy) && !isAnyType(val->getType()))
                val = wrapInAny(val);
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
    info.capturedVars = capturedNames;
    info.capturedTypes = capturedTypes;
    info.capturedArcKinds = capturedArcKinds;
    info.capturedResourceKinds = capturedResourceKinds;
    info.capturedClosureInfos = capturedClosureInfos;
    fn_type_info_[func] = info;

    // If no captures, just return the function pointer
    if (capturedNames.empty())
        return func;

    // With captures: pack {fn_ptr, cap1, cap2, ...} into a struct
    std::vector<llvm::Type*> closureFields;
    closureFields.push_back(ptrTy_);  // function pointer
    for (auto *t : capturedTypes)
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
        if (capturedArcKinds[i] != CAK_None) {
            auto *hdr = emitArcGetHeaderFromData(capturedValues[i]);
            emitArcRetain(hdr, false);
        }
    }

    // Register the closure pointer with fn_type_info
    fn_type_info_[closurePtr] = info;

    return closurePtr;
}

// ===== Lambda return type inference =====

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
            auto it = functions_.find(v->callee);
            if (it != functions_.end() && !it->second.empty())
                return it->second[0].func->getReturnType();
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
        } else {
            return i64Ty_; // fallback
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
        std::string n = findAdtEnumName(st);
        if (!n.empty()) return n;
    }
    return "any"; // fallback
}
