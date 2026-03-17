#include "ry/codegen.hpp"
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <functional>
#include <stdexcept>

// ===== LambdaExpr =====

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<LambdaExpr> &e) {
    // Collect free variables (captured from outer scope)
    std::vector<std::string> capturedNames;
    std::vector<llvm::Value*> capturedValues;
    std::vector<llvm::Type*> capturedTypes;

    // Build a set of parameter names
    std::unordered_set<std::string> paramNames;
    for (auto &p : e->params)
        paramNames.insert(p.name);

    // Simple free variable analysis: scan for VariableExpr in the body
    // We use a lambda to recursively walk the AST
    std::function<void(const ExprNode&)> scanExpr;
    std::function<void(const StmtNode&)> scanStmt;
    std::unordered_set<std::string> found;

    scanExpr = [&](const ExprNode &node) {
        std::visit([&](const auto &v) {
            using T = std::decay_t<decltype(v)>;
            if constexpr (std::is_same_v<T, VariableExpr>) {
                if (!paramNames.count(v.name) && !found.count(v.name)) {
                    if (llvm::AllocaInst *alloca = findVar(v.name)) {
                        found.insert(v.name);
                        capturedNames.push_back(v.name);
                        llvm::Value *val = builder_.CreateLoad(
                            alloca->getAllocatedType(), alloca, v.name + ".cap");
                        capturedValues.push_back(val);
                        capturedTypes.push_back(val->getType());
                    }
                }
            } else if constexpr (std::is_same_v<T, std::unique_ptr<BinaryExpr>>) {
                scanExpr(*v->lhs); scanExpr(*v->rhs);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<UnaryExpr>>) {
                scanExpr(*v->operand);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<CallExpr>>) {
                for (auto &arg : v->args) scanExpr(*arg);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<FieldAccessExpr>>) {
                scanExpr(*v->object);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<TupleExpr>>) {
                for (auto &el : v->elements) scanExpr(*el);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<ListExpr>>) {
                for (auto &el : v->elements) scanExpr(*el);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<IndexExpr>>) {
                scanExpr(*v->object); scanExpr(*v->index);
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
            if constexpr (std::is_same_v<T, LetStmt>) {
                scanExpr(*s.value);
            } else if constexpr (std::is_same_v<T, VarStmt>) {
                scanExpr(*s.value);
            } else if constexpr (std::is_same_v<T, AssignStmt>) {
                scanExpr(*s.value);
            } else if constexpr (std::is_same_v<T, CallStmt>) {
                for (auto &arg : s.args) scanExpr(*arg);
            } else if constexpr (std::is_same_v<T, ReturnStmt>) {
                if (s.value) scanExpr(*s.value);
            } else if constexpr (std::is_same_v<T, IndexAssignStmt>) {
                scanExpr(*s.object); scanExpr(*s.index); scanExpr(*s.value);
            } else if constexpr (std::is_same_v<T, FieldAssignStmt>) {
                scanExpr(*s.object); scanExpr(*s.value);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<IfStmt>>) {
                for (auto &br : s->branches) {
                    scanExpr(*br.condition);
                    for (auto &st : br.body) scanStmt(st);
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
        paramTypes.push_back(resolveType(p.type));
    std::vector<llvm::Type*> allParamTypes = paramTypes;
    for (auto *t : capturedTypes)
        allParamTypes.push_back(t);

    llvm::Type *retTy;
    if (e->return_type.empty()) {
        // 戻り値型を推論
        std::unordered_map<std::string, llvm::Type*> paramTypeMap;
        for (auto &p : e->params)
            paramTypeMap[p.name] = resolveType(p.type);

        if (e->expr_body) {
            retTy = inferExprType(*e->expr_body, paramTypeMap);
        } else {
            retTy = inferReturnType(e->body, paramTypeMap);
        }
    } else {
        retTy = resolveType(e->return_type);
    }

    // Create the LLVM function
    std::string lambdaName = "__lambda." + std::to_string(lambda_counter_++);
    llvm::FunctionType *ft = llvm::FunctionType::get(retTy, allParamTypes, false);
    llvm::Function *func = llvm::Function::Create(
        ft, llvm::Function::InternalLinkage, lambdaName, *mod_);

    {
        FnScope guard(*this);
        fn_ = func;
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
                const std::string &ptype = e->params[idx].type;
                if (ptype.size() > 3 && ptype.substr(0, 3) == "fn(") {
                    fn_type_info_[alloca] = parseFnTypeAnnotation(ptype);
                }
            } else {
                // Captured variable
                size_t capIdx = idx - e->params.size();
                arg.setName(capturedNames[capIdx] + ".cap");
                llvm::AllocaInst *alloca = builder_.CreateAlloca(
                    capturedTypes[capIdx], nullptr, capturedNames[capIdx]);
                builder_.CreateStore(&arg, alloca);
                scope_stack_.back()[capturedNames[capIdx]] = alloca;
            }
            ++idx;
        }

        // Emit body
        if (e->expr_body) {
            llvm::Value *val = emitExpr(*e->expr_body);
            builder_.CreateRet(val);
        } else {
            for (auto &stmt : e->body)
                std::visit([this](auto &st) { emitStmt(st); }, stmt);

            if (!builder_.GetInsertBlock()->getTerminator()) {
                if (retTy->isVoidTy())
                    builder_.CreateRetVoid();
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
            throw std::runtime_error("IR verify error in lambda: " + err);
    }

    // Register fn_type_info for the function pointer value
    FnTypeInfo info;
    info.paramTypes = paramTypes;  // only the user-visible params
    info.returnType = retTy;
    info.capturedVars = capturedNames;
    info.capturedTypes = capturedTypes;
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

    llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t closureSize = dl.getTypeAllocSize(closureTy);
    llvm::Value *closurePtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, closureSize)}, "closure");

    // Store function pointer
    llvm::Value *fnField = builder_.CreateStructGEP(closureTy, closurePtr, 0, "closure.fn");
    builder_.CreateStore(func, fnField);

    // Store captured values
    for (size_t i = 0; i < capturedValues.size(); ++i) {
        llvm::Value *capField = builder_.CreateStructGEP(
            closureTy, closurePtr, i + 1, "closure.cap." + std::to_string(i));
        builder_.CreateStore(capturedValues[i], capField);
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
            return i64Ty_;
        } else if constexpr (std::is_same_v<T, FloatExpr>) {
            return f64Ty_;
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
            return inferExprType(*v->operand, paramTypeMap);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<CallExpr>>) {
            // Look up the function return type
            auto it = functions_.find(v->callee);
            if (it != functions_.end() && !it->second.empty())
                return it->second[0].func->getReturnType();
            // Known builtin return types
            const std::string &c = v->callee;
            if (c == "len" || c == "to_int" || c == "find")
                return i64Ty_;
            if (c == "to_float")
                return f64Ty_;
            if (c == "contains" || c == "starts_with" || c == "ends_with" || c == "has_key")
                return i1Ty_;
            if (c == "to_str" || c == "to_upper" || c == "to_lower" ||
                c == "trim" || c == "trim_start" || c == "trim_end" ||
                c == "substring" || c == "char_at" || c == "replace" ||
                c == "repeat" || c == "reverse" || c == "join" ||
                c == "filter" || c == "map" || c == "sort")
                return ptrTy_;
            return i64Ty_; // fallback
        } else if constexpr (std::is_same_v<T, std::unique_ptr<TernaryExpr>>) {
            return inferExprType(*v->true_expr, paramTypeMap);
        } else {
            return i64Ty_; // fallback
        }
    }, expr.data);
}

llvm::Type *CodeGen::inferReturnType(const std::vector<StmtNode> &body,
    const std::unordered_map<std::string, llvm::Type*> &paramTypeMap) {
    // Scan for ReturnStmt with a value
    for (auto &stmt : body) {
        auto result = std::visit([&](const auto &s) -> llvm::Type* {
            using T = std::decay_t<decltype(s)>;
            if constexpr (std::is_same_v<T, ReturnStmt>) {
                if (s.value)
                    return inferExprType(*s.value, paramTypeMap);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<IfStmt>>) {
                for (auto &br : s->branches) {
                    auto *ty = inferReturnType(br.body, paramTypeMap);
                    if (ty) return ty;
                }
                auto *ty = inferReturnType(s->else_body, paramTypeMap);
                if (ty) return ty;
            }
            return static_cast<llvm::Type*>(nullptr);
        }, stmt);
        if (result) return result;
    }
    // No return found → void
    return llvm::Type::getVoidTy(*ctx_);
}
