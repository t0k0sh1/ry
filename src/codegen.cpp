#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <stdexcept>

CodeGen::CodeGen(bool test_mode, const SourceManager *sm) : ctx_(std::make_unique<llvm::LLVMContext>()),
                     mod_(std::make_unique<llvm::Module>("ry", *ctx_)),
                     builder_(*ctx_),
                     test_mode_(test_mode),
                     sm_(sm) {
    i64Ty_ = llvm::Type::getInt64Ty(*ctx_);
    i32Ty_ = llvm::Type::getInt32Ty(*ctx_);
    i8Ty_  = llvm::Type::getInt8Ty(*ctx_);
    f64Ty_ = llvm::Type::getDoubleTy(*ctx_);
    i1Ty_  = llvm::Type::getInt1Ty(*ctx_);
    ptrTy_ = llvm::PointerType::getUnqual(*ctx_);

    builtins_["print"] = [this](const std::vector<ExprPtr> &args) { emitPrint(args); };
    builtins_["exit"] = [this](const std::vector<ExprPtr> &args) { emitExit(args); };

    errorTy_ = llvm::StructType::create(*ctx_, {ptrTy_, i64Ty_}, "Error");

    listHeaderTy_ = llvm::StructType::create(*ctx_, {i64Ty_, i64Ty_, ptrTy_}, "ListHeader");
    mapHeaderTy_ = llvm::StructType::create(*ctx_, {i64Ty_, i64Ty_, ptrTy_, ptrTy_, i64Ty_, ptrTy_}, "MapHeader");
    setHeaderTy_ = llvm::StructType::create(*ctx_, {i64Ty_, i64Ty_, ptrTy_, i64Ty_, ptrTy_}, "SetHeader");
}

// ===== B5: FnScope RAII =====

CodeGen::FnScope::FnScope(CodeGen &cg) : cg_(cg) {
    savedFn_ = cg_.fn_;
    savedScope_ = std::move(cg_.scope_stack_);
    savedConstScope_ = std::move(cg_.immutable_scope_stack_);
    savedBlock_ = cg_.builder_.GetInsertBlock();
    savedPoint_ = cg_.builder_.GetInsertPoint();
    savedPostconditions_ = cg_.current_postconditions_;
    savedResultAlloca_ = cg_.result_alloca_;
    savedInEnsureContext_ = cg_.in_ensure_context_;
    savedOldValueMap_ = std::move(cg_.old_value_map_);
    cg_.scope_stack_.clear();
    cg_.immutable_scope_stack_.clear();
    cg_.current_postconditions_ = nullptr;
    cg_.result_alloca_ = nullptr;
    cg_.in_ensure_context_ = false;
    cg_.old_value_map_.clear();
}

CodeGen::FnScope::~FnScope() {
    cg_.fn_ = savedFn_;
    cg_.scope_stack_ = std::move(savedScope_);
    cg_.immutable_scope_stack_ = std::move(savedConstScope_);
    cg_.builder_.SetInsertPoint(savedBlock_, savedPoint_);
    cg_.current_postconditions_ = savedPostconditions_;
    cg_.result_alloca_ = savedResultAlloca_;
    cg_.in_ensure_context_ = savedInEnsureContext_;
    cg_.old_value_map_ = std::move(savedOldValueMap_);
}

// ===== Error helpers =====

[[noreturn]] void CodeGen::codegenError(const SourceLocation &loc, const std::string &msg) {
    throw DiagnosticError(loc, msg, sm_);
}

[[noreturn]] void CodeGen::codegenError(const std::string &msg) {
    codegenError(current_loc_, msg);
}

// ===== Scope management =====

void CodeGen::pushScope() {
    scope_stack_.emplace_back();
    immutable_scope_stack_.emplace_back();
}

void CodeGen::popScope() {
    scope_stack_.pop_back();
    immutable_scope_stack_.pop_back();
}

llvm::AllocaInst *CodeGen::findVar(const std::string &name) {
    for (auto it = scope_stack_.rbegin(); it != scope_stack_.rend(); ++it) {
        auto found = it->find(name);
        if (found != it->end())
            return found->second;
    }
    return nullptr;
}

bool CodeGen::isImmutable(const std::string &name) const {
    for (auto it = immutable_scope_stack_.rbegin(); it != immutable_scope_stack_.rend(); ++it) {
        if (it->count(name))
            return true;
    }
    return false;
}

// Pre-pass: collect all function names targeted by mock() in the AST
static void collectMockedFunctions(const std::vector<StmtNode> &stmts,
                                    std::unordered_set<std::string> &out) {
    for (const auto &stmt : stmts) {
        std::visit([&](const auto &s) {
            using T = std::decay_t<decltype(s)>;
            if constexpr (std::is_same_v<T, CallStmt>) {
                if (s.callee == "mock" && !s.args.empty()) {
                    if (auto *str = std::get_if<StringExpr>(&s.args[0]->data))
                        out.insert(str->value);
                }
            } else if constexpr (std::is_same_v<T, std::unique_ptr<IfStmt>>) {
                for (auto &br : s->branches)
                    collectMockedFunctions(br.body, out);
                collectMockedFunctions(s->else_body, out);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<WhileStmt>>) {
                collectMockedFunctions(s->body, out);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<ForStmt>>) {
                collectMockedFunctions(s->body, out);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<FnStmt>>) {
                collectMockedFunctions(s->body, out);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<MatchStmt>>) {
                for (auto &arm : s->arms)
                    collectMockedFunctions(arm.body, out);
            }
        }, stmt);
        // Also scan lambda args (e.g., describe/it trailing blocks)
        std::visit([&](const auto &s) {
            using T = std::decay_t<decltype(s)>;
            if constexpr (std::is_same_v<T, CallStmt>) {
                for (auto &arg : s.args) {
                    if (auto *lam = std::get_if<std::unique_ptr<LambdaExpr>>(&arg->data))
                        collectMockedFunctions((*lam)->body, out);
                }
            }
        }, stmt);
    }
}

llvm::orc::ThreadSafeModule CodeGen::compile(Program &prog) {
    // Pre-pass: collect all mock targets before codegen
    if (test_mode_)
        collectMockedFunctions(prog, mocked_functions_);

    llvm::FunctionType *ft = llvm::FunctionType::get(i32Ty_, false);
    fn_ = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "__ry_main__", *mod_);
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(*ctx_, "entry", fn_);
    builder_.SetInsertPoint(bb);

    pushScope();

    for (auto &stmt : prog) {
        std::visit([this](auto &s) { emitStmt(s); }, stmt);
    }

    if (!builder_.GetInsertBlock()->getTerminator()) {
        if (test_mode_) {
            // Call __ry_test_summary() and return its result as exit code
            llvm::FunctionType *summaryTy = llvm::FunctionType::get(i32Ty_, false);
            llvm::FunctionCallee summaryFn = mod_->getOrInsertFunction("__ry_test_summary", summaryTy);
            llvm::Value *result = builder_.CreateCall(summaryFn, {}, "test_result");
            builder_.CreateRet(result);
        } else {
            builder_.CreateRet(llvm::ConstantInt::get(i32Ty_, 0));
        }
    }

    std::string err;
    llvm::raw_string_ostream errStream(err);
    if (llvm::verifyFunction(*fn_, &errStream))
        codegenError("IR verify error: " + err);

    return llvm::orc::ThreadSafeModule(std::move(mod_), std::move(ctx_));
}

llvm::AllocaInst *CodeGen::getOrCreateVar(const std::string &name, llvm::Type *ty) {
    auto &current = scope_stack_.back();
    auto it = current.find(name);
    if (it != current.end()) {
        return it->second;
    }
    llvm::IRBuilder<> entryBuilder(&fn_->getEntryBlock(),
                                    fn_->getEntryBlock().begin());
    llvm::AllocaInst *alloca = entryBuilder.CreateAlloca(ty, nullptr, name);
    current[name] = alloca;
    return alloca;
}

// ===== B1: Type promotion helpers =====

llvm::Value *CodeGen::promoteToInt(llvm::Value *v) {
    if (v->getType() == i1Ty_)
        return builder_.CreateZExt(v, i64Ty_, "boolext");
    if (v->getType() == i8Ty_)
        return builder_.CreateZExt(v, i64Ty_, "byteext");
    return v;
}

std::pair<llvm::Value*, llvm::Value*> CodeGen::promoteToFloat(llvm::Value *lhs, llvm::Value *rhs) {
    if (!lhs->getType()->isDoubleTy()) {
        if (lhs->getType() == i8Ty_)
            lhs = builder_.CreateUIToFP(lhs, f64Ty_, "lhs_f");
        else
            lhs = builder_.CreateSIToFP(lhs, f64Ty_, "lhs_f");
    }
    if (!rhs->getType()->isDoubleTy()) {
        if (rhs->getType() == i8Ty_)
            rhs = builder_.CreateUIToFP(rhs, f64Ty_, "rhs_f");
        else
            rhs = builder_.CreateSIToFP(rhs, f64Ty_, "rhs_f");
    }
    return {lhs, rhs};
}


// ===== B4: emitUserFnCall =====

llvm::Function *CodeGen::resolveOverload(const std::string &callee,
                                          const std::vector<ExprPtr> &args,
                                          std::vector<llvm::Value*> &outArgVals) {
    auto fit = functions_.find(callee);
    if (fit == functions_.end())
        codegenError("undefined function: " + callee);

    auto &overloads = fit->second;

    // Identify which args are None literals
    std::vector<bool> isNone(args.size(), false);
    for (size_t i = 0; i < args.size(); ++i) {
        if (auto *ve = std::get_if<VariableExpr>(&args[i]->data); ve && ve->name == "None")
            isNone[i] = true;
    }

    // Emit non-None args to get their types
    std::vector<llvm::Value*> emittedArgs(args.size(), nullptr);
    for (size_t i = 0; i < args.size(); ++i) {
        if (!isNone[i])
            emittedArgs[i] = emitExpr(*args[i]);
    }

    // Filter candidates
    std::vector<OverloadEntry*> candidates;
    for (auto &entry : overloads) {
        if (entry.paramTypes.size() != args.size())
            continue;
        bool match = true;
        for (size_t i = 0; i < args.size(); ++i) {
            if (isNone[i]) {
                if (!isOptionType(entry.paramTypes[i])) { match = false; break; }
            } else {
                if (emittedArgs[i]->getType() != entry.paramTypes[i]) {
                    // Check if param is a union type that accepts this arg type
                    if (i < entry.paramTypeNames.size() && isUnionType(entry.paramTypeNames[i])) {
                        std::string norm = normalizeUnionType(entry.paramTypeNames[i]);
                        auto uIt = union_type_info_.find(norm);
                        if (uIt != union_type_info_.end()) {
                            bool found = false;
                            for (auto *ct : uIt->second.componentTypes) {
                                if (ct == emittedArgs[i]->getType()) { found = true; break; }
                            }
                            if (!found) { match = false; break; }
                        } else { match = false; break; }
                    } else { match = false; break; }
                }
            }
        }
        if (match)
            candidates.push_back(&entry);
    }

    if (candidates.empty())
        codegenError("no matching overload for '" + callee + "'");
    if (candidates.size() > 1)
        codegenError("ambiguous call to '" + callee + "'");

    auto *chosen = candidates[0];

    // Build final arg values (fill in None args with proper Option type, wrap union args)
    outArgVals.clear();
    for (size_t i = 0; i < args.size(); ++i) {
        if (isNone[i]) {
            outArgVals.push_back(buildNoneValue(chosen->paramTypes[i]));
        } else if (emittedArgs[i]->getType() != chosen->paramTypes[i] &&
                   i < chosen->paramTypeNames.size() &&
                   isUnionType(chosen->paramTypeNames[i])) {
            outArgVals.push_back(wrapInUnion(emittedArgs[i], chosen->paramTypeNames[i]));
        } else {
            outArgVals.push_back(emittedArgs[i]);
        }
    }

    return chosen->func;
}

llvm::Value *CodeGen::emitUserFnCall(const std::string &callee, const std::vector<ExprPtr> &args) {
    if (deprecated_functions_.count(callee))
        emitDeprecationWarning(callee);
    std::vector<llvm::Value*> argVals;
    llvm::Function *fn = resolveOverload(callee, args, argVals);

    // Find the matching overload entry (single scan for constraints + result type)
    OverloadEntry *matchedEntry = nullptr;
    auto fit = functions_.find(callee);
    if (fit != functions_.end()) {
        for (auto &entry : fit->second) {
            if (entry.func == fn) { matchedEntry = &entry; break; }
        }
    }

    // Check literal/range constraints on arguments at call site
    if (matchedEntry) {
        for (size_t i = 0; i < matchedEntry->paramTypeNames.size() && i < argVals.size(); ++i) {
            std::string resolvedPtype = resolveTypeAlias(matchedEntry->paramTypeNames[i]);
            auto constraint = parseTypeConstraint(resolvedPtype);
            if (constraint) {
                std::string paramName = fn->getArg(i)->getName().str();
                emitConstraintCheck(argVals[i], *constraint, paramName);
            }
        }
    }

    // In test mode, inject mock dispatch only for functions targeted by mock()
    if (test_mode_ && mocked_functions_.count(callee)) {
        llvm::FunctionType *mockGetTy = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
        llvm::FunctionCallee mockGetFn = mod_->getOrInsertFunction("__ry_mock_get", mockGetTy);
        llvm::FunctionType *mockIncTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
        llvm::FunctionCallee mockIncFn = mod_->getOrInsertFunction("__ry_mock_increment_call", mockIncTy);

        auto &nameStr = mock_name_strings_[callee];
        if (!nameStr) nameStr = builder_.CreateGlobalString(callee, ".mock." + callee);
        llvm::Value *mockPtr = builder_.CreateCall(mockGetFn, {nameStr}, "mock_ptr");
        llvm::Value *nullPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
        llvm::Value *isMocked = builder_.CreateICmpNE(mockPtr, nullPtr, "is_mocked");

        llvm::BasicBlock *mockBB = llvm::BasicBlock::Create(*ctx_, "mock_bb", fn_);
        llvm::BasicBlock *origBB = llvm::BasicBlock::Create(*ctx_, "orig_bb", fn_);
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "merge_bb", fn_);

        builder_.CreateCondBr(isMocked, mockBB, origBB);

        // Mock path
        builder_.SetInsertPoint(mockBB);
        builder_.CreateCall(mockIncFn, {nameStr});
        llvm::FunctionType *fnTy = fn->getFunctionType();
        if (fn->getReturnType()->isVoidTy()) {
            builder_.CreateCall(fnTy, mockPtr, argVals);
            builder_.CreateBr(mergeBB);

            // Original path (void case)
            builder_.SetInsertPoint(origBB);
            builder_.CreateCall(fn, argVals);
            builder_.CreateBr(mergeBB);

            builder_.SetInsertPoint(mergeBB);
            return nullptr;
        }

        llvm::Value *mockResult = builder_.CreateCall(fnTy, mockPtr, argVals, "mock_result");
        builder_.CreateBr(mergeBB);
        llvm::BasicBlock *mockEndBB = builder_.GetInsertBlock();

        // Original path
        builder_.SetInsertPoint(origBB);
        llvm::Value *origResult = builder_.CreateCall(fn, argVals, "orig_result");
        builder_.CreateBr(mergeBB);
        llvm::BasicBlock *origEndBB = builder_.GetInsertBlock();

        // Merge
        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = builder_.CreatePHI(fn->getReturnType(), 2, "call_result");
        phi->addIncoming(mockResult, mockEndBB);
        phi->addIncoming(origResult, origEndBB);
        return phi;
    }

    if (fn->getReturnType()->isVoidTy())
        return builder_.CreateCall(fn, argVals);
    llvm::Value *callResult = builder_.CreateCall(fn, argVals, "calltmp");

    return callResult;
}

void CodeGen::emitStmt(CallStmt &s) {
    if (s.callee == "describe") {
        emitDescribeCall(s);
        return;
    }
    if (s.callee == "it") {
        emitItCall(s);
        return;
    }
    if (s.callee == "mock") {
        emitMockCall(s);
        return;
    }
    auto it = builtins_.find(s.callee);
    if (it != builtins_.end()) {
        it->second(s.args);
        return;
    }
    auto sit = struct_types_.find(s.callee);
    if (sit != struct_types_.end()) {
        emitStructConstructor(sit->second, s.callee, s.args);
        return;
    }
    // Intercept collection operations and route through CallExpr emitter
    if (!s.args.empty()) {
        bool intercept = false;
        if (auto *ve = std::get_if<VariableExpr>(&s.args[0]->data)) {
            llvm::AllocaInst *alloca = findVar(ve->name);
            if (alloca) {
                bool isList = getListElementType(alloca) != nullptr;
                bool isSet = !isList && getSetElementType(alloca) != nullptr;
                bool isMap = !isList && !isSet && getMapKeyType(alloca) != nullptr;
                size_t nargs = s.args.size();

                if (isList &&
                    ((s.callee == "append" && nargs == 2) ||
                     (s.callee == "pop" && nargs == 1) ||
                     (s.callee == "insert" && nargs == 3) ||
                     (s.callee == "remove_at" && nargs == 2) ||
                     (s.callee == "remove" && nargs == 2))) {
                    intercept = true;
                } else if (isSet &&
                    ((s.callee == "add" && nargs == 2) ||
                     (s.callee == "remove" && nargs == 2) ||
                     (s.callee == "union" && nargs == 2) ||
                     (s.callee == "intersection" && nargs == 2) ||
                     (s.callee == "difference" && nargs == 2) ||
                     (s.callee == "symmetric_difference" && nargs == 2) ||
                     (s.callee == "is_subset" && nargs == 2) ||
                     (s.callee == "is_superset" && nargs == 2))) {
                    intercept = true;
                } else if (isMap &&
                    ((s.callee == "remove" && nargs == 2) ||
                     (s.callee == "items" && nargs == 1) ||
                     (s.callee == "get" && nargs == 3))) {
                    intercept = true;
                }
            }
        }
        if (intercept) {
            auto ce = std::make_unique<CallExpr>();
            ce->callee = s.callee;
            ce->args = std::move(s.args);
            emitExprVariant(ce);
            return;
        }
    }
    emitUserFnCall(s.callee, s.args);
}

llvm::Value *CodeGen::toBool(llvm::Value *v) {
    if (v->getType() == i1Ty_)
        return v;
    if (v->getType()->isDoubleTy())
        return builder_.CreateFCmpONE(
            v, llvm::ConstantFP::get(f64Ty_, 0.0), "ftobool");
    return builder_.CreateICmpNE(
        v, llvm::ConstantInt::get(v->getType(), 0), "itobool");
}

llvm::Type *CodeGen::resolveType(const std::string &typeName) {
    // Built-in primitive types first (cannot be shadowed by aliases)
    if (typeName == "int")   return i64Ty_;
    if (typeName == "byte")  return i8Ty_;
    if (typeName == "float") return f64Ty_;
    if (typeName == "bool")  return i1Ty_;
    if (typeName == "str")   return ptrTy_;
    if (typeName == "Error") return errorTy_;
    if (typeName == "Unit")  return llvm::Type::getVoidTy(*ctx_);

    // Optional type suffix: "int?" -> Option<int>
    if (!typeName.empty() && typeName.back() == '?') {
        std::string inner = typeName.substr(0, typeName.size() - 1);
        llvm::Type *innerTy = resolveType(inner);
        return getOptionType(innerTy);
    }

    // Check type alias (with cycle detection)
    auto aliasIt = type_aliases_.find(typeName);
    if (aliasIt != type_aliases_.end()) {
        std::string resolved = resolveTypeAlias(typeName);
        return resolveType(resolved);
    }

    // Int literal type: "42", "-5"
    if (isIntLiteralType(typeName))
        return i64Ty_;

    // Range type: "1..12"
    if (isRangeType(typeName))
        return i64Ty_;

    // String literal type: "\"N\""
    if (isStrLiteralType(typeName))
        return ptrTy_;

    // Literal union type: "0 | 1 | 2" or "\"N\" | \"S\""
    if (isLiteralUnionType(typeName))
        return parseTypeConstraint(typeName)->kind == TypeConstraint::IntLiteral ? i64Ty_ : ptrTy_;

    // Union type: "int | str"
    if (typeName.find(" | ") != std::string::npos) {
        std::string normalized = normalizeUnionType(typeName);
        auto it = union_type_info_.find(normalized);
        if (it != union_type_info_.end()) return it->second.llvmType;

        auto components = parseUnionComponents(normalized);
        std::vector<llvm::Type*> compTypes;
        uint64_t maxSize = 0;
        const auto &dl = mod_->getDataLayout();
        for (auto &c : components) {
            auto *ty = resolveType(c);
            compTypes.push_back(ty);
            maxSize = std::max(maxSize, (uint64_t)dl.getTypeAllocSize(ty));
        }
        auto *dataTy = llvm::ArrayType::get(
            llvm::Type::getInt8Ty(*ctx_), maxSize);
        auto *unionTy = llvm::StructType::create(
            *ctx_, {i64Ty_, dataTy}, "union." + normalized);

        union_type_info_[normalized] = {unionTy, components, compTypes};
        return unionTy;
    }

    // Tuple type: "(int, float)"
    if (!typeName.empty() && typeName.front() == '(') {
        // Parse element types from "(T1, T2, ...)"
        std::string inner = typeName.substr(1, typeName.size() - 2); // strip parens
        std::vector<llvm::Type*> elementTypes;
        size_t depth = 0;
        size_t start = 0;
        for (size_t i = 0; i <= inner.size(); ++i) {
            if (i < inner.size() && inner[i] == '(') ++depth;
            else if (i < inner.size() && inner[i] == ')') --depth;
            else if ((i == inner.size() || inner[i] == ',') && depth == 0) {
                std::string elem = inner.substr(start, i - start);
                // trim leading/trailing spaces
                size_t s = elem.find_first_not_of(' ');
                size_t e = elem.find_last_not_of(' ');
                if (s != std::string::npos)
                    elem = elem.substr(s, e - s + 1);
                elementTypes.push_back(resolveType(elem));
                start = i + 1;
            }
        }
        return llvm::StructType::get(*ctx_, elementTypes);
    }

    // fn(...) -> T function type → opaque pointer
    if (typeName.size() > 3 && typeName.substr(0, 3) == "fn(") {
        return ptrTy_;
    }

    // List<T> parsing
    if (typeName.size() > 5 && typeName.substr(0, 5) == "List<" && typeName.back() == '>') {
        return ptrTy_;
    }

    // Map<K, V> parsing
    if (typeName.size() > 4 && typeName.substr(0, 4) == "Map<" && typeName.back() == '>') {
        return ptrTy_;
    }

    // Set<T> parsing
    if (typeName.size() > 4 && typeName.substr(0, 4) == "Set<" && typeName.back() == '>') {
        return ptrTy_;
    }

    // Option<T> parsing
    if (typeName.size() > 7 && typeName.substr(0, 7) == "Option<" && typeName.back() == '>') {
        std::string inner = typeName.substr(7, typeName.size() - 8);
        llvm::Type *innerTy = resolveType(inner);
        return getOptionType(innerTy);
    }

    auto it = struct_types_.find(typeName);
    if (it != struct_types_.end()) return it->second.llvmType;

    // enum name → i64
    if (enum_types_.count(typeName)) return i64Ty_;

    codegenError("unknown type: " + typeName);
}

llvm::StructType *CodeGen::getOptionType(llvm::Type *innerTy) {
    auto it = option_types_.find(innerTy);
    if (it != option_types_.end()) return it->second;
    llvm::StructType *optTy = llvm::StructType::create(
        *ctx_, {i1Ty_, innerTy}, "Option");
    option_types_[innerTy] = optTy;
    return optTy;
}

bool CodeGen::isOptionType(llvm::Type *ty) {
    auto *st = llvm::dyn_cast<llvm::StructType>(ty);
    if (!st) return false;
    for (auto &pair : option_types_) {
        if (pair.second == st) return true;
    }
    return false;
}

std::pair<llvm::Type*, llvm::Type*> CodeGen::parseMapTypeAnnotation(const std::string &typeStr) {
    std::string inner = typeStr.substr(4, typeStr.size() - 5);
    size_t depth = 0;
    for (size_t i = 0; i < inner.size(); ++i) {
        if (inner[i] == '<') ++depth;
        else if (inner[i] == '>') --depth;
        else if (inner[i] == ',' && depth == 0) {
            std::string kStr = inner.substr(0, i);
            std::string vStr = inner.substr(i + 1);
            while (!kStr.empty() && kStr.back() == ' ') kStr.pop_back();
            while (!vStr.empty() && vStr.front() == ' ') vStr = vStr.substr(1);
            return {resolveType(kStr), resolveType(vStr)};
        }
    }
    return {nullptr, nullptr};
}

CodeGen::FnTypeInfo CodeGen::parseFnTypeAnnotation(const std::string &typeStr) {
    // Parse "fn(int, float) -> int"
    FnTypeInfo info;
    // Find the opening paren
    size_t openParen = typeStr.find('(');
    size_t closeParen = typeStr.find(')');
    if (openParen == std::string::npos || closeParen == std::string::npos)
        codegenError("invalid function type: " + typeStr);

    std::string paramStr = typeStr.substr(openParen + 1, closeParen - openParen - 1);
    // Parse comma-separated parameter types
    if (!paramStr.empty()) {
        size_t start = 0;
        int depth = 0;
        for (size_t i = 0; i <= paramStr.size(); ++i) {
            if (i < paramStr.size() && paramStr[i] == '(') ++depth;
            else if (i < paramStr.size() && paramStr[i] == ')') --depth;
            else if ((i == paramStr.size() || paramStr[i] == ',') && depth == 0) {
                std::string p = paramStr.substr(start, i - start);
                // trim
                size_t s = p.find_first_not_of(' ');
                size_t e = p.find_last_not_of(' ');
                if (s != std::string::npos)
                    p = p.substr(s, e - s + 1);
                info.paramTypes.push_back(resolveType(p));
                start = i + 1;
            }
        }
    }

    // Parse return type after " -> "
    size_t arrow = typeStr.find("->", closeParen);
    if (arrow != std::string::npos) {
        std::string retStr = typeStr.substr(arrow + 2);
        size_t s = retStr.find_first_not_of(' ');
        if (s != std::string::npos)
            retStr = retStr.substr(s);
        info.returnType = resolveType(retStr);
    } else {
        info.returnType = llvm::Type::getVoidTy(*ctx_);
    }

    return info;
}

llvm::Value *CodeGen::buildNoneValue(llvm::Type *optionTy) {
    llvm::Value *val = llvm::UndefValue::get(optionTy);
    val = builder_.CreateInsertValue(val, llvm::ConstantInt::get(i1Ty_, 0), 0);
    val = builder_.CreateInsertValue(val, llvm::UndefValue::get(
        llvm::cast<llvm::StructType>(optionTy)->getElementType(1)), 1);
    return val;
}

void CodeGen::emitRuntimeError(const std::string &message, const std::string &globalName) {
    llvm::FunctionType *printfTy = llvm::FunctionType::get(i32Ty_, {ptrTy_}, true);
    llvm::FunctionCallee printfFn = mod_->getOrInsertFunction("printf", printfTy);
    llvm::Constant *errMsg = builder_.CreateGlobalString(message, globalName);
    builder_.CreateCall(printfFn, {errMsg});
    llvm::FunctionType *exitTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {i32Ty_}, false);
    llvm::FunctionCallee exitFn = mod_->getOrInsertFunction("exit", exitTy);
    builder_.CreateCall(exitFn, {llvm::ConstantInt::get(i32Ty_, 1)});
    builder_.CreateUnreachable();
}

void CodeGen::emitPrintValue(llvm::Value *val, llvm::Type *ty,
                              llvm::FunctionCallee printfFn, const std::string &suffix) {
    if (ty == i1Ty_) {
        llvm::Constant *t = builder_.CreateGlobalString("true", ".fmt_true" + suffix);
        llvm::Constant *f = builder_.CreateGlobalString("false", ".fmt_false" + suffix);
        builder_.CreateCall(printfFn, {builder_.CreateSelect(val, t, f, "bool_fmt")});
    } else if (ty->isPointerTy()) {
        llvm::Constant *fmt = builder_.CreateGlobalString("%s", ".fmt_s" + suffix);
        builder_.CreateCall(printfFn, {fmt, val});
    } else if (ty == i8Ty_) {
        llvm::Value *ext = builder_.CreateZExt(val, i32Ty_, "byte_print");
        llvm::Constant *fmt = builder_.CreateGlobalString("%d", ".fmt_b" + suffix);
        builder_.CreateCall(printfFn, {fmt, ext});
    } else if (ty->isDoubleTy()) {
        llvm::Constant *fmt = builder_.CreateGlobalString("%g", ".fmt_f" + suffix);
        builder_.CreateCall(printfFn, {fmt, val});
    } else if (ty == errorTy_) {
        llvm::Value *msg = builder_.CreateExtractValue(val, 0, "err_msg");
        llvm::Value *code = builder_.CreateExtractValue(val, 1, "err_code");
        llvm::Constant *fmt = builder_.CreateGlobalString("Error: %s (code: %ld)", ".fmt_err" + suffix);
        builder_.CreateCall(printfFn, {fmt, msg, code});
    } else {
        llvm::Constant *fmt = builder_.CreateGlobalString("%ld", ".fmt_i" + suffix);
        builder_.CreateCall(printfFn, {fmt, val});
    }
}

// ===== Literal/Range type helpers =====

std::string CodeGen::resolveTypeAlias(const std::string &typeName) {
    std::unordered_set<std::string> visited;
    std::string current = typeName;
    while (true) {
        if (!visited.insert(current).second)
            codegenError("Circular type alias detected: " + typeName);
        auto it = type_aliases_.find(current);
        if (it == type_aliases_.end())
            break;
        current = it->second;
    }
    return current;
}

bool CodeGen::isIntLiteralType(const std::string &typeName) {
    if (typeName.empty()) return false;
    size_t start = (typeName[0] == '-') ? 1 : 0;
    if (start >= typeName.size()) return false;
    for (size_t i = start; i < typeName.size(); ++i) {
        if (!std::isdigit(typeName[i])) return false;
    }
    return true;
}

bool CodeGen::isStrLiteralType(const std::string &typeName) {
    if (typeName.size() < 2 || typeName.front() != '"' || typeName.back() != '"')
        return false;
    // Ensure it's a single quoted string, not a union like "N" | "S"
    // Check there's exactly one opening and one closing quote
    size_t quoteCount = 0;
    for (char c : typeName)
        if (c == '"') quoteCount++;
    return quoteCount == 2;
}

bool CodeGen::isRangeType(const std::string &typeName) {
    auto pos = typeName.find("..");
    if (pos == std::string::npos || pos == 0 || pos == typeName.size() - 2)
        return false;
    std::string lo = typeName.substr(0, pos);
    std::string hi = typeName.substr(pos + 2);
    return isIntLiteralType(lo) && isIntLiteralType(hi);
}

bool CodeGen::isLiteralUnionType(const std::string &typeName) {
    if (typeName.find(" | ") == std::string::npos) return false;
    auto components = parseUnionComponents(typeName);
    if (components.empty()) return false;
    bool allInt = true, allStr = true;
    for (auto &c : components) {
        if (allInt && !isIntLiteralType(c)) allInt = false;
        if (allStr && !isStrLiteralType(c)) allStr = false;
        if (!allInt && !allStr) return false;
    }
    return allInt || allStr;
}

std::optional<CodeGen::TypeConstraint> CodeGen::parseTypeConstraint(const std::string &typeName) {
    // Callers are responsible for resolving type aliases before calling this function.

    // Range type: "1..12"
    if (isRangeType(typeName)) {
        auto pos = typeName.find("..");
        TypeConstraint tc;
        tc.kind = TypeConstraint::IntRange;
        tc.range_low = std::stoll(typeName.substr(0, pos));
        tc.range_high = std::stoll(typeName.substr(pos + 2));
        if (tc.range_low > tc.range_high)
            codegenError("invalid range type: low bound " +
                std::to_string(tc.range_low) + " > high bound " +
                std::to_string(tc.range_high));
        return tc;
    }

    // Single int literal: "42"
    if (isIntLiteralType(typeName)) {
        TypeConstraint tc;
        tc.kind = TypeConstraint::IntLiteral;
        tc.int_values.push_back(std::stoll(typeName));
        return tc;
    }

    // Single str literal: "\"N\""
    if (isStrLiteralType(typeName)) {
        TypeConstraint tc;
        tc.kind = TypeConstraint::StrLiteral;
        tc.str_values.push_back(typeName.substr(1, typeName.size() - 2));
        return tc;
    }

    // Union of literals
    if (typeName.find(" | ") != std::string::npos) {
        auto components = parseUnionComponents(typeName);
        if (components.empty()) return std::nullopt;

        // Check if all int literals
        bool allInt = true;
        for (auto &c : components) {
            if (!isIntLiteralType(c)) { allInt = false; break; }
        }
        if (allInt) {
            TypeConstraint tc;
            tc.kind = TypeConstraint::IntLiteral;
            for (auto &c : components)
                tc.int_values.push_back(std::stoll(c));
            return tc;
        }

        // Check if all str literals
        bool allStr = true;
        for (auto &c : components) {
            if (!isStrLiteralType(c)) { allStr = false; break; }
        }
        if (allStr) {
            TypeConstraint tc;
            tc.kind = TypeConstraint::StrLiteral;
            for (auto &c : components)
                tc.str_values.push_back(c.substr(1, c.size() - 2));
            return tc;
        }
    }

    return std::nullopt;
}

void CodeGen::emitConstraintCheck(llvm::Value *val, const TypeConstraint &constraint,
                                   const std::string &varName) {
    if (constraint.kind == TypeConstraint::IntLiteral) {
        // Compile-time check if constant
        if (auto *ci = llvm::dyn_cast<llvm::ConstantInt>(val)) {
            int64_t v = ci->getSExtValue();
            bool found = false;
            for (int64_t allowed : constraint.int_values) {
                if (v == allowed) { found = true; break; }
            }
            if (!found) {
                std::string allowed_str;
                for (size_t i = 0; i < constraint.int_values.size(); ++i) {
                    if (i > 0) allowed_str += " | ";
                    allowed_str += std::to_string(constraint.int_values[i]);
                }
                codegenError(
                    "value " + std::to_string(v) + " is not in literal type " + allowed_str +
                    " for variable '" + varName + "'");
            }
            return; // Compile-time check passed
        }
        // Runtime check: compare against each allowed value, OR results
        llvm::Value *anyMatch = llvm::ConstantInt::get(i1Ty_, 0);
        for (int64_t allowed : constraint.int_values) {
            llvm::Value *cmp = builder_.CreateICmpEQ(
                val, llvm::ConstantInt::get(i64Ty_, allowed), "lit_cmp");
            anyMatch = builder_.CreateOr(anyMatch, cmp, "lit_or");
        }
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "constraint.ok", fn_);
        llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "constraint.fail", fn_);
        builder_.CreateCondBr(anyMatch, okBB, failBB);
        builder_.SetInsertPoint(failBB);
        emitRuntimeError("runtime error: value out of range for '" + varName + "'\n",
                          ".constraint_err_" + std::to_string(constraint_err_counter_++));
        builder_.SetInsertPoint(okBB);

    } else if (constraint.kind == TypeConstraint::IntRange) {
        // Compile-time check if constant
        if (auto *ci = llvm::dyn_cast<llvm::ConstantInt>(val)) {
            int64_t v = ci->getSExtValue();
            if (v < constraint.range_low || v > constraint.range_high) {
                codegenError(
                    "value " + std::to_string(v) + " is out of range " +
                    std::to_string(constraint.range_low) + ".." +
                    std::to_string(constraint.range_high) +
                    " for variable '" + varName + "'");
            }
            return; // Compile-time check passed
        }
        // Runtime check: low <= val <= high
        llvm::Value *geLow = builder_.CreateICmpSGE(
            val, llvm::ConstantInt::get(i64Ty_, constraint.range_low), "range_ge");
        llvm::Value *leHigh = builder_.CreateICmpSLE(
            val, llvm::ConstantInt::get(i64Ty_, constraint.range_high), "range_le");
        llvm::Value *inRange = builder_.CreateAnd(geLow, leHigh, "in_range");
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "constraint.ok", fn_);
        llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "constraint.fail", fn_);
        builder_.CreateCondBr(inRange, okBB, failBB);
        builder_.SetInsertPoint(failBB);
        emitRuntimeError("runtime error: value out of range for '" + varName + "'\n",
                          ".constraint_err_" + std::to_string(constraint_err_counter_++));
        builder_.SetInsertPoint(okBB);

    } else if (constraint.kind == TypeConstraint::StrLiteral) {
        // Compile-time check: if the value is a global string constant, check it
        if (auto *constExpr = llvm::dyn_cast<llvm::ConstantExpr>(val)) {
            // Can't easily extract string from ConstantExpr, fall through to runtime
        }
        // For string literals, we need runtime strcmp checks
        llvm::FunctionType *strcmpTy = llvm::FunctionType::get(
            i32Ty_, {ptrTy_, ptrTy_}, false);
        llvm::FunctionCallee strcmpFn = mod_->getOrInsertFunction("strcmp", strcmpTy);

        llvm::Value *anyMatch = llvm::ConstantInt::get(i1Ty_, 0);
        for (const auto &allowed : constraint.str_values) {
            llvm::Constant *allowedStr = builder_.CreateGlobalString(
                allowed, ".str_lit_" + std::to_string(constraint_err_counter_) + "_" + allowed);
            llvm::Value *cmpResult = builder_.CreateCall(strcmpFn, {val, allowedStr}, "strcmp_res");
            llvm::Value *isEq = builder_.CreateICmpEQ(
                cmpResult, llvm::ConstantInt::get(i32Ty_, 0), "str_eq");
            anyMatch = builder_.CreateOr(anyMatch, isEq, "str_or");
        }
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "constraint.ok", fn_);
        llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "constraint.fail", fn_);
        builder_.CreateCondBr(anyMatch, okBB, failBB);
        builder_.SetInsertPoint(failBB);
        emitRuntimeError("runtime error: value not in allowed set for '" + varName + "'\n",
                          ".constraint_err_" + std::to_string(constraint_err_counter_++));
        builder_.SetInsertPoint(okBB);
    }
}

