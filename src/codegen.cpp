#include "ry/codegen.hpp"
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <stdexcept>

CodeGen::CodeGen(bool test_mode) : ctx_(std::make_unique<llvm::LLVMContext>()),
                     mod_(std::make_unique<llvm::Module>("ry", *ctx_)),
                     builder_(*ctx_),
                     test_mode_(test_mode) {
    i64Ty_ = llvm::Type::getInt64Ty(*ctx_);
    i32Ty_ = llvm::Type::getInt32Ty(*ctx_);
    i8Ty_  = llvm::Type::getInt8Ty(*ctx_);
    f64Ty_ = llvm::Type::getDoubleTy(*ctx_);
    i1Ty_  = llvm::Type::getInt1Ty(*ctx_);
    ptrTy_ = llvm::PointerType::getUnqual(*ctx_);

    builtins_["print"] = [this](const std::vector<ExprPtr> &args) { emitPrint(args); };

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
    cg_.scope_stack_.clear();
    cg_.immutable_scope_stack_.clear();
}

CodeGen::FnScope::~FnScope() {
    cg_.fn_ = savedFn_;
    cg_.scope_stack_ = std::move(savedScope_);
    cg_.immutable_scope_stack_ = std::move(savedConstScope_);
    cg_.builder_.SetInsertPoint(savedBlock_, savedPoint_);
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

llvm::orc::ThreadSafeModule CodeGen::compile(Program &prog) {
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
        throw std::runtime_error("IR verify error: " + err);

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
        throw std::runtime_error("undefined function: " + callee);

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
        throw std::runtime_error("no matching overload for '" + callee + "'");
    if (candidates.size() > 1)
        throw std::runtime_error("ambiguous call to '" + callee + "'");

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
    if (fn->getReturnType()->isVoidTy())
        return builder_.CreateCall(fn, argVals);
    llvm::Value *callResult = builder_.CreateCall(fn, argVals, "calltmp");
    // Propagate result type info from overload entry
    auto fit = functions_.find(callee);
    if (fit != functions_.end()) {
        for (auto &entry : fit->second) {
            if (entry.func == fn && !entry.returnTypeName.empty()) {
                if (isResultTypeName(entry.returnTypeName)) {
                    result_value_types_[callResult] = entry.returnTypeName;
                }
                break;
            }
        }
    }
    return callResult;
}

void CodeGen::emitStmt(CallStmt &s) {
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
    // Intercept add/remove for sets (UFCS: s.add(x) → add(s, x))
    if ((s.callee == "add" || s.callee == "remove") && s.args.size() == 2) {
        // Peek at first arg: is it a set variable?
        if (auto *ve = std::get_if<VariableExpr>(&s.args[0]->data)) {
            llvm::AllocaInst *alloca = findVar(ve->name);
            if (alloca && getSetElementType(alloca)) {
                // Route through CallExpr emitter which handles set add/remove
                auto ce = std::make_unique<CallExpr>();
                ce->callee = s.callee;
                ce->args = std::move(s.args);
                emitExprVariant(ce);
                return;
            }
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
    if (typeName == "int")   return i64Ty_;
    if (typeName == "byte")  return i8Ty_;
    if (typeName == "float") return f64Ty_;
    if (typeName == "bool")  return i1Ty_;
    if (typeName == "str")   return ptrTy_;
    if (typeName == "Unit")  return llvm::Type::getVoidTy(*ctx_);

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

    // Result<T, E> parsing
    if (isResultTypeName(typeName)) {
        auto &info = getOrCreateResultType(typeName);
        return info.llvmType;
    }

    auto it = struct_types_.find(typeName);
    if (it != struct_types_.end()) return it->second.llvmType;

    // enum name → i64
    if (enum_types_.count(typeName)) return i64Ty_;

    throw std::runtime_error("unknown type: " + typeName);
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
        throw std::runtime_error("invalid function type: " + typeStr);

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

CodeGen::ResultTypeInfo &CodeGen::getOrCreateResultType(const std::string &typeStr) {
    auto it = result_types_.find(typeStr);
    if (it != result_types_.end()) return it->second;

    // Parse "Result<T, E>" → extract T and E
    std::string inner = typeStr.substr(7, typeStr.size() - 8); // strip "Result<" and ">"
    size_t depth = 0;
    size_t commaPos = std::string::npos;
    for (size_t i = 0; i < inner.size(); ++i) {
        if (inner[i] == '<') ++depth;
        else if (inner[i] == '>') --depth;
        else if (inner[i] == ',' && depth == 0) { commaPos = i; break; }
    }
    if (commaPos == std::string::npos)
        throw std::runtime_error("invalid Result type: " + typeStr);

    std::string okStr = inner.substr(0, commaPos);
    std::string errStr = inner.substr(commaPos + 1);
    // Trim spaces
    while (!okStr.empty() && okStr.back() == ' ') okStr.pop_back();
    while (!errStr.empty() && errStr.front() == ' ') errStr = errStr.substr(1);

    llvm::Type *okTy = resolveType(okStr);
    llvm::Type *errTy = resolveType(errStr);

    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t okSize = dl.getTypeAllocSize(okTy);
    uint64_t errSize = dl.getTypeAllocSize(errTy);
    uint64_t maxSize = std::max(okSize, errSize);
    if (maxSize < 8) maxSize = 8;

    auto *dataTy = llvm::ArrayType::get(i8Ty_, maxSize);
    auto *resultTy = llvm::StructType::get(*ctx_, {i64Ty_, dataTy});

    result_types_[typeStr] = {resultTy, okTy, errTy};
    return result_types_[typeStr];
}

bool CodeGen::isResultType(llvm::Type *ty) {
    for (auto &pair : result_types_) {
        if (pair.second.llvmType == ty) return true;
    }
    return false;
}

CodeGen::ResultTypeInfo *CodeGen::findResultTypeInfoByLLVMType(llvm::Type *ty) {
    for (auto &pair : result_types_) {
        if (pair.second.llvmType == ty) return &pair.second;
    }
    return nullptr;
}

std::string CodeGen::getResultTypeStr(llvm::Value *val) {
    auto it = result_value_types_.find(val);
    if (it != result_value_types_.end()) return it->second;
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
        auto it2 = result_value_types_.find(load->getPointerOperand());
        if (it2 != result_value_types_.end()) return it2->second;
    }
    return "";
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
    } else {
        llvm::Constant *fmt = builder_.CreateGlobalString("%ld", ".fmt_i" + suffix);
        builder_.CreateCall(printfFn, {fmt, val});
    }
}

