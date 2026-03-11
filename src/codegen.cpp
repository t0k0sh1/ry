#include "ry/codegen.hpp"
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <stdexcept>

CodeGen::CodeGen() : ctx_(std::make_unique<llvm::LLVMContext>()),
                     mod_(std::make_unique<llvm::Module>("ry", *ctx_)),
                     builder_(*ctx_) {
    i64Ty_ = llvm::Type::getInt64Ty(*ctx_);
    i32Ty_ = llvm::Type::getInt32Ty(*ctx_);
    f64Ty_ = llvm::Type::getDoubleTy(*ctx_);
    i1Ty_  = llvm::Type::getInt1Ty(*ctx_);
    ptrTy_ = llvm::PointerType::getUnqual(*ctx_);

    builtins_["print"] = [this](const std::vector<ExprPtr> &args) { emitPrint(args); };

    listHeaderTy_ = llvm::StructType::create(*ctx_, {i64Ty_, i64Ty_, ptrTy_}, "ListHeader");
    mapHeaderTy_ = llvm::StructType::create(*ctx_, {i64Ty_, i64Ty_, ptrTy_, ptrTy_}, "MapHeader");
}

// ===== B5: FnScope RAII =====

CodeGen::FnScope::FnScope(CodeGen &cg) : cg_(cg) {
    savedFn_ = cg_.fn_;
    savedScope_ = std::move(cg_.scope_stack_);
    savedConstScope_ = std::move(cg_.const_scope_stack_);
    savedBlock_ = cg_.builder_.GetInsertBlock();
    savedPoint_ = cg_.builder_.GetInsertPoint();
    cg_.scope_stack_.clear();
    cg_.const_scope_stack_.clear();
}

CodeGen::FnScope::~FnScope() {
    cg_.fn_ = savedFn_;
    cg_.scope_stack_ = std::move(savedScope_);
    cg_.const_scope_stack_ = std::move(savedConstScope_);
    cg_.builder_.SetInsertPoint(savedBlock_, savedPoint_);
}

// ===== Scope management =====

void CodeGen::pushScope() {
    scope_stack_.emplace_back();
    const_scope_stack_.emplace_back();
}

void CodeGen::popScope() {
    scope_stack_.pop_back();
    const_scope_stack_.pop_back();
}

llvm::AllocaInst *CodeGen::findVar(const std::string &name) {
    for (auto it = scope_stack_.rbegin(); it != scope_stack_.rend(); ++it) {
        auto found = it->find(name);
        if (found != it->end())
            return found->second;
    }
    return nullptr;
}

bool CodeGen::isConst(const std::string &name) const {
    for (auto it = const_scope_stack_.rbegin(); it != const_scope_stack_.rend(); ++it) {
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

    if (!builder_.GetInsertBlock()->getTerminator())
        builder_.CreateRet(llvm::ConstantInt::get(i32Ty_, 0));

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
    return v;
}

std::pair<llvm::Value*, llvm::Value*> CodeGen::promoteToFloat(llvm::Value *lhs, llvm::Value *rhs) {
    if (!lhs->getType()->isDoubleTy())
        lhs = builder_.CreateSIToFP(lhs, f64Ty_, "lhs_f");
    if (!rhs->getType()->isDoubleTy())
        rhs = builder_.CreateSIToFP(rhs, f64Ty_, "rhs_f");
    return {lhs, rhs};
}

// ===== B3: emitVarDecl =====

void CodeGen::emitVarDecl(const std::string &name,
                           const std::optional<std::string> &type_annotation,
                           ExprNode &value, bool is_const) {
    if (scope_stack_.back().count(name))
        throw std::runtime_error("redeclared variable: " + name);

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
        if (is_const)
            const_scope_stack_.back().insert(name);
        return;
    }

    llvm::Value *val = emitExpr(value);
    llvm::Type *newTy = val->getType();

    if (type_annotation) {
        llvm::Type *annotTy = resolveType(*type_annotation);
        if (annotTy != newTy)
            throw std::runtime_error(
                "type error: annotation '" + *type_annotation +
                "' does not match expression type for variable '" + name + "'");
    }

    llvm::AllocaInst *ptr = getOrCreateVar(name, newTy);
    builder_.CreateStore(val, ptr);

    // Track list/map element types if this is a ptr value
    if (newTy == ptrTy_) {
        // --- List tracking ---
        llvm::Type *elemTy = getListElementType(val);
        if (!elemTy && type_annotation && type_annotation->size() > 5 &&
            type_annotation->substr(0, 5) == "list[") {
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
        // From type annotation: map[K, V]
        if (!keyTy && type_annotation && type_annotation->size() > 4 &&
            type_annotation->substr(0, 4) == "map[") {
            std::tie(keyTy, valTy) = parseMapTypeAnnotation(*type_annotation);
        }
        if (keyTy) map_key_types_[ptr] = keyTy;
        if (valTy) map_value_types_[ptr] = valTy;
    }

    if (is_const)
        const_scope_stack_.back().insert(name);
}

void CodeGen::emitStmt(LetStmt &s)   { emitVarDecl(s.name, s.type_annotation, *s.value, false); }
void CodeGen::emitStmt(ConstStmt &s) { emitVarDecl(s.name, s.type_annotation, *s.value, true); }

void CodeGen::emitStmt(AssignStmt &s) {
    llvm::AllocaInst *ptr = findVar(s.name);
    if (!ptr)
        throw std::runtime_error("undeclared variable: " + s.name);

    if (isConst(s.name))
        throw std::runtime_error("cannot reassign const variable: " + s.name);

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

    if (ptr->getAllocatedType() != newTy)
        throw std::runtime_error(
            "type error: variable '" + s.name +
            "' cannot be reassigned to a different type");

    builder_.CreateStore(val, ptr);
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
                if (emittedArgs[i]->getType() != entry.paramTypes[i]) { match = false; break; }
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

    // Build final arg values (fill in None args with proper Option type)
    outArgVals.clear();
    for (size_t i = 0; i < args.size(); ++i) {
        if (isNone[i]) {
            outArgVals.push_back(buildNoneValue(chosen->paramTypes[i]));
        } else {
            outArgVals.push_back(emittedArgs[i]);
        }
    }

    return chosen->func;
}

llvm::Value *CodeGen::emitUserFnCall(const std::string &callee, const std::vector<ExprPtr> &args) {
    std::vector<llvm::Value*> argVals;
    llvm::Function *fn = resolveOverload(callee, args, argVals);
    if (fn->getReturnType()->isVoidTy())
        return builder_.CreateCall(fn, argVals);
    return builder_.CreateCall(fn, argVals, "calltmp");
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
    pushScope();
    for (auto &stmt : s->body)
        std::visit([this](auto &st) { emitStmt(st); }, stmt);
    popScope();
    if (!builder_.GetInsertBlock()->getTerminator())
        builder_.CreateBr(condBB);

    builder_.SetInsertPoint(endBB);
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

llvm::Value *CodeGen::emitExpr(const ExprNode &node) {
    return std::visit([this](const auto &e) -> llvm::Value* { return emitExprVariant(e); },
                      node.data);
}

llvm::Value *CodeGen::emitExprVariant(const NumberExpr &e) {
    return llvm::ConstantInt::get(i64Ty_, e.value, true);
}

llvm::Value *CodeGen::emitExprVariant(const FloatExpr &e) {
    return llvm::ConstantFP::get(f64Ty_, e.value);
}

llvm::Value *CodeGen::emitExprVariant(const BoolExpr &e) {
    return llvm::ConstantInt::get(i1Ty_, e.value ? 1 : 0, false);
}

llvm::Value *CodeGen::emitExprVariant(const StringExpr &e) {
    return builder_.CreateGlobalString(e.value, ".str");
}

llvm::Value *CodeGen::emitExprVariant(const VariableExpr &e) {
    llvm::AllocaInst *alloca = findVar(e.name);
    if (!alloca)
        throw std::runtime_error("undefined variable: " + e.name);
    llvm::Type *ty = alloca->getAllocatedType();
    return builder_.CreateLoad(ty, alloca, e.name);
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<UnaryExpr> &e) {
    llvm::Value *val = emitExpr(*e->operand);

    // Try user-defined unary operator first
    std::string opFnName = "operator" + e->op;
    if (auto *result = tryUnaryOperatorCall(opFnName, val))
        return result;

    if (e->op == "+") {
        return val;
    }
    if (e->op == "-") {
        if (val->getType()->isDoubleTy())
            return builder_.CreateFNeg(val, "fneg");
        val = promoteToInt(val);
        return builder_.CreateNeg(val, "neg");
    }
    if (e->op == "not") {
        llvm::Value *boolVal = toBool(val);
        return builder_.CreateNot(boolVal, "not");
    }
    if (e->op == "~") {
        if (val->getType()->isDoubleTy())
            throw std::runtime_error("bitwise NOT (~) requires integer, got float");
        val = promoteToInt(val);
        return builder_.CreateNot(val, "bnot");
    }
    throw std::runtime_error("unknown unary operator: " + e->op);
}

// ===== Operator overload helpers =====

llvm::Value *CodeGen::tryOperatorCall(const std::string &opFnName,
                                       llvm::Value *lhs, llvm::Value *rhs) {
    auto fit = functions_.find(opFnName);
    if (fit == functions_.end())
        return nullptr;

    llvm::Type *lhsTy = lhs->getType();
    llvm::Type *rhsTy = rhs->getType();

    for (auto &entry : fit->second) {
        if (entry.paramTypes.size() == 2 &&
            entry.paramTypes[0] == lhsTy &&
            entry.paramTypes[1] == rhsTy) {
            if (entry.func->getReturnType()->isVoidTy())
                return builder_.CreateCall(entry.func, {lhs, rhs});
            return builder_.CreateCall(entry.func, {lhs, rhs}, "opcall");
        }
    }
    return nullptr;
}

llvm::Value *CodeGen::tryUnaryOperatorCall(const std::string &opFnName,
                                            llvm::Value *operand) {
    auto fit = functions_.find(opFnName);
    if (fit == functions_.end())
        return nullptr;

    llvm::Type *opTy = operand->getType();

    for (auto &entry : fit->second) {
        if (entry.paramTypes.size() == 1 &&
            entry.paramTypes[0] == opTy) {
            if (entry.func->getReturnType()->isVoidTy())
                return builder_.CreateCall(entry.func, {operand});
            return builder_.CreateCall(entry.func, {operand}, "opcall");
        }
    }
    return nullptr;
}

// ===== B2: BinaryExpr sub-dispatchers =====

llvm::Value *CodeGen::emitComparisonOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs) {
    // String comparison via strcmp
    if (lhs->getType() == ptrTy_ && rhs->getType() == ptrTy_) {
        auto strcmpTy = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_}, false);
        auto strcmpFn = mod_->getOrInsertFunction("strcmp", strcmpTy);
        llvm::Value *cmp = builder_.CreateCall(strcmpFn, {lhs, rhs}, "strcmp");
        llvm::Value *zero = llvm::ConstantInt::get(i32Ty_, 0);
        if (op == "==") return builder_.CreateICmpEQ(cmp, zero, "str_eq");
        if (op == "!=") return builder_.CreateICmpNE(cmp, zero, "str_ne");
        if (op == "<")  return builder_.CreateICmpSLT(cmp, zero, "str_lt");
        if (op == "<=") return builder_.CreateICmpSLE(cmp, zero, "str_le");
        if (op == ">")  return builder_.CreateICmpSGT(cmp, zero, "str_gt");
        if (op == ">=") return builder_.CreateICmpSGE(cmp, zero, "str_ge");
        throw std::runtime_error("unsupported string comparison: " + op);
    }

    lhs = promoteToInt(lhs);
    rhs = promoteToInt(rhs);

    bool lf = lhs->getType()->isDoubleTy();
    bool rf = rhs->getType()->isDoubleTy();
    if (lf || rf) {
        std::tie(lhs, rhs) = promoteToFloat(lhs, rhs);
        llvm::CmpInst::Predicate pred;
        if      (op == "==") pred = llvm::CmpInst::FCMP_OEQ;
        else if (op == "!=") pred = llvm::CmpInst::FCMP_ONE;
        else if (op == "<")  pred = llvm::CmpInst::FCMP_OLT;
        else if (op == "<=") pred = llvm::CmpInst::FCMP_OLE;
        else if (op == ">")  pred = llvm::CmpInst::FCMP_OGT;
        else                 pred = llvm::CmpInst::FCMP_OGE;
        return builder_.CreateFCmp(pred, lhs, rhs, "fcmp");
    }
    llvm::CmpInst::Predicate pred;
    if      (op == "==") pred = llvm::CmpInst::ICMP_EQ;
    else if (op == "!=") pred = llvm::CmpInst::ICMP_NE;
    else if (op == "<")  pred = llvm::CmpInst::ICMP_SLT;
    else if (op == "<=") pred = llvm::CmpInst::ICMP_SLE;
    else if (op == ">")  pred = llvm::CmpInst::ICMP_SGT;
    else                 pred = llvm::CmpInst::ICMP_SGE;
    return builder_.CreateICmp(pred, lhs, rhs, "icmp");
}

llvm::Value *CodeGen::emitLogicalOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs) {
    llvm::Value *lhsBool = toBool(lhs);
    llvm::Value *rhsBool = toBool(rhs);
    if (op == "and")
        return builder_.CreateAnd(lhsBool, rhsBool, "and");
    return builder_.CreateOr(lhsBool, rhsBool, "or");
}

llvm::Value *CodeGen::emitBitwiseOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs) {
    if (lhs->getType()->isDoubleTy() || rhs->getType()->isDoubleTy())
        throw std::runtime_error(
            "bitwise operator '" + op + "' requires integer operands, got float");
    lhs = promoteToInt(lhs);
    rhs = promoteToInt(rhs);
    if (op == "&")  return builder_.CreateAnd(lhs, rhs,  "band");
    if (op == "|")  return builder_.CreateOr(lhs,  rhs,  "bor");
    if (op == "^")  return builder_.CreateXor(lhs, rhs,  "bxor");
    if (op == "<<") return builder_.CreateShl(lhs,  rhs, "shl");
    if (op == ">>") return builder_.CreateAShr(lhs, rhs, "ashr");
    throw std::runtime_error("unknown bitwise operator: " + op);
}

llvm::Value *CodeGen::emitArithmeticOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs) {
    // ** 累乗: 常にf64、libmのpow()を呼ぶ
    if (op == "**") {
        if (lhs->getType()->isIntegerTy()) lhs = builder_.CreateSIToFP(lhs, f64Ty_, "lhs_f");
        if (rhs->getType()->isIntegerTy()) rhs = builder_.CreateSIToFP(rhs, f64Ty_, "rhs_f");
        llvm::FunctionType *powTy = llvm::FunctionType::get(f64Ty_, {f64Ty_, f64Ty_}, false);
        llvm::FunctionCallee powFn = mod_->getOrInsertFunction("pow", powTy);
        return builder_.CreateCall(powFn, {lhs, rhs}, "pow");
    }

    // String concatenation
    if (op == "+" && lhs->getType() == ptrTy_ && rhs->getType() == ptrTy_) {
        auto strlenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto strlenFn = mod_->getOrInsertFunction("strlen", strlenTy);
        auto mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        auto mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
        auto strcpyTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
        auto strcpyFn = mod_->getOrInsertFunction("strcpy", strcpyTy);
        auto strcatTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
        auto strcatFn = mod_->getOrInsertFunction("strcat", strcatTy);

        llvm::Value *lenL = builder_.CreateCall(strlenFn, {lhs}, "len_l");
        llvm::Value *lenR = builder_.CreateCall(strlenFn, {rhs}, "len_r");
        llvm::Value *total = builder_.CreateAdd(lenL, lenR, "total_len");
        total = builder_.CreateAdd(total, llvm::ConstantInt::get(i64Ty_, 1), "total_plus_null");
        llvm::Value *buf = builder_.CreateCall(mallocFn, {total}, "concat_buf");
        builder_.CreateCall(strcpyFn, {buf, lhs});
        builder_.CreateCall(strcatFn, {buf, rhs});
        return buf;
    }

    // // 整数除算: f64入力はi64に変換してからsdiv
    if (op == "//") {
        lhs = promoteToInt(lhs);
        rhs = promoteToInt(rhs);
        if (lhs->getType()->isDoubleTy()) lhs = builder_.CreateFPToSI(lhs, i64Ty_, "lhs_i");
        if (rhs->getType()->isDoubleTy()) rhs = builder_.CreateFPToSI(rhs, i64Ty_, "rhs_i");
        return builder_.CreateSDiv(lhs, rhs, "idiv");
    }

    // / 除算: 常にf64
    if (op == "/") {
        std::tie(lhs, rhs) = promoteToFloat(lhs, rhs);
        return builder_.CreateFDiv(lhs, rhs, "div");
    }

    // % 剰余: 片方f64ならfrem、両方i64ならsrem
    if (op == "%") {
        lhs = promoteToInt(lhs);
        rhs = promoteToInt(rhs);
        bool lf = lhs->getType()->isDoubleTy();
        bool rf = rhs->getType()->isDoubleTy();
        if (lf || rf) {
            std::tie(lhs, rhs) = promoteToFloat(lhs, rhs);
            return builder_.CreateFRem(lhs, rhs, "frem");
        }
        return builder_.CreateSRem(lhs, rhs, "srem");
    }

    // +/-/*: 片方f64なら浮動小数点命令
    lhs = promoteToInt(lhs);
    rhs = promoteToInt(rhs);
    bool lf = lhs->getType()->isDoubleTy();
    bool rf = rhs->getType()->isDoubleTy();
    if (lf || rf) {
        std::tie(lhs, rhs) = promoteToFloat(lhs, rhs);
        if (op == "+") return builder_.CreateFAdd(lhs, rhs, "fadd");
        if (op == "-") return builder_.CreateFSub(lhs, rhs, "fsub");
        if (op == "*") return builder_.CreateFMul(lhs, rhs, "fmul");
        throw std::runtime_error("unknown operator: " + op);
    }
    if (op == "+") return builder_.CreateAdd(lhs, rhs, "add");
    if (op == "-") return builder_.CreateSub(lhs, rhs, "sub");
    if (op == "*") return builder_.CreateMul(lhs, rhs, "mul");
    throw std::runtime_error("unknown operator: " + op);
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<BinaryExpr> &e) {
    llvm::Value *lhs = emitExpr(*e->lhs);
    llvm::Value *rhs = emitExpr(*e->rhs);
    const std::string &op = e->op;

    // Try user-defined binary operator first
    std::string opFnName = "operator" + op;
    if (auto *result = tryOperatorCall(opFnName, lhs, rhs))
        return result;

    if (op == "==" || op == "!=" || op == "<" ||
        op == "<=" || op == ">"  || op == ">=")
        return emitComparisonOp(op, lhs, rhs);

    if (op == "and" || op == "or")
        return emitLogicalOp(op, lhs, rhs);

    if (op == "&" || op == "|" || op == "^" ||
        op == "<<" || op == ">>")
        return emitBitwiseOp(op, lhs, rhs);

    return emitArithmeticOp(op, lhs, rhs);
}

llvm::Type *CodeGen::resolveType(const std::string &typeName) {
    if (typeName == "int")   return i64Ty_;
    if (typeName == "float") return f64Ty_;
    if (typeName == "bool")  return i1Ty_;
    if (typeName == "str")   return ptrTy_;
    if (typeName == "Unit")  return llvm::Type::getVoidTy(*ctx_);

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

    // list[T] parsing
    if (typeName.size() > 5 && typeName.substr(0, 5) == "list[" && typeName.back() == ']') {
        return ptrTy_;
    }

    // map[K, V] parsing
    if (typeName.size() > 4 && typeName.substr(0, 4) == "map[" && typeName.back() == ']') {
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
        if (inner[i] == '[') ++depth;
        else if (inner[i] == ']') --depth;
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
    } else if (ty->isDoubleTy()) {
        llvm::Constant *fmt = builder_.CreateGlobalString("%g", ".fmt_f" + suffix);
        builder_.CreateCall(printfFn, {fmt, val});
    } else {
        llvm::Constant *fmt = builder_.CreateGlobalString("%ld", ".fmt_i" + suffix);
        builder_.CreateCall(printfFn, {fmt, val});
    }
}

llvm::Type *CodeGen::getListElementType(llvm::Value *listAlloca) {
    auto it = list_element_types_.find(listAlloca);
    if (it != list_element_types_.end())
        return it->second;
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(listAlloca)) {
        auto it2 = list_element_types_.find(load->getPointerOperand());
        if (it2 != list_element_types_.end())
            return it2->second;
    }
    return nullptr;
}

llvm::Type *CodeGen::getMapKeyType(llvm::Value *mapVal) {
    auto it = map_key_types_.find(mapVal);
    if (it != map_key_types_.end()) return it->second;
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(mapVal)) {
        auto it2 = map_key_types_.find(load->getPointerOperand());
        if (it2 != map_key_types_.end()) return it2->second;
    }
    return nullptr;
}

llvm::Type *CodeGen::getMapValueType(llvm::Value *mapVal) {
    auto it = map_value_types_.find(mapVal);
    if (it != map_value_types_.end()) return it->second;
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(mapVal)) {
        auto it2 = map_value_types_.find(load->getPointerOperand());
        if (it2 != map_value_types_.end()) return it2->second;
    }
    return nullptr;
}

llvm::Value *CodeGen::emitMapKeyLookup(llvm::Value *mapPtr, llvm::Value *key, llvm::Type *keyTy) {
    // Linear scan of keys array, returns index (i64) or -1 if not found
    llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, mapPtr, 0, "map_len_ptr");
    llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "map_len");
    llvm::Value *keysPtrField = builder_.CreateStructGEP(mapHeaderTy_, mapPtr, 2, "map_keys_ptr");
    llvm::Value *keysPtr = builder_.CreateLoad(ptrTy_, keysPtrField, "map_keys");

    // Allocate result variable
    llvm::AllocaInst *resultVar = builder_.CreateAlloca(i64Ty_, nullptr, "lookup_result");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, -1), resultVar);

    llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "lookup_i");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

    llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "lookup.cond", fn_);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "lookup.body", fn_);
    llvm::BasicBlock *foundBB = llvm::BasicBlock::Create(*ctx_, "lookup.found", fn_);
    llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "lookup.next", fn_);
    llvm::BasicBlock *exitBB = llvm::BasicBlock::Create(*ctx_, "lookup.exit", fn_);

    builder_.CreateBr(condBB);

    // cond: i < length
    builder_.SetInsertPoint(condBB);
    llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "i");
    llvm::Value *cond = builder_.CreateICmpSLT(iVal, length, "lookup_cond");
    builder_.CreateCondBr(cond, bodyBB, exitBB);

    // body: compare keys[i] with key
    builder_.SetInsertPoint(bodyBB);
    llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "i_cur");
    llvm::Value *keyElemPtr = builder_.CreateGEP(keyTy, keysPtr, {iCur}, "key_elem_ptr");
    llvm::Value *keyElem = builder_.CreateLoad(keyTy, keyElemPtr, "key_elem");

    llvm::Value *isEqual;
    if (keyTy == ptrTy_) {
        // String comparison using strcmp
        llvm::FunctionType *strcmpTy = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_}, false);
        llvm::FunctionCallee strcmpFn = mod_->getOrInsertFunction("strcmp", strcmpTy);
        llvm::Value *cmpResult = builder_.CreateCall(strcmpFn, {keyElem, key}, "strcmp_result");
        isEqual = builder_.CreateICmpEQ(cmpResult, llvm::ConstantInt::get(i32Ty_, 0), "str_eq");
    } else if (keyTy->isDoubleTy()) {
        isEqual = builder_.CreateFCmpOEQ(keyElem, key, "key_eq");
    } else {
        isEqual = builder_.CreateICmpEQ(keyElem, key, "key_eq");
    }
    builder_.CreateCondBr(isEqual, foundBB, nextBB);

    // found: store index
    builder_.SetInsertPoint(foundBB);
    llvm::Value *iFound = builder_.CreateLoad(i64Ty_, iVar, "i_found");
    builder_.CreateStore(iFound, resultVar);
    builder_.CreateBr(exitBB);

    // next: i++
    builder_.SetInsertPoint(nextBB);
    llvm::Value *iNext = builder_.CreateAdd(
        builder_.CreateLoad(i64Ty_, iVar, "i_next_load"),
        llvm::ConstantInt::get(i64Ty_, 1), "i_next");
    builder_.CreateStore(iNext, iVar);
    builder_.CreateBr(condBB);

    // exit: return result
    builder_.SetInsertPoint(exitBB);
    return builder_.CreateLoad(i64Ty_, resultVar, "lookup_idx");
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
        if (val->getType() != retTy)
            throw std::runtime_error("return type mismatch");
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

    overloads.push_back({func, paramTypes});

    {
        FnScope guard(*this);
        fn_ = func;
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
            if (ptype.size() > 5 && ptype.substr(0, 5) == "list[" && ptype.back() == ']') {
                std::string inner = ptype.substr(5, ptype.size() - 6);
                list_element_types_[alloca] = resolveType(inner);
            }
            // Track map key/value types for map parameters
            if (ptype.size() > 4 && ptype.substr(0, 4) == "map[" && ptype.back() == ']') {
                auto [kTy, vTy] = parseMapTypeAnnotation(ptype);
                if (kTy) map_key_types_[alloca] = kTy;
                if (vTy) map_value_types_[alloca] = vTy;
            }
            ++idx;
        }

        for (auto &stmt : s->body)
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
                builder_.CreateRet(llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)));
            else if (llvm::isa<llvm::StructType>(retTy))
                builder_.CreateRet(llvm::UndefValue::get(retTy));
        }

        std::string err;
        llvm::raw_string_ostream errStream(err);
        if (llvm::verifyFunction(*func, &errStream))
            throw std::runtime_error("IR verify error in function '" + s->name + "': " + err);
    }
    // FnScope destructor restores fn_, scope_stack_, const_scope_stack_, builder_ insert point
}

// ===== B4: CallExpr using emitUserFnCall =====

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<CallExpr> &e) {
    // len(xs) → list/map length
    if (e->callee == "len") {
        if (e->args.size() != 1)
            throw std::runtime_error("len() takes exactly 1 argument");
        llvm::Value *ptr = emitExpr(*e->args[0]);
        if (ptr->getType() != ptrTy_)
            throw std::runtime_error("len() requires list, map, or str argument");
        // Check if it's a map
        llvm::Type *mapKeyTy = getMapKeyType(ptr);
        if (mapKeyTy) {
            llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, ptr, 0, "map_len_ptr");
            return builder_.CreateLoad(i64Ty_, lenPtr, "map_len");
        }
        // Check if it's a list
        if (getListElementType(ptr)) {
            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, ptr, 0, "len_ptr");
            return builder_.CreateLoad(i64Ty_, lenPtr, "len");
        }
        // String: call strlen
        auto strlenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto strlenFn = mod_->getOrInsertFunction("strlen", strlenTy);
        return builder_.CreateCall(strlenFn, {ptr}, "str_len");
    }

    // Some(x) → Option<T> constructor
    if (e->callee == "Some") {
        if (e->args.size() != 1)
            throw std::runtime_error("Some() takes exactly 1 argument");
        llvm::Value *inner = emitExpr(*e->args[0]);
        llvm::StructType *optTy = getOptionType(inner->getType());
        llvm::Value *result = llvm::UndefValue::get(optTy);
        result = builder_.CreateInsertValue(result, llvm::ConstantInt::get(i1Ty_, 1), 0);
        result = builder_.CreateInsertValue(result, inner, 1);
        return result;
    }

    // unwrap(opt) → extract value or runtime error
    if (e->callee == "unwrap") {
        if (e->args.size() != 1)
            throw std::runtime_error("unwrap() takes exactly 1 argument");
        llvm::Value *opt = emitExpr(*e->args[0]);
        if (!isOptionType(opt->getType()))
            throw std::runtime_error("unwrap() requires Option type argument");

        llvm::Value *hasValue = builder_.CreateExtractValue(opt, 0, "has_value");

        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "unwrap.ok", fn_);
        llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "unwrap.fail", fn_);

        builder_.CreateCondBr(hasValue, okBB, failBB);

        // fail: print error and exit
        builder_.SetInsertPoint(failBB);
        emitRuntimeError("runtime error: unwrap() called on None\n", ".unwrap_err");

        // ok: extract value
        builder_.SetInsertPoint(okBB);
        return builder_.CreateExtractValue(opt, 1, "unwrap_val");
    }

    // has_key(map, key) → bool
    if (e->callee == "has_key") {
        if (e->args.size() != 2)
            throw std::runtime_error("has_key() takes exactly 2 arguments");
        llvm::Value *mapPtr = emitExpr(*e->args[0]);
        if (mapPtr->getType() != ptrTy_)
            throw std::runtime_error("has_key() requires map as first argument");
        llvm::Type *keyTy = getMapKeyType(mapPtr);
        if (!keyTy)
            throw std::runtime_error("has_key() requires map as first argument");
        llvm::Value *key = emitExpr(*e->args[1]);
        if (key->getType() != keyTy)
            throw std::runtime_error("has_key() key type mismatch");
        llvm::Value *idx = emitMapKeyLookup(mapPtr, key, keyTy);
        return builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "has_key");
    }

    // contains(s, sub) → bool
    if (e->callee == "contains") {
        if (e->args.size() != 2)
            throw std::runtime_error("contains() takes exactly 2 arguments");
        llvm::Value *s = emitExpr(*e->args[0]);
        llvm::Value *sub = emitExpr(*e->args[1]);
        if (s->getType() != ptrTy_ || sub->getType() != ptrTy_)
            throw std::runtime_error("contains() requires str arguments");
        auto strstrTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
        auto strstrFn = mod_->getOrInsertFunction("strstr", strstrTy);
        llvm::Value *result = builder_.CreateCall(strstrFn, {s, sub}, "strstr");
        llvm::Value *null = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
        return builder_.CreateICmpNE(result, null, "contains");
    }

    // starts_with(s, prefix) → bool
    if (e->callee == "starts_with") {
        if (e->args.size() != 2)
            throw std::runtime_error("starts_with() takes exactly 2 arguments");
        llvm::Value *s = emitExpr(*e->args[0]);
        llvm::Value *prefix = emitExpr(*e->args[1]);
        if (s->getType() != ptrTy_ || prefix->getType() != ptrTy_)
            throw std::runtime_error("starts_with() requires str arguments");
        auto strlenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto strlenFn = mod_->getOrInsertFunction("strlen", strlenTy);
        auto strncmpTy = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_, i64Ty_}, false);
        auto strncmpFn = mod_->getOrInsertFunction("strncmp", strncmpTy);
        llvm::Value *prefixLen = builder_.CreateCall(strlenFn, {prefix}, "prefix_len");
        llvm::Value *cmp = builder_.CreateCall(strncmpFn, {s, prefix, prefixLen}, "strncmp");
        return builder_.CreateICmpEQ(cmp, llvm::ConstantInt::get(i32Ty_, 0), "starts_with");
    }

    // ends_with(s, suffix) → bool
    if (e->callee == "ends_with") {
        if (e->args.size() != 2)
            throw std::runtime_error("ends_with() takes exactly 2 arguments");
        llvm::Value *s = emitExpr(*e->args[0]);
        llvm::Value *suffix = emitExpr(*e->args[1]);
        if (s->getType() != ptrTy_ || suffix->getType() != ptrTy_)
            throw std::runtime_error("ends_with() requires str arguments");
        auto strlenTy = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
        auto strlenFn = mod_->getOrInsertFunction("strlen", strlenTy);
        auto strncmpTy = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_, i64Ty_}, false);
        auto strncmpFn = mod_->getOrInsertFunction("strncmp", strncmpTy);
        llvm::Value *sLen = builder_.CreateCall(strlenFn, {s}, "s_len");
        llvm::Value *suffixLen = builder_.CreateCall(strlenFn, {suffix}, "suffix_len");

        // if suffixLen > sLen, return false; else strncmp(s + offset, suffix, suffixLen) == 0
        llvm::Value *tooLong = builder_.CreateICmpUGT(suffixLen, sLen, "too_long");

        llvm::BasicBlock *checkBB = llvm::BasicBlock::Create(*ctx_, "ew.check", fn_);
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "ew.merge", fn_);
        llvm::BasicBlock *curBB = builder_.GetInsertBlock();

        builder_.CreateCondBr(tooLong, mergeBB, checkBB);

        // checkBB: compute strncmp
        builder_.SetInsertPoint(checkBB);
        llvm::Value *offset = builder_.CreateSub(sLen, suffixLen, "offset");
        llvm::Value *tailPtr = builder_.CreateGEP(builder_.getInt8Ty(), s, offset, "tail_ptr");
        llvm::Value *cmp = builder_.CreateCall(strncmpFn, {tailPtr, suffix, suffixLen}, "strncmp");
        llvm::Value *match = builder_.CreateICmpEQ(cmp, llvm::ConstantInt::get(i32Ty_, 0), "match");
        builder_.CreateBr(mergeBB);

        // mergeBB: PHI
        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = builder_.CreatePHI(i1Ty_, 2, "ends_with");
        phi->addIncoming(llvm::ConstantInt::get(i1Ty_, 0), curBB);
        phi->addIncoming(match, checkBB);
        return phi;
    }

    auto sit = struct_types_.find(e->callee);
    if (sit != struct_types_.end())
        return emitStructConstructor(sit->second, e->callee, e->args);
    return emitUserFnCall(e->callee, e->args);
}

void CodeGen::emitStmt(TypeStmt &s) {
    if (struct_types_.count(s.name))
        throw std::runtime_error("redefined type: " + s.name);

    std::vector<llvm::Type*> fieldTypes;
    for (auto &f : s.fields)
        fieldTypes.push_back(resolveType(f.type));

    llvm::StructType *structTy = llvm::StructType::create(*ctx_, fieldTypes, s.name);
    struct_types_[s.name] = {structTy, s.fields};
}

llvm::Value *CodeGen::emitStructConstructor(const StructInfo &info,
                                             const std::string &name,
                                             const std::vector<ExprPtr> &args) {
    if (args.size() != info.fields.size())
        throw std::runtime_error("type '" + name + "': expected " +
                                 std::to_string(info.fields.size()) + " arguments, got " +
                                 std::to_string(args.size()));

    llvm::Value *result = llvm::UndefValue::get(info.llvmType);

    for (unsigned i = 0; i < info.fields.size(); ++i) {
        llvm::Value *val = emitExpr(*args[i]);
        llvm::Type *expectedTy = info.llvmType->getElementType(i);
        if (val->getType() != expectedTy)
            throw std::runtime_error("type '" + name + "': field '" + info.fields[i].name +
                                     "' type mismatch");
        result = builder_.CreateInsertValue(result, val, i);
    }

    return result;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<FieldAccessExpr> &e) {
    llvm::Value *obj = emitExpr(*e->object);
    llvm::Type *objTy = obj->getType();

    llvm::StructType *structTy = llvm::dyn_cast<llvm::StructType>(objTy);
    if (!structTy)
        throw std::runtime_error("field access on non-struct type");

    // Numeric index access for tuples (.0, .1, ...)
    if (!e->field.empty() && std::isdigit(static_cast<unsigned char>(e->field[0]))) {
        unsigned idx = std::stoul(e->field);
        if (idx >= structTy->getNumElements())
            throw std::runtime_error("tuple index " + e->field + " out of range");
        return builder_.CreateExtractValue(obj, idx, "tuple." + e->field);
    }

    std::string typeName = structTy->getName().str();
    auto it = struct_types_.find(typeName);
    if (it == struct_types_.end())
        throw std::runtime_error("unknown struct type: " + typeName);

    const auto &info = it->second;
    for (unsigned i = 0; i < info.fields.size(); ++i) {
        if (info.fields[i].name == e->field)
            return builder_.CreateExtractValue(obj, i, e->field);
    }

    throw std::runtime_error("type '" + typeName + "' has no field '" + e->field + "'");
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<TupleExpr> &e) {
    std::vector<llvm::Type*> types;
    std::vector<llvm::Value*> vals;
    for (auto &el : e->elements) {
        llvm::Value *v = emitExpr(*el);
        types.push_back(v->getType());
        vals.push_back(v);
    }
    llvm::StructType *tupleType = llvm::StructType::get(*ctx_, types);
    llvm::Value *result = llvm::UndefValue::get(tupleType);
    for (unsigned i = 0; i < vals.size(); ++i)
        result = builder_.CreateInsertValue(result, vals[i], i);
    return result;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<ListExpr> &e) {
    if (e->elements.empty())
        throw std::runtime_error("empty list literal requires type annotation (not yet supported)");

    // Evaluate all elements
    std::vector<llvm::Value*> vals;
    for (auto &el : e->elements)
        vals.push_back(emitExpr(*el));

    // Check all elements have the same type
    llvm::Type *elemTy = vals[0]->getType();
    for (size_t i = 1; i < vals.size(); ++i) {
        if (vals[i]->getType() != elemTy)
            throw std::runtime_error("list elements must all have the same type");
    }

    int64_t count = static_cast<int64_t>(vals.size());

    // Allocate list header: { i64 length, i64 capacity, ptr data }
    llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);

    // Allocate header
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t headerSize = dl.getTypeAllocSize(listHeaderTy_);
    llvm::Value *headerPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "list_header");

    // Allocate data
    uint64_t elemSize = dl.getTypeAllocSize(elemTy);
    llvm::Value *dataSize = llvm::ConstantInt::get(i64Ty_, elemSize * count);
    llvm::Value *dataPtr = builder_.CreateCall(mallocFn, {dataSize}, "list_data");

    // Store elements into data
    for (int64_t i = 0; i < count; ++i) {
        llvm::Value *elemPtr = builder_.CreateGEP(
            elemTy, dataPtr, {llvm::ConstantInt::get(i64Ty_, i)}, "elem_ptr");
        builder_.CreateStore(vals[i], elemPtr);
    }

    // Store length, capacity, data pointer into header
    llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 0, "len_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, count), lenPtr);

    llvm::Value *capPtr = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 1, "cap_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, count), capPtr);

    llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, headerPtr, 2, "data_ptr");
    builder_.CreateStore(dataPtr, dataPtrField);

    // Track element type
    list_element_types_[headerPtr] = elemTy;

    return headerPtr;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<MapExpr> &e) {
    if (e->keys.empty())
        throw std::runtime_error("empty map literal requires type annotation");

    // Evaluate all keys and values
    std::vector<llvm::Value*> keyVals, valVals;
    for (auto &k : e->keys) keyVals.push_back(emitExpr(*k));
    for (auto &v : e->values) valVals.push_back(emitExpr(*v));

    // Check all keys have the same type
    llvm::Type *keyTy = keyVals[0]->getType();
    for (size_t i = 1; i < keyVals.size(); ++i) {
        if (keyVals[i]->getType() != keyTy)
            throw std::runtime_error("map keys must all have the same type");
    }

    // Check all values have the same type
    llvm::Type *valTy = valVals[0]->getType();
    for (size_t i = 1; i < valVals.size(); ++i) {
        if (valVals[i]->getType() != valTy)
            throw std::runtime_error("map values must all have the same type");
    }

    int64_t count = static_cast<int64_t>(keyVals.size());

    llvm::FunctionType *mallocTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    llvm::FunctionCallee mallocFn = mod_->getOrInsertFunction("malloc", mallocTy);
    const llvm::DataLayout &dl = mod_->getDataLayout();

    // Allocate MapHeader
    uint64_t headerSize = dl.getTypeAllocSize(mapHeaderTy_);
    llvm::Value *headerPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, headerSize)}, "map_header");

    // Allocate keys array
    uint64_t keySize = dl.getTypeAllocSize(keyTy);
    llvm::Value *keysPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, keySize * count)}, "map_keys");

    // Allocate values array
    uint64_t valSize = dl.getTypeAllocSize(valTy);
    llvm::Value *valsPtr = builder_.CreateCall(
        mallocFn, {llvm::ConstantInt::get(i64Ty_, valSize * count)}, "map_vals");

    // Store keys and values
    for (int64_t i = 0; i < count; ++i) {
        llvm::Value *kp = builder_.CreateGEP(keyTy, keysPtr,
            {llvm::ConstantInt::get(i64Ty_, i)}, "key_ptr");
        builder_.CreateStore(keyVals[i], kp);
        llvm::Value *vp = builder_.CreateGEP(valTy, valsPtr,
            {llvm::ConstantInt::get(i64Ty_, i)}, "val_ptr");
        builder_.CreateStore(valVals[i], vp);
    }

    // Store header fields: length, capacity, keys_ptr, values_ptr
    llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 0, "map_len_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, count), lenPtr);

    llvm::Value *capPtr = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 1, "map_cap_ptr");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, count), capPtr);

    llvm::Value *keysPtrField = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 2, "map_keys_field");
    builder_.CreateStore(keysPtr, keysPtrField);

    llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, headerPtr, 3, "map_vals_field");
    builder_.CreateStore(valsPtr, valsPtrField);

    // Track types
    map_key_types_[headerPtr] = keyTy;
    map_value_types_[headerPtr] = valTy;

    return headerPtr;
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<IndexExpr> &e) {
    llvm::Value *objPtr = emitExpr(*e->object);
    llvm::Value *index = emitExpr(*e->index);

    if (objPtr->getType() != ptrTy_)
        throw std::runtime_error("index operator requires list or map");

    // Check if this is a map
    llvm::Type *mapKeyTy = getMapKeyType(objPtr);
    if (mapKeyTy) {
        llvm::Type *mapValTy = getMapValueType(objPtr);
        if (!mapValTy)
            throw std::runtime_error("cannot determine map value type");

        // Check key type matches
        if (index->getType() != mapKeyTy)
            throw std::runtime_error("map key type mismatch");

        // Lookup key
        llvm::Value *idx = emitMapKeyLookup(objPtr, index, mapKeyTy);

        // Check if found
        llvm::Value *notFound = builder_.CreateICmpSLT(idx, llvm::ConstantInt::get(i64Ty_, 0), "not_found");

        llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "map.notfound", fn_);
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "map.found", fn_);

        builder_.CreateCondBr(notFound, failBB, okBB);

        // Not found: print error and exit
        builder_.SetInsertPoint(failBB);
        emitRuntimeError("runtime error: map key not found\n", ".map_key_err");

        // Found: get value
        builder_.SetInsertPoint(okBB);
        llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, objPtr, 3, "map_vals_ptr");
        llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "map_vals");
        llvm::Value *valElemPtr = builder_.CreateGEP(mapValTy, valsPtr, {idx}, "val_elem_ptr");
        return builder_.CreateLoad(mapValTy, valElemPtr, "map_val");
    }

    // List index access
    llvm::Type *elemTy = getListElementType(objPtr);
    if (!elemTy)
        throw std::runtime_error("cannot determine list element type for index access");

    if (index->getType() == i1Ty_)
        index = builder_.CreateZExt(index, i64Ty_, "idx_ext");

    llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, objPtr, 0, "len_ptr");
    llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "length");

    llvm::Value *negCheck = builder_.CreateICmpSLT(index, llvm::ConstantInt::get(i64Ty_, 0), "neg_check");
    llvm::Value *overCheck = builder_.CreateICmpSGE(index, length, "over_check");
    llvm::Value *outOfBounds = builder_.CreateOr(negCheck, overCheck, "oob");

    llvm::BasicBlock *oobBB = llvm::BasicBlock::Create(*ctx_, "index.oob", fn_);
    llvm::BasicBlock *okBB2 = llvm::BasicBlock::Create(*ctx_, "index.ok", fn_);

    builder_.CreateCondBr(outOfBounds, oobBB, okBB2);

    builder_.SetInsertPoint(oobBB);
    emitRuntimeError("runtime error: list index out of range\n", ".idx_err");

    builder_.SetInsertPoint(okBB2);
    llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, objPtr, 2, "data_ptr");
    llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "data");
    llvm::Value *elemPtr = builder_.CreateGEP(elemTy, dataPtr, {index}, "elem_ptr");
    return builder_.CreateLoad(elemTy, elemPtr, "elem");
}

void CodeGen::emitPrint(const std::vector<ExprPtr> &args) {
    if (args.size() != 1)
        throw std::runtime_error("print() takes exactly 1 argument");

    llvm::FunctionType *printfTy = llvm::FunctionType::get(
        i32Ty_, {llvm::PointerType::getUnqual(*ctx_)}, /*isVarArg=*/true);
    llvm::FunctionCallee printfFn = mod_->getOrInsertFunction("printf", printfTy);

    llvm::Value *val = emitExpr(*args[0]);

    // Option type printing
    if (isOptionType(val->getType())) {
        llvm::Value *hasValue = builder_.CreateExtractValue(val, 0, "has_value");
        llvm::BasicBlock *someBB = llvm::BasicBlock::Create(*ctx_, "print.some", fn_);
        llvm::BasicBlock *noneBB = llvm::BasicBlock::Create(*ctx_, "print.none", fn_);
        llvm::BasicBlock *endBB  = llvm::BasicBlock::Create(*ctx_, "print.end", fn_);

        builder_.CreateCondBr(hasValue, someBB, noneBB);

        // None branch
        builder_.SetInsertPoint(noneBB);
        llvm::Constant *noneFmt = builder_.CreateGlobalString("None\n", ".fmt_none");
        builder_.CreateCall(printfFn, {noneFmt});
        builder_.CreateBr(endBB);

        // Some branch
        builder_.SetInsertPoint(someBB);
        llvm::Value *innerVal = builder_.CreateExtractValue(val, 1, "opt_value");
        llvm::Type *innerTy = innerVal->getType();

        llvm::Constant *somePrefix = builder_.CreateGlobalString("Some(", ".fmt_some_pre");
        builder_.CreateCall(printfFn, {somePrefix});

        emitPrintValue(innerVal, innerTy, printfFn, "_opt");

        llvm::Constant *someSuffix = builder_.CreateGlobalString(")\n", ".fmt_some_post");
        builder_.CreateCall(printfFn, {someSuffix});
        builder_.CreateBr(endBB);

        builder_.SetInsertPoint(endBB);
        return;
    }

    // Map/List printing: check if ptr type
    if (val->getType() == ptrTy_) {
        // Check if it's a map first
        llvm::Type *mapKeyTy = getMapKeyType(val);
        llvm::Type *mapValTy = getMapValueType(val);
        if (mapKeyTy && mapValTy) {
            // Print map as {key: value, key: value}
            llvm::Value *lenPtr = builder_.CreateStructGEP(mapHeaderTy_, val, 0, "map_len_ptr");
            llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "map_len");
            llvm::Value *keysPtrField = builder_.CreateStructGEP(mapHeaderTy_, val, 2, "map_keys_ptr");
            llvm::Value *keysPtr = builder_.CreateLoad(ptrTy_, keysPtrField, "map_keys");
            llvm::Value *valsPtrField = builder_.CreateStructGEP(mapHeaderTy_, val, 3, "map_vals_ptr");
            llvm::Value *valsPtr = builder_.CreateLoad(ptrTy_, valsPtrField, "map_vals");

            llvm::Constant *lbrace = builder_.CreateGlobalString("{", ".fmt_lbrace");
            llvm::Constant *rbrace = builder_.CreateGlobalString("}\n", ".fmt_rbrace");
            llvm::Constant *comma = builder_.CreateGlobalString(", ", ".fmt_comma_m");
            llvm::Constant *colon = builder_.CreateGlobalString(": ", ".fmt_colon");
            builder_.CreateCall(printfFn, {lbrace});

            // Loop through entries
            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "print_map.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "print_map.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "print_map.end", fn_);

            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "print_map_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(condBB);
            llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "i");
            llvm::Value *cond = builder_.CreateICmpSLT(iVal, length, "cond");
            builder_.CreateCondBr(cond, bodyBB, endBB);

            builder_.SetInsertPoint(bodyBB);
            llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "i_cur");

            // Print comma if not first
            llvm::Value *notFirst = builder_.CreateICmpSGT(iCur, llvm::ConstantInt::get(i64Ty_, 0), "not_first");
            llvm::BasicBlock *commaBB = llvm::BasicBlock::Create(*ctx_, "print_map.comma", fn_);
            llvm::BasicBlock *kvBB = llvm::BasicBlock::Create(*ctx_, "print_map.kv", fn_);
            builder_.CreateCondBr(notFirst, commaBB, kvBB);

            builder_.SetInsertPoint(commaBB);
            builder_.CreateCall(printfFn, {comma});
            builder_.CreateBr(kvBB);

            builder_.SetInsertPoint(kvBB);
            llvm::Value *iKV = builder_.CreateLoad(i64Ty_, iVar, "i_kv");

            // Print key
            llvm::Value *keyPtr = builder_.CreateGEP(mapKeyTy, keysPtr, {iKV}, "key_ptr");
            llvm::Value *keyVal = builder_.CreateLoad(mapKeyTy, keyPtr, "key_val");
            emitPrintValue(keyVal, mapKeyTy, printfFn, "_mk");

            builder_.CreateCall(printfFn, {colon});

            // Print value
            llvm::Value *valPtr = builder_.CreateGEP(mapValTy, valsPtr, {iKV}, "val_ptr");
            llvm::Value *valVal = builder_.CreateLoad(mapValTy, valPtr, "val_val");
            emitPrintValue(valVal, mapValTy, printfFn, "_mv");

            // i++
            llvm::Value *iNext = builder_.CreateAdd(iKV, llvm::ConstantInt::get(i64Ty_, 1), "i_next");
            builder_.CreateStore(iNext, iVar);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(endBB);
            builder_.CreateCall(printfFn, {rbrace});
            return;
        }

        // Check if it's a list - try to find element type
        llvm::Type *elemTy = getListElementType(val);
        if (elemTy) {
            // Print list as [elem, elem, ...]
            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, val, 0, "len_ptr");
            llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "length");
            llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, val, 2, "data_ptr");
            llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "data");

            llvm::Constant *lbracket = builder_.CreateGlobalString("[", ".fmt_lb");
            llvm::Constant *rbracketNl = builder_.CreateGlobalString("]\n", ".fmt_rb");
            llvm::Constant *comma = builder_.CreateGlobalString(", ", ".fmt_comma");
            builder_.CreateCall(printfFn, {lbracket});

            // Loop through elements
            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "print_list.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "print_list.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "print_list.end", fn_);

            // i = 0
            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "print_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);
            builder_.CreateBr(condBB);

            // cond: i < length
            builder_.SetInsertPoint(condBB);
            llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "i");
            llvm::Value *cond = builder_.CreateICmpSLT(iVal, length, "cond");
            builder_.CreateCondBr(cond, bodyBB, endBB);

            // body: print comma if i > 0, then print element
            builder_.SetInsertPoint(bodyBB);
            llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "i_cur");
            llvm::Value *notFirst = builder_.CreateICmpSGT(iCur, llvm::ConstantInt::get(i64Ty_, 0), "not_first");

            llvm::BasicBlock *commaBB = llvm::BasicBlock::Create(*ctx_, "print_list.comma", fn_);
            llvm::BasicBlock *elemBB = llvm::BasicBlock::Create(*ctx_, "print_list.elem", fn_);
            builder_.CreateCondBr(notFirst, commaBB, elemBB);

            builder_.SetInsertPoint(commaBB);
            builder_.CreateCall(printfFn, {comma});
            builder_.CreateBr(elemBB);

            builder_.SetInsertPoint(elemBB);
            llvm::Value *iElem = builder_.CreateLoad(i64Ty_, iVar, "i_elem");
            llvm::Value *elemPtr = builder_.CreateGEP(elemTy, dataPtr, {iElem}, "elem_ptr");
            llvm::Value *elem = builder_.CreateLoad(elemTy, elemPtr, "elem");

            emitPrintValue(elem, elemTy, printfFn, "_l");

            // i = i + 1
            llvm::Value *iNext = builder_.CreateAdd(iElem, llvm::ConstantInt::get(i64Ty_, 1), "i_next");
            builder_.CreateStore(iNext, iVar);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(endBB);
            builder_.CreateCall(printfFn, {rbracketNl});
            return;
        }
    }

    if (llvm::isa<llvm::StructType>(val->getType()))
        throw std::runtime_error("print() does not support struct types");

    if (val->getType() == i1Ty_) {
        llvm::Constant *trueStr  = builder_.CreateGlobalString("true\n",  ".fmt_true");
        llvm::Constant *falseStr = builder_.CreateGlobalString("false\n", ".fmt_false");
        llvm::Value *fmtPtr = builder_.CreateSelect(val, trueStr, falseStr, "bool_fmt");
        builder_.CreateCall(printfFn, {fmtPtr});
        return;
    }

    if (val->getType()->isPointerTy()) {
        llvm::Constant *fmt = builder_.CreateGlobalString("%s\n", ".fmt_s");
        builder_.CreateCall(printfFn, {fmt, val});
        return;
    }

    llvm::Constant *fmt;
    if (val->getType()->isDoubleTy())
        fmt = builder_.CreateGlobalString("%g\n", ".fmt_f");
    else
        fmt = builder_.CreateGlobalString("%ld\n", ".fmt_i");

    builder_.CreateCall(printfFn, {fmt, val});
}
