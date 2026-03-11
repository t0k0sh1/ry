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
        llvm::Value *val = llvm::UndefValue::get(annotTy);
        val = builder_.CreateInsertValue(val, llvm::ConstantInt::get(i1Ty_, 0), 0);
        val = builder_.CreateInsertValue(val, llvm::UndefValue::get(
            llvm::cast<llvm::StructType>(annotTy)->getElementType(1)), 1);
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

    // Track list element type if this is a list value
    if (newTy == ptrTy_) {
        llvm::Type *elemTy = nullptr;
        // Direct mapping (from ListExpr)
        auto it = list_element_types_.find(val);
        if (it != list_element_types_.end()) {
            elemTy = it->second;
        }
        // From variable load (from another list variable)
        if (!elemTy) {
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
                auto it2 = list_element_types_.find(load->getPointerOperand());
                if (it2 != list_element_types_.end())
                    elemTy = it2->second;
            }
        }
        // From type annotation
        if (!elemTy && type_annotation && type_annotation->size() > 5 &&
            type_annotation->substr(0, 5) == "list[") {
            std::string inner = type_annotation->substr(5, type_annotation->size() - 6);
            elemTy = resolveType(inner);
        }
        if (elemTy)
            list_element_types_[ptr] = elemTy;
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
        llvm::Value *val = llvm::UndefValue::get(varTy);
        val = builder_.CreateInsertValue(val, llvm::ConstantInt::get(i1Ty_, 0), 0);
        val = builder_.CreateInsertValue(val, llvm::UndefValue::get(
            llvm::cast<llvm::StructType>(varTy)->getElementType(1)), 1);
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

llvm::Value *CodeGen::emitUserFnCall(const std::string &callee, const std::vector<ExprPtr> &args) {
    auto fit = functions_.find(callee);
    if (fit == functions_.end())
        throw std::runtime_error("undefined function: " + callee);
    llvm::Function *fn = fit->second;
    if (args.size() != fn->arg_size())
        throw std::runtime_error("function '" + callee + "': expected " +
                                 std::to_string(fn->arg_size()) + " arguments, got " +
                                 std::to_string(args.size()));
    std::vector<llvm::Value*> argVals;
    unsigned idx = 0;
    for (auto &arg : args) {
        // Handle None literal as function argument
        if (auto *ve = std::get_if<VariableExpr>(&arg->data); ve && ve->name == "None") {
            llvm::Type *expected = fn->getFunctionType()->getParamType(idx);
            if (!isOptionType(expected))
                throw std::runtime_error("function '" + callee + "': argument " +
                                         std::to_string(idx + 1) + " None requires Option type");
            llvm::Value *val = llvm::UndefValue::get(expected);
            val = builder_.CreateInsertValue(val, llvm::ConstantInt::get(i1Ty_, 0), 0);
            val = builder_.CreateInsertValue(val, llvm::UndefValue::get(
                llvm::cast<llvm::StructType>(expected)->getElementType(1)), 1);
            argVals.push_back(val);
            ++idx;
            continue;
        }
        llvm::Value *v = emitExpr(*arg);
        llvm::Type *expected = fn->getFunctionType()->getParamType(idx);
        if (v->getType() != expected)
            throw std::runtime_error("function '" + callee + "': argument " +
                                     std::to_string(idx + 1) + " type mismatch");
        argVals.push_back(v);
        ++idx;
    }
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

// ===== B2: BinaryExpr sub-dispatchers =====

llvm::Value *CodeGen::emitComparisonOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs) {
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

llvm::Type *CodeGen::getListElementType(llvm::Value *listAlloca) {
    auto it = list_element_types_.find(listAlloca);
    if (it != list_element_types_.end())
        return it->second;
    return nullptr;
}

void CodeGen::emitStmt(ImportStmt &s) {
    throw std::runtime_error("unresolved import: " + s.module_path +
                             " (ModuleLoader should have resolved this)");
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

    llvm::FunctionType *ft = llvm::FunctionType::get(retTy, paramTypes, false);
    llvm::Function *func = llvm::Function::Create(
        ft, llvm::Function::ExternalLinkage, s->name, *mod_);

    functions_[s->name] = func;

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
    // len(xs) → list length
    if (e->callee == "len") {
        if (e->args.size() != 1)
            throw std::runtime_error("len() takes exactly 1 argument");
        // We need the alloca to look up element type; evaluate and get list ptr
        llvm::Value *listPtr = emitExpr(*e->args[0]);
        if (listPtr->getType() != ptrTy_)
            throw std::runtime_error("len() requires list argument");
        // Load length field from list header (field 0)
        llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, listPtr, 0, "len_ptr");
        return builder_.CreateLoad(i64Ty_, lenPtr, "len");
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
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "unwrap.merge", fn_);

        builder_.CreateCondBr(hasValue, okBB, failBB);

        // fail: print error and exit
        builder_.SetInsertPoint(failBB);
        llvm::FunctionType *printfTy = llvm::FunctionType::get(
            i32Ty_, {llvm::PointerType::getUnqual(*ctx_)}, true);
        llvm::FunctionCallee printfFn = mod_->getOrInsertFunction("printf", printfTy);
        llvm::Constant *errMsg = builder_.CreateGlobalString(
            "runtime error: unwrap() called on None\n", ".unwrap_err");
        builder_.CreateCall(printfFn, {errMsg});

        llvm::FunctionType *exitTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {i32Ty_}, false);
        llvm::FunctionCallee exitFn = mod_->getOrInsertFunction("exit", exitTy);
        builder_.CreateCall(exitFn, {llvm::ConstantInt::get(i32Ty_, 1)});
        builder_.CreateUnreachable();

        // ok: extract value
        builder_.SetInsertPoint(okBB);
        llvm::Value *val = builder_.CreateExtractValue(opt, 1, "unwrap_val");
        builder_.CreateBr(mergeBB);

        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = builder_.CreatePHI(val->getType(), 1, "unwrap_result");
        phi->addIncoming(val, okBB);
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

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<IndexExpr> &e) {
    llvm::Value *listPtr = emitExpr(*e->object);
    llvm::Value *index = emitExpr(*e->index);

    if (listPtr->getType() != ptrTy_)
        throw std::runtime_error("index operator requires list");

    // Find element type - check if the object is a variable load, trace back to alloca
    llvm::Type *elemTy = nullptr;

    // Check direct list_element_types_ mapping (for literals)
    auto it = list_element_types_.find(listPtr);
    if (it != list_element_types_.end()) {
        elemTy = it->second;
    }

    // If not found, check the underlying alloca (for variables)
    if (!elemTy) {
        if (auto *load = llvm::dyn_cast<llvm::LoadInst>(listPtr)) {
            auto it2 = list_element_types_.find(load->getPointerOperand());
            if (it2 != list_element_types_.end())
                elemTy = it2->second;
        }
    }

    if (!elemTy)
        throw std::runtime_error("cannot determine list element type for index access");

    // Convert bool index to i64
    if (index->getType() == i1Ty_)
        index = builder_.CreateZExt(index, i64Ty_, "idx_ext");

    // Runtime bounds check
    llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, listPtr, 0, "len_ptr");
    llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "length");

    // Check index < 0 || index >= length
    llvm::Value *negCheck = builder_.CreateICmpSLT(index, llvm::ConstantInt::get(i64Ty_, 0), "neg_check");
    llvm::Value *overCheck = builder_.CreateICmpSGE(index, length, "over_check");
    llvm::Value *outOfBounds = builder_.CreateOr(negCheck, overCheck, "oob");

    llvm::BasicBlock *oobBB = llvm::BasicBlock::Create(*ctx_, "index.oob", fn_);
    llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "index.ok", fn_);

    builder_.CreateCondBr(outOfBounds, oobBB, okBB);

    // Out of bounds: print error and exit
    builder_.SetInsertPoint(oobBB);
    llvm::FunctionType *printfTy = llvm::FunctionType::get(
        i32Ty_, {ptrTy_}, true);
    llvm::FunctionCallee printfFn = mod_->getOrInsertFunction("printf", printfTy);
    llvm::Constant *errMsg = builder_.CreateGlobalString(
        "runtime error: list index out of range\n", ".idx_err");
    builder_.CreateCall(printfFn, {errMsg});

    llvm::FunctionType *exitTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {i32Ty_}, false);
    llvm::FunctionCallee exitFn = mod_->getOrInsertFunction("exit", exitTy);
    builder_.CreateCall(exitFn, {llvm::ConstantInt::get(i32Ty_, 1)});
    builder_.CreateUnreachable();

    // OK: access element
    builder_.SetInsertPoint(okBB);
    llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, listPtr, 2, "data_ptr");
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

        if (innerTy == i1Ty_) {
            llvm::Constant *trueStr  = builder_.CreateGlobalString("true", ".fmt_true_opt");
            llvm::Constant *falseStr = builder_.CreateGlobalString("false", ".fmt_false_opt");
            llvm::Value *fmtPtr = builder_.CreateSelect(innerVal, trueStr, falseStr, "bool_fmt");
            builder_.CreateCall(printfFn, {fmtPtr});
        } else if (innerTy->isPointerTy()) {
            llvm::Constant *fmt = builder_.CreateGlobalString("%s", ".fmt_s_opt");
            builder_.CreateCall(printfFn, {fmt, innerVal});
        } else if (innerTy->isDoubleTy()) {
            llvm::Constant *fmt = builder_.CreateGlobalString("%g", ".fmt_f_opt");
            builder_.CreateCall(printfFn, {fmt, innerVal});
        } else {
            llvm::Constant *fmt = builder_.CreateGlobalString("%ld", ".fmt_i_opt");
            builder_.CreateCall(printfFn, {fmt, innerVal});
        }

        llvm::Constant *someSuffix = builder_.CreateGlobalString(")\n", ".fmt_some_post");
        builder_.CreateCall(printfFn, {someSuffix});
        builder_.CreateBr(endBB);

        builder_.SetInsertPoint(endBB);
        return;
    }

    // List printing: check if ptr type and in list_element_types_
    if (val->getType() == ptrTy_) {
        // Check if it's a list - try to find element type
        llvm::Type *elemTy = nullptr;
        auto it = list_element_types_.find(val);
        if (it != list_element_types_.end()) {
            elemTy = it->second;
        }
        if (!elemTy) {
            if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
                auto it2 = list_element_types_.find(load->getPointerOperand());
                if (it2 != list_element_types_.end())
                    elemTy = it2->second;
            }
        }
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

            if (elemTy == i1Ty_) {
                llvm::Constant *trueStr = builder_.CreateGlobalString("true", ".fmt_true_l");
                llvm::Constant *falseStr = builder_.CreateGlobalString("false", ".fmt_false_l");
                llvm::Value *fmtPtr = builder_.CreateSelect(elem, trueStr, falseStr, "bool_fmt");
                builder_.CreateCall(printfFn, {fmtPtr});
            } else if (elemTy->isPointerTy()) {
                llvm::Constant *fmt = builder_.CreateGlobalString("%s", ".fmt_s_l");
                builder_.CreateCall(printfFn, {fmt, elem});
            } else if (elemTy->isDoubleTy()) {
                llvm::Constant *fmt = builder_.CreateGlobalString("%g", ".fmt_f_l");
                builder_.CreateCall(printfFn, {fmt, elem});
            } else {
                llvm::Constant *fmt = builder_.CreateGlobalString("%ld", ".fmt_i_l");
                builder_.CreateCall(printfFn, {fmt, elem});
            }

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
