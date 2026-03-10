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
    if (typeName == "string") return ptrTy_;
    auto it = struct_types_.find(typeName);
    if (it != struct_types_.end()) return it->second.llvmType;
    throw std::runtime_error("unknown type: " + typeName);
}

void CodeGen::emitStmt(ImportStmt &s) {
    throw std::runtime_error("unresolved import: " + s.module_path +
                             " (ModuleLoader should have resolved this)");
}

void CodeGen::emitStmt(ReturnStmt &s) {
    llvm::Value *val = emitExpr(*s.value);
    llvm::Type *retTy = fn_->getReturnType();
    if (val->getType() != retTy)
        throw std::runtime_error("return type mismatch");
    builder_.CreateRet(val);
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
            ++idx;
        }

        for (auto &stmt : s->body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);

        if (!builder_.GetInsertBlock()->getTerminator()) {
            if (retTy == i64Ty_)
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

void CodeGen::emitPrint(const std::vector<ExprPtr> &args) {
    if (args.size() != 1)
        throw std::runtime_error("print() takes exactly 1 argument");

    llvm::FunctionType *printfTy = llvm::FunctionType::get(
        i32Ty_, {llvm::PointerType::getUnqual(*ctx_)}, /*isVarArg=*/true);
    llvm::FunctionCallee printfFn = mod_->getOrInsertFunction("printf", printfTy);

    llvm::Value *val = emitExpr(*args[0]);

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
