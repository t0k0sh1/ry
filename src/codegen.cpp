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

    // Register built-in functions
    builtins_["print"] = [this](const std::vector<ExprPtr> &args) { emitPrint(args); };
}

llvm::orc::ThreadSafeModule CodeGen::compile(Program &prog) {
    // Create entry function: i32 @__ry_main__()
    llvm::FunctionType *ft = llvm::FunctionType::get(i32Ty_, false);
    fn_ = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "__ry_main__", *mod_);
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(*ctx_, "entry", fn_);
    builder_.SetInsertPoint(bb);

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
    auto it = vars_.find(name);
    if (it != vars_.end()) {
        // emitStmt で型一致を保証済み
        return it->second;
    }
    // 初回: エントリブロック先頭に alloca を作成
    llvm::IRBuilder<> entryBuilder(&fn_->getEntryBlock(),
                                    fn_->getEntryBlock().begin());
    llvm::AllocaInst *alloca = entryBuilder.CreateAlloca(ty, nullptr, name);
    vars_[name] = alloca;
    return alloca;
}

void CodeGen::emitStmt(LetStmt &s) {
    if (vars_.count(s.name))
        throw std::runtime_error("redeclared variable: " + s.name);

    llvm::Value *val = emitExpr(*s.value);
    llvm::Type *newTy = val->getType();

    if (s.type_annotation) {
        llvm::Type *annotTy = nullptr;
        if (*s.type_annotation == "int")   annotTy = i64Ty_;
        else if (*s.type_annotation == "float") annotTy = f64Ty_;
        else if (*s.type_annotation == "bool")  annotTy = i1Ty_;
        if (annotTy && annotTy != newTy)
            throw std::runtime_error(
                "type error: annotation '" + *s.type_annotation +
                "' does not match expression type for variable '" + s.name + "'");
    }

    llvm::AllocaInst *ptr = getOrCreateVar(s.name, newTy);
    builder_.CreateStore(val, ptr);
}

void CodeGen::emitStmt(ConstStmt &s) {
    if (vars_.count(s.name))
        throw std::runtime_error("redeclared variable: " + s.name);

    llvm::Value *val = emitExpr(*s.value);
    llvm::Type *newTy = val->getType();

    if (s.type_annotation) {
        llvm::Type *annotTy = nullptr;
        if (*s.type_annotation == "int")   annotTy = i64Ty_;
        else if (*s.type_annotation == "float") annotTy = f64Ty_;
        else if (*s.type_annotation == "bool")  annotTy = i1Ty_;
        if (annotTy && annotTy != newTy)
            throw std::runtime_error(
                "type error: annotation '" + *s.type_annotation +
                "' does not match expression type for variable '" + s.name + "'");
    }

    llvm::AllocaInst *ptr = getOrCreateVar(s.name, newTy);
    builder_.CreateStore(val, ptr);
    const_vars_.insert(s.name);
}

void CodeGen::emitStmt(AssignStmt &s) {
    auto it = vars_.find(s.name);
    if (it == vars_.end())
        throw std::runtime_error("undeclared variable: " + s.name);

    if (const_vars_.count(s.name))
        throw std::runtime_error("cannot reassign const variable: " + s.name);

    llvm::Value *val = emitExpr(*s.value);
    llvm::Type *newTy = val->getType();

    if (it->second->getAllocatedType() != newTy)
        throw std::runtime_error(
            "type error: variable '" + s.name +
            "' cannot be reassigned to a different type");

    builder_.CreateStore(val, it->second);
}

void CodeGen::emitStmt(CallStmt &s) {
    auto it = builtins_.find(s.callee);
    if (it != builtins_.end()) {
        it->second(s.args);
        return;
    }
    auto fit = functions_.find(s.callee);
    if (fit == functions_.end())
        throw std::runtime_error("unknown function: " + s.callee);
    llvm::Function *callee = fit->second;
    if (s.args.size() != callee->arg_size())
        throw std::runtime_error("function '" + s.callee + "': expected " +
                                 std::to_string(callee->arg_size()) + " arguments, got " +
                                 std::to_string(s.args.size()));
    std::vector<llvm::Value*> argVals;
    unsigned idx = 0;
    for (auto &arg : s.args) {
        llvm::Value *v = emitExpr(*arg);
        llvm::Type *expected = callee->getFunctionType()->getParamType(idx);
        if (v->getType() != expected)
            throw std::runtime_error("function '" + s.callee + "': argument " +
                                     std::to_string(idx + 1) + " type mismatch");
        argVals.push_back(v);
        ++idx;
    }
    builder_.CreateCall(callee, argVals);
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
    for (auto &stmt : s->body)
        std::visit([this](auto &st) { emitStmt(st); }, stmt);
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
        for (auto &stmt : branch.body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);
        if (!builder_.GetInsertBlock()->getTerminator())
            builder_.CreateBr(mergeBB);

        builder_.SetInsertPoint(elseBB);
    }

    // else body
    if (!s->else_body.empty()) {
        for (auto &stmt : s->else_body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);
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

llvm::Value *CodeGen::emitExprVariant(const VariableExpr &e) {
    auto it = vars_.find(e.name);
    if (it == vars_.end())
        throw std::runtime_error("undefined variable: " + e.name);
    llvm::AllocaInst *alloca = it->second;
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
        if (val->getType() == i1Ty_)
            val = builder_.CreateZExt(val, i64Ty_, "boolext");
        return builder_.CreateNeg(val, "neg");
    }
    if (e->op == "not") {
        auto toBool = [this](llvm::Value *v) -> llvm::Value* {
            if (v->getType() == i1Ty_)
                return v;
            if (v->getType()->isDoubleTy())
                return builder_.CreateFCmpONE(
                    v, llvm::ConstantFP::get(f64Ty_, 0.0), "ftobool");
            return builder_.CreateICmpNE(
                v, llvm::ConstantInt::get(v->getType(), 0), "itobool");
        };
        llvm::Value *boolVal = toBool(val);
        return builder_.CreateNot(boolVal, "not"); // LLVM: xor i1 %v, true
    }
    if (e->op == "~") {
        if (val->getType()->isDoubleTy())
            throw std::runtime_error("bitwise NOT (~) requires integer, got float");
        if (val->getType() == i1Ty_)
            val = builder_.CreateZExt(val, i64Ty_, "boolext");
        return builder_.CreateNot(val, "bnot"); // xor %v, -1
    }
    throw std::runtime_error("unknown unary operator: " + e->op);
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<BinaryExpr> &e) {
    llvm::Value *lhs = emitExpr(*e->lhs);
    llvm::Value *rhs = emitExpr(*e->rhs);
    const std::string &op = e->op;

    // ===== 比較演算子 =====
    if (op == "==" || op == "!=" || op == "<" ||
        op == "<=" || op == ">"  || op == ">=") {

        // i1（bool）は先に i64 に ZExt
        if (lhs->getType() == i1Ty_) lhs = builder_.CreateZExt(lhs, i64Ty_, "lhs_i");
        if (rhs->getType() == i1Ty_) rhs = builder_.CreateZExt(rhs, i64Ty_, "rhs_i");

        bool lf = lhs->getType()->isDoubleTy();
        bool rf = rhs->getType()->isDoubleTy();
        if (lf || rf) {
            if (!lf) lhs = builder_.CreateSIToFP(lhs, f64Ty_, "cmp_lf");
            if (!rf) rhs = builder_.CreateSIToFP(rhs, f64Ty_, "cmp_rf");
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
    // ===== 比較演算子ここまで =====

    // ===== 論理演算子 =====
    if (op == "and" || op == "or") {
        auto toBool = [this](llvm::Value *v) -> llvm::Value* {
            if (v->getType() == i1Ty_)
                return v;
            if (v->getType()->isDoubleTy())
                return builder_.CreateFCmpONE(
                    v, llvm::ConstantFP::get(f64Ty_, 0.0), "ftobool");
            return builder_.CreateICmpNE(
                v, llvm::ConstantInt::get(v->getType(), 0), "itobool");
        };
        llvm::Value *lhsBool = toBool(lhs);
        llvm::Value *rhsBool = toBool(rhs);
        if (op == "and")
            return builder_.CreateAnd(lhsBool, rhsBool, "and");
        else
            return builder_.CreateOr(lhsBool, rhsBool, "or");
    }
    // ===== 論理演算子ここまで =====

    // ===== ビット演算子 =====
    if (op == "&" || op == "|" || op == "^" ||
        op == "<<" || op == ">>") {
        if (lhs->getType()->isDoubleTy() || rhs->getType()->isDoubleTy())
            throw std::runtime_error(
                "bitwise operator '" + op + "' requires integer operands, got float");
        if (lhs->getType() == i1Ty_) lhs = builder_.CreateZExt(lhs, i64Ty_, "lhs_i");
        if (rhs->getType() == i1Ty_) rhs = builder_.CreateZExt(rhs, i64Ty_, "rhs_i");
        if (op == "&")  return builder_.CreateAnd(lhs, rhs,  "band");
        if (op == "|")  return builder_.CreateOr(lhs,  rhs,  "bor");
        if (op == "^")  return builder_.CreateXor(lhs, rhs,  "bxor");
        if (op == "<<") return builder_.CreateShl(lhs,  rhs, "shl");
        if (op == ">>") return builder_.CreateAShr(lhs, rhs, "ashr"); // 算術右シフト
    }
    // ===== ビット演算子ここまで =====

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
        if (lhs->getType() == i1Ty_) lhs = builder_.CreateZExt(lhs, i64Ty_, "lhs_i");
        if (rhs->getType() == i1Ty_) rhs = builder_.CreateZExt(rhs, i64Ty_, "rhs_i");
        if (lhs->getType()->isDoubleTy()) lhs = builder_.CreateFPToSI(lhs, i64Ty_, "lhs_i");
        if (rhs->getType()->isDoubleTy()) rhs = builder_.CreateFPToSI(rhs, i64Ty_, "rhs_i");
        return builder_.CreateSDiv(lhs, rhs, "idiv");
    }

    // / 除算: 常にf64
    if (op == "/") {
        if (lhs->getType()->isIntegerTy()) lhs = builder_.CreateSIToFP(lhs, f64Ty_, "lhs_f");
        if (rhs->getType()->isIntegerTy()) rhs = builder_.CreateSIToFP(rhs, f64Ty_, "rhs_f");
        return builder_.CreateFDiv(lhs, rhs, "div");
    }

    // % 剰余: 片方f64ならfrem、両方i64ならsrem
    if (op == "%") {
        if (lhs->getType() == i1Ty_) lhs = builder_.CreateZExt(lhs, i64Ty_, "lhs_i");
        if (rhs->getType() == i1Ty_) rhs = builder_.CreateZExt(rhs, i64Ty_, "rhs_i");
        bool lf = lhs->getType()->isDoubleTy();
        bool rf = rhs->getType()->isDoubleTy();
        if (lf || rf) {
            if (!lf) lhs = builder_.CreateSIToFP(lhs, f64Ty_, "lhs_f");
            if (!rf) rhs = builder_.CreateSIToFP(rhs, f64Ty_, "rhs_f");
            return builder_.CreateFRem(lhs, rhs, "frem");
        }
        return builder_.CreateSRem(lhs, rhs, "srem");
    }

    // +/-/*: 片方f64なら浮動小数点命令
    if (lhs->getType() == i1Ty_) lhs = builder_.CreateZExt(lhs, i64Ty_, "lhs_i");
    if (rhs->getType() == i1Ty_) rhs = builder_.CreateZExt(rhs, i64Ty_, "rhs_i");
    bool lf = lhs->getType()->isDoubleTy();
    bool rf = rhs->getType()->isDoubleTy();
    if (lf || rf) {
        if (!lf) lhs = builder_.CreateSIToFP(lhs, f64Ty_, "lhs_f");
        if (!rf) rhs = builder_.CreateSIToFP(rhs, f64Ty_, "rhs_f");
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

llvm::Type *CodeGen::resolveType(const std::string &typeName) {
    if (typeName == "int")   return i64Ty_;
    if (typeName == "float") return f64Ty_;
    if (typeName == "bool")  return i1Ty_;
    throw std::runtime_error("unknown type: " + typeName);
}

void CodeGen::emitStmt(ReturnStmt &s) {
    llvm::Value *val = emitExpr(*s.value);
    llvm::Type *retTy = fn_->getReturnType();
    if (val->getType() != retTy)
        throw std::runtime_error("return type mismatch");
    builder_.CreateRet(val);
    // Create dead block for code after return
    llvm::BasicBlock *deadBB = llvm::BasicBlock::Create(*ctx_, "dead", fn_);
    builder_.SetInsertPoint(deadBB);
}

void CodeGen::emitStmt(std::unique_ptr<FnStmt> &s) {
    // Resolve parameter types and return type
    std::vector<llvm::Type*> paramTypes;
    for (auto &p : s->params)
        paramTypes.push_back(resolveType(p.type));
    llvm::Type *retTy = resolveType(s->return_type);

    llvm::FunctionType *ft = llvm::FunctionType::get(retTy, paramTypes, false);
    llvm::Function *func = llvm::Function::Create(
        ft, llvm::Function::ExternalLinkage, s->name, *mod_);

    // Register before emitting body (enables recursion)
    functions_[s->name] = func;

    // Save current context
    llvm::Function *savedFn = fn_;
    auto savedVars = std::move(vars_);
    auto savedConstVars = std::move(const_vars_);
    llvm::BasicBlock *savedBlock = builder_.GetInsertBlock();
    llvm::BasicBlock::iterator savedPoint = builder_.GetInsertPoint();

    // Set up new function
    fn_ = func;
    vars_.clear();
    const_vars_.clear();

    llvm::BasicBlock *entry = llvm::BasicBlock::Create(*ctx_, "entry", func);
    builder_.SetInsertPoint(entry);

    // Allocate and store parameters
    unsigned idx = 0;
    for (auto &arg : func->args()) {
        arg.setName(s->params[idx].name);
        llvm::AllocaInst *alloca = builder_.CreateAlloca(
            paramTypes[idx], nullptr, s->params[idx].name);
        builder_.CreateStore(&arg, alloca);
        vars_[s->params[idx].name] = alloca;
        ++idx;
    }

    // Emit body
    for (auto &stmt : s->body)
        std::visit([this](auto &st) { emitStmt(st); }, stmt);

    // If no terminator, add default return
    if (!builder_.GetInsertBlock()->getTerminator()) {
        if (retTy == i64Ty_)
            builder_.CreateRet(llvm::ConstantInt::get(i64Ty_, 0));
        else if (retTy == f64Ty_)
            builder_.CreateRet(llvm::ConstantFP::get(f64Ty_, 0.0));
        else if (retTy == i1Ty_)
            builder_.CreateRet(llvm::ConstantInt::get(i1Ty_, 0));
    }

    // Verify function
    std::string err;
    llvm::raw_string_ostream errStream(err);
    if (llvm::verifyFunction(*func, &errStream))
        throw std::runtime_error("IR verify error in function '" + s->name + "': " + err);

    // Restore context
    fn_ = savedFn;
    vars_ = std::move(savedVars);
    const_vars_ = std::move(savedConstVars);
    builder_.SetInsertPoint(savedBlock, savedPoint);
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<CallExpr> &e) {
    auto fit = functions_.find(e->callee);
    if (fit == functions_.end())
        throw std::runtime_error("undefined function: " + e->callee);
    llvm::Function *callee = fit->second;
    if (e->args.size() != callee->arg_size())
        throw std::runtime_error("function '" + e->callee + "': expected " +
                                 std::to_string(callee->arg_size()) + " arguments, got " +
                                 std::to_string(e->args.size()));
    std::vector<llvm::Value*> argVals;
    unsigned idx = 0;
    for (auto &arg : e->args) {
        llvm::Value *v = emitExpr(*arg);
        llvm::Type *expected = callee->getFunctionType()->getParamType(idx);
        if (v->getType() != expected)
            throw std::runtime_error("function '" + e->callee + "': argument " +
                                     std::to_string(idx + 1) + " type mismatch");
        argVals.push_back(v);
        ++idx;
    }
    return builder_.CreateCall(callee, argVals, "calltmp");
}

void CodeGen::emitPrint(const std::vector<ExprPtr> &args) {
    if (args.size() != 1)
        throw std::runtime_error("print() takes exactly 1 argument");

    // Declare printf
    llvm::FunctionType *printfTy = llvm::FunctionType::get(
        i32Ty_, {llvm::PointerType::getUnqual(*ctx_)}, /*isVarArg=*/true);
    llvm::FunctionCallee printfFn = mod_->getOrInsertFunction("printf", printfTy);

    llvm::Value *val = emitExpr(*args[0]);

    // Bool 出力
    if (val->getType() == i1Ty_) {
        llvm::Constant *trueStr  = builder_.CreateGlobalString("true\n",  ".fmt_true");
        llvm::Constant *falseStr = builder_.CreateGlobalString("false\n", ".fmt_false");
        llvm::Value *fmtPtr = builder_.CreateSelect(val, trueStr, falseStr, "bool_fmt");
        builder_.CreateCall(printfFn, {fmtPtr});
        return;
    }

    llvm::Constant *fmt;
    if (val->getType()->isDoubleTy())
        fmt = builder_.CreateGlobalString("%g\n", ".fmt_f");
    else
        fmt = builder_.CreateGlobalString("%ld\n", ".fmt_i");

    builder_.CreateCall(printfFn, {fmt, val});
}
