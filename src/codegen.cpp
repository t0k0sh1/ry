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

void CodeGen::emitStmt(AssignStmt &s) {
    llvm::Value *val = emitExpr(*s.value);
    llvm::Type *newTy = val->getType();

    // 型変更再代入を禁止
    auto it = vars_.find(s.name);
    if (it != vars_.end() && it->second->getAllocatedType() != newTy) {
        throw std::runtime_error(
            "type error: variable '" + s.name +
            "' cannot be reassigned to a different type");
    }

    llvm::AllocaInst *ptr = getOrCreateVar(s.name, newTy);
    builder_.CreateStore(val, ptr);
}

void CodeGen::emitStmt(CallStmt &s) {
    auto it = builtins_.find(s.callee);
    if (it == builtins_.end())
        throw std::runtime_error("unknown function: " + s.callee);
    it->second(s.args);
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

llvm::Value *CodeGen::emitExprVariant(const VariableExpr &e) {
    if (e.name == "true")  return llvm::ConstantInt::get(i1Ty_, 1, false);
    if (e.name == "false") return llvm::ConstantInt::get(i1Ty_, 0, false);
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
