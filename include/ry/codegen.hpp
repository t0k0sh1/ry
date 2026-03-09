#pragma once

#include "ry/ast.hpp"
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include <functional>
#include <memory>
#include <string>
#include <unordered_map>

class CodeGen {
public:
    CodeGen();
    llvm::orc::ThreadSafeModule compile(Program &prog);

private:
    std::unique_ptr<llvm::LLVMContext> ctx_;
    std::unique_ptr<llvm::Module> mod_;
    llvm::IRBuilder<> builder_;
    llvm::Function *fn_ = nullptr;
    llvm::Type *i64Ty_, *i32Ty_, *f64Ty_, *i1Ty_;
    std::unordered_map<std::string, llvm::AllocaInst*> vars_;
    using BuiltinFn = std::function<void(const std::vector<ExprPtr>&)>;
    std::unordered_map<std::string, BuiltinFn> builtins_;

    llvm::AllocaInst *getOrCreateVar(const std::string &name, llvm::Type *ty);
    void emitStmt(AssignStmt &s);
    void emitStmt(CallStmt &s);
    llvm::Value *emitExpr(const ExprNode &node);
    llvm::Value *emitExprVariant(const NumberExpr &e);
    llvm::Value *emitExprVariant(const FloatExpr &e);
    llvm::Value *emitExprVariant(const BoolExpr &e);
    llvm::Value *emitExprVariant(const VariableExpr &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<UnaryExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<BinaryExpr> &e);
    void emitPrint(const std::vector<ExprPtr> &args);
};
