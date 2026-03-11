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
#include <unordered_set>

class CodeGen {
public:
    CodeGen();
    llvm::orc::ThreadSafeModule compile(Program &prog);

private:
    std::unique_ptr<llvm::LLVMContext> ctx_;
    std::unique_ptr<llvm::Module> mod_;
    llvm::IRBuilder<> builder_;
    llvm::Function *fn_ = nullptr;
    llvm::Type *i64Ty_, *i32Ty_, *f64Ty_, *i1Ty_, *ptrTy_;
    llvm::StructType *listHeaderTy_;
    llvm::StructType *mapHeaderTy_;
    std::vector<std::unordered_map<std::string, llvm::AllocaInst*>> scope_stack_;
    std::vector<std::unordered_set<std::string>> const_scope_stack_;
    struct OverloadEntry {
        llvm::Function *func;
        std::vector<llvm::Type*> paramTypes;
    };
    std::unordered_map<std::string, std::vector<OverloadEntry>> functions_;
    using BuiltinFn = std::function<void(const std::vector<ExprPtr>&)>;
    std::unordered_map<std::string, BuiltinFn> builtins_;

    struct StructInfo {
        llvm::StructType *llvmType;
        std::vector<FieldDef> fields;
    };
    std::unordered_map<std::string, StructInfo> struct_types_;
    std::unordered_map<llvm::Type*, llvm::StructType*> option_types_;
    std::unordered_map<llvm::Value*, llvm::Type*> list_element_types_;
    std::unordered_map<llvm::Value*, llvm::Type*> map_key_types_;
    std::unordered_map<llvm::Value*, llvm::Type*> map_value_types_;

    // RAII scope for function emission (B5)
    class FnScope {
    public:
        explicit FnScope(CodeGen &cg);
        ~FnScope();
        FnScope(const FnScope&) = delete;
        FnScope& operator=(const FnScope&) = delete;
    private:
        CodeGen &cg_;
        llvm::Function *savedFn_;
        std::vector<std::unordered_map<std::string, llvm::AllocaInst*>> savedScope_;
        std::vector<std::unordered_set<std::string>> savedConstScope_;
        llvm::BasicBlock *savedBlock_;
        llvm::BasicBlock::iterator savedPoint_;
    };

    void pushScope();
    void popScope();
    llvm::AllocaInst *findVar(const std::string &name);
    bool isConst(const std::string &name) const;
    llvm::AllocaInst *getOrCreateVar(const std::string &name, llvm::Type *ty);

    // Variable declaration (B3)
    void emitVarDecl(const std::string &name,
                     const std::optional<std::string> &type_annotation,
                     ExprNode &value, bool is_const);

    void emitStmt(LetStmt &s);
    void emitStmt(ConstStmt &s);
    void emitStmt(AssignStmt &s);
    void emitStmt(CallStmt &s);
    void emitStmt(ReturnStmt &s);
    void emitStmt(ImportStmt &s);
    void emitStmt(TypeStmt &s);
    void emitStmt(IndexAssignStmt &s);
    void emitStmt(std::unique_ptr<IfStmt> &s);
    void emitStmt(std::unique_ptr<WhileStmt> &s);
    void emitStmt(std::unique_ptr<FnStmt> &s);
    llvm::Value *toBool(llvm::Value *v);

    // Type promotion helpers (B1)
    llvm::Value *promoteToInt(llvm::Value *v);
    std::pair<llvm::Value*, llvm::Value*> promoteToFloat(llvm::Value *lhs, llvm::Value *rhs);

    llvm::Value *emitExpr(const ExprNode &node);
    llvm::Value *emitExprVariant(const NumberExpr &e);
    llvm::Value *emitExprVariant(const FloatExpr &e);
    llvm::Value *emitExprVariant(const BoolExpr &e);
    llvm::Value *emitExprVariant(const StringExpr &e);
    llvm::Value *emitExprVariant(const VariableExpr &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<UnaryExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<BinaryExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<CallExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<FieldAccessExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<TupleExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<ListExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<IndexExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<MapExpr> &e);

    // BinaryExpr sub-dispatchers (B2)
    llvm::Value *emitComparisonOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs);
    llvm::Value *emitLogicalOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs);
    llvm::Value *emitBitwiseOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs);
    llvm::Value *emitArithmeticOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs);

    // Function call helper (B4)
    llvm::Value *emitUserFnCall(const std::string &callee, const std::vector<ExprPtr> &args);
    llvm::Function *resolveOverload(const std::string &callee,
                                    const std::vector<ExprPtr> &args,
                                    std::vector<llvm::Value*> &outArgVals);

    llvm::Value *emitStructConstructor(const StructInfo &info, const std::string &name, const std::vector<ExprPtr> &args);
    llvm::Type *resolveType(const std::string &typeName);
    llvm::StructType *getOptionType(llvm::Type *innerTy);
    bool isOptionType(llvm::Type *ty);
    llvm::Type *getListElementType(llvm::Value *listAlloca);
    llvm::Type *getMapKeyType(llvm::Value *mapVal);
    llvm::Type *getMapValueType(llvm::Value *mapVal);
    llvm::Value *emitMapKeyLookup(llvm::Value *mapPtr, llvm::Value *key, llvm::Type *keyTy);
    void emitPrint(const std::vector<ExprPtr> &args);
};
