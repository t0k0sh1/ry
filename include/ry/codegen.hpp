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
    explicit CodeGen(bool test_mode = false);
    llvm::orc::ThreadSafeModule compile(Program &prog);

private:
    std::unique_ptr<llvm::LLVMContext> ctx_;
    std::unique_ptr<llvm::Module> mod_;
    llvm::IRBuilder<> builder_;
    llvm::Function *fn_ = nullptr;
    llvm::Type *i64Ty_, *i32Ty_, *f64Ty_, *i1Ty_, *ptrTy_;
    llvm::StructType *listHeaderTy_;
    llvm::StructType *mapHeaderTy_;
    llvm::StructType *setHeaderTy_;
    std::vector<std::unordered_map<std::string, llvm::AllocaInst*>> scope_stack_;
    std::vector<std::unordered_set<std::string>> const_scope_stack_;
    struct OverloadEntry {
        llvm::Function *func;
        std::vector<llvm::Type*> paramTypes;
        std::vector<std::string> paramTypeNames;
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
    std::unordered_map<llvm::Value*, llvm::Type*> set_element_types_;

    struct UnionTypeInfo {
        llvm::StructType *llvmType;
        std::vector<std::string> componentNames;
        std::vector<llvm::Type*> componentTypes;
    };
    std::unordered_map<std::string, UnionTypeInfo> union_type_info_;
    std::unordered_map<llvm::Value*, std::string> union_value_types_;
    std::string current_fn_return_type_;

    struct EnumInfo {
        std::string name;
        std::unordered_map<std::string, int64_t> variants;
        llvm::GlobalVariable *nameArray;
        size_t variantCount;
    };
    std::unordered_map<std::string, EnumInfo> enum_types_;
    std::unordered_map<llvm::Value*, std::string> enum_value_types_;

    // Function type info for indirect calls (lambda / function pointers)
    struct FnTypeInfo {
        std::vector<llvm::Type*> paramTypes;
        llvm::Type *returnType;
        std::vector<std::string> capturedVars;   // for closure support
        std::vector<llvm::Type*> capturedTypes;  // types of captured variables
    };
    std::unordered_map<llvm::Value*, FnTypeInfo> fn_type_info_;
    int lambda_counter_ = 0;
    bool test_mode_ = false;
    int test_fn_counter_ = 0;

    // Loop context stack for break/continue (condBB, endBB)
    std::vector<std::pair<llvm::BasicBlock*, llvm::BasicBlock*>> loop_stack_;

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
    void emitStmt(VarStmt &s);
    void emitStmt(AssignStmt &s);
    void emitStmt(CallStmt &s);
    void emitStmt(ReturnStmt &s);
    void emitStmt(ImportStmt &s);
    void emitStmt(TypeStmt &s);
    void emitStmt(IndexAssignStmt &s);
    void emitStmt(BreakStmt &s);
    void emitStmt(ContinueStmt &s);
    void emitStmt(FieldAssignStmt &s);
    void emitStmt(EnumStmt &s);
    void emitStmt(ExpectStmt &s);
    void emitStmt(std::unique_ptr<IfStmt> &s);
    void emitStmt(std::unique_ptr<WhileStmt> &s);
    void emitStmt(std::unique_ptr<ForStmt> &s);
    void emitStmt(std::unique_ptr<FnStmt> &s);
    void emitStmt(std::unique_ptr<DescribeStmt> &s);
    void emitStmt(std::unique_ptr<MatchStmt> &s);
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
    llvm::Value *emitExprVariant(const std::unique_ptr<SetExpr> &e);
    llvm::Value *emitExprVariant(const EnumAccessExpr &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<LambdaExpr> &e);

    // Operator overload helpers
    llvm::Value *tryOperatorCall(const std::string &opFnName,
                                 llvm::Value *lhs, llvm::Value *rhs);
    llvm::Value *tryUnaryOperatorCall(const std::string &opFnName,
                                      llvm::Value *operand);

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
    llvm::Value *buildNoneValue(llvm::Type *optionTy);
    std::pair<llvm::Type*, llvm::Type*> parseMapTypeAnnotation(const std::string &typeStr);
    FnTypeInfo parseFnTypeAnnotation(const std::string &typeStr);
    void emitRuntimeError(const std::string &message, const std::string &globalName);
    void emitPrintValue(llvm::Value *val, llvm::Type *ty,
                        llvm::FunctionCallee printfFn, const std::string &suffix);
    llvm::Type *getListElementType(llvm::Value *listAlloca);
    llvm::Type *getMapKeyType(llvm::Value *mapVal);
    llvm::Type *getMapValueType(llvm::Value *mapVal);
    llvm::Value *emitMapKeyLookup(llvm::Value *mapPtr, llvm::Value *key, llvm::Type *keyTy);
    llvm::Type *getSetElementType(llvm::Value *setVal);
    llvm::Value *emitSetElementLookup(llvm::Value *setPtr, llvm::Value *elem, llvm::Type *elemTy);
    void emitPrint(const std::vector<ExprPtr> &args);

    // Union type helpers
    std::vector<std::string> parseUnionComponents(const std::string &typeName);
    std::string normalizeUnionType(const std::string &typeName);
    bool isUnionType(const std::string &typeName);
    llvm::Value *wrapInUnion(llvm::Value *val, const std::string &unionTypeName);
};
