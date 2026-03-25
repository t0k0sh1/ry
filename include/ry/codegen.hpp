#pragma once

#include "ry/ast.hpp"
#include "ry/source_location.hpp"
#include "ry/source_manager.hpp"
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include <functional>
#include <map>
#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>

class CodeGen {
public:
    explicit CodeGen(bool test_mode = false, const SourceManager *sm = nullptr,
                     bool coverage_mode = false, int coverage_file_id_offset = 0);
    llvm::orc::ThreadSafeModule compile(Program &prog);
    const std::vector<std::string>& getWarnings() const { return warnings_; }

private:
    std::unique_ptr<llvm::LLVMContext> ctx_;
    std::unique_ptr<llvm::Module> mod_;
    llvm::IRBuilder<> builder_;
    llvm::Function *fn_ = nullptr;
    llvm::Type *i64Ty_, *i32Ty_, *i8Ty_, *f64Ty_, *i1Ty_, *ptrTy_;
    llvm::StructType *listHeaderTy_;
    llvm::StructType *mapHeaderTy_;
    llvm::StructType *setHeaderTy_;
    llvm::StructType *iteratorHeaderTy_;
    llvm::StructType *errorTy_;
    std::vector<std::unordered_map<std::string, llvm::AllocaInst*>> scope_stack_;
    std::vector<std::unordered_set<std::string>> immutable_scope_stack_;
    struct OverloadEntry {
        llvm::Function *func;
        std::vector<llvm::Type*> paramTypes;
        std::vector<std::string> paramTypeNames;
        std::string returnTypeName;
    };
    std::unordered_map<std::string, std::vector<OverloadEntry>> functions_;
    using BuiltinFn = std::function<void(const std::vector<ExprPtr>&)>;
    std::unordered_map<std::string, BuiltinFn> builtins_;

    struct StructInfo {
        llvm::StructType *llvmType;
        std::vector<FieldDef> fields;
        std::vector<ExprPtr> invariants;
    };
    std::unordered_map<std::string, StructInfo> struct_types_;
    std::unordered_map<std::string, std::string> type_aliases_;
    std::unordered_map<llvm::Type*, llvm::StructType*> option_types_;
    std::map<std::pair<llvm::Type*, llvm::Type*>, llvm::StructType*> result_types_;
    std::unordered_map<llvm::Value*, llvm::Type*> list_element_types_;
    std::unordered_map<llvm::Value*, llvm::Type*> map_key_types_;
    std::unordered_map<llvm::Value*, llvm::Type*> map_value_types_;
    std::unordered_map<llvm::Value*, llvm::Type*> set_element_types_;
    std::unordered_map<llvm::Value*, llvm::Type*> nested_list_element_types_;
    std::unordered_map<llvm::Value*, llvm::Type*> task_result_types_;
    std::unordered_map<llvm::Value*, llvm::Type*> channel_element_types_;
    std::unordered_map<llvm::Value*, llvm::Type*> iterator_element_types_;
    int iterator_fn_counter_ = 0;

    struct UnionTypeInfo {
        llvm::StructType *llvmType;
        std::vector<std::string> componentNames;
        std::vector<llvm::Type*> componentTypes;
    };
    std::unordered_map<std::string, UnionTypeInfo> union_type_info_;
    std::unordered_map<llvm::Value*, std::string> union_value_types_;
    std::string current_fn_return_type_;

    struct VariantFieldInfo {
        std::vector<llvm::Type*> fieldTypes;
        std::vector<std::string> fieldTypeNames;
    };
    struct EnumInfo {
        std::string name;
        std::unordered_map<std::string, int64_t> variants;
        llvm::GlobalVariable *nameArray;
        size_t variantCount;
        bool isADT = false;
        llvm::StructType *adtType = nullptr;   // { i64 tag, [N x i8] payload }
        size_t maxPayloadSize = 0;
        std::unordered_map<std::string, VariantFieldInfo> variantFields;
    };
    std::unordered_map<std::string, EnumInfo> enum_types_;
    std::unordered_map<llvm::Value*, std::string> enum_value_types_;

    struct GenericEnumTemplate {
        std::string name;
        std::vector<std::string> typeParams;
        std::vector<EnumVariant> variants;
    };
    std::unordered_map<std::string, GenericEnumTemplate> generic_enum_templates_;
    void instantiateGenericEnum(const std::string &fullName, const std::string &baseName,
                                const std::vector<std::string> &typeArgs);

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
    bool coverage_mode_ = false;
    int coverage_file_id_offset_ = 0;
    int test_fn_counter_ = 0;
    const SourceManager *sm_ = nullptr;
    SourceLocation current_loc_;
    std::unordered_set<int64_t> registered_coverage_lines_;

    void emitCoverage(const SourceLocation &loc);

    // Contract (Design by Contract) support
    std::vector<ExprPtr> *current_postconditions_ = nullptr;
    std::vector<std::string> *ensure_bindings_ = nullptr;
    bool in_ensure_context_ = false;
    int contract_err_counter_ = 0;

    // Directive support
    std::unordered_set<std::string> deprecated_functions_;
    std::unordered_set<std::string> deprecated_types_;
    std::unordered_set<std::string> deprecated_variables_;
    std::unordered_set<std::string> deprecated_fields_;  // "TypeName.fieldName"
    std::vector<std::string> warnings_;

    void emitDeprecationWarning(const std::string &name);

    // @native let constants
    std::unordered_set<std::string> native_constants_;
    llvm::Value *emitNativeConstant(const std::string &name);

    // @native fn signature registry (argument count per overload)
    std::unordered_map<std::string, std::vector<size_t>> native_fn_arg_counts_;
    void validateNativeCallArgs(const std::string &callee, const std::vector<ExprPtr> &args);

    // Literal/range type constraints
    struct TypeConstraint {
        enum Kind { IntLiteral, StrLiteral, IntRange };
        Kind kind;
        std::vector<int64_t> int_values;    // for IntLiteral
        std::vector<std::string> str_values; // for StrLiteral
        int64_t range_low = 0, range_high = 0; // for IntRange
    };
    std::unordered_map<llvm::AllocaInst*, TypeConstraint> type_constraints_;
    int constraint_err_counter_ = 0;

    bool isIntLiteralType(const std::string &typeName);
    bool isStrLiteralType(const std::string &typeName);
    bool isRangeType(const std::string &typeName);
    bool isLiteralUnionType(const std::string &typeName);
    std::optional<TypeConstraint> parseTypeConstraint(const std::string &typeName);
    void emitConstraintCheck(llvm::Value *val, const TypeConstraint &constraint,
                              const std::string &varName);
    std::string resolveTypeAlias(const std::string &typeName);

    // Loop context stack for break/continue (condBB, endBB)
    std::vector<std::pair<llvm::BasicBlock*, llvm::BasicBlock*>> loop_stack_;

    // Indexed for-loop helper: emits loop scaffolding and calls bindVars for each iteration
    void emitIndexedForLoop(llvm::Value *length,
                            std::vector<StmtNode> &body,
                            std::function<void(llvm::Value *iCur)> bindVars);
    void emitChannelForLoop(ForStmt &s, llvm::Value *channel, llvm::Type *elemTy);
    void emitParallelForRange(ForStmt &s, llvm::Value *begin, llvm::Value *end, llvm::Value *step);
    void validateParallelFor(const ForStmt &s);

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
        std::vector<ExprPtr> *savedPostconditions_;
        std::vector<std::string> *savedEnsureBindings_;
        bool savedInEnsureContext_;
    };

    [[noreturn]] void codegenError(const SourceLocation &loc, const std::string &msg);
    [[noreturn]] void codegenError(const std::string &msg);

    void pushScope();
    void popScope();
    llvm::AllocaInst *findVar(const std::string &name);
    bool isImmutable(const std::string &name) const;
    llvm::AllocaInst *getOrCreateVar(const std::string &name, llvm::Type *ty);

    // Variable declaration (B3)
    void emitVarDecl(const std::string &name,
                     const std::optional<std::string> &type_annotation,
                     ExprNode &value, bool is_immutable);

    void emitStmt(AssignStmt &s);
    void emitStmt(CallStmt &s);
    void emitStmt(ReturnStmt &s);
    void emitStmt(ImportStmt &s);
    void emitStmt(RecordStmt &s);
    void emitStmt(TypeAliasStmt &s);
    void emitStmt(IndexAssignStmt &s);
    void emitStmt(BreakStmt &s);
    void emitStmt(ContinueStmt &s);
    void emitStmt(EllipsisStmt &s);
    void emitStmt(FieldAssignStmt &s);
    void emitStmt(EnumStmt &s);
    void emitStmt(ExpectStmt &s);
    void emitStmt(AwaitStmt &s);
    void emitStmt(TupleDestructStmt &s);
    void emitStmt(std::unique_ptr<SelectStmt> &s);
    void emitStmt(std::unique_ptr<IfStmt> &s);
    void emitStmt(std::unique_ptr<WhileStmt> &s);
    void emitStmt(std::unique_ptr<ForStmt> &s);
    void emitStmt(std::unique_ptr<FnStmt> &s);
    void emitStmt(std::unique_ptr<MatchStmt> &s);
    void emitDescribeCall(CallStmt &s);
    void emitItCall(CallStmt &s);
    void emitEachItCall(CallStmt &s);
    void emitPropertyItCall(CallStmt &s);
    std::pair<llvm::FunctionCallee, llvm::FunctionCallee> getTestItFunctions();
    llvm::Function *emitTestFunction(const std::string &namePrefix,
        const std::vector<llvm::Type*> &paramTypes, LambdaExpr &lam, const std::string &context);
    void emitMockCall(CallStmt &s);
    std::unordered_set<std::string> mocked_functions_;
    std::unordered_map<std::string, llvm::Constant*> mock_name_strings_;
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
    llvm::Value *emitExprVariant(const std::unique_ptr<CastExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<InterpolatedStringExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<TernaryExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<RangeExpr> &e);
    llvm::Value *emitExprVariant(const NoneExpr &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<ErrorPropagateExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<SpawnExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<AwaitExpr> &e);
    llvm::Value *valueToString(llvm::Value *val);

    // Operator overload helpers
    llvm::Value *tryOperatorCall(const std::string &opFnName,
                                 llvm::Value *lhs, llvm::Value *rhs);
    llvm::Value *tryUnaryOperatorCall(const std::string &opFnName,
                                      llvm::Value *operand);

    // BinaryExpr sub-dispatchers (B2)
    llvm::Value *emitComparisonOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs);
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
    llvm::Value *buildSomeValue(llvm::Value *inner, llvm::Type *optionTy);
    llvm::StructType *getResultType(llvm::Type *okTy, llvm::Type *errTy);
    bool isResultType(llvm::Type *ty);
    llvm::Value *buildOkValue(llvm::Value *inner, llvm::StructType *resultTy);
    llvm::Value *buildErrValue(llvm::Value *inner, llvm::StructType *resultTy);
    llvm::Value *buildStaticError(const std::string &msg, const std::string &globalName);
    std::pair<llvm::Type*, llvm::Type*> parseMapTypeAnnotation(const std::string &typeStr);
    FnTypeInfo parseFnTypeAnnotation(const std::string &typeStr);
    void emitRuntimeError(const std::string &message, const std::string &globalName);
    void emitContractCheck(const std::string &kind, const std::string &fn_name,
                           const ExprPtr &cond);
    void emitEnsureChecks(llvm::Value *retVal);
    void emitInvariantCheck(const std::string &typeName, const StructInfo &info,
                            llvm::Value *structVal);
    void emitPrintValue(llvm::Value *val, llvm::Type *ty,
                        llvm::FunctionCallee printfFn, const std::string &suffix);
    llvm::Type *getListElementType(llvm::Value *listAlloca);
    llvm::Type *getMapKeyType(llvm::Value *mapVal);
    llvm::Type *getMapValueType(llvm::Value *mapVal);
    llvm::Value *emitMapKeyLookup(llvm::Value *mapPtr, llvm::Value *key, llvm::Type *keyTy);
    llvm::Value *emitIsWhitespace(llvm::Value *ch);

    // C stdlib function helpers
    llvm::FunctionCallee getStdlibMalloc();
    llvm::FunctionCallee getStdlibRealloc();
    llvm::FunctionCallee getStdlibFree();
    llvm::FunctionCallee getStdlibStrlen();
    llvm::FunctionCallee getStdlibMemcpy();
    llvm::FunctionCallee getStdlibMemmove();
    llvm::FunctionCallee getStdlibMemset();
    llvm::FunctionCallee getStdlibStrcmp();
    llvm::FunctionCallee getStdlibStrncmp();
    llvm::FunctionCallee getStdlibStrstr();
    llvm::FunctionCallee getStdlibStrcpy();
    llvm::FunctionCallee getStdlibStrcat();
    llvm::FunctionCallee getStdlibSnprintf();
    llvm::FunctionCallee getStdlibPrintf();
    llvm::FunctionCallee getStdlibExit();
    llvm::Type *getSetElementType(llvm::Value *setVal);
    llvm::Type *getNestedListElementType(llvm::Value *listVal);
    llvm::Value *emitSetElementLookup(llvm::Value *setPtr, llvm::Value *elem, llvm::Type *elemTy);
    llvm::Type *getTaskResultType(llvm::Value *taskVal);
    llvm::Type *getChannelElementType(llvm::Value *channelVal);

    // Hash function resolution helper (Step 1)
    struct HashFnInfo {
        std::string hashFnName;
        std::string rehashFnName;
        llvm::Type *hashArgTy;
    };
    HashFnInfo resolveHashFn(llvm::Type *keyTy);

    // Collection type lookup helper (Step 2)
    static llvm::Type *lookupCollectionType(
        const std::unordered_map<llvm::Value*, llvm::Type*> &map, llvm::Value *val);

    // Unified hash table lookup helper (Step 3)
    struct HashTableLayout {
        unsigned lenIdx;
        unsigned bucketCountIdx;
        unsigned bucketsPtrIdx;
        unsigned keysPtrIdx;
    };
    llvm::Value *emitHashTableLookup(llvm::Value *containerPtr, llvm::StructType *headerTy,
                                      const HashTableLayout &layout,
                                      llvm::Value *key, llvm::Type *keyTy);

    // Builtin dispatch helpers (Step 4)
    llvm::Value *emitBuiltinCore(const CallExpr &e);
    llvm::Value *emitBuiltinCollection(const CallExpr &e);
    llvm::Value *emitBuiltinString(const CallExpr &e);
    llvm::Value *emitBuiltinHigherOrder(const CallExpr &e);
    llvm::Value *emitSortCore(llvm::Value *listVal, const std::vector<ExprPtr> &args, const std::string &callee);
    llvm::Value *emitBuiltinQuery(const CallExpr &e);
    llvm::Value *emitBuiltinSetOps(const CallExpr &e);
    llvm::Value *emitBuiltinConversion(const CallExpr &e);
    llvm::Value *emitBuiltinRegex(const CallExpr &e);
    llvm::Value *emitBuiltinMath(const CallExpr &e);
    llvm::Value *emitBuiltinIO(const CallExpr &e);
    llvm::Value *emitBuiltinNet(const CallExpr &e);
    llvm::Value *emitBuiltinHttp(const CallExpr &e);
    bool isTcpListener(llvm::Value *val);
    bool isTcpStream(llvm::Value *val);
    bool isHttpRequest(llvm::Value *val);
    bool isHttpResponse(llvm::Value *val);
    bool isHttpClientResponse(llvm::Value *val);
    void propagateResourceTracking(llvm::Value *src, llvm::Value *dst);
    void propagateResourceTrackingWide(llvm::Value *src, llvm::Value *dst);
    void registerResourceByTypeName(const std::string &typeName, llvm::Value *val);
    llvm::Value *emitPtrToResult(llvm::Value *ptr, const std::string &name,
                                 const std::string &errMsg,
                                 std::unordered_set<llvm::Value*> &trackingSet);
    std::unordered_set<llvm::Value*> tcp_listener_values_;
    std::unordered_set<llvm::Value*> tcp_stream_values_;
    std::unordered_set<llvm::Value*> http_request_values_;
    std::unordered_set<llvm::Value*> http_response_values_;
    std::unordered_set<llvm::Value*> http_client_response_values_;
    llvm::Value *emitBuiltinIterator(const CallExpr &e);
    llvm::Type *getIteratorElementType(llvm::Value *iterVal);
    void emitBucketInit(llvm::Value *headerPtr, llvm::StructType *headerTy,
                        unsigned bucketCountIdx, unsigned bucketsPtrIdx,
                        int64_t initialBucketCount);
    void emitBucketInsertAndRehashCheck(llvm::Value *headerPtr, llvm::StructType *headerTy,
                                         unsigned lenIdx, unsigned bucketCountIdx, unsigned bucketsPtrIdx,
                                         llvm::Value *key, llvm::Type *keyTy, llvm::Value *denseIndex);
    void emitPrint(const std::vector<ExprPtr> &args);
    void emitExit(const std::vector<ExprPtr> &args);

    // Lambda call helper: invoke a lambda/closure value with given args
    llvm::Value *emitLambdaCall(llvm::Value *lambdaVal, const FnTypeInfo &info,
                                std::vector<llvm::Value*> args, const std::string &name);

    // Lambda return type inference
    llvm::Type *inferExprType(const ExprNode &expr,
        const std::unordered_map<std::string, llvm::Type*> &paramTypeMap);
    llvm::Type *inferReturnType(const std::vector<StmtNode> &body,
        const std::unordered_map<std::string, llvm::Type*> &paramTypeMap);

    // Union type helpers
    std::vector<std::string> parseUnionComponents(const std::string &typeName);
    std::string normalizeUnionType(const std::string &typeName);
    bool isUnionType(const std::string &typeName);
    llvm::Value *wrapInUnion(llvm::Value *val, const std::string &unionTypeName);
};
