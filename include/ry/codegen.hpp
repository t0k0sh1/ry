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
    llvm::Type *i64Ty_, *i32Ty_, *i16Ty_, *i8Ty_, *f64Ty_, *f32Ty_, *i1Ty_, *ptrTy_;
    llvm::FunctionType *fnTy_ptr_to_ptr_;
    llvm::FunctionType *fnTy_ptr_to_i64_;
    llvm::FunctionType *fnTy_ptr_to_void_;
    llvm::FunctionType *fnTy_ptr_ptr_to_ptr_;
    llvm::FunctionType *fnTy_ptr_ptr_to_i64_;
    llvm::FunctionType *fnTy_ptr_i64_to_ptr_;
    llvm::FunctionType *fnTy_ptr_ptr_ptr_to_ptr_;
    llvm::FunctionType *fnTy_void_to_ptr_;
    llvm::StructType *listHeaderTy_;
    llvm::StructType *mapHeaderTy_;
    llvm::StructType *setHeaderTy_;
    llvm::StructType *iteratorHeaderTy_;
    llvm::StructType *errorTy_;
    llvm::StructType *anyTy_;

    // Resource type tracking
    enum ResourceKind : int {
        RK_TcpListener, RK_TcpStream, RK_TlsStream,
        RK_HttpRequest, RK_HttpResponse, RK_HttpClientResponse, RK_JsonValue,
        RK_Thread, RK_Lock, RK_RWLock, RK_Semaphore, RK_Barrier,
        RK_AtomicInt, RK_AtomicBool, RK_COUNT
    };

    // ARC infrastructure
    llvm::StructType *arcHeaderTy_;                       // { i64 strong_count, i64 weak_count }
    static constexpr uint64_t ARC_HEADER_SIZE = 16;
    static constexpr int64_t ARC_IMMORTAL = INT64_MAX;    // sentinel: never retain/release
    std::unordered_set<llvm::Value*> arc_atomic_values_;  // values requiring atomic refcount ops
    std::unordered_set<llvm::AllocaInst*> arc_managed_vars_; // allocas holding ARC-managed ptrs
    std::unordered_set<llvm::Value*> arc_owned_values_;  // values produced by emitArcAlloc (data ptrs)
    std::unordered_set<llvm::AllocaInst*> arc_backed_vars_; // allocas that hold ARC-allocated collections (have ARC header)
    std::unordered_set<llvm::AllocaInst*> weak_managed_vars_; // allocas holding weak ref ptrs (header ptrs)
    std::unordered_map<llvm::AllocaInst*, std::string> weak_inner_type_names_; // inner type name for upgrade
    std::unordered_map<llvm::AllocaInst*, ResourceKind> resource_managed_vars_;

    // ARC emit methods
    llvm::Value *emitArcAlloc(llvm::Value *dataSize);
    void emitArcRetain(llvm::Value *headerPtr, bool atomic = false);
    void emitArcRelease(llvm::Value *headerPtr, bool atomic = false,
                        llvm::FunctionCallee destructor = {});
    llvm::Value *emitArcGetDataPtr(llvm::Value *headerPtr);
    llvm::Value *emitArcGetHeaderFromData(llvm::Value *dataPtr);
    bool isArcAtomic(llvm::Value *val) const;
    void markArcAtomic(llvm::Value *val);
    void markArcManaged(llvm::AllocaInst *alloca);
    bool isArcManaged(llvm::AllocaInst *alloca) const;
    void emitScopeCleanup();
    void emitScopeCleanupToDepth(size_t targetDepth);
    llvm::FunctionCallee resolveCollectionDestructor(llvm::AllocaInst *alloca);
    void emitArcReleaseVar(const std::string &name, llvm::AllocaInst *alloca);
    bool tryRetainArcSource(llvm::Value *val);

    // Weak reference operations
    static bool isWeakTypeName(const std::string &typeName);
    static std::string weakInnerTypeName(const std::string &typeName);
    void markWeakManaged(llvm::AllocaInst *alloca);
    bool isWeakManaged(llvm::AllocaInst *alloca) const;
    void emitWeakRetain(llvm::Value *headerPtr);
    void emitWeakRelease(llvm::Value *headerPtr);
    llvm::Value *emitWeakUpgrade(llvm::Value *headerPtr, const std::string &innerTypeName);
    void emitWeakReleaseVar(const std::string &name, llvm::AllocaInst *alloca);

    // ARC destructor generation — frees internal buffers of collections
    enum class CollectionKind { List, Map, Set };
    llvm::FunctionCallee getOrCreateCollectionDestructor(CollectionKind kind);

    // Copy-on-Write (CoW) support
    llvm::AllocaInst *tryGetReceiverAlloca(const ExprNode &expr);
    llvm::Value *emitCowCheck(llvm::Value *dataPtr, llvm::AllocaInst *alloca, CollectionKind kind);
    llvm::Value *emitCowDeepCopyList(llvm::Value *oldDataPtr, llvm::Type *elemTy);
    llvm::Value *emitCowDeepCopyMap(llvm::Value *oldDataPtr, llvm::Type *keyTy, llvm::Type *valTy);
    llvm::Value *emitCowDeepCopySet(llvm::Value *oldDataPtr, llvm::Type *elemTy);
    void emitCowRetainArcElements(llvm::Value *buf, llvm::Value *len, const std::string &tag);
    std::map<CollectionKind, llvm::FunctionCallee> arc_destructors_cache_;
    llvm::FunctionCallee getOrCreateResourceDestructor(ResourceKind rk);
    std::map<ResourceKind, llvm::FunctionCallee> resource_destructors_cache_;
    llvm::FunctionCallee resolveDestructor(llvm::AllocaInst *alloca);
    ResourceKind detectResourceKind(llvm::Value *val);
    void nullifyResourceVar(const ExprNode &argExpr);

    std::unordered_map<std::string, llvm::Constant*> global_string_cache_;
    llvm::Constant *cachedGlobalString(const std::string &str, const llvm::Twine &name = "");
    static constexpr int64_t TAG_INT   = 0;
    static constexpr int64_t TAG_FLOAT = 1;
    static constexpr int64_t TAG_BOOL  = 2;
    static constexpr int64_t TAG_STR   = 3;
    static constexpr int64_t TAG_UNIT  = 4; // reserved for future use
    std::vector<std::unordered_map<std::string, llvm::AllocaInst*>> scope_stack_;
    std::vector<std::unordered_set<std::string>> immutable_scope_stack_;
    struct OverloadEntry {
        llvm::Function *func;
        std::vector<llvm::Type*> paramTypes;
        std::vector<std::string> paramTypeNames;
        std::string returnTypeName;
        size_t minArity = 0;
        std::vector<ExprPtr> defaultValues;
    };
    std::unordered_map<std::string, std::vector<OverloadEntry>> functions_;
    using BuiltinFn = std::function<void(const std::vector<ExprPtr>&)>;
    std::unordered_map<std::string, BuiltinFn> builtins_;

    struct StructInfo {
        llvm::StructType *llvmType;
        std::vector<FieldDef> fields;
        std::vector<ExprPtr> invariants;
        std::string parent_name;
    };
    std::unordered_map<std::string, StructInfo> struct_types_;
    std::unordered_map<std::string, std::string> type_aliases_;
    std::unordered_map<llvm::Type*, llvm::StructType*> option_types_;
    std::map<std::pair<llvm::Type*, llvm::Type*>, llvm::StructType*> result_types_;
    enum TypeMeta {
        TM_ListElem, TM_MapKey, TM_MapValue, TM_SetElem,
        TM_NestedListElem, TM_TaskResult, TM_IteratorElem, TM_COUNT
    };
    std::unordered_map<llvm::Value*, llvm::Type*> type_meta_[TM_COUNT];
    std::unordered_map<llvm::Value*, std::string> low_level_type_names_;

    // Fixed-length array element type names (e.g., "i32", "u8")
    // elementType and size are derivable from AllocaInst->getAllocatedType()
    std::unordered_map<llvm::AllocaInst*, std::string> array_elem_type_names_;

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
        std::vector<std::string> variantOrder;
        llvm::GlobalVariable *nameArray;
        size_t variantCount;
        bool isADT = false;
        bool hasExplicitValues = false;
        llvm::StructType *adtType = nullptr;   // { i64 tag, [N x i8] payload }
        size_t maxPayloadSize = 0;
        std::unordered_map<std::string, VariantFieldInfo> variantFields;
    };
    std::unordered_map<std::string, EnumInfo> enum_types_;
    std::unordered_map<llvm::Value*, std::string> enum_value_types_;

    struct GenericEnumTemplate {
        std::string name;
        std::vector<TypeParam> typeParams;
        std::vector<EnumVariant> variants;
    };
    std::unordered_map<std::string, GenericEnumTemplate> generic_enum_templates_;
    void instantiateGenericEnum(const std::string &fullName, const std::string &baseName,
                                const std::vector<std::string> &typeArgs);

    // Generic function templates
    struct GenericFnTemplate {
        std::unique_ptr<FnStmt> fnStmt;
    };
    std::unordered_map<std::string, GenericFnTemplate> generic_fn_templates_;
    std::unordered_set<std::string> generic_fn_instantiated_;
    std::unordered_map<std::string, std::string> type_param_scope_;

    void instantiateGenericFn(const std::string &baseName,
                              const std::vector<std::string> &typeArgs);
    void validateTypeBounds(const std::vector<TypeParam> &typeParams,
                            const std::vector<std::string> &typeArgs,
                            const std::string &context);
    std::vector<std::string> inferTypeArgs(const std::string &baseName,
                                           const std::vector<ExprPtr> &args);
    std::string reverseResolveType(llvm::Value *val);

    // Function type info for indirect calls (lambda / function pointers)
    struct FnTypeInfo {
        std::vector<llvm::Type*> paramTypes;
        std::vector<std::string> paramTypeNames;
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
    static bool isNativeConstant(const std::string &name);
    llvm::Value *emitNativeConstant(const std::string &name);

    // @native fn signature registry (argument count per overload)
    std::unordered_map<std::string, std::vector<size_t>> native_fn_arg_counts_;

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
    int arith_zero_err_counter_ = 0;

    bool isIntLiteralType(const std::string &typeName);
    bool isStrLiteralType(const std::string &typeName);
    bool isRangeType(const std::string &typeName);
    bool isLiteralUnionType(const std::string &typeName);
    std::optional<TypeConstraint> parseTypeConstraint(const std::string &typeName);
    void emitConstraintCheck(llvm::Value *val, const TypeConstraint &constraint,
                              const std::string &varName);
    std::string resolveTypeAlias(const std::string &typeName);

    // Loop context stack for break/continue (condBB, endBB)
    // { condBB, endBB, scopeDepth at loop entry }
    std::vector<std::tuple<llvm::BasicBlock*, llvm::BasicBlock*, size_t>> loop_stack_;

    // Indexed for-loop helper: emits loop scaffolding and calls bindVars for each iteration
    void emitIndexedForLoop(llvm::Value *length,
                            std::vector<StmtNode> &body,
                            std::function<void(llvm::Value *iCur)> bindVars);
    void emitTupleDestructure(const std::vector<std::string> &var_names,
                              llvm::Value *tupleVal, llvm::StructType *structTy);
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
        std::unordered_set<llvm::AllocaInst*> savedArcManaged_;
        std::unordered_set<llvm::Value*> savedArcOwned_;
        std::unordered_set<llvm::AllocaInst*> savedArcBacked_;
        std::unordered_set<llvm::AllocaInst*> savedWeakManaged_;
        std::unordered_map<llvm::AllocaInst*, std::string> savedWeakInnerTypeNames_;
        std::unordered_map<llvm::AllocaInst*, ResourceKind> savedResourceManaged_;
        llvm::BasicBlock *savedBlock_;
        llvm::BasicBlock::iterator savedPoint_;
        std::vector<ExprPtr> *savedPostconditions_;
        std::vector<std::string> *savedEnsureBindings_;
        bool savedInEnsureContext_;
        std::string savedFnReturnType_;
    };

    [[noreturn]] void codegenError(const SourceLocation &loc, const std::string &msg);
    [[noreturn]] void codegenError(const std::string &msg);

    void requireArgs(const CallExpr &e, size_t expected);
    void requireArgs(const std::string &callee, size_t actual, size_t expected);

    void pushScope();
    void popScope();
    llvm::AllocaInst *findVar(const std::string &name);
    bool isImmutable(const std::string &name) const;
    llvm::AllocaInst *getOrCreateVar(const std::string &name, llvm::Type *ty);

    // Variable declaration (B3)
    void emitVarDecl(const std::string &name,
                     const TypeNodePtr &type_annotation,
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
    void emitFailCall(CallStmt &s);
    std::unordered_set<std::string> mocked_functions_;
    std::unordered_map<std::string, llvm::Constant*> mock_name_strings_;
    llvm::Constant *fail_empty_msg_ = nullptr;
    llvm::Value *toBool(llvm::Value *v);

    // Low-level type helpers
    const std::string &getLowLevelTypeName(llvm::Value *val) const;
    bool isUnsignedLowLevel(llvm::Value *val) const;
    static bool isUnsignedLowLevelName(const std::string &name);
    static bool isLowLevelTypeName(const std::string &name);
    static std::string getExprLowLevelSuffix(const ExprNode &node);
    bool isLowLevelIntTy(llvm::Type *ty) const;
    bool isLowLevelIntTy(llvm::Value *val) const;
    bool isLowLevelFloatTy(llvm::Type *ty) const;
    bool isLowLevelTy(llvm::Type *ty) const;
    bool isLowLevelTy(llvm::Value *val) const;
    void checkLowLevelTypeMix(llvm::Value *lhs, llvm::Value *rhs, const std::string &op);
    llvm::Value *coerceToLowLevelType(llvm::Value *val, llvm::Type *targetTy,
                                       const std::string &typeName,
                                       const std::string &context,
                                       const std::string &truncName);

    // Checked/Saturating/Wrapping arithmetic helpers
    void validateCheckedArithArgs(llvm::Value *lhs, llvm::Value *rhs, const std::string &callee);
    llvm::Value *emitCheckedArithmetic(const std::string &callee, llvm::Value *lhs, llvm::Value *rhs);
    llvm::Value *emitSaturatingArithmetic(const std::string &callee, llvm::Value *lhs, llvm::Value *rhs);
    llvm::Value *emitWrappingArithmetic(const std::string &callee, llvm::Value *lhs, llvm::Value *rhs);

    // Type promotion helpers (B1)
    llvm::Value *promoteToInt(llvm::Value *v);
    std::pair<llvm::Value*, llvm::Value*> promoteToFloat(llvm::Value *lhs, llvm::Value *rhs);

    // Implicit widening conversion helpers
    bool isWideningConversion(llvm::Value *argVal, llvm::Type *paramTy,
                              const std::string &paramTypeName) const;
    llvm::Value *emitWideningConversion(llvm::Value *argVal, llvm::Type *paramTy);
    bool isSubtypeOf(const std::string &childType, const std::string &parentType) const;
    llvm::Value *emitSubtypeSlice(llvm::Value *childVal,
                                   const std::string &childTypeName,
                                   const std::string &parentTypeName);
    llvm::Value *tryEmitSubtypeCoerce(llvm::Value *val, llvm::Type *targetTy);

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
    llvm::Value *emitExprVariant(const std::unique_ptr<AwaitExpr> &e);
    llvm::Value *emitExprVariant(const std::unique_ptr<WeakExpr> &e);
    llvm::Value *valueToString(llvm::Value *val);
    llvm::Value *structToString(llvm::Value *val);

    // Operator overload helpers
    llvm::Value *findAndCallOverload(const std::string &opFnName,
                                      llvm::ArrayRef<llvm::Value*> args,
                                      const char *callName = "opcall");
    llvm::Value *tryOperatorCall(const std::string &opFnName,
                                 llvm::Value *lhs, llvm::Value *rhs);
    llvm::Value *tryUnaryOperatorCall(const std::string &opFnName,
                                      llvm::Value *operand);
    llvm::Value *trySubscriptOperatorCall(llvm::Value *object,
                                           llvm::ArrayRef<llvm::Value*> indices);
    llvm::Value *tryCallOperator(const std::string &callee,
                                 const std::vector<ExprPtr> &args);
    bool trySubscriptAssignOperatorCall(llvm::Value *object,
                                         llvm::ArrayRef<llvm::Value*> indices,
                                         llvm::Value *value);

    // Binary operation dispatch (user-defined → any → built-in)
    llvm::Value *emitBinaryOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs,
                               const std::string &llNameHint = "");

    // BinaryExpr sub-dispatchers (B2)
    llvm::Value *emitComparisonOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs,
                                   const std::string &llNameHint = "");
    llvm::Value *emitStructComparison(const std::string &op, llvm::Value *lhs,
                                       llvm::Value *rhs, const StructInfo &info);
    llvm::Value *emitBitwiseOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs,
                                const std::string &llNameHint = "");
    llvm::Value *emitArithmeticOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs,
                                   const std::string &llNameHint = "");
    llvm::Value *emitAnyBinaryOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs);
    llvm::Value *emitAnyUnaryNeg(llvm::Value *operand);

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
    void emitRuntimeError(const std::string &message, const std::string &globalName,
                          llvm::ArrayRef<llvm::Value *> extraArgs = {});
    void emitBoundsError(llvm::Value *index, llvm::Value *size,
                         const std::string &fmtMsg, const std::string &globalName);
    llvm::Value *emitNegativeIndexWrap(llvm::Value *idx, llvm::Value *wrapBase,
                                        const std::string &prefix);
    void emitBoundsCheck(llvm::Value *&index, llvm::Value *size,
                         const std::string &errMsg, const std::string &globalName,
                         const std::string &bbPrefix);
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

    // Runtime function helper: getOrInsertFunction with inline FunctionType creation
    llvm::FunctionCallee getRuntimeFn(const char *name, llvm::Type *retTy,
                                       llvm::ArrayRef<llvm::Type*> argTys);

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
    llvm::FunctionCallee getStdlibStrcasestr();
    llvm::FunctionCallee getStdlibStrncasecmp();
    llvm::FunctionCallee getStdlibStrcpy();
    llvm::FunctionCallee getStdlibStrcat();
    llvm::FunctionCallee getStdlibSnprintf();
    llvm::FunctionCallee getStdlibPrintf();
    llvm::FunctionCallee getStdlibExit();
    llvm::Type *getSetElementType(llvm::Value *setVal);
    llvm::Type *getNestedListElementType(llvm::Value *listVal);
    llvm::Value *emitSetElementLookup(llvm::Value *setPtr, llvm::Value *elem, llvm::Type *elemTy);
    llvm::Type *getTaskResultType(llvm::Value *taskVal);
    llvm::Value *emitTaskWait(llvm::Value *taskVal, const char *runtimeFn, const char *label);

    // Hash function resolution helper (Step 1)
    struct HashFnInfo {
        std::string hashFnName;
        std::string rehashFnName;
        llvm::Type *hashArgTy;
    };
    HashFnInfo resolveHashFn(llvm::Type *keyTy);
    llvm::Value *coerceHashKey(llvm::Value *key, llvm::Type *keyTy,
                               llvm::Type *hashArgTy, const llvm::Twine &prefix);

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
    // Set header:  { len, cap, elems*, bucketCount, buckets* }
    static constexpr HashTableLayout kSetLayout = {0, 3, 4, 2};
    // Map header:  { len, cap, keys*, vals*, bucketCount, buckets* }
    static constexpr HashTableLayout kMapLayout = {0, 4, 5, 2};

    llvm::Value *emitHashTableLookup(llvm::Value *containerPtr, llvm::StructType *headerTy,
                                      const HashTableLayout &layout,
                                      llvm::Value *key, llvm::Type *keyTy);

    struct BucketContext {
        llvm::Value *bucketsPtr;
        llvm::Value *bucketMask;
        llvm::Type *hashArgTy;
        llvm::FunctionCallee hashFn;
    };
    BucketContext emitHashTableRemoveBucket(
        llvm::Value *headerPtr, llvm::StructType *headerTy,
        const HashTableLayout &layout,
        llvm::Value *key, llvm::Type *keyTy, llvm::Value *denseIndex,
        const std::string &prefix);
    void emitHashTableUpdateIndex(
        const BucketContext &bc,
        llvm::Value *value, llvm::Type *valueTy,
        llvm::Value *oldIndex, llvm::Value *newIndex,
        const std::string &prefix);

    // Data structure field helpers
    struct ListFields {
        llvm::Value *lenPtr = nullptr;
        llvm::Value *len = nullptr;
        llvm::Value *capPtr = nullptr;
        llvm::Value *cap = nullptr;
        llvm::Value *dataPtr = nullptr;
        llvm::Value *data = nullptr;
    };
    ListFields loadListHeader(llvm::Value *listVal, const std::string &prefix);

    struct SetFields {
        llvm::Value *lenPtr = nullptr;
        llvm::Value *len = nullptr;
        llvm::Value *capPtr = nullptr;
        llvm::Value *cap = nullptr;
        llvm::Value *elemsPtr = nullptr;
        llvm::Value *elems = nullptr;
    };
    SetFields loadSetHeader(llvm::Value *setVal, const std::string &prefix);

    struct MapFields {
        llvm::Value *lenPtr = nullptr;
        llvm::Value *len = nullptr;
        llvm::Value *capPtr = nullptr;
        llvm::Value *cap = nullptr;
        llvm::Value *keysPtr = nullptr;
        llvm::Value *keys = nullptr;
        llvm::Value *valsPtr = nullptr;
        llvm::Value *vals = nullptr;
    };
    MapFields loadMapHeader(llvm::Value *mapVal, const std::string &prefix);

    llvm::Value *wrapPtrAsOption(llvm::Value *ptr, const std::string &hint);

    // Builtin dispatch helpers (Step 4)
    llvm::Value *emitBuiltinCore(const CallExpr &e);
    llvm::Value *emitBuiltinCollection(const CallExpr &e);

    // Collection operation handlers
    llvm::Value *emitCollOp_add(const CallExpr &e);
    llvm::Value *emitCollOp_remove(const CallExpr &e);
    llvm::Value *emitSetRemove(llvm::Value *containerPtr, llvm::Value *elem, llvm::Type *elemTy);
    llvm::Value *emitListRemove(llvm::Value *containerPtr, llvm::Value *val, llvm::Type *listElemTy);
    llvm::Value *emitMapRemove(llvm::Value *containerPtr, llvm::Value *key, llvm::Type *keyTy, llvm::Type *valTy);
    llvm::Value *emitCollOp_append(const CallExpr &e);
    llvm::Value *emitCollOp_appended(const CallExpr &e);
    llvm::Value *emitCollOp_pop(const CallExpr &e);
    llvm::Value *emitCollOp_slice(const CallExpr &e);
    llvm::Value *emitCollOp_take(const CallExpr &e);
    llvm::Value *emitCollOp_insert(const CallExpr &e);
    llvm::Value *emitCollOp_remove_at(const CallExpr &e);
    llvm::Value *emitCollOp_distinct(const CallExpr &e);
    llvm::Value *emitCollOp_flatten(const CallExpr &e);
    llvm::Value *emitCollOp_items(const CallExpr &e);
    llvm::Value *emitCollOp_get(const CallExpr &e);
    llvm::Value *emitCollOp_merge(const CallExpr &e);

    llvm::Value *emitBuiltinString(const CallExpr &e);

    // String operation handlers
    llvm::Value *emitStrOp_contains(const CallExpr &e);
    llvm::Value *emitStrOp_starts_with(const CallExpr &e);
    llvm::Value *emitStrOp_ends_with(const CallExpr &e);
    llvm::Value *emitStrOp_find(const CallExpr &e);
    llvm::Value *emitStrOp_substring(const CallExpr &e);
    llvm::Value *emitStrOp_char_at(const CallExpr &e);
    llvm::Value *emitStrOp_replace(const CallExpr &e);
    llvm::Value *emitStrOp_to_upper(const CallExpr &e);
    llvm::Value *emitStrOp_to_lower(const CallExpr &e);
    llvm::Value *emitStrOp_trim(const CallExpr &e);
    llvm::Value *emitStrOp_trim_start(const CallExpr &e);
    llvm::Value *emitStrOp_trim_end(const CallExpr &e);
    llvm::Value *emitStrOp_repeat(const CallExpr &e);
    llvm::Value *emitStringRepeat(llvm::Value *strVal, llvm::Value *n);
    llvm::Value *emitStrOp_reverse(const CallExpr &e);
    llvm::Value *emitStrOp_reverse_mut(const CallExpr &e);
    llvm::Value *emitStrOp_split(const CallExpr &e);
    llvm::Value *emitStrOp_join(const CallExpr &e);

    llvm::Value *emitBuiltinHigherOrder(const CallExpr &e);
    llvm::Value *emitSortCore(llvm::Value *listVal, const std::vector<ExprPtr> &args, const std::string &callee);
    llvm::Value *emitBuiltinQuery(const CallExpr &e);
    llvm::Value *emitBuiltinSetOps(const CallExpr &e);
    llvm::Value *emitSubsetCheck(llvm::Value *iterSet, llvm::Value *lookupSet,
                                  const std::string &prefix);
    // Set operation handlers
    llvm::Value *emitSetOp_union(const CallExpr &e);
    llvm::Value *emitSetOp_intersection(const CallExpr &e);
    llvm::Value *emitSetOp_difference(const CallExpr &e);
    llvm::Value *emitSetOp_symmetric_difference(const CallExpr &e);
    llvm::Value *emitSetOp_is_subset(const CallExpr &e);
    llvm::Value *emitSetOp_is_superset(const CallExpr &e);
    llvm::Value *emitBuiltinConversion(const CallExpr &e);
    llvm::Value *emitBuiltinRegex(const CallExpr &e);
    llvm::Value *emitBuiltinMath(const CallExpr &e);
    llvm::Value *emitBuiltinIO(const CallExpr &e);
    llvm::Value *emitBuiltinNet(const CallExpr &e);
    llvm::Value *emitBuiltinHttp(const CallExpr &e);
    llvm::Value *emitBuiltinJson(const CallExpr &e);
    llvm::Value *emitBuiltinBase64(const CallExpr &e);
    llvm::Value *emitBuiltinPath(const CallExpr &e);
    llvm::Value *emitBuiltinFilesystem(const CallExpr &e);
    llvm::Value *emitBuiltinThread(const CallExpr &e);
    bool isTcpListener(llvm::Value *val);
    bool isTcpStream(llvm::Value *val);
    bool isTlsStream(llvm::Value *val);
    bool isHttpRequest(llvm::Value *val);
    bool isHttpResponse(llvm::Value *val);
    bool isHttpClientResponse(llvm::Value *val);
    bool isJsonValue(llvm::Value *val);
    bool isThread(llvm::Value *val);
    bool isLock(llvm::Value *val);
    bool isRWLock(llvm::Value *val);
    bool isSemaphore(llvm::Value *val);
    bool isBarrier(llvm::Value *val);
    bool isAtomicInt(llvm::Value *val);
    bool isAtomicBool(llvm::Value *val);
    void propagateResourceTracking(llvm::Value *src, llvm::Value *dst);
    void propagateResourceTrackingWide(llvm::Value *src, llvm::Value *dst);
    void propagateCollectionMetadata(llvm::Value *src, llvm::Value *dst);
    void propagateAllMetadata(llvm::Value *src, llvm::Value *dst);
    void propagateAllMetadataWide(llvm::Value *src, llvm::Value *dst);
    void registerResourceByTypeName(const std::string &typeName, llvm::Value *val);
    // Shared Result-wrapping helpers for stdlib dispatchers
    llvm::Value *emitResultBranch(llvm::Value *isErr, llvm::StructType *resTy,
                                   std::function<llvm::Value*()> buildOk,
                                   std::function<llvm::Value*()> buildErr);
    llvm::Value *buildErrorFromRuntime(const char *errFnName = "__ry_get_last_error");
    llvm::Value *wrapPtrAsResult(llvm::Value *ptr, const char *errFnName = "__ry_get_last_error");
    llvm::Value *wrapStatusAsResult(llvm::Value *status, const char *errFnName = "__ry_get_last_error");

    std::unordered_set<llvm::Value*> resource_sets_[RK_COUNT];

    llvm::Value *emitPtrToResult(llvm::Value *ptr, const std::string &name,
                                 const std::string &errMsg, ResourceKind rk);
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
    std::vector<llvm::Value*> coerceCallArgs(const FnTypeInfo &info,
                                             std::vector<llvm::Value*> args,
                                             const std::string &context);
    llvm::Value *emitLambdaCall(llvm::Value *lambdaVal, const FnTypeInfo &info,
                                std::vector<llvm::Value*> args, const std::string &name);

    // Return type inference
    llvm::Type *inferExprType(const ExprNode &expr,
        const std::unordered_map<std::string, llvm::Type*> &paramTypeMap);
    llvm::Type *inferReturnType(const std::vector<StmtNode> &body,
        const std::unordered_map<std::string, llvm::Type*> &paramTypeMap);
    void collectReturnTypes(const std::vector<StmtNode> &body,
        const std::unordered_map<std::string, llvm::Type*> &paramTypeMap,
        std::vector<llvm::Type*> &out);
    llvm::Type *deduceReturnType(const std::vector<llvm::Type*> &types);
    std::string reverseResolveTypeName(llvm::Type *ty);

    // Union type helpers
    std::vector<std::string> parseUnionComponents(const std::string &typeName);
    std::string normalizeUnionType(const std::string &typeName);
    bool isUnionType(const std::string &typeName);
    llvm::Value *wrapInUnion(llvm::Value *val, const std::string &unionTypeName);

    int64_t getAnyTypeTag(llvm::Type *ty);
    llvm::Value *wrapInAny(llvm::Value *val);
    llvm::Value *buildUnitAny();
    llvm::Value *unwrapFromAny(llvm::Value *anyVal, llvm::Type *targetTy);
    bool isAnyType(llvm::Type *ty) const;
    bool canAnyHoldType(llvm::Type *ty) const;
    bool isNonStrPointer(llvm::Value *val);
    llvm::Value *emitAnyToString(llvm::Value *anyVal);
};
