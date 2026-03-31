#include "ry/codegen.hpp"
#include "ry/coverage_runtime.hpp"
#include "ry/diagnostic.hpp"
#include <climits>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

CodeGen::CodeGen(bool test_mode, const SourceManager *sm, bool coverage_mode,
                 int coverage_file_id_offset, bool outline_mode)
    : ctx_(std::make_unique<llvm::LLVMContext>()),
      mod_(std::make_unique<llvm::Module>("ry", *ctx_)),
      builder_(*ctx_),
      test_mode_(test_mode),
      outline_mode_(outline_mode),
      coverage_mode_(coverage_mode),
      coverage_file_id_offset_(coverage_file_id_offset),
      sm_(sm) {
    i64Ty_ = llvm::Type::getInt64Ty(*ctx_);
    i32Ty_ = llvm::Type::getInt32Ty(*ctx_);
    i16Ty_ = llvm::Type::getInt16Ty(*ctx_);
    i8Ty_  = llvm::Type::getInt8Ty(*ctx_);
    f64Ty_ = llvm::Type::getDoubleTy(*ctx_);
    f32Ty_ = llvm::Type::getFloatTy(*ctx_);
    i1Ty_  = llvm::Type::getInt1Ty(*ctx_);
    ptrTy_ = llvm::PointerType::getUnqual(*ctx_);

    builtins_["print"] = [this](const std::vector<ExprPtr> &args) { emitPrint(args); };
    builtins_["exit"] = [this](const std::vector<ExprPtr> &args) { emitExit(args); };

    errorTy_ = llvm::StructType::create(*ctx_, {ptrTy_, i64Ty_}, "Error");
    {
        std::vector<FieldDef> errorFields;
        errorFields.push_back({"message", TypeNode::makeBasic("str"), {}});
        errorFields.push_back({"code", TypeNode::makeBasic("int"), {}});
        struct_types_["Error"] = {errorTy_, std::move(errorFields), {}, ""};
    }

    listHeaderTy_ = llvm::StructType::create(*ctx_, {i64Ty_, i64Ty_, ptrTy_}, "ListHeader");
    mapHeaderTy_ = llvm::StructType::create(*ctx_, {i64Ty_, i64Ty_, ptrTy_, ptrTy_, i64Ty_, ptrTy_}, "MapHeader");
    setHeaderTy_ = llvm::StructType::create(*ctx_, {i64Ty_, i64Ty_, ptrTy_, i64Ty_, ptrTy_}, "SetHeader");
    iteratorHeaderTy_ = llvm::StructType::create(*ctx_, {ptrTy_, ptrTy_}, "IteratorHeader");
    arcHeaderTy_ = llvm::StructType::create(*ctx_, {i64Ty_, i64Ty_}, "ArcHeader");

    anyTy_ = llvm::StructType::create(
        *ctx_, {i64Ty_, llvm::ArrayType::get(i8Ty_, 8)}, "Any");

    fnTy_ptr_to_ptr_       = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
    fnTy_ptr_to_i64_       = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
    fnTy_ptr_to_void_      = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
    fnTy_ptr_ptr_to_ptr_   = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
    fnTy_ptr_ptr_to_i64_   = llvm::FunctionType::get(i64Ty_, {ptrTy_, ptrTy_}, false);
    fnTy_ptr_i64_to_ptr_   = llvm::FunctionType::get(ptrTy_, {ptrTy_, i64Ty_}, false);
    fnTy_ptr_ptr_ptr_to_ptr_ = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, ptrTy_}, false);
    fnTy_void_to_ptr_      = llvm::FunctionType::get(ptrTy_, {}, false);
}

llvm::FunctionCallee CodeGen::getRuntimeFn(const char *name, llvm::Type *retTy,
                                            llvm::ArrayRef<llvm::Type*> argTys) {
    auto *fnTy = llvm::FunctionType::get(retTy, argTys, false);
    return mod_->getOrInsertFunction(name, fnTy);
}

llvm::Constant *CodeGen::cachedGlobalString(const std::string &str, const llvm::Twine &name) {
    auto it = global_string_cache_.find(str);
    if (it != global_string_cache_.end()) return it->second;

    // Create global with ARC header prefix: { i64 INT64_MAX, i64 0, [N+1 x i8] "...\0" }
    // The returned pointer points to the string data (after ARC header),
    // so existing code uses it as char*. If ARC retain/release is called,
    // ptr-16 leads to the immortal sentinel (INT64_MAX) which skips the op.
    auto *strData = llvm::ConstantDataArray::getString(*ctx_, str);
    auto *wrapTy = llvm::StructType::get(
        *ctx_, {i64Ty_, i64Ty_, strData->getType()});
    auto *initVal = llvm::ConstantStruct::get(wrapTy,
        {llvm::ConstantInt::get(i64Ty_, ARC_IMMORTAL),
         llvm::ConstantInt::get(i64Ty_, 0),
         strData});
    auto *gv = new llvm::GlobalVariable(
        *mod_, wrapTy, /*isConstant=*/true,
        llvm::GlobalValue::PrivateLinkage, initVal,
        name + ".arc");
    gv->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    gv->setAlignment(llvm::Align(8));

    // GEP to the string data part (index 2, then index 0 for first byte)
    auto *zero = llvm::ConstantInt::get(i64Ty_, 0);
    auto *idx2 = llvm::ConstantInt::get(i32Ty_, 2);
    auto *gs = llvm::ConstantExpr::getInBoundsGetElementPtr(
        wrapTy, gv, llvm::ArrayRef<llvm::Constant*>{zero, idx2, zero});

    global_string_cache_[str] = gs;
    return gs;
}

// ===== B5: FnScope RAII =====

CodeGen::FnScope::FnScope(CodeGen &cg) : cg_(cg) {
    savedFn_ = cg_.fn_;
    savedScope_ = std::move(cg_.scope_stack_);
    savedConstScope_ = std::move(cg_.immutable_scope_stack_);
    savedArcManaged_ = std::move(cg_.arc_managed_vars_);
    savedArcOwned_ = std::move(cg_.arc_owned_values_);
    savedArcBacked_ = std::move(cg_.arc_backed_vars_);
    savedWeakManaged_ = std::move(cg_.weak_managed_vars_);
    savedWeakInnerTypeNames_ = std::move(cg_.weak_inner_type_names_);
    savedResourceManaged_ = std::move(cg_.resource_managed_vars_);
    savedClosureManaged_ = std::move(cg_.closure_managed_vars_);
    savedBlock_ = cg_.builder_.GetInsertBlock();
    savedPoint_ = cg_.builder_.GetInsertPoint();
    savedPostconditions_ = cg_.current_postconditions_;
    savedEnsureBindings_ = cg_.ensure_bindings_;
    savedInEnsureContext_ = cg_.in_ensure_context_;
    savedFnReturnType_ = std::move(cg_.current_fn_return_type_);
    savedFnName_ = std::move(cg_.current_function_name_);
    cg_.scope_stack_.clear();
    cg_.immutable_scope_stack_.clear();
    cg_.arc_managed_vars_.clear();
    cg_.arc_owned_values_.clear();
    cg_.weak_managed_vars_.clear();
    cg_.weak_inner_type_names_.clear();
    cg_.resource_managed_vars_.clear();
    cg_.closure_managed_vars_.clear();
    cg_.current_postconditions_ = nullptr;
    cg_.ensure_bindings_ = nullptr;
    cg_.in_ensure_context_ = false;
    cg_.current_function_name_.clear();
}

CodeGen::FnScope::~FnScope() {
    cg_.fn_ = savedFn_;
    cg_.scope_stack_ = std::move(savedScope_);
    cg_.immutable_scope_stack_ = std::move(savedConstScope_);
    cg_.arc_managed_vars_ = std::move(savedArcManaged_);
    cg_.arc_owned_values_ = std::move(savedArcOwned_);
    cg_.arc_backed_vars_ = std::move(savedArcBacked_);
    cg_.weak_managed_vars_ = std::move(savedWeakManaged_);
    cg_.weak_inner_type_names_ = std::move(savedWeakInnerTypeNames_);
    cg_.resource_managed_vars_ = std::move(savedResourceManaged_);
    cg_.closure_managed_vars_ = std::move(savedClosureManaged_);
    cg_.builder_.SetInsertPoint(savedBlock_, savedPoint_);
    cg_.current_postconditions_ = savedPostconditions_;
    cg_.ensure_bindings_ = savedEnsureBindings_;
    cg_.in_ensure_context_ = savedInEnsureContext_;
    cg_.current_fn_return_type_ = std::move(savedFnReturnType_);
    cg_.current_function_name_ = std::move(savedFnName_);
}

// ===== Coverage instrumentation =====

void CodeGen::emitCoverage(const SourceLocation &loc) {
    if (!coverage_mode_ || loc.line <= 0) return;
    int32_t gid = loc.file_id + coverage_file_id_offset_;

    // Register line at compile time (deduplicated)
    int64_t key = (static_cast<int64_t>(gid) << 32) | loc.line;
    if (registered_coverage_lines_.insert(key).second)
        __ry_coverage_register_line(gid, loc.line);

    // Emit runtime hit call
    auto *ft = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {i32Ty_, i32Ty_}, false);
    auto hitFn = mod_->getOrInsertFunction("__ry_coverage_hit", ft);
    builder_.CreateCall(hitFn,
        {llvm::ConstantInt::get(i32Ty_, gid),
         llvm::ConstantInt::get(i32Ty_, loc.line)});
}

void CodeGen::emitTraceSymbolDefine(const std::string &kind, const std::string &name,
                                    const SourceLocation &loc) {
    if (!ry::traceEnabled()) return;
    // Skip stdlib symbols to reduce noise — users care about their own definitions
    if (sm_ && loc.file_id >= 0 && loc.file_id < sm_->getFileCount()) {
        const auto &fname = sm_->getFilename(loc.file_id);
        if (fname.find("/lib/std/") != std::string::npos)
            return;
    }
    ry::emitTraceEvent("symbol.define", "compile", &loc,
                       {ry::TraceField("kind", kind),
                        ry::TraceField("symbol", name),
                        ry::TraceField("file", sm_ ? sm_->getFilename(loc.file_id) : "")});
}

llvm::Value *CodeGen::emitTraceSourceString(const std::string &text) {
    return cachedGlobalString(text, ".trace_str");
}

llvm::Value *CodeGen::emitTraceFileString(const SourceLocation &loc) {
    if (!sm_ || loc.file_id < 0 || loc.file_id >= sm_->getFileCount())
        return emitTraceSourceString("");
    return emitTraceSourceString(sm_->getFilename(loc.file_id));
}

void CodeGen::emitTraceFunctionEnter(const std::string &fnName, const SourceLocation &loc) {
    if (!ry::traceEnabled()) return;
    auto callee = mod_->getOrInsertFunction(
        "__ry_trace_function_enter",
        llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_),
                                {ptrTy_, ptrTy_, i32Ty_, i32Ty_}, false));
    builder_.CreateCall(callee, {emitTraceSourceString(fnName),
                                 emitTraceFileString(loc),
                                 llvm::ConstantInt::get(i32Ty_, loc.line),
                                 llvm::ConstantInt::get(i32Ty_, loc.col)});
}

void CodeGen::emitTraceFunctionExit(const std::string &fnName, const SourceLocation &loc) {
    if (!ry::traceEnabled()) return;
    auto callee = mod_->getOrInsertFunction(
        "__ry_trace_function_exit",
        llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_),
                                {ptrTy_, ptrTy_, i32Ty_, i32Ty_}, false));
    builder_.CreateCall(callee, {emitTraceSourceString(fnName),
                                 emitTraceFileString(loc),
                                 llvm::ConstantInt::get(i32Ty_, loc.line),
                                 llvm::ConstantInt::get(i32Ty_, loc.col)});
}

void CodeGen::emitTraceReturn(const SourceLocation &loc) {
    if (!ry::traceEnabled()) return;
    auto callee = mod_->getOrInsertFunction(
        "__ry_trace_return",
        llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_),
                                {ptrTy_, ptrTy_, i32Ty_, i32Ty_}, false));
    builder_.CreateCall(callee, {emitTraceSourceString(current_function_name_),
                                 emitTraceFileString(loc),
                                 llvm::ConstantInt::get(i32Ty_, loc.line),
                                 llvm::ConstantInt::get(i32Ty_, loc.col)});
}

void CodeGen::emitTraceIfBranch(llvm::Value *cond, const SourceLocation &loc) {
    if (!ry::traceEnabled()) return;
    auto callee = mod_->getOrInsertFunction(
        "__ry_trace_branch_if",
        llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_),
                                {ptrTy_, i32Ty_, i32Ty_, i32Ty_}, false));
    llvm::Value *taken = builder_.CreateZExt(cond, i32Ty_, "trace_if_taken");
    builder_.CreateCall(callee, {emitTraceFileString(loc),
                                 llvm::ConstantInt::get(i32Ty_, loc.line),
                                 llvm::ConstantInt::get(i32Ty_, loc.col),
                                 taken});
}

void CodeGen::emitTraceWhenBranch(int armIndex, const SourceLocation &loc) {
    if (!ry::traceEnabled()) return;
    auto callee = mod_->getOrInsertFunction(
        "__ry_trace_branch_when",
        llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_),
                                {ptrTy_, i32Ty_, i32Ty_, i32Ty_}, false));
    builder_.CreateCall(callee, {emitTraceFileString(loc),
                                 llvm::ConstantInt::get(i32Ty_, loc.line),
                                 llvm::ConstantInt::get(i32Ty_, loc.col),
                                 llvm::ConstantInt::get(i32Ty_, armIndex)});
}

// ===== Error helpers =====

[[noreturn]] void CodeGen::codegenError(const SourceLocation &loc, const std::string &msg) {
    throw DiagnosticError(loc, msg, sm_);
}

[[noreturn]] void CodeGen::codegenError(const std::string &msg) {
    codegenError(current_loc_, msg);
}

// ===== Scope management =====

void CodeGen::pushScope() {
    scope_stack_.emplace_back();
    immutable_scope_stack_.emplace_back();
}

void CodeGen::popScope() {
    emitScopeCleanup();
    scope_stack_.pop_back();
    immutable_scope_stack_.pop_back();
}

void CodeGen::emitScopeCleanup() {
    if (scope_stack_.empty()) return;
    emitScopeCleanupToDepth(scope_stack_.size() - 1);
}

void CodeGen::emitScopeCleanupToDepth(size_t targetDepth) {
    for (size_t i = scope_stack_.size(); i > targetDepth; --i) {
        auto &scope = scope_stack_[i - 1];
        for (auto &[name, alloca] : scope) {
            if (weak_managed_vars_.count(alloca)) {
                emitWeakReleaseVar(name, alloca);
                weak_managed_vars_.erase(alloca);
                weak_inner_type_names_.erase(alloca);
                continue;
            }
            if (!arc_managed_vars_.count(alloca)) continue;
            emitArcReleaseVar(name, alloca);
            arc_managed_vars_.erase(alloca);
        }
    }
}

llvm::AllocaInst *CodeGen::findVar(const std::string &name) {
    for (auto it = scope_stack_.rbegin(); it != scope_stack_.rend(); ++it) {
        auto found = it->find(name);
        if (found != it->end())
            return found->second;
    }
    return nullptr;
}

bool CodeGen::isImmutable(const std::string &name) const {
    for (auto it = immutable_scope_stack_.rbegin(); it != immutable_scope_stack_.rend(); ++it) {
        if (it->count(name))
            return true;
    }
    return false;
}

// Pre-pass: collect all function names targeted by mock() in the AST
static void collectMockedFunctions(const std::vector<StmtNode> &stmts,
                                    std::unordered_set<std::string> &out) {
    for (const auto &stmt : stmts) {
        std::visit([&](const auto &s) {
            using T = std::decay_t<decltype(s)>;
            if constexpr (std::is_same_v<T, CallStmt>) {
                if (s.callee == "mock" && !s.args.empty()) {
                    if (auto *str = std::get_if<StringExpr>(&s.args[0]->data))
                        out.insert(str->value);
                }
            } else if constexpr (std::is_same_v<T, std::unique_ptr<IfStmt>>) {
                collectMockedFunctions(s->branch.body, out);
                collectMockedFunctions(s->else_body, out);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<WhenCondStmt>>) {
                for (auto &arm : s->arms)
                    collectMockedFunctions(arm.body, out);
                collectMockedFunctions(s->else_body, out);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<WhileStmt>>) {
                collectMockedFunctions(s->body, out);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<ForStmt>>) {
                collectMockedFunctions(s->body, out);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<FnStmt>>) {
                collectMockedFunctions(s->body, out);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<WhenMatchStmt>>) {
                for (auto &arm : s->arms)
                    collectMockedFunctions(arm.body, out);
            }
        }, stmt);
        // Also scan lambda args (e.g., describe/it trailing blocks)
        std::visit([&](const auto &s) {
            using T = std::decay_t<decltype(s)>;
            if constexpr (std::is_same_v<T, CallStmt>) {
                for (auto &arg : s.args) {
                    if (auto *lam = std::get_if<std::unique_ptr<LambdaExpr>>(&arg->data))
                        collectMockedFunctions((*lam)->body, out);
                }
            }
        }, stmt);
    }
}

llvm::orc::ThreadSafeModule CodeGen::compile(Program &prog) {
    // Pre-pass: collect all mock targets before codegen
    if (test_mode_)
        collectMockedFunctions(prog, mocked_functions_);

    // Pre-pass: compute potentially cyclic types for GC candidate tracking
    computeCyclicTypes(prog);

    llvm::FunctionType *ft = llvm::FunctionType::get(i32Ty_, false);
    fn_ = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "__ry_main__", *mod_);
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(*ctx_, "entry", fn_);
    builder_.SetInsertPoint(bb);

    pushScope();

    for (auto &stmt : prog) {
        std::visit([this](auto &s) { emitStmt(s); }, stmt);
    }

    if (!builder_.GetInsertBlock()->getTerminator()) {
        if (test_mode_ && !outline_mode_) {
            // Call __ry_test_summary() and return its result as exit code
            llvm::FunctionType *summaryTy = llvm::FunctionType::get(i32Ty_, false);
            llvm::FunctionCallee summaryFn = mod_->getOrInsertFunction("__ry_test_summary", summaryTy);
            llvm::Value *result = builder_.CreateCall(summaryFn, {}, "test_result");
            builder_.CreateRet(result);
        } else {
            builder_.CreateRet(llvm::ConstantInt::get(i32Ty_, 0));
        }
    }

    std::string err;
    llvm::raw_string_ostream errStream(err);
    if (llvm::verifyFunction(*fn_, &errStream))
        codegenError("IR verify error: " + err);

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

// ===== Low-level type helpers =====

const std::string &CodeGen::getLowLevelTypeName(llvm::Value *val) const {
    static const std::string empty;
    auto it = low_level_type_names_.find(val);
    if (it != low_level_type_names_.end()) return it->second;
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val)) {
        auto it2 = low_level_type_names_.find(load->getPointerOperand());
        if (it2 != low_level_type_names_.end()) return it2->second;
    }
    return empty;
}

bool CodeGen::isUnsignedLowLevel(llvm::Value *val) const {
    std::string name = getLowLevelTypeName(val);
    return isUnsignedLowLevelName(name);
}

bool CodeGen::isUnsignedLowLevelName(const std::string &name) {
    return !name.empty() && name[0] == 'u';
}

bool CodeGen::isLowLevelTypeName(const std::string &name) {
    return name == "i8" || name == "i16" || name == "i32" || name == "i64" ||
           name == "u8" || name == "u16" || name == "u32" || name == "u64" || name == "f32";
}

std::string CodeGen::getExprLowLevelSuffix(const ExprNode &node) {
    if (auto *ne = std::get_if<NumberExpr>(&node.data)) {
        if (isLowLevelTypeName(ne->suffix)) return ne->suffix;
    }
    if (auto *fe = std::get_if<FloatExpr>(&node.data)) {
        if (isLowLevelTypeName(fe->suffix)) return fe->suffix;
    }
    return "";
}

bool CodeGen::isLowLevelIntTy(llvm::Type *ty) const {
    return ty == i16Ty_ || ty == i32Ty_;
}

bool CodeGen::isLowLevelIntTy(llvm::Value *val) const {
    const std::string &name = getLowLevelTypeName(val);
    if (!name.empty()) return name != "f32";
    return isLowLevelIntTy(val->getType());
}

bool CodeGen::isLowLevelFloatTy(llvm::Type *ty) const {
    return ty == f32Ty_;
}

bool CodeGen::isLowLevelTy(llvm::Type *ty) const {
    return isLowLevelIntTy(ty) || isLowLevelFloatTy(ty);
}

bool CodeGen::isLowLevelTy(llvm::Value *val) const {
    return isLowLevelIntTy(val) || isLowLevelFloatTy(val->getType());
}

void CodeGen::checkLowLevelTypeMix(llvm::Value *lhs, llvm::Value *rhs, const std::string &op) {
    std::string lhsName = getLowLevelTypeName(lhs);
    std::string rhsName = getLowLevelTypeName(rhs);
    bool lhsLL = !lhsName.empty() || isLowLevelTy(lhs->getType());
    bool rhsLL = !rhsName.empty() || isLowLevelTy(rhs->getType());
    if (lhsLL || rhsLL) {
        if (lhs->getType() != rhs->getType()) {
            codegenError("type error: cannot mix types in operator '" + op +
                         "'; low-level numeric types require explicit 'as' cast");
        }
        // Both have metadata: must match (e.g., u32 vs i32 is an error)
        if (!lhsName.empty() && !rhsName.empty() && lhsName != rhsName) {
            codegenError("type error: cannot mix types in operator '" + op +
                         "'; low-level numeric types require explicit 'as' cast");
        }
    }
}

// ===== B1: Type promotion helpers =====

void CodeGen::ensureNumericType(llvm::Value *v, const std::string &context) {
    llvm::Type *ty = v->getType();
    if (ty->isStructTy())
        codegenError("type error: " + context + " requires numeric type, got struct");
    if (ty->isPointerTy())
        codegenError("type error: " + context + " requires numeric type, got pointer");
}

llvm::Value *CodeGen::promoteToInt(llvm::Value *v) {
    ensureNumericType(v, "numeric operation");
    if (v->getType() == i1Ty_)
        return builder_.CreateZExt(v, i64Ty_, "boolext");
    if (v->getType() == i8Ty_ && !isLowLevelIntTy(v))
        return builder_.CreateZExt(v, i64Ty_, "u8ext");
    return v;
}

std::pair<llvm::Value*, llvm::Value*> CodeGen::promoteToFloat(llvm::Value *lhs, llvm::Value *rhs) {
    ensureNumericType(lhs, "numeric operation");
    ensureNumericType(rhs, "numeric operation");
    if (!lhs->getType()->isDoubleTy() && !isLowLevelTy(lhs)) {
        if (lhs->getType() == i8Ty_)
            lhs = builder_.CreateUIToFP(lhs, f64Ty_, "lhs_f");
        else
            lhs = builder_.CreateSIToFP(lhs, f64Ty_, "lhs_f");
    }
    if (!rhs->getType()->isDoubleTy() && !isLowLevelTy(rhs)) {
        if (rhs->getType() == i8Ty_)
            rhs = builder_.CreateUIToFP(rhs, f64Ty_, "rhs_f");
        else
            rhs = builder_.CreateSIToFP(rhs, f64Ty_, "rhs_f");
    }
    return {lhs, rhs};
}


// ===== B3.4: Record subtype helpers =====

bool CodeGen::isSubtypeOf(const std::string &childType, const std::string &parentType) const {
    std::string current = childType;
    while (!current.empty()) {
        auto it = struct_types_.find(current);
        if (it == struct_types_.end()) return false;
        if (it->second.parent_name == parentType) return true;
        current = it->second.parent_name;
    }
    return false;
}

llvm::Value *CodeGen::emitSubtypeSlice(llvm::Value *childVal,
                                         const std::string &childTypeName,
                                         const std::string &parentTypeName) {
    auto pit = struct_types_.find(parentTypeName);
    if (pit == struct_types_.end())
        codegenError("unknown parent type: " + parentTypeName);
    llvm::Value *result = llvm::UndefValue::get(pit->second.llvmType);
    for (unsigned i = 0; i < pit->second.fields.size(); ++i) {
        llvm::Value *field = builder_.CreateExtractValue(childVal, i, "slice." + std::to_string(i));
        result = builder_.CreateInsertValue(result, field, i);
    }
    return result;
}

llvm::Value *CodeGen::tryEmitSubtypeCoerce(llvm::Value *val, llvm::Type *targetTy) {
    auto *argST = llvm::dyn_cast<llvm::StructType>(val->getType());
    auto *paramST = llvm::dyn_cast<llvm::StructType>(targetTy);
    if (!argST || !paramST) return nullptr;
    std::string argName = argST->getName().str();
    std::string paramName = paramST->getName().str();
    if (!isSubtypeOf(argName, paramName)) return nullptr;
    return emitSubtypeSlice(val, argName, paramName);
}

// ===== B3.5: Implicit widening conversion helpers =====

bool CodeGen::isWideningConversion(llvm::Value *argVal, llvm::Type *paramTy,
                                   const std::string &paramTypeName) const {
    // Block low-level types except i8/u8 (u8 is the successor of byte)
    if (isLowLevelTy(argVal) && argVal->getType() != i8Ty_) return false;
    if (isLowLevelTypeName(paramTypeName)) return false;
    auto *argTy = argVal->getType();
    return (argTy == i8Ty_  && paramTy == i64Ty_) ||   // u8 -> int
           (argTy == i8Ty_  && paramTy == f64Ty_) ||   // u8 -> float
           (argTy == i64Ty_ && paramTy == f64Ty_);     // int  -> float
}

llvm::Value *CodeGen::emitWideningConversion(llvm::Value *argVal, llvm::Type *paramTy) {
    auto *argTy = argVal->getType();
    if (argTy == i8Ty_ && paramTy == i64Ty_) {
        std::string name = getLowLevelTypeName(argVal);
        if (name == "i8")
            return builder_.CreateSExt(argVal, i64Ty_, "i8_to_int");
        return builder_.CreateZExt(argVal, i64Ty_, "u8_to_int");
    }
    if (argTy == i8Ty_ && paramTy == f64Ty_) {
        std::string name = getLowLevelTypeName(argVal);
        if (name == "i8")
            return builder_.CreateSIToFP(argVal, f64Ty_, "i8_to_float");
        return builder_.CreateUIToFP(argVal, f64Ty_, "u8_to_float");
    }
    if (argTy == i64Ty_ && paramTy == f64Ty_)
        return builder_.CreateSIToFP(argVal, f64Ty_, "int_to_float");
    llvm_unreachable("invalid widening conversion");
}

// ===== B4: emitUserFnCall =====

llvm::Function *CodeGen::resolveOverload(const std::string &callee,
                                          const std::vector<ExprPtr> &args,
                                          std::vector<llvm::Value*> &outArgVals) {
    auto fit = functions_.find(callee);
    if (fit == functions_.end()) {
        if (native_fn_arg_counts_.count(callee)) {
            codegenError("@native function '" + callee +
                "' is declared but not handled by any builtin dispatcher. "
                "Did you forget to add a case in codegen_call_*.cpp "
                "or add emitBuiltin*() to the dispatch chain in codegen_call.cpp?");
        } else {
            codegenError("undefined function: " + callee);
        }
    }

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

    struct RankedCandidate {
        OverloadEntry *entry;
        int exactMatches = 0;
        int subtypeMatches = 0;
        int wideningMatches = 0;
        int unionMatches = 0;
        int anyMatches = 0;
        int defaultsUsed = 0;
    };

    auto isBetterCandidate = [](const RankedCandidate &lhs, const RankedCandidate &rhs) {
        if (lhs.exactMatches != rhs.exactMatches)
            return lhs.exactMatches > rhs.exactMatches;
        if (lhs.subtypeMatches != rhs.subtypeMatches)
            return lhs.subtypeMatches > rhs.subtypeMatches;
        if (lhs.wideningMatches != rhs.wideningMatches)
            return lhs.wideningMatches > rhs.wideningMatches;
        if (lhs.unionMatches != rhs.unionMatches)
            return lhs.unionMatches > rhs.unionMatches;
        if (lhs.anyMatches != rhs.anyMatches)
            return lhs.anyMatches < rhs.anyMatches;
        if (lhs.defaultsUsed != rhs.defaultsUsed)
            return lhs.defaultsUsed < rhs.defaultsUsed;
        return false;
    };

    // Filter and rank candidates
    std::vector<RankedCandidate> candidates;
    for (auto &entry : overloads) {
        if (args.size() < entry.minArity || args.size() > entry.paramTypes.size())
            continue;
        bool match = true;
        RankedCandidate candidate{&entry, 0, 0, 0, 0, 0,
                                  static_cast<int>(entry.paramTypes.size() - args.size())};
        for (size_t i = 0; i < args.size(); ++i) {
            std::string resolvedParamTypeName =
                i < entry.paramTypeNames.size() ? resolveTypeAlias(entry.paramTypeNames[i]) : "";
            if (isNone[i]) {
                if (!isOptionType(entry.paramTypes[i])) { match = false; break; }
                continue;
            }

            if (emittedArgs[i]->getType() == entry.paramTypes[i]) {
                candidate.exactMatches++;
                continue;
            }

            if (auto *argST = llvm::dyn_cast<llvm::StructType>(emittedArgs[i]->getType())) {
                if (auto *paramST = llvm::dyn_cast<llvm::StructType>(entry.paramTypes[i])) {
                    if (isSubtypeOf(argST->getName().str(), paramST->getName().str())) {
                        candidate.subtypeMatches++;
                        continue;
                    }
                }
            }

            if (isWideningConversion(emittedArgs[i], entry.paramTypes[i], resolvedParamTypeName)) {
                candidate.wideningMatches++;
                continue;
            }

            if (isAnyType(entry.paramTypes[i])) {
                // Match: any type accepts all primitives; wrapping deferred to arg building
                candidate.anyMatches++;
            } else if (isUnionType(resolvedParamTypeName)) {
                std::string norm = normalizeUnionType(resolvedParamTypeName);
                auto uIt = union_type_info_.find(norm);
                if (uIt != union_type_info_.end()) {
                    bool found = false;
                    for (auto *ct : uIt->second.componentTypes) {
                        if (ct == emittedArgs[i]->getType()) { found = true; break; }
                    }
                    if (!found) { match = false; break; }
                    candidate.unionMatches++;
                } else { match = false; break; }
            } else if (isAnyType(emittedArgs[i]->getType()) &&
                       canAnyHoldType(entry.paramTypes[i])) {
                // Matching a concrete parameter from an any-typed value requires runtime unwrap,
                // so treat it with the same low specificity as an any fallback.
                candidate.anyMatches++;
            } else { match = false; break; }
        }
        if (match)
            candidates.push_back(candidate);
    }

    if (candidates.empty())
        codegenError("no matching overload for '" + callee + "'");

    RankedCandidate *best = &candidates[0];
    bool ambiguous = false;
    for (size_t i = 1; i < candidates.size(); ++i) {
        if (isBetterCandidate(candidates[i], *best)) {
            best = &candidates[i];
            ambiguous = false;
        } else if (!isBetterCandidate(*best, candidates[i])) {
            ambiguous = true;
        }
    }

    if (ambiguous)
        codegenError("ambiguous call to '" + callee + "'");

    auto *chosen = best->entry;

    // Build final arg values (fill in None args with proper Option type, wrap union args)
    outArgVals.clear();
    for (size_t i = 0; i < args.size(); ++i) {
        std::string resolvedParamTypeName =
            i < chosen->paramTypeNames.size() ? resolveTypeAlias(chosen->paramTypeNames[i]) : "";
        if (isNone[i]) {
            outArgVals.push_back(buildNoneValue(chosen->paramTypes[i]));
        } else if (emittedArgs[i]->getType() != chosen->paramTypes[i] &&
                   isWideningConversion(emittedArgs[i], chosen->paramTypes[i], resolvedParamTypeName)) {
            outArgVals.push_back(emitWideningConversion(emittedArgs[i], chosen->paramTypes[i]));
        } else if (auto *sliced = tryEmitSubtypeCoerce(emittedArgs[i], chosen->paramTypes[i])) {
            outArgVals.push_back(sliced);
        } else if (emittedArgs[i]->getType() != chosen->paramTypes[i] &&
                   isAnyType(chosen->paramTypes[i])) {
            outArgVals.push_back(wrapInAny(emittedArgs[i]));
        } else if (isAnyType(emittedArgs[i]->getType()) &&
                   emittedArgs[i]->getType() != chosen->paramTypes[i]) {
            outArgVals.push_back(unwrapFromAny(emittedArgs[i], chosen->paramTypes[i]));
        } else if (emittedArgs[i]->getType() != chosen->paramTypes[i] &&
                   isUnionType(resolvedParamTypeName)) {
            outArgVals.push_back(wrapInUnion(emittedArgs[i], resolvedParamTypeName));
        } else {
            outArgVals.push_back(emittedArgs[i]);
        }
    }

    // Fill in default values for omitted parameters
    for (size_t i = args.size(); i < chosen->paramTypes.size(); ++i) {
        bool isNoneLit = std::holds_alternative<NoneExpr>(chosen->defaultValues[i]->data) ||
                         (std::holds_alternative<VariableExpr>(chosen->defaultValues[i]->data) &&
                          std::get<VariableExpr>(chosen->defaultValues[i]->data).name == "None");
        if (isNoneLit) {
            outArgVals.push_back(buildNoneValue(chosen->paramTypes[i]));
            continue;
        }
        llvm::Value *defVal = emitExpr(*chosen->defaultValues[i]);
        std::string resolvedParamTypeName =
            i < chosen->paramTypeNames.size() ? resolveTypeAlias(chosen->paramTypeNames[i]) : "";
        if (defVal->getType() != chosen->paramTypes[i] &&
            isWideningConversion(defVal, chosen->paramTypes[i], resolvedParamTypeName)) {
            outArgVals.push_back(emitWideningConversion(defVal, chosen->paramTypes[i]));
        } else if (auto *sliced = tryEmitSubtypeCoerce(defVal, chosen->paramTypes[i])) {
            outArgVals.push_back(sliced);
        } else if (defVal->getType() != chosen->paramTypes[i] &&
                   isAnyType(chosen->paramTypes[i])) {
            outArgVals.push_back(wrapInAny(defVal));
        } else if (isAnyType(defVal->getType()) &&
                   defVal->getType() != chosen->paramTypes[i]) {
            outArgVals.push_back(unwrapFromAny(defVal, chosen->paramTypes[i]));
        } else if (defVal->getType() != chosen->paramTypes[i] &&
                   isUnionType(resolvedParamTypeName)) {
            outArgVals.push_back(wrapInUnion(defVal, resolvedParamTypeName));
        } else {
            outArgVals.push_back(defVal);
        }
    }

    return chosen->func;
}

llvm::Value *CodeGen::emitUserFnCall(const std::string &callee, const std::vector<ExprPtr> &args) {
    if (deprecated_functions_.count(callee))
        emitDeprecationWarning(callee);
    std::vector<llvm::Value*> argVals;
    llvm::Function *fn = resolveOverload(callee, args, argVals);

    // Find the matching overload entry (single scan for constraints + result type)
    OverloadEntry *matchedEntry = nullptr;
    auto fit = functions_.find(callee);
    if (fit != functions_.end()) {
        for (auto &entry : fit->second) {
            if (entry.func == fn) { matchedEntry = &entry; break; }
        }
    }

    // ARC: retain arguments that are ARC-managed before passing to callee
    for (auto *argVal : argVals)
        tryRetainArcSource(argVal);

    // Check literal/range constraints on arguments at call site
    if (matchedEntry) {
        for (size_t i = 0; i < matchedEntry->paramTypeNames.size() && i < argVals.size(); ++i) {
            std::string resolvedPtype = resolveTypeAlias(matchedEntry->paramTypeNames[i]);
            auto constraint = parseTypeConstraint(resolvedPtype);
            if (constraint) {
                std::string paramName = fn->getArg(i)->getName().str();
                emitConstraintCheck(argVals[i], *constraint, paramName);
            }
        }
    }

    auto bindContractValue = [&](const std::string &name, llvm::Value *val,
                                 const std::string *typeName) {
        llvm::AllocaInst *alloca = builder_.CreateAlloca(val->getType(), nullptr, name);
        builder_.CreateStore(val, alloca);
        scope_stack_.back()[name] = alloca;
        immutable_scope_stack_.back().insert(name);
        if (!typeName) return;

        std::string resolvedType = resolveTypeAlias(*typeName);
        if (isLowLevelTypeName(resolvedType))
            low_level_type_names_[alloca] = resolvedType;
        if (resolvedType.size() > 9 && resolvedType.compare(0, 9, "function(") == 0)
            fn_type_info_[alloca] = parseFnTypeAnnotation(resolvedType);
        auto constraint = parseTypeConstraint(resolvedType);
        if (constraint)
            type_constraints_[alloca] = *constraint;
        else if (isUnionType(resolvedType))
            union_value_types_[alloca] = normalizeUnionType(resolvedType);
    };

    auto bindMockContractParams = [&]() {
        if (!matchedEntry) return;
        for (size_t i = 0; i < matchedEntry->paramNames.size() && i < argVals.size(); ++i) {
            const std::string *typeName = i < matchedEntry->paramTypeNames.size()
                ? &matchedEntry->paramTypeNames[i]
                : nullptr;
            bindContractValue(matchedEntry->paramNames[i], argVals[i], typeName);
        }
    };

    auto emitMockRequireChecks = [&]() {
        if (!matchedEntry || !matchedEntry->preconditions || matchedEntry->preconditions->empty())
            return;
        pushScope();
        bindMockContractParams();
        for (const auto &precondition : *matchedEntry->preconditions)
            emitContractCheck("require", callee, precondition);
        popScope();
    };

    auto emitMockEnsureChecks = [&](llvm::Value *retVal) {
        if (!matchedEntry || !matchedEntry->postconditions || matchedEntry->postconditions->empty() ||
            !matchedEntry->ensureBindings)
            return;

        pushScope();
        bindMockContractParams();
        auto &bindings = *matchedEntry->ensureBindings;
        if (bindings.size() == 1) {
            bindContractValue(bindings[0], retVal, nullptr);
        } else {
            auto *structTy = llvm::dyn_cast<llvm::StructType>(retVal->getType());
            if (!structTy || structTy->isLiteral() || structTy->getNumElements() != bindings.size())
                codegenError("ensure destructuring requires tuple return; binding count does not match tuple element count");
            for (unsigned i = 0; i < bindings.size(); ++i) {
                llvm::Value *elem = builder_.CreateExtractValue(retVal, i);
                bindContractValue(bindings[i], elem, nullptr);
            }
        }

        bool savedInEnsureContext = in_ensure_context_;
        in_ensure_context_ = true;
        for (const auto &postcondition : *matchedEntry->postconditions)
            emitContractCheck("ensure", callee, postcondition);
        in_ensure_context_ = savedInEnsureContext;
        popScope();
    };

    // In test mode, inject mock dispatch only for functions targeted by mock()
    if (test_mode_ && mocked_functions_.count(callee)) {
        llvm::FunctionType *mockGetTy = llvm::FunctionType::get(ptrTy_, {ptrTy_}, false);
        llvm::FunctionCallee mockGetFn = mod_->getOrInsertFunction("__ry_mock_get", mockGetTy);
        llvm::FunctionType *mockIncTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
        llvm::FunctionCallee mockIncFn = mod_->getOrInsertFunction("__ry_mock_increment_call", mockIncTy);

        auto &nameStr = mock_name_strings_[callee];
        if (!nameStr) nameStr = cachedGlobalString(callee, ".mock." + callee);
        llvm::Value *mockPtr = builder_.CreateCall(mockGetFn, {nameStr}, "mock_ptr");
        llvm::Value *nullPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_));
        llvm::Value *isMocked = builder_.CreateICmpNE(mockPtr, nullPtr, "is_mocked");

        llvm::BasicBlock *mockBB = llvm::BasicBlock::Create(*ctx_, "mock_bb", fn_);
        llvm::BasicBlock *origBB = llvm::BasicBlock::Create(*ctx_, "orig_bb", fn_);
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "merge_bb", fn_);

        builder_.CreateCondBr(isMocked, mockBB, origBB);

        // Mock path
        builder_.SetInsertPoint(mockBB);
        emitMockRequireChecks();
        builder_.CreateCall(mockIncFn, {nameStr});
        llvm::FunctionType *fnTy = fn->getFunctionType();
        if (fn->getReturnType()->isVoidTy()) {
            builder_.CreateCall(fnTy, mockPtr, argVals);
            builder_.CreateBr(mergeBB);

            // Original path (void case)
            builder_.SetInsertPoint(origBB);
            builder_.CreateCall(fn, argVals);
            builder_.CreateBr(mergeBB);

            builder_.SetInsertPoint(mergeBB);
            return nullptr;
        }

        llvm::Value *mockResult = builder_.CreateCall(fnTy, mockPtr, argVals, "mock_result");
        emitMockEnsureChecks(mockResult);
        builder_.CreateBr(mergeBB);
        llvm::BasicBlock *mockEndBB = builder_.GetInsertBlock();

        // Original path
        builder_.SetInsertPoint(origBB);
        llvm::Value *origResult = builder_.CreateCall(fn, argVals, "orig_result");
        builder_.CreateBr(mergeBB);
        llvm::BasicBlock *origEndBB = builder_.GetInsertBlock();

        // Merge
        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = builder_.CreatePHI(fn->getReturnType(), 2, "call_result");
        phi->addIncoming(mockResult, mockEndBB);
        phi->addIncoming(origResult, origEndBB);
        if (matchedEntry && matchedEntry->returnTypeName.size() > 5 &&
            matchedEntry->returnTypeName.compare(0, 5, "Task<") == 0 &&
            matchedEntry->returnTypeName.back() == '>') {
            std::string inner = matchedEntry->returnTypeName.substr(
                5, matchedEntry->returnTypeName.size() - 6);
            type_meta_[TM_TaskResult][phi] = resolveType(inner);
        }
        if (matchedEntry && isLowLevelTypeName(matchedEntry->returnTypeName))
            low_level_type_names_[phi] = matchedEntry->returnTypeName;
        return phi;
    }

    if (fn->getReturnType()->isVoidTy())
        return builder_.CreateCall(fn, argVals);
    llvm::Value *callResult = builder_.CreateCall(fn, argVals, "calltmp");

    if (matchedEntry && matchedEntry->returnTypeName.size() > 5 &&
        matchedEntry->returnTypeName.compare(0, 5, "Task<") == 0 &&
        matchedEntry->returnTypeName.back() == '>') {
        std::string inner = matchedEntry->returnTypeName.substr(5, matchedEntry->returnTypeName.size() - 6);
        type_meta_[TM_TaskResult][callResult] = resolveType(inner);
    }

    // Propagate low-level type metadata from return type
    if (matchedEntry && isLowLevelTypeName(matchedEntry->returnTypeName))
        low_level_type_names_[callResult] = matchedEntry->returnTypeName;

    return callResult;
}

void CodeGen::emitStmt(CallStmt &s) {
    emitCoverage(s.loc);
    if (s.callee == "describe") {
        emitDescribeCall(s);
        return;
    }
    if (s.callee == "it") {
        emitItCall(s);
        return;
    }
    if (s.callee == "mock") {
        emitMockCall(s);
        return;
    }
    if (s.callee == "fail") {
        emitFailCall(s);
        return;
    }
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
    // Intercept collection operations and route through CallExpr emitter
    if (!s.args.empty()) {
        bool intercept = false;
        if (auto *ve = std::get_if<VariableExpr>(&s.args[0]->data)) {
            llvm::AllocaInst *alloca = findVar(ve->name);
            if (alloca) {
                bool isList = getListElementType(alloca) != nullptr;
                bool isSet = !isList && getSetElementType(alloca) != nullptr;
                bool isMap = !isList && !isSet && getMapKeyType(alloca) != nullptr;
                size_t nargs = s.args.size();

                if (isList &&
                    ((s.callee == "append" && nargs == 2) ||
                     (s.callee == "append!" && nargs == 2) ||
                     (s.callee == "pop" && nargs == 1) ||
                     (s.callee == "insert" && nargs == 3) ||
                     (s.callee == "remove_at" && nargs == 2) ||
                     (s.callee == "remove" && nargs == 2) ||
                     (s.callee == "sort!" && (nargs == 1 || nargs == 2)) ||
                     (s.callee == "reverse!" && nargs == 1))) {
                    intercept = true;
                } else if (isSet &&
                    ((s.callee == "add" && nargs == 2) ||
                     (s.callee == "remove" && nargs == 2) ||
                     (s.callee == "union" && nargs == 2) ||
                     (s.callee == "intersection" && nargs == 2) ||
                     (s.callee == "difference" && nargs == 2) ||
                     (s.callee == "symmetric_difference" && nargs == 2) ||
                     (s.callee == "is_subset" && nargs == 2) ||
                     (s.callee == "is_superset" && nargs == 2))) {
                    intercept = true;
                } else if (isMap &&
                    ((s.callee == "remove" && nargs == 2) ||
                     (s.callee == "items" && nargs == 1) ||
                     (s.callee == "get" && (nargs == 2 || nargs == 3)))) {
                    intercept = true;
                }
            }
        }
        if (intercept) {
            auto ce = std::make_unique<CallExpr>();
            ce->callee = s.callee;
            ce->args = std::move(s.args);
            emitExprVariant(ce);
            return;
        }
    }
    if (tryCallOperator(s.callee, s.args))
        return;
    // Route all remaining calls through the unified CallExpr dispatch chain.
    // This covers @native stdlib functions, language builtins (close, range,
    // sleep, etc.), and user-defined functions without a hardcoded whitelist.
    auto ce = std::make_unique<CallExpr>();
    ce->callee = s.callee;
    ce->args = std::move(s.args);
    emitExprVariant(ce);
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

// ===== C stdlib function helpers =====

llvm::FunctionCallee CodeGen::getStdlibMalloc() {
    auto ty = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
    return mod_->getOrInsertFunction("malloc", ty);
}

llvm::FunctionCallee CodeGen::getStdlibRealloc() {
    auto ty = llvm::FunctionType::get(ptrTy_, {ptrTy_, i64Ty_}, false);
    return mod_->getOrInsertFunction("realloc", ty);
}

llvm::FunctionCallee CodeGen::getStdlibFree() {
    auto ty = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
    return mod_->getOrInsertFunction("free", ty);
}

llvm::FunctionCallee CodeGen::getStdlibStrlen() {
    auto ty = llvm::FunctionType::get(i64Ty_, {ptrTy_}, false);
    return mod_->getOrInsertFunction("strlen", ty);
}

llvm::FunctionCallee CodeGen::getStdlibMemcpy() {
    auto ty = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
    return mod_->getOrInsertFunction("memcpy", ty);
}

llvm::FunctionCallee CodeGen::getStdlibMemmove() {
    auto ty = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_, i64Ty_}, false);
    return mod_->getOrInsertFunction("memmove", ty);
}

llvm::FunctionCallee CodeGen::getStdlibMemset() {
    auto ty = llvm::FunctionType::get(ptrTy_, {ptrTy_, i32Ty_, i64Ty_}, false);
    return mod_->getOrInsertFunction("memset", ty);
}

llvm::FunctionCallee CodeGen::getStdlibStrcmp() {
    auto ty = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_}, false);
    return mod_->getOrInsertFunction("strcmp", ty);
}

llvm::FunctionCallee CodeGen::getStdlibStrncmp() {
    auto ty = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_, i64Ty_}, false);
    return mod_->getOrInsertFunction("strncmp", ty);
}

llvm::FunctionCallee CodeGen::getStdlibStrstr() {
    auto ty = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
    return mod_->getOrInsertFunction("strstr", ty);
}

llvm::FunctionCallee CodeGen::getStdlibStrcasestr() {
    auto ty = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
    return mod_->getOrInsertFunction("strcasestr", ty);
}

llvm::FunctionCallee CodeGen::getStdlibStrncasecmp() {
    auto ty = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_, i64Ty_}, false);
    return mod_->getOrInsertFunction("strncasecmp", ty);
}

llvm::FunctionCallee CodeGen::getStdlibStrcpy() {
    auto ty = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
    return mod_->getOrInsertFunction("strcpy", ty);
}

llvm::FunctionCallee CodeGen::getStdlibStrcat() {
    auto ty = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
    return mod_->getOrInsertFunction("strcat", ty);
}

llvm::FunctionCallee CodeGen::getStdlibSnprintf() {
    auto ty = llvm::FunctionType::get(i32Ty_, {ptrTy_, i64Ty_, ptrTy_}, true);
    return mod_->getOrInsertFunction("snprintf", ty);
}

llvm::FunctionCallee CodeGen::getStdlibPrintf() {
    auto ty = llvm::FunctionType::get(i32Ty_, {ptrTy_}, true);
    return mod_->getOrInsertFunction("printf", ty);
}

llvm::FunctionCallee CodeGen::getBufferedPrintf() {
    auto ty = llvm::FunctionType::get(i32Ty_, {ptrTy_}, true);
    return mod_->getOrInsertFunction("__ry_print_printf", ty);
}

llvm::FunctionCallee CodeGen::getStdlibExit() {
    auto ty = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {i32Ty_}, false);
    return mod_->getOrInsertFunction("exit", ty);
}

void CodeGen::emitRuntimeError(const std::string &message, const std::string &globalName,
                                llvm::ArrayRef<llvm::Value *> extraArgs) {
    auto fprintfTy = llvm::FunctionType::get(i32Ty_, {ptrTy_, ptrTy_}, true);
    auto fprintfFn = mod_->getOrInsertFunction("fprintf", fprintfTy);
#ifdef __APPLE__
    const char *stderrName = "__stderrp";
#else
    const char *stderrName = "stderr";
#endif
    auto *stderrGlobal = mod_->getOrInsertGlobal(stderrName, ptrTy_);
    llvm::Value *stderrVal = builder_.CreateLoad(ptrTy_, stderrGlobal, "stderr");
    llvm::Constant *errMsg = cachedGlobalString(message, globalName);
    llvm::SmallVector<llvm::Value *, 4> args = {stderrVal, errMsg};
    args.append(extraArgs.begin(), extraArgs.end());
    builder_.CreateCall(fprintfFn, args);
    auto exitFn = getStdlibExit();
    builder_.CreateCall(exitFn, {llvm::ConstantInt::get(i32Ty_, 1)});
    builder_.CreateUnreachable();
}

void CodeGen::emitBoundsError(llvm::Value *index, llvm::Value *size,
                               const std::string &fmtMsg, const std::string &globalName) {
    emitRuntimeError(fmtMsg, globalName, {index, size});
}

llvm::Value *CodeGen::emitNegativeIndexWrap(llvm::Value *idx, llvm::Value *wrapBase,
                                              const std::string &prefix) {
    llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
    llvm::Value *isNeg = builder_.CreateICmpSLT(idx, zero, prefix + "_is_neg");
    llvm::Value *wrapped = builder_.CreateAdd(idx, wrapBase, prefix + "_wrapped");
    return builder_.CreateSelect(isNeg, wrapped, idx, prefix + "_idx");
}

void CodeGen::emitBoundsCheck(llvm::Value *&index, llvm::Value *size,
                               const std::string &errMsg, const std::string &globalName,
                               const std::string &bbPrefix) {
    if (index->getType() == i1Ty_)
        index = builder_.CreateZExt(index, i64Ty_, "idx_ext");

    // Compile-time constant check with negative index wrap-around
    if (auto *ci = llvm::dyn_cast<llvm::ConstantInt>(index)) {
        if (auto *cs = llvm::dyn_cast<llvm::ConstantInt>(size)) {
            int64_t idx = ci->getSExtValue();
            int64_t sz = static_cast<int64_t>(cs->getZExtValue());
            if (idx < 0) idx += sz;
            if (idx < 0 || idx >= sz)
                codegenError("index " + std::to_string(ci->getSExtValue()) +
                             " out of bounds (size " + std::to_string(sz) + ")");
            index = llvm::ConstantInt::get(i64Ty_, idx);
            return;
        }
    }

    llvm::Value *origIndex = index;
    index = emitNegativeIndexWrap(index, size, bbPrefix);

    llvm::Value *zero = llvm::ConstantInt::get(i64Ty_, 0);
    llvm::Value *negCheck = builder_.CreateICmpSLT(
        index, zero, bbPrefix + "_neg");
    llvm::Value *overCheck = builder_.CreateICmpSGE(index, size, bbPrefix + "_over");
    llvm::Value *oob = builder_.CreateOr(negCheck, overCheck, bbPrefix + "_oob");
    llvm::BasicBlock *oobBB = llvm::BasicBlock::Create(*ctx_, bbPrefix + ".oob", fn_);
    llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, bbPrefix + ".ok", fn_);
    builder_.CreateCondBr(oob, oobBB, okBB);
    builder_.SetInsertPoint(oobBB);
    emitBoundsError(origIndex, size, errMsg, globalName);
    builder_.SetInsertPoint(okBB);
}

llvm::Value *CodeGen::coerceToLowLevelType(llvm::Value *val, llvm::Type *targetTy,
                                            const std::string &typeName,
                                            const std::string &context,
                                            const std::string &truncName) {
    if (val->getType() == f64Ty_ && targetTy == f32Ty_)
        return builder_.CreateFPTrunc(val, f32Ty_, truncName);

    if (val->getType() == i64Ty_ && (targetTy == i8Ty_ || targetTy == i16Ty_ || targetTy == i32Ty_)) {
        if (auto *ci = llvm::dyn_cast<llvm::ConstantInt>(val)) {
            int64_t v = ci->getSExtValue();
            bool isUnsigned = (!typeName.empty() && typeName[0] == 'u');
            bool outOfRange = false;
            if (targetTy == i8Ty_) {
                outOfRange = isUnsigned ? (v < 0 || v > 255) : (v < INT8_MIN || v > INT8_MAX);
            } else if (targetTy == i16Ty_) {
                outOfRange = isUnsigned ? (v < 0 || v > (int64_t)UINT16_MAX) : (v < INT16_MIN || v > INT16_MAX);
            } else {
                outOfRange = isUnsigned ? (v < 0 || v > (int64_t)UINT32_MAX) : (v < INT32_MIN || v > INT32_MAX);
            }
            if (outOfRange)
                codegenError(typeName + " value out of range" + context + ": " + std::to_string(v));
        }
        return builder_.CreateTrunc(val, targetTy, truncName);
    }

    return nullptr;
}

void CodeGen::emitPrintValue(llvm::Value *val, llvm::Type *ty,
                              llvm::FunctionCallee printfFn, const std::string &suffix) {
    if (ty == i1Ty_) {
        llvm::Constant *t = cachedGlobalString("true", ".fmt_true" + suffix);
        llvm::Constant *f = cachedGlobalString("false", ".fmt_false" + suffix);
        builder_.CreateCall(printfFn, {builder_.CreateSelect(val, t, f, "bool_fmt")});
    } else if (ty->isPointerTy()) {
        llvm::Constant *fmt = cachedGlobalString("%s", ".fmt_s" + suffix);
        builder_.CreateCall(printfFn, {fmt, val});
    } else if (ty == i8Ty_) {
        std::string llName = getLowLevelTypeName(val);
        if (llName == "i8") {
            llvm::Value *ext = builder_.CreateSExt(val, i32Ty_, "i8_print");
            llvm::Constant *fmt = cachedGlobalString("%d", ".fmt_i8" + suffix);
            builder_.CreateCall(printfFn, {fmt, ext});
        } else if (llName == "u8") {
            llvm::Value *ext = builder_.CreateZExt(val, i32Ty_, "u8_print");
            llvm::Constant *fmt = cachedGlobalString("%u", ".fmt_u8" + suffix);
            builder_.CreateCall(printfFn, {fmt, ext});
        } else {
            llvm::Value *ext = builder_.CreateZExt(val, i32Ty_, "u8_print");
            llvm::Constant *fmt = cachedGlobalString("%u", ".fmt_u8_def" + suffix);
            builder_.CreateCall(printfFn, {fmt, ext});
        }
    } else if (ty == i16Ty_) {
        std::string llName = getLowLevelTypeName(val);
        if (llName == "u16") {
            llvm::Value *ext = builder_.CreateZExt(val, i32Ty_, "u16_print");
            llvm::Constant *fmt = cachedGlobalString("%u", ".fmt_u16" + suffix);
            builder_.CreateCall(printfFn, {fmt, ext});
        } else {
            llvm::Value *ext = builder_.CreateSExt(val, i32Ty_, "i16_print");
            llvm::Constant *fmt = cachedGlobalString("%d", ".fmt_i16" + suffix);
            builder_.CreateCall(printfFn, {fmt, ext});
        }
    } else if (ty == i32Ty_) {
        std::string llName = getLowLevelTypeName(val);
        if (llName == "u32") {
            llvm::Constant *fmt = cachedGlobalString("%u", ".fmt_u32" + suffix);
            builder_.CreateCall(printfFn, {fmt, val});
        } else {
            llvm::Constant *fmt = cachedGlobalString("%d", ".fmt_i32" + suffix);
            builder_.CreateCall(printfFn, {fmt, val});
        }
    } else if (ty == f32Ty_) {
        llvm::Value *ext = builder_.CreateFPExt(val, f64Ty_, "f32_print");
        llvm::Constant *fmt = cachedGlobalString("%g", ".fmt_f32" + suffix);
        builder_.CreateCall(printfFn, {fmt, ext});
    } else if (ty->isDoubleTy()) {
        llvm::Constant *fmt = cachedGlobalString("%g", ".fmt_f" + suffix);
        builder_.CreateCall(printfFn, {fmt, val});
    } else if (ty == anyTy_) {
        llvm::Value *str = emitAnyToString(val);
        llvm::Constant *fmt = cachedGlobalString("%s", ".fmt_any" + suffix);
        builder_.CreateCall(printfFn, {fmt, str});
    } else if (ty == errorTy_) {
        llvm::Value *msg = builder_.CreateExtractValue(val, 0, "err_msg");
        llvm::Value *code = builder_.CreateExtractValue(val, 1, "err_code");
        llvm::Constant *fmt = cachedGlobalString("Error: %s (code: %ld)", ".fmt_err" + suffix);
        builder_.CreateCall(printfFn, {fmt, msg, code});
    } else if (auto *st = llvm::dyn_cast<llvm::StructType>(ty)) {
        std::string name = st->getName().str();
        if (struct_types_.count(name)) {
            llvm::Value *str = structToString(val);
            llvm::Constant *fmt = cachedGlobalString("%s", ".fmt_struct" + suffix);
            builder_.CreateCall(printfFn, {fmt, str});
        }
    } else {
        std::string llName = getLowLevelTypeName(val);
        if (llName == "u64") {
            llvm::Constant *fmt = cachedGlobalString("%lu", ".fmt_u64" + suffix);
            builder_.CreateCall(printfFn, {fmt, val});
        } else {
            llvm::Constant *fmt = cachedGlobalString("%ld", ".fmt_i" + suffix);
            builder_.CreateCall(printfFn, {fmt, val});
        }
    }
}
