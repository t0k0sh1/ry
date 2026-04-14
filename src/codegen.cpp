#include "ry/codegen.hpp"
#include "ry/coverage_runtime.hpp"
#include "ry/diagnostic.hpp"
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>


namespace ry {

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

    builtins_["print"] = [this](const std::vector<ExprPtr> &args, const std::vector<NamedArg> &named) { emitPrint(args, named); };
    builtins_["exit"] = [this](const std::vector<ExprPtr> &args, const std::vector<NamedArg> &named) {
        if (!named.empty())
            codegenError("unknown named argument '" + named.front().name + "' for exit()");
        emitExit(args);
    };

    errorTy_ = llvm::StructType::create(*ctx_, {ptrTy_, i64Ty_}, "Error");
    {
        std::vector<FieldDef> errorFields;
        errorFields.push_back({"message", TypeNode::makeBasic("str"), {}});
        errorFields.push_back({"code", TypeNode::makeBasic("int"), {}});
        struct_types_["Error"] = {errorTy_, std::move(errorFields), {}, "", next_type_id_++};
    }

    // Match type: {full: str, groups: List<str>}
    // Registered globally so `from regex import find_all` works without
    // explicitly importing Match — same rationale as the Error type above.
    {
        auto *matchTy = llvm::StructType::create(*ctx_, {ptrTy_, ptrTy_}, "Match");
        std::vector<FieldDef> matchFields;
        matchFields.push_back({"full", TypeNode::makeBasic("str"), {}});
        matchFields.push_back({"groups", TypeNode::makeBasic("List<str>"), {}});
        struct_types_["Match"] = {matchTy, std::move(matchFields), {}, "", next_type_id_++};
    }

    listHeaderTy_ = llvm::StructType::create(*ctx_, {i64Ty_, i64Ty_, ptrTy_}, "ListHeader");
    mapHeaderTy_ = llvm::StructType::create(*ctx_, {i64Ty_, i64Ty_, ptrTy_, ptrTy_, i64Ty_, ptrTy_}, "MapHeader");
    setHeaderTy_ = llvm::StructType::create(*ctx_, {i64Ty_, i64Ty_, ptrTy_, i64Ty_, ptrTy_}, "SetHeader");
    iteratorHeaderTy_ = llvm::StructType::create(*ctx_, {ptrTy_, ptrTy_}, "IteratorHeader");
    arcHeaderTy_ = llvm::StructType::create(*ctx_, {i64Ty_, i64Ty_}, "ArcHeader");

    anyTy_ = llvm::StructType::create(
        *ctx_, {i64Ty_, llvm::ArrayType::get(i8Ty_, 8)}, "Any");

    typeTy_ = llvm::StructType::create(*ctx_, {i64Ty_, ptrTy_}, "Type");

    // Pre-allocate canonical type IDs for primitives, collections, and other
    // built-in types so that type_of returns stable identities across a compile.
    for (const char *name : {
             "int", "float", "bool", "str", "any", "Unit",
             "i8", "i16", "i32", "i64",
             "u8", "u16", "u32", "u64", "f32",
             "List", "Map", "Set",
             "Option", "Result",
             "None", "function", "Type"}) {
        canonical_type_ids_[name] = next_type_id_++;
    }

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

int64_t CodeGen::getOrAllocateCanonicalTypeId(const std::string &canonicalName) {
    auto [it, inserted] = canonical_type_ids_.try_emplace(canonicalName, next_type_id_);
    if (inserted) ++next_type_id_;
    return it->second;
}

llvm::Constant *CodeGen::buildArcGlobal(
        const std::string &str, const llvm::Twine &name,
        std::unordered_map<std::string, llvm::Constant*> &cache) {
    auto it = cache.find(str);
    if (it != cache.end()) return it->second;

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

    cache[str] = gs;
    return gs;
}

llvm::Constant *CodeGen::cachedGlobalString(const std::string &str, const llvm::Twine &name) {
    return buildArcGlobal(str, name, global_string_cache_);
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
    savedIteratorMallocs_ = std::move(cg_.iterator_malloc_stack_);
    savedBlock_ = cg_.builder_.GetInsertBlock();
    savedPoint_ = cg_.builder_.GetInsertPoint();
    savedPostconditions_ = cg_.current_postconditions_;
    savedEnsureBindings_ = cg_.ensure_bindings_;
    savedInEnsureContext_ = cg_.in_ensure_context_;
    savedFnReturnType_ = std::move(cg_.current_fn_return_type_);
    savedFnName_ = std::move(cg_.current_function_name_);
    savedCapturedVars_ = std::move(cg_.captured_vars_);
    savedFnNestingDepth_ = cg_.fn_nesting_depth_;
    savedFnScopeStackSize_ = cg_.fn_scope_stack_.size();
    cg_.fn_nesting_depth_++;
    cg_.captured_vars_.clear();
    cg_.scope_stack_.clear();
    cg_.immutable_scope_stack_.clear();
    cg_.arc_managed_vars_.clear();
    cg_.arc_owned_values_.clear();
    cg_.weak_managed_vars_.clear();
    cg_.weak_inner_type_names_.clear();
    cg_.resource_managed_vars_.clear();
    cg_.closure_managed_vars_.clear();
    cg_.iterator_malloc_stack_.clear();
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
    cg_.iterator_malloc_stack_ = std::move(savedIteratorMallocs_);
    cg_.builder_.SetInsertPoint(savedBlock_, savedPoint_);
    cg_.current_postconditions_ = savedPostconditions_;
    cg_.ensure_bindings_ = savedEnsureBindings_;
    cg_.in_ensure_context_ = savedInEnsureContext_;
    cg_.current_fn_return_type_ = std::move(savedFnReturnType_);
    cg_.current_function_name_ = std::move(savedFnName_);
    cg_.captured_vars_ = std::move(savedCapturedVars_);
    cg_.fn_nesting_depth_ = savedFnNestingDepth_;
    // Trim fn_scope_stack_ levels added during the function body.
    // Unlike scope_stack_ (which is fully saved/restored), fn_scope_stack_
    // is kept alive across FnScope boundaries so nested functions can see
    // outer definitions. We only remove levels pushed inside this body.
    cg_.fn_scope_stack_.resize(savedFnScopeStackSize_);
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
        {llvm::ConstantInt::get(i32Ty_, static_cast<uint64_t>(gid)),
         llvm::ConstantInt::get(i32Ty_, static_cast<uint64_t>(loc.line))});
}

void CodeGen::emitTraceSymbolDefine(const std::string &kind, const std::string &name,
                                    const SourceLocation &loc) {
    if (!ry::traceEnabled()) return;
    // Skip stdlib symbols to reduce noise — users care about their own definitions
    if (sm_ && loc.file_id >= 0 && loc.file_id < sm_->getFileCount()) {
        const auto &fname = sm_->getFilename(loc.file_id);
        if (fname.find("/share/std/") != std::string::npos ||
            fname.find("/lib/std/") != std::string::npos) {
            return;
        }
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
                                 llvm::ConstantInt::get(i32Ty_, static_cast<uint64_t>(loc.line)),
                                 llvm::ConstantInt::get(i32Ty_, static_cast<uint64_t>(loc.col))});
}

void CodeGen::emitTraceFunctionExit(const std::string &fnName, const SourceLocation &loc) {
    if (!ry::traceEnabled()) return;
    auto callee = mod_->getOrInsertFunction(
        "__ry_trace_function_exit",
        llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_),
                                {ptrTy_, ptrTy_, i32Ty_, i32Ty_}, false));
    builder_.CreateCall(callee, {emitTraceSourceString(fnName),
                                 emitTraceFileString(loc),
                                 llvm::ConstantInt::get(i32Ty_, static_cast<uint64_t>(loc.line)),
                                 llvm::ConstantInt::get(i32Ty_, static_cast<uint64_t>(loc.col))});
}

void CodeGen::emitTraceReturn(const SourceLocation &loc) {
    if (!ry::traceEnabled()) return;
    auto callee = mod_->getOrInsertFunction(
        "__ry_trace_return",
        llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_),
                                {ptrTy_, ptrTy_, i32Ty_, i32Ty_}, false));
    builder_.CreateCall(callee, {emitTraceSourceString(current_function_name_),
                                 emitTraceFileString(loc),
                                 llvm::ConstantInt::get(i32Ty_, static_cast<uint64_t>(loc.line)),
                                 llvm::ConstantInt::get(i32Ty_, static_cast<uint64_t>(loc.col))});
}

void CodeGen::emitTraceIfBranch(llvm::Value *cond, const SourceLocation &loc) {
    if (!ry::traceEnabled()) return;
    auto callee = mod_->getOrInsertFunction(
        "__ry_trace_branch_if",
        llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_),
                                {ptrTy_, i32Ty_, i32Ty_, i32Ty_}, false));
    llvm::Value *taken = builder_.CreateZExt(cond, i32Ty_, "trace_if_taken");
    builder_.CreateCall(callee, {emitTraceFileString(loc),
                                 llvm::ConstantInt::get(i32Ty_, static_cast<uint64_t>(loc.line)),
                                 llvm::ConstantInt::get(i32Ty_, static_cast<uint64_t>(loc.col)),
                                 taken});
}

void CodeGen::emitTraceWhenBranch(int armIndex, const SourceLocation &loc) {
    if (!ry::traceEnabled()) return;
    auto callee = mod_->getOrInsertFunction(
        "__ry_trace_branch_when",
        llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_),
                                {ptrTy_, i32Ty_, i32Ty_, i32Ty_}, false));
    builder_.CreateCall(callee, {emitTraceFileString(loc),
                                 llvm::ConstantInt::get(i32Ty_, static_cast<uint64_t>(loc.line)),
                                 llvm::ConstantInt::get(i32Ty_, static_cast<uint64_t>(loc.col)),
                                 llvm::ConstantInt::get(i32Ty_, static_cast<uint64_t>(armIndex))});
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
    iterator_malloc_stack_.emplace_back();
    if (fn_nesting_depth_ > 0)
        fn_scope_stack_.emplace_back();
}

void CodeGen::popScope() {
    emitScopeCleanup();
    scope_stack_.pop_back();
    immutable_scope_stack_.pop_back();
    iterator_malloc_stack_.pop_back();
    if (fn_nesting_depth_ > 0 && !fn_scope_stack_.empty())
        fn_scope_stack_.pop_back();
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
            // Records with ARC fields (#854 Layer 2): release each ARC
            // field to pair with the retain-on-copy at emitVarDecl /
            // AssignStmt so path CoW at `r.field[i] = v` observes the
            // correct strong_count on inner containers.
            if (arc_field_struct_vars_.count(alloca)) {
                auto *recSt = llvm::cast<llvm::StructType>(alloca->getAllocatedType());
                llvm::Value *recVal = builder_.CreateLoad(
                    recSt, alloca, name + ".record_scope_cleanup");
                emitRecordArcFieldsRelease(recVal, recSt);
                arc_field_struct_vars_.erase(alloca);
                continue;
            }
            if (!arc_managed_vars_.count(alloca)) continue;
            emitArcReleaseVar(name, alloca);
            arc_managed_vars_.erase(alloca);
        }
        auto &iterMallocs = iterator_malloc_stack_[i - 1];
        if (!iterMallocs.empty()) {
            auto freeFn = getStdlibFree();
            for (auto *ptr : iterMallocs) {
                builder_.CreateCall(freeFn, {ptr});
            }
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

std::vector<CodeGen::OverloadEntry> *CodeGen::findFunction(const std::string &name) {
    for (auto it = fn_scope_stack_.rbegin(); it != fn_scope_stack_.rend(); ++it) {
        auto found = it->find(name);
        if (found != it->end())
            return &found->second;
    }
    auto fit = functions_.find(name);
    if (fit != functions_.end())
        return &fit->second;
    return nullptr;
}

bool CodeGen::isImmutable(const std::string &name) const {
    for (auto it = immutable_scope_stack_.rbegin(); it != immutable_scope_stack_.rend(); ++it) {
        if (it->count(name))
            return true;
    }
    // Module-level @const bindings (#817). Only consult when NO local binding
    // with this name exists in any enclosing scope — otherwise a mutable
    // local/parameter/loop variable that happens to shadow a top-level
    // @const would be incorrectly rejected as immutable.
    for (auto it = scope_stack_.rbegin(); it != scope_stack_.rend(); ++it) {
        if (it->count(name))
            return false;
    }
    auto mit = module_globals_.find(name);
    if (mit != module_globals_.end() && mit->second.is_immutable)
        return true;
    return false;
}

// ===== Module-level bindings (#817) =====

const CodeGen::ModuleBinding *CodeGen::findModuleGlobal(const std::string &name) const {
    auto it = module_globals_.find(name);
    if (it == module_globals_.end())
        return nullptr;
    return &it->second;
}

void CodeGen::registerModuleGlobal(const std::string &name,
                                    llvm::AllocaInst *alloca,
                                    bool is_immutable) {
    // The trampoline is a private module-level pointer variable initialized to
    // null at module load; __ry_main__ stores the alloca address into it just
    // after the alloca is materialized, so lookups from other top-level
    // functions (which can only run after __ry_main__ has executed up to that
    // point) always find a valid pointer.
    auto *gv = new llvm::GlobalVariable(
        *mod_,
        ptrTy_,
        /*isConstant=*/false,
        llvm::GlobalValue::PrivateLinkage,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptrTy_)),
        "__ry_modvar_" + name);
    // Store the alloca address into the trampoline global at __ry_main__'s
    // current insert point, which is source-order during the top-level loop.
    builder_.CreateStore(alloca, gv);
    // Capture lifetime-management classification NOW, while we are still in
    // __ry_main__ context and the per-function tracking sets (weak, arc,
    // resource, closure) are populated. FnScope will clear these sets when
    // a top-level function body begins, so the write-through / read paths
    // must rely on these cached flags rather than querying the sets from
    // inside a foreign function.
    ModuleBinding mb;
    mb.gv_ptr = gv;
    mb.original_alloca = alloca;
    mb.is_immutable = is_immutable;
    mb.is_weak = isWeakManaged(alloca);
    mb.is_arc_managed = isArcManaged(alloca);
    mb.is_arc_atomic = arc_atomic_values_.count(alloca) > 0;
    mb.is_resource = resource_managed_vars_.count(alloca) > 0;
    mb.destructor = resolveDestructor(alloca);
    module_globals_[name] = mb;
}

llvm::Value *CodeGen::loadModuleGlobalStorage(const ModuleBinding &b,
                                              const std::string &name) {
    return builder_.CreateLoad(ptrTy_, b.gv_ptr, name + ".modptr");
}

bool CodeGen::isCapturedVar(llvm::AllocaInst *ptr) const {
    return captured_vars_.count(ptr) > 0;
}

// Forward declaration for mutual recursion.
static void collectMockedFunctionsFromStmts(const std::vector<StmtNode> &stmts,
                                             std::unordered_set<std::string> &out);

// Scan a single statement (and its nested bodies) for mock() call targets.
static void collectMockedFunctionsFromStmt(const StmtNode &stmt,
                                            std::unordered_set<std::string> &out) {
    std::visit([&](const auto &s) {
        using T = std::decay_t<decltype(s)>;
        if constexpr (std::is_same_v<T, CallStmt>) {
            if (s.callee == "mock" && !s.args.empty()) {
                if (auto *str = std::get_if<StringExpr>(&s.args[0]->data))
                    out.insert(str->value);
            }
            for (auto &arg : s.args) {
                if (auto *lam = std::get_if<std::unique_ptr<LambdaExpr>>(&arg->data))
                    collectMockedFunctionsFromStmts((*lam)->body, out);
            }
        } else if constexpr (std::is_same_v<T, std::unique_ptr<IfStmt>>) {
            collectMockedFunctionsFromStmts(s->branch.body, out);
            collectMockedFunctionsFromStmts(s->else_body, out);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<CaseCondStmt>>) {
            for (auto &arm : s->arms)
                collectMockedFunctionsFromStmts(arm.body, out);
            collectMockedFunctionsFromStmts(s->else_body, out);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<WhileStmt>>) { // NOLINT(bugprone-branch-clone)
            collectMockedFunctionsFromStmts(s->body, out);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<ForStmt>>) {
            collectMockedFunctionsFromStmts(s->body, out);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<FnStmt>>) {
            collectMockedFunctionsFromStmts(s->body, out);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<CaseStmt>>) {
            for (auto &arm : s->arms)
                collectMockedFunctionsFromStmts(arm.body, out);
        }
    }, stmt);
}

static void collectMockedFunctionsFromStmts(const std::vector<StmtNode> &stmts,
                                             std::unordered_set<std::string> &out) {
    for (const auto &stmt : stmts)
        collectMockedFunctionsFromStmt(stmt, out);
}

llvm::orc::ThreadSafeModule CodeGen::compile(Program &prog) {
    // Single pre-pass: collect mock targets and build cyclic type graph together
    std::unordered_map<std::string, std::unordered_set<std::string>> typeGraph;
    std::unordered_set<std::string> allTypes;
    for (auto &stmt : prog) {
        collectTypeGraphFromStmt(stmt, typeGraph, allTypes);
        if (test_mode_)
            collectMockedFunctionsFromStmt(stmt, mocked_functions_);
    }
    runCyclicTypeAnalysis(typeGraph, allTypes);

    llvm::FunctionType *ft = llvm::FunctionType::get(i32Ty_, false);
    fn_ = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "__ry_main__", *mod_);
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(*ctx_, "entry", fn_);
    builder_.SetInsertPoint(bb);

    pushScope();

    // Forward-declare top-level functions for mutual recursion support
    forwardDeclareFunctions(prog);

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
    // Check unified metadata first
    if (auto *meta = getMeta(val)) {
        if (!meta->low_level_type_name.empty())
            return meta->low_level_type_name;
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
    if (auto *ue = std::get_if<std::unique_ptr<UnaryExpr>>(&node.data)) {
        if ((*ue)->op == "+" || (*ue)->op == "-")
            return getExprLowLevelSuffix(*(*ue)->operand);
    }
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

void CodeGen::checkLowLevelTypeMix(llvm::Value *lhs, llvm::Value *rhs, const std::string &op,
                                    const std::string &lhsHint, const std::string &rhsHint) {
    // Fall back to AST suffix hints for constants that lack metadata (#311, #595)
    const std::string &lhsRef = getLowLevelTypeName(lhs);
    const std::string &rhsRef = getLowLevelTypeName(rhs);
    const std::string &lhsName = !lhsRef.empty() ? lhsRef : lhsHint;
    const std::string &rhsName = !rhsRef.empty() ? rhsRef : rhsHint;
    bool lhsLL = !lhsName.empty() || isLowLevelTy(lhs->getType());
    bool rhsLL = !rhsName.empty() || isLowLevelTy(rhs->getType());
    if (!lhsLL && !rhsLL) return;

    auto mixError = [&] {
        codegenError("type error: cannot mix types in operator '" + op +
                     "'; low-level numeric types require explicit 'as' cast");
    };
    if (lhsLL != rhsLL) mixError();                                    // (#595)
    if (lhs->getType() != rhs->getType()) mixError();                 // different widths
    if (!lhsName.empty() && !rhsName.empty() && lhsName != rhsName)   // e.g., u32 vs i32
        mixError();
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
        if (it->second.parentName == parentType) return true;
        current = it->second.parentName;
    }
    return false;
}

llvm::Value *CodeGen::emitSubtypeSlice(llvm::Value *childVal,
                                         const std::string &childTypeName,
                                         const std::string &parentTypeName) {
    (void)childTypeName;
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

} // namespace ry
