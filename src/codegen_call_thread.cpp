#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"
#include "ry/diagnostic/diagnostic.hpp"
#include <functional>


namespace ry {

static int rk_thread, rk_lock, rk_rwlock, rk_semaphore, rk_barrier, rk_atomic_int, rk_atomic_bool;
namespace {
struct ThreadResourceReg { ThreadResourceReg() {
    auto &r = ResourceKindRegistry::instance();
    rk_thread = r.registerKind("Thread", "__ry_arc_dtor_thread", "__ry_thread_cleanup", "thread");
    rk_lock = r.registerKind("Lock", "__ry_arc_dtor_lock", "__ry_lock_cleanup", "thread");
    rk_rwlock = r.registerKind("RWLock", "__ry_arc_dtor_rwlock", "__ry_rwlock_cleanup", "thread");
    rk_semaphore = r.registerKind("Semaphore", "__ry_arc_dtor_semaphore", "__ry_semaphore_cleanup", "thread");
    rk_barrier = r.registerKind("Barrier", "__ry_arc_dtor_barrier", "__ry_barrier_cleanup", "thread");
    rk_atomic_int = r.registerKind("AtomicInt", "__ry_arc_dtor_atomic_int", "__ry_atomic_int_cleanup", "thread");
    rk_atomic_bool = r.registerKind("AtomicBool", "__ry_arc_dtor_atomic_bool", "__ry_atomic_bool_cleanup", "thread");
}} thread_resource_reg;
}

// Collect variable names referenced in a lambda body (free variable analysis).
// Mirrors the scan logic in codegen_lambda.cpp but only collects names.
static std::unordered_set<std::string> collectReferencedVars(const LambdaExpr &lam) {
    std::unordered_set<std::string> refs;
    std::function<void(const ExprNode&)> scanExpr;
    std::function<void(const StmtNode&)> scanStmt;

    scanExpr = [&](const ExprNode &node) {
        std::visit([&](const auto &v) {
            using T = std::decay_t<decltype(v)>;
            if constexpr (std::is_same_v<T, VariableExpr>) {
                refs.insert(v.name);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<BinaryExpr>>) {
                scanExpr(*v->lhs); scanExpr(*v->rhs);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<UnaryExpr>>) {
                scanExpr(*v->operand);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<CallExpr>>) {
                for (auto &arg : v->args) scanExpr(*arg);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<FieldAccessExpr>>) {
                scanExpr(*v->object);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<TupleExpr>>) {
                for (auto &el : v->elements) scanExpr(*el);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<ListExpr>>) {
                for (auto &el : v->elements) scanExpr(*el);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<IndexExpr>>) {
                scanExpr(*v->object); for (auto &idx : v->indices) scanExpr(*idx);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<MapExpr>>) {
                for (auto &k : v->keys) scanExpr(*k);
                for (auto &val : v->values) scanExpr(*val);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<LambdaExpr>>) {
                if (v->expr_body) scanExpr(*v->expr_body);
                for (auto &st : v->body) scanStmt(st);
            }
        }, node.data);
    };

    scanStmt = [&](const StmtNode &stmt) {
        std::visit([&](const auto &s) {
            using T = std::decay_t<decltype(s)>;
            if constexpr (std::is_same_v<T, AssignStmt>) { // NOLINT(bugprone-branch-clone)
                if (s.value) scanExpr(*s.value);
            } else if constexpr (std::is_same_v<T, CallStmt>) {
                for (auto &arg : s.args) scanExpr(*arg);
            } else if constexpr (std::is_same_v<T, ReturnStmt>) {
                if (s.value) scanExpr(*s.value);
            } else if constexpr (std::is_same_v<T, IndexAssignStmt>) {
                scanExpr(*s.object); for (auto &idx : s.indices) scanExpr(*idx); scanExpr(*s.value);
            } else if constexpr (std::is_same_v<T, FieldAssignStmt>) {
                scanExpr(*s.object); scanExpr(*s.value);
            } else if constexpr (std::is_same_v<T, TupleDestructStmt>) {
                scanExpr(*s.value);
            } else if constexpr (std::is_same_v<T, ExprStmt>) {
                scanExpr(*s.expr);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<IfStmt>>) {
                scanExpr(*s->branch.condition);
                for (auto &st : s->branch.body) scanStmt(st);
                for (auto &st : s->else_body) scanStmt(st);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<CaseCondStmt>>) {
                for (auto &arm : s->arms) {
                    scanExpr(*arm.condition);
                    for (auto &st : arm.body) scanStmt(st);
                }
                for (auto &st : s->else_body) scanStmt(st);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<WhileStmt>>) {
                scanExpr(*s->condition);
                for (auto &st : s->body) scanStmt(st);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<ForStmt>>) {
                scanExpr(*s->iterable);
                for (auto &st : s->body) scanStmt(st);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<FnStmt>>) {
                for (auto &st : s->body) scanStmt(st);
            } else if constexpr (std::is_same_v<T, std::unique_ptr<CaseStmt>>) {
                scanExpr(*s->subject);
                for (auto &arm : s->arms) {
                    if (arm.guard) scanExpr(*arm.guard);
                    for (auto &st : arm.body) scanStmt(st);
                }
            }
        }, stmt);
    };

    if (lam.expr_body)
        scanExpr(*lam.expr_body);
    else
        for (auto &stmt : lam.body)
            scanStmt(stmt);

    return refs;
}

// ===== Thread custom emitters =====

// Reject worker return types that don't fit the 8-byte ThreadHandle slot.
// MVP (#828): only Unit / int / float / bool are supported. void passes
// through (Unit worker); other shapes (any / ARC / sum types) would either
// silently corrupt the slot or trip an sret ABI mismatch.
static void rejectIfUnsupportedThreadReturn(CodeGen &cg, llvm::Type *retTy) {
    if (!retTy->isVoidTy() &&
        retTy != cg.i64Ty_ &&
        retTy != cg.f64Ty_ &&
        retTy != cg.i1Ty_) {
        cg.codegenError(
            "threadSpawn() MVP (#828) supports only () -> Unit, int, float, "
            "or bool return types; ARC-managed types (str, List, Map, Set, "
            "records) are tracked in #877, sum types (Option, Result, enum) "
            "are tracked in #878");
    }
}

// Write the worker's return value into the ThreadHandle's 8-byte result slot.
// i1 is widened to i64 so the full slot is initialised; threadJoin reads back
// as i64 and truncates to i1. No-op for void workers (Unit).
static void emitStoreThreadResult(CodeGen &cg, llvm::Value *retVal,
                                  llvm::Type *workerRetTy,
                                  llvm::Value *resultRaw) {
    if (workerRetTy->isVoidTy())
        return;
    llvm::Value *toStore = retVal;
    if (workerRetTy == cg.i1Ty_)
        toStore = cg.emitZExt(retVal, cg.i64Ty_, "thread_ret_bool_ext");
    cg.emitStore(toStore, resultRaw);
}

static llvm::Value *emitThreadSpawn(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);

    llvm::Value *envPtr = nullptr;
    llvm::Function *thunk = nullptr;
    // MVP scope (#828): supported worker return types are Unit / int / float
    // / bool. The set is enforced uniformly via rejectIfUnsupportedThreadReturn
    // across inline-lambda and variable-reference workers.
    llvm::Type *workerRetTy = nullptr;
    int64_t resultSize = 0;

    // Case 1: inline lambda expression
    auto *lambda = std::get_if<std::unique_ptr<LambdaExpr>>(&e.args[0]->data);
    if (lambda) {
        LambdaExpr &lam = **lambda;

        // MVP scope rejections applied at annotation level. The actual
        // workerRetTy is derived from the emitted body's val->getType()
        // below (inferExprType falls back to i64 for several callable
        // shapes, which would tag bool/float returns as int — see #882
        // CodeRabbit review).
        //
        // (1) Block-bodied lambdas with a non-Unit return-type annotation:
        //     hooking ReturnStmt to write into the thread result slot is
        //     out of MVP scope (#879).
        if (!lam.expr_body && lam.return_type) {
            const std::string retTypeStr = lam.return_type->toString();
            if (retTypeStr != "Unit" && retTypeStr != "any") {
                cg.codegenError(
                    "threadSpawn() MVP (#828): block-bodied lambda with a "
                    "non-Unit return type is not supported; use an "
                    "expression-bodied lambda () => <expr> (tracked in #879)");
            }
        }
        // (2) Expression-bodied lambdas with an out-of-scope annotation
        //     are rejected early so the user gets the MVP error without
        //     walking the body first.
        if (lam.expr_body && lam.return_type) {
            if (llvm::Type *annotated = cg.resolveType(lam.return_type->toString()))
                rejectIfUnsupportedThreadReturn(cg, annotated);
        }

        auto referencedVars = collectReferencedVars(lam);
        std::vector<std::pair<std::string, llvm::AllocaInst*>> captures;
        std::unordered_set<std::string> seen;
        for (auto scopeIt = cg.scope_stack_.rbegin(); scopeIt != cg.scope_stack_.rend(); ++scopeIt) {
            for (const auto &[name, alloca] : *scopeIt) {
                if (seen.count(name) || !referencedVars.count(name))
                    continue;
                seen.insert(name);
                captures.push_back({name, alloca});
            }
        }

        std::vector<llvm::Type*> envFields;
        if (captures.empty())
            envFields.push_back(cg.i8Ty_);
        else
            for (const auto &[_, alloca] : captures)
                envFields.push_back(alloca->getAllocatedType());
        llvm::StructType *envTy = llvm::StructType::get(*cg.ctx_, envFields);

        const llvm::DataLayout &dl = cg.mod_->getDataLayout();
        uint64_t envSize = std::max<uint64_t>(1, dl.getTypeAllocSize(envTy));
        envPtr = cg.emitRuntimeCallDirect(
            "malloc", cg.ptrTy_, {cg.i64Ty_},
            {cg.emitConstInt(cg.i64Ty_, envSize)}, "thread_env");

        if (captures.empty()) {
            llvm::Value *dummyField = cg.emitStructGEP(envTy, envPtr, 0, "thread_env_dummy");
            cg.emitStore(cg.emitConstInt(cg.i8Ty_, 0), dummyField);
        } else {
            for (size_t i = 0; i < captures.size(); ++i) {
                llvm::Value *fieldPtr = cg.emitStructGEP(envTy, envPtr, static_cast<unsigned>(i), "thread_env_field");
                llvm::AllocaInst *src = captures[i].second;
                cg.emitStore(
                    cg.emitLoad(src->getAllocatedType(), src, (captures[i].first + ".thr_cap").c_str()),
                    fieldPtr);
            }
        }

        llvm::FunctionType *thunkTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*cg.ctx_), {cg.ptrTy_, cg.ptrTy_}, false);
        std::string thunkName = "__ry_thread." + std::to_string(cg.lambda_counter_++);
        thunk = cg.emitCreateFunction(
            thunkTy, llvm::Function::InternalLinkage, thunkName.c_str());

        {
            CodeGen::FnScope guard(cg);
            cg.fn_ = thunk;
            cg.pushScope();

            llvm::BasicBlock *entryBB = cg.createBBInFn("entry", thunk);
            cg.builder_.SetInsertPoint(entryBB);

            llvm::Value *envRaw = cg.emitGetParam(thunk, 0);
            envRaw->setName("env_raw");
            llvm::Value *resultRaw = cg.emitGetParam(thunk, 1);
            resultRaw->setName("result_raw");
            if (!captures.empty()) {
                for (size_t i = 0; i < captures.size(); ++i) {
                    const auto &[name, src] = captures[i];
                    llvm::Type *capTy = src->getAllocatedType();
                    llvm::Value *fieldPtr = cg.emitStructGEP(envTy, envRaw, static_cast<unsigned>(i), (name + ".field").c_str());
                    llvm::AllocaInst *dst = llvm::cast<llvm::AllocaInst>(cg.emitAlloca(capTy, name.c_str()));
                    cg.emitStore(cg.emitLoad(capTy, fieldPtr, (name + ".cap").c_str()), dst);
                    cg.scope_stack_.back()[name] = dst;

                    cg.propagateMeta(src, dst);
                }
            }

            if (lam.expr_body) {
                llvm::Value *val = cg.emitExpr(*lam.expr_body);
                // Source the worker return type from the emitted value so
                // we reflect what the body actually produced, not what
                // static inference guessed. inferExprType falls back to i64
                // for several callable shapes (see codegen_lambda.cpp:578),
                // which would silently mis-tag bool/float callbacks.
                workerRetTy = val->getType();
                rejectIfUnsupportedThreadReturn(cg, workerRetTy);
                emitStoreThreadResult(cg, val, workerRetTy, resultRaw);
            } else {
                // Block-bodied inline lambda: always Unit (the non-Unit
                // annotation case was rejected above).
                workerRetTy = llvm::Type::getVoidTy(*cg.ctx_);
                for (auto &stmt : lam.body)
                    std::visit([&cg](auto &st) { cg.emitStmt(st); }, stmt);
            }
            resultSize = workerRetTy->isVoidTy() ? 0 : 8;

            // popScope() must run BEFORE the void return because it emits ARC
            // release diamonds (Load + CondBr). If the BB is already
            // terminated those instructions would land after the terminator,
            // producing malformed IR that crashes LowerExpectIntrinsicPass (#1090).
            // If the body ended with an explicit return (already terminated),
            // ReturnStmt already called emitScopeCleanupToDepth(0) and drained
            // arc_managed_vars_; skip both to avoid post-terminator IR.
            if (!cg.builder_.GetInsertBlock()->getTerminator()) {
                cg.popScope();
                if (!cg.builder_.GetInsertBlock()->getTerminator())
                    cg.emitRet(nullptr);
            }
        }

        {
            std::string err;
            llvm::raw_string_ostream errStream(err);
            if (llvm::verifyFunction(*thunk, &errStream))
                cg.codegenError("Internal: malformed thread thunk IR: " + err);
        }

    } else {
        // Case 2: variable reference (named function or variable holding a
        // fn value). Both the capturing and non-capturing sub-branches surface
        // the worker's return value through the thread result slot, matching
        // Case 1's ABI; supported return types match Case 1's MVP set.
        llvm::Value *fnVal = cg.emitExpr(*e.args[0]);
        const llvm::DataLayout &dl = cg.mod_->getDataLayout();

        auto *fnInfoPtr = cg.lookupFnTypeInfo(fnVal);
        bool hasCaps = fnInfoPtr && !fnInfoPtr->capturedVars.empty();

        // Derive the worker's return type from FnTypeInfo. Named fns and
        // function references carry a statically-known return type via
        // codegen_expr.cpp's getOrCreateMeta() population. Fall back to Unit
        // when type info is unavailable (e.g. opaque function pointers).
        workerRetTy = (fnInfoPtr && fnInfoPtr->returnType)
            ? fnInfoPtr->returnType
            : llvm::Type::getVoidTy(*cg.ctx_);
        rejectIfUnsupportedThreadReturn(cg, workerRetTy);

        llvm::FunctionType *thunkTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*cg.ctx_), {cg.ptrTy_, cg.ptrTy_}, false);
        std::string thunkName = "__ry_thread_tramp." + std::to_string(cg.lambda_counter_++);
        thunk = cg.emitCreateFunction(
            thunkTy, llvm::Function::InternalLinkage, thunkName.c_str());

        if (hasCaps) {
            const CodeGen::FnTypeInfo info = *fnInfoPtr;

            std::vector<llvm::Type*> closureFields;
            closureFields.push_back(cg.ptrTy_);
            for (auto *ct : info.capturedTypes)
                closureFields.push_back(ct);
            llvm::StructType *closureTy = llvm::StructType::get(*cg.ctx_, closureFields);

            envPtr = cg.emitRuntimeCallDirect(
                "malloc", cg.ptrTy_, {cg.i64Ty_},
                {cg.emitConstInt(cg.i64Ty_, dl.getTypeAllocSize(closureTy))},
                "thread_env");

            llvm::Value *srcFnPtr = cg.emitStructGEP(closureTy, fnVal, 0, "src.fn_ptr");
            llvm::Value *dstFnPtr = cg.emitStructGEP(closureTy, envPtr, 0, "dst.fn_ptr");
            cg.emitStore(cg.emitLoad(cg.ptrTy_, srcFnPtr, "fn_ptr.val"), dstFnPtr);
            for (size_t i = 0; i < info.capturedTypes.size(); ++i) {
                std::string srcCapName = "src.cap." + std::to_string(i);
                std::string dstCapName = "dst.cap." + std::to_string(i);
                std::string capValName = "cap.val." + std::to_string(i);
                llvm::Value *srcCap = cg.emitStructGEP(closureTy, fnVal, static_cast<unsigned>(i + 1), srcCapName.c_str());
                llvm::Value *dstCap = cg.emitStructGEP(closureTy, envPtr, static_cast<unsigned>(i + 1), dstCapName.c_str());
                cg.emitStore(
                    cg.emitLoad(info.capturedTypes[i], srcCap, capValName.c_str()),
                    dstCap);
            }

            {
                CodeGen::FnScope guard(cg);
                cg.fn_ = thunk;
                llvm::BasicBlock *entryBB = cg.createBBInFn("entry", thunk);
                cg.builder_.SetInsertPoint(entryBB);

                // Thunk signature is (env, result_buf). For non-Unit workers
                // we write the return value into result_buf; Unit workers
                // leave it untouched.
                llvm::Value *envRaw = cg.emitGetParam(thunk, 0);
                envRaw->setName("env_raw");
                llvm::Value *resultRaw = cg.emitGetParam(thunk, 1);
                resultRaw->setName("result_raw");

                llvm::Value *loadedFnPtr = cg.emitLoad(
                    cg.ptrTy_,
                    cg.emitStructGEP(closureTy, envRaw, 0, "tramp.fn_ptr"),
                    "tramp.fn");

                std::vector<llvm::Value*> callArgs;
                std::vector<llvm::Type*> allParamTypes;
                for (size_t i = 0; i < info.capturedTypes.size(); ++i) {
                    std::string capFieldName = "tramp.cap." + std::to_string(i);
                    std::string capValName = "tramp.cap_val." + std::to_string(i);
                    llvm::Value *capField = cg.emitStructGEP(
                        closureTy, envRaw, static_cast<unsigned>(i + 1), capFieldName.c_str());
                    callArgs.push_back(cg.emitLoad(
                        info.capturedTypes[i], capField, capValName.c_str()));
                    allParamTypes.push_back(info.capturedTypes[i]);
                }

                llvm::FunctionType *callTy = llvm::FunctionType::get(
                    workerRetTy, allParamTypes, false);
                llvm::Value *callRet = cg.emitCallIndirect(callTy, loadedFnPtr, callArgs, "");
                emitStoreThreadResult(cg, callRet, workerRetTy, resultRaw);
                cg.emitRet(nullptr);
            }
        } else {
            llvm::Type *envFieldTypes[] = {cg.ptrTy_};
            llvm::StructType *envTy = llvm::StructType::get(*cg.ctx_, llvm::ArrayRef(envFieldTypes));
            envPtr = cg.emitRuntimeCallDirect(
                "malloc", cg.ptrTy_, {cg.i64Ty_},
                {cg.emitConstInt(cg.i64Ty_, dl.getTypeAllocSize(envTy))},
                "thread_env");
            llvm::Value *fnField = cg.emitStructGEP(envTy, envPtr, 0, "thread_env_fn");
            cg.emitStore(fnVal, fnField);

            {
                CodeGen::FnScope guard(cg);
                cg.fn_ = thunk;
                llvm::BasicBlock *entryBB = cg.createBBInFn("entry", thunk);
                cg.builder_.SetInsertPoint(entryBB);

                // Thunk signature is (env, result_buf). For non-Unit workers
                // we write the worker's return value into result_buf so the
                // matching threadJoin can unwrap it; Unit workers leave
                // result_buf untouched.
                llvm::Value *envRaw = cg.emitGetParam(thunk, 0);
                envRaw->setName("env_raw");
                llvm::Value *resultRaw = cg.emitGetParam(thunk, 1);
                resultRaw->setName("result_raw");

                llvm::Value *fnPtrField = cg.emitStructGEP(
                    envTy, envRaw, 0, "tramp.fn_ptr");
                llvm::Value *loadedFn = cg.emitLoad(cg.ptrTy_, fnPtrField, "tramp.fn");

                // Build the indirect call type from the worker's actual
                // return type so non-void returns land in a register
                // (sret-shaped returns have already been rejected by
                // rejectIfUnsupportedThreadReturn above).
                llvm::FunctionType *callTy = llvm::FunctionType::get(
                    workerRetTy, {}, false);
                llvm::Value *callRet = cg.emitCallIndirect(callTy, loadedFn, {}, "");
                emitStoreThreadResult(cg, callRet, workerRetTy, resultRaw);
                cg.emitRet(nullptr);
            }
        }
        resultSize = workerRetTy->isVoidTy() ? 0 : 8;
    }

    // Call __ry_thread_spawn(thunk, env, result_size).
    // thunk is `llvm::Function*` (already `ptr` under opaque pointers — no
    // bitcast needed); pass it directly as a Value*.
    llvm::Value *thread = cg.emitRuntimeCallDirect(
        "__ry_thread_spawn", cg.ptrTy_, {cg.ptrTy_, cg.ptrTy_, cg.i64Ty_},
        {thunk, envPtr, cg.emitConstInt(cg.i64Ty_, static_cast<uint64_t>(resultSize))},
        "thread");
    cg.addResourceKind(thread, rk_thread);
    // Attach the worker's return type as metadata so that emitThreadJoin
    // can emit the matching unwrap sequence. Unit workers receive no
    // metadata, which preserves the legacy Result<Unit, Error> join path.
    if (workerRetTy && !workerRetTy->isVoidTy())
        cg.setTypeMeta(CodeGen::TypeMeta::ThreadResult, thread, workerRetTy);
    return thread;
}

static llvm::Value *emitThreadJoin(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *thread = cg.emitExpr(*e.args[0]);
    if (!cg.isThread(thread))
        cg.codegenError("threadJoin() requires Thread argument");

    // ThreadResult metadata is attached by emitThreadSpawn; Unit workers
    // carry none and fall through to the legacy status-only path.
    llvm::Type *retTy = cg.getThreadResultType(thread);

    if (!retTy || retTy->isVoidTy()) {
        llvm::Value *nullBuf =
            llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(cg.ptrTy_));
        llvm::Value *status = cg.emitRuntimeCallDirect(
            "__ry_thread_join", cg.i64Ty_, {cg.ptrTy_, cg.ptrTy_},
            {thread, nullBuf}, "join_status");
        return cg.wrapStatusAsResult(status);
    }

    // Mirrors the ResultOutParam pattern (src/codegen_call_native.cpp:283-308).
    // bool uses an i64 slot because the worker zext'd the i1 value on store
    // so the full 8-byte ThreadHandle slot is initialized.
    llvm::Type *slotTy = retTy->isIntegerTy(1) ? cg.i64Ty_ : retTy;
    llvm::AllocaInst *outSlot =
        llvm::cast<llvm::AllocaInst>(cg.emitAlloca(slotTy, "thread_join_out"));
    llvm::Value *status = cg.emitRuntimeCallDirect(
        "__ry_thread_join", cg.i64Ty_, {cg.ptrTy_, cg.ptrTy_},
        {thread, outSlot}, "join_status");
    llvm::Value *isErr = cg.emitICmpNE(
        status, cg.emitConstInt(cg.i64Ty_, 0), "thread_join_err");
    llvm::StructType *resTy = cg.getResultType(retTy, cg.errorTy_);
    return cg.emitResultBranch(
        isErr, resTy,
        [&]() -> llvm::Value * {
            llvm::Value *loaded =
                cg.emitLoad(slotTy, outSlot, "thread_join_val");
            if (retTy->isIntegerTy(1))
                loaded = cg.emitTrunc(loaded, cg.i1Ty_, "thread_join_bool");
            return cg.buildOkValue(loaded, resTy);
        },
        [&]() -> llvm::Value * {
            return cg.buildErrValue(cg.buildErrorFromRuntime(), resTy);
        });
}

// lockNew, rwlockNew: 0-arg → ptr + resource tracking
static llvm::Value *emitThreadSyncNew(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 0);
    const char *rtName;
    int rk;
    if (e.callee == "lockNew") { rtName = "__ry_lock_new"; rk = rk_lock; }
    else { rtName = "__ry_rwlock_new"; rk = rk_rwlock; }
    llvm::Value *result = cg.emitRuntimeCallDirect(
        rtName, cg.ptrTy_, {}, {}, e.callee.c_str());
    cg.addResourceKind(result, rk);
    return result;
}

// semaphoreNew, barrierNew: 1-arg → wrapPtrAsResult + resource tracking
static llvm::Value *emitThreadSyncResultNew(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *count = cg.emitExpr(*e.args[0]);
    const char *rtName;
    int rk;
    if (e.callee == "semaphoreNew") { rtName = "__ry_semaphore_new"; rk = rk_semaphore; }
    else { rtName = "__ry_barrier_new"; rk = rk_barrier; }
    llvm::Value *ptr = cg.emitRuntimeCallDirect(
        rtName, cg.ptrTy_, {cg.i64Ty_}, {count}, e.callee.c_str());
    llvm::Value *result = cg.wrapPtrAsResult(ptr);
    cg.addResourceKind(result, rk);
    return result;
}

// Status-returning operations: acquire, release, lock, unlock, wait
static llvm::Value *emitThreadSyncOp(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *arg = cg.emitExpr(*e.args[0]);

    // Type-check and derive runtime name
    struct OpInfo { const char *rt; bool (CodeGen::*check)(llvm::Value*); const char *type; };
    static const std::unordered_map<std::string, OpInfo> ops = {
        {"lockAcquire",     {"__ry_lock_acquire",     &CodeGen::isLock,      "Lock"}},
        {"lockRelease",     {"__ry_lock_release",     &CodeGen::isLock,      "Lock"}},
        {"rwlockReadLock",  {"__ry_rwlock_read_lock", &CodeGen::isRWLock,    "RWLock"}},
        {"rwlockWriteLock", {"__ry_rwlock_write_lock",&CodeGen::isRWLock,    "RWLock"}},
        {"rwlockUnlock",    {"__ry_rwlock_unlock",    &CodeGen::isRWLock,    "RWLock"}},
        {"semaphoreAcquire",{"__ry_semaphore_acquire",&CodeGen::isSemaphore, "Semaphore"}},
        {"semaphoreRelease",{"__ry_semaphore_release",&CodeGen::isSemaphore, "Semaphore"}},
        {"barrierWait",     {"__ry_barrier_wait",     &CodeGen::isBarrier,   "Barrier"}},
    };

    auto it = ops.find(e.callee);
    if (it == ops.end()) return nullptr;
    if (!(cg.*(it->second.check))(arg))
        cg.codegenError(e.callee + "() requires " + it->second.type + " argument");
    std::string statusName = e.callee + "_status";
    llvm::Value *status = cg.emitRuntimeCallDirect(
        it->second.rt, cg.i64Ty_, {cg.ptrTy_}, {arg}, statusName.c_str());
    return cg.wrapStatusAsResult(status);
}

// All *_free operations: type-check + emitResourceFree
static llvm::Value *emitThreadSyncFree(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *arg = cg.emitExpr(*e.args[0]);

    struct FreeInfo { int rk; const char *type; };
    // rk_* are populated during static init, before any codegen runs.
    static const std::unordered_map<std::string, FreeInfo> frees = {
        {"lockFree",        {rk_lock,        "Lock"}},
        {"rwlockFree",      {rk_rwlock,      "RWLock"}},
        {"semaphoreFree",   {rk_semaphore,   "Semaphore"}},
        {"barrierFree",     {rk_barrier,     "Barrier"}},
        {"atomicIntFree",   {rk_atomic_int,  "AtomicInt"}},
        {"atomicBoolFree",  {rk_atomic_bool, "AtomicBool"}},
    };

    auto it = frees.find(e.callee);
    if (it == frees.end()) return nullptr;
    if (!cg.isResourceKind(it->second.rk, arg))
        cg.codegenError(e.callee + "() requires " + it->second.type + " argument");
    return cg.emitResourceFree(arg, it->second.rk, *e.args[0]);
}

static llvm::Value *emitThreadAtomicIntNew(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *val = cg.emitExpr(*e.args[0]);
    llvm::Value *atom = cg.emitRuntimeCallDirect(
        "__ry_atomic_int_new", cg.ptrTy_, {cg.i64Ty_}, {val}, "atomic_int");
    cg.addResourceKind(atom, rk_atomic_int);
    return atom;
}

// atomicIntLoad, store, add, sub
static llvm::Value *emitThreadAtomicIntOp(CodeGen &cg, const CallExpr &e) {
    if (e.args.empty()) return nullptr;
    llvm::Value *atom = cg.emitExpr(*e.args[0]);
    if (!cg.isAtomicInt(atom))
        cg.codegenError(e.callee + "() requires AtomicInt as first argument");

    if (e.callee == "atomicIntLoad") {
        cg.requireArgs(e, 1);
        return cg.emitRuntimeCallDirect(
            "__ry_atomic_int_load", cg.i64Ty_, {cg.ptrTy_}, {atom}, "atomic_int_load");
    }
    if (e.callee == "atomicIntStore") {
        cg.requireArgs(e, 2);
        llvm::Value *val = cg.emitExpr(*e.args[1]);
        return cg.emitRuntimeCallDirect(
            "__ry_atomic_int_store", llvm::Type::getVoidTy(*cg.ctx_),
            {cg.ptrTy_, cg.i64Ty_}, {atom, val}, "");
    }
    // add, sub
    cg.requireArgs(e, 2);
    llvm::Value *delta = cg.emitExpr(*e.args[1]);
    const char *rtName = nullptr;
    if (e.callee == "atomicIntAdd") {
        rtName = "__ry_atomic_int_add";
    } else if (e.callee == "atomicIntSub") {
        rtName = "__ry_atomic_int_sub";
    } else {
        cg.codegenError("emitThreadAtomicIntOp: unsupported callee '" + e.callee + "'");
    }
    return cg.emitRuntimeCallDirect(
        rtName, cg.i64Ty_, {cg.ptrTy_, cg.i64Ty_}, {atom, delta}, e.callee.c_str());
}

static llvm::Value *emitThreadAtomicIntCas(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 3);
    llvm::Value *atom = cg.emitExpr(*e.args[0]);
    if (!cg.isAtomicInt(atom))
        cg.codegenError("atomicIntCas() requires AtomicInt as first argument");
    llvm::Value *expected = cg.emitExpr(*e.args[1]);
    llvm::Value *desired = cg.emitExpr(*e.args[2]);
    llvm::Value *result = cg.emitRuntimeCallDirect(
        "__ry_atomic_int_cas", cg.i64Ty_, {cg.ptrTy_, cg.i64Ty_, cg.i64Ty_},
        {atom, expected, desired}, "atomic_int_cas");
    return cg.emitTrunc(result, cg.i1Ty_, "atomic_int_cas_bool");
}

static llvm::Value *emitThreadAtomicBoolNew(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *val = cg.emitExpr(*e.args[0]);
    if (val->getType() != cg.i1Ty_)
        cg.codegenError("atomicBoolNew() requires bool argument");
    llvm::Value *extended = cg.emitZExt(val, cg.i64Ty_, "atomic_bool_ext");
    llvm::Value *atom = cg.emitRuntimeCallDirect(
        "__ry_atomic_bool_new", cg.ptrTy_, {cg.i64Ty_}, {extended}, "atomic_bool");
    cg.addResourceKind(atom, rk_atomic_bool);
    return atom;
}

// atomicBoolLoad, atomicBoolStore
static llvm::Value *emitThreadAtomicBoolOp(CodeGen &cg, const CallExpr &e) {
    if (e.args.empty()) return nullptr;
    llvm::Value *atom = cg.emitExpr(*e.args[0]);
    if (!cg.isAtomicBool(atom))
        cg.codegenError(e.callee + "() requires AtomicBool as first argument");

    if (e.callee == "atomicBoolLoad") {
        cg.requireArgs(e, 1);
        llvm::Value *result = cg.emitRuntimeCallDirect(
            "__ry_atomic_bool_load", cg.i64Ty_, {cg.ptrTy_}, {atom}, "atomic_bool_load");
        return cg.emitTrunc(result, cg.i1Ty_, "atomic_bool_load_bool");
    }
    // atomicBoolStore
    cg.requireArgs(e, 2);
    llvm::Value *val = cg.emitExpr(*e.args[1]);
    if (val->getType() != cg.i1Ty_)
        cg.codegenError("atomicBoolStore() requires bool as second argument");
    llvm::Value *extended = cg.emitZExt(val, cg.i64Ty_, "atomic_bool_store_ext");
    return cg.emitRuntimeCallDirect(
        "__ry_atomic_bool_store", llvm::Type::getVoidTy(*cg.ctx_),
        {cg.ptrTy_, cg.i64Ty_}, {atom, extended}, "");
}

// ===== Thread dispatch table =====

static const CodeGen::NativeDispatchEntry thread_table[] = {
    {"threadSpawn",      nullptr, {}, 0, nullptr, emitThreadSpawn},
    {"threadJoin",       nullptr, {}, 0, nullptr, emitThreadJoin},
    // Sync primitives: new
    {"lockNew",          nullptr, {}, 0, nullptr, emitThreadSyncNew},
    {"rwlockNew",        nullptr, {}, 0, nullptr, emitThreadSyncNew},
    {"semaphoreNew",     nullptr, {}, 0, nullptr, emitThreadSyncResultNew},
    {"barrierNew",       nullptr, {}, 0, nullptr, emitThreadSyncResultNew},
    // Sync primitives: operations
    {"lockAcquire",      nullptr, {}, 0, nullptr, emitThreadSyncOp},
    {"lockRelease",      nullptr, {}, 0, nullptr, emitThreadSyncOp},
    {"rwlockReadLock",   nullptr, {}, 0, nullptr, emitThreadSyncOp},
    {"rwlockWriteLock",  nullptr, {}, 0, nullptr, emitThreadSyncOp},
    {"rwlockUnlock",     nullptr, {}, 0, nullptr, emitThreadSyncOp},
    {"semaphoreAcquire", nullptr, {}, 0, nullptr, emitThreadSyncOp},
    {"semaphoreRelease", nullptr, {}, 0, nullptr, emitThreadSyncOp},
    {"barrierWait",      nullptr, {}, 0, nullptr, emitThreadSyncOp},
    // Sync primitives: free
    {"lockFree",         nullptr, {}, 0, nullptr, emitThreadSyncFree},
    {"rwlockFree",       nullptr, {}, 0, nullptr, emitThreadSyncFree},
    {"semaphoreFree",    nullptr, {}, 0, nullptr, emitThreadSyncFree},
    {"barrierFree",      nullptr, {}, 0, nullptr, emitThreadSyncFree},
    // AtomicInt
    {"atomicIntNew",     nullptr, {}, 0, nullptr, emitThreadAtomicIntNew},
    {"atomicIntLoad",    nullptr, {}, 0, nullptr, emitThreadAtomicIntOp},
    {"atomicIntStore",   nullptr, {}, 0, nullptr, emitThreadAtomicIntOp},
    {"atomicIntAdd",     nullptr, {}, 0, nullptr, emitThreadAtomicIntOp},
    {"atomicIntSub",     nullptr, {}, 0, nullptr, emitThreadAtomicIntOp},
    {"atomicIntCas",     nullptr, {}, 0, nullptr, emitThreadAtomicIntCas},
    {"atomicIntFree",    nullptr, {}, 0, nullptr, emitThreadSyncFree},
    // AtomicBool
    {"atomicBoolNew",    nullptr, {}, 0, nullptr, emitThreadAtomicBoolNew},
    {"atomicBoolLoad",   nullptr, {}, 0, nullptr, emitThreadAtomicBoolOp},
    {"atomicBoolStore",  nullptr, {}, 0, nullptr, emitThreadAtomicBoolOp},
    {"atomicBoolFree",   nullptr, {}, 0, nullptr, emitThreadSyncFree},
};

RY_REGISTER_STDLIB_PACKAGE(thread, "share/std/thread/thread.ry", dispatchThread)
static llvm::Value *dispatchThread(CodeGen &cg, const CallExpr &e) {
    return cg.emitTableDrivenNativeCall(e, "thread", thread_table, std::size(thread_table));
}

} // namespace ry
