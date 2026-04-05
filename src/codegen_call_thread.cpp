#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"
#include <functional>

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
            if constexpr (std::is_same_v<T, AssignStmt>) {
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
            } else if constexpr (std::is_same_v<T, std::unique_ptr<WhenCondStmt>>) {
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
            } else if constexpr (std::is_same_v<T, std::unique_ptr<MatchStmt>>) {
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

llvm::Value *CodeGen::emitThreadSpawn(const CallExpr &e) {
    requireArgs(e, 1);

    llvm::Value *envPtr = nullptr;
    llvm::Function *thunk = nullptr;

    // Case 1: inline lambda expression
    auto *lambda = std::get_if<std::unique_ptr<LambdaExpr>>(&e.args[0]->data);
    if (lambda) {
        LambdaExpr &lam = **lambda;

        auto referencedVars = collectReferencedVars(lam);
        std::vector<std::pair<std::string, llvm::AllocaInst*>> captures;
        std::unordered_set<std::string> seen;
        for (auto scopeIt = scope_stack_.rbegin(); scopeIt != scope_stack_.rend(); ++scopeIt) {
            for (const auto &[name, alloca] : *scopeIt) {
                if (seen.count(name) || !referencedVars.count(name))
                    continue;
                seen.insert(name);
                captures.push_back({name, alloca});
            }
        }

        std::vector<llvm::Type*> envFields;
        if (captures.empty())
            envFields.push_back(i8Ty_);
        else
            for (const auto &[_, alloca] : captures)
                envFields.push_back(alloca->getAllocatedType());
        llvm::StructType *envTy = llvm::StructType::get(*ctx_, envFields);

        llvm::FunctionCallee mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();
        uint64_t envSize = std::max<uint64_t>(1, dl.getTypeAllocSize(envTy));
        envPtr = builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(i64Ty_, envSize)}, "thread_env");

        if (captures.empty()) {
            llvm::Value *dummyField = builder_.CreateStructGEP(envTy, envPtr, 0, "thread_env_dummy");
            builder_.CreateStore(llvm::ConstantInt::get(i8Ty_, 0), dummyField);
        } else {
            for (size_t i = 0; i < captures.size(); ++i) {
                llvm::Value *fieldPtr = builder_.CreateStructGEP(envTy, envPtr, i, "thread_env_field");
                llvm::AllocaInst *src = captures[i].second;
                builder_.CreateStore(
                    builder_.CreateLoad(src->getAllocatedType(), src, captures[i].first + ".thr_cap"),
                    fieldPtr);
            }
        }

        llvm::FunctionType *thunkTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
        thunk = llvm::Function::Create(
            thunkTy, llvm::Function::InternalLinkage,
            "__ry_thread." + std::to_string(lambda_counter_++), *mod_);

        {
            FnScope guard(*this);
            fn_ = thunk;
            pushScope();

            llvm::BasicBlock *entryBB = llvm::BasicBlock::Create(*ctx_, "entry", thunk);
            builder_.SetInsertPoint(entryBB);

            llvm::Value *envRaw = &*thunk->arg_begin();
            envRaw->setName("env_raw");
            if (!captures.empty()) {
                for (size_t i = 0; i < captures.size(); ++i) {
                    const auto &[name, src] = captures[i];
                    llvm::Type *capTy = src->getAllocatedType();
                    llvm::Value *fieldPtr = builder_.CreateStructGEP(envTy, envRaw, i, name + ".field");
                    llvm::AllocaInst *dst = builder_.CreateAlloca(capTy, nullptr, name);
                    builder_.CreateStore(builder_.CreateLoad(capTy, fieldPtr, name + ".cap"), dst);
                    scope_stack_.back()[name] = dst;

                    propagateAllMetadata(src, dst);
                }
            }

            if (lam.expr_body) {
                emitExpr(*lam.expr_body);
            } else {
                for (auto &stmt : lam.body)
                    std::visit([this](auto &st) { emitStmt(st); }, stmt);
            }

            if (!builder_.GetInsertBlock()->getTerminator())
                builder_.CreateRetVoid();

            popScope();
        }

    } else {
        // Case 2: variable reference (named function or variable holding fn() -> Unit)
        llvm::Value *fnVal = emitExpr(*e.args[0]);
        llvm::FunctionCallee mallocFn = getStdlibMalloc();
        const llvm::DataLayout &dl = mod_->getDataLayout();

        auto fnInfoIt = lookupFnTypeInfo(fnVal);
        bool hasCaps = fnInfoIt != fn_type_info_.end() && !fnInfoIt->second.capturedVars.empty();

        llvm::FunctionType *thunkTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*ctx_), {ptrTy_}, false);
        thunk = llvm::Function::Create(
            thunkTy, llvm::Function::InternalLinkage,
            "__ry_thread_tramp." + std::to_string(lambda_counter_++), *mod_);

        if (hasCaps) {
            const FnTypeInfo &info = fnInfoIt->second;

            std::vector<llvm::Type*> closureFields;
            closureFields.push_back(ptrTy_);
            for (auto *ct : info.capturedTypes)
                closureFields.push_back(ct);
            llvm::StructType *closureTy = llvm::StructType::get(*ctx_, closureFields);

            envPtr = builder_.CreateCall(
                mallocFn,
                {llvm::ConstantInt::get(i64Ty_, dl.getTypeAllocSize(closureTy))},
                "thread_env");

            llvm::Value *srcFnPtr = builder_.CreateStructGEP(closureTy, fnVal, 0, "src.fn_ptr");
            llvm::Value *dstFnPtr = builder_.CreateStructGEP(closureTy, envPtr, 0, "dst.fn_ptr");
            builder_.CreateStore(builder_.CreateLoad(ptrTy_, srcFnPtr, "fn_ptr.val"), dstFnPtr);
            for (size_t i = 0; i < info.capturedTypes.size(); ++i) {
                llvm::Value *srcCap = builder_.CreateStructGEP(closureTy, fnVal, i + 1, "src.cap." + std::to_string(i));
                llvm::Value *dstCap = builder_.CreateStructGEP(closureTy, envPtr, i + 1, "dst.cap." + std::to_string(i));
                builder_.CreateStore(
                    builder_.CreateLoad(info.capturedTypes[i], srcCap, "cap.val." + std::to_string(i)),
                    dstCap);
            }

            {
                FnScope guard(*this);
                fn_ = thunk;
                llvm::BasicBlock *entryBB = llvm::BasicBlock::Create(*ctx_, "entry", thunk);
                builder_.SetInsertPoint(entryBB);

                llvm::Value *envRaw = &*thunk->arg_begin();
                envRaw->setName("env_raw");

                llvm::Value *loadedFnPtr = builder_.CreateLoad(
                    ptrTy_,
                    builder_.CreateStructGEP(closureTy, envRaw, 0, "tramp.fn_ptr"),
                    "tramp.fn");

                std::vector<llvm::Value*> callArgs;
                std::vector<llvm::Type*> allParamTypes;
                for (size_t i = 0; i < info.capturedTypes.size(); ++i) {
                    llvm::Value *capField = builder_.CreateStructGEP(
                        closureTy, envRaw, i + 1, "tramp.cap." + std::to_string(i));
                    callArgs.push_back(builder_.CreateLoad(
                        info.capturedTypes[i], capField, "tramp.cap_val." + std::to_string(i)));
                    allParamTypes.push_back(info.capturedTypes[i]);
                }

                llvm::FunctionType *callTy = llvm::FunctionType::get(
                    llvm::Type::getVoidTy(*ctx_), allParamTypes, false);
                builder_.CreateCall(callTy, loadedFnPtr, callArgs);
                builder_.CreateRetVoid();
            }
        } else {
            llvm::Type *envFieldTypes[] = {ptrTy_};
            llvm::StructType *envTy = llvm::StructType::get(*ctx_, llvm::ArrayRef(envFieldTypes));
            envPtr = builder_.CreateCall(
                mallocFn,
                {llvm::ConstantInt::get(i64Ty_, dl.getTypeAllocSize(envTy))},
                "thread_env");
            llvm::Value *fnField = builder_.CreateStructGEP(envTy, envPtr, 0, "thread_env_fn");
            builder_.CreateStore(fnVal, fnField);

            {
                FnScope guard(*this);
                fn_ = thunk;
                llvm::BasicBlock *entryBB = llvm::BasicBlock::Create(*ctx_, "entry", thunk);
                builder_.SetInsertPoint(entryBB);

                llvm::Value *envRaw = &*thunk->arg_begin();
                envRaw->setName("env_raw");

                llvm::Value *fnPtrField = builder_.CreateStructGEP(
                    envTy, envRaw, 0, "tramp.fn_ptr");
                llvm::Value *loadedFn = builder_.CreateLoad(ptrTy_, fnPtrField, "tramp.fn");

                llvm::FunctionType *callTy = llvm::FunctionType::get(
                    llvm::Type::getVoidTy(*ctx_), {}, false);
                builder_.CreateCall(callTy, loadedFn);
                builder_.CreateRetVoid();
            }
        }
    }

    // Call __ry_thread_spawn(thunk, env)
    auto spawnTy = llvm::FunctionType::get(ptrTy_, {ptrTy_, ptrTy_}, false);
    auto spawnFn = mod_->getOrInsertFunction("__ry_thread_spawn", spawnTy);
    llvm::Value *thread = builder_.CreateCall(
        spawnFn, {thunk, envPtr}, "thread");
    resource_sets_[RK_Thread].insert(thread);
    return thread;
}

llvm::Value *CodeGen::emitThreadJoin(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *thread = emitExpr(*e.args[0]);
    if (!isThread(thread))
        codegenError("thread_join() requires Thread argument");
    auto fn = getRuntimeFn("__ry_thread_join", i64Ty_, {ptrTy_});
    llvm::Value *status = builder_.CreateCall(fn, {thread}, "join_status");
    return wrapStatusAsResult(status);
}

// lock_new, rwlock_new: 0-arg → ptr + resource tracking
llvm::Value *CodeGen::emitThreadSyncNew(const CallExpr &e) {
    requireArgs(e, 0);
    const char *rtName;
    ResourceKind rk;
    if (e.callee == "lock_new") { rtName = "__ry_lock_new"; rk = RK_Lock; }
    else { rtName = "__ry_rwlock_new"; rk = RK_RWLock; }
    auto fn = getRuntimeFn(rtName, ptrTy_, {});
    llvm::Value *result = builder_.CreateCall(fn, {}, e.callee);
    resource_sets_[rk].insert(result);
    return result;
}

// semaphore_new, barrier_new: 1-arg → wrapPtrAsResult + resource tracking
llvm::Value *CodeGen::emitThreadSyncResultNew(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *count = emitExpr(*e.args[0]);
    const char *rtName;
    ResourceKind rk;
    if (e.callee == "semaphore_new") { rtName = "__ry_semaphore_new"; rk = RK_Semaphore; }
    else { rtName = "__ry_barrier_new"; rk = RK_Barrier; }
    auto fn = getRuntimeFn(rtName, ptrTy_, {i64Ty_});
    llvm::Value *ptr = builder_.CreateCall(fn, {count}, e.callee);
    llvm::Value *result = wrapPtrAsResult(ptr);
    resource_sets_[rk].insert(result);
    return result;
}

// Status-returning operations: acquire, release, lock, unlock, wait
llvm::Value *CodeGen::emitThreadSyncOp(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *arg = emitExpr(*e.args[0]);

    // Type-check and derive runtime name
    struct OpInfo { const char *rt; bool (CodeGen::*check)(llvm::Value*); const char *type; };
    static const std::unordered_map<std::string, OpInfo> ops = {
        {"lock_acquire",     {"__ry_lock_acquire",     &CodeGen::isLock,      "Lock"}},
        {"lock_release",     {"__ry_lock_release",     &CodeGen::isLock,      "Lock"}},
        {"rwlock_read_lock", {"__ry_rwlock_read_lock", &CodeGen::isRWLock,    "RWLock"}},
        {"rwlock_write_lock",{"__ry_rwlock_write_lock",&CodeGen::isRWLock,    "RWLock"}},
        {"rwlock_unlock",    {"__ry_rwlock_unlock",    &CodeGen::isRWLock,    "RWLock"}},
        {"semaphore_acquire",{"__ry_semaphore_acquire",&CodeGen::isSemaphore, "Semaphore"}},
        {"semaphore_release",{"__ry_semaphore_release",&CodeGen::isSemaphore, "Semaphore"}},
        {"barrier_wait",     {"__ry_barrier_wait",     &CodeGen::isBarrier,   "Barrier"}},
    };

    auto it = ops.find(e.callee);
    if (it == ops.end()) return nullptr;
    if (!(this->*(it->second.check))(arg))
        codegenError(e.callee + "() requires " + it->second.type + " argument");
    auto fn = getRuntimeFn(it->second.rt, i64Ty_, {ptrTy_});
    llvm::Value *status = builder_.CreateCall(fn, {arg}, e.callee + "_status");
    return wrapStatusAsResult(status);
}

// All *_free operations: type-check + emitResourceFree
llvm::Value *CodeGen::emitThreadSyncFree(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *arg = emitExpr(*e.args[0]);

    struct FreeInfo { bool (CodeGen::*check)(llvm::Value*); ResourceKind rk; const char *type; };
    static const std::unordered_map<std::string, FreeInfo> frees = {
        {"lock_free",        {&CodeGen::isLock,       RK_Lock,       "Lock"}},
        {"rwlock_free",      {&CodeGen::isRWLock,     RK_RWLock,     "RWLock"}},
        {"semaphore_free",   {&CodeGen::isSemaphore,  RK_Semaphore,  "Semaphore"}},
        {"barrier_free",     {&CodeGen::isBarrier,    RK_Barrier,    "Barrier"}},
        {"atomic_int_free",  {&CodeGen::isAtomicInt,  RK_AtomicInt,  "AtomicInt"}},
        {"atomic_bool_free", {&CodeGen::isAtomicBool, RK_AtomicBool, "AtomicBool"}},
    };

    auto it = frees.find(e.callee);
    if (it == frees.end()) return nullptr;
    if (!(this->*(it->second.check))(arg))
        codegenError(e.callee + "() requires " + it->second.type + " argument");
    return emitResourceFree(arg, it->second.rk, *e.args[0]);
}

llvm::Value *CodeGen::emitThreadAtomicIntNew(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *val = emitExpr(*e.args[0]);
    auto fn = getRuntimeFn("__ry_atomic_int_new", ptrTy_, {i64Ty_});
    llvm::Value *atom = builder_.CreateCall(fn, {val}, "atomic_int");
    resource_sets_[RK_AtomicInt].insert(atom);
    return atom;
}

// atomic_int_load, store, add, sub
llvm::Value *CodeGen::emitThreadAtomicIntOp(const CallExpr &e) {
    if (e.args.empty()) return nullptr;
    llvm::Value *atom = emitExpr(*e.args[0]);
    if (!isAtomicInt(atom))
        codegenError(e.callee + "() requires AtomicInt as first argument");

    if (e.callee == "atomic_int_load") {
        requireArgs(e, 1);
        auto fn = getRuntimeFn("__ry_atomic_int_load", i64Ty_, {ptrTy_});
        return builder_.CreateCall(fn, {atom}, "atomic_int_load");
    }
    if (e.callee == "atomic_int_store") {
        requireArgs(e, 2);
        llvm::Value *val = emitExpr(*e.args[1]);
        auto fn = getRuntimeFn("__ry_atomic_int_store", llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_});
        return builder_.CreateCall(fn, {atom, val});
    }
    // add, sub
    requireArgs(e, 2);
    llvm::Value *delta = emitExpr(*e.args[1]);
    std::string rtName = "__ry_" + e.callee;
    auto fn = getRuntimeFn(rtName.c_str(), i64Ty_, {ptrTy_, i64Ty_});
    return builder_.CreateCall(fn, {atom, delta}, e.callee);
}

llvm::Value *CodeGen::emitThreadAtomicIntCas(const CallExpr &e) {
    requireArgs(e, 3);
    llvm::Value *atom = emitExpr(*e.args[0]);
    if (!isAtomicInt(atom))
        codegenError("atomic_int_cas() requires AtomicInt as first argument");
    llvm::Value *expected = emitExpr(*e.args[1]);
    llvm::Value *desired = emitExpr(*e.args[2]);
    auto fn = getRuntimeFn("__ry_atomic_int_cas", i64Ty_, {ptrTy_, i64Ty_, i64Ty_});
    llvm::Value *result = builder_.CreateCall(fn, {atom, expected, desired}, "atomic_int_cas");
    return builder_.CreateTrunc(result, i1Ty_, "atomic_int_cas_bool");
}

llvm::Value *CodeGen::emitThreadAtomicBoolNew(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *val = emitExpr(*e.args[0]);
    if (val->getType() != i1Ty_)
        codegenError("atomic_bool_new() requires bool argument");
    llvm::Value *extended = builder_.CreateZExt(val, i64Ty_, "atomic_bool_ext");
    auto fn = getRuntimeFn("__ry_atomic_bool_new", ptrTy_, {i64Ty_});
    llvm::Value *atom = builder_.CreateCall(fn, {extended}, "atomic_bool");
    resource_sets_[RK_AtomicBool].insert(atom);
    return atom;
}

// atomic_bool_load, atomic_bool_store
llvm::Value *CodeGen::emitThreadAtomicBoolOp(const CallExpr &e) {
    if (e.args.empty()) return nullptr;
    llvm::Value *atom = emitExpr(*e.args[0]);
    if (!isAtomicBool(atom))
        codegenError(e.callee + "() requires AtomicBool as first argument");

    if (e.callee == "atomic_bool_load") {
        requireArgs(e, 1);
        auto fn = getRuntimeFn("__ry_atomic_bool_load", i64Ty_, {ptrTy_});
        llvm::Value *result = builder_.CreateCall(fn, {atom}, "atomic_bool_load");
        return builder_.CreateTrunc(result, i1Ty_, "atomic_bool_load_bool");
    }
    // atomic_bool_store
    requireArgs(e, 2);
    llvm::Value *val = emitExpr(*e.args[1]);
    if (val->getType() != i1Ty_)
        codegenError("atomic_bool_store() requires bool as second argument");
    llvm::Value *extended = builder_.CreateZExt(val, i64Ty_, "atomic_bool_store_ext");
    auto fn = getRuntimeFn("__ry_atomic_bool_store", llvm::Type::getVoidTy(*ctx_), {ptrTy_, i64Ty_});
    return builder_.CreateCall(fn, {atom, extended});
}

// ===== Thread dispatch table =====

static const CodeGen::NativeDispatchEntry thread_table[] = {
    {"thread_spawn",      nullptr, {}, 0, nullptr, &CodeGen::emitThreadSpawn},
    {"thread_join",       nullptr, {}, 0, nullptr, &CodeGen::emitThreadJoin},
    // Sync primitives: new
    {"lock_new",          nullptr, {}, 0, nullptr, &CodeGen::emitThreadSyncNew},
    {"rwlock_new",        nullptr, {}, 0, nullptr, &CodeGen::emitThreadSyncNew},
    {"semaphore_new",     nullptr, {}, 0, nullptr, &CodeGen::emitThreadSyncResultNew},
    {"barrier_new",       nullptr, {}, 0, nullptr, &CodeGen::emitThreadSyncResultNew},
    // Sync primitives: operations
    {"lock_acquire",      nullptr, {}, 0, nullptr, &CodeGen::emitThreadSyncOp},
    {"lock_release",      nullptr, {}, 0, nullptr, &CodeGen::emitThreadSyncOp},
    {"rwlock_read_lock",  nullptr, {}, 0, nullptr, &CodeGen::emitThreadSyncOp},
    {"rwlock_write_lock", nullptr, {}, 0, nullptr, &CodeGen::emitThreadSyncOp},
    {"rwlock_unlock",     nullptr, {}, 0, nullptr, &CodeGen::emitThreadSyncOp},
    {"semaphore_acquire", nullptr, {}, 0, nullptr, &CodeGen::emitThreadSyncOp},
    {"semaphore_release", nullptr, {}, 0, nullptr, &CodeGen::emitThreadSyncOp},
    {"barrier_wait",      nullptr, {}, 0, nullptr, &CodeGen::emitThreadSyncOp},
    // Sync primitives: free
    {"lock_free",         nullptr, {}, 0, nullptr, &CodeGen::emitThreadSyncFree},
    {"rwlock_free",       nullptr, {}, 0, nullptr, &CodeGen::emitThreadSyncFree},
    {"semaphore_free",    nullptr, {}, 0, nullptr, &CodeGen::emitThreadSyncFree},
    {"barrier_free",      nullptr, {}, 0, nullptr, &CodeGen::emitThreadSyncFree},
    // AtomicInt
    {"atomic_int_new",    nullptr, {}, 0, nullptr, &CodeGen::emitThreadAtomicIntNew},
    {"atomic_int_load",   nullptr, {}, 0, nullptr, &CodeGen::emitThreadAtomicIntOp},
    {"atomic_int_store",  nullptr, {}, 0, nullptr, &CodeGen::emitThreadAtomicIntOp},
    {"atomic_int_add",    nullptr, {}, 0, nullptr, &CodeGen::emitThreadAtomicIntOp},
    {"atomic_int_sub",    nullptr, {}, 0, nullptr, &CodeGen::emitThreadAtomicIntOp},
    {"atomic_int_cas",    nullptr, {}, 0, nullptr, &CodeGen::emitThreadAtomicIntCas},
    {"atomic_int_free",   nullptr, {}, 0, nullptr, &CodeGen::emitThreadSyncFree},
    // AtomicBool
    {"atomic_bool_new",   nullptr, {}, 0, nullptr, &CodeGen::emitThreadAtomicBoolNew},
    {"atomic_bool_load",  nullptr, {}, 0, nullptr, &CodeGen::emitThreadAtomicBoolOp},
    {"atomic_bool_store", nullptr, {}, 0, nullptr, &CodeGen::emitThreadAtomicBoolOp},
    {"atomic_bool_free",  nullptr, {}, 0, nullptr, &CodeGen::emitThreadSyncFree},
};

llvm::Value *CodeGen::emitBuiltinThread(const CallExpr &e) {
    return emitTableDrivenNativeCall(e, "thread", thread_table,
                                     std::size(thread_table));
}
