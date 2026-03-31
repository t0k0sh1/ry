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

// ===== Builtin Thread =====

llvm::Value *CodeGen::emitBuiltinThread(const CallExpr &e) {
    if (!native_fn_arg_counts_.count(e.callee))
        return nullptr;

    // ----- thread_spawn(body: fn() -> Unit) -> Thread -----
    if (e.callee == "thread_spawn") {
        requireArgs(e, 1);

        llvm::Value *envPtr = nullptr;
        llvm::Function *thunk = nullptr;

        // Case 1: inline lambda expression
        auto *lambda = std::get_if<std::unique_ptr<LambdaExpr>>(&e.args[0]->data);
        if (lambda) {
            LambdaExpr &lam = **lambda;

            // Only capture variables actually referenced in the lambda body
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

            // fn_type_info_ may be keyed on the alloca, not the loaded value
            auto fnInfoIt = fn_type_info_.find(fnVal);
            if (fnInfoIt == fn_type_info_.end()) {
                if (auto *load = llvm::dyn_cast<llvm::LoadInst>(fnVal))
                    fnInfoIt = fn_type_info_.find(load->getPointerOperand());
            }
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

    // ----- thread_join(thread: Thread) -> Result<Unit, Error> -----
    if (e.callee == "thread_join") {
        requireArgs(e, 1);
        llvm::Value *thread = emitExpr(*e.args[0]);
        if (!isThread(thread))
            codegenError("thread_join() requires Thread argument");
        auto fn = getRuntimeFn("__ry_thread_join", i64Ty_, {ptrTy_});
        llvm::Value *status = builder_.CreateCall(fn, {thread}, "join_status");
        // Don't nullify: __ry_thread_join only joins (no free). ARC cleanup at
        // scope exit will call __ry_thread_cleanup which sees joinable()==false
        // and just destructs the handle, then ARC frees the block.
        return wrapStatusAsResult(status);
    }

    // ----- Lock -----
    if (e.callee == "lock_new") {
        requireArgs(e, 0);
        auto fn = getRuntimeFn("__ry_lock_new", ptrTy_, {});
        llvm::Value *lock = builder_.CreateCall(fn, {}, "lock");
        resource_sets_[RK_Lock].insert(lock);
        return lock;
    }
    if (e.callee == "lock_acquire") {
        requireArgs(e, 1);
        llvm::Value *lock = emitExpr(*e.args[0]);
        if (!isLock(lock))
            codegenError("lock_acquire() requires Lock argument");
        auto fn = getRuntimeFn("__ry_lock_acquire", i64Ty_, {ptrTy_});
        llvm::Value *status = builder_.CreateCall(fn, {lock}, "lock_acquire_status");
        return wrapStatusAsResult(status);
    }
    if (e.callee == "lock_release") {
        requireArgs(e, 1);
        llvm::Value *lock = emitExpr(*e.args[0]);
        if (!isLock(lock))
            codegenError("lock_release() requires Lock argument");
        auto fn = getRuntimeFn("__ry_lock_release", i64Ty_, {ptrTy_});
        llvm::Value *status = builder_.CreateCall(fn, {lock}, "lock_release_status");
        return wrapStatusAsResult(status);
    }
    if (e.callee == "lock_free") {
        requireArgs(e, 1);
        llvm::Value *lock = emitExpr(*e.args[0]);
        if (!isLock(lock))
            codegenError("lock_free() requires Lock argument");
        return emitResourceFree(lock, RK_Lock, *e.args[0]);
    }

    // ----- RWLock -----
    if (e.callee == "rwlock_new") {
        requireArgs(e, 0);
        auto fn = getRuntimeFn("__ry_rwlock_new", ptrTy_, {});
        llvm::Value *rwlock = builder_.CreateCall(fn, {}, "rwlock");
        resource_sets_[RK_RWLock].insert(rwlock);
        return rwlock;
    }
    if (e.callee == "rwlock_read_lock") {
        requireArgs(e, 1);
        llvm::Value *rwlock = emitExpr(*e.args[0]);
        if (!isRWLock(rwlock))
            codegenError("rwlock_read_lock() requires RWLock argument");
        auto fn = getRuntimeFn("__ry_rwlock_read_lock", i64Ty_, {ptrTy_});
        llvm::Value *status = builder_.CreateCall(fn, {rwlock}, "rwlock_read_lock_status");
        return wrapStatusAsResult(status);
    }
    if (e.callee == "rwlock_write_lock") {
        requireArgs(e, 1);
        llvm::Value *rwlock = emitExpr(*e.args[0]);
        if (!isRWLock(rwlock))
            codegenError("rwlock_write_lock() requires RWLock argument");
        auto fn = getRuntimeFn("__ry_rwlock_write_lock", i64Ty_, {ptrTy_});
        llvm::Value *status = builder_.CreateCall(fn, {rwlock}, "rwlock_write_lock_status");
        return wrapStatusAsResult(status);
    }
    if (e.callee == "rwlock_unlock") {
        requireArgs(e, 1);
        llvm::Value *rwlock = emitExpr(*e.args[0]);
        if (!isRWLock(rwlock))
            codegenError("rwlock_unlock() requires RWLock argument");
        auto fn = getRuntimeFn("__ry_rwlock_unlock", i64Ty_, {ptrTy_});
        llvm::Value *status = builder_.CreateCall(fn, {rwlock}, "rwlock_unlock_status");
        return wrapStatusAsResult(status);
    }
    if (e.callee == "rwlock_free") {
        requireArgs(e, 1);
        llvm::Value *rwlock = emitExpr(*e.args[0]);
        if (!isRWLock(rwlock))
            codegenError("rwlock_free() requires RWLock argument");
        return emitResourceFree(rwlock, RK_RWLock, *e.args[0]);
    }

    // ----- Semaphore -----
    if (e.callee == "semaphore_new") {
        requireArgs(e, 1);
        llvm::Value *count = emitExpr(*e.args[0]);
        auto fn = getRuntimeFn("__ry_semaphore_new", ptrTy_, {i64Ty_});
        llvm::Value *sem = builder_.CreateCall(fn, {count}, "semaphore");
        llvm::Value *result = wrapPtrAsResult(sem);
        resource_sets_[RK_Semaphore].insert(result);
        return result;
    }
    if (e.callee == "semaphore_acquire") {
        requireArgs(e, 1);
        llvm::Value *sem = emitExpr(*e.args[0]);
        if (!isSemaphore(sem))
            codegenError("semaphore_acquire() requires Semaphore argument");
        auto fn = getRuntimeFn("__ry_semaphore_acquire", i64Ty_, {ptrTy_});
        llvm::Value *status = builder_.CreateCall(fn, {sem}, "sem_acquire_status");
        return wrapStatusAsResult(status);
    }
    if (e.callee == "semaphore_release") {
        requireArgs(e, 1);
        llvm::Value *sem = emitExpr(*e.args[0]);
        if (!isSemaphore(sem))
            codegenError("semaphore_release() requires Semaphore argument");
        auto fn = getRuntimeFn("__ry_semaphore_release", i64Ty_, {ptrTy_});
        llvm::Value *status = builder_.CreateCall(fn, {sem}, "sem_release_status");
        return wrapStatusAsResult(status);
    }
    if (e.callee == "semaphore_free") {
        requireArgs(e, 1);
        llvm::Value *sem = emitExpr(*e.args[0]);
        if (!isSemaphore(sem))
            codegenError("semaphore_free() requires Semaphore argument");
        return emitResourceFree(sem, RK_Semaphore, *e.args[0]);
    }

    // ----- Barrier -----
    if (e.callee == "barrier_new") {
        requireArgs(e, 1);
        llvm::Value *count = emitExpr(*e.args[0]);
        auto fn = getRuntimeFn("__ry_barrier_new", ptrTy_, {i64Ty_});
        llvm::Value *barrier = builder_.CreateCall(fn, {count}, "barrier");
        llvm::Value *result = wrapPtrAsResult(barrier);
        resource_sets_[RK_Barrier].insert(result);
        return result;
    }
    if (e.callee == "barrier_wait") {
        requireArgs(e, 1);
        llvm::Value *barrier = emitExpr(*e.args[0]);
        if (!isBarrier(barrier))
            codegenError("barrier_wait() requires Barrier argument");
        auto fn = getRuntimeFn("__ry_barrier_wait", i64Ty_, {ptrTy_});
        llvm::Value *status = builder_.CreateCall(fn, {barrier}, "barrier_wait_status");
        return wrapStatusAsResult(status);
    }
    if (e.callee == "barrier_free") {
        requireArgs(e, 1);
        llvm::Value *barrier = emitExpr(*e.args[0]);
        if (!isBarrier(barrier))
            codegenError("barrier_free() requires Barrier argument");
        return emitResourceFree(barrier, RK_Barrier, *e.args[0]);
    }

    // ----- AtomicInt -----
    if (e.callee == "atomic_int_new") {
        requireArgs(e, 1);
        llvm::Value *val = emitExpr(*e.args[0]);
        auto fn = getRuntimeFn("__ry_atomic_int_new", ptrTy_, {i64Ty_});
        llvm::Value *atom = builder_.CreateCall(fn, {val}, "atomic_int");
        resource_sets_[RK_AtomicInt].insert(atom);
        return atom;
    }
    if (e.callee == "atomic_int_load") {
        requireArgs(e, 1);
        llvm::Value *atom = emitExpr(*e.args[0]);
        if (!isAtomicInt(atom))
            codegenError("atomic_int_load() requires AtomicInt argument");
        auto fn = getRuntimeFn("__ry_atomic_int_load", i64Ty_, {ptrTy_});
        return builder_.CreateCall(fn, {atom}, "atomic_int_load");
    }
    if (e.callee == "atomic_int_store") {
        requireArgs(e, 2);
        llvm::Value *atom = emitExpr(*e.args[0]);
        if (!isAtomicInt(atom))
            codegenError("atomic_int_store() requires AtomicInt as first argument");
        llvm::Value *val = emitExpr(*e.args[1]);
        auto *voidTy = llvm::Type::getVoidTy(*ctx_);
        auto fn = getRuntimeFn("__ry_atomic_int_store", voidTy, {ptrTy_, i64Ty_});
        return builder_.CreateCall(fn, {atom, val});
    }
    if (e.callee == "atomic_int_add") {
        requireArgs(e, 2);
        llvm::Value *atom = emitExpr(*e.args[0]);
        if (!isAtomicInt(atom))
            codegenError("atomic_int_add() requires AtomicInt as first argument");
        llvm::Value *delta = emitExpr(*e.args[1]);
        auto fn = getRuntimeFn("__ry_atomic_int_add", i64Ty_, {ptrTy_, i64Ty_});
        return builder_.CreateCall(fn, {atom, delta}, "atomic_int_add");
    }
    if (e.callee == "atomic_int_sub") {
        requireArgs(e, 2);
        llvm::Value *atom = emitExpr(*e.args[0]);
        if (!isAtomicInt(atom))
            codegenError("atomic_int_sub() requires AtomicInt as first argument");
        llvm::Value *delta = emitExpr(*e.args[1]);
        auto fn = getRuntimeFn("__ry_atomic_int_sub", i64Ty_, {ptrTy_, i64Ty_});
        return builder_.CreateCall(fn, {atom, delta}, "atomic_int_sub");
    }
    if (e.callee == "atomic_int_cas") {
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
    if (e.callee == "atomic_int_free") {
        requireArgs(e, 1);
        llvm::Value *atom = emitExpr(*e.args[0]);
        if (!isAtomicInt(atom))
            codegenError("atomic_int_free() requires AtomicInt argument");
        return emitResourceFree(atom, RK_AtomicInt, *e.args[0]);
    }

    // ----- AtomicBool -----
    if (e.callee == "atomic_bool_new") {
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
    if (e.callee == "atomic_bool_load") {
        requireArgs(e, 1);
        llvm::Value *atom = emitExpr(*e.args[0]);
        if (!isAtomicBool(atom))
            codegenError("atomic_bool_load() requires AtomicBool argument");
        auto fn = getRuntimeFn("__ry_atomic_bool_load", i64Ty_, {ptrTy_});
        llvm::Value *result = builder_.CreateCall(fn, {atom}, "atomic_bool_load");
        return builder_.CreateTrunc(result, i1Ty_, "atomic_bool_load_bool");
    }
    if (e.callee == "atomic_bool_store") {
        requireArgs(e, 2);
        llvm::Value *atom = emitExpr(*e.args[0]);
        if (!isAtomicBool(atom))
            codegenError("atomic_bool_store() requires AtomicBool as first argument");
        llvm::Value *val = emitExpr(*e.args[1]);
        if (val->getType() != i1Ty_)
            codegenError("atomic_bool_store() requires bool as second argument");
        llvm::Value *extended = builder_.CreateZExt(val, i64Ty_, "atomic_bool_store_ext");
        auto *voidTy = llvm::Type::getVoidTy(*ctx_);
        auto fn = getRuntimeFn("__ry_atomic_bool_store", voidTy, {ptrTy_, i64Ty_});
        return builder_.CreateCall(fn, {atom, extended});
    }
    if (e.callee == "atomic_bool_free") {
        requireArgs(e, 1);
        llvm::Value *atom = emitExpr(*e.args[0]);
        if (!isAtomicBool(atom))
            codegenError("atomic_bool_free() requires AtomicBool argument");
        return emitResourceFree(atom, RK_AtomicBool, *e.args[0]);
    }

    return nullptr;
}
