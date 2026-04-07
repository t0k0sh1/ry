#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"
#include "ry/diagnostic.hpp"
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

static llvm::Value *emitThreadSpawn(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);

    llvm::Value *envPtr = nullptr;
    llvm::Function *thunk = nullptr;

    // Case 1: inline lambda expression
    auto *lambda = std::get_if<std::unique_ptr<LambdaExpr>>(&e.args[0]->data);
    if (lambda) {
        LambdaExpr &lam = **lambda;

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

        llvm::FunctionCallee mallocFn = cg.getStdlibMalloc();
        const llvm::DataLayout &dl = cg.mod_->getDataLayout();
        uint64_t envSize = std::max<uint64_t>(1, dl.getTypeAllocSize(envTy));
        envPtr = cg.builder_.CreateCall(
            mallocFn, {llvm::ConstantInt::get(cg.i64Ty_, envSize)}, "thread_env");

        if (captures.empty()) {
            llvm::Value *dummyField = cg.builder_.CreateStructGEP(envTy, envPtr, 0, "thread_env_dummy");
            cg.builder_.CreateStore(llvm::ConstantInt::get(cg.i8Ty_, 0), dummyField);
        } else {
            for (size_t i = 0; i < captures.size(); ++i) {
                llvm::Value *fieldPtr = cg.builder_.CreateStructGEP(envTy, envPtr, i, "thread_env_field");
                llvm::AllocaInst *src = captures[i].second;
                cg.builder_.CreateStore(
                    cg.builder_.CreateLoad(src->getAllocatedType(), src, captures[i].first + ".thr_cap"),
                    fieldPtr);
            }
        }

        llvm::FunctionType *thunkTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*cg.ctx_), {cg.ptrTy_}, false);
        thunk = llvm::Function::Create(
            thunkTy, llvm::Function::InternalLinkage,
            "__ry_thread." + std::to_string(cg.lambda_counter_++), *cg.mod_);

        {
            CodeGen::FnScope guard(cg);
            cg.fn_ = thunk;
            cg.pushScope();

            llvm::BasicBlock *entryBB = llvm::BasicBlock::Create(*cg.ctx_, "entry", thunk);
            cg.builder_.SetInsertPoint(entryBB);

            llvm::Value *envRaw = &*thunk->arg_begin();
            envRaw->setName("env_raw");
            if (!captures.empty()) {
                for (size_t i = 0; i < captures.size(); ++i) {
                    const auto &[name, src] = captures[i];
                    llvm::Type *capTy = src->getAllocatedType();
                    llvm::Value *fieldPtr = cg.builder_.CreateStructGEP(envTy, envRaw, i, name + ".field");
                    llvm::AllocaInst *dst = cg.builder_.CreateAlloca(capTy, nullptr, name);
                    cg.builder_.CreateStore(cg.builder_.CreateLoad(capTy, fieldPtr, name + ".cap"), dst);
                    cg.scope_stack_.back()[name] = dst;

                    cg.propagateMeta(src, dst);
                }
            }

            if (lam.expr_body) {
                cg.emitExpr(*lam.expr_body);
            } else {
                for (auto &stmt : lam.body)
                    std::visit([&cg](auto &st) { cg.emitStmt(st); }, stmt);
            }

            if (!cg.builder_.GetInsertBlock()->getTerminator())
                cg.builder_.CreateRetVoid();

            cg.popScope();
        }

    } else {
        // Case 2: variable reference (named function or variable holding fn() -> Unit)
        llvm::Value *fnVal = cg.emitExpr(*e.args[0]);
        llvm::FunctionCallee mallocFn = cg.getStdlibMalloc();
        const llvm::DataLayout &dl = cg.mod_->getDataLayout();

        auto *fnInfoPtr = cg.lookupFnTypeInfo(fnVal);
        bool hasCaps = fnInfoPtr && !fnInfoPtr->capturedVars.empty();

        llvm::FunctionType *thunkTy = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*cg.ctx_), {cg.ptrTy_}, false);
        thunk = llvm::Function::Create(
            thunkTy, llvm::Function::InternalLinkage,
            "__ry_thread_tramp." + std::to_string(cg.lambda_counter_++), *cg.mod_);

        if (hasCaps) {
            const CodeGen::FnTypeInfo info = *fnInfoPtr;

            std::vector<llvm::Type*> closureFields;
            closureFields.push_back(cg.ptrTy_);
            for (auto *ct : info.capturedTypes)
                closureFields.push_back(ct);
            llvm::StructType *closureTy = llvm::StructType::get(*cg.ctx_, closureFields);

            envPtr = cg.builder_.CreateCall(
                mallocFn,
                {llvm::ConstantInt::get(cg.i64Ty_, dl.getTypeAllocSize(closureTy))},
                "thread_env");

            llvm::Value *srcFnPtr = cg.builder_.CreateStructGEP(closureTy, fnVal, 0, "src.fn_ptr");
            llvm::Value *dstFnPtr = cg.builder_.CreateStructGEP(closureTy, envPtr, 0, "dst.fn_ptr");
            cg.builder_.CreateStore(cg.builder_.CreateLoad(cg.ptrTy_, srcFnPtr, "fn_ptr.val"), dstFnPtr);
            for (size_t i = 0; i < info.capturedTypes.size(); ++i) {
                llvm::Value *srcCap = cg.builder_.CreateStructGEP(closureTy, fnVal, i + 1, "src.cap." + std::to_string(i));
                llvm::Value *dstCap = cg.builder_.CreateStructGEP(closureTy, envPtr, i + 1, "dst.cap." + std::to_string(i));
                cg.builder_.CreateStore(
                    cg.builder_.CreateLoad(info.capturedTypes[i], srcCap, "cap.val." + std::to_string(i)),
                    dstCap);
            }

            {
                CodeGen::FnScope guard(cg);
                cg.fn_ = thunk;
                llvm::BasicBlock *entryBB = llvm::BasicBlock::Create(*cg.ctx_, "entry", thunk);
                cg.builder_.SetInsertPoint(entryBB);

                llvm::Value *envRaw = &*thunk->arg_begin();
                envRaw->setName("env_raw");

                llvm::Value *loadedFnPtr = cg.builder_.CreateLoad(
                    cg.ptrTy_,
                    cg.builder_.CreateStructGEP(closureTy, envRaw, 0, "tramp.fn_ptr"),
                    "tramp.fn");

                std::vector<llvm::Value*> callArgs;
                std::vector<llvm::Type*> allParamTypes;
                for (size_t i = 0; i < info.capturedTypes.size(); ++i) {
                    llvm::Value *capField = cg.builder_.CreateStructGEP(
                        closureTy, envRaw, i + 1, "tramp.cap." + std::to_string(i));
                    callArgs.push_back(cg.builder_.CreateLoad(
                        info.capturedTypes[i], capField, "tramp.cap_val." + std::to_string(i)));
                    allParamTypes.push_back(info.capturedTypes[i]);
                }

                llvm::FunctionType *callTy = llvm::FunctionType::get(
                    llvm::Type::getVoidTy(*cg.ctx_), allParamTypes, false);
                cg.builder_.CreateCall(callTy, loadedFnPtr, callArgs);
                cg.builder_.CreateRetVoid();
            }
        } else {
            llvm::Type *envFieldTypes[] = {cg.ptrTy_};
            llvm::StructType *envTy = llvm::StructType::get(*cg.ctx_, llvm::ArrayRef(envFieldTypes));
            envPtr = cg.builder_.CreateCall(
                mallocFn,
                {llvm::ConstantInt::get(cg.i64Ty_, dl.getTypeAllocSize(envTy))},
                "thread_env");
            llvm::Value *fnField = cg.builder_.CreateStructGEP(envTy, envPtr, 0, "thread_env_fn");
            cg.builder_.CreateStore(fnVal, fnField);

            {
                CodeGen::FnScope guard(cg);
                cg.fn_ = thunk;
                llvm::BasicBlock *entryBB = llvm::BasicBlock::Create(*cg.ctx_, "entry", thunk);
                cg.builder_.SetInsertPoint(entryBB);

                llvm::Value *envRaw = &*thunk->arg_begin();
                envRaw->setName("env_raw");

                llvm::Value *fnPtrField = cg.builder_.CreateStructGEP(
                    envTy, envRaw, 0, "tramp.fn_ptr");
                llvm::Value *loadedFn = cg.builder_.CreateLoad(cg.ptrTy_, fnPtrField, "tramp.fn");

                llvm::FunctionType *callTy = llvm::FunctionType::get(
                    llvm::Type::getVoidTy(*cg.ctx_), {}, false);
                cg.builder_.CreateCall(callTy, loadedFn);
                cg.builder_.CreateRetVoid();
            }
        }
    }

    // Call __ry_thread_spawn(thunk, env)
    auto spawnTy = llvm::FunctionType::get(cg.ptrTy_, {cg.ptrTy_, cg.ptrTy_}, false);
    auto spawnFn = cg.mod_->getOrInsertFunction("__ry_thread_spawn", spawnTy);
    llvm::Value *thread = cg.builder_.CreateCall(
        spawnFn, {thunk, envPtr}, "thread");
    cg.addResourceKind(thread, rk_thread);
    return thread;
}

static llvm::Value *emitThreadJoin(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *thread = cg.emitExpr(*e.args[0]);
    if (!cg.isThread(thread))
        cg.codegenError("thread_join() requires Thread argument");
    auto fn = cg.getRuntimeFn("__ry_thread_join", cg.i64Ty_, {cg.ptrTy_});
    llvm::Value *status = cg.builder_.CreateCall(fn, {thread}, "join_status");
    return cg.wrapStatusAsResult(status);
}

// lock_new, rwlock_new: 0-arg → ptr + resource tracking
static llvm::Value *emitThreadSyncNew(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 0);
    const char *rtName;
    int rk;
    if (e.callee == "lock_new") { rtName = "__ry_lock_new"; rk = rk_lock; }
    else { rtName = "__ry_rwlock_new"; rk = rk_rwlock; }
    auto fn = cg.getRuntimeFn(rtName, cg.ptrTy_, {});
    llvm::Value *result = cg.builder_.CreateCall(fn, {}, e.callee);
    cg.addResourceKind(result, rk);
    return result;
}

// semaphore_new, barrier_new: 1-arg → wrapPtrAsResult + resource tracking
static llvm::Value *emitThreadSyncResultNew(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *count = cg.emitExpr(*e.args[0]);
    const char *rtName;
    int rk;
    if (e.callee == "semaphore_new") { rtName = "__ry_semaphore_new"; rk = rk_semaphore; }
    else { rtName = "__ry_barrier_new"; rk = rk_barrier; }
    auto fn = cg.getRuntimeFn(rtName, cg.ptrTy_, {cg.i64Ty_});
    llvm::Value *ptr = cg.builder_.CreateCall(fn, {count}, e.callee);
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
    if (!(cg.*(it->second.check))(arg))
        cg.codegenError(e.callee + "() requires " + it->second.type + " argument");
    auto fn = cg.getRuntimeFn(it->second.rt, cg.i64Ty_, {cg.ptrTy_});
    llvm::Value *status = cg.builder_.CreateCall(fn, {arg}, e.callee + "_status");
    return cg.wrapStatusAsResult(status);
}

// All *_free operations: type-check + emitResourceFree
static llvm::Value *emitThreadSyncFree(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *arg = cg.emitExpr(*e.args[0]);

    struct FreeInfo { int rk; const char *type; };
    // rk_* are populated during static init, before any codegen runs.
    static const std::unordered_map<std::string, FreeInfo> frees = {
        {"lock_free",        {rk_lock,        "Lock"}},
        {"rwlock_free",      {rk_rwlock,      "RWLock"}},
        {"semaphore_free",   {rk_semaphore,   "Semaphore"}},
        {"barrier_free",     {rk_barrier,     "Barrier"}},
        {"atomic_int_free",  {rk_atomic_int,  "AtomicInt"}},
        {"atomic_bool_free", {rk_atomic_bool, "AtomicBool"}},
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
    auto fn = cg.getRuntimeFn("__ry_atomic_int_new", cg.ptrTy_, {cg.i64Ty_});
    llvm::Value *atom = cg.builder_.CreateCall(fn, {val}, "atomic_int");
    cg.addResourceKind(atom, rk_atomic_int);
    return atom;
}

// atomic_int_load, store, add, sub
static llvm::Value *emitThreadAtomicIntOp(CodeGen &cg, const CallExpr &e) {
    if (e.args.empty()) return nullptr;
    llvm::Value *atom = cg.emitExpr(*e.args[0]);
    if (!cg.isAtomicInt(atom))
        cg.codegenError(e.callee + "() requires AtomicInt as first argument");

    if (e.callee == "atomic_int_load") {
        cg.requireArgs(e, 1);
        auto fn = cg.getRuntimeFn("__ry_atomic_int_load", cg.i64Ty_, {cg.ptrTy_});
        return cg.builder_.CreateCall(fn, {atom}, "atomic_int_load");
    }
    if (e.callee == "atomic_int_store") {
        cg.requireArgs(e, 2);
        llvm::Value *val = cg.emitExpr(*e.args[1]);
        auto fn = cg.getRuntimeFn("__ry_atomic_int_store", llvm::Type::getVoidTy(*cg.ctx_), {cg.ptrTy_, cg.i64Ty_});
        return cg.builder_.CreateCall(fn, {atom, val});
    }
    // add, sub
    cg.requireArgs(e, 2);
    llvm::Value *delta = cg.emitExpr(*e.args[1]);
    std::string rtName = "__ry_" + e.callee;
    auto fn = cg.getRuntimeFn(rtName.c_str(), cg.i64Ty_, {cg.ptrTy_, cg.i64Ty_});
    return cg.builder_.CreateCall(fn, {atom, delta}, e.callee);
}

static llvm::Value *emitThreadAtomicIntCas(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 3);
    llvm::Value *atom = cg.emitExpr(*e.args[0]);
    if (!cg.isAtomicInt(atom))
        cg.codegenError("atomic_int_cas() requires AtomicInt as first argument");
    llvm::Value *expected = cg.emitExpr(*e.args[1]);
    llvm::Value *desired = cg.emitExpr(*e.args[2]);
    auto fn = cg.getRuntimeFn("__ry_atomic_int_cas", cg.i64Ty_, {cg.ptrTy_, cg.i64Ty_, cg.i64Ty_});
    llvm::Value *result = cg.builder_.CreateCall(fn, {atom, expected, desired}, "atomic_int_cas");
    return cg.builder_.CreateTrunc(result, cg.i1Ty_, "atomic_int_cas_bool");
}

static llvm::Value *emitThreadAtomicBoolNew(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *val = cg.emitExpr(*e.args[0]);
    if (val->getType() != cg.i1Ty_)
        cg.codegenError("atomic_bool_new() requires bool argument");
    llvm::Value *extended = cg.builder_.CreateZExt(val, cg.i64Ty_, "atomic_bool_ext");
    auto fn = cg.getRuntimeFn("__ry_atomic_bool_new", cg.ptrTy_, {cg.i64Ty_});
    llvm::Value *atom = cg.builder_.CreateCall(fn, {extended}, "atomic_bool");
    cg.addResourceKind(atom, rk_atomic_bool);
    return atom;
}

// atomic_bool_load, atomic_bool_store
static llvm::Value *emitThreadAtomicBoolOp(CodeGen &cg, const CallExpr &e) {
    if (e.args.empty()) return nullptr;
    llvm::Value *atom = cg.emitExpr(*e.args[0]);
    if (!cg.isAtomicBool(atom))
        cg.codegenError(e.callee + "() requires AtomicBool as first argument");

    if (e.callee == "atomic_bool_load") {
        cg.requireArgs(e, 1);
        auto fn = cg.getRuntimeFn("__ry_atomic_bool_load", cg.i64Ty_, {cg.ptrTy_});
        llvm::Value *result = cg.builder_.CreateCall(fn, {atom}, "atomic_bool_load");
        return cg.builder_.CreateTrunc(result, cg.i1Ty_, "atomic_bool_load_bool");
    }
    // atomic_bool_store
    cg.requireArgs(e, 2);
    llvm::Value *val = cg.emitExpr(*e.args[1]);
    if (val->getType() != cg.i1Ty_)
        cg.codegenError("atomic_bool_store() requires bool as second argument");
    llvm::Value *extended = cg.builder_.CreateZExt(val, cg.i64Ty_, "atomic_bool_store_ext");
    auto fn = cg.getRuntimeFn("__ry_atomic_bool_store", llvm::Type::getVoidTy(*cg.ctx_), {cg.ptrTy_, cg.i64Ty_});
    return cg.builder_.CreateCall(fn, {atom, extended});
}

// ===== Thread dispatch table =====

static const CodeGen::NativeDispatchEntry thread_table[] = {
    {"thread_spawn",      nullptr, {}, 0, nullptr, emitThreadSpawn},
    {"thread_join",       nullptr, {}, 0, nullptr, emitThreadJoin},
    // Sync primitives: new
    {"lock_new",          nullptr, {}, 0, nullptr, emitThreadSyncNew},
    {"rwlock_new",        nullptr, {}, 0, nullptr, emitThreadSyncNew},
    {"semaphore_new",     nullptr, {}, 0, nullptr, emitThreadSyncResultNew},
    {"barrier_new",       nullptr, {}, 0, nullptr, emitThreadSyncResultNew},
    // Sync primitives: operations
    {"lock_acquire",      nullptr, {}, 0, nullptr, emitThreadSyncOp},
    {"lock_release",      nullptr, {}, 0, nullptr, emitThreadSyncOp},
    {"rwlock_read_lock",  nullptr, {}, 0, nullptr, emitThreadSyncOp},
    {"rwlock_write_lock", nullptr, {}, 0, nullptr, emitThreadSyncOp},
    {"rwlock_unlock",     nullptr, {}, 0, nullptr, emitThreadSyncOp},
    {"semaphore_acquire", nullptr, {}, 0, nullptr, emitThreadSyncOp},
    {"semaphore_release", nullptr, {}, 0, nullptr, emitThreadSyncOp},
    {"barrier_wait",      nullptr, {}, 0, nullptr, emitThreadSyncOp},
    // Sync primitives: free
    {"lock_free",         nullptr, {}, 0, nullptr, emitThreadSyncFree},
    {"rwlock_free",       nullptr, {}, 0, nullptr, emitThreadSyncFree},
    {"semaphore_free",    nullptr, {}, 0, nullptr, emitThreadSyncFree},
    {"barrier_free",      nullptr, {}, 0, nullptr, emitThreadSyncFree},
    // AtomicInt
    {"atomic_int_new",    nullptr, {}, 0, nullptr, emitThreadAtomicIntNew},
    {"atomic_int_load",   nullptr, {}, 0, nullptr, emitThreadAtomicIntOp},
    {"atomic_int_store",  nullptr, {}, 0, nullptr, emitThreadAtomicIntOp},
    {"atomic_int_add",    nullptr, {}, 0, nullptr, emitThreadAtomicIntOp},
    {"atomic_int_sub",    nullptr, {}, 0, nullptr, emitThreadAtomicIntOp},
    {"atomic_int_cas",    nullptr, {}, 0, nullptr, emitThreadAtomicIntCas},
    {"atomic_int_free",   nullptr, {}, 0, nullptr, emitThreadSyncFree},
    // AtomicBool
    {"atomic_bool_new",   nullptr, {}, 0, nullptr, emitThreadAtomicBoolNew},
    {"atomic_bool_load",  nullptr, {}, 0, nullptr, emitThreadAtomicBoolOp},
    {"atomic_bool_store", nullptr, {}, 0, nullptr, emitThreadAtomicBoolOp},
    {"atomic_bool_free",  nullptr, {}, 0, nullptr, emitThreadSyncFree},
};

RY_REGISTER_STDLIB_PACKAGE(thread, "share/std/thread/thread.ry", dispatchThread)
static llvm::Value *dispatchThread(CodeGen &cg, const CallExpr &e) {
    return cg.emitTableDrivenNativeCall(e, "thread", thread_table, std::size(thread_table));
}

} // namespace ry
