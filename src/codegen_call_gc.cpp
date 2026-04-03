#include "ry/codegen.hpp"

llvm::Value *CodeGen::emitBuiltinGc(const CallExpr &e) {
    if (!native_fn_arg_counts_.count(e.callee))
        return nullptr;

    // collect() -> int
    if (e.callee == "collect") {
        requireArgs(e, 0);
        auto *fnTy = llvm::FunctionType::get(i64Ty_, {}, false);
        auto fn = mod_->getOrInsertFunction("__ry_gc_collect", fnTy);
        return builder_.CreateCall(fn, {}, "gc_collected");
    }

    // enable() -> Unit
    if (e.callee == "enable") {
        requireArgs(e, 0);
        auto *fnTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {}, false);
        auto fn = mod_->getOrInsertFunction("__ry_gc_enable", fnTy);
        builder_.CreateCall(fn, {});
        return llvm::ConstantInt::get(i8Ty_, 0);  // Unit
    }

    // disable() -> Unit
    if (e.callee == "disable") {
        requireArgs(e, 0);
        auto *fnTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {}, false);
        auto fn = mod_->getOrInsertFunction("__ry_gc_disable", fnTy);
        builder_.CreateCall(fn, {});
        return llvm::ConstantInt::get(i8Ty_, 0);  // Unit
    }

    // set_threshold(n: int) -> Unit
    if (e.callee == "set_threshold") {
        requireArgs(e, 1);
        llvm::Value *n = emitExpr(*e.args[0]);
        if (n->getType() != i64Ty_)
            codegenError("set_threshold() requires int argument");
        auto *fnTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*ctx_), {i64Ty_}, false);
        auto fn = mod_->getOrInsertFunction("__ry_gc_set_threshold", fnTy);
        builder_.CreateCall(fn, {n});
        return llvm::ConstantInt::get(i8Ty_, 0);  // Unit
    }

    return nullptr;
}
