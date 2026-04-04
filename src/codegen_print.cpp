#include "ry/codegen.hpp"

// ===== emitPrint (variadic) =====

void CodeGen::emitPrint(const std::vector<ExprPtr> &args) {
    builder_.CreateCall(getRuntimeFn("__ry_print_begin",
        llvm::Type::getVoidTy(*ctx_), {}));

    auto printfFn = getBufferedPrintf();
    llvm::Constant *fmtS = cachedGlobalString("%s", ".fmt_print_s");
    llvm::Constant *space = args.size() > 1
        ? cachedGlobalString(" ", ".fmt_space") : nullptr;

    for (size_t i = 0; i < args.size(); ++i) {
        if (i > 0)
            builder_.CreateCall(printfFn, {space});
        llvm::Value *str = valueToString(emitExpr(*args[i]));
        builder_.CreateCall(printfFn, {fmtS, str});
    }

    builder_.CreateCall(printfFn, {cachedGlobalString("\n", ".fmt_nl")});

    builder_.CreateCall(getRuntimeFn("__ry_print_end",
        llvm::Type::getVoidTy(*ctx_), {}));
}
