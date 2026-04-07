#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"


namespace ry {

// ===== Builtin Result Methods (and_then, map) =====

llvm::Value *CodeGen::emitBuiltinResult(const CallExpr &e, llvm::Value *preEmittedArg0) {
    if (e.args.size() != 2) return nullptr;
    bool isAndThen = (e.callee == "and_then");
    bool isMap     = (e.callee == "map");
    if (!isAndThen && !isMap) return nullptr;

    llvm::Value *resultVal = preEmittedArg0 ? preEmittedArg0 : emitExpr(*e.args[0]);
    if (!isResultType(resultVal->getType()))
        return nullptr;

    llvm::Value *lambdaVal = emitExpr(*e.args[1]);
    auto *fnInfo = lookupFnTypeInfo(lambdaVal);
    if (!fnInfo)
        codegenError(e.callee + "() on Result requires a function as second argument");
    auto info = *fnInfo;

    auto *srcResTy = llvm::cast<llvm::StructType>(resultVal->getType());
    llvm::StructType *outResTy;

    if (isAndThen) {
        if (!isResultType(info.returnType))
            codegenError("and_then() closure must return a Result type");
        outResTy = llvm::cast<llvm::StructType>(info.returnType);
        // Verify error type compatibility (source Err must match closure Result Err)
        llvm::Type *srcErrTy = srcResTy->getElementType(2);
        llvm::Type *outErrTy = outResTy->getElementType(2);
        if (srcErrTy != outErrTy)
            codegenError("and_then() error type mismatch: source and closure Result must have the same error type");
    } else {
        llvm::Type *errTy = srcResTy->getElementType(2);
        outResTy = getResultType(info.returnType, errTy);
    }

    llvm::Value *isOk = builder_.CreateExtractValue(resultVal, 0, "is_ok");
    llvm::Value *isErr = builder_.CreateNot(isOk, "is_err");

    llvm::Value *okIncoming = nullptr;
    llvm::Value *mergedResult = emitResultBranch(isErr, outResTy,
        [&]() {
            llvm::Value *okVal = builder_.CreateExtractValue(resultVal, 1, "ok_val");
            llvm::Value *result = emitLambdaCall(lambdaVal, info, {okVal},
                                                  isAndThen ? "and_then" : "mapped");
            okIncoming = isAndThen ? result : buildOkValue(result, outResTy);
            return okIncoming;
        },
        [&]() {
            llvm::Value *errVal = builder_.CreateExtractValue(resultVal, 2, "err_val");
            return buildErrValue(errVal, outResTy);
        });

    // Propagate collection/resource metadata through the branch PHI node
    if (okIncoming)
        propagateMeta(okIncoming, mergedResult);

    return mergedResult;
}

} // namespace ry
