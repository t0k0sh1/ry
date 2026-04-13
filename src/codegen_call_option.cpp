#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"


namespace ry {

// ===== Builtin Option Methods (map) =====

llvm::Value *CodeGen::emitBuiltinOption(const CallExpr &e, llvm::Value *preEmittedArg0) {
    if (e.args.size() != 2) return nullptr;
    if (e.callee != "map") return nullptr;

    llvm::Value *optVal = preEmittedArg0 ? preEmittedArg0 : emitExpr(*e.args[0]);
    if (!isOptionType(optVal->getType()))
        return nullptr;

    llvm::Value *lambdaVal = emitExpr(*e.args[1]);
    auto *fnInfo = lookupFnTypeInfo(lambdaVal);
    if (!fnInfo)
        codegenError("map() on Option requires a function as second argument");
    const FnTypeInfo &info = *fnInfo;

    llvm::StructType *outOptTy = getOptionType(info.returnType);

    llvm::Value *hasVal = builder_.CreateExtractValue(optVal, 0, "has_val");
    llvm::Value *isNone = builder_.CreateNot(hasVal, "is_none");

    llvm::Value *someResult = nullptr;
    llvm::Value *mergedResult = emitResultBranch(isNone, outOptTy,
        [&]() {
            llvm::Value *innerVal = builder_.CreateExtractValue(optVal, 1, "some_val");
            someResult = buildSomeValue(emitLambdaCall(lambdaVal, info, {innerVal}, "mapped"), outOptTy);
            return someResult;
        },
        [&]() { return buildNoneValue(outOptTy); });

    if (someResult)
        propagateMeta(someResult, mergedResult);

    return mergedResult;
}

} // namespace ry
