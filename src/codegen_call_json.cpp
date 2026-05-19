#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"
#include "ry/diagnostic.hpp"


namespace ry {

// ===== JSON custom emitters (#1698: any-based API) =====

// load(text: str) -> Result<any, Error>
static llvm::Value *emitJsonLoad(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *text = cg.emitExpr(*e.args[0]);
    if (text->getType() != cg.ptrTy_)
        cg.codegenError("load() requires a str argument");

    // Allocate a stack RyAny out-param and call __ry_json_parse_to_any(text, &out).
    llvm::AllocaInst *outSlot =
        cg.builder_.CreateAlloca(cg.anyTy_, nullptr, "json_load_out");
    llvm::Type *paramTys[] = {cg.ptrTy_, cg.ptrTy_};
    auto *fnTy = llvm::FunctionType::get(cg.i64Ty_, paramTys, false);
    auto fn = cg.mod_->getOrInsertFunction("__ry_json_parse_to_any", fnTy);
    llvm::Value *status =
        cg.builder_.CreateCall(fn, {text, outSlot}, "json_load_status");
    llvm::Value *isErr = cg.builder_.CreateICmpNE(
        status, llvm::ConstantInt::get(cg.i64Ty_, 0), "json_load_err");

    llvm::StructType *resTy = cg.getResultType(cg.anyTy_, cg.errorTy_);
    return cg.emitResultBranch(
        isErr, resTy,
        [&]() {
            llvm::Value *loaded =
                cg.builder_.CreateLoad(cg.anyTy_, outSlot, "json_load_val");
            return cg.buildOkValue(loaded, resTy);
        },
        [&]() { return cg.buildErrValue(cg.buildErrorFromRuntime(), resTy); });
}

// stringify(value: any) -> str
// stringify(value: any, indent: int) -> str
static llvm::Value *emitJsonStringify(CodeGen &cg, const CallExpr &e) {
    if (e.args.size() != 1 && e.args.size() != 2)
        cg.codegenError("stringify() takes 1 or 2 arguments");
    llvm::Value *val = cg.emitExpr(*e.args[0]);
    if (val->getType() != cg.anyTy_)
        cg.codegenError("stringify() requires an any argument");

    llvm::AllocaInst *slot =
        cg.builder_.CreateAlloca(cg.anyTy_, nullptr, "json_any_in");
    cg.builder_.CreateStore(val, slot);

    llvm::Value *indent;
    const char *callName;
    if (e.args.size() == 1) {
        indent = llvm::ConstantInt::getSigned(cg.i64Ty_, -1);
        callName = "json_stringify_any";
    } else {
        indent = cg.emitExpr(*e.args[1]);
        callName = "json_stringify_any_pretty";
    }
    llvm::Type *paramTys[] = {cg.ptrTy_, cg.i64Ty_};
    auto *fnTy = llvm::FunctionType::get(cg.ptrTy_, paramTys, false);
    auto fn = cg.mod_->getOrInsertFunction("__ry_json_stringify_any", fnTy);
    return cg.builder_.CreateCall(fn, {slot, indent}, callName);
}


// ===== JSON dispatch table =====

static const CodeGen::NativeDispatchEntry json_table[] = {
    {"load",      nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, emitJsonLoad},
    {"stringify", nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, emitJsonStringify},
};

RY_REGISTER_STDLIB_PACKAGE(json, "share/std/json/json.ry", dispatchJson)
static llvm::Value *dispatchJson(CodeGen &cg, const CallExpr &e) {
    return cg.emitTableDrivenNativeCall(e, "json", json_table, std::size(json_table));
}

} // namespace ry
