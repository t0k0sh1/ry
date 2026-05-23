#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"
#include "ry/diagnostic.hpp"


namespace ry {

// ===== JSON custom emitters (#1698: any-based API) =====

// load(text: str) -> Result<any, Error>
// load(f: File) -> Result<any, Error>      (#1854)
//
// Both runtime entry points share the same calling convention: int64_t status
// (0 ok / non-zero err) with an out-param RyAny slot.  Selecting between
// `__ry_json_parse_to_any` and `__ry_json_load_file` based on the arg type
// means the resulting IR is identical for str and File overloads — only the
// runtime symbol name differs.
static llvm::Value *emitJsonLoad(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *arg0 = cg.emitExpr(*e.args[0]);

    const char *runtimeFn;
    if (cg.isFile(arg0)) {
        runtimeFn = "__ry_json_load_file";
    } else {
        if (arg0->getType() != cg.ptrTy_)
            cg.codegenError("load() requires a str or File argument");
        runtimeFn = "__ry_json_parse_to_any";
    }

    // Allocate a stack RyAny out-param and call <runtimeFn>(arg, &out).
    llvm::AllocaInst *outSlot =
        cg.builder_.CreateAlloca(cg.anyTy_, nullptr, "json_load_out");
    llvm::Type *paramTys[] = {cg.ptrTy_, cg.ptrTy_};
    auto *fnTy = llvm::FunctionType::get(cg.i64Ty_, paramTys, false);
    auto fn = cg.mod_->getOrInsertFunction(runtimeFn, fnTy);
    llvm::Value *status =
        cg.builder_.CreateCall(fn, {arg0, outSlot}, "json_load_status");
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
        val = cg.wrapInAny(val);

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
        if (indent->getType() != cg.i64Ty_)
            cg.codegenError("stringify() indent must be an int");
        callName = "json_stringify_any_pretty";
    }
    llvm::Type *paramTys[] = {cg.ptrTy_, cg.i64Ty_};
    auto *fnTy = llvm::FunctionType::get(cg.ptrTy_, paramTys, false);
    auto fn = cg.mod_->getOrInsertFunction("__ry_json_stringify_any", fnTy);
    return cg.builder_.CreateCall(fn, {slot, indent}, callName);
}

// dump(f: File, value: any) -> Result<Unit, Error>           (#1854)
// dump(f: File, value: any, indent: int) -> Result<Unit, Error>
//
// Routes through __ry_json_dump_file in runtime_json.cpp, which serializes the
// value via __ry_json_stringify_any and writes the buffer to the file via
// __ry_io_file_write_text.  ARC release of the intermediate Ry string lives in
// the C runtime so the codegen IR stays a single status-returning call.
static llvm::Value *emitJsonDumpFile(CodeGen &cg, const CallExpr &e) {
    if (e.args.size() != 2 && e.args.size() != 3)
        cg.codegenError("dump() takes 2 or 3 arguments");

    llvm::Value *fileHandle = cg.emitExpr(*e.args[0]);
    if (!cg.isFile(fileHandle))
        cg.codegenError("dump(f, value): first argument must be a File handle");

    llvm::Value *val = cg.emitExpr(*e.args[1]);
    if (val->getType() != cg.anyTy_)
        val = cg.wrapInAny(val);

    llvm::AllocaInst *slot =
        cg.builder_.CreateAlloca(cg.anyTy_, nullptr, "json_dump_in");
    cg.builder_.CreateStore(val, slot);

    llvm::Value *indent;
    if (e.args.size() == 2) {
        indent = llvm::ConstantInt::getSigned(cg.i64Ty_, -1);
    } else {
        indent = cg.emitExpr(*e.args[2]);
        if (indent->getType() != cg.i64Ty_)
            cg.codegenError("dump() indent must be an int");
    }

    llvm::Type *paramTys[] = {cg.ptrTy_, cg.ptrTy_, cg.i64Ty_};
    auto *fnTy = llvm::FunctionType::get(cg.i64Ty_, paramTys, false);
    auto fn = cg.mod_->getOrInsertFunction("__ry_json_dump_file", fnTy);
    llvm::Value *status = cg.builder_.CreateCall(
        fn, {fileHandle, slot, indent}, "json_dump_status");
    return cg.wrapStatusAsResult(status);
}


// ===== JSON dispatch table =====

static const CodeGen::NativeDispatchEntry json_table[] = {
    // arity field is metadata only when customEmitter is set — actual arity
    // dispatch happens via registered @native sigs at the custom-emitter gate
    // in emitTableDrivenNativeCall (see codegen_call_native.cpp).
    {"load",      nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, emitJsonLoad},
    {"stringify", nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, emitJsonStringify},
    {"dump",      nullptr, CodeGen::ReturnWrapping::Direct, 2, nullptr, emitJsonDumpFile},
};

RY_REGISTER_STDLIB_PACKAGE(json, "share/std/json/json.ry", dispatchJson)
static llvm::Value *dispatchJson(CodeGen &cg, const CallExpr &e) {
    // Register libry_json.dylib for JIT loading.  The custom emitters reached
    // through this dispatcher bypass the sig.library-driven insert that
    // happens inside emitTableDrivenNativeCall's customEmitter branch only
    // after arg type / arity checks succeed — registering here ensures the
    // library is loaded before any early codegenError path can fire (per
    // codegen-stdlib-dispatcher.md #1856).
    cg.used_native_libraries_.insert("json");

    return cg.emitTableDrivenNativeCall(e, "json", json_table, std::size(json_table));
}

} // namespace ry
