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

// Helper for stringify / stringifySafe / stringifySorted / stringifySortedSafe.
// `runtimeFn`: the C entry point to call. `wrapResult`: whether to wrap the
// nullable-ptr return in Result<str, Error> via wrapPtrAsResult (Safe variants
// only). The non-Safe variants pass `wrapResult=false` and return the raw
// `char*` (ARC string), matching the existing `stringify` contract.
static llvm::Value *emitJsonStringifyImpl(CodeGen &cg, const CallExpr &e,
                                           const char *runtimeFn,
                                           const char *callName,
                                           bool wrapResult) {
    if (e.args.size() != 1 && e.args.size() != 2)
        cg.codegenError(std::string(e.callee) + "() takes 1 or 2 arguments");
    llvm::Value *val = cg.emitExpr(*e.args[0]);
    if (val->getType() != cg.anyTy_)
        val = cg.wrapInAny(val);

    llvm::AllocaInst *slot =
        cg.builder_.CreateAlloca(cg.anyTy_, nullptr, "json_any_in");
    cg.builder_.CreateStore(val, slot);

    llvm::Value *indent;
    if (e.args.size() == 1) {
        indent = llvm::ConstantInt::getSigned(cg.i64Ty_, -1);
    } else {
        indent = cg.emitExpr(*e.args[1]);
        if (indent->getType() != cg.i64Ty_)
            cg.codegenError(std::string(e.callee) + "() indent must be an int");
    }
    llvm::Type *paramTys[] = {cg.ptrTy_, cg.i64Ty_};
    auto *fnTy = llvm::FunctionType::get(cg.ptrTy_, paramTys, false);
    auto fn = cg.mod_->getOrInsertFunction(runtimeFn, fnTy);
    llvm::Value *ptr = cg.builder_.CreateCall(fn, {slot, indent}, callName);
    if (!wrapResult) return ptr;
    return cg.wrapPtrAsResult(ptr);
}

// stringify(value: any) -> str
// stringify(value: any, indent: int) -> str
static llvm::Value *emitJsonStringify(CodeGen &cg, const CallExpr &e) {
    return emitJsonStringifyImpl(cg, e, "__ry_json_stringify_any",
                                  "json_stringify_any", /*wrapResult=*/false);
}

// stringifySafe(value: any[, indent: int]) -> Result<str, Error>   (#1853)
static llvm::Value *emitJsonStringifySafe(CodeGen &cg, const CallExpr &e) {
    return emitJsonStringifyImpl(cg, e, "__ry_json_stringify_any_safe",
                                  "json_stringify_any_safe", /*wrapResult=*/true);
}

// stringifySorted(value: any[, indent: int]) -> str                 (#1853)
static llvm::Value *emitJsonStringifySorted(CodeGen &cg, const CallExpr &e) {
    return emitJsonStringifyImpl(cg, e, "__ry_json_stringify_any_sorted",
                                  "json_stringify_any_sorted",
                                  /*wrapResult=*/false);
}

// stringifySortedSafe(value: any[, indent: int]) -> Result<str, Error> (#1853)
static llvm::Value *emitJsonStringifySortedSafe(CodeGen &cg, const CallExpr &e) {
    return emitJsonStringifyImpl(cg, e, "__ry_json_stringify_any_sorted_safe",
                                  "json_stringify_any_sorted_safe",
                                  /*wrapResult=*/true);
}

// loadAs<T>(text: str) -> Result<T, Error>                   (#1852)
// loadAs<T>(f: File) -> Result<T, Error>
//
// Parses JSON via the same runtime entry as `load`, then coerces the resulting
// `any` to the concrete target type via `tryUnwrapFromAny` — which returns
// `Err(Error)` on tag mismatch instead of panicking. The interceptor receives
// the type-arg as a literal string (e.g. "Person", "List<Person>") extracted
// from `e.callee` by `dispatchJson`.
static llvm::Value *emitJsonLoadAs(CodeGen &cg, const CallExpr &e,
                                    const std::string &typeArg) {
    cg.requireArgs(e, 1);
    llvm::Value *arg0 = cg.emitExpr(*e.args[0]);

    const char *runtimeFn;
    if (cg.isFile(arg0)) {
        runtimeFn = "__ry_json_load_file";
    } else {
        if (arg0->getType() != cg.ptrTy_)
            cg.codegenError("loadAs() requires a str or File argument");
        runtimeFn = "__ry_json_parse_to_any";
    }

    llvm::Type *targetTy = cg.resolveType(typeArg);
    llvm::StructType *resTy = cg.getResultType(targetTy, cg.errorTy_);

    llvm::AllocaInst *outSlot =
        cg.builder_.CreateAlloca(cg.anyTy_, nullptr, "json_loadas_out");
    llvm::Type *paramTys[] = {cg.ptrTy_, cg.ptrTy_};
    auto *fnTy = llvm::FunctionType::get(cg.i64Ty_, paramTys, false);
    auto fn = cg.mod_->getOrInsertFunction(runtimeFn, fnTy);
    llvm::Value *status =
        cg.builder_.CreateCall(fn, {arg0, outSlot}, "json_loadas_status");
    llvm::Value *isParseErr = cg.builder_.CreateICmpNE(
        status, llvm::ConstantInt::get(cg.i64Ty_, 0), "json_loadas_parse_err");

    llvm::Value *result = cg.emitResultBranch(
        isParseErr, resTy,
        [&]() -> llvm::Value * {
            llvm::Value *anyVal = cg.builder_.CreateLoad(
                cg.anyTy_, outSlot, "json_loadas_any");
            return cg.tryUnwrapFromAny(anyVal, targetTy, typeArg);
        },
        [&]() -> llvm::Value * {
            return cg.buildErrValue(cg.buildErrorFromRuntime(), resTy);
        });
    // Stamp the lossless source-level Result type onto the outer PHI so the
    // `case` subject alloca propagates it to OkPattern via
    // `getMeta(subjectAlloca)->source_type_name`. Without this, the
    // OkPattern arm falls back to lossy `reverseResolveTypeName(ptrTy_) = "str"`
    // and `xs[0]` is dispatched against `isStringValue(xs) == true`.
    cg.propagateTypeMeta("Result<" + typeArg + ", Error>", result);
    return result;
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
    {"load",                nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, emitJsonLoad},
    {"stringify",           nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, emitJsonStringify},
    {"stringifySafe",       nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, emitJsonStringifySafe},
    {"stringifySorted",     nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, emitJsonStringifySorted},
    {"stringifySortedSafe", nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, emitJsonStringifySortedSafe},
    {"dump",                nullptr, CodeGen::ReturnWrapping::Direct, 2, nullptr, emitJsonDumpFile},
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

    // Intercept `loadAs<T>(...)` calls (#1852). The parser concatenates the
    // type-arg literally into `e.callee` (e.g. "loadAs<Person>",
    // "loadAs<List<Person>>"), so we strip the `loadAs<` prefix and trailing
    // `>` to recover the type-name. The interceptor runs before
    // `emitTableDrivenNativeCall` because the table's name-keyed exact match
    // does not handle the parametric suffix.
    constexpr const char kLoadAsPrefix[] = "loadAs<";
    constexpr size_t kLoadAsPrefixLen = sizeof(kLoadAsPrefix) - 1;
    if (e.callee.size() > kLoadAsPrefixLen + 1 &&
        e.callee.compare(0, kLoadAsPrefixLen, kLoadAsPrefix) == 0 &&
        e.callee.back() == '>') {
        std::string typeArg = e.callee.substr(
            kLoadAsPrefixLen,
            e.callee.size() - kLoadAsPrefixLen - 1);
        return emitJsonLoadAs(cg, e, typeArg);
    }

    return cg.emitTableDrivenNativeCall(e, "json", json_table, std::size(json_table));
}

} // namespace ry
