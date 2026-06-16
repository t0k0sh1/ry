#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"
#include "ry/diagnostic/diagnostic.hpp"


namespace ry {

// ===== JSON custom emitters (#1698: any-based API; #1887: typed-only load) =====

// Helper for stringify / stringifySafe. Both accept an optional named arg
// `sortKeys: bool` (default false) that selects byte-lexicographic Map<str,
// any> key ordering — folded in from the former stringifySorted /
// stringifySortedSafe variants per #1890. `runtimeFn`: the unified C entry
// point (__ry_json_stringify_any_ex / __ry_json_stringify_any_safe_ex).
// `wrapResult`: whether to wrap the nullable-ptr return in Result<str, Error>
// via wrapPtrAsResult (Safe variants only).
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

    // Named argument: sortKeys (bool, default false).
    llvm::Value *sortKeysI8 = nullptr;
    for (const auto &na : e.named_args) {
        if (na.name != "sortKeys")
            cg.codegenError("unknown named argument '" + na.name +
                            "' for " + std::string(e.callee) + "()");
        llvm::Value *v = cg.emitExpr(*na.value);
        if (v->getType() != cg.i1Ty_)
            cg.codegenError(std::string(e.callee) +
                            "() 'sortKeys' must be bool");
        sortKeysI8 = cg.builder_.CreateZExt(v, cg.i8Ty_, "sort_keys_i8");
    }
    if (!sortKeysI8)
        sortKeysI8 = llvm::ConstantInt::get(cg.i8Ty_, 0);

    llvm::Type *paramTys[] = {cg.ptrTy_, cg.i64Ty_, cg.i8Ty_};
    auto *fnTy = llvm::FunctionType::get(cg.ptrTy_, paramTys, false);
    auto fn = cg.mod_->getOrInsertFunction(runtimeFn, fnTy);
    llvm::Value *ptr =
        cg.builder_.CreateCall(fn, {slot, indent, sortKeysI8}, callName);
    if (!wrapResult) return ptr;
    return cg.wrapPtrAsResult(ptr);
}

// stringify(value: any[, indent: int][, sortKeys=bool]) -> str
static llvm::Value *emitJsonStringify(CodeGen &cg, const CallExpr &e) {
    return emitJsonStringifyImpl(cg, e, "__ry_json_stringify_any_ex",
                                  "json_stringify_any_ex",
                                  /*wrapResult=*/false);
}

// stringifySafe(value: any[, indent: int][, sortKeys=bool]) -> Result<str, Error>
static llvm::Value *emitJsonStringifySafe(CodeGen &cg, const CallExpr &e) {
    return emitJsonStringifyImpl(cg, e, "__ry_json_stringify_any_safe_ex",
                                  "json_stringify_any_safe_ex",
                                  /*wrapResult=*/true);
}

// load[T](text: str) -> Result<T, Error>                  (#1852, #1887)
// load[T](f: File) -> Result<T, Error>
//
// Parses JSON via `__ry_json_parse_to_any` / `__ry_json_load_file`, then
// coerces the resulting `any` to the concrete target type via
// `tryUnwrapFromAny` — which returns `Err(Error)` on tag mismatch instead of
// panicking. The interceptor receives the type-arg as a literal string
// (e.g. "Person", "List<Person>") extracted from `e.callee` by `dispatchJson`.
//
// #1887: the pre-existing non-generic `load() -> Result<any, Error>` form was
// removed because it had no safe accessor. `T = any` is rejected by
// `tryUnwrapFromAny` (anyTy_ is a non-record StructType and falls through to
// the "non-record struct target not yet supported" branch); callers wanting
// the historical `Map`/`List<any>` shape should pick `load[Map<str, any>]` or
// `load[List<any>]` explicitly.
static llvm::Value *emitJsonLoad(CodeGen &cg, const CallExpr &e,
                                 const std::string &typeArg) {
    cg.requireArgs(e, 1);
    llvm::Value *arg0 = cg.emitExpr(*e.args[0]);

    const char *runtimeFn;
    if (cg.isFile(arg0)) {
        runtimeFn = "__ry_json_load_file";
    } else {
        if (!cg.isStringValue(arg0))
            cg.codegenError("load[T]() requires a str or File argument");
        runtimeFn = "__ry_json_parse_to_any";
    }

    llvm::Type *targetTy = cg.resolveType(typeArg);
    llvm::StructType *resTy = cg.getResultType(targetTy, cg.errorTy_);

    llvm::AllocaInst *outSlot =
        cg.builder_.CreateAlloca(cg.anyTy_, nullptr, "json_load_out");
    llvm::Type *paramTys[] = {cg.ptrTy_, cg.ptrTy_};
    auto *fnTy = llvm::FunctionType::get(cg.i64Ty_, paramTys, false);
    auto fn = cg.mod_->getOrInsertFunction(runtimeFn, fnTy);
    llvm::Value *status =
        cg.builder_.CreateCall(fn, {arg0, outSlot}, "json_load_status");
    llvm::Value *isParseErr = cg.builder_.CreateICmpNE(
        status, llvm::ConstantInt::get(cg.i64Ty_, 0), "json_load_parse_err");

    llvm::Value *result = cg.emitResultBranch(
        isParseErr, resTy,
        [&]() -> llvm::Value * {
            llvm::Value *anyVal = cg.builder_.CreateLoad(
                cg.anyTy_, outSlot, "json_load_any");
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

    auto fn = cg.getRuntimeFn("__ry_json_dump_file", cg.i64Ty_,
                              {cg.ptrTy_, cg.ptrTy_, cg.i64Ty_});
    llvm::Value *status = cg.builder_.CreateCall(
        fn, {fileHandle, slot, indent}, "json_dump_status");
    return cg.wrapStatusAsResult(status);
}


// ===== JSON dispatch table =====

static const CodeGen::NativeDispatchEntry json_table[] = {
    // arity field is metadata only when customEmitter is set — actual arity
    // dispatch happens via registered @native sigs at the custom-emitter gate
    // in emitTableDrivenNativeCall (see codegen_call_native.cpp).
    //
    // `load[T]` is intercepted by `dispatchJson` via suffix match against
    // `e.callee` (which is mangled to `load<T>` by the parser), so it has no
    // entry in this table.
    {"stringify",     nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, emitJsonStringify},
    {"stringifySafe", nullptr, CodeGen::ReturnWrapping::Direct, 1, nullptr, emitJsonStringifySafe},
    {"dump",          nullptr, CodeGen::ReturnWrapping::Direct, 2, nullptr, emitJsonDumpFile},
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

    // Intercept `load[T](...)` calls (#1852, #1887). The parser uses `[T]`
    // syntax for generic function calls at user-facing call sites, but
    // constructs the internal callee representation with `<T>` (e.g.
    // "load<Person>", "load<List<Person>>"), so we strip the `load<` prefix
    // and trailing `>` to recover the type-name. The interceptor runs before
    // `emitTableDrivenNativeCall` because the table's name-keyed exact match
    // does not handle the parametric suffix.
    constexpr const char kLoadPrefix[] = "load<";
    constexpr size_t kLoadPrefixLen = sizeof(kLoadPrefix) - 1;
    if (e.callee.size() > kLoadPrefixLen + 1 &&
        e.callee.compare(0, kLoadPrefixLen, kLoadPrefix) == 0 &&
        e.callee.back() == '>') {
        std::string typeArg = e.callee.substr(
            kLoadPrefixLen,
            e.callee.size() - kLoadPrefixLen - 1);
        return emitJsonLoad(cg, e, typeArg);
    }

    // Reject bare `load(...)` without a type argument (#1887). The pre-#1887
    // non-generic `load() -> Result<any, Error>` form was removed because it
    // had no safe accessor; a friendly diagnostic here beats the generic
    // "undefined function: load" that `emitTableDrivenNativeCall` would
    // otherwise emit.
    if (e.callee == "load") {
        cg.codegenError(
            "load() requires an explicit type argument: load[T](text|File). "
            "Pick a concrete T such as load[Map<str, any>], load[List<any>], "
            "or load[int]. load[any] is intentionally not supported (#1887).");
    }

    return cg.emitTableDrivenNativeCall(e, "json", json_table, std::size(json_table));
}

} // namespace ry
