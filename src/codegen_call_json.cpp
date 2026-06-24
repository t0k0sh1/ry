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

    llvm::Value *slot = cg.emitAlloca(cg.anyTy_, "json_any_in");
    cg.emitStore(val, slot);

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

    auto fn = cg.getRuntimeFn(runtimeFn, cg.ptrTy_, {cg.ptrTy_, cg.i64Ty_, cg.i8Ty_});
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
        if (!cg.isStrLike(arg0))
            cg.codegenError("load[T]() requires a str or File argument");
        runtimeFn = "__ry_json_parse_to_any";
    }

    llvm::Type *targetTy = cg.resolveType(typeArg);
    llvm::StructType *resTy = cg.getResultType(targetTy, cg.errorTy_);

    llvm::Value *outSlot = cg.emitAlloca(cg.anyTy_, "json_load_out");
    auto fn = cg.getRuntimeFn(runtimeFn, cg.i64Ty_, {cg.ptrTy_, cg.ptrTy_});
    llvm::Value *status =
        cg.builder_.CreateCall(fn, {arg0, outSlot}, "json_load_status");
    llvm::Value *isParseErr = cg.emitICmpNE(
        status, llvm::ConstantInt::get(cg.i64Ty_, 0), "json_load_parse_err");

    llvm::Value *result = cg.emitResultBranch(
        isParseErr, resTy,
        [&]() -> llvm::Value * {
            llvm::Value *anyVal = cg.emitLoad(cg.anyTy_, outSlot, "json_load_any");
            return cg.tryUnwrapFromAny(anyVal, targetTy, typeArg);
        },
        [&]() -> llvm::Value * {
            return cg.buildErrValue(cg.buildErrorFromRuntime(), resTy);
        });
    // Stamp the lossless source-level Result type onto the outer PHI so the
    // `case` subject alloca propagates it to OkPattern via
    // `getMeta(subjectAlloca)->source_type_name`. Without this, the
    // OkPattern arm falls back to lossy `reverseResolveTypeName(ptrTy_) = "str"`
    // and `xs[0]` is dispatched against `isStrLike(xs) == true`.
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

    llvm::Value *slot = cg.emitAlloca(cg.anyTy_, "json_dump_in");
    cg.emitStore(val, slot);

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
//
// `stringify` / `stringifySafe` were carved out to emitBuiltinJson (Pattern B)
// per #2340 — they are polymorphic over the Ry value type and never matched a
// declarative descriptor. `load[T]` is intercepted by `dispatchJson` via
// suffix match against `e.callee`; `dump` stays here because it remains a
// declaration-driven 2/3-arity File-first call.
static const CodeGen::NativeDispatchEntry json_table[] = {
    {"dump", nullptr, CodeGen::ReturnWrapping::Direct, 2, nullptr, emitJsonDumpFile},
};

// Gate the dispatcher: only proceed if any json symbol is actually
// registered in `native_fn_sigs_`. Without this, json would race with
// the json5 dispatcher (#1855) over the `load<T>` interceptor — both
// claim every `load<T>` call regardless of which package the user imported
// from, and alphabetical ordering would put json first, routing
// `from json5 import load; load[T](...)` to `__ry_json_parse_to_any`.
// The check mirrors the package-level gate `emitTableDrivenNativeCall`
// performs at L17.
static bool isJsonImported(CodeGen &cg) {
    const auto &sigs = cg.getNativeFnSigs();
    return sigs.count("json::load") || sigs.count("json::stringify")
        || sigs.count("json::stringifySafe") || sigs.count("json::dump");
}

RY_REGISTER_STDLIB_PACKAGE(json, "share/std/json/json.ry", dispatchJson)
static llvm::Value *dispatchJson(CodeGen &cg, const CallExpr &e) {
    if (!isJsonImported(cg))
        return nullptr;

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

// ===== Builtin Json / Json5 shared body (Pattern B carve-out, #2340) =====
//
// stringify / stringifySafe are polymorphic over the Ry value type and never
// fit the declarative descriptor shape, so they live here as compiler builtins.
// json and json5 share the surface verbatim — same callee names, same sig-gate
// pattern, only the package and runtime entry-point symbols differ. The chain
// order in codegen_call_dispatch.cpp puts json before json5, matching the
// pre-carve precedence (alphabetical package ordering put `dispatchJson` ahead
// of `dispatchJson5`).
//
// Sig gate: intercept only when `<package>::<name>` is registered. Without the
// gate every bare `stringify` call in a program that did not import json /
// json5 would short-circuit user fns; the reserved-name guard in
// builtin_names.hpp blocks user *definitions*, but a same-named identifier
// bound to a variable / lambda must still reach the indirect-call path in
// emitExprVariant.
//
// Library registration: the carved emitters register `<package>` directly so
// the JIT loads `libry_<package>` before the runtime symbol is referenced.
// dispatchJson / dispatchJson5 still cover the load[T] / dump paths.
llvm::Value *CodeGen::emitBuiltinJsonModuleStringify(
    const CallExpr &e,
    const char *package,
    CodeGenCustomEmitterFn stringifyEmitter,
    CodeGenCustomEmitterFn stringifySafeEmitter) {
    const std::string pkgPrefix = std::string(package) + "::";
    if (e.callee == "stringify" &&
        native_fn_sigs_.count(pkgPrefix + "stringify")) {
        used_native_libraries_.insert(package);
        return emitBuiltinNativeOrMock(e, package, stringifyEmitter);
    }
    if (e.callee == "stringifySafe" &&
        native_fn_sigs_.count(pkgPrefix + "stringifySafe")) {
        used_native_libraries_.insert(package);
        return emitBuiltinNativeOrMock(e, package, stringifySafeEmitter);
    }
    return nullptr;
}

llvm::Value *CodeGen::emitBuiltinJson(const CallExpr &e) {
    return emitBuiltinJsonModuleStringify(
        e, "json", &emitJsonStringify, &emitJsonStringifySafe);
}

} // namespace ry
