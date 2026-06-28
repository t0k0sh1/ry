#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"
#include "ry/diagnostic/diagnostic.hpp"


namespace ry {

// ===== JSON5 custom emitters (#1855) =====
//
// Structurally identical to codegen_call_json.cpp's emitters. JSON5 has the
// same 8-function surface (`load[T]` × 2 / `dump` × 2 / `stringify` × 2 /
// `stringifySafe` × 2) and the same Result-channel contract; only the
// underlying runtime symbols differ (__ry_json5_* instead of __ry_json_*).

static llvm::Value *emitJson5StringifyImpl(CodeGen &cg, const CallExpr &e,
                                            const char *runtimeFn,
                                            const char *callName,
                                            bool wrapResult) {
    if (e.args.size() != 1 && e.args.size() != 2)
        cg.codegenError(std::string(e.callee) + "() takes 1 or 2 arguments");
    llvm::Value *val = cg.emitExpr(*e.args[0]);
    if (val->getType() != cg.anyTy_)
        val = cg.wrapInAny(val);

    llvm::Value *slot = cg.emitAlloca(cg.anyTy_, "json5_any_in");
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
        if (na.name != "sortKeys") {
            std::string msg = "unknown named argument '";
            msg += na.name;
            msg += "' for ";
            msg += e.callee;
            msg += "()";
            cg.codegenError(msg);
        }
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
static llvm::Value *emitJson5Stringify(CodeGen &cg, const CallExpr &e) {
    return emitJson5StringifyImpl(cg, e, "__ry_json5_stringify_any_ex",
                                   "json5_stringify_any_ex",
                                   /*wrapResult=*/false);
}

// stringifySafe(value: any[, indent: int][, sortKeys=bool]) -> Result<str, Error>
static llvm::Value *emitJson5StringifySafe(CodeGen &cg, const CallExpr &e) {
    return emitJson5StringifyImpl(cg, e, "__ry_json5_stringify_any_safe_ex",
                                   "json5_stringify_any_safe_ex",
                                   /*wrapResult=*/true);
}

// load[T](text: str) -> Result<T, Error>
// load[T](f: File)  -> Result<T, Error>
static llvm::Value *emitJson5Load(CodeGen &cg, const CallExpr &e,
                                   const std::string &typeArg) {
    cg.requireArgs(e, 1);
    llvm::Value *arg0 = cg.emitExpr(*e.args[0]);

    const char *runtimeFn;
    if (cg.isFile(arg0)) {
        runtimeFn = "__ry_json5_load_file";
    } else {
        if (!cg.isStrLike(arg0))
            cg.codegenError("load[T]() requires a str or File argument");
        runtimeFn = "__ry_json5_parse_to_any";
    }

    llvm::Type *targetTy = cg.resolveType(typeArg);
    llvm::StructType *resTy = cg.getResultType(targetTy, cg.errorTy_);

    llvm::Value *outSlot = cg.emitAlloca(cg.anyTy_, "json5_load_out");
    auto fn = cg.getRuntimeFn(runtimeFn, cg.i64Ty_, {cg.ptrTy_, cg.ptrTy_});
    llvm::Value *status =
        cg.builder_.CreateCall(fn, {arg0, outSlot}, "json5_load_status");
    llvm::Value *isParseErr = cg.emitICmpNE(
        status, llvm::ConstantInt::get(cg.i64Ty_, 0), "json5_load_parse_err");

    llvm::Value *result = cg.emitResultBranch(
        isParseErr, resTy,
        [&]() -> llvm::Value * {
            llvm::Value *anyVal = cg.emitLoad(cg.anyTy_, outSlot, "json5_load_any");
            return cg.tryUnwrapFromAny(anyVal, targetTy, typeArg);
        },
        [&]() -> llvm::Value * {
            return cg.buildErrValue(cg.buildErrorFromRuntime(), resTy);
        });
    cg.propagateTypeMeta("Result<" + typeArg + ", Error>", result);
    return result;
}

// Gate the dispatcher: only proceed if any json5 symbol is actually
// registered in `native_fn_sigs_`. Without this, json5 would race with
// json over the `load<T>` interceptor (both register dispatchers and
// alphabetical ordering picks json first, routing every `load[T]` call
// to `__ry_json_parse_to_any` — even when the user wrote
// `from json5 import load`). `load` is in the gate list even though it
// has no descriptor entry (it's intercepted below as a parametric
// callee) — without it, `from json5 import load` programs that don't
// also import another json5 symbol would silently fall through. The
// other three siblings (`dump`, `stringify`, `stringifySafe`) act as
// module-presence proxies because importing any symbol from `ry.json5`
// processes the whole module and registers those non-generic declarations.
static bool isJson5Imported(CodeGen &cg) {
    const auto &sigs = cg.getNativeFnSigs();
    return sigs.count("json5::load") || sigs.count("json5::stringify")
        || sigs.count("json5::stringifySafe") || sigs.count("json5::dump");
}

RY_REGISTER_STDLIB_PACKAGE(json5, "share/std/json5/json5.ry", dispatchJson5)
static llvm::Value *dispatchJson5(CodeGen &cg, const CallExpr &e) {
    if (!isJson5Imported(cg))
        return nullptr;

    // libry_json5.dylib registration is auto-derived inside `getRuntimeFn`
    // via the symbol→library auto-link (#2393) — every `__ry_json5_*` runtime
    // symbol the emitters issue registers "json5" automatically.

    // Intercept `load[T](...)` calls. The parser uses `[T]` syntax for generic
    // function calls but constructs the internal callee as `load<T>` (e.g.
    // `load<Person>`, `load<List<Person>>`), so we strip the `load<` prefix
    // and trailing `>` to recover the type-name.
    constexpr const char kLoadPrefix[] = "load<";
    constexpr size_t kLoadPrefixLen = sizeof(kLoadPrefix) - 1;
    if (e.callee.size() > kLoadPrefixLen + 1 &&
        e.callee.compare(0, kLoadPrefixLen, kLoadPrefix) == 0 &&
        e.callee.back() == '>') {
        std::string typeArg = e.callee.substr(
            kLoadPrefixLen,
            e.callee.size() - kLoadPrefixLen - 1);
        return emitJson5Load(cg, e, typeArg);
    }

    if (e.callee == "load") {
        cg.codegenError(
            "load() requires an explicit type argument: load[T](text|File). "
            "Pick a concrete T such as load[Map<str, any>], load[List<any>], "
            "or load[int]. load[any] is intentionally not supported.");
    }

    // #2482: `dump` is now descriptor-driven (see kOverrides). The remaining
    // chain (descriptor → emitGenericNativeCall) is reached by returning
    // nullptr here, mirroring dispatchIO / dispatchNet / dispatchPath /
    // dispatchThread / dispatchJson after their respective migrations.
    return nullptr;
}

// ===== Builtin Json5 (Pattern B carve-out, #2340) =====
//
// Delegates to the shared emitBuiltinJsonModuleStringify body in
// codegen_call_json.cpp; ordering and sig-gate rationale live there.
llvm::Value *CodeGen::emitBuiltinJson5(const CallExpr &e) {
    return emitBuiltinJsonModuleStringify(
        e, "json5", &emitJson5Stringify, &emitJson5StringifySafe);
}

} // namespace ry
