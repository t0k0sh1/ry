#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"

static bool lookupJsonSet(const std::unordered_set<llvm::Value*> &set, llvm::Value *val) {
    if (set.count(val)) return true;
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val))
        if (load->getType()->isPointerTy())
            return set.count(load->getPointerOperand()) > 0;
    return false;
}

bool CodeGen::isJsonValue(llvm::Value *val) {
    return lookupJsonSet(resource_sets_[RK_JsonValue], val) ||
           lookupJsonSet(json_type_only_, val);
}

// ===== JSON custom emitters =====

llvm::Value *CodeGen::emitJsonParse(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *text = emitExpr(*e.args[0]);
    if (text->getType() != ptrTy_)
        codegenError("parse() requires a str argument");
    auto fnTy = fnTy_ptr_to_ptr_;
    auto fn = mod_->getOrInsertFunction("__ry_json_parse", fnTy);
    llvm::Value *ptr = builder_.CreateCall(fn, {text}, "json_parse");
    llvm::Value *result = wrapPtrAsResult(ptr);
    resource_sets_[RK_JsonValue].insert(result);
    return result;
}

llvm::Value *CodeGen::emitJsonStringify(const CallExpr &e) {
    if (e.args.size() == 1) {
        llvm::Value *val = emitExpr(*e.args[0]);
        if (!isJsonValue(val))
            codegenError("stringify() requires a JsonValue argument");
        auto fnTy = fnTy_ptr_to_ptr_;
        auto fn = mod_->getOrInsertFunction("__ry_json_stringify", fnTy);
        return builder_.CreateCall(fn, {val}, "json_stringify");
    }
    if (e.args.size() == 2) {
        llvm::Value *val = emitExpr(*e.args[0]);
        if (!isJsonValue(val))
            codegenError("stringify() requires a JsonValue as first argument");
        llvm::Value *indent = emitExpr(*e.args[1]);
        auto fnTy = fnTy_ptr_i64_to_ptr_;
        auto fn = mod_->getOrInsertFunction("__ry_json_stringify_pretty", fnTy);
        return builder_.CreateCall(fn, {val, indent}, "json_stringify_pretty");
    }
    codegenError("stringify() takes 1 or 2 arguments");
}

llvm::Value *CodeGen::emitJsonKind(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *val = emitExpr(*e.args[0]);
    if (!isJsonValue(val))
        codegenError("kind() requires a JsonValue argument");
    auto fnTy = fnTy_ptr_to_ptr_;
    auto fn = mod_->getOrInsertFunction("__ry_json_type", fnTy);
    return builder_.CreateCall(fn, {val}, "json_kind");
}

llvm::Value *CodeGen::emitJsonGet(const CallExpr &e) {
    requireArgs(e, 2);
    llvm::Value *val = emitExpr(*e.args[0]);
    if (!isJsonValue(val))
        codegenError("get() requires a JsonValue as first argument");
    llvm::Value *key = emitExpr(*e.args[1]);
    if (key->getType() != ptrTy_)
        codegenError("get() requires a str key");
    auto fnTy = fnTy_ptr_ptr_to_ptr_;
    auto fn = mod_->getOrInsertFunction("__ry_json_get", fnTy);
    llvm::Value *ptr = builder_.CreateCall(fn, {val, key}, "json_get");
    llvm::Value *result = wrapPtrAsResult(ptr);
    json_type_only_.insert(result);
    return result;
}

llvm::Value *CodeGen::emitJsonAt(const CallExpr &e) {
    requireArgs(e, 2);
    llvm::Value *val = emitExpr(*e.args[0]);
    if (!isJsonValue(val))
        codegenError("at() requires a JsonValue as first argument");
    llvm::Value *idx = emitExpr(*e.args[1]);
    auto fnTy = fnTy_ptr_i64_to_ptr_;
    auto fn = mod_->getOrInsertFunction("__ry_json_at", fnTy);
    llvm::Value *ptr = builder_.CreateCall(fn, {val, idx}, "json_at");
    llvm::Value *result = wrapPtrAsResult(ptr);
    json_type_only_.insert(result);
    return result;
}

llvm::Value *CodeGen::emitJsonToStr(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *val = emitExpr(*e.args[0]);
    if (!isJsonValue(val)) return nullptr;
    auto fnTy = fnTy_ptr_to_ptr_;
    auto fn = mod_->getOrInsertFunction("__ry_json_str", fnTy);
    llvm::Value *ptr = builder_.CreateCall(fn, {val}, "json_str");
    return wrapPtrAsResult(ptr);
}

llvm::Value *CodeGen::emitJsonToInt(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *val = emitExpr(*e.args[0]);
    if (!isJsonValue(val)) return nullptr;
    llvm::AllocaInst *outSlot = builder_.CreateAlloca(i64Ty_, nullptr, "json_int_out");
    auto fn = mod_->getOrInsertFunction("__ry_json_int", fnTy_ptr_ptr_to_i64_);
    llvm::Value *status = builder_.CreateCall(fn, {val, outSlot}, "json_int_status");
    llvm::Value *isErr = builder_.CreateICmpNE(status,
        llvm::ConstantInt::get(i64Ty_, 0), "json_int_err");
    llvm::StructType *resTy = getResultType(i64Ty_, errorTy_);
    return emitResultBranch(isErr, resTy,
        [&]() {
            llvm::Value *loaded = builder_.CreateLoad(i64Ty_, outSlot, "json_int_val");
            return buildOkValue(loaded, resTy);
        },
        [&]() { return buildErrValue(buildErrorFromRuntime(), resTy); });
}

llvm::Value *CodeGen::emitJsonToFloat(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *val = emitExpr(*e.args[0]);
    if (!isJsonValue(val)) return nullptr;
    llvm::AllocaInst *outSlot = builder_.CreateAlloca(f64Ty_, nullptr, "json_float_out");
    auto fn = mod_->getOrInsertFunction("__ry_json_float", fnTy_ptr_ptr_to_i64_);
    llvm::Value *status = builder_.CreateCall(fn, {val, outSlot}, "json_float_status");
    llvm::Value *isErr = builder_.CreateICmpNE(status,
        llvm::ConstantInt::get(i64Ty_, 0), "json_float_err");
    llvm::StructType *resTy = getResultType(f64Ty_, errorTy_);
    return emitResultBranch(isErr, resTy,
        [&]() {
            llvm::Value *loaded = builder_.CreateLoad(f64Ty_, outSlot, "json_float_val");
            return buildOkValue(loaded, resTy);
        },
        [&]() { return buildErrValue(buildErrorFromRuntime(), resTy); });
}

llvm::Value *CodeGen::emitJsonToBool(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *val = emitExpr(*e.args[0]);
    if (!isJsonValue(val))
        codegenError("to_bool() requires a JsonValue argument");
    llvm::AllocaInst *outSlot = builder_.CreateAlloca(i64Ty_, nullptr, "json_bool_out");
    auto fn = mod_->getOrInsertFunction("__ry_json_bool", fnTy_ptr_ptr_to_i64_);
    llvm::Value *status = builder_.CreateCall(fn, {val, outSlot}, "json_bool_status");
    llvm::Value *isErr = builder_.CreateICmpNE(status,
        llvm::ConstantInt::get(i64Ty_, 0), "json_bool_err");
    llvm::StructType *resTy = getResultType(i1Ty_, errorTy_);
    return emitResultBranch(isErr, resTy,
        [&]() {
            llvm::Value *loaded = builder_.CreateLoad(i64Ty_, outSlot, "json_bool_i64");
            llvm::Value *boolVal = builder_.CreateTrunc(loaded, i1Ty_, "json_bool_val");
            return buildOkValue(boolVal, resTy);
        },
        [&]() { return buildErrValue(buildErrorFromRuntime(), resTy); });
}

llvm::Value *CodeGen::emitJsonLength(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *val = emitExpr(*e.args[0]);
    if (!isJsonValue(val)) return nullptr;
    auto fnTy = fnTy_ptr_to_i64_;
    auto fn = mod_->getOrInsertFunction("__ry_json_len", fnTy);
    return builder_.CreateCall(fn, {val}, "json_len");
}

llvm::Value *CodeGen::emitJsonKeys(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *val = emitExpr(*e.args[0]);
    if (!isJsonValue(val)) return nullptr;
    auto fnTy = fnTy_ptr_to_ptr_;
    auto fn = mod_->getOrInsertFunction("__ry_json_keys", fnTy);
    llvm::Value *ptr = builder_.CreateCall(fn, {val}, "json_keys");
    llvm::Value *result = wrapPtrAsResult(ptr);
    type_meta_[TM_ListElem][result] = ptrTy_;
    return result;
}

llvm::Value *CodeGen::emitJsonFree(const CallExpr &e) {
    requireArgs(e, 1);
    llvm::Value *val = emitExpr(*e.args[0]);
    if (!lookupJsonSet(resource_sets_[RK_JsonValue], val))
        codegenError("json_free() requires a JsonValue argument");
    if (lookupJsonSet(json_type_only_, val))
        codegenError("json_free() cannot free borrowed child values from get()/at()");
    return emitResourceFree(val, RK_JsonValue, *e.args[0]);
}

// ===== JSON dispatch table =====

static const CodeGen::NativeDispatchEntry json_table[] = {
    {"parse",     nullptr, CodeGen::ReturnWrapping::ResultPtr,     1, nullptr, &CodeGen::emitJsonParse},
    {"stringify", nullptr, CodeGen::ReturnWrapping::Direct,        1, nullptr, &CodeGen::emitJsonStringify},
    {"kind",      nullptr, CodeGen::ReturnWrapping::Direct,        1, nullptr, &CodeGen::emitJsonKind},
    {"get",       nullptr, CodeGen::ReturnWrapping::ResultPtr,     2, nullptr, &CodeGen::emitJsonGet},
    {"at",        nullptr, CodeGen::ReturnWrapping::ResultPtr,     2, nullptr, &CodeGen::emitJsonAt},
    {"to_str",    nullptr, CodeGen::ReturnWrapping::ResultPtr,     1, nullptr, &CodeGen::emitJsonToStr},
    {"to_int",    nullptr, CodeGen::ReturnWrapping::ResultOutParam,1, "int",   &CodeGen::emitJsonToInt},
    {"to_float",  nullptr, CodeGen::ReturnWrapping::ResultOutParam,1, "float", &CodeGen::emitJsonToFloat},
    {"to_bool",   nullptr, CodeGen::ReturnWrapping::ResultOutParam,1, "int",   &CodeGen::emitJsonToBool},
    {"length",    nullptr, CodeGen::ReturnWrapping::Direct,        1, nullptr, &CodeGen::emitJsonLength},
    {"keys",      nullptr, CodeGen::ReturnWrapping::ResultPtr,     1, nullptr, &CodeGen::emitJsonKeys},
    {"json_free", nullptr, CodeGen::ReturnWrapping::Direct,        1, nullptr, &CodeGen::emitJsonFree},
};

llvm::Value *CodeGen::emitBuiltinJson(const CallExpr &e) {
    return emitTableDrivenNativeCall(e, "json", json_table,
                                     std::size(json_table));
}
