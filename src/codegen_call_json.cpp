#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"
#include "ry/diagnostic.hpp"


namespace ry {

static int rk_json_value;
namespace {
struct JsonResourceReg { JsonResourceReg() {
    rk_json_value = ResourceKindRegistry::instance().registerKind(
        "JsonValue", "__ry_arc_dtor_json_value", "__ry_json_cleanup", "json");
}} json_resource_reg;
}

bool CodeGen::isJsonValue(llvm::Value *val) {
    // JsonValue is always represented as a ptr at the LLVM level. Metadata
    // like `json_type_only` can ride on wrappers (e.g. Result<JsonValue,
    // Error>) to keep the inner type known, but those wrapper values are not
    // themselves JsonValues — routing them through the JSON runtime would
    // pass a struct to a function expecting `ptr` and fail IR verification
    // (#805). Gate on the LLVM type so metadata alone cannot mislabel a
    // non-ptr value as a JsonValue.
    if (val->getType() != ptrTy_)
        return false;
    if (hasResourceKind(val, rk_json_value))
        return true;
    auto *meta = getMeta(val);
    return meta && meta->json_type_only;
}

// ===== JSON custom emitters =====

static llvm::Value *emitJsonParse(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *text = cg.emitExpr(*e.args[0]);
    if (text->getType() != cg.ptrTy_)
        cg.codegenError("parse() requires a str argument");
    auto fnTy = cg.fnTy_ptr_to_ptr_;
    auto fn = cg.mod_->getOrInsertFunction("__ry_json_parse", fnTy);
    llvm::Value *ptr = cg.builder_.CreateCall(fn, {text}, "json_parse");
    llvm::Value *result = cg.wrapPtrAsResult(ptr);
    cg.addResourceKind(result, rk_json_value);
    return result;
}

static llvm::Value *emitJsonStringify(CodeGen &cg, const CallExpr &e) {
    if (e.args.size() == 1) {
        llvm::Value *val = cg.emitExpr(*e.args[0]);
        if (!cg.isJsonValue(val))
            cg.codegenError("stringify() requires a JsonValue argument");
        auto fnTy = cg.fnTy_ptr_to_ptr_;
        auto fn = cg.mod_->getOrInsertFunction("__ry_json_stringify", fnTy);
        return cg.builder_.CreateCall(fn, {val}, "json_stringify");
    }
    if (e.args.size() == 2) {
        llvm::Value *val = cg.emitExpr(*e.args[0]);
        if (!cg.isJsonValue(val))
            cg.codegenError("stringify() requires a JsonValue as first argument");
        llvm::Value *indent = cg.emitExpr(*e.args[1]);
        auto fnTy = cg.fnTy_ptr_i64_to_ptr_;
        auto fn = cg.mod_->getOrInsertFunction("__ry_json_stringify_pretty", fnTy);
        return cg.builder_.CreateCall(fn, {val, indent}, "json_stringify_pretty");
    }
    cg.codegenError("stringify() takes 1 or 2 arguments");
}

static llvm::Value *emitJsonKind(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *val = cg.emitExpr(*e.args[0]);
    if (!cg.isJsonValue(val))
        cg.codegenError("kind() requires a JsonValue argument");
    auto fnTy = cg.fnTy_ptr_to_ptr_;
    auto fn = cg.mod_->getOrInsertFunction("__ry_json_type", fnTy);
    return cg.builder_.CreateCall(fn, {val}, "json_kind");
}

static llvm::Value *emitJsonGet(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 2);
    llvm::Value *val = cg.emitExpr(*e.args[0]);
    if (!cg.isJsonValue(val))
        cg.codegenError("get() requires a JsonValue as first argument");
    llvm::Value *key = cg.emitExpr(*e.args[1]);
    if (key->getType() != cg.ptrTy_)
        cg.codegenError("get() requires a str key");
    auto fnTy = cg.fnTy_ptr_ptr_to_ptr_;
    auto fn = cg.mod_->getOrInsertFunction("__ry_json_get", fnTy);
    llvm::Value *ptr = cg.builder_.CreateCall(fn, {val, key}, "json_get");
    llvm::Value *result = cg.wrapPtrAsResult(ptr);
    cg.getOrCreateMeta(result).json_type_only = true;
    return result;
}

static llvm::Value *emitJsonAt(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 2);
    llvm::Value *val = cg.emitExpr(*e.args[0]);
    if (!cg.isJsonValue(val))
        cg.codegenError("at() requires a JsonValue as first argument");
    llvm::Value *idx = cg.emitExpr(*e.args[1]);
    auto fnTy = cg.fnTy_ptr_i64_to_ptr_;
    auto fn = cg.mod_->getOrInsertFunction("__ry_json_at", fnTy);
    llvm::Value *ptr = cg.builder_.CreateCall(fn, {val, idx}, "json_at");
    llvm::Value *result = cg.wrapPtrAsResult(ptr);
    cg.getOrCreateMeta(result).json_type_only = true;
    return result;
}

static llvm::Value *emitJsonToStr(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *val = cg.emitExpr(*e.args[0]);
    if (!cg.isJsonValue(val)) return nullptr;
    auto fnTy = cg.fnTy_ptr_to_ptr_;
    auto fn = cg.mod_->getOrInsertFunction("__ry_json_str", fnTy);
    llvm::Value *ptr = cg.builder_.CreateCall(fn, {val}, "json_str");
    return cg.wrapPtrAsResult(ptr);
}

static llvm::Value *emitJsonToInt(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *val = cg.emitExpr(*e.args[0]);
    if (!cg.isJsonValue(val)) return nullptr;
    llvm::AllocaInst *outSlot = cg.builder_.CreateAlloca(cg.i64Ty_, nullptr, "json_int_out");
    auto fn = cg.mod_->getOrInsertFunction("__ry_json_int", cg.fnTy_ptr_ptr_to_i64_);
    llvm::Value *status = cg.builder_.CreateCall(fn, {val, outSlot}, "json_int_status");
    llvm::Value *isErr = cg.builder_.CreateICmpNE(status,
        llvm::ConstantInt::get(cg.i64Ty_, 0), "json_int_err");
    llvm::StructType *resTy = cg.getResultType(cg.i64Ty_, cg.errorTy_);
    return cg.emitResultBranch(isErr, resTy,
        [&]() {
            llvm::Value *loaded = cg.builder_.CreateLoad(cg.i64Ty_, outSlot, "json_int_val");
            return cg.buildOkValue(loaded, resTy);
        },
        [&]() { return cg.buildErrValue(cg.buildErrorFromRuntime(), resTy); });
}

static llvm::Value *emitJsonToFloat(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *val = cg.emitExpr(*e.args[0]);
    if (!cg.isJsonValue(val)) return nullptr;
    llvm::AllocaInst *outSlot = cg.builder_.CreateAlloca(cg.f64Ty_, nullptr, "json_float_out");
    auto fn = cg.mod_->getOrInsertFunction("__ry_json_float", cg.fnTy_ptr_ptr_to_i64_);
    llvm::Value *status = cg.builder_.CreateCall(fn, {val, outSlot}, "json_float_status");
    llvm::Value *isErr = cg.builder_.CreateICmpNE(status,
        llvm::ConstantInt::get(cg.i64Ty_, 0), "json_float_err");
    llvm::StructType *resTy = cg.getResultType(cg.f64Ty_, cg.errorTy_);
    return cg.emitResultBranch(isErr, resTy,
        [&]() {
            llvm::Value *loaded = cg.builder_.CreateLoad(cg.f64Ty_, outSlot, "json_float_val");
            return cg.buildOkValue(loaded, resTy);
        },
        [&]() { return cg.buildErrValue(cg.buildErrorFromRuntime(), resTy); });
}

static llvm::Value *emitJsonToBool(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *val = cg.emitExpr(*e.args[0]);
    if (!cg.isJsonValue(val))
        cg.codegenError("to_bool() requires a JsonValue argument");
    llvm::AllocaInst *outSlot = cg.builder_.CreateAlloca(cg.i64Ty_, nullptr, "json_bool_out");
    auto fn = cg.mod_->getOrInsertFunction("__ry_json_bool", cg.fnTy_ptr_ptr_to_i64_);
    llvm::Value *status = cg.builder_.CreateCall(fn, {val, outSlot}, "json_bool_status");
    llvm::Value *isErr = cg.builder_.CreateICmpNE(status,
        llvm::ConstantInt::get(cg.i64Ty_, 0), "json_bool_err");
    llvm::StructType *resTy = cg.getResultType(cg.i1Ty_, cg.errorTy_);
    return cg.emitResultBranch(isErr, resTy,
        [&]() {
            llvm::Value *loaded = cg.builder_.CreateLoad(cg.i64Ty_, outSlot, "json_bool_i64");
            llvm::Value *boolVal = cg.builder_.CreateTrunc(loaded, cg.i1Ty_, "json_bool_val");
            return cg.buildOkValue(boolVal, resTy);
        },
        [&]() { return cg.buildErrValue(cg.buildErrorFromRuntime(), resTy); });
}

static llvm::Value *emitJsonLength(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *val = cg.emitExpr(*e.args[0]);
    if (!cg.isJsonValue(val)) return nullptr;
    auto fnTy = cg.fnTy_ptr_to_i64_;
    auto fn = cg.mod_->getOrInsertFunction("__ry_json_len", fnTy);
    return cg.builder_.CreateCall(fn, {val}, "json_len");
}

static llvm::Value *emitJsonKeys(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *val = cg.emitExpr(*e.args[0]);
    if (!cg.isJsonValue(val)) return nullptr;
    auto fnTy = cg.fnTy_ptr_to_ptr_;
    auto fn = cg.mod_->getOrInsertFunction("__ry_json_keys", fnTy);
    llvm::Value *ptr = cg.builder_.CreateCall(fn, {val}, "json_keys");
    llvm::Value *result = cg.wrapPtrAsResult(ptr);
    cg.setTypeMeta(CodeGen::TypeMeta::ListElem, result, cg.ptrTy_);
    return result;
}

static llvm::Value *emitJsonFree(CodeGen &cg, const CallExpr &e) {
    cg.requireArgs(e, 1);
    llvm::Value *val = cg.emitExpr(*e.args[0]);
    if (!cg.hasResourceKind(val, rk_json_value))
        cg.codegenError("json_free() requires a JsonValue argument");
    {
        auto *meta = cg.getMeta(val);
        if (meta && meta->json_type_only)
            cg.codegenError("json_free() cannot free borrowed child values from get()/at()");
    }
    return cg.emitResourceFree(val, rk_json_value, *e.args[0]);
}

// ===== JSON dispatch table =====

static const CodeGen::NativeDispatchEntry json_table[] = {
    {"parse",     nullptr, CodeGen::ReturnWrapping::ResultPtr,     1, nullptr, emitJsonParse},
    {"stringify", nullptr, CodeGen::ReturnWrapping::Direct,        1, nullptr, emitJsonStringify},
    {"kind",      nullptr, CodeGen::ReturnWrapping::Direct,        1, nullptr, emitJsonKind},
    {"get",       nullptr, CodeGen::ReturnWrapping::ResultPtr,     2, nullptr, emitJsonGet},
    {"at",        nullptr, CodeGen::ReturnWrapping::ResultPtr,     2, nullptr, emitJsonAt},
    {"to_str",    nullptr, CodeGen::ReturnWrapping::ResultPtr,     1, nullptr, emitJsonToStr},
    {"to_int",    nullptr, CodeGen::ReturnWrapping::ResultOutParam,1, "int",   emitJsonToInt},
    {"to_float",  nullptr, CodeGen::ReturnWrapping::ResultOutParam,1, "float", emitJsonToFloat},
    {"to_bool",   nullptr, CodeGen::ReturnWrapping::ResultOutParam,1, "int",   emitJsonToBool},
    {"length",    nullptr, CodeGen::ReturnWrapping::Direct,        1, nullptr, emitJsonLength},
    {"keys",      nullptr, CodeGen::ReturnWrapping::ResultPtr,     1, nullptr, emitJsonKeys},
    {"json_free", nullptr, CodeGen::ReturnWrapping::Direct,        1, nullptr, emitJsonFree},
};

RY_REGISTER_STDLIB_PACKAGE(json, "share/std/json/json.ry", dispatchJson)
static llvm::Value *dispatchJson(CodeGen &cg, const CallExpr &e) {
    return cg.emitTableDrivenNativeCall(e, "json", json_table, std::size(json_table));
}

} // namespace ry
