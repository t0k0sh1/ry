#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"

bool CodeGen::isJsonValue(llvm::Value *val) {
    if (resource_sets_[RK_JsonValue].count(val)) return true;
    if (auto *load = llvm::dyn_cast<llvm::LoadInst>(val))
        if (load->getType()->isPointerTy())
            return resource_sets_[RK_JsonValue].count(load->getPointerOperand()) > 0;
    return false;
}

llvm::Value *CodeGen::emitBuiltinJson(const CallExpr &e) {
    if (!native_fn_arg_counts_.count(e.callee))
        return nullptr;

    // Helper: wrap ptr as Result and optionally track as JsonValue
    auto wrapJsonPtrResult = [&](llvm::Value *ptr, bool trackJson = false) -> llvm::Value * {
        llvm::Value *result = wrapPtrAsResult(ptr);
        if (trackJson) resource_sets_[RK_JsonValue].insert(result);
        return result;
    };

    // parse(text) -> Result<JsonValue, Error>
    if (e.callee == "parse") {
        requireArgs(e, 1);
        llvm::Value *text = emitExpr(*e.args[0]);
        if (text->getType() != ptrTy_)
            codegenError("parse() requires a str argument");
        auto fnTy = fnTy_ptr_to_ptr_;
        auto fn = mod_->getOrInsertFunction("__ry_json_parse", fnTy);
        llvm::Value *ptr = builder_.CreateCall(fn, {text}, "json_parse");
        return wrapJsonPtrResult(ptr, true);
    }

    // stringify(value) -> str
    // stringify(value, indent) -> str
    if (e.callee == "stringify") {
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

    // json_type(value) -> str
    if (e.callee == "json_type") {
        requireArgs(e, 1);
        llvm::Value *val = emitExpr(*e.args[0]);
        if (!isJsonValue(val))
            codegenError("json_type() requires a JsonValue argument");
        auto fnTy = fnTy_ptr_to_ptr_;
        auto fn = mod_->getOrInsertFunction("__ry_json_type", fnTy);
        return builder_.CreateCall(fn, {val}, "json_type");
    }

    // json_get(value, key) -> Result<JsonValue, Error>
    if (e.callee == "json_get") {
        requireArgs(e, 2);
        llvm::Value *val = emitExpr(*e.args[0]);
        if (!isJsonValue(val))
            codegenError("json_get() requires a JsonValue as first argument");
        llvm::Value *key = emitExpr(*e.args[1]);
        if (key->getType() != ptrTy_)
            codegenError("json_get() requires a str key");
        auto fnTy = fnTy_ptr_ptr_to_ptr_;
        auto fn = mod_->getOrInsertFunction("__ry_json_get", fnTy);
        llvm::Value *ptr = builder_.CreateCall(fn, {val, key}, "json_get");
        return wrapJsonPtrResult(ptr, true);
    }

    // json_at(value, index) -> Result<JsonValue, Error>
    if (e.callee == "json_at") {
        requireArgs(e, 2);
        llvm::Value *val = emitExpr(*e.args[0]);
        if (!isJsonValue(val))
            codegenError("json_at() requires a JsonValue as first argument");
        llvm::Value *idx = emitExpr(*e.args[1]);
        auto fnTy = fnTy_ptr_i64_to_ptr_;
        auto fn = mod_->getOrInsertFunction("__ry_json_at", fnTy);
        llvm::Value *ptr = builder_.CreateCall(fn, {val, idx}, "json_at");
        return wrapJsonPtrResult(ptr, true);
    }

    // json_str(value) -> Result<str, Error>
    if (e.callee == "json_str") {
        requireArgs(e, 1);
        llvm::Value *val = emitExpr(*e.args[0]);
        if (!isJsonValue(val))
            codegenError("json_str() requires a JsonValue argument");
        auto fnTy = fnTy_ptr_to_ptr_;
        auto fn = mod_->getOrInsertFunction("__ry_json_str", fnTy);
        llvm::Value *ptr = builder_.CreateCall(fn, {val}, "json_str");
        return wrapJsonPtrResult(ptr);
    }

    // json_int(value) -> Result<int, Error>
    if (e.callee == "json_int") {
        requireArgs(e, 1);
        llvm::Value *val = emitExpr(*e.args[0]);
        if (!isJsonValue(val))
            codegenError("json_int() requires a JsonValue argument");
        // Allocate stack slot for out parameter
        llvm::AllocaInst *outSlot = builder_.CreateAlloca(i64Ty_, nullptr, "json_int_out");
        auto fnTy = fnTy_ptr_ptr_to_i64_;
        auto fn = mod_->getOrInsertFunction("__ry_json_int", fnTy);
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

    // json_float(value) -> Result<float, Error>
    if (e.callee == "json_float") {
        requireArgs(e, 1);
        llvm::Value *val = emitExpr(*e.args[0]);
        if (!isJsonValue(val))
            codegenError("json_float() requires a JsonValue argument");
        llvm::AllocaInst *outSlot = builder_.CreateAlloca(f64Ty_, nullptr, "json_float_out");
        auto fnTy = fnTy_ptr_ptr_to_i64_;
        auto fn = mod_->getOrInsertFunction("__ry_json_float", fnTy);
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

    // json_bool(value) -> Result<bool, Error>
    if (e.callee == "json_bool") {
        requireArgs(e, 1);
        llvm::Value *val = emitExpr(*e.args[0]);
        if (!isJsonValue(val))
            codegenError("json_bool() requires a JsonValue argument");
        llvm::AllocaInst *outSlot = builder_.CreateAlloca(i64Ty_, nullptr, "json_bool_out");
        auto fnTy = fnTy_ptr_ptr_to_i64_;
        auto fn = mod_->getOrInsertFunction("__ry_json_bool", fnTy);
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

    // json_len(value) -> int
    if (e.callee == "json_len") {
        requireArgs(e, 1);
        llvm::Value *val = emitExpr(*e.args[0]);
        if (!isJsonValue(val))
            codegenError("json_len() requires a JsonValue argument");
        auto fnTy = fnTy_ptr_to_i64_;
        auto fn = mod_->getOrInsertFunction("__ry_json_len", fnTy);
        return builder_.CreateCall(fn, {val}, "json_len");
    }

    // json_keys(value) -> List<str>
    if (e.callee == "json_keys") {
        requireArgs(e, 1);
        llvm::Value *val = emitExpr(*e.args[0]);
        if (!isJsonValue(val))
            codegenError("json_keys() requires a JsonValue argument");
        auto fnTy = fnTy_ptr_to_ptr_;
        auto fn = mod_->getOrInsertFunction("__ry_json_keys", fnTy);
        llvm::Value *result = builder_.CreateCall(fn, {val}, "json_keys");
        type_meta_[TM_ListElem][result] = ptrTy_;
        return result;
    }

    // json_free(value) -> Unit
    if (e.callee == "json_free") {
        requireArgs(e, 1);
        llvm::Value *val = emitExpr(*e.args[0]);
        if (!isJsonValue(val))
            codegenError("json_free() requires a JsonValue argument");
        return emitResourceFree(val, RK_JsonValue, *e.args[0]);
    }

    return nullptr;
}
