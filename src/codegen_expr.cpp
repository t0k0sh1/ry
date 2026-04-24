#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"
#include "ry/diagnostic.hpp"
#include <climits>
#include <unordered_set>


namespace ry {

static int rk_regex;
namespace {
struct RegexResourceReg { RegexResourceReg() {
    rk_regex = ResourceKindRegistry::instance().registerKind(
        "Regex", nullptr, nullptr, nullptr);
}} regex_resource_reg;
}

// Range check for suffixed integer literals.
//
// NumberExpr.value holds the unsigned bit pattern of a non-negative magnitude
// (see ast.hpp: negation is expressed as UnaryExpr). Signed suffixes accept
// magnitudes up to 2^(N-1) — `-INT{N}_MIN` for `-INT{N}_MIN` itself via
// UnaryExpr. Unsigned suffixes compare the bit pattern as uint64_t because
// `u64` max is stored as `int64_t(-1)`.
template<typename ErrorFn>
static void validateIntRange(int64_t value, const std::string &suffix,
                             ErrorFn error) {
    uint64_t uval = static_cast<uint64_t>(value);
    auto fmtValue = [&]() {
        // Unsigned suffixes: show the bit pattern as unsigned so large u64
        // literals don't render as negative numbers.
        if (!suffix.empty() && suffix[0] == 'u')
            return std::to_string(uval);
        return std::to_string(value);
    };
    // Signed suffixes: positive literals must fit in INT{N}_MAX. The extra
    // value `abs(INT{N}_MIN)` is only legal via a unary-minus wrapper, which
    // is handled by a constant-fold fast-path in emitExprVariant(UnaryExpr)
    // before this check runs.
    if (suffix == "i8") {
        if (uval > static_cast<uint64_t>(INT8_MAX))
            error("i8 literal out of range: " + fmtValue());
    } else if (suffix == "i16") {
        if (uval > static_cast<uint64_t>(INT16_MAX))
            error("i16 literal out of range: " + fmtValue());
    } else if (suffix == "i32") {
        if (uval > static_cast<uint64_t>(INT32_MAX))
            error("i32 literal out of range: " + fmtValue());
    } else if (suffix == "i64") {
        // Magnitude > INT64_MAX cannot fit in a signed i64 even via unary
        // minus (except the INT64_MIN edge case, which is caught by the
        // same UnaryExpr fast-path).
        if (value < 0)
            error("i64 literal out of range: " + fmtValue());
    } else if (suffix == "u8") {
        if (uval > UINT8_MAX)
            error("u8 literal out of range: " + fmtValue());
    } else if (suffix == "u16") {
        if (uval > UINT16_MAX)
            error("u16 literal out of range: " + fmtValue());
    } else if (suffix == "u32") {
        if (uval > UINT32_MAX)
            error("u32 literal out of range: " + fmtValue());
    }
    // suffix == "u64": any 64-bit bit pattern is valid; unary minus on a
    // u64 is rejected earlier by isUnsignedLowLevelName in the UnaryExpr
    // path, so no negative magnitude can reach this site.
}

llvm::Value *CodeGen::emitExpr(const ExprNode &node) {
    if (node.loc.isValid()) current_loc_ = node.loc;
    return std::visit([this](const auto &e) -> llvm::Value* { return emitExprVariant(e); },
                      node.data);
}

llvm::Value *CodeGen::emitExprVariant(const NumberExpr &e) {
    if (e.suffix.empty()) {
        // Bare `int` is i64. With the strtoull-based parser a negative bit
        // pattern means the literal exceeds INT64_MAX (negative literals
        // arrive as UnaryExpr, so NumberExpr.value is always a non-negative
        // magnitude). Reject so users must either add a `u64` suffix or a
        // u-type annotation (handled by emitVarDecl suffix injection).
        if (e.value < 0)
            codegenError("integer literal out of range for int: " +
                         std::to_string(static_cast<uint64_t>(e.value)));
        return llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(e.value), true);
    }

    validateIntRange(e.value, e.suffix,
        [this](const std::string &msg) { codegenError(msg); });

    llvm::Type *ty = resolveType(e.suffix);
    bool isSigned = !isUnsignedLowLevelName(e.suffix);
    auto *result = llvm::ConstantInt::get(ty, static_cast<uint64_t>(e.value), isSigned);
    // Do NOT set low_level_type_names_ on ConstantInt: LLVM's constant uniquing
    // shares the same pointer for identical (type, value) pairs, causing metadata
    // corruption when different suffixes map to the same constant (#311).
    // Suffix info is propagated via AST (getExprLowLevelSuffix) instead.
    return result;
}

llvm::Value *CodeGen::emitExprVariant(const FloatExpr &e) {
    if (e.suffix.empty() || e.suffix == "f64")
        return llvm::ConstantFP::get(f64Ty_, e.value);
    // suffix == "f32"
    auto *result = llvm::ConstantFP::get(f32Ty_, e.value);
    // Do NOT set low_level_type_names_ on ConstantFP (same reason as NumberExpr, #311).
    return result;
}

llvm::Value *CodeGen::emitExprVariant(const BoolExpr &e) {
    return llvm::ConstantInt::get(i1Ty_, e.value ? 1 : 0, false);
}

llvm::Value *CodeGen::emitExprVariant(const StringExpr &e) {
    return cachedGlobalString(e.value, ".str");
}

llvm::Value *CodeGen::emitExprVariant(const RegexExpr &e) {
    // Separate cache prevents collision with string literals of the same
    // content — otherwise marking the pointer as RK_Regex would poison a
    // later string literal, causing isStringValue() to return false.
    auto *gs = buildArcGlobal(e.pattern, ".regex", regex_global_cache_);
    addResourceKind(gs, rk_regex);
    return gs;
}

llvm::Value *CodeGen::emitExprVariant(const VariableExpr &e) {
    llvm::AllocaInst *alloca = findVar(e.name);
    if (alloca) {
        if (deprecated_variables_.count(e.name))
            emitDeprecationWarning(e.name);
        // Weak reference auto-upgrade: access returns Option<T>
        if (isWeakManaged(alloca)) {
            auto *headerPtr = builder_.CreateLoad(ptrTy_, alloca, e.name + ".weak_hdr");
            auto it = weak_inner_type_names_.find(alloca);
            if (it == weak_inner_type_names_.end())
                codegenError("internal: weak variable missing inner type name: " + e.name);
            auto *result = emitWeakUpgrade(headerPtr, it->second);
            // Propagate collection metadata from the original weak alloca to the
            // upgrade result so that match bindings inherit element types
            propagateMeta(alloca, result);
            return result;
        }
        llvm::Type *ty = alloca->getAllocatedType();
        // Fixed-length arrays: return alloca pointer for GEP-based indexing
        if (llvm::isa<llvm::ArrayType>(ty))
            return alloca;
        return builder_.CreateLoad(ty, alloca, e.name);
    }
    // Module-level bindings declared at top level (#817). Reached when this
    // expression is inside a function body (scope_stack_ cleared by FnScope)
    // and `e.name` refers to a top-level `let` or `@const` declared earlier
    // in source order. We load the storage pointer from the module-level
    // trampoline global, then load the value through it.
    if (auto *b = findModuleGlobal(e.name)) {
        if (deprecated_variables_.count(e.name))
            emitDeprecationWarning(e.name);
        // Weak and resource-typed top-level bindings are out of scope for
        // v1. These flags were captured at `registerModuleGlobal` time
        // (while still in __ry_main__ context) because FnScope clears the
        // per-function weak_managed_vars_/resource_managed_vars_ sets when
        // entering a function body — so the original alloca would no longer
        // register as weak/resource here and the intended errors would
        // silently not fire.
        if (b->is_weak)
            codegenError("weak top-level variables are not yet accessible from functions (#817 follow-up)");
        if (b->is_resource)
            codegenError("resource-typed top-level variables are not yet accessible from functions (#817 follow-up)");
        auto *storagePtr = loadModuleGlobalStorage(*b, e.name);
        llvm::Type *valueTy = b->valueTy();
        // Fixed-length arrays: return the storage pointer for GEP-based
        // indexing. Record the reverse mapping so that `IndexExpr` consumers
        // which dispatch on `dyn_cast<AllocaInst>` can still reach the array
        // alloca (and its `array_elem_type_names_` entry) through the
        // module-global trampoline (#817).
        if (llvm::isa<llvm::ArrayType>(valueTy)) {
            array_storage_to_alloca_[storagePtr] = b->original_alloca;
            return storagePtr;
        }
        auto *loaded = builder_.CreateLoad(valueTy, storagePtr, e.name);
        // Propagate metadata from the original alloca (low-level type names,
        // collection element types, enum value type, union value type,
        // fn_type_info, etc.) so the loaded value behaves the same as a load
        // from the original alloca in __ry_main__.
        propagateMeta(b->original_alloca, loaded);
        return loaded;
    }
    // Check native constants (PI, E, Inf, NaN) after local scope
    if (!native_constants_.empty() && native_constants_.count(e.name))
        return emitNativeConstant(e.name);
    // Try named function reference
    auto *fnOverloads = findFunction(e.name);
    if (fnOverloads && fnOverloads->size() == 1) {
        if (deprecated_functions_.count(e.name))
            emitDeprecationWarning(e.name);
        auto &entry = (*fnOverloads)[0];
        llvm::Function *func = entry.func;
        FnTypeInfo info;
        info.paramTypes = entry.paramTypes;
        info.paramTypeNames = entry.paramTypeNames;
        info.returnType = func->getReturnType();
        {
            std::string resolved = resolveTypeAlias(entry.returnTypeName);
            if (isFunctionTypeName(resolved))
                info.returnFnTypeInfo = std::make_unique<FnTypeInfo>(parseFnTypeAnnotation(resolved));
        }

        // If this nested function has captures, materialize as a closure struct
        if (!entry.capturedNames.empty()) {
            info.capturedVars = entry.capturedNames;
            info.capturedTypes = entry.capturedTypes;
            info.capturedArcKinds = entry.capturedArcKinds;
            info.capturedResourceKinds = entry.capturedResourceKinds;
            if (entry.capturedClosureInfos)
                info.capturedClosureInfos = std::make_unique<std::unordered_map<size_t, FnTypeInfo>>(*entry.capturedClosureInfos);
            info.sourceFn = func;
            getOrCreateMeta(func).fn_type_info = info;

            // Load captured values from the current scope
            std::vector<llvm::Value*> capturedValues;
            for (auto &capName : entry.capturedNames) {
                llvm::AllocaInst *capAlloca = findVar(capName);
                if (!capAlloca)
                    codegenError("captured variable '" + capName + "' not found when materializing function '" + e.name + "'");
                capturedValues.push_back(builder_.CreateLoad(
                    capAlloca->getAllocatedType(), capAlloca, capName + ".cap_mat"));
            }
            return buildClosureStruct(func, info, capturedValues);
        }

        info.sourceFn = func;
        getOrCreateMeta(func).fn_type_info = info;
        return func;
    }
    codegenError("undefined variable: " + e.name);
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<UnaryExpr> &e) {
    // Constant-fold `-<int literal>` before recursing into emitExpr, so that
    // magnitudes up to `|INT{N}_MIN|` are accepted (e.g. `-128i8`,
    // `-9223372036854775808i64`, `-9223372036854775808`). validateIntRange
    // limits a bare NumberExpr to INT{N}_MAX; the unary-minus wrapper is the
    // only way to reach the signed MIN edge. Bare int (empty suffix) is
    // treated as i64 (#1025).
    if (e->op == "-") {
        if (auto *ne = std::get_if<NumberExpr>(&e->operand->data)) {
            const bool isBareInt = ne->suffix.empty();
            const bool isSignedSuffix =
                !ne->suffix.empty() && isLowLevelTypeName(ne->suffix) &&
                !isUnsignedLowLevelName(ne->suffix) && ne->suffix != "f32";
            if (isBareInt || isSignedSuffix) {
                llvm::Type *ty = isBareInt ? i64Ty_ : resolveType(ne->suffix);
                const unsigned bits = ty->getIntegerBitWidth();
                const uint64_t absMin = static_cast<uint64_t>(1) << (bits - 1);
                const uint64_t mag = static_cast<uint64_t>(ne->value);
                if (mag > absMin) {
                    if (isBareInt)
                        codegenError(
                            "integer literal out of range for int: -" +
                            std::to_string(mag));
                    else
                        codegenError(ne->suffix + " literal out of range: -" +
                                     std::to_string(mag));
                }
                // Unsigned two's-complement negation avoids signed overflow
                // when mag == 2^63 (INT64_MIN). Equivalent to
                // -static_cast<int64_t>(mag) for all representable values.
                const uint64_t negBits = static_cast<uint64_t>(0) - mag;
                return llvm::ConstantInt::get(ty, negBits, /*isSigned=*/false);
            }
        }
    }

    llvm::Value *val = emitExpr(*e->operand);

    // Try user-defined unary operator first
    std::string opFnName = "operator" + e->op;
    if (auto *result = tryUnaryOperatorCall(opFnName, val))
        return result;

    // any-type unary dispatch (#223)
    if (isAnyType(val->getType())) {
        if (e->op == "-") return emitAnyUnaryNeg(val);
        if (e->op == "+") return val;
        codegenError("operator '" + e->op + "' not supported for any type");
    }

    if (e->op == "+") {
        return val;
    }
    if (e->op == "-") {
        if (val->getType()->isDoubleTy() || val->getType()->isFloatTy())
            return builder_.CreateFNeg(val, "fneg");
        // Check for unsigned suffix on the operand AST node
        if (auto *ne = std::get_if<NumberExpr>(&e->operand->data)) {
            if (isUnsignedLowLevelName(ne->suffix))
                codegenError("cannot negate unsigned type '" + ne->suffix + "'");
        }
        if (isUnsignedLowLevel(val))
            codegenError("cannot negate unsigned type '" + getLowLevelTypeName(val) + "'");
        rejectBoolInOperator(val, "-", "arithmetic");
        val = promoteToInt(val);
        // Overflow check only for high-level int; low-level i64 wraps
        bool isLowLevel = isLowLevelTy(val);
        if (!isLowLevel) {
            if (auto *ne = std::get_if<NumberExpr>(&e->operand->data))
                isLowLevel = isLowLevelTypeName(ne->suffix);
        }
        if (val->getType() == i64Ty_ && !isLowLevel)
            return emitIntOverflowCheck(llvm::Intrinsic::ssub_with_overflow,
                                         llvm::ConstantInt::get(i64Ty_, 0), val, "neg");
        llvm::Value *neg = builder_.CreateNeg(val, "neg");
        // Propagate low-level metadata through unary negation (#595)
        std::string llName = getLowLevelTypeName(val);
        if (llName.empty()) {
            if (auto *ne = std::get_if<NumberExpr>(&e->operand->data))
                if (isLowLevelTypeName(ne->suffix)) llName = ne->suffix;
        }
        if (!llName.empty()) getOrCreateMeta(neg).low_level_type_name = llName;
        return neg;
    }
    if (e->op == "not") {
        llvm::Value *boolVal = toBool(val);
        return builder_.CreateNot(boolVal, "not");
    }
    if (e->op == "~") {
        if (val->getType()->isDoubleTy())
            codegenError("bitwise NOT (~) requires integer, got float");
        rejectBoolInOperator(val, "~", "bitwise");
        val = promoteToInt(val);
        return builder_.CreateNot(val, "bnot");
    }
    codegenError("unknown unary operator: " + e->op);
}

// ===== Operator overload helpers =====

llvm::Value *CodeGen::findAndCallOverload(const std::string &opFnName,
                                           llvm::ArrayRef<llvm::Value*> args,
                                           const char *callName) {
    auto *fit = findFunction(opFnName);
    if (!fit) return nullptr;

    for (auto &entry : *fit) {
        if (entry.paramTypes.size() != args.size()) continue;
        bool match = true;
        for (size_t i = 0; i < args.size(); ++i) {
            if (entry.paramTypes[i] != args[i]->getType()) {
                match = false; break;
            }
        }
        if (!match) continue;
        if (entry.func->getReturnType()->isVoidTy())
            return builder_.CreateCall(entry.func, args);
        llvm::Value *result = builder_.CreateCall(entry.func, args, callName);
        propagateReturnTypeMeta(&entry, result);
        propagateReturnFnTypeMeta(&entry, entry.func, result);
        return result;
    }
    return nullptr;
}

llvm::Value *CodeGen::tryOperatorCall(const std::string &opFnName,
                                       llvm::Value *lhs, llvm::Value *rhs) {
    return findAndCallOverload(opFnName, {lhs, rhs}, "opcall");
}

llvm::Value *CodeGen::tryUnaryOperatorCall(const std::string &opFnName,
                                            llvm::Value *operand) {
    return findAndCallOverload(opFnName, {operand}, "opcall");
}

llvm::Value *CodeGen::trySubscriptOperatorCall(
    llvm::Value *object, llvm::ArrayRef<llvm::Value*> indices) {
    llvm::SmallVector<llvm::Value*, 4> args;
    args.push_back(object);
    args.append(indices.begin(), indices.end());
    return findAndCallOverload("operator[]", args, "subscript");
}

bool CodeGen::trySubscriptAssignOperatorCall(
    llvm::Value *object, llvm::ArrayRef<llvm::Value*> indices, llvm::Value *value) {
    llvm::SmallVector<llvm::Value*, 4> args;
    args.push_back(object);
    args.append(indices.begin(), indices.end());
    args.push_back(value);
    return findAndCallOverload("operator[]=", args, "subscr_assign") != nullptr;
}

llvm::Value *CodeGen::tryCallOperator(const std::string &callee,
                                       const std::vector<ExprPtr> &args) {
    llvm::AllocaInst *varPtr = findVar(callee);
    if (!varPtr) return nullptr;
    llvm::Type *allocTy = varPtr->getAllocatedType();
    if (!allocTy->isStructTy()) return nullptr;
    llvm::Value *obj = builder_.CreateLoad(allocTy, varPtr, callee + ".val");
    llvm::SmallVector<llvm::Value*, 4> callArgs;
    callArgs.push_back(obj);
    for (auto &arg : args)
        callArgs.push_back(emitExpr(*arg));
    return findAndCallOverload("operator()", callArgs, "call_op");
}

// ===== B2: BinaryExpr sub-dispatchers =====

llvm::Value *CodeGen::emitComparisonOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs,
                                        const std::string &lhsHint, const std::string &rhsHint) {
    const std::string &llNameHint = !lhsHint.empty() ? lhsHint : rhsHint;
    // Shared primitive-type allowlist used by union/set equality guards.
    static const std::unordered_set<std::string> kEqPrimitives = {
        "int", "float", "str", "bool"
    };

    // Type (from type_of) comparison: identity by id field only (ignore name)
    if (lhs->getType() == typeTy_ && rhs->getType() == typeTy_ &&
        (op == "==" || op == "!=")) {
        llvm::Value *lid = builder_.CreateExtractValue(lhs, 0, "type_lhs_id");
        llvm::Value *rid = builder_.CreateExtractValue(rhs, 0, "type_rhs_id");
        if (op == "==") return builder_.CreateICmpEQ(lid, rid, "type_eq");
        return builder_.CreateICmpNE(lid, rid, "type_ne");
    }

    // Option type comparison with none: check has_value flag only
    // Only allowed when at least one side is Option and both sides are Option
    // (none is also an Option value with has_value=false)
    bool lhsIsOpt = isOptionType(lhs->getType());
    bool rhsIsOpt = isOptionType(rhs->getType());
    if (lhsIsOpt && rhsIsOpt && (op == "==" || op == "!=")) {
        llvm::Value *lhsFlag = builder_.CreateExtractValue(lhs, 0, "lhs_has");
        llvm::Value *rhsFlag = builder_.CreateExtractValue(rhs, 0, "rhs_has");

        // When Option types differ, one operand must be a 'none' literal whose
        // LLVM type was not widened to match the other side (e.g. Error? == none).
        // In this case only the has_value flag matters: Some(...) vs none always differ.
        // The type checker ensures that Some(a)==Some(b) with incompatible inner
        // types is rejected before codegen is reached.
        if (lhs->getType() != rhs->getType()) {
            if (op == "==") return builder_.CreateICmpEQ(lhsFlag, rhsFlag, "opt_eq");
            return builder_.CreateICmpNE(lhsFlag, rhsFlag, "opt_ne");
        }

        // Same Option type: only compare inner values when both are Some.
        // Use control flow to avoid UB from comparison on undef inner values (None case).
        llvm::Value *bothNone = builder_.CreateAnd(
            builder_.CreateNot(lhsFlag), builder_.CreateNot(rhsFlag), "both_none");
        llvm::Value *bothSome = builder_.CreateAnd(lhsFlag, rhsFlag, "both_some");

        llvm::BasicBlock *startBB    = builder_.GetInsertBlock();
        llvm::Function   *curFn      = startBB->getParent();
        llvm::BasicBlock *cmpInnerBB = llvm::BasicBlock::Create(*ctx_, "opt.cmp_inner", curFn);
        llvm::BasicBlock *mergeBB    = llvm::BasicBlock::Create(*ctx_, "opt.merge",     curFn);

        builder_.CreateCondBr(bothSome, cmpInnerBB, mergeBB);

        builder_.SetInsertPoint(cmpInnerBB);
        llvm::Value *lhsInner = builder_.CreateExtractValue(lhs, 1, "opt_l_inner");
        llvm::Value *rhsInner = builder_.CreateExtractValue(rhs, 1, "opt_r_inner");

        // Option<Collection>: ExtractValue loses ValueMetadata; rebuild from outer aggregate.
        // Snapshot name before propagateTypeMeta — it may rehash value_metadata_.
        if (llvm::cast<llvm::StructType>(lhs->getType())->getElementType(1) == ptrTy_) {
            std::string innerName = buildTypeNameFromMeta(lhs);
            if (innerName.empty())
                innerName = buildTypeNameFromMeta(rhs);
            if (!innerName.empty() && innerName != "str") {
                propagateTypeMeta(innerName, lhsInner);
                propagateMeta(lhsInner, rhsInner);
            }
        }

        llvm::Value *innerEq  = emitComparisonOp("==", lhsInner, rhsInner, "", "");
        llvm::BasicBlock *cmpDoneBB = builder_.GetInsertBlock();
        builder_.CreateBr(mergeBB);

        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *eqResult = builder_.CreatePHI(i1Ty_, 2, "opt_eq");
        eqResult->addIncoming(bothNone, startBB);
        eqResult->addIncoming(innerEq, cmpDoneBB);
        if (op == "!=") return builder_.CreateNot(eqResult, "opt_ne");
        return eqResult;
    }

    // Result type equality: compare is_ok flag, then only the active variant's payload.
    // Use control flow to avoid UB from comparison on inactive (garbage) variant data.
    if (isResultType(lhs->getType()) && isResultType(rhs->getType()) &&
        lhs->getType() == rhs->getType() && (op == "==" || op == "!=")) {
        llvm::Value *lhsOk   = builder_.CreateExtractValue(lhs, 0, "lhs_is_ok");
        llvm::Value *rhsOk   = builder_.CreateExtractValue(rhs, 0, "rhs_is_ok");
        llvm::Value *flagsEq = builder_.CreateICmpEQ(lhsOk, rhsOk, "flags_eq");

        llvm::Function   *curFn     = builder_.GetInsertBlock()->getParent();
        llvm::BasicBlock *startBB   = builder_.GetInsertBlock();
        llvm::BasicBlock *sameKindBB = llvm::BasicBlock::Create(*ctx_, "req.same",  curFn);
        llvm::BasicBlock *isOkBB    = llvm::BasicBlock::Create(*ctx_, "req.ok",    curFn);
        llvm::BasicBlock *isErrBB   = llvm::BasicBlock::Create(*ctx_, "req.err",   curFn);
        llvm::BasicBlock *mergeBB   = llvm::BasicBlock::Create(*ctx_, "req.merge", curFn);

        builder_.CreateCondBr(flagsEq, sameKindBB, mergeBB);

        builder_.SetInsertPoint(sameKindBB);
        builder_.CreateCondBr(lhsOk, isOkBB, isErrBB);

        // Result<Collection, E>: ExtractValue drops ValueMetadata; rebuild from outer aggregate.
        // Snapshot name before propagateTypeMeta — it may rehash value_metadata_ (KNOWLEDGE #858).
        auto *resST   = llvm::cast<llvm::StructType>(lhs->getType());
        bool okIsPtr  = (resST->getElementType(1) == ptrTy_);
        bool errIsPtr = (resST->getElementType(2) == ptrTy_);
        std::string innerName;
        if (okIsPtr || errIsPtr) {
            innerName = buildTypeNameFromMeta(lhs);
            if (innerName.empty())
                innerName = buildTypeNameFromMeta(rhs);
        }

        builder_.SetInsertPoint(isOkBB);
        llvm::Value *lhsOkPayload = builder_.CreateExtractValue(lhs, 1, "lhs_ok");
        llvm::Value *rhsOkPayload = builder_.CreateExtractValue(rhs, 1, "rhs_ok");
        if (okIsPtr && !innerName.empty() && innerName != "str") {
            propagateTypeMeta(innerName, lhsOkPayload);
            propagateMeta(lhsOkPayload, rhsOkPayload);
        }
        llvm::Value *okEq = emitComparisonOp("==", lhsOkPayload, rhsOkPayload, "", "");
        llvm::BasicBlock *okDoneBB = builder_.GetInsertBlock();
        builder_.CreateBr(mergeBB);

        builder_.SetInsertPoint(isErrBB);
        llvm::Value *lhsErrPayload = builder_.CreateExtractValue(lhs, 2, "lhs_err");
        llvm::Value *rhsErrPayload = builder_.CreateExtractValue(rhs, 2, "rhs_err");
        if (errIsPtr && !innerName.empty() && innerName != "str") {
            propagateTypeMeta(innerName, lhsErrPayload);
            propagateMeta(lhsErrPayload, rhsErrPayload);
        }
        llvm::Value *errEq = emitComparisonOp("==", lhsErrPayload, rhsErrPayload, "", "");
        llvm::BasicBlock *errDoneBB = builder_.GetInsertBlock();
        builder_.CreateBr(mergeBB);

        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *eqResult = builder_.CreatePHI(i1Ty_, 3, "res_eq");
        eqResult->addIncoming(llvm::ConstantInt::getFalse(*ctx_), startBB);
        eqResult->addIncoming(okEq, okDoneBB);
        eqResult->addIncoming(errEq, errDoneBB);
        if (op == "!=") return builder_.CreateNot(eqResult, "res_ne");
        return eqResult;
    }

    // Record (struct) type comparison: field-by-field (only == and != supported)
    if (op == "==" || op == "!=") {
        auto *lhsST = llvm::dyn_cast<llvm::StructType>(lhs->getType());
        auto *rhsST = llvm::dyn_cast<llvm::StructType>(rhs->getType());
        if (lhsST && rhsST && lhsST == rhsST) {
            std::string typeName = lhsST->getName().str();
            auto it = record_types_.find(typeName);
            if (it != record_types_.end())
                return emitRecordComparison(op, lhs, rhs, it->second);
            // Tuple (anonymous struct) comparison: field-by-field
            if (isTupleStructType(lhsST)) {
                RecordInfo synth;
                synth.llvmType = lhsST;
                synth.fields.reserve(lhsST->getNumElements());
                for (unsigned i = 0; i < lhsST->getNumElements(); ++i) {
                    FieldDef fd;
                    fd.name = std::to_string(i);
                    synth.fields.push_back(std::move(fd));
                }
                return emitRecordComparison(op, lhs, rhs, synth);
            }
            // ADT enum: compare by tag, then by payload field-by-field (#959)
            {
                std::string adtName = findAdtEnumName(lhsST);
                if (!adtName.empty()) {
                    const EnumInfo &einfo = enum_types_.at(adtName);

                    // Check for function-typed fields before emitting any IR.
                    for (const auto &vname : einfo.variantOrder) {
                        auto fit = einfo.variantFields.find(vname);
                        if (fit == einfo.variantFields.end()) continue;
                        for (size_t fi = 0; fi < fit->second.fieldTypeNames.size(); ++fi) {
                            const std::string &ftn = fit->second.fieldTypeNames[fi];
                            if (isFunctionTypeName(ftn))
                                codegenError("ADT enum == / != is not supported for "
                                    "function-typed payload '" + vname + "." +
                                    std::to_string(fi) + "'");
                        }
                    }

                    // Fast path: all variants have no payload → tag-only comparison
                    bool anyPayload = false;
                    for (const auto &vname : einfo.variantOrder) {
                        auto apit = einfo.variantFields.find(vname);
                        if (apit != einfo.variantFields.end() &&
                                !apit->second.fieldTypes.empty()) {
                            anyPayload = true; break;
                        }
                    }
                    if (!anyPayload) {
                        llvm::Value *lhsTag = builder_.CreateExtractValue(lhs, 0, "lhs.tag");
                        llvm::Value *rhsTag = builder_.CreateExtractValue(rhs, 0, "rhs.tag");
                        if (op == "==") return builder_.CreateICmpEQ(lhsTag, rhsTag, "enum_eq");
                        return builder_.CreateICmpNE(lhsTag, rhsTag, "enum_ne");
                    }

                    // Full comparison: tag mismatch → false; tag match → switch per-variant
                    llvm::Value *lhsTag = builder_.CreateExtractValue(lhs, 0, "lhs.etag");
                    llvm::Value *rhsTag = builder_.CreateExtractValue(rhs, 0, "rhs.etag");
                    llvm::Function *curFn = builder_.GetInsertBlock()->getParent();
                    llvm::BasicBlock *sameTagBB  = llvm::BasicBlock::Create(*ctx_, "aeq.same",  curFn);
                    llvm::BasicBlock *invalidBB  = llvm::BasicBlock::Create(*ctx_, "aeq.inv",   curFn);
                    llvm::BasicBlock *mergeBB    = llvm::BasicBlock::Create(*ctx_, "aeq.merge", curFn);

                    llvm::BasicBlock *entryBB = builder_.GetInsertBlock();
                    builder_.CreateCondBr(
                        builder_.CreateICmpEQ(lhsTag, rhsTag, "aeq_tag"), sameTagBB, mergeBB);

                    builder_.SetInsertPoint(sameTagBB);

                    const llvm::DataLayout &dl = mod_->getDataLayout();

                    // Store lhs/rhs into allocas so we can GEP into the payload bytes
                    llvm::AllocaInst *lhsTmp = builder_.CreateAlloca(lhsST, nullptr, "aeq.la");
                    llvm::AllocaInst *rhsTmp = builder_.CreateAlloca(lhsST, nullptr, "aeq.ra");
                    lhsTmp->setAlignment(dl.getABITypeAlign(lhsST));
                    rhsTmp->setAlignment(dl.getABITypeAlign(lhsST));
                    builder_.CreateStore(lhs, lhsTmp);
                    builder_.CreateStore(rhs, rhsTmp);

                    llvm::SwitchInst *sw = builder_.CreateSwitch(
                        lhsTag, invalidBB,
                        static_cast<unsigned>(einfo.variantOrder.size()));

                    builder_.SetInsertPoint(invalidBB);
                    builder_.CreateBr(mergeBB);

                    // PHI: entry→false, invalid→false, per-variant→result
                    builder_.SetInsertPoint(mergeBB);
                    auto *phi = builder_.CreatePHI(
                        i1Ty_,
                        static_cast<unsigned>(einfo.variantOrder.size() + 2),
                        "aeq.phi");
                    phi->addIncoming(llvm::ConstantInt::getFalse(*ctx_), entryBB);
                    phi->addIncoming(llvm::ConstantInt::getFalse(*ctx_), invalidBB);

                    for (const auto &vname : einfo.variantOrder) {
                        auto tagVal = static_cast<uint64_t>(einfo.variants.at(vname));
                        llvm::BasicBlock *caseBB = llvm::BasicBlock::Create(
                            *ctx_, "aeq.v." + vname, curFn);
                        sw->addCase(
                            llvm::ConstantInt::get(
                                llvm::cast<llvm::IntegerType>(i64Ty_), tagVal),
                            caseBB);
                        builder_.SetInsertPoint(caseBB);

                        auto vfit = einfo.variantFields.find(vname);
                        bool hasFields = (vfit != einfo.variantFields.end() &&
                                          !vfit->second.fieldTypes.empty());

                        if (!hasFields) {
                            // No payload: tags matched → equal
                            phi->addIncoming(llvm::ConstantInt::getTrue(*ctx_), caseBB);
                            builder_.CreateBr(mergeBB);
                            continue;
                        }

                        // Get pointer to start of payload ([N x i8])
                        llvm::Value *lhsPayload = builder_.CreateStructGEP(
                            einfo.adtType, lhsTmp, 1, "aeq.lp." + vname);
                        llvm::Value *rhsPayload = builder_.CreateStructGEP(
                            einfo.adtType, rhsTmp, 1, "aeq.rp." + vname);

                        // Compare fields sequentially with true short-circuit AND:
                        // for fi > 0, branch on the previous result before loading
                        // the next field so that loads/comparisons for field N are
                        // only emitted inside continBB (i.e., when field N-1 was equal).
                        size_t offset = 0;
                        llvm::Value *lastEq = llvm::ConstantInt::getTrue(*ctx_);

                        for (size_t fi = 0; fi < vfit->second.fieldTypes.size(); ++fi) {
                            llvm::Type *fieldTy = vfit->second.fieldTypes[fi];
                            const std::string &fieldTyName = vfit->second.fieldTypeNames[fi];

                            uint64_t align = dl.getABITypeAlign(fieldTy).value();
                            offset = (offset + align - 1) / align * align;

                            std::string sfx;
                            sfx.reserve(vname.size() + 2 + 4);
                            sfx += vname;
                            sfx += '.';
                            sfx += std::to_string(fi);

                            // Branch on previous result before loading this field
                            if (fi > 0) {
                                llvm::BasicBlock *continBB = llvm::BasicBlock::Create(
                                    *ctx_, "aeq.fc." + sfx, curFn);
                                llvm::BasicBlock *shortBB = llvm::BasicBlock::Create(
                                    *ctx_, "aeq.fs." + sfx, curFn);
                                builder_.CreateCondBr(lastEq, continBB, shortBB);

                                builder_.SetInsertPoint(shortBB);
                                phi->addIncoming(llvm::ConstantInt::getFalse(*ctx_), shortBB);
                                builder_.CreateBr(mergeBB);

                                builder_.SetInsertPoint(continBB);
                            }

                            llvm::Value *lhsFieldPtr = builder_.CreateGEP(
                                llvm::Type::getInt8Ty(*ctx_), lhsPayload,
                                {llvm::ConstantInt::get(i64Ty_, offset)},
                                "aeq.lfp." + sfx);
                            llvm::Value *rhsFieldPtr = builder_.CreateGEP(
                                llvm::Type::getInt8Ty(*ctx_), rhsPayload,
                                {llvm::ConstantInt::get(i64Ty_, offset)},
                                "aeq.rfp." + sfx);

                            llvm::Value *lf = builder_.CreateLoad(
                                fieldTy, lhsFieldPtr, "aeq.lf." + sfx);
                            llvm::Value *rf = builder_.CreateLoad(
                                fieldTy, rhsFieldPtr, "aeq.rf." + sfx);

                            // Rebuild metadata for pointer-typed fields (#736 pattern)
                            if (fieldTy == ptrTy_ &&
                                    !fieldTyName.empty() && fieldTyName != "str") {
                                propagateTypeMeta(fieldTyName, lf);
                                propagateMeta(lf, rf);
                            }

                            lastEq = emitComparisonOp("==", lf, rf, "", "");

                            offset += dl.getTypeAllocSize(fieldTy);
                        }

                        phi->addIncoming(lastEq, builder_.GetInsertBlock());
                        builder_.CreateBr(mergeBB);
                    }

                    builder_.SetInsertPoint(mergeBB);
                    if (op == "!=") return builder_.CreateNot(phi, "aeq_ne");
                    return phi;
                }
            }
            // Union type: compare tag then dispatch to per-variant inner comparison
            for (auto &[uname, uinfo] : union_type_info_) {
                if (uinfo.llvmType != lhsST) continue;

                // Reject function-typed variants; collections/records use propagateTypeMeta below.
                for (const auto &cname : uinfo.componentNames) {
                    if (isFunctionTypeName(cname))
                        codegenError("union == / != is not supported for "
                            "function-typed variant '" + cname + "'");
                }

                llvm::Function *curFn = builder_.GetInsertBlock()->getParent();
                llvm::BasicBlock *sameTagBB = llvm::BasicBlock::Create(*ctx_, "ueq.same",  curFn);
                llvm::BasicBlock *invalidBB = llvm::BasicBlock::Create(*ctx_, "ueq.inv",   curFn);
                llvm::BasicBlock *mergeBB   = llvm::BasicBlock::Create(*ctx_, "ueq.merge", curFn);

                llvm::Value *lhsTag = builder_.CreateExtractValue(lhs, 0, "lhs.utag");
                llvm::Value *rhsTag = builder_.CreateExtractValue(rhs, 0, "rhs.utag");
                llvm::BasicBlock *entryBB = builder_.GetInsertBlock();
                builder_.CreateCondBr(
                    builder_.CreateICmpEQ(lhsTag, rhsTag, "utag_eq"), sameTagBB, mergeBB);

                builder_.SetInsertPoint(sameTagBB);
                auto *dataTy = uinfo.llvmType->getElementType(1);
                llvm::AllocaInst *lhsTmp = builder_.CreateAlloca(dataTy, nullptr, "ueq.ld");
                llvm::AllocaInst *rhsTmp = builder_.CreateAlloca(dataTy, nullptr, "ueq.rd");
                lhsTmp->setAlignment(mod_->getDataLayout().getABITypeAlign(dataTy));
                rhsTmp->setAlignment(mod_->getDataLayout().getABITypeAlign(dataTy));
                builder_.CreateStore(builder_.CreateExtractValue(lhs, 1, "lhs.udata"), lhsTmp);
                builder_.CreateStore(builder_.CreateExtractValue(rhs, 1, "rhs.udata"), rhsTmp);
                llvm::SwitchInst *sw = builder_.CreateSwitch(
                    lhsTag, invalidBB, static_cast<unsigned>(uinfo.componentTypes.size()));

                builder_.SetInsertPoint(invalidBB);
                builder_.CreateBr(mergeBB);

                builder_.SetInsertPoint(mergeBB);
                auto *phi = builder_.CreatePHI(
                    i1Ty_, static_cast<unsigned>(uinfo.componentTypes.size() + 2), "ueq.phi");
                phi->addIncoming(llvm::ConstantInt::getFalse(*ctx_), entryBB);
                phi->addIncoming(llvm::ConstantInt::getFalse(*ctx_), invalidBB);

                for (size_t ci = 0; ci < uinfo.componentTypes.size(); ++ci) {
                    llvm::BasicBlock *caseBB = llvm::BasicBlock::Create(
                        *ctx_, "ueq.c" + std::to_string(ci), curFn);
                    sw->addCase(
                        llvm::ConstantInt::get(llvm::cast<llvm::IntegerType>(i64Ty_), ci),
                        caseBB);
                    builder_.SetInsertPoint(caseBB);
                    llvm::Type *compTy = uinfo.componentTypes[ci];
                    llvm::Value *li = builder_.CreateLoad(compTy, lhsTmp, "ueq.li");
                    llvm::Value *ri = builder_.CreateLoad(compTy, rhsTmp, "ueq.ri");
                    // Rebuild metadata for pointer-typed variants (#736, #960 pattern)
                    const std::string &compName = uinfo.componentNames[ci];
                    if (compTy == ptrTy_ && !compName.empty() && compName != "str") {
                        propagateTypeMeta(compName, li);
                        propagateMeta(li, ri);
                    }
                    llvm::Value *caseEq = emitComparisonOp("==", li, ri, "", "");
                    phi->addIncoming(caseEq, builder_.GetInsertBlock());
                    builder_.CreateBr(mergeBB);
                }

                builder_.SetInsertPoint(mergeBB);
                if (op == "!=") return builder_.CreateNot(phi, "ueq_ne");
                return phi;
            }
        }
    }

    // List equality: element-wise comparison (only == and != supported)
    {
        llvm::Type *lhsElemTy = getListElementType(lhs);
        llvm::Type *rhsElemTy = getListElementType(rhs);
        if (lhsElemTy && rhsElemTy && (op == "==" || op == "!=")) {
            if (lhsElemTy != rhsElemTy)
                codegenError("cannot compare List values with different element types");
            // Hoist pointer-element metadata checks before loop scaffolding:
            // lhs metadata is loop-invariant; resolve and validate it once here.
            std::string leqElemName;
            if (lhsElemTy == ptrTy_) {
                auto *lhsMeta = getMeta(lhs);
                if (lhsMeta && lhsMeta->list_elem_fn_type_info)
                    codegenError("list == / != is not supported for function-typed elements");
                if (lhsMeta && !lhsMeta->list_elem_type_name.empty() &&
                        lhsMeta->list_elem_type_name != "str")
                    leqElemName = lhsMeta->list_elem_type_name;
            }
            auto lf = loadListHeader(lhs, "leq_l");
            auto rf = loadListHeader(rhs, "leq_r");
            llvm::Function *curFn = builder_.GetInsertBlock()->getParent();
            llvm::BasicBlock *entryBB   = builder_.GetInsertBlock();
            llvm::BasicBlock *sameLenBB = llvm::BasicBlock::Create(*ctx_, "leq.slen",  curFn);
            llvm::BasicBlock *condBB    = llvm::BasicBlock::Create(*ctx_, "leq.cond",  curFn);
            llvm::BasicBlock *bodyBB    = llvm::BasicBlock::Create(*ctx_, "leq.body",  curFn);
            llvm::BasicBlock *nextBB    = llvm::BasicBlock::Create(*ctx_, "leq.next",  curFn);
            llvm::BasicBlock *failBB    = llvm::BasicBlock::Create(*ctx_, "leq.fail",  curFn);
            llvm::BasicBlock *mergeBB   = llvm::BasicBlock::Create(*ctx_, "leq.merge", curFn);
            builder_.CreateCondBr(
                builder_.CreateICmpEQ(lf.len, rf.len, "leq_leneq"), sameLenBB, mergeBB);
            builder_.SetInsertPoint(sameLenBB);
            llvm::AllocaInst *leqI = builder_.CreateAlloca(i64Ty_, nullptr, "leq_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), leqI);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(condBB);
            llvm::Value *leqIv = builder_.CreateLoad(i64Ty_, leqI, "leq_iv");
            builder_.CreateCondBr(builder_.CreateICmpSLT(leqIv, lf.len), bodyBB, mergeBB);
            builder_.SetInsertPoint(bodyBB);
            llvm::Value *leqIc = builder_.CreateLoad(i64Ty_, leqI, "leq_ic");
            llvm::Value *le = builder_.CreateLoad(lhsElemTy,
                builder_.CreateGEP(lhsElemTy, lf.data, {leqIc}, "leq_lep"), "leq_le");
            llvm::Value *re = builder_.CreateLoad(lhsElemTy,
                builder_.CreateGEP(lhsElemTy, rf.data, {leqIc}, "leq_rep"), "leq_re");
            // Pointer elements lose ValueMetadata when loaded via GEP.
            // Rebuild from the pre-resolved type name so emitComparisonOp recurses correctly.
            if (!leqElemName.empty()) {
                propagateTypeMeta(leqElemName, le);
                propagateMeta(le, re);  // copy from le; avoids re-parsing the type name
            }
            builder_.CreateCondBr(emitComparisonOp("==", le, re, "", ""), nextBB, failBB);
            builder_.SetInsertPoint(nextBB);
            builder_.CreateStore(
                builder_.CreateAdd(leqIc, llvm::ConstantInt::get(i64Ty_, 1), "leq_in"), leqI);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(failBB);
            builder_.CreateBr(mergeBB);
            builder_.SetInsertPoint(mergeBB);
            auto *leqPhi = builder_.CreatePHI(i1Ty_, 3, "leq.phi");
            leqPhi->addIncoming(llvm::ConstantInt::getFalse(*ctx_), entryBB);
            leqPhi->addIncoming(llvm::ConstantInt::getTrue(*ctx_),  condBB);
            leqPhi->addIncoming(llvm::ConstantInt::getFalse(*ctx_), failBB);
            if (op == "!=") return builder_.CreateNot(leqPhi, "leq_ne");
            return leqPhi;
        }
    }

    // Set equality: same length + lhs is a subset of rhs
    {
        llvm::Type *lhsSetElemTy = getSetElementType(lhs);
        llvm::Type *rhsSetElemTy = getSetElementType(rhs);
        if (lhsSetElemTy && rhsSetElemTy && (op == "==" || op == "!=")) {
            if (lhsSetElemTy != rhsSetElemTy)
                codegenError("cannot compare Set values with different element types");
            const ValueMetadata *lhsSetMeta = getMeta(lhs);
            if (lhsSetMeta && lhsSetMeta->set_elem_fn_type_info)
                codegenError("set == / != is not supported for function-typed elements");
            auto lf = loadSetHeader(lhs, "seq_l");
            auto rf = loadSetHeader(rhs, "seq_r");
            llvm::Value *lenEq = builder_.CreateICmpEQ(lf.len, rf.len, "seq_leneq");

            // Short-circuit: skip the subset-check loop when lengths differ.
            llvm::Function   *curFn    = builder_.GetInsertBlock()->getParent();
            llvm::BasicBlock *startBB  = builder_.GetInsertBlock();
            llvm::BasicBlock *checkBB  = llvm::BasicBlock::Create(*ctx_, "seq.check", curFn);
            llvm::BasicBlock *mergeBB  = llvm::BasicBlock::Create(*ctx_, "seq.merge",  curFn);

            builder_.CreateCondBr(lenEq, checkBB, mergeBB);

            builder_.SetInsertPoint(checkBB);
            llvm::Value *isSubset = emitSubsetCheck(lhs, rhs, "seq_sub");
            llvm::BasicBlock *checkDoneBB = builder_.GetInsertBlock();
            builder_.CreateBr(mergeBB);

            builder_.SetInsertPoint(mergeBB);
            llvm::PHINode *eqResult = builder_.CreatePHI(i1Ty_, 2, "seq_eq");
            eqResult->addIncoming(llvm::ConstantInt::getFalse(*ctx_), startBB);
            eqResult->addIncoming(isSubset, checkDoneBB);

            if (op == "!=") return builder_.CreateNot(eqResult, "seq_ne");
            return eqResult;
        }
    }

    // Map equality: same length + all keys/values from lhs exist with equal values in rhs
    {
        llvm::Type *lhsKeyTy = getMapKeyType(lhs);
        llvm::Type *rhsKeyTy = getMapKeyType(rhs);
        if (lhsKeyTy && rhsKeyTy && (op == "==" || op == "!=")) {
            if (lhsKeyTy != rhsKeyTy)
                codegenError("cannot compare Map values with different key types");
            llvm::Type *valTy = getMapValueType(lhs);
            if (!valTy || valTy != getMapValueType(rhs))
                codegenError("cannot compare Map values with different value types");
            // Hoist key-side and value-side metadata checks before loop scaffolding.
            // Keys: guard against function-typed keys; capture name for linear-scan
            // fallback (records, tuples, nested collections have no hash function).
            std::string meqKeyName;
            {
                auto *lhsMeta = getMeta(lhs);
                if (lhsMeta && lhsMeta->map_key_fn_type_info)
                    codegenError("map == / != is not supported for function-typed keys");
                if (lhsMeta && !lhsMeta->map_key_type_name.empty() &&
                        !kEqPrimitives.count(lhsMeta->map_key_type_name))
                    meqKeyName = lhsMeta->map_key_type_name;
                // StructType keys (records, tuples) always need the linear scan.
                // When map_key_type_name is absent (e.g. non-empty literal without
                // annotation), fall back to a sentinel that opts into the scan path
                // without triggering metadata rebuild inside emitMapKeyLookup.
                if (meqKeyName.empty() && llvm::isa<llvm::StructType>(lhsKeyTy))
                    meqKeyName = "__record__";
            }
            std::string meqValName;
            if (valTy == ptrTy_) {
                auto *lhsMeta = getMeta(lhs);
                if (lhsMeta && lhsMeta->map_value_fn_type_info)
                    codegenError("map == / != is not supported for function-typed values");
                if (lhsMeta && !lhsMeta->map_value_type_name.empty() &&
                        lhsMeta->map_value_type_name != "str")
                    meqValName = lhsMeta->map_value_type_name;
            }
            auto lf = loadMapHeader(lhs, "meq_l");
            auto rf = loadMapHeader(rhs, "meq_r");
            llvm::Function *curFn = builder_.GetInsertBlock()->getParent();
            llvm::BasicBlock *entryBB   = builder_.GetInsertBlock();
            llvm::BasicBlock *sameLenBB = llvm::BasicBlock::Create(*ctx_, "meq.slen",  curFn);
            llvm::BasicBlock *condBB    = llvm::BasicBlock::Create(*ctx_, "meq.cond",  curFn);
            llvm::BasicBlock *bodyBB    = llvm::BasicBlock::Create(*ctx_, "meq.body",  curFn);
            llvm::BasicBlock *valBB     = llvm::BasicBlock::Create(*ctx_, "meq.val",   curFn);
            llvm::BasicBlock *nextBB    = llvm::BasicBlock::Create(*ctx_, "meq.next",  curFn);
            llvm::BasicBlock *failBB    = llvm::BasicBlock::Create(*ctx_, "meq.fail",  curFn);
            llvm::BasicBlock *mergeBB   = llvm::BasicBlock::Create(*ctx_, "meq.merge", curFn);
            builder_.CreateCondBr(
                builder_.CreateICmpEQ(lf.len, rf.len, "meq_leneq"), sameLenBB, mergeBB);
            builder_.SetInsertPoint(sameLenBB);
            llvm::AllocaInst *meqI = builder_.CreateAlloca(i64Ty_, nullptr, "meq_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), meqI);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(condBB);
            llvm::Value *meqIv = builder_.CreateLoad(i64Ty_, meqI, "meq_iv");
            builder_.CreateCondBr(builder_.CreateICmpSLT(meqIv, lf.len), bodyBB, mergeBB);
            builder_.SetInsertPoint(bodyBB);
            llvm::Value *meqIc = builder_.CreateLoad(i64Ty_, meqI, "meq_ic");
            llvm::Value *key = builder_.CreateLoad(lhsKeyTy,
                builder_.CreateGEP(lhsKeyTy, lf.keys, {meqIc}, "meq_kep"), "meq_k");
            llvm::Value *rhsIdx = emitMapKeyLookup(rhs, key, lhsKeyTy, meqKeyName);
            builder_.CreateCondBr(
                builder_.CreateICmpSGE(rhsIdx, llvm::ConstantInt::get(i64Ty_, 0), "meq_found"),
                valBB, failBB);
            builder_.SetInsertPoint(valBB);
            llvm::Value *lv = builder_.CreateLoad(valTy,
                builder_.CreateGEP(valTy, lf.vals, {meqIc}, "meq_lvep"), "meq_lv");
            llvm::Value *rv = builder_.CreateLoad(valTy,
                builder_.CreateGEP(valTy, rf.vals, {rhsIdx}, "meq_rvep"), "meq_rv");
            // Pointer values lose ValueMetadata when loaded via GEP.
            // Rebuild from the pre-resolved type name so emitComparisonOp recurses correctly.
            if (!meqValName.empty()) {
                propagateTypeMeta(meqValName, lv);
                propagateMeta(lv, rv);  // copy from lv; avoids re-parsing the type name
            }
            builder_.CreateCondBr(emitComparisonOp("==", lv, rv, "", ""), nextBB, failBB);
            builder_.SetInsertPoint(nextBB);
            builder_.CreateStore(
                builder_.CreateAdd(meqIc, llvm::ConstantInt::get(i64Ty_, 1), "meq_in"), meqI);
            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(failBB);
            builder_.CreateBr(mergeBB);
            builder_.SetInsertPoint(mergeBB);
            auto *meqPhi = builder_.CreatePHI(i1Ty_, 3, "meq.phi");
            meqPhi->addIncoming(llvm::ConstantInt::getFalse(*ctx_), entryBB);
            meqPhi->addIncoming(llvm::ConstantInt::getTrue(*ctx_),  condBB);
            meqPhi->addIncoming(llvm::ConstantInt::getFalse(*ctx_), failBB);
            if (op == "!=") return builder_.CreateNot(meqPhi, "meq_ne");
            return meqPhi;
        }
    }

    // Closure: equality comparison is not supported
    {
        auto *lhsMeta = getMeta(lhs);
        auto *rhsMeta = getMeta(rhs);
        if ((lhsMeta && lhsMeta->fn_type_info) || (rhsMeta && rhsMeta->fn_type_info))
            codegenError("operator '" + op + "' is not supported for closure values");
    }

    bool lhsIsStr = isStringValue(lhs);
    bool rhsIsStr = isStringValue(rhs);

    // String comparison — NUL-safe via __ry_str_cmp (byte_len + memcmp)
    if (lhsIsStr && rhsIsStr) {
        llvm::Value *lenL = emitStringByteLen(lhs);
        llvm::Value *lenR = emitStringByteLen(rhs);
        auto strCmpTy = llvm::FunctionType::get(i32Ty_, {ptrTy_, i64Ty_, ptrTy_, i64Ty_}, false);
        auto strCmpFn = mod_->getOrInsertFunction("__ry_str_cmp", strCmpTy);
        llvm::Value *cmp = builder_.CreateCall(strCmpFn, {lhs, lenL, rhs, lenR}, "str_cmp");
        llvm::Value *zero = llvm::ConstantInt::get(i32Ty_, 0);
        if (op == "==") return builder_.CreateICmpEQ(cmp, zero, "str_eq");
        if (op == "!=") return builder_.CreateICmpNE(cmp, zero, "str_ne");
        if (op == "<")  return builder_.CreateICmpSLT(cmp, zero, "str_lt");
        if (op == "<=") return builder_.CreateICmpSLE(cmp, zero, "str_le");
        if (op == ">")  return builder_.CreateICmpSGT(cmp, zero, "str_gt");
        if (op == ">=") return builder_.CreateICmpSGE(cmp, zero, "str_ge");
        codegenError("unsupported string comparison: " + op);
    }

    // Reject str with non-str operands
    if (lhsIsStr || rhsIsStr)
        codegenError("type error: operator '" + op + "' not supported between str and non-str types");

    // Low-level type mix check
    checkLowLevelTypeMix(lhs, rhs, op, lhsHint, rhsHint);

    // Low-level type native-width comparison
    // Enter when metadata or matching AST hints identify a low-level pair (#595)
    bool cmpLowLevel = lhs->getType() == rhs->getType() &&
        ((isLowLevelTy(lhs) || isLowLevelTy(rhs)) ||
         (!lhsHint.empty() && lhsHint == rhsHint && isLowLevelTypeName(lhsHint)));
    if (cmpLowLevel) {
        if (isLowLevelFloatTy(lhs->getType())) {
            llvm::CmpInst::Predicate pred;
            if      (op == "==") pred = llvm::CmpInst::FCMP_OEQ;
            else if (op == "!=") pred = llvm::CmpInst::FCMP_UNE;
            else if (op == "<")  pred = llvm::CmpInst::FCMP_OLT;
            else if (op == "<=") pred = llvm::CmpInst::FCMP_OLE;
            else if (op == ">")  pred = llvm::CmpInst::FCMP_OGT;
            else                 pred = llvm::CmpInst::FCMP_OGE;
            return builder_.CreateFCmp(pred, lhs, rhs, "fcmp_ll");
        }
        bool isUnsigned = isUnsignedLowLevel(lhs) || isUnsignedLowLevelName(llNameHint);
        llvm::CmpInst::Predicate pred;
        if      (op == "==") pred = llvm::CmpInst::ICMP_EQ;
        else if (op == "!=") pred = llvm::CmpInst::ICMP_NE;
        else if (op == "<")  pred = isUnsigned ? llvm::CmpInst::ICMP_ULT : llvm::CmpInst::ICMP_SLT;
        else if (op == "<=") pred = isUnsigned ? llvm::CmpInst::ICMP_ULE : llvm::CmpInst::ICMP_SLE;
        else if (op == ">")  pred = isUnsigned ? llvm::CmpInst::ICMP_UGT : llvm::CmpInst::ICMP_SGT;
        else                 pred = isUnsigned ? llvm::CmpInst::ICMP_UGE : llvm::CmpInst::ICMP_SGE;
        return builder_.CreateICmp(pred, lhs, rhs, "icmp_ll");
    }

    lhs = promoteToInt(lhs);
    rhs = promoteToInt(rhs);

    bool lf = lhs->getType()->isDoubleTy();
    bool rf = rhs->getType()->isDoubleTy();
    if (lf || rf) {
        std::tie(lhs, rhs) = promoteToFloat(lhs, rhs);
        llvm::CmpInst::Predicate pred;
        if      (op == "==") pred = llvm::CmpInst::FCMP_OEQ;
        else if (op == "!=") pred = llvm::CmpInst::FCMP_UNE;
        else if (op == "<")  pred = llvm::CmpInst::FCMP_OLT;
        else if (op == "<=") pred = llvm::CmpInst::FCMP_OLE;
        else if (op == ">")  pred = llvm::CmpInst::FCMP_OGT;
        else                 pred = llvm::CmpInst::FCMP_OGE;
        return builder_.CreateFCmp(pred, lhs, rhs, "fcmp");
    }
    llvm::CmpInst::Predicate pred;
    if      (op == "==") pred = llvm::CmpInst::ICMP_EQ;
    else if (op == "!=") pred = llvm::CmpInst::ICMP_NE;
    else if (op == "<")  pred = llvm::CmpInst::ICMP_SLT;
    else if (op == "<=") pred = llvm::CmpInst::ICMP_SLE;
    else if (op == ">")  pred = llvm::CmpInst::ICMP_SGT;
    else                 pred = llvm::CmpInst::ICMP_SGE;
    return builder_.CreateICmp(pred, lhs, rhs, "icmp");
}

llvm::Value *CodeGen::emitRecordComparison(const std::string &op, llvm::Value *lhs,
                                            llvm::Value *rhs, const RecordInfo &info) {
    llvm::Value *result = llvm::ConstantInt::getTrue(*ctx_);
    for (unsigned i = 0; i < info.fields.size(); ++i) {
        llvm::Value *fieldL = builder_.CreateExtractValue(lhs, i, "l." + info.fields[i].name);
        llvm::Value *fieldR = builder_.CreateExtractValue(rhs, i, "r." + info.fields[i].name);
        llvm::Value *fieldEq = emitComparisonOp("==", fieldL, fieldR, "");
        result = builder_.CreateAnd(result, fieldEq, "and.eq");
    }
    if (op == "!=")
        return builder_.CreateNot(result, "record_ne");
    return result;
}

llvm::Value *CodeGen::emitBitwiseOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs,
                                     const std::string &lhsHint, const std::string &rhsHint) {
    const std::string &llNameHint = !lhsHint.empty() ? lhsHint : rhsHint;
    if (lhs->getType()->isDoubleTy() || rhs->getType()->isDoubleTy() ||
        isLowLevelFloatTy(lhs->getType()) || isLowLevelFloatTy(rhs->getType()))
        codegenError(
            "bitwise operator '" + op + "' requires integer operands, got float");
    // Reject str operands
    if (isStringValue(lhs) || isStringValue(rhs))
        codegenError("type error: bitwise operator '" + op + "' not supported for str type");
    checkLowLevelTypeMix(lhs, rhs, op, lhsHint, rhsHint);
    // Low-level integer bitwise at native width (#595)
    bool bwLowLevel = lhs->getType() == rhs->getType() &&
        ((isLowLevelIntTy(lhs) || isLowLevelIntTy(rhs)) ||
         (!lhsHint.empty() && lhsHint == rhsHint && isLowLevelTypeName(lhsHint) && lhsHint != "f32"));
    if (bwLowLevel) {
        std::string llName = getLowLevelTypeName(lhs);
        if (llName.empty()) llName = getLowLevelTypeName(rhs);
        if (llName.empty()) llName = llNameHint;
        auto propagate = [&](llvm::Value *result) -> llvm::Value* {
            if (!llName.empty()) getOrCreateMeta(result).low_level_type_name = llName;
            return result;
        };
        if (op == "&")  return propagate(builder_.CreateAnd(lhs, rhs,  "band_ll"));
        if (op == "|")  return propagate(builder_.CreateOr(lhs,  rhs,  "bor_ll"));
        if (op == "^")  return propagate(builder_.CreateXor(lhs, rhs,  "bxor_ll"));
        if (op == "<<") return propagate(builder_.CreateShl(lhs,  rhs, "shl_ll"));
        if (op == ">>") {
            if (isUnsignedLowLevel(lhs) || isUnsignedLowLevelName(llNameHint))
                return propagate(builder_.CreateLShr(lhs, rhs, "lshr_ll"));
            return propagate(builder_.CreateAShr(lhs, rhs, "ashr_ll"));
        }
        if (op == ">>>") return propagate(builder_.CreateLShr(lhs, rhs, "lshr_ll"));
        codegenError("unknown bitwise operator: " + op);
    }
    rejectBoolInOperator(lhs, op, "bitwise");
    rejectBoolInOperator(rhs, op, "bitwise");
    lhs = promoteToInt(lhs);
    rhs = promoteToInt(rhs);
    if (op == "&")  return builder_.CreateAnd(lhs, rhs,  "band");
    if (op == "|")  return builder_.CreateOr(lhs,  rhs,  "bor");
    if (op == "^")  return builder_.CreateXor(lhs, rhs,  "bxor");
    if (op == "<<") return builder_.CreateShl(lhs,  rhs, "shl");
    if (op == ">>") return builder_.CreateAShr(lhs, rhs, "ashr");
    if (op == ">>>") return builder_.CreateLShr(lhs, rhs, "lshr");
    codegenError("unknown bitwise operator: " + op);
}

llvm::Value *CodeGen::emitArithmeticOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs,
                                        const std::string &lhsHint, const std::string &rhsHint) {
    const std::string &llNameHint = !lhsHint.empty() ? lhsHint : rhsHint;
    // Low-level type mix check (must come first)
    checkLowLevelTypeMix(lhs, rhs, op, lhsHint, rhsHint);

    // Low-level type native-width arithmetic (#595)
    bool arithLowLevel = lhs->getType() == rhs->getType() &&
        ((isLowLevelTy(lhs) || isLowLevelTy(rhs)) ||
         (!lhsHint.empty() && lhsHint == rhsHint && isLowLevelTypeName(lhsHint)));
    if (arithLowLevel) {
        llvm::Type *ty = lhs->getType();
        if (op == "**")
            codegenError("operator '**' is not supported for low-level numeric types");
        // Propagate low-level type metadata to result
        std::string llName = getLowLevelTypeName(lhs);
        if (llName.empty()) llName = getLowLevelTypeName(rhs);
        if (llName.empty()) llName = llNameHint;
        auto propagate = [&](llvm::Value *result) -> llvm::Value* {
            if (!llName.empty()) getOrCreateMeta(result).low_level_type_name = llName;
            return result;
        };
        if (isLowLevelFloatTy(ty)) {
            if (op == "//")
                codegenError("operator '//' is not supported for f32");
            if (op == "%")  return propagate(builder_.CreateFRem(lhs, rhs, "frem32"));
            if (op == "/")  return propagate(builder_.CreateFDiv(lhs, rhs, "fdiv32"));
            if (op == "+")  return propagate(builder_.CreateFAdd(lhs, rhs, "fadd32"));
            if (op == "-")  return propagate(builder_.CreateFSub(lhs, rhs, "fsub32"));
            if (op == "*")  return propagate(builder_.CreateFMul(lhs, rhs, "fmul32"));
            codegenError("unknown operator: " + op);
        }
        // Low-level integer
        bool isUnsigned = isUnsignedLowLevel(lhs) || isUnsignedLowLevelName(llNameHint);
        if (op == "/" || op == "//") {
            emitIntZeroDivGuard(rhs, "div_ll", "runtime error: division by zero\n");
            if (isUnsigned) return propagate(builder_.CreateUDiv(lhs, rhs, "udiv_ll"));
            return propagate(builder_.CreateSDiv(lhs, rhs, "sdiv_ll"));
        }
        if (op == "%") {
            emitIntZeroDivGuard(rhs, "mod_ll", "runtime error: modulo by zero\n");
            if (isUnsigned) return propagate(builder_.CreateURem(lhs, rhs, "urem_ll"));
            return propagate(builder_.CreateSRem(lhs, rhs, "srem_ll"));
        }
        if (op == "+")  return propagate(builder_.CreateAdd(lhs, rhs, "add_ll"));
        if (op == "-")  return propagate(builder_.CreateSub(lhs, rhs, "sub_ll"));
        if (op == "*")  return propagate(builder_.CreateMul(lhs, rhs, "mul_ll"));
        codegenError("unknown operator: " + op);
    }

    // ** 累乗: 常にf64、libmのpow()を呼ぶ
    if (op == "**") {
        ensureNumericType(lhs, "operator '**'");
        ensureNumericType(rhs, "operator '**'");
        rejectBoolInOperator(lhs, "**", "arithmetic");
        rejectBoolInOperator(rhs, "**", "arithmetic");
        if (lhs->getType() == i8Ty_)
            lhs = builder_.CreateUIToFP(lhs, f64Ty_, "lhs_f");
        else if (lhs->getType()->isIntegerTy())
            lhs = builder_.CreateSIToFP(lhs, f64Ty_, "lhs_f");
        if (rhs->getType() == i8Ty_)
            rhs = builder_.CreateUIToFP(rhs, f64Ty_, "rhs_f");
        else if (rhs->getType()->isIntegerTy())
            rhs = builder_.CreateSIToFP(rhs, f64Ty_, "rhs_f");
        llvm::FunctionType *powTy = llvm::FunctionType::get(f64Ty_, {f64Ty_, f64Ty_}, false);
        llvm::FunctionCallee powFn = mod_->getOrInsertFunction("pow", powTy);
        return builder_.CreateCall(powFn, {lhs, rhs}, "pow");
    }

    // Auto-convert non-str to str for + operator (#393)
    bool lhsIsStr = isStringValue(lhs);
    bool rhsIsStr = isStringValue(rhs);
    auto isScalarTy = [&](llvm::Value *v) {
        return v->getType()->isIntegerTy() || v->getType()->isDoubleTy();
    };
    if (op == "+" && lhsIsStr && isScalarTy(rhs)) {
        rhs = valueToString(rhs);
        rhsIsStr = true;
    } else if (op == "+" && rhsIsStr && isScalarTy(lhs)) {
        lhs = valueToString(lhs);
        lhsIsStr = true;
    }

    // String concatenation — NUL-safe via StringHeader (byte_len + makeStringUninit + memcpy)
    if (op == "+" && lhsIsStr && rhsIsStr) {
        llvm::Value *lenL = emitStringByteLen(lhs);
        llvm::Value *lenR = emitStringByteLen(rhs);
        llvm::Value *total = builder_.CreateAdd(lenL, lenR, "concat_len");

        // Overflow guard: if lenL + lenR wraps (total < lenL for non-negative inputs),
        // abort before underallocating the buffer.
        llvm::Value *catOverflow = builder_.CreateICmpSLT(total, lenL, "cat_ovf");
        llvm::BasicBlock *catErrBB   = llvm::BasicBlock::Create(*ctx_, "str_cat.ovf_err",  fn_);
        llvm::BasicBlock *catAllocBB = llvm::BasicBlock::Create(*ctx_, "str_cat.alloc",    fn_);
        builder_.CreateCondBr(catOverflow, catErrBB, catAllocBB);

        builder_.SetInsertPoint(catErrBB);
        emitRuntimeError("runtime error: str + str overflows\n", ".str_cat_overflow");
        // emitRuntimeError ends with CreateUnreachable(); no fall-through.

        builder_.SetInsertPoint(catAllocBB);
        auto makeUninitTy = llvm::FunctionType::get(ptrTy_, {i64Ty_}, false);
        auto makeUninitFn = mod_->getOrInsertFunction("__ry_string_make_uninit", makeUninitTy);
        llvm::Value *buf = builder_.CreateCall(makeUninitFn, {total}, "concat_buf");
        builder_.CreateCall(getStdlibMemcpy(), {buf, lhs, lenL});
        llvm::Value *dst = builder_.CreateGEP(i8Ty_, buf, lenL, "concat_dst");
        builder_.CreateCall(getStdlibMemcpy(), {dst, rhs, lenR});
        arc_str_owned_values_.insert(buf);
        return buf;
    }

    // String repetition: "ab" * 3 or 3 * "ab"
    if (op == "*") {
        llvm::Value *strVal = nullptr;
        llvm::Value *intVal = nullptr;
        if (lhsIsStr && rhs->getType()->isIntegerTy()) {
            strVal = lhs; intVal = rhs;
        } else if (rhsIsStr && lhs->getType()->isIntegerTy()) {
            strVal = rhs; intVal = lhs;
        }
        if (strVal) {
            if (intVal->getType() == i1Ty_ || intVal->getType() == i8Ty_)
                intVal = builder_.CreateZExt(intVal, i64Ty_, "n_ext");
            return emitStringRepeat(strVal, intVal);
        }
    }

    // List concatenation: [1,2] + [3,4] → [1,2,3,4]
    if (op == "+") {
        llvm::Type *lhsElemTy = getListElementType(lhs);
        llvm::Type *rhsElemTy = getListElementType(rhs);
        if (lhsElemTy && rhsElemTy) {
            if (lhsElemTy != rhsElemTy)
                codegenError("type error: list concatenation requires matching element types");
            return emitListConcat(lhs, rhs, lhsElemTy);
        }
    }

    // Map/Set collection operands for '+': merge, union, or named reject.
    // Must precede the str-vs-non-str reject below. (#863, #866)
    if (op == "+") {
        llvm::Type *lhsKeyTy  = getMapKeyType(lhs);
        llvm::Type *rhsKeyTy  = getMapKeyType(rhs);
        llvm::Type *lhsElemTy = getSetElementType(lhs);
        llvm::Type *rhsElemTy = getSetElementType(rhs);

        // Map + Map merge (rhs-wins on key collision)
        if (lhsKeyTy && rhsKeyTy) {
            llvm::Type *lhsValTy = getMapValueType(lhs);
            llvm::Type *rhsValTy = getMapValueType(rhs);
            if (!lhsValTy || !rhsValTy || lhsKeyTy != rhsKeyTy || lhsValTy != rhsValTy)
                codegenError("type error: map merge requires matching key/value types");
            return emitMapMergeCore(lhs, rhs, lhsKeyTy, lhsValTy);
        }

        // Set + Set union
        if (lhsElemTy && rhsElemTy) {
            if (lhsElemTy != rhsElemTy)
                codegenError("type error: set union requires matching element types");
            return emitSetUnionCore(lhs, rhs, lhsElemTy);
        }

        // Mixed / one-sided Map or Set: emit a named diagnostic before the
        // generic str-vs-non-str path misclassifies the pointer operand.
        bool lhsIsMapOrSet = lhsKeyTy || getMapValueType(lhs) || lhsElemTy;
        bool rhsIsMapOrSet = rhsKeyTy || getMapValueType(rhs) || rhsElemTy;
        if (lhsIsMapOrSet || rhsIsMapOrSet) {
            std::string lhsName = inferCollectionTypeName(lhs);
            std::string rhsName = inferCollectionTypeName(rhs);
            if (lhsName.empty()) lhsName = "non-collection";
            if (rhsName.empty()) rhsName = "non-collection";
            codegenError("type error: operator '+' is not defined for " +
                         lhsName + " and " + rhsName);
        }
    }

    // Reject str with non-str operands (must come after string concat/repeat checks)
    if (lhsIsStr || rhsIsStr)
        codegenError("type error: operator '" + op + "' not supported between str and non-str types");

    // // floor division (toward -∞)
    if (op == "//") {
        rejectBoolInOperator(lhs, "//", "arithmetic");
        rejectBoolInOperator(rhs, "//", "arithmetic");
        lhs = promoteToInt(lhs);
        rhs = promoteToInt(rhs);
        bool lf = lhs->getType()->isDoubleTy();
        bool rf = rhs->getType()->isDoubleTy();
        if (lf || rf) {
            // float: floor(fdiv)
            std::tie(lhs, rhs) = promoteToFloat(lhs, rhs);
            llvm::Value *div = builder_.CreateFDiv(lhs, rhs, "fdiv");
            llvm::Function *floorFn = llvm::Intrinsic::getOrInsertDeclaration(
                mod_.get(), llvm::Intrinsic::floor, {f64Ty_});
            return builder_.CreateCall(floorFn, {div}, "floordiv");
        }
        // int: zero-division guard
        emitIntZeroDivGuard(rhs, "floordiv", "runtime error: division by zero\n");
        // int: sdiv + floor adjustment
        llvm::Value *q   = builder_.CreateSDiv(lhs, rhs, "q");
        llvm::Value *rem  = builder_.CreateSRem(lhs, rhs, "rem");
        llvm::Value *xorV = builder_.CreateXor(lhs, rhs, "xor");
        llvm::Value *signsDiffer = builder_.CreateICmpSLT(
            xorV, llvm::ConstantInt::get(i64Ty_, 0), "signs_differ");
        llvm::Value *hasRem = builder_.CreateICmpNE(
            rem, llvm::ConstantInt::get(i64Ty_, 0), "has_rem");
        llvm::Value *needsAdj = builder_.CreateAnd(signsDiffer, hasRem, "needs_adj");
        llvm::Value *adjusted = builder_.CreateSub(
            q, llvm::ConstantInt::get(i64Ty_, 1), "adjusted");
        return builder_.CreateSelect(needsAdj, adjusted, q, "floordiv");
    }

    // / 除算: 常にf64, IEEE 754 semantics (x/0 → ±inf, 0/0 → nan) (#1023)
    if (op == "/") {
        rejectBoolInOperator(lhs, "/", "arithmetic");
        rejectBoolInOperator(rhs, "/", "arithmetic");
        std::tie(lhs, rhs) = promoteToFloat(lhs, rhs);
        return builder_.CreateFDiv(lhs, rhs, "div");
    }

    // % 剰余: 片方f64ならfrem、両方i64ならsrem
    if (op == "%") {
        rejectBoolInOperator(lhs, "%", "arithmetic");
        rejectBoolInOperator(rhs, "%", "arithmetic");
        lhs = promoteToInt(lhs);
        rhs = promoteToInt(rhs);
        bool lf = lhs->getType()->isDoubleTy();
        bool rf = rhs->getType()->isDoubleTy();
        if (lf || rf) {
            std::tie(lhs, rhs) = promoteToFloat(lhs, rhs);
            // Floor modulo for float: r = frem(a,b); if (r != 0 && sign(r) != sign(b)) r += b
            llvm::Value *frem = builder_.CreateFRem(lhs, rhs, "frem");
            llvm::Value *zero = llvm::ConstantFP::get(f64Ty_, 0.0);
            llvm::Value *remNonZero = builder_.CreateFCmpONE(frem, zero, "frem_nz");
            // XOR the sign bits: if frem and rhs have different signs, adjust
            llvm::Value *fremNeg = builder_.CreateFCmpOLT(frem, zero, "frem_neg");
            llvm::Value *rhsNeg  = builder_.CreateFCmpOLT(rhs, zero, "rhs_neg");
            llvm::Value *signsDiffer = builder_.CreateXor(fremNeg, rhsNeg, "fsigns_differ");
            llvm::Value *needsAdj = builder_.CreateAnd(remNonZero, signsDiffer, "fmod_adj");
            llvm::Value *adjusted = builder_.CreateFAdd(frem, rhs, "fmod_adjusted");
            return builder_.CreateSelect(needsAdj, adjusted, frem, "ffloormod");
        }
        // int: zero-division guard
        emitIntZeroDivGuard(rhs, "mod", "runtime error: modulo by zero\n");
        // Floor modulo: r = srem(a,b); if (r != 0 && sign(r) != sign(b)) r += b
        llvm::Value *rem = builder_.CreateSRem(lhs, rhs, "srem");
        llvm::Value *remNonZero = builder_.CreateICmpNE(
            rem, llvm::ConstantInt::get(i64Ty_, 0), "rem_nz");
        llvm::Value *xorV = builder_.CreateXor(rem, rhs, "rem_xor_rhs");
        llvm::Value *signsDiffer = builder_.CreateICmpSLT(
            xorV, llvm::ConstantInt::get(i64Ty_, 0), "signs_differ");
        llvm::Value *needsAdj = builder_.CreateAnd(remNonZero, signsDiffer, "mod_adj");
        llvm::Value *adjusted = builder_.CreateAdd(rem, rhs, "mod_adjusted");
        return builder_.CreateSelect(needsAdj, adjusted, rem, "floormod");
    }

    // +/-/*: 片方f64なら浮動小数点命令
    rejectBoolInOperator(lhs, op, "arithmetic");
    rejectBoolInOperator(rhs, op, "arithmetic");
    lhs = promoteToInt(lhs);
    rhs = promoteToInt(rhs);
    bool lf = lhs->getType()->isDoubleTy();
    bool rf = rhs->getType()->isDoubleTy();
    if (lf || rf) {
        std::tie(lhs, rhs) = promoteToFloat(lhs, rhs);
        if (op == "+") return builder_.CreateFAdd(lhs, rhs, "fadd");
        if (op == "-") return builder_.CreateFSub(lhs, rhs, "fsub");
        if (op == "*") return builder_.CreateFMul(lhs, rhs, "fmul");
        codegenError("unknown operator: " + op);
    }
    // Overflow check only for high-level int; low-level i64/u64 wraps
    if (llNameHint.empty()) {
        if (op == "+") return emitIntOverflowCheck(llvm::Intrinsic::sadd_with_overflow, lhs, rhs, "add");
        if (op == "-") return emitIntOverflowCheck(llvm::Intrinsic::ssub_with_overflow, lhs, rhs, "sub");
        if (op == "*") return emitIntOverflowCheck(llvm::Intrinsic::smul_with_overflow, lhs, rhs, "mul");
    }
    // Low-level i64/u64 fallthrough: wrap and propagate metadata
    // Note: This may tag ConstantInt values (#311), but the risk is limited because
    // propagateHint only fires when an explicit i64/u64 suffix is in the AST.
    // A proper fix (recursive getExprLowLevelSuffix) is tracked in #595.
    auto propagateHint = [&](llvm::Value *result) -> llvm::Value* {
        if (!llNameHint.empty()) getOrCreateMeta(result).low_level_type_name = llNameHint;
        return result;
    };
    if (op == "+") return propagateHint(builder_.CreateAdd(lhs, rhs, "add"));
    if (op == "-") return propagateHint(builder_.CreateSub(lhs, rhs, "sub"));
    if (op == "*") return propagateHint(builder_.CreateMul(lhs, rhs, "mul"));
    codegenError("unknown operator: " + op);
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<BinaryExpr> &e) {
    // Handle 'in' / 'not in' operator: lhs in rhs (set, list, or map)
    if (e->op == "in" || e->op == "not in") {
        llvm::Value *elem = emitExpr(*e->lhs);
        llvm::Value *container = emitExpr(*e->rhs);

        if (auto *result = tryOperatorCall("operatorin", elem, container)) {
            if (e->op == "not in")
                result = builder_.CreateNot(result, "user_not_in");
            return result;
        }

        // Try set
        llvm::Type *setElemTy = getSetElementType(container);
        if (setElemTy) {
            if (elem->getType() != setElemTy) {
                if (isAnyType(setElemTy))
                    elem = wrapInAny(elem);
                else if (isAnyType(elem->getType()) && canAnyHoldType(setElemTy))
                    elem = unwrapFromAny(elem, setElemTy);
                else
                    codegenError("'" + e->op + "' operator: element type mismatch");
            }
            std::string inElemName = getSetElemName(container);
            validateSetElemType(inElemName, elem, "'" + e->op + "' operator");
            llvm::Value *idx = emitSetElementLookup(container, elem, setElemTy, inElemName);
            llvm::Value *result = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "set_in");
            if (e->op == "not in")
                result = builder_.CreateNot(result, "set_not_in");
            return result;
        }

        // Try map (key lookup)
        llvm::Type *mapKeyTy = getMapKeyType(container);
        if (mapKeyTy) {
            if (elem->getType() != mapKeyTy) {
                if (isAnyType(mapKeyTy))
                    elem = wrapInAny(elem);
                else if (isAnyType(elem->getType()) && canAnyHoldType(mapKeyTy))
                    elem = unwrapFromAny(elem, mapKeyTy);
                else
                    codegenError("'" + e->op + "' operator: key type mismatch");
            }
            std::string inKeyName;
            if (const ValueMetadata *meta = getMeta(container))
                inKeyName = meta->map_key_type_name;
            llvm::Value *idx = emitMapKeyLookup(container, elem, mapKeyTy, inKeyName);
            llvm::Value *result = builder_.CreateICmpSGE(idx, llvm::ConstantInt::get(i64Ty_, 0), "map_in");
            if (e->op == "not in")
                result = builder_.CreateNot(result, "map_not_in");
            return result;
        }

        // Try list (linear search)
        llvm::Type *listElemTy = getListElementType(container);
        if (listElemTy) {
            if (elem->getType() != listElemTy) {
                if (isAnyType(listElemTy))
                    elem = wrapInAny(elem);
                else if (isAnyType(elem->getType()) && canAnyHoldType(listElemTy))
                    elem = unwrapFromAny(elem, listElemTy);
                else
                    codegenError("'" + e->op + "' operator: element type mismatch");
            }

            // For List<any>, hoist scratch allocas and __ry_any_eq outside the loop.
            const bool listElemIsAny = isAnyType(listElemTy);
            llvm::AllocaInst *anyElemPtr = nullptr;
            llvm::AllocaInst *anyCandPtr = nullptr;
            llvm::FunctionCallee anyEqFn;
            if (listElemIsAny) {
                anyElemPtr = builder_.CreateAlloca(anyTy_, nullptr, "in.any.elem");
                builder_.CreateStore(elem, anyElemPtr);
                anyCandPtr = builder_.CreateAlloca(anyTy_, nullptr, "in.any.cand");
                llvm::FunctionType *fnTy = llvm::FunctionType::get(i64Ty_, {ptrTy_, ptrTy_}, false);
                anyEqFn = mod_->getOrInsertFunction("__ry_any_eq", fnTy);
            }

            // Linear search loop
            llvm::Value *lenPtr = builder_.CreateStructGEP(listHeaderTy_, container, 0, "in_len_ptr");
            llvm::Value *length = builder_.CreateLoad(i64Ty_, lenPtr, "in_length");
            llvm::Value *dataPtrField = builder_.CreateStructGEP(listHeaderTy_, container, 2, "in_data_ptr");
            llvm::Value *dataPtr = builder_.CreateLoad(ptrTy_, dataPtrField, "in_data");

            llvm::AllocaInst *foundVar = builder_.CreateAlloca(i1Ty_, nullptr, "in_found");
            builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 0), foundVar);
            llvm::AllocaInst *iVar = builder_.CreateAlloca(i64Ty_, nullptr, "in_i");
            builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, 0), iVar);

            llvm::BasicBlock *condBB = llvm::BasicBlock::Create(*ctx_, "in.cond", fn_);
            llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(*ctx_, "in.body", fn_);
            llvm::BasicBlock *endBB = llvm::BasicBlock::Create(*ctx_, "in.end", fn_);

            builder_.CreateBr(condBB);
            builder_.SetInsertPoint(condBB);
            llvm::Value *iVal = builder_.CreateLoad(i64Ty_, iVar, "in_iv");
            llvm::Value *cond = builder_.CreateICmpSLT(iVal, length, "in_cond");
            builder_.CreateCondBr(cond, bodyBB, endBB);

            builder_.SetInsertPoint(bodyBB);
            llvm::Value *iCur = builder_.CreateLoad(i64Ty_, iVar, "in_ic");
            llvm::Value *elemPtr = builder_.CreateGEP(listElemTy, dataPtr, {iCur}, "in_elem_ptr");
            llvm::Value *listElem = builder_.CreateLoad(listElemTy, elemPtr, "in_elem");

            llvm::Value *match;
            if (listElemIsAny) {
                builder_.CreateStore(listElem, anyCandPtr);
                llvm::Value *r = builder_.CreateCall(anyEqFn, {anyElemPtr, anyCandPtr}, "in.any.eq");
                match = builder_.CreateICmpNE(r, builder_.getInt64(0), "in_match");
            } else if (listElemTy == ptrTy_) {
                // Reject non-str pointer elements: the comparison below calls strcmp, which is
                // UB on Map/Set/List/closure/resource headers. Positive allowlist on
                // list_elem_type_name (empty or "str" counts as str) with structural fallbacks
                // for NestedListElem / list_elem_fn_type_info in case the name is unset.
                const ValueMetadata *meta = getMeta(container);
                const std::string &elemName = meta ? meta->list_elem_type_name : std::string{};
                const bool isNonStrName = !elemName.empty() && elemName != "str";
                const bool hasNestedList = meta && meta->nested_list_elem != nullptr;
                const bool hasFnInfo = meta && meta->list_elem_fn_type_info.has_value();
                if (isNonStrName || hasNestedList || hasFnInfo)
                    codegenError("'" + e->op + "' operator is only supported for lists of primitive values or strings");
                // String comparison
                auto strcmpFn = getStdlibStrcmp();
                llvm::Value *cmpResult = builder_.CreateCall(strcmpFn, {elem, listElem}, "in_strcmp");
                match = builder_.CreateICmpEQ(cmpResult, llvm::ConstantInt::get(i32Ty_, 0), "in_match");
            } else if (listElemTy->isDoubleTy()) {
                match = builder_.CreateFCmpOEQ(elem, listElem, "in_match");
            } else {
                match = builder_.CreateICmpEQ(elem, listElem, "in_match");
            }

            llvm::BasicBlock *foundBB = llvm::BasicBlock::Create(*ctx_, "in.found", fn_);
            llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "in.next", fn_);
            builder_.CreateCondBr(match, foundBB, nextBB);

            builder_.SetInsertPoint(foundBB);
            builder_.CreateStore(llvm::ConstantInt::get(i1Ty_, 1), foundVar);
            builder_.CreateBr(endBB);

            builder_.SetInsertPoint(nextBB);
            llvm::Value *iNext = builder_.CreateAdd(iCur, llvm::ConstantInt::get(i64Ty_, 1), "in_next");
            builder_.CreateStore(iNext, iVar);
            builder_.CreateBr(condBB);

            builder_.SetInsertPoint(endBB);
            llvm::Value *result = builder_.CreateLoad(i1Ty_, foundVar, "in_result");
            if (e->op == "not in")
                result = builder_.CreateNot(result, "list_not_in");
            return result;
        }

        // Try str (substring check) — #1032
        if (isStringValue(container)) {
            if (!isStringValue(elem)) {
                // Safety: wrapInAny() rejects non-str pointers (List/Map/Set) at
                // compile time, so any<ptrTy_> can only carry a str handle.
                // unwrapFromAny also performs a runtime RyAnyTag::Str check and
                // aborts on mismatch, providing a second layer of defense.
                if (isAnyType(elem->getType()) && canAnyHoldType(ptrTy_))
                    elem = unwrapFromAny(elem, ptrTy_);
                else
                    codegenError("'" + e->op + "' operator: left side must be str when right side is str");
            }
            llvm::Value *hlen = emitStringByteLen(container);
            llvm::Value *nlen = emitStringByteLen(elem);
            auto findByteFn = getRuntimeFn("__ry_str_find_byte", i64Ty_,
                                           {ptrTy_, i64Ty_, ptrTy_, i64Ty_, i32Ty_});
            llvm::Value *byteOff = builder_.CreateCall(findByteFn,
                {container, hlen, elem, nlen, llvm::ConstantInt::get(i32Ty_, 0)}, "str_in_find");
            llvm::Value *result = builder_.CreateICmpNE(byteOff,
                llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(-1LL)), "str_in");
            if (e->op == "not in")
                result = builder_.CreateNot(result, "str_not_in");
            return result;
        }

        codegenError("'" + e->op + "' operator requires a set, list, map, or str on the right side");
    }

    // Short-circuit evaluation for 'and' / 'or'
    if (e->op == "and" || e->op == "or") {
        llvm::Value *lhs = emitExpr(*e->lhs);
        llvm::Value *lhsBool = toBool(lhs);

        llvm::BasicBlock *rhsBB = llvm::BasicBlock::Create(*ctx_, "sc.rhs", fn_);
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "sc.merge", fn_);
        llvm::BasicBlock *lhsBB = builder_.GetInsertBlock();

        if (e->op == "and")
            builder_.CreateCondBr(lhsBool, rhsBB, mergeBB);
        else
            builder_.CreateCondBr(lhsBool, mergeBB, rhsBB);

        builder_.SetInsertPoint(rhsBB);
        llvm::Value *rhs = emitExpr(*e->rhs);
        llvm::Value *rhsBool = toBool(rhs);
        llvm::BasicBlock *rhsEndBB = builder_.GetInsertBlock();
        builder_.CreateBr(mergeBB);

        builder_.SetInsertPoint(mergeBB);
        llvm::PHINode *phi = builder_.CreatePHI(i1Ty_, 2, e->op);
        phi->addIncoming(lhsBool, lhsBB);
        phi->addIncoming(rhsBool, rhsEndBB);
        return phi;
    }

    // Null coalescing operator: lhs ?? rhs
    // Accepts Option<T> or Result<T, E> on the left side. In either case the
    // struct's tag lives at index 0 (Some=1 / Ok=1, None=0 / Err=0) and the
    // happy-path value lives at index 1. This lets us share one CreateSelect
    // for both cases.
    if (e->op == "??") {
        llvm::Value *lhs = emitExpr(*e->lhs);
        llvm::Type *lhsTy = lhs->getType();
        bool lhsIsOption = isOptionType(lhsTy);
        bool lhsIsResult = isResultType(lhsTy);
        if (!lhsIsOption && !lhsIsResult)
            codegenError("'" "??" "' operator requires Option or Result type on the left side");
        llvm::Value *tag = builder_.CreateExtractValue(
            lhs, {0}, lhsIsOption ? "has_val" : "is_ok");
        llvm::Value *happyVal = builder_.CreateExtractValue(
            lhs, {1}, lhsIsOption ? "inner_val" : "ok_val");
        // Carry metadata (list_elem_type_name, map_key_type_name, etc.) from
        // the Option/Result wrapper down to the extracted inner value so the
        // downstream type check can tell e.g. `List<int>` from `str` — both
        // are backed by `ptrTy_`, so raw LLVM-type equality is not enough.
        propagateMeta(lhs, happyVal);
        llvm::Value *rhs = emitExpr(*e->rhs);
        validateBranchTypes(happyVal, rhs,
            lhsIsOption ? "'" "??" "' on Option"
                        : "'" "??" "' on Result");
        return builder_.CreateSelect(tag, happyVal, rhs, "coalesce");
    }

    llvm::Value *lhs = emitExpr(*e->lhs);
    llvm::Value *rhs = emitExpr(*e->rhs);
    const std::string &op = e->op;

    // Per-operand low-level type hints from AST suffixes (#311, #595).
    std::string lhsHint = getExprLowLevelSuffix(*e->lhs);
    std::string rhsHint = getExprLowLevelSuffix(*e->rhs);

    return emitBinaryOp(op, lhs, rhs, lhsHint, rhsHint);
}

llvm::Value *CodeGen::emitBinaryOp(const std::string &op, llvm::Value *lhs, llvm::Value *rhs,
                                    const std::string &lhsHint, const std::string &rhsHint) {
    // Try user-defined binary operator first
    std::string opFnName = "operator" + op;
    if (auto *result = tryOperatorCall(opFnName, lhs, rhs))
        return result;

    // any-type dynamic dispatch (#223)
    if (isAnyType(lhs->getType()) || isAnyType(rhs->getType())) {
        if (!isAnyType(lhs->getType())) lhs = wrapInAny(lhs);
        if (!isAnyType(rhs->getType())) rhs = wrapInAny(rhs);
        return emitAnyBinaryOp(op, lhs, rhs);
    }

    if (op == "==" || op == "!=" || op == "<" ||
        op == "<=" || op == ">"  || op == ">=")
        return emitComparisonOp(op, lhs, rhs, lhsHint, rhsHint);

    if (op == "&" || op == "|" || op == "^" ||
        op == "<<" || op == ">>" || op == ">>>")
        return emitBitwiseOp(op, lhs, rhs, lhsHint, rhsHint);

    return emitArithmeticOp(op, lhs, rhs, lhsHint, rhsHint);
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<CaseCondExpr> &e) {
    // Pre-scan: scan all arm values against else_expr to find a concrete inner
    // type for None() hint. Stop at the first non-nullptr result (#1154).
    // Guard is re-installed per value emit to avoid leaking into conditions.
    llvm::Type *caseCondHint = nullptr;
    for (const auto &arm : e->arms) {
        caseCondHint = computeBranchOptionInnerHint(*arm.value, *e->else_expr);
        if (caseCondHint) break;
    }
    llvm::Type *caseCondFallback = caseCondHint ? caseCondHint : option_decl_annotation_inner_;

    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "case.expr.merge", fn_);
    std::vector<std::pair<llvm::Value*, llvm::BasicBlock*>> incoming;

    llvm::Value *firstVal = nullptr;

    for (size_t i = 0; i < e->arms.size(); ++i) {
        auto &arm = e->arms[i];
        llvm::Value *cond = emitExpr(*arm.condition);
        cond = toBool(cond);

        llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(*ctx_, "case.expr.then", fn_);
        llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "case.expr.next", fn_);
        builder_.CreateCondBr(cond, thenBB, nextBB);

        builder_.SetInsertPoint(thenBB);
        llvm::Value *armVal;
        {
            OptionNoneHintGuard g(*this, caseCondFallback);
            armVal = emitExpr(*arm.value);
        }
        if (!firstVal) firstVal = armVal;
        else validateBranchTypes(firstVal, armVal, "case expression");
        llvm::BasicBlock *armEndBB = builder_.GetInsertBlock();
        builder_.CreateBr(mergeBB);
        incoming.push_back({armVal, armEndBB});

        builder_.SetInsertPoint(nextBB);
    }

    llvm::Value *elseVal;
    {
        OptionNoneHintGuard g(*this, caseCondFallback);
        elseVal = emitExpr(*e->else_expr);
    }
    if (!firstVal) firstVal = elseVal;
    else validateBranchTypes(firstVal, elseVal, "case expression");
    llvm::BasicBlock *elseEndBB = builder_.GetInsertBlock();
    builder_.CreateBr(mergeBB);
    incoming.push_back({elseVal, elseEndBB});

    builder_.SetInsertPoint(mergeBB);
    llvm::PHINode *phi = builder_.CreatePHI(firstVal->getType(), static_cast<unsigned>(incoming.size()), "case.expr");
    for (auto &[val, bb] : incoming)
        phi->addIncoming(val, bb);
    propagateMeta(firstVal, phi);
    return phi;
}

// ===== IfExpr (single-expression form: `if cond => then_val else else_val`) =====

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<IfExpr> &e) {
    // Pre-scan: compute None() hint before any emit so the fallback is stable
    // (#1154). The guard is installed *after* the condition is emitted so that
    // none/None() in the condition expression is not affected by arm context.
    llvm::Type *ifExprHint = computeBranchOptionInnerHint(*e->then_value, *e->else_value);
    llvm::Type *ifExprFallback = ifExprHint ? ifExprHint : option_decl_annotation_inner_;

    llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(*ctx_, "if.expr.then", fn_);
    llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(*ctx_, "if.expr.else", fn_);
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "if.expr.merge", fn_);

    llvm::Value *cond = toBool(emitExpr(*e->condition));
    // Install the arm hint only after the condition has been emitted.
    OptionNoneHintGuard ifExprGuard(*this, ifExprFallback);
    builder_.CreateCondBr(cond, thenBB, elseBB);

    builder_.SetInsertPoint(thenBB);
    llvm::Value *thenVal = emitExpr(*e->then_value);
    llvm::BasicBlock *thenEndBB = builder_.GetInsertBlock();
    builder_.CreateBr(mergeBB);

    builder_.SetInsertPoint(elseBB);
    llvm::Value *elseVal = emitExpr(*e->else_value);
    llvm::BasicBlock *elseEndBB = builder_.GetInsertBlock();
    validateBranchTypes(thenVal, elseVal, "if expression");
    builder_.CreateBr(mergeBB);

    builder_.SetInsertPoint(mergeBB);
    llvm::PHINode *phi = builder_.CreatePHI(thenVal->getType(), 2, "if.expr");
    phi->addIncoming(thenVal, thenEndBB);
    phi->addIncoming(elseVal, elseEndBB);
    propagateMeta(thenVal, phi);
    return phi;
}

// ===== IfBlockExpr (block form: `if cond: body else: body`) =====
//
// Both blocks must end with an ExprStmt (tail-expression semantics). This is
// the minimal block-valued expression mechanism introduced for the `if`
// expression form — it is not (yet) a general language feature.
llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<IfBlockExpr> &e) {
    auto extractTailExpr = [this](const std::vector<StmtNode> &body, const char *branch)
        -> const ExprNode * {
        if (body.empty())
            codegenError("if expression (block form) " + std::string(branch) +
                         " branch body cannot be empty");
        const auto *exprStmt = std::get_if<ExprStmt>(&body.back());
        if (!exprStmt || !exprStmt->expr)
            codegenError("if expression (block form) " + std::string(branch) +
                         " branch must end with an expression");
        return exprStmt->expr.get();
    };

    const ExprNode *thenTail = extractTailExpr(e->then_body, "then");
    const ExprNode *elseTail = extractTailExpr(e->else_body, "else");

    // Pre-scan: compute hint before condition emit; guard installed after.
    llvm::Type *ifBlockHint = computeBranchOptionInnerHint(*thenTail, *elseTail);
    llvm::Type *ifBlockFallback = ifBlockHint ? ifBlockHint : option_decl_annotation_inner_;

    // Emit one branch body into an existing basic block: execute the
    // non-tail statements, then evaluate the tail expression. Returns
    // {tailValue, endBasicBlock} so the caller can wire up the phi.
    //
    // `const_cast` note: `emitStmt` takes non-const references because
    // statement codegen mutates codegen state (allocas, ARC bookkeeping).
    // `IfBlockExpr` is reached via the `const` expression visitor, so
    // `body` arrives as a const vector. The AST itself is not mutated —
    // only codegen side state. See KNOWLEDGE.md "IfBlockExpr const_cast"
    // for the full architectural rationale.
    // Emit one branch body: non-tail stmts, then tail with hint active only
    // during the tail expression so non-tail stmts are not affected (#1154).
    auto emitBodyTail = [this](llvm::BasicBlock *entry, const std::vector<StmtNode> &body,
                               const ExprNode *tail, llvm::Type *tailHint)
        -> std::pair<llvm::Value *, llvm::BasicBlock *> {
        builder_.SetInsertPoint(entry);
        pushScope();
        auto &mutBody = const_cast<std::vector<StmtNode> &>(body);
        for (size_t i = 0; i + 1 < mutBody.size(); ++i)
            std::visit([this](auto &s) { emitStmt(s); }, mutBody[i]);
        OptionNoneHintGuard g(*this, tailHint);
        llvm::Value *val = emitExpr(*tail);
        popScope();
        return {val, builder_.GetInsertBlock()};
    };

    llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(*ctx_, "if.block.then", fn_);
    llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(*ctx_, "if.block.else", fn_);
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "if.block.merge", fn_);

    llvm::Value *cond = toBool(emitExpr(*e->condition));
    builder_.CreateCondBr(cond, thenBB, elseBB);

    auto [thenVal, thenEndBB] = emitBodyTail(thenBB, e->then_body, thenTail, ifBlockFallback);
    builder_.CreateBr(mergeBB);

    auto [elseVal, elseEndBB] = emitBodyTail(elseBB, e->else_body, elseTail, ifBlockFallback);
    validateBranchTypes(thenVal, elseVal, "if expression");
    builder_.CreateBr(mergeBB);

    builder_.SetInsertPoint(mergeBB);
    llvm::PHINode *phi = builder_.CreatePHI(thenVal->getType(), 2, "if.block.expr");
    phi->addIncoming(thenVal, thenEndBB);
    phi->addIncoming(elseVal, elseEndBB);
    propagateMeta(thenVal, phi);
    return phi;
}

// ===== NoneExpr =====

llvm::Value *CodeGen::emitExprVariant(const NoneExpr &) {
    // Prefer branch-merge hint (#1154), then enclosing function return type.
    if (option_none_hint_inner_)
        return buildNoneValue(getOptionType(option_none_hint_inner_));
    llvm::Type *innerTy = i64Ty_;
    if (fn_) {
        llvm::Type *retTy = fn_->getReturnType();
        if (isOptionType(retTy))
            innerTy = llvm::cast<llvm::StructType>(retTy)->getElementType(1);
    }
    return buildNoneValue(getOptionType(innerTy));
}

// ===== ErrorPropagateExpr (?) =====

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<ErrorPropagateExpr> &e) {
    llvm::Value *operandVal = emitExpr(*e->operand);
    llvm::Type *operandTy = operandVal->getType();
    bool operandIsResult = isResultType(operandTy);
    bool operandIsOption = isOptionType(operandTy);
    if (!operandIsResult && !operandIsOption)
        codegenError("'?' operator requires a Result or Option type operand");

    // At the top level (inside __ry_main__), `?` desugars to "print the error
    // to stderr and exit(1)" on the unhappy path. See #745. We detect this by
    // inspecting the enclosing LLVM function name; __ry_main__'s return type
    // (i32, for the process exit code) is intentionally left unchanged so the
    // existing test_summary / outline / CreateRet(0) paths keep working.
    bool topLevel = fn_ && fn_->getName() == "__ry_main__";

    if (topLevel) {
        if (operandIsResult) {
            llvm::StructType *operandResultTy =
                llvm::cast<llvm::StructType>(operandTy);
            llvm::Type *operandErrTy = operandResultTy->getElementType(2);
            if (operandErrTy != errorTy_)
                codegenError("'?' at top level: Result err type must be Error");

            llvm::Value *isOk =
                builder_.CreateExtractValue(operandVal, 0, "is_ok");
            llvm::BasicBlock *okBB =
                llvm::BasicBlock::Create(*ctx_, "try.top.ok", fn_);
            llvm::BasicBlock *errBB =
                llvm::BasicBlock::Create(*ctx_, "try.top.err", fn_);
            builder_.CreateCondBr(isOk, okBB, errBB);

            builder_.SetInsertPoint(errBB);
            llvm::Value *errVal =
                builder_.CreateExtractValue(operandVal, 2, "err_val");
            llvm::Value *msgPtr =
                builder_.CreateExtractValue(errVal, 0, "err_msg_ptr");
            emitRuntimeError("error: %s\n", ".top_ep_err_msg", {msgPtr});

            builder_.SetInsertPoint(okBB);
            llvm::Value *okVal =
                builder_.CreateExtractValue(operandVal, 1, "ok_val");
            propagateMeta(operandVal, okVal);
            return okVal;
        }

        // Option operand at the top level: None aborts with a fixed message.
        llvm::Value *hasVal =
            builder_.CreateExtractValue(operandVal, 0, "has_val");
        llvm::BasicBlock *someBB =
            llvm::BasicBlock::Create(*ctx_, "try.top.some", fn_);
        llvm::BasicBlock *noneBB =
            llvm::BasicBlock::Create(*ctx_, "try.top.none", fn_);
        builder_.CreateCondBr(hasVal, someBB, noneBB);

        builder_.SetInsertPoint(noneBB);
        emitRuntimeError("error: unexpected None\n", ".top_ep_none");

        builder_.SetInsertPoint(someBB);
        llvm::Value *someVal =
            builder_.CreateExtractValue(operandVal, 1, "some_val");
        propagateMeta(operandVal, someVal);
        return someVal;
    }

    llvm::Type *fnRetTy = fn_->getReturnType();

    if (operandIsResult) {
        if (!isResultType(fnRetTy))
            codegenError("'?' on Result can only be used in a fn that returns Result");

        llvm::StructType *operandResultTy = llvm::cast<llvm::StructType>(operandTy);
        llvm::StructType *retResultTy = llvm::cast<llvm::StructType>(fnRetTy);
        llvm::Type *operandErrTy = operandResultTy->getElementType(2);
        llvm::Type *retErrTy = retResultTy->getElementType(2);

        llvm::Value *isOk = builder_.CreateExtractValue(operandVal, 0, "is_ok");
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "try.ok", fn_);
        llvm::BasicBlock *errBB = llvm::BasicBlock::Create(*ctx_, "try.err", fn_);
        builder_.CreateCondBr(isOk, okBB, errBB);

        // Err path: extract error, wrap in function return type, return
        builder_.SetInsertPoint(errBB);
        llvm::Value *errVal = builder_.CreateExtractValue(operandVal, 2, "err_val");
        if (operandErrTy != retErrTy) {
            if (auto *sliced = tryEmitSubtypeCoerce(errVal, retErrTy))
                errVal = sliced;
            else
                codegenError("'?' operator error type mismatch: operand and function return different error types");
        }
        llvm::Value *retErr = buildErrValue(errVal, retResultTy);
        emitEnsureChecks(retErr);
        emitScopeCleanupToDepth(0);
        builder_.CreateRet(retErr);

        // Ok path: extract ok value, continue
        builder_.SetInsertPoint(okBB);
        llvm::Value *okVal = builder_.CreateExtractValue(operandVal, 1, "ok_val");
        propagateMeta(operandVal, okVal);
        return okVal;
    }

    if (!isOptionType(fnRetTy))
        codegenError("'?' on Option can only be used in a fn that returns Option");

    llvm::StructType *retOptionTy = llvm::cast<llvm::StructType>(fnRetTy);

    llvm::Value *hasVal = builder_.CreateExtractValue(operandVal, 0, "has_val");
    llvm::BasicBlock *someBB = llvm::BasicBlock::Create(*ctx_, "try.some", fn_);
    llvm::BasicBlock *noneBB = llvm::BasicBlock::Create(*ctx_, "try.none", fn_);
    builder_.CreateCondBr(hasVal, someBB, noneBB);

    // None path: return None in the enclosing function's Option type.
    builder_.SetInsertPoint(noneBB);
    llvm::Value *retNone = buildNoneValue(retOptionTy);
    emitEnsureChecks(retNone);
    emitScopeCleanupToDepth(0);
    builder_.CreateRet(retNone);

    // Some path: extract inner value, continue.
    builder_.SetInsertPoint(someBB);
    llvm::Value *someVal = builder_.CreateExtractValue(operandVal, 1, "some_val");
    propagateMeta(operandVal, someVal);
    return someVal;
}

llvm::Value *CodeGen::emitTaskWait(llvm::Value *taskVal, const char *runtimeFn, const char *label) {
    llvm::Type *resultTy = getTaskResultType(taskVal);
    if (!resultTy)
        codegenError(std::string(label) + "() requires a Task value");

    llvm::FunctionType *fnTy = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*ctx_), {ptrTy_, ptrTy_}, false);
    llvm::FunctionCallee fn = mod_->getOrInsertFunction(runtimeFn, fnTy);

    if (resultTy->isVoidTy()) {
        return builder_.CreateCall(fn, {taskVal, llvm::ConstantPointerNull::get(
            llvm::cast<llvm::PointerType>(ptrTy_))});
    }

    llvm::AllocaInst *resultSlot = builder_.CreateAlloca(resultTy, nullptr, std::string(label) + "_result");
    builder_.CreateCall(fn, {taskVal, resultSlot});
    return builder_.CreateLoad(resultTy, resultSlot, std::string(label) + "_val");
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<AwaitExpr> &e) {
    return emitTaskWait(emitExpr(*e->operand), "__ry_task_join", "await");
}

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<WeakExpr> &e) {
    llvm::Value *val = emitExpr(*e->operand);
    if (val->getType() != ptrTy_)
        codegenError("weak can only be applied to ARC-managed reference values (str, List, Map, Set)");
    // Return the raw data pointer. The caller (codegen_stmt.cpp) has the inner
    // type name from the annotation and performs the correct header offset:
    // STRING_HEADER_SIZE (24) for str, ARC_HEADER_SIZE (16) for collections.
    // We cannot use isStringValue() here because captured List/Map/Set values
    // may lack collection metadata, causing false positives.
    return val;
}

llvm::Value *CodeGen::emitListConcat(llvm::Value *lhs, llvm::Value *rhs, llvm::Type *elemTy) {
    const llvm::DataLayout &dl = mod_->getDataLayout();
    uint64_t elemSize = dl.getTypeAllocSize(elemTy);
    auto mallocFn = getStdlibMalloc();
    auto memcpyFn = getStdlibMemcpy();

    auto lf = loadListHeader(lhs, "catl");
    auto rf = loadListHeader(rhs, "catr");

    llvm::Value *newLen = builder_.CreateAdd(lf.len, rf.len, "cat_len");

    llvm::Value *newHeader = emitArcAllocCollectionHeader(listHeaderTy_);

    llvm::Value *dataSize = builder_.CreateMul(newLen, llvm::ConstantInt::get(i64Ty_, elemSize), "cat_ds");
    llvm::Value *newData = builder_.CreateCall(mallocFn, {dataSize}, "cat_data");

    llvm::Value *lhsSize = builder_.CreateMul(lf.len, llvm::ConstantInt::get(i64Ty_, elemSize), "cat_ls");
    builder_.CreateCall(memcpyFn, {newData, lf.data, lhsSize});

    llvm::Value *rhsDst = builder_.CreateGEP(elemTy, newData, lf.len, "cat_rhs_dst");
    llvm::Value *rhsSize = builder_.CreateMul(rf.len, llvm::ConstantInt::get(i64Ty_, elemSize), "cat_rs");
    builder_.CreateCall(memcpyFn, {rhsDst, rf.data, rhsSize});

    // Reference-typed elements share ownership with the source lists. memcpy
    // duplicates raw pointers without bumping refcounts; without retention,
    // releasing either source (or a dropped alias) frees the elements that the
    // concatenated result still points at (#1236, same defect class as #1204 /
    // #1235). lhs and rhs carry identical element-type metadata (typecheck
    // rejects mismatched operands), so querying lhs suffices for both halves.
    CollectionKind elemArcKind = CollectionKind::List;
    if (elementTypeIsArcManaged(lhs, CollectionKind::List, &elemArcKind)) {
        emitCowRetainArcElements(newData, newLen, "cat_elem", elemArcKind);
    }

    storeListHeaderFields(newHeader, newLen, newLen, newData);

    setTypeMeta(TypeMeta::ListElem, newHeader, elemTy);

    // Propagate nested-list metadata so flatten() works on concatenated results
    if (elemTy == ptrTy_) {
        llvm::Type *nestedL = getTypeMeta(TypeMeta::NestedListElem, lhs);
        llvm::Type *nestedR = getTypeMeta(TypeMeta::NestedListElem, rhs);
        if (nestedL && nestedR && nestedL == nestedR)
            setTypeMeta(TypeMeta::NestedListElem, newHeader, nestedL);
    }

    return newHeader;
}

} // namespace ry
