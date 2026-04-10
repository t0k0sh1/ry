#include "ry/codegen.hpp"
#include "ry/stdlib_registry.hpp"


namespace ry {

// ===== Type resolution and related helpers =====

llvm::Type *CodeGen::resolveType(const std::string &typeName) {
    // Resolve generic type parameter if in scope
    if (!type_param_scope_.empty()) {
        auto tpit = type_param_scope_.find(typeName);
        if (tpit != type_param_scope_.end())
            return resolveType(tpit->second);
    }

    // Built-in primitive types first (cannot be shadowed by aliases)
    if (typeName == "int")   return i64Ty_;
    if (typeName == "float") return f64Ty_;
    if (typeName == "bool")  return i1Ty_;
    if (typeName == "str")   return ptrTy_;
    if (typeName == "Error") return errorTy_;
    if (typeName == "any")   return anyTy_;
    if (typeName == "Unit")  return llvm::Type::getVoidTy(*ctx_);
    // Low-level numeric types
    if (typeName == "i8")    return i8Ty_;
    if (typeName == "i16")   return i16Ty_;
    if (typeName == "i32")   return i32Ty_;
    if (typeName == "i64")   return i64Ty_;
    if (typeName == "u8")    return i8Ty_;
    if (typeName == "u16")   return i16Ty_;
    if (typeName == "u32")   return i32Ty_;
    if (typeName == "u64")   return i64Ty_;
    if (typeName == "f32")   return f32Ty_;

    // Weak reference type: "weak str", "weak List<int>"
    if (isWeakTypeName(typeName)) {
        std::string inner = weakInnerTypeName(typeName);
        // Resolve aliases to canonical name
        auto aliasIt = type_aliases_.find(inner);
        std::string canonical = (aliasIt != type_aliases_.end()) ? resolveTypeAlias(inner) : inner;
        // Extract base type name (before generic args)
        std::string base = canonical;
        auto ltPos = base.find('<');
        if (ltPos != std::string::npos)
            base = base.substr(0, ltPos);
        if (base != "str" && base != "List" && base != "Map" && base != "Set")
            codegenError("weak references require an ARC-managed type (str, List, Map, Set), got: " + inner);
        resolveType(inner);  // validate inner type exists
        return ptrTy_;  // weak ref stores a header pointer
    }

    // Optional type suffix: "int?" -> Option<int>
    if (!typeName.empty() && typeName.back() == '?') {
        std::string inner = typeName.substr(0, typeName.size() - 1);
        llvm::Type *innerTy = resolveType(inner);
        return getOptionType(innerTy);
    }

    // Check type alias (with cycle detection)
    auto aliasIt = type_aliases_.find(typeName);
    if (aliasIt != type_aliases_.end()) {
        std::string resolved = resolveTypeAlias(typeName);
        return resolveType(resolved);
    }

    // Int literal type: "42", "-5"
    if (isIntLiteralType(typeName))
        return i64Ty_;

    // Range type: "1..12"
    if (isRangeType(typeName))
        return i64Ty_;

    // String literal type: "\"N\""
    if (isStrLiteralType(typeName))
        return ptrTy_;

    // Literal union type: "0 | 1 | 2" or "\"N\" | \"S\""
    if (isLiteralUnionType(typeName))
        return parseTypeConstraint(typeName)->kind == TypeConstraint::Kind::IntLiteral ? i64Ty_ : ptrTy_;

    // Union type: "int | str"
    if (typeName.find(" | ") != std::string::npos) {
        std::string normalized = normalizeUnionType(typeName);
        auto it = union_type_info_.find(normalized);
        if (it != union_type_info_.end()) return it->second.llvmType;

        auto components = parseUnionComponents(normalized);
        std::vector<llvm::Type*> compTypes;
        uint64_t maxSize = 0;
        const auto &dl = mod_->getDataLayout();
        for (auto &c : components) {
            auto *ty = resolveType(c);
            compTypes.push_back(ty);
            maxSize = std::max(maxSize, (uint64_t)dl.getTypeAllocSize(ty));
        }
        auto *dataTy = llvm::ArrayType::get(
            llvm::Type::getInt8Ty(*ctx_), maxSize);
        auto *unionTy = llvm::StructType::create(
            *ctx_, {i64Ty_, dataTy}, "union." + normalized);

        union_type_info_[normalized] = {unionTy, components, compTypes};
        return unionTy;
    }

    // Fixed-length array type: T[N]
    if (!typeName.empty() && typeName.back() == ']') {
        size_t bracketPos = typeName.find('[');
        if (bracketPos != std::string::npos && bracketPos > 0) {
            std::string elemStr = typeName.substr(0, bracketPos);
            std::string sizeStr = typeName.substr(bracketPos + 1, typeName.size() - bracketPos - 2);
            size_t s = sizeStr.find_first_not_of(' ');
            if (s != std::string::npos) sizeStr = sizeStr.substr(s);
            while (!sizeStr.empty() && sizeStr.back() == ' ') sizeStr.pop_back();

            if (!isLowLevelTypeName(elemStr))
                codegenError("array element type must be a low-level type: " + elemStr);
            llvm::Type *elemTy = resolveType(elemStr);
            uint64_t size = std::stoull(sizeStr);
            if (size == 0) codegenError("array size must be > 0");
            return llvm::ArrayType::get(elemTy, size);
        }
    }

    // Tuple type: "(int, float)"
    if (!typeName.empty() && typeName.front() == '(') {
        // Parse element types from "(T1, T2, ...)"
        std::string inner = typeName.substr(1, typeName.size() - 2); // strip parens
        std::vector<llvm::Type*> elementTypes;
        size_t depth = 0;
        size_t start = 0;
        for (size_t i = 0; i <= inner.size(); ++i) {
            if (i < inner.size() && inner[i] == '(') ++depth;
            else if (i < inner.size() && inner[i] == ')') --depth;
            else if ((i == inner.size() || inner[i] == ',') && depth == 0) {
                std::string elem = inner.substr(start, i - start);
                // trim leading/trailing spaces
                size_t s = elem.find_first_not_of(' ');
                size_t e = elem.find_last_not_of(' ');
                if (s != std::string::npos)
                    elem = elem.substr(s, e - s + 1);
                if (elem.empty()) continue; // trailing comma
                elementTypes.push_back(resolveType(elem));
                start = i + 1;
            }
        }
        return llvm::StructType::get(*ctx_, elementTypes);
    }

    // fn(...) -> T function type → opaque pointer
    if (typeName.size() > 9 && typeName.compare(0, 9, "function(") == 0) {
        return ptrTy_;
    }

    // Collection type parsing (List<T>, Map<K, V>, Set<T>)
    if (isCollectionTypeName(typeName) && typeName.back() == '>') {
        return ptrTy_;
    }

    // Task<T> parsing
    if (typeName.size() > 5 && typeName.compare(0, 5, "Task<") == 0 && typeName.back() == '>') {
        return ptrTy_;
    }

    // Channel<T> parsing
    if (typeName.size() > 8 && typeName.compare(0, 8, "Channel<") == 0 && typeName.back() == '>') {
        return ptrTy_;
    }

    // Opaque resource handle types (dynamically registered)
    if (ResourceKindRegistry::instance().lookupByTypeName(typeName) != ResourceKindRegistry::NONE)
        return ptrTy_;

    // Option<T> parsing
    if (typeName.size() > 7 && typeName.compare(0, 7, "Option<") == 0 && typeName.back() == '>') {
        std::string inner = typeName.substr(7, typeName.size() - 8);
        llvm::Type *innerTy = resolveType(inner);
        return getOptionType(innerTy);
    }

    // Result<V, E> parsing
    if (typeName.size() > 7 && typeName.compare(0, 7, "Result<") == 0 && typeName.back() == '>') {
        std::string inner = typeName.substr(7, typeName.size() - 8);
        // Find comma separating V and E, respecting nested angle brackets
        int depth = 0;
        size_t commaPos = std::string::npos;
        for (size_t i = 0; i < inner.size(); ++i) {
            if (inner[i] == '<') ++depth;
            else if (inner[i] == '>') --depth;
            else if (inner[i] == ',' && depth == 0) {
                commaPos = i;
                break;
            }
        }
        if (commaPos == std::string::npos)
            codegenError("Result type requires two type parameters: Result<V, E>");
        std::string okStr = inner.substr(0, commaPos);
        std::string errStr = inner.substr(commaPos + 1);
        // Trim whitespace
        while (!okStr.empty() && okStr.back() == ' ') okStr.pop_back();
        while (!errStr.empty() && errStr.front() == ' ') errStr.erase(errStr.begin());
        llvm::Type *okTy = (okStr == "Unit") ? i8Ty_ : resolveType(okStr);
        llvm::Type *errTy = resolveType(errStr);
        return getResultType(okTy, errTy);
    }

    auto it = struct_types_.find(typeName);
    if (it != struct_types_.end()) return it->second.llvmType;

    // enum name → i64 (simple) or ADT struct type
    {
        auto eit = enum_types_.find(typeName);
        if (eit != enum_types_.end())
            return eit->second.isADT ? eit->second.adtType : i64Ty_;
    }

    codegenError("unknown type: " + typeName);
}

std::string CodeGen::findAdtEnumName(llvm::StructType *st) const {
    for (auto &[name, info] : enum_types_)
        if (info.isADT && info.adtType == st) return name;
    return {};
}

std::string CodeGen::findStructTypeName(llvm::StructType *st) const {
    for (auto &[name, info] : struct_types_)
        if (info.llvmType == st) return name;
    return findAdtEnumName(st);
}

llvm::StructType *CodeGen::getOptionType(llvm::Type *innerTy) {
    auto it = option_types_.find(innerTy);
    if (it != option_types_.end()) return it->second;
    llvm::StructType *optTy = llvm::StructType::create(
        *ctx_, {i1Ty_, innerTy}, "Option");
    option_types_[innerTy] = optTy;
    return optTy;
}

bool CodeGen::isTupleStructType(llvm::StructType *st) {
    if (st->hasName()) {
        std::string name = st->getName().str();
        if (struct_types_.count(name)) return false;
    }
    for (auto &[name, info] : union_type_info_)
        if (info.llvmType == st) return false;
    if (isOptionType(st)) return false;
    if (isResultType(st)) return false;
    if (st == errorTy_) return false;
    if (!findAdtEnumName(st).empty()) return false;
    return true;
}

bool CodeGen::isNoneLiteral(const ExprNode &expr) {
    return std::holds_alternative<NoneExpr>(expr.data) ||
           (std::holds_alternative<VariableExpr>(expr.data) &&
            std::get<VariableExpr>(expr.data).name == "None");
}

bool CodeGen::isOptionType(llvm::Type *ty) {
    auto *st = llvm::dyn_cast<llvm::StructType>(ty);
    if (!st) return false;
    for (auto &pair : option_types_) {
        if (pair.second == st) return true;
    }
    return false;
}

llvm::StructType *CodeGen::getResultType(llvm::Type *okTy, llvm::Type *errTy) {
    auto key = std::make_pair(okTy, errTy);
    auto it = result_types_.find(key);
    if (it != result_types_.end()) return it->second;
    llvm::StructType *resTy = llvm::StructType::create(
        *ctx_, {i1Ty_, okTy, errTy}, "Result");
    result_types_[key] = resTy;
    return resTy;
}

bool CodeGen::isResultType(llvm::Type *ty) {
    auto *st = llvm::dyn_cast<llvm::StructType>(ty);
    if (!st) return false;
    for (auto &pair : result_types_) {
        if (pair.second == st) return true;
    }
    return false;
}

llvm::Value *CodeGen::buildOkValue(llvm::Value *inner, llvm::StructType *resultTy) {
    llvm::Value *val = llvm::ConstantAggregateZero::get(resultTy);
    val = builder_.CreateInsertValue(val, llvm::ConstantInt::get(i1Ty_, 1), 0, "res.ok");
    val = builder_.CreateInsertValue(val, inner, 1, "res.ok_val");
    val = builder_.CreateInsertValue(val, llvm::Constant::getNullValue(resultTy->getElementType(2)), 2);
    propagateMeta(inner, val);
    return val;
}

llvm::Value *CodeGen::buildErrValue(llvm::Value *inner, llvm::StructType *resultTy) {
    llvm::Value *val = llvm::ConstantAggregateZero::get(resultTy);
    val = builder_.CreateInsertValue(val, llvm::ConstantInt::get(i1Ty_, 0), 0, "res.err");
    val = builder_.CreateInsertValue(val, llvm::Constant::getNullValue(resultTy->getElementType(1)), 1);
    val = builder_.CreateInsertValue(val, inner, 2, "res.err_val");
    return val;
}

llvm::Value *CodeGen::buildStaticError(const std::string &msg, const std::string &globalName) {
    llvm::Value *errMsgStr = cachedGlobalString(msg, globalName);
    llvm::Value *errStruct = llvm::UndefValue::get(errorTy_);
    errStruct = builder_.CreateInsertValue(errStruct, errMsgStr, 0, "err.msg");
    errStruct = builder_.CreateInsertValue(errStruct, llvm::ConstantInt::get(i64Ty_, 0), 1, "err.code");
    return errStruct;
}

std::vector<std::string> CodeGen::splitTypeArgs(const std::string &argsStr) {
    std::vector<std::string> typeArgs;
    std::string curr;
    int angleBrackets = 0;
    int parens = 0;
    for (char c : argsStr) {
        if (c == '<') angleBrackets++;
        else if (c == '>') angleBrackets--;
        else if (c == '(') parens++;
        else if (c == ')') parens--;
        else if (c == ',' && angleBrackets == 0 && parens == 0) {
            typeArgs.push_back(curr);
            curr.clear();
            continue;
        }
        curr += c;
    }
    if (!curr.empty()) typeArgs.push_back(curr);
    return typeArgs;
}

std::pair<llvm::Type*, llvm::Type*> CodeGen::parseMapTypeAnnotation(const std::string &typeStr) {
    std::string inner = typeStr.substr(4, typeStr.size() - 5);
    auto parts = splitTypeArgs(inner);
    if (parts.size() != 2) return {nullptr, nullptr};
    auto &kStr = parts[0];
    auto &vStr = parts[1];
    while (!kStr.empty() && kStr.back() == ' ') kStr.pop_back();
    while (!vStr.empty() && vStr.front() == ' ') vStr = vStr.substr(1);
    return {resolveType(kStr), resolveType(vStr)};
}

llvm::Type *CodeGen::getTaskResultType(llvm::Value *taskVal) {
    return getTypeMeta(TypeMeta::TaskResult, taskVal);
}

size_t CodeGen::findMatchingCloseParen(const std::string &s, size_t openParen) {
    if (openParen >= s.size() || s[openParen] != '(')
        return std::string::npos;
    int depth = 1;
    for (size_t i = openParen + 1; i < s.size(); ++i) {
        if (s[i] == '(') ++depth;
        else if (s[i] == ')' && --depth == 0)
            return i;
    }
    return std::string::npos;
}

CodeGen::FnTypeInfo CodeGen::parseFnTypeAnnotation(const std::string &typeStr) {
    // Parse "function(int, float) -> int"
    FnTypeInfo info;
    // Find the opening paren
    size_t openParen = typeStr.find('(');
    if (openParen == std::string::npos)
        codegenError("invalid function type: " + typeStr);
    size_t closeParen = findMatchingCloseParen(typeStr, openParen);
    if (closeParen == std::string::npos)
        codegenError("invalid function type: " + typeStr);

    std::string paramStr = typeStr.substr(openParen + 1, closeParen - openParen - 1);
    // Parse comma-separated parameter types
    if (!paramStr.empty()) {
        size_t start = 0;
        int depth = 0;
        for (size_t i = 0; i <= paramStr.size(); ++i) {
            if (i < paramStr.size() && paramStr[i] == '(') ++depth;
            else if (i < paramStr.size() && paramStr[i] == ')') --depth;
            else if ((i == paramStr.size() || paramStr[i] == ',') && depth == 0) {
                std::string p = paramStr.substr(start, i - start);
                // trim
                size_t s = p.find_first_not_of(' ');
                size_t e = p.find_last_not_of(' ');
                if (s != std::string::npos)
                    p = p.substr(s, e - s + 1);
                info.paramTypes.push_back(resolveType(p));
                info.paramTypeNames.push_back(p);
                start = i + 1;
            }
        }
    }

    // Parse return type after " -> "
    size_t arrow = typeStr.find("->", closeParen);
    if (arrow != std::string::npos) {
        std::string retStr = typeStr.substr(arrow + 2);
        size_t s = retStr.find_first_not_of(' ');
        if (s != std::string::npos)
            retStr = retStr.substr(s);
        info.returnType = resolveType(retStr);
        std::string resolvedRetStr = resolveTypeAlias(retStr);
        {
            size_t rs = resolvedRetStr.find_first_not_of(' ');
            size_t re = resolvedRetStr.find_last_not_of(' ');
            if (rs != std::string::npos)
                resolvedRetStr = resolvedRetStr.substr(rs, re - rs + 1);
        }
        if (isFunctionTypeName(resolvedRetStr))
            info.returnFnTypeInfo = std::make_unique<FnTypeInfo>(parseFnTypeAnnotation(resolvedRetStr));
    } else {
        info.returnType = llvm::Type::getVoidTy(*ctx_);
    }

    info.isUniformClosure = true;
    return info;
}

llvm::Value *CodeGen::buildNoneValue(llvm::Type *optionTy) {
    llvm::Value *val = llvm::UndefValue::get(optionTy);
    val = builder_.CreateInsertValue(val, llvm::ConstantInt::get(i1Ty_, 0), 0);
    val = builder_.CreateInsertValue(val, llvm::UndefValue::get(
        llvm::cast<llvm::StructType>(optionTy)->getElementType(1)), 1);
    return val;
}

llvm::Value *CodeGen::buildSomeValue(llvm::Value *inner, llvm::Type *optionTy) {
    llvm::Value *val = llvm::UndefValue::get(optionTy);
    val = builder_.CreateInsertValue(val, llvm::ConstantInt::get(i1Ty_, 1), 0);
    val = builder_.CreateInsertValue(val, inner, 1);
    propagateMeta(inner, val);
    return val;
}

// ===== Literal/Range type helpers =====

std::string CodeGen::resolveTypeAlias(const std::string &typeName) {
    std::unordered_set<std::string> visited;
    std::string current = typeName;
    while (true) {
        if (!visited.insert(current).second)
            codegenError("Circular type alias detected: " + typeName);
        auto it = type_aliases_.find(current);
        if (it == type_aliases_.end())
            break;
        current = it->second;
    }
    return current;
}

bool CodeGen::isIntLiteralType(const std::string &typeName) {
    if (typeName.empty()) return false;
    size_t start = (typeName[0] == '-') ? 1 : 0;
    if (start >= typeName.size()) return false;
    for (size_t i = start; i < typeName.size(); ++i) {
        if (!std::isdigit(typeName[i])) return false;
    }
    return true;
}

bool CodeGen::isStrLiteralType(const std::string &typeName) {
    if (typeName.size() < 2 || typeName.front() != '"' || typeName.back() != '"')
        return false;
    // Ensure it's a single quoted string, not a union like "N" | "S"
    // Check there's exactly one opening and one closing quote
    size_t quoteCount = 0;
    for (char c : typeName)
        if (c == '"') quoteCount++;
    return quoteCount == 2;
}

bool CodeGen::isRangeType(const std::string &typeName) {
    auto pos = typeName.find("..");
    if (pos == std::string::npos || pos == 0 || pos == typeName.size() - 2)
        return false;
    std::string lo = typeName.substr(0, pos);
    std::string hi = typeName.substr(pos + 2);
    return isIntLiteralType(lo) && isIntLiteralType(hi);
}

bool CodeGen::isLiteralUnionType(const std::string &typeName) {
    if (typeName.find(" | ") == std::string::npos) return false;
    auto components = parseUnionComponents(typeName);
    if (components.empty()) return false;
    bool allInt = true, allStr = true;
    for (auto &c : components) {
        if (allInt && !isIntLiteralType(c)) allInt = false;
        if (allStr && !isStrLiteralType(c)) allStr = false;
        if (!allInt && !allStr) return false;
    }
    return allInt || allStr;
}

std::optional<CodeGen::TypeConstraint> CodeGen::parseTypeConstraint(const std::string &typeName) {
    // Callers are responsible for resolving type aliases before calling this function.

    // Range type: "1..12"
    if (isRangeType(typeName)) {
        auto pos = typeName.find("..");
        TypeConstraint tc;
        tc.kind = TypeConstraint::Kind::IntRange;
        tc.range_low = std::stoll(typeName.substr(0, pos));
        tc.range_high = std::stoll(typeName.substr(pos + 2));
        if (tc.range_low > tc.range_high)
            codegenError("invalid range type: low bound " +
                std::to_string(tc.range_low) + " > high bound " +
                std::to_string(tc.range_high));
        return tc;
    }

    // Single int literal: "42"
    if (isIntLiteralType(typeName)) {
        TypeConstraint tc;
        tc.kind = TypeConstraint::Kind::IntLiteral;
        tc.int_values.push_back(std::stoll(typeName));
        return tc;
    }

    // Single str literal: "\"N\""
    if (isStrLiteralType(typeName)) {
        TypeConstraint tc;
        tc.kind = TypeConstraint::Kind::StrLiteral;
        tc.str_values.push_back(typeName.substr(1, typeName.size() - 2));
        return tc;
    }

    // Union of literals
    if (typeName.find(" | ") != std::string::npos) {
        auto components = parseUnionComponents(typeName);
        if (components.empty()) return std::nullopt;

        // Check if all int literals
        bool allInt = true;
        for (auto &c : components) {
            if (!isIntLiteralType(c)) { allInt = false; break; }
        }
        if (allInt) {
            TypeConstraint tc;
            tc.kind = TypeConstraint::Kind::IntLiteral;
            for (auto &c : components)
                tc.int_values.push_back(std::stoll(c));
            return tc;
        }

        // Check if all str literals
        bool allStr = true;
        for (auto &c : components) {
            if (!isStrLiteralType(c)) { allStr = false; break; }
        }
        if (allStr) {
            TypeConstraint tc;
            tc.kind = TypeConstraint::Kind::StrLiteral;
            for (auto &c : components)
                tc.str_values.push_back(c.substr(1, c.size() - 2));
            return tc;
        }
    }

    return std::nullopt;
}

void CodeGen::emitConstraintCheck(llvm::Value *val, const TypeConstraint &constraint,
                                   const std::string &varName) {
    if (constraint.kind == TypeConstraint::Kind::IntLiteral) {
        // Compile-time check if constant
        if (auto *ci = llvm::dyn_cast<llvm::ConstantInt>(val)) {
            int64_t v = ci->getSExtValue();
            bool found = false;
            for (int64_t allowed : constraint.int_values) {
                if (v == allowed) { found = true; break; }
            }
            if (!found) {
                std::string allowed_str;
                for (size_t i = 0; i < constraint.int_values.size(); ++i) {
                    if (i > 0) allowed_str += " | ";
                    allowed_str += std::to_string(constraint.int_values[i]);
                }
                codegenError(
                    "value " + std::to_string(v) + " is not in literal type " + allowed_str +
                    " for variable '" + varName + "'");
            }
            return; // Compile-time check passed
        }
        // Runtime check: compare against each allowed value, OR results
        llvm::Value *anyMatch = llvm::ConstantInt::get(i1Ty_, 0);
        for (int64_t allowed : constraint.int_values) {
            llvm::Value *cmp = builder_.CreateICmpEQ(
                val, llvm::ConstantInt::get(i64Ty_, allowed), "lit_cmp");
            anyMatch = builder_.CreateOr(anyMatch, cmp, "lit_or");
        }
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "constraint.ok", fn_);
        llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "constraint.fail", fn_);
        builder_.CreateCondBr(anyMatch, okBB, failBB);
        builder_.SetInsertPoint(failBB);
        emitRuntimeError("runtime error: value out of range for '" + varName + "'\n",
                          ".constraint_err_" + std::to_string(constraint_err_counter_++));
        builder_.SetInsertPoint(okBB);

    } else if (constraint.kind == TypeConstraint::Kind::IntRange) {
        // Compile-time check if constant
        if (auto *ci = llvm::dyn_cast<llvm::ConstantInt>(val)) {
            int64_t v = ci->getSExtValue();
            if (v < constraint.range_low || v > constraint.range_high) {
                codegenError(
                    "value " + std::to_string(v) + " is out of range " +
                    std::to_string(constraint.range_low) + ".." +
                    std::to_string(constraint.range_high) +
                    " for variable '" + varName + "'");
            }
            return; // Compile-time check passed
        }
        // Runtime check: low <= val <= high
        llvm::Value *geLow = builder_.CreateICmpSGE(
            val, llvm::ConstantInt::get(i64Ty_, constraint.range_low), "range_ge");
        llvm::Value *leHigh = builder_.CreateICmpSLE(
            val, llvm::ConstantInt::get(i64Ty_, constraint.range_high), "range_le");
        llvm::Value *inRange = builder_.CreateAnd(geLow, leHigh, "in_range");
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "constraint.ok", fn_);
        llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "constraint.fail", fn_);
        builder_.CreateCondBr(inRange, okBB, failBB);
        builder_.SetInsertPoint(failBB);
        emitRuntimeError("runtime error: value out of range for '" + varName + "'\n",
                          ".constraint_err_" + std::to_string(constraint_err_counter_++));
        builder_.SetInsertPoint(okBB);

    } else if (constraint.kind == TypeConstraint::Kind::StrLiteral) {
        // Compile-time check: if the value is a global string constant, check it
        if (auto *constExpr = llvm::dyn_cast<llvm::ConstantExpr>(val)) {
            // Can't easily extract string from ConstantExpr, fall through to runtime
        }
        // For string literals, we need runtime strcmp checks
        auto strcmpFn = getStdlibStrcmp();

        llvm::Value *anyMatch = llvm::ConstantInt::get(i1Ty_, 0);
        for (const auto &allowed : constraint.str_values) {
            llvm::Constant *allowedStr = cachedGlobalString(
                allowed, ".str_lit_" + std::to_string(constraint_err_counter_) + "_" + allowed);
            llvm::Value *cmpResult = builder_.CreateCall(strcmpFn, {val, allowedStr}, "strcmp_res");
            llvm::Value *isEq = builder_.CreateICmpEQ(
                cmpResult, llvm::ConstantInt::get(i32Ty_, 0), "str_eq");
            anyMatch = builder_.CreateOr(anyMatch, isEq, "str_or");
        }
        llvm::BasicBlock *okBB = llvm::BasicBlock::Create(*ctx_, "constraint.ok", fn_);
        llvm::BasicBlock *failBB = llvm::BasicBlock::Create(*ctx_, "constraint.fail", fn_);
        builder_.CreateCondBr(anyMatch, okBB, failBB);
        builder_.SetInsertPoint(failBB);
        emitRuntimeError("runtime error: value not in allowed set for '" + varName + "'\n",
                          ".constraint_err_" + std::to_string(constraint_err_counter_++));
        builder_.SetInsertPoint(okBB);
    }
}

} // namespace ry
