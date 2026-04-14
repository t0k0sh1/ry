#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"


namespace ry {

// ===== Shared match helpers =====

std::string CodeGen::resolveEnumType(llvm::Value *val) const {
    auto *meta = getMeta(val);
    if (meta && !meta->enum_value_type.empty())
        return meta->enum_value_type;
    return {};
}

void CodeGen::validateBranchTypes(llvm::Value *lhs, llvm::Value *rhs, const char *exprKind) {
    if (lhs->getType() != rhs->getType())
        codegenError(std::string(exprKind) + ": all branches must have the same type");

    if (lhs->getType() == ptrTy_) {
        enum class SemanticKind { Str, List, Map, Set, Other };
        auto classify = [&](llvm::Value *v) -> SemanticKind {
            if (isStringValue(v)) return SemanticKind::Str;
            if (getTypeMeta(TypeMeta::ListElem, v)) return SemanticKind::List;
            if (getTypeMeta(TypeMeta::MapKey, v)) return SemanticKind::Map;
            if (getTypeMeta(TypeMeta::SetElem, v)) return SemanticKind::Set;
            return SemanticKind::Other;
        };
        SemanticKind lhsKind = classify(lhs);
        SemanticKind rhsKind = classify(rhs);
        if (lhsKind != rhsKind)
            codegenError(std::string(exprKind) + ": all branches must have the same type");
        if (lhsKind == SemanticKind::List) {
            llvm::Type *lhsElem = getTypeMeta(TypeMeta::ListElem, lhs);
            llvm::Type *rhsElem = getTypeMeta(TypeMeta::ListElem, rhs);
            if (lhsElem && rhsElem && lhsElem != rhsElem)
                codegenError(std::string(exprKind) + ": all branches must have the same type");
        }
    }
}

void CodeGen::checkMatchExhaustiveness(
    const std::vector<std::pair<const Pattern*, bool>> &armPatterns,
    llvm::Type *subjectTy, const std::string &subjectEnumType) {

    bool hasWildcardOrVar = false;
    auto checkWildcardOrVar = [](const Pattern &p) {
        return std::holds_alternative<WildcardPattern>(p) ||
               std::holds_alternative<VariablePattern>(p);
    };
    for (auto &[pat, hasGuard] : armPatterns) {
        if (!hasGuard) {
            if (checkWildcardOrVar(*pat)) {
                hasWildcardOrVar = true;
            } else if (auto *op = std::get_if<std::unique_ptr<OrPattern>>(pat)) {
                for (auto &alt : (*op)->alternatives) {
                    if (checkWildcardOrVar(alt)) {
                        hasWildcardOrVar = true;
                        break;
                    }
                }
            }
        }
    }

    if (hasWildcardOrVar) return;

    // Check enum exhaustiveness
    std::string enumName;
    for (auto &[pat, hasGuard] : armPatterns) {
        if (auto *ep = std::get_if<EnumPattern>(pat)) {
            enumName = ep->enum_name;
            break;
        }
        if (auto *ecp = std::get_if<EnumConstructorPattern>(pat)) {
            enumName = ecp->enum_name;
            break;
        }
        if (auto *op = std::get_if<std::unique_ptr<OrPattern>>(pat)) {
            for (auto &alt : (*op)->alternatives) {
                if (auto *ep2 = std::get_if<EnumPattern>(&alt)) {
                    enumName = ep2->enum_name;
                    break;
                }
                if (auto *ecp2 = std::get_if<EnumConstructorPattern>(&alt)) {
                    enumName = ecp2->enum_name;
                    break;
                }
            }
            if (!enumName.empty()) break;
        }
    }
    if (!enumName.empty()) {
        if (!enum_types_.count(enumName) && !subjectEnumType.empty()) {
            auto ltPos = subjectEnumType.find('<');
            if (ltPos != std::string::npos && subjectEnumType.substr(0, ltPos) == enumName)
                enumName = subjectEnumType;
        }
        auto it = enum_types_.find(enumName);
        if (it != enum_types_.end()) {
            std::unordered_set<std::string> covered;
            for (auto &[pat, hasGuard] : armPatterns) {
                if (!hasGuard) {
                    if (auto *ep = std::get_if<EnumPattern>(pat))
                        covered.insert(ep->variant_name);
                    if (auto *ecp = std::get_if<EnumConstructorPattern>(pat))
                        covered.insert(ecp->variant_name);
                    if (auto *op = std::get_if<std::unique_ptr<OrPattern>>(pat)) {
                        for (auto &alt : (*op)->alternatives) {
                            if (auto *ep2 = std::get_if<EnumPattern>(&alt))
                                covered.insert(ep2->variant_name);
                            if (auto *ecp2 = std::get_if<EnumConstructorPattern>(&alt))
                                covered.insert(ecp2->variant_name);
                        }
                    }
                }
            }
            for (auto &[vname, _] : it->second.variants) {
                if (!covered.count(vname)) {
                    std::string exhaustMsg = "non-exhaustive match: missing variant '";
                    exhaustMsg += enumName;
                    exhaustMsg += "::";
                    exhaustMsg += vname;
                    exhaustMsg += "'";
                    codegenError(exhaustMsg);
                }
            }
        }
    }

    // Check Option exhaustiveness
    bool hasSome = false, hasNone = false;
    auto checkOptionPattern = [&](const Pattern &p) {
        if (std::holds_alternative<SomePattern>(p)) hasSome = true;
        if (std::holds_alternative<NonePattern>(p)) hasNone = true;
    };
    for (auto &[pat, hasGuard] : armPatterns) {
        if (!hasGuard) {
            checkOptionPattern(*pat);
            if (auto *op = std::get_if<std::unique_ptr<OrPattern>>(pat)) {
                for (auto &alt : (*op)->alternatives)
                    checkOptionPattern(alt);
            }
        }
    }
    if ((hasSome && !hasNone) || (!hasSome && hasNone))
        codegenError("non-exhaustive match: Option requires both Some and None cases (or use '_')");

    // Check Result exhaustiveness
    bool hasOk = false, hasErr = false;
    auto checkResultPattern = [&](const Pattern &p) {
        if (std::holds_alternative<OkPattern>(p)) hasOk = true;
        if (std::holds_alternative<ErrPattern>(p)) hasErr = true;
    };
    for (auto &[pat, hasGuard] : armPatterns) {
        if (!hasGuard) {
            checkResultPattern(*pat);
            if (auto *op = std::get_if<std::unique_ptr<OrPattern>>(pat)) {
                for (auto &alt : (*op)->alternatives)
                    checkResultPattern(alt);
            }
        }
    }
    if ((hasOk && !hasErr) || (!hasOk && hasErr))
        codegenError("non-exhaustive match: Result requires both Ok and Err cases (or use '_')");

    // Check bool exhaustiveness
    bool hasTrue = false, hasFalse = false;
    auto checkBoolPattern = [&](const Pattern &p) {
        if (auto *lp = std::get_if<LiteralPattern>(&p)) {
            if (auto *be = std::get_if<BoolExpr>(&lp->value->data)) {
                if (be->value) hasTrue = true;
                if (!be->value) hasFalse = true;
            }
        }
    };
    for (auto &[pat, hasGuard] : armPatterns) {
        if (!hasGuard) {
            checkBoolPattern(*pat);
            if (auto *op = std::get_if<std::unique_ptr<OrPattern>>(pat)) {
                for (auto &alt : (*op)->alternatives)
                    checkBoolPattern(alt);
            }
        }
    }
    if (subjectTy == i1Ty_ && !(hasTrue && hasFalse))
        codegenError("non-exhaustive match: bool requires both true and false cases (or use '_')");

    if (enumName.empty() && !hasSome && !hasNone && !hasOk && !hasErr && !hasTrue && !hasFalse)
        codegenError("non-exhaustive match: literal patterns require a wildcard '_' case");
}

llvm::Value *CodeGen::emitPatternTest(const Pattern &pattern,
    llvm::Value *subjectVal, llvm::Type *subjectTy, const std::string &subjectEnumType) {
    llvm::Value *testResult = nullptr;
    std::visit([&](auto &pat) {
        using T = std::decay_t<decltype(pat)>;
        if constexpr (std::is_same_v<T, WildcardPattern>) { // NOLINT(bugprone-branch-clone)
            testResult = llvm::ConstantInt::get(i1Ty_, 1);
        } else if constexpr (std::is_same_v<T, LiteralPattern>) {
            llvm::Value *litVal = emitExpr(*pat.value);
            if (subjectTy == i64Ty_ && litVal->getType() == i64Ty_) {
                testResult = builder_.CreateICmpEQ(subjectVal, litVal, "match.eq");
            } else if (subjectTy == f64Ty_ && litVal->getType() == f64Ty_) {
                testResult = builder_.CreateFCmpOEQ(subjectVal, litVal, "match.feq");
            } else if (subjectTy == i1Ty_ && litVal->getType() == i1Ty_) {
                testResult = builder_.CreateICmpEQ(subjectVal, litVal, "match.beq");
            } else if (subjectTy == ptrTy_ && litVal->getType() == ptrTy_) {
                auto strcmpFn = getStdlibStrcmp();
                llvm::Value *cmp = builder_.CreateCall(strcmpFn, {subjectVal, litVal}, "strcmp");
                testResult = builder_.CreateICmpEQ(cmp, llvm::ConstantInt::get(i32Ty_, 0), "match.streq");
            } else {
                codegenError("match: incompatible types in literal pattern");
            }
        } else if constexpr (std::is_same_v<T, VariablePattern>) {
            testResult = llvm::ConstantInt::get(i1Ty_, 1);
        } else if constexpr (std::is_same_v<T, EnumPattern>) {
            std::string resolvedEnum = pat.enum_name;
            auto enumIt = enum_types_.find(resolvedEnum);
            if (enumIt == enum_types_.end() && !subjectEnumType.empty()) {
                auto ltPos = subjectEnumType.find('<');
                if (ltPos != std::string::npos && subjectEnumType.substr(0, ltPos) == pat.enum_name) {
                    resolvedEnum = subjectEnumType;
                    enumIt = enum_types_.find(resolvedEnum);
                }
            }
            if (enumIt == enum_types_.end())
                codegenError("match: unknown enum '" + pat.enum_name + "'");
            auto varIt = enumIt->second.variants.find(pat.variant_name);
            if (varIt == enumIt->second.variants.end())
                codegenError("match: unknown variant '" + pat.enum_name + "::" + pat.variant_name + "'");
            if (enumIt->second.isADT) {
                llvm::Value *subjectTag = builder_.CreateExtractValue(subjectVal, 0, "adt.tag");
                testResult = builder_.CreateICmpEQ(subjectTag, llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(varIt->second)), "match.adt_eq");
            } else {
                llvm::Value *tag = llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(varIt->second));
                testResult = builder_.CreateICmpEQ(subjectVal, tag, "match.enum_eq");
            }
        } else if constexpr (std::is_same_v<T, EnumConstructorPattern>) {
            std::string resolvedEnum = pat.enum_name;
            auto enumIt = enum_types_.find(resolvedEnum);
            if (enumIt == enum_types_.end() && !subjectEnumType.empty()) {
                auto ltPos = subjectEnumType.find('<');
                if (ltPos != std::string::npos && subjectEnumType.substr(0, ltPos) == pat.enum_name) {
                    resolvedEnum = subjectEnumType;
                    enumIt = enum_types_.find(resolvedEnum);
                }
            }
            if (enumIt == enum_types_.end())
                codegenError("match: unknown enum '" + pat.enum_name + "'");
            if (!enumIt->second.isADT)
                codegenError("match: constructor pattern requires ADT enum, but '" + pat.enum_name + "' is not ADT");
            auto varIt = enumIt->second.variants.find(pat.variant_name);
            if (varIt == enumIt->second.variants.end())
                codegenError("match: unknown variant '" + pat.enum_name + "::" + pat.variant_name + "'");
            llvm::Value *subjectTag = builder_.CreateExtractValue(subjectVal, 0, "adt.tag");
            testResult = builder_.CreateICmpEQ(subjectTag, llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(varIt->second)), "match.adt_eq");
        } else if constexpr (std::is_same_v<T, SomePattern>) {
            if (!isOptionType(subjectTy))
                codegenError("match: Some pattern requires Option type");
            llvm::Value *hasValue = builder_.CreateExtractValue(subjectVal, 0, "has_value");
            testResult = hasValue;
        } else if constexpr (std::is_same_v<T, NonePattern>) {
            if (!isOptionType(subjectTy))
                codegenError("match: None pattern requires Option type");
            llvm::Value *hasValue = builder_.CreateExtractValue(subjectVal, 0, "has_value");
            testResult = builder_.CreateNot(hasValue, "is_none");
        } else if constexpr (std::is_same_v<T, OkPattern>) {
            if (!isResultType(subjectTy))
                codegenError("match: Ok pattern requires Result type");
            llvm::Value *isOk = builder_.CreateExtractValue(subjectVal, 0, "is_ok");
            testResult = isOk;
        } else if constexpr (std::is_same_v<T, ErrPattern>) {
            if (!isResultType(subjectTy))
                codegenError("match: Err pattern requires Result type");
            llvm::Value *isOk = builder_.CreateExtractValue(subjectVal, 0, "is_ok");
            testResult = builder_.CreateNot(isOk, "is_err");
        } else if constexpr (std::is_same_v<T, std::unique_ptr<OrPattern>>) {
            testResult = llvm::ConstantInt::get(i1Ty_, 0);
            for (auto &alt : pat->alternatives) {
                llvm::Value *altResult = emitPatternTest(alt, subjectVal, subjectTy, subjectEnumType);
                testResult = builder_.CreateOr(testResult, altResult, "or.comb");
            }
        }
    }, pattern);
    return testResult;
}

void CodeGen::emitPatternBindings(const Pattern &pattern,
    llvm::AllocaInst *subjectAlloca, llvm::Type *subjectTy,
    const std::string &subjectEnumType) {
    std::visit([&](auto &pat) {
        using T = std::decay_t<decltype(pat)>;
        if constexpr (std::is_same_v<T, VariablePattern>) {
            llvm::Value *sv = builder_.CreateLoad(subjectTy, subjectAlloca, pat.name);
            llvm::AllocaInst *varAlloca = getOrCreateVar(pat.name, subjectTy);
            builder_.CreateStore(sv, varAlloca);
            if (!subjectEnumType.empty())
                getOrCreateMeta(varAlloca).enum_value_type = subjectEnumType;
        } else if constexpr (std::is_same_v<T, SomePattern>) {
            if (pat.binding != "_") {
                llvm::Value *sv = builder_.CreateLoad(subjectTy, subjectAlloca, "opt_val");
                llvm::Value *inner = builder_.CreateExtractValue(sv, 1, "some_val");
                llvm::AllocaInst *varAlloca = getOrCreateVar(pat.binding, inner->getType());
                builder_.CreateStore(inner, varAlloca);
                propagateMeta(subjectAlloca, varAlloca);
            }
        } else if constexpr (std::is_same_v<T, OkPattern>) {
            if (pat.binding != "_") {
                llvm::Value *sv = builder_.CreateLoad(subjectTy, subjectAlloca, "res_val");
                llvm::Value *okVal = builder_.CreateExtractValue(sv, 1, "ok_val");
                llvm::AllocaInst *varAlloca = getOrCreateVar(pat.binding, okVal->getType());
                builder_.CreateStore(okVal, varAlloca);
                propagateMeta(subjectAlloca, varAlloca);
            }
        } else if constexpr (std::is_same_v<T, ErrPattern>) {
            if (pat.binding != "_") {
                llvm::Value *sv = builder_.CreateLoad(subjectTy, subjectAlloca, "res_val");
                llvm::Value *errVal = builder_.CreateExtractValue(sv, 2, "err_val");
                llvm::AllocaInst *varAlloca = getOrCreateVar(pat.binding, errVal->getType());
                builder_.CreateStore(errVal, varAlloca);
            }
        } else if constexpr (std::is_same_v<T, EnumConstructorPattern>) {
            std::string resolvedEnum = pat.enum_name;
            if (!enum_types_.count(resolvedEnum) && !subjectEnumType.empty()) {
                auto ltPos = subjectEnumType.find('<');
                if (ltPos != std::string::npos && subjectEnumType.substr(0, ltPos) == pat.enum_name)
                    resolvedEnum = subjectEnumType;
            }
            auto enumIt = enum_types_.find(resolvedEnum);
            if (enumIt != enum_types_.end()) {
                auto fit = enumIt->second.variantFields.find(pat.variant_name);
                if (fit != enumIt->second.variantFields.end()) {
                    llvm::Value *sv = builder_.CreateLoad(subjectTy, subjectAlloca, "adt.val");
                    llvm::AllocaInst *tmpAlloca = builder_.CreateAlloca(subjectTy, nullptr, "adt.tmp");
                    builder_.CreateStore(sv, tmpAlloca);
                    llvm::Value *payloadPtr = builder_.CreateStructGEP(
                        enumIt->second.adtType, tmpAlloca, 1, "adt.payload");
                    const llvm::DataLayout &dl = mod_->getDataLayout();
                    size_t offset = 0;
                    for (size_t bi = 0; bi < pat.bindings.size() && bi < fit->second.fieldTypes.size(); ++bi) {
                        llvm::Type *fieldTy = fit->second.fieldTypes[bi];
                        const std::string &bindingName = pat.bindings[bi];
                        uint64_t align = dl.getABITypeAlign(fieldTy).value();
                        offset = (offset + align - 1) / align * align;
                        if (bindingName == "_") {
                            offset += dl.getTypeAllocSize(fieldTy);
                            continue;
                        }
                        llvm::Value *fieldPtr = builder_.CreateGEP(
                            llvm::Type::getInt8Ty(*ctx_), payloadPtr,
                            {llvm::ConstantInt::get(i64Ty_, offset)},
                            "adt.bind." + std::to_string(bi));
                        llvm::Value *fieldVal = builder_.CreateLoad(fieldTy, fieldPtr, bindingName);
                        llvm::AllocaInst *bindAlloca = getOrCreateVar(bindingName, fieldTy);
                        builder_.CreateStore(fieldVal, bindAlloca);
                        offset += dl.getTypeAllocSize(fieldTy);
                    }
                }
            }
        }
    }, pattern);
}

// ===== CaseStmt =====

void CodeGen::emitStmt(std::unique_ptr<CaseStmt> &s) {
    emitCoverage(s->loc);
    llvm::Value *subject = emitExpr(*s->subject);
    llvm::Type *subjectTy = subject->getType();

    // --- Exhaustiveness check ---
    std::vector<std::pair<const Pattern*, bool>> armPatterns;
    for (auto &arm : s->arms)
        armPatterns.push_back({&arm.pattern, arm.guard != nullptr});

    std::string subjectEnumTypeForCheck = resolveEnumType(subject);
    checkMatchExhaustiveness(armPatterns, subjectTy, subjectEnumTypeForCheck);

    // --- Code generation: chain of conditional branches ---
    llvm::BasicBlock *matchEndBB = llvm::BasicBlock::Create(*ctx_, "match.end", fn_);

    llvm::AllocaInst *subjectAlloca = builder_.CreateAlloca(subjectTy, nullptr, "match.subject");
    builder_.CreateStore(subject, subjectAlloca);

    const auto &subjectEnumType = subjectEnumTypeForCheck;
    if (!subjectEnumType.empty())
        getOrCreateMeta(subjectAlloca).enum_value_type = subjectEnumType;

    propagateMetaWide(subject, subjectAlloca);

    for (size_t i = 0; i < s->arms.size(); ++i) {
        auto &arm = s->arms[i];
        llvm::BasicBlock *armBodyBB = llvm::BasicBlock::Create(*ctx_, "match.arm.body", fn_);
        llvm::BasicBlock *nextArmBB = (i + 1 < s->arms.size())
            ? llvm::BasicBlock::Create(*ctx_, "match.arm.test", fn_)
            : matchEndBB;

        llvm::Value *subjectVal = builder_.CreateLoad(subjectTy, subjectAlloca, "match.subj");
        llvm::Value *testResult = emitPatternTest(arm.pattern, subjectVal, subjectTy, subjectEnumType);

        if (arm.guard) {
            llvm::BasicBlock *guardBB = llvm::BasicBlock::Create(*ctx_, "match.guard", fn_);
            builder_.CreateCondBr(testResult, guardBB, nextArmBB);
            builder_.SetInsertPoint(guardBB);

            pushScope();
            emitPatternBindings(arm.pattern, subjectAlloca, subjectTy, subjectEnumType);

            llvm::Value *guardVal = emitExpr(*arm.guard);
            guardVal = toBool(guardVal);
            popScope();

            builder_.CreateCondBr(guardVal, armBodyBB, nextArmBB);
        } else {
            builder_.CreateCondBr(testResult, armBodyBB, nextArmBB);
        }

        builder_.SetInsertPoint(armBodyBB);
        emitTraceWhenBranch(static_cast<int>(i), s->loc);
        pushScope();
        emitPatternBindings(arm.pattern, subjectAlloca, subjectTy, subjectEnumType);

        for (auto &stmt : arm.body)
            std::visit([this](auto &st) { emitStmt(st); }, stmt);

        popScope();
        if (!builder_.GetInsertBlock()->getTerminator())
            builder_.CreateBr(matchEndBB);

        if (i + 1 < s->arms.size())
            builder_.SetInsertPoint(nextArmBB);
    }

    builder_.SetInsertPoint(matchEndBB);
}

// ===== CaseExpr =====

llvm::Value *CodeGen::emitExprVariant(const std::unique_ptr<CaseExpr> &e) {
    llvm::Value *subject = emitExpr(*e->subject);
    llvm::Type *subjectTy = subject->getType();

    // --- Exhaustiveness check ---
    std::vector<std::pair<const Pattern*, bool>> armPatterns;
    for (auto &arm : e->arms)
        armPatterns.push_back({&arm.pattern, arm.guard != nullptr});

    std::string subjectEnumType = resolveEnumType(subject);
    checkMatchExhaustiveness(armPatterns, subjectTy, subjectEnumType);

    // --- Code generation with PHI node ---
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "match.expr.merge", fn_);
    std::vector<std::pair<llvm::Value*, llvm::BasicBlock*>> incoming;

    llvm::AllocaInst *subjectAlloca = builder_.CreateAlloca(subjectTy, nullptr, "match.subject");
    builder_.CreateStore(subject, subjectAlloca);

    if (!subjectEnumType.empty())
        getOrCreateMeta(subjectAlloca).enum_value_type = subjectEnumType;
    propagateMetaWide(subject, subjectAlloca);

    llvm::Value *firstVal = nullptr;

    for (size_t i = 0; i < e->arms.size(); ++i) {
        auto &arm = e->arms[i];
        llvm::Value *subjectVal = builder_.CreateLoad(subjectTy, subjectAlloca, "match.subj");
        llvm::Value *testResult = emitPatternTest(arm.pattern, subjectVal, subjectTy, subjectEnumType);

        llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(*ctx_, "match.expr.then", fn_);
        llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "match.expr.next", fn_);

        if (arm.guard) {
            llvm::BasicBlock *guardBB = llvm::BasicBlock::Create(*ctx_, "match.expr.guard", fn_);
            builder_.CreateCondBr(testResult, guardBB, nextBB);
            builder_.SetInsertPoint(guardBB);

            pushScope();
            emitPatternBindings(arm.pattern, subjectAlloca, subjectTy, subjectEnumType);

            llvm::Value *guardVal = emitExpr(*arm.guard);
            guardVal = toBool(guardVal);
            popScope();

            builder_.CreateCondBr(guardVal, thenBB, nextBB);
        } else {
            builder_.CreateCondBr(testResult, thenBB, nextBB);
        }

        builder_.SetInsertPoint(thenBB);
        pushScope();
        emitPatternBindings(arm.pattern, subjectAlloca, subjectTy, subjectEnumType);

        llvm::Value *armVal = emitExpr(*arm.value);
        if (!firstVal) firstVal = armVal;
        else validateBranchTypes(firstVal, armVal, "match expression");

        llvm::BasicBlock *armEndBB = builder_.GetInsertBlock();
        popScope();
        builder_.CreateBr(mergeBB);
        incoming.push_back({armVal, armEndBB});

        builder_.SetInsertPoint(nextBB);
    }

    // After all arms — unreachable (exhaustiveness guaranteed)
    builder_.CreateUnreachable();

    builder_.SetInsertPoint(mergeBB);
    assert(firstVal != nullptr && "match expression must have at least one arm");
    llvm::PHINode *phi = builder_.CreatePHI(firstVal->getType(), static_cast<unsigned>(incoming.size()), "match.expr");
    for (auto &[val, bb] : incoming)
        phi->addIncoming(val, bb);
    propagateMeta(firstVal, phi);
    return phi;
}

// ===== Union type helpers =====

std::vector<std::string> CodeGen::parseUnionComponents(const std::string &typeName) {
    std::vector<std::string> components;
    size_t sepCount = 0;
    for (size_t p = 0; (p = typeName.find(" | ", p)) != std::string::npos; p += 3)
        ++sepCount;
    components.reserve(sepCount + 1);
    size_t start = 0;
    while (start < typeName.size()) {
        size_t pos = typeName.find(" | ", start);
        if (pos == std::string::npos) {
            std::string comp = typeName.substr(start);
            size_t s = comp.find_first_not_of(' ');
            size_t e = comp.find_last_not_of(' ');
            if (s != std::string::npos)
                components.push_back(comp.substr(s, e - s + 1));
            break;
        }
        std::string comp = typeName.substr(start, pos - start);
        size_t s = comp.find_first_not_of(' ');
        size_t e = comp.find_last_not_of(' ');
        if (s != std::string::npos)
            components.push_back(comp.substr(s, e - s + 1));
        start = pos + 3;
    }
    return components;
}

static std::string joinSortedUnion(std::vector<std::string> &components) {
    std::sort(components.begin(), components.end());
    std::string result;
    for (size_t i = 0; i < components.size(); ++i) {
        if (i > 0) result += " | ";
        result += components[i];
    }
    return result;
}

std::string CodeGen::normalizeUnionType(const std::string &typeName) {
    auto components = parseUnionComponents(typeName);
    return joinSortedUnion(components);
}

bool CodeGen::isUnionType(const std::string &typeName) {
    return typeName.find(" | ") != std::string::npos;
}

std::string CodeGen::flattenUnionWithAliases(const std::string &typeName) {
    std::string resolved = resolveTypeAlias(typeName);
    if (!isUnionType(resolved))
        return resolved;
    // Literal unions take a separate codepath in resolveType and never
    // contain alias components.
    if (isLiteralUnionType(resolved))
        return normalizeUnionType(resolved);

    std::vector<std::string> out;
    std::unordered_set<std::string> seenLeaves;
    // Tracks union strings whose expansion has already started, so we
    // terminate on both genuine cycles (`type A = B | int; type B = A | str`)
    // and redundant references (`type C = B | A` where B already expands A).
    std::unordered_set<std::string> visitedUnions;

    std::vector<std::string> worklist;
    worklist.push_back(resolved);
    visitedUnions.insert(resolved);

    while (!worklist.empty()) {
        std::string current = std::move(worklist.back());
        worklist.pop_back();
        for (auto &c : parseUnionComponents(current)) {
            std::string r = resolveTypeAlias(c);
            if (!isUnionType(r) || isLiteralUnionType(r)) {
                if (seenLeaves.insert(r).second)
                    out.push_back(r);
            } else if (visitedUnions.insert(r).second) {
                worklist.push_back(std::move(r));
            }
        }
    }

    // Every expansion path cycled back into an already-visited union and
    // no concrete leaf was ever found (e.g. `type A = B | C; type B = A;
    // type C = A`). Reject instead of returning an empty type name.
    if (out.empty())
        codegenError("Circular type alias: " + typeName);

    return joinSortedUnion(out);
}

void CodeGen::storeFlattenedUnionMeta(llvm::Value *target,
                                      const std::string &typeName) {
    std::string flattened = flattenUnionWithAliases(typeName);
    // Skip if dedupe collapsed to a single leaf, or if the canonical form
    // is a literal union (e.g. `"N" | "S"`). Neither produces a
    // union_type_info_ entry, so storing them would crash downstream
    // wrapInUnion lookups.
    if (!isUnionType(flattened) || isLiteralUnionType(flattened))
        return;
    getOrCreateMeta(target).union_value_type = std::move(flattened);
}

llvm::Value *CodeGen::wrapInUnion(llvm::Value *val, const std::string &unionTypeName) {
    std::string norm = flattenUnionWithAliases(unionTypeName);
    auto infoIt = union_type_info_.find(norm);
    if (infoIt == union_type_info_.end()) {
        resolveType(norm);
        infoIt = union_type_info_.find(norm);
    }
    auto &info = infoIt->second;
    size_t tagIdx = std::string::npos;
    // When multiple ptr-backed variants share the same LLVM type (e.g.
    // `List<int> | Map<str, int>` — both `ptr`, or `List<int> | List<str>` —
    // both `ptr` with the same kind), we must disambiguate by the value's
    // collection/function metadata.  Otherwise ptr values always bind to the
    // first ptr variant and are mis-dispatched at runtime.
    const ValueMetadata *meta =
        (val->getType() == ptrTy_) ? getMeta(val) : nullptr;
    // Pass 1: exact canonical type-name match (handles same-kind variants like
    // `List<int> | List<str>`).
    if (meta) {
        std::string canonical = buildTypeNameFromMeta(val);
        if (!canonical.empty()) {
            for (size_t i = 0; i < info.componentTypes.size(); ++i) {
                if (info.componentTypes[i] == val->getType() &&
                    info.componentNames[i] == canonical) {
                    tagIdx = i;
                    break;
                }
            }
        }
    }
    // Pass 2: coarse kind match (handles `List<int> | Map<str, int>` when the
    // canonical name couldn't be built — e.g. literals without annotations).
    if (tagIdx == std::string::npos && meta) {
        for (size_t i = 0; i < info.componentTypes.size(); ++i) {
            if (info.componentTypes[i] != val->getType()) continue;
            const auto &compName = info.componentNames[i];
            if ((meta->map_key || meta->map_value) && isMapTypeName(compName)) { tagIdx = i; break; }
            if (meta->set_elem && isSetTypeName(compName))                     { tagIdx = i; break; }
            if (meta->list_elem && isListTypeName(compName))                   { tagIdx = i; break; }
            if (meta->fn_type_info && isFunctionTypeName(compName))            { tagIdx = i; break; }
        }
    }
    // Fallback: first variant with matching LLVM type.
    if (tagIdx == std::string::npos) {
        for (size_t i = 0; i < info.componentTypes.size(); ++i) {
            if (info.componentTypes[i] == val->getType()) { tagIdx = i; break; }
        }
    }
    if (tagIdx == std::string::npos)
        codegenError("type is not in union " + norm);

    llvm::AllocaInst *tmp = builder_.CreateAlloca(info.llvmType, nullptr, "union.tmp");
    auto *tagPtr = builder_.CreateStructGEP(info.llvmType, tmp, 0, "union.tag");
    builder_.CreateStore(llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(tagIdx)), tagPtr);
    auto *dataPtr = builder_.CreateStructGEP(info.llvmType, tmp, 1, "union.data");
    builder_.CreateStore(val, dataPtr);
    return builder_.CreateLoad(info.llvmType, tmp, "union.val");
}

// ===== exit(code) =====

void CodeGen::emitExit(const std::vector<ExprPtr> &args) {
    if (args.size() != 1)
        codegenError("exit() takes exactly 1 argument");
    llvm::Value *code = emitExpr(*args[0]);
    if (!code->getType()->isIntegerTy())
        codegenError("exit() argument must be an integer");
    if (code->getType() != i32Ty_)
        code = builder_.CreateIntCast(code, i32Ty_, true, "exit_code");
    auto exitFn = getStdlibExit();
    builder_.CreateCall(exitFn, {code});
    builder_.CreateUnreachable();
    // Callers may continue emitting statements after exit() — e.g. the body
    // of `emitStmt(CallStmt)` routes `exit(0)` through `builtins_["exit"]`
    // and then goes back to the statement-emission loop. Switch to a fresh
    // dead block so trailing IR does not land on a terminated block and trip
    // LLVM verification (#821). LLVM DCE removes the unreachable block.
    llvm::BasicBlock *deadBB =
        llvm::BasicBlock::Create(*ctx_, "exit.dead", fn_);
    builder_.SetInsertPoint(deadBB);
}

} // namespace ry
