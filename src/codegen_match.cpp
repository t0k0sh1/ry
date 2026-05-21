#include "ry/codegen.hpp"
#include "ry/diagnostic.hpp"
#include "ry/stdlib_registry.hpp"
#include <cassert>


namespace ry {

// ===== Shared match helpers =====

// When an EnumConstructorPattern has a single TuplePattern whose element count equals the
// variant's field count, the user wrote e.g. Event::Click((x, y)) — the outer parens form a
// tuple pattern that serves as sugar for matching all fields individually.  Unwrap it so the
// recursive emitPatternTest / emitPatternBindings calls receive one element per field.
static const std::vector<Pattern> *unwrapEnumPayloadTuple(const std::vector<Pattern> &bindings,
                                                          size_t fieldCount) {
    if (bindings.size() == 1) {
        if (auto *tp = std::get_if<std::unique_ptr<TuplePattern>>(&bindings[0])) {
            if ((*tp)->elements.size() == fieldCount)
                return &(*tp)->elements;
        }
    }
    return &bindings;
}

std::string CodeGen::resolveEnumType(llvm::Value *val) const {
    auto *meta = getMeta(val);
    if (meta && !meta->enum_value_type.empty())
        return meta->enum_value_type;
    return {};
}

// Returns a broad Ry source-level type name for a match subject: returns
// subjectEnumName unchanged when non-empty, otherwise reconstructs from LLVM
// type (Option<T>, Result<T,E>, tuple). Used for structural pattern checks —
// NOT for ARC payload extraction, since reconstruction is lossy for nested
// generics (Option<List<int>> → "Option<str>").
std::string CodeGen::resolveSubjectSourceTypeName(const std::string &subjectEnumName, llvm::Type *subjectTy) {
    if (!subjectEnumName.empty())
        return subjectEnumName;
    auto *st = llvm::dyn_cast<llvm::StructType>(subjectTy);
    if (!st) return {};
    if (isOptionType(st))
        return "Option<" + reverseResolveTypeName(st->getElementType(1)) + ">";
    if (isResultType(st))
        return "Result<" + reverseResolveTypeName(st->getElementType(1)) +
               ", " + reverseResolveTypeName(st->getElementType(2)) + ">";
    if (isTupleStructType(st)) {
        std::string sig = "(";
        for (unsigned i = 0; i < st->getNumElements(); ++i) {
            if (i) sig += ", ";
            sig += reverseResolveTypeName(st->getElementType(i));
        }
        return sig + ")";
    }
    return {};
}

std::string CodeGen::filterToEnumOnly(const std::string &typeSig) {
    const std::string resolved = resolveTypeAlias(typeSig);
    return enum_types_.count(resolved) ? resolved : std::string{};
}

// tmp is an intermediate field alloca that is NOT in scope_stack_, so there
// is no matching release — the recursive VariablePattern leaf owns the refcount.
void CodeGen::markFieldAllocaArcManaged(llvm::AllocaInst *tmp, llvm::Type *ty, const std::string &typeSig) {
    if (ty != ptrTy_) return;
    CollectionKind fk;
    if (fieldTypeIsArcManaged(typeSig, &fk)) {
        markArcManaged(tmp);
        if (fk == CollectionKind::Str)
            arc_str_managed_vars_.insert(tmp);
    }
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

// Strips the outer parens from a Ry tuple type signature like "(int, List<str>)" and
// returns the resolved, trimmed per-element type names.  Returns an empty vector when
// the signature is absent or not a parenthesised tuple string.
std::vector<std::string> CodeGen::splitTupleSig(const std::string &tupleTypeSig) {
    if (tupleTypeSig.empty()) return {};
    const std::string resolved = resolveTypeAlias(tupleTypeSig);
    if (resolved.size() < 2 || resolved.front() != '(' || resolved.back() != ')') return {};
    std::vector<std::string> parts = splitTypeArgs(resolved.substr(1, resolved.size() - 2));
    for (auto &p : parts) p = trimTypeNameSpaces(p);
    return parts;
}

void CodeGen::checkMatchExhaustiveness(
    const std::vector<std::pair<const Pattern*, bool>> &armPatterns,
    llvm::Type *subjectTy, const std::string &subjectEnumName) {

    // Recursively determines whether a pattern is irrefutable (always matches).
    // A TuplePattern is irrefutable iff every element is irrefutable.
    // EnumConstructorPattern is not irrefutable: it has a tag discriminator.
    std::function<bool(const Pattern &)> isIrrefutable = [&](const Pattern &p) -> bool {
        if (std::holds_alternative<WildcardPattern>(p)) return true;
        if (std::holds_alternative<VariablePattern>(p)) return true;
        if (auto *tp = std::get_if<std::unique_ptr<TuplePattern>>(&p))
            return std::all_of((*tp)->elements.begin(), (*tp)->elements.end(), isIrrefutable);
        // RecordPattern is irrefutable iff every element is irrefutable (records have one shape).
        if (auto *rp = std::get_if<std::unique_ptr<RecordPattern>>(&p))
            return std::all_of((*rp)->elements.begin(), (*rp)->elements.end(), isIrrefutable);
        return false;
    };
    for (auto &[pat, hasGuard] : armPatterns) {
        if (hasGuard) continue;
        if (isIrrefutable(*pat)) return;
        if (auto *op = std::get_if<std::unique_ptr<OrPattern>>(pat)) {
            for (auto &alt : (*op)->alternatives) {
                if (isIrrefutable(alt)) return;
            }
        }
    }

    // Check enum exhaustiveness
    std::string enumName;
    for (auto &[pat, hasGuard] : armPatterns) {
        if (auto *ep = std::get_if<EnumPattern>(pat)) {
            enumName = ep->enum_name;
            break;
        }
        if (auto *ecp = std::get_if<std::unique_ptr<EnumConstructorPattern>>(pat)) {
            enumName = (*ecp)->enum_name;
            break;
        }
        if (auto *op = std::get_if<std::unique_ptr<OrPattern>>(pat)) {
            for (auto &alt : (*op)->alternatives) {
                if (auto *ep2 = std::get_if<EnumPattern>(&alt)) {
                    enumName = ep2->enum_name;
                    break;
                }
                if (auto *ecp2 = std::get_if<std::unique_ptr<EnumConstructorPattern>>(&alt)) {
                    enumName = (*ecp2)->enum_name;
                    break;
                }
            }
            if (!enumName.empty()) break;
        }
    }
    if (!enumName.empty()) {
        if (!findEnumType(enumName) && !subjectEnumName.empty()) {
            auto ltPos = subjectEnumName.find('<');
            if (ltPos != std::string::npos && subjectEnumName.substr(0, ltPos) == enumName)
                enumName = subjectEnumName;
        }
        if (auto *einfoPtr = findEnumType(enumName)) {
            auto &info = *einfoPtr;
            std::unordered_set<std::string> covered;
            for (auto &[pat, hasGuard] : armPatterns) {
                if (!hasGuard) {
                    if (auto *ep = std::get_if<EnumPattern>(pat))
                        covered.insert(ep->variant_name);
                    if (auto *ecp = std::get_if<std::unique_ptr<EnumConstructorPattern>>(pat))
                        covered.insert((*ecp)->variant_name);
                    if (auto *op = std::get_if<std::unique_ptr<OrPattern>>(pat)) {
                        for (auto &alt : (*op)->alternatives) {
                            if (auto *ep2 = std::get_if<EnumPattern>(&alt))
                                covered.insert(ep2->variant_name);
                            if (auto *ecp2 = std::get_if<std::unique_ptr<EnumConstructorPattern>>(&alt))
                                covered.insert((*ecp2)->variant_name);
                        }
                    }
                }
            }
            for (auto &[vname, _] : info.variants) {
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
    llvm::Value *subjectVal, llvm::Type *subjectTy,
    const std::string &subjectEnumName, const std::string &subjectSourceTypeName) {
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
            auto *enumInfo = findEnumType(resolvedEnum);
            if (!enumInfo && !subjectEnumName.empty()) {
                auto ltPos = subjectEnumName.find('<');
                if (ltPos != std::string::npos && subjectEnumName.substr(0, ltPos) == pat.enum_name) {
                    resolvedEnum = subjectEnumName;
                    enumInfo = findEnumType(resolvedEnum);
                }
            }
            if (!enumInfo)
                codegenError("match: unknown enum '" + pat.enum_name + "'");
            auto varIt = enumInfo->variants.find(pat.variant_name);
            if (varIt == enumInfo->variants.end())
                codegenError("match: unknown variant '" + pat.enum_name + "::" + pat.variant_name + "'");
            if (enumInfo->isADT) {
                llvm::Value *subjectTag = builder_.CreateExtractValue(subjectVal, 0, "adt.tag");
                testResult = builder_.CreateICmpEQ(subjectTag, llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(varIt->second)), "match.adt_eq");
            } else {
                llvm::Value *tag = llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(varIt->second));
                testResult = builder_.CreateICmpEQ(subjectVal, tag, "match.enum_eq");
            }
        } else if constexpr (std::is_same_v<T, std::unique_ptr<EnumConstructorPattern>>) {
            std::string resolvedEnum = pat->enum_name;
            auto *enumInfo = findEnumType(resolvedEnum);
            if (!enumInfo && !subjectEnumName.empty()) {
                auto ltPos = subjectEnumName.find('<');
                if (ltPos != std::string::npos && subjectEnumName.substr(0, ltPos) == pat->enum_name) {
                    resolvedEnum = subjectEnumName;
                    enumInfo = findEnumType(resolvedEnum);
                }
            }
            if (!enumInfo)
                codegenError("match: unknown enum '" + pat->enum_name + "'");
            if (!enumInfo->isADT)
                codegenError("match: constructor pattern requires ADT enum, but '" + pat->enum_name + "' is not ADT");
            auto varIt = enumInfo->variants.find(pat->variant_name);
            if (varIt == enumInfo->variants.end())
                codegenError("match: unknown variant '" + pat->enum_name + "::" + pat->variant_name + "'");
            llvm::Value *subjectTag = builder_.CreateExtractValue(subjectVal, 0, "adt.tag");
            testResult = builder_.CreateICmpEQ(subjectTag, llvm::ConstantInt::get(i64Ty_, static_cast<uint64_t>(varIt->second)), "match.adt_eq");
            auto fit = enumInfo->variantFields.find(pat->variant_name);
            if (fit != enumInfo->variantFields.end() && !pat->bindings.empty()) {
                const std::vector<Pattern> *fieldPats =
                    unwrapEnumPayloadTuple(pat->bindings, fit->second.fieldTypes.size());
                // Branch on tag match so payload loads only run when the tag is correct.
                // Unconditional payload loads would reinterpret wrong-typed bytes (e.g.
                // an int payload read as a str pointer) and could crash nested tests like strcmp.
                llvm::BasicBlock *tagMatchBB = builder_.GetInsertBlock();
                llvm::Function *fn = tagMatchBB->getParent();
                auto *payloadBB = llvm::BasicBlock::Create(*ctx_, "ecp.payload", fn);
                auto *mergeBB = llvm::BasicBlock::Create(*ctx_, "ecp.merge", fn);
                builder_.CreateCondBr(testResult, payloadBB, mergeBB);

                builder_.SetInsertPoint(payloadBB);
                llvm::AllocaInst *tmpAlloca = builder_.CreateAlloca(subjectTy, nullptr, "ecp.test.tmp");
                builder_.CreateStore(subjectVal, tmpAlloca);
                llvm::Value *payloadPtr = builder_.CreateStructGEP(
                    enumInfo->adtType, tmpAlloca, 1, "ecp.test.payload");
                const llvm::DataLayout &dl = mod_->getDataLayout();
                size_t offset = 0;
                llvm::Value *fieldsMatch = llvm::ConstantInt::get(i1Ty_, 1);
                for (size_t i = 0; i < fieldPats->size() && i < fit->second.fieldTypes.size(); ++i) {
                    llvm::Type *fieldTy = fit->second.fieldTypes[i];
                    uint64_t align = dl.getABITypeAlign(fieldTy).value();
                    offset = (offset + align - 1) / align * align;
                    llvm::Value *fieldPtr = builder_.CreateGEP(
                        llvm::Type::getInt8Ty(*ctx_), payloadPtr,
                        {llvm::ConstantInt::get(i64Ty_, offset)},
                        "ecp.test.field." + std::to_string(i));
                    llvm::Value *fieldVal = builder_.CreateLoad(fieldTy, fieldPtr, "ecp.test.fval");
                    const std::string &fieldTypeName = (i < fit->second.fieldTypeNames.size())
                        ? fit->second.fieldTypeNames[i] : std::string{};
                    llvm::Value *sub = emitPatternTest((*fieldPats)[i], fieldVal, fieldTy,
                                                      filterToEnumOnly(fieldTypeName), fieldTypeName);
                    fieldsMatch = builder_.CreateAnd(fieldsMatch, sub, "ecp.test.and");
                    offset += dl.getTypeAllocSize(fieldTy);
                }
                llvm::BasicBlock *payloadEndBB = builder_.GetInsertBlock();
                builder_.CreateBr(mergeBB);

                builder_.SetInsertPoint(mergeBB);
                llvm::PHINode *phi = builder_.CreatePHI(i1Ty_, 2, "ecp.final");
                phi->addIncoming(llvm::ConstantInt::get(i1Ty_, 0), tagMatchBB);
                phi->addIncoming(fieldsMatch, payloadEndBB);
                testResult = phi;
            }
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
                llvm::Value *altResult = emitPatternTest(alt, subjectVal, subjectTy,
                                                        subjectEnumName, subjectSourceTypeName);
                testResult = builder_.CreateOr(testResult, altResult, "or.comb");
            }
        } else if constexpr (std::is_same_v<T, std::unique_ptr<TuplePattern>>) {
            auto *sTy = llvm::dyn_cast<llvm::StructType>(subjectTy);
            // #1156: structural reject of non-tuple LLVM structs (Option, Result,
            // record, ADT enum, union, error). Fires even when subjectSourceTypeName is
            // empty, closing the silent-destructure path where Option<T> = {i1, T}
            // silently passed the 2-arity check and produced wrong IR.
            if (!sTy || !isTupleStructType(sTy))
                codegenError("case: tuple pattern applied to non-tuple subject");
            const std::vector<std::string> elemSigs = splitTupleSig(subjectSourceTypeName);
            if (sTy->getNumElements() != pat->elements.size())
                codegenError("case: tuple pattern arity mismatch: subject has " +
                             std::to_string(sTy->getNumElements()) +
                             " elements, pattern has " +
                             std::to_string(pat->elements.size()));
            testResult = llvm::ConstantInt::get(i1Ty_, 1);
            for (size_t i = 0; i < pat->elements.size(); ++i) {
                llvm::Value *elem = builder_.CreateExtractValue(subjectVal, static_cast<unsigned>(i), "tup.elem");
                llvm::Type  *elemTy = sTy->getElementType(static_cast<unsigned>(i));
                const std::string elemSig = (i < elemSigs.size()) ? elemSigs[i] : std::string{};
                llvm::Value *sub = emitPatternTest(pat->elements[i], elem, elemTy, elemSig, elemSig);
                testResult = builder_.CreateAnd(testResult, sub, "tup.and");
            }
        } else if constexpr (std::is_same_v<T, std::unique_ptr<RecordPattern>>) {
            const std::string resolvedName = resolveTypeAlias(pat->name);
            auto sit = record_types_.find(resolvedName);
            if (sit == record_types_.end()) {
                if (enum_types_.count(resolvedName))
                    codegenError("case: '" + pat->name +
                                 "' is an enum, not a record; use '::' for enum constructor patterns");
                codegenError("case: unknown record type '" + pat->name + "' in pattern");
            }
            // When Ry type metadata is available, verify subject is this record type.
            const std::string resolvedSubject = subjectSourceTypeName.empty()
                ? std::string{} : resolveTypeAlias(subjectSourceTypeName);
            if (!resolvedSubject.empty() && resolvedSubject != resolvedName)
                codegenError("case: record pattern '" + pat->name +
                             "' applied to subject of type '" + subjectSourceTypeName + "'");
            const RecordInfo &info = sit->second;
            if (subjectTy != info.llvmType)
                codegenError("case: record pattern '" + pat->name + "' applied to non-record subject");
            if (info.fields.size() != pat->elements.size())
                codegenError("case: record pattern '" + pat->name + "' has " +
                             std::to_string(pat->elements.size()) + " element(s) but record has " +
                             std::to_string(info.fields.size()) + " field(s)");
            testResult = llvm::ConstantInt::get(i1Ty_, 1);
            for (size_t i = 0; i < pat->elements.size(); ++i) {
                llvm::Value *elem = builder_.CreateExtractValue(subjectVal, static_cast<unsigned>(i), "rec.elem");
                llvm::Type  *elemTy = info.llvmType->getElementType(static_cast<unsigned>(i));
                const std::string elemSig = info.fields[i].type->toString();
                llvm::Value *sub = emitPatternTest(pat->elements[i], elem, elemTy, elemSig, elemSig);
                testResult = builder_.CreateAnd(testResult, sub, "rec.and");
            }
        }
    }, pattern);
    return testResult;
}

// Extract the Nth type argument (0-indexed) from a generic type string like
// "Option<T>", "T?" (OptionalType shorthand for Option<T>), or "Result<T, E>".
// Returns "" when the string does not start with `prefix` or the index is out
// of range.  Delegates to splitTypeArgs so that both angle-bracket and
// parenthesis depth are tracked correctly (handles function types such as
// "Option<(int, str) -> bool>").
static std::string extractGenericTypeArg(const std::string &typeStr,
                                          const std::string &prefix,
                                          size_t argIdx) {
    // T? suffix is OptionalType::toString() shorthand for Option<T> (#1003, #1015).
    // Only Option has a shorthand form; Result<T, E> does not.
    if (prefix == "Option<" && typeStr.size() > 1 && typeStr.back() == '?') {
        if (argIdx != 0)
            return {};
        return CodeGen::trimTypeNameSpaces(typeStr.substr(0, typeStr.size() - 1));
    }
    if (typeStr.size() <= prefix.size() ||
        typeStr.compare(0, prefix.size(), prefix) != 0 ||
        typeStr.back() != '>')
        return {};
    const std::string inner = typeStr.substr(prefix.size(),
                                              typeStr.size() - prefix.size() - 1);
    const auto parts = CodeGen::splitTypeArgs(inner);
    if (argIdx >= parts.size()) return {};
    return CodeGen::trimTypeNameSpaces(parts[argIdx]);
}

void CodeGen::maybeRegisterTaggedUnionSubject(llvm::AllocaInst *subjectAlloca,
                                                llvm::Type *subjectTy,
                                                const std::string &subjectEnumName) {
    // #1640: Tag-driven release at scope exit for `case` subject Result/Option
    // struct allocas. Without this registration, payload retains in
    // buildOkValue / buildErrValue / buildSomeValue go un-balanced and leak
    // one ARC header per active payload slot per case evaluation.
    if (!subjectAlloca || !subjectTy) return;
    if (!isResultType(subjectTy) && !isOptionType(subjectTy)) return;

    // Read the lossless source_type_name (#1638) stamped at the
    // Result/Option/T? construction site. Without it, ARC classification
    // would have to fall back to lossy reverseResolveTypeName which #1156
    // forbids. If the channel is empty (e.g. unannotated complex literal
    // shape that propagateTypeMeta did not stamp), skip registration to
    // avoid heap corruption from misclassification.
    std::string sourceTypeName;
    if (auto *m = getMeta(subjectAlloca))
        sourceTypeName = m->source_type_name;
    if (sourceTypeName.empty() && !subjectEnumName.empty())
        sourceTypeName = resolveTypeAlias(subjectEnumName);
    if (sourceTypeName.empty()) return;

    // Determine if any active slot is ARC-managed; if not, no IR cost path.
    std::string resolvedSource = resolveTypeAlias(sourceTypeName);
    std::string okName, errName;
    const bool isOpt = isOptionType(subjectTy);
    if (isOpt && resolvedSource.size() > 1 && resolvedSource.back() == '?') {
        okName = trimTypeNameSpaces(
            resolvedSource.substr(0, resolvedSource.size() - 1));
    } else {
        std::string head;
        std::vector<std::string> innerArgs;
        if (splitGenericTypeName(resolvedSource, head, innerArgs)) {
            if (head == "Result") {
                if (!innerArgs.empty()) okName = trimTypeNameSpaces(innerArgs[0]);
                if (innerArgs.size() >= 2) errName = trimTypeNameSpaces(innerArgs[1]);
            } else if (head == "Option") {
                if (!innerArgs.empty()) okName = trimTypeNameSpaces(innerArgs[0]);
            }
        }
    }

    CollectionKind dummyKind = CollectionKind::List;
    const bool okArc = !okName.empty() && fieldTypeIsArcManaged(okName, &dummyKind);
    const bool errArc = !errName.empty() && fieldTypeIsArcManaged(errName, &dummyKind);
    if (!okArc && !errArc) return;

    // Register so emitScopeCleanupToDepth visits the alloca and dispatches
    // to emitTaggedUnionRelease. The synthetic name guarantees no collision
    // with any user-declared variable.
    arc_tagged_union_vars_[subjectAlloca] = sourceTypeName;
    static unsigned counter = 0;
    std::string syntheticName =
        "$match_subject_" + std::to_string(counter++);
    if (!scope_stack_.empty())
        scope_stack_.back()[syntheticName] = subjectAlloca;
}

void CodeGen::emitPatternBindings(const Pattern &pattern,
    llvm::AllocaInst *subjectAlloca, llvm::Type *subjectTy,
    const std::string &subjectEnumName, const std::string &subjectSourceTypeName) {
    std::visit([&](auto &pat) {
        using T = std::decay_t<decltype(pat)>;
        if constexpr (std::is_same_v<T, VariablePattern>) {
            llvm::Value *sv = builder_.CreateLoad(subjectTy, subjectAlloca, pat.name);
            llvm::AllocaInst *varAlloca = getOrCreateVar(pat.name, subjectTy);
            builder_.CreateStore(sv, varAlloca);
            propagateMeta(subjectAlloca, varAlloca);
            // Defence-in-depth: subjectEnumName may carry field-type names from recursive calls.
            if (!subjectEnumName.empty() &&
                    enum_types_.count(resolveTypeAlias(subjectEnumName)))
                getOrCreateMeta(varAlloca).enum_value_type = subjectEnumName;
            emitPatternBindingArc(sv, varAlloca, subjectSourceTypeName);
        } else if constexpr (std::is_same_v<T, SomePattern>) {
            if (pat.binding != "_") {
                llvm::Value *sv = builder_.CreateLoad(subjectTy, subjectAlloca, "opt_val");
                llvm::Value *inner = builder_.CreateExtractValue(sv, 1, "some_val");
                llvm::AllocaInst *varAlloca = getOrCreateVar(pat.binding, inner->getType());
                builder_.CreateStore(inner, varAlloca);
                // #1638: when the lossless source_type_name channel is set
                // (typically by an explicit annotation), prefer it so the
                // binding's metadata is rebuilt from the binding's actual
                // type — avoiding the wrong-side metadata leak that
                // `propagateMeta(subject, var)` introduces for typed
                // `Result<ARC-type, str>` Err bindings. When unset (e.g.
                // `Some([1,2,3])` without annotation), the subject alloca
                // only carries the Some-side metadata anyway, so a bulk
                // copy is correct and preserves pre-#1638 behavior.
                std::string subjectSourceName;
                if (auto *sm = getMeta(subjectAlloca))
                    subjectSourceName = sm->source_type_name;
                const std::string lookupName = !subjectSourceName.empty()
                    ? subjectSourceName
                    : resolveTypeAlias(subjectEnumName);
                const std::string innerSig = extractGenericTypeArg(lookupName, "Option<", 0);
                if (!innerSig.empty())
                    propagateTypeMeta(innerSig, varAlloca);
                else
                    propagateMeta(subjectAlloca, varAlloca);
                emitPatternBindingArc(inner, varAlloca, innerSig);
            }
        } else if constexpr (std::is_same_v<T, OkPattern>) {
            if (pat.binding != "_") {
                llvm::Value *sv = builder_.CreateLoad(subjectTy, subjectAlloca, "res_val");
                llvm::Value *okVal = builder_.CreateExtractValue(sv, 1, "ok_val");
                llvm::AllocaInst *varAlloca = getOrCreateVar(pat.binding, okVal->getType());
                builder_.CreateStore(okVal, varAlloca);
                // #1638: see SomePattern arm above for the rationale.
                std::string subjectSourceName;
                if (auto *sm = getMeta(subjectAlloca))
                    subjectSourceName = sm->source_type_name;
                const std::string lookupName = !subjectSourceName.empty()
                    ? subjectSourceName
                    : resolveTypeAlias(subjectEnumName);
                const std::string innerSig = extractGenericTypeArg(lookupName, "Result<", 0);
                if (!innerSig.empty())
                    propagateTypeMeta(innerSig, varAlloca);
                else
                    propagateMeta(subjectAlloca, varAlloca);
                emitPatternBindingArc(okVal, varAlloca, innerSig);
            }
        } else if constexpr (std::is_same_v<T, ErrPattern>) {
            if (pat.binding != "_") {
                llvm::Value *sv = builder_.CreateLoad(subjectTy, subjectAlloca, "res_val");
                llvm::Value *errVal = builder_.CreateExtractValue(sv, 2, "err_val");
                llvm::AllocaInst *varAlloca = getOrCreateVar(pat.binding, errVal->getType());
                builder_.CreateStore(errVal, varAlloca);
                // #1638: root cause for the typed `Result<List<int>, str>`
                // case — the lossless `source_type_name = "Result<List<int>, str>"`
                // lets us extract `innerSig = "str"` and rebuild only the
                // Err side's metadata, so `isStringValue(e)` is true,
                // `"got: " + e` typechecks, and `f"got: {e}"` reads a valid
                // str representation. Falls back to the legacy
                // `propagateMeta` bulk copy for unannotated subjects.
                std::string subjectSourceName;
                if (auto *sm = getMeta(subjectAlloca))
                    subjectSourceName = sm->source_type_name;
                const std::string lookupName = !subjectSourceName.empty()
                    ? subjectSourceName
                    : resolveTypeAlias(subjectEnumName);
                const std::string innerSig = extractGenericTypeArg(lookupName, "Result<", 1);
                if (!innerSig.empty())
                    propagateTypeMeta(innerSig, varAlloca);
                else
                    propagateMeta(subjectAlloca, varAlloca);
                emitPatternBindingArc(errVal, varAlloca, innerSig);
            }
        } else if constexpr (std::is_same_v<T, std::unique_ptr<TuplePattern>>) {
            llvm::Value *loaded = builder_.CreateLoad(subjectTy, subjectAlloca, "tup.load");
            auto *sTy = llvm::cast<llvm::StructType>(subjectTy);
            const std::vector<std::string> elemSigs = splitTupleSig(subjectSourceTypeName);
            assert(sTy->getNumElements() == pat->elements.size() &&
                   "TuplePattern arity must be verified by emitPatternTest before binding");
            for (size_t i = 0; i < pat->elements.size(); ++i) {
                llvm::Value *elem = builder_.CreateExtractValue(loaded, static_cast<unsigned>(i), "tup.bind");
                llvm::Type  *elemTy = sTy->getElementType(static_cast<unsigned>(i));
                llvm::AllocaInst *tmp = builder_.CreateAlloca(elemTy, nullptr, "tup.bind.alloca");
                builder_.CreateStore(elem, tmp);
                const std::string elemSig = (i < elemSigs.size()) ? elemSigs[i] : std::string{};
                if (!elemSig.empty())
                    propagateTypeMeta(elemSig, tmp);
                markFieldAllocaArcManaged(tmp, elemTy, elemSig);
                emitPatternBindings(pat->elements[i], tmp, elemTy, filterToEnumOnly(elemSig), elemSig);
            }
        } else if constexpr (std::is_same_v<T, std::unique_ptr<RecordPattern>>) {
            const std::string resolvedName = resolveTypeAlias(pat->name);
            auto sit = record_types_.find(resolvedName);
            if (sit == record_types_.end()) return; // error already reported in emitPatternTest
            llvm::Value *loaded = builder_.CreateLoad(subjectTy, subjectAlloca, "rec.load");
            const RecordInfo &info = sit->second;
            for (size_t i = 0; i < pat->elements.size() && i < info.fields.size(); ++i) {
                llvm::Value *elem = builder_.CreateExtractValue(loaded, static_cast<unsigned>(i), "rec.bind");
                llvm::Type  *elemTy = info.llvmType->getElementType(static_cast<unsigned>(i));
                llvm::AllocaInst *tmp = builder_.CreateAlloca(elemTy, nullptr, "rec.bind.alloca");
                builder_.CreateStore(elem, tmp);
                const std::string elemSig = info.fields[i].type->toString();
                if (!elemSig.empty())
                    propagateTypeMeta(elemSig, tmp);
                markFieldAllocaArcManaged(tmp, elemTy, elemSig);
                emitPatternBindings(pat->elements[i], tmp, elemTy, filterToEnumOnly(elemSig), elemSig);
            }
        } else if constexpr (std::is_same_v<T, std::unique_ptr<EnumConstructorPattern>>) {
            std::string resolvedEnum = pat->enum_name;
            if (!findEnumType(resolvedEnum) && !subjectEnumName.empty()) {
                auto ltPos = subjectEnumName.find('<');
                if (ltPos != std::string::npos && subjectEnumName.substr(0, ltPos) == pat->enum_name)
                    resolvedEnum = subjectEnumName;
            }
            // emitPatternTest already validated the enum; skip silently if lookup fails here.
            if (auto *enumInfo = findEnumType(resolvedEnum)) {
                auto fit = enumInfo->variantFields.find(pat->variant_name);
                if (fit != enumInfo->variantFields.end()) {
                    const std::vector<Pattern> *fieldPats =
                        unwrapEnumPayloadTuple(pat->bindings, fit->second.fieldTypes.size());
                    llvm::Value *sv = builder_.CreateLoad(subjectTy, subjectAlloca, "adt.val");
                    llvm::AllocaInst *tmpAlloca = builder_.CreateAlloca(subjectTy, nullptr, "adt.tmp");
                    builder_.CreateStore(sv, tmpAlloca);
                    llvm::Value *payloadPtr = builder_.CreateStructGEP(
                        enumInfo->adtType, tmpAlloca, 1, "adt.payload");
                    const llvm::DataLayout &dl = mod_->getDataLayout();
                    size_t offset = 0;
                    for (size_t bi = 0; bi < fieldPats->size() && bi < fit->second.fieldTypes.size(); ++bi) {
                        llvm::Type *fieldTy = fit->second.fieldTypes[bi];
                        uint64_t align = dl.getABITypeAlign(fieldTy).value();
                        offset = (offset + align - 1) / align * align;
                        llvm::Value *fieldPtr = builder_.CreateGEP(
                            llvm::Type::getInt8Ty(*ctx_), payloadPtr,
                            {llvm::ConstantInt::get(i64Ty_, offset)},
                            "adt.bind." + std::to_string(bi));
                        llvm::Value *fieldVal = builder_.CreateLoad(fieldTy, fieldPtr,
                                                                    "adt.bind.fval." + std::to_string(bi));
                        llvm::AllocaInst *tmp = builder_.CreateAlloca(fieldTy, nullptr,
                                                                       "adt.bind.alloca." + std::to_string(bi));
                        builder_.CreateStore(fieldVal, tmp);
                        const std::string &fieldTypeName = (bi < fit->second.fieldTypeNames.size())
                            ? fit->second.fieldTypeNames[bi] : std::string{};
                        if (!fieldTypeName.empty())
                            propagateTypeMeta(fieldTypeName, tmp);
                        markFieldAllocaArcManaged(tmp, fieldTy, fieldTypeName);
                        emitPatternBindings((*fieldPats)[bi], tmp, fieldTy, filterToEnumOnly(fieldTypeName), fieldTypeName);
                        offset += dl.getTypeAllocSize(fieldTy);
                    }
                }
            }
        }
    }, pattern);
}

// Emit ARC retain and register `bindAlloca` for scope cleanup, mirroring
// `emitVarDecl`'s ARC tracking for values obtained via pattern extraction.
//
// `val`       — the LLVM value just stored into `bindAlloca` (Load / ExtractValue).
// `bindAlloca`— the destination alloca that will hold the bound variable.
// `typeSig`   — Ry source-level type name for val, or "" when unknown.
//
// Call sites must invoke `propagateMeta`/`propagateTypeMeta` BEFORE calling
// this helper so that collection-type metadata is already present on
// `bindAlloca` when `typeSig` is empty (used by the heuristic fallback).
void CodeGen::emitPatternBindingArc(llvm::Value *val, llvm::AllocaInst *bindAlloca,
                                     const std::string &typeSig) {
    // --- Path 1: Record struct with ARC fields ---
    if (auto *recSt = llvm::dyn_cast<llvm::StructType>(val->getType())) {
        if (recordHasArcFields(recSt)) {
            if (llvm::isa<llvm::LoadInst>(val) || llvm::isa<llvm::ExtractValueInst>(val))
                emitRecordArcFieldsRetain(val, recSt);
            arc_field_record_vars_.insert(bindAlloca);
        }
        return;
    }

    // --- Path 2: Opaque pointer ---
    if (val->getType() != ptrTy_)
        return;

    // 2a: Type signature provided — use it to classify the Ry type.
    if (!typeSig.empty()) {
        const std::string resolved = resolveTypeAlias(typeSig);
        if (isCollectionTypeName(resolved)) {
            // Heap-allocated ARC type (str or collection).  Propagate collection
            // metadata so the correct destructor is selected at scope cleanup;
            // the call is idempotent when propagateMeta already set the slots.
            propagateTypeMeta(resolved, bindAlloca);
            retainArcValue(val);
            markArcManaged(bindAlloca);
            arc_backed_vars_.insert(bindAlloca);
            return;
        }
        if (isFunctionTypeName(resolved)) {
            // Distinguish capturing closure from bare function pointer via
            // fn_type_info metadata on the source value.
            const ValueMetadata *m = getMeta(val);
            if (!m) {
                if (auto *ld = llvm::dyn_cast<llvm::LoadInst>(val))
                    m = getMeta(ld->getPointerOperand());
            }
            if (m && m->fn_type_info &&
                (!m->fn_type_info->capturedVars.empty() ||
                 m->fn_type_info->isUniformClosure)) {
                retainArcValue(val);
                markArcManaged(bindAlloca);
                closure_managed_vars_.insert(bindAlloca);
            }
            // Bare function pointer: no ARC management.
            return;
        }
        if (resolved == "str") {
            // str is ARC-managed. Header is at handle-STRING_HEADER_SIZE (24).
            // Immortal literals are no-ops; dynamic strs increment strong_count.
            if (!tryRetainArcSource(val)) {
                auto *hdr = emitStrGetHeaderFromData(val);
                emitArcRetain(hdr, /*atomic=*/false);
            }
            markArcManaged(bindAlloca);
            arc_str_managed_vars_.insert(bindAlloca);
            return;
        }
        // Resource handles (currently File only) — register so scope exit
        // releases the ARC ref. Required for `Ok(f): close(f); lines(f)` to
        // work safely: with this registration, scope cleanup releases the
        // File's strong count exactly once. No retain on bind because the
        // enclosing Result<File, Error> subject is not in
        // arc_tagged_union_vars_ (File does not qualify as
        // fieldTypeIsArcManaged), so the original strong count from
        // __ry_io_file_open transfers through the Ok-arm extract without
        // a balancing subject-side release.
        if (resolved == "File") {
            int rk = ResourceKindRegistry::instance().lookupByTypeName(resolved);
            if (rk != ResourceKindRegistry::NONE) {
                // Guarded-arm copy of the binding lives in a throwaway scope
                // that pops between the pattern test and the body re-bind.
                // Without a retain, the popScope cleanup runs
                // __ry_io_file_cleanup on the File and the body bind would
                // observe a freed handle (#1818 PR review).
                if (pattern_binding_in_guard_depth_ > 0) {
                    auto *hdr = emitArcGetHeaderFromData(val);
                    emitArcRetain(hdr, /*atomic=*/false);
                }
                markArcManaged(bindAlloca);
                resource_managed_vars_[bindAlloca] = rk;
                addResourceKind(bindAlloca, rk);
                return;
            }
        }
        return;
    }

    // 2b: No type signature — probe heuristically.

    // LoadInst from an ARC-managed alloca: tryRetainArcSource emits the retain.
    if (tryRetainArcSource(val)) {
        markArcManaged(bindAlloca);
        // Propagate closure vs arc-backed distinction from the source alloca.
        if (auto *ld = llvm::dyn_cast<llvm::LoadInst>(val)) {
            auto *src = llvm::dyn_cast<llvm::AllocaInst>(ld->getPointerOperand());
            if (src && closure_managed_vars_.count(src))
                closure_managed_vars_.insert(bindAlloca);
            else if (src && arc_str_managed_vars_.count(src))
                arc_str_managed_vars_.insert(bindAlloca);
            else
                arc_backed_vars_.insert(bindAlloca);
        } else if (arc_str_owned_values_.count(val)) {
            arc_str_managed_vars_.insert(bindAlloca);
        } else {
            arc_backed_vars_.insert(bindAlloca);
        }
        return;
    }
    // Freshly allocated ARC value (e.g., produced by emitArcAlloc).
    if (arc_owned_values_.count(val)) {
        markArcManaged(bindAlloca);
        arc_backed_vars_.insert(bindAlloca);
        return;
    }
    // Collection type metadata propagated earlier (e.g., via propagateMeta from
    // a TuplePattern/RecordPattern intermediate alloca).
    if (getTypeMeta(TypeMeta::ListElem, bindAlloca) ||
        getTypeMeta(TypeMeta::MapKey,   bindAlloca) ||
        getTypeMeta(TypeMeta::SetElem,  bindAlloca)) {
        retainArcValue(val);
        markArcManaged(bindAlloca);
        arc_backed_vars_.insert(bindAlloca);
    }
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

    std::string subjectEnumName = resolveEnumType(subject);
    std::string subjectSourceTypeName = resolveSubjectSourceTypeName(subjectEnumName, subjectTy);
    checkMatchExhaustiveness(armPatterns, subjectTy, subjectEnumName);

    // --- Code generation: chain of conditional branches ---
    llvm::BasicBlock *matchEndBB = llvm::BasicBlock::Create(*ctx_, "match.end", fn_);

    llvm::AllocaInst *subjectAlloca = builder_.CreateAlloca(subjectTy, nullptr, "match.subject");
    builder_.CreateStore(subject, subjectAlloca);

    if (!subjectEnumName.empty())
        getOrCreateMeta(subjectAlloca).enum_value_type = subjectEnumName;

    propagateMetaWide(subject, subjectAlloca);
    maybeRegisterTaggedUnionSubject(subjectAlloca, subjectTy, subjectEnumName);

    for (size_t i = 0; i < s->arms.size(); ++i) {
        auto &arm = s->arms[i];
        llvm::BasicBlock *armBodyBB = llvm::BasicBlock::Create(*ctx_, "match.arm.body", fn_);
        llvm::BasicBlock *nextArmBB = (i + 1 < s->arms.size())
            ? llvm::BasicBlock::Create(*ctx_, "match.arm.test", fn_)
            : matchEndBB;

        llvm::Value *subjectVal = builder_.CreateLoad(subjectTy, subjectAlloca, "match.subj");
        llvm::Value *testResult = emitPatternTest(arm.pattern, subjectVal, subjectTy,
                                                  subjectEnumName, subjectSourceTypeName);

        if (arm.guard) {
            llvm::BasicBlock *guardBB = llvm::BasicBlock::Create(*ctx_, "match.guard", fn_);
            builder_.CreateCondBr(testResult, guardBB, nextArmBB);
            builder_.SetInsertPoint(guardBB);

            pushScope();
            pattern_binding_in_guard_depth_++;
            emitPatternBindings(arm.pattern, subjectAlloca, subjectTy,
                                subjectEnumName, subjectSourceTypeName);
            pattern_binding_in_guard_depth_--;

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
        emitPatternBindings(arm.pattern, subjectAlloca, subjectTy,
                            subjectEnumName, subjectSourceTypeName);

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

    std::string subjectEnumName = resolveEnumType(subject);
    std::string subjectSourceTypeName = resolveSubjectSourceTypeName(subjectEnumName, subjectTy);
    checkMatchExhaustiveness(armPatterns, subjectTy, subjectEnumName);

    // --- Code generation with PHI node ---
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(*ctx_, "match.expr.merge", fn_);
    std::vector<std::pair<llvm::Value*, llvm::BasicBlock*>> incoming;

    llvm::AllocaInst *subjectAlloca = builder_.CreateAlloca(subjectTy, nullptr, "match.subject");
    builder_.CreateStore(subject, subjectAlloca);

    if (!subjectEnumName.empty())
        getOrCreateMeta(subjectAlloca).enum_value_type = subjectEnumName;
    propagateMetaWide(subject, subjectAlloca);
    maybeRegisterTaggedUnionSubject(subjectAlloca, subjectTy, subjectEnumName);

    // If the scrutinee is Option<T>, seed the None() hint so that None() / bare
    // none in any arm value inherits the correct inner type (#1154). This avoids
    // the pattern-variable scope problem of compile-time pre-scan: we read the
    // runtime subjectTy directly instead of inspecting arm expressions.
    llvm::Type *scrutineeOptInner = isOptionType(subjectTy)
        ? llvm::cast<llvm::StructType>(subjectTy)->getElementType(1)
        : nullptr;
    OptionNoneHintGuard scrutineeHintGuard(*this,
        scrutineeOptInner ? scrutineeOptInner : option_decl_annotation_inner_);

    llvm::Value *firstVal = nullptr;

    for (size_t i = 0; i < e->arms.size(); ++i) {
        auto &arm = e->arms[i];
        llvm::Value *subjectVal = builder_.CreateLoad(subjectTy, subjectAlloca, "match.subj");
        llvm::Value *testResult = emitPatternTest(arm.pattern, subjectVal, subjectTy,
                                                  subjectEnumName, subjectSourceTypeName);

        llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(*ctx_, "match.expr.then", fn_);
        llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(*ctx_, "match.expr.next", fn_);

        if (arm.guard) {
            llvm::BasicBlock *guardBB = llvm::BasicBlock::Create(*ctx_, "match.expr.guard", fn_);
            builder_.CreateCondBr(testResult, guardBB, nextBB);
            builder_.SetInsertPoint(guardBB);

            pushScope();
            pattern_binding_in_guard_depth_++;
            emitPatternBindings(arm.pattern, subjectAlloca, subjectTy,
                                subjectEnumName, subjectSourceTypeName);
            pattern_binding_in_guard_depth_--;

            llvm::Value *guardVal = emitExpr(*arm.guard);
            guardVal = toBool(guardVal);
            popScope();

            builder_.CreateCondBr(guardVal, thenBB, nextBB);
        } else {
            builder_.CreateCondBr(testResult, thenBB, nextBB);
        }

        builder_.SetInsertPoint(thenBB);
        pushScope();
        emitPatternBindings(arm.pattern, subjectAlloca, subjectTy,
                            subjectEnumName, subjectSourceTypeName);

        llvm::Value *armVal = emitExpr(*arm.value);
        if (!firstVal) firstVal = armVal;
        else validateBranchTypes(firstVal, armVal, "match expression");

        popScope();
        llvm::BasicBlock *armEndBB = builder_.GetInsertBlock();
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
