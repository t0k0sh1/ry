#include "ry/sema_return.hpp"


namespace ry {

namespace {

// Returns true if `p` can never fail to match at runtime — i.e. it contains no
// literals, no EnumPattern/EnumConstructorPattern with fixed tags, etc.
// Used to decide whether an EnumConstructorPattern arm fully covers its variant.
static bool isIrrefutable(const Pattern &p) {
    return std::visit([](const auto &v) -> bool {
        using T = std::decay_t<decltype(v)>;
        if constexpr (std::is_same_v<T, WildcardPattern> ||
                      std::is_same_v<T, VariablePattern>)
            return true;
        if constexpr (std::is_same_v<T, std::unique_ptr<TuplePattern>>)
            return v && std::all_of(v->elements.begin(), v->elements.end(), isIrrefutable);
        if constexpr (std::is_same_v<T, std::unique_ptr<EnumConstructorPattern>>)
            return v && std::all_of(v->bindings.begin(), v->bindings.end(), isIrrefutable);
        return false; // LiteralPattern, EnumPattern, SomePattern, etc. are refutable
    }, p);
}

struct PatternCoverage {
    bool hasOk = false, hasErr = false;
    bool hasSome = false, hasNone = false;
    bool hasTrue = false, hasFalse = false;
    std::string enumName;
    std::unordered_set<std::string> coveredVariants;
};

void collectPatternInfo(const Pattern &pat, PatternCoverage &cov,
                        bool &hasCatchAll) {
    std::visit([&](const auto &p) {
        using T = std::decay_t<decltype(p)>;
        if constexpr (std::is_same_v<T, std::unique_ptr<OrPattern>>) {
            for (const auto &alt : p->alternatives)
                collectPatternInfo(alt, cov, hasCatchAll);
        } else if constexpr (std::is_same_v<T, WildcardPattern> ||
                             std::is_same_v<T, VariablePattern>) {
            hasCatchAll = true;
        } else if constexpr (std::is_same_v<T, OkPattern>) {
            cov.hasOk = true;
        } else if constexpr (std::is_same_v<T, ErrPattern>) {
            cov.hasErr = true;
        } else if constexpr (std::is_same_v<T, SomePattern>) {
            cov.hasSome = true;
        } else if constexpr (std::is_same_v<T, NonePattern>) {
            cov.hasNone = true;
        } else if constexpr (std::is_same_v<T, EnumPattern>) {
            if (cov.enumName.empty()) cov.enumName = p.enum_name;
            cov.coveredVariants.insert(p.variant_name);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<EnumConstructorPattern>>) {
            if (cov.enumName.empty()) cov.enumName = p->enum_name;
            // Only mark variant as fully covered when every binding is irrefutable
            // (variable or wildcard). Restrictive payloads like Click((0, 0)) only
            // cover the variant partially and must not satisfy exhaustiveness.
            if (p && std::all_of(p->bindings.begin(), p->bindings.end(), isIrrefutable))
                cov.coveredVariants.insert(p->variant_name);
        } else if constexpr (std::is_same_v<T, LiteralPattern>) {
            if (auto *be = std::get_if<BoolExpr>(&p.value->data)) {
                if (be->value) cov.hasTrue = true;
                else cov.hasFalse = true;
            }
        }
    }, pat);
}

bool isExhaustiveMatch(const std::vector<CaseArm> &arms,
                       const EnumVariantRegistry &registry) {
    PatternCoverage cov;

    for (auto &arm : arms) {
        if (arm.guard) continue;
        bool hasCatchAll = false;
        collectPatternInfo(arm.pattern, cov, hasCatchAll);
        if (hasCatchAll) return true;
    }

    if ((cov.hasOk && cov.hasErr) || (cov.hasSome && cov.hasNone))
        return true;

    if (cov.hasTrue && cov.hasFalse)
        return true;

    if (!cov.enumName.empty()) {
        auto it = registry.find(cov.enumName);
        if (it != registry.end()) {
            for (auto &vname : it->second) {
                if (!cov.coveredVariants.count(vname))
                    return false;
            }
            return true;
        }
    }

    return false;
}

bool stmtReturnsOnAllPaths(const StmtNode &stmt,
                           const EnumVariantRegistry &registry) {
    return std::visit([&](const auto &s) -> bool {
        using T = std::decay_t<decltype(s)>;

        if constexpr (std::is_same_v<T, ReturnStmt>) {
            return true;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<IfStmt>>) {
            if (s->else_body.empty()) return false;
            if (!allPathsReturn(s->branch.body, registry)) return false;
            return allPathsReturn(s->else_body, registry);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<CaseCondStmt>>) {
            if (s->else_body.empty()) return false;
            for (auto &arm : s->arms) {
                if (!allPathsReturn(arm.body, registry)) return false;
            }
            return allPathsReturn(s->else_body, registry);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<CaseStmt>>) {
            if (!isExhaustiveMatch(s->arms, registry)) return false;
            for (auto &arm : s->arms) {
                if (!allPathsReturn(arm.body, registry)) return false;
            }
            return true;
        } else {
            return false;
        }
    }, stmt);
}

} // namespace

bool allPathsReturn(const std::vector<StmtNode> &body,
                    const EnumVariantRegistry &enumRegistry) {
    for (auto &stmt : body) {
        if (stmtReturnsOnAllPaths(stmt, enumRegistry)) return true;
    }
    return false;
}

} // namespace ry
