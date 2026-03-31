#include "ry/sema_return.hpp"

namespace {

void collectPatternInfo(const Pattern &pat,
                        bool &hasOk, bool &hasErr,
                        bool &hasSome, bool &hasNone,
                        bool &hasCatchAll) {
    std::visit([&](const auto &p) {
        using T = std::decay_t<decltype(p)>;
        if constexpr (std::is_same_v<T, std::unique_ptr<OrPattern>>) {
            for (const auto &alt : p->alternatives)
                collectPatternInfo(alt, hasOk, hasErr, hasSome, hasNone, hasCatchAll);
        } else if constexpr (std::is_same_v<T, WildcardPattern> ||
                             std::is_same_v<T, VariablePattern>) {
            hasCatchAll = true;
        } else if constexpr (std::is_same_v<T, OkPattern>) {
            hasOk = true;
        } else if constexpr (std::is_same_v<T, ErrPattern>) {
            hasErr = true;
        } else if constexpr (std::is_same_v<T, SomePattern>) {
            hasSome = true;
        } else if constexpr (std::is_same_v<T, NonePattern>) {
            hasNone = true;
        }
    }, pat);
}

bool isExhaustiveMatch(const std::vector<MatchArm> &arms) {
    bool hasOk = false, hasErr = false, hasSome = false, hasNone = false;
    for (auto &arm : arms) {
        if (arm.guard) continue;
        bool hasCatchAll = false;
        collectPatternInfo(arm.pattern, hasOk, hasErr, hasSome, hasNone, hasCatchAll);
        if (hasCatchAll) return true;
    }
    return (hasOk && hasErr) || (hasSome && hasNone);
}

bool stmtReturnsOnAllPaths(const StmtNode &stmt) {
    return std::visit([](const auto &s) -> bool {
        using T = std::decay_t<decltype(s)>;

        if constexpr (std::is_same_v<T, ReturnStmt>) {
            return true;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<IfStmt>>) {
            if (s->else_body.empty()) return false;
            if (!allPathsReturn(s->branch.body)) return false;
            return allPathsReturn(s->else_body);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<WhenCondStmt>>) {
            if (s->else_body.empty()) return false;
            for (auto &arm : s->arms) {
                if (!allPathsReturn(arm.body)) return false;
            }
            return allPathsReturn(s->else_body);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<MatchStmt>>) {
            if (!isExhaustiveMatch(s->arms)) return false;
            for (auto &arm : s->arms) {
                if (!allPathsReturn(arm.body)) return false;
            }
            return true;
        } else {
            return false;
        }
    }, stmt);
}

} // namespace

bool allPathsReturn(const std::vector<StmtNode> &body) {
    for (auto &stmt : body) {
        if (stmtReturnsOnAllPaths(stmt)) return true;
    }
    return false;
}
