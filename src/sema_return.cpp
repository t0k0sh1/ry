#include "ry/sema_return.hpp"

namespace {

bool patternIsCatchAll(const Pattern &pat) {
    return std::visit([](const auto &p) -> bool {
        using T = std::decay_t<decltype(p)>;
        if constexpr (std::is_same_v<T, WildcardPattern> ||
                      std::is_same_v<T, VariablePattern>)
            return true;
        return false;
    }, pat);
}

bool isExhaustiveMatch(const std::vector<MatchArm> &arms) {
    bool hasOk = false, hasErr = false, hasSome = false, hasNone = false;
    for (auto &arm : arms) {
        if (arm.guard) continue;
        if (patternIsCatchAll(arm.pattern)) return true;
        std::visit([&](const auto &p) {
            using T = std::decay_t<decltype(p)>;
            if constexpr (std::is_same_v<T, OkPattern>) hasOk = true;
            else if constexpr (std::is_same_v<T, ErrPattern>) hasErr = true;
            else if constexpr (std::is_same_v<T, SomePattern>) hasSome = true;
            else if constexpr (std::is_same_v<T, NonePattern>) hasNone = true;
        }, arm.pattern);
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
            for (auto &branch : s->branches) {
                if (!allPathsReturn(branch.body)) return false;
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
