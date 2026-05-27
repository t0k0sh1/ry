#pragma once

#include "ry/ast/ast.hpp"
#include <unordered_map>
#include <unordered_set>
#include <string>


namespace ry {

/// Lightweight enum variant registry (no LLVM dependency).
/// Maps enum_name -> set of variant names.
using EnumVariantRegistry =
    std::unordered_map<std::string, std::unordered_set<std::string>>;

/// Returns true if all control-flow paths through `body` contain a ReturnStmt.
bool allPathsReturn(const std::vector<StmtNode> &body,
                    const EnumVariantRegistry &enumRegistry = {});

} // namespace ry
