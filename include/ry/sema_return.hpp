#pragma once

#include "ry/ast.hpp"

/// Returns true if all control-flow paths through `body` contain a ReturnStmt.
bool allPathsReturn(const std::vector<StmtNode> &body);
