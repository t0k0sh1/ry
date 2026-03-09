#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

struct NumberExpr   { int64_t value; };
struct FloatExpr    { double value; };
struct BoolExpr     { bool value; };
struct VariableExpr { std::string name; };
struct BinaryExpr;
struct UnaryExpr;

struct ExprNode {
    std::variant<NumberExpr, FloatExpr, BoolExpr, VariableExpr,
                 std::unique_ptr<BinaryExpr>,
                 std::unique_ptr<UnaryExpr>> data;
};
using ExprPtr = std::unique_ptr<ExprNode>;

struct BinaryExpr {
    std::string op;
    ExprPtr lhs, rhs;
};

struct UnaryExpr {
    std::string op;
    ExprPtr operand;
};

struct AssignStmt { std::string name; std::optional<std::string> type_annotation; ExprPtr value; };
struct CallStmt   { std::string callee; std::vector<ExprPtr> args; };
using StmtNode = std::variant<AssignStmt, CallStmt>;
using Program  = std::vector<StmtNode>;
