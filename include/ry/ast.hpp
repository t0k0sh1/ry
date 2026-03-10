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

struct LetStmt    { std::string name; std::optional<std::string> type_annotation; ExprPtr value; };
struct ConstStmt  { std::string name; std::optional<std::string> type_annotation; ExprPtr value; };
struct AssignStmt { std::string name; ExprPtr value; };
struct CallStmt   { std::string callee; std::vector<ExprPtr> args; };

struct IfStmt;

using StmtNode = std::variant<LetStmt, ConstStmt, AssignStmt, CallStmt,
                              std::unique_ptr<IfStmt>>;
using Program  = std::vector<StmtNode>;

struct IfBranch {
    ExprPtr condition;
    std::vector<StmtNode> body;
};

struct IfStmt {
    std::vector<IfBranch> branches;    // if + elif*
    std::vector<StmtNode> else_body;   // else（空なら else なし）
};
