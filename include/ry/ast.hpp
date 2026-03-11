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
struct StringExpr   { std::string value; };
struct VariableExpr { std::string name; };
struct BinaryExpr;
struct UnaryExpr;

struct CallExpr;
struct FieldAccessExpr;
struct TupleExpr;

struct ExprNode {
    std::variant<NumberExpr, FloatExpr, BoolExpr, StringExpr, VariableExpr,
                 std::unique_ptr<BinaryExpr>,
                 std::unique_ptr<UnaryExpr>,
                 std::unique_ptr<CallExpr>,
                 std::unique_ptr<FieldAccessExpr>,
                 std::unique_ptr<TupleExpr>> data;
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

struct CallExpr {
    std::string callee;
    std::vector<ExprPtr> args;
};

struct FieldAccessExpr {
    ExprPtr object;
    std::string field;
};

struct TupleExpr {
    std::vector<ExprPtr> elements;
};

struct LetStmt    { std::string name; std::optional<std::string> type_annotation; ExprPtr value; };
struct ConstStmt  { std::string name; std::optional<std::string> type_annotation; ExprPtr value; };
struct AssignStmt { std::string name; ExprPtr value; };
struct CallStmt   { std::string callee; std::vector<ExprPtr> args; };

struct ReturnStmt { ExprPtr value; };
struct FnParam { std::string name; std::string type; };

struct ImportStmt {
    std::string module_path;              // "utils/math.ry"
    std::vector<std::string> names;       // {"add", "sub"} — empty means import all
    int line;
};

struct FieldDef { std::string name; std::string type; };

struct TypeStmt { std::string name; std::vector<FieldDef> fields; };

struct IfStmt;
struct WhileStmt;
struct FnStmt;

using StmtNode = std::variant<LetStmt, ConstStmt, AssignStmt, CallStmt,
                              ReturnStmt, ImportStmt, TypeStmt,
                              std::unique_ptr<IfStmt>,
                              std::unique_ptr<WhileStmt>,
                              std::unique_ptr<FnStmt>>;
using Program  = std::vector<StmtNode>;

struct IfBranch {
    ExprPtr condition;
    std::vector<StmtNode> body;
};

struct IfStmt {
    std::vector<IfBranch> branches;    // if + elif*
    std::vector<StmtNode> else_body;   // else（空なら else なし）
};

struct WhileStmt {
    ExprPtr condition;
    std::vector<StmtNode> body;
};

struct FnStmt {
    std::string name;
    std::vector<FnParam> params;
    std::string return_type;
    std::vector<StmtNode> body;
};
