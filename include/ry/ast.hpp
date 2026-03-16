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
struct ListExpr;
struct IndexExpr;
struct MapExpr;
struct SetExpr;
struct LambdaExpr;

struct EnumAccessExpr {
    std::string enum_name;
    std::string variant_name;
};

struct OldExpr;
struct ResultExpr {};

struct ExprNode {
    std::variant<NumberExpr, FloatExpr, BoolExpr, StringExpr, VariableExpr,
                 std::unique_ptr<BinaryExpr>,
                 std::unique_ptr<UnaryExpr>,
                 std::unique_ptr<CallExpr>,
                 std::unique_ptr<FieldAccessExpr>,
                 std::unique_ptr<TupleExpr>,
                 std::unique_ptr<ListExpr>,
                 std::unique_ptr<IndexExpr>,
                 std::unique_ptr<MapExpr>,
                 std::unique_ptr<SetExpr>,
                 EnumAccessExpr,
                 std::unique_ptr<LambdaExpr>,
                 std::unique_ptr<OldExpr>,
                 ResultExpr> data;
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

struct ListExpr {
    std::vector<ExprPtr> elements;
};

struct IndexExpr {
    ExprPtr object;
    ExprPtr index;
};

struct MapExpr {
    std::vector<ExprPtr> keys;
    std::vector<ExprPtr> values;
};

struct SetExpr {
    std::vector<ExprPtr> elements;
};

struct LetStmt    { std::string name; std::optional<std::string> type_annotation; ExprPtr value; };  // immutable
struct VarStmt    { std::string name; std::optional<std::string> type_annotation; ExprPtr value; };  // mutable
struct AssignStmt { std::string name; ExprPtr value; };
struct CallStmt   { std::string callee; std::vector<ExprPtr> args; };

struct ReturnStmt { ExprPtr value; };
struct FnParam { std::string name; std::string type; };

struct ImportStmt {
    std::string module_path;              // "utils/math.ry"
    std::vector<std::string> names;       // {"add", "sub"} — empty means import all
    int line;
};

struct IndexAssignStmt {
    ExprPtr object;
    ExprPtr index;
    ExprPtr value;
};

struct FieldDef { std::string name; std::string type; };

struct TypeStmt { std::string name; std::vector<FieldDef> fields; std::vector<ExprPtr> invariants; };

struct BreakStmt {};
struct ContinueStmt {};

struct EnumStmt {
    std::string name;
    std::vector<std::string> variants;
};

struct FieldAssignStmt {
    ExprPtr object;
    std::string field;
    ExprPtr value;
};

struct IfStmt;
struct WhileStmt;
struct ForStmt;
struct FnStmt;
struct DescribeStmt;
struct MatchStmt;

struct ExpectStmt {
    ExprPtr actual;
    std::string matcher;   // "to_eq", "to_be_true", "to_be_false", "to_be_none"
    ExprPtr expected;      // to_eq only
    int line;
};

using StmtNode = std::variant<LetStmt, VarStmt, AssignStmt, CallStmt,
                              ReturnStmt, ImportStmt, TypeStmt,
                              IndexAssignStmt, BreakStmt, ContinueStmt,
                              FieldAssignStmt, EnumStmt, ExpectStmt,
                              std::unique_ptr<IfStmt>,
                              std::unique_ptr<WhileStmt>,
                              std::unique_ptr<ForStmt>,
                              std::unique_ptr<FnStmt>,
                              std::unique_ptr<DescribeStmt>,
                              std::unique_ptr<MatchStmt>>;
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

struct ForStmt {
    std::string var_name;
    ExprPtr iterable;
    std::vector<StmtNode> body;
};

struct OldExpr { ExprPtr expr; };

struct FnStmt {
    std::string name;
    std::vector<FnParam> params;
    std::string return_type;
    std::vector<StmtNode> body;
    bool is_operator = false;
    std::vector<ExprPtr> preconditions;
    std::vector<ExprPtr> postconditions;
};

struct LambdaExpr {
    std::vector<FnParam> params;
    std::string return_type;
    std::vector<StmtNode> body;   // multi-line lambda
    ExprPtr expr_body;            // single-expression lambda (if non-null, use this)
};

struct ItBlock {
    std::string description;
    std::vector<StmtNode> body;
};

struct DescribeStmt {
    std::string description;
    std::vector<ItBlock> cases;
};

// ===== Match patterns =====

struct WildcardPattern {};
struct LiteralPattern { ExprPtr value; };
struct VariablePattern { std::string name; };
struct EnumPattern { std::string enum_name; std::string variant_name; };
struct SomePattern { std::string binding; };
struct NonePattern {};

using Pattern = std::variant<
    WildcardPattern, LiteralPattern, VariablePattern,
    EnumPattern, SomePattern, NonePattern
>;

struct MatchArm {
    Pattern pattern;
    ExprPtr guard;
    std::vector<StmtNode> body;
};

struct MatchStmt {
    ExprPtr subject;
    std::vector<MatchArm> arms;
};
