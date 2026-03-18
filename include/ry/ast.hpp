#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

// ===== Directive =====

struct DirectiveParam {
    std::string key;
    std::string value;
};

struct Directive {
    std::string name;
    std::vector<DirectiveParam> params;
    int line;
};

inline bool hasDirective(const std::vector<Directive> &directives, std::string_view name) {
    for (const auto &d : directives)
        if (d.name == name) return true;
    return false;
}

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
struct CastExpr;
struct InterpolatedStringExpr;
struct TernaryExpr;
struct RangeExpr;
struct NoneExpr {};

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
                 ResultExpr,
                 std::unique_ptr<CastExpr>,
                 std::unique_ptr<InterpolatedStringExpr>,
                 std::unique_ptr<TernaryExpr>,
                 std::unique_ptr<RangeExpr>,
                 NoneExpr> data;
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

struct LetStmt    { std::string name; std::optional<std::string> type_annotation; ExprPtr value; std::vector<Directive> directives; };  // immutable
struct VarStmt    { std::string name; std::optional<std::string> type_annotation; ExprPtr value; std::vector<Directive> directives; };  // mutable
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

struct FieldDef { std::string name; std::string type; std::vector<Directive> directives; };

struct RecordStmt { std::string name; std::vector<FieldDef> fields; std::vector<ExprPtr> invariants; std::vector<Directive> directives; };

struct TypeAliasStmt { std::string name; std::string target_type; };

struct BreakStmt {};
struct ContinueStmt {};

struct EnumVariant {
    std::string name;
    std::vector<std::string> field_types;  // empty = no associated data
};

struct EnumStmt {
    std::string name;
    std::vector<std::string> type_params;  // for generics
    std::vector<EnumVariant> variants;
};

struct TupleDestructStmt {
    std::vector<std::string> names;  // "_" is wildcard
    ExprPtr value;
    bool is_immutable;
    std::vector<Directive> directives;
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
struct MatchStmt;

struct ExpectStmt {
    ExprPtr actual;
    std::string matcher;   // "to_eq", "to_be_true", "to_be_false", "to_be_none"
    ExprPtr expected;      // to_eq only
    int line;
};

using StmtNode = std::variant<LetStmt, VarStmt, AssignStmt, CallStmt,
                              ReturnStmt, ImportStmt, RecordStmt,
                              IndexAssignStmt, BreakStmt, ContinueStmt,
                              FieldAssignStmt, EnumStmt, ExpectStmt,
                              TupleDestructStmt, TypeAliasStmt,
                              std::unique_ptr<IfStmt>,
                              std::unique_ptr<WhileStmt>,
                              std::unique_ptr<ForStmt>,
                              std::unique_ptr<FnStmt>,
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
    std::optional<std::string> var_name2;
    ExprPtr iterable;
    std::vector<StmtNode> body;
};

struct OldExpr { ExprPtr expr; };

struct CastExpr {
    ExprPtr value;
    std::string target_type;
};

struct TernaryExpr {
    ExprPtr condition;
    ExprPtr true_expr;
    ExprPtr false_expr;
};

struct InterpolatedStringExpr {
    std::vector<std::string> parts;   // literal text segments (parts.size() == exprs.size() + 1)
    std::vector<ExprPtr> exprs;       // interpolated expressions
};

struct RangeExpr {
    ExprPtr start;
    ExprPtr end;
};

struct FnStmt {
    std::string name;
    std::vector<FnParam> params;
    std::string return_type;
    std::vector<StmtNode> body;
    bool is_operator = false;
    std::vector<ExprPtr> preconditions;
    std::vector<ExprPtr> postconditions;
    std::vector<Directive> directives;
};

struct LambdaExpr {
    std::vector<FnParam> params;
    std::string return_type;
    std::vector<StmtNode> body;   // multi-line lambda
    ExprPtr expr_body;            // single-expression lambda (if non-null, use this)
};

// ===== Match patterns =====

struct WildcardPattern {};
struct LiteralPattern { ExprPtr value; };
struct VariablePattern { std::string name; };
struct EnumPattern { std::string enum_name; std::string variant_name; };
struct SomePattern { std::string binding; };
struct NonePattern {};
struct OkPattern { std::string binding; };
struct ErrPattern { std::string binding; };
struct EnumConstructorPattern {
    std::string enum_name;
    std::string variant_name;
    std::vector<std::string> bindings;
};

struct OrPattern;

using Pattern = std::variant<
    WildcardPattern, LiteralPattern, VariablePattern,
    EnumPattern, SomePattern, NonePattern,
    OkPattern, ErrPattern, EnumConstructorPattern,
    std::unique_ptr<OrPattern>
>;

struct OrPattern { std::vector<Pattern> alternatives; };

struct MatchArm {
    Pattern pattern;
    ExprPtr guard;
    std::vector<StmtNode> body;
};

struct MatchStmt {
    ExprPtr subject;
    std::vector<MatchArm> arms;
};
