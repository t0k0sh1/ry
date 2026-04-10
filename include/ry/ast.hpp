#pragma once

#include "ry/source_location.hpp"
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>


namespace ry {

// ===== Forward declarations =====
struct ExprNode;
using ExprPtr = std::unique_ptr<ExprNode>;

// ===== Type AST nodes =====

struct TypeNode;
using TypeNodePtr = std::unique_ptr<TypeNode>;

struct BasicType    { std::string name; };
struct GenericType  { std::string name; std::vector<TypeNodePtr> type_args; };
struct ArrayType    { TypeNodePtr element_type; uint64_t size; };
struct TupleType    { std::vector<TypeNodePtr> elements; };
struct FnType       { std::vector<TypeNodePtr> param_types; TypeNodePtr return_type; };
struct UnionType    { std::vector<TypeNodePtr> components; };
struct OptionalType { TypeNodePtr inner; };
struct WeakType     { TypeNodePtr inner; };
struct RangeType    { std::string start; std::string end; };

struct TypeNode {
    std::variant<
        BasicType,
        GenericType,
        ArrayType,
        TupleType,
        FnType,
        UnionType,
        OptionalType,
        WeakType,
        RangeType
    > data;

    std::string toString() const;

    static TypeNodePtr makeBasic(std::string name);
    static TypeNodePtr makeGeneric(std::string name, std::vector<TypeNodePtr> args);
    static TypeNodePtr makeArray(TypeNodePtr elem, uint64_t size);
    static TypeNodePtr makeTuple(std::vector<TypeNodePtr> elems);
    static TypeNodePtr makeFn(std::vector<TypeNodePtr> params, TypeNodePtr ret);
    static TypeNodePtr makeUnion(std::vector<TypeNodePtr> comps);
    static TypeNodePtr makeOptional(TypeNodePtr inner);
    static TypeNodePtr makeWeak(TypeNodePtr inner);
    static TypeNodePtr makeRange(std::string start, std::string end);
    static TypeNodePtr clone(const TypeNodePtr &src);
};

// ===== Directive =====

struct DirectiveArg {
    std::optional<std::string> name;  // nullopt = 位置引数
    ExprPtr value;                     // 任意の式
};

struct Directive {
    std::string name;
    std::vector<DirectiveArg> args;
    SourceLocation loc;

    Directive() = default;
    Directive(Directive &&) = default;
    Directive &operator=(Directive &&) = default;
    Directive(const Directive &) = delete;
    Directive &operator=(const Directive &) = delete;
};

inline bool hasDirective(const std::vector<Directive> &directives, std::string_view name) {
    for (const auto &d : directives)
        if (d.name == name) return true;
    return false;
}

inline Directive *findDirective(std::vector<Directive> &directives, std::string_view name) {
    for (auto &d : directives)
        if (d.name == name) return &d;
    return nullptr;
}

// Get the first positional string argument of a named directive.
// Returns empty string if not found or not a StringExpr.
std::string getDirectivePositionalArg(const std::vector<Directive> &directives,
                                      std::string_view name);

// Get a named argument's expression node from a directive.
// Returns nullptr if not found.
const ExprNode *getDirectiveNamedArg(const std::vector<Directive> &directives,
                                     std::string_view directive_name,
                                     std::string_view arg_name);

// Returns true for operator names whose return type must be bool.
inline bool isBoolConstrainedOperator(const std::string &name) {
    return name == "operator==" || name == "operator!=" ||
           name == "operator<"  || name == "operator<=" ||
           name == "operator>"  || name == "operator>=" ||
           name == "operatornot" || name == "operatorand" || name == "operatoror" ||
           name == "operatorin";
}

// Strips the "operator" prefix from an operator function name.
inline std::string operatorSymbol(const std::string &name) {
    return name.substr(8);
}

// Returns true for compound assignment operator names (operator+=, operator-=, etc.).
inline bool isCompoundAssignOperator(const std::string &name) {
    return name == "operator+=" || name == "operator-=" ||
           name == "operator*=" || name == "operator/=" ||
           name == "operator%=" || name == "operator//=" ||
           name == "operator**=" ||
           name == "operator&=" || name == "operator|=" ||
           name == "operator^=" ||
           name == "operator<<=" || name == "operator>>=";
}

struct TypeParam {
    std::string name;
    std::optional<std::string> bound;    // record name constraint; validated at instantiation
};
// `value` is the unsigned bit pattern of a non-negative magnitude stored in
// int64_t (so UINT64_MAX arrives as int64_t(-1)). Negative literals are
// represented as `UnaryExpr("-", NumberExpr{...})`, so every NumberExpr
// node is logically non-negative. Codegen re-interprets the bit pattern
// according to `suffix` (or an injected annotation suffix in emitVarDecl).
struct NumberExpr   { int64_t value; std::string suffix; };
struct FloatExpr    { double value;  std::string suffix; };
struct BoolExpr     { bool value; };
struct StringExpr   { std::string value; };
struct RegexExpr    { std::string pattern; };
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

struct CastExpr;
struct InterpolatedStringExpr;
struct WhenCondExpr;
struct MatchExpr;
struct RangeExpr;
struct NoneExpr {};
struct ErrorPropagateExpr;
struct AwaitExpr;
struct WeakExpr;

struct ExprNode {
    std::variant<NumberExpr, FloatExpr, BoolExpr, StringExpr, RegexExpr, VariableExpr,
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
                 std::unique_ptr<CastExpr>,
                 std::unique_ptr<InterpolatedStringExpr>,
                 std::unique_ptr<WhenCondExpr>,
                 std::unique_ptr<MatchExpr>,
                 std::unique_ptr<RangeExpr>,
                 NoneExpr,
                 std::unique_ptr<ErrorPropagateExpr>,
                 std::unique_ptr<AwaitExpr>,
                 std::unique_ptr<WeakExpr>> data;
    SourceLocation loc;
};

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
    std::vector<ExprPtr> indices;
};

struct MapExpr {
    std::vector<ExprPtr> keys;
    std::vector<ExprPtr> values;
};

struct SetExpr {
    std::vector<ExprPtr> elements;
};

struct AssignStmt { std::string name; TypeNodePtr type_annotation; ExprPtr value; std::optional<std::string> compound_op; std::vector<Directive> directives; SourceLocation loc; };
struct CallStmt   { std::string callee; std::vector<ExprPtr> args; std::vector<Directive> directives; SourceLocation loc; };
struct ExprStmt   { ExprPtr expr; SourceLocation loc; };

struct ReturnStmt { ExprPtr value; SourceLocation loc; };
struct FnParam { std::string name; TypeNodePtr type; ExprPtr default_value; };

struct ImportStmt {
    std::string module_path;              // "utils/math" (resolved to dir or .ry file)
    std::vector<std::string> names;       // {"add", "sub"} — empty means import all
    SourceLocation loc;
};

struct IndexAssignStmt {
    ExprPtr object;
    std::vector<ExprPtr> indices;
    ExprPtr value;
    SourceLocation loc;
};

struct FieldDef { std::string name; TypeNodePtr type; std::vector<Directive> directives; };

struct RecordStmt { std::string name; std::optional<std::string> parent_name; std::vector<FieldDef> fields; std::vector<ExprPtr> invariants; std::vector<Directive> directives; SourceLocation loc; };

struct TypeAliasStmt { std::string name; TypeNodePtr target_type; SourceLocation loc; };

struct BreakStmt { SourceLocation loc; };
struct ContinueStmt { SourceLocation loc; };
struct EllipsisStmt { SourceLocation loc; };

struct EnumVariant {
    std::string name;
    std::vector<TypeNodePtr> field_types;  // empty = no associated data
    std::vector<std::string> field_names;  // parallel to field_types; empty when unnamed
    std::optional<int64_t> explicit_value;
};

struct EnumStmt {
    std::string name;
    std::vector<TypeParam> type_params;  // for generics: <T, U> or <T: Bound>
    std::vector<EnumVariant> variants;
    SourceLocation loc;
};

struct TupleDestructStmt {
    std::vector<std::string> names;  // "_" is wildcard
    ExprPtr value;
    bool is_immutable;
    std::vector<Directive> directives;
    SourceLocation loc;
};

struct FieldAssignStmt {
    ExprPtr object;
    std::string field;
    ExprPtr value;
    SourceLocation loc;
};

struct IfStmt;
struct WhileStmt;
struct ForStmt;
struct FnStmt;
struct MatchStmt;
struct WhenCondStmt;
struct AwaitStmt;

struct ExpectStmt {
    ExprPtr actual;
    std::string matcher;   // "to_eq", "to_be_true", "to_be_false", "to_be_none"
    ExprPtr expected;      // to_eq only
    SourceLocation loc;
};

using StmtNode = std::variant<AssignStmt, CallStmt, ExprStmt,
                              ReturnStmt, ImportStmt, RecordStmt,
                              IndexAssignStmt, BreakStmt, ContinueStmt, EllipsisStmt,
                              FieldAssignStmt, EnumStmt, ExpectStmt, AwaitStmt,
                              TupleDestructStmt, TypeAliasStmt,
                              std::unique_ptr<IfStmt>,
                              std::unique_ptr<WhenCondStmt>,
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
    IfBranch branch;
    std::vector<StmtNode> else_body;   // else（空なら else なし）
    SourceLocation loc;
};

struct WhenCondArm {
    ExprPtr condition;
    std::vector<StmtNode> body;
};

struct WhenCondStmt {
    std::vector<WhenCondArm> arms;
    std::vector<StmtNode> else_body;
    SourceLocation loc;
};

struct WhileStmt {
    ExprPtr condition;
    std::vector<StmtNode> body;
    SourceLocation loc;
};

struct ForStmt {
    std::vector<std::string> var_names; // 1+ variable names; "_" = wildcard
    ExprPtr iterable;
    std::vector<StmtNode> body;
    std::vector<Directive> directives;
    SourceLocation loc;
};

struct CastExpr {
    ExprPtr value;
    TypeNodePtr target_type;
};

struct WhenCondExprArm {
    ExprPtr condition;
    ExprPtr value;
};

struct WhenCondExpr {
    std::vector<WhenCondExprArm> arms;
    ExprPtr else_expr;
};

struct InterpolatedStringExpr {
    std::vector<std::string> parts;   // literal text segments (parts.size() == exprs.size() + 1)
    std::vector<ExprPtr> exprs;       // interpolated expressions
};

struct RangeExpr {
    ExprPtr start;
    ExprPtr end;
};

struct ErrorPropagateExpr {
    ExprPtr operand;
};

struct AwaitExpr {
    ExprPtr operand;
};

struct WeakExpr {
    ExprPtr operand;
};

struct AwaitStmt {
    ExprPtr operand;
    SourceLocation loc;
};

struct FnStmt {
    std::string name;
    std::vector<TypeParam> type_params;  // for generics: <T, U> or <T: Bound>
    std::vector<FnParam> params;
    TypeNodePtr return_type;
    std::vector<StmtNode> body;
    bool is_operator = false;
    bool is_async = false;
    std::vector<ExprPtr> preconditions;
    std::vector<ExprPtr> postconditions;
    std::vector<std::string> ensure_bindings;
    std::vector<Directive> directives;
    SourceLocation loc;
};

struct LambdaExpr {
    std::vector<FnParam> params;
    TypeNodePtr return_type;
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
    OkPattern, ErrPattern,
    EnumConstructorPattern,
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
    SourceLocation loc;
};

struct MatchExprArm {
    Pattern pattern;
    ExprPtr guard;
    ExprPtr value;
};

struct MatchExpr {
    ExprPtr subject;
    std::vector<MatchExprArm> arms;
};

// True if `name` is a low-level integer type that can be injected as a
// NumberExpr suffix. Excludes `f32` (the only non-integer low-level type).
inline bool isLowLevelIntTypeName(const std::string &name) {
    return name == "i8" || name == "i16" || name == "i32" || name == "i64" ||
           name == "u8" || name == "u16" || name == "u32" || name == "u64";
}

// If `value` is a bare integer literal (optionally wrapped in UnaryExpr(+/-)),
// propagate `annot` onto the inner NumberExpr so downstream consumers see the
// literal typed against its target annotation. Used by codegen (for range
// checks) and by the formatter (for correct unsigned rendering). Does not
// touch already-suffixed literals or non-trivial initializer expressions.
inline void injectLowLevelSuffix(ExprNode &value, const std::string &annot) {
    if (!isLowLevelIntTypeName(annot)) return;
    if (auto *ne = std::get_if<NumberExpr>(&value.data)) {
        if (ne->suffix.empty())
            ne->suffix = annot;
        return;
    }
    if (auto *ue = std::get_if<std::unique_ptr<UnaryExpr>>(&value.data)) {
        if ((*ue)->op != "-" && (*ue)->op != "+") return;
        if (auto *inner = std::get_if<NumberExpr>(&(*ue)->operand->data)) {
            if (inner->suffix.empty())
                inner->suffix = annot;
        }
    }
}

} // namespace ry
