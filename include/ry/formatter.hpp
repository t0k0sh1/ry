#pragma once

#include "ry/ast.hpp"
#include <string>
#include <vector>


namespace ry {

struct Comment {
    int line;
    int col;
    std::string text;
    bool is_inline;
};

class Formatter {
public:
    explicit Formatter(const std::string &source, int indent_width = 2);

    std::string format(const Program &prog);
    static std::string formatSource(const std::string &source, int indent_width = 2);
    static bool verifyFormatting(const std::string &formatted, std::string &reason);

private:
    std::string source_;
    int indent_width_;
    std::vector<Comment> comments_;
    std::string out_;
    int indent_level_ = 0;
    int last_emitted_line_ = 0;
    size_t next_comment_idx_ = 0;

    // Comment extraction
    std::vector<Comment> extractComments(const std::string &source);

    // Comment emission
    void emitCommentsBefore(int line);
    void emitInlineComment(int line);
    void emitTrailingComments();

    // Output helpers
    void emit(const std::string &s);
    void emitIndent();
    void emitNewline();
    bool endsWithBlankLine() const;
    void indent();
    void dedent();

    // Statement formatting
    void formatProgram(const Program &prog);
    void formatStmt(const StmtNode &stmt);
    void formatAssign(const AssignStmt &s);
    void formatCall(const CallStmt &s);
    void formatExprStmt(const ExprStmt &s);
    void formatReturn(const ReturnStmt &s);
    void formatImport(const ImportStmt &s);
    void formatRecord(const RecordStmt &s);
    void formatEnum(const EnumStmt &s);
    void formatTypeAlias(const TypeAliasStmt &s);
    void formatIf(const IfStmt &s);
    void formatCaseCond(const CaseCondStmt &s);
    void formatWhile(const WhileStmt &s);
    void formatFor(const ForStmt &s);
    void formatFn(const FnStmt &s);
    void formatCase(const CaseStmt &s);
    void formatIndexAssign(const IndexAssignStmt &s);
    void formatFieldAssign(const FieldAssignStmt &s);
    void formatBreak(const BreakStmt &s);
    void formatContinue(const ContinueStmt &s);
    void formatEllipsis(const EllipsisStmt &s);
    void formatExpect(const ExpectStmt &s);
    void formatAwaitStmt(const AwaitStmt &s);
    void formatTupleDestruct(const TupleDestructStmt &s);

    // Expression formatting
    std::string formatExpr(const ExprNode &expr);
    std::string formatExprInner(const ExprNode &expr);

    // Helpers
    void formatDirectives(const std::vector<Directive> &directives);
    void formatBlock(const std::vector<StmtNode> &body);
    std::string formatParams(const std::vector<FnParam> &params);
    void emitTypeParams(const std::vector<TypeParam> &params);
    std::string formatPattern(const Pattern &pat);
    std::string escapeString(const std::string &s);
    std::string formatFloat(double v);
    std::string formatNamedArgs(const std::vector<NamedArg> &named, bool has_positionals);

    bool isDefinition(const StmtNode &stmt) const;
    bool hasDirectives(const StmtNode &stmt) const;
    int getStmtLine(const StmtNode &stmt) const;
    int operatorPrecedence(const std::string &op) const;
    bool needsParens(const ExprNode &child, const std::string &parent_op, bool is_right) const;
};

int cmd_fmt(int argc, char *argv[]);

} // namespace ry
