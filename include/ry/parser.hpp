#pragma once

#include "ry/lexer.hpp"
#include "ry/ast.hpp"
#include "ry/source_manager.hpp"

#include <initializer_list>

class Parser {
public:
    explicit Parser(Lexer &lex, const SourceManager *sm = nullptr, int fileId = 0)
        : lex_(lex), sm_(sm), file_id_(fileId) {}

    Program parseProgram();

private:
    Lexer &lex_;
    const SourceManager *sm_ = nullptr;
    int file_id_ = 0;
    int recursion_depth_ = 0;
    static constexpr int MAX_RECURSION_DEPTH = 256;

    struct RecursionGuard {
        Parser &p_;
        explicit RecursionGuard(Parser &p) : p_(p) {
            if (++p_.recursion_depth_ > MAX_RECURSION_DEPTH)
                p_.parseError("expression nesting too deep (limit: " +
                              std::to_string(MAX_RECURSION_DEPTH) + ")");
        }
        ~RecursionGuard() { --p_.recursion_depth_; }
        RecursionGuard(const RecursionGuard &) = delete;
        RecursionGuard &operator=(const RecursionGuard &) = delete;
    };

    // Error helpers
    [[noreturn]] void parseError(int line, const std::string &msg);
    [[noreturn]] void parseError(const std::string &msg);

    SourceLocation locFromToken(const Token &t) const { return {t.line, t.col, file_id_}; }

    std::vector<Directive> parseDirectives();
    void skipNewlines();
    void skipStructuralTokens();
    StmtNode parseImportStatement();
    StmtNode parseStatement();


    StmtNode parseIfStatement();
    StmtNode parseWhileStatement();
    StmtNode parseForStatement();
    StmtNode parseFnStatement(const std::vector<Directive> &directives, bool is_async = false);
    StmtNode parseRecordStatement();
    StmtNode parseTypeAliasStatement();
    StmtNode parseEnumStatement();
    StmtNode parseReturnStatement();
    StmtNode parseExpectStatement();
    void tryParseTrailingBlock(CallStmt &s);
    ExprPtr parseTrailingBlockAsLambda();
    StmtNode parseMatchStatement();
    StmtNode parseSelectStatement();
    Pattern parsePattern();
    std::string parseTypeName();
    std::string parseTypeNameSingle();
    std::vector<StmtNode> parseBlock();
    std::vector<ExprPtr> parseArgList();
    void parseContractClause(const std::string &clauseName, std::vector<ExprPtr> &out);
    void parseEnsureClause(FnStmt &fn);

    // Desugar helper: x = x op rhs
    AssignStmt makeDesugarAssign(const Token &nameTok, const Token &opTok, const std::string &op, ExprPtr rhs);

    // Binary expression helper (left-associative)
    using ParseFn = ExprPtr (Parser::*)();
    ExprPtr parseBinaryLeft(ParseFn operand, std::initializer_list<TokenKind> ops);

    ExprPtr parseExpr();
    ExprPtr parseTerm();
    ExprPtr parsePower();
    ExprPtr parseCast();
    ExprPtr parsePostfix();
    ExprPtr makeErrorPropagateExpr(ExprPtr operand, const Token &tok);
    ExprPtr parsePrimary();
    ExprPtr parseComparison();
    ExprPtr parseBitwiseOr();
    ExprPtr parseBitwiseXor();
    ExprPtr parseBitwiseAnd();
    ExprPtr parseShift();
    ExprPtr parseLogicalNot();
    ExprPtr parseLogicalAnd();
    ExprPtr parseLogicalOr();
    ExprPtr parseTernary();
    ExprPtr parseNullCoalesce();
    ExprPtr parseRange();
    ExprPtr parseLambdaExpr();
    ExprPtr parseSpawnExpr();
    ExprPtr parseAwaitExpr();
};
