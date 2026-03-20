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

    // Error helpers
    [[noreturn]] void parseError(int line, const std::string &msg);
    [[noreturn]] void parseError(const std::string &msg);

    SourceLocation locFromToken(const Token &t) const { return {t.line, t.col, file_id_}; }

    std::vector<Directive> parseDirectives();
    void skipNewlines();
    StmtNode parseImportStatement();
    StmtNode parseStatement();
    StmtNode parseLetOrVar();
    StmtNode parseIfStatement();
    StmtNode parseWhileStatement();
    StmtNode parseForStatement();
    StmtNode parseFnStatement(const std::vector<Directive> &directives);
    StmtNode parseRecordStatement();
    StmtNode parseTypeAliasStatement();
    StmtNode parseEnumStatement();
    StmtNode parseReturnStatement();
    StmtNode parseExpectStatement();
    void tryParseTrailingBlock(CallStmt &s);
    ExprPtr parseTrailingBlockAsLambda();
    StmtNode parseMatchStatement();
    Pattern parsePattern();
    std::string parseTypeName();
    std::string parseTypeNameSingle();
    std::vector<StmtNode> parseBlock();
    std::vector<ExprPtr> parseArgList();
    void parseContractClause(const std::string &clauseName, std::vector<ExprPtr> &out);

    // Binary expression helper (left-associative)
    using ParseFn = ExprPtr (Parser::*)();
    ExprPtr parseBinaryLeft(ParseFn operand, std::initializer_list<TokenKind> ops);

    ExprPtr parseExpr();
    ExprPtr parseTerm();
    ExprPtr parsePower();
    ExprPtr parseCast();
    ExprPtr parsePostfix();
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
};
