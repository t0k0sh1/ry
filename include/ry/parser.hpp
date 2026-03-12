#pragma once

#include "ry/lexer.hpp"
#include "ry/ast.hpp"

#include <initializer_list>

class Parser {
public:
    explicit Parser(Lexer &lex) : lex_(lex) {}

    Program parseProgram();

private:
    Lexer &lex_;

    // Error helpers
    [[noreturn]] void parseError(int line, const std::string &msg);
    [[noreturn]] void parseError(const std::string &msg);

    void skipNewlines();
    StmtNode parseImportStatement();
    StmtNode parseStatement();
    StmtNode parseLetOrConst();
    StmtNode parseIfStatement();
    StmtNode parseWhileStatement();
    StmtNode parseForStatement();
    StmtNode parseFnStatement();
    StmtNode parseTypeStatement();
    StmtNode parseEnumStatement();
    StmtNode parseReturnStatement();
    StmtNode parseDescribeStatement();
    StmtNode parseExpectStatement();
    std::string parseTypeName();
    std::vector<StmtNode> parseBlock();
    std::vector<ExprPtr> parseArgList();

    // Binary expression helper (left-associative)
    using ParseFn = ExprPtr (Parser::*)();
    ExprPtr parseBinaryLeft(ParseFn operand, std::initializer_list<TokenKind> ops);

    ExprPtr parseExpr();
    ExprPtr parseTerm();
    ExprPtr parsePower();
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
    ExprPtr parseLambdaExpr();
};
