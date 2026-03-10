#pragma once

#include "ry/lexer.hpp"
#include "ry/ast.hpp"

class Parser {
public:
    explicit Parser(Lexer &lex) : lex_(lex) {}

    Program parseProgram();

private:
    Lexer &lex_;

    void skipNewlines();
    StmtNode parseStatement();
    StmtNode parseIfStatement();
    StmtNode parseWhileStatement();
    StmtNode parseFnStatement();
    StmtNode parseReturnStatement();
    std::vector<StmtNode> parseBlock();
    ExprPtr parseExpr();
    ExprPtr parseTerm();
    ExprPtr parsePower();
    ExprPtr parsePrimary();
    ExprPtr parseComparison();
    ExprPtr parseBitwiseOr();
    ExprPtr parseBitwiseXor();
    ExprPtr parseBitwiseAnd();
    ExprPtr parseShift();
    ExprPtr parseLogicalNot();
    ExprPtr parseLogicalAnd();
    ExprPtr parseLogicalOr();
};
