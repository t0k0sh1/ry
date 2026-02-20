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
    ExprPtr parseExpr();
    ExprPtr parseTerm();
    ExprPtr parsePower();
    ExprPtr parsePrimary();
    ExprPtr parseComparison();
    ExprPtr parseLogicalNot();
    ExprPtr parseLogicalAnd();
    ExprPtr parseLogicalOr();
};
