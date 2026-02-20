#include "ry/parser.hpp"
#include <stdexcept>
#include <string>

Program Parser::parseProgram() {
    Program prog;
    skipNewlines();
    while (lex_.peek().kind != TokenKind::Eof) {
        prog.push_back(parseStatement());
        // expect newline or EOF after statement
        if (lex_.peek().kind == TokenKind::Newline)
            lex_.next();
        else if (lex_.peek().kind != TokenKind::Eof)
            throw std::runtime_error("line " + std::to_string(lex_.peek().line) +
                                     ": expected newline, got '" + lex_.peek().value + "'");
        skipNewlines();
    }
    return prog;
}

void Parser::skipNewlines() {
    while (lex_.peek().kind == TokenKind::Newline) lex_.next();
}

StmtNode Parser::parseStatement() {
    Token id = lex_.peek();
    if (id.kind != TokenKind::Ident)
        throw std::runtime_error("line " + std::to_string(id.line) +
                                 ": expected identifier, got '" + id.value + "'");
    lex_.next(); // consume ident

    Token next = lex_.peek();
    if (next.kind == TokenKind::Equals) {
        lex_.next(); // consume '='
        AssignStmt s;
        s.name  = id.value;
        s.value = parseLogicalOr();
        return s;
    } else if (next.kind == TokenKind::LParen) {
        lex_.next(); // consume '('
        CallStmt s;
        s.callee = id.value;
        if (lex_.peek().kind != TokenKind::RParen) {
            s.args.push_back(parseLogicalOr());
            while (lex_.peek().kind == TokenKind::Comma) {
                lex_.next();
                s.args.push_back(parseLogicalOr());
            }
        }
        if (lex_.peek().kind != TokenKind::RParen)
            throw std::runtime_error("line " + std::to_string(lex_.peek().line) +
                                     ": expected ')'");
        lex_.next(); // consume ')'
        return s;
    }
    throw std::runtime_error("line " + std::to_string(next.line) +
                             ": expected '=' or '(' after identifier");
}

ExprPtr Parser::parseExpr() {
    ExprPtr lhs = parseTerm();
    while (lex_.peek().kind == TokenKind::Plus || lex_.peek().kind == TokenKind::Minus) {
        std::string op = lex_.next().value;
        ExprPtr rhs = parseTerm();
        auto bin = std::make_unique<BinaryExpr>();
        bin->op  = op;
        bin->lhs = std::move(lhs);
        bin->rhs = std::move(rhs);
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(bin);
        lhs = std::move(node);
    }
    return lhs;
}

ExprPtr Parser::parseTerm() {
    ExprPtr lhs = parsePower();
    while (lex_.peek().kind == TokenKind::Star   ||
           lex_.peek().kind == TokenKind::Slash  ||
           lex_.peek().kind == TokenKind::SlashSlash ||
           lex_.peek().kind == TokenKind::Percent) {
        std::string op = lex_.next().value;
        ExprPtr rhs = parsePower();
        auto bin = std::make_unique<BinaryExpr>();
        bin->op  = op;
        bin->lhs = std::move(lhs);
        bin->rhs = std::move(rhs);
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(bin);
        lhs = std::move(node);
    }
    return lhs;
}

ExprPtr Parser::parsePower() {
    ExprPtr lhs = parsePrimary();
    if (lex_.peek().kind == TokenKind::StarStar) {
        std::string op = lex_.next().value;
        ExprPtr rhs = parsePower();  // 右結合: 再帰呼び出し
        auto bin = std::make_unique<BinaryExpr>();
        bin->op  = op;
        bin->lhs = std::move(lhs);
        bin->rhs = std::move(rhs);
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(bin);
        return node;
    }
    return lhs;
}

ExprPtr Parser::parsePrimary() {
    Token t = lex_.peek();
    // 単項 + / -
    if (t.kind == TokenKind::Plus || t.kind == TokenKind::Minus) {
        lex_.next();
        ExprPtr operand = parsePrimary(); // 右結合
        auto unary = std::make_unique<UnaryExpr>();
        unary->op = t.value;
        unary->operand = std::move(operand);
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(unary);
        return node;
    }
    if (t.kind == TokenKind::Number) {
        lex_.next();
        auto node = std::make_unique<ExprNode>();
        node->data = NumberExpr{std::stoll(t.value)};
        return node;
    }
    if (t.kind == TokenKind::Float) {
        lex_.next();
        auto node = std::make_unique<ExprNode>();
        node->data = FloatExpr{std::stod(t.value)};
        return node;
    }
    if (t.kind == TokenKind::Ident) {
        lex_.next();
        auto node = std::make_unique<ExprNode>();
        node->data = VariableExpr{t.value};
        return node;
    }
    if (t.kind == TokenKind::LParen) {
        lex_.next();
        ExprPtr e = parseLogicalOr();
        if (lex_.peek().kind != TokenKind::RParen)
            throw std::runtime_error("line " + std::to_string(lex_.peek().line) +
                                     ": expected ')'");
        lex_.next();
        return e;
    }
    throw std::runtime_error("line " + std::to_string(t.line) +
                             ": unexpected token '" + t.value + "'");
}

ExprPtr Parser::parseComparison() {
    ExprPtr lhs = parseExpr();
    for (;;) {
        TokenKind k = lex_.peek().kind;
        if (k != TokenKind::EqEq    && k != TokenKind::BangEq  &&
            k != TokenKind::Less    && k != TokenKind::LessEq  &&
            k != TokenKind::Greater && k != TokenKind::GreaterEq)
            break;
        std::string op = lex_.next().value;
        ExprPtr rhs = parseExpr();
        auto bin = std::make_unique<BinaryExpr>();
        bin->op  = op;
        bin->lhs = std::move(lhs);
        bin->rhs = std::move(rhs);
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(bin);
        lhs = std::move(node);
    }
    return lhs;
}

ExprPtr Parser::parseLogicalNot() {
    if (lex_.peek().kind == TokenKind::Not) {
        lex_.next(); // consume 'not'
        ExprPtr operand = parseLogicalNot(); // 右結合（not not a が動く）
        auto unary = std::make_unique<UnaryExpr>();
        unary->op = "not";
        unary->operand = std::move(operand);
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(unary);
        return node;
    }
    return parseComparison();
}

ExprPtr Parser::parseLogicalAnd() {
    ExprPtr lhs = parseLogicalNot();
    while (lex_.peek().kind == TokenKind::And) {
        std::string op = lex_.next().value;
        ExprPtr rhs = parseLogicalNot();
        auto bin = std::make_unique<BinaryExpr>();
        bin->op  = op;
        bin->lhs = std::move(lhs);
        bin->rhs = std::move(rhs);
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(bin);
        lhs = std::move(node);
    }
    return lhs;
}

ExprPtr Parser::parseLogicalOr() {
    ExprPtr lhs = parseLogicalAnd();
    while (lex_.peek().kind == TokenKind::Or) {
        std::string op = lex_.next().value;
        ExprPtr rhs = parseLogicalAnd();
        auto bin = std::make_unique<BinaryExpr>();
        bin->op  = op;
        bin->lhs = std::move(lhs);
        bin->rhs = std::move(rhs);
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(bin);
        lhs = std::move(node);
    }
    return lhs;
}
