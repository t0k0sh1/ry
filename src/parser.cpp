#include "ry/parser.hpp"
#include <stdexcept>
#include <string>

Program Parser::parseProgram() {
    Program prog;
    skipNewlines();
    while (lex_.peek().kind != TokenKind::Eof) {
        if (lex_.peek().kind == TokenKind::From) {
            prog.push_back(parseImportStatement());
        } else {
            prog.push_back(parseStatement());
        }
        if (lex_.peek().kind == TokenKind::Newline)
            lex_.next();
        skipNewlines();
    }
    return prog;
}

void Parser::skipNewlines() {
    while (lex_.peek().kind == TokenKind::Newline) lex_.next();
}

StmtNode Parser::parseImportStatement() {
    Token fromTok = lex_.next(); // consume 'from'

    // Parse module path: Ident (Dot Ident)*
    Token modTok = lex_.peek();
    if (modTok.kind != TokenKind::Ident)
        throw std::runtime_error("line " + std::to_string(modTok.line) +
                                 ": expected module name after 'from'");
    std::string modulePath = lex_.next().value;

    while (lex_.peek().kind == TokenKind::Dot) {
        lex_.next(); // consume '.'
        Token part = lex_.peek();
        if (part.kind != TokenKind::Ident)
            throw std::runtime_error("line " + std::to_string(part.line) +
                                     ": expected identifier after '.'");
        modulePath += "/" + lex_.next().value;
    }
    modulePath += ".ry";

    // Parse optional import list
    std::vector<std::string> names;
    if (lex_.peek().kind == TokenKind::Import) {
        lex_.next(); // consume 'import'
        Token name = lex_.peek();
        if (name.kind != TokenKind::Ident)
            throw std::runtime_error("line " + std::to_string(name.line) +
                                     ": expected function name after 'import'");
        names.push_back(lex_.next().value);

        while (lex_.peek().kind == TokenKind::Comma) {
            lex_.next(); // consume ','
            Token next = lex_.peek();
            if (next.kind != TokenKind::Ident)
                throw std::runtime_error("line " + std::to_string(next.line) +
                                         ": expected function name after ','");
            names.push_back(lex_.next().value);
        }
    }

    return ImportStmt{modulePath, names, fromTok.line};
}

StmtNode Parser::parseStatement() {
    Token first = lex_.peek();

    // from statement is only allowed at top level
    if (first.kind == TokenKind::From)
        throw std::runtime_error("line " + std::to_string(first.line) +
                                 ": 'from' import is only allowed at top level");

    // fn statement
    if (first.kind == TokenKind::Fn)
        return parseFnStatement();

    // return statement
    if (first.kind == TokenKind::Return)
        return parseReturnStatement();

    // if statement
    if (first.kind == TokenKind::If)
        return parseIfStatement();

    // while statement
    if (first.kind == TokenKind::While)
        return parseWhileStatement();

    // let / const declaration
    if (first.kind == TokenKind::Let || first.kind == TokenKind::Const) {
        bool isConst = (first.kind == TokenKind::Const);
        lex_.next(); // consume let/const

        Token id = lex_.peek();
        if (id.kind != TokenKind::Ident)
            throw std::runtime_error("line " + std::to_string(id.line) +
                                     ": expected identifier after '" + first.value + "'");
        lex_.next(); // consume ident

        std::optional<std::string> typeAnnotation;
        if (lex_.peek().kind == TokenKind::Colon) {
            lex_.next(); // consume ':'
            Token typeTok = lex_.peek();
            if (typeTok.kind != TokenKind::Ident)
                throw std::runtime_error("line " + std::to_string(typeTok.line) +
                                         ": expected type name after ':'");
            if (typeTok.value != "int" && typeTok.value != "float" && typeTok.value != "bool")
                throw std::runtime_error("line " + std::to_string(typeTok.line) +
                                         ": unknown type '" + typeTok.value + "'");
            typeAnnotation = typeTok.value;
            lex_.next(); // consume type name
        }

        if (lex_.peek().kind != TokenKind::Equals)
            throw std::runtime_error("line " + std::to_string(lex_.peek().line) +
                                     ": expected '=' in " + first.value + " declaration");
        lex_.next(); // consume '='

        ExprPtr value = parseLogicalOr();

        if (isConst) {
            ConstStmt s;
            s.name = id.value;
            s.type_annotation = typeAnnotation;
            s.value = std::move(value);
            return s;
        } else {
            LetStmt s;
            s.name = id.value;
            s.type_annotation = typeAnnotation;
            s.value = std::move(value);
            return s;
        }
    }

    // identifier-leading statements: assignment or function call
    if (first.kind != TokenKind::Ident)
        throw std::runtime_error("line " + std::to_string(first.line) +
                                 ": expected 'let', 'const', 'if', 'while', 'fn', 'return', or identifier, got '" + first.value + "'");
    lex_.next(); // consume ident

    Token next = lex_.peek();
    if (next.kind == TokenKind::Colon) {
        throw std::runtime_error("line " + std::to_string(next.line) +
                                 ": type annotation requires 'let' or 'const'");
    } else if (next.kind == TokenKind::Equals) {
        lex_.next(); // consume '='
        AssignStmt s;
        s.name  = first.value;
        s.value = parseLogicalOr();
        return s;
    } else if (next.kind == TokenKind::LParen) {
        lex_.next(); // consume '('
        CallStmt s;
        s.callee = first.value;
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
                             ": expected '=', or '(' after identifier");
}

std::vector<StmtNode> Parser::parseBlock() {
    if (lex_.peek().kind != TokenKind::Newline)
        throw std::runtime_error("line " + std::to_string(lex_.peek().line) +
                                 ": expected newline after ':'");
    lex_.next(); // consume Newline
    skipNewlines();

    if (lex_.peek().kind != TokenKind::Indent)
        throw std::runtime_error("line " + std::to_string(lex_.peek().line) +
                                 ": expected indented block");
    lex_.next(); // consume Indent

    std::vector<StmtNode> stmts;
    while (lex_.peek().kind != TokenKind::Dedent &&
           lex_.peek().kind != TokenKind::Eof) {
        stmts.push_back(parseStatement());
        if (lex_.peek().kind == TokenKind::Newline)
            lex_.next();
        skipNewlines();
    }

    if (stmts.empty())
        throw std::runtime_error("line " + std::to_string(lex_.peek().line) +
                                 ": empty block is not allowed");

    if (lex_.peek().kind == TokenKind::Dedent)
        lex_.next(); // consume Dedent

    return stmts;
}

StmtNode Parser::parseWhileStatement() {
    lex_.next(); // consume 'while'
    ExprPtr cond = parseLogicalOr();

    if (lex_.peek().kind != TokenKind::Colon)
        throw std::runtime_error("line " + std::to_string(lex_.peek().line) +
                                 ": expected ':' after while condition");
    lex_.next(); // consume ':'

    auto whileStmt = std::make_unique<WhileStmt>();
    whileStmt->condition = std::move(cond);
    whileStmt->body = parseBlock();
    return whileStmt;
}

StmtNode Parser::parseIfStatement() {
    auto ifStmt = std::make_unique<IfStmt>();

    // if branch
    lex_.next(); // consume 'if'
    ExprPtr cond = parseLogicalOr();

    if (lex_.peek().kind != TokenKind::Colon)
        throw std::runtime_error("line " + std::to_string(lex_.peek().line) +
                                 ": expected ':' after if condition");
    lex_.next(); // consume ':'

    IfBranch branch;
    branch.condition = std::move(cond);
    branch.body = parseBlock();
    ifStmt->branches.push_back(std::move(branch));

    // elif branches
    while (lex_.peek().kind == TokenKind::Elif) {
        lex_.next(); // consume 'elif'
        ExprPtr elifCond = parseLogicalOr();

        if (lex_.peek().kind != TokenKind::Colon)
            throw std::runtime_error("line " + std::to_string(lex_.peek().line) +
                                     ": expected ':' after elif condition");
        lex_.next(); // consume ':'

        IfBranch elifBranch;
        elifBranch.condition = std::move(elifCond);
        elifBranch.body = parseBlock();
        ifStmt->branches.push_back(std::move(elifBranch));
    }

    // else branch
    if (lex_.peek().kind == TokenKind::Else) {
        lex_.next(); // consume 'else'

        if (lex_.peek().kind != TokenKind::Colon)
            throw std::runtime_error("line " + std::to_string(lex_.peek().line) +
                                     ": expected ':' after else");
        lex_.next(); // consume ':'

        ifStmt->else_body = parseBlock();
    }

    return ifStmt;
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
    // 単項 + / - / ~
    if (t.kind == TokenKind::Plus  ||
        t.kind == TokenKind::Minus ||
        t.kind == TokenKind::Tilde) {
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
    if (t.kind == TokenKind::True || t.kind == TokenKind::False) {
        lex_.next();
        auto node = std::make_unique<ExprNode>();
        node->data = BoolExpr{t.kind == TokenKind::True};
        return node;
    }
    if (t.kind == TokenKind::Ident) {
        lex_.next();
        if (lex_.peek().kind == TokenKind::LParen) {
            lex_.next(); // consume '('
            auto call = std::make_unique<CallExpr>();
            call->callee = t.value;
            if (lex_.peek().kind != TokenKind::RParen) {
                call->args.push_back(parseLogicalOr());
                while (lex_.peek().kind == TokenKind::Comma) {
                    lex_.next();
                    call->args.push_back(parseLogicalOr());
                }
            }
            if (lex_.peek().kind != TokenKind::RParen)
                throw std::runtime_error("line " + std::to_string(lex_.peek().line) +
                                         ": expected ')'");
            lex_.next(); // consume ')'
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(call);
            return node;
        }
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
    ExprPtr lhs = parseBitwiseOr();
    for (;;) {
        TokenKind k = lex_.peek().kind;
        if (k != TokenKind::EqEq    && k != TokenKind::BangEq  &&
            k != TokenKind::Less    && k != TokenKind::LessEq  &&
            k != TokenKind::Greater && k != TokenKind::GreaterEq)
            break;
        std::string op = lex_.next().value;
        ExprPtr rhs = parseBitwiseOr();
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

ExprPtr Parser::parseBitwiseOr() {
    ExprPtr lhs = parseBitwiseXor();
    while (lex_.peek().kind == TokenKind::Pipe) {
        std::string op = lex_.next().value;
        ExprPtr rhs = parseBitwiseXor();
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

ExprPtr Parser::parseBitwiseXor() {
    ExprPtr lhs = parseBitwiseAnd();
    while (lex_.peek().kind == TokenKind::Caret) {
        std::string op = lex_.next().value;
        ExprPtr rhs = parseBitwiseAnd();
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

ExprPtr Parser::parseBitwiseAnd() {
    ExprPtr lhs = parseShift();
    while (lex_.peek().kind == TokenKind::Amp) {
        std::string op = lex_.next().value;
        ExprPtr rhs = parseShift();
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

ExprPtr Parser::parseShift() {
    ExprPtr lhs = parseExpr();
    while (lex_.peek().kind == TokenKind::LessLess ||
           lex_.peek().kind == TokenKind::GreaterGreater) {
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

StmtNode Parser::parseFnStatement() {
    lex_.next(); // consume 'fn'

    Token nameTok = lex_.peek();
    if (nameTok.kind != TokenKind::Ident)
        throw std::runtime_error("line " + std::to_string(nameTok.line) +
                                 ": expected function name after 'fn'");
    lex_.next(); // consume name

    if (lex_.peek().kind != TokenKind::LParen)
        throw std::runtime_error("line " + std::to_string(lex_.peek().line) +
                                 ": expected '(' after function name");
    lex_.next(); // consume '('

    auto fnStmt = std::make_unique<FnStmt>();
    fnStmt->name = nameTok.value;

    // parse parameters
    if (lex_.peek().kind != TokenKind::RParen) {
        for (;;) {
            Token paramName = lex_.peek();
            if (paramName.kind != TokenKind::Ident)
                throw std::runtime_error("line " + std::to_string(paramName.line) +
                                         ": expected parameter name");
            lex_.next(); // consume param name

            if (lex_.peek().kind != TokenKind::Colon)
                throw std::runtime_error("line " + std::to_string(lex_.peek().line) +
                                         ": expected ':' after parameter name");
            lex_.next(); // consume ':'

            Token paramType = lex_.peek();
            if (paramType.kind != TokenKind::Ident)
                throw std::runtime_error("line " + std::to_string(paramType.line) +
                                         ": expected type name");
            if (paramType.value != "int" && paramType.value != "float" && paramType.value != "bool")
                throw std::runtime_error("line " + std::to_string(paramType.line) +
                                         ": unknown type '" + paramType.value + "'");
            lex_.next(); // consume type

            fnStmt->params.push_back({paramName.value, paramType.value});

            if (lex_.peek().kind != TokenKind::Comma)
                break;
            lex_.next(); // consume ','
        }
    }

    if (lex_.peek().kind != TokenKind::RParen)
        throw std::runtime_error("line " + std::to_string(lex_.peek().line) +
                                 ": expected ')'");
    lex_.next(); // consume ')'

    if (lex_.peek().kind != TokenKind::Arrow)
        throw std::runtime_error("line " + std::to_string(lex_.peek().line) +
                                 ": expected '->' after ')'");
    lex_.next(); // consume '->'

    Token retType = lex_.peek();
    if (retType.kind != TokenKind::Ident)
        throw std::runtime_error("line " + std::to_string(retType.line) +
                                 ": expected return type");
    if (retType.value != "int" && retType.value != "float" && retType.value != "bool")
        throw std::runtime_error("line " + std::to_string(retType.line) +
                                 ": unknown type '" + retType.value + "'");
    fnStmt->return_type = retType.value;
    lex_.next(); // consume return type

    if (lex_.peek().kind != TokenKind::Colon)
        throw std::runtime_error("line " + std::to_string(lex_.peek().line) +
                                 ": expected ':' after return type");
    lex_.next(); // consume ':'

    fnStmt->body = parseBlock();
    return fnStmt;
}

StmtNode Parser::parseReturnStatement() {
    lex_.next(); // consume 'return'
    ReturnStmt s;
    s.value = parseLogicalOr();
    return s;
}
