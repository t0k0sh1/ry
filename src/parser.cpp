#include "ry/parser.hpp"
#include <stdexcept>
#include <string>
#include <unordered_set>

// ===== A1: parseError helpers =====

[[noreturn]] void Parser::parseError(int line, const std::string &msg) {
    throw std::runtime_error("line " + std::to_string(line) + ": " + msg);
}

[[noreturn]] void Parser::parseError(const std::string &msg) {
    parseError(lex_.peek().line, msg);
}

// ===== A2: parseBinaryLeft helper =====

ExprPtr Parser::parseBinaryLeft(ParseFn operand, std::initializer_list<TokenKind> ops) {
    ExprPtr lhs = (this->*operand)();
    for (;;) {
        TokenKind k = lex_.peek().kind;
        bool matched = false;
        for (TokenKind op : ops) {
            if (k == op) { matched = true; break; }
        }
        if (!matched) break;
        std::string op = lex_.next().value;
        ExprPtr rhs = (this->*operand)();
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

// ===== A3: parseArgList helper =====

std::vector<ExprPtr> Parser::parseArgList() {
    std::vector<ExprPtr> args;
    if (lex_.peek().kind != TokenKind::RParen) {
        args.push_back(parseLogicalOr());
        while (lex_.peek().kind == TokenKind::Comma) {
            lex_.next();
            args.push_back(parseLogicalOr());
        }
    }
    if (lex_.peek().kind != TokenKind::RParen)
        parseError("expected ')'");
    lex_.next(); // consume ')'
    return args;
}

// ===== Core parse methods =====

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

    Token modTok = lex_.peek();
    if (modTok.kind != TokenKind::Ident)
        parseError(modTok.line, "expected module name after 'from'");
    std::string modulePath = lex_.next().value;

    while (lex_.peek().kind == TokenKind::Dot) {
        lex_.next(); // consume '.'
        Token part = lex_.peek();
        if (part.kind != TokenKind::Ident)
            parseError(part.line, "expected identifier after '.'");
        modulePath += "/" + lex_.next().value;
    }
    modulePath += ".ry";

    std::vector<std::string> names;
    if (lex_.peek().kind == TokenKind::Import) {
        lex_.next(); // consume 'import'
        Token name = lex_.peek();
        if (name.kind != TokenKind::Ident)
            parseError(name.line, "expected function name after 'import'");
        names.push_back(lex_.next().value);

        while (lex_.peek().kind == TokenKind::Comma) {
            lex_.next(); // consume ','
            Token next = lex_.peek();
            if (next.kind != TokenKind::Ident)
                parseError(next.line, "expected function name after ','");
            names.push_back(lex_.next().value);
        }
    }

    return ImportStmt{modulePath, names, fromTok.line};
}

StmtNode Parser::parseStatement() {
    Token first = lex_.peek();

    if (first.kind == TokenKind::From)
        parseError(first.line, "'from' import is only allowed at top level");

    if (first.kind == TokenKind::Type)
        return parseTypeStatement();

    if (first.kind == TokenKind::Fn)
        return parseFnStatement();

    if (first.kind == TokenKind::Return)
        return parseReturnStatement();

    if (first.kind == TokenKind::If)
        return parseIfStatement();

    if (first.kind == TokenKind::While)
        return parseWhileStatement();

    // A4: let / const declaration
    if (first.kind == TokenKind::Let || first.kind == TokenKind::Const)
        return parseLetOrConst();

    // identifier-leading statements: assignment, index assignment, or function call
    if (first.kind != TokenKind::Ident)
        parseError(first.line, "expected 'let', 'const', 'if', 'while', 'fn', 'return', or identifier, got '" + first.value + "'");
    lex_.next(); // consume ident

    Token next = lex_.peek();
    if (next.kind == TokenKind::Colon) {
        parseError(next.line, "type annotation requires 'let' or 'const'");
    } else if (next.kind == TokenKind::LBracket) {
        // index assignment: ident[expr] = value
        lex_.next(); // consume '['
        ExprPtr index = parseLogicalOr();
        if (lex_.peek().kind != TokenKind::RBracket)
            parseError("expected ']'");
        lex_.next(); // consume ']'
        if (lex_.peek().kind != TokenKind::Equals)
            parseError("expected '=' after index expression");
        lex_.next(); // consume '='
        ExprPtr val = parseLogicalOr();
        IndexAssignStmt s;
        auto obj = std::make_unique<ExprNode>();
        obj->data = VariableExpr{first.value};
        s.object = std::move(obj);
        s.index = std::move(index);
        s.value = std::move(val);
        return s;
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
        s.args = parseArgList();
        return s;
    }
    parseError(next.line, "expected '=', '[', or '(' after identifier");
}

// ===== A4: parseLetOrConst =====

StmtNode Parser::parseLetOrConst() {
    Token first = lex_.next(); // consume let/const
    bool isConst = (first.kind == TokenKind::Const);

    Token id = lex_.peek();
    if (id.kind != TokenKind::Ident)
        parseError(id.line, "expected identifier after '" + first.value + "'");
    lex_.next(); // consume ident

    std::optional<std::string> typeAnnotation;
    if (lex_.peek().kind == TokenKind::Colon) {
        lex_.next(); // consume ':'
        typeAnnotation = parseTypeName();
    }

    if (lex_.peek().kind != TokenKind::Equals)
        parseError("expected '=' in " + first.value + " declaration");
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

std::vector<StmtNode> Parser::parseBlock() {
    if (lex_.peek().kind != TokenKind::Newline)
        parseError("expected newline after ':'");
    lex_.next(); // consume Newline
    skipNewlines();

    if (lex_.peek().kind != TokenKind::Indent)
        parseError("expected indented block");
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
        parseError("empty block is not allowed");

    if (lex_.peek().kind == TokenKind::Dedent)
        lex_.next(); // consume Dedent

    return stmts;
}

StmtNode Parser::parseWhileStatement() {
    lex_.next(); // consume 'while'
    ExprPtr cond = parseLogicalOr();

    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after while condition");
    lex_.next(); // consume ':'

    auto whileStmt = std::make_unique<WhileStmt>();
    whileStmt->condition = std::move(cond);
    whileStmt->body = parseBlock();
    return whileStmt;
}

StmtNode Parser::parseIfStatement() {
    auto ifStmt = std::make_unique<IfStmt>();

    lex_.next(); // consume 'if'
    ExprPtr cond = parseLogicalOr();

    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after if condition");
    lex_.next(); // consume ':'

    IfBranch branch;
    branch.condition = std::move(cond);
    branch.body = parseBlock();
    ifStmt->branches.push_back(std::move(branch));

    while (lex_.peek().kind == TokenKind::Elif) {
        lex_.next(); // consume 'elif'
        ExprPtr elifCond = parseLogicalOr();

        if (lex_.peek().kind != TokenKind::Colon)
            parseError("expected ':' after elif condition");
        lex_.next(); // consume ':'

        IfBranch elifBranch;
        elifBranch.condition = std::move(elifCond);
        elifBranch.body = parseBlock();
        ifStmt->branches.push_back(std::move(elifBranch));
    }

    if (lex_.peek().kind == TokenKind::Else) {
        lex_.next(); // consume 'else'

        if (lex_.peek().kind != TokenKind::Colon)
            parseError("expected ':' after else");
        lex_.next(); // consume ':'

        ifStmt->else_body = parseBlock();
    }

    return ifStmt;
}

// ===== A2: Binary expression parsers using parseBinaryLeft =====

ExprPtr Parser::parseLogicalOr()  { return parseBinaryLeft(&Parser::parseLogicalAnd, {TokenKind::Or}); }
ExprPtr Parser::parseLogicalAnd() { return parseBinaryLeft(&Parser::parseLogicalNot, {TokenKind::And}); }
ExprPtr Parser::parseComparison() { return parseBinaryLeft(&Parser::parseBitwiseOr, {TokenKind::EqEq, TokenKind::BangEq, TokenKind::Less, TokenKind::LessEq, TokenKind::Greater, TokenKind::GreaterEq}); }
ExprPtr Parser::parseBitwiseOr()  { return parseBinaryLeft(&Parser::parseBitwiseXor, {TokenKind::Pipe}); }
ExprPtr Parser::parseBitwiseXor() { return parseBinaryLeft(&Parser::parseBitwiseAnd, {TokenKind::Caret}); }
ExprPtr Parser::parseBitwiseAnd() { return parseBinaryLeft(&Parser::parseShift, {TokenKind::Amp}); }
ExprPtr Parser::parseShift()      { return parseBinaryLeft(&Parser::parseExpr, {TokenKind::LessLess, TokenKind::GreaterGreater}); }
ExprPtr Parser::parseExpr()       { return parseBinaryLeft(&Parser::parseTerm, {TokenKind::Plus, TokenKind::Minus}); }
ExprPtr Parser::parseTerm()       { return parseBinaryLeft(&Parser::parsePower, {TokenKind::Star, TokenKind::Slash, TokenKind::SlashSlash, TokenKind::Percent}); }

ExprPtr Parser::parsePower() {
    ExprPtr lhs = parsePostfix();
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

ExprPtr Parser::parseLogicalNot() {
    if (lex_.peek().kind == TokenKind::Not) {
        lex_.next(); // consume 'not'
        ExprPtr operand = parseLogicalNot(); // 右結合
        auto unary = std::make_unique<UnaryExpr>();
        unary->op = "not";
        unary->operand = std::move(operand);
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(unary);
        return node;
    }
    return parseComparison();
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
    if (t.kind == TokenKind::String) {
        lex_.next();
        auto node = std::make_unique<ExprNode>();
        node->data = StringExpr{t.value};
        return node;
    }
    if (t.kind == TokenKind::Ident) {
        lex_.next();
        if (lex_.peek().kind == TokenKind::LParen) {
            lex_.next(); // consume '('
            auto call = std::make_unique<CallExpr>();
            call->callee = t.value;
            call->args = parseArgList();
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(call);
            return node;
        }
        auto node = std::make_unique<ExprNode>();
        node->data = VariableExpr{t.value};
        return node;
    }
    if (t.kind == TokenKind::LBrace) {
        lex_.next(); // consume '{'
        auto map = std::make_unique<MapExpr>();
        if (lex_.peek().kind == TokenKind::RBrace) {
            parseError(t.line, "empty map literal requires type annotation (use let m: map[K, V] = ...)");
        }
        // Parse key: value pairs
        ExprPtr key = parseLogicalOr();
        if (lex_.peek().kind != TokenKind::Colon)
            parseError("expected ':' after map key");
        lex_.next(); // consume ':'
        ExprPtr val = parseLogicalOr();
        map->keys.push_back(std::move(key));
        map->values.push_back(std::move(val));
        while (lex_.peek().kind == TokenKind::Comma) {
            lex_.next(); // consume ','
            ExprPtr k = parseLogicalOr();
            if (lex_.peek().kind != TokenKind::Colon)
                parseError("expected ':' after map key");
            lex_.next(); // consume ':'
            ExprPtr v = parseLogicalOr();
            map->keys.push_back(std::move(k));
            map->values.push_back(std::move(v));
        }
        if (lex_.peek().kind != TokenKind::RBrace)
            parseError("expected '}'");
        lex_.next(); // consume '}'
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(map);
        return node;
    }
    if (t.kind == TokenKind::LBracket) {
        lex_.next(); // consume '['
        auto list = std::make_unique<ListExpr>();
        if (lex_.peek().kind != TokenKind::RBracket) {
            list->elements.push_back(parseLogicalOr());
            while (lex_.peek().kind == TokenKind::Comma) {
                lex_.next(); // consume ','
                list->elements.push_back(parseLogicalOr());
            }
        }
        if (lex_.peek().kind != TokenKind::RBracket)
            parseError("expected ']'");
        lex_.next(); // consume ']'
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(list);
        return node;
    }
    if (t.kind == TokenKind::LParen) {
        lex_.next();
        ExprPtr first = parseLogicalOr();
        if (lex_.peek().kind == TokenKind::Comma) {
            // Tuple literal: (expr, expr, ...)
            auto tuple = std::make_unique<TupleExpr>();
            tuple->elements.push_back(std::move(first));
            while (lex_.peek().kind == TokenKind::Comma) {
                lex_.next(); // consume ','
                tuple->elements.push_back(parseLogicalOr());
            }
            if (lex_.peek().kind != TokenKind::RParen)
                parseError("expected ')'");
            lex_.next();
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(tuple);
            return node;
        }
        // Grouping: (expr)
        if (lex_.peek().kind != TokenKind::RParen)
            parseError("expected ')'");
        lex_.next();
        return first;
    }
    parseError(t.line, "unexpected token '" + t.value + "'");
}

StmtNode Parser::parseFnStatement() {
    lex_.next(); // consume 'fn'

    auto fnStmt = std::make_unique<FnStmt>();

    if (lex_.peek().kind == TokenKind::Operator) {
        lex_.next(); // consume 'operator'
        fnStmt->is_operator = true;

        // Read the operator symbol
        Token opTok = lex_.peek();
        std::string opName;
        switch (opTok.kind) {
            case TokenKind::Plus: case TokenKind::Minus:
            case TokenKind::Star: case TokenKind::Slash:
            case TokenKind::Percent: case TokenKind::StarStar:
            case TokenKind::SlashSlash:
            case TokenKind::EqEq: case TokenKind::BangEq:
            case TokenKind::Less: case TokenKind::LessEq:
            case TokenKind::Greater: case TokenKind::GreaterEq:
            case TokenKind::Amp: case TokenKind::Pipe:
            case TokenKind::Caret: case TokenKind::Tilde:
            case TokenKind::LessLess: case TokenKind::GreaterGreater:
            case TokenKind::And: case TokenKind::Or:
            case TokenKind::Not:
                opName = opTok.value;
                lex_.next(); // consume operator
                break;
            default:
                parseError(opTok.line, "expected operator symbol after 'operator'");
        }
        fnStmt->name = "operator" + opName;
    } else {
        Token nameTok = lex_.peek();
        if (nameTok.kind != TokenKind::Ident)
            parseError(nameTok.line, "expected function name after 'fn'");
        lex_.next(); // consume name
        fnStmt->name = nameTok.value;
    }

    if (lex_.peek().kind != TokenKind::LParen)
        parseError("expected '(' after function name");
    lex_.next(); // consume '('

    // parse parameters
    if (lex_.peek().kind != TokenKind::RParen) {
        for (;;) {
            Token paramName = lex_.peek();
            if (paramName.kind != TokenKind::Ident)
                parseError(paramName.line, "expected parameter name");
            lex_.next(); // consume param name

            if (lex_.peek().kind != TokenKind::Colon)
                parseError("expected ':' after parameter name");
            lex_.next(); // consume ':'

            std::string paramType = parseTypeName();

            fnStmt->params.push_back({paramName.value, paramType});

            if (lex_.peek().kind != TokenKind::Comma)
                break;
            lex_.next(); // consume ','
        }
    }

    if (lex_.peek().kind != TokenKind::RParen)
        parseError("expected ')'");
    lex_.next(); // consume ')'

    if (lex_.peek().kind == TokenKind::Arrow) {
        lex_.next(); // consume '->'
        fnStmt->return_type = parseTypeName();
    } else {
        fnStmt->return_type = "Unit";
    }

    // Validate operator parameter count
    if (fnStmt->is_operator) {
        size_t nParams = fnStmt->params.size();
        const std::string &opName = fnStmt->name;
        // Unary-only operators
        if (opName == "operator~" || opName == "operatornot") {
            if (nParams != 1)
                parseError("unary operator requires exactly 1 parameter");
        } else if (nParams != 1 && nParams != 2) {
            parseError("operator function requires 1 or 2 parameters");
        }
    }

    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after return type");
    lex_.next(); // consume ':'

    fnStmt->body = parseBlock();
    return fnStmt;
}

StmtNode Parser::parseReturnStatement() {
    lex_.next(); // consume 'return'
    ReturnStmt s;
    TokenKind next = lex_.peek().kind;
    if (next == TokenKind::Newline || next == TokenKind::Dedent || next == TokenKind::Eof) {
        s.value = nullptr;
    } else {
        s.value = parseLogicalOr();
    }
    return s;
}

StmtNode Parser::parseTypeStatement() {
    lex_.next(); // consume 'type'

    Token nameTok = lex_.peek();
    if (nameTok.kind != TokenKind::Ident)
        parseError(nameTok.line, "expected type name after 'type'");
    lex_.next(); // consume name

    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after type name");
    lex_.next(); // consume ':'

    if (lex_.peek().kind != TokenKind::Newline)
        parseError("expected newline after ':'");
    lex_.next(); // consume Newline
    skipNewlines();

    if (lex_.peek().kind != TokenKind::Indent)
        parseError("expected indented block");
    lex_.next(); // consume Indent

    TypeStmt ts;
    ts.name = nameTok.value;
    std::unordered_set<std::string> seenFields;

    while (lex_.peek().kind != TokenKind::Dedent &&
           lex_.peek().kind != TokenKind::Eof) {
        Token fieldName = lex_.peek();
        if (fieldName.kind != TokenKind::Ident)
            parseError(fieldName.line, "expected field name");
        lex_.next(); // consume field name

        if (lex_.peek().kind != TokenKind::Colon)
            parseError("expected ':' after field name");
        lex_.next(); // consume ':'

        std::string fieldType = parseTypeName();

        if (seenFields.count(fieldName.value))
            parseError(fieldName.line, "duplicate field name '" + fieldName.value + "'");
        ts.fields.push_back({fieldName.value, fieldType});
        seenFields.insert(fieldName.value);

        if (lex_.peek().kind == TokenKind::Newline)
            lex_.next();
        skipNewlines();
    }

    if (ts.fields.empty())
        parseError("type definition must have at least one field");

    if (lex_.peek().kind == TokenKind::Dedent)
        lex_.next(); // consume Dedent

    return ts;
}

std::string Parser::parseTypeName() {
    // Tuple type: (int, float)
    if (lex_.peek().kind == TokenKind::LParen) {
        lex_.next(); // consume '('
        std::string name = "(" + parseTypeName();
        while (lex_.peek().kind == TokenKind::Comma) {
            lex_.next(); // consume ','
            name += ", " + parseTypeName();
        }
        if (lex_.peek().kind != TokenKind::RParen)
            parseError("expected ')' in tuple type");
        lex_.next(); // consume ')'
        name += ")";
        return name;
    }

    Token t = lex_.peek();
    if (t.kind != TokenKind::Ident)
        parseError(t.line, "expected type name");
    std::string name = t.value;
    lex_.next(); // consume type name

    if (lex_.peek().kind == TokenKind::LBracket) {
        lex_.next(); // consume '['
        std::string inner = parseTypeName();
        if (name == "map" && lex_.peek().kind == TokenKind::Comma) {
            // map[K, V] parsing
            lex_.next(); // consume ','
            std::string valueTy = parseTypeName();
            if (lex_.peek().kind != TokenKind::RBracket)
                parseError("expected ']' in map type");
            lex_.next(); // consume ']'
            name += "[" + inner + ", " + valueTy + "]";
        } else {
            if (lex_.peek().kind != TokenKind::RBracket)
                parseError("expected ']' in list type");
            lex_.next(); // consume ']'
            name += "[" + inner + "]";
        }
    } else if (lex_.peek().kind == TokenKind::Less) {
        lex_.next(); // consume '<'
        std::string inner = parseTypeName();
        if (lex_.peek().kind != TokenKind::Greater)
            parseError("expected '>' after generic type parameter");
        lex_.next(); // consume '>'
        name += "<" + inner + ">";
    }

    return name;
}

ExprPtr Parser::parsePostfix() {
    ExprPtr expr = parsePrimary();
    while (lex_.peek().kind == TokenKind::Dot || lex_.peek().kind == TokenKind::LBracket) {
        if (lex_.peek().kind == TokenKind::LBracket) {
            lex_.next(); // consume '['
            ExprPtr index = parseLogicalOr();
            if (lex_.peek().kind != TokenKind::RBracket)
                parseError("expected ']'");
            lex_.next(); // consume ']'
            auto idx = std::make_unique<IndexExpr>();
            idx->object = std::move(expr);
            idx->index = std::move(index);
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(idx);
            expr = std::move(node);
            continue;
        }
        // Dot access follows below
        lex_.next(); // consume '.'
        Token field = lex_.peek();
        if (field.kind != TokenKind::Ident && field.kind != TokenKind::Number)
            parseError(field.line, "expected field name or index after '.'");
        lex_.next(); // consume field name/number
        if (lex_.peek().kind == TokenKind::LParen) {
            // UFCS: a.f(b, c) → f(a, b, c)
            lex_.next(); // consume '('
            auto call = std::make_unique<CallExpr>();
            call->callee = field.value;
            call->args.push_back(std::move(expr));
            auto rest = parseArgList(); // consumes ')'
            for (auto &arg : rest)
                call->args.push_back(std::move(arg));
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(call);
            expr = std::move(node);
        } else {
            // Field access: a.x
            auto fa = std::make_unique<FieldAccessExpr>();
            fa->object = std::move(expr);
            fa->field = field.value;
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(fa);
            expr = std::move(node);
        }
    }
    return expr;
}
