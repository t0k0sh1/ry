#include "ry/parser.hpp"
#include <stdexcept>
#include <string>
#include <unordered_set>

// ===== Helper: parse integer literals (decimal, hex, binary) =====

static int64_t parseIntLiteral(const std::string& s) {
    if (s.size() > 2 && s[0] == '0') {
        if (s[1] == 'x' || s[1] == 'X') return std::stoll(s, nullptr, 16);
        if (s[1] == 'b' || s[1] == 'B') return std::stoll(s.substr(2), nullptr, 2);
    }
    return std::stoll(s);
}

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

// ===== Directive parsing =====

static const std::unordered_set<std::string> known_directives = {"deprecated"};

std::vector<Directive> Parser::parseDirectives() {
    std::vector<Directive> directives;
    while (lex_.peek().kind == TokenKind::At) {
        Token atTok = lex_.next(); // consume '@'
        Token nameTok = lex_.peek();
        if (nameTok.kind != TokenKind::Ident)
            parseError(nameTok.line, "expected directive name after '@'");
        lex_.next(); // consume name

        if (known_directives.find(nameTok.value) == known_directives.end())
            parseError(atTok.line, "unknown directive '@" + nameTok.value + "'");

        Directive d;
        d.name = nameTok.value;
        d.line = atTok.line;

        // Optional parameters: @name(key=value, ...)
        if (lex_.peek().kind == TokenKind::LParen) {
            lex_.next(); // consume '('
            while (lex_.peek().kind != TokenKind::RParen) {
                Token keyTok = lex_.peek();
                if (keyTok.kind != TokenKind::Ident)
                    parseError(keyTok.line, "expected parameter name in directive");
                lex_.next(); // consume key

                if (lex_.peek().kind != TokenKind::Equals)
                    parseError("expected '=' after directive parameter name");
                lex_.next(); // consume '='

                Token valTok = lex_.peek();
                if (valTok.kind != TokenKind::Ident &&
                    valTok.kind != TokenKind::Number &&
                    valTok.kind != TokenKind::Float &&
                    valTok.kind != TokenKind::String &&
                    valTok.kind != TokenKind::True &&
                    valTok.kind != TokenKind::False)
                    parseError(valTok.line, "expected value after '=' in directive parameter");
                lex_.next(); // consume value

                d.params.push_back({keyTok.value, valTok.value});

                if (lex_.peek().kind == TokenKind::Comma)
                    lex_.next(); // consume ','
            }
            if (lex_.peek().kind != TokenKind::RParen)
                parseError("expected ')' after directive parameters");
            lex_.next(); // consume ')'
        }

        directives.push_back(std::move(d));

        // Consume newlines between directives
        skipNewlines();
    }
    return directives;
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
    // Parse directives before the statement
    auto directives = parseDirectives();

    Token first = lex_.peek();

    if (first.kind == TokenKind::From)
        parseError(first.line, "'from' import is only allowed at top level");

    // Directive-accepting statements: fn, type, let/var
    if (first.kind == TokenKind::Type) {
        auto stmt = parseTypeStatement();
        auto &ts = std::get<TypeStmt>(stmt);
        ts.directives = std::move(directives);
        return stmt;
    }

    if (first.kind == TokenKind::Fn) {
        auto stmt = parseFnStatement();
        auto &fs = std::get<std::unique_ptr<FnStmt>>(stmt);
        fs->directives = std::move(directives);
        return stmt;
    }

    if (first.kind == TokenKind::Let || first.kind == TokenKind::Var) {
        auto stmt = parseLetOrVar();
        if (auto *ls = std::get_if<LetStmt>(&stmt))
            ls->directives = std::move(directives);
        else if (auto *vs = std::get_if<VarStmt>(&stmt))
            vs->directives = std::move(directives);
        return stmt;
    }

    // All remaining statements do not accept directives
    if (!directives.empty())
        parseError(first.line, "directives are not supported on this statement");

    if (first.kind == TokenKind::Describe)
        return parseDescribeStatement();

    if (first.kind == TokenKind::Expect)
        return parseExpectStatement();

    if (first.kind == TokenKind::Enum)
        return parseEnumStatement();

    if (first.kind == TokenKind::Return)
        return parseReturnStatement();

    if (first.kind == TokenKind::Match)
        return parseMatchStatement();

    if (first.kind == TokenKind::If)
        return parseIfStatement();

    if (first.kind == TokenKind::While)
        return parseWhileStatement();

    if (first.kind == TokenKind::For)
        return parseForStatement();

    if (first.kind == TokenKind::Break) {
        lex_.next();
        return BreakStmt{};
    }

    if (first.kind == TokenKind::Continue) {
        lex_.next();
        return ContinueStmt{};
    }

    // identifier-leading statements: assignment, index assignment, or function call
    if (first.kind != TokenKind::Ident)
        parseError(first.line, "expected 'let', 'var', 'if', 'while', 'for', 'fn', 'return', 'break', 'continue', 'enum', 'match', 'describe', 'expect', or identifier, got '" + first.value + "'");
    lex_.next(); // consume ident

    Token next = lex_.peek();
    if (next.kind == TokenKind::Colon) {
        parseError(next.line, "type annotation requires 'let' or 'var'");
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
    } else if (next.kind == TokenKind::Dot) {
        // field assignment: ident.field = value
        lex_.next(); // consume '.'
        Token fieldTok = lex_.peek();
        if (fieldTok.kind != TokenKind::Ident)
            parseError(fieldTok.line, "expected field name after '.'");
        lex_.next(); // consume field name

        if (lex_.peek().kind == TokenKind::LParen) {
            // UFCS call statement: ident.method(args)
            lex_.next(); // consume '('
            CallStmt s;
            s.callee = fieldTok.value;
            auto obj = std::make_unique<ExprNode>();
            obj->data = VariableExpr{first.value};
            s.args.push_back(std::move(obj));
            auto rest = parseArgList();
            for (auto &arg : rest)
                s.args.push_back(std::move(arg));
            return s;
        }

        if (lex_.peek().kind != TokenKind::Equals)
            parseError("expected '=' after field name");
        lex_.next(); // consume '='
        ExprPtr val = parseLogicalOr();
        FieldAssignStmt s;
        auto obj = std::make_unique<ExprNode>();
        obj->data = VariableExpr{first.value};
        s.object = std::move(obj);
        s.field = fieldTok.value;
        s.value = std::move(val);
        return s;
    } else if (next.kind == TokenKind::Equals) {
        lex_.next(); // consume '='
        AssignStmt s;
        s.name  = first.value;
        s.value = parseLogicalOr();
        return s;
    } else if (next.kind == TokenKind::PlusEq  || next.kind == TokenKind::MinusEq ||
               next.kind == TokenKind::StarEq  || next.kind == TokenKind::SlashEq ||
               next.kind == TokenKind::PercentEq) {
        // Compound assignment: desugar x += e → x = x + e
        Token opTok = lex_.next(); // consume +=, -=, etc.
        std::string op(1, opTok.value[0]); // extract "+" from "+="
        ExprPtr rhs = parseLogicalOr();
        auto varRef = std::make_unique<ExprNode>();
        varRef->data = VariableExpr{first.value};
        auto bin = std::make_unique<BinaryExpr>();
        bin->op = op;
        bin->lhs = std::move(varRef);
        bin->rhs = std::move(rhs);
        auto binNode = std::make_unique<ExprNode>();
        binNode->data = std::move(bin);
        AssignStmt s;
        s.name = first.value;
        s.value = std::move(binNode);
        return s;
    } else if (next.kind == TokenKind::LParen) {
        lex_.next(); // consume '('
        CallStmt s;
        s.callee = first.value;
        s.args = parseArgList();
        return s;
    }
    parseError(next.line, "expected '=', '+=', '-=', '*=', '/=', '%=', '.', '[', or '(' after identifier");
}

// ===== A4: parseLetOrVar =====

StmtNode Parser::parseLetOrVar() {
    Token first = lex_.next(); // consume let/var
    bool isVar = (first.kind == TokenKind::Var);

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

    if (isVar) {
        VarStmt s;
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

StmtNode Parser::parseForStatement() {
    lex_.next(); // consume 'for'

    Token varTok = lex_.peek();
    if (varTok.kind != TokenKind::Ident)
        parseError(varTok.line, "expected variable name after 'for'");
    lex_.next(); // consume var name

    if (lex_.peek().kind != TokenKind::In)
        parseError("expected 'in' after variable name in for loop");
    lex_.next(); // consume 'in'

    ExprPtr iterable = parseLogicalOr();

    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after for loop iterable");
    lex_.next(); // consume ':'

    auto forStmt = std::make_unique<ForStmt>();
    forStmt->var_name = varTok.value;
    forStmt->iterable = std::move(iterable);
    forStmt->body = parseBlock();
    return forStmt;
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
ExprPtr Parser::parseComparison() {
    ExprPtr lhs = parseBitwiseOr();
    for (;;) {
        TokenKind k = lex_.peek().kind;
        // Handle "not in" as a two-token operator
        if (k == TokenKind::Not) {
            auto saved = lex_.saveState();
            lex_.next(); // consume 'not'
            if (lex_.peek().kind == TokenKind::In) {
                lex_.next(); // consume 'in'
                ExprPtr rhs = parseBitwiseOr();
                auto bin = std::make_unique<BinaryExpr>();
                bin->op = "not in";
                bin->lhs = std::move(lhs);
                bin->rhs = std::move(rhs);
                auto node = std::make_unique<ExprNode>();
                node->data = std::move(bin);
                lhs = std::move(node);
                continue;
            }
            lex_.restoreState(saved);
            break;
        }
        if (k == TokenKind::EqEq || k == TokenKind::BangEq ||
            k == TokenKind::Less || k == TokenKind::LessEq ||
            k == TokenKind::Greater || k == TokenKind::GreaterEq ||
            k == TokenKind::In) {
            std::string op = lex_.next().value;
            ExprPtr rhs = parseBitwiseOr();
            auto bin = std::make_unique<BinaryExpr>();
            bin->op = op;
            bin->lhs = std::move(lhs);
            bin->rhs = std::move(rhs);
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(bin);
            lhs = std::move(node);
            continue;
        }
        break;
    }
    return lhs;
}
ExprPtr Parser::parseBitwiseOr()  { return parseBinaryLeft(&Parser::parseBitwiseXor, {TokenKind::Pipe}); }
ExprPtr Parser::parseBitwiseXor() { return parseBinaryLeft(&Parser::parseBitwiseAnd, {TokenKind::Caret}); }
ExprPtr Parser::parseBitwiseAnd() { return parseBinaryLeft(&Parser::parseShift, {TokenKind::Amp}); }
ExprPtr Parser::parseShift()      { return parseBinaryLeft(&Parser::parseExpr, {TokenKind::LessLess, TokenKind::GreaterGreater, TokenKind::GreaterGreaterGreater}); }
ExprPtr Parser::parseExpr()       { return parseBinaryLeft(&Parser::parseTerm, {TokenKind::Plus, TokenKind::Minus}); }
ExprPtr Parser::parseTerm()       { return parseBinaryLeft(&Parser::parsePower, {TokenKind::Star, TokenKind::Slash, TokenKind::SlashSlash, TokenKind::Percent}); }

ExprPtr Parser::parsePower() {
    ExprPtr lhs = parseCast();
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

ExprPtr Parser::parseCast() {
    ExprPtr expr = parsePostfix();
    while (lex_.peek().kind == TokenKind::As) {
        lex_.next(); // consume 'as'
        Token typeTok = lex_.peek();
        if (typeTok.kind != TokenKind::Ident)
            parseError(typeTok.line, "expected type name after 'as'");
        std::string targetType = lex_.next().value;
        auto cast = std::make_unique<CastExpr>();
        cast->value = std::move(expr);
        cast->target_type = targetType;
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(cast);
        expr = std::move(node);
    }
    return expr;
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
        node->data = NumberExpr{parseIntLiteral(t.value)};
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
    // f-string: FStringEnd (no interpolation) or FStringStart...FStringEnd
    if (t.kind == TokenKind::FStringEnd) {
        lex_.next();
        auto node = std::make_unique<ExprNode>();
        node->data = StringExpr{t.value};
        return node;
    }
    if (t.kind == TokenKind::FStringStart) {
        lex_.next(); // consume FStringStart
        auto interp = std::make_unique<InterpolatedStringExpr>();
        interp->parts.push_back(t.value);
        interp->exprs.push_back(parseLogicalOr());
        while (lex_.peek().kind == TokenKind::FStringMid) {
            Token mid = lex_.next();
            interp->parts.push_back(mid.value);
            interp->exprs.push_back(parseLogicalOr());
        }
        if (lex_.peek().kind != TokenKind::FStringEnd)
            parseError("expected end of f-string");
        Token end = lex_.next();
        interp->parts.push_back(end.value);
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(interp);
        return node;
    }
    if (t.kind == TokenKind::Result) {
        lex_.next(); // consume 'result'
        auto node = std::make_unique<ExprNode>();
        node->data = ResultExpr{};
        return node;
    }
    if (t.kind == TokenKind::Old) {
        lex_.next(); // consume 'old'
        if (lex_.peek().kind != TokenKind::LParen)
            parseError("expected '(' after 'old'");
        lex_.next(); // consume '('
        ExprPtr expr = parseLogicalOr();
        if (lex_.peek().kind != TokenKind::RParen)
            parseError("expected ')' after old expression");
        lex_.next(); // consume ')'
        auto oldExpr = std::make_unique<OldExpr>();
        oldExpr->expr = std::move(expr);
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(oldExpr);
        return node;
    }
    // Ok(expr) / Err(expr) → CallExpr
    if (t.kind == TokenKind::Ok || t.kind == TokenKind::Err) {
        std::string callee = lex_.next().value;
        if (lex_.peek().kind != TokenKind::LParen)
            parseError("expected '(' after '" + callee + "'");
        lex_.next(); // consume '('
        auto call = std::make_unique<CallExpr>();
        call->callee = callee;
        call->args = parseArgList();
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(call);
        return node;
    }
    if (t.kind == TokenKind::Ident) {
        lex_.next();
        if (lex_.peek().kind == TokenKind::ColonColon) {
            lex_.next(); // consume '::'
            Token variant = lex_.peek();
            if (variant.kind != TokenKind::Ident)
                parseError(variant.line, "expected variant name after '::'");
            lex_.next(); // consume variant name
            auto node = std::make_unique<ExprNode>();
            node->data = EnumAccessExpr{t.value, variant.value};
            return node;
        }
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
        if (lex_.peek().kind == TokenKind::RBrace) {
            // Empty {} — could be empty set or map depending on context
            // Parser cannot decide, so return empty SetExpr (codegen resolves via type annotation)
            lex_.next(); // consume '}'
            auto set = std::make_unique<SetExpr>();
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(set);
            return node;
        }
        // Parse first expression
        ExprPtr first = parseLogicalOr();
        if (lex_.peek().kind == TokenKind::Colon) {
            // Map literal: first expression was the key
            lex_.next(); // consume ':'
            auto map = std::make_unique<MapExpr>();
            ExprPtr val = parseLogicalOr();
            map->keys.push_back(std::move(first));
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
        } else {
            // Set literal
            auto set = std::make_unique<SetExpr>();
            set->elements.push_back(std::move(first));
            while (lex_.peek().kind == TokenKind::Comma) {
                lex_.next(); // consume ','
                set->elements.push_back(parseLogicalOr());
            }
            if (lex_.peek().kind != TokenKind::RBrace)
                parseError("expected '}'");
            lex_.next(); // consume '}'
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(set);
            return node;
        }
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
        // Try lambda: save state and attempt lambda parse
        auto saved = lex_.saveState();
        try {
            return parseLambdaExpr();
        } catch (...) {
            lex_.restoreState(std::move(saved));
        }

        // Not a lambda — parse as grouping or tuple
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
            case TokenKind::GreaterGreaterGreater:
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

    // Parse function body, possibly with require/ensure clauses
    if (lex_.peek().kind != TokenKind::Newline)
        parseError("expected newline after ':'");
    lex_.next(); // consume Newline
    skipNewlines();

    if (lex_.peek().kind != TokenKind::Indent)
        parseError("expected indented block");
    lex_.next(); // consume Indent

    // Parse optional require clause
    if (lex_.peek().kind == TokenKind::Require) {
        lex_.next(); // consume 'require'
        parseContractClause("require", fnStmt->preconditions);
    }

    // Parse optional ensure clause
    if (lex_.peek().kind == TokenKind::Ensure) {
        lex_.next(); // consume 'ensure'
        parseContractClause("ensure", fnStmt->postconditions);
    }

    // Parse body statements
    while (lex_.peek().kind != TokenKind::Dedent &&
           lex_.peek().kind != TokenKind::Eof) {
        fnStmt->body.push_back(parseStatement());
        if (lex_.peek().kind == TokenKind::Newline)
            lex_.next();
        skipNewlines();
    }

    if (fnStmt->body.empty())
        parseError("empty function body is not allowed");

    if (lex_.peek().kind == TokenKind::Dedent)
        lex_.next(); // consume Dedent

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

void Parser::parseContractClause(const std::string &clauseName, std::vector<ExprPtr> &out) {
    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after '" + clauseName + "'");
    lex_.next(); // consume ':'
    if (lex_.peek().kind != TokenKind::Newline)
        parseError("expected newline after '" + clauseName + ":'");
    lex_.next(); // consume Newline
    skipNewlines();
    if (lex_.peek().kind != TokenKind::Indent)
        parseError("expected indented block after '" + clauseName + ":'");
    lex_.next(); // consume Indent
    while (lex_.peek().kind != TokenKind::Dedent &&
           lex_.peek().kind != TokenKind::Eof) {
        out.push_back(parseLogicalOr());
        if (lex_.peek().kind == TokenKind::Newline)
            lex_.next();
        skipNewlines();
    }
    if (lex_.peek().kind == TokenKind::Dedent)
        lex_.next(); // consume Dedent
    if (lex_.peek().kind == TokenKind::Newline)
        lex_.next();
    skipNewlines();
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
           lex_.peek().kind != TokenKind::Eof &&
           lex_.peek().kind != TokenKind::Invariant) {
        auto fieldDirectives = parseDirectives();

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
        ts.fields.push_back({fieldName.value, fieldType, std::move(fieldDirectives)});
        seenFields.insert(fieldName.value);

        if (lex_.peek().kind == TokenKind::Newline)
            lex_.next();
        skipNewlines();
    }

    if (ts.fields.empty())
        parseError("type definition must have at least one field");

    // Parse optional invariant clause
    if (lex_.peek().kind == TokenKind::Invariant) {
        lex_.next(); // consume 'invariant'
        parseContractClause("invariant", ts.invariants);
    }

    if (lex_.peek().kind == TokenKind::Dedent)
        lex_.next(); // consume Dedent

    return ts;
}

StmtNode Parser::parseEnumStatement() {
    lex_.next(); // consume 'enum'

    Token nameTok = lex_.peek();
    if (nameTok.kind != TokenKind::Ident)
        parseError(nameTok.line, "expected enum name after 'enum'");
    lex_.next(); // consume name

    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after enum name");
    lex_.next(); // consume ':'

    if (lex_.peek().kind != TokenKind::Newline)
        parseError("expected newline after ':'");
    lex_.next(); // consume Newline
    skipNewlines();

    if (lex_.peek().kind != TokenKind::Indent)
        parseError("expected indented block");
    lex_.next(); // consume Indent

    EnumStmt es;
    es.name = nameTok.value;
    std::unordered_set<std::string> seenVariants;

    while (lex_.peek().kind != TokenKind::Dedent &&
           lex_.peek().kind != TokenKind::Eof) {
        Token variantName = lex_.peek();
        if (variantName.kind != TokenKind::Ident)
            parseError(variantName.line, "expected variant name");
        lex_.next(); // consume variant name

        if (seenVariants.count(variantName.value))
            parseError(variantName.line, "duplicate variant name '" + variantName.value + "'");
        es.variants.push_back(variantName.value);
        seenVariants.insert(variantName.value);

        if (lex_.peek().kind == TokenKind::Newline)
            lex_.next();
        skipNewlines();
    }

    if (es.variants.empty())
        parseError("enum definition must have at least one variant");

    if (lex_.peek().kind == TokenKind::Dedent)
        lex_.next(); // consume Dedent

    return es;
}

std::string Parser::parseTypeName() {
    std::string name = parseTypeNameSingle();
    while (lex_.peek().kind == TokenKind::Pipe) {
        lex_.next(); // consume '|'
        name += " | " + parseTypeNameSingle();
    }
    return name;
}

std::string Parser::parseTypeNameSingle() {
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

    // fn(int, int) -> int  function type
    if (lex_.peek().kind == TokenKind::Fn) {
        lex_.next(); // consume 'fn'
        std::string name = "fn";
        if (lex_.peek().kind != TokenKind::LParen)
            parseError("expected '(' after 'fn' in function type");
        lex_.next(); // consume '('
        name += "(";
        if (lex_.peek().kind != TokenKind::RParen) {
            name += parseTypeName();
            while (lex_.peek().kind == TokenKind::Comma) {
                lex_.next(); // consume ','
                name += ", " + parseTypeName();
            }
        }
        if (lex_.peek().kind != TokenKind::RParen)
            parseError("expected ')' in function type");
        lex_.next(); // consume ')'
        name += ")";
        if (lex_.peek().kind == TokenKind::Arrow) {
            lex_.next(); // consume '->'
            name += " -> " + parseTypeName();
        }
        return name;
    }

    Token t = lex_.peek();
    if (t.kind != TokenKind::Ident)
        parseError(t.line, "expected type name");
    std::string name = t.value;
    lex_.next(); // consume type name

    // fn(int, int) -> int  function type (when fn comes as an ident, shouldn't happen normally)
    if (name == "fn" && lex_.peek().kind == TokenKind::LParen) {
        lex_.next(); // consume '('
        name += "(";
        if (lex_.peek().kind != TokenKind::RParen) {
            name += parseTypeName();
            while (lex_.peek().kind == TokenKind::Comma) {
                lex_.next(); // consume ','
                name += ", " + parseTypeName();
            }
        }
        if (lex_.peek().kind != TokenKind::RParen)
            parseError("expected ')' in function type");
        lex_.next(); // consume ')'
        name += ")";
        if (lex_.peek().kind == TokenKind::Arrow) {
            lex_.next(); // consume '->'
            name += " -> " + parseTypeName();
        }
        return name;
    }

    if (lex_.peek().kind == TokenKind::Less) {
        lex_.next(); // consume '<'
        std::string inner = parseTypeName();
        if ((name == "Map" || name == "Result") && lex_.peek().kind == TokenKind::Comma) {
            // Map<K, V> parsing
            lex_.next(); // consume ','
            std::string valueTy = parseTypeName();
            if (lex_.peek().kind != TokenKind::Greater)
                parseError("expected '>' in " + name + " type");
            lex_.next(); // consume '>'
            name += "<" + inner + ", " + valueTy + ">";
        } else {
            if (lex_.peek().kind != TokenKind::Greater)
                parseError("expected '>' after generic type parameter");
            lex_.next(); // consume '>'
            name += "<" + inner + ">";
        }
    }

    return name;
}

ExprPtr Parser::parseLambdaExpr() {
    // (params): return_type => expr_or_block
    if (lex_.peek().kind != TokenKind::LParen)
        throw std::runtime_error("not a lambda");
    lex_.next(); // consume '('

    auto lambda = std::make_unique<LambdaExpr>();

    // Parse parameters
    if (lex_.peek().kind != TokenKind::RParen) {
        for (;;) {
            Token paramName = lex_.peek();
            if (paramName.kind != TokenKind::Ident)
                throw std::runtime_error("not a lambda");
            lex_.next(); // consume param name

            if (lex_.peek().kind != TokenKind::Colon)
                throw std::runtime_error("not a lambda");
            lex_.next(); // consume ':'

            std::string paramType = parseTypeName();
            lambda->params.push_back({paramName.value, paramType});

            if (lex_.peek().kind != TokenKind::Comma)
                break;
            lex_.next(); // consume ','
        }
    }

    if (lex_.peek().kind != TokenKind::RParen)
        throw std::runtime_error("not a lambda");
    lex_.next(); // consume ')'

    // -> で直接ボディへ（戻り値型は推論）
    if (lex_.peek().kind != TokenKind::Arrow)
        throw std::runtime_error("not a lambda");
    lex_.next(); // consume '->'
    lambda->return_type = "";  // 推論に委ねる

    // Check for multi-line lambda (newline + indent)
    if (lex_.peek().kind == TokenKind::Newline) {
        // Peek ahead to see if indent follows
        auto saved2 = lex_.saveState();
        lex_.next(); // consume newline
        skipNewlines();
        if (lex_.peek().kind == TokenKind::Indent) {
            lex_.restoreState(std::move(saved2));
            // parseBlock expects newline then indent
            // We need to provide the colon context; use parseBlock directly
            // Actually parseBlock expects newline, then skips newlines, then indent
            // We're at Newline now, so we can call parseBlock-like logic
            lex_.next(); // consume newline
            skipNewlines();
            lex_.next(); // consume Indent

            while (lex_.peek().kind != TokenKind::Dedent &&
                   lex_.peek().kind != TokenKind::Eof) {
                lambda->body.push_back(parseStatement());
                if (lex_.peek().kind == TokenKind::Newline)
                    lex_.next();
                skipNewlines();
            }
            if (lambda->body.empty())
                parseError("empty lambda body is not allowed");
            if (lex_.peek().kind == TokenKind::Dedent)
                lex_.next(); // consume Dedent
        } else {
            lex_.restoreState(std::move(saved2));
            lambda->expr_body = parseLogicalOr();
        }
    } else {
        // Single expression lambda
        lambda->expr_body = parseLogicalOr();
    }

    auto node = std::make_unique<ExprNode>();
    node->data = std::move(lambda);
    return node;
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

StmtNode Parser::parseDescribeStatement() {
    lex_.next(); // consume 'describe'

    Token descTok = lex_.peek();
    if (descTok.kind != TokenKind::String)
        parseError(descTok.line, "expected string after 'describe'");
    lex_.next(); // consume string

    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after describe description");
    lex_.next(); // consume ':'

    if (lex_.peek().kind != TokenKind::Newline)
        parseError("expected newline after ':'");
    lex_.next(); // consume Newline
    skipNewlines();

    if (lex_.peek().kind != TokenKind::Indent)
        parseError("expected indented block");
    lex_.next(); // consume Indent

    auto desc = std::make_unique<DescribeStmt>();
    desc->description = descTok.value;

    while (lex_.peek().kind != TokenKind::Dedent &&
           lex_.peek().kind != TokenKind::Eof) {
        if (lex_.peek().kind != TokenKind::It)
            parseError(lex_.peek().line, "expected 'it' inside describe block");
        lex_.next(); // consume 'it'

        Token itDescTok = lex_.peek();
        if (itDescTok.kind != TokenKind::String)
            parseError(itDescTok.line, "expected string after 'it'");
        lex_.next(); // consume string

        if (lex_.peek().kind != TokenKind::Colon)
            parseError("expected ':' after it description");
        lex_.next(); // consume ':'

        ItBlock itBlock;
        itBlock.description = itDescTok.value;
        itBlock.body = parseBlock();

        desc->cases.push_back(std::move(itBlock));

        skipNewlines();
    }

    if (desc->cases.empty())
        parseError("describe block must have at least one 'it' case");

    if (lex_.peek().kind == TokenKind::Dedent)
        lex_.next(); // consume Dedent

    return desc;
}

StmtNode Parser::parseExpectStatement() {
    Token expectTok = lex_.next(); // consume 'expect'

    if (lex_.peek().kind != TokenKind::LParen)
        parseError("expected '(' after 'expect'");
    lex_.next(); // consume '('

    ExprPtr actual = parseLogicalOr();

    if (lex_.peek().kind != TokenKind::RParen)
        parseError("expected ')'");
    lex_.next(); // consume ')'

    if (lex_.peek().kind != TokenKind::Dot)
        parseError("expected '.' after expect(...)");
    lex_.next(); // consume '.'

    Token matcherTok = lex_.peek();
    if (matcherTok.kind != TokenKind::Ident)
        parseError(matcherTok.line, "expected matcher name after '.'");
    lex_.next(); // consume matcher name

    std::string matcher = matcherTok.value;

    ExpectStmt es;
    es.actual = std::move(actual);
    es.matcher = matcher;
    es.line = expectTok.line;

    if (lex_.peek().kind != TokenKind::LParen)
        parseError("expected '(' after matcher name");
    lex_.next(); // consume '('

    if (matcher == "to_eq") {
        es.expected = parseLogicalOr();
    } else if (matcher == "to_be_true" || matcher == "to_be_false" || matcher == "to_be_none") {
        // no argument
    } else {
        parseError(matcherTok.line, "unknown matcher '" + matcher + "'");
    }

    if (lex_.peek().kind != TokenKind::RParen)
        parseError("expected ')'");
    lex_.next(); // consume ')'

    return es;
}

Pattern Parser::parsePattern() {
    Token t = lex_.peek();

    // Wildcard: _
    if (t.kind == TokenKind::Ident && t.value == "_") {
        lex_.next();
        return WildcardPattern{};
    }

    // None pattern
    if (t.kind == TokenKind::Ident && t.value == "None") {
        lex_.next();
        return NonePattern{};
    }

    // Ok(binding) pattern
    if (t.kind == TokenKind::Ok) {
        lex_.next();
        if (lex_.peek().kind != TokenKind::LParen)
            parseError("expected '(' after 'Ok'");
        lex_.next();
        Token binding = lex_.peek();
        if (binding.kind != TokenKind::Ident)
            parseError(binding.line, "expected variable name in Ok pattern");
        lex_.next();
        if (lex_.peek().kind != TokenKind::RParen)
            parseError("expected ')' after Ok binding");
        lex_.next();
        return OkPattern{binding.value};
    }

    // Err(binding) pattern
    if (t.kind == TokenKind::Err) {
        lex_.next();
        if (lex_.peek().kind != TokenKind::LParen)
            parseError("expected '(' after 'Err'");
        lex_.next();
        Token binding = lex_.peek();
        if (binding.kind != TokenKind::Ident)
            parseError(binding.line, "expected variable name in Err pattern");
        lex_.next();
        if (lex_.peek().kind != TokenKind::RParen)
            parseError("expected ')' after Err binding");
        lex_.next();
        return ErrPattern{binding.value};
    }

    // Some(binding) pattern
    if (t.kind == TokenKind::Ident && t.value == "Some") {
        lex_.next(); // consume 'Some'
        if (lex_.peek().kind != TokenKind::LParen)
            parseError("expected '(' after 'Some'");
        lex_.next(); // consume '('
        Token binding = lex_.peek();
        if (binding.kind != TokenKind::Ident)
            parseError(binding.line, "expected variable name in Some pattern");
        lex_.next(); // consume binding name
        if (lex_.peek().kind != TokenKind::RParen)
            parseError("expected ')' after Some binding");
        lex_.next(); // consume ')'
        return SomePattern{binding.value};
    }

    // Literal patterns: number, float, string, true, false
    if (t.kind == TokenKind::Number) {
        lex_.next();
        auto node = std::make_unique<ExprNode>();
        node->data = NumberExpr{parseIntLiteral(t.value)};
        return LiteralPattern{std::move(node)};
    }
    if (t.kind == TokenKind::Float) {
        lex_.next();
        auto node = std::make_unique<ExprNode>();
        node->data = FloatExpr{std::stod(t.value)};
        return LiteralPattern{std::move(node)};
    }
    if (t.kind == TokenKind::String) {
        lex_.next();
        auto node = std::make_unique<ExprNode>();
        node->data = StringExpr{t.value};
        return LiteralPattern{std::move(node)};
    }
    if (t.kind == TokenKind::True || t.kind == TokenKind::False) {
        lex_.next();
        auto node = std::make_unique<ExprNode>();
        node->data = BoolExpr{t.kind == TokenKind::True};
        return LiteralPattern{std::move(node)};
    }

    // Negative number literal: -N
    if (t.kind == TokenKind::Minus) {
        lex_.next(); // consume '-'
        Token num = lex_.peek();
        if (num.kind == TokenKind::Number) {
            lex_.next();
            auto node = std::make_unique<ExprNode>();
            node->data = NumberExpr{-parseIntLiteral(num.value)};
            return LiteralPattern{std::move(node)};
        }
        if (num.kind == TokenKind::Float) {
            lex_.next();
            auto node = std::make_unique<ExprNode>();
            node->data = FloatExpr{-std::stod(num.value)};
            return LiteralPattern{std::move(node)};
        }
        parseError(num.line, "expected number after '-' in pattern");
    }

    // Identifier: could be Enum::Variant or variable binding
    if (t.kind == TokenKind::Ident) {
        lex_.next(); // consume ident
        if (lex_.peek().kind == TokenKind::ColonColon) {
            lex_.next(); // consume '::'
            Token variant = lex_.peek();
            if (variant.kind != TokenKind::Ident)
                parseError(variant.line, "expected variant name after '::'");
            lex_.next(); // consume variant name
            return EnumPattern{t.value, variant.value};
        }
        return VariablePattern{t.value};
    }

    parseError(t.line, "expected pattern");
}

StmtNode Parser::parseMatchStatement() {
    lex_.next(); // consume 'match'
    ExprPtr subject = parseLogicalOr();

    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after match subject");
    lex_.next(); // consume ':'

    if (lex_.peek().kind != TokenKind::Newline)
        parseError("expected newline after ':'");
    lex_.next(); // consume Newline
    skipNewlines();

    if (lex_.peek().kind != TokenKind::Indent)
        parseError("expected indented block");
    lex_.next(); // consume Indent

    auto matchStmt = std::make_unique<MatchStmt>();
    matchStmt->subject = std::move(subject);

    while (lex_.peek().kind != TokenKind::Dedent &&
           lex_.peek().kind != TokenKind::Eof) {
        if (lex_.peek().kind != TokenKind::Case)
            parseError(lex_.peek().line, "expected 'case' in match block");
        lex_.next(); // consume 'case'

        MatchArm arm;
        arm.pattern = parsePattern();

        // Optional guard: if <expr>
        if (lex_.peek().kind == TokenKind::If) {
            lex_.next(); // consume 'if'
            arm.guard = parseLogicalOr();
        }

        if (lex_.peek().kind != TokenKind::Colon)
            parseError("expected ':' after case pattern");
        lex_.next(); // consume ':'

        arm.body = parseBlock();
        matchStmt->arms.push_back(std::move(arm));

        skipNewlines();
    }

    if (matchStmt->arms.empty())
        parseError("match block must have at least one case");

    if (lex_.peek().kind == TokenKind::Dedent)
        lex_.next(); // consume Dedent

    return matchStmt;
}
