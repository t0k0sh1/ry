#include "ry/parser.hpp"
#include "ry/diagnostic.hpp"
#include <regex>
#include <stdexcept>
#include <string>
#include <unordered_set>

// ===== Mock/verify helper: coerce first arg identifier to string =====

static void coerceFirstArgToString(std::vector<ExprPtr> &args) {
    if (!args.empty()) {
        if (auto *ve = std::get_if<VariableExpr>(&args[0]->data)) {
            args[0]->data = StringExpr{ve->name};
        }
    }
}

// ===== Naming convention helpers =====

static bool isSnakeCase(const std::string &name) {
    if (name.empty()) return false;
    if (name == "_") return true;
    static const std::regex pattern("[a-z_][a-z0-9_]*");
    return std::regex_match(name, pattern);
}

static bool isScreamingSnakeCase(const std::string &name) {
    if (name.empty()) return false;
    static const std::regex pattern("[A-Z][A-Z0-9_]*");
    return std::regex_match(name, pattern);
}

static bool isPascalCase(const std::string &name) {
    if (name.empty()) return false;
    static const std::regex pattern("[A-Z][a-zA-Z0-9]*");
    return std::regex_match(name, pattern);
}

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
    throw DiagnosticError({line, lex_.peek().col, file_id_}, msg, sm_);
}

[[noreturn]] void Parser::parseError(const std::string &msg) {
    parseError(lex_.peek().line, msg);
}

// ===== Desugar helper: x = x op rhs =====

AssignStmt Parser::makeDesugarAssign(const Token &nameTok, const Token &opTok, const std::string &op, ExprPtr rhs) {
    auto varRef = std::make_unique<ExprNode>();
    varRef->data = VariableExpr{nameTok.value};
    varRef->loc = locFromToken(nameTok);
    auto bin = std::make_unique<BinaryExpr>();
    bin->op = op;
    bin->lhs = std::move(varRef);
    bin->rhs = std::move(rhs);
    auto binNode = std::make_unique<ExprNode>();
    binNode->data = std::move(bin);
    binNode->loc = locFromToken(opTok);
    AssignStmt s;
    s.name = nameTok.value;
    s.value = std::move(binNode);
    s.loc = locFromToken(nameTok);
    return s;
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
        auto opTok = lex_.next();
        ExprPtr rhs = (this->*operand)();
        auto bin = std::make_unique<BinaryExpr>();
        bin->op  = opTok.value;
        bin->lhs = std::move(lhs);
        bin->rhs = std::move(rhs);
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(bin);
        node->loc = locFromToken(opTok);
        lhs = std::move(node);
    }
    return lhs;
}

// ===== A3: parseArgList helper =====

std::vector<ExprPtr> Parser::parseArgList() {
    std::vector<ExprPtr> args;
    if (lex_.peek().kind != TokenKind::RParen) {
        args.push_back(parseTernary());
        while (lex_.peek().kind == TokenKind::Comma) {
            lex_.next();
            args.push_back(parseTernary());
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

void Parser::skipStructuralTokens() {
    while (lex_.peek().kind == TokenKind::Newline ||
           lex_.peek().kind == TokenKind::Indent ||
           lex_.peek().kind == TokenKind::Dedent)
        lex_.next();
}

// ===== Directive parsing =====

static const std::unordered_set<std::string> known_directives = {"deprecated", "native", "parallel", "each", "property", "const"};

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
        d.loc = {atTok.line, atTok.col, file_id_};

        // Optional parameters: @name(key=value, ...) or @each([ ... ])
        if (lex_.peek().kind == TokenKind::LParen) {
            lex_.next(); // consume '('

            if (nameTok.value == "each") {
                // @each([ ... ]) — parse list expression
                if (lex_.peek().kind != TokenKind::LBracket)
                    parseError("expected '[' after '@each('");
                d.expr = parsePrimary();
                if (lex_.peek().kind != TokenKind::RParen)
                    parseError("expected ')' after @each list");
                lex_.next(); // consume ')'
            } else {
                while (lex_.peek().kind != TokenKind::RParen) {
                    Token keyTok2 = lex_.peek();
                    if (keyTok2.kind != TokenKind::Ident)
                        parseError(keyTok2.line, "expected parameter name in directive");
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

                    d.params.push_back({keyTok2.value, valTok.value});

                    if (lex_.peek().kind == TokenKind::Comma)
                        lex_.next(); // consume ','
                }
                if (lex_.peek().kind != TokenKind::RParen)
                    parseError("expected ')' after directive parameters");
                lex_.next(); // consume ')'
            }
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
        parseError(modTok.line, "expected package name after 'from'");
    std::string modulePath = lex_.next().value;

    while (lex_.peek().kind == TokenKind::Dot) {
        lex_.next(); // consume '.'
        Token part = lex_.peek();
        if (part.kind != TokenKind::Ident)
            parseError(part.line, "expected identifier after '.'");
        modulePath += "/" + lex_.next().value;
    }
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

    return ImportStmt{modulePath, names, {fromTok.line, fromTok.col, file_id_}};
}

StmtNode Parser::parseStatement() {
    // Parse directives before the statement
    auto directives = parseDirectives();

    Token first = lex_.peek();

    if (first.kind == TokenKind::From)
        parseError(first.line, "'from' import is only allowed at top level");

    // Directive-accepting statements (see branches below)
    if (first.kind == TokenKind::Record) {
        auto stmt = parseRecordStatement();
        auto &ts = std::get<RecordStmt>(stmt);
        ts.directives = std::move(directives);
        return stmt;
    }

    if (first.kind == TokenKind::Type) {
        if (!directives.empty())
            parseError(first.line, "directives are not supported on type alias");
        return parseTypeAliasStatement();
    }

    if (first.kind == TokenKind::Fn) {
        auto stmt = parseFnStatement(directives);
        auto &fs = std::get<std::unique_ptr<FnStmt>>(stmt);
        fs->directives = std::move(directives);
        return stmt;
    }

    if (first.kind == TokenKind::Async) {
        lex_.next(); // consume 'async'
        if (lex_.peek().kind != TokenKind::Fn)
            parseError(first.line, "expected 'fn' after 'async'");
        auto stmt = parseFnStatement(directives, true);
        auto &fs = std::get<std::unique_ptr<FnStmt>>(stmt);
        fs->directives = std::move(directives);
        return stmt;
    }

    if (first.kind == TokenKind::For) {
        if (!directives.empty() && !hasDirective(directives, "parallel"))
            parseError(first.line, "only @parallel is supported on for statements");
        if (directives.size() > 1)
            parseError(first.line, "for statements support only a single @parallel directive");
        auto stmt = parseForStatement();
        auto &fs = std::get<std::unique_ptr<ForStmt>>(stmt);
        fs->directives = std::move(directives);
        return stmt;
    }

    // @each / @property are only allowed on `it` calls
    if (!directives.empty()) {
        bool hasTestDirective = hasDirective(directives, "each") || hasDirective(directives, "property");
        if (hasTestDirective) {
            if (first.kind != TokenKind::Ident || first.value != "it")
                parseError(first.line, "@each / @property can only be applied to 'it' calls");
            lex_.next(); // consume 'it'
            if (lex_.peek().kind != TokenKind::LParen)
                parseError("expected '(' after 'it'");
            lex_.next(); // consume '('
            CallStmt s;
            s.callee = "it";
            s.args = parseArgList();
            s.loc = locFromToken(first);
            s.directives = std::move(directives);
            return s;
        }
    }

    // Non-identifier statements (except those already handled above) do not accept directives
    if (!directives.empty() && first.kind != TokenKind::Ident)
        parseError(first.line, "directives are not supported on this statement");

    if (first.kind == TokenKind::Expect)
        return parseExpectStatement();

    if (first.kind == TokenKind::Enum)
        return parseEnumStatement();

    if (first.kind == TokenKind::Return)
        return parseReturnStatement();

    if (first.kind == TokenKind::Match)
        return parseMatchStatement();

    if (first.kind == TokenKind::Select)
        return parseSelectStatement();

    if (first.kind == TokenKind::If)
        return parseIfStatement();

    if (first.kind == TokenKind::While)
        return parseWhileStatement();

    if (first.kind == TokenKind::For)
        return parseForStatement();

    if (first.kind == TokenKind::Break) {
        lex_.next();
        return BreakStmt{locFromToken(first)};
    }

    if (first.kind == TokenKind::Continue) {
        lex_.next();
        return ContinueStmt{locFromToken(first)};
    }

    if (first.kind == TokenKind::Ellipsis) {
        lex_.next();
        return EllipsisStmt{locFromToken(first)};
    }

    if (first.kind == TokenKind::Await) {
        Token awaitTok = lex_.next(); // consume 'await'
        AwaitStmt s;
        s.operand = parseLogicalNot();
        s.loc = locFromToken(awaitTok);
        return s;
    }

    // identifier-leading statements: assignment, index assignment, or function call
    if (first.kind != TokenKind::Ident)
        parseError(first.line, "expected 'if', 'while', 'for', 'fn', 'async fn', 'return', 'break', 'continue', 'await', '...', 'enum', 'match', 'select', 'expect', 'record', 'type', or identifier, got '" + first.value + "'");
    lex_.next(); // consume ident

    Token next = lex_.peek();
    if (next.kind == TokenKind::Colon) {
        // Type-annotated declaration: x: int = 42 or @native @const PI: float
        lex_.next(); // consume ':'
        std::string typeAnnotation = parseTypeName();
        AssignStmt s;
        s.name = first.value;
        s.type_annotation = typeAnnotation;
        s.directives = std::move(directives);
        s.loc = locFromToken(first);
        if (lex_.peek().kind == TokenKind::Equals) {
            lex_.next(); // consume '='
            s.value = parseTernary();
        } else {
            // Value-less declaration (e.g., @native @const PI: float)
            if (!hasDirective(s.directives, "native"))
                parseError("expected '=' after type annotation");
        }
        return s;
    } else if (next.kind == TokenKind::LBracket) {
        if (!directives.empty())
            parseError(first.line, "directives are not supported on index assignment");
        // index assignment: ident[expr] = value
        lex_.next(); // consume '['
        ExprPtr index = parseTernary();
        if (lex_.peek().kind != TokenKind::RBracket)
            parseError("expected ']'");
        lex_.next(); // consume ']'
        if (lex_.peek().kind != TokenKind::Equals)
            parseError("expected '=' after index expression");
        lex_.next(); // consume '='
        ExprPtr val = parseTernary();
        IndexAssignStmt s;
        auto obj = std::make_unique<ExprNode>();
        obj->data = VariableExpr{first.value};
        obj->loc = locFromToken(first);
        s.object = std::move(obj);
        s.index = std::move(index);
        s.value = std::move(val);
        s.loc = locFromToken(first);
        return s;
    } else if (next.kind == TokenKind::Dot) {
        if (!directives.empty())
            parseError(first.line, "directives are not supported on field assignment or method call");
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
            s.loc = locFromToken(first);
            auto obj = std::make_unique<ExprNode>();
            obj->data = VariableExpr{first.value};
            obj->loc = locFromToken(first);
            s.args.push_back(std::move(obj));
            auto rest = parseArgList();
            for (auto &arg : rest)
                s.args.push_back(std::move(arg));
            tryParseTrailingBlock(s);
            return s;
        }

        if (lex_.peek().kind != TokenKind::Equals)
            parseError("expected '=' after field name");
        lex_.next(); // consume '='
        ExprPtr val = parseTernary();
        FieldAssignStmt s;
        auto obj = std::make_unique<ExprNode>();
        obj->data = VariableExpr{first.value};
        obj->loc = locFromToken(first);
        s.object = std::move(obj);
        s.field = fieldTok.value;
        s.value = std::move(val);
        s.loc = locFromToken(first);
        return s;
    } else if (next.kind == TokenKind::Comma) {
        // Tuple destructuring: a, b = (10, 20)
        std::vector<std::string> names;
        names.push_back(first.value);
        while (lex_.peek().kind == TokenKind::Comma) {
            lex_.next(); // consume ','
            Token n = lex_.peek();
            if (n.kind != TokenKind::Ident && n.value != "_")
                parseError("expected identifier or '_' in tuple destructuring");
            lex_.next(); // consume ident
            names.push_back(n.value);
        }
        if (lex_.peek().kind != TokenKind::Equals)
            parseError("expected '=' in tuple destructuring");
        lex_.next(); // consume '='
        ExprPtr value = parseTernary();
        TupleDestructStmt s;
        s.names = std::move(names);
        s.value = std::move(value);
        s.is_immutable = hasDirective(directives, "const");
        s.directives = std::move(directives);
        s.loc = locFromToken(first);
        return s;
    } else if (next.kind == TokenKind::Equals) {
        lex_.next(); // consume '='
        AssignStmt s;
        s.name  = first.value;
        s.value = parseTernary();
        s.directives = std::move(directives);
        s.loc = locFromToken(first);
        return s;
    } else if (next.kind == TokenKind::PlusEq  || next.kind == TokenKind::MinusEq ||
               next.kind == TokenKind::StarEq  || next.kind == TokenKind::SlashEq ||
               next.kind == TokenKind::PercentEq ||
               next.kind == TokenKind::SlashSlashEq || next.kind == TokenKind::StarStarEq ||
               next.kind == TokenKind::AmpEq  || next.kind == TokenKind::PipeEq ||
               next.kind == TokenKind::CaretEq ||
               next.kind == TokenKind::LessLessEq || next.kind == TokenKind::GreaterGreaterEq) {
        if (!directives.empty())
            parseError(first.line, "directives are not supported on compound assignment");
        // Compound assignment: desugar x += e → x = x + e
        Token opTok = lex_.next(); // consume +=, -=, //=, **=, etc.
        std::string op = opTok.value.substr(0, opTok.value.size() - 1); // extract "//" from "//="
        return makeDesugarAssign(first, opTok, op, parseTernary());
    } else if (next.kind == TokenKind::PlusPlus || next.kind == TokenKind::MinusMinus) {
        Token opTok = lex_.next(); // consume ++ or --
        std::string op = (opTok.kind == TokenKind::PlusPlus) ? "+" : "-";
        auto one = std::make_unique<ExprNode>();
        one->data = NumberExpr{1};
        one->loc = locFromToken(first);
        return makeDesugarAssign(first, opTok, op, std::move(one));
    } else if (next.kind == TokenKind::LParen) {
        if (!directives.empty())
            parseError(first.line, "directives are not supported on function calls");
        lex_.next(); // consume '('
        CallStmt s;
        s.callee = first.value;
        s.args = parseArgList();
        s.loc = locFromToken(first);
        if (s.callee == "mock")
            coerceFirstArgToString(s.args);
        if (s.callee != "mock" && s.callee != "it" && s.callee != "describe") {
            tryParseTrailingBlock(s);
        }
        return s;
    }
    parseError(next.line, "expected '=', '+=', '-=', '*=', '/=', '%=', '//=', '**=', '&=', '|=', '^=', '<<=', '>>=', '++', '--', '.', '[', or '(' after identifier");
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
    Token whileTok = lex_.next(); // consume 'while'
    ExprPtr cond = parseTernary();

    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after while condition");
    lex_.next(); // consume ':'

    auto whileStmt = std::make_unique<WhileStmt>();
    whileStmt->condition = std::move(cond);
    whileStmt->body = parseBlock();
    whileStmt->loc = locFromToken(whileTok);
    return whileStmt;
}

StmtNode Parser::parseForStatement() {
    Token forTok = lex_.next(); // consume 'for'

    Token varTok = lex_.peek();
    if (varTok.kind != TokenKind::Ident)
        parseError(varTok.line, "expected variable name after 'for'");
    if (!isSnakeCase(varTok.value))
        parseError(varTok.line, "loop variable name '" + varTok.value + "' must be snake_case");
    lex_.next(); // consume var name

    std::optional<std::string> var2;
    if (lex_.peek().kind == TokenKind::Comma) {
        lex_.next(); // consume ','
        Token var2Tok = lex_.peek();
        if (var2Tok.kind != TokenKind::Ident)
            parseError(var2Tok.line, "expected second variable name after ',' in for loop");
        if (!isSnakeCase(var2Tok.value))
            parseError(var2Tok.line, "loop variable name '" + var2Tok.value + "' must be snake_case");
        lex_.next(); // consume var2
        var2 = var2Tok.value;
    }

    if (lex_.peek().kind != TokenKind::In)
        parseError("expected 'in' after variable name in for loop");
    lex_.next(); // consume 'in'

    ExprPtr iterable = parseTernary();

    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after for loop iterable");
    lex_.next(); // consume ':'

    auto forStmt = std::make_unique<ForStmt>();
    forStmt->var_name = varTok.value;
    forStmt->var_name2 = var2;
    forStmt->iterable = std::move(iterable);
    forStmt->body = parseBlock();
    forStmt->loc = locFromToken(forTok);
    return forStmt;
}

StmtNode Parser::parseIfStatement() {
    auto ifStmt = std::make_unique<IfStmt>();

    Token ifTok = lex_.next(); // consume 'if'
    ifStmt->loc = locFromToken(ifTok);
    ExprPtr cond = parseTernary();

    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after if condition");
    lex_.next(); // consume ':'

    IfBranch branch;
    branch.condition = std::move(cond);
    branch.body = parseBlock();
    ifStmt->branches.push_back(std::move(branch));

    while (lex_.peek().kind == TokenKind::Elif) {
        lex_.next(); // consume 'elif'
        ExprPtr elifCond = parseTernary();

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

ExprPtr Parser::parseTernary() {
    ExprPtr expr = parseNullCoalesce();
    if (lex_.peek().kind == TokenKind::Question) {
        Token qTok = lex_.next(); // consume '?'
        ExprPtr trueExpr = parseTernary(); // right-associative
        if (lex_.peek().kind != TokenKind::Colon)
            parseError("expected ':' in ternary expression");
        lex_.next(); // consume ':'
        ExprPtr falseExpr = parseTernary(); // right-associative
        auto ternary = std::make_unique<TernaryExpr>();
        ternary->condition = std::move(expr);
        ternary->true_expr = std::move(trueExpr);
        ternary->false_expr = std::move(falseExpr);
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(ternary);
        node->loc = locFromToken(qTok);
        return node;
    }
    return expr;
}

ExprPtr Parser::parseNullCoalesce() {
    ExprPtr lhs = parseLogicalOr();
    while (lex_.peek().kind == TokenKind::QuestionQuestion) {
        Token opTok = lex_.next(); // consume '??'
        ExprPtr rhs = parseLogicalOr();
        auto bin = std::make_unique<BinaryExpr>();
        bin->op = "??";
        bin->lhs = std::move(lhs);
        bin->rhs = std::move(rhs);
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(bin);
        node->loc = locFromToken(opTok);
        lhs = std::move(node);
    }
    return lhs;
}
ExprPtr Parser::parseLogicalAnd() { return parseBinaryLeft(&Parser::parseLogicalNot, {TokenKind::And}); }
ExprPtr Parser::parseRange() {
    ExprPtr lhs = parseBitwiseOr();
    if (lex_.peek().kind == TokenKind::DotDot) {
        Token opTok = lex_.next(); // consume '..'
        ExprPtr rhs = parseBitwiseOr();
        auto range = std::make_unique<RangeExpr>();
        range->start = std::move(lhs);
        range->end = std::move(rhs);
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(range);
        node->loc = locFromToken(opTok);
        return node;
    }
    return lhs;
}

ExprPtr Parser::parseComparison() {
    ExprPtr lhs = parseRange();
    for (;;) {
        TokenKind k = lex_.peek().kind;
        // Handle "not in" as a two-token operator
        if (k == TokenKind::Not) {
            auto saved = lex_.saveState();
            Token notTok = lex_.next(); // consume 'not'
            if (lex_.peek().kind == TokenKind::In) {
                lex_.next(); // consume 'in'
                ExprPtr rhs = parseBitwiseOr();
                auto bin = std::make_unique<BinaryExpr>();
                bin->op = "not in";
                bin->lhs = std::move(lhs);
                bin->rhs = std::move(rhs);
                auto node = std::make_unique<ExprNode>();
                node->data = std::move(bin);
                node->loc = locFromToken(notTok);
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
            Token opTok = lex_.next();
            ExprPtr rhs = parseBitwiseOr();
            auto bin = std::make_unique<BinaryExpr>();
            bin->op = opTok.value;
            bin->lhs = std::move(lhs);
            bin->rhs = std::move(rhs);
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(bin);
            node->loc = locFromToken(opTok);
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
ExprPtr Parser::parseTerm()       { return parseBinaryLeft(&Parser::parseCast, {TokenKind::Star, TokenKind::Slash, TokenKind::SlashSlash, TokenKind::Percent}); }

ExprPtr Parser::parsePower() {
    ExprPtr lhs = parsePostfix();
    if (lex_.peek().kind == TokenKind::StarStar) {
        Token opTok = lex_.next();
        ExprPtr rhs = parsePower();  // 右結合: 再帰呼び出し
        auto bin = std::make_unique<BinaryExpr>();
        bin->op  = opTok.value;
        bin->lhs = std::move(lhs);
        bin->rhs = std::move(rhs);
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(bin);
        node->loc = locFromToken(opTok);
        return node;
    }
    return lhs;
}

ExprPtr Parser::parseCast() {
    ExprPtr expr = parsePower();
    while (lex_.peek().kind == TokenKind::As) {
        Token asTok = lex_.next(); // consume 'as'
        Token typeTok = lex_.peek();
        if (typeTok.kind != TokenKind::Ident)
            parseError(typeTok.line, "expected type name after 'as'");
        std::string targetType = lex_.next().value;
        auto cast = std::make_unique<CastExpr>();
        cast->value = std::move(expr);
        cast->target_type = targetType;
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(cast);
        node->loc = locFromToken(asTok);
        expr = std::move(node);
    }
    return expr;
}

ExprPtr Parser::parseLogicalNot() {
    if (lex_.peek().kind == TokenKind::Not) {
        Token notTok = lex_.next(); // consume 'not'
        ExprPtr operand = parseLogicalNot(); // 右結合
        auto unary = std::make_unique<UnaryExpr>();
        unary->op = "not";
        unary->operand = std::move(operand);
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(unary);
        node->loc = locFromToken(notTok);
        return node;
    }
    if (lex_.peek().kind == TokenKind::Spawn)
        return parseSpawnExpr();
    if (lex_.peek().kind == TokenKind::Await)
        return parseAwaitExpr();
    return parseComparison();
}

ExprPtr Parser::parseSpawnExpr() {
    Token spawnTok = lex_.next(); // consume 'spawn'
    ExprPtr operand = parseLogicalNot();
    auto spawnExpr = std::make_unique<SpawnExpr>();
    spawnExpr->operand = std::move(operand);
    auto node = std::make_unique<ExprNode>();
    node->data = std::move(spawnExpr);
    node->loc = locFromToken(spawnTok);
    return node;
}

ExprPtr Parser::parseAwaitExpr() {
    Token awaitTok = lex_.next(); // consume 'await'
    ExprPtr operand = parseLogicalNot();
    auto awaitExpr = std::make_unique<AwaitExpr>();
    awaitExpr->operand = std::move(operand);
    auto node = std::make_unique<ExprNode>();
    node->data = std::move(awaitExpr);
    node->loc = locFromToken(awaitTok);
    return node;
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
        node->loc = locFromToken(t);
        return node;
    }
    if (t.kind == TokenKind::Number) {
        lex_.next();
        auto node = std::make_unique<ExprNode>();
        node->data = NumberExpr{parseIntLiteral(t.value)};
        node->loc = locFromToken(t);
        return node;
    }
    if (t.kind == TokenKind::Float) {
        lex_.next();
        auto node = std::make_unique<ExprNode>();
        node->data = FloatExpr{std::stod(t.value)};
        node->loc = locFromToken(t);
        return node;
    }
    if (t.kind == TokenKind::True || t.kind == TokenKind::False) {
        lex_.next();
        auto node = std::make_unique<ExprNode>();
        node->data = BoolExpr{t.kind == TokenKind::True};
        node->loc = locFromToken(t);
        return node;
    }
    if (t.kind == TokenKind::String) {
        lex_.next();
        auto node = std::make_unique<ExprNode>();
        node->data = StringExpr{t.value};
        node->loc = locFromToken(t);
        return node;
    }
    // f-string: FStringEnd (no interpolation) or FStringStart...FStringEnd
    if (t.kind == TokenKind::FStringEnd) {
        lex_.next();
        auto node = std::make_unique<ExprNode>();
        node->data = StringExpr{t.value};
        node->loc = locFromToken(t);
        return node;
    }
    if (t.kind == TokenKind::FStringStart) {
        lex_.next(); // consume FStringStart
        auto interp = std::make_unique<InterpolatedStringExpr>();
        interp->parts.push_back(t.value);
        interp->exprs.push_back(parseTernary());
        while (lex_.peek().kind == TokenKind::FStringMid) {
            Token mid = lex_.next();
            interp->parts.push_back(mid.value);
            interp->exprs.push_back(parseTernary());
        }
        if (lex_.peek().kind != TokenKind::FStringEnd)
            parseError("expected end of f-string");
        Token end = lex_.next();
        interp->parts.push_back(end.value);
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(interp);
        node->loc = locFromToken(t);
        return node;
    }
    // none keyword → NoneExpr
    if (t.kind == TokenKind::NoneKw) {
        lex_.next();
        auto node = std::make_unique<ExprNode>();
        node->data = NoneExpr{};
        node->loc = locFromToken(t);
        return node;
    }
    // Error(expr) → CallExpr
    if (t.kind == TokenKind::ErrorKw) {
        lex_.next(); // consume 'Error'
        if (lex_.peek().kind != TokenKind::LParen)
            parseError("expected '(' after 'Error'");
        lex_.next(); // consume '('
        auto call = std::make_unique<CallExpr>();
        call->callee = "Error";
        call->args = parseArgList();
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(call);
        node->loc = locFromToken(t);
        return node;
    }
    if (t.kind == TokenKind::Fn) {
        // Lambda expression: fn(params) [-> retType]: body
        lex_.next(); // consume 'fn'
        auto lambdaNode = parseLambdaExpr();
        lambdaNode->loc = locFromToken(t);
        return lambdaNode;
    }
    if (t.kind == TokenKind::Ident) {
        lex_.next();
        if (lex_.peek().kind == TokenKind::LBracket) {
            auto savedState = lex_.saveState();
            try {
                lex_.next(); // consume '['
                std::string typeArg;
                int depth = 1;
                while (depth > 0 && lex_.peek().kind != TokenKind::Eof) {
                    Token tk = lex_.peek();
                    if (tk.kind == TokenKind::LBracket) depth++;
                    else if (tk.kind == TokenKind::RBracket) {
                        depth--;
                        if (depth == 0) {
                            lex_.next(); // consume final ']'
                            break;
                        }
                    }
                    typeArg += tk.value;
                    lex_.next();
                    if (depth > 0 && lex_.peek().kind == TokenKind::Comma) {
                        typeArg += ",";
                        lex_.next();
                    }
                }
                if (lex_.peek().kind == TokenKind::LParen) {
                    lex_.next(); // consume '('
                    auto call = std::make_unique<CallExpr>();
                    call->callee = t.value + "<" + typeArg + ">";
                    call->args = parseArgList();
                    auto node = std::make_unique<ExprNode>();
                    node->data = std::move(call);
                    node->loc = locFromToken(t);
                    return node;
                }
                lex_.restoreState(savedState);
            } catch (...) {
                lex_.restoreState(savedState);
            }
        }
        // Generic enum constructor: MyOption<int>::MySome(42)
        if (lex_.peek().kind == TokenKind::Less) {
            // Try to parse as generic type: Ident<Type>::Variant(...)
            auto savedState = lex_.saveState();
            try {
                lex_.next(); // consume '<'
                std::string typeArgs = "<";
                int depth = 1;
                while (depth > 0 && lex_.peek().kind != TokenKind::Eof) {
                    Token tk = lex_.peek();
                    if (tk.kind == TokenKind::Less) depth++;
                    else if (tk.kind == TokenKind::Greater) {
                        depth--;
                        if (depth == 0) {
                            lex_.next(); // consume final '>'
                            typeArgs += ">";
                            break;
                        }
                    }
                    typeArgs += tk.value;
                    lex_.next();
                    if (depth > 0 && lex_.peek().kind == TokenKind::Comma) {
                        typeArgs += ",";
                        lex_.next();
                    }
                }
                // Expect ::
                if (lex_.peek().kind == TokenKind::ColonColon) {
                    lex_.next(); // consume '::'
                    Token variant = lex_.peek();
                    if (variant.kind != TokenKind::Ident)
                        parseError(variant.line, "expected variant name after '::'");
                    lex_.next();
                    std::string fullEnumName = t.value + typeArgs;
                    if (lex_.peek().kind == TokenKind::LParen) {
                        lex_.next(); // consume '('
                        auto call = std::make_unique<CallExpr>();
                        call->callee = fullEnumName + "::" + variant.value;
                        call->args = parseArgList();
                        auto node = std::make_unique<ExprNode>();
                        node->data = std::move(call);
                        node->loc = locFromToken(t);
                        return node;
                    }
                    auto node = std::make_unique<ExprNode>();
                    node->data = EnumAccessExpr{fullEnumName, variant.value};
                    node->loc = locFromToken(t);
                    return node;
                }
                // Not a generic enum access, restore
                lex_.restoreState(savedState);
            } catch (...) {
                lex_.restoreState(savedState);
            }
        }
        if (lex_.peek().kind == TokenKind::ColonColon) {
            lex_.next(); // consume '::'
            Token variant = lex_.peek();
            if (variant.kind != TokenKind::Ident)
                parseError(variant.line, "expected variant name after '::'");
            lex_.next(); // consume variant name
            // ADT constructor: Enum::Variant(args...)
            if (lex_.peek().kind == TokenKind::LParen) {
                lex_.next(); // consume '('
                auto call = std::make_unique<CallExpr>();
                call->callee = t.value + "::" + variant.value;
                call->args = parseArgList();
                auto node = std::make_unique<ExprNode>();
                node->data = std::move(call);
                node->loc = locFromToken(t);
                return node;
            }
            auto node = std::make_unique<ExprNode>();
            node->data = EnumAccessExpr{t.value, variant.value};
            node->loc = locFromToken(t);
            return node;
        }
        if (lex_.peek().kind == TokenKind::LParen) {
            lex_.next(); // consume '('
            auto call = std::make_unique<CallExpr>();
            call->callee = t.value;
            call->args = parseArgList();
            if (call->callee == "verify")
                coerceFirstArgToString(call->args);
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(call);
            node->loc = locFromToken(t);
            return node;
        }
        auto node = std::make_unique<ExprNode>();
        node->data = VariableExpr{t.value};
        node->loc = locFromToken(t);
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
            node->loc = locFromToken(t);
            return node;
        }
        // Parse first expression
        ExprPtr first = parseTernary();
        if (lex_.peek().kind == TokenKind::Colon) {
            // Map literal: first expression was the key
            lex_.next(); // consume ':'
            auto map = std::make_unique<MapExpr>();
            ExprPtr val = parseTernary();
            map->keys.push_back(std::move(first));
            map->values.push_back(std::move(val));
            while (lex_.peek().kind == TokenKind::Comma) {
                lex_.next(); // consume ','
                ExprPtr k = parseTernary();
                if (lex_.peek().kind != TokenKind::Colon)
                    parseError("expected ':' after map key");
                lex_.next(); // consume ':'
                ExprPtr v = parseTernary();
                map->keys.push_back(std::move(k));
                map->values.push_back(std::move(v));
            }
            if (lex_.peek().kind != TokenKind::RBrace)
                parseError("expected '}'");
            lex_.next(); // consume '}'
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(map);
            node->loc = locFromToken(t);
            return node;
        } else {
            // Set literal
            auto set = std::make_unique<SetExpr>();
            set->elements.push_back(std::move(first));
            while (lex_.peek().kind == TokenKind::Comma) {
                lex_.next(); // consume ','
                set->elements.push_back(parseTernary());
            }
            if (lex_.peek().kind != TokenKind::RBrace)
                parseError("expected '}'");
            lex_.next(); // consume '}'
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(set);
            node->loc = locFromToken(t);
            return node;
        }
    }
    if (t.kind == TokenKind::LBracket) {
        lex_.next(); // consume '['
        auto list = std::make_unique<ListExpr>();
        skipStructuralTokens();
        if (lex_.peek().kind != TokenKind::RBracket) {
            list->elements.push_back(parseTernary());
            skipStructuralTokens();
            while (lex_.peek().kind == TokenKind::Comma) {
                lex_.next(); // consume ','
                skipStructuralTokens();
                list->elements.push_back(parseTernary());
                skipStructuralTokens();
            }
        }
        skipStructuralTokens();
        if (lex_.peek().kind != TokenKind::RBracket)
            parseError("expected ']'");
        lex_.next(); // consume ']'
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(list);
        node->loc = locFromToken(t);
        return node;
    }
    if (t.kind == TokenKind::LParen) {
        // Grouping or tuple (lambda now uses fn keyword)
        lex_.next();
        ExprPtr first = parseTernary();
        if (lex_.peek().kind == TokenKind::Comma) {
            // Tuple literal: (expr, expr, ...)
            auto tuple = std::make_unique<TupleExpr>();
            tuple->elements.push_back(std::move(first));
            while (lex_.peek().kind == TokenKind::Comma) {
                lex_.next(); // consume ','
                tuple->elements.push_back(parseTernary());
            }
            if (lex_.peek().kind != TokenKind::RParen)
                parseError("expected ')'");
            lex_.next();
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(tuple);
            node->loc = locFromToken(t);
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

StmtNode Parser::parseFnStatement(const std::vector<Directive> &directives, bool is_async) {
    Token fnTok = lex_.next(); // consume 'fn'

    auto fnStmt = std::make_unique<FnStmt>();
    fnStmt->loc = locFromToken(fnTok);
    fnStmt->is_async = is_async;

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
        bool validName = isSnakeCase(nameTok.value) ||
                         (hasDirective(directives, "native") && isScreamingSnakeCase(nameTok.value));
        if (!validName)
            parseError(nameTok.line, "function name '" + nameTok.value + "' must be snake_case (or SCREAMING_SNAKE_CASE for @native functions)");
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
            if (!isSnakeCase(paramName.value))
                parseError(paramName.line, "parameter name '" + paramName.value + "' must be snake_case");
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

    // @native fn: body-less declaration
    if (hasDirective(directives, "native")) {
        if (lex_.peek().kind == TokenKind::Colon)
            parseError("@native function must not have a body");
        return StmtNode(std::move(fnStmt));
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

    // Parse optional ensure clause with variable binding
    if (lex_.peek().kind == TokenKind::Ensure) {
        if (fnStmt->return_type == "Unit")
            parseError("'ensure' requires a non-Unit return type");
        lex_.next(); // consume 'ensure'
        parseEnsureClause(*fnStmt);
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
    Token retTok = lex_.next(); // consume 'return'
    ReturnStmt s;
    s.loc = locFromToken(retTok);
    TokenKind next = lex_.peek().kind;
    if (next == TokenKind::Newline || next == TokenKind::Dedent || next == TokenKind::Eof) {
        s.value = nullptr;
    } else {
        s.value = parseTernary();
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
        out.push_back(parseTernary());
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

void Parser::parseEnsureClause(FnStmt &fn) {
    if (lex_.peek().kind != TokenKind::Ident)
        parseError("expected variable name after 'ensure'");
    fn.ensure_bindings.push_back(lex_.next().value);
    while (lex_.peek().kind == TokenKind::Comma) {
        lex_.next(); // consume ','
        if (lex_.peek().kind != TokenKind::Ident)
            parseError("expected variable name in ensure binding");
        fn.ensure_bindings.push_back(lex_.next().value);
    }
    parseContractClause("ensure", fn.postconditions);
}

StmtNode Parser::parseRecordStatement() {
    Token recordTok = lex_.next(); // consume 'record'

    Token nameTok = lex_.peek();
    if (nameTok.kind != TokenKind::Ident)
        parseError(nameTok.line, "expected record name after 'record'");
    if (!isPascalCase(nameTok.value))
        parseError(nameTok.line, "record name '" + nameTok.value + "' must be PascalCase");
    lex_.next(); // consume name

    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after record name");
    lex_.next(); // consume ':'

    if (lex_.peek().kind != TokenKind::Newline)
        parseError("expected newline after ':'");
    lex_.next(); // consume Newline
    skipNewlines();

    if (lex_.peek().kind != TokenKind::Indent)
        parseError("expected indented block");
    lex_.next(); // consume Indent

    RecordStmt ts;
    ts.name = nameTok.value;
    ts.loc = locFromToken(recordTok);
    std::unordered_set<std::string> seenFields;

    while (lex_.peek().kind != TokenKind::Dedent &&
           lex_.peek().kind != TokenKind::Eof &&
           lex_.peek().kind != TokenKind::Invariant) {
        auto fieldDirectives = parseDirectives();

        Token fieldName = lex_.peek();
        if (fieldName.kind != TokenKind::Ident)
            parseError(fieldName.line, "expected field name");
        if (!isSnakeCase(fieldName.value))
            parseError(fieldName.line, "field name '" + fieldName.value + "' must be snake_case");
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
        parseError("record definition must have at least one field");

    // Parse optional invariant clause
    if (lex_.peek().kind == TokenKind::Invariant) {
        lex_.next(); // consume 'invariant'
        parseContractClause("invariant", ts.invariants);
    }

    if (lex_.peek().kind == TokenKind::Dedent)
        lex_.next(); // consume Dedent

    return ts;
}

StmtNode Parser::parseTypeAliasStatement() {
    Token typeTok = lex_.next(); // consume 'type'

    Token nameTok = lex_.peek();
    if (nameTok.kind != TokenKind::Ident)
        parseError(nameTok.line, "expected type name after 'type'");
    lex_.next(); // consume name

    if (!isPascalCase(nameTok.value))
        parseError(nameTok.line, "type alias name '" + nameTok.value + "' must be PascalCase");

    if (lex_.peek().kind != TokenKind::Equals)
        parseError("expected '=' in type alias declaration");
    lex_.next(); // consume '='

    std::string targetType = parseTypeName();

    TypeAliasStmt s;
    s.name = nameTok.value;
    s.target_type = targetType;
    s.loc = locFromToken(typeTok);
    return s;
}

StmtNode Parser::parseEnumStatement() {
    Token enumTok = lex_.next(); // consume 'enum'

    Token nameTok = lex_.peek();
    if (nameTok.kind != TokenKind::Ident)
        parseError(nameTok.line, "expected enum name after 'enum'");
    lex_.next(); // consume name

    if (!isPascalCase(nameTok.value))
        parseError(nameTok.line, "enum name '" + nameTok.value + "' must be PascalCase");

    // Optional type parameters: enum Name<T, U>:
    std::vector<std::string> typeParams;
    if (lex_.peek().kind == TokenKind::Less) {
        lex_.next(); // consume '<'
        for (;;) {
            Token tp = lex_.peek();
            if (tp.kind != TokenKind::Ident)
                parseError(tp.line, "expected type parameter name");
            lex_.next();
            typeParams.push_back(tp.value);
            if (lex_.peek().kind != TokenKind::Comma)
                break;
            lex_.next(); // consume ','
        }
        if (lex_.peek().kind != TokenKind::Greater)
            parseError("expected '>' after type parameters");
        lex_.next(); // consume '>'
    }

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
    es.type_params = std::move(typeParams);
    es.loc = locFromToken(enumTok);
    std::unordered_set<std::string> seenVariants;

    while (lex_.peek().kind != TokenKind::Dedent &&
           lex_.peek().kind != TokenKind::Eof) {
        Token variantName = lex_.peek();
        if (variantName.kind != TokenKind::Ident)
            parseError(variantName.line, "expected variant name");
        lex_.next(); // consume variant name

        if (!isPascalCase(variantName.value))
            parseError(variantName.line, "enum variant '" + variantName.value + "' must be PascalCase");

        if (seenVariants.count(variantName.value))
            parseError(variantName.line, "duplicate variant name '" + variantName.value + "'");

        EnumVariant variant;
        variant.name = variantName.value;

        // Associated data: Variant(type1, type2, ...)
        if (lex_.peek().kind == TokenKind::LParen) {
            lex_.next(); // consume '('
            if (lex_.peek().kind != TokenKind::RParen) {
                for (;;) {
                    variant.field_types.push_back(parseTypeName());
                    if (lex_.peek().kind != TokenKind::Comma)
                        break;
                    lex_.next(); // consume ','
                }
            }
            if (lex_.peek().kind != TokenKind::RParen)
                parseError("expected ')' in enum variant definition");
            lex_.next(); // consume ')'
        }

        es.variants.push_back(std::move(variant));
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

    // Int literal type (42, -10) or range type (1..12, -10..10)
    if (lex_.peek().kind == TokenKind::Number || lex_.peek().kind == TokenKind::Minus) {
        std::string name;
        if (lex_.peek().kind == TokenKind::Minus) {
            lex_.next(); // consume '-'
            if (lex_.peek().kind != TokenKind::Number)
                parseError("expected number after '-' in type");
            name = "-" + lex_.peek().value;
        } else {
            name = lex_.peek().value;
        }
        // Reject non-decimal numeric literals in type positions
        const auto &numVal = (name[0] == '-') ? name.substr(1) : name;
        if (numVal.size() > 1 && numVal[0] == '0' && !std::isdigit(numVal[1])) {
            parseError("only decimal integer literals are allowed in type positions");
        }
        lex_.next(); // consume number
        // Range type: N..M
        if (lex_.peek().kind == TokenKind::DotDot) {
            lex_.next(); // consume '..'
            bool negEnd = (lex_.peek().kind == TokenKind::Minus);
            if (negEnd) lex_.next(); // consume '-'
            if (lex_.peek().kind != TokenKind::Number)
                parseError("expected number after '..' in range type");
            // Reject non-decimal numeric literals in range end
            const auto &endNum = lex_.peek().value;
            if (endNum.size() > 1 && endNum[0] == '0' && !std::isdigit(endNum[1])) {
                parseError("only decimal integer literals are allowed in type positions");
            }
            std::string endVal = (negEnd ? "-" : "") + endNum;
            lex_.next(); // consume end number
            return name + ".." + endVal;
        }
        return name;
    }

    // String literal type: "N"
    if (lex_.peek().kind == TokenKind::String) {
        std::string name = "\"" + lex_.peek().value + "\"";
        lex_.next(); // consume string
        return name;
    }

    Token t = lex_.peek();
    if (t.kind == TokenKind::ErrorKw) {
        lex_.next(); // consume 'Error'
        std::string name = "Error";
        if (lex_.peek().kind == TokenKind::Question) {
            lex_.next(); // consume '?'
            name += "?";
        }
        return name;
    }
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
            // Two-parameter generic: Map<K, V> or Result<V, E>
            lex_.next(); // consume ','
            std::string secondTy = parseTypeName();
            if (lex_.peek().kind != TokenKind::Greater)
                parseError("expected '>' in " + name + " type");
            lex_.next(); // consume '>'
            name += "<" + inner + ", " + secondTy + ">";
        } else {
            if (lex_.peek().kind != TokenKind::Greater)
                parseError("expected '>' after generic type parameter");
            lex_.next(); // consume '>'
            name += "<" + inner + ">";
        }
    }

    // Optional type suffix: int? => wraps in Option<T>
    if (lex_.peek().kind == TokenKind::Question) {
        lex_.next(); // consume '?'
        name = name + "?";
    }

    return name;
}

ExprPtr Parser::parseLambdaExpr() {
    // fn(params) [-> retType]: body
    // 'fn' is consumed by caller, now at '('
    if (lex_.peek().kind != TokenKind::LParen)
        parseError("expected '(' after 'fn' in lambda");
    lex_.next(); // consume '('

    auto lambda = std::make_unique<LambdaExpr>();

    // Parse parameters
    if (lex_.peek().kind != TokenKind::RParen) {
        for (;;) {
            Token paramName = lex_.peek();
            if (paramName.kind != TokenKind::Ident)
                parseError(paramName.line, "expected parameter name in lambda");
            lex_.next(); // consume param name

            if (lex_.peek().kind != TokenKind::Colon)
                parseError("expected ':' after parameter name in lambda");
            lex_.next(); // consume ':'

            std::string paramType = parseTypeName();
            lambda->params.push_back({paramName.value, paramType});

            if (lex_.peek().kind != TokenKind::Comma)
                break;
            lex_.next(); // consume ','
        }
    }

    if (lex_.peek().kind != TokenKind::RParen)
        parseError("expected ')' in lambda");
    lex_.next(); // consume ')'

    // Optional return type: -> type
    if (lex_.peek().kind == TokenKind::Arrow) {
        lex_.next(); // consume '->'
        lambda->return_type = parseTypeName();
    } else {
        lambda->return_type = "";  // infer
    }

    // Expect ':'
    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after lambda parameters");
    lex_.next(); // consume ':'

    // Body: multi-line (newline + indent) or single expression
    if (lex_.peek().kind == TokenKind::Newline) {
        auto saved = lex_.saveState();
        lex_.next(); // consume newline
        skipNewlines();
        if (lex_.peek().kind == TokenKind::Indent) {
            lex_.restoreState(std::move(saved));
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
            lex_.restoreState(std::move(saved));
            lambda->expr_body = parseTernary();
        }
    } else {
        // Single expression lambda
        lambda->expr_body = parseTernary();
    }

    auto node = std::make_unique<ExprNode>();
    node->data = std::move(lambda);
    return node;
}

ExprPtr Parser::parsePostfix() {
    ExprPtr expr = parsePrimary();
    while (lex_.peek().kind == TokenKind::Dot || lex_.peek().kind == TokenKind::LBracket ||
           lex_.peek().kind == TokenKind::BangBang) {
        if (lex_.peek().kind == TokenKind::BangBang) {
            Token bangTok = lex_.next(); // consume '!!'
            auto ep = std::make_unique<ErrorPropagateExpr>();
            ep->operand = std::move(expr);
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(ep);
            node->loc = locFromToken(bangTok);
            expr = std::move(node);
            continue;
        }
        if (lex_.peek().kind == TokenKind::LBracket) {
            Token lbTok = lex_.next(); // consume '['
            ExprPtr index = parseTernary();
            if (lex_.peek().kind != TokenKind::RBracket)
                parseError("expected ']'");
            lex_.next(); // consume ']'
            auto idx = std::make_unique<IndexExpr>();
            idx->object = std::move(expr);
            idx->index = std::move(index);
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(idx);
            node->loc = locFromToken(lbTok);
            expr = std::move(node);
            continue;
        }
        // Dot access follows below
        Token dotTok = lex_.next(); // consume '.'
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
            node->loc = locFromToken(dotTok);
            expr = std::move(node);
        } else {
            // Field access: a.x
            auto fa = std::make_unique<FieldAccessExpr>();
            fa->object = std::move(expr);
            fa->field = field.value;
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(fa);
            node->loc = locFromToken(dotTok);
            expr = std::move(node);
        }
    }
    return expr;
}

void Parser::tryParseTrailingBlock(CallStmt &s) {
    if (lex_.peek().kind == TokenKind::Colon) {
        lex_.next(); // consume ':'
        s.args.push_back(parseTrailingBlockAsLambda());
    }
}

ExprPtr Parser::parseTrailingBlockAsLambda() {
    // ':' is already consumed. Parse the block and wrap it in a LambdaExpr.
    auto body = parseBlock();
    auto lambda = std::make_unique<LambdaExpr>();
    lambda->return_type = "";
    lambda->body = std::move(body);
    auto node = std::make_unique<ExprNode>();
    node->data = std::move(lambda);
    return node;
}

StmtNode Parser::parseExpectStatement() {
    Token expectTok = lex_.next(); // consume 'expect'

    if (lex_.peek().kind != TokenKind::LParen)
        parseError("expected '(' after 'expect'");
    lex_.next(); // consume '('

    ExprPtr actual = parseTernary();

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
    es.loc = {expectTok.line, expectTok.col, file_id_};

    if (lex_.peek().kind != TokenKind::LParen)
        parseError("expected '(' after matcher name");
    lex_.next(); // consume '('

    static const std::unordered_set<std::string> matchers_with_arg = {
        "to_eq", "to_not_eq", "to_contain", "to_not_contain",
        "to_be_greater_than", "to_be_less_than",
        "to_be_greater_than_or_eq", "to_be_less_than_or_eq",
        "to_have_length", "to_start_with", "to_end_with"
    };
    static const std::unordered_set<std::string> matchers_no_arg = {
        "to_be_true", "to_be_false", "to_be_none", "to_be_some", "to_be_ok", "to_be_err", "to_be_empty"
    };

    if (matchers_with_arg.count(matcher)) {
        es.expected = parseTernary();
    } else if (matchers_no_arg.count(matcher)) {
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

    // Helper: parse a binding pattern like Some(x), Ok(x), Err(x)
    auto parseBindingPattern = [&](const std::string &name) -> std::string {
        lex_.next(); // consume pattern name
        if (lex_.peek().kind != TokenKind::LParen)
            parseError("expected '(' after '" + name + "'");
        lex_.next(); // consume '('
        Token binding = lex_.peek();
        if (binding.kind != TokenKind::Ident)
            parseError(binding.line, "expected variable name in " + name + " pattern");
        lex_.next(); // consume binding name
        if (lex_.peek().kind != TokenKind::RParen)
            parseError("expected ')' after " + name + " binding");
        lex_.next(); // consume ')'
        return binding.value;
    };

    if (t.kind == TokenKind::Ident && t.value == "Some")
        return SomePattern{parseBindingPattern("Some")};
    if (t.kind == TokenKind::Ident && t.value == "Ok")
        return OkPattern{parseBindingPattern("Ok")};
    if (t.kind == TokenKind::Ident && t.value == "Err")
        return ErrPattern{parseBindingPattern("Err")};

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

    // Identifier: could be Enum::Variant, Enum::Variant(bindings), or variable binding
    if (t.kind == TokenKind::Ident) {
        lex_.next(); // consume ident
        if (lex_.peek().kind == TokenKind::ColonColon) {
            lex_.next(); // consume '::'
            Token variant = lex_.peek();
            if (variant.kind != TokenKind::Ident)
                parseError(variant.line, "expected variant name after '::'");
            lex_.next(); // consume variant name
            // Check for constructor pattern: Enum::Variant(a, b, ...)
            if (lex_.peek().kind == TokenKind::LParen) {
                lex_.next(); // consume '('
                std::vector<std::string> bindings;
                if (lex_.peek().kind != TokenKind::RParen) {
                    for (;;) {
                        Token binding = lex_.peek();
                        if (binding.kind != TokenKind::Ident)
                            parseError(binding.line, "expected binding name in constructor pattern");
                        lex_.next();
                        bindings.push_back(binding.value);
                        if (lex_.peek().kind != TokenKind::Comma)
                            break;
                        lex_.next(); // consume ','
                    }
                }
                if (lex_.peek().kind != TokenKind::RParen)
                    parseError("expected ')' in constructor pattern");
                lex_.next(); // consume ')'
                return EnumConstructorPattern{t.value, variant.value, std::move(bindings)};
            }
            return EnumPattern{t.value, variant.value};
        }
        return VariablePattern{t.value};
    }

    parseError(t.line, "expected pattern");
}

StmtNode Parser::parseMatchStatement() {
    Token matchTok = lex_.next(); // consume 'match'
    ExprPtr subject = parseTernary();

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
    matchStmt->loc = locFromToken(matchTok);

    while (lex_.peek().kind != TokenKind::Dedent &&
           lex_.peek().kind != TokenKind::Eof) {
        if (lex_.peek().kind != TokenKind::Case)
            parseError(lex_.peek().line, "expected 'case' in match block");
        lex_.next(); // consume 'case'

        MatchArm arm;
        arm.pattern = parsePattern();

        // Check for OR pattern: case A | B | C:
        if (lex_.peek().kind == TokenKind::Pipe) {
            auto orPat = std::make_unique<OrPattern>();
            // Check that first pattern has no variable binding
            if (std::holds_alternative<VariablePattern>(arm.pattern) ||
                std::holds_alternative<SomePattern>(arm.pattern))
                parseError("OR pattern cannot contain variable bindings");
            orPat->alternatives.push_back(std::move(arm.pattern));
            while (lex_.peek().kind == TokenKind::Pipe) {
                lex_.next(); // consume '|'
                Pattern alt = parsePattern();
                if (std::holds_alternative<VariablePattern>(alt) ||
                    std::holds_alternative<SomePattern>(alt))
                    parseError("OR pattern cannot contain variable bindings");
                orPat->alternatives.push_back(std::move(alt));
            }
            arm.pattern = std::move(orPat);
        }

        // Optional guard: if <expr>
        if (lex_.peek().kind == TokenKind::If) {
            lex_.next(); // consume 'if'
            arm.guard = parseTernary();
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

StmtNode Parser::parseSelectStatement() {
    Token selectTok = lex_.next(); // consume 'select'
    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after select");
    lex_.next(); // consume ':'

    if (lex_.peek().kind != TokenKind::Newline)
        parseError("expected newline after ':'");
    lex_.next(); // consume Newline
    skipNewlines();

    if (lex_.peek().kind != TokenKind::Indent)
        parseError("expected indented block");
    lex_.next(); // consume Indent

    auto selectStmt = std::make_unique<SelectStmt>();
    selectStmt->loc = locFromToken(selectTok);
    bool seenElse = false;
    bool seenTimeout = false;

    while (lex_.peek().kind != TokenKind::Dedent &&
           lex_.peek().kind != TokenKind::Eof) {
        if (lex_.peek().kind == TokenKind::Else) {
            if (seenElse)
                parseError("select may have at most one else branch");
            if (seenTimeout)
                parseError("select cannot have both else and timeout branches");
            lex_.next(); // consume else
            if (lex_.peek().kind != TokenKind::Colon)
                parseError("expected ':' after else");
            lex_.next(); // consume ':'
            selectStmt->else_body = parseBlock();
            seenElse = true;
            skipNewlines();
            if (lex_.peek().kind != TokenKind::Dedent)
                parseError("select else branch must be last");
            break;
        }

        if (lex_.peek().kind == TokenKind::Ident && lex_.peek().value == "timeout") {
            if (seenTimeout)
                parseError("select may have at most one timeout branch");
            if (seenElse)
                parseError("select cannot have both else and timeout branches");
            Token timeoutTok = lex_.next(); // consume timeout
            selectStmt->timeout_loc = locFromToken(timeoutTok);
            selectStmt->timeout_ms = parseTernary();
            if (lex_.peek().kind != TokenKind::Colon)
                parseError("expected ':' after select timeout");
            lex_.next(); // consume ':'
            selectStmt->timeout_body = parseBlock();
            seenTimeout = true;
            skipNewlines();
            if (lex_.peek().kind != TokenKind::Dedent)
                parseError("select timeout branch must be last");
            break;
        }

        if (lex_.peek().kind != TokenKind::Case)
            parseError(lex_.peek().line, "expected 'case', 'else', or 'timeout' in select block");
        Token caseTok = lex_.next(); // consume case

        if (lex_.peek().kind == TokenKind::Ident && lex_.peek().value == "let") {
            lex_.next(); // consume 'let' identifier
            Token nameTok = lex_.peek();
            if (nameTok.kind != TokenKind::Ident)
                parseError(nameTok.line, "expected variable name after 'let' in select recv case");
            if (!isSnakeCase(nameTok.value))
                parseError(nameTok.line, "variable name '" + nameTok.value + "' must be snake_case");
            lex_.next(); // consume name
            if (lex_.peek().kind != TokenKind::Equals)
                parseError("expected '=' after select recv binding");
            lex_.next(); // consume '='

            Token recvTok = lex_.peek();
            if (recvTok.kind != TokenKind::Ident ||
                (recvTok.value != "recv" && recvTok.value != "recv_opt"))
                parseError("select case must be 'let name = recv(ch)', 'let name = recv_opt(ch)', or 'send(ch, value)'");
            lex_.next(); // consume recv / recv_opt
            if (lex_.peek().kind != TokenKind::LParen)
                parseError("expected '(' after " + recvTok.value);
            lex_.next(); // consume '('
            ExprPtr channel = parseTernary();
            if (lex_.peek().kind != TokenKind::RParen)
                parseError("expected ')' after " + recvTok.value + " argument");
            lex_.next(); // consume ')'
            if (lex_.peek().kind != TokenKind::Colon)
                parseError("expected ':' after select case");
            lex_.next(); // consume ':'

            SelectRecvCase recvCase;
            recvCase.name = nameTok.value;
            recvCase.channel = std::move(channel);
            recvCase.mode = recvTok.value == "recv_opt"
                ? SelectRecvMode::Optional
                : SelectRecvMode::Strict;
            recvCase.loc = locFromToken(caseTok);
            recvCase.body = parseBlock();
            selectStmt->cases.push_back(std::move(recvCase));
        } else {
            Token sendTok = lex_.peek();
            if (sendTok.kind != TokenKind::Ident || sendTok.value != "send")
                parseError("select case must be 'let name = recv(ch)', 'let name = recv_opt(ch)', or 'send(ch, value)'");
            lex_.next(); // consume send
            if (lex_.peek().kind != TokenKind::LParen)
                parseError("expected '(' after send");
            lex_.next(); // consume '('
            ExprPtr channel = parseTernary();
            if (lex_.peek().kind != TokenKind::Comma)
                parseError("expected ',' in send()");
            lex_.next(); // consume ','
            ExprPtr value = parseTernary();
            if (lex_.peek().kind != TokenKind::RParen)
                parseError("expected ')' after send arguments");
            lex_.next(); // consume ')'
            if (lex_.peek().kind != TokenKind::Colon)
                parseError("expected ':' after select case");
            lex_.next(); // consume ':'

            SelectSendCase sendCase;
            sendCase.channel = std::move(channel);
            sendCase.value = std::move(value);
            sendCase.loc = locFromToken(caseTok);
            sendCase.body = parseBlock();
            selectStmt->cases.push_back(std::move(sendCase));
        }

        skipNewlines();
    }

    if (selectStmt->cases.empty())
        parseError("select requires at least one case");

    if (lex_.peek().kind == TokenKind::Dedent)
        lex_.next(); // consume Dedent

    return selectStmt;
}
