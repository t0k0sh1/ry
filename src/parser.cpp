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

// ===== A1: parseError helpers =====

[[noreturn]] void Parser::parseError(int line, const std::string &msg) {
    throw DiagnosticError({line, lex_.peek().col, file_id_}, msg, sm_);
}

[[noreturn]] void Parser::parseError(const std::string &msg) {
    parseError(lex_.peek().line, msg);
}

// ===== Compound assignment helper: x op= rhs =====

AssignStmt Parser::makeCompoundAssign(const Token &nameTok, const std::string &op, ExprPtr rhs) {
    AssignStmt s;
    s.name = nameTok.value;
    s.value = std::move(rhs);
    s.compound_op = op;
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

static const std::unordered_set<std::string> known_directives = {"deprecated", "native", "parallel", "each", "property", "const", "inline"};

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
    RecursionGuard guard(*this);
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
        parseError(first.line, "expected 'if', 'while', 'for', 'fn', 'async fn', 'return', 'break', 'continue', 'await', '...', 'enum', 'match', 'expect', 'record', 'type', or identifier, got '" + first.value + "'");
    lex_.next(); // consume ident

    Token next = lex_.peek();
    if (next.kind == TokenKind::Colon) {
        // Type-annotated declaration: x: int = 42 or @native @const PI: float
        lex_.next(); // consume ':'
        auto typeAnnotation = parseTypeName();
        AssignStmt s;
        s.name = first.value;
        s.type_annotation = std::move(typeAnnotation);
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
        // Compound assignment: preserve compound_op for codegen resolution
        Token opTok = lex_.next(); // consume +=, -=, //=, **=, etc.
        std::string op = opTok.value.substr(0, opTok.value.size() - 1); // extract "//" from "//="
        return makeCompoundAssign(first, op, parseTernary());
    } else if (next.kind == TokenKind::PlusPlus || next.kind == TokenKind::MinusMinus) {
        Token opTok = lex_.next(); // consume ++ or --
        std::string op = (opTok.kind == TokenKind::PlusPlus) ? "+" : "-";
        auto one = std::make_unique<ExprNode>();
        one->data = NumberExpr{1, ""};
        one->loc = locFromToken(first);
        return makeCompoundAssign(first, op, std::move(one));
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
    RecursionGuard guard(*this);
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

    std::vector<std::string> var_names;

    Token varTok = lex_.peek();
    if (varTok.kind != TokenKind::Ident)
        parseError(varTok.line, "expected variable name after 'for'");
    if (!isSnakeCase(varTok.value))
        parseError(varTok.line, "loop variable name '" + varTok.value + "' must be snake_case");
    lex_.next(); // consume var name
    var_names.push_back(varTok.value);

    while (lex_.peek().kind == TokenKind::Comma) {
        lex_.next(); // consume ','
        Token vTok = lex_.peek();
        if (vTok.kind != TokenKind::Ident)
            parseError(vTok.line, "expected variable name after ',' in for loop");
        if (!isSnakeCase(vTok.value))
            parseError(vTok.line, "loop variable name '" + vTok.value + "' must be snake_case");
        lex_.next(); // consume var
        var_names.push_back(vTok.value);
    }

    if (lex_.peek().kind != TokenKind::In)
        parseError("expected 'in' after variable name in for loop");
    lex_.next(); // consume 'in'

    ExprPtr iterable = parseTernary();

    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after for loop iterable");
    lex_.next(); // consume ':'

    auto forStmt = std::make_unique<ForStmt>();
    forStmt->var_names = std::move(var_names);
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

