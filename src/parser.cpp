#include "ry/parser.hpp"
#include "ry/diagnostic.hpp"
#include "ry/directive_meta.hpp"
#include <stdexcept>
#include <string>
#include <unordered_set>


namespace ry {

// ===== Mock/verify helper: coerce first arg identifier to string =====

void Parser::coerceFirstArgToString(std::vector<ExprPtr> &args) {
    if (!args.empty()) {
        if (auto *ve = std::get_if<VariableExpr>(&args[0]->data)) {
            args[0]->data = StringExpr{ve->name};
        }
    }
}

// ===== Naming convention helpers =====

bool Parser::isMutationFnName(const std::string &name) {
    if (name.empty()) return false;
    size_t len = name.back() == '!' ? name.size() - 1 : name.size();
    if (len == 0) return false;
    size_t start = 0;
    if (name[0] == '_') {
        if (len == 1) return false;
        start = 1;
    }
    unsigned char first = static_cast<unsigned char>(name[start]);
    if (!(first >= 'a' && first <= 'z')) return false;
    for (size_t i = start + 1; i < len; ++i) {
        unsigned char ch = static_cast<unsigned char>(name[i]);
        if (!((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9')))
            return false;
    }
    return true;
}

bool Parser::isScreamingSnakeCase(const std::string &name) {
    if (name.empty()) return false;
    unsigned char first = static_cast<unsigned char>(name[0]);
    if (!(first >= 'A' && first <= 'Z')) return false;
    for (size_t i = 1; i < name.size(); ++i) {
        unsigned char ch = static_cast<unsigned char>(name[i]);
        if (!((ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9') || ch == '_'))
            return false;
    }
    return true;
}

bool Parser::isPascalCase(const std::string &name) {
    if (name.empty()) return false;
    unsigned char first = static_cast<unsigned char>(name[0]);
    if (!(first >= 'A' && first <= 'Z')) return false;
    for (size_t i = 1; i < name.size(); ++i) {
        unsigned char ch = static_cast<unsigned char>(name[i]);
        if (!((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9')))
            return false;
    }
    return true;
}

bool Parser::isCamelCase(const std::string &name) {
    if (name.empty()) return false;
    size_t start = 0;
    if (name[0] == '_') {
        if (name.size() == 1) return false;
        start = 1;
    }
    unsigned char first = static_cast<unsigned char>(name[start]);
    if (!(first >= 'a' && first <= 'z')) return false;
    for (size_t i = start + 1; i < name.size(); ++i) {
        unsigned char ch = static_cast<unsigned char>(name[i]);
        if (!((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9')))
            return false;
    }
    return true;
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

// ===== Chained LHS finalizer =====
//
// Given a postfix expression chain whose root is an Ident (built by the
// Dot/LBracket paths of parseStatement), look at the next token and produce:
//   - IndexAssignStmt   if the chain tail is an IndexExpr   and '='/'+='/etc. follows
//   - FieldAssignStmt   if the chain tail is a FieldAccessExpr and '='/'+='/etc. follows
//   - ExprStmt          if no assignment operator follows and the tail is a CallExpr
//                       (e.g. deeper UFCS chains like `a.b.c(d)`)
//   - parseError        preserving the existing "expected '=' after index expression" /
//                       "expected '=' after field name" messages for the common typo
//                       cases that the old 1-hop parser used to emit.
//
// The helper is intentionally narrow: the caller has already committed to a
// statement starting with `Ident` followed by `.` or `[`, so the chain root
// is always a VariableExpr.
StmtNode Parser::finishChainedLhs(ExprPtr chain, const Token &first) {
    auto tailKindIsIndex = [](const ExprNode &n) {
        return std::holds_alternative<std::unique_ptr<IndexExpr>>(n.data);
    };
    auto tailKindIsField = [](const ExprNode &n) {
        return std::holds_alternative<std::unique_ptr<FieldAccessExpr>>(n.data);
    };

    auto isCompoundAssignTok = [](TokenKind k) {
        return k == TokenKind::PlusEq  || k == TokenKind::MinusEq ||
               k == TokenKind::StarEq  || k == TokenKind::SlashEq ||
               k == TokenKind::PercentEq ||
               k == TokenKind::SlashSlashEq || k == TokenKind::StarStarEq ||
               k == TokenKind::AmpEq  || k == TokenKind::PipeEq ||
               k == TokenKind::CaretEq ||
               k == TokenKind::LessLessEq || k == TokenKind::GreaterGreaterEq;
    };

    auto buildStmt = [&](ExprPtr value, std::optional<std::string> op) -> StmtNode {
        if (tailKindIsIndex(*chain)) {
            auto &idx = std::get<std::unique_ptr<IndexExpr>>(chain->data);
            IndexAssignStmt s;
            s.object = std::move(idx->object);
            s.indices = std::move(idx->indices);
            s.value = std::move(value);
            s.compound_op = std::move(op);
            s.loc = locFromToken(first);
            return s;
        }
        if (tailKindIsField(*chain)) {
            auto &fa = std::get<std::unique_ptr<FieldAccessExpr>>(chain->data);
            FieldAssignStmt s;
            s.object = std::move(fa->object);
            s.field = fa->field;
            s.value = std::move(value);
            s.compound_op = std::move(op);
            s.loc = locFromToken(first);
            return s;
        }
        // A chain that begins with `[` or `.` can still end in a CallExpr if
        // parsePostfixContinuation consumed a UFCS-style `.f(args)` at the
        // tail, but that is not an assignable lvalue. Reject here with a
        // clear message rather than silently producing a garbage stmt.
        parseError(first.line, "left side of assignment is not an lvalue");
    };

    Token peek = lex_.peek();
    if (peek.kind == TokenKind::Equals) {
        lex_.next(); // consume '='
        ExprPtr value = parseConditional();
        return buildStmt(std::move(value), std::nullopt);
    }
    if (isCompoundAssignTok(peek.kind)) {
        Token opTok = lex_.next(); // consume compound op
        std::string op = opTok.value.substr(0, opTok.value.size() - 1);
        ExprPtr value = parseConditional();
        return buildStmt(std::move(value), op);
    }
    if (peek.kind == TokenKind::PlusPlus || peek.kind == TokenKind::MinusMinus) {
        Token opTok = lex_.next();
        std::string op = (opTok.kind == TokenKind::PlusPlus) ? "+" : "-";
        auto one = std::make_unique<ExprNode>();
        one->data = NumberExpr{1, ""};
        one->loc = locFromToken(first);
        return buildStmt(std::move(one), op);
    }

    // No assignment operator. Preserve the old strict errors for the direct
    // typo cases (ending in index or field access), and route legitimate
    // statement-level postfix expressions (UFCS chains, error-propagate
    // tails, etc.) to ExprStmt so they behave like any other expression
    // statement.
    if (tailKindIsIndex(*chain))
        parseError(peek.line, "expected '=' after index expression");
    if (tailKindIsField(*chain))
        parseError(peek.line, "expected '=' after field name");
    return ExprStmt{std::move(chain), locFromToken(first)};
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

std::vector<ExprPtr> Parser::parseArgList(std::vector<NamedArg> *named_out) {
    std::vector<ExprPtr> args;
    args.reserve(4);
    bool seen_named = false;
    std::unordered_set<std::string> seen_named_names;
    if (lex_.peek().kind != TokenKind::RParen) {
        auto parseOne = [&]() {
            ExprPtr expr = parseConditional();
            auto *var = std::get_if<VariableExpr>(&expr->data);
            if (var != nullptr && lex_.peek().kind == TokenKind::Equals) {
                if (named_out == nullptr)
                    parseError("named arguments are not supported here");
                lex_.next(); // consume '='
                NamedArg na;
                na.name = var->name;
                na.value = parseConditional();
                if (!seen_named_names.insert(na.name).second)
                    parseError("duplicate named argument '" + na.name + "'");
                named_out->push_back(std::move(na));
                seen_named = true;
            } else {
                if (seen_named)
                    parseError("positional arguments cannot follow named arguments");
                args.push_back(std::move(expr));
            }
        };
        parseOne();
        while (lex_.peek().kind == TokenKind::Comma) {
            lex_.next();
            if (lex_.peek().kind == TokenKind::RParen)
                break;
            parseOne();
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
    prog.reserve(32);
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

std::vector<Directive> Parser::parseDirectives() {
    std::vector<Directive> directives;
    while (lex_.peek().kind == TokenKind::At) {
        Token atTok = lex_.next(); // consume '@'
        Token nameTok = lex_.peek();
        if (nameTok.kind != TokenKind::Ident)
            parseError(nameTok.line, "expected directive name after '@'");
        lex_.next(); // consume name

        Directive d;
        d.name = nameTok.value;
        d.loc = {atTok.line, atTok.col, file_id_};

        // Optional argument list: @name(arg1, key=arg2, ...)
        // Exception (#1189): if the `(` starts a parenthesized tuple-destructure
        // LHS — shape `(Ident (, Ident)+ ) =` — leave it for parseStatement so
        // `@const (a, b) = expr` parses as directive + statement, not as
        // `@const(a, b)` with positional arguments.
        if (lex_.peek().kind == TokenKind::LParen &&
            !looksLikeParenthesizedTupleDestructure()) {
            lex_.next(); // consume '('

            while (lex_.peek().kind != TokenKind::RParen) {
                // Parse a full expression. If it turns out to be a bare
                // VariableExpr followed by '=', treat it as a named argument.
                ExprPtr expr = parseConditional();
                auto *var = std::get_if<VariableExpr>(&expr->data);
                if (var != nullptr && lex_.peek().kind == TokenKind::Equals) {
                    lex_.next(); // consume '='
                    DirectiveArg arg;
                    arg.name = var->name;
                    arg.value = parseConditional();
                    d.args.push_back(std::move(arg));
                } else {
                    DirectiveArg arg;
                    arg.name = std::nullopt;
                    arg.value = std::move(expr);
                    d.args.push_back(std::move(arg));
                }

                if (lex_.peek().kind == TokenKind::Comma)
                    lex_.next(); // consume ','
            }
            lex_.next(); // consume ')'
        }

        directives.push_back(std::move(d));

        // Consume newlines between directives
        skipNewlines();
    }
    return directives;
}

StmtNode Parser::parseImportStatement() {
    static constexpr const char *kHyphenError =
        "hyphens '-' are not allowed in module names; "
        "use underscores '_' instead";

    // Parse dot-separated path segments: ident ('.' ident)*
    // Checks for hyphens after each segment and after each dot.
    auto parseDotPath = [&](std::string &path) {
        if (lex_.peek().kind == TokenKind::Minus)
            parseError(lex_.peek().line, kHyphenError);
        while (lex_.peek().kind == TokenKind::Dot) {
            lex_.next();
            Token part = lex_.peek();
            if (part.kind == TokenKind::Minus)
                parseError(part.line, kHyphenError);
            if (part.kind != TokenKind::Ident)
                parseError(part.line, "expected identifier after '.'");
            path += "/" + lex_.next().value;
        }
    };

    Token fromTok = lex_.next(); // consume 'from'

    std::string modulePath;
    Token modTok = lex_.peek();

    if (modTok.kind == TokenKind::DotDot) {
        parseError(modTok.line,
            "parent directory imports ('from ..') are not supported");
    } else if (modTok.kind == TokenKind::Dot) {
        lex_.next();
        modulePath = ".";
        if (lex_.peek().kind == TokenKind::Minus)
            parseError(lex_.peek().line, kHyphenError);
        if (lex_.peek().kind == TokenKind::Ident) {
            modulePath += "/" + lex_.next().value;
            parseDotPath(modulePath);
        }
    } else if (modTok.kind == TokenKind::Ident) {
        modulePath = lex_.next().value;
        parseDotPath(modulePath);
    } else {
        parseError(modTok.line, "expected module name after 'from'");
    }

    std::vector<std::string> names;
    names.reserve(4);
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

    // @directive(...) declares a new directive (#708). Must be followed by `fn`.
    // Only @public is permitted alongside @directive so stdlib directive
    // definitions can be marked public for the v0.0.19 visibility model. Other
    // directives (e.g. @inline, @deprecated, @native) remain rejected so misuse
    // surfaces with a clear message.
    if (const Directive *dirAnnot = findDirective(directives, "directive")) {
        for (const auto &d : directives) {
            if (d.name != "directive" && d.name != "public")
                parseError(d.loc.line,
                    "@directive can only be combined with @public");
        }
        if (first.kind != TokenKind::Fn)
            parseError(first.line, "@directive must be followed by 'fn'");
        return parseDirectiveDefStatement(dirAnnot, std::move(directives));
    }

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
        // Reject built-in directives (registry-tracked); user-defined directives
        // pass through to codegen validateDirectives() (silent no-op on target
        // mismatch per #1425). @parallel is intentionally not in the registry —
        // it is gated by the separate parallel_count check below so it can be
        // mixed with user-defined directives on the same for-loop.
        const auto &builtins = builtinDirectiveRegistry();
        int parallel_count = 0;
        for (const auto &d : directives) {
            if (builtins.count(d.name) > 0)
                parseError(first.line,
                    "built-in directive '@" + d.name + "' is not supported on for statements");
            if (d.name == "parallel")
                ++parallel_count;
        }
        if (parallel_count > 1)
            parseError(first.line, "for statements support only a single @parallel directive");
        auto stmt = parseForStatement();
        auto &fs = std::get<std::unique_ptr<ForStmt>>(stmt);
        fs->directives = std::move(directives);
        return stmt;
    }

    // @each / @property on `it` calls: special-case parse for backward compatibility.
    // (On fn statements these directives are allowed and handled in codegen.)
    if (!directives.empty()) {
        bool hasTestDirective = hasDirective(directives, "each") || hasDirective(directives, "property");
        if (hasTestDirective && first.kind == TokenKind::Ident && first.value == "it") {
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

    // Parenthesized tuple destructuring (#1189). Must run before the directive
    // gate below so `@const (a, b) = expr` passes through.
    if (first.kind == TokenKind::LParen && looksLikeParenthesizedTupleDestructure()) {
        lex_.next();
        std::vector<std::string> names;
        {
            Token n = lex_.peek();
            if (n.value != "_" && !isCamelCase(n.value))
                parseError(n.line, "tuple-destructure name '" + n.value + "' must be camelCase");
            names.push_back(n.value);
            lex_.next();
        }
        while (lex_.peek().kind == TokenKind::Comma) {
            lex_.next();
            Token n = lex_.peek();
            if (n.value != "_" && !isCamelCase(n.value))
                parseError(n.line, "tuple-destructure name '" + n.value + "' must be camelCase");
            names.push_back(n.value);
            lex_.next();
        }
        if (lex_.peek().kind != TokenKind::RParen)
            parseError(lex_.peek().line, "expected ')' in tuple destructuring");
        lex_.next();
        if (lex_.peek().kind != TokenKind::Equals)
            parseError(lex_.peek().line, "expected '=' after tuple destructuring pattern");
        lex_.next();
        ExprPtr value = parseConditional();
        TupleDestructStmt s;
        s.names = std::move(names);
        s.value = std::move(value);
        s.is_immutable = hasDirective(directives, "const");
        s.directives = std::move(directives);
        s.loc = locFromToken(first);
        return s;
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

    if (first.kind == TokenKind::Case)
        return parseCaseStatement();

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
        if (!in_async_fn_)
            parseError(awaitTok.line, "'await' can only be used inside an 'async fn'; use 'blockOn()' in synchronous context");
        AwaitStmt s;
        s.operand = parseLogicalNot();
        s.loc = locFromToken(awaitTok);
        return s;
    }

    // Non-identifier expression statement: e.g. [1, 2, 3].map(f)
    if (first.kind != TokenKind::Ident) {
        ExprPtr expr = parseConditional();
        return ExprStmt{std::move(expr), locFromToken(first)};
    }
    lex_.next(); // consume ident

    Token next = lex_.peek();
    if (next.kind == TokenKind::Colon) {
        // Type-annotated declaration: x: int = 42 or @native @const PI: float
        // #1470: enforce camelCase on the binding name; SCREAMING_SNAKE_CASE
        // is allowed when @native or @const is present (e.g. @const PI: float
        // = 3.14, @native @const PI: float — established Ry convention for
        // module-level constants, see docs/reference/functions.md).
        bool allowsScreamingSnake = hasDirective(directives, "native") ||
                                    hasDirective(directives, "const");
        if (!isCamelCase(first.value) &&
            !(allowsScreamingSnake && isScreamingSnakeCase(first.value))) {
            parseError(first.line,
                "variable name '" + first.value +
                "' must be camelCase (or SCREAMING_SNAKE_CASE for @native or @const variable names)");
        }
        lex_.next(); // consume ':'
        auto typeAnnotation = parseTypeName();
        AssignStmt s;
        s.name = first.value;
        s.type_annotation = std::move(typeAnnotation);
        s.directives = std::move(directives);
        s.loc = locFromToken(first);
        if (lex_.peek().kind == TokenKind::Equals) {
            lex_.next(); // consume '='
            s.value = parseConditional();
        } else {
            // Value-less declaration (e.g., @native @const PI: float)
            if (!hasDirective(s.directives, "native"))
                parseError("expected '=' after type annotation");
        }
        return s;
    } else if (next.kind == TokenKind::LBracket) {
        if (!directives.empty())
            parseError(first.line, "directives are not supported on index assignment");
        // Chained index LHS: parse `ident[...]` and then any further postfix
        // hops so we can handle `a[i].field = v`, `a[i][j] = v`,
        // `a[i] += v`, etc. The trailing token is examined by
        // finishChainedLhs which emits the correct stmt variant.
        Token lbTok = lex_.next(); // consume '['
        std::vector<ExprPtr> indices;
        indices.push_back(parseConditional());
        while (lex_.peek().kind == TokenKind::Comma) {
            lex_.next(); // consume ','
            indices.push_back(parseConditional());
        }
        if (lex_.peek().kind != TokenKind::RBracket)
            parseError("expected ']'");
        lex_.next(); // consume ']'

        auto varExpr = std::make_unique<ExprNode>();
        varExpr->data = VariableExpr{first.value};
        varExpr->loc = locFromToken(first);
        auto idxExpr = std::make_unique<IndexExpr>();
        idxExpr->object = std::move(varExpr);
        idxExpr->indices = std::move(indices);
        auto chain = std::make_unique<ExprNode>();
        chain->data = std::move(idxExpr);
        chain->loc = locFromToken(lbTok);

        chain = parsePostfixContinuation(std::move(chain));
        return finishChainedLhs(std::move(chain), first);
    } else if (next.kind == TokenKind::Dot) {
        if (!directives.empty())
            parseError(first.line, "directives are not supported on field assignment or method call");
        // Consume the first `.field` hop manually so we can preserve the
        // existing 1-hop UFCS call-statement special case (`ident.method(...)`)
        // which produces a CallStmt rather than an ExprStmt<CallExpr>.
        Token dotTok = lex_.next(); // consume '.'
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
            auto rest = parseArgList(&s.named_args);
            for (auto &arg : rest)
                s.args.push_back(std::move(arg));
            tryParseTrailingBlock(s);
            return s;
        }

        // Build `ident.field` as the chain base and continue parsing any
        // further postfix hops (`.a`, `[i]`, etc.) to support chained LHS
        // forms like `rec.field[i] = v`, `rec.a.b = v`, `rec.a[i].x = v`.
        auto varExpr = std::make_unique<ExprNode>();
        varExpr->data = VariableExpr{first.value};
        varExpr->loc = locFromToken(first);
        auto fa = std::make_unique<FieldAccessExpr>();
        fa->object = std::move(varExpr);
        fa->field = fieldTok.value;
        auto chain = std::make_unique<ExprNode>();
        chain->data = std::move(fa);
        chain->loc = locFromToken(dotTok);

        chain = parsePostfixContinuation(std::move(chain));
        return finishChainedLhs(std::move(chain), first);
    } else if (next.kind == TokenKind::Comma) {
        // Tuple destructuring: a, b = (10, 20)
        std::vector<std::string> names;
        if (first.value != "_" && !isCamelCase(first.value))
            parseError(first.line, "tuple-destructure name '" + first.value + "' must be camelCase");
        names.push_back(first.value);
        while (lex_.peek().kind == TokenKind::Comma) {
            lex_.next(); // consume ','
            Token n = lex_.peek();
            if (n.kind != TokenKind::Ident && n.value != "_")
                parseError("expected identifier or '_' in tuple destructuring");
            if (n.value != "_" && !isCamelCase(n.value))
                parseError(n.line, "tuple-destructure name '" + n.value + "' must be camelCase");
            lex_.next(); // consume ident
            names.push_back(n.value);
        }
        if (lex_.peek().kind != TokenKind::Equals)
            parseError("expected '=' in tuple destructuring");
        lex_.next(); // consume '='
        ExprPtr value = parseConditional();
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
        s.value = parseConditional();
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
        return makeCompoundAssign(first, op, parseConditional());
    } else if (next.kind == TokenKind::PlusPlus || next.kind == TokenKind::MinusMinus) {
        Token opTok = lex_.next(); // consume ++ or --
        std::string op = (opTok.kind == TokenKind::PlusPlus) ? "+" : "-";
        auto one = std::make_unique<ExprNode>();
        one->data = NumberExpr{1, ""};
        one->loc = locFromToken(first);
        return makeCompoundAssign(first, op, std::move(one));
    } else if (next.kind == TokenKind::LParen) {
        // Reject built-in directives on call statements; user-defined directives
        // pass through and are evaluated by codegen validateDirectives() (silent
        // no-op on target mismatch per #1425).
        const auto &builtins = builtinDirectiveRegistry();
        for (const auto &d : directives) {
            if (builtins.count(d.name) > 0)
                parseError(first.line,
                    "built-in directive '@" + d.name + "' is not supported on function calls");
        }
        lex_.next(); // consume '('
        CallStmt s;
        s.callee = first.value;
        s.args = parseArgList(&s.named_args);
        s.loc = locFromToken(first);
        if (s.callee == "mock")
            coerceFirstArgToString(s.args);
        if (s.callee != "mock" && s.callee != "it" && s.callee != "describe") {
            tryParseTrailingBlock(s);
        }
        s.directives = std::move(directives);
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
    stmts.reserve(8);
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

std::vector<StmtNode> Parser::parseBlockOrInline() {
    if (lex_.peek().kind == TokenKind::Newline) {
        return parseBlock();
    }
    // Inline body: single statement on same line
    std::vector<StmtNode> stmts;
    stmts.push_back(parseStatement());
    return stmts;
}

StmtNode Parser::parseWhileStatement() {
    Token whileTok = lex_.next(); // consume 'while'
    ExprPtr cond = parseConditional();

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

    Pattern binding;
    if (lex_.peek().kind == TokenKind::LParen) {
        binding = parsePattern();
    } else {
        Token firstTok = lex_.peek();
        if (firstTok.kind != TokenKind::Ident)
            parseError(firstTok.line, "expected variable name after 'for'");
        if (firstTok.value == "_")
            binding = WildcardPattern{};
        else
            binding = VariablePattern{firstTok.value};
        lex_.next(); // consume first binding token

        if (lex_.peek().kind == TokenKind::Comma) {
            auto tuple = std::make_unique<TuplePattern>();
            tuple->elements.push_back(std::move(binding));
            while (lex_.peek().kind == TokenKind::Comma) {
                lex_.next(); // consume ','
                if (lex_.peek().kind == TokenKind::LParen) {
                    tuple->elements.push_back(parsePattern());
                    continue;
                }
                Token vTok = lex_.peek();
                if (vTok.kind != TokenKind::Ident)
                    parseError(vTok.line, "expected variable name after ',' in for loop");
                if (vTok.value == "_")
                    tuple->elements.push_back(WildcardPattern{});
                else
                    tuple->elements.push_back(VariablePattern{vTok.value});
                lex_.next(); // consume var
            }
            binding = std::move(tuple);
        }
    }
    validateForBindingPattern(binding);

    if (lex_.peek().kind != TokenKind::In)
        parseError("expected 'in' after for loop binding");
    lex_.next(); // consume 'in'

    ExprPtr iterable = parseConditional();

    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after for loop iterable");
    lex_.next(); // consume ':'

    auto forStmt = std::make_unique<ForStmt>();
    forStmt->binding = std::move(binding);
    forStmt->iterable = std::move(iterable);
    forStmt->body = parseBlock();
    forStmt->loc = locFromToken(forTok);
    return forStmt;
}

StmtNode Parser::parseIfStatement() {
    auto ifStmt = std::make_unique<IfStmt>();

    Token ifTok = lex_.next(); // consume 'if'
    ifStmt->loc = locFromToken(ifTok);
    ExprPtr cond = parseConditional();

    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after if condition");
    lex_.next(); // consume ':'

    ifStmt->branch.condition = std::move(cond);
    ifStmt->branch.body = parseBlock();

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

    ExprPtr actual = parseConditional();

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
        "toEq", "toNotEq", "toContain", "toNotContain",
        "toBeGreaterThan", "toBeLessThan",
        "toBeGreaterThanOrEq", "toBeLessThanOrEq",
        "toHaveLen", "toStartWith", "toEndWith"
    };
    static const std::unordered_set<std::string> matchers_no_arg = {
        "toBeTrue", "toBeFalse", "toBeNone", "toBeSome", "toBeOk", "toBeErr", "toBeEmpty"
    };

    if (matchers_with_arg.count(matcher)) {
        es.expected = parseConditional();
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

} // namespace ry
