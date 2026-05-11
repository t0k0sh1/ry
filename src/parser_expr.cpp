#include "ry/parser.hpp"
#include "ry/diagnostic.hpp"
#include <stdexcept>
#include <string>


namespace ry {

ExprPtr Parser::parseLogicalOr()  { return parseBinaryLeft(&Parser::parseLogicalAnd, {TokenKind::Or}); }

// Parses `case ...:` expression forms (issues #799/#800).
//
// Two forms are recognized:
//   1. `case:`        — no subject, condition-based arms (replaces `when:`)
//   2. `case <expr>:` — with subject, pattern-based arms (replaces `match`)
//
// The leading `case` token is consumed here.
ExprPtr Parser::parseCaseExpr() {
    Token caseTok = lex_.next(); // consume 'case'
    if (lex_.peek().kind == TokenKind::Colon)
        return parseCaseExprNoSubject(caseTok);
    return parseCaseExprWithSubject(caseTok);
}

ExprPtr Parser::parseCaseExprNoSubject(const Token &caseTok) {
    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after 'case'");
    lex_.next(); // consume ':'

    if (lex_.peek().kind != TokenKind::Newline)
        parseError("expected newline after ':'");
    lex_.next();
    skipNewlines();

    if (lex_.peek().kind != TokenKind::Indent)
        parseError("expected indented block");
    lex_.next();

    auto caseExpr = std::make_unique<CaseCondExpr>();
    bool seenWildcard = false;
    while (lex_.peek().kind != TokenKind::Dedent &&
           lex_.peek().kind != TokenKind::Eof) {
        Token first = lex_.peek();
        bool isWildcard = (first.kind == TokenKind::Ident && first.value == "_");
        if (isWildcard) {
            // Need to differentiate `_ : value` (wildcard arm) from `_` used as
            // a sub-expression of a condition`, but in this context the `_`
            // must always be a wildcard default — no other interpretation is
            // legal at arm position.
            const auto &saved = first;
            lex_.next(); // consume '_'
            if (lex_.peek().kind != TokenKind::Colon)
                parseError(saved.line, "expected ':' after '_' in case expression wildcard arm");
            if (seenWildcard)
                parseError(saved.line, "duplicate '_' arm in case expression");
            lex_.next(); // consume ':'
            caseExpr->else_expr = parseConditional();
            seenWildcard = true;
        } else {
            if (seenWildcard)
                parseError("condition arms must appear before '_:'");
            CaseCondExprArm arm;
            arm.condition = parseConditional();
            if (lex_.peek().kind != TokenKind::Colon)
                parseError("expected ':' after case condition");
            lex_.next();
            arm.value = parseConditional();
            caseExpr->arms.push_back(std::move(arm));
        }

        if (lex_.peek().kind == TokenKind::Newline)
            lex_.next();
        skipNewlines();
    }

    if (lex_.peek().kind == TokenKind::Dedent)
        lex_.next();
    if (caseExpr->arms.empty())
        parseError("case expression must have at least one condition arm");
    if (!caseExpr->else_expr)
        parseError("case expression requires a '_: ...' wildcard arm");

    auto node = std::make_unique<ExprNode>();
    node->data = std::move(caseExpr);
    node->loc = locFromToken(caseTok);
    return node;
}

ExprPtr Parser::parseCaseExprWithSubject(const Token &caseTok) {
    ExprPtr subject = parseConditional();

    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after case subject");
    lex_.next(); // consume ':'

    if (lex_.peek().kind != TokenKind::Newline)
        parseError("expected newline after ':'");
    lex_.next();
    skipNewlines();

    if (lex_.peek().kind != TokenKind::Indent)
        parseError("expected indented block");
    lex_.next();

    auto caseExpr = std::make_unique<CaseExpr>();
    caseExpr->subject = std::move(subject);

    while (lex_.peek().kind != TokenKind::Dedent &&
           lex_.peek().kind != TokenKind::Eof) {
        CaseExprArm arm;
        arm.pattern = parsePattern();
        parseOrPattern(arm.pattern);

        if (lex_.peek().kind == TokenKind::If) {
            lex_.next();
            arm.guard = parseConditional();
        }

        if (lex_.peek().kind != TokenKind::Colon)
            parseError("expected ':' after case pattern in case expression");
        lex_.next(); // consume ':'

        arm.value = parseConditional();
        caseExpr->arms.push_back(std::move(arm));

        if (lex_.peek().kind == TokenKind::Newline)
            lex_.next();
        skipNewlines();
    }

    if (caseExpr->arms.empty())
        parseError("case expression must have at least one arm");

    if (lex_.peek().kind == TokenKind::Dedent)
        lex_.next();

    auto node = std::make_unique<ExprNode>();
    node->data = std::move(caseExpr);
    node->loc = locFromToken(caseTok);
    return node;
}

std::vector<StmtNode> Parser::parseIfExpressionBranchBody() {
    if (lex_.peek().kind == TokenKind::Newline)
        return parseBlock();

    std::vector<StmtNode> body;
    body.reserve(1);
    ExprStmt stmt;
    stmt.expr = parseConditional();
    body.push_back(std::move(stmt));
    return body;
}

// Parses the expression form of `if` (issue #798):
//   1. Single-expression form: `if <cond> => <then_val> else <else_val>`
//   2. Colon form: `if <cond>: <then_body> else: <else_body>` — each branch
//      may be a same-line expression or an indented block; both branches must
//      end with an expression statement (tail-expression semantics).
//
// The leading `if` token is consumed here.
ExprPtr Parser::parseIfExpression() {
    Token ifTok = lex_.next(); // consume 'if'
    bool prev_in_if_cond = in_if_cond_;
    in_if_cond_ = true;
    ExprPtr cond;
    try {
        cond = parseConditional();
    } catch (...) {
        in_if_cond_ = prev_in_if_cond;
        throw;
    }
    in_if_cond_ = prev_in_if_cond;

    if (lex_.peek().kind == TokenKind::FatArrow) {
        // Single-expression form: if <cond> => <then_val> else <else_val>
        lex_.next(); // consume '=>'
        ExprPtr thenVal = parseConditional();

        if (lex_.peek().kind != TokenKind::Else)
            parseError("expected 'else' in if expression");
        lex_.next(); // consume 'else'

        // The `else` branch in the single-expression form takes a value directly
        // (without `=>`), since `else` is an unconditional fallback.
        ExprPtr elseVal = parseConditional();

        auto ifExpr = std::make_unique<IfExpr>();
        ifExpr->condition = std::move(cond);
        ifExpr->then_value = std::move(thenVal);
        ifExpr->else_value = std::move(elseVal);
        ifExpr->loc = locFromToken(ifTok);

        auto node = std::make_unique<ExprNode>();
        node->data = std::move(ifExpr);
        node->loc = locFromToken(ifTok);
        return node;
    }

    if (lex_.peek().kind == TokenKind::Colon) {
        // Colon form: if <cond>: <inline-expr|block> else: <inline-expr|block>
        lex_.next(); // consume ':'
        std::vector<StmtNode> thenBody = parseIfExpressionBranchBody();

        // Inline-expr form may place `else:` on the next line. The trailing
        // newline after the then-branch expression is not a statement
        // terminator here — it is interior whitespace inside the if.
        if (lex_.peek().kind == TokenKind::Newline)
            lex_.next();
        if (lex_.peek().kind != TokenKind::Else)
            parseError("if expression (colon form) requires an 'else:' branch");
        lex_.next(); // consume 'else'

        if (lex_.peek().kind != TokenKind::Colon)
            parseError("expected ':' after 'else' in if expression colon form");
        lex_.next(); // consume ':'
        std::vector<StmtNode> elseBody = parseIfExpressionBranchBody();

        auto ifBlock = std::make_unique<IfBlockExpr>();
        ifBlock->condition = std::move(cond);
        ifBlock->then_body = std::move(thenBody);
        ifBlock->else_body = std::move(elseBody);
        ifBlock->loc = locFromToken(ifTok);

        auto node = std::make_unique<ExprNode>();
        node->data = std::move(ifBlock);
        node->loc = locFromToken(ifTok);
        return node;
    }

    parseError("expected '=>' or ':' after if condition in expression context");
}

ExprPtr Parser::parseConditional() {
    RecursionGuard guard(*this);
    if (lex_.peek().kind == TokenKind::Case)
        return parseCaseExpr();
    if (lex_.peek().kind == TokenKind::If)
        return parseIfExpression();
    ExprPtr expr = parseNullCoalesce();
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
        if (k == TokenKind::EqEq || k == TokenKind::BangEq ||
            k == TokenKind::Less || k == TokenKind::LessEq ||
            k == TokenKind::Greater || k == TokenKind::GreaterEq ||
            k == TokenKind::In || k == TokenKind::NotIn) {
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
    RecursionGuard guard(*this);
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
        auto targetType = parseCastTypeName();
        auto cast = std::make_unique<CastExpr>();
        cast->value = std::move(expr);
        cast->target_type = std::move(targetType);
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(cast);
        node->loc = locFromToken(asTok);
        expr = std::move(node);
    }
    return expr;
}

ExprPtr Parser::parseLogicalNot() {
    RecursionGuard guard(*this);
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
    if (lex_.peek().kind == TokenKind::Await)
        return parseAwaitExpr();
    return parseComparison();
}

ExprPtr Parser::parseAwaitExpr() {
    Token awaitTok = lex_.next(); // consume 'await'
    if (!in_async_fn_)
        parseError(awaitTok.line, "'await' can only be used inside an 'async fn'; use 'blockOn()' in synchronous context");
    ExprPtr operand = parseLogicalNot();
    auto awaitExpr = std::make_unique<AwaitExpr>();
    awaitExpr->operand = std::move(operand);
    auto node = std::make_unique<ExprNode>();
    node->data = std::move(awaitExpr);
    node->loc = locFromToken(awaitTok);
    return node;
}

ExprPtr Parser::parsePrimary() {
    RecursionGuard guard(*this);
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
        auto [numStr, suffix] = splitNumericSuffix(t.value);
        auto node = std::make_unique<ExprNode>();
        if (suffix == "f32" || suffix == "f64") {
            lex_.next();
            node->data = FloatExpr{parseFloatLiteral(numStr), suffix};
        } else {
            // Validate before consuming so parseError() points at the literal token.
            int64_t val;
            if (!tryParseIntLiteral(numStr, &val))
                parseError("integer literal out of range for int: " + numStr);
            lex_.next();
            node->data = NumberExpr{val, suffix};
        }
        node->loc = locFromToken(t);
        return node;
    }
    if (t.kind == TokenKind::Float) {
        lex_.next();
        auto [numStr, suffix] = splitNumericSuffix(t.value);
        if (!suffix.empty() && suffix[0] != 'f')
            parseError("cannot use integer suffix '" + suffix + "' on float literal");
        auto node = std::make_unique<ExprNode>();
        node->data = FloatExpr{parseFloatLiteral(numStr), suffix};
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
    if (t.kind == TokenKind::RegexLiteral) {
        lex_.next();
        auto node = std::make_unique<ExprNode>();
        node->data = RegexExpr{t.value};
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
        interp->parts.reserve(4);
        interp->exprs.reserve(4);
        interp->parts.push_back(t.value);
        interp->exprs.push_back(parseConditional());
        while (lex_.peek().kind == TokenKind::FStringMid) {
            Token mid = lex_.next();
            interp->parts.push_back(mid.value);
            interp->exprs.push_back(parseConditional());
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
    // weak expr — create weak reference from strong reference
    if (t.kind == TokenKind::Ident && t.value == "weak") {
        lex_.next(); // consume 'weak'
        ExprPtr operand = parsePrimary();
        auto weakExpr = std::make_unique<WeakExpr>();
        weakExpr->operand = std::move(operand);
        auto node = std::make_unique<ExprNode>();
        node->data = std::move(weakExpr);
        node->loc = locFromToken(t);
        return node;
    }
    if (t.kind == TokenKind::Ident) {
        lex_.next();
        if (lex_.peek().kind == TokenKind::LBracket) {
            auto savedState = lex_.saveState();
            try {
                lex_.next(); // consume '['
                std::string typeArg;
                typeArg.reserve(32);
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
        if (lex_.peek().kind == TokenKind::Less && couldBeGenericEnum()) {
            // Try to parse as generic type: Ident<Type>::Variant(...)
            auto savedState = lex_.saveState();
            try {
                lex_.next(); // consume '<'
                std::string typeArgs = "<";
                typeArgs.reserve(32);
                int depth = 1;
                while (depth > 0 && lex_.peek().kind != TokenKind::Eof) {
                    Token tk = lex_.peek();
                    if (tk.kind == TokenKind::Less) {
                        depth++;
                    } else if (tk.kind == TokenKind::Greater) {
                        depth--;
                        if (depth == 0) {
                            lex_.next(); // consume final '>'
                            typeArgs += ">";
                            break;
                        }
                    } else if (tk.kind == TokenKind::GreaterGreater) {
                        // ">>" counts as two '>' closings
                        depth -= 2;
                        if (depth <= 0) {
                            // Split: consume first '>', leave second as current
                            lex_.consumeGreaterInTypeContext();
                            typeArgs += ">";
                            if (depth < 0)
                                break; // unbalanced, will fail at :: check
                            // depth == 0: need to consume the remaining '>'
                            if (lex_.peek().kind == TokenKind::Greater) {
                                lex_.next();
                                typeArgs += ">";
                            }
                            break;
                        }
                        typeArgs += ">>";
                        lex_.next();
                        continue;
                    } else if (tk.kind == TokenKind::GreaterGreaterGreater) {
                        depth -= 3;
                        if (depth <= 0) {
                            // Split: consume first '>', leave ">>" as current
                            lex_.consumeGreaterInTypeContext();
                            typeArgs += ">";
                            // consume remaining '>' tokens
                            while (depth < 0 || lex_.peek().kind == TokenKind::Greater || lex_.peek().kind == TokenKind::GreaterGreater) {
                                if (lex_.peek().kind == TokenKind::Greater) {
                                    lex_.next();
                                    typeArgs += ">";
                                } else {
                                    lex_.consumeGreaterInTypeContext();
                                    typeArgs += ">";
                                }
                                depth++;
                                if (depth >= 0) break;
                            }
                            break;
                        }
                        typeArgs += ">>>";
                        lex_.next();
                        continue;
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
            call->args = parseArgList(&call->named_args);
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(call);
            node->loc = locFromToken(t);
            return node;
        }
        // Bare-paren-omitted single-param lambda: `s => expr`.
        // Suppressed inside an if-expression condition so `if flag => then ...`
        // is parsed as the if then-arm rather than a lambda.
        if (lex_.peek().kind == TokenKind::FatArrow && !in_if_cond_) {
            if (!isCamelCase(t.value))
                parseError(t.line, "parameter name '" + t.value + "' must be camelCase");
            // #1723: bare lambda param must not shadow an imported module —
            // the body would then route `math.sqrt(...)` to the qualified
            // call instead of treating `math` as the lambda parameter.
            // Mirrors the same guard on parenthesized lambda params
            // (parseParenLambdaExpr) and fn-decl params (parser_decl.cpp).
            rejectImportShadowing(t);
            lex_.next(); // consume '=>'
            auto lambda = std::make_unique<LambdaExpr>();
            lambda->params.push_back({t.value, TypeNode::makeBasic("any"), nullptr});
            lambda->return_type = nullptr;
            bool prev_in_async = in_async_fn_;
            in_async_fn_ = false;
            try {
                lambda->expr_body = parseConditional();
            } catch (...) {
                in_async_fn_ = prev_in_async;
                throw;
            }
            in_async_fn_ = prev_in_async;
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(lambda);
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
        ExprPtr first = parseConditional();
        if (lex_.peek().kind == TokenKind::Colon) {
            // Map literal: first expression was the key
            lex_.next(); // consume ':'
            auto map = std::make_unique<MapExpr>();
            map->keys.reserve(4);
            map->values.reserve(4);
            ExprPtr val = parseConditional();
            map->keys.push_back(std::move(first));
            map->values.push_back(std::move(val));
            while (lex_.peek().kind == TokenKind::Comma) {
                lex_.next(); // consume ','
                if (lex_.peek().kind == TokenKind::RBrace)
                    break;
                ExprPtr k = parseConditional();
                if (lex_.peek().kind != TokenKind::Colon)
                    parseError("expected ':' after map key");
                lex_.next(); // consume ':'
                ExprPtr v = parseConditional();
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
            set->elements.reserve(4);
            set->elements.push_back(std::move(first));
            while (lex_.peek().kind == TokenKind::Comma) {
                lex_.next(); // consume ','
                if (lex_.peek().kind == TokenKind::RBrace)
                    break;
                set->elements.push_back(parseConditional());
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
        list->elements.reserve(4);
        skipStructuralTokens();
        if (lex_.peek().kind != TokenKind::RBracket) {
            list->elements.push_back(parseConditional());
            skipStructuralTokens();
            while (lex_.peek().kind == TokenKind::Comma) {
                lex_.next(); // consume ','
                skipStructuralTokens();
                if (lex_.peek().kind == TokenKind::RBracket)
                    break;
                list->elements.push_back(parseConditional());
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
        // Try Option A lambda first: (params) [-> type]: expr|block
        // Use lookahead predicate to avoid expensive try-catch for non-lambda cases
        if (couldBeLambda()) {
            auto saved = lex_.saveState();
            const bool prev_committed = lambda_committed_;
            lambda_committed_ = false;
            try {
                auto lambda = parseParenLambdaExpr();
                lambda->loc = locFromToken(t);
                lambda_committed_ = prev_committed;
                return lambda;
            } catch (...) {
                // If the lambda was already committed past the param list,
                // the error is a real lambda-shape error (e.g. snake_case
                // param name) and must surface — do not fall back to tuple.
                if (lambda_committed_) {
                    lambda_committed_ = prev_committed;
                    throw;
                }
                lambda_committed_ = prev_committed;
                lex_.restoreState(std::move(saved));
            }
        }
        // Grouping or tuple
        lex_.next();
        ExprPtr first = parseConditional();
        if (lex_.peek().kind == TokenKind::Comma) {
            // Tuple literal: (expr, expr, ...)
            auto tuple = std::make_unique<TupleExpr>();
            tuple->elements.reserve(4);
            tuple->elements.push_back(std::move(first));
            while (lex_.peek().kind == TokenKind::Comma) {
                lex_.next(); // consume ','
                if (lex_.peek().kind == TokenKind::RParen)
                    break;
                tuple->elements.push_back(parseConditional());
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

bool Parser::couldBeLambda() {
    // Lookahead predicate to avoid expensive try-catch for non-lambda cases.
    // Returns true if the current '(' *might* begin a lambda; false if it
    // definitely does NOT.  Must be conservative: false negatives break
    // valid programs, false positives only cost one try-catch.
    auto saved = lex_.saveState();
    lex_.next(); // consume '('
    TokenKind first = lex_.peek().kind;
    bool result;
    if (first == TokenKind::RParen) {
        // () — could be lambda if followed by ':', '=>', or '->'
        lex_.next(); // consume ')'
        TokenKind after = lex_.peek().kind;
        result = (after == TokenKind::Colon || after == TokenKind::Arrow || after == TokenKind::FatArrow);
    } else {
        // (ident...) could be lambda; (non-ident...) cannot
        result = (first == TokenKind::Ident);
    }
    lex_.restoreState(std::move(saved));
    return result;
}

bool Parser::looksLikeParenthesizedTupleDestructure() {
    // Must always restore lexer state — false negatives fall through to the
    // existing expression-statement / lambda branches.
    auto saved = lex_.saveState();
    lex_.next();
    if (lex_.peek().kind != TokenKind::Ident) {
        lex_.restoreState(std::move(saved));
        return false;
    }
    lex_.next();
    if (lex_.peek().kind != TokenKind::Comma) {
        lex_.restoreState(std::move(saved));
        return false;
    }
    while (lex_.peek().kind == TokenKind::Comma) {
        lex_.next();
        if (lex_.peek().kind != TokenKind::Ident) {
            lex_.restoreState(std::move(saved));
            return false;
        }
        lex_.next();
    }
    if (lex_.peek().kind != TokenKind::RParen) {
        lex_.restoreState(std::move(saved));
        return false;
    }
    lex_.next();
    bool result = (lex_.peek().kind == TokenKind::Equals);
    lex_.restoreState(std::move(saved));
    return result;
}

TypeNodePtr Parser::parseCastTypeName() {
    // In expression context after 'as', '<' is ambiguous between generic type
    // parameter and comparison operator.  Use speculative parse: try
    // parseTypeName() and fall back to a basic type if it fails (i.e. '<' was
    // actually a comparison operator).
    auto saved = lex_.saveState();
    try {
        auto ty = parseTypeName();
        // If parseTypeName consumed '<' as generic, verify it also consumed '>'.
        // A quick sanity check: the next token must NOT be inside an
        // unfinished generic parse.  Since parseTypeName throws on malformed
        // generics, reaching here means the parse was valid.
        return ty;
    } catch (...) {
        lex_.restoreState(std::move(saved));
    }
    // Fallback: parse a simple identifier type (original behaviour).
    Token typeTok = lex_.peek();
    if (typeTok.kind != TokenKind::Ident)
        parseError(typeTok.line, "expected type name after 'as'");
    std::string targetType = lex_.next().value;
    return TypeNode::makeBasic(targetType);
}

bool Parser::couldBeGenericEnum() {
    // Conservative: return false only when '<' definitely starts a
    // comparison.  Must cover all tokens that can begin a type
    // (see parseTypeNameSingle): identifiers, Error, tuple/function
    // types, and literal types.
    auto saved = lex_.saveState();
    lex_.next();
    TokenKind first = lex_.peek().kind;
    bool result;
    switch (first) {
        case TokenKind::Ident:
        case TokenKind::ErrorKw:
        case TokenKind::LParen:
        case TokenKind::Fn:
        case TokenKind::Number:
        case TokenKind::Minus:
        case TokenKind::String:
            result = true;
            break;
        default:
            result = false;
            break;
    }
    lex_.restoreState(std::move(saved));
    return result;
}

ExprPtr Parser::parseParenLambdaExpr() {
    if (lex_.peek().kind != TokenKind::LParen)
        parseError("expected '(' to start lambda");
    lex_.next(); // consume '('

    auto lambda = std::make_unique<LambdaExpr>();
    lambda->params.reserve(4);
    // Defer camelCase validation of param names until after lambda is committed
    // (i.e., after we see '->', '=>', or ':' following the closing paren). The
    // caller wraps parseParenLambdaExpr in try/catch and falls back to tuple
    // parsing on any throw, so an early throw would surface as the wrong
    // diagnostic ("expected '=', ..." from the statement parser).
    std::vector<Token> paramNameTokens;
    paramNameTokens.reserve(4);
    if (lex_.peek().kind != TokenKind::RParen) {
        for (;;) {
            Token paramName = lex_.peek();
            if (paramName.kind != TokenKind::Ident)
                parseError(paramName.line, "expected parameter name in lambda");
            lex_.next(); // consume param name

            TypeNodePtr paramType = TypeNode::makeBasic("any");
            if (lex_.peek().kind == TokenKind::Colon) {
                lex_.next(); // consume ':'
                paramType = parseTypeName();
            }
            if (lex_.peek().kind == TokenKind::Equals)
                parseError(paramName.line, "default arguments are not supported in lambda expressions");

            lambda->params.push_back({paramName.value, std::move(paramType), nullptr});
            paramNameTokens.push_back(paramName);
            if (lex_.peek().kind != TokenKind::Comma)
                break;
            lex_.next(); // consume ','
            if (lex_.peek().kind == TokenKind::RParen)
                break;
        }
    }

    if (lex_.peek().kind != TokenKind::RParen)
        parseError("expected ')' in lambda");
    lex_.next(); // consume ')'

    // Lambda is committed only when one of '->', '=>', or ':' follows the
    // closing paren. Bare '(idents)' without a body-marker is a tuple, so we
    // must not validate param names yet — let the speculative path fall back.
    {
        TokenKind k = lex_.peek().kind;
        if (k != TokenKind::Arrow && k != TokenKind::FatArrow && k != TokenKind::Colon)
            parseError("expected '->', '=>', or ':' after lambda parameter list");
    }
    // Past this point any error must propagate through the speculative
    // try/catch in parsePrimary; raise the commit flag before validating
    // param names so their camelCase diagnostics surface correctly.
    lambda_committed_ = true;
    for (const auto &tok : paramNameTokens) {
        if (!isCamelCase(tok.value))
            parseError(tok.line, "parameter name '" + tok.value + "' must be camelCase");
        rejectImportShadowing(tok);
    }

    if (lex_.peek().kind == TokenKind::Arrow) {
        lex_.next(); // consume '->'
        lambda->return_type = parseTypeName();
    } else {
        lambda->return_type = nullptr;
    }

    bool prev_in_async = in_async_fn_;
    in_async_fn_ = false;

    if (lex_.peek().kind == TokenKind::FatArrow) {
        // Single-expression lambda: (params) => expr
        lex_.next(); // consume '=>'
        lambda->expr_body = parseConditional();
    } else if (lex_.peek().kind == TokenKind::Colon) {
        lex_.next(); // consume ':'
        if (lex_.peek().kind == TokenKind::Newline) {
            // Block lambda: (params):\n  body
            lex_.next();
            skipNewlines();
            if (lex_.peek().kind != TokenKind::Indent)
                parseError("expected indented block after ':' in lambda");
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
                lex_.next();
        } else {
            parseError("use '=>' instead of ':' for single-expression lambdas");
        }
    } else {
        parseError("expected '=>' or ':' after lambda signature");
    }
    in_async_fn_ = prev_in_async;

    auto node = std::make_unique<ExprNode>();
    node->data = std::move(lambda);
    return node;
}

ExprPtr Parser::makeErrorPropagateExpr(ExprPtr operand, const Token &tok) {
    auto ep = std::make_unique<ErrorPropagateExpr>();
    ep->operand = std::move(operand);
    auto node = std::make_unique<ExprNode>();
    node->data = std::move(ep);
    node->loc = locFromToken(tok);
    return node;
}

ExprPtr Parser::parsePostfix() {
    return parsePostfixContinuation(parsePrimary());
}

ExprPtr Parser::parsePostfixContinuation(ExprPtr expr) {
    while (lex_.peek().kind == TokenKind::Dot || lex_.peek().kind == TokenKind::LBracket ||
           lex_.peek().kind == TokenKind::Question) {
        if (lex_.peek().kind == TokenKind::Question) {
            Token qTok = lex_.next(); // consume '?'
            expr = makeErrorPropagateExpr(std::move(expr), qTok);
            continue;
        }
        if (lex_.peek().kind == TokenKind::LBracket) {
            Token lbTok = lex_.next(); // consume '['
            std::vector<ExprPtr> indices;
            indices.reserve(2);
            indices.push_back(parseConditional());
            while (lex_.peek().kind == TokenKind::Comma) {
                lex_.next(); // consume ','
                indices.push_back(parseConditional());
            }
            if (lex_.peek().kind != TokenKind::RBracket)
                parseError("expected ']'");
            lex_.next(); // consume ']'
            auto idx = std::make_unique<IndexExpr>();
            idx->object = std::move(expr);
            idx->indices = std::move(indices);
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(idx);
            node->loc = locFromToken(lbTok);
            expr = std::move(node);
            continue;
        }
        // Dot access follows below
        Token dotTok = lex_.next(); // consume '.'
        Token field = lex_.peek();
        // After '.', accept identifiers, numbers (tuple indices), and keyword
        // tokens (`.and()`, `.expect(...)`, …). The same predicate is reused
        // by the statement-side dot fast path in parser.cpp so qualified
        // calls whose member is a keyword behave identically at statement and
        // expression positions.
        if (!isFieldNameTokenKind(field.kind))
            parseError(field.line, "expected field name or index after '.'");
        lex_.next(); // consume field name/number
        // Qualified module dispatch (#1723): when the LHS is a bare identifier
        // whose name was registered via `import <mod>`, the dot is a namespace
        // access, not UFCS or field access. The original VariableExpr is left
        // intact (stored on CallExpr/FieldAccessExpr) but codegen short-circuits
        // before evaluating it once `qualified_module` is set.
        std::optional<std::string> qualified_module;
        if (auto *ve = std::get_if<VariableExpr>(&expr->data)) {
            if (imported_modules_.count(ve->name) > 0)
                qualified_module = ve->name;
        }
        if (lex_.peek().kind == TokenKind::LParen) {
            lex_.next(); // consume '('
            auto call = std::make_unique<CallExpr>();
            call->callee = field.value;
            if (qualified_module.has_value()) {
                // Qualified call: do NOT prepend LHS as receiver.
                call->qualified_module = std::move(qualified_module);
            } else {
                // UFCS: a.f(b, c) → f(a, b, c)
                call->args.push_back(std::move(expr));
            }
            auto rest = parseArgList(&call->named_args); // consumes ')'
            for (auto &arg : rest)
                call->args.push_back(std::move(arg));
            auto node = std::make_unique<ExprNode>();
            node->data = std::move(call);
            node->loc = locFromToken(dotTok);
            expr = std::move(node);
        } else {
            // Field access: a.x (or qualified const access: math.PI)
            auto fa = std::make_unique<FieldAccessExpr>();
            fa->object = std::move(expr);
            fa->field = field.value;
            fa->qualified_module = std::move(qualified_module);
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
    lambda->return_type = nullptr;  // inferred at codegen time
    lambda->body = std::move(body);
    auto node = std::make_unique<ExprNode>();
    node->data = std::move(lambda);
    return node;
}

} // namespace ry
