#include "ry/parser.hpp"
#include "ry/diagnostic.hpp"
#include <stdexcept>
#include <string>

static void coerceFirstArgToString(std::vector<ExprPtr> &args) {
    if (!args.empty()) {
        if (auto *ve = std::get_if<VariableExpr>(&args[0]->data)) {
            args[0]->data = StringExpr{ve->name};
        }
    }
}

ExprPtr Parser::parseLogicalOr()  { return parseBinaryLeft(&Parser::parseLogicalAnd, {TokenKind::Or}); }

ExprPtr Parser::parseWhenExpr() {
    Token whenTok = lex_.next(); // consume 'when'
    if (lex_.peek().kind != TokenKind::Colon)
        parseError("single-condition when expressions are not supported; use 'when:'");
    lex_.next(); // consume ':'

    if (lex_.peek().kind != TokenKind::Newline)
        parseError("expected newline after ':'");
    lex_.next();
    skipNewlines();

    if (lex_.peek().kind != TokenKind::Indent)
        parseError("expected indented block");
    lex_.next();

    auto whenExpr = std::make_unique<WhenCondExpr>();
    bool seenElse = false;
    while (lex_.peek().kind != TokenKind::Dedent &&
           lex_.peek().kind != TokenKind::Eof) {
        if (lex_.peek().kind == TokenKind::Else) {
            if (seenElse)
                parseError("duplicate 'else' arm in when expression");
            lex_.next();
            if (lex_.peek().kind != TokenKind::FatArrow)
                parseError("expected '=>' after else");
            lex_.next();
            whenExpr->else_expr = parseConditional();
            seenElse = true;
        } else {
            if (seenElse)
                parseError("condition arms must appear before 'else =>'");
            WhenCondExprArm arm;
            arm.condition = parseConditional();
            if (lex_.peek().kind != TokenKind::FatArrow)
                parseError("expected '=>' after when condition");
            lex_.next();
            arm.value = parseConditional();
            whenExpr->arms.push_back(std::move(arm));
        }

        if (lex_.peek().kind == TokenKind::Newline)
            lex_.next();
        skipNewlines();
    }

    if (lex_.peek().kind == TokenKind::Dedent)
        lex_.next();
    if (whenExpr->arms.empty())
        parseError("when expression must have at least one condition");
    if (!whenExpr->else_expr)
        parseError("when expression requires an 'else => ...' arm");

    auto node = std::make_unique<ExprNode>();
    node->data = std::move(whenExpr);
    node->loc = locFromToken(whenTok);
    return node;
}

ExprPtr Parser::parseConditional() {
    RecursionGuard guard(*this);
    if (lex_.peek().kind == TokenKind::When)
        return parseWhenExpr();
    ExprPtr expr = parseNullCoalesce();
    if (lex_.peek().kind == TokenKind::Question) {
        parseError("ternary expressions are no longer supported; use 'when:'");
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
        Token typeTok = lex_.peek();
        if (typeTok.kind != TokenKind::Ident)
            parseError(typeTok.line, "expected type name after 'as'");
        std::string targetType = lex_.next().value;
        auto cast = std::make_unique<CastExpr>();
        cast->value = std::move(expr);
        cast->target_type = TypeNode::makeBasic(targetType);
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
        parseError(awaitTok.line, "'await' can only be used inside an 'async function'; use 'block_on()' in synchronous context");
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
        lex_.next();
        auto [numStr, suffix] = splitNumericSuffix(t.value);
        auto node = std::make_unique<ExprNode>();
        if (suffix == "f32" || suffix == "f64")
            node->data = FloatExpr{parseFloatLiteral(numStr), suffix};
        else
            node->data = NumberExpr{parseIntLiteral(numStr), suffix};
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
    if (t.kind == TokenKind::Fn) {
        parseError(t.line,
            "anonymous 'function' expressions (e.g. 'function(x) => x + 1') "
            "are not supported; use paren-lambda syntax instead: (x: int): x + 1");
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
        ExprPtr first = parseConditional();
        if (lex_.peek().kind == TokenKind::Colon) {
            // Map literal: first expression was the key
            lex_.next(); // consume ':'
            auto map = std::make_unique<MapExpr>();
            ExprPtr val = parseConditional();
            map->keys.push_back(std::move(first));
            map->values.push_back(std::move(val));
            while (lex_.peek().kind == TokenKind::Comma) {
                lex_.next(); // consume ','
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
            set->elements.push_back(std::move(first));
            while (lex_.peek().kind == TokenKind::Comma) {
                lex_.next(); // consume ','
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
        skipStructuralTokens();
        if (lex_.peek().kind != TokenKind::RBracket) {
            list->elements.push_back(parseConditional());
            skipStructuralTokens();
            while (lex_.peek().kind == TokenKind::Comma) {
                lex_.next(); // consume ','
                skipStructuralTokens();
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
        auto saved = lex_.saveState();
        try {
            auto lambda = parseParenLambdaExpr();
            lambda->loc = locFromToken(t);
            return lambda;
        } catch (...) {
            lex_.restoreState(std::move(saved));
        }
        // Grouping or tuple
        lex_.next();
        ExprPtr first = parseConditional();
        if (lex_.peek().kind == TokenKind::Comma) {
            // Tuple literal: (expr, expr, ...)
            auto tuple = std::make_unique<TupleExpr>();
            tuple->elements.push_back(std::move(first));
            while (lex_.peek().kind == TokenKind::Comma) {
                lex_.next(); // consume ','
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

ExprPtr Parser::parseParenLambdaExpr() {
    if (lex_.peek().kind != TokenKind::LParen)
        parseError("expected '(' to start lambda");
    lex_.next(); // consume '('

    auto lambda = std::make_unique<LambdaExpr>();
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
            if (lex_.peek().kind != TokenKind::Comma)
                break;
            lex_.next(); // consume ','
        }
    }

    if (lex_.peek().kind != TokenKind::RParen)
        parseError("expected ')' in lambda");
    lex_.next(); // consume ')'

    if (lex_.peek().kind == TokenKind::Arrow) {
        lex_.next(); // consume '->'
        lambda->return_type = parseTypeName();
    } else {
        lambda->return_type = nullptr;
    }

    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after lambda signature");
    lex_.next(); // consume ':'

    bool prev_in_async = in_async_fn_;
    in_async_fn_ = false;
    if (lex_.peek().kind == TokenKind::Newline) {
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
        lambda->expr_body = parseConditional();
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
    ExprPtr expr = parsePrimary();
    while (lex_.peek().kind == TokenKind::Dot || lex_.peek().kind == TokenKind::LBracket ||
           lex_.peek().kind == TokenKind::BangBang || lex_.peek().kind == TokenKind::Question) {
        if (lex_.peek().kind == TokenKind::Question) {
            auto saved = lex_.saveState();
            Token qTok = lex_.next(); // consume '?'
            TokenKind next = lex_.peek().kind;
            // Disambiguate postfix ? (error propagation) from ternary ? :
            // by checking whether the next token can start an expression
            // but is NOT an operator/postfix continuation (those mean the
            // ? was postfix, e.g. `result? + 1` or `result?[0]`).
            // Tokens that unambiguously continue a postfix expression
            // (binary operators, field access, chained ?).
            bool isPostfixContinuation =
                (next == TokenKind::Plus || next == TokenKind::Minus ||
                 next == TokenKind::Tilde ||
                 next == TokenKind::Dot || next == TokenKind::BangBang ||
                 next == TokenKind::Question);
            if (!isPostfixContinuation &&
                (next == TokenKind::Ident || next == TokenKind::Number ||
                 next == TokenKind::Float || next == TokenKind::String ||
                 next == TokenKind::True || next == TokenKind::False ||
                 next == TokenKind::LParen || next == TokenKind::LBrace ||
                 next == TokenKind::Not ||
                 next == TokenKind::NoneKw || next == TokenKind::ErrorKw ||
                 next == TokenKind::FStringStart ||
                 next == TokenKind::Await ||
                 next == TokenKind::LBracket ||
                 next == TokenKind::RegexLiteral)) {
                lex_.restoreState(std::move(saved));
                break; // delegate to parseConditional()
            }
            expr = makeErrorPropagateExpr(std::move(expr), qTok);
            continue;
        }
        if (lex_.peek().kind == TokenKind::BangBang) {
            Token bangTok = lex_.next(); // consume '!!'
            expr = makeErrorPropagateExpr(std::move(expr), bangTok);
            continue;
        }
        if (lex_.peek().kind == TokenKind::LBracket) {
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
    lambda->return_type = nullptr;  // inferred at codegen time
    lambda->body = std::move(body);
    auto node = std::make_unique<ExprNode>();
    node->data = std::move(lambda);
    return node;
}
