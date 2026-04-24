#include "ry/parser.hpp"
#include "ry/diagnostic.hpp"
#include <cerrno>
#include <cstdlib>
#include <stdexcept>
#include <string>
#include <unordered_set>


namespace ry {

namespace {
// True if `mag` fits in the signed int64 range when interpreted as the
// magnitude of a literal. `negative` allows one extra value (|INT64_MIN|)
// because two's complement lets `-INT64_MIN` land back on INT64_MIN.
// NumberExpr.value stores the unsigned bit pattern of a non-negative
// magnitude, so negation sites (enum values, pattern literals) must reject
// magnitudes beyond this range before applying the sign.
bool isSignedInt64Magnitude(uint64_t mag, bool negative) {
    uint64_t cap = static_cast<uint64_t>(INT64_MAX) + (negative ? 1u : 0u);
    return mag <= cap;
}
} // namespace

TypeParam Parser::parseOneTypeParam() {
    Token tp = lex_.peek();
    if (tp.kind != TokenKind::Ident)
        parseError(tp.line, "expected type parameter name");
    lex_.next();
    TypeParam param;
    param.name = tp.value;
    if (lex_.peek().kind == TokenKind::Colon) {
        lex_.next(); // consume ':'
        Token bound = lex_.peek();
        if (bound.kind != TokenKind::Ident)
            parseError(bound.line, "expected type constraint name after ':'");
        lex_.next();
        param.bound = bound.value;
    }
    return param;
}


StmtNode Parser::parseFnStatement(const std::vector<Directive> &directives, bool is_async) {
    Token fnTok = lex_.next(); // consume 'fn'

    auto fnStmt = std::make_unique<FnStmt>();
    fnStmt->params.reserve(4);
    fnStmt->loc = locFromToken(fnTok);
    fnStmt->is_async = is_async;

    if (lex_.peek().kind == TokenKind::Operator) {
        Token opKwTok = lex_.peek();
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
            // Compound assignment operators
            case TokenKind::PlusEq: case TokenKind::MinusEq:
            case TokenKind::StarEq: case TokenKind::SlashEq:
            case TokenKind::PercentEq: case TokenKind::SlashSlashEq:
            case TokenKind::StarStarEq:
            case TokenKind::AmpEq: case TokenKind::PipeEq:
            case TokenKind::CaretEq:
            case TokenKind::LessLessEq: case TokenKind::GreaterGreaterEq: {
                if (opTok.line != opKwTok.line ||
                    opTok.col != opKwTok.col + static_cast<int>(opKwTok.value.size())) {
                    parseError(opTok.line,
                        "no whitespace allowed between 'operator' and symbolic operator '" +
                        opTok.value + "' (write 'operator" + opTok.value + "')");
                }
                opName = opTok.value;
                lex_.next();
                break;
            }
            case TokenKind::And: case TokenKind::Or:
            case TokenKind::Not:
                opName = opTok.value;
                lex_.next();
                break;
            case TokenKind::LBracket: {
                lex_.next(); // consume '['
                if (lex_.peek().kind != TokenKind::RBracket)
                    parseError("expected ']' after '[' in operator declaration");
                lex_.next(); // consume ']'
                if (lex_.peek().kind == TokenKind::Equals) {
                    lex_.next(); // consume '='
                    opName = "[]=";
                } else {
                    opName = "[]";
                }
                break;
            }
            case TokenKind::In:
                opName = "in";
                lex_.next(); // consume 'in'
                break;
            case TokenKind::LParen: {
                lex_.next(); // consume '('
                if (lex_.peek().kind != TokenKind::RParen)
                    parseError("expected ')' after '(' in operator declaration");
                lex_.next(); // consume ')'
                opName = "()";
                break;
            }
            case TokenKind::As:
                opName = "as";
                lex_.next(); // consume 'as'
                break;
            default:
                parseError(opTok.line, "expected operator symbol after 'operator'");
        }
        fnStmt->name = "operator" + opName;
    } else {
        Token nameTok = lex_.peek();
        if (nameTok.kind != TokenKind::Ident)
            parseError(nameTok.line, "expected function name after 'fn'");
        bool validName = isMutationFnName(nameTok.value) ||
                         (hasDirective(directives, "native") && isScreamingSnakeCase(nameTok.value));
        if (!validName)
            parseError(nameTok.line, "fn name '" + nameTok.value + "' must be snake_case (or SCREAMING_SNAKE_CASE for @native fn names)");
        lex_.next(); // consume name
        fnStmt->name = nameTok.value;

        // Parse optional type parameters: fn name<T, U>(...) or fn name<T: Bound>(...)
        if (lex_.peek().kind == TokenKind::Less) {
            lex_.next(); // consume '<'
            fnStmt->type_params.reserve(2);
            for (;;) {
                fnStmt->type_params.push_back(parseOneTypeParam());
                if (lex_.peek().kind != TokenKind::Comma)
                    break;
                lex_.next(); // consume ','
                if (lex_.peek().kind == TokenKind::Greater)
                    break;
            }
            if (lex_.peek().kind != TokenKind::Greater)
                parseError("expected '>' after type parameters");
            lex_.next(); // consume '>'
        }
    }

    if (lex_.peek().kind != TokenKind::LParen)
        parseError("expected '(' after function name");
    lex_.next(); // consume '('

    // parse parameters
    bool seen_default = false;
    if (lex_.peek().kind != TokenKind::RParen) {
        for (;;) {
            Token paramName = lex_.peek();
            if (paramName.kind != TokenKind::Ident)
                parseError(paramName.line, "expected parameter name");
            if (!isSnakeCase(paramName.value))
                parseError(paramName.line, "parameter name '" + paramName.value + "' must be snake_case");
            lex_.next(); // consume param name

            TypeNodePtr paramType = TypeNode::makeBasic("any");  // default when type is omitted
            bool has_explicit_type = false;
            if (lex_.peek().kind == TokenKind::Colon) {
                lex_.next(); // consume ':'
                paramType = parseTypeName();
                has_explicit_type = true;
            }

            ExprPtr default_value;
            if (lex_.peek().kind == TokenKind::Equals) {
                if (!has_explicit_type)
                    parseError(paramName.line,
                        "parameter '" + paramName.value + "' with default value must have an explicit type annotation");
                lex_.next(); // consume '='
                default_value = parseConditional();
                seen_default = true;
            } else if (seen_default) {
                parseError(paramName.line,
                    "parameter '" + paramName.value + "' must have a default value "
                    "(all parameters after a default parameter must also have defaults)");
            }

            fnStmt->params.push_back({paramName.value, std::move(paramType), std::move(default_value)});

            if (lex_.peek().kind != TokenKind::Comma)
                break;
            lex_.next(); // consume ','
            if (lex_.peek().kind == TokenKind::RParen)
                break;
        }
    }

    if (lex_.peek().kind != TokenKind::RParen)
        parseError("expected ')'");
    lex_.next(); // consume ')'

    if (lex_.peek().kind == TokenKind::Arrow) {
        lex_.next(); // consume '->'
        fnStmt->return_type = parseTypeName();
    } else {
        fnStmt->return_type = nullptr;
    }

    // Validate operator parameter count
    if (fnStmt->is_operator) {
        size_t nParams = fnStmt->params.size();
        const std::string &opName = fnStmt->name;
        // Unary-only operators
        if (opName == "operator~" || opName == "operatornot") {
            if (nParams != 1)
                parseError("unary operator requires exactly 1 parameter");
        } else if (isCompoundAssignOperator(opName)) {
            if (nParams != 2)
                parseError("compound assignment operator requires exactly 2 parameters");
        } else if (opName == "operator[]") {
            if (nParams < 2)
                parseError("operator[] requires at least 2 parameters (object + index)");
        } else if (opName == "operator[]=") {
            if (nParams < 3)
                parseError("operator[]= requires at least 3 parameters (object + index + value)");
        } else if (opName == "operatorin") {
            if (nParams != 2)
                parseError("operator in requires exactly 2 parameters");
        } else if (opName == "operator()") {
            if (nParams < 2)
                parseError("operator() requires at least 2 parameters (object + arguments)");
        } else if (opName == "operatoras") {
            if (nParams != 1)
                parseError("operator as requires exactly 1 parameter");
            if (!fnStmt->return_type)
                parseError("operator as requires a return type");
        } else if (nParams != 1 && nParams != 2) {
            parseError("operator function requires 1 or 2 parameters");
        }
    }

    // Validate return type for comparison/logical operators
    if (fnStmt->is_operator && fnStmt->return_type) {
        if (isBoolConstrainedOperator(fnStmt->name) && fnStmt->return_type->toString() != "bool") {
            parseError(fnTok.line,
                "operator '" + operatorSymbol(fnStmt->name) + "' must return 'bool', but returns '" +
                fnStmt->return_type->toString() + "'");
        }
    }

    // @native fn: body-less declaration
    if (hasDirective(directives, "native")) {
        if (lex_.peek().kind == TokenKind::Colon)
            parseError("@native fn must not have a body");
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
        if (fnStmt->return_type && fnStmt->return_type->toString() == "Unit")
            parseError("'ensure' requires a non-Unit return type");
        lex_.next(); // consume 'ensure'
        parseEnsureClause(*fnStmt);
    }

    // Track async context for await restriction
    bool prev_in_async = in_async_fn_;
    in_async_fn_ = is_async;

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

    // Restore async context
    in_async_fn_ = prev_in_async;

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
        s.value = parseConditional();
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
        out.push_back(parseConditional());
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

    RecordStmt ts;
    ts.name = nameTok.value;
    ts.loc = locFromToken(recordTok);
    ts.fields.reserve(8);

    // Optional parent type: record Child < Parent:
    if (lex_.peek().kind == TokenKind::Less) {
        lex_.next(); // consume '<'
        Token parentTok = lex_.peek();
        if (parentTok.kind != TokenKind::Ident && parentTok.kind != TokenKind::ErrorKw)
            parseError(parentTok.line, "expected parent record name after '<'");
        if (parentTok.kind == TokenKind::Ident && !isPascalCase(parentTok.value))
            parseError(parentTok.line, "parent record name '" + parentTok.value + "' must be PascalCase");
        lex_.next(); // consume parent name
        ts.parent_name = parentTok.value;
    }

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

        auto fieldType = parseTypeName();

        if (seenFields.count(fieldName.value))
            parseError(fieldName.line, "duplicate field name '" + fieldName.value + "'");
        ts.fields.push_back({fieldName.value, std::move(fieldType), std::move(fieldDirectives)});
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

    auto targetType = parseTypeName();

    TypeAliasStmt s;
    s.name = nameTok.value;
    s.target_type = std::move(targetType);
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

    // Optional type parameters: enum Name<T, U>: or enum Name<T: Bound>:
    std::vector<TypeParam> typeParams;
    if (lex_.peek().kind == TokenKind::Less) {
        lex_.next(); // consume '<'
        for (;;) {
            typeParams.push_back(parseOneTypeParam());
            if (lex_.peek().kind != TokenKind::Comma)
                break;
            lex_.next(); // consume ','
            if (lex_.peek().kind == TokenKind::Greater ||
                lex_.peek().kind == TokenKind::GreaterGreater ||
                lex_.peek().kind == TokenKind::GreaterGreaterGreater)
                break;
        }
        if (!lex_.consumeGreaterInTypeContext())
            parseError("expected '>' after type parameters");
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
    es.variants.reserve(4);
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

        // Associated data: Variant(type1, type2, ...) or Variant(name: type, ...)
        if (lex_.peek().kind == TokenKind::LParen) {
            lex_.next(); // consume '('
            if (lex_.peek().kind != TokenKind::RParen) {
                bool hasNames = false;
                bool firstField = true;
                for (;;) {
                    // Try to detect named field: name: type
                    bool isNamed = false;
                    if (lex_.peek().kind == TokenKind::Ident) {
                        auto saved = lex_.saveState();
                        lex_.next(); // consume potential field name
                        if (lex_.peek().kind == TokenKind::Colon) {
                            isNamed = true;
                        }
                        lex_.restoreState(saved);
                    }

                    if (firstField) {
                        hasNames = isNamed;
                        firstField = false;
                    } else if (isNamed != hasNames) {
                        parseError(lex_.peek().line,
                            "cannot mix named and unnamed fields in enum variant");
                    }

                    if (isNamed) {
                        Token fieldName = lex_.peek();
                        lex_.next(); // consume field name
                        if (!isSnakeCase(fieldName.value))
                            parseError(fieldName.line,
                                "enum variant field name '" + fieldName.value + "' must be snake_case");
                        lex_.next(); // consume ':'
                        variant.field_names.push_back(fieldName.value);
                    }

                    variant.field_types.push_back(parseTypeName());
                    if (lex_.peek().kind != TokenKind::Comma)
                        break;
                    lex_.next(); // consume ','
                    if (lex_.peek().kind == TokenKind::RParen)
                        break;
                }
                // Check for duplicate field names
                if (hasNames) {
                    std::unordered_set<std::string> seen;
                    for (auto &fn : variant.field_names) {
                        if (!seen.insert(fn).second)
                            parseError(variantName.line,
                                "duplicate field name '" + fn + "' in enum variant '" + variantName.value + "'");
                    }
                }
            }
            if (lex_.peek().kind != TokenKind::RParen)
                parseError("expected ')' in enum variant definition");
            lex_.next(); // consume ')'
        }

        // Explicit value: Variant = 200
        if (lex_.peek().kind == TokenKind::Equals) {
            lex_.next(); // consume '='
            if (!variant.field_types.empty())
                parseError(variantName.line, "explicit values are not allowed on ADT enum variants with associated data");
            bool negative = false;
            if (lex_.peek().kind == TokenKind::Minus) {
                negative = true;
                lex_.next(); // consume '-'
            }
            Token valTok = lex_.peek();
            if (valTok.kind != TokenKind::Number)
                parseError(valTok.line, "expected integer literal for enum variant value");
            lex_.next(); // consume number
            int64_t val;
            if (!tryParseIntLiteral(valTok.value, &val))
                parseError(valTok.line, "integer literal out of range for int: " + valTok.value);
            // Enum explicit values are i64-only; tryParseIntLiteral accepts
            // up to UINT64_MAX (for u64 literals elsewhere), so reject
            // anything that doesn't fit the signed range here.
            if (!isSignedInt64Magnitude(static_cast<uint64_t>(val), negative))
                parseError(valTok.line,
                           "integer literal out of range for int: " +
                           std::string(negative ? "-" : "") + valTok.value);
            if (negative)
                val = static_cast<int64_t>(-static_cast<uint64_t>(val));
            variant.explicit_value = val;
        }

        es.variants.push_back(std::move(variant));
        seenVariants.insert(variantName.value);

        if (lex_.peek().kind == TokenKind::Newline)
            lex_.next();
        skipNewlines();
    }

    if (es.variants.empty())
        parseError("enum definition must have at least one variant");

    // Validate: explicit values must be all-or-nothing
    bool anyExplicit = false, anyImplicit = false;
    for (auto &v : es.variants) {
        if (v.explicit_value.has_value()) anyExplicit = true;
        else anyImplicit = true;
    }
    if (anyExplicit && anyImplicit)
        parseError("enum variants must either all have explicit values or none");

    if (lex_.peek().kind == TokenKind::Dedent)
        lex_.next(); // consume Dedent

    return es;
}

TypeNodePtr Parser::parseTypeName() {
    auto node = parseTypeNameSingle();
    if (lex_.peek().kind != TokenKind::Pipe)
        return node;
    // Union type
    std::vector<TypeNodePtr> components;
    components.reserve(4);
    components.push_back(std::move(node));
    while (lex_.peek().kind == TokenKind::Pipe) {
        lex_.next(); // consume '|'
        components.push_back(parseTypeNameSingle());
    }
    return TypeNode::makeUnion(std::move(components));
}

TypeNodePtr Parser::parseTypeNameSingle() {
    // Tuple type: (int, float)
    if (lex_.peek().kind == TokenKind::LParen) {
        lex_.next(); // consume '('
        std::vector<TypeNodePtr> elements;
        elements.reserve(4);
        elements.push_back(parseTypeName());
        while (lex_.peek().kind == TokenKind::Comma) {
            lex_.next(); // consume ','
            if (lex_.peek().kind == TokenKind::RParen)
                break;
            elements.push_back(parseTypeName());
        }
        if (lex_.peek().kind != TokenKind::RParen)
            parseError("expected ')' in tuple type");
        lex_.next(); // consume ')'
        return TypeNode::makeTuple(std::move(elements));
    }

    // fn(int, int) -> int  function type
    if (lex_.peek().kind == TokenKind::Fn) {
        lex_.next(); // consume 'fn'
        return parseFnType();
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
            return TypeNode::makeRange(name, endVal);
        }
        return TypeNode::makeBasic(name);
    }

    // String literal type: "N"
    if (lex_.peek().kind == TokenKind::String) {
        std::string name = "\"" + lex_.peek().value + "\"";
        lex_.next(); // consume string
        return TypeNode::makeBasic(name);
    }

    // weak T  — weak reference type
    if (lex_.peek().kind == TokenKind::Ident && lex_.peek().value == "weak") {
        lex_.next(); // consume 'weak'
        auto inner = parseTypeNameSingle();
        return TypeNode::makeWeak(std::move(inner));
    }

    Token t = lex_.peek();
    if (t.kind == TokenKind::ErrorKw) {
        lex_.next(); // consume 'Error'
        if (lex_.peek().kind == TokenKind::Question) {
            lex_.next(); // consume '?'
            return TypeNode::makeOptional(TypeNode::makeBasic("Error"));
        }
        return TypeNode::makeBasic("Error");
    }
    if (t.kind != TokenKind::Ident)
        parseError(t.line, "expected type name");
    std::string name = t.value;
    lex_.next(); // consume type name

    TypeNodePtr result;
    if (lex_.peek().kind == TokenKind::Less) {
        lex_.next(); // consume '<'
        std::vector<TypeNodePtr> typeArgs;
        typeArgs.reserve(2);
        typeArgs.push_back(parseTypeName());
        if ((name == "Map" || name == "Result") && lex_.peek().kind == TokenKind::Comma) {
            // Two-parameter generic: Map<K, V> or Result<V, E>
            lex_.next(); // consume ','
            typeArgs.push_back(parseTypeName());
            if (lex_.peek().kind == TokenKind::Comma)
                lex_.next();
            if (!lex_.consumeGreaterInTypeContext())
                parseError("expected '>' in " + name + " type");
        } else {
            if (lex_.peek().kind == TokenKind::Comma)
                lex_.next();
            if (!lex_.consumeGreaterInTypeContext())
                parseError("expected '>' after generic type parameter");
        }
        result = TypeNode::makeGeneric(name, std::move(typeArgs));
    } else {
        result = TypeNode::makeBasic(name);
    }

    // Postfix array type: T[N]
    if (lex_.peek().kind == TokenKind::LBracket) {
        lex_.next(); // consume '['
        if (lex_.peek().kind != TokenKind::Number)
            parseError("expected integer size in array type T[N]");
        const std::string &sizeTok = lex_.peek().value;
        char *end = nullptr;
        errno = 0;
        auto size = static_cast<uint64_t>(std::strtoull(sizeTok.c_str(), &end, 10));
        if (errno == ERANGE || end != sizeTok.c_str() + sizeTok.size())
            parseError("invalid or out-of-range array size in array type T[N]");
        lex_.next(); // consume number
        if (lex_.peek().kind != TokenKind::RBracket)
            parseError("expected ']' in array type T[N]");
        lex_.next(); // consume ']'
        result = TypeNode::makeArray(std::move(result), size);
    }

    // Optional type suffix: int? => wraps in Option<T>
    if (lex_.peek().kind == TokenKind::Question) {
        lex_.next(); // consume '?'
        result = TypeNode::makeOptional(std::move(result));
    }

    return result;
}

TypeNodePtr Parser::parseFnType() {
    if (lex_.peek().kind != TokenKind::LParen)
        parseError("expected '(' after 'fn' in function type");
    lex_.next(); // consume '('
    std::vector<TypeNodePtr> paramTypes;
    paramTypes.reserve(4);
    if (lex_.peek().kind != TokenKind::RParen) {
        paramTypes.push_back(parseTypeName());
        while (lex_.peek().kind == TokenKind::Comma) {
            lex_.next(); // consume ','
            if (lex_.peek().kind == TokenKind::RParen)
                break;
            paramTypes.push_back(parseTypeName());
        }
    }
    if (lex_.peek().kind != TokenKind::RParen)
        parseError("expected ')' in function type");
    lex_.next(); // consume ')'
    TypeNodePtr retType;
    if (lex_.peek().kind == TokenKind::Arrow) {
        lex_.next(); // consume '->'
        retType = parseTypeName();
    }
    return TypeNode::makeFn(std::move(paramTypes), std::move(retType));
}

bool Parser::patternHasBinding(const Pattern &p) {
    if (std::holds_alternative<VariablePattern>(p))
        return true;
    if (std::holds_alternative<SomePattern>(p))
        return std::get<SomePattern>(p).binding != "_";
    if (std::holds_alternative<OkPattern>(p))
        return std::get<OkPattern>(p).binding != "_";
    if (std::holds_alternative<ErrPattern>(p))
        return std::get<ErrPattern>(p).binding != "_";
    if (auto *ecp = std::get_if<std::unique_ptr<EnumConstructorPattern>>(&p)) {
        return std::any_of((*ecp)->bindings.begin(), (*ecp)->bindings.end(), patternHasBinding);
    }
    if (auto *tp = std::get_if<std::unique_ptr<TuplePattern>>(&p)) {
        return std::any_of((*tp)->elements.begin(), (*tp)->elements.end(), patternHasBinding);
    }
    if (auto *rp = std::get_if<std::unique_ptr<RecordPattern>>(&p)) {
        return std::any_of((*rp)->elements.begin(), (*rp)->elements.end(), patternHasBinding);
    }
    return false;
}

void Parser::validateForBindingPattern(const Pattern &p) {
    std::visit([&](const auto &pat) {
        using T = std::decay_t<decltype(pat)>;
        if constexpr (std::is_same_v<T, WildcardPattern>) {
            return;
        } else if constexpr (std::is_same_v<T, VariablePattern>) {
            if (!isSnakeCase(pat.name))
                parseError("loop variable name '" + pat.name + "' must be snake_case");
        } else if constexpr (std::is_same_v<T, std::unique_ptr<TuplePattern>>) {
            for (const auto &elem : pat->elements)
                validateForBindingPattern(elem);
        } else {
            parseError("for loop pattern only supports variables, '_', and tuple destructuring");
        }
    }, p);
}

void Parser::parseOrPattern(Pattern &pat) {
    if (lex_.peek().kind != TokenKind::Pipe) return;
    auto orPat = std::make_unique<OrPattern>();
    if (patternHasBinding(pat))
        parseError("OR pattern cannot contain variable bindings");
    orPat->alternatives.push_back(std::move(pat));
    while (lex_.peek().kind == TokenKind::Pipe) {
        lex_.next();
        Pattern alt = parsePattern();
        if (patternHasBinding(alt))
            parseError("OR pattern cannot contain variable bindings");
        orPat->alternatives.push_back(std::move(alt));
    }
    pat = std::move(orPat);
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
        return LiteralPattern{std::move(node)};
    }
    if (t.kind == TokenKind::Float) {
        lex_.next();
        auto [numStr, suffix] = splitNumericSuffix(t.value);
        auto node = std::make_unique<ExprNode>();
        node->data = FloatExpr{parseFloatLiteral(numStr), suffix};
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

    // Tuple/grouping pattern: '(' p ')' is grouping; '(' p ',' ... ')' is a TuplePattern.
    if (t.kind == TokenKind::LParen) {
        lex_.next();
        if (lex_.peek().kind == TokenKind::RParen)
            parseError(t.line, "zero-tuple pattern '()' is not supported");
        Pattern first = parsePattern();
        if (lex_.peek().kind != TokenKind::Comma) {
            if (lex_.peek().kind != TokenKind::RParen)
                parseError(lex_.peek().line, "expected ')' in grouped pattern");
            lex_.next();
            return first;
        }
        auto tup = std::make_unique<TuplePattern>();
        tup->elements.push_back(std::move(first));
        while (lex_.peek().kind == TokenKind::Comma) {
            lex_.next();
            if (lex_.peek().kind == TokenKind::RParen) break;
            tup->elements.push_back(parsePattern());
        }
        if (lex_.peek().kind != TokenKind::RParen)
            parseError(lex_.peek().line, "expected ')' in tuple pattern");
        lex_.next();
        return Pattern{std::move(tup)};
    }

    // Negative number literal: -N
    if (t.kind == TokenKind::Minus) {
        lex_.next(); // consume '-'
        Token num = lex_.peek();
        if (num.kind == TokenKind::Number) {
            auto [numStr, suffix] = splitNumericSuffix(num.value);
            auto node = std::make_unique<ExprNode>();
            if (suffix == "f32" || suffix == "f64") {
                lex_.next();
                node->data = FloatExpr{-parseFloatLiteral(numStr), suffix};
            } else {
                // Validate before consuming so parseError() points at the literal token.
                int64_t val;
                if (!tryParseIntLiteral(numStr, &val))
                    parseError("integer literal out of range for int: -" + numStr);
                if (!isSignedInt64Magnitude(static_cast<uint64_t>(val), /*negative=*/true))
                    parseError("integer literal out of range for int: -" + numStr);
                lex_.next();
                // Wrap the positive magnitude in UnaryExpr("-") instead of
                // pre-negating: NumberExpr.value must stay a non-negative
                // magnitude (see the struct comment in ast.hpp). Codegen
                // applies the negation through its unary-minus path.
                auto inner = std::make_unique<ExprNode>();
                inner->data = NumberExpr{val, suffix};
                auto unary = std::make_unique<UnaryExpr>();
                unary->op = "-";
                unary->operand = std::move(inner);
                node->data = std::move(unary);
            }
            return LiteralPattern{std::move(node)};
        }
        if (num.kind == TokenKind::Float) {
            lex_.next();
            auto [numStr, suffix] = splitNumericSuffix(num.value);
            auto node = std::make_unique<ExprNode>();
            node->data = FloatExpr{-parseFloatLiteral(numStr), suffix};
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
            // Check for constructor pattern: Enum::Variant(a, b, ...) or Enum::Variant((x, y), ...)
            if (lex_.peek().kind == TokenKind::LParen) {
                lex_.next(); // consume '('
                std::vector<Pattern> bindings;
                bindings.reserve(4);
                if (lex_.peek().kind != TokenKind::RParen) {
                    for (;;) {
                        bindings.push_back(parsePattern());
                        if (lex_.peek().kind != TokenKind::Comma)
                            break;
                        lex_.next(); // consume ','
                        if (lex_.peek().kind == TokenKind::RParen)
                            break; // trailing comma
                    }
                }
                if (lex_.peek().kind != TokenKind::RParen)
                    parseError("expected ')' in constructor pattern");
                lex_.next(); // consume ')'
                return std::make_unique<EnumConstructorPattern>(
                    EnumConstructorPattern{t.value, variant.value, std::move(bindings)});
            }
            return EnumPattern{t.value, variant.value};
        }
        // Positional record destructuring pattern: Point(a, b)
        if (lex_.peek().kind == TokenKind::LParen) {
            lex_.next(); // consume '('
            if (lex_.peek().kind == TokenKind::RParen)
                parseError("record pattern must have at least one field");
            std::vector<Pattern> elements;
            elements.reserve(4);
            for (;;) {
                elements.push_back(parsePattern());
                if (lex_.peek().kind != TokenKind::Comma) break;
                lex_.next(); // consume ','
                if (lex_.peek().kind == TokenKind::RParen) break; // trailing comma
            }
            if (lex_.peek().kind != TokenKind::RParen)
                parseError("expected ')' in record pattern");
            lex_.next(); // consume ')'
            return std::make_unique<RecordPattern>(RecordPattern{t.value, std::move(elements)});
        }
        return VariablePattern{t.value};
    }

    parseError(t.line, "expected pattern");
}

// Parses `case ...:` statements (issues #798/#799/#800).
//
// Two forms are recognized:
//   1. `case:`        — no subject, condition-based arms (replaces `when:`)
//   2. `case <expr>:` — with subject, pattern-based arms (replaces `match`)
//
// The leading `case` token has not yet been consumed when this is called.
StmtNode Parser::parseCaseStatement() {
    Token caseTok = lex_.next(); // consume 'case'

    // `case:` (no subject) — next token is the block-introducing colon.
    if (lex_.peek().kind == TokenKind::Colon)
        return parseCaseStatementNoSubject(caseTok);

    // `case <expr>:` (with subject) — the next token starts the subject expression.
    return parseCaseStatementWithSubject(caseTok);
}

// Parse the body of `case:` (no subject). Called with the leading `case` token
// already consumed and the upcoming token being the `:`.
StmtNode Parser::parseCaseStatementNoSubject(const Token &caseTok) {
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

    auto caseStmt = std::make_unique<CaseCondStmt>();
    caseStmt->loc = locFromToken(caseTok);
    caseStmt->arms.reserve(4);

    bool seenWildcard = false;
    while (lex_.peek().kind != TokenKind::Dedent &&
           lex_.peek().kind != TokenKind::Eof) {
        // The wildcard arm in the subject-less form is `_:`. We detect this
        // via the identifier `_` followed by `:`, so that expressions starting
        // with `_` (unlikely, but conceivable) don't mistakenly get hijacked.
        Token first = lex_.peek();
        bool isWildcard = (first.kind == TokenKind::Ident && first.value == "_");
        if (isWildcard) {
            // Peek ahead to see if the next non-`_` token is `:`.
            const auto &saved = first;
            lex_.next(); // consume '_'
            if (lex_.peek().kind != TokenKind::Colon) {
                parseError(saved.line, "'_' in case: block must be followed by ':' (wildcard arm)");
            }
            if (seenWildcard)
                parseError(saved.line, "duplicate '_' arm in case: block");
            lex_.next(); // consume ':'
            caseStmt->else_body = parseBlockOrInline();
            seenWildcard = true;
        } else {
            if (seenWildcard)
                parseError("condition arms must appear before '_:'");
            CaseCondArm arm;
            arm.condition = parseConditional();
            if (lex_.peek().kind != TokenKind::Colon)
                parseError("expected ':' after case condition");
            lex_.next();
            arm.body = parseBlockOrInline();
            caseStmt->arms.push_back(std::move(arm));
        }
        skipNewlines();
    }

    if (caseStmt->arms.empty() && !seenWildcard)
        parseError("case: block must have at least one arm");
    if (lex_.peek().kind == TokenKind::Dedent)
        lex_.next();
    return caseStmt;
}

// Parse the body of `case <expr>:` (with subject). Called with the leading
// `case` token already consumed and the upcoming tokens being the subject
// expression.
StmtNode Parser::parseCaseStatementWithSubject(const Token &caseTok) {
    ExprPtr subject = parseConditional();

    if (lex_.peek().kind != TokenKind::Colon)
        parseError("expected ':' after case subject");
    lex_.next(); // consume ':'

    if (lex_.peek().kind != TokenKind::Newline)
        parseError("expected newline after ':'");
    lex_.next(); // consume Newline
    skipNewlines();

    if (lex_.peek().kind != TokenKind::Indent)
        parseError("expected indented block");
    lex_.next(); // consume Indent

    auto caseStmt = std::make_unique<CaseStmt>();
    caseStmt->subject = std::move(subject);
    caseStmt->loc = locFromToken(caseTok);
    caseStmt->arms.reserve(4);

    while (lex_.peek().kind != TokenKind::Dedent &&
           lex_.peek().kind != TokenKind::Eof) {
        CaseArm arm;
        arm.pattern = parsePattern();
        parseOrPattern(arm.pattern);

        if (lex_.peek().kind == TokenKind::If) {
            lex_.next();
            arm.guard = parseConditional();
        }

        if (lex_.peek().kind != TokenKind::Colon)
            parseError("expected ':' after case pattern");
        lex_.next();

        arm.body = parseBlockOrInline();
        caseStmt->arms.push_back(std::move(arm));
        skipNewlines();
    }

    if (caseStmt->arms.empty())
        parseError("case block must have at least one arm");

    if (lex_.peek().kind == TokenKind::Dedent)
        lex_.next();

    return caseStmt;
}

} // namespace ry
