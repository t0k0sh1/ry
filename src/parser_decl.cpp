#include "ry/parser.hpp"
#include "ry/diagnostic.hpp"
#include <regex>
#include <stdexcept>
#include <string>
#include <unordered_set>

static bool isSnakeCase(const std::string &name) {
    if (name.empty()) return false;
    if (name == "_") return true;
    static const std::regex pattern("[a-z_][a-z0-9_]*");
    return std::regex_match(name, pattern);
}

// snake_case with optional trailing '!' for mutating function names (sort!, reverse!)
static bool isMutationFnName(const std::string &name) {
    if (name.empty()) return false;
    static const std::regex pattern("[a-z_][a-z0-9_]*!?");
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
            // Compound assignment operators
            case TokenKind::PlusEq: case TokenKind::MinusEq:
            case TokenKind::StarEq: case TokenKind::SlashEq:
            case TokenKind::PercentEq: case TokenKind::SlashSlashEq:
            case TokenKind::StarStarEq:
            case TokenKind::AmpEq: case TokenKind::PipeEq:
            case TokenKind::CaretEq:
            case TokenKind::LessLessEq: case TokenKind::GreaterGreaterEq:
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
        bool validName = isMutationFnName(nameTok.value) ||
                         (hasDirective(directives, "native") && isScreamingSnakeCase(nameTok.value));
        if (!validName)
            parseError(nameTok.line, "function name '" + nameTok.value + "' must be snake_case (or SCREAMING_SNAKE_CASE for @native functions)");
        lex_.next(); // consume name
        fnStmt->name = nameTok.value;

        // Parse optional type parameters: fn name<T, U>(...)
        if (lex_.peek().kind == TokenKind::Less) {
            lex_.next(); // consume '<'
            for (;;) {
                Token tp = lex_.peek();
                if (tp.kind != TokenKind::Ident)
                    parseError(tp.line, "expected type parameter name");
                lex_.next();
                fnStmt->type_params.push_back(tp.value);
                if (lex_.peek().kind != TokenKind::Comma)
                    break;
                lex_.next(); // consume ','
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

            std::string paramType = "any";  // default when type is omitted
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
                default_value = parseTernary();
                seen_default = true;
            } else if (seen_default) {
                parseError(paramName.line,
                    "parameter '" + paramName.value + "' must have a default value "
                    "(all parameters after a default parameter must also have defaults)");
            }

            fnStmt->params.push_back({paramName.value, paramType, std::move(default_value)});

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
        fnStmt->return_type = "";
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
        } else if (nParams != 1 && nParams != 2) {
            parseError("operator function requires 1 or 2 parameters");
        }
    }

    // Validate return type for comparison/logical operators
    if (fnStmt->is_operator && !fnStmt->return_type.empty()) {
        if (isBoolConstrainedOperator(fnStmt->name) && fnStmt->return_type != "bool") {
            parseError(fnTok.line,
                "operator '" + operatorSymbol(fnStmt->name) + "' must return 'bool', but returns '" +
                fnStmt->return_type + "'");
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
            int64_t val = std::stoll(valTok.value);
            if (negative) val = -val;
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

std::string Parser::parseTypeName() {
    std::string name = parseTypeNameSingle();
    while (lex_.peek().kind == TokenKind::Pipe) {
        lex_.next(); // consume '|'
        name += " | " + parseTypeNameSingle();
    }
    return name;
}

std::string Parser::parseTypeNameSingle() {
    // Fixed-length array type: [T; N]
    if (lex_.peek().kind == TokenKind::LBracket) {
        lex_.next(); // consume '['
        std::string elemType = parseTypeNameSingle();
        if (lex_.peek().kind != TokenKind::Semi)
            parseError("expected ';' in array type [T; N]");
        lex_.next(); // consume ';'
        if (lex_.peek().kind != TokenKind::Number)
            parseError("expected integer size in array type [T; N]");
        std::string size = lex_.peek().value;
        lex_.next(); // consume number
        if (lex_.peek().kind != TokenKind::RBracket)
            parseError("expected ']' in array type");
        lex_.next(); // consume ']'
        return "[" + elemType + "; " + size + "]";
    }

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
            if (!lex_.consumeGreaterInTypeContext())
                parseError("expected '>' in " + name + " type");
            name += "<" + inner + ", " + secondTy + ">";
        } else {
            if (!lex_.consumeGreaterInTypeContext())
                parseError("expected '>' after generic type parameter");
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
        auto [numStr, suffix] = splitNumericSuffix(t.value);
        auto node = std::make_unique<ExprNode>();
        if (suffix == "f32" || suffix == "f64")
            node->data = FloatExpr{std::stod(numStr), suffix};
        else
            node->data = NumberExpr{parseIntLiteral(numStr), suffix};
        return LiteralPattern{std::move(node)};
    }
    if (t.kind == TokenKind::Float) {
        lex_.next();
        auto [numStr, suffix] = splitNumericSuffix(t.value);
        auto node = std::make_unique<ExprNode>();
        node->data = FloatExpr{std::stod(numStr), suffix};
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
            auto [numStr, suffix] = splitNumericSuffix(num.value);
            auto node = std::make_unique<ExprNode>();
            if (suffix == "f32" || suffix == "f64")
                node->data = FloatExpr{-std::stod(numStr), suffix};
            else
                node->data = NumberExpr{-parseIntLiteral(numStr), suffix};
            return LiteralPattern{std::move(node)};
        }
        if (num.kind == TokenKind::Float) {
            lex_.next();
            auto [numStr, suffix] = splitNumericSuffix(num.value);
            auto node = std::make_unique<ExprNode>();
            node->data = FloatExpr{-std::stod(numStr), suffix};
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
            auto hasBinding = [](const Pattern &p) {
                if (std::holds_alternative<VariablePattern>(p))
                    return true;
                if (std::holds_alternative<SomePattern>(p))
                    return std::get<SomePattern>(p).binding != "_";
                if (std::holds_alternative<OkPattern>(p))
                    return std::get<OkPattern>(p).binding != "_";
                if (std::holds_alternative<ErrPattern>(p))
                    return std::get<ErrPattern>(p).binding != "_";
                if (std::holds_alternative<EnumConstructorPattern>(p)) {
                    const auto &ec = std::get<EnumConstructorPattern>(p);
                    return std::any_of(ec.bindings.begin(), ec.bindings.end(),
                                       [](const std::string &b) { return b != "_"; });
                }
                return false;
            };
            auto orPat = std::make_unique<OrPattern>();
            if (hasBinding(arm.pattern))
                parseError("OR pattern cannot contain variable bindings");
            orPat->alternatives.push_back(std::move(arm.pattern));
            while (lex_.peek().kind == TokenKind::Pipe) {
                lex_.next(); // consume '|'
                Pattern alt = parsePattern();
                if (hasBinding(alt))
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

