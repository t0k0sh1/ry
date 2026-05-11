#include "ry/formatter.hpp"
#include <functional>


namespace ry {

// --- Statement formatting methods ---

void Formatter::formatAssign(const AssignStmt &s) {
    formatDirectives(s.directives);
    if (!s.directives.empty()) emitIndent();

    emit(s.name);
    if (s.type_annotation) {
        emit(": " + s.type_annotation->toString());
    }

    // Propagate the annotation type onto a bare integer literal initializer
    // so a u64 max value like `h: u64 = 18446744073709551615` renders as the
    // unsigned form instead of `-1` (mirrors codegen's suffix injection).
    if (s.type_annotation && s.value)
        injectLowLevelSuffix(*s.value, s.type_annotation->toString());
    // @native @const declarations have no value
    if (!s.value) {
        emitInlineComment(s.loc.line);
        emitNewline();
        last_emitted_line_ = s.loc.line;
        return;
    }

    if (s.compound_op) {
        std::string op = " ";
        op += *s.compound_op;
        op += "= ";
        emit(op);
    } else {
        emit(" = ");
    }

    // Check if value is a multi-line lambda
    if (auto *lambda_ptr = std::get_if<std::unique_ptr<LambdaExpr>>(&s.value->data)) {
        const auto &lambda = **lambda_ptr;
        if (!lambda.expr_body && !lambda.body.empty()) {
            emit(formatLambdaSig(lambda));
            emit(":");
            emitInlineComment(s.loc.line);
            emitNewline();
            last_emitted_line_ = s.loc.line;
            formatBlock(lambda.body);
            return;
        }
    }

    std::string val = formatExpr(*s.value);
    emit(val);
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
}

void Formatter::formatCall(const CallStmt &s) {
    formatDirectives(s.directives);
    if (!s.directives.empty()) emitIndent();

    emit(s.callee + "(");

    // Check for trailing lambda pattern
    bool has_trailing_lambda = false;
    size_t lambda_idx = 0;
    if (!s.args.empty()) {
        auto *lambda_ptr = std::get_if<std::unique_ptr<LambdaExpr>>(&s.args.back()->data);
        if (lambda_ptr && !(*lambda_ptr)->expr_body && !(*lambda_ptr)->body.empty()) {
            has_trailing_lambda = true;
            lambda_idx = s.args.size() - 1;
        }
    }

    for (size_t i = 0; i < s.args.size(); ++i) {
        if (i > 0) emit(", ");

        if (has_trailing_lambda && i == lambda_idx) {
            const auto &lambda = *std::get<std::unique_ptr<LambdaExpr>>(s.args[i]->data);
            emit(formatLambdaSig(lambda));
            emit(":");
            emitInlineComment(s.loc.line);
            emitNewline();
            last_emitted_line_ = s.loc.line;
            formatBlock(lambda.body);
            emitIndent();
            emit(formatNamedArgs(s.named_args, true));
            emit(")");
            emitNewline();
            return;
        } else {
            // Check for inline lambda with multi-line body as non-last arg
            auto *lp = std::get_if<std::unique_ptr<LambdaExpr>>(&s.args[i]->data);
            if (lp && !(*lp)->expr_body && !(*lp)->body.empty()) {
                const auto &lambda = **lp;
                emit(formatLambdaSig(lambda));
                emit(":");
                emitNewline();
                last_emitted_line_ = s.loc.line;
                formatBlock(lambda.body);
                // Continue with next args on same indentation
                continue;
            }
            emit(formatExpr(*s.args[i]));
        }
    }

    emit(formatNamedArgs(s.named_args, !s.args.empty()));
    emit(")");
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
}

void Formatter::formatExprStmt(const ExprStmt &s) {
    emit(formatExpr(*s.expr));
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
}

void Formatter::formatReturn(const ReturnStmt &s) {
    emit("return");
    if (s.value) {
        emit(" " + formatExpr(*s.value));
    }
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
}

void Formatter::formatImport(const ImportStmt &s) {
    // Convert internal path back to dot notation.
    // Relative paths: "." stays ".", "./utils/calc" becomes ".utils.calc"
    // Absolute paths: "std/io" becomes "std.io"
    std::string path = s.module_path;
    if (path.size() >= 2 && path[0] == '.' && path[1] == '/') {
        // Relative: strip "./" prefix, convert '/' to '.', prepend '.'
        std::string rest = path.substr(2);
        for (auto &c : rest) {
            if (c == '/') c = '.';
        }
        path = "." + rest;
    } else if (path != ".") {
        for (auto &c : path) {
            if (c == '/') c = '.';
        }
    }
    emit("from " + path);
    if (!s.names.empty()) {
        emit(" import ");
        for (size_t i = 0; i < s.names.size(); ++i) {
            if (i > 0) emit(", ");
            emit(s.names[i].name);
            if (s.names[i].alias.has_value()) {
                emit(" as ");
                emit(*s.names[i].alias);
            }
        }
    }
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
}

void Formatter::formatQualifiedImport(const QualifiedImportStmt &s) {
    emit("import ");
    emit(s.module_name);
    if (s.alias.has_value()) {
        emit(" as ");
        emit(*s.alias);
    }
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
}

void Formatter::formatRecord(const RecordStmt &s) {
    formatDirectives(s.directives);
    if (!s.directives.empty()) emitIndent();

    emit("record " + s.name);
    if (s.parent_name)
        emit(" < " + *s.parent_name);
    emit(":");
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
    indent();
    for (const auto &field : s.fields) {
        formatDirectives(field.directives);
        emitIndent();
        emit(field.name + ": " + field.type->toString());
        emitNewline();
    }
    if (!s.invariants.empty()) {
        emitIndent();
        emit("invariant:");
        emitNewline();
        indent();
        for (const auto &inv : s.invariants) {
            emitIndent();
            emit(formatExpr(*inv));
            emitNewline();
        }
        dedent();
    }
    dedent();
}

void Formatter::emitTypeParams(const std::vector<TypeParam> &params) {
    if (params.empty()) return;
    emit("<");
    for (size_t i = 0; i < params.size(); ++i) {
        if (i > 0) emit(", ");
        emit(params[i].name);
        if (params[i].bound)
            emit(": " + *params[i].bound);
    }
    emit(">");
}

void Formatter::formatEnum(const EnumStmt &s) {
    formatDirectives(s.directives);
    if (!s.directives.empty()) emitIndent();
    emit("enum " + s.name);
    emitTypeParams(s.type_params);
    emit(":");
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
    indent();
    for (const auto &variant : s.variants) {
        emitIndent();
        emit(variant.name);
        if (!variant.field_types.empty()) {
            emit("(");
            for (size_t i = 0; i < variant.field_types.size(); ++i) {
                if (i > 0) emit(", ");
                emit(variant.field_types[i]->toString());
            }
            emit(")");
        }
        emitNewline();
    }
    dedent();
}

void Formatter::formatTypeAlias(const TypeAliasStmt &s) {
    formatDirectives(s.directives);
    if (!s.directives.empty()) emitIndent();
    emit("type " + s.name + " = " + s.target_type->toString());
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
}

void Formatter::formatIf(const IfStmt &s) {
    emit("if " + formatExpr(*s.branch.condition) + ":");
    emitNewline();
    formatBlock(s.branch.body);
    if (!s.else_body.empty()) {
        emitIndent();
        emit("else:");
        emitNewline();
        formatBlock(s.else_body);
    }
    last_emitted_line_ = s.loc.line;
}

void Formatter::formatCaseCond(const CaseCondStmt &s) {
    emit("case:");
    emitNewline();
    last_emitted_line_ = s.loc.line;
    indent();
    for (const auto &arm : s.arms) {
        emitIndent();
        emit(formatExpr(*arm.condition) + ":");
        emitNewline();
        formatBlock(arm.body);
    }
    if (!s.else_body.empty()) {
        emitIndent();
        emit("_:");
        emitNewline();
        formatBlock(s.else_body);
    }
    dedent();
}

void Formatter::formatWhile(const WhileStmt &s) {
    emit("while " + formatExpr(*s.condition) + ":");
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
    formatBlock(s.body);
}

void Formatter::formatFor(const ForStmt &s) {
    formatDirectives(s.directives);
    if (!s.directives.empty()) emitIndent();

    std::function<std::string(const Pattern &, bool)> formatForBinding =
        [&](const Pattern &pat, bool topLevel) -> std::string {
            if (auto *tp = std::get_if<std::unique_ptr<TuplePattern>>(&pat)) {
                std::string result;
                const bool wrap = !topLevel || (*tp)->elements.size() == 1;
                if (wrap)
                    result += "(";
                for (size_t i = 0; i < (*tp)->elements.size(); ++i) {
                    if (i > 0)
                        result += ", ";
                    result += formatForBinding((*tp)->elements[i], false);
                }
                if ((*tp)->elements.size() == 1)
                    result += ",";
                if (wrap)
                    result += ")";
                return result;
            }
            return formatPattern(pat);
        };

    emit("for " + formatForBinding(s.binding, true));
    emit(" in " + formatExpr(*s.iterable) + ":");
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
    formatBlock(s.body);
}

void Formatter::formatFn(const FnStmt &s) {
    formatDirectives(s.directives);
    if (!s.directives.empty()) emitIndent();

    if (s.is_async) emit("async ");
    emit("fn " + s.name);
    emitTypeParams(s.type_params);
    emit("(" + formatParams(s.params) + ")");
    if (s.return_type) {
        emit(" -> " + s.return_type->toString());
    }

    // @native fn (no body): no colon, no block
    if (s.body.empty() && s.preconditions.empty() && s.postconditions.empty()) {
        emitInlineComment(s.loc.line);
        emitNewline();
        last_emitted_line_ = s.loc.line;
        return;
    }

    emit(":");
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;

    indent();
    // Preconditions
    if (!s.preconditions.empty()) {
        emitIndent();
        emit("require:");
        emitNewline();
        indent();
        for (const auto &pre : s.preconditions) {
            emitIndent();
            emit(formatExpr(*pre));
            emitNewline();
        }
        dedent();
    }
    // Postconditions
    if (!s.postconditions.empty()) {
        emitIndent();
        emit("ensure");
        if (!s.ensure_bindings.empty()) {
            emit(" " + s.ensure_bindings[0]);
            for (size_t i = 1; i < s.ensure_bindings.size(); ++i) {
                emit(", " + s.ensure_bindings[i]);
            }
        }
        emit(":");
        emitNewline();
        indent();
        for (const auto &post : s.postconditions) {
            emitIndent();
            emit(formatExpr(*post));
            emitNewline();
        }
        dedent();
    }
    dedent();

    formatBlock(s.body);
}

void Formatter::formatCase(const CaseStmt &s) {
    emit("case " + formatExpr(*s.subject) + ":");
    emitNewline();
    last_emitted_line_ = s.loc.line;
    indent();
    for (const auto &arm : s.arms) {
        emitIndent();
        emit(formatPattern(arm.pattern));
        if (arm.guard) {
            emit(" if " + formatExpr(*arm.guard));
        }
        emit(":");
        emitNewline();
        formatBlock(arm.body);
    }
    dedent();
}

void Formatter::formatIndexAssign(const IndexAssignStmt &s) {
    std::string idxStr;
    for (size_t i = 0; i < s.indices.size(); ++i) {
        if (i > 0) idxStr += ", ";
        idxStr += formatExpr(*s.indices[i]);
    }
    std::string op = s.compound_op ? (" " + *s.compound_op + "= ") : " = ";
    emit(formatExpr(*s.object) + "[" + idxStr + "]" + op + formatExpr(*s.value));
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
}

void Formatter::formatFieldAssign(const FieldAssignStmt &s) {
    std::string op = s.compound_op ? (" " + *s.compound_op + "= ") : " = ";
    emit(formatExpr(*s.object) + "." + s.field + op + formatExpr(*s.value));
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
}

void Formatter::formatBreak(const BreakStmt &s) {
    emit("break");
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
}

void Formatter::formatContinue(const ContinueStmt &s) {
    emit("continue");
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
}

void Formatter::formatEllipsis(const EllipsisStmt &s) {
    emit("...");
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
}

void Formatter::formatExpect(const ExpectStmt &s) {
    emit("expect(" + formatExpr(*s.actual) + ")." + s.matcher);
    if (s.expected) {
        emit("(" + formatExpr(*s.expected) + ")");
    } else {
        emit("()");
    }
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
}

void Formatter::formatAwaitStmt(const AwaitStmt &s) {
    std::string awaitStr = "await ";
    awaitStr += formatExpr(*s.operand);
    emit(awaitStr);
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
}

void Formatter::formatTupleDestruct(const TupleDestructStmt &s) {
    formatDirectives(s.directives);
    if (!s.directives.empty()) emitIndent();

    emit("(");
    for (size_t i = 0; i < s.names.size(); ++i) {
        if (i > 0) emit(", ");
        emit(s.names[i]);
    }
    emit(")");
    emit(" = " + formatExpr(*s.value));
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
}

void Formatter::formatDirectiveDef(const DirectiveDefStmt &s) {
    formatDirectives(s.directives);
    if (!s.directives.empty())
        emitIndent();
    emit("@directive(target=[");
    for (size_t i = 0; i < s.targets.size(); ++i) {
        if (i > 0) emit(", ");
        emit("\"" + s.targets[i] + "\"");
    }
    emit("])");
    emitNewline();
    emitIndent();
    emit("fn " + s.name + "(" + formatParams(s.params) + ")");
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
}

} // namespace ry
