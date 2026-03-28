#include "ry/formatter.hpp"

// --- Statement formatting methods ---

void Formatter::formatAssign(const AssignStmt &s) {
    formatDirectives(s.directives);
    if (!s.directives.empty()) emitIndent();

    emit(s.name);
    if (s.type_annotation) {
        emit(": " + s.type_annotation->toString());
    }
    // @native @const declarations have no value
    if (!s.value) {
        emitInlineComment(s.loc.line);
        emitNewline();
        last_emitted_line_ = s.loc.line;
        return;
    }

    if (s.compound_op) {
        emit(" " + *s.compound_op + "= ");
    } else {
        emit(" = ");
    }

    // Check if value is a multi-line lambda
    if (auto *lambda_ptr = std::get_if<std::unique_ptr<LambdaExpr>>(&s.value->data)) {
        const auto &lambda = **lambda_ptr;
        if (!lambda.expr_body && !lambda.body.empty()) {
            emit("fn(" + formatParams(lambda.params) + ")");
            if (lambda.return_type) emit(" -> " + lambda.return_type->toString());
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
            emit("fn(" + formatParams(lambda.params) + ")");
            if (lambda.return_type) emit(" -> " + lambda.return_type->toString());
            emit(":");
            emitInlineComment(s.loc.line);
            emitNewline();
            last_emitted_line_ = s.loc.line;
            formatBlock(lambda.body);
            emitIndent();
            emit(")");
            emitNewline();
            return;
        } else {
            // Check for inline lambda with multi-line body as non-last arg
            auto *lp = std::get_if<std::unique_ptr<LambdaExpr>>(&s.args[i]->data);
            if (lp && !(*lp)->expr_body && !(*lp)->body.empty()) {
                const auto &lambda = **lp;
                emit("fn(" + formatParams(lambda.params) + ")");
                if (lambda.return_type) emit(" -> " + lambda.return_type->toString());
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

    emit(")");
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
    // Parser converts "std.io" to "std/io", convert back to dot notation
    std::string path = s.module_path;
    for (auto &c : path) {
        if (c == '/') c = '.';
    }
    emit("from " + path + " import ");
    for (size_t i = 0; i < s.names.size(); ++i) {
        if (i > 0) emit(", ");
        emit(s.names[i]);
    }
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
}

void Formatter::formatRecord(const RecordStmt &s) {
    formatDirectives(s.directives);
    if (!s.directives.empty()) emitIndent();

    emit("record " + s.name + ":");
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

void Formatter::formatEnum(const EnumStmt &s) {
    emit("enum " + s.name);
    if (!s.type_params.empty()) {
        emit("<");
        for (size_t i = 0; i < s.type_params.size(); ++i) {
            if (i > 0) emit(", ");
            emit(s.type_params[i]);
        }
        emit(">");
    }
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
    emit("type " + s.name + " = " + s.target_type->toString());
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
}

void Formatter::formatIf(const IfStmt &s) {
    for (size_t i = 0; i < s.branches.size(); ++i) {
        if (i == 0) {
            emit("if ");
        } else {
            emitIndent();
            emit("elif ");
        }
        emit(formatExpr(*s.branches[i].condition) + ":");
        emitNewline();
        formatBlock(s.branches[i].body);
    }
    if (!s.else_body.empty()) {
        emitIndent();
        emit("else:");
        emitNewline();
        formatBlock(s.else_body);
    }
    last_emitted_line_ = s.loc.line;
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

    emit("for " + s.var_names[0]);
    for (size_t i = 1; i < s.var_names.size(); ++i)
        emit(", " + s.var_names[i]);
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
    if (!s.type_params.empty()) {
        emit("<");
        for (size_t i = 0; i < s.type_params.size(); ++i) {
            if (i > 0) emit(", ");
            emit(s.type_params[i]);
        }
        emit(">");
    }
    emit("(" + formatParams(s.params) + ")");
    if (s.return_type) {
        emit(" -> " + s.return_type->toString());
    }

    // @native functions without body: no colon, no block
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

void Formatter::formatMatch(const MatchStmt &s) {
    emit("match " + formatExpr(*s.subject) + ":");
    emitNewline();
    last_emitted_line_ = s.loc.line;
    indent();
    for (const auto &arm : s.arms) {
        emitIndent();
        emit("case " + formatPattern(arm.pattern));
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
    emit(formatExpr(*s.object) + "[" + idxStr + "] = " + formatExpr(*s.value));
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
}

void Formatter::formatFieldAssign(const FieldAssignStmt &s) {
    emit(formatExpr(*s.object) + "." + s.field + " = " + formatExpr(*s.value));
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
    emit("await " + formatExpr(*s.operand));
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
    if (s.is_immutable) emit(": ");
    emit(" = " + formatExpr(*s.value));
    emitInlineComment(s.loc.line);
    emitNewline();
    last_emitted_line_ = s.loc.line;
}
