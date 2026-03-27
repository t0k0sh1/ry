#include "ry/formatter.hpp"
#include "ry/lexer.hpp"
#include "ry/parser.hpp"
#include "ry/project_config.hpp"

#include <algorithm>
#include <cmath>
#include <charconv>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>

namespace fs = std::filesystem;

// ===== Formatter =====

Formatter::Formatter(const std::string &source, int indent_width)
    : source_(source), indent_width_(indent_width) {
    comments_ = extractComments(source);
}

// --- Comment extraction ---

std::vector<Comment> Formatter::extractComments(const std::string &source) {
    std::vector<Comment> result;
    int line = 1;
    size_t i = 0;
    while (i < source.size()) {
        size_t line_start = i;

        // Find end of line
        size_t line_end = source.find('\n', i);
        if (line_end == std::string::npos) line_end = source.size();

        std::string_view line_str(source.data() + line_start, line_end - line_start);

        // Scan for '#' outside string literals
        bool in_string = false;
        char quote_char = 0;
        bool has_code = false;
        for (size_t j = 0; j < line_str.size(); ++j) {
            char c = line_str[j];
            if (in_string) {
                if (c == '\\' && j + 1 < line_str.size()) {
                    ++j; // skip escaped char
                } else if (c == quote_char) {
                    in_string = false;
                }
            } else {
                if (c == '"') {
                    in_string = true;
                    quote_char = c;
                    has_code = true;
                } else if (c == '#') {
                    int col = static_cast<int>(j) + 1;
                    std::string text(line_str.substr(j));
                    // Trim trailing whitespace
                    while (!text.empty() && (text.back() == '\r' || text.back() == ' ' || text.back() == '\t'))
                        text.pop_back();
                    result.push_back({line, col, std::move(text), has_code});
                    break;
                } else if (c != ' ' && c != '\t') {
                    has_code = true;
                }
            }
        }

        i = line_end + 1;
        ++line;
    }
    return result;
}

// --- Output helpers ---

void Formatter::emit(const std::string &s) { out_ += s; }
void Formatter::emitIndent() {
    out_.append(indent_level_ * indent_width_, ' ');
}
void Formatter::emitNewline() { out_ += '\n'; }
void Formatter::indent() { ++indent_level_; }
void Formatter::dedent() { --indent_level_; }
bool Formatter::endsWithBlankLine() const {
    return out_.size() >= 2 && out_[out_.size() - 1] == '\n' && out_[out_.size() - 2] == '\n';
}

// --- Comment emission ---

void Formatter::emitCommentsBefore(int line) {
    while (next_comment_idx_ < comments_.size()) {
        const auto &c = comments_[next_comment_idx_];
        if (c.line >= line) break;
        if (c.is_inline) { ++next_comment_idx_; continue; }

        // Preserve blank line before comment if source had one and output doesn't already
        if (last_emitted_line_ > 0 && c.line > last_emitted_line_ + 1 && !endsWithBlankLine()) {
            emitNewline();
        }

        emitIndent();
        emit(c.text);
        emitNewline();
        last_emitted_line_ = c.line;
        ++next_comment_idx_;
    }
}

void Formatter::emitInlineComment(int line) {
    for (size_t i = next_comment_idx_; i < comments_.size(); ++i) {
        if (comments_[i].line == line && comments_[i].is_inline) {
            emit("  ");
            emit(comments_[i].text);
            // Mark as consumed by moving next_comment_idx_ past it if sequential
            if (i == next_comment_idx_) ++next_comment_idx_;
            return;
        }
        if (comments_[i].line > line) break;
    }
}

void Formatter::emitTrailingComments() {
    while (next_comment_idx_ < comments_.size()) {
        const auto &c = comments_[next_comment_idx_];
        if (c.is_inline) { ++next_comment_idx_; continue; }
        if (last_emitted_line_ > 0 && c.line > last_emitted_line_ + 1) {
            emitNewline();
        }
        emitIndent();
        emit(c.text);
        emitNewline();
        last_emitted_line_ = c.line;
        ++next_comment_idx_;
    }
}

// --- String helpers ---

std::string Formatter::escapeString(const std::string &s) {
    std::string result;
    for (char c : s) {
        switch (c) {
            case '\n': result += "\\n"; break;
            case '\r': result += "\\r"; break;
            case '\t': result += "\\t"; break;
            case '\\': result += "\\\\"; break;
            case '"':  result += "\\\""; break;
            case '\0': result += "\\0"; break;
            default:   result += c; break;
        }
    }
    return result;
}

std::string Formatter::formatFloat(double v) {
    // Use std::to_chars with fixed format for round-trip safe output
    // without scientific notation
    char buf[64];
    auto [ptr, ec] = std::to_chars(buf, buf + sizeof(buf), v, std::chars_format::fixed);
    std::string s(buf, ptr);
    // Ensure at least one decimal digit
    if (s.find('.') == std::string::npos) {
        s += ".0";
    }
    return s;
}

// --- Operator precedence ---

int Formatter::operatorPrecedence(const std::string &op) const {
    if (op == "or")  return 1;
    if (op == "and") return 2;
    if (op == "not") return 3;
    if (op == "in" || op == "not in") return 4;
    if (op == "==" || op == "!=" || op == "<" || op == "<=" || op == ">" || op == ">=") return 5;
    if (op == "|")   return 6;
    if (op == "^")   return 7;
    if (op == "&")   return 8;
    if (op == "<<" || op == ">>" || op == ">>>") return 9;
    if (op == "+" || op == "-")  return 10;
    if (op == "*" || op == "/" || op == "//" || op == "%") return 11;
    if (op == "**")  return 12;
    if (op == "??")  return 0;
    return 20; // unknown - highest precedence, no parens
}

bool Formatter::needsParens(const ExprNode &child, const std::string &parent_op, bool is_right) const {
    if (auto *bin = std::get_if<std::unique_ptr<BinaryExpr>>(&child.data)) {
        int child_prec = operatorPrecedence((*bin)->op);
        int parent_prec = operatorPrecedence(parent_op);
        if (child_prec < parent_prec) return true;
        if (child_prec == parent_prec && is_right && parent_op != "**") return true;
    }
    if (auto *tern = std::get_if<std::unique_ptr<TernaryExpr>>(&child.data)) {
        return true; // always parenthesize ternary inside binary
    }
    if (std::get_if<std::unique_ptr<AwaitExpr>>(&child.data)) {
        return true; // parenthesize await inside binary
    }
    return false;
}

// --- Expression formatting ---

std::string Formatter::formatExpr(const ExprNode &expr) {
    return formatExprInner(expr);
}

std::string Formatter::formatExprInner(const ExprNode &expr) {
    return std::visit([this](const auto &v) -> std::string {
        using T = std::decay_t<decltype(v)>;

        if constexpr (std::is_same_v<T, NumberExpr>) {
            return std::to_string(v.value) + v.suffix;
        } else if constexpr (std::is_same_v<T, FloatExpr>) {
            return formatFloat(v.value) + v.suffix;
        } else if constexpr (std::is_same_v<T, BoolExpr>) {
            return v.value ? "true" : "false";
        } else if constexpr (std::is_same_v<T, StringExpr>) {
            return "\"" + escapeString(v.value) + "\"";
        } else if constexpr (std::is_same_v<T, VariableExpr>) {
            return v.name;
        } else if constexpr (std::is_same_v<T, NoneExpr>) {
            return "none";
        } else if constexpr (std::is_same_v<T, EnumAccessExpr>) {
            return v.enum_name + "::" + v.variant_name;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<BinaryExpr>>) {
            const auto &bin = *v;
            std::string lhs = formatExpr(*bin.lhs);
            std::string rhs = formatExpr(*bin.rhs);
            if (needsParens(*bin.lhs, bin.op, false)) lhs = "(" + lhs + ")";
            if (needsParens(*bin.rhs, bin.op, true))  rhs = "(" + rhs + ")";
            return lhs + " " + bin.op + " " + rhs;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<UnaryExpr>>) {
            const auto &u = *v;
            std::string operand = formatExpr(*u.operand);
            if (u.op == "not") return "not " + operand;
            if (u.op == "-" || u.op == "~") return u.op + operand;
            if (u.op == "++" || u.op == "--") return operand + u.op;
            return u.op + operand;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<CallExpr>>) {
            const auto &call = *v;
            std::string args;
            for (size_t i = 0; i < call.args.size(); ++i) {
                if (i > 0) args += ", ";
                args += formatExpr(*call.args[i]);
            }
            // Restore bracket syntax: parser converts name[T]() to name<T>()
            // but generic enum constructors use <> with :: (e.g. Option<int>::Some)
            std::string callee = call.callee;
            auto lt = callee.find('<');
            if (lt != std::string::npos && callee.back() == '>'
                && callee.find("::") == std::string::npos) {
                callee[lt] = '[';
                callee.back() = ']';
            }
            return callee + "(" + args + ")";
        } else if constexpr (std::is_same_v<T, std::unique_ptr<FieldAccessExpr>>) {
            return formatExpr(*v->object) + "." + v->field;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<TupleExpr>>) {
            std::string elems;
            for (size_t i = 0; i < v->elements.size(); ++i) {
                if (i > 0) elems += ", ";
                elems += formatExpr(*v->elements[i]);
            }
            return "(" + elems + ")";
        } else if constexpr (std::is_same_v<T, std::unique_ptr<ListExpr>>) {
            std::string elems;
            for (size_t i = 0; i < v->elements.size(); ++i) {
                if (i > 0) elems += ", ";
                elems += formatExpr(*v->elements[i]);
            }
            return "[" + elems + "]";
        } else if constexpr (std::is_same_v<T, std::unique_ptr<IndexExpr>>) {
            return formatExpr(*v->object) + "[" + formatExpr(*v->index) + "]";
        } else if constexpr (std::is_same_v<T, std::unique_ptr<MapExpr>>) {
            std::string pairs;
            for (size_t i = 0; i < v->keys.size(); ++i) {
                if (i > 0) pairs += ", ";
                pairs += formatExpr(*v->keys[i]) + ": " + formatExpr(*v->values[i]);
            }
            return "{" + pairs + "}";
        } else if constexpr (std::is_same_v<T, std::unique_ptr<SetExpr>>) {
            std::string elems;
            for (size_t i = 0; i < v->elements.size(); ++i) {
                if (i > 0) elems += ", ";
                elems += formatExpr(*v->elements[i]);
            }
            return "{" + elems + "}";
        } else if constexpr (std::is_same_v<T, std::unique_ptr<CastExpr>>) {
            return formatExpr(*v->value) + " as " + v->target_type;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<TernaryExpr>>) {
            return formatExpr(*v->condition) + " ? " + formatExpr(*v->true_expr) + " : " + formatExpr(*v->false_expr);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<RangeExpr>>) {
            return formatExpr(*v->start) + ".." + formatExpr(*v->end);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<ErrorPropagateExpr>>) {
            return formatExpr(*v->operand) + "?";
        } else if constexpr (std::is_same_v<T, std::unique_ptr<AwaitExpr>>) {
            return "await " + formatExpr(*v->operand);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<InterpolatedStringExpr>>) {
            std::string result = "f\"";
            for (size_t i = 0; i < v->parts.size(); ++i) {
                // Escape the literal part (but not { and })
                for (char c : v->parts[i]) {
                    switch (c) {
                        case '\n': result += "\\n"; break;
                        case '\r': result += "\\r"; break;
                        case '\t': result += "\\t"; break;
                        case '\\': result += "\\\\"; break;
                        case '"':  result += "\\\""; break;
                        default:   result += c; break;
                    }
                }
                if (i < v->exprs.size()) {
                    result += "{" + formatExpr(*v->exprs[i]) + "}";
                }
            }
            result += "\"";
            return result;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<LambdaExpr>>) {
            std::string result = "fn(" + formatParams(v->params) + ")";
            if (!v->return_type.empty()) result += " -> " + v->return_type;
            if (v->expr_body) {
                result += ": " + formatExpr(*v->expr_body);
                return result;
            } else if (!v->body.empty()) {
                result += ":\n";
                int saved = indent_level_;
                indent();
                for (size_t i = 0; i < v->body.size(); ++i) {
                    emitCommentsBefore(getStmtLine(v->body[i]));
                    if (!hasDirectives(v->body[i])) {
                        emitIndent();
                    }
                    formatStmt(v->body[i]);
                }
                indent_level_ = saved;
            }
            return "";  // multi-line lambda body already emitted to out_
        } else {
            return "/* unknown expr */";
        }
    }, expr.data);
}

// --- Parameter formatting ---

std::string Formatter::formatParams(const std::vector<FnParam> &params) {
    std::string result;
    for (size_t i = 0; i < params.size(); ++i) {
        if (i > 0) result += ", ";
        result += params[i].name + ": " + params[i].type;
    }
    return result;
}

// --- Pattern formatting ---

std::string Formatter::formatPattern(const Pattern &pat) {
    return std::visit([this](const auto &v) -> std::string {
        using T = std::decay_t<decltype(v)>;
        if constexpr (std::is_same_v<T, WildcardPattern>) {
            return "_";
        } else if constexpr (std::is_same_v<T, LiteralPattern>) {
            return formatExpr(*v.value);
        } else if constexpr (std::is_same_v<T, VariablePattern>) {
            return v.name;
        } else if constexpr (std::is_same_v<T, EnumPattern>) {
            return v.enum_name + "::" + v.variant_name;
        } else if constexpr (std::is_same_v<T, SomePattern>) {
            return "Some(" + v.binding + ")";
        } else if constexpr (std::is_same_v<T, NonePattern>) {
            return "None";
        } else if constexpr (std::is_same_v<T, OkPattern>) {
            return "Ok(" + v.binding + ")";
        } else if constexpr (std::is_same_v<T, ErrPattern>) {
            return "Err(" + v.binding + ")";
        } else if constexpr (std::is_same_v<T, EnumConstructorPattern>) {
            std::string result = v.enum_name + "::" + v.variant_name + "(";
            for (size_t i = 0; i < v.bindings.size(); ++i) {
                if (i > 0) result += ", ";
                result += v.bindings[i];
            }
            return result + ")";
        } else if constexpr (std::is_same_v<T, std::unique_ptr<OrPattern>>) {
            std::string result;
            for (size_t i = 0; i < v->alternatives.size(); ++i) {
                if (i > 0) result += " | ";
                result += formatPattern(v->alternatives[i]);
            }
            return result;
        } else {
            return "/* unknown pattern */";
        }
    }, pat);
}

// --- Directive formatting ---

void Formatter::formatDirectives(const std::vector<Directive> &directives) {
    for (const auto &d : directives) {
        emitCommentsBefore(d.loc.line);
        emitIndent();
        emit("@" + d.name);
        if (d.expr) {
            emit("(");
            emit(formatExpr(*d.expr));
            emit(")");
        } else if (!d.params.empty()) {
            emit("(");
            for (size_t i = 0; i < d.params.size(); ++i) {
                if (i > 0) emit(", ");
                emit(d.params[i].key + "=" + d.params[i].value);
            }
            emit(")");
        }
        emitNewline();
        last_emitted_line_ = d.loc.line;
    }
}

// --- Block formatting ---

bool Formatter::hasDirectives(const StmtNode &stmt) const {
    return std::visit([](const auto &v) -> bool {
        using T = std::decay_t<decltype(v)>;
        if constexpr (std::is_same_v<T, AssignStmt>) return !v.directives.empty();
        else if constexpr (std::is_same_v<T, CallStmt>) return !v.directives.empty();
        else if constexpr (std::is_same_v<T, RecordStmt>) return !v.directives.empty();
        else if constexpr (std::is_same_v<T, TupleDestructStmt>) return !v.directives.empty();
        else if constexpr (std::is_same_v<T, std::unique_ptr<FnStmt>>) return !v->directives.empty();
        else if constexpr (std::is_same_v<T, std::unique_ptr<ForStmt>>) return !v->directives.empty();
        else return false;
    }, stmt);
}

void Formatter::formatBlock(const std::vector<StmtNode> &body) {
    indent();
    for (size_t i = 0; i < body.size(); ++i) {
        int stmt_line = getStmtLine(body[i]);
        emitCommentsBefore(stmt_line);
        // Directive-bearing statements emit their own indent via formatDirectives
        if (!hasDirectives(body[i])) {
            emitIndent();
        }
        formatStmt(body[i]);
    }
    dedent();
}

// --- Statement helpers ---

bool Formatter::isDefinition(const StmtNode &stmt) const {
    return std::holds_alternative<std::unique_ptr<FnStmt>>(stmt) ||
           std::holds_alternative<RecordStmt>(stmt) ||
           std::holds_alternative<EnumStmt>(stmt) ||
           std::holds_alternative<TypeAliasStmt>(stmt);
}

int Formatter::getStmtLine(const StmtNode &stmt) const {
    return std::visit([](const auto &v) -> int {
        using T = std::decay_t<decltype(v)>;
        if constexpr (std::is_same_v<T, std::unique_ptr<IfStmt>>) return v->loc.line;
        else if constexpr (std::is_same_v<T, std::unique_ptr<WhileStmt>>) return v->loc.line;
        else if constexpr (std::is_same_v<T, std::unique_ptr<ForStmt>>) return v->loc.line;
        else if constexpr (std::is_same_v<T, std::unique_ptr<FnStmt>>) {
            if (!v->directives.empty()) return v->directives.front().loc.line;
            return v->loc.line;
        }
        else if constexpr (std::is_same_v<T, std::unique_ptr<MatchStmt>>) return v->loc.line;
        else if constexpr (std::is_same_v<T, AssignStmt>) {
            if (!v.directives.empty()) return v.directives.front().loc.line;
            return v.loc.line;
        }
        else if constexpr (std::is_same_v<T, CallStmt>) {
            if (!v.directives.empty()) return v.directives.front().loc.line;
            return v.loc.line;
        }
        else if constexpr (std::is_same_v<T, ReturnStmt>) return v.loc.line;
        else if constexpr (std::is_same_v<T, ImportStmt>) return v.loc.line;
        else if constexpr (std::is_same_v<T, RecordStmt>) {
            if (!v.directives.empty()) return v.directives.front().loc.line;
            return v.loc.line;
        }
        else if constexpr (std::is_same_v<T, IndexAssignStmt>) return v.loc.line;
        else if constexpr (std::is_same_v<T, BreakStmt>) return v.loc.line;
        else if constexpr (std::is_same_v<T, ContinueStmt>) return v.loc.line;
        else if constexpr (std::is_same_v<T, EllipsisStmt>) return v.loc.line;
        else if constexpr (std::is_same_v<T, FieldAssignStmt>) return v.loc.line;
        else if constexpr (std::is_same_v<T, EnumStmt>) return v.loc.line;
        else if constexpr (std::is_same_v<T, ExpectStmt>) return v.loc.line;
        else if constexpr (std::is_same_v<T, AwaitStmt>) return v.loc.line;
        else if constexpr (std::is_same_v<T, TupleDestructStmt>) {
            if (!v.directives.empty()) return v.directives.front().loc.line;
            return v.loc.line;
        }
        else if constexpr (std::is_same_v<T, TypeAliasStmt>) return v.loc.line;
        else return 0;
    }, stmt);
}

// --- Statement formatting ---

void Formatter::formatStmt(const StmtNode &stmt) {
    std::visit([this](const auto &v) {
        using T = std::decay_t<decltype(v)>;
        if constexpr (std::is_same_v<T, AssignStmt>) formatAssign(v);
        else if constexpr (std::is_same_v<T, CallStmt>) formatCall(v);
        else if constexpr (std::is_same_v<T, ReturnStmt>) formatReturn(v);
        else if constexpr (std::is_same_v<T, ImportStmt>) formatImport(v);
        else if constexpr (std::is_same_v<T, RecordStmt>) formatRecord(v);
        else if constexpr (std::is_same_v<T, IndexAssignStmt>) formatIndexAssign(v);
        else if constexpr (std::is_same_v<T, BreakStmt>) formatBreak(v);
        else if constexpr (std::is_same_v<T, ContinueStmt>) formatContinue(v);
        else if constexpr (std::is_same_v<T, EllipsisStmt>) formatEllipsis(v);
        else if constexpr (std::is_same_v<T, FieldAssignStmt>) formatFieldAssign(v);
        else if constexpr (std::is_same_v<T, EnumStmt>) formatEnum(v);
        else if constexpr (std::is_same_v<T, ExpectStmt>) formatExpect(v);
        else if constexpr (std::is_same_v<T, AwaitStmt>) formatAwaitStmt(v);
        else if constexpr (std::is_same_v<T, TupleDestructStmt>) formatTupleDestruct(v);
        else if constexpr (std::is_same_v<T, TypeAliasStmt>) formatTypeAlias(v);
        else if constexpr (std::is_same_v<T, std::unique_ptr<IfStmt>>) formatIf(*v);
        else if constexpr (std::is_same_v<T, std::unique_ptr<WhileStmt>>) formatWhile(*v);
        else if constexpr (std::is_same_v<T, std::unique_ptr<ForStmt>>) formatFor(*v);
        else if constexpr (std::is_same_v<T, std::unique_ptr<FnStmt>>) formatFn(*v);
        else if constexpr (std::is_same_v<T, std::unique_ptr<MatchStmt>>) formatMatch(*v);
    }, stmt);
}

void Formatter::formatAssign(const AssignStmt &s) {
    formatDirectives(s.directives);
    if (!s.directives.empty()) emitIndent();

    emit(s.name);
    if (s.type_annotation) {
        emit(": " + *s.type_annotation);
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
            if (!lambda.return_type.empty()) emit(" -> " + lambda.return_type);
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
            if (!lambda.return_type.empty()) emit(" -> " + lambda.return_type);
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
                if (!lambda.return_type.empty()) emit(" -> " + lambda.return_type);
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
        emit(field.name + ": " + field.type);
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
                emit(variant.field_types[i]);
            }
            emit(")");
        }
        emitNewline();
    }
    dedent();
}

void Formatter::formatTypeAlias(const TypeAliasStmt &s) {
    emit("type " + s.name + " = " + s.target_type);
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

    emit("for " + s.var_name);
    if (s.var_name2) emit(", " + *s.var_name2);
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
    if (!s.return_type.empty()) {
        emit(" -> " + s.return_type);
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
    emit(formatExpr(*s.object) + "[" + formatExpr(*s.index) + "] = " + formatExpr(*s.value));
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

// --- Top-level formatting ---

std::string Formatter::format(const Program &prog) {
    out_.clear();
    out_.reserve(source_.size());
    indent_level_ = 0;
    last_emitted_line_ = 0;
    next_comment_idx_ = 0;

    formatProgram(prog);
    emitTrailingComments();

    // Ensure file ends with newline
    if (!out_.empty() && out_.back() != '\n') {
        out_ += '\n';
    }

    // Remove trailing blank lines (keep single newline at end)
    while (endsWithBlankLine()) {
        out_.pop_back();
    }

    return out_;
}

void Formatter::formatProgram(const Program &prog) {
    for (size_t i = 0; i < prog.size(); ++i) {
        int stmt_line = getStmtLine(prog[i]);
        bool is_def = isDefinition(prog[i]);
        bool prev_is_def = (i > 0) && isDefinition(prog[i - 1]);

        // Blank line between top-level definitions or around definitions
        if (i > 0 && (is_def || prev_is_def)) {
            emitNewline();
        } else if (i > 0 && stmt_line > last_emitted_line_ + 1) {
            // Preserve blank line from source
            emitNewline();
        }

        emitCommentsBefore(stmt_line);
        if (!hasDirectives(prog[i])) {
            emitIndent();
        }
        formatStmt(prog[i]);
    }
}

// --- Static convenience ---

std::string Formatter::formatSource(const std::string &source, int indent_width) {
    Formatter fmt(source, indent_width);
    Lexer lexer(source);
    Parser parser(lexer);
    Program prog = parser.parseProgram();
    return fmt.format(prog);
}

bool Formatter::verifyFormatting(const std::string &formatted, std::string &reason) {
    reason.clear();
    try {
        auto reformatted = Formatter::formatSource(formatted);
        if (reformatted != formatted) {
            reason = "formatting is not idempotent (round-trip produced different output)";
            return false;
        }
        return true;
    } catch (const std::exception &e) {
        reason = std::string("formatted output failed to re-parse (") + e.what() + ")";
        return false;
    }
}

// ===== cmd_fmt =====

static void collectRyFiles(const std::string &dir, std::vector<std::string> &files) {
    std::error_code ec;
    for (auto &entry : fs::recursive_directory_iterator(dir, fs::directory_options::skip_permission_denied, ec)) {
        if (!entry.is_regular_file()) continue;
        auto path = entry.path();

        // Skip common directories
        std::string path_str = path.string();
        if (path_str.find("/.git/") != std::string::npos ||
            path_str.find("/build/") != std::string::npos ||
            path_str.find("/node_modules/") != std::string::npos)
            continue;

        if (path.extension() == ".ry") {
            files.push_back(path.string());
        }
    }
    std::sort(files.begin(), files.end());
}

static std::string readFile(const std::string &path) {
    std::ifstream f(path);
    if (!f) throw std::runtime_error("cannot open " + path);
    std::ostringstream ss;
    ss << f.rdbuf();
    return ss.str();
}

static void writeFile(const std::string &path, const std::string &content) {
    std::ofstream f(path);
    if (!f) throw std::runtime_error("cannot write " + path);
    f << content;
}

int cmd_fmt(int argc, char *argv[]) {
    bool check_mode = false;
    std::vector<std::string> targets;

    for (int i = 0; i < argc; ++i) {
        if (std::strcmp(argv[i], "--check") == 0) {
            check_mode = true;
        } else {
            targets.push_back(argv[i]);
        }
    }

    if (targets.empty()) {
        auto root = findProjectRoot();
        if (!root) {
            std::cerr << "Error: ry.toml not found. Specify a file or directory.\n";
            return 1;
        }
        targets.push_back(*root);
    }

    std::vector<std::string> files;
    for (const auto &target : targets) {
        std::error_code ec;
        if (fs::is_directory(target, ec)) {
            collectRyFiles(target, files);
        } else {
            files.push_back(target);
        }
    }

    int unformatted = 0;
    int skipped = 0;
    for (const auto &file : files) {
        try {
            auto source = readFile(file);
            auto formatted = Formatter::formatSource(source);

            if (source != formatted) {
                std::string reason;
                if (!Formatter::verifyFormatting(formatted, reason)) {
                    std::cerr << "warning: skipping " << file << ": " << reason << "\n";
                    ++skipped;
                    continue;
                }

                if (check_mode) {
                    std::cerr << file << " needs formatting\n";
                    ++unformatted;
                } else {
                    writeFile(file, formatted);
                    std::cout << "formatted " << file << "\n";
                }
            }
        } catch (const std::exception &e) {
            std::cerr << "Error formatting " << file << ": " << e.what() << "\n";
            return 1;
        }
    }

    if (skipped > 0) {
        std::cerr << skipped
                  << " file(s) skipped (round-trip verification failed)\n";
    }
    if (check_mode && unformatted > 0) {
        std::cerr << unformatted << " file(s) need formatting\n";
    }
    return (check_mode && (unformatted > 0 || skipped > 0)) ? 1 : 0;
}
