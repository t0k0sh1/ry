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


namespace ry {

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
    out_.append(static_cast<size_t>(indent_level_) * static_cast<size_t>(indent_width_), ' ');
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

std::string Formatter::formatNamedArgs(const std::vector<NamedArg> &named, bool has_positionals) {
    std::string result;
    for (size_t i = 0; i < named.size(); ++i) {
        if (i > 0 || has_positionals) result += ", ";
        result += named[i].name + "=" + formatExpr(*named[i].value);
    }
    return result;
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
    // Multi-branch / multi-line expressions must always be parenthesized
    // inside a binary expression to avoid ambiguity with surrounding operators.
    if (std::holds_alternative<std::unique_ptr<CaseCondExpr>>(child.data) ||
        std::holds_alternative<std::unique_ptr<CaseExpr>>(child.data) ||
        std::holds_alternative<std::unique_ptr<IfExpr>>(child.data) ||
        std::holds_alternative<std::unique_ptr<IfBlockExpr>>(child.data) ||
        std::holds_alternative<std::unique_ptr<AwaitExpr>>(child.data))
        return true;
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
            // Unsigned suffixes store a bit pattern in int64_t; cast to
            // uint64_t so that u64 max renders as 18446744073709551615
            // instead of -1.
            if (!v.suffix.empty() && v.suffix[0] == 'u')
                return std::to_string(static_cast<uint64_t>(v.value)) + v.suffix;
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
            args += formatNamedArgs(call.named_args, !call.args.empty());
            // Restore bracket syntax: parser converts name[T]() to name<T>()
            // but generic enum constructors use <> with :: (e.g. Option<int>::Some)
            std::string callee = call.callee;
            auto lt = callee.find('<');
            if (lt != std::string::npos && callee.back() == '>'
                && callee.find("::") == std::string::npos) {
                callee[lt] = '[';
                callee.back() = ']';
            }
            // Restore `module.callee(...)` form for qualified imports (#1723).
            std::string prefix;
            if (call.qualified_module.has_value())
                prefix = *call.qualified_module + ".";
            return prefix + callee + "(" + args + ")";
        } else if constexpr (std::is_same_v<T, std::unique_ptr<FieldAccessExpr>>) {
            // Qualified const access via `import xxx` (#1723): the parser
            // creates a FieldAccessExpr with an empty/placeholder object and
            // `qualified_module` set. Render as `module.field`.
            if (v->qualified_module.has_value())
                return *v->qualified_module + "." + v->field;
            return formatExpr(*v->object) + "." + v->field;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<TupleExpr>>) {
            std::string elems;
            for (size_t i = 0; i < v->elements.size(); ++i) {
                if (i > 0) elems += ", ";
                elems += formatExpr(*v->elements[i]);
            }
            if (v->elements.size() == 1)
                return "(" + elems + ",)";
            return "(" + elems + ")";
        } else if constexpr (std::is_same_v<T, std::unique_ptr<ListExpr>>) {
            std::string elems;
            for (size_t i = 0; i < v->elements.size(); ++i) {
                if (i > 0) elems += ", ";
                elems += formatExpr(*v->elements[i]);
            }
            return "[" + elems + "]";
        } else if constexpr (std::is_same_v<T, std::unique_ptr<IndexExpr>>) {
            std::string idxStr;
            for (size_t i = 0; i < v->indices.size(); ++i) {
                if (i > 0) idxStr += ", ";
                idxStr += formatExpr(*v->indices[i]);
            }
            // #1699: round-trip the Option-returning postfix `?` modifier.
            return formatExpr(*v->object) + "[" + idxStr + "]" +
                   (v->try_mode ? "?" : "");
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
            return formatExpr(*v->value) + " as " + v->target_type->toString();
        } else if constexpr (std::is_same_v<T, std::unique_ptr<CaseCondExpr>>) {
            std::string indent(static_cast<size_t>(indent_level_ + 1) * static_cast<size_t>(indent_width_), ' ');
            std::string out = "case:\n";
            for (const auto &arm : v->arms) {
                out += indent + formatExpr(*arm.condition) + " : " + formatExpr(*arm.value) + "\n";
            }
            out += indent + "_ : " + formatExpr(*v->else_expr);
            return out;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<CaseExpr>>) {
            std::string indent(static_cast<size_t>(indent_level_ + 1) * static_cast<size_t>(indent_width_), ' ');
            std::string out = "case " + formatExpr(*v->subject) + ":\n";
            for (size_t i = 0; i < v->arms.size(); ++i) {
                const auto &arm = v->arms[i];
                out += indent + formatPattern(arm.pattern);
                if (arm.guard)
                    out += " if " + formatExpr(*arm.guard);
                out += " : " + formatExpr(*arm.value);
                if (i + 1 < v->arms.size())
                    out += "\n";
            }
            return out;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<IfExpr>>) {
            return "if " + formatExpr(*v->condition) + " => "
                 + formatExpr(*v->then_value) + " else " + formatExpr(*v->else_value);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<IfBlockExpr>>) {
            auto getInlineExprText = [this](const std::vector<StmtNode> &body) -> std::optional<std::string> {
                if (body.size() != 1)
                    return std::nullopt;
                const auto *es = std::get_if<ExprStmt>(&body[0]);
                if (!es)
                    return std::nullopt;
                std::string formatted = formatExpr(*es->expr);
                if (formatted.find('\n') != std::string::npos)
                    return std::nullopt;
                return formatted;
            };
            auto appendBody = [&](std::string &out, const std::vector<StmtNode> &body, bool inlineAllowed) -> bool {
                if (inlineAllowed) {
                    if (auto inlineExpr = getInlineExprText(body)) {
                        out += " " + *inlineExpr;
                        return true;
                    }
                }
                out += "\n";
                for (size_t i = 0; i < body.size(); ++i) {
                    std::string saved_out = std::move(out_);
                    out_.clear();
                    int saved_indent = indent_level_;
                    indent_level_ += 1;
                    emitIndent();
                    formatStmt(body[i]);
                    indent_level_ = saved_indent;
                    std::string stmt_text = std::move(out_);
                    out_ = std::move(saved_out);
                    if (!stmt_text.empty() && stmt_text.back() == '\n')
                        stmt_text.pop_back();
                    out += stmt_text;
                    if (i + 1 < body.size())
                        out += "\n";
                }
                return false;
            };

            std::string out = "if " + formatExpr(*v->condition) + ":";
            bool thenInline = appendBody(out, v->then_body, true);
            out += thenInline ? " else:" : "\nelse:";
            appendBody(out, v->else_body, true);
            return out;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<RangeExpr>>) {
            return formatExpr(*v->start) + ".." + formatExpr(*v->end);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<ErrorPropagateExpr>>) {
            return formatExpr(*v->operand) + "?";
        } else if constexpr (std::is_same_v<T, std::unique_ptr<AwaitExpr>>) {
            return "await " + formatExpr(*v->operand);
        } else if constexpr (std::is_same_v<T, std::unique_ptr<WeakExpr>>) {
            return "weak " + formatExpr(*v->operand);
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
                    result += '{';
                    result += formatExpr(*v->exprs[i]);
                    result += '}';
                }
            }
            result += "\"";
            return result;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<LambdaExpr>>) {
            std::string result = formatLambdaSig(*v);
            if (v->expr_body) {
                result += " => ";
                result += formatExpr(*v->expr_body);
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
        result += params[i].name;
        result += ": ";
        result += params[i].type->toString();
        if (params[i].default_value) {
            result += " = ";
            result += formatExpr(*params[i].default_value);
        }
    }
    return result;
}

std::string Formatter::formatLambdaSig(const LambdaExpr &lambda) {
    std::string sig = "(";
    sig += formatParams(lambda.params);
    sig += ')';
    if (lambda.return_type) {
        sig += " -> ";
        sig += lambda.return_type->toString();
    }
    return sig;
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
        } else if constexpr (std::is_same_v<T, std::unique_ptr<EnumConstructorPattern>>) {
            std::string result = v->enum_name + "::" + v->variant_name + "(";
            for (size_t i = 0; i < v->bindings.size(); ++i) {
                if (i > 0) result += ", ";
                result += formatPattern(v->bindings[i]);
            }
            return result + ")";
        } else if constexpr (std::is_same_v<T, std::unique_ptr<OrPattern>>) {
            std::string result;
            for (size_t i = 0; i < v->alternatives.size(); ++i) {
                if (i > 0) result += " | ";
                result += formatPattern(v->alternatives[i]);
            }
            return result;
        } else if constexpr (std::is_same_v<T, std::unique_ptr<TuplePattern>>) {
            std::string result = "(";
            for (size_t i = 0; i < v->elements.size(); ++i) {
                if (i > 0) result += ", ";
                result += formatPattern(v->elements[i]);
            }
            if (v->elements.size() == 1) result += ",";
            return result + ")";
        } else if constexpr (std::is_same_v<T, std::unique_ptr<RecordPattern>>) {
            std::string result = v->name + "(";
            for (size_t i = 0; i < v->elements.size(); ++i) {
                if (i > 0) result += ", ";
                result += formatPattern(v->elements[i]);
            }
            return result + ")";
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
        if (!d.args.empty()) {
            emit("(");
            for (size_t i = 0; i < d.args.size(); ++i) {
                if (i > 0) emit(", ");
                const auto &a = d.args[i];
                if (a.name.has_value()) {
                    // Named argument: key=expr
                    emit(*a.name + "=");
                }
                if (a.value)
                    emit(formatExpr(*a.value));
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
        if constexpr (std::is_same_v<T, AssignStmt>) return !v.directives.empty(); // NOLINT(bugprone-branch-clone)
        else if constexpr (std::is_same_v<T, CallStmt>) return !v.directives.empty();
        else if constexpr (std::is_same_v<T, RecordStmt>) return !v.directives.empty();
        else if constexpr (std::is_same_v<T, EnumStmt>) return !v.directives.empty();
        else if constexpr (std::is_same_v<T, TypeAliasStmt>) return !v.directives.empty();
        else if constexpr (std::is_same_v<T, TupleDestructStmt>) return !v.directives.empty();
        else if constexpr (std::is_same_v<T, std::unique_ptr<FnStmt>>) return !v->directives.empty(); // NOLINT(bugprone-branch-clone)
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
        if constexpr (std::is_same_v<T, std::unique_ptr<IfStmt>>) return v->loc.line; // NOLINT(bugprone-branch-clone)
        else if constexpr (std::is_same_v<T, std::unique_ptr<CaseCondStmt>>) return v->loc.line;
        else if constexpr (std::is_same_v<T, std::unique_ptr<WhileStmt>>) return v->loc.line;
        else if constexpr (std::is_same_v<T, std::unique_ptr<ForStmt>>) return v->loc.line;
        else if constexpr (std::is_same_v<T, std::unique_ptr<FnStmt>>) {
            if (!v->directives.empty()) return v->directives.front().loc.line;
            return v->loc.line;
        }
        else if constexpr (std::is_same_v<T, std::unique_ptr<CaseStmt>>) return v->loc.line;
        else if constexpr (std::is_same_v<T, AssignStmt>) { // NOLINT(bugprone-branch-clone)
            if (!v.directives.empty()) return v.directives.front().loc.line;
            return v.loc.line;
        }
        else if constexpr (std::is_same_v<T, CallStmt>) {
            if (!v.directives.empty()) return v.directives.front().loc.line;
            return v.loc.line;
        }
        else if constexpr (std::is_same_v<T, ReturnStmt>) return v.loc.line; // NOLINT(bugprone-branch-clone)
        else if constexpr (std::is_same_v<T, ExprStmt>) return v.loc.line;
        else if constexpr (std::is_same_v<T, ImportStmt>) return v.loc.line;
        else if constexpr (std::is_same_v<T, std::unique_ptr<QualifiedImportStmt>>) return v->loc.line;
        else if constexpr (std::is_same_v<T, RecordStmt>) {
            if (!v.directives.empty()) return v.directives.front().loc.line;
            return v.loc.line;
        }
        else if constexpr (std::is_same_v<T, IndexAssignStmt>) return v.loc.line;
        else if constexpr (std::is_same_v<T, BreakStmt>) return v.loc.line;
        else if constexpr (std::is_same_v<T, ContinueStmt>) return v.loc.line;
        else if constexpr (std::is_same_v<T, EllipsisStmt>) return v.loc.line;
        else if constexpr (std::is_same_v<T, FieldAssignStmt>) return v.loc.line;
        else if constexpr (std::is_same_v<T, EnumStmt>) {
            if (!v.directives.empty()) return v.directives.front().loc.line;
            return v.loc.line;
        }
        else if constexpr (std::is_same_v<T, ExpectStmt>) return v.loc.line;
        else if constexpr (std::is_same_v<T, AwaitStmt>) return v.loc.line;
        else if constexpr (std::is_same_v<T, TupleDestructStmt>) {
            if (!v.directives.empty()) return v.directives.front().loc.line;
            return v.loc.line;
        }
        else if constexpr (std::is_same_v<T, TypeAliasStmt>) {
            if (!v.directives.empty()) return v.directives.front().loc.line;
            return v.loc.line;
        }
        else if constexpr (std::is_same_v<T, DirectiveDefStmt>) return v.loc.line;
        else return 0;
    }, stmt);
}

// --- Statement formatting ---

void Formatter::formatStmt(const StmtNode &stmt) {
    std::visit([this](const auto &v) {
        using T = std::decay_t<decltype(v)>;
        if constexpr (std::is_same_v<T, AssignStmt>) formatAssign(v);
        else if constexpr (std::is_same_v<T, CallStmt>) formatCall(v);
        else if constexpr (std::is_same_v<T, ExprStmt>) formatExprStmt(v);
        else if constexpr (std::is_same_v<T, ReturnStmt>) formatReturn(v);
        else if constexpr (std::is_same_v<T, ImportStmt>) formatImport(v);
        else if constexpr (std::is_same_v<T, std::unique_ptr<QualifiedImportStmt>>) formatQualifiedImport(*v);
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
        else if constexpr (std::is_same_v<T, DirectiveDefStmt>) formatDirectiveDef(v);
        else if constexpr (std::is_same_v<T, std::unique_ptr<IfStmt>>) formatIf(*v);
        else if constexpr (std::is_same_v<T, std::unique_ptr<CaseCondStmt>>) formatCaseCond(*v);
        else if constexpr (std::is_same_v<T, std::unique_ptr<WhileStmt>>) formatWhile(*v);
        else if constexpr (std::is_same_v<T, std::unique_ptr<ForStmt>>) formatFor(*v);
        else if constexpr (std::is_same_v<T, std::unique_ptr<FnStmt>>) formatFn(*v);
        else if constexpr (std::is_same_v<T, std::unique_ptr<CaseStmt>>) formatCase(*v);
    }, stmt);
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
        if (i > 0 && ((is_def || prev_is_def) || stmt_line > last_emitted_line_ + 1)) {
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
        const auto &path = entry.path();

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
            std::cerr << "Error: package.toml not found. Specify a file or directory.\n";
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

} // namespace ry
