#include "ry/lexer.hpp"
#include <cctype>
#include <stdexcept>

Token Lexer::next() {
    Token t = current_;
    current_ = readToken();
    return t;
}

Lexer::State Lexer::saveState() const {
    return {pos_, line_, col_, at_line_start_, indent_stack_, pending_, current_, fstring_brace_depth_};
}

void Lexer::restoreState(State s) {
    pos_ = s.pos;
    line_ = s.line;
    col_ = s.col;
    at_line_start_ = s.at_line_start;
    indent_stack_ = std::move(s.indent_stack);
    pending_ = std::move(s.pending);
    current_ = std::move(s.current);
    fstring_brace_depth_ = s.fstring_brace_depth;
}

Token Lexer::readToken() {
    // 1. Return pending tokens (multiple DEDENTs)
    if (!pending_.empty()) {
        Token t = pending_.front();
        pending_.pop();
        return t;
    }

    // 2. Indent processing at line start
    if (at_line_start_) {
        at_line_start_ = false;
        col_ = 1;

        int indent = 0;
        while (pos_ < src_.size() && src_[pos_] == ' ') {
            ++indent;
            ++pos_;
            ++col_;
        }

        // Only process indent for lines with actual content
        bool has_content = pos_ < src_.size() &&
                           src_[pos_] != '\n' && src_[pos_] != '\r' &&
                           src_[pos_] != '#' && src_[pos_] != '\t';

        if (has_content) {
            if (indent > indent_stack_.back()) {
                indent_stack_.push_back(indent);
                return {TokenKind::Indent, "", line_, col_};
            }
            while (indent < indent_stack_.back()) {
                indent_stack_.pop_back();
                pending_.push({TokenKind::Dedent, "", line_, col_});
            }
            if (indent != indent_stack_.back()) {
                throw std::runtime_error(
                    "line " + std::to_string(line_) +
                    ": dedent does not match any outer indentation level");
            }
            if (!pending_.empty()) {
                Token t = pending_.front();
                pending_.pop();
                return t;
            }
        }
    }

    // 3. Skip spaces/tabs
    while (pos_ < src_.size() && (src_[pos_] == ' ' || src_[pos_] == '\t')) {
        ++pos_;
        ++col_;
    }

    // 4. Skip comment
    if (pos_ < src_.size() && src_[pos_] == '#') {
        while (pos_ < src_.size() && src_[pos_] != '\n' && src_[pos_] != '\r') {
            ++pos_;
            ++col_;
        }
        // Fall through to handle the newline/EOF normally (no recursive call)
    }

    // 5. EOF - generate remaining DEDENTs
    if (pos_ >= src_.size()) {
        if (indent_stack_.size() > 1) {
            while (indent_stack_.size() > 1) {
                indent_stack_.pop_back();
                pending_.push({TokenKind::Dedent, "", line_, col_});
            }
            Token t = pending_.front();
            pending_.pop();
            return t;
        }
        return {TokenKind::Eof, "", line_, col_};
    }

    char c = src_[pos_];
    int startCol = col_;

    if (c == '\n') {
        ++pos_;
        at_line_start_ = true;
        int ln = line_++;
        col_ = 1;
        return {TokenKind::Newline, "\n", ln, startCol};
    }
    if (c == '\r') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '\n') ++pos_;
        at_line_start_ = true;
        int ln = line_++;
        col_ = 1;
        return {TokenKind::Newline, "\n", ln, startCol};
    }
    if (c == '+') {
        ++pos_; ++col_;
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; ++col_; return {TokenKind::PlusEq, "+=", line_, startCol};
        }
        return {TokenKind::Plus, "+", line_, startCol};
    }
    if (c == '-') {
        ++pos_; ++col_;
        if (pos_ < src_.size() && src_[pos_] == '>') {
            ++pos_; ++col_; return {TokenKind::Arrow, "->", line_, startCol};
        }
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; ++col_; return {TokenKind::MinusEq, "-=", line_, startCol};
        }
        return {TokenKind::Minus, "-", line_, startCol};
    }
    if (c == '%') {
        ++pos_; ++col_;
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; ++col_; return {TokenKind::PercentEq, "%=", line_, startCol};
        }
        return {TokenKind::Percent, "%", line_, startCol};
    }
    if (c == '*') {
        ++pos_; ++col_;
        if (pos_ < src_.size() && src_[pos_] == '*') {
            ++pos_; ++col_;
            if (pos_ < src_.size() && src_[pos_] == '=') {
                ++pos_; ++col_; return {TokenKind::StarStarEq, "**=", line_, startCol};
            }
            return {TokenKind::StarStar, "**", line_, startCol};
        }
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; ++col_; return {TokenKind::StarEq, "*=", line_, startCol};
        }
        return {TokenKind::Star, "*", line_, startCol};
    }
    if (c == '/') {
        ++pos_; ++col_;
        if (pos_ < src_.size() && src_[pos_] == '/') {
            ++pos_; ++col_;
            if (pos_ < src_.size() && src_[pos_] == '=') {
                ++pos_; ++col_; return {TokenKind::SlashSlashEq, "//=", line_, startCol};
            }
            return {TokenKind::SlashSlash, "//", line_, startCol};
        }
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; ++col_; return {TokenKind::SlashEq, "/=", line_, startCol};
        }
        return {TokenKind::Slash, "/", line_, startCol};
    }
    if (c == '=') {
        ++pos_; ++col_;
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; ++col_; return {TokenKind::EqEq, "==", line_, startCol};
        }
        return {TokenKind::Equals, "=", line_, startCol};
    }
    if (c == '(') { ++pos_; ++col_; return {TokenKind::LParen, "(", line_, startCol}; }
    if (c == ')') { ++pos_; ++col_; return {TokenKind::RParen, ")", line_, startCol}; }
    if (c == ',') { ++pos_; ++col_; return {TokenKind::Comma,  ",", line_, startCol}; }
    if (c == '!') {
        ++pos_; ++col_;
        if (pos_ < src_.size() && src_[pos_] == '!') {
            ++pos_; ++col_; return {TokenKind::BangBang, "!!", line_, startCol};
        }
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; ++col_; return {TokenKind::BangEq, "!=", line_, startCol};
        }
        return {TokenKind::Error, "!", line_, startCol};
    }
    if (c == '<') {
        ++pos_; ++col_;
        if (pos_ < src_.size() && src_[pos_] == '<') {
            ++pos_; ++col_;
            if (pos_ < src_.size() && src_[pos_] == '=') {
                ++pos_; ++col_; return {TokenKind::LessLessEq, "<<=", line_, startCol};
            }
            return {TokenKind::LessLess, "<<", line_, startCol};
        }
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; ++col_; return {TokenKind::LessEq, "<=", line_, startCol};
        }
        return {TokenKind::Less, "<", line_, startCol};
    }
    if (c == '>') {
        ++pos_; ++col_;
        if (pos_ < src_.size() && src_[pos_] == '>') {
            ++pos_; ++col_;
            if (pos_ < src_.size() && src_[pos_] == '>') {
                ++pos_; ++col_; return {TokenKind::GreaterGreaterGreater, ">>>", line_, startCol};
            }
            if (pos_ < src_.size() && src_[pos_] == '=') {
                ++pos_; ++col_; return {TokenKind::GreaterGreaterEq, ">>=", line_, startCol};
            }
            return {TokenKind::GreaterGreater, ">>", line_, startCol};
        }
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; ++col_; return {TokenKind::GreaterEq, ">=", line_, startCol};
        }
        return {TokenKind::Greater, ">", line_, startCol};
    }
    if (c == '&') {
        ++pos_; ++col_;
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; ++col_; return {TokenKind::AmpEq, "&=", line_, startCol};
        }
        return {TokenKind::Amp, "&", line_, startCol};
    }
    if (c == '|') {
        ++pos_; ++col_;
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; ++col_; return {TokenKind::PipeEq, "|=", line_, startCol};
        }
        return {TokenKind::Pipe, "|", line_, startCol};
    }
    if (c == '^') {
        ++pos_; ++col_;
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; ++col_; return {TokenKind::CaretEq, "^=", line_, startCol};
        }
        return {TokenKind::Caret, "^", line_, startCol};
    }
    if (c == '~') { ++pos_; ++col_; return {TokenKind::Tilde, "~", line_, startCol}; }
    if (c == ':') {
        ++pos_; ++col_;
        if (pos_ < src_.size() && src_[pos_] == ':') {
            ++pos_; ++col_; return {TokenKind::ColonColon, "::", line_, startCol};
        }
        return {TokenKind::Colon, ":", line_, startCol};
    }
    if (c == '.') {
        ++pos_; ++col_;
        if (pos_ < src_.size() && src_[pos_] == '.') {
            ++pos_; ++col_;
            if (pos_ < src_.size() && src_[pos_] == '.') {
                ++pos_; ++col_; return {TokenKind::Ellipsis, "...", line_, startCol};
            }
            return {TokenKind::DotDot, "..", line_, startCol};
        }
        return {TokenKind::Dot, ".", line_, startCol};
    }
    if (c == '[') { ++pos_; ++col_; return {TokenKind::LBracket, "[", line_, startCol}; }
    if (c == ']') { ++pos_; ++col_; return {TokenKind::RBracket, "]", line_, startCol}; }
    if (c == '{') {
        ++pos_; ++col_;
        if (fstring_brace_depth_ > 0)
            ++fstring_brace_depth_;
        return {TokenKind::LBrace, "{", line_, startCol};
    }
    if (c == '}') {
        if (fstring_brace_depth_ > 1) {
            --fstring_brace_depth_;
            ++pos_; ++col_;
            return {TokenKind::RBrace, "}", line_, startCol};
        }
        if (fstring_brace_depth_ == 1) {
            fstring_brace_depth_ = 0;
            ++pos_; ++col_;
            return readFStringSegment(false);
        }
        ++pos_; ++col_;
        return {TokenKind::RBrace, "}", line_, startCol};
    }
    if (c == '@') { ++pos_; ++col_; return {TokenKind::At, "@", line_, startCol}; }
    if (c == '?') {
        ++pos_; ++col_;
        if (pos_ < src_.size() && src_[pos_] == '?') {
            ++pos_; ++col_; return {TokenKind::QuestionQuestion, "??", line_, startCol};
        }
        return {TokenKind::Question, "?", line_, startCol};
    }

    // r-string: r"..." (raw string, no escape processing)
    if (c == 'r' && pos_ + 1 < src_.size() && src_[pos_ + 1] == '"') {
        pos_ += 2; col_ += 2;
        std::string str;
        while (pos_ < src_.size() && src_[pos_] != '"') {
            if (src_[pos_] == '\n' || src_[pos_] == '\r')
                throw std::runtime_error("line " + std::to_string(line_) +
                                         ": unterminated raw string literal");
            str += src_[pos_];
            ++pos_; ++col_;
        }
        if (pos_ >= src_.size())
            throw std::runtime_error("line " + std::to_string(line_) +
                                     ": unterminated raw string literal");
        ++pos_; ++col_;
        return {TokenKind::String, str, line_, startCol};
    }

    // f-string: f"..."
    if (c == 'f' && pos_ + 1 < src_.size() && src_[pos_ + 1] == '"') {
        if (fstring_brace_depth_ > 0)
            throw std::runtime_error("line " + std::to_string(line_) +
                ": nested f-strings are not supported");
        ++pos_; ++col_;
        ++pos_; ++col_;
        return readFStringSegment(true);
    }

    if (c == '"') {
        ++pos_; ++col_;
        std::string str;
        while (pos_ < src_.size() && src_[pos_] != '"') {
            if (src_[pos_] == '\n' || src_[pos_] == '\r')
                throw std::runtime_error("line " + std::to_string(line_) +
                                         ": unterminated string literal");
            if (src_[pos_] == '\\') {
                ++pos_; ++col_;
                if (pos_ >= src_.size())
                    throw std::runtime_error("line " + std::to_string(line_) +
                                             ": unterminated escape sequence");
                switch (src_[pos_]) {
                    case 'n':  str += '\n'; break;
                    case 't':  str += '\t'; break;
                    case '\\': str += '\\'; break;
                    case '"':  str += '"';  break;
                    case '0':  str += '\0'; break;
                    default:
                        throw std::runtime_error("line " + std::to_string(line_) +
                                                 ": unknown escape sequence '\\" +
                                                 std::string(1, src_[pos_]) + "'");
                }
                ++pos_; ++col_;
            } else {
                str += src_[pos_];
                ++pos_; ++col_;
            }
        }
        if (pos_ >= src_.size())
            throw std::runtime_error("line " + std::to_string(line_) +
                                     ": unterminated string literal");
        ++pos_; ++col_;
        return {TokenKind::String, str, line_, startCol};
    }

    if (std::isdigit(c)) {
        size_t start = pos_;
        if (c == '0' && pos_ + 1 < src_.size()) {
            char next = src_[pos_ + 1];
            if (next == 'x' || next == 'X') {
                pos_ += 2; col_ += 2;
                if (pos_ >= src_.size() || !std::isxdigit(src_[pos_]))
                    throw std::runtime_error("line " + std::to_string(line_) + ": invalid hex literal");
                while (pos_ < src_.size() && std::isxdigit(src_[pos_])) { ++pos_; ++col_; }
                return {TokenKind::Number, std::string(src_, start, pos_ - start), line_, startCol};
            }
            if (next == 'b' || next == 'B') {
                pos_ += 2; col_ += 2;
                if (pos_ >= src_.size() || (src_[pos_] != '0' && src_[pos_] != '1'))
                    throw std::runtime_error("line " + std::to_string(line_) + ": invalid binary literal");
                while (pos_ < src_.size() && (src_[pos_] == '0' || src_[pos_] == '1')) { ++pos_; ++col_; }
                return {TokenKind::Number, std::string(src_, start, pos_ - start), line_, startCol};
            }
        }
        while (pos_ < src_.size() && std::isdigit(src_[pos_])) { ++pos_; ++col_; }
        if (pos_ < src_.size() && src_[pos_] == '.' &&
            !(pos_ + 1 < src_.size() && src_[pos_ + 1] == '.')) {
            ++pos_; ++col_;
            while (pos_ < src_.size() && std::isdigit(src_[pos_])) { ++pos_; ++col_; }
            return {TokenKind::Float, std::string(src_, start, pos_ - start), line_, startCol};
        }
        return {TokenKind::Number, std::string(src_, start, pos_ - start), line_, startCol};
    }

    if (std::isalpha(c) || c == '_') {
        size_t start = pos_;
        while (pos_ < src_.size() && (std::isalnum(src_[pos_]) || src_[pos_] == '_')) { ++pos_; ++col_; }
        std::string id(src_, start, pos_ - start);
        if (id == "and")   return {TokenKind::And,   "and",   line_, startCol};
        if (id == "or")    return {TokenKind::Or,    "or",    line_, startCol};
        if (id == "not")   return {TokenKind::Not,   "not",   line_, startCol};
        if (id == "true")  return {TokenKind::True,  "true",  line_, startCol};
        if (id == "false") return {TokenKind::False, "false", line_, startCol};
        if (id == "let")   return {TokenKind::Let,   "let",   line_, startCol};
        if (id == "var")   return {TokenKind::Var,   "var",   line_, startCol};
        if (id == "if")    return {TokenKind::If,    "if",    line_, startCol};
        if (id == "elif")  return {TokenKind::Elif,  "elif",  line_, startCol};
        if (id == "else")  return {TokenKind::Else,  "else",  line_, startCol};
        if (id == "while") return {TokenKind::While, "while", line_, startCol};
        if (id == "for")      return {TokenKind::For,      "for",      line_, startCol};
        if (id == "in")       return {TokenKind::In,       "in",       line_, startCol};
        if (id == "break")    return {TokenKind::Break,    "break",    line_, startCol};
        if (id == "continue") return {TokenKind::Continue, "continue", line_, startCol};
        if (id == "fn")     return {TokenKind::Fn,     "fn",     line_, startCol};
        if (id == "return") return {TokenKind::Return, "return", line_, startCol};
        if (id == "from")   return {TokenKind::From,   "from",   line_, startCol};
        if (id == "import") return {TokenKind::Import, "import", line_, startCol};
        if (id == "type")     return {TokenKind::Type,     "type",     line_, startCol};
        if (id == "record")   return {TokenKind::Record,   "record",   line_, startCol};
        if (id == "operator") return {TokenKind::Operator, "operator", line_, startCol};
        if (id == "enum")     return {TokenKind::Enum,     "enum",     line_, startCol};
        if (id == "match")    return {TokenKind::Match,    "match",    line_, startCol};
        if (id == "case")     return {TokenKind::Case,     "case",     line_, startCol};
        if (id == "expect")   return {TokenKind::Expect,   "expect",   line_, startCol};
        if (id == "require")   return {TokenKind::Require,   "require",   line_, startCol};
        if (id == "ensure")    return {TokenKind::Ensure,    "ensure",    line_, startCol};
        if (id == "invariant") return {TokenKind::Invariant, "invariant", line_, startCol};
        if (id == "old")       return {TokenKind::Old,       "old",       line_, startCol};
        if (id == "result")    return {TokenKind::Result,    "result",    line_, startCol};
        if (id == "none")      return {TokenKind::NoneKw,    "none",      line_, startCol};
        if (id == "as")        return {TokenKind::As,        "as",        line_, startCol};
        if (id == "Error")     return {TokenKind::ErrorKw,   "Error",     line_, startCol};
        return {TokenKind::Ident, std::move(id), line_, startCol};
    }

    ++pos_; ++col_;
    return {TokenKind::Error, std::string(1, c), line_, startCol};
}

Token Lexer::readFStringSegment(bool isStart) {
    int startCol = col_;
    std::string str;
    while (pos_ < src_.size() && src_[pos_] != '"') {
        if (src_[pos_] == '\n' || src_[pos_] == '\r')
            throw std::runtime_error("line " + std::to_string(line_) +
                                     ": unterminated f-string literal");
        if (src_[pos_] == '{') {
            if (pos_ + 1 < src_.size() && src_[pos_ + 1] == '{') {
                str += '{';
                pos_ += 2; col_ += 2;
                continue;
            }
            ++pos_; ++col_;
            fstring_brace_depth_ = 1;
            if (isStart)
                return {TokenKind::FStringStart, str, line_, startCol};
            else
                return {TokenKind::FStringMid, str, line_, startCol};
        }
        if (src_[pos_] == '}') {
            if (pos_ + 1 < src_.size() && src_[pos_ + 1] == '}') {
                str += '}';
                pos_ += 2; col_ += 2;
                continue;
            }
            throw std::runtime_error("line " + std::to_string(line_) +
                                     ": unmatched '}' in f-string");
        }
        if (src_[pos_] == '\\') {
            ++pos_; ++col_;
            if (pos_ >= src_.size())
                throw std::runtime_error("line " + std::to_string(line_) +
                                         ": unterminated escape sequence in f-string");
            switch (src_[pos_]) {
                case 'n':  str += '\n'; break;
                case 't':  str += '\t'; break;
                case '\\': str += '\\'; break;
                case '"':  str += '"';  break;
                case '0':  str += '\0'; break;
                default:
                    throw std::runtime_error("line " + std::to_string(line_) +
                                             ": unknown escape sequence '\\" +
                                             std::string(1, src_[pos_]) + "' in f-string");
            }
            ++pos_; ++col_;
        } else {
            str += src_[pos_];
            ++pos_; ++col_;
        }
    }
    if (pos_ >= src_.size())
        throw std::runtime_error("line " + std::to_string(line_) +
                                 ": unterminated f-string literal");
    ++pos_; ++col_;
    return {TokenKind::FStringEnd, str, line_, startCol};
}
