#include "ry/lexer.hpp"
#include <cctype>
#include <stdexcept>

Token Lexer::next() {
    Token t = current_;
    current_ = readToken();
    return t;
}

Lexer::State Lexer::saveState() const {
    return {pos_, line_, at_line_start_, indent_stack_, pending_, current_, fstring_brace_depth_};
}

void Lexer::restoreState(State s) {
    pos_ = s.pos;
    line_ = s.line;
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

        int indent = 0;
        while (pos_ < src_.size() && src_[pos_] == ' ') {
            ++indent;
            ++pos_;
        }

        // Only process indent for lines with actual content
        bool has_content = pos_ < src_.size() &&
                           src_[pos_] != '\n' && src_[pos_] != '\r' &&
                           src_[pos_] != '#' && src_[pos_] != '\t';

        if (has_content) {
            if (indent > indent_stack_.back()) {
                indent_stack_.push_back(indent);
                return {TokenKind::Indent, "", line_};
            }
            while (indent < indent_stack_.back()) {
                indent_stack_.pop_back();
                pending_.push({TokenKind::Dedent, "", line_});
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
    while (pos_ < src_.size() && (src_[pos_] == ' ' || src_[pos_] == '\t'))
        ++pos_;

    // 4. Skip comment
    if (pos_ < src_.size() && src_[pos_] == '#') {
        while (pos_ < src_.size() && src_[pos_] != '\n' && src_[pos_] != '\r')
            ++pos_;
        return readToken();
    }

    // 5. EOF - generate remaining DEDENTs
    if (pos_ >= src_.size()) {
        if (indent_stack_.size() > 1) {
            while (indent_stack_.size() > 1) {
                indent_stack_.pop_back();
                pending_.push({TokenKind::Dedent, "", line_});
            }
            Token t = pending_.front();
            pending_.pop();
            return t;
        }
        return {TokenKind::Eof, "", line_};
    }

    char c = src_[pos_];

    if (c == '\n') { ++pos_; at_line_start_ = true; return {TokenKind::Newline, "\n", line_++}; }
    if (c == '\r') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '\n') ++pos_;
        at_line_start_ = true;
        return {TokenKind::Newline, "\n", line_++};
    }
    if (c == '+') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; return {TokenKind::PlusEq, "+=", line_};
        }
        return {TokenKind::Plus, "+", line_};
    }
    if (c == '-') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '>') {
            ++pos_; return {TokenKind::Arrow, "->", line_};
        }
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; return {TokenKind::MinusEq, "-=", line_};
        }
        return {TokenKind::Minus, "-", line_};
    }
    if (c == '%') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; return {TokenKind::PercentEq, "%=", line_};
        }
        return {TokenKind::Percent, "%", line_};
    }
    if (c == '*') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '*') {
            ++pos_;
            if (pos_ < src_.size() && src_[pos_] == '=') {
                ++pos_; return {TokenKind::StarStarEq, "**=", line_};
            }
            return {TokenKind::StarStar, "**", line_};
        }
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; return {TokenKind::StarEq, "*=", line_};
        }
        return {TokenKind::Star, "*", line_};
    }
    if (c == '/') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '/') {
            ++pos_;
            if (pos_ < src_.size() && src_[pos_] == '=') {
                ++pos_; return {TokenKind::SlashSlashEq, "//=", line_};
            }
            return {TokenKind::SlashSlash, "//", line_};
        }
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; return {TokenKind::SlashEq, "/=", line_};
        }
        return {TokenKind::Slash, "/", line_};
    }
    if (c == '=') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; return {TokenKind::EqEq, "==", line_};
        }
        return {TokenKind::Equals, "=", line_};
    }
    if (c == '(') { ++pos_; return {TokenKind::LParen, "(", line_}; }
    if (c == ')') { ++pos_; return {TokenKind::RParen, ")", line_}; }
    if (c == ',') { ++pos_; return {TokenKind::Comma,  ",", line_}; }
    if (c == '!') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; return {TokenKind::BangEq, "!=", line_};
        }
        return {TokenKind::Error, "!", line_};
    }
    if (c == '<') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '<') {
            ++pos_;
            if (pos_ < src_.size() && src_[pos_] == '=') {
                ++pos_; return {TokenKind::LessLessEq, "<<=", line_};
            }
            return {TokenKind::LessLess, "<<", line_};
        }
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; return {TokenKind::LessEq, "<=", line_};
        }
        return {TokenKind::Less, "<", line_};
    }
    if (c == '>') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '>') {
            ++pos_;
            if (pos_ < src_.size() && src_[pos_] == '>') {
                ++pos_; return {TokenKind::GreaterGreaterGreater, ">>>", line_};
            }
            if (pos_ < src_.size() && src_[pos_] == '=') {
                ++pos_; return {TokenKind::GreaterGreaterEq, ">>=", line_};
            }
            return {TokenKind::GreaterGreater, ">>", line_};
        }
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; return {TokenKind::GreaterEq, ">=", line_};
        }
        return {TokenKind::Greater, ">", line_};
    }
    if (c == '&') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; return {TokenKind::AmpEq, "&=", line_};
        }
        return {TokenKind::Amp, "&", line_};
    }
    if (c == '|') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; return {TokenKind::PipeEq, "|=", line_};
        }
        return {TokenKind::Pipe, "|", line_};
    }
    if (c == '^') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; return {TokenKind::CaretEq, "^=", line_};
        }
        return {TokenKind::Caret, "^", line_};
    }
    if (c == '~') { ++pos_; return {TokenKind::Tilde, "~", line_}; }
    if (c == ':') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == ':') {
            ++pos_; return {TokenKind::ColonColon, "::", line_};
        }
        return {TokenKind::Colon, ":", line_};
    }
    if (c == '.') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '.') {
            ++pos_; return {TokenKind::DotDot, "..", line_};
        }
        return {TokenKind::Dot, ".", line_};
    }
    if (c == '[') { ++pos_; return {TokenKind::LBracket, "[", line_}; }
    if (c == ']') { ++pos_; return {TokenKind::RBracket, "]", line_}; }
    if (c == '{') {
        ++pos_;
        if (fstring_brace_depth_ > 0)
            ++fstring_brace_depth_;
        return {TokenKind::LBrace, "{", line_};
    }
    if (c == '}') {
        if (fstring_brace_depth_ > 1) {
            --fstring_brace_depth_;
            ++pos_;
            return {TokenKind::RBrace, "}", line_};
        }
        if (fstring_brace_depth_ == 1) {
            fstring_brace_depth_ = 0;
            ++pos_; // skip '}'
            return readFStringSegment(false);
        }
        ++pos_;
        return {TokenKind::RBrace, "}", line_};
    }
    if (c == '@') { ++pos_; return {TokenKind::At,      "@", line_}; }
    if (c == '?') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '?') {
            ++pos_; return {TokenKind::QuestionQuestion, "??", line_};
        }
        return {TokenKind::Question, "?", line_};
    }

    // r-string: r"..." (raw string, no escape processing)
    if (c == 'r' && pos_ + 1 < src_.size() && src_[pos_ + 1] == '"') {
        pos_ += 2; // skip 'r' and '"'
        std::string str;
        while (pos_ < src_.size() && src_[pos_] != '"') {
            if (src_[pos_] == '\n' || src_[pos_] == '\r')
                throw std::runtime_error("line " + std::to_string(line_) +
                                         ": unterminated raw string literal");
            str += src_[pos_];
            ++pos_;
        }
        if (pos_ >= src_.size())
            throw std::runtime_error("line " + std::to_string(line_) +
                                     ": unterminated raw string literal");
        ++pos_; // skip closing '"'
        return {TokenKind::String, str, line_};
    }

    // f-string: f"..."
    if (c == 'f' && pos_ + 1 < src_.size() && src_[pos_ + 1] == '"') {
        if (fstring_brace_depth_ > 0)
            throw std::runtime_error("line " + std::to_string(line_) +
                ": nested f-strings are not supported");
        ++pos_; // skip 'f'
        ++pos_; // skip '"'
        return readFStringSegment(true);
    }

    if (c == '"') {
        ++pos_;
        std::string str;
        while (pos_ < src_.size() && src_[pos_] != '"') {
            if (src_[pos_] == '\n' || src_[pos_] == '\r')
                throw std::runtime_error("line " + std::to_string(line_) +
                                         ": unterminated string literal");
            if (src_[pos_] == '\\') {
                ++pos_;
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
                ++pos_;
            } else {
                str += src_[pos_];
                ++pos_;
            }
        }
        if (pos_ >= src_.size())
            throw std::runtime_error("line " + std::to_string(line_) +
                                     ": unterminated string literal");
        ++pos_;
        return {TokenKind::String, str, line_};
    }

    if (std::isdigit(c)) {
        size_t start = pos_;
        if (c == '0' && pos_ + 1 < src_.size()) {
            char next = src_[pos_ + 1];
            if (next == 'x' || next == 'X') {
                pos_ += 2; // skip "0x"
                if (pos_ >= src_.size() || !std::isxdigit(src_[pos_]))
                    throw std::runtime_error("line " + std::to_string(line_) + ": invalid hex literal");
                while (pos_ < src_.size() && std::isxdigit(src_[pos_])) ++pos_;
                return {TokenKind::Number, std::string(src_, start, pos_ - start), line_};
            }
            if (next == 'b' || next == 'B') {
                pos_ += 2; // skip "0b"
                if (pos_ >= src_.size() || (src_[pos_] != '0' && src_[pos_] != '1'))
                    throw std::runtime_error("line " + std::to_string(line_) + ": invalid binary literal");
                while (pos_ < src_.size() && (src_[pos_] == '0' || src_[pos_] == '1')) ++pos_;
                return {TokenKind::Number, std::string(src_, start, pos_ - start), line_};
            }
        }
        while (pos_ < src_.size() && std::isdigit(src_[pos_]))
            ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '.') {
            ++pos_;
            while (pos_ < src_.size() && std::isdigit(src_[pos_]))
                ++pos_;
            return {TokenKind::Float, std::string(src_, start, pos_ - start), line_};
        }
        return {TokenKind::Number, std::string(src_, start, pos_ - start), line_};
    }

    if (std::isalpha(c) || c == '_') {
        size_t start = pos_;
        while (pos_ < src_.size() && (std::isalnum(src_[pos_]) || src_[pos_] == '_'))
            ++pos_;
        std::string id(src_, start, pos_ - start);
        if (id == "and")   return {TokenKind::And,   "and",   line_};
        if (id == "or")    return {TokenKind::Or,    "or",    line_};
        if (id == "not")   return {TokenKind::Not,   "not",   line_};
        if (id == "true")  return {TokenKind::True,  "true",  line_};
        if (id == "false") return {TokenKind::False, "false", line_};
        if (id == "let")   return {TokenKind::Let,   "let",   line_};
        if (id == "var")   return {TokenKind::Var,   "var",   line_};
        if (id == "if")    return {TokenKind::If,    "if",    line_};
        if (id == "elif")  return {TokenKind::Elif,  "elif",  line_};
        if (id == "else")  return {TokenKind::Else,  "else",  line_};
        if (id == "while") return {TokenKind::While, "while", line_};
        if (id == "for")      return {TokenKind::For,      "for",      line_};
        if (id == "in")       return {TokenKind::In,       "in",       line_};
        if (id == "break")    return {TokenKind::Break,    "break",    line_};
        if (id == "continue") return {TokenKind::Continue, "continue", line_};
        if (id == "fn")     return {TokenKind::Fn,     "fn",     line_};
        if (id == "return") return {TokenKind::Return, "return", line_};
        if (id == "from")   return {TokenKind::From,   "from",   line_};
        if (id == "import") return {TokenKind::Import, "import", line_};
        if (id == "type")     return {TokenKind::Type,     "type",     line_};
        if (id == "record")   return {TokenKind::Record,   "record",   line_};
        if (id == "operator") return {TokenKind::Operator, "operator", line_};
        if (id == "enum")     return {TokenKind::Enum,     "enum",     line_};
        if (id == "match")    return {TokenKind::Match,    "match",    line_};
        if (id == "case")     return {TokenKind::Case,     "case",     line_};
        if (id == "describe") return {TokenKind::Describe, "describe", line_};
        if (id == "it")       return {TokenKind::It,       "it",       line_};
        if (id == "expect")   return {TokenKind::Expect,   "expect",   line_};
        if (id == "require")   return {TokenKind::Require,   "require",   line_};
        if (id == "ensure")    return {TokenKind::Ensure,    "ensure",    line_};
        if (id == "invariant") return {TokenKind::Invariant, "invariant", line_};
        if (id == "old")       return {TokenKind::Old,       "old",       line_};
        if (id == "result")    return {TokenKind::Result,    "result",    line_};
        if (id == "none")      return {TokenKind::NoneKw,    "none",      line_};
        if (id == "as")        return {TokenKind::As,        "as",        line_};
        if (id == "Ok")        return {TokenKind::Ok,        "Ok",        line_};
        if (id == "Err")       return {TokenKind::Err,       "Err",       line_};
        return {TokenKind::Ident, std::move(id), line_};
    }

    ++pos_;
    return {TokenKind::Error, std::string(1, c), line_};
}

Token Lexer::readFStringSegment(bool isStart) {
    std::string str;
    while (pos_ < src_.size() && src_[pos_] != '"') {
        if (src_[pos_] == '\n' || src_[pos_] == '\r')
            throw std::runtime_error("line " + std::to_string(line_) +
                                     ": unterminated f-string literal");
        if (src_[pos_] == '{') {
            if (pos_ + 1 < src_.size() && src_[pos_ + 1] == '{') {
                str += '{';
                pos_ += 2;
                continue;
            }
            ++pos_; // skip '{'
            fstring_brace_depth_ = 1;
            if (isStart)
                return {TokenKind::FStringStart, str, line_};
            else
                return {TokenKind::FStringMid, str, line_};
        }
        if (src_[pos_] == '}') {
            if (pos_ + 1 < src_.size() && src_[pos_ + 1] == '}') {
                str += '}';
                pos_ += 2;
                continue;
            }
            throw std::runtime_error("line " + std::to_string(line_) +
                                     ": unmatched '}' in f-string");
        }
        if (src_[pos_] == '\\') {
            ++pos_;
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
            ++pos_;
        } else {
            str += src_[pos_];
            ++pos_;
        }
    }
    if (pos_ >= src_.size())
        throw std::runtime_error("line " + std::to_string(line_) +
                                 ": unterminated f-string literal");
    ++pos_; // skip closing '"'
    return {TokenKind::FStringEnd, str, line_};
}
