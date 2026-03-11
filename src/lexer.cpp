#include "ry/lexer.hpp"
#include <cctype>
#include <stdexcept>

Token Lexer::next() {
    Token t = current_;
    current_ = readToken();
    return t;
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
    if (c == '+') { ++pos_; return {TokenKind::Plus,   "+", line_}; }
    if (c == '-') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '>') {
            ++pos_; return {TokenKind::Arrow, "->", line_};
        }
        return {TokenKind::Minus, "-", line_};
    }
    if (c == '%') { ++pos_; return {TokenKind::Percent, "%", line_}; }
    if (c == '*') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '*') {
            ++pos_; return {TokenKind::StarStar, "**", line_};
        }
        return {TokenKind::Star, "*", line_};
    }
    if (c == '/') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '/') {
            ++pos_; return {TokenKind::SlashSlash, "//", line_};
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
            ++pos_; return {TokenKind::LessLess, "<<", line_};
        }
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; return {TokenKind::LessEq, "<=", line_};
        }
        return {TokenKind::Less, "<", line_};
    }
    if (c == '>') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '>') {
            ++pos_; return {TokenKind::GreaterGreater, ">>", line_};
        }
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; return {TokenKind::GreaterEq, ">=", line_};
        }
        return {TokenKind::Greater, ">", line_};
    }
    if (c == '&') { ++pos_; return {TokenKind::Amp,   "&", line_}; }
    if (c == '|') { ++pos_; return {TokenKind::Pipe,  "|", line_}; }
    if (c == '^') { ++pos_; return {TokenKind::Caret, "^", line_}; }
    if (c == '~') { ++pos_; return {TokenKind::Tilde, "~", line_}; }
    if (c == ':') { ++pos_; return {TokenKind::Colon, ":", line_}; }
    if (c == '.') { ++pos_; return {TokenKind::Dot,   ".", line_}; }
    if (c == '[') { ++pos_; return {TokenKind::LBracket, "[", line_}; }
    if (c == ']') { ++pos_; return {TokenKind::RBracket, "]", line_}; }
    if (c == '{') { ++pos_; return {TokenKind::LBrace,   "{", line_}; }
    if (c == '}') { ++pos_; return {TokenKind::RBrace,   "}", line_}; }

    if (c == '"') {
        ++pos_;
        size_t start = pos_;
        while (pos_ < src_.size() && src_[pos_] != '"') {
            if (src_[pos_] == '\n' || src_[pos_] == '\r')
                throw std::runtime_error("line " + std::to_string(line_) +
                                         ": unterminated string literal");
            ++pos_;
        }
        if (pos_ >= src_.size())
            throw std::runtime_error("line " + std::to_string(line_) +
                                     ": unterminated string literal");
        std::string str(src_, start, pos_ - start);
        ++pos_;
        return {TokenKind::String, str, line_};
    }

    if (std::isdigit(c)) {
        size_t start = pos_;
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
        if (id == "const") return {TokenKind::Const, "const", line_};
        if (id == "if")    return {TokenKind::If,    "if",    line_};
        if (id == "elif")  return {TokenKind::Elif,  "elif",  line_};
        if (id == "else")  return {TokenKind::Else,  "else",  line_};
        if (id == "while") return {TokenKind::While, "while", line_};
        if (id == "fn")     return {TokenKind::Fn,     "fn",     line_};
        if (id == "return") return {TokenKind::Return, "return", line_};
        if (id == "from")   return {TokenKind::From,   "from",   line_};
        if (id == "import") return {TokenKind::Import, "import", line_};
        if (id == "type")   return {TokenKind::Type,   "type",   line_};
        return {TokenKind::Ident, std::move(id), line_};
    }

    ++pos_;
    return {TokenKind::Error, std::string(1, c), line_};
}
