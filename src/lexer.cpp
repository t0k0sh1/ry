#include "ry/lexer.hpp"
#include <cctype>
#include <stdexcept>

Token Lexer::next() {
    Token t = current_;
    current_ = readToken();
    return t;
}

Token Lexer::readToken() {
    // skip spaces/tabs
    while (pos_ < src_.size() && (src_[pos_] == ' ' || src_[pos_] == '\t'))
        ++pos_;

    if (pos_ >= src_.size())
        return {TokenKind::Eof, "", line_};

    char c = src_[pos_];

    if (c == '\n') { ++pos_; return {TokenKind::Newline, "\n", line_++}; }
    if (c == '\r') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '\n') ++pos_;
        return {TokenKind::Newline, "\n", line_++};
    }
    if (c == '+') { ++pos_; return {TokenKind::Plus,   "+", line_}; }
    if (c == '-') { ++pos_; return {TokenKind::Minus,  "-", line_}; }
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
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; return {TokenKind::LessEq, "<=", line_};
        }
        return {TokenKind::Less, "<", line_};
    }
    if (c == '>') {
        ++pos_;
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; return {TokenKind::GreaterEq, ">=", line_};
        }
        return {TokenKind::Greater, ">", line_};
    }

    if (std::isdigit(c)) {
        std::string num;
        while (pos_ < src_.size() && std::isdigit(src_[pos_]))
            num += src_[pos_++];
        if (pos_ < src_.size() && src_[pos_] == '.') {
            num += src_[pos_++];
            while (pos_ < src_.size() && std::isdigit(src_[pos_]))
                num += src_[pos_++];
            return {TokenKind::Float, num, line_};
        }
        return {TokenKind::Number, num, line_};
    }

    if (std::isalpha(c) || c == '_') {
        std::string id;
        while (pos_ < src_.size() && (std::isalnum(src_[pos_]) || src_[pos_] == '_'))
            id += src_[pos_++];
        if (id == "and") return {TokenKind::And, "and", line_};
        if (id == "or")  return {TokenKind::Or,  "or",  line_};
        if (id == "not") return {TokenKind::Not, "not", line_};
        return {TokenKind::Ident, id, line_};
    }

    ++pos_;
    return {TokenKind::Error, std::string(1, c), line_};
}
