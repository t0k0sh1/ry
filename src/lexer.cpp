#include "ry/lexer.hpp"
#include <cctype>
#include <cstring>
#include <stdexcept>
#include <unordered_map>


namespace ry {

static const auto isDecDigit = [](unsigned char c) { return std::isdigit(c) != 0; };
static const auto isHexDigit = [](unsigned char c) { return std::isxdigit(c) != 0; };
static const auto isBinDigit = [](unsigned char c) { return c == '0' || c == '1'; };

// Consume digits with optional underscore separators.
// Underscore must appear between two valid digits (no leading/trailing/consecutive _).
template <typename Pred>
static void consumeDigitsWithSeparators(const std::string &src, size_t &pos,
                                        int &col, int line, Pred isValid) {
    while (pos < src.size()) {
        if (isValid(static_cast<unsigned char>(src[pos]))) {
            ++pos; ++col;
        } else if (src[pos] == '_') {
            if (pos + 1 >= src.size() ||
                !isValid(static_cast<unsigned char>(src[pos + 1])))
                throw std::runtime_error(
                    "line " + std::to_string(line) +
                    ": invalid underscore in numeric literal");
            ++pos; ++col;
        } else {
            break;
        }
    }
}

static const std::unordered_map<std::string, TokenKind> keyword_map = {
    {"and",       TokenKind::And},
    {"or",        TokenKind::Or},
    {"not",       TokenKind::Not},
    {"true",      TokenKind::True},
    {"false",     TokenKind::False},
    {"if",        TokenKind::If},
    {"else",      TokenKind::Else},
    {"while",     TokenKind::While},
    {"for",       TokenKind::For},
    {"in",        TokenKind::In},
    {"break",     TokenKind::Break},
    {"continue",  TokenKind::Continue},
    {"function",  TokenKind::Fn},
    {"fn",        TokenKind::Fn},
    {"return",    TokenKind::Return},
    {"from",      TokenKind::From},
    {"import",    TokenKind::Import},
    {"type",      TokenKind::Type},
    {"record",    TokenKind::Record},
    {"operator",  TokenKind::Operator},
    {"enum",      TokenKind::Enum},
    {"case",      TokenKind::Case},
    {"expect",    TokenKind::Expect},
    {"require",   TokenKind::Require},
    {"ensure",    TokenKind::Ensure},
    {"invariant", TokenKind::Invariant},
    {"none",      TokenKind::NoneKw},
    {"as",        TokenKind::As},
    {"Error",     TokenKind::ErrorKw},
    {"async",     TokenKind::Async},
    {"await",     TokenKind::Await},
};

Token Lexer::next() {
    Token t = current_;
    prev_kind_ = t.kind;
    current_ = readToken();
    return t;
}

bool Lexer::consumeGreaterInTypeContext() {
    if (current_.kind == TokenKind::Greater) {
        current_ = readToken();
        return true;
    }
    if (current_.kind == TokenKind::GreaterGreater) {
        current_ = {TokenKind::Greater, ">", current_.line, current_.col + 1};
        return true;
    }
    if (current_.kind == TokenKind::GreaterGreaterGreater) {
        current_ = {TokenKind::GreaterGreater, ">>", current_.line, current_.col + 1};
        return true;
    }
    return false;
}

Lexer::State Lexer::saveState() const {
    return {pos_, line_, col_, at_line_start_, indent_stack_, pending_, current_, fstring_brace_depth_, prev_kind_};
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
    prev_kind_ = s.prev_kind;
}

void Lexer::tryConsumeNumericSuffix(TokenKind &kind) {
    if (pos_ >= src_.size()) return;
    char ch = src_[pos_];
    if (ch != 'i' && ch != 'u' && ch != 'f') return;

    static const char *suffixes[] = {
        "i8", "i16", "i32", "i64",
        "u8", "u16", "u32", "u64",
        "f32", "f64"
    };
    for (const char *suf : suffixes) {
        size_t len = std::strlen(suf);
        if (pos_ + len > src_.size()) continue;
        if (src_.compare(pos_, len, suf) != 0) continue;
        // Avoid matching partial identifiers like `42i32x`
        if (pos_ + len < src_.size()) {
            char after = src_[pos_ + len];
            if (std::isalnum(static_cast<unsigned char>(after)) || after == '_') continue;
        }
        pos_ += len;
        col_ += static_cast<int>(len);
        if (suf[0] == 'f' && kind == TokenKind::Number)
            kind = TokenKind::Float;
        return;
    }
}

void Lexer::checkNoTrailingIdentStart() const {
    if (pos_ < src_.size()) {
        char ch = src_[pos_];
        if (std::isalpha(static_cast<unsigned char>(ch)) || ch == '_')
            throw std::runtime_error(
                "line " + std::to_string(line_) +
                ": invalid character after numeric literal");
    }
}

bool Lexer::consumeExponentIfPresent() {
    if (pos_ >= src_.size()) return false;
    if (src_[pos_] != 'e' && src_[pos_] != 'E') return false;
    // One-char lookahead over an optional sign so `1exp` and `1e` fall
    // through to checkNoTrailingIdentStart (treating `e` as an identifier
    // start) instead of being stolen as a malformed exponent.
    size_t look = pos_ + 1;
    if (look < src_.size() && (src_[look] == '+' || src_[look] == '-'))
        ++look;
    if (look >= src_.size() || !std::isdigit(static_cast<unsigned char>(src_[look])))
        return false;
    ++pos_; ++col_; // consume 'e' / 'E'
    if (pos_ < src_.size() && (src_[pos_] == '+' || src_[pos_] == '-')) {
        ++pos_; ++col_;
    }
    consumeDigitsWithSeparators(src_, pos_, col_, line_, isDecDigit);
    return true;
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
        if (pos_ < src_.size() && src_[pos_] == '+') {
            // Emit PlusPlus only when followed by a statement terminator
            // (whitespace, newline, EOF, comment) to avoid breaking `x + +1`
            size_t after = pos_ + 1;
            if (after >= src_.size() || src_[after] == ' ' || src_[after] == '\t' ||
                src_[after] == '\n' || src_[after] == '\r' || src_[after] == '#') {
                ++pos_; ++col_; return {TokenKind::PlusPlus, "++", line_, startCol};
            }
        }
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
        if (pos_ < src_.size() && src_[pos_] == '-') {
            // Emit MinusMinus only when followed by a statement terminator
            size_t after = pos_ + 1;
            if (after >= src_.size() || src_[after] == ' ' || src_[after] == '\t' ||
                src_[after] == '\n' || src_[after] == '\r' || src_[after] == '#') {
                ++pos_; ++col_; return {TokenKind::MinusMinus, "--", line_, startCol};
            }
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
        // Regex literal: `/` starts a regex when NOT preceded by a value-producing token
        if (prev_kind_ != TokenKind::Number && prev_kind_ != TokenKind::Float &&
            prev_kind_ != TokenKind::String && prev_kind_ != TokenKind::FStringEnd &&
            prev_kind_ != TokenKind::Ident && prev_kind_ != TokenKind::RParen &&
            prev_kind_ != TokenKind::RBracket && prev_kind_ != TokenKind::RBrace &&
            prev_kind_ != TokenKind::True && prev_kind_ != TokenKind::False &&
            prev_kind_ != TokenKind::NoneKw && prev_kind_ != TokenKind::PlusPlus &&
            prev_kind_ != TokenKind::MinusMinus && prev_kind_ != TokenKind::RegexLiteral) {
            std::string pattern;
            size_t runStart = pos_;
            while (pos_ < src_.size() && src_[pos_] != '/' &&
                   src_[pos_] != '\n' && src_[pos_] != '\r') {
                if (src_[pos_] == '\\') {
                    if (pos_ + 1 >= src_.size() || src_[pos_ + 1] == '\n' ||
                        src_[pos_ + 1] == '\r')
                        return {TokenKind::Error, "unterminated regex literal", line_, startCol};
                    if (src_[pos_ + 1] == '0') {
                        // \0 → NUL byte (mirrors string literal escape handling)
                        pattern.append(src_, runStart, pos_ - runStart);
                        pattern += '\0';
                        pos_ += 2; col_ += 2;
                        runStart = pos_;
                    } else {
                        // Other escapes (e.g. \/, \d, \\) pass through verbatim for the runtime parser
                        pos_ += 2; col_ += 2;
                    }
                } else {
                    ++pos_; ++col_;
                }
            }
            if (pos_ >= src_.size() || src_[pos_] != '/')
                return {TokenKind::Error, "unterminated regex literal", line_, startCol};
            pattern.append(src_, runStart, pos_ - runStart);
            ++pos_; ++col_; // consume closing /
            return {TokenKind::RegexLiteral, std::move(pattern), line_, startCol};
        }
        return {TokenKind::Slash, "/", line_, startCol};
    }
    if (c == '=') {
        ++pos_; ++col_;
        if (pos_ < src_.size() && src_[pos_] == '=') {
            ++pos_; ++col_; return {TokenKind::EqEq, "==", line_, startCol};
        }
        if (pos_ < src_.size() && src_[pos_] == '>') {
            ++pos_; ++col_; return {TokenKind::FatArrow, "=>", line_, startCol};
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
        if (pos_ < src_.size() && std::isdigit(static_cast<unsigned char>(src_[pos_])) &&
            prev_kind_ != TokenKind::Ident && prev_kind_ != TokenKind::Number &&
            prev_kind_ != TokenKind::Float && prev_kind_ != TokenKind::String &&
            prev_kind_ != TokenKind::RParen && prev_kind_ != TokenKind::RBracket &&
            prev_kind_ != TokenKind::RBrace && prev_kind_ != TokenKind::True &&
            prev_kind_ != TokenKind::False && prev_kind_ != TokenKind::NoneKw &&
            prev_kind_ != TokenKind::FStringEnd) {
            size_t start = pos_ - 1;
            consumeDigitsWithSeparators(src_, pos_, col_, line_,
                isDecDigit);
            TokenKind numKind = TokenKind::Float;
            consumeExponentIfPresent();
            tryConsumeNumericSuffix(numKind);
            checkNoTrailingIdentStart();
            return {numKind, std::string(src_, start, pos_ - start), line_, startCol};
        }
        return {TokenKind::Dot, ".", line_, startCol};
    }
    if (c == '[') { ++pos_; ++col_; return {TokenKind::LBracket, "[", line_, startCol}; }
    if (c == ']') { ++pos_; ++col_; return {TokenKind::RBracket, "]", line_, startCol}; }
    if (c == ';') { ++pos_; ++col_; return {TokenKind::Semi, ";", line_, startCol}; }
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
        size_t strStart = pos_;
        while (pos_ < src_.size() && src_[pos_] != '"') {
            if (src_[pos_] == '\n' || src_[pos_] == '\r')
                throw std::runtime_error("line " + std::to_string(line_) +
                                         ": unterminated raw string literal");
            ++pos_; ++col_;
        }
        if (pos_ >= src_.size())
            throw std::runtime_error("line " + std::to_string(line_) +
                                     ": unterminated raw string literal");
        std::string str(src_, strStart, pos_ - strStart);
        ++pos_; ++col_;
        return {TokenKind::String, std::move(str), line_, startCol};
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
        size_t runStart = pos_;
        while (pos_ < src_.size() && src_[pos_] != '"') {
            if (src_[pos_] == '\n' || src_[pos_] == '\r')
                throw std::runtime_error("line " + std::to_string(line_) +
                                         ": unterminated string literal");
            if (src_[pos_] == '\\') {
                str.append(src_, runStart, pos_ - runStart);
                ++pos_; ++col_;
                if (pos_ >= src_.size())
                    throw std::runtime_error("line " + std::to_string(line_) +
                                             ": unterminated escape sequence");
                switch (src_[pos_]) {
                    case 'n':  str += '\n'; break;
                    case 'r':  str += '\r'; break;
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
                runStart = pos_;
            } else {
                ++pos_; ++col_;
            }
        }
        str.append(src_, runStart, pos_ - runStart);
        if (pos_ >= src_.size())
            throw std::runtime_error("line " + std::to_string(line_) +
                                     ": unterminated string literal");
        ++pos_; ++col_;
        return {TokenKind::String, str, line_, startCol};
    }

    if (std::isdigit(static_cast<unsigned char>(c))) {
        size_t start = pos_;
        TokenKind numKind = TokenKind::Number;
        if (c == '0' && pos_ + 1 < src_.size()) {
            char next = src_[pos_ + 1];
            if (next == 'x' || next == 'X') {
                pos_ += 2; col_ += 2;
                if (pos_ >= src_.size() || !std::isxdigit(static_cast<unsigned char>(src_[pos_])))
                    throw std::runtime_error("line " + std::to_string(line_) + ": invalid hex literal");
                consumeDigitsWithSeparators(src_, pos_, col_, line_,
                    isHexDigit);
                tryConsumeNumericSuffix(numKind);
                checkNoTrailingIdentStart();
                return {numKind, std::string(src_, start, pos_ - start), line_, startCol};
            }
            if (next == 'b' || next == 'B') {
                pos_ += 2; col_ += 2;
                if (pos_ >= src_.size() || (src_[pos_] != '0' && src_[pos_] != '1'))
                    throw std::runtime_error("line " + std::to_string(line_) + ": invalid binary literal");
                consumeDigitsWithSeparators(src_, pos_, col_, line_,
                    isBinDigit);
                tryConsumeNumericSuffix(numKind);
                checkNoTrailingIdentStart();
                return {numKind, std::string(src_, start, pos_ - start), line_, startCol};
            }
            if (next == 'o' || next == 'O') {
                throw std::runtime_error(
                    "line " + std::to_string(line_) +
                    ": octal literals (0o...) are not supported; "
                    "use hex (0x...) or binary (0b...) instead");
            }
        }
        consumeDigitsWithSeparators(src_, pos_, col_, line_,
            isDecDigit);
        // Fraction part (e.g., 3.14).
        if (pos_ < src_.size() && src_[pos_] == '.' &&
            pos_ + 1 < src_.size() && std::isdigit(static_cast<unsigned char>(src_[pos_ + 1]))) {
            ++pos_; ++col_;
            consumeDigitsWithSeparators(src_, pos_, col_, line_,
                isDecDigit);
            numKind = TokenKind::Float;
        }
        // Exponent part (e.g. `1e10`, `1.5e-3`, `2E+8`).
        if (consumeExponentIfPresent())
            numKind = TokenKind::Float;
        tryConsumeNumericSuffix(numKind);
        checkNoTrailingIdentStart();
        return {numKind, std::string(src_, start, pos_ - start), line_, startCol};
    }

    if (std::isalpha(static_cast<unsigned char>(c)) || c == '_') {
        size_t start = pos_;
        while (pos_ < src_.size() && (std::isalnum(static_cast<unsigned char>(src_[pos_])) || src_[pos_] == '_')) { ++pos_; ++col_; }
        // Allow trailing '!' for mutating method names (e.g., sort!, reverse!)
        // but not '!=' (not-equal) or '!!' (error-propagate operator)
        if (pos_ < src_.size() && src_[pos_] == '!' &&
            (pos_ + 1 >= src_.size() ||
             (src_[pos_ + 1] != '=' && src_[pos_ + 1] != '!'))) {
            ++pos_; ++col_;
        }
        std::string id(src_, start, pos_ - start);
        auto kit = keyword_map.find(id);
        if (kit != keyword_map.end()) {
            if (kit->second == TokenKind::Not) {
                size_t savedPos = pos_;
                int savedCol = col_;
                while (pos_ < src_.size() && (src_[pos_] == ' ' || src_[pos_] == '\t')) {
                    ++pos_; ++col_;
                }
                if (pos_ + 2 <= src_.size() &&
                    src_[pos_] == 'i' && src_[pos_ + 1] == 'n' &&
                    (pos_ + 2 >= src_.size() ||
                     (!std::isalnum(static_cast<unsigned char>(src_[pos_ + 2])) && src_[pos_ + 2] != '_'))) {
                    pos_ += 2; col_ += 2;
                    return {TokenKind::NotIn, "not in", line_, startCol};
                }
                pos_ = savedPos;
                col_ = savedCol;
            }
            return {kit->second, std::move(id), line_, startCol};
        }
        return {TokenKind::Ident, std::move(id), line_, startCol};
    }

    ++pos_; ++col_;
    return {TokenKind::Error, std::string(1, c), line_, startCol};
}

Token Lexer::readFStringSegment(bool isStart) {
    int startCol = col_;
    std::string str;
    size_t runStart = pos_;
    while (pos_ < src_.size() && src_[pos_] != '"') {
        if (src_[pos_] == '\n' || src_[pos_] == '\r')
            throw std::runtime_error("line " + std::to_string(line_) +
                                     ": unterminated f-string literal");
        if (src_[pos_] == '{') {
            str.append(src_, runStart, pos_ - runStart);
            if (pos_ + 1 < src_.size() && src_[pos_ + 1] == '{') {
                str += '{';
                pos_ += 2; col_ += 2;
                runStart = pos_;
                continue;
            }
            ++pos_; ++col_;
            fstring_brace_depth_ = 1;
            if (isStart)
                return {TokenKind::FStringStart, std::move(str), line_, startCol};
            else
                return {TokenKind::FStringMid, std::move(str), line_, startCol};
        }
        if (src_[pos_] == '}') {
            str.append(src_, runStart, pos_ - runStart);
            if (pos_ + 1 < src_.size() && src_[pos_ + 1] == '}') {
                str += '}';
                pos_ += 2; col_ += 2;
                runStart = pos_;
                continue;
            }
            throw std::runtime_error("line " + std::to_string(line_) +
                                     ": unmatched '}' in f-string");
        }
        if (src_[pos_] == '\\') {
            str.append(src_, runStart, pos_ - runStart);
            ++pos_; ++col_;
            if (pos_ >= src_.size())
                throw std::runtime_error("line " + std::to_string(line_) +
                                         ": unterminated escape sequence in f-string");
            switch (src_[pos_]) {
                case 'n':  str += '\n'; break;
                case 'r':  str += '\r'; break;
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
            runStart = pos_;
        } else {
            ++pos_; ++col_;
        }
    }
    str.append(src_, runStart, pos_ - runStart);
    if (pos_ >= src_.size())
        throw std::runtime_error("line " + std::to_string(line_) +
                                 ": unterminated f-string literal");
    ++pos_; ++col_;
    return {TokenKind::FStringEnd, str, line_, startCol};
}

} // namespace ry
