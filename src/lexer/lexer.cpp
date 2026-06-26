#include "ry/lexer/lexer.hpp"
#include <cctype>
#include <cstdint>
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

// Decode the UTF-8 character at `src[pos]` and return {sequence, byte_length}.
// Accepts ONLY well-formed UTF-8 per RFC 3629: lead-byte ranges exclude the
// overlong 2-byte leads `0xC0`/`0xC1` and out-of-range 4-byte leads
// `0xF5..0xFF`, and continuation-byte ranges further reject overlong 3-byte
// (`E0 80..9F`) / 4-byte (`F0 80..8F`) encodings, surrogate pairs
// (`ED A0..BF`), and code points above `U+10FFFF` (`F4 90..BF`). Any
// non-well-formed byte falls back to the printable hex escape "\xHH" of
// the leading byte with length 1 so the caller advances deterministically
// without invoking decoder UB on the truncated/invalid tail.
// #2442: used by the "unexpected token" fallback and the trailing-ident
// check after a numeric literal so the diagnostic names the actual code
// point instead of the leading byte alone (which rendered as U+FFFD).
static std::pair<std::string, size_t> decodeUtf8Char(const std::string &src, size_t pos) {
    unsigned char c = static_cast<unsigned char>(src[pos]);
    int len = 0;
    if (c < 0x80u) len = 1;
    else if (c >= 0xC2u && c <= 0xDFu) len = 2;
    else if (c >= 0xE0u && c <= 0xEFu) len = 3;
    else if (c >= 0xF0u && c <= 0xF4u) len = 4;
    auto hexEscape = [c]() -> std::string {
        static const char hex[] = "0123456789ABCDEF";
        std::string s = "\\x";
        s += hex[(c >> 4) & 0xFu];
        s += hex[c & 0xFu];
        return s;
    };
    if (len == 0 || pos + static_cast<size_t>(len) > src.size())
        return {hexEscape(), 1};
    for (size_t i = 1; i < static_cast<size_t>(len); ++i) {
        unsigned char cc = static_cast<unsigned char>(src[pos + i]);
        if ((cc & 0xC0u) != 0x80u) return {hexEscape(), 1};
    }
    if (len == 3) {
        unsigned char c1 = static_cast<unsigned char>(src[pos + 1]);
        if ((c == 0xE0u && c1 < 0xA0u) || (c == 0xEDu && c1 > 0x9Fu))
            return {hexEscape(), 1};
    } else if (len == 4) {
        unsigned char c1 = static_cast<unsigned char>(src[pos + 1]);
        if ((c == 0xF0u && c1 < 0x90u) || (c == 0xF4u && c1 > 0x8Fu))
            return {hexEscape(), 1};
    }
    return {src.substr(pos, static_cast<size_t>(len)), static_cast<size_t>(len)};
}

// Append a Unicode scalar value as UTF-8 bytes to `dst`. The caller is
// responsible for ensuring `cp <= 0x10FFFF` and `cp` is not a surrogate
// (see decodeUnicodeEscape, which validates before calling).
static void appendUtf8(std::string &dst, uint32_t cp) {
    if (cp < 0x80u) {
        dst.push_back(static_cast<char>(cp));
    } else if (cp < 0x800u) {
        dst.push_back(static_cast<char>(0xC0u | (cp >> 6)));
        dst.push_back(static_cast<char>(0x80u | (cp & 0x3Fu)));
    } else if (cp < 0x10000u) {
        dst.push_back(static_cast<char>(0xE0u | (cp >> 12)));
        dst.push_back(static_cast<char>(0x80u | ((cp >> 6) & 0x3Fu)));
        dst.push_back(static_cast<char>(0x80u | (cp & 0x3Fu)));
    } else {
        dst.push_back(static_cast<char>(0xF0u | (cp >> 18)));
        dst.push_back(static_cast<char>(0x80u | ((cp >> 12) & 0x3Fu)));
        dst.push_back(static_cast<char>(0x80u | ((cp >> 6) & 0x3Fu)));
        dst.push_back(static_cast<char>(0x80u | (cp & 0x3Fu)));
    }
}

// Decode a `\u{HHHH}` escape. On entry, `pos` is positioned at the `{`
// (the caller has already consumed the leading `\` and `u`). On normal
// return, the parsed UTF-8 bytes are appended to `dst` and `pos`/`col`
// are advanced past the last hex digit so that `pos` points AT the
// closing `}` — the caller's trailing `++pos; ++col` (which always
// follows the escape switch) then consumes the `}`. This matches the
// single-character escape contract used by the surrounding code.
//
// Rejects: missing `{`, missing `}`, empty `\u{}`, non-hex characters,
// more than 6 hex digits, surrogate range `0xD800..0xDFFF`, and values
// above `0x10FFFF`.
static void decodeUnicodeEscape(const std::string &src, size_t &pos,
                                int &col, int line, std::string &dst) {
    if (pos >= src.size() || src[pos] != '{') {
        throw std::runtime_error("line " + std::to_string(line) +
                                 ": expected '{' after '\\u' in unicode escape");
    }
    ++pos; ++col;  // consume '{'
    size_t start = pos;
    uint32_t cp = 0;
    while (pos < src.size() && src[pos] != '}') {
        char ch = src[pos];
        if (pos - start >= 6) {
            throw std::runtime_error("line " + std::to_string(line) +
                                     ": unicode escape has too many digits (max 6)");
        }
        uint32_t digit;
        if (ch >= '0' && ch <= '9') {
            digit = static_cast<uint32_t>(ch - '0');
        } else if (ch >= 'a' && ch <= 'f') {
            digit = static_cast<uint32_t>(ch - 'a') + 10u;
        } else if (ch >= 'A' && ch <= 'F') {
            digit = static_cast<uint32_t>(ch - 'A') + 10u;
        } else {
            // #2442: decode the offending code point (or fall back to the
            // "\xHH" hex escape for invalid bytes) so a non-ASCII char like
            // `α` inside `\u{...}` renders as itself instead of U+FFFD.
            auto [s, len] = decodeUtf8Char(src, pos);
            (void)len;
            throw std::runtime_error("line " + std::to_string(line) +
                                     ": invalid hex digit '" + s +
                                     "' in unicode escape");
        }
        cp = (cp << 4) | digit;
        ++pos; ++col;
    }
    if (pos >= src.size()) {
        throw std::runtime_error("line " + std::to_string(line) +
                                 ": unterminated unicode escape (missing '}')");
    }
    if (pos == start) {
        throw std::runtime_error("line " + std::to_string(line) +
                                 ": empty unicode escape '\\u{}'");
    }
    if (cp > 0x10FFFFu) {
        throw std::runtime_error("line " + std::to_string(line) +
                                 ": unicode code point out of range (max 0x10FFFF)");
    }
    if (cp >= 0xD800u && cp <= 0xDFFFu) {
        throw std::runtime_error("line " + std::to_string(line) +
                                 ": unicode code point in surrogate range");
    }
    appendUtf8(dst, cp);
    // Leave `pos` AT the closing '}'. The caller's trailing `++pos; ++col`
    // (after the surrounding escape switch) consumes the '}'.
}

// Decode a `\xNN` escape. On entry, `pos` is positioned at the first hex
// digit (the caller has already consumed the leading `\` and `x`). Exactly
// two hex digits are required, producing a single raw byte (0x00 - 0xFF)
// appended to `dst`. On normal return, `pos`/`col` are advanced to the
// SECOND hex digit so that the caller's trailing `++pos; ++col` (which
// always follows the escape switch) consumes it — matching the single-
// character escape contract used by the surrounding code.
//
// Unlike `\u{...}`, the result is NOT UTF-8 encoded; `\xFF` produces a
// single 0xFF byte even though that byte is not valid UTF-8 standalone.
//
// Rejects: EOF mid-escape, non-hex digit at either position.
static void decodeHexEscape(const std::string &src, size_t &pos,
                            int &col, int line, std::string &dst) {
    auto hexValue = [](char ch) -> int {
        if (ch >= '0' && ch <= '9') return ch - '0';
        if (ch >= 'a' && ch <= 'f') return (ch - 'a') + 10;
        if (ch >= 'A' && ch <= 'F') return (ch - 'A') + 10;
        return -1;
    };
    if (pos >= src.size()) {
        throw std::runtime_error("line " + std::to_string(line) +
                                 ": incomplete '\\x' escape: expected 2 hex digits");
    }
    int hi = hexValue(src[pos]);
    if (hi < 0) {
        // #2442: same UTF-8-aware rendering as decodeUnicodeEscape.
        auto [s, len] = decodeUtf8Char(src, pos);
        (void)len;
        throw std::runtime_error("line " + std::to_string(line) +
                                 ": invalid hex digit '" + s +
                                 "' in '\\x' escape");
    }
    ++pos; ++col;
    if (pos >= src.size()) {
        throw std::runtime_error("line " + std::to_string(line) +
                                 ": incomplete '\\x' escape: expected 2 hex digits");
    }
    int lo = hexValue(src[pos]);
    if (lo < 0) {
        auto [s, len] = decodeUtf8Char(src, pos);
        (void)len;
        throw std::runtime_error("line " + std::to_string(line) +
                                 ": invalid hex digit '" + s +
                                 "' in '\\x' escape");
    }
    dst.push_back(static_cast<char>((hi << 4) | lo));
    // Leave `pos` AT the second hex digit. The caller's trailing
    // `++pos; ++col` (after the surrounding escape switch) consumes it.
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
    {"using",     TokenKind::Using},
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
        unsigned char ch = static_cast<unsigned char>(src_[pos_]);
        // #2442: include the offending character in the message. The
        // predicate stays ASCII-only on purpose — non-ASCII follow-ons
        // (`123ℕ` / `123α`) intentionally fall through to the default
        // fallback so the parser surfaces them as a proper DiagnosticError
        // with a source snippet + caret, not a SourceManager-less
        // std::runtime_error here.
        if (std::isalpha(ch) || ch == '_') {
            auto [s, len] = decodeUtf8Char(src_, pos_);
            (void)len;
            throw std::runtime_error(
                "line " + std::to_string(line_) +
                ": invalid character '" + s + "' after numeric literal");
        }
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
    // #2137: comment-only lines are skipped via `continue` on this outer
    // loop (was `return readToken()` until ASan caught a stack overflow
    // on long runs of consecutive comment lines). Every non-skip path
    // either `return`s a token or falls through to the body emit below.
    for (;;) {
    // 1. Return pending tokens (multiple DEDENTs)
    if (!pending_.empty()) {
        Token t = pending_.front();
        pending_.pop();
        return t;
    }

    // Snapshot whether this iteration entered at line start; Step 2 clears
    // the member immediately, so we need a local copy to drive the
    // comment-only line transparency in Step 4 (#2137).
    bool entered_at_line_start = at_line_start_;

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
        // #2137: comment-only lines (entered_at_line_start && first
        // non-whitespace was `#`) must be transparent at the token level
        // so multiline UFCS chains can carry mid-chain comments. Suppress
        // the trailing Newline and restart the outer loop to process the
        // next line. mid-line comments (`foo() # x`) have
        // entered_at_line_start=false and fall through to the normal
        // Newline emit. Indent/Dedent are unaffected because
        // `has_content == false` already gates Step 2.
        if (entered_at_line_start && pos_ < src_.size() &&
            (src_[pos_] == '\n' || src_[pos_] == '\r')) {
            if (src_[pos_] == '\r' && pos_ + 1 < src_.size() &&
                src_[pos_ + 1] == '\n') {
                ++pos_;
            }
            ++pos_;
            ++line_;
            col_ = 1;
            at_line_start_ = true;
            continue;
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
            prev_kind_ != TokenKind::String && prev_kind_ != TokenKind::BlockString &&
            prev_kind_ != TokenKind::FStringEnd &&
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
            prev_kind_ != TokenKind::BlockString &&
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

    // Block string: """..."""
    // Detected before the regular '"' branch so that """ is not consumed as
    // an opening "" followed by a stray ". The block string lexer spans
    // multiple lines, processes escape sequences identically to a regular
    // string, and normalizes indentation per the closing delimiter.
    if (c == '"' && pos_ + 2 < src_.size() &&
        src_[pos_ + 1] == '"' && src_[pos_ + 2] == '"') {
        pos_ += 3; col_ += 3;
        std::string raw;
        while (pos_ < src_.size()) {
            // Closing """ ?
            if (src_[pos_] == '"' && pos_ + 2 < src_.size() &&
                src_[pos_ + 1] == '"' && src_[pos_ + 2] == '"') {
                break;
            }
            if (src_[pos_] == '\\') {
                ++pos_; ++col_;
                if (pos_ >= src_.size())
                    throw std::runtime_error("line " + std::to_string(line_) +
                                             ": unterminated escape sequence");
                switch (src_[pos_]) {
                    case 'n':  raw += '\n'; break;
                    case 'r':  raw += '\r'; break;
                    case 't':  raw += '\t'; break;
                    case '\\': raw += '\\'; break;
                    case '"':  raw += '"';  break;
                    case '0':  raw += '\0'; break;
                    case 'u':
                        ++pos_; ++col_;  // past 'u'; helper expects '{'
                        decodeUnicodeEscape(src_, pos_, col_, line_, raw);
                        break;
                    case 'x':
                        ++pos_; ++col_;  // past 'x'; helper expects 1st hex digit
                        decodeHexEscape(src_, pos_, col_, line_, raw);
                        break;
                    default:
                        throw std::runtime_error("line " + std::to_string(line_) +
                                                 ": unknown escape sequence '\\" +
                                                 std::string(1, src_[pos_]) + "'");
                }
                ++pos_; ++col_;
            } else if (src_[pos_] == '\r') {
                raw += '\n';
                ++pos_;
                if (pos_ < src_.size() && src_[pos_] == '\n') ++pos_;
                ++line_;
                col_ = 1;
            } else if (src_[pos_] == '\n') {
                raw += '\n';
                ++pos_;
                ++line_;
                col_ = 1;
            } else {
                raw += src_[pos_];
                ++pos_; ++col_;
            }
        }
        if (pos_ >= src_.size())
            throw std::runtime_error("line " + std::to_string(line_) +
                                     ": unterminated block string literal");

        // Determine baseline indent from trailing run of spaces on the
        // closing line. If the closing """ is not preceded by a newline
        // (mid-line closing), baseline stays 0 and no leading-space strip
        // is applied to any content line.
        int baseline = 0;
        {
            size_t s = raw.size();
            while (s > 0 && raw[s - 1] == ' ') --s;
            if (s == 0 || raw[s - 1] == '\n') {
                baseline = static_cast<int>(raw.size() - s);
                raw.resize(s);
            }
        }

        size_t startIdx = 0;
        if (startIdx < raw.size() && raw[startIdx] == '\n') ++startIdx;
        size_t endIdx = raw.size();
        if (endIdx > startIdx && raw[endIdx - 1] == '\n') --endIdx;

        std::string normalized;
        bool firstLine = true;
        size_t i = startIdx;
        for (;;) {
            size_t lineEnd = i;
            while (lineEnd < endIdx && raw[lineEnd] != '\n') ++lineEnd;
            if (!firstLine) normalized += '\n';
            firstLine = false;
            size_t ls = i;
            int stripped = 0;
            while (ls < lineEnd && stripped < baseline && raw[ls] == ' ') {
                ++ls;
                ++stripped;
            }
            normalized.append(raw, ls, lineEnd - ls);
            if (lineEnd >= endIdx) break;
            i = lineEnd + 1;
        }

        pos_ += 3; col_ += 3;
        at_line_start_ = false;
        return {TokenKind::BlockString, std::move(normalized), line_, startCol};
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
                    case 'u':
                        ++pos_; ++col_;  // past 'u'; helper expects '{'
                        decodeUnicodeEscape(src_, pos_, col_, line_, str);
                        break;
                    case 'x':
                        ++pos_; ++col_;  // past 'x'; helper expects 1st hex digit
                        decodeHexEscape(src_, pos_, col_, line_, str);
                        break;
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
        // Fraction part (e.g., 3.14). Suppress when the integer literal
        // directly follows a Dot (e.g. `t.0.0`) so nested tuple/record
        // field access lexes as Number-Dot-Number, not Float.
        if (pos_ < src_.size() && src_[pos_] == '.' &&
            pos_ + 1 < src_.size() && std::isdigit(static_cast<unsigned char>(src_[pos_ + 1])) &&
            prev_kind_ != TokenKind::Dot) {
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
        // but not '!=' (not-equal).
        if (pos_ < src_.size() && src_[pos_] == '!' &&
            (pos_ + 1 >= src_.size() || src_[pos_ + 1] != '=')) {
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

    // #2442: a non-ASCII byte at this point is a UTF-8 lead byte. Consume
    // the full sequence so the Error token carries the actual code point
    // (not a single byte that the terminal renders as U+FFFD), and so the
    // trailing continuation bytes don't cascade as per-byte Error tokens.
    auto [val, len] = decodeUtf8Char(src_, pos_);
    pos_ += len;
    ++col_;
    return {TokenKind::Error, std::move(val), line_, startCol};
    }  // close `for (;;)` opened in Step 1 (#2137)
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
                case 'u':
                    ++pos_; ++col_;  // past 'u'; helper expects '{'
                    decodeUnicodeEscape(src_, pos_, col_, line_, str);
                    break;
                case 'x':
                    ++pos_; ++col_;  // past 'x'; helper expects 1st hex digit
                    decodeHexEscape(src_, pos_, col_, line_, str);
                    break;
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
