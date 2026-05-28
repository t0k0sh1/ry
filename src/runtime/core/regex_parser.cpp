#include "ry/runtime/core/regex_internal.hpp"


namespace ry {

// ============================================================
// RegexParser implementation
// ============================================================

RegexParser::RegexParser(const char *pattern, size_t len)
    : src_(pattern), len_(len), pos_(0) {}

RegexNodePtr RegexParser::parse() {
    auto node = parseAlternation();
    if (pos_ < len_) {
        fail("regex error: unexpected character '%c' at position %zu in pattern '%.*s'",
             src_[pos_], pos_, (int)len_, src_);
    }
    return node;
}

char RegexParser::peek() const {
    if (pos_ >= len_) return '\0';
    return src_[pos_];
}

char RegexParser::advance() {
    return src_[pos_++];
}

bool RegexParser::atEnd() const {
    return pos_ >= len_;
}

[[noreturn]] void RegexParser::fail(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    char buf[512];
    vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);
    throw std::runtime_error(buf);
}

RegexNodePtr RegexParser::parseAlternation() {
    auto left = parseConcat();
    if (peek() == '|') {
        auto node = std::make_unique<RegexNode>();
        node->kind = RegexNodeKind::Alternation;
        node->children.push_back(std::move(left));
        while (peek() == '|') {
            advance(); // consume '|'
            node->children.push_back(parseConcat());
        }
        return node;
    }
    return left;
}

RegexNodePtr RegexParser::parseConcat() {
    std::vector<RegexNodePtr> parts;
    while (!atEnd() && peek() != '|' && peek() != ')') {
        parts.push_back(parseRepeat());
    }
    if (parts.empty()) {
        // Empty concat = match empty string (literal with special handling)
        auto node = std::make_unique<RegexNode>();
        node->kind = RegexNodeKind::Concat;
        return node;
    }
    if (parts.size() == 1) return std::move(parts[0]);
    auto node = std::make_unique<RegexNode>();
    node->kind = RegexNodeKind::Concat;
    for (auto &p : parts) node->children.push_back(std::move(p));
    return node;
}

RegexNodePtr RegexParser::parseRepeat() {
    auto atom = parseAtom();
    if (atEnd()) return atom;

    int rmin = -1, rmax = -1;
    bool hasQuantifier = false;

    char c = peek();
    if (c == '*') {
        advance();
        rmin = 0; rmax = -1;
        hasQuantifier = true;
    } else if (c == '+') {
        advance();
        rmin = 1; rmax = -1;
        hasQuantifier = true;
    } else if (c == '?') {
        advance();
        rmin = 0; rmax = 1;
        hasQuantifier = true;
    } else if (c == '{') {
        size_t saved = pos_;
        if (parseQuantifierBrace(rmin, rmax)) {
            hasQuantifier = true;
        } else {
            pos_ = saved; // fallback: treat '{' as literal
        }
    }

    if (hasQuantifier) {
        auto node = std::make_unique<RegexNode>();
        node->kind = RegexNodeKind::Repeat;
        node->repeatMin = rmin;
        node->repeatMax = rmax;
        node->greedy = true;
        // Non-greedy suffix '?'
        if (!atEnd() && peek() == '?') {
            advance();
            node->greedy = false;
        }
        node->children.push_back(std::move(atom));
        return node;
    }
    return atom;
}

// Try to parse {n}, {n,}, {n,m}. Returns true on success.
bool RegexParser::parseQuantifierBrace(int &rmin, int &rmax) {
    advance(); // consume '{'
    if (atEnd() || !std::isdigit(static_cast<unsigned char>(peek()))) return false;
    int n = parseNumber();
    if (n > 1000) {
        fail("regex error: quantifier value %d exceeds maximum (1000) in pattern '%.*s'",
             n, (int)len_, src_);
    }
    if (atEnd()) return false;
    if (peek() == '}') {
        advance();
        rmin = n; rmax = n;
        return true;
    }
    if (peek() == ',') {
        advance();
        if (atEnd()) return false;
        if (peek() == '}') {
            advance();
            rmin = n; rmax = -1;
            return true;
        }
        if (!std::isdigit(static_cast<unsigned char>(peek()))) return false;
        int m = parseNumber();
        if (m > 1000) {
            fail("regex error: quantifier value %d exceeds maximum (1000) in pattern '%.*s'",
                 m, (int)len_, src_);
        }
        if (atEnd() || peek() != '}') return false;
        advance();
        if (n > m) {
            fail("regex error: invalid quantifier {%d,%d} in pattern '%.*s'",
                 n, m, (int)len_, src_);
        }
        rmin = n; rmax = m;
        return true;
    }
    return false;
}

int RegexParser::parseNumber() {
    int val = 0;
    while (!atEnd() && std::isdigit(static_cast<unsigned char>(peek()))) {
        val = val * 10 + (advance() - '0');
        if (val > 1000) return val; // early exit on overflow
    }
    return val;
}

RegexNodePtr RegexParser::parseAtom() {
    char c = peek();
    if (c == '(') {
        if (++groupDepth_ > MAX_GROUP_DEPTH) {
            fail("regex error: group nesting too deep (limit: %d) in pattern '%.*s'",
                 MAX_GROUP_DEPTH, (int)len_, src_);
        }
        advance(); // consume '('
        // Check for inline flags (?i)
        if (peek() == '?' && pos_ + 1 < len_ && src_[pos_ + 1] == 'i') {
            advance(); // consume '?'
            advance(); // consume 'i'
            if (peek() != ')') {
                fail("regex error: expected ')' after '(?i' in pattern '%.*s'",
                     (int)len_, src_);
            }
            advance(); // consume ')'
            --groupDepth_;
            caseInsensitive_ = true;
            // Return next atom (flag is stateful, not a node)
            return parseAtom();
        }
        auto inner = parseAlternation();
        if (peek() != ')') {
            fail("regex error: unmatched '(' in pattern '%.*s'", (int)len_, src_);
        }
        advance(); // consume ')'
        --groupDepth_;
        auto node = std::make_unique<RegexNode>();
        node->kind = RegexNodeKind::Group;
        node->groupIndex = ++groupCount_;
        node->children.push_back(std::move(inner));
        return node;
    }
    if (c == '[') {
        return parseCharClass();
    }
    if (c == '.') {
        advance();
        auto node = std::make_unique<RegexNode>();
        node->kind = RegexNodeKind::Dot;
        return node;
    }
    if (c == '^' || c == '$') {
        advance();
        auto node = std::make_unique<RegexNode>();
        node->kind = RegexNodeKind::Anchor;
        node->ch = c;
        return node;
    }
    if (c == '\\') {
        advance(); // consume backslash
        if (atEnd()) {
            fail("regex error: trailing backslash in pattern '%.*s'", (int)len_, src_);
        }
        char escaped = advance();
        // Handle word boundary
        if (escaped == 'b' || escaped == 'B') {
            auto node = std::make_unique<RegexNode>();
            node->kind = RegexNodeKind::WordBoundary;
            node->negated = (escaped == 'B');
            return node;
        }
        // Handle shorthand character classes
        if (escaped == 'd' || escaped == 'D' ||
            escaped == 'w' || escaped == 'W' ||
            escaped == 's' || escaped == 'S') {
            return parseShorthandClass(escaped);
        }
        auto node = std::make_unique<RegexNode>();
        node->kind = RegexNodeKind::Literal;
        node->ch = escaped;
        return node;
    }
    if (c == '*' || c == '+' || c == '?') {
        fail("regex error: nothing to repeat at position %zu in pattern '%.*s'",
             pos_, (int)len_, src_);
    }
    if (atEnd() || c == '|' || c == ')') {
        fail("regex error: unexpected end or character in pattern '%.*s'",
             (int)len_, src_);
    }
    // Regular literal
    advance();
    auto node = std::make_unique<RegexNode>();
    node->kind = RegexNodeKind::Literal;
    node->ch = c;
    return node;
}

RegexNodePtr RegexParser::parseShorthandClass(char code) {
    auto node = std::make_unique<RegexNode>();
    node->kind = RegexNodeKind::CharClass;
    switch (code) {
        case 'd': node->negated = false; node->ranges = {{'0','9'}}; break;
        case 'D': node->negated = true;  node->ranges = {{'0','9'}}; break;
        case 'w': node->negated = false; node->ranges = WORD_CHAR_RANGES; break;
        case 'W': node->negated = true;  node->ranges = WORD_CHAR_RANGES; break;
        case 's': node->negated = false; node->ranges = {{' ',' '},{'\t','\t'},{'\n','\n'},{'\r','\r'},{'\f','\f'}}; break;
        case 'S': node->negated = true;  node->ranges = {{' ',' '},{'\t','\t'},{'\n','\n'},{'\r','\r'},{'\f','\f'}}; break;
        default: break;
    }
    return node;
}

RegexNodePtr RegexParser::parseCharClass() {
    advance(); // consume '['
    auto node = std::make_unique<RegexNode>();
    node->kind = RegexNodeKind::CharClass;
    node->negated = false;
    if (peek() == '^') {
        node->negated = true;
        advance();
    }
    while (!atEnd() && peek() != ']') {
        char lo = parseClassChar();
        if (peek() == '-' && pos_ + 1 < len_ && src_[pos_ + 1] != ']') {
            advance(); // consume '-'
            char hi = parseClassChar();
            node->ranges.push_back({lo, hi});
        } else {
            node->ranges.push_back({lo, lo});
        }
    }
    if (peek() != ']') {
        fail("regex error: unmatched '[' in pattern '%.*s'", (int)len_, src_);
    }
    advance(); // consume ']'
    return node;
}

char RegexParser::parseClassChar() {
    if (peek() == '\\') {
        advance();
        if (atEnd()) {
            fail("regex error: trailing backslash in character class in pattern '%.*s'",
                 (int)len_, src_);
        }
        char c = advance();
        switch (c) {
            case 'n': return '\n';
            case 't': return '\t';
            case 'r': return '\r';
            default: return c;
        }
    }
    return advance();
}

} // namespace ry
