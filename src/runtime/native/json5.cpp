#include "ry/runtime/native/json5.hpp"
#include "ry/runtime/core/alloc.hpp"
#include "ry/runtime/native/io.hpp"
#include "ry/runtime/core/arc.hpp"
#include "ry/runtime/core/any_typed_coll.hpp"
#include "ry/runtime/core/list.hpp"
#include "ry/runtime/core/string.hpp"
#include "ry/runtime/native/http/http_types.hpp" // MapHeader + __ry_ht_rehash_str

#include <algorithm>
#include <cctype>
#include <cerrno>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <limits>
#include <numeric>
#include <string>
#include <vector>


namespace ry {

// ===== JSON5 string escape helper (used by stringify_any) =====
//
// Identical to json.cpp's escape_string — we emit strict-JSON-compatible
// strings even from json5.stringify so the output is always valid JSON.

static void escape_string(const char *s, int64_t len, std::string &out) {
    for (int64_t i = 0; i < len; i++) {
        unsigned char c = (unsigned char)s[i];
        switch (c) {
            case '"': out += "\\\""; break;
            case '\\': out += "\\\\"; break;
            case '\b': out += "\\b"; break;
            case '\f': out += "\\f"; break;
            case '\n': out += "\\n"; break;
            case '\r': out += "\\r"; break;
            case '\t': out += "\\t"; break;
            default:
                if (c < 0x20) {
                    char esc[8];
                    snprintf(esc, sizeof(esc), "\\u%04x", c);
                    out += esc;
                } else {
                    out += (char)c;
                }
        }
    }
}


// ===== `any`-based parser / stringify (JSON5; #1855) =====
//
// Forward declarations for the release helpers (mutual recursion through
// `releaseOwnedAny`).
static void releaseOwnedAny(RyAny &v);

static void releaseOwnedList(void *list_header_ptr) {
    if (!list_header_ptr) return;
    auto *hdr = static_cast<ListHeader *>(list_header_ptr);
    if (hdr->data) {
        auto *items = reinterpret_cast<RyAny *>(hdr->data);
        for (int64_t i = 0; i < hdr->len; ++i) releaseOwnedAny(items[i]);
        free(hdr->data);
    }
    arc_free(hdr);
}

static void releaseOwnedMap(void *map_header_ptr) {
    if (!map_header_ptr) return;
    auto *hdr = static_cast<MapHeader *>(map_header_ptr);
    if (hdr->keys) {
        for (int64_t i = 0; i < hdr->len; ++i) freeStringSlot(hdr->keys[i]);
        free(hdr->keys);
    }
    if (hdr->vals) {
        auto *vals = reinterpret_cast<RyAny *>(hdr->vals);
        for (int64_t i = 0; i < hdr->len; ++i) releaseOwnedAny(vals[i]);
        free(hdr->vals);
    }
    if (hdr->buckets) free(hdr->buckets);
    arc_free(hdr);
}

static void releaseOwnedAny(RyAny &v) {
    switch (static_cast<RyAnyTag>(v.tag)) {
        case RyAnyTag::Str: {
            char *handle = nullptr;
            memcpy(&handle, v.data, sizeof(handle));
            freeStringSlot(handle);
            break;
        }
        case RyAnyTag::List: {
            void *ptr = nullptr;
            memcpy(&ptr, v.data, sizeof(ptr));
            releaseOwnedList(ptr);
            break;
        }
        case RyAnyTag::Map: {
            void *ptr = nullptr;
            memcpy(&ptr, v.data, sizeof(ptr));
            releaseOwnedMap(ptr);
            break;
        }
        default: break; // Int/Float/Bool/Unit: no-op
    }
}

extern "C" void __ry_json5_release_any(RyAny *value) {
    if (value) releaseOwnedAny(*value);
}

// TU-local parser type. Named `Json5AnyParser` (not `Parser`) per
// runtime-memory-safety rule: TU-local types in namespace ry must not
// shadow public class names from include/ry/*.hpp.
struct Json5AnyParser {
    static constexpr int MAX_NESTING_DEPTH = 256;

    const char *src;
    size_t pos;
    size_t src_len;
    std::string error;
    int depth = 0;

    explicit Json5AnyParser(const char *text)
        : src(text), pos(0), src_len((size_t)stringByteLen(text)) {}

    struct DepthGuard {
        Json5AnyParser *p;
        explicit DepthGuard(Json5AnyParser *parser) : p(parser) { ++p->depth; }
        ~DepthGuard() { --p->depth; }
        DepthGuard(const DepthGuard&) = delete;
        DepthGuard& operator=(const DepthGuard&) = delete;
    };

    char peek() const { return src[pos]; }
    char advance() { return src[pos++]; }
    bool at_end() const { return pos >= src_len; }

    // Format byte_pos as "line L, column C (offset O)" — same format as
    // json.cpp's formatPosition for diagnostic-message parity.
    std::string formatPosition(size_t byte_pos) const {
        size_t line = 1, col = 1;
        for (size_t i = 0; i < byte_pos && i < src_len; ++i) {
            unsigned char c = (unsigned char)src[i];
            if (c == '\n') {
                line++;
                col = 1;
            } else if ((c & 0xC0) != 0x80) {
                col++;
            }
        }
        std::string s = "line ";
        s += std::to_string(line);
        s += ", column ";
        s += std::to_string(col);
        s += " (offset ";
        s += std::to_string(byte_pos);
        s += ")";
        return s;
    }

    // JSON5 whitespace + comments. Per spec WhiteSpace includes \t \v \f
    // SP NBSP and Unicode Zs; for the MVP we accept the ASCII subset plus
    // \n \r (line terminators). Block comments do NOT nest.
    bool skip_ws_and_comments() {
        while (!at_end()) {
            char c = src[pos];
            if (c == ' ' || c == '\t' || c == '\n' || c == '\r' ||
                c == '\v' || c == '\f') {
                pos++;
                continue;
            }
            if (c == '/' && pos + 1 < src_len) {
                char n = src[pos + 1];
                if (n == '/') {
                    pos += 2;
                    while (!at_end() && src[pos] != '\n' && src[pos] != '\r')
                        pos++;
                    continue;
                }
                if (n == '*') {
                    size_t open_pos = pos;
                    pos += 2;
                    bool closed = false;
                    while (pos + 1 < src_len) {
                        if (src[pos] == '*' && src[pos + 1] == '/') {
                            pos += 2;
                            closed = true;
                            break;
                        }
                        pos++;
                    }
                    if (!closed) {
                        error = "unterminated block comment at ";
                        error += formatPosition(open_pos);
                        return false;
                    }
                    continue;
                }
            }
            break;
        }
        return true;
    }

    bool expect(char c) {
        if (!skip_ws_and_comments()) return false;
        if (at_end() || src[pos] != c) {
            error = "expected '";
            error += c;
            error += "' at ";
            error += formatPosition(pos);
            return false;
        }
        pos++;
        return true;
    }

    // ECMAScript IdentifierStart (ASCII subset): letter, underscore, dollar.
    static bool is_id_start(char c) {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
            || c == '_' || c == '$';
    }
    // ECMAScript IdentifierPart (ASCII subset): id_start or digit.
    static bool is_id_part(char c) {
        return is_id_start(c) || (c >= '0' && c <= '9');
    }
    static int hex_digit_value(char c) {
        if (c >= '0' && c <= '9') return c - '0';
        if (c >= 'a' && c <= 'f') return 10 + (c - 'a');
        if (c >= 'A' && c <= 'F') return 10 + (c - 'A');
        return -1;
    }

    bool parse_value(RyAny &out) {
        if (!skip_ws_and_comments()) return false;
        if (at_end()) {
            error = "unexpected end of input at ";
            error += formatPosition(pos);
            return false;
        }
        char c = peek();
        if (c == '"' || c == '\'') return parse_string(out);
        if (c == '{') return parse_object(out);
        if (c == '[') return parse_array(out);
        if (c == 't' || c == 'f') return parse_bool(out);
        if (c == 'n') return parse_null(out);
        if (c == '-' || c == '+' || c == '.' || c == 'I' || c == 'N'
            || (c >= '0' && c <= '9')) return parse_number(out);
        error = "unexpected character '";
        error += c;
        error += "' at ";
        error += formatPosition(pos);
        return false;
    }

    // Append decoded UTF-8 bytes of a JSON5 string body to `out`.
    // Accepts both `"` and `'` quotes (chosen by the opening quote);
    // accepts `\<LineTerminator>` for line continuation; accepts
    // `\xHH` byte escape in addition to JSON's `\uHHHH`.
    bool parse_string_bytes(std::string &out) {
        size_t open_pos = pos;
        char quote = src[pos];
        pos++; // skip opening quote

        // Fast path: scan for closing quote without backslash or LF/CR
        size_t scan = pos;
        while (scan < src_len && src[scan] != quote && src[scan] != '\\'
               && src[scan] != '\n' && src[scan] != '\r') {
            if (static_cast<unsigned char>(src[scan]) < 0x20) {
                error = "unescaped control character in string at ";
                error += formatPosition(scan);
                return false;
            }
            scan++;
        }
        if (scan < src_len && src[scan] == quote) {
            out.assign(src + pos, scan - pos);
            pos = scan + 1;
            return true;
        }

        // Slow path: contains escape sequences or unescaped LF/CR
        if (scan > pos) {
            out.append(src + pos, scan - pos);
            pos = scan;
        }
        while (!at_end()) {
            char c = advance();
            if (c == quote) return true;
            if (c == '\n' || c == '\r') {
                error = "unescaped line terminator in string at ";
                error += formatPosition(pos - 1);
                return false;
            }
            if (c == '\\') {
                if (at_end()) {
                    error = "unterminated string escape at ";
                    error += formatPosition(pos - 1);
                    return false;
                }
                char esc = advance();
                switch (esc) {
                    case '"': out += '"'; break;
                    case '\'': out += '\''; break;
                    case '\\': out += '\\'; break;
                    case '/': out += '/'; break;
                    case 'b': out += '\b'; break;
                    case 'f': out += '\f'; break;
                    case 'n': out += '\n'; break;
                    case 'r': out += '\r'; break;
                    case 't': out += '\t'; break;
                    case 'v': out += '\v'; break;
                    case '0':
                        // JSON5 spec: \0 produces NUL; a following digit
                        // would form an octal escape, which JSON5 forbids.
                        if (!at_end() && src[pos] >= '0' && src[pos] <= '9') {
                            error = "octal escape is not allowed at ";
                            error += formatPosition(pos - 2);
                            return false;
                        }
                        out += '\0';
                        break;
                    case '\n':
                        // line continuation — no output bytes
                        break;
                    case '\r':
                        // line continuation: skip optional following \n
                        if (!at_end() && src[pos] == '\n') pos++;
                        break;
                    case 'x': {
                        if (pos + 2 > src_len) {
                            error = "incomplete \\x escape at ";
                            error += formatPosition(pos);
                            return false;
                        }
                        int hi = hex_digit_value(src[pos]);
                        int lo = hex_digit_value(src[pos + 1]);
                        if (hi < 0 || lo < 0) {
                            error = "invalid hex digit in \\x escape at ";
                            error += formatPosition(pos);
                            return false;
                        }
                        pos += 2;
                        out += (char)((hi << 4) | lo);
                        break;
                    }
                    case 'u': {
                        auto parse_hex4 = [&](unsigned &cp_out) -> bool {
                            if (pos + 4 > src_len) {
                                error = "incomplete unicode escape at ";
                                error += formatPosition(pos);
                                return false;
                            }
                            unsigned v = 0;
                            for (size_t hi = 0; hi < 4; hi++) {
                                int d = hex_digit_value(src[pos + hi]);
                                if (d < 0) {
                                    error = "invalid hex digit in unicode escape at ";
                                    error += formatPosition(pos + static_cast<size_t>(hi));
                                    return false;
                                }
                                v = (v << 4) | (unsigned)d;
                            }
                            pos += 4;
                            cp_out = v;
                            return true;
                        };
                        unsigned cp;
                        if (!parse_hex4(cp)) return false;
                        // Surrogate pair handling — identical to json.cpp.
                        if (cp >= 0xD800 && cp <= 0xDBFF) {
                            if (pos + 2 > src_len || src[pos] != '\\' || src[pos + 1] != 'u') {
                                error = "unpaired high surrogate in unicode escape at ";
                                error += formatPosition(pos);
                                return false;
                            }
                            pos += 2;
                            unsigned low;
                            if (!parse_hex4(low)) return false;
                            if (low < 0xDC00 || low > 0xDFFF) {
                                error = "invalid low surrogate in unicode escape at ";
                                error += formatPosition(pos);
                                return false;
                            }
                            cp = 0x10000 + ((cp - 0xD800) << 10) + (low - 0xDC00);
                        } else if (cp >= 0xDC00 && cp <= 0xDFFF) {
                            error = "unpaired low surrogate in unicode escape at ";
                            error += formatPosition(pos);
                            return false;
                        }
                        if (cp < 0x80) {
                            out += (char)cp;
                        } else if (cp < 0x800) {
                            out += (char)(0xC0 | (cp >> 6));
                            out += (char)(0x80 | (cp & 0x3F));
                        } else if (cp < 0x10000) {
                            out += (char)(0xE0 | (cp >> 12));
                            out += (char)(0x80 | ((cp >> 6) & 0x3F));
                            out += (char)(0x80 | (cp & 0x3F));
                        } else {
                            out += (char)(0xF0 | (cp >> 18));
                            out += (char)(0x80 | ((cp >> 12) & 0x3F));
                            out += (char)(0x80 | ((cp >> 6) & 0x3F));
                            out += (char)(0x80 | (cp & 0x3F));
                        }
                        break;
                    }
                    default:
                        // JSON5 allows passing through any other character
                        // unchanged when escaped, but reject digit escapes
                        // (octal) and disallow ECMAScript reserved escapes.
                        if (esc >= '1' && esc <= '9') {
                            error = "invalid escape sequence at ";
                            error += formatPosition(pos - 2);
                            return false;
                        }
                        out += esc;
                        break;
                }
            } else {
                if (static_cast<unsigned char>(c) < 0x20) {
                    error = "unescaped control character in string at ";
                    error += formatPosition(pos - 1);
                    return false;
                }
                out += c;
            }
        }
        error = "unterminated string at ";
        error += formatPosition(open_pos);
        return false;
    }

    bool parse_string(RyAny &out) {
        std::string buf;
        if (!parse_string_bytes(buf)) return false;
        char *handle = makeString(buf.data(), buf.size());
        out = anyFromStr(handle);
        return true;
    }

    // Read an ECMAScript ASCII IdentifierName into `out`. Caller has
    // already verified the next character is a valid id_start.
    void scan_identifier(std::string &out) {
        size_t start = pos;
        pos++; // consume id_start
        while (!at_end() && is_id_part(src[pos])) pos++;
        out.assign(src + start, pos - start);
    }

    bool parse_identifier_key(std::string &out) {
        if (at_end() || !is_id_start(src[pos])) {
            error = "expected object key at ";
            error += formatPosition(pos);
            return false;
        }
        scan_identifier(out);
        return true;
    }

    // JSON5 number: optional sign, then hex, decimal, Infinity, or NaN.
    bool parse_number(RyAny &out) {
        size_t start = pos;
        auto fail = [&](const char *msg) {
            error = msg;
            error += formatPosition(start);
            return false;
        };
        bool negative = false;
        if (!at_end() && (src[pos] == '+' || src[pos] == '-')) {
            negative = (src[pos] == '-');
            pos++;
        }
        if (at_end()) return fail("invalid number at ");

        // Infinity
        if (src[pos] == 'I') {
            constexpr const char kInf[] = "Infinity";
            constexpr size_t kInfLen = sizeof(kInf) - 1;
            if (src_len - pos >= kInfLen && memcmp(src + pos, kInf, kInfLen) == 0) {
                pos += kInfLen;
                double d = negative ? -std::numeric_limits<double>::infinity()
                                    : std::numeric_limits<double>::infinity();
                out = anyFromFloat(d);
                return true;
            }
            return fail("invalid number at ");
        }
        // NaN (sign is allowed by JSON5 grammar but doesn't affect NaN).
        if (src[pos] == 'N') {
            constexpr const char kNaN[] = "NaN";
            constexpr size_t kNaNLen = sizeof(kNaN) - 1;
            if (src_len - pos >= kNaNLen && memcmp(src + pos, kNaN, kNaNLen) == 0) {
                pos += kNaNLen;
                out = anyFromFloat(std::numeric_limits<double>::quiet_NaN());
                return true;
            }
            return fail("invalid number at ");
        }

        // Hex integer: 0x... / 0X...
        if (src[pos] == '0' && pos + 1 < src_len
            && (src[pos + 1] == 'x' || src[pos + 1] == 'X')) {
            pos += 2;
            size_t hex_start = pos;
            while (!at_end() && hex_digit_value(src[pos]) >= 0) pos++;
            if (pos == hex_start) return fail("invalid hex literal at ");
            // Reject identifier-trailing characters (e.g. 0xZZ already caught by
            // hex_digit_value; an alphanumeric immediately after hex is a syntax
            // error).
            if (!at_end() && is_id_part(src[pos])) return fail("invalid hex literal at ");
            uint64_t v = 0;
            for (size_t i = hex_start; i < pos; i++) {
                v = (v << 4) | (uint64_t)hex_digit_value(src[i]);
            }
            int64_t signed_v = negative ? -(int64_t)v : (int64_t)v;
            out = anyFromInt(signed_v);
            return true;
        }

        // Decimal (with JSON5 extensions: leading/trailing dot, leading +)
        bool is_float = false;
        size_t int_digits_start = pos;
        while (!at_end() && isdigit((unsigned char)src[pos])) pos++;
        bool has_int_digits = pos > int_digits_start;
        bool has_frac_digits = false;
        if (!at_end() && src[pos] == '.') {
            is_float = true;
            pos++;
            size_t frac_digits_start = pos;
            while (!at_end() && isdigit((unsigned char)src[pos])) pos++;
            has_frac_digits = pos > frac_digits_start;
        }
        if (!has_int_digits && !has_frac_digits) return fail("invalid number at ");
        if (!at_end() && (src[pos] == 'e' || src[pos] == 'E')) {
            is_float = true;
            pos++;
            if (!at_end() && (src[pos] == '+' || src[pos] == '-')) pos++;
            size_t exp_digit_start = pos;
            while (!at_end() && isdigit((unsigned char)src[pos])) pos++;
            if (pos == exp_digit_start)
                return fail("invalid number: expected digit in exponent at ");
        }

        // NUL-terminate the literal for strtod/strtoll. Real JSON5 numbers
        // (max-precision i64 is 20 chars, max double in %.17g is ~25) fit a
        // small stack buffer with room to spare; fall back to a heap string
        // only for pathological inputs (e.g. 1000-digit zero strings).
        // Skip the leading '+' (strtoll/strtod don't reject it, but uniform
        // input keeps the overflow check below behaving the same as for '-').
        size_t lit_start = start;
        if (src[lit_start] == '+') lit_start++;
        size_t lit_len = pos - lit_start;
        char stack_buf[64];
        std::string heap_buf;
        const char *nts;
        if (lit_len < sizeof(stack_buf)) {
            memcpy(stack_buf, src + lit_start, lit_len);
            stack_buf[lit_len] = '\0';
            nts = stack_buf;
        } else {
            heap_buf.assign(src + lit_start, lit_len);
            nts = heap_buf.c_str();
        }

        if (is_float) {
            double d = strtod(nts, nullptr);
            if (!std::isfinite(d)) return fail("number out of range at ");
            out = anyFromFloat(d);
        } else {
            errno = 0;
            int64_t i = strtoll(nts, nullptr, 10);
            if (errno == ERANGE) {
                error = "integer overflow at ";
                error += formatPosition(start);
                return false;
            }
            out = anyFromInt(i);
        }
        return true;
    }

    bool parse_bool(RyAny &out) {
        if (src_len - pos >= 4 && memcmp(src + pos, "true", 4) == 0 &&
            (pos + 4 >= src_len || !is_id_part(src[pos + 4]))) {
            pos += 4;
            out = anyFromBool(1);
            return true;
        }
        if (src_len - pos >= 5 && memcmp(src + pos, "false", 5) == 0 &&
            (pos + 5 >= src_len || !is_id_part(src[pos + 5]))) {
            pos += 5;
            out = anyFromBool(0);
            return true;
        }
        error = "invalid literal at ";
        error += formatPosition(pos);
        return false;
    }

    bool parse_null(RyAny &out) {
        if (src_len - pos >= 4 && memcmp(src + pos, "null", 4) == 0 &&
            (pos + 4 >= src_len || !is_id_part(src[pos + 4]))) {
            pos += 4;
            out = anyFromUnit();
            return true;
        }
        error = "invalid literal at ";
        error += formatPosition(pos);
        return false;
    }

    bool parse_array(RyAny &out) {
        if (depth >= MAX_NESTING_DEPTH) {
            error = "json5: maximum nesting depth exceeded at ";
            error += formatPosition(pos);
            return false;
        }
        DepthGuard guard(this);
        size_t open_pos = pos;
        pos++; // skip [
        if (!skip_ws_and_comments()) return false;
        if (!at_end() && peek() == ']') {
            pos++;
            auto *header = (ListHeader *)arc_alloc(sizeof(ListHeader));
            header->len = 0;
            header->cap = 0;
            header->data = nullptr;
            out = anyFromListOfAny(header);
            return true;
        }

        size_t cap = 8, len = 0;
        RyAny *items = (RyAny *)checked_array_malloc(cap, sizeof(RyAny));

        auto cleanup = [&]() {
            for (size_t i = 0; i < len; i++) releaseOwnedAny(items[i]);
            free(items);
        };

        while (true) {
            RyAny item{};
            if (!parse_value(item)) { cleanup(); return false; }
            if (len == cap) {
                if (cap > SIZE_MAX / 2 / sizeof(RyAny)) {
                    releaseOwnedAny(item);
                    cleanup();
                    error = "array too large at ";
                    error += formatPosition(open_pos);
                    return false;
                }
                cap *= 2;
                items = (RyAny *)checked_array_realloc(items, cap, sizeof(RyAny));
            }
            items[len++] = item;
            if (!skip_ws_and_comments()) { cleanup(); return false; }
            if (at_end()) {
                error = "unterminated array at ";
                error += formatPosition(open_pos);
                cleanup();
                return false;
            }
            if (peek() == ']') { pos++; break; }
            if (!expect(',')) { cleanup(); return false; }
            // JSON5: allow trailing comma — re-skip ws and check for ']'.
            if (!skip_ws_and_comments()) { cleanup(); return false; }
            if (!at_end() && peek() == ']') { pos++; break; }
        }

        auto *header = (ListHeader *)arc_alloc(sizeof(ListHeader));
        header->len = (int64_t)len;
        header->cap = (int64_t)cap;
        header->data = reinterpret_cast<char **>(items);
        out = anyFromListOfAny(header);
        return true;
    }

    bool parse_object(RyAny &out) {
        if (depth >= MAX_NESTING_DEPTH) {
            error = "json5: maximum nesting depth exceeded at ";
            error += formatPosition(pos);
            return false;
        }
        DepthGuard guard(this);
        size_t open_pos = pos;
        pos++; // skip {
        if (!skip_ws_and_comments()) return false;
        if (!at_end() && peek() == '}') {
            pos++;
            auto *header = (MapHeader *)arc_alloc(sizeof(MapHeader));
            header->len = 0;
            header->cap = 0;
            header->keys = nullptr;
            header->vals = nullptr;
            header->bucket_count = 4;
            header->buckets = __ry_ht_rehash_str(nullptr, 0, 4);
            out = anyFromMapStrAny(header);
            return true;
        }

        size_t cap = 8, len = 0;
        char **keys = (char **)checked_array_malloc(cap, sizeof(char *));
        RyAny *vals = (RyAny *)checked_array_malloc(cap, sizeof(RyAny));

        auto cleanup = [&]() {
            for (size_t i = 0; i < len; i++) {
                freeStringSlot(keys[i]);
                releaseOwnedAny(vals[i]);
            }
            free(keys);
            free(vals);
        };

        // Hoisted above the loop so the allocated capacity is reused across keys.
        std::string keybuf;
        while (true) {
            if (!skip_ws_and_comments()) { cleanup(); return false; }
            if (at_end()) {
                error = "expected object key at ";
                error += formatPosition(pos);
                cleanup();
                return false;
            }
            keybuf.clear();
            char kc = peek();
            if (kc == '"' || kc == '\'') {
                if (!parse_string_bytes(keybuf)) { cleanup(); return false; }
            } else if (is_id_start(kc)) {
                if (!parse_identifier_key(keybuf)) { cleanup(); return false; }
            } else {
                error = "expected object key at ";
                error += formatPosition(pos);
                cleanup();
                return false;
            }
            char *key = makeString(keybuf.data(), keybuf.size());

            if (!expect(':')) { freeStringSlot(key); cleanup(); return false; }

            RyAny val{};
            if (!parse_value(val)) { freeStringSlot(key); cleanup(); return false; }

            if (len == cap) {
                if (cap > SIZE_MAX / 2 / sizeof(RyAny)) {
                    freeStringSlot(key);
                    releaseOwnedAny(val);
                    cleanup();
                    error = "object too large at ";
                    error += formatPosition(open_pos);
                    return false;
                }
                cap *= 2;
                keys = (char **)checked_array_realloc(keys, cap, sizeof(char *));
                vals = (RyAny *)checked_array_realloc(vals, cap, sizeof(RyAny));
            }
            keys[len] = key;
            vals[len] = val;
            len++;

            if (!skip_ws_and_comments()) { cleanup(); return false; }
            if (at_end()) {
                error = "unterminated object at ";
                error += formatPosition(open_pos);
                cleanup();
                return false;
            }
            if (peek() == '}') { pos++; break; }
            if (!expect(',')) { cleanup(); return false; }
            // JSON5: allow trailing comma — re-skip and check for '}'.
            if (!skip_ws_and_comments()) { cleanup(); return false; }
            if (!at_end() && peek() == '}') { pos++; break; }
        }

        // bucket_count: smallest power of 2 ≥ max(4, len*2)
        int64_t bc = 4;
        while (bc < (int64_t)len * 2) bc *= 2;

        auto *header = (MapHeader *)arc_alloc(sizeof(MapHeader));
        header->len = (int64_t)len;
        header->cap = (int64_t)cap;
        header->keys = keys;
        header->vals = reinterpret_cast<char **>(vals);
        header->bucket_count = bc;
        header->buckets = __ry_ht_rehash_str((const char **)keys, (int64_t)len, bc);
        out = anyFromMapStrAny(header);
        return true;
    }
};

// ===== stringify (json5 variant) =====
//
// Mirrors json.cpp's stringify_any with one behavioral change: non-finite
// floats are emitted as JSON5 bare tokens ("NaN", "Infinity", "-Infinity")
// instead of triggering the report_stringify_error path. Every other
// rejection branch (typed-collection side-table, Set, Record, Enum) is
// inherited verbatim — those values still have no JSON5 representation.

[[nodiscard]] static bool report_stringify_error(std::string *error_out,
                                                  const char *msg) {
    if (error_out) {
        *error_out = msg;
        return false;
    }
    fprintf(stderr, "%s\n", msg);
    ry_runtime_trap_exit();
}

[[nodiscard]] static bool report_typed_coll_error(std::string *error_out,
                                                   const char *coll_type) {
    std::string buf = "stringify: any holds typed collection '";
    buf += coll_type;
    buf += "' — use List<any> / Map<str, any> / Set<any> instead";
    if (error_out) {
        *error_out = std::move(buf);
        return false;
    }
    fprintf(stderr, "%s\n", buf.c_str());
    ry_runtime_trap_exit();
}

[[nodiscard]] static bool stringify_any(const RyAny *v, std::string &out,
                                         size_t indent, size_t depth,
                                         bool pretty, bool sort_keys,
                                         std::string *error_out) {
    if (!v) { out += "null"; return true; }
    switch (static_cast<RyAnyTag>(v->tag)) {
        case RyAnyTag::Unit: out += "null"; return true;
        case RyAnyTag::Bool: {
            int64_t bv;
            memcpy(&bv, v->data, sizeof(bv));
            out += bv ? "true" : "false";
            return true;
        }
        case RyAnyTag::Int: {
            int64_t iv;
            memcpy(&iv, v->data, sizeof(iv));
            out += std::to_string(iv);
            return true;
        }
        case RyAnyTag::Float: {
            double fv;
            memcpy(&fv, v->data, sizeof(fv));
            // JSON5 difference vs json: NaN / ±Infinity are valid tokens.
            if (std::isnan(fv)) { out += "NaN"; return true; }
            if (std::isinf(fv)) { out += (fv > 0 ? "Infinity" : "-Infinity"); return true; }
            char buf[64];
            snprintf(buf, sizeof(buf), "%.17g", fv);
            out += buf;
            return true;
        }
        case RyAnyTag::Str: {
            char *handle;
            memcpy(&handle, v->data, sizeof(handle));
            out += '"';
            escape_string(handle, stringByteLen(handle), out);
            out += '"';
            return true;
        }
        case RyAnyTag::List: {
            void *hdr_ptr;
            memcpy(&hdr_ptr, v->data, sizeof(hdr_ptr));
            if (const char *coll_type = __ry_any_lookup_typed_coll(hdr_ptr))
                return report_typed_coll_error(error_out, coll_type);
            auto *hdr = static_cast<ListHeader *>(hdr_ptr);
            if (hdr->len == 0) { out += "[]"; return true; }
            auto *items = reinterpret_cast<RyAny *>(hdr->data);
            out += '[';
            for (int64_t i = 0; i < hdr->len; i++) {
                if (i > 0) out += ',';
                if (pretty) { out += '\n'; out.append((depth + 1) * indent, ' '); }
                if (!stringify_any(&items[i], out, indent, depth + 1, pretty,
                                    sort_keys, error_out))
                    return false;
            }
            if (pretty) { out += '\n'; out.append(depth * indent, ' '); }
            out += ']';
            return true;
        }
        case RyAnyTag::Map: {
            void *hdr_ptr;
            memcpy(&hdr_ptr, v->data, sizeof(hdr_ptr));
            if (const char *coll_type = __ry_any_lookup_typed_coll(hdr_ptr))
                return report_typed_coll_error(error_out, coll_type);
            auto *hdr = static_cast<MapHeader *>(hdr_ptr);
            if (hdr->len == 0) { out += "{}"; return true; }
            auto *vals = reinterpret_cast<RyAny *>(hdr->vals);
            // sort_keys=false (the default) keeps insertion order — avoid the
            // O(N) vector allocation per Map node in that case.
            std::vector<int64_t> order;
            if (sort_keys) {
                order.resize(static_cast<size_t>(hdr->len));
                std::iota(order.begin(), order.end(), int64_t{0});
                std::sort(order.begin(), order.end(),
                    [&](int64_t a, int64_t b) {
                        const char *ka = hdr->keys[a];
                        const char *kb = hdr->keys[b];
                        size_t la = static_cast<size_t>(stringByteLen(ka));
                        size_t lb = static_cast<size_t>(stringByteLen(kb));
                        int c = std::memcmp(ka, kb, std::min(la, lb));
                        if (c != 0) return c < 0;
                        return la < lb;
                    });
            }
            out += '{';
            for (int64_t i = 0; i < hdr->len; i++) {
                int64_t idx = sort_keys ? order[static_cast<size_t>(i)] : i;
                if (i > 0) out += ',';
                if (pretty) { out += '\n'; out.append((depth + 1) * indent, ' '); }
                out += '"';
                escape_string(hdr->keys[idx], stringByteLen(hdr->keys[idx]), out);
                out += '"';
                out += ':';
                if (pretty) out += ' ';
                if (!stringify_any(&vals[idx], out, indent, depth + 1, pretty,
                                    sort_keys, error_out))
                    return false;
            }
            if (pretty) { out += '\n'; out.append(depth * indent, ' '); }
            out += '}';
            return true;
        }
        case RyAnyTag::Set:
            return report_stringify_error(error_out,
                "json5 stringify: Set is not representable in JSON5");
        case RyAnyTag::Record:
            return report_stringify_error(error_out,
                "json5 stringify: record is not representable in JSON5");
        case RyAnyTag::Enum:
            return report_stringify_error(error_out,
                "json5 stringify: enum is not representable in JSON5");
    }
    return true;
}


// ===== extern "C" implementations =====

extern "C" {

int64_t __ry_json5_parse_to_any(const char *text, RyAny *out) {
    if (!text) {
        __ry_set_last_error("json5 parse: input is null");
        return 1;
    }
    if (!out) {
        __ry_set_last_error("json5 parse: out is null");
        return 1;
    }
    Json5AnyParser parser(text);
    RyAny v{};
    if (!parser.parse_value(v)) {
        __ry_set_last_error(parser.error.c_str());
        return 1;
    }
    if (!parser.skip_ws_and_comments()) {
        releaseOwnedAny(v);
        __ry_set_last_error(parser.error.c_str());
        return 1;
    }
    if (!parser.at_end()) {
        releaseOwnedAny(v);
        __ry_set_last_error("json5 parse: unexpected trailing content");
        return 1;
    }
    *out = v;
    return 0;
}

const char *__ry_json5_stringify_any_ex(const RyAny *value,
                                         int64_t indent_or_neg1,
                                         uint8_t sort_keys) {
    std::string out;
    bool pretty = indent_or_neg1 >= 0;
    size_t indent = pretty ? static_cast<size_t>(indent_or_neg1) : 0;
    (void)stringify_any(value, out, indent, 0, pretty,
                         /*sort_keys=*/sort_keys != 0,
                         /*error_out=*/nullptr);
    return makeString(out.data(), out.size());
}

const char *__ry_json5_stringify_any_safe_ex(const RyAny *value,
                                              int64_t indent_or_neg1,
                                              uint8_t sort_keys) {
    std::string out;
    bool pretty = indent_or_neg1 >= 0;
    size_t indent = pretty ? static_cast<size_t>(indent_or_neg1) : 0;
    std::string err;
    if (!stringify_any(value, out, indent, 0, pretty,
                        /*sort_keys=*/sort_keys != 0, &err)) {
        __ry_set_last_error(err.c_str());
        return nullptr;
    }
    return makeString(out.data(), out.size());
}

int64_t __ry_json5_load_file(void *handle, RyAny *out) {
    if (!handle) {
        __ry_set_last_error("json5 load: file handle is null");
        return 1;
    }
    if (!out) {
        __ry_set_last_error("json5 load: out is null");
        return 1;
    }
    const char *text = __ry_io_file_read_all(handle);
    if (!text) {
        return 1; // io populated __ry_set_last_error already
    }
    // Delegate to the str overload so parse + error reporting + trailing-content
    // checks live in one place.
    int64_t status = __ry_json5_parse_to_any(text, out);
    freeStringSlot(const_cast<char *>(text));
    return status;
}

int64_t __ry_json5_dump_file(void *handle, const RyAny *value,
                              int64_t indent_or_neg1) {
    if (!handle) {
        __ry_set_last_error("json5 dump: file handle is null");
        return 1;
    }
    if (!value) {
        __ry_set_last_error("json5 dump: value is null");
        return 1;
    }
    // Trap semantics inherited from __ry_json5_stringify_any_ex for Set /
    // Record / Enum / typed-collection inputs — dump cannot recover at
    // the JSON5 layer.
    const char *s = __ry_json5_stringify_any_ex(value, indent_or_neg1, 0);
    int64_t status = __ry_io_file_write_text(handle, s);
    freeStringSlot(const_cast<char *>(s));
    if (status != 0) {
        return 1;
    }
    return 0;
}

} // extern "C"

} // namespace ry
