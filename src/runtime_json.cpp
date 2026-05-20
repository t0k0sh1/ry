#include "ry/runtime_json.hpp"
#include "ry/runtime_io.hpp"
#include "ry/runtime_arc.hpp"
#include "ry/runtime_list.hpp"
#include "ry/runtime_string.hpp"
#include "ry/runtime_http_types.hpp" // MapHeader + __ry_ht_rehash_str

#include <cctype>
#include <cerrno>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>


namespace ry {

// ===== JSON string escape helper (shared by stringify_any) =====

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


// ===== `any`-based parser / stringify (#1698) =====
//
// Builds RyAny values directly using the C++ helpers from runtime_any.hpp.
// Object → Map<str, any> (parallel keys/vals arrays + power-of-2 bucket table
// via __ry_ht_rehash_str). Array → List<any> (data buffer of RyAny stride).
// Caller owns the returned payloads; emitAnyReleaseVar handles release at the
// codegen side.

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

extern "C" void __ry_json_release_any(RyAny *value) {
    if (value) releaseOwnedAny(*value);
}

struct JsonAnyParser {
    static constexpr int MAX_NESTING_DEPTH = 256;

    const char *src;
    size_t pos;
    size_t src_len;
    std::string error;
    int depth = 0;

    explicit JsonAnyParser(const char *text)
        : src(text), pos(0), src_len((size_t)stringByteLen(text)) {}

    struct DepthGuard {
        JsonAnyParser *p;
        explicit DepthGuard(JsonAnyParser *parser) : p(parser) { ++p->depth; }
        ~DepthGuard() { --p->depth; }
        DepthGuard(const DepthGuard&) = delete;
        DepthGuard& operator=(const DepthGuard&) = delete;
    };

    char peek() const { return src[pos]; }
    char advance() { return src[pos++]; }
    bool at_end() const { return pos >= src_len; }

    void skip_ws() {
        while (!at_end() && (src[pos] == ' ' || src[pos] == '\t' ||
                             src[pos] == '\n' || src[pos] == '\r'))
            pos++;
    }

    bool expect(char c) {
        skip_ws();
        if (at_end() || src[pos] != c) {
            error = "expected '";
            error += c;
            error += "' at position ";
            error += std::to_string(pos);
            return false;
        }
        pos++;
        return true;
    }

    bool parse_value(RyAny &out) {
        skip_ws();
        if (at_end()) { error = "unexpected end of input"; return false; }
        char c = peek();
        if (c == '"') return parse_string(out);
        if (c == '{') return parse_object(out);
        if (c == '[') return parse_array(out);
        if (c == 't' || c == 'f') return parse_bool(out);
        if (c == 'n') return parse_null(out);
        if (c == '-' || (c >= '0' && c <= '9')) return parse_number(out);
        error = "unexpected character '";
        error += c;
        error += "' at position ";
        error += std::to_string(pos);
        return false;
    }

    // Parse a JSON string body and append the decoded bytes (UTF-8) to `out`.
    // Used both for top-level string values and for object keys.
    bool parse_string_bytes(std::string &out) {
        pos++; // skip opening "

        // Fast path: scan for closing quote without backslash (common case)
        size_t scan = pos;
        while (scan < src_len && src[scan] != '"' && src[scan] != '\\') {
            if (static_cast<unsigned char>(src[scan]) < 0x20) {
                error = "unescaped control character in string at position " + std::to_string(scan);
                return false;
            }
            scan++;
        }
        if (scan < src_len && src[scan] == '"') {
            out.assign(src + pos, scan - pos);
            pos = scan + 1;
            return true;
        }

        // Slow path: contains escape sequences
        if (scan > pos) {
            out.append(src + pos, scan - pos);
            pos = scan;
        }
        while (!at_end()) {
            char c = advance();
            if (c == '"') return true;
            if (c == '\\') {
                if (at_end()) { error = "unterminated string escape"; return false; }
                char esc = advance();
                switch (esc) {
                    case '"': out += '"'; break;
                    case '\\': out += '\\'; break;
                    case '/': out += '/'; break;
                    case 'b': out += '\b'; break;
                    case 'f': out += '\f'; break;
                    case 'n': out += '\n'; break;
                    case 'r': out += '\r'; break;
                    case 't': out += '\t'; break;
                    case 'u': {
                        auto parse_hex4 = [&](unsigned &cp_out) -> bool {
                            if (pos + 4 > src_len) { error = "incomplete unicode escape"; return false; }
                            char hex[5] = {src[pos], src[pos+1], src[pos+2], src[pos+3], 0};
                            for (int hi = 0; hi < 4; hi++) {
                                char hc = hex[hi];
                                if (!((hc >= '0' && hc <= '9') || (hc >= 'a' && hc <= 'f') || (hc >= 'A' && hc <= 'F'))) {
                                    error = "invalid hex digit in unicode escape";
                                    return false;
                                }
                            }
                            pos += 4;
                            cp_out = (unsigned)strtoul(hex, nullptr, 16);
                            return true;
                        };
                        unsigned cp;
                        if (!parse_hex4(cp)) return false;
                        // High surrogate: expect a following \uXXXX low surrogate.
                        if (cp >= 0xD800 && cp <= 0xDBFF) {
                            if (pos + 2 > src_len || src[pos] != '\\' || src[pos+1] != 'u') {
                                error = "unpaired high surrogate in unicode escape";
                                return false;
                            }
                            pos += 2;
                            unsigned low;
                            if (!parse_hex4(low)) return false;
                            if (low < 0xDC00 || low > 0xDFFF) {
                                error = "invalid low surrogate in unicode escape";
                                return false;
                            }
                            cp = 0x10000 + ((cp - 0xD800) << 10) + (low - 0xDC00);
                        } else if (cp >= 0xDC00 && cp <= 0xDFFF) {
                            error = "unpaired low surrogate in unicode escape";
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
                        error = "invalid escape character '\\";
                        error += esc;
                        error += "'";
                        return false;
                }
            } else {
                if (static_cast<unsigned char>(c) < 0x20) {
                    error = "unescaped control character in string at position " + std::to_string(pos - 1);
                    return false;
                }
                out += c;
            }
        }
        error = "unterminated string";
        return false;
    }

    bool parse_string(RyAny &out) {
        std::string buf;
        if (!parse_string_bytes(buf)) return false;
        char *handle = makeString(buf.data(), buf.size());
        out = anyFromStr(handle);
        return true;
    }

    bool parse_number(RyAny &out) {
        size_t start = pos;
        bool is_float = false;
        if (src[pos] == '-') pos++;
        if (at_end() || !isdigit((unsigned char)src[pos])) {
            error = "invalid number at position " + std::to_string(start);
            return false;
        }
        if (src[pos] == '0' && pos + 1 < src_len && isdigit((unsigned char)src[pos + 1])) {
            error = "leading zeros not allowed at position " + std::to_string(start);
            return false;
        }
        while (!at_end() && isdigit((unsigned char)src[pos])) pos++;
        if (!at_end() && src[pos] == '.') {
            is_float = true;
            pos++;
            if (at_end() || !isdigit((unsigned char)src[pos])) {
                error = "invalid number: expected digit after decimal point";
                return false;
            }
            while (!at_end() && isdigit((unsigned char)src[pos])) pos++;
        }
        if (!at_end() && (src[pos] == 'e' || src[pos] == 'E')) {
            is_float = true;
            pos++;
            if (!at_end() && (src[pos] == '+' || src[pos] == '-')) pos++;
            if (at_end() || !isdigit((unsigned char)src[pos])) {
                error = "invalid number: expected digit in exponent";
                return false;
            }
            while (!at_end() && isdigit((unsigned char)src[pos])) pos++;
        }
        if (is_float) {
            errno = 0;
            double d = strtod(src + start, nullptr);
            if (!std::isfinite(d)) {
                error = "number out of range at position " + std::to_string(start);
                return false;
            }
            out = anyFromFloat(d);
        } else {
            errno = 0;
            int64_t i = strtoll(src + start, nullptr, 10);
            if (errno == ERANGE) {
                error = "integer overflow at position " + std::to_string(start);
                return false;
            }
            out = anyFromInt(i);
        }
        return true;
    }

    bool parse_bool(RyAny &out) {
        if (src_len - pos >= 4 && memcmp(src + pos, "true", 4) == 0 &&
            (pos+4 >= src_len || !isalnum((unsigned char)src[pos+4]))) {
            pos += 4;
            out = anyFromBool(1);
            return true;
        }
        if (src_len - pos >= 5 && memcmp(src + pos, "false", 5) == 0 &&
            (pos+5 >= src_len || !isalnum((unsigned char)src[pos+5]))) {
            pos += 5;
            out = anyFromBool(0);
            return true;
        }
        error = "invalid literal at position " + std::to_string(pos);
        return false;
    }

    bool parse_null(RyAny &out) {
        if (src_len - pos >= 4 && memcmp(src + pos, "null", 4) == 0 &&
            (pos+4 >= src_len || !isalnum((unsigned char)src[pos+4]))) {
            pos += 4;
            out = anyFromUnit();
            return true;
        }
        error = "invalid literal at position " + std::to_string(pos);
        return false;
    }

    bool parse_array(RyAny &out) {
        if (depth >= MAX_NESTING_DEPTH) {
            error = "json: maximum nesting depth exceeded";
            return false;
        }
        DepthGuard guard(this);
        pos++; // skip [
        skip_ws();
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
                    error = "array too large";
                    return false;
                }
                cap *= 2;
                items = (RyAny *)checked_array_realloc(items, cap, sizeof(RyAny));
            }
            items[len++] = item;
            skip_ws();
            if (at_end()) { error = "unterminated array"; cleanup(); return false; }
            if (peek() == ']') { pos++; break; }
            if (!expect(',')) { cleanup(); return false; }
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
            error = "json: maximum nesting depth exceeded";
            return false;
        }
        DepthGuard guard(this);
        pos++; // skip {
        skip_ws();
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

        while (true) {
            skip_ws();
            if (at_end() || peek() != '"') {
                error = "expected string key at position " + std::to_string(pos);
                cleanup();
                return false;
            }
            std::string keybuf;
            if (!parse_string_bytes(keybuf)) { cleanup(); return false; }
            char *key = makeString(keybuf.data(), keybuf.size());

            if (!expect(':')) { freeStringSlot(key); cleanup(); return false; }

            RyAny val{};
            if (!parse_value(val)) { freeStringSlot(key); cleanup(); return false; }

            if (len == cap) {
                if (cap > SIZE_MAX / 2 / sizeof(RyAny)) {
                    freeStringSlot(key);
                    releaseOwnedAny(val);
                    cleanup();
                    error = "object too large";
                    return false;
                }
                cap *= 2;
                keys = (char **)checked_array_realloc(keys, cap, sizeof(char *));
                vals = (RyAny *)checked_array_realloc(vals, cap, sizeof(RyAny));
            }
            keys[len] = key;
            vals[len] = val;
            len++;

            skip_ws();
            if (at_end()) { error = "unterminated object"; cleanup(); return false; }
            if (peek() == '}') { pos++; break; }
            if (!expect(',')) { cleanup(); return false; }
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

static void stringify_any(const RyAny *v, std::string &out,
                          size_t indent, size_t depth, bool pretty) {
    if (!v) { out += "null"; return; }
    switch (static_cast<RyAnyTag>(v->tag)) {
        case RyAnyTag::Unit: out += "null"; break;
        case RyAnyTag::Bool: {
            int64_t bv;
            memcpy(&bv, v->data, sizeof(bv));
            out += bv ? "true" : "false";
            break;
        }
        case RyAnyTag::Int: {
            int64_t iv;
            memcpy(&iv, v->data, sizeof(iv));
            out += std::to_string(iv);
            break;
        }
        case RyAnyTag::Float: {
            double fv;
            memcpy(&fv, v->data, sizeof(fv));
            if (!std::isfinite(fv)) {
                fprintf(stderr,
                        "json stringify: non-finite float is not representable in JSON\n");
                exit(1);
            }
            char buf[64];
            snprintf(buf, sizeof(buf), "%.17g", fv);
            out += buf;
            break;
        }
        case RyAnyTag::Str: {
            char *handle;
            memcpy(&handle, v->data, sizeof(handle));
            out += '"';
            escape_string(handle, stringByteLen(handle), out);
            out += '"';
            break;
        }
        case RyAnyTag::List: {
            void *hdr_ptr;
            memcpy(&hdr_ptr, v->data, sizeof(hdr_ptr));
            auto *hdr = static_cast<ListHeader *>(hdr_ptr);
            if (hdr->len == 0) { out += "[]"; break; }
            auto *items = reinterpret_cast<RyAny *>(hdr->data);
            out += '[';
            for (int64_t i = 0; i < hdr->len; i++) {
                if (i > 0) out += ',';
                if (pretty) { out += '\n'; out.append((depth + 1) * indent, ' '); }
                stringify_any(&items[i], out, indent, depth + 1, pretty);
            }
            if (pretty) { out += '\n'; out.append(depth * indent, ' '); }
            out += ']';
            break;
        }
        case RyAnyTag::Map: {
            void *hdr_ptr;
            memcpy(&hdr_ptr, v->data, sizeof(hdr_ptr));
            auto *hdr = static_cast<MapHeader *>(hdr_ptr);
            if (hdr->len == 0) { out += "{}"; break; }
            auto *vals = reinterpret_cast<RyAny *>(hdr->vals);
            out += '{';
            for (int64_t i = 0; i < hdr->len; i++) {
                if (i > 0) out += ',';
                if (pretty) { out += '\n'; out.append((depth + 1) * indent, ' '); }
                out += '"';
                escape_string(hdr->keys[i], stringByteLen(hdr->keys[i]), out);
                out += '"';
                out += ':';
                if (pretty) out += ' ';
                stringify_any(&vals[i], out, indent, depth + 1, pretty);
            }
            if (pretty) { out += '\n'; out.append(depth * indent, ' '); }
            out += '}';
            break;
        }
        case RyAnyTag::Set:
            fprintf(stderr, "json stringify: Set is not representable in JSON\n");
            exit(1);
        case RyAnyTag::Record:
            fprintf(stderr, "json stringify: record is not representable in JSON\n");
            exit(1);
        case RyAnyTag::Enum:
            fprintf(stderr, "json stringify: enum is not representable in JSON\n");
            exit(1);
    }
}


// ===== extern "C" implementations =====

extern "C" {

int64_t __ry_json_parse_to_any(const char *text, RyAny *out) {
    if (!text) {
        __ry_set_last_error("json parse: input is null");
        return 1;
    }
    if (!out) {
        __ry_set_last_error("json parse: out is null");
        return 1;
    }
    JsonAnyParser parser(text);
    RyAny v{};
    if (!parser.parse_value(v)) {
        __ry_set_last_error(parser.error.c_str());
        return 1;
    }
    parser.skip_ws();
    if (!parser.at_end()) {
        releaseOwnedAny(v);
        __ry_set_last_error("json parse: unexpected trailing content");
        return 1;
    }
    *out = v;
    return 0;
}

const char *__ry_json_stringify_any(const RyAny *value, int64_t indent_or_neg1) {
    std::string out;
    bool pretty = indent_or_neg1 >= 0;
    size_t indent = pretty ? static_cast<size_t>(indent_or_neg1) : 0;
    stringify_any(value, out, indent, 0, pretty);
    return makeString(out.data(), out.size());
}

} // extern "C"

} // namespace ry
