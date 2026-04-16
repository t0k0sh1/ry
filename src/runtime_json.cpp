#include "ry/runtime_json.hpp"
#include "ry/runtime_io.hpp"
#include "ry/runtime_arc.hpp"
#include "ry/runtime_list.hpp"
#include "ry/runtime_string.hpp"

#include <cctype>
#include <new>
#include <cerrno>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>


namespace ry {

// ===== JsonValue definition =====

enum class JsonType { Null, Bool, Int, Float, String, Array, Object };

struct JsonValue;
struct JsonArrayVal { JsonValue **items; int64_t len; };
struct JsonObjectVal { char **keys; JsonValue **values; int64_t len; };

struct JsonValue {
    JsonType type;
    union {
        bool bool_val;
        int64_t int_val;
        double float_val;
        char *string_val;
        JsonArrayVal array_val;
        JsonObjectVal object_val;
    };
};

// ===== Memory management =====

// Release internal fields without freeing the node itself.
static void json_release_contents(JsonValue *v);

static void json_free_recursive(JsonValue *v) {
    if (!v) return;
    json_release_contents(v);
    arc_free(v);
}

static void json_release_contents(JsonValue *v) {
    switch (v->type) {
        case JsonType::String: freeStringSlot(v->string_val); break;
        case JsonType::Array:
            for (int64_t i = 0; i < v->array_val.len; i++)
                json_free_recursive(v->array_val.items[i]);
            free(v->array_val.items);
            break;
        case JsonType::Object:
            for (int64_t i = 0; i < v->object_val.len; i++) {
                freeStringSlot(v->object_val.keys[i]);
                json_free_recursive(v->object_val.values[i]);
            }
            free(v->object_val.keys);
            free(v->object_val.values);
            break;
        default: break;
    }
}

// ===== Parser =====

struct Parser {
    const char *src;
    size_t pos;
    size_t src_len;
    std::string error;

    Parser(const char *text) : src(text), pos(0), src_len((size_t)stringByteLen(text)) {}

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

    JsonValue *parse_value() {
        skip_ws();
        if (at_end()) { error = "unexpected end of input"; return nullptr; }
        char c = peek();
        if (c == '"') return parse_string();
        if (c == '{') return parse_object();
        if (c == '[') return parse_array();
        if (c == 't' || c == 'f') return parse_bool();
        if (c == 'n') return parse_null();
        if (c == '-' || (c >= '0' && c <= '9')) return parse_number();
        error = "unexpected character '";
        error += c;
        error += "' at position ";
        error += std::to_string(pos);
        return nullptr;
    }

    JsonValue *parse_string() {
        pos++; // skip opening "

        // Fast path: scan for closing quote without backslash (common case)
        size_t scan = pos;
        while (scan < src_len && src[scan] != '"' && src[scan] != '\\') {
            if (static_cast<unsigned char>(src[scan]) < 0x20) {
                error = "unescaped control character in string at position " + std::to_string(scan);
                return nullptr;
            }
            scan++;
        }
        if (scan < src_len && src[scan] == '"') {
            void *mem = arc_alloc(sizeof(JsonValue));
            if (!mem) {
                error = "out of memory";
                return nullptr;
            }
            auto *v = new (mem) JsonValue;
            v->type = JsonType::String;
            v->string_val = makeString(src + pos, scan - pos);
            pos = scan + 1;
            return v;
        }

        // Slow path: string contains escape sequences
        std::string buf;
        // Pre-fill with already-scanned clean prefix (avoids re-scanning)
        if (scan > pos) {
            buf.append(src + pos, scan - pos);
            pos = scan;
        }
        while (!at_end()) {
            char c = advance();
            if (c == '"') {
                void *mem = arc_alloc(sizeof(JsonValue));
                if (!mem) {
                    error = "out of memory";
                    return nullptr;
                }
                auto *v = new (mem) JsonValue;
                v->type = JsonType::String;
                v->string_val = makeString(buf.data(), buf.size());
                return v;
            }
            if (c == '\\') {
                if (at_end()) { error = "unterminated string escape"; return nullptr; }
                char esc = advance();
                switch (esc) {
                    case '"': buf += '"'; break;
                    case '\\': buf += '\\'; break;
                    case '/': buf += '/'; break;
                    case 'b': buf += '\b'; break;
                    case 'f': buf += '\f'; break;
                    case 'n': buf += '\n'; break;
                    case 'r': buf += '\r'; break;
                    case 't': buf += '\t'; break;
                    case 'u': {
                        if (pos + 4 > src_len) {
                            error = "incomplete unicode escape"; return nullptr;
                        }
                        char hex[5] = {src[pos], src[pos+1], src[pos+2], src[pos+3], 0};
                        for (int hi = 0; hi < 4; hi++) {
                            char hc = hex[hi];
                            if (!((hc >= '0' && hc <= '9') || (hc >= 'a' && hc <= 'f') || (hc >= 'A' && hc <= 'F'))) {
                                error = "invalid hex digit in unicode escape";
                                return nullptr;
                            }
                        }
                        pos += 4;
                        unsigned cp = (unsigned)strtoul(hex, nullptr, 16);
                        // UTF-8 encode (cp == 0 → push literal NUL byte; RFC 8259 allows \u0000)
                        if (cp < 0x80) {
                            buf += (char)cp;
                        } else if (cp < 0x800) {
                            buf += (char)(0xC0 | (cp >> 6));
                            buf += (char)(0x80 | (cp & 0x3F));
                        } else {
                            buf += (char)(0xE0 | (cp >> 12));
                            buf += (char)(0x80 | ((cp >> 6) & 0x3F));
                            buf += (char)(0x80 | (cp & 0x3F));
                        }
                        break;
                    }
                    default:
                        error = "invalid escape character '\\";
                        error += esc;
                        error += "'";
                        return nullptr;
                }
            } else {
                if (static_cast<unsigned char>(c) < 0x20) {
                    error = "unescaped control character in string at position " + std::to_string(pos - 1);
                    return nullptr;
                }
                buf += c;
            }
        }
        error = "unterminated string";
        return nullptr;
    }

    JsonValue *parse_number() {
        size_t start = pos;
        bool is_float = false;
        if (src[pos] == '-') pos++;
        if (at_end() || !isdigit((unsigned char)src[pos])) {
            error = "invalid number at position " + std::to_string(start);
            return nullptr;
        }
        if (src[pos] == '0' && pos + 1 < src_len && isdigit((unsigned char)src[pos + 1])) {
            error = "leading zeros not allowed at position " + std::to_string(start);
            return nullptr;
        }
        while (!at_end() && isdigit((unsigned char)src[pos])) pos++;
        if (!at_end() && src[pos] == '.') {
            is_float = true;
            pos++;
            if (at_end() || !isdigit((unsigned char)src[pos])) {
                error = "invalid number: expected digit after decimal point";
                return nullptr;
            }
            while (!at_end() && isdigit((unsigned char)src[pos])) pos++;
        }
        if (!at_end() && (src[pos] == 'e' || src[pos] == 'E')) {
            is_float = true;
            pos++;
            if (!at_end() && (src[pos] == '+' || src[pos] == '-')) pos++;
            if (at_end() || !isdigit((unsigned char)src[pos])) {
                error = "invalid number: expected digit in exponent";
                return nullptr;
            }
            while (!at_end() && isdigit((unsigned char)src[pos])) pos++;
        }
        // src is NUL-terminated and pos sits at a non-numeric character
        // (delimiter, whitespace, or NUL), so strtod/strtoll stop correctly.
        void *mem = arc_alloc(sizeof(JsonValue));
        if (!mem) {
            error = "out of memory";
            return nullptr;
        }
        auto *v = new (mem) JsonValue;
        if (is_float) {
            v->type = JsonType::Float;
            errno = 0;
            v->float_val = strtod(src + start, nullptr);
            if (!std::isfinite(v->float_val)) {
                arc_free(v);
                error = "number out of range at position " + std::to_string(start);
                return nullptr;
            }
        } else {
            v->type = JsonType::Int;
            errno = 0;
            v->int_val = strtoll(src + start, nullptr, 10);
            if (errno == ERANGE) {
                arc_free(v);
                error = "integer overflow at position " + std::to_string(start);
                return nullptr;
            }
        }
        return v;
    }

    JsonValue *parse_bool() {
        if (strncmp(src + pos, "true", 4) == 0 &&
            (pos+4 >= src_len || !isalnum((unsigned char)src[pos+4]))) {
            pos += 4;
            void *mem = arc_alloc(sizeof(JsonValue));
            if (!mem) {
                error = "out of memory";
                return nullptr;
            }
            auto *v = new (mem) JsonValue;
            v->type = JsonType::Bool;
            v->bool_val = true;
            return v;
        }
        if (strncmp(src + pos, "false", 5) == 0 &&
            (pos+5 >= src_len || !isalnum((unsigned char)src[pos+5]))) {
            pos += 5;
            void *mem = arc_alloc(sizeof(JsonValue));
            if (!mem) {
                error = "out of memory";
                return nullptr;
            }
            auto *v = new (mem) JsonValue;
            v->type = JsonType::Bool;
            v->bool_val = false;
            return v;
        }
        error = "invalid literal at position " + std::to_string(pos);
        return nullptr;
    }

    JsonValue *parse_null() {
        if (strncmp(src + pos, "null", 4) == 0 &&
            (pos+4 >= src_len || !isalnum((unsigned char)src[pos+4]))) {
            pos += 4;
            void *mem = arc_alloc(sizeof(JsonValue));
            if (!mem) {
                error = "out of memory";
                return nullptr;
            }
            auto *v = new (mem) JsonValue;
            v->type = JsonType::Null;
            return v;
        }
        error = "invalid literal at position " + std::to_string(pos);
        return nullptr;
    }

    JsonValue *parse_array() {
        pos++; // skip [
        skip_ws();
        if (!at_end() && peek() == ']') {
            pos++;
            void *mem = arc_alloc(sizeof(JsonValue));
            if (!mem) {
                error = "out of memory";
                return nullptr;
            }
            auto *v = new (mem) JsonValue;
            v->type = JsonType::Array;
            v->array_val.items = nullptr;
            v->array_val.len = 0;
            return v;
        }

        size_t cap = 8, len = 0;
        JsonValue **items = (JsonValue**)checked_array_malloc(cap, sizeof(JsonValue*));

        while (true) {
            JsonValue *item = parse_value();
            if (!item) {
                for (size_t i = 0; i < len; i++) json_free_recursive(items[i]);
                free(items);
                return nullptr;
            }
            if (len == cap) {
                if (cap > SIZE_MAX / 2 / sizeof(JsonValue*)) {
                    json_free_recursive(item);
                    for (size_t i = 0; i < len; i++) json_free_recursive(items[i]);
                    free(items);
                    error = "array too large";
                    return nullptr;
                }
                cap *= 2;
                items = (JsonValue**)checked_array_realloc(items, cap, sizeof(JsonValue*));
            }
            items[len++] = item;
            skip_ws();
            if (at_end()) { error = "unterminated array"; break; }
            if (peek() == ']') { pos++; break; }
            if (!expect(',')) {
                for (size_t i = 0; i < len; i++) json_free_recursive(items[i]);
                free(items);
                return nullptr;
            }
        }
        if (!error.empty()) {
            for (size_t i = 0; i < len; i++) json_free_recursive(items[i]);
            free(items);
            return nullptr;
        }
        void *mem = arc_alloc(sizeof(JsonValue));
        if (!mem) {
            for (size_t i = 0; i < len; i++) json_free_recursive(items[i]);
            free(items);
            error = "out of memory";
            return nullptr;
        }
        auto *v = new (mem) JsonValue;
        v->type = JsonType::Array;
        v->array_val.len = (int64_t)len;
        v->array_val.items = items;
        return v;
    }

    JsonValue *parse_object() {
        pos++; // skip {
        skip_ws();
        if (!at_end() && peek() == '}') {
            pos++;
            void *mem = arc_alloc(sizeof(JsonValue));
            if (!mem) {
                error = "out of memory";
                return nullptr;
            }
            auto *v = new (mem) JsonValue;
            v->type = JsonType::Object;
            v->object_val.keys = nullptr;
            v->object_val.values = nullptr;
            v->object_val.len = 0;
            return v;
        }

        size_t cap = 8, len = 0;
        char **keys = (char**)checked_array_malloc(cap, sizeof(char*));
        JsonValue **vals = (JsonValue**)checked_array_malloc(cap, sizeof(JsonValue*));

        auto cleanup = [&]() {
            for (size_t i = 0; i < len; i++) { freeStringSlot(keys[i]); json_free_recursive(vals[i]); }
            free(keys); free(vals);
        };

        while (true) {
            skip_ws();
            if (at_end() || peek() != '"') {
                error = "expected string key at position " + std::to_string(pos);
                break;
            }
            JsonValue *keyVal = parse_string();
            if (!keyVal) break;
            char *key = keyVal->string_val;
            keyVal->string_val = nullptr;
            arc_free(keyVal);

            if (!expect(':')) { freeStringSlot(key); break; }

            JsonValue *val = parse_value();
            if (!val) { freeStringSlot(key); break; }

            if (len == cap) {
                if (cap > SIZE_MAX / 2 / sizeof(JsonValue*)) {
                    freeStringSlot(key); json_free_recursive(val);
                    error = "object too large"; cleanup(); return nullptr;
                }
                cap *= 2;
                keys = (char**)checked_array_realloc(keys, cap, sizeof(char*));
                vals = (JsonValue**)checked_array_realloc(vals, cap, sizeof(JsonValue*));
            }
            keys[len] = key;
            vals[len] = val;
            len++;

            skip_ws();
            if (at_end()) { error = "unterminated object"; break; }
            if (peek() == '}') { pos++; break; }
            if (!expect(',')) break;
        }
        if (!error.empty()) {
            cleanup();
            return nullptr;
        }
        void *mem = arc_alloc(sizeof(JsonValue));
        if (!mem) {
            cleanup();
            error = "out of memory";
            return nullptr;
        }
        auto *v = new (mem) JsonValue;
        v->type = JsonType::Object;
        v->object_val.len = (int64_t)len;
        v->object_val.keys = keys;
        v->object_val.values = vals;
        return v;
    }

};

// ===== Stringify =====

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

static void stringify_value(const JsonValue *v, std::string &out,
                            size_t indent, size_t depth, bool pretty) {
    if (!v) { out += "null"; return; }
    switch (v->type) {
        case JsonType::Null: out += "null"; break;
        case JsonType::Bool: out += v->bool_val ? "true" : "false"; break;
        case JsonType::Int: out += std::to_string(v->int_val); break;
        case JsonType::Float: {
            char buf[64];
            snprintf(buf, sizeof(buf), "%.17g", v->float_val);
            out += buf;
            break;
        }
        case JsonType::String: {
            out += '"';
            escape_string(v->string_val, stringByteLen(v->string_val), out);
            out += '"';
            break;
        }
        case JsonType::Array: {
            if (v->array_val.len == 0) { out += "[]"; break; }
            out += '[';
            for (int64_t i = 0; i < v->array_val.len; i++) {
                if (i > 0) out += ',';
                if (pretty) {
                    out += '\n';
                    out.append((depth + 1) * indent, ' ');
                }
                stringify_value(v->array_val.items[i], out, indent, depth + 1, pretty);
            }
            if (pretty) {
                out += '\n';
                out.append(depth * indent, ' ');
            }
            out += ']';
            break;
        }
        case JsonType::Object: {
            if (v->object_val.len == 0) { out += "{}"; break; }
            out += '{';
            for (int64_t i = 0; i < v->object_val.len; i++) {
                if (i > 0) out += ',';
                if (pretty) {
                    out += '\n';
                    out.append((depth + 1) * indent, ' ');
                }
                out += '"';
                escape_string(v->object_val.keys[i], stringByteLen(v->object_val.keys[i]), out);
                out += '"';
                out += ':';
                if (pretty) out += ' ';
                stringify_value(v->object_val.values[i], out, indent, depth + 1, pretty);
            }
            if (pretty) {
                out += '\n';
                out.append(depth * indent, ' ');
            }
            out += '}';
            break;
        }
    }
}


// ===== extern "C" implementations =====

extern "C" {

void *__ry_json_parse(const char *text) {
    if (!text) {
        __ry_set_last_error("json parse: input is null");
        return nullptr;
    }
    Parser parser(text);
    JsonValue *val = parser.parse_value();
    if (!val) {
        __ry_set_last_error(parser.error.c_str());
        return nullptr;
    }
    // Check for trailing content
    parser.skip_ws();
    if (!parser.at_end()) {
        __ry_set_last_error("json parse: unexpected trailing content");
        json_free_recursive(val);
        return nullptr;
    }
    return val;
}

const char *__ry_json_stringify(void *value) {
    auto *v = (JsonValue*)value;
    std::string out;
    stringify_value(v, out, 0, 0, false);
    return makeString(out.data(), out.size());
}

const char *__ry_json_stringify_pretty(void *value, int64_t indent) {
    auto *v = (JsonValue*)value;
    std::string out;
    if (indent < 0) {
        stringify_value(v, out, 0, 0, false);
    } else {
        stringify_value(v, out, static_cast<size_t>(indent), 0, true);
    }
    return makeString(out.data(), out.size());
}

const char *__ry_json_type(void *value) {
    const char *s = nullptr;
    if (!value) {
        s = "null";
    } else {
        auto *v = (JsonValue*)value;
        switch (v->type) {
            case JsonType::Null:   s = "null";    break;
            case JsonType::Bool:   s = "boolean"; break;
            case JsonType::Int:
            case JsonType::Float:  s = "number";  break;
            case JsonType::String: s = "string";  break;
            case JsonType::Array:  s = "array";   break;
            case JsonType::Object: s = "object";  break;
        }
        if (!s) s = "unknown";
    }
    return makeString(s, strlen(s));
}

void *__ry_json_get(void *value, const char *key) {
    if (!value) {
        __ry_set_last_error("json_get: value is null");
        return nullptr;
    }
    if (!key) {
        __ry_set_last_error("json_get: key is null");
        return nullptr;
    }
    auto *v = (JsonValue*)value;
    if (v->type != JsonType::Object) {
        __ry_set_last_error("json_get: value is not an object");
        return nullptr;
    }
    int64_t qlen = stringByteLen(key);
    for (int64_t i = 0; i < v->object_val.len; i++) {
        int64_t klen = stringByteLen(v->object_val.keys[i]);
        if (klen == qlen && memcmp(v->object_val.keys[i], key, (size_t)klen) == 0)
            return v->object_val.values[i];
    }
    __ry_set_last_error("json_get: key not found");
    return nullptr;
}

void *__ry_json_at(void *value, int64_t index) {
    if (!value) {
        __ry_set_last_error("json_at: value is null");
        return nullptr;
    }
    auto *v = (JsonValue*)value;
    if (v->type != JsonType::Array) {
        __ry_set_last_error("json_at: value is not an array");
        return nullptr;
    }
    if (index < 0 || index >= v->array_val.len) {
        __ry_set_last_error("json_at: index out of bounds");
        return nullptr;
    }
    return v->array_val.items[index];
}

const char *__ry_json_str(void *value) {
    if (!value) {
        __ry_set_last_error("json_str: value is null");
        return nullptr;
    }
    auto *v = (JsonValue*)value;
    if (v->type != JsonType::String) {
        __ry_set_last_error("json_str: value is not a string");
        return nullptr;
    }
    return makeString(v->string_val, (size_t)stringByteLen(v->string_val));
}

int64_t __ry_json_int(void *value, int64_t *out) {
    if (!value) {
        __ry_set_last_error("json_int: value is null");
        return 1;
    }
    auto *v = (JsonValue*)value;
    if (v->type == JsonType::Int) {
        *out = v->int_val;
        return 0;
    }
    if (v->type == JsonType::Float) {
        double d = v->float_val;
        if (d == std::floor(d) && !std::isinf(d)) {
            *out = (int64_t)d;
            return 0;
        }
    }
    __ry_set_last_error("json_int: value is not an integer");
    return 1;
}

int64_t __ry_json_float(void *value, double *out) {
    if (!value) {
        __ry_set_last_error("json_float: value is null");
        return 1;
    }
    auto *v = (JsonValue*)value;
    if (v->type == JsonType::Float) {
        *out = v->float_val;
        return 0;
    }
    if (v->type == JsonType::Int) {
        *out = (double)v->int_val;
        return 0;
    }
    __ry_set_last_error("json_float: value is not a number");
    return 1;
}

int64_t __ry_json_bool(void *value, int64_t *out) {
    if (!value) {
        __ry_set_last_error("json_bool: value is null");
        return 1;
    }
    auto *v = (JsonValue*)value;
    if (v->type != JsonType::Bool) {
        __ry_set_last_error("json_bool: value is not a boolean");
        return 1;
    }
    *out = v->bool_val ? 1 : 0;
    return 0;
}

int64_t __ry_json_len(void *value) {
    if (!value) return 0;
    auto *v = (JsonValue*)value;
    if (v->type == JsonType::Array) return v->array_val.len;
    if (v->type == JsonType::Object) return v->object_val.len;
    return 0;
}

void *__ry_json_keys(void *value) {
    if (!value) {
        __ry_set_last_error("json_keys: value is null");
        return nullptr;
    }
    auto *v = static_cast<JsonValue*>(value);
    if (v->type != JsonType::Object) {
        __ry_set_last_error("json_keys: value is not an object");
        return nullptr;
    }
    int64_t len = v->object_val.len;
    try {
        std::vector<std::string> keys;
        keys.reserve(static_cast<size_t>(len));
        for (int64_t i = 0; i < len; i++)
            keys.emplace_back(v->object_val.keys[i], (size_t)stringByteLen(v->object_val.keys[i]));
        if (auto *result = makeStringList(keys))
            return result;
    } catch (const std::exception &) { // NOLINT(bugprone-empty-catch)
        // Fall through to set error below.
    }
    __ry_set_last_error("json_keys: out of memory");
    return nullptr;
}

void __ry_json_free(void *value) {
    json_free_recursive((JsonValue*)value);
}

void __ry_json_cleanup(void *value) {
    auto *v = (JsonValue*)value;
    if (!v) return;
    json_release_contents(v);
}

} // extern "C"

} // namespace ry
