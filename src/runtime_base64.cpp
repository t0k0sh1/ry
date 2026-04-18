#include "ry/runtime_alloc.hpp"
#include "ry/runtime_error.hpp"
#include "ry/runtime_string.hpp"

#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <mutex>


namespace ry {

DEFINE_LAST_ERROR(base64)

// Standard base64 alphabet
static const char std_table[] =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

// URL-safe base64 alphabet
static const char url_table[] =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

// Pre-computed decode lookup tables (avoids rebuilding on every call)
static int8_t std_decode_tbl[256];
static int8_t url_decode_tbl[256];

static void build_decode_table(const char *table, int8_t *decode_tbl) {
    memset(decode_tbl, -1, 256);
    for (int i = 0; i < 64; i++)
        decode_tbl[(uint8_t)table[i]] = (int8_t)i;
}

static std::once_flag decode_tables_init;
static void init_decode_tables() {
    build_decode_table(std_table, std_decode_tbl);
    build_decode_table(url_table, url_decode_tbl);
}
static void ensure_decode_tables() {
    std::call_once(decode_tables_init, init_decode_tables);
}

static char *base64_encode_impl(const char *input, size_t len, const char *table, bool pad) {
    size_t out_len = 4 * ((len + 2) / 3);
    if (!pad) {
        out_len = 4 * (len / 3);
        size_t rem = len % 3;
        if (rem == 1) out_len += 2;
        else if (rem == 2) out_len += 3;
    }
    char *out = makeStringUninit(out_len);

    size_t j = 0;
    for (size_t i = 0; i < len; i += 3) {
        uint32_t a = (uint8_t)input[i];
        uint32_t b = (i + 1 < len) ? (uint8_t)input[i + 1] : 0;
        uint32_t c = (i + 2 < len) ? (uint8_t)input[i + 2] : 0;
        uint32_t triple = (a << 16) | (b << 8) | c;

        out[j++] = table[(triple >> 18) & 0x3F];
        out[j++] = table[(triple >> 12) & 0x3F];
        if (i + 1 < len)
            out[j++] = table[(triple >> 6) & 0x3F];
        else if (pad)
            out[j++] = '=';
        if (i + 2 < len)
            out[j++] = table[triple & 0x3F];
        else if (pad)
            out[j++] = '=';
    }
    // NUL at out[out_len] already written by makeStringUninit
    return out;
}

static char *base64_decode_impl(const char *input, size_t len, const int8_t *decode_tbl) {
    // Strip trailing padding
    while (len > 0 && input[len - 1] == '=')
        len--;

    size_t out_cap = len * 3 / 4;
    // Use a plain temp buffer; wrap with makeString at the end.
    char *out = (char *)checked_malloc(out_cap + 1);
    size_t j = 0;

    for (size_t i = 0; i < len; ) {
        uint32_t sextet[4] = {0, 0, 0, 0};
        int count = 0;
        for (int k = 0; k < 4 && i < len; k++, i++) {
            int8_t val = decode_tbl[(uint8_t)input[i]];
            if (val < 0) {
                free(out);
                setLastError("invalid base64 character at position %zu", i);
                return nullptr;
            }
            sextet[k] = static_cast<uint32_t>(static_cast<unsigned char>(val));
            count++;
        }
        if (count < 2) {
            free(out);
            setLastError("invalid base64: truncated input");
            return nullptr;
        }
        uint32_t triple = (sextet[0] << 18) | (sextet[1] << 12) | (sextet[2] << 6) | sextet[3];
        if (count >= 2) out[j++] = (char)((triple >> 16) & 0xFF);
        if (count >= 3) out[j++] = (char)((triple >> 8) & 0xFF);
        if (count >= 4) out[j++] = (char)(triple & 0xFF);
    }
    char *result = makeString(out, j);
    free(out);
    return result;
}

// Null/empty input guard shared by all public functions
static const char *empty_guard(const char *input, size_t *len) {
    if (!input) return makeString("", 0);
    *len = static_cast<size_t>(stringByteLen(input));
    if (*len == 0) return makeString("", 0);
    return nullptr;
}

// ===== Public API =====

extern "C" const char *__ry_base64_encode(const char *input) {
    size_t len;
    if (auto *r = empty_guard(input, &len)) return r;
    return base64_encode_impl(input, len, std_table, true);
}

extern "C" const char *__ry_base64_decode(const char *input) {
    size_t len;
    if (auto *r = empty_guard(input, &len)) return r;
    ensure_decode_tables();
    return base64_decode_impl(input, len, std_decode_tbl);
}

extern "C" const char *__ry_base64_encode_url_safe(const char *input) {
    size_t len;
    if (auto *r = empty_guard(input, &len)) return r;
    return base64_encode_impl(input, len, url_table, false);
}

extern "C" const char *__ry_base64_decode_url_safe(const char *input) {
    size_t len;
    if (auto *r = empty_guard(input, &len)) return r;
    ensure_decode_tables();
    return base64_decode_impl(input, len, url_decode_tbl);
}

} // namespace ry
