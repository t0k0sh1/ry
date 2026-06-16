#include "ry/runtime/core/alloc.hpp"
#include "ry/runtime/core/error.hpp"
#include "ry/runtime/native/io.hpp"
#include "ry/runtime/core/string.hpp"

#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <memory>
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

// Core decode: writes decoded bytes into a heap buffer owned by the returned
// unique_ptr. Returns an empty unique_ptr + setLastError on invalid input;
// writes byte count to *out_len on success.
static std::unique_ptr<uint8_t, decltype(&std::free)>
base64_decode_raw(const char *input, size_t len,
                  const int8_t *decode_tbl, size_t *out_len) {
    // Validate padding per RFC 4648 §3.2 strict mode. When any '=' padding is
    // present, the total length must be a multiple of 4 and the padding count
    // must be at most 2. Inputs with no padding (e.g. URL-safe canonical form)
    // are unaffected.
    size_t pad_count = 0;
    while (pad_count < len && input[len - 1 - pad_count] == '=')
        pad_count++;
    if (pad_count > 0) {
        if (pad_count > 2) {
            setLastError("invalid base64: excess padding (%zu padding characters)", pad_count);
            return {nullptr, &std::free};
        }
        if (len % 4 != 0) {
            setLastError("invalid base64: input length must be a multiple of 4 when padding is present");
            return {nullptr, &std::free};
        }
    }
    len -= pad_count;

    std::unique_ptr<uint8_t, decltype(&std::free)> out(
        static_cast<uint8_t *>(checked_malloc(len * 3 / 4 + 1)), &std::free);
    uint8_t *buf = out.get();
    size_t j = 0;

    for (size_t i = 0; i < len; ) {
        uint32_t sextet[4] = {0, 0, 0, 0};
        int count = 0;
        for (int k = 0; k < 4 && i < len; k++, i++) {
            int8_t val = decode_tbl[(uint8_t)input[i]];
            if (val < 0) {
                setLastError("invalid base64 character at position %zu", i);
                return {nullptr, &std::free};
            }
            sextet[k] = static_cast<uint32_t>(static_cast<unsigned char>(val));
            count++;
        }
        if (count < 2) {
            setLastError("invalid base64: truncated input");
            return {nullptr, &std::free};
        }
        uint32_t triple = (sextet[0] << 18) | (sextet[1] << 12) | (sextet[2] << 6) | sextet[3];
        if (count >= 2) buf[j++] = (uint8_t)((triple >> 16) & 0xFF);
        if (count >= 3) buf[j++] = (uint8_t)((triple >> 8) & 0xFF);
        if (count >= 4) buf[j++] = (uint8_t)(triple & 0xFF);
    }
    *out_len = j;
    return out;
}

static char *base64_decode_impl(const char *input, size_t len, const int8_t *decode_tbl) {
    size_t j;
    auto raw = base64_decode_raw(input, len, decode_tbl, &j);
    if (!raw) return nullptr;
    return makeString(reinterpret_cast<const char *>(raw.get()), j);
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

// ===== List<u8> variants =====

extern "C" const char *__ry_base64_encode_bytes(void *list) {
    auto *header = (IOListHeader *)list;
    if (header->len == 0) return makeString("", 0);
    return base64_encode_impl(reinterpret_cast<const char *>(header->data),
                              static_cast<size_t>(header->len), std_table, true);
}

extern "C" const char *__ry_base64_encode_bytes_url_safe(void *list) {
    auto *header = (IOListHeader *)list;
    if (header->len == 0) return makeString("", 0);
    return base64_encode_impl(reinterpret_cast<const char *>(header->data),
                              static_cast<size_t>(header->len), url_table, false);
}

extern "C" void *__ry_base64_decode_bytes(const char *input) {
    if (!input) return makeEmptyIOList();
    size_t len = static_cast<size_t>(stringByteLen(input));
    if (len == 0) return makeEmptyIOList();
    ensure_decode_tables();
    size_t out_len;
    auto raw = base64_decode_raw(input, len, std_decode_tbl, &out_len);
    if (!raw) return nullptr;
    return makeByteList(raw.get(), static_cast<int64_t>(out_len));
}

extern "C" void *__ry_base64_decode_bytes_url_safe(const char *input) {
    if (!input) return makeEmptyIOList();
    size_t len = static_cast<size_t>(stringByteLen(input));
    if (len == 0) return makeEmptyIOList();
    ensure_decode_tables();
    size_t out_len;
    auto raw = base64_decode_raw(input, len, url_decode_tbl, &out_len);
    if (!raw) return nullptr;
    return makeByteList(raw.get(), static_cast<int64_t>(out_len));
}

} // namespace ry
