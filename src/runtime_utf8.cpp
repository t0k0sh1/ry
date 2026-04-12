#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "ry/runtime_alloc.hpp"
#include "ry/runtime_list.hpp"


namespace ry {

static inline bool is_cont(unsigned char c) { return (c & 0xC0) == 0x80; }

// UTF-8 lead byte → byte count using null-terminator detection.
// Safe because is_cont('\0') is false: (0x00 & 0xC0) == 0x00 != 0x80,
// so truncated sequences at string end are naturally rejected.
static int utf8_char_len_nul(const char *s) {
    unsigned char c = static_cast<unsigned char>(s[0]);
    if (c == 0) return 0;
    if (c < 0x80) return 1;
    if ((c & 0xE0) == 0xC0 && is_cont(static_cast<unsigned char>(s[1]))) return 2;
    if ((c & 0xF0) == 0xE0 && is_cont(static_cast<unsigned char>(s[1])) && is_cont(static_cast<unsigned char>(s[2]))) return 3;
    if ((c & 0xF8) == 0xF0 && is_cont(static_cast<unsigned char>(s[1])) && is_cont(static_cast<unsigned char>(s[2])) && is_cont(static_cast<unsigned char>(s[3]))) return 4;
    return 1; // invalid/truncated byte treated as 1
}

extern "C" {

int64_t __ry_utf8_len(const char *s) {
    int64_t count = 0;
    while (*s) {
        s += utf8_char_len_nul(s);
        ++count;
    }
    return count;
}

char *__ry_utf8_char_at(const char *s, int64_t i) {
    const char *p = s;
    for (int64_t idx = 0; *p; ++idx) {
        size_t len = static_cast<size_t>(utf8_char_len_nul(p));
        if (idx == i) {
            char *buf = static_cast<char *>(checked_malloc(len + 1));
            memcpy(buf, p, len);
            buf[len] = '\0';
            return buf;
        }
        p += len;
    }
    // Safety net: codegen should have caught this via emitBoundsCheck
    fprintf(stderr, "runtime error: char_at() index out of bounds\n");
    exit(1);
}

char *__ry_utf8_char_at_checked(const char *s, int64_t i) {
    const char *p = s;

    if (i >= 0) {
        // Positive index: single forward scan, stop at target — O(i).
        int64_t idx = 0;
        while (*p) {
            size_t len = static_cast<size_t>(utf8_char_len_nul(p));
            if (idx == i) {
                char *buf = static_cast<char *>(checked_malloc(len + 1));
                memcpy(buf, p, len);
                buf[len] = '\0';
                return buf;
            }
            p += len;
            ++idx;
        }
        // Fell through: index out of bounds. idx == string length.
        fprintf(stderr,
                "runtime error: index %lld out of bounds for string of length %lld\n",
                (long long)i, (long long)idx);
        exit(1);
    }

    // Negative index: count all codepoints to resolve wrap.
    int64_t count = 0;
    while (*p) {
        p += utf8_char_len_nul(p);
        ++count;
    }

    if (i < -count) {
        fprintf(stderr,
                "runtime error: index %lld out of bounds for string of length %lld\n",
                (long long)i, (long long)count);
        exit(1);
    }
    int64_t resolved = i + count;

    // Second pass: scan to the resolved position — O(resolved).
    p = s;
    for (int64_t idx = 0; idx < resolved; ++idx)
        p += utf8_char_len_nul(p);

    size_t len = static_cast<size_t>(utf8_char_len_nul(p));
    char *buf = static_cast<char *>(checked_malloc(len + 1));
    memcpy(buf, p, len);
    buf[len] = '\0';
    return buf;
}

char *__ry_utf8_substring(const char *s, int64_t start, int64_t endIdx) {
    const char *p = s;
    const char *startPtr = nullptr;
    const char *endPtr = nullptr;
    int64_t idx = 0;

    while (*p) {
        if (idx == start) startPtr = p;
        if (idx == endIdx) { endPtr = p; break; }
        p += utf8_char_len_nul(p);
        ++idx;
    }
    if (idx == start) startPtr = p;
    if (!endPtr) endPtr = p;
    if (!startPtr) startPtr = endPtr;

    size_t byteLen = static_cast<size_t>(endPtr - startPtr);
    char *buf = static_cast<char *>(checked_malloc(byteLen + 1));
    memcpy(buf, startPtr, byteLen);
    buf[byteLen] = '\0';
    return buf;
}

char *__ry_utf8_reverse(const char *s) {
    // Collect character byte-offsets and lengths
    size_t totalBytes = strlen(s);
    char *buf = static_cast<char *>(checked_malloc(totalBytes + 1));

    // First pass: collect codepoint boundaries
    struct CPInfo { const char *ptr; size_t len; };
    size_t capacity = 64;
    size_t count = 0;
    CPInfo *cps = static_cast<CPInfo *>(checked_array_malloc(capacity, sizeof(CPInfo)));

    const char *p = s;
    while (*p) {
        if (count == capacity) {
            capacity *= 2;
            cps = static_cast<CPInfo *>(checked_array_realloc(cps, capacity, sizeof(CPInfo)));
        }
        size_t len = static_cast<size_t>(utf8_char_len_nul(p));
        cps[count++] = {p, len};
        p += len;
    }

    // Second pass: write in reverse
    char *dst = buf;
    for (size_t i = count; i > 0; --i) {
        memcpy(dst, cps[i - 1].ptr, cps[i - 1].len);
        dst += cps[i - 1].len;
    }
    *dst = '\0';

    free(cps);
    return buf;
}

int64_t __ry_utf8_char_index(const char *s, int64_t byte_offset) {
    const char *p = s;
    int64_t charIdx = 0;
    int64_t byteIdx = 0;
    while (*p && byteIdx < byte_offset) {
        p += utf8_char_len_nul(p);
        byteIdx = p - s;
        ++charIdx;
    }
    return charIdx;
}

void *__ry_split_chars(const char *s) {
    // First pass: count UTF-8 characters
    int64_t count = 0;
    for (const char *p = s; *p; p += utf8_char_len_nul(p))
        ++count;

    // Build ListHeader directly (avoids intermediate vector + double-copy)
    auto *header = (ListHeader *)checked_malloc(sizeof(ListHeader));
    header->len = count;
    header->cap = count;
    header->data = (char **)checked_array_malloc(count ? static_cast<size_t>(count) : 1, sizeof(char *));

    // Second pass: populate string array
    const char *p = s;
    for (int64_t i = 0; i < count; ++i) {
        size_t len = static_cast<size_t>(utf8_char_len_nul(p));
        header->data[i] = dupString(p, len);
        p += len;
    }
    return header;
}

} // extern "C"

} // namespace ry
