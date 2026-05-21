#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "ry/runtime_alloc.hpp"
#include "ry/runtime_arc.hpp"
#include "ry/runtime_list.hpp"
#include "ry/runtime_string.hpp"


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

// Bounded codepoint step: safe for non-NUL-terminated byte buffers.
// Never reads past end; falls back to 1 for truncated multi-byte sequences.
static inline size_t utf8_codepoint_step_n(const char *p, const char *end) {
    unsigned char c = static_cast<unsigned char>(*p);
    size_t rem = static_cast<size_t>(end - p);
    if (c < 0x80) return 1;
    if ((c & 0xE0) == 0xC0 && rem >= 2 && is_cont(static_cast<unsigned char>(p[1]))) return 2;
    if ((c & 0xF0) == 0xE0 && rem >= 3 && is_cont(static_cast<unsigned char>(p[1])) && is_cont(static_cast<unsigned char>(p[2]))) return 3;
    if ((c & 0xF8) == 0xF0 && rem >= 4 && is_cont(static_cast<unsigned char>(p[1])) && is_cont(static_cast<unsigned char>(p[2])) && is_cont(static_cast<unsigned char>(p[3]))) return 4;
    return 1; // truncated, invalid, or NUL — advance by one byte
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

// NUL-safe variant: walks exactly byte_len bytes, counting UTF-8 codepoints.
// Embedded NUL bytes each count as one character unit.
int64_t __ry_utf8_len_n(const char *s, int64_t byte_len) {
    int64_t count = 0;
    const char *end = s + byte_len;
    while (s < end) {
        s += utf8_codepoint_step_n(s, end);
        ++count;
    }
    return count;
}

char *__ry_utf8_char_at(const char *s, int64_t i) {
    const char *p = s;
    for (int64_t idx = 0; *p; ++idx) {
        size_t len = static_cast<size_t>(utf8_char_len_nul(p));
        if (idx == i)
            return makeString(p, len);
        p += len;
    }
    // Safety net: codegen should have caught this via emitBoundsCheck
    fprintf(stderr, "runtime error: charAt() index out of bounds\n");
    ry_runtime_trap_exit();
}

char *__ry_utf8_char_at_checked(const char *s, int64_t byte_len, int64_t i) {
    const char *p = s;
    const char *end = s + byte_len;

    if (i >= 0) {
        // Positive index: single forward scan, stop at target — O(i).
        int64_t idx = 0;
        while (p < end) {
            size_t len = utf8_codepoint_step_n(p, end);
            if (idx == i)
                return makeString(p, len);
            p += len;
            ++idx;
        }
        // Fell through: index out of bounds. idx == string length.
        fprintf(stderr,
                "runtime error: index %lld out of bounds for string of length %lld\n",
                (long long)i, (long long)idx);
        ry_runtime_trap_exit();
    }

    // Negative index: count all codepoints to resolve wrap.
    int64_t count = 0;
    while (p < end) {
        p += utf8_codepoint_step_n(p, end);
        ++count;
    }

    if (i < -count) {
        fprintf(stderr,
                "runtime error: index %lld out of bounds for string of length %lld\n",
                (long long)i, (long long)count);
        ry_runtime_trap_exit();
    }
    int64_t resolved = i + count;

    // Second pass: scan to the resolved position — O(resolved).
    p = s;
    for (int64_t idx = 0; idx < resolved; ++idx)
        p += utf8_codepoint_step_n(p, end);

    size_t len = utf8_codepoint_step_n(p, end);
    return makeString(p, len);
}

char *__ry_utf8_substring(const char *s, int64_t byte_len,
                          int64_t start, int64_t endIdx) {
    const char *p = s;
    const char *end = s + byte_len;
    const char *startPtr = nullptr;
    const char *endPtr = nullptr;
    int64_t idx = 0;

    while (p < end) {
        if (idx == start) startPtr = p;
        if (idx == endIdx) { endPtr = p; break; }
        p += utf8_codepoint_step_n(p, end);
        ++idx;
    }
    if (idx == start) startPtr = p;
    if (!endPtr) endPtr = p;
    if (!startPtr) startPtr = endPtr;

    size_t byteLen = static_cast<size_t>(endPtr - startPtr);
    return makeString(startPtr, byteLen);
}

char *__ry_utf8_reverse(const char *s, int64_t byte_len) {
    // Collect character byte-offsets and lengths
    char *buf = makeStringUninit(static_cast<size_t>(byte_len));

    // First pass: collect codepoint boundaries.
    // byte_len is an exact upper bound on codepoint count (≥1 byte per codepoint),
    // so a single allocation suffices — no realloc needed.
    struct CPInfo { const char *ptr; size_t len; };
    size_t count = 0;
    auto cap = static_cast<size_t>(byte_len > 0 ? byte_len : 1);
    CPInfo *cps = static_cast<CPInfo *>(checked_array_malloc(cap, sizeof(CPInfo)));

    const char *p = s;
    const char *end = s + byte_len;
    while (p < end) {
        size_t len = utf8_codepoint_step_n(p, end);
        cps[count++] = {p, len};
        p += len;
    }

    // Second pass: write in reverse
    char *dst = buf;
    for (size_t i = count; i > 0; --i) {
        memcpy(dst, cps[i - 1].ptr, cps[i - 1].len);
        dst += cps[i - 1].len;
    }
    // NUL at buf[byte_len] is already written by makeStringUninit

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

// NUL-safe variant: walks exactly byte_offset bytes (ignoring any embedded NUL
// bytes), counting UTF-8 codepoints.  Each NUL counts as one character unit,
// matching __ry_utf8_len_n.  Precondition: 0 <= byte_offset <= byte_len.
// Called by codegen for find() to convert a NUL-safe byte offset to a char index.
int64_t __ry_utf8_char_index_n(const char *s, int64_t byte_len, int64_t byte_offset) {
    const char *p = s;
    const char *end = s + byte_len;
    const char *target = s + byte_offset;
    int64_t charIdx = 0;
    while (p < target) {
        p += utf8_codepoint_step_n(p, end);
        ++charIdx;
    }
    return charIdx;
}

void *__ry_split_chars(const char *s, int64_t byte_len) {
    const char *end = s + byte_len;

    // First pass: count UTF-8 characters
    int64_t count = 0;
    for (const char *p = s; p < end; ) {
        p += utf8_codepoint_step_n(p, end);
        ++count;
    }

    // Build ListHeader directly (avoids intermediate vector + double-copy)
    auto *header = (ListHeader *)arc_alloc(sizeof(ListHeader));
    header->len = count;
    header->cap = count;
    header->data = (char **)checked_array_malloc(count ? static_cast<size_t>(count) : 1, sizeof(char *));

    // Second pass: populate string array
    const char *p = s;
    for (int64_t i = 0; i < count; ++i) {
        size_t len = utf8_codepoint_step_n(p, end);
        header->data[i] = dupString(p, len);
        p += len;
    }
    return header;
}

} // extern "C"

} // namespace ry
