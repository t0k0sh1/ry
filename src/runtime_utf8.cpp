#include <cstdint>
#include <cstdlib>
#include <cstring>

// UTF-8 lead byte → byte count
static int utf8_char_len(unsigned char c) {
    if (c < 0x80) return 1;
    if ((c & 0xE0) == 0xC0) return 2;
    if ((c & 0xF0) == 0xE0) return 3;
    if ((c & 0xF8) == 0xF0) return 4;
    return 1; // invalid byte treated as 1
}

extern "C" {

int64_t __ry_utf8_len(const char *s) {
    int64_t count = 0;
    while (*s) {
        s += utf8_char_len(static_cast<unsigned char>(*s));
        ++count;
    }
    return count;
}

char *__ry_utf8_char_at(const char *s, int64_t i) {
    const char *p = s;
    for (int64_t idx = 0; *p; ++idx) {
        int len = utf8_char_len(static_cast<unsigned char>(*p));
        if (idx == i) {
            char *buf = static_cast<char *>(malloc(len + 1));
            memcpy(buf, p, len);
            buf[len] = '\0';
            return buf;
        }
        p += len;
    }
    // Out of bounds: return empty string
    char *buf = static_cast<char *>(malloc(1));
    buf[0] = '\0';
    return buf;
}

char *__ry_utf8_substring(const char *s, int64_t start, int64_t end) {
    const char *p = s;
    const char *startPtr = nullptr;
    const char *endPtr = nullptr;
    int64_t idx = 0;

    while (*p) {
        if (idx == start) startPtr = p;
        if (idx == end) { endPtr = p; break; }
        p += utf8_char_len(static_cast<unsigned char>(*p));
        ++idx;
    }
    if (idx == start) startPtr = p;
    if (!endPtr) endPtr = p;
    if (!startPtr) startPtr = endPtr;

    size_t byteLen = endPtr - startPtr;
    char *buf = static_cast<char *>(malloc(byteLen + 1));
    memcpy(buf, startPtr, byteLen);
    buf[byteLen] = '\0';
    return buf;
}

char *__ry_utf8_reverse(const char *s) {
    // Collect character byte-offsets and lengths
    size_t totalBytes = strlen(s);
    char *buf = static_cast<char *>(malloc(totalBytes + 1));

    // First pass: collect codepoint boundaries
    struct CPInfo { const char *ptr; int len; };
    size_t capacity = 64;
    size_t count = 0;
    CPInfo *cps = static_cast<CPInfo *>(malloc(capacity * sizeof(CPInfo)));

    const char *p = s;
    while (*p) {
        if (count == capacity) {
            capacity *= 2;
            cps = static_cast<CPInfo *>(realloc(cps, capacity * sizeof(CPInfo)));
        }
        int len = utf8_char_len(static_cast<unsigned char>(*p));
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
        p += utf8_char_len(static_cast<unsigned char>(*p));
        byteIdx = p - s;
        ++charIdx;
    }
    return charIdx;
}

} // extern "C"
