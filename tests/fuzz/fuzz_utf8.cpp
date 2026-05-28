#include "ry/runtime/core/string.hpp"

#include <cstddef>
#include <cstdint>
#include <limits>

using namespace ry;

extern "C" {
int64_t __ry_utf8_len_n(const char *s, int64_t byte_len);
char   *__ry_utf8_char_at_checked(const char *s, int64_t byte_len, int64_t i);
char   *__ry_utf8_reverse(const char *s, int64_t byte_len);
char   *__ry_utf8_substring(const char *s, int64_t byte_len,
                              int64_t start_idx, int64_t end_idx);
}

extern "C" int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
    if (size > static_cast<size_t>(std::numeric_limits<int64_t>::max()))
        return 0;
    const char *s = reinterpret_cast<const char *>(data);
    int64_t len = static_cast<int64_t>(size);

    // Count codepoints (pure walk, no allocation).
    int64_t char_count = __ry_utf8_len_n(s, len);

    // Index access: only with valid indices to avoid exit(1) on OOB.
    if (char_count > 0) {
        char *first = __ry_utf8_char_at_checked(s, len, 0);
        freeStringSlot(first);
        char *last = __ry_utf8_char_at_checked(s, len, char_count - 1);
        freeStringSlot(last);
    }

    // Reverse (handles empty input safely).
    char *rev = __ry_utf8_reverse(s, len);
    freeStringSlot(rev);

    // Substring: first half.
    if (char_count >= 2) {
        char *sub = __ry_utf8_substring(s, len, 0, char_count / 2);
        freeStringSlot(sub);
    }

    return 0;
}
