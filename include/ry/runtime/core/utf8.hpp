#pragma once

#include <cstdint>

namespace ry {

extern "C" {

// Count UTF-8 codepoints in s[0..byte_offset), stopping exactly at byte_offset.
// NUL-safe: NUL bytes are counted as single codepoints.
// Precondition: 0 <= byte_offset <= byte_len.
// Defined in src/runtime/core/utf8.cpp.
int64_t __ry_utf8_char_index_n(const char *s, int64_t byte_len, int64_t byte_offset);

}

} // namespace ry
